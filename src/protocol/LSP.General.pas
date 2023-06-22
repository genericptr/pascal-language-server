// Pascal Language Server
// Copyright 2020 Arjan Adriaanse
// Copyright 2020 Ryan Joseph

// This file is part of Pascal Language Server.

// Pascal Language Server is free software: you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// Pascal Language Server is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Pascal Language Server.  If not, see
// <https://www.gnu.org/licenses/>.

unit LSP.General;

{$mode objfpc}{$H+}
{$modeswitch arrayoperators}

interface

uses
  { RTL }
  Classes, LazUTF8, typinfo,
  { Code Tools }
  CodeToolManager, CodeToolsConfig, 
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Capabilities, LSP.DocumentSymbol,
  { Utils }
  PasLS.Settings, PasLS.Symbols, PasLS.Commands, PasLS.LazConfig;

type

  { TWorkspaceFolder }

  TWorkspaceFolder = class(TCollectionItem)
  private
    fUri: TDocumentUri;
    fName: String;
  Public
    Procedure assign(Source : TPersistent); override;
  published
    // The associated URI for this workspace folder.
    property uri: TDocumentUri read fUri write fUri;
    // The name of the workspace folder. Used to refer to this
    // workspace folder in the user interface.
    property name: string read fName write fName;
  end;
  TWorkspaceFolderItems = specialize TGenericCollection<TWorkspaceFolder>;

  { TVoidParams }

  TVoidParams = class(TLSPStreamable);

  { TInitializationOptions }

  TInitializationOptions = class(TServerSettings)
  end;

  { TInitializeParams }

  TInitializeParams = class(TLSPStreamable)
  private
    fProcessId: integer;
    fClientInfo: TClientInfo;
    fRootUri: string;
    fCapabilities: TClientCapabilities;
    fTrace: string;
    fInitializationOptions: TInitializationOptions;
    fWorkspaceFolders: TWorkspaceFolderItems;
    procedure SetCapabilities(AValue: TClientCapabilities);
    procedure SetClientInfo(AValue: TClientInfo);
    procedure SetInitializationOptions(AValue: TInitializationOptions);
    procedure SetWorkspaceFolders(AValue: TWorkspaceFolderItems);
  published
    // The process Id of the parent process that started
    // the server. Is null if the process has not been started by another process.
    // If the parent process is not alive then the server should exit (see exit notification) its process.
    property processId: integer read fProcessId write fProcessId;
    // Information about the client
    property clientInfo: TClientInfo read fClientInfo write SetClientInfo;
    // The rootUri of the workspace. Is null if no
    // folder is open. If both `rootPath` and `rootUri` are set
    // `rootUri` wins.
    property rootUri: string read fRootUri write fRootUri;
    // User provided initialization options.
    property initializationOptions: TInitializationOptions read fInitializationOptions write SetInitializationOptions;
    // The capabilities provided by the client (editor or tool)
    property capabilities: TClientCapabilities read fCapabilities write SetCapabilities;
    // The initial trace setting. If omitted trace is disabled ('off').
    property trace: string read fTrace write fTrace;
    // The workspace folders configured in the client when the server starts.
    // This property is only available if the client supports workspace folders.
    // It can be `null` if the client supports workspace folders but none are
    // configured.
    property workspaceFolders: TWorkspaceFolderItems read fWorkspaceFolders write SetWorkspaceFolders;
  public
    constructor create; override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  end;

  { TInitializeResult }

  TInitializeResult = class(TLSPStreamable)
  private
    fCapabilities: TServerCapabilities;
    procedure SetCapabilities(AValue: TServerCapabilities);
  Public
    constructor Create; override;
    destructor Destroy; override;
  published
    property capabilities: TServerCapabilities read fCapabilities write SetCapabilities;
  end;

  { TInitialize }

  TInitialize = class(specialize TLSPRequest<TInitializeParams, TInitializeResult>)
  private
    function CheckProgramSetting: Boolean;
    procedure CollectWorkSpacePaths(WorkspaceFolders: TWorkspaceFolderItems;
      aPaths: TStrings);
    procedure DoLog(const Msg: String);
    procedure DoLog(const Fmt: String; const args: array of const);
    procedure DoLog(const Msg: String; aBool: Boolean);
    Function IsPasExt(Const aExtension : String) : Boolean;
    procedure SetFPCPaths(Paths, Opts: TStrings; AsUnitPath, asIncludePath: Boolean);
    procedure SetPlatformDefaults(CodeToolsOptions : TCodeToolsOptions);
    procedure ApplyConfigSettings(CodeToolsOptions: TCodeToolsOptions);
    procedure FindPascalSourceDirectories(RootPath: String; Results: TStrings);
    Procedure ShowConfigStatus(Params : TInitializeParams; CodeToolsOptions: TCodeToolsOptions);
  Public
    function Process(var Params : TInitializeParams): TInitializeResult; override;
  end;

  { TInitialized }

  TInitialized = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancelParams }

  TCancelParams = class(TLSPStreamable)
  private
    fId: Integer;
  published
    property id: Integer read fId write fId;
  end;

  { TShutdown }

  TShutdown = class(specialize TLSPRequest<TVoidParams, TLSPStreamable>)
    function Process(var Params : TVoidParams): TLSPStreamable; override;
  end;

  { TExit }

  TExit = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancel }

  TCancel = class(specialize TLSPNotification<TCancelParams>)
    procedure Process(var Params : TCancelParams); override;
  end;

implementation

uses
  SysUtils, RegExpr, DefineTemplates;

const
  kStatusPrefix = '✓ ';
  kFailedPrefix = '⚠️ ';
  kSettingPrefix = '  ► ';

{ TWorkspaceFolder }

procedure TWorkspaceFolder.assign(Source : TPersistent);

var
  WF : TWorkspaceFolder absolute source;

begin
  if source is TWorkspaceFolder then
    begin
    Uri:=wf.uri;
    Name:=wf.name;

    end
  else
    inherited assign(Source);
end;

{ TInitializeParams }

procedure TInitializeParams.SetCapabilities(AValue: TClientCapabilities);
begin
  if fCapabilities=AValue then Exit;
  fCapabilities.Assign(AValue);
end;

procedure TInitializeParams.SetClientInfo(AValue: TClientInfo);
begin
  if fClientInfo=AValue then Exit;
  fClientInfo.Assign(AValue);
end;

procedure TInitializeParams.SetInitializationOptions(
  AValue: TInitializationOptions);
begin
  if fInitializationOptions=AValue then Exit;
  fInitializationOptions.Assign(AValue);
end;

procedure TInitializeParams.SetWorkspaceFolders(AValue: TWorkspaceFolderItems);
begin
  if fWorkspaceFolders=AValue then Exit;
  fWorkspaceFolders.Assign(AValue);
end;

constructor TInitializeParams.create;
begin
  inherited;
  fclientInfo := TClientInfo.Create;
  finitializationOptions := TInitializationOptions.Create;
  fworkspaceFolders := TWorkspaceFolderItems.Create;
  fCapabilities:=TClientCapabilities.Create;
end;


destructor TInitializeParams.Destroy; 
begin
  FreeAndNil(fCapabilities);
  FreeAndNil(finitializationOptions);
  FreeAndNil(fclientInfo);
  FreeAndNil(fworkspaceFolders);
  inherited;
end;

procedure TInitializeParams.Assign(Source : TPersistent);

var
  Src : TInitializeParams absolute Source;

begin
  if Source is TInitializeParams then
    begin
    processId:=Src.ProcessID;
    clientInfo:=Src.ClientInfo;
    rootUri:=Src.RootUri;
    initializationOptions:=Src.initializationOptions;
    capabilities:=Src.Capabilities;
    trace:=Src.Trace;
    workspaceFolders:=Src.workspaceFolders;
    end
  else
    inherited Assign(Source);
end;

{ TInitializeResult }

procedure TInitializeResult.SetCapabilities(AValue: TServerCapabilities);
begin
  if fCapabilities=AValue then Exit;
  fCapabilities.Assign(AValue);
end;

constructor TInitializeResult.Create;
begin
  fCapabilities:=TServerCapabilities.Create(Nil);
end;

destructor TInitializeResult.Destroy;
begin
  FreeAndNil(fCapabilities);
  inherited Destroy;
end;

{ TInitialize }


procedure TInitialize.ApplyConfigSettings(CodeToolsOptions: TCodeToolsOptions);

  function MaybeSet(aValue,aDefault : String) : String;

  begin
    Result:=aValue;
    if Result='' then
      Result:=aDefault;
  end;

Var
  env : TConfigEnvironmentSettings;

begin
  env:=EnvironmentSettings;
  with CodeToolsOptions do
    begin
      FPCPath:=MaybeSet(Env.pp,FPCPath);
      FPCSrcDir:=MaybeSet(Env.fpcDir,FPCSrcDir);
      LazarusSrcDir:=MaybeSet(Env.lazarusDir,LazarusSrcDir);
      TargetOS:=MaybeSet(Env.fpcTarget,TargetOS);
      TargetProcessor:=MaybeSet(Env.fpcTargetCPU,TargetProcessor);
    end;
end;

procedure TInitialize.SetPlatformDefaults(CodeToolsOptions: TCodeToolsOptions);
begin
  // Compile time defaults/
  CodeToolsOptions.TargetOS := {$i %FPCTARGETOS%};
  CodeToolsOptions.TargetProcessor := {$i %FPCTARGETCPU%};

  {$ifdef windows}
  CodeToolsOptions.FPCPath := 'C:\FPC';
  CodeToolsOptions.FPCSrcDir := 'C:\FPC\Src';
  CodeToolsOptions.LazarusSrcDir := 'C:\Lazarus';
  {$endif}

  {$ifdef unix}
  {$ifdef DARWIN}
  CodeToolsOptions.FPCPath := '/usr/local/bin/fpc';
  CodeToolsOptions.FPCSrcDir := '/usr/local/share/fpcsrc';
  CodeToolsOptions.LazarusSrcDir := '/usr/local/share/lazsrc';
  {$else}
  CodeToolsOptions.FPCPath := '/usr/local/bin/fpc';
  CodeToolsOptions.FPCSrcDir := '/usr/local/share/fpcsrc';
  CodeToolsOptions.LazarusSrcDir := '/usr/local/share/lazsrc';
  {$endif}
  {$endif}

end;

{ Find all sub directories which contain Pascal source files }

Function TInitialize.IsPasExt(Const aExtension : String) : Boolean;

var
  E : String;

begin
  E:=LowerCase(aExtension);
  result:=(E = '.pas') or (E = '.pp') or (E = '.inc');
end;


Procedure TInitialize.DoLog(const Msg : String);
begin
  Transport.SendDiagnostic(Msg);
end;


Procedure TInitialize.DoLog(const Fmt : String; Const args : Array of const);
begin
  Transport.SendDiagnostic(Fmt,Args);
end;

Procedure TInitialize.DoLog(const Msg : String; aBool : Boolean);
begin
  Transport.SendDiagnostic(Msg+BoolToStr(aBool,'True','False'));
end;


procedure TInitialize.FindPascalSourceDirectories(RootPath: String; Results: TStrings);

var
  Info : TSearchRec;
  havePas : Boolean;

begin
  havePas:=False;
  If FindFirst(RootPath+AllFilesMask,faAnyFile,Info)=0 then
    try
      Repeat
        if ((Info.Attr and faDirectory)<>0) and Not ((Info.Name='.') or (Info.Name='..')) then
          FindPascalSourceDirectories(IncludeTrailingPathDelimiter(RootPath+Info.Name),Results);
        if IsPasExt(ExtractFileExt(Info.Name)) then
          HavePas:=True;
      until (FindNext(Info)<>0);
    finally
      FindClose(Info)
    end;
  if HavePas then
    if Results.IndexOf(RootPath)=-1 then
      Results.Add(RootPath);
end;

procedure TInitialize.CollectWorkSpacePaths(WorkspaceFolders : TWorkspaceFolderItems; aPaths : TStrings);

Var
  Item: TCollectionItem;

begin
  for Item in workspaceFolders do
    FindPascalSourceDirectories(IncludeTrailingPathDelimiter(UriToPath(TWorkspaceFolder(Item).uri)), aPaths);
end;


procedure TInitialize.ShowConfigStatus(Params : TInitializeParams; CodeToolsOptions: TCodeToolsOptions);

begin
  DoLog( kStatusPrefix+'Server: ' + {$INCLUDE %DATE%});
  DoLog( kStatusPrefix+'Client: ' + Params.clientInfo.name + ' ' + Params.clientInfo.version);

  DoLog( kStatusPrefix+'FPCPath: ' + CodeToolsOptions.FPCPath);
  DoLog( kStatusPrefix+'FPCSrcDir: ' + CodeToolsOptions.FPCSrcDir);
  DoLog( kStatusPrefix+'TargetOS: ' + CodeToolsOptions.TargetOS);
  DoLog( kStatusPrefix+'TargetProcessor: '+ CodeToolsOptions.TargetProcessor);

  DoLog( kStatusPrefix+'Working directory: ' + GetCurrentDir);

  if CodeToolsOptions.FPCOptions <> '' then
    DoLog( kStatusPrefix+'FPCOptions: '+CodeToolsOptions.FPCOptions)
  else
    DoLog( kStatusPrefix+'FPCOptions: [unspecified]');

  if ServerSettings.&program <> '' then
    DoLog( kStatusPrefix+'Main program file: ' + ServerSettings.&program);

  if CodeToolsOptions.ProjectDir <> '' then
    DoLog( kStatusPrefix+'ProjectDir: ' + CodeToolsOptions.ProjectDir)
  else
    DoLog( kStatusPrefix+'ProjectDir: [unspecified]');

  if ServerSettings.symbolDatabase <> '' then
    DoLog( kStatusPrefix+'Symbol Database: ' + ServerSettings.symbolDatabase)
  else
    DoLog( kStatusPrefix+'Symbol Database: [unspecified]');

  // other settings
  DoLog(kStatusPrefix+'Settings:');
  DoLog('maximumCompletions: %d', [ServerSettings.maximumCompletions]);
  DoLog('  ► overloadPolicy: %s', [GetEnumName(TypeInfo(TOverloadPolicy),Ord(ServerSettings.overloadPolicy))]);
  DoLog('  ► insertCompletionsAsSnippets: ', ServerSettings.insertCompletionsAsSnippets);
  DoLog('  ► insertCompletionProcedureBrackets: ', ServerSettings.insertCompletionProcedureBrackets);
  DoLog('  ► includeWorkspaceFoldersAsUnitPaths: ', ServerSettings.includeWorkspaceFoldersAsUnitPaths);
  DoLog('  ► includeWorkspaceFoldersAsIncludePaths: ', ServerSettings.includeWorkspaceFoldersAsIncludePaths);
  DoLog('  ► checkSyntax: ', ServerSettings.checkSyntax);
  DoLog('  ► publishDiagnostics: ', ServerSettings.publishDiagnostics);
  DoLog('  ► workspaceSymbols: ', ServerSettings.workspaceSymbols);
  DoLog('  ► documentSymbols: ', ServerSettings.documentSymbols);
  DoLog('  ► minimalisticCompletions: ', ServerSettings.minimalisticCompletions);
  DoLog('  ► showSyntaxErrors: ', ServerSettings.showSyntaxErrors);
end;


procedure TInitialize.SetFPCPaths(Paths,Opts: TStrings; AsUnitPath,asIncludePath : Boolean);

var
  aPath : String;

begin
  for aPath in Paths do
    begin
      // add directory as search paths
      if AsUnitPath then
        Opts.Add('-Fu'+aPath);
      if AsIncludePath then
        Opts.Add('-Fi'+aPath);
    end;
end;

function TInitialize. CheckProgramSetting : Boolean;

Var
  aPath : String;

begin
  aPath:=ServerSettings.&program;
  if aPath = '' then
    exit(False);
  aPath:=ExpandFileName(aPath);
  Result:=FileExists(aPath);
  if Result then
    ServerSettings.&program := aPath
  else
    begin
      DoLog(kFailedPrefix+'Main program file '+ aPath+ ' can''t be found.');
      ServerSettings.&program := '';
    end;
end;

function TInitialize.Process(var Params : TInitializeParams): TInitializeResult;


var
  Proj, Option, aPath, ConfigPath: String;
  CodeToolsOptions: TCodeToolsOptions;
  re: TRegExpr;
  Macros: TMacroMap;
  Paths: TStringList;
  RootPath,IncludePathTemplate,UnitPathTemplate: TDefineTemplate;

begin
  Result := TInitializeResult.Create;
  CodeToolsOptions:=nil;
  Re:=nil;
  Paths:=Nil;
  Macros:=nil;
  try
    Macros := TMacroMap.Create;
    CodeToolsOptions := TCodeToolsOptions.Create;
    re := TRegExpr.Create('^(-(Fu|Fi)+)(.*)$');
    Paths:=TStringList.Create;
    Paths.StrictDelimiter:=True;
    Paths.Delimiter:=';';
    Paths.Sorted:=True;
    Paths.Duplicates:=dupIgnore;

    Result.capabilities.executeCommandProvider.commands.Clear;
    CommandFactory.GetCommandList(Result.capabilities.executeCommandProvider.commands);

    ServerSettings.Assign(Params.initializationOptions);
    PasLS.Settings.ClientInfo.Assign(Params.ClientInfo);

    // replace macros in server settings
    Macros.Add('tmpdir', GetTempDir(true));
    Macros.Add('root', URIToPath(Params.rootUri));

    ServerSettings.ReplaceMacros(Macros);

    // set the project directory based on root URI path
    if Params.rootUri <> '' then
      CodeToolsOptions.ProjectDir := URIToPath(Params.rootURI);

    // print the root URI so we know which workspace folder is default
    DoLog(kSettingprefix+'RootURI: '+Params.rootUri);
    DoLog(kSettingprefix+'ProjectDir: '+CodeToolsOptions.ProjectDir);

    {
      For more information on CodeTools see:
      https://wiki.freepascal.org/Codetools
    }

    // set some built-in defaults based on platform
    SetPlatformDefaults(CodeToolsOptions);
    ApplyConfigSettings(CodeToolsOptions);

    { Override default settings with environment variables.
      These are the required values which must be set:

      FPCDIR       = path to FPC source directory
      PP           = path of the Free Pascal compiler. For example /usr/bin/ppc386.
      LAZARUSDIR   = path of the lazarus sources
      FPCTARGET    = FPC target OS like linux, win32, darwin
      FPCTARGETCPU = FPC target cpu like i386, x86_64, arm }
    CodeToolsOptions.InitWithEnvironmentVariables;

    GuessCodeToolConfig(Transport,CodeToolsOptions);
    Proj:=Params.initializationOptions.&program;
    if (Proj<>'') and FileExists(Proj) then
      ConfigureSingleProject(Transport,Proj);

    // load the symbol manager if it's enabled
    if ServerSettings.documentSymbols or ServerSettings.workspaceSymbols then
      begin
      SymbolManager := TSymbolManager.Create;
      Result.capabilities.documentSymbolProvider:=True;
      Result.capabilities.workspaceSymbolProvider := ServerSettings.CanProvideWorkspaceSymbols;
      end;

    // attempt to load optional config file
    ConfigPath := ExpandFileName(Params.initializationOptions.CodeToolsConfig);
    if FileExists(ConfigPath) then
      begin
        DoLog('Loading config file: '+ ConfigPath);
        CodeToolsOptions.LoadFromFile(ConfigPath);
      end;
    // include workspace paths as search paths
    if ServerSettings.includeWorkspaceFoldersAsUnitPaths or
       ServerSettings.includeWorkspaceFoldersAsIncludePaths then
      begin
        CollectWorkSpacePaths(Params.workspaceFolders,Paths);
      end;
    // Add the in order specified
    Paths.Sorted:=False;
    for Option in Params.initializationOptions.FPCOptions do
      begin
        // expand file names in switches with paths
        if re.Exec(Option) then
          begin
          if Paths.IndexOf(re.Match[3])=-1 then
            Paths.Add(re.Match[3])
          end
        else
          CodeToolsOptions.FPCOptions := CodeToolsOptions.FPCOptions + Option + ' ';
      end;

    if Result.Capabilities.workspaceSymbolProvider then
      for aPath in Paths do
        SymbolManager.Scan(aPath, false);

    CheckProgramSetting;

    ShowConfigStatus(Params,CodeToolsOptions);
    DoLog(kSettingprefix+'Workspace paths (%d) : %s',[Paths.Count,Paths.DelimitedText]);

    with CodeToolBoss do
      begin
        Init(CodeToolsOptions);
        IdentifierList.SortForHistory := True;
        IdentifierList.SortForScope := True;
      end;

    // Set search path for codetools.
    RootPath:=TDefineTemplate.Create('RootPath','RootPath','',CodetoolsOptions.ProjectDir,da_Directory);
    if ServerSettings.includeWorkspaceFoldersAsUnitPaths then
      begin
      UnitPathTemplate:=TDefineTemplate.Create('RootUnitPath','RootUnitPath',UnitPathMacroName, UnitPathMacro+';'+Paths.DelimitedText, da_DefineRecurse);
      RootPath.AddChild(UnitPathTemplate);
      end;
    if ServerSettings.includeWorkspaceFoldersAsIncludePaths then
      begin
      IncludePathTemplate:=TDefineTemplate.Create('RootIncludePath','RootIncludePath',IncludePathMacroName, IncludePathMacro+';'+Paths.DelimitedText, da_DefineRecurse);
      RootPath.AddChild(IncludePathTemplate);
      end;
    CodeToolBoss.DefineTree.Add(RootPath);
  finally
    Paths.Free;
    re.Free;
    CodeToolsOptions.Free;
    Macros.Free;
  end;
end;

{ TInitialized }

procedure TInitialized.Process(var Params : TVoidParams);
begin
  // do nothing
end;

{ TShutdown }

function TShutdown.Process(var Params : TVoidParams): TLSPStreamable;
begin
  // do nothing
  result := nil;
end;

{ TExit }

procedure TExit.Process(var Params : TVoidParams);
begin
  Halt(0);
end;

{ TCancel }

procedure TCancel.Process(var Params : TCancelParams);
begin
  // not supported
end;

initialization
  LSPHandlerManager.RegisterHandler('initialize', TInitialize);
  LSPHandlerManager.RegisterHandler('initialized', TInitialized);
  LSPHandlerManager.RegisterHandler('shutdown', TShutdown);
  LSPHandlerManager.RegisterHandler('exit', TExit);
  LSPHandlerManager.RegisterHandler('$/cancelRequest', TCancel);
end.

