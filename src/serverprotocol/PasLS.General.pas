unit PasLS.General;

{$mode objfpc}{$H+}
{$modeswitch arrayoperators}

interface

uses
  {$ifdef FreePascalMake}
  FPMConfig, FPMUtils,
  {$endif}
  { RTL }
  Classes, URIParser, typinfo,
  { Code Tools }
  CodeToolManager, CodeToolsConfig,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Capabilities, LSP.DocumentSymbol, LSP.General,
  { Utils }
  PasLS.Settings, PasLS.Symbols, PasLS.Commands, PasLS.LazConfig;


Type

  TServerCapabilitiesHelper = class helper for TServerCapabilities
    procedure ApplySettings(settings: TServerSettings);
  end;


  { TLSPInitializeParams }

  TLSPInitializeParams = Class(TInitializeParams)
  Protected
    Function createInitializationOptions: TInitializationOptions; override;
  end;
  { TInitialize }

  TInitialize = class(specialize TLSPRequest<TLSPInitializeParams, TInitializeResult>)
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
    function Process(var Params : TLSPInitializeParams): TInitializeResult; override;
  end;

  { TInitialized }

  TInitialized = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
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
  SysUtils, RegExpr, IdentCompletionTool, DefineTemplates;


const
  kStatusPrefix = '✓ ';
  kFailedPrefix = '⚠️ ';
  kSettingPrefix = '  ► ';

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

function TInitialize.CheckProgramSetting : Boolean;

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

function TInitialize.Process(var Params : TLSPInitializeParams): TInitializeResult;


var
  Proj, Option, aPath, ConfigPath: String;
  CodeToolsOptions: TCodeToolsOptions;
  re: TRegExpr;
  Macros: TMacroMap;
  Paths: TStringList;
  RootPath,IncludePathTemplate,UnitPathTemplate: TDefineTemplate;
  opt : TServerSettings;

begin
  if Params.initializationOptions is TServerSettings then
    Opt:=TServerSettings(Params.initializationOptions)
  else
    Opt:=Nil;
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
    if Assigned(Opt) then
      Proj:=Opt.&program;
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
    if Assigned(Opt) then
      ConfigPath := ExpandFileName(Opt.CodeToolsConfig);
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
    if Assigned(Opt) then
      for Option in Opt.FPCOptions do
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
        IdentifierList.SortMethodForCompletion:=icsScopedAlphabetic;
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



procedure TServerCapabilitiesHelper.ApplySettings(settings: TServerSettings);
begin
  if not Assigned(Settings) then
    exit;
  workspaceSymbolProvider := settings.CanProvideWorkspaceSymbols;
  workspace.workspaceFolders.supported := true;
  workspace.workspaceFolders.changeNotifications := true;

  hoverProvider := true;
  declarationProvider := true;
  definitionProvider := true;
  implementationProvider := true;
  referencesProvider := true;
  documentHighlightProvider := true;
  // finlayHintProvider:= TInlayHintOptions.Create;

  documentSymbolProvider := Assigned(SymbolManager);

  completionProvider.triggerCharacters.Add('.');
  completionProvider.triggerCharacters.Add('^');

  signatureHelpProvider.triggerCharacters.Add('(');
  signatureHelpProvider.triggerCharacters.Add(')');
  signatureHelpProvider.triggerCharacters.Add(',');

end;

{ TLSPInitializeParams }

function TLSPInitializeParams.createInitializationOptions: TInitializationOptions;
begin
  Result:=TServerSettings.Create;
end;


end.

