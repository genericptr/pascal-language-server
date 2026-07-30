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
unit PasLS.General;

{$mode objfpc}{$H+}
{$modeswitch arrayoperators}

interface
uses
  { RTL }
  Classes, URIParser, typinfo,
  { Code Tools }
  CodeToolManager, CodeToolsConfig,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Capabilities, LSP.DocumentSymbol, LSP.General,
  { Utils }
  PasLS.Settings, PasLS.Symbols, PasLS.Commands, PasLS.LazConfig;

type
  
  { TServerCapabilitiesHelper }

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
    procedure CollectWorkSpacePaths(WorkspaceFolders: TWorkspaceFolderItems; aPaths: TStrings; ExcludeFolders: TStrings);
    procedure DoLog(const Msg: String);
    procedure DoLog(const Fmt: String; const args: array of const);
    procedure DoLog(const Msg: String; aBool: Boolean);
    function IsPasExt(const aExtension : String) : Boolean;
    function IsPathExcluded(const aPath: String; ExcludeFolders: TStrings): Boolean;
    procedure SetFPCPaths(Paths, Opts: TStrings; AsUnitPath, asIncludePath: Boolean);
    procedure SetPlatformDefaults(CodeToolsOptions : TCodeToolsOptions);
    procedure ApplyConfigSettings(CodeToolsOptions: TCodeToolsOptions);
    procedure FindPascalSourceDirectories(RootPath: String; Results: TStrings; ExcludeFolders: TStrings);
    procedure ShowConfigStatus(Params : TInitializeParams; CodeToolsOptions: TCodeToolsOptions);
  public
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
  SysUtils, RegExpr, IdentCompletionTool, DefineTemplates,
  PasLS.CodeUtils;

const
  kStatusPrefix = '✓ ';
  kFailedPrefix = '⚠️ ';
  kSettingPrefix = '  ► ';
  kEmptyPrefix = '  ';

{ TInitialize }

procedure TInitialize.ApplyConfigSettings(CodeToolsOptions: TCodeToolsOptions);

  function MaybeSet(aValue, aDefault: String): String;
  begin
    Result := aValue;
    if Result = '' then
      Result := aDefault;
  end;

var
  env: TConfigEnvironmentSettings;
begin
  env := EnvironmentSettings;
  with CodeToolsOptions do
    begin
      FPCPath := MaybeSet(Env.pp, FPCPath);
      FPCSrcDir := MaybeSet(Env.fpcDir, FPCSrcDir);
      LazarusSrcDir := MaybeSet(Env.lazarusDir, LazarusSrcDir);
      TargetOS := MaybeSet(Env.fpcTarget, TargetOS);
      TargetProcessor := MaybeSet(Env.fpcTargetCPU, TargetProcessor);
    end;
end;


procedure TInitialize.SetPlatformDefaults(CodeToolsOptions: TCodeToolsOptions);
begin
  // Compile time defaults
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

function TInitialize.IsPasExt(const aExtension: String): Boolean;
var
  E : String;
begin
  E := LowerCase(aExtension);
  result := (E = '.pas') or (E = '.pp') or (E = '.inc');
end;

procedure TInitialize.DoLog(const Msg: String);
begin
  Transport.SendDiagnostic(Msg);
end;

procedure TInitialize.DoLog(const Fmt: String; const args: Array of const);
begin
  Transport.SendDiagnostic(Fmt, Args);
end;

procedure TInitialize.DoLog(const Msg: String; aBool: Boolean);
begin
  Transport.SendDiagnostic(Msg+BoolToStr(aBool, 'True', 'False'));
end;

function TInitialize.IsPathExcluded(const aPath: String; ExcludeFolders: TStrings): Boolean;
var
  ExcludePath: String;
  NormalizedPath: String;
  NormalizedExclude: String;
begin
  Result := False;
  if (ExcludeFolders = nil) or (ExcludeFolders.Count = 0) then
    exit;

  NormalizedPath := ExcludeTrailingPathDelimiter(aPath);

  for ExcludePath in ExcludeFolders do
    begin
      NormalizedExclude := ExcludeTrailingPathDelimiter(ExpandFileName(ExcludePath));
      // Check if the path starts with the excluded path
      if (Pos(NormalizedExclude, NormalizedPath) = 1) then
        begin
          Result := True;
          exit;
        end;
    end;
end;

procedure TInitialize.FindPascalSourceDirectories(RootPath: String; Results: TStrings; ExcludeFolders: TStrings);
var
  Info: TSearchRec;
  havePas: Boolean;
  SubDirPath: String;
begin
  // Skip this directory if it's excluded
  if IsPathExcluded(RootPath, ExcludeFolders) then
    exit;

  havePas:=False;
  if FindFirst(RootPath + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        if ((Info.Attr and faDirectory) <> 0) and not((Info.Name = '.') or (Info.Name = '..')) then
          begin
            SubDirPath := IncludeTrailingPathDelimiter(RootPath+Info.Name);
            // Only recurse if the subdirectory is not excluded
            if not IsPathExcluded(SubDirPath, ExcludeFolders) then
              FindPascalSourceDirectories(SubDirPath, Results, ExcludeFolders);
          end;

        if IsPasExt(ExtractFileExt(Info.Name)) then
          HavePas := true;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;

  if HavePas then
    if Results.IndexOf(RootPath) = -1 then
      Results.Add(RootPath);
end;

procedure TInitialize.CollectWorkSpacePaths(WorkspaceFolders: TWorkspaceFolderItems; aPaths: TStrings; ExcludeFolders: TStrings);
var
  Item: TCollectionItem;
begin
  for Item in workspaceFolders do
    FindPascalSourceDirectories(IncludeTrailingPathDelimiter(UriToPath(TWorkspaceFolder(Item).uri)), aPaths, ExcludeFolders);
end;

procedure TInitialize.ShowConfigStatus(Params: TInitializeParams; CodeToolsOptions: TCodeToolsOptions);
var
  ExcludeList, Option: String;
  clientInfoVersion: String;
  I: Integer;
  FPCOptions: TStringArray;
begin
  if Params.clientInfo.version.HasValue then
    clientInfoVersion := ' ' + Params.clientInfo.version.Value
  else
    clientInfoVersion := '';

  DoLog(kStatusPrefix+'Server: ' + {$INCLUDE %DATE%});
  DoLog(kStatusPrefix+'Client: ' + Params.clientInfo.name + clientInfoVersion);

  DoLog(kStatusPrefix+'FPCPath: ' + CodeToolsOptions.FPCPath);
  DoLog(kStatusPrefix+'FPCSrcDir: ' + CodeToolsOptions.FPCSrcDir);
  DoLog(kStatusPrefix+'LazarusSrcDir: ' + CodeToolsOptions.LazarusSrcDir);
  DoLog(kStatusPrefix+'TargetOS: ' + CodeToolsOptions.TargetOS);
  DoLog(kStatusPrefix+'TargetProcessor: '+ CodeToolsOptions.TargetProcessor);

  DoLog(kStatusPrefix+'Working directory: ' + GetCurrentDir);

  if CodeToolsOptions.FPCOptions <> '' then
    begin
      DoLog(kStatusPrefix+'FPCOptions:');
      FPCOptions := SplitString(CodeToolsOptions.FPCOptions, ' ');
      for Option in FPCOptions do
        DoLog('  '+Option);
    end
  else
    DoLog(kStatusPrefix+'FPCOptions: [unspecified]');

  if ServerSettings.&program <> '' then
    DoLog(kStatusPrefix+'Main program file: ' + ServerSettings.&program);

  if CodeToolsOptions.ProjectDir <> '' then
    DoLog(kStatusPrefix+'ProjectDir: ' + CodeToolsOptions.ProjectDir)
  else
    DoLog(kStatusPrefix+'ProjectDir: [unspecified]');

  {$IFDEF USE_SQLITE}
  if ServerSettings.symbolDatabase <> '' then
    DoLog(kStatusPrefix+'Symbol Database: ' + ServerSettings.symbolDatabase)
  else
    DoLog(kStatusPrefix+'Symbol Database: [unspecified]');
  {$ENDIF}

  // other settings
  DoLog(kStatusPrefix+'Settings:');
  DoLog(kSettingPrefix+'maximumCompletions: %d', [ServerSettings.maximumCompletions]);
  DoLog(kSettingPrefix+'overloadPolicy: %s', [GetEnumName(TypeInfo(TOverloadPolicy),Ord(ServerSettings.overloadPolicy))]);
  DoLog(kSettingPrefix+'insertCompletionsAsSnippets: ', ServerSettings.insertCompletionsAsSnippets);
  DoLog(kSettingPrefix+'insertCompletionProcedureBrackets: ', ServerSettings.insertCompletionProcedureBrackets);
  DoLog(kSettingPrefix+'includeWorkspaceFoldersAsUnitPaths: ', ServerSettings.includeWorkspaceFoldersAsUnitPaths);
  DoLog(kSettingPrefix+'includeWorkspaceFoldersAsIncludePaths: ', ServerSettings.includeWorkspaceFoldersAsIncludePaths);
  DoLog(kSettingPrefix+'checkSyntax: ', ServerSettings.checkSyntax);
  DoLog(kSettingPrefix+'publishDiagnostics: ', ServerSettings.publishDiagnostics);
  DoLog(kSettingPrefix+'workspaceSymbols: ', ServerSettings.workspaceSymbols);
  DoLog(kSettingPrefix+'documentSymbols: ', ServerSettings.documentSymbols);
  DoLog(kSettingPrefix+'minimalisticCompletions: ', ServerSettings.minimalisticCompletions);
  DoLog(kSettingPrefix+'showSyntaxErrors: ', ServerSettings.showSyntaxErrors);
  DoLog(kSettingPrefix+'flatSymbolMode: ', ServerSettings.flatSymbolMode);

  // Show excludeSymbols
  if ServerSettings.excludeSymbols.Count > 0 then
    begin
      ExcludeList := '';
      for I := 0 to ServerSettings.excludeSymbols.Count - 1 do
        begin
          if ExcludeList <> '' then
            ExcludeList := ExcludeList + ', ';
          ExcludeList := ExcludeList + ServerSettings.excludeSymbols[I];
        end;
      DoLog(kSettingPrefix+'excludeSymbols: [' + ExcludeList + ']');
    end;
end;

procedure TInitialize.SetFPCPaths(Paths,Opts: TStrings; AsUnitPath, asIncludePath: Boolean);
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

function TInitialize.CheckProgramSetting: Boolean;
Var
  aPath: String;
begin
  aPath := ServerSettings.&program;
  if aPath = '' then
    exit(False);
  aPath := ExpandFileName(aPath);
  Result := FileExists(aPath);
  if Result then
    ServerSettings.&program := aPath
  else
    begin
      DoLog(kFailedPrefix+'Main program file '+ aPath + ' can''t be found.');
      ServerSettings.&program := '';
    end;
end;

function TInitialize.Process(var Params : TLSPInitializeParams): TInitializeResult;

  function EscapeFileName(const Name: String): String;
  begin
    if Pos(' ', Name) > 0 then
      Result := '"' + Name + '"'
    else
      Result := Name;
  end;

var
  Proj, Option, aPath, ConfigPath: String;
  CodeToolsOptions: TCodeToolsOptions;
  PathSwitchRegex: TRegExpr;
  Macros: TMacroMap;
  WorkspacePaths: TStringList;
  RootPath, IncludePathTemplate, UnitPathTemplate: TDefineTemplate;
  Opt: TServerSettings;
  FPCOptions: TStringArray;
begin
  if Params.initializationOptions is TServerSettings then
    Opt := TServerSettings(Params.initializationOptions)
  else
    Opt := nil;

  Result := TInitializeResult.Create;
  CodeToolsOptions := nil;
  PathSwitchRegex := nil;
  WorkspacePaths := nil;
  Macros := nil;
  FPCOptions := [];

  try
    Macros := TMacroMap.Create;
    CodeToolsOptions := TCodeToolsOptions.Create;
    PathSwitchRegex := TRegExpr.Create('^(-(Fu|Fi)+)(.*)$');
    
    WorkspacePaths := TStringList.Create;
    WorkspacePaths.StrictDelimiter := true;
    WorkspacePaths.Delimiter := ';';
    WorkspacePaths.Sorted := false;
    WorkspacePaths.Duplicates := dupIgnore;

    Result.capabilities.executeCommandProvider.commands.Clear;
    CommandFactory.GetCommandList(Result.capabilities.executeCommandProvider.commands);

    ServerSettings.Assign(Params.initializationOptions);
    PasLS.Settings.ClientInfo.Assign(Params.ClientInfo);

    // Detect hierarchical document symbol support
    if Assigned(Params.capabilities) and
       Assigned(Params.capabilities.textDocument) and
       Assigned(Params.capabilities.textDocument.documentSymbol) then
      SetClientCapabilities(Params.capabilities.textDocument.documentSymbol.hierarchicalDocumentSymbolSupport)
    else
      SetClientCapabilities(false);

    // replace macros in server settings
    Macros.Add('tmpdir', GetTempDir(true));
    Macros.Add('root', URIToPath(Params.rootUri));

    ServerSettings.ReplaceMacros(Macros);

    // set the project directory based on root URI path
    if Params.rootUri <> '' then
      CodeToolsOptions.ProjectDir := URIToPath(Params.rootURI);

    // print the root URI so we know which workspace folder is default
    DoLog(kStatusPrefix+'RootURI: '+Params.rootUri);
    DoLog(kStatusPrefix+'ProjectDir: '+CodeToolsOptions.ProjectDir);

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

    GuessCodeToolConfig(Transport, CodeToolsOptions);
    if Assigned(Opt) then
      Proj := Opt.&program;
    if (Proj <> '') and FileExists(Proj) then
      ConfigureSingleProject(Transport, Proj);

    // load the symbol manager if it's enabled
    if ServerSettings.documentSymbols or ServerSettings.workspaceSymbols then
      begin
        SymbolManager := TSymbolManager.Create;
        SymbolManager.Transport := Transport;
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

    CollectWorkSpacePaths(Params.workspaceFolders, WorkspacePaths, ServerSettings.excludeWorkspaceFolders);

    // Add the paths in order specified
    if Assigned(Opt) then
      for Option in Opt.FPCOptions do
        begin
          // Add paths from path switches to workspace paths
          if PathSwitchRegex.Exec(Option) then
            begin
              if WorkspacePaths.IndexOf(PathSwitchRegex.Match[3]) = -1 then
                WorkspacePaths.Add(PathSwitchRegex.Match[3]);
            end;
          FPCOptions += [Option];
        end;

    // include workspace paths as search paths
    for aPath in WorkspacePaths do
      begin
        if ServerSettings.includeWorkspaceFoldersAsUnitPaths then
          FPCOptions += ['-Fu' + EscapeFileName(ExpandFileName(aPath))];

        if ServerSettings.includeWorkspaceFoldersAsIncludePaths then
          FPCOptions += ['-Fi' + EscapeFileName(ExpandFileName(aPath))];
      end;

    CodeToolsOptions.FPCOptions := JoinString(FPCOptions, ' ');

    // Scan workspace for symbols
    if Result.Capabilities.workspaceSymbolProvider then
      begin
        SymbolManager.WorkspacePaths.Clear;
        for aPath in WorkspacePaths do
          begin
            SymbolManager.WorkspacePaths.Add(SymbolManager.NormalizePath(aPath));
            SymbolManager.Scan(aPath, false);
          end;
      end;

    CheckProgramSetting;

    ShowConfigStatus(Params, CodeToolsOptions);

    with CodeToolBoss do
      begin
        Init(CodeToolsOptions);
        IdentifierList.SortForHistory := True;
        IdentifierList.SortMethodForCompletion := icsScopedAlphabetic;
      end;

    Result.Capabilities.ApplySettings(ServerSettings);

    // Set search path for codetools.
    RootPath := TDefineTemplate.Create('RootPath', 'RootPath', '', CodetoolsOptions.ProjectDir, da_Directory);
    if ServerSettings.includeWorkspaceFoldersAsUnitPaths then
      begin
        UnitPathTemplate := TDefineTemplate.Create('RootUnitPath', 'RootUnitPath', UnitPathMacroName, UnitPathMacro+';'+WorkspacePaths.DelimitedText, da_DefineRecurse);
        RootPath.AddChild(UnitPathTemplate);
      end;
    if ServerSettings.includeWorkspaceFoldersAsIncludePaths then
      begin
        IncludePathTemplate := TDefineTemplate.Create('RootIncludePath', 'RootIncludePath', IncludePathMacroName, IncludePathMacro+';'+WorkspacePaths.DelimitedText, da_DefineRecurse);
        RootPath.AddChild(IncludePathTemplate);
      end;
    CodeToolBoss.DefineTree.Add(RootPath);
  finally
    WorkspacePaths.Free;
    PathSwitchRegex.Free;
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

  // NOTE: inlay hints are disabled because they were performing poorly
  // and I felt like they were unsable in real use.
  // finlayHintProvider:= TInlayHintOptions.Create;

  documentSymbolProvider := Assigned(SymbolManager);

  completionProvider.triggerCharacters.Add('.');
  completionProvider.triggerCharacters.Add('^');

  signatureHelpProvider.triggerCharacters.Add('(');
  signatureHelpProvider.triggerCharacters.Add(')');
  signatureHelpProvider.triggerCharacters.Add(',');

  renameProvider.prepareProvider := true;
end;

{ TLSPInitializeParams }

function TLSPInitializeParams.createInitializationOptions: TInitializationOptions;
begin
  result := TServerSettings.Create;
end;

end.

