{ Diagnostic tests for crash localization when USE_SQLITE is not defined.
  Run this test suite on MacOS to help identify where the Access Violation occurs.

  Usage:
    testlsp.exe --suite=TDiagnosticTests

  Each test outputs detailed diagnostic information to help pinpoint the crash location.
}
unit Tests.Diagnostic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  fpjson, fpjsonrtti,
  LSP.Streaming, LSP.Base, LSP.BaseTypes, LSP.Basic, LSP.General,
  PasLS.Settings, PasLS.Symbols;

type

  { TDiagnosticTests }

  TDiagnosticTests = class(TTestCase)
  private
    procedure Log(const Msg: String);
    function MakeTempDir(const Prefix: String): String;
    procedure WriteText(const FileName, Text: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Test 1: Basic TServerSettings creation
    procedure Test01_CreateServerSettings;

    // Test 2: JSON deserialization with known properties only
    procedure Test02_DeserializeKnownProperties;

    // Test 3: JSON deserialization with unknown properties (like symbolDatabase)
    procedure Test03_DeserializeUnknownProperties;

    // Test 4: TServerSettings.Assign method
    procedure Test04_ServerSettingsAssign;

    // Test 5: TServerSettings.ReplaceMacros method
    procedure Test05_ServerSettingsReplaceMacros;

    // Test 6: Create TSymbolManager
    procedure Test06_CreateSymbolManager;

    // Test 7: TSymbolManager with Transport set
    procedure Test07_SymbolManagerWithTransport;

    // Test 8: Access ServerSettings properties
    procedure Test08_AccessServerSettingsProperties;

    // Test 9: Full initialization simulation
    procedure Test09_SimulateInitialization;

    // Test 10: Project .pasls.cfg loading and relative path handling
    procedure Test10_ProjectConfigLoadsRelativePaths;

    // Test 11: Project main program persistence writes only project config
    procedure Test11_SaveProjectMainProgram;
  end;

implementation

uses
  TypInfo;

{ TDiagnosticTests }

procedure TDiagnosticTests.Log(const Msg: String);
begin
  WriteLn('[DIAG] ' + Msg);
end;

function TDiagnosticTests.MakeTempDir(const Prefix: String): String;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + Prefix + '-' +
    IntToStr(Random(1000000)) + DirectorySeparator;
  ForceDirectories(Result);
end;

procedure TDiagnosticTests.WriteText(const FileName, Text: String);
var
  S: TStringList;
begin
  ForceDirectories(ExtractFilePath(FileName));
  S := TStringList.Create;
  try
    S.Text := Text;
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

procedure TDiagnosticTests.SetUp;
begin
  Log('=== SetUp ===');
end;

procedure TDiagnosticTests.TearDown;
begin
  Log('=== TearDown ===');
end;

procedure TDiagnosticTests.Test01_CreateServerSettings;
var
  Settings: TServerSettings;
begin
  Log('Test01: Creating TServerSettings...');
  Settings := TServerSettings.Create;
  try
    Log('Test01: TServerSettings created successfully');
    Log('Test01: Checking default values...');
    Log('Test01: maximumCompletions = ' + IntToStr(Settings.maximumCompletions));
    Log('Test01: documentSymbols = ' + BoolToStr(Settings.documentSymbols, true));
    Log('Test01: workspaceSymbols = ' + BoolToStr(Settings.workspaceSymbols, true));
    AssertNotNull('Settings should not be nil', Settings);
    Log('Test01: PASSED');
  finally
    Settings.Free;
  end;
end;

procedure TDiagnosticTests.Test02_DeserializeKnownProperties;
var
  Settings: TServerSettings;
  JSON: TJSONObject;
  DeStreamer: TJSONDeStreamer;
begin
  Log('Test02: Deserializing known properties...');
  Settings := TServerSettings.Create;
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.Options := DeStreamer.Options + [jdoIgnorePropertyErrors, jdoIgnoreNulls];

    Log('Test02: Creating JSON with known properties...');
    JSON := TJSONObject.Create;
    try
      JSON.Add('maximumCompletions', 100);
      JSON.Add('documentSymbols', true);
      JSON.Add('workspaceSymbols', true);
      JSON.Add('program', '/path/to/program.lpr');

      Log('Test02: JSON = ' + JSON.AsJSON);
      Log('Test02: Calling JSONToObject...');
      DeStreamer.JSONToObject(JSON, Settings);
      Log('Test02: JSONToObject completed');

      Log('Test02: Checking deserialized values...');
      AssertEquals('maximumCompletions', 100, Settings.maximumCompletions);
      AssertTrue('documentSymbols', Settings.documentSymbols);
      AssertTrue('workspaceSymbols', Settings.workspaceSymbols);
      Log('Test02: PASSED');
    finally
      JSON.Free;
    end;
  finally
    DeStreamer.Free;
    Settings.Free;
  end;
end;

procedure TDiagnosticTests.Test03_DeserializeUnknownProperties;
var
  Settings: TServerSettings;
  JSON: TJSONObject;
  DeStreamer: TJSONDeStreamer;
begin
  Log('Test03: Deserializing with UNKNOWN properties (symbolDatabase)...');
  Log('Test03: This test simulates what happens when config contains symbolDatabase');
  Log('Test03: but USE_SQLITE is not defined');

  Settings := TServerSettings.Create;
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.Options := DeStreamer.Options + [jdoIgnorePropertyErrors, jdoIgnoreNulls];

    Log('Test03: Creating JSON with unknown property symbolDatabase...');
    JSON := TJSONObject.Create;
    try
      JSON.Add('maximumCompletions', 100);
      JSON.Add('documentSymbols', true);
      // This property does NOT exist when USE_SQLITE is not defined
      JSON.Add('symbolDatabase', '/tmp/symbols.db');
      // Add another unknown property for comparison
      JSON.Add('nonExistentProperty', 'test');

      Log('Test03: JSON = ' + JSON.AsJSON);
      Log('Test03: Calling JSONToObject with unknown properties...');
      Log('Test03: jdoIgnorePropertyErrors is set, so this should NOT crash...');
      DeStreamer.JSONToObject(JSON, Settings);
      Log('Test03: JSONToObject completed - no crash!');

      AssertEquals('maximumCompletions should be set', 100, Settings.maximumCompletions);
      Log('Test03: PASSED');
    finally
      JSON.Free;
    end;
  finally
    DeStreamer.Free;
    Settings.Free;
  end;
end;

procedure TDiagnosticTests.Test04_ServerSettingsAssign;
var
  Source, Target: TServerSettings;
begin
  Log('Test04: Testing TServerSettings.Assign...');
  Source := TServerSettings.Create;
  Target := TServerSettings.Create;
  try
    Log('Test04: Setting up source settings...');
    Source.maximumCompletions := 50;
    Source.documentSymbols := true;
    Source.workspaceSymbols := true;
    Source.&program := '/test/path';

    Log('Test04: Calling Target.Assign(Source)...');
    Target.Assign(Source);
    Log('Test04: Assign completed');

    Log('Test04: Checking assigned values...');
    AssertEquals('maximumCompletions', 50, Target.maximumCompletions);
    AssertTrue('documentSymbols', Target.documentSymbols);
    Log('Test04: PASSED');
  finally
    Source.Free;
    Target.Free;
  end;
end;

procedure TDiagnosticTests.Test05_ServerSettingsReplaceMacros;
var
  Settings: TServerSettings;
  Macros: TMacroMap;
begin
  Log('Test05: Testing TServerSettings.ReplaceMacros...');
  Settings := TServerSettings.Create;
  Macros := TMacroMap.Create;
  try
    Log('Test05: Setting up macros...');
    Macros.Add('tmpdir', '/tmp');
    Macros.Add('root', '/workspace');

    Log('Test05: Setting up settings with macro placeholders...');
    Settings.&program := '${root}/project.lpr';

    Log('Test05: Calling ReplaceMacros...');
    Settings.ReplaceMacros(Macros);
    Log('Test05: ReplaceMacros completed');

    Log('Test05: program = ' + Settings.&program);
    Log('Test05: PASSED');
  finally
    Macros.Free;
    Settings.Free;
  end;
end;

procedure TDiagnosticTests.Test06_CreateSymbolManager;
var
  Manager: TSymbolManager;
begin
  Log('Test06: Creating TSymbolManager...');
  Log('Test06: This tests TSymbolManager.Create without Transport');

  Manager := TSymbolManager.Create;
  try
    Log('Test06: TSymbolManager created successfully');
    AssertNotNull('Manager should not be nil', Manager);
    AssertNotNull('WorkspacePaths should not be nil', Manager.WorkspacePaths);
    Log('Test06: PASSED');
  finally
    Manager.Free;
  end;
end;

procedure TDiagnosticTests.Test07_SymbolManagerWithTransport;
var
  Manager: TSymbolManager;
begin
  Log('Test07: Testing TSymbolManager with Transport = nil...');
  Log('Test07: This tests setting Transport property');

  Manager := TSymbolManager.Create;
  try
    Log('Test07: Setting Transport to nil...');
    Manager.Transport := nil;
    Log('Test07: Transport set to nil - no crash');

    Log('Test07: PASSED');
  finally
    Manager.Free;
  end;
end;

procedure TDiagnosticTests.Test08_AccessServerSettingsProperties;
var
  Settings: TServerSettings;
  BoolVal: Boolean;
  IntVal: Integer;
  StrVal: String;
begin
  Log('Test08: Accessing all ServerSettings properties...');
  Settings := TServerSettings.Create;
  try
    Log('Test08: Reading boolean properties...');
    BoolVal := Settings.documentSymbols;
    Log('Test08: documentSymbols = ' + BoolToStr(BoolVal, true));
    BoolVal := Settings.workspaceSymbols;
    Log('Test08: workspaceSymbols = ' + BoolToStr(BoolVal, true));
    BoolVal := Settings.checkSyntax;
    Log('Test08: checkSyntax = ' + BoolToStr(BoolVal, true));
    BoolVal := Settings.publishDiagnostics;
    Log('Test08: publishDiagnostics = ' + BoolToStr(BoolVal, true));
    BoolVal := Settings.insertCompletionsAsSnippets;
    Log('Test08: insertCompletionsAsSnippets = ' + BoolToStr(BoolVal, true));
    BoolVal := Settings.flatSymbolMode;
    Log('Test08: flatSymbolMode = ' + BoolToStr(BoolVal, true));

    Log('Test08: Reading integer properties...');
    IntVal := Settings.maximumCompletions;
    Log('Test08: maximumCompletions = ' + IntToStr(IntVal));

    Log('Test08: Reading string properties...');
    StrVal := Settings.&program;
    Log('Test08: program = "' + StrVal + '"');
    StrVal := Settings.codeToolsConfig;
    Log('Test08: codeToolsConfig = "' + StrVal + '"');
    StrVal := Settings.config;
    Log('Test08: config = "' + StrVal + '"');

    Log('Test08: Reading TStrings properties...');
    Log('Test08: fpcOptions.Count = ' + IntToStr(Settings.fpcOptions.Count));
    Log('Test08: excludeWorkspaceFolders.Count = ' + IntToStr(Settings.excludeWorkspaceFolders.Count));
    Log('Test08: scanFilePatterns.Count = ' + IntToStr(Settings.scanFilePatterns.Count));
    Log('Test08: excludeSymbols.Count = ' + IntToStr(Settings.excludeSymbols.Count));

    Log('Test08: Calling CanProvideWorkspaceSymbols...');
    BoolVal := Settings.CanProvideWorkspaceSymbols;
    Log('Test08: CanProvideWorkspaceSymbols = ' + BoolToStr(BoolVal, true));

    Log('Test08: PASSED');
  finally
    Settings.Free;
  end;
end;

procedure TDiagnosticTests.Test09_SimulateInitialization;
var
  Settings: TServerSettings;
  Manager: TSymbolManager;
  Macros: TMacroMap;
  JSON: TJSONObject;
  DeStreamer: TJSONDeStreamer;
begin
  Log('Test09: Simulating full initialization sequence...');
  Log('Test09: This replicates TInitialize.Process flow');

  Settings := TServerSettings.Create;
  DeStreamer := TJSONDeStreamer.Create(nil);
  Macros := TMacroMap.Create;
  Manager := nil;
  try
    DeStreamer.Options := DeStreamer.Options + [jdoIgnorePropertyErrors, jdoIgnoreNulls];

    // Step 1: Deserialize JSON (like initializationOptions)
    Log('Test09: Step 1 - Deserializing initializationOptions...');
    JSON := TJSONObject.Create;
    try
      JSON.Add('maximumCompletions', 100);
      JSON.Add('documentSymbols', true);
      JSON.Add('workspaceSymbols', true);
      JSON.Add('symbolDatabase', '/tmp/test.db'); // Unknown property when USE_SQLITE not defined

      DeStreamer.JSONToObject(JSON, Settings);
      Log('Test09: Step 1 - DONE');
    finally
      JSON.Free;
    end;

    // Step 2: Replace macros
    Log('Test09: Step 2 - ReplaceMacros...');
    Macros.Add('tmpdir', '/tmp');
    Macros.Add('root', '/workspace');
    Settings.ReplaceMacros(Macros);
    Log('Test09: Step 2 - DONE');

    // Step 3: Check symbol settings
    Log('Test09: Step 3 - Checking symbol settings...');
    Log('Test09:   documentSymbols = ' + BoolToStr(Settings.documentSymbols, true));
    Log('Test09:   workspaceSymbols = ' + BoolToStr(Settings.workspaceSymbols, true));
    Log('Test09: Step 3 - DONE');

    // Step 4: Create TSymbolManager if enabled
    if Settings.documentSymbols or Settings.workspaceSymbols then
      begin
        Log('Test09: Step 4 - Creating TSymbolManager...');
        Manager := TSymbolManager.Create;
        Log('Test09: Step 4a - TSymbolManager created');

        Log('Test09: Step 4b - Setting Transport to nil...');
        Manager.Transport := nil;
        Log('Test09: Step 4b - Transport set');

        Log('Test09: Step 4c - Calling CanProvideWorkspaceSymbols...');
        Log('Test09:   Result = ' + BoolToStr(Settings.CanProvideWorkspaceSymbols, true));
        Log('Test09: Step 4 - DONE');
      end;

    Log('Test09: PASSED - Full initialization simulation completed without crash');
  finally
    Manager.Free;
    Macros.Free;
    DeStreamer.Free;
    Settings.Free;
  end;
end;

procedure TDiagnosticTests.Test10_ProjectConfigLoadsRelativePaths;
var
  Root, ConfigFile: String;
  Config: TPasLSFileConfig;
  Settings: TServerSettings;
  Env: TConfigEnvironmentSettings;
begin
  Root := MakeTempDir('pasls-config');
  ConfigFile := IncludeTrailingPathDelimiter(Root) + kPasLSProjectConfigFile;
  WriteText(ConfigFile,
    '[Project]' + LineEnding +
    'MainProgram=src/app.lpr' + LineEnding +
    LineEnding +
    '[PasLS]' + LineEnding +
    'CodeToolsConfig=codetools.config' + LineEnding +
    'LazarusConfig=.lazarus' + LineEnding +
    LineEnding +
    '[CodeTools]' + LineEnding +
    'Compiler=bin/fpc' + LineEnding +
    'FPCDir=fpcsrc' + LineEnding +
    'LazarusDir=lazarus' + LineEnding +
    'TargetOS=linux' + LineEnding +
    'TargetCPU=x86_64' + LineEnding +
    'FPCOptions=-Fuunits -Fiinclude -dDEBUG' + LineEnding);

  Config := TPasLSFileConfig.Create;
  Settings := TServerSettings.Create;
  Env := TConfigEnvironmentSettings.Create;
  try
    Config.LoadFromFile(ConfigFile, Root);
    Config.ApplyToServerSettings(Settings, True);
    Config.ApplyToEnvironment(Env, False);

    AssertEquals('program path', ExpandFileName(Root + 'src/app.lpr'), Settings.&program);
    AssertEquals('codetools config', ExpandFileName(Root + 'codetools.config'), Settings.codeToolsConfig);
    AssertEquals('lazarus config', ExpandFileName(Root + '.lazarus'), Settings.config);
    AssertEquals('fpc options count', 3, Settings.fpcOptions.Count);
    AssertEquals('unit path option', '-Fu' + ExpandFileName(Root + 'units'), Settings.fpcOptions[0]);
    AssertEquals('include path option', '-Fi' + ExpandFileName(Root + 'include'), Settings.fpcOptions[1]);
    AssertEquals('define option', '-dDEBUG', Settings.fpcOptions[2]);

    AssertEquals('compiler', ExpandFileName(Root + 'bin/fpc'), Env.pp);
    AssertEquals('fpc dir', ExpandFileName(Root + 'fpcsrc'), Env.fpcDir);
    AssertEquals('lazarus dir', ExpandFileName(Root + 'lazarus'), Env.lazarusDir);
    AssertEquals('target os', 'linux', Env.fpcTarget);
    AssertEquals('target cpu', 'x86_64', Env.fpcTargetCPU);
  finally
    Env.Free;
    Settings.Free;
    Config.Free;
  end;
end;

procedure TDiagnosticTests.Test11_SaveProjectMainProgram;
var
  Root, ConfigFile, MainFile: String;
  Config: TPasLSFileConfig;
begin
  Root := MakeTempDir('pasls-save');
  ConfigFile := IncludeTrailingPathDelimiter(Root) + kPasLSProjectConfigFile;
  MainFile := IncludeTrailingPathDelimiter(Root) + 'src' + DirectorySeparator + 'app.lpr';
  WriteText(MainFile, 'program app; begin end.' + LineEnding);
  if FileExists(ConfigFile) then
    DeleteFile(ConfigFile);

  SetProjectConfigContext(Root, ConfigFile);
  AssertFalse('config file should not exist before save', FileExists(ConfigFile));
  AssertTrue('save should create project config', SaveProjectMainProgram(MainFile));
  AssertTrue('config file should exist after save', FileExists(ConfigFile));

  Config := TPasLSFileConfig.Create;
  try
    Config.LoadFromFile(ConfigFile, Root);
    AssertEquals('saved main program', ExpandFileName(MainFile), Config.MainProgram);
  finally
    Config.Free;
  end;
end;

initialization
  RegisterTest(TDiagnosticTests);

end.
