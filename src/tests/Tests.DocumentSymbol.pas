unit Tests.DocumentSymbol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CodeToolManager, CodeCache,
  PasLS.Symbols;

type

  { TTestDocumentSymbol }

  TTestDocumentSymbol = class(TTestCase)
  private
    FTestCode: TCodeBuffer;
    FTestFile: String;
    procedure CreateTestFile(const AContent: String);
    procedure CleanupTestFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSymbolExtractionHierarchical;
    procedure TestSymbolExtractionFlat;
  end;

implementation

const
  TEST_UNIT_WITH_PROPERTY_AND_FIELD =
    'unit TestUnit;' + LineEnding +
    '' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    '' + LineEnding +
    'interface' + LineEnding +
    '' + LineEnding +
    'type' + LineEnding +
    '  TUser = class' + LineEnding +
    '  private' + LineEnding +
    '    FName: String;' + LineEnding +
    '    FAge: Integer;' + LineEnding +
    '  public' + LineEnding +
    '    property Name: String read FName write FName;' + LineEnding +
    '    property Age: Integer read FAge write FAge;' + LineEnding +
    '    procedure PrintInfo;' + LineEnding +
    '    function GetFullName: String;' + LineEnding +
    '  end;' + LineEnding +
    '' + LineEnding +
    'implementation' + LineEnding +
    '' + LineEnding +
    'procedure TUser.PrintInfo;' + LineEnding +
    'begin' + LineEnding +
    '  writeln(FName);' + LineEnding +
    'end;' + LineEnding +
    '' + LineEnding +
    'function TUser.GetFullName: String;' + LineEnding +
    'begin' + LineEnding +
    '  Result := FName;' + LineEnding +
    'end;' + LineEnding +
    '' + LineEnding +
    'end.';

{ TTestDocumentSymbol }

procedure TTestDocumentSymbol.CreateTestFile(const AContent: String);
var
  F: TextFile;
begin
  FTestFile := GetTempFileName('', 'testunit');
  FTestFile := ChangeFileExt(FTestFile, '.pas');

  AssignFile(F, FTestFile);
  try
    Rewrite(F);
    Write(F, AContent);
  finally
    CloseFile(F);
  end;
end;

procedure TTestDocumentSymbol.CleanupTestFile;
begin
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
  FTestFile := '';
end;

procedure TTestDocumentSymbol.SetUp;
begin
  inherited SetUp;
  FTestCode := nil;
  FTestFile := '';

  // Ensure SymbolManager is initialized
  if SymbolManager = nil then
    SymbolManager := TSymbolManager.Create;

  // Set hierarchical mode for tests
  SetClientCapabilities(True);
end;

procedure TTestDocumentSymbol.TearDown;
begin
  CleanupTestFile;
  FTestCode := nil;
  inherited TearDown;
end;

procedure TTestDocumentSymbol.TestSymbolExtractionHierarchical;
var
  RawJSON: String;
begin
  // Ensure hierarchical mode
  SetClientCapabilities(True);

  // Create test file
  CreateTestFile(TEST_UNIT_WITH_PROPERTY_AND_FIELD);

  // Load code buffer
  FTestCode := CodeToolBoss.LoadFile(FTestFile, True, False);
  AssertNotNull('Code buffer should be loaded', FTestCode);

  // Use SymbolManager to reload and extract symbols (public API)
  SymbolManager.Reload(FTestCode, True);

  // Get the raw JSON from SymbolManager
  RawJSON := SymbolManager.FindDocumentSymbols(FTestFile).AsJSON;

  // Verify we extracted symbols
  AssertTrue('Should have extracted symbols', RawJSON <> '');

  // Verify the extracted symbols contain our expected names
  AssertTrue('JSON should contain TUser class', Pos('"TUser"', RawJSON) > 0);
  AssertTrue('JSON should contain FName field', Pos('FName', RawJSON) > 0);
  AssertTrue('JSON should contain FAge field', Pos('FAge', RawJSON) > 0);
  AssertTrue('JSON should contain Name property', Pos('"Name"', RawJSON) > 0);
  AssertTrue('JSON should contain Age property', Pos('"Age"', RawJSON) > 0);
  AssertTrue('JSON should contain PrintInfo method', Pos('PrintInfo', RawJSON) > 0);
  AssertTrue('JSON should contain GetFullName method', Pos('GetFullName', RawJSON) > 0);

  // Check for hierarchical structure (children array)
  AssertTrue('Should have children in hierarchical mode', Pos('"children"', RawJSON) > 0);

  // Check for correct symbol kinds (note: JSON has spaces around colons)
  AssertTrue('Should have Class kind (5)', Pos('"kind" : 5', RawJSON) > 0);
  AssertTrue('Should have Field kind (8)', Pos('"kind" : 8', RawJSON) > 0);
  AssertTrue('Should have Property kind (7)', Pos('"kind" : 7', RawJSON) > 0);
  AssertTrue('Should have Function/Method kind (12)', Pos('"kind" : 12', RawJSON) > 0);
end;

procedure TTestDocumentSymbol.TestSymbolExtractionFlat;
var
  RawJSON: String;
begin
  // Ensure flat mode
  SetClientCapabilities(False);

  // Create test file
  CreateTestFile(TEST_UNIT_WITH_PROPERTY_AND_FIELD);

  // Load code buffer
  FTestCode := CodeToolBoss.LoadFile(FTestFile, True, False);
  AssertNotNull('Code buffer should be loaded', FTestCode);

  // Use SymbolManager to reload and extract symbols (public API)
  SymbolManager.Reload(FTestCode, True);

  // Get the raw JSON from SymbolManager
  RawJSON := SymbolManager.FindDocumentSymbols(FTestFile).AsJSON;

  // Verify we extracted symbols
  AssertTrue('Should have extracted symbols', RawJSON <> '');

  // Verify the extracted symbols contain our expected names
  AssertTrue('JSON should contain TUser class', Pos('"TUser"', RawJSON) > 0);
  AssertTrue('JSON should contain FName field', Pos('FName', RawJSON) > 0);
  AssertTrue('JSON should contain FAge field', Pos('FAge', RawJSON) > 0);
  AssertTrue('JSON should contain Name property', Pos('"Name"', RawJSON) > 0);
  AssertTrue('JSON should contain Age property', Pos('"Age"', RawJSON) > 0);
  AssertTrue('JSON should contain PrintInfo method', Pos('PrintInfo', RawJSON) > 0);
  AssertTrue('JSON should contain GetFullName method', Pos('GetFullName', RawJSON) > 0);

  // In flat mode, should NOT have children array
  AssertTrue('Should NOT have children in flat mode', Pos('"children"', RawJSON) = 0);

  // In flat mode, should have containerName for properties and fields
  AssertTrue('Should have containerName in flat mode', Pos('"containerName"', RawJSON) > 0);

  // Check for correct symbol kinds (note: JSON has spaces around colons)
  AssertTrue('Should have Class kind (5)', Pos('"kind" : 5', RawJSON) > 0);
  AssertTrue('Should have Field kind (8)', Pos('"kind" : 8', RawJSON) > 0);
  AssertTrue('Should have Property kind (7)', Pos('"kind" : 7', RawJSON) > 0);
  AssertTrue('Should have Function/Method kind (12)', Pos('"kind" : 12', RawJSON) > 0);
end;

initialization
  RegisterTest(TTestDocumentSymbol);
end.
