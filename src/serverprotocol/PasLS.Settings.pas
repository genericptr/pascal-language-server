// Pascal Language Server
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

unit PasLS.Settings;

{$mode objfpc}{$H+}
{$scopedenums on}

interface
uses
  Classes, FGL, LSP.BaseTypes, LSP.General;

{ Global String Constants }
const
  kSymbolName_Interface = 'interface';
  kSymbolName_Implementation = 'implementation';

type
  TOverloadPolicy = ( __UNUSED__,
                      Duplicates,  // duplicate function names appear in the list
                      Ignore,      // after the original definition ignore others
                      Suffix       // add a suffix which denotes the overload count
                      );

  { Excludable symbol types for excludeSymbols array }
  TExcludableSymbol = (
    esSectionContainers,      // interface/implementation section containers
    esInterfaceMethodDecls,   // method declarations from interface section
    esImplClassDefs,          // class definitions from implementation section
    esFields,                 // class/record fields
    esProperties,             // class properties
    esConstants,              // constants (global and class constants)
    esEnumMembers             // enum members
  );
  TExcludableSymbols = set of TExcludableSymbol;

const
  { String names for JSON serialization }
  ExcludableSymbolNames: array[TExcludableSymbol] of string = (
    'sectionContainers',
    'interfaceMethodDecls',
    'implClassDefs',
    'fields',
    'properties',
    'constants',
    'enumMembers'
  );

{ Helper functions for TExcludableSymbol }
function TryStrToExcludableSymbol(const S: string; out Symbol: TExcludableSymbol): Boolean;
function ExcludableSymbolToStr(Symbol: TExcludableSymbol): string;

type

  TMacroMap = specialize TFPGMap<ShortString, String>;

  { TServerSettings }

  TServerSettings = class(TInitializationOptions)
  private
    fBooleans: array[0..32] of Boolean;
    fProgram: String;
    {$IFDEF USE_SQLITE}
    fSymbolDatabase: String;
    {$ENDIF}
    fFPCOptions: TStrings;
    fExcludeWorkspaceFolders: TStrings;
    fCodeToolsConfig: String;
    fMaximumCompletions: Integer;
    fOverloadPolicy: TOverloadPolicy;
    fConfig: String;
    fScanFilePatterns: TStrings;
    fExcludeSymbols: TStrings;
    fExcludedSymbolSet: TExcludableSymbols;
    procedure SetFPCOptions(AValue: TStrings);
    procedure SetExcludeWorkspaceFolders(AValue: TStrings);
    procedure SetScanFilePatterns(AValue: TStrings);
    procedure SetExcludeSymbols(AValue: TStrings);
  published
    // Path to the main program file for resolving references
    // if not available the path of the current document will be used
    property &program: String read fProgram write fProgram;
    {$IFDEF USE_SQLITE}
    // Path to SQLite3 database for symbols (requires USE_SQLITE compile switch)
    property symbolDatabase: String read fSymbolDatabase write fSymbolDatabase;
    {$ENDIF}
    // FPC compiler options (passed to Code Tools)
    property fpcOptions: TStrings read fFPCOptions write SetFPCOptions;
    // Optional codetools.config file to load settings from
    property codeToolsConfig: String read fCodeToolsConfig write fCodeToolsConfig;
    // Maximum number of completion items to be returned
    // if the threshold is reached then CompletionList.isIncomplete = true
    property maximumCompletions: Integer read fMaximumCompletions write fMaximumCompletions;
    // Policy which determines how overloaded document symbols are displayed
    property overloadPolicy: TOverloadPolicy read fOverloadPolicy write fOverloadPolicy;
    // procedure completions with parameters are inserted as snippets
    property insertCompletionsAsSnippets: Boolean read fBooleans[0] write fBooleans[0];
    // procedure completions with parameters (non-snippet) insert
    // empty brackets (and insert as snippet)
    property insertCompletionProcedureBrackets: Boolean read fBooleans[1] write fBooleans[1];
    // workspaces folders will be added to unit paths (i.e. -Fu)
    property includeWorkspaceFoldersAsUnitPaths: Boolean read fBooleans[2] write fBooleans[2];
    // workspaces folders will be added to include paths (i.e. -Fi)
    property includeWorkspaceFoldersAsIncludePaths: Boolean read fBooleans[3] write fBooleans[3];
    // workspace folder paths to exclude from workspace path collection
    property excludeWorkspaceFolders: TStrings read fExcludeWorkspaceFolders write SetExcludeWorkspaceFolders;
    // syntax will be checked when file opens or saves
    property checkSyntax: Boolean read fBooleans[4] write fBooleans[4];
    // syntax errors will be published as diagnostics
    property publishDiagnostics: Boolean read fBooleans[5] write fBooleans[5];
    // enable workspace symbols
    property workspaceSymbols: Boolean read fBooleans[6] write fBooleans[6];
    // enable document symbols
    property documentSymbols: Boolean read fBooleans[7] write fBooleans[7];
    // completions contain a minimal amount of extra information
    property minimalisticCompletions: Boolean read fBooleans[8] write fBooleans[8];
    // syntax errors as shown in the UI with ‘window/showMessage’
    property showSyntaxErrors: Boolean read fBooleans[9] write fBooleans[9];
    // ignores completion items like "begin" and "var" which may interfer with IDE snippets
    property ignoreTextCompletions: Boolean read fBooleans[10] write fBooleans[10];
    // config file or directory to read settings from (will support multiple formats)
    property config: String read fConfig write fConfig;
    // File patterns for workspace scanning (array of glob patterns)
    property scanFilePatterns: TStrings read fScanFilePatterns write SetScanFilePatterns;
    // Check inactive regions
    property checkInactiveRegions : Boolean read fBooleans[11] write fBooleans[11];
    // Force flat symbol mode (SymbolInformation[]) instead of hierarchical
    property flatSymbolMode: Boolean read fBooleans[12] write fBooleans[12];
    // Array of symbol types to exclude from document symbols
    property excludeSymbols: TStrings read fExcludeSymbols write SetExcludeSymbols;
  public
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReplaceMacros(Macros: TMacroMap);
    Procedure Assign(aSource : TPersistent); override;
    function CanProvideWorkspaceSymbols: boolean;
    function IsSymbolExcluded(Symbol: TExcludableSymbol): Boolean; inline;
    class function GetPropertyDescription(const PropName: String): String;
  end;


  { TConfigEnvironmentSettings }

  TConfigEnvironmentSettings = class(TLSPStreamable)
  Private
    ffpcDir: string;
    ffpcTarget: string;
    ffpcTargetCPU: string;
    flazarusDir: string;
    fpp: string;
  Public
    Constructor Create; override;
    Procedure Assign(aSource : TPersistent); override;
  published
    property fpcDir : string Read ffpcDir Write ffpcDir;
    property pp : string read fpp write fpp;
    property lazarusDir : string read flazarusDir write flazarusDir;
    property fpcTarget : string read ffpcTarget write ffpcTarget;
    property fpcTargetCPU : string read ffpcTargetCPU write ffpcTargetCPU;
  end;


type
  TClients = class
  public const
    SublimeTextLSP = 'Sublime Text LSP';
    VSCode = 'vscode';
  end;

Function ServerSettings: TServerSettings;
Function ClientInfo: TClientInfo;
Function EnvironmentSettings:TConfigEnvironmentSettings;

implementation

uses
  SysUtils, lazUTF8;

var
  _ServerSettings: TServerSettings;
  _ClientInfo: TClientInfo;
  _EnvironmentSettings:TConfigEnvironmentSettings;

{ TExcludableSymbol helper functions }

function TryStrToExcludableSymbol(const S: string; out Symbol: TExcludableSymbol): Boolean;
var
  I: TExcludableSymbol;
begin
  Result := False;
  for I := Low(TExcludableSymbol) to High(TExcludableSymbol) do
    if SameText(S, ExcludableSymbolNames[I]) then
    begin
      Symbol := I;
      Exit(True);
    end;
end;

function ExcludableSymbolToStr(Symbol: TExcludableSymbol): string;
begin
  Result := ExcludableSymbolNames[Symbol];
end;

Function EnvironmentSettings:TConfigEnvironmentSettings;

begin
  if _EnvironmentSettings=Nil then
    _EnvironmentSettings:=TConfigEnvironmentSettings.Create;
  Result:=_EnvironmentSettings;
end;

Function ServerSettings: TServerSettings;

begin
  if _ServerSettings=Nil then
    _ServerSettings:=TServerSettings.Create;
  Result:=_ServerSettings;
end;

Function ClientInfo: TClientInfo;

begin
  if _ClientInfo=Nil then
    _ClientInfo:=TClientInfo.Create;
  Result:=_ClientInfo;
end;

{ TServerSettings }

procedure TServerSettings.ReplaceMacros(Macros: TMacroMap);

  function ReplaceMacro(const S: String): String;
  var
    I: Integer;
  begin
    { supported multiple formats:
      1) $macro
      2) $MACRO
      3) $(macro)
      4) $(MACRO)
    }
    Result := S;
    for I := 0 to Macros.Count - 1 do
      begin
        Result := StringReplace(Result, '$('+Macros.Keys[I]+')', Macros.Data[I], [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '$'+Macros.Keys[I], Macros.Data[I], [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '$('+LowerCase(Macros.Keys[I])+')', Macros.Data[I], [rfReplaceAll]);
        Result := StringReplace(Result, '$('+UpperCase(Macros.Keys[I])+')', Macros.Data[I], [rfReplaceAll]);
        Result := StringReplace(Result, '$'+LowerCase(Macros.Keys[I]), Macros.Data[I], [rfReplaceAll]);
        Result := StringReplace(Result, '$'+UpperCase(Macros.Keys[I]), Macros.Data[I], [rfReplaceAll]);
      end;
  end;

var
  ExpandedOption: String;
  I: integer;
begin
  &program := ReplaceMacro(&program);
  {$IFDEF USE_SQLITE}
  symbolDatabase := ReplaceMacro(symbolDatabase);
  {$ENDIF}

  for I := 0 to fpcOptions.Count - 1 do
    begin
      ExpandedOption := ReplaceMacro(fpcOptions[I]);
      fpcOptions.Delete(I);
      fpcOptions.Insert(I, ExpandedOption);
    end;
end;

procedure TServerSettings.Assign(aSource : TPersistent);

var
  src : TServerSettings absolute aSource;

begin
  if (aSource is TServerSettings) then
    begin
    fBooleans:=Src.FBooleans;
    fProgram:=Src.fProgram;;
    {$IFDEF USE_SQLITE}
    SymbolDatabase:=Src.SymbolDatabase;
    {$ENDIF}
    FPCOptions:=Src.fpcOptions;
    ExcludeWorkspaceFolders:=Src.ExcludeWorkspaceFolders;
    CodeToolsConfig:=Src.CodeToolsConfig;
    MaximumCompletions:=Src.MaximumCompletions;
    OverloadPolicy:=Src.OverloadPolicy;
    Config:=Src.Config;
    ScanFilePatterns:=Src.ScanFilePatterns;
    ExcludeSymbols:=Src.ExcludeSymbols;
    end
  else
    inherited Assign(aSource);
end;

procedure TServerSettings.SetFPCOptions(AValue: TStrings);
begin
  if fFPCOptions=AValue then Exit;
  fFPCOptions.Assign(AValue);
end;

procedure TServerSettings.SetExcludeWorkspaceFolders(AValue: TStrings);
begin
  if fExcludeWorkspaceFolders=AValue then Exit;
  fExcludeWorkspaceFolders.Assign(AValue);
end;

procedure TServerSettings.SetScanFilePatterns(AValue: TStrings);
begin
  if fScanFilePatterns = AValue then Exit;
  fScanFilePatterns.Assign(AValue);
end;

procedure TServerSettings.SetExcludeSymbols(AValue: TStrings);
var
  I: Integer;
  Symbol: TExcludableSymbol;
begin
  // Only copy if different object; always re-parse the set
  if fExcludeSymbols <> AValue then
    fExcludeSymbols.Assign(AValue);

  // Parse strings into set for efficient lookup
  fExcludedSymbolSet := [];
  for I := 0 to fExcludeSymbols.Count - 1 do
  begin
    if TryStrToExcludableSymbol(fExcludeSymbols[I], Symbol) then
      Include(fExcludedSymbolSet, Symbol);
    // Silently ignore unrecognized values for forward compatibility
  end;
end;

function TServerSettings.IsSymbolExcluded(Symbol: TExcludableSymbol): Boolean;
begin
  Result := Symbol in fExcludedSymbolSet;
end;

function TServerSettings.CanProvideWorkspaceSymbols: boolean;
begin
  result := workspaceSymbols;
end;

class function TServerSettings.GetPropertyDescription(const PropName: String): String;
begin
  case PropName of
    'program': Result := 'Path to the main program file for resolving references';
    {$IFDEF USE_SQLITE}
    'symbolDatabase': Result := 'Path to SQLite3 database for symbols (requires USE_SQLITE)';
    {$ENDIF}
    'fpcOptions': Result := 'FPC compiler options (passed to Code Tools)';
    'codeToolsConfig': Result := 'Optional codetools.config file to load settings from';
    'maximumCompletions': Result := 'Maximum number of completion items to be returned';
    'overloadPolicy': Result := 'Policy which determines how overloaded document symbols are displayed';
    'insertCompletionsAsSnippets': Result := 'Procedure completions with parameters are inserted as snippets';
    'insertCompletionProcedureBrackets': Result := 'Procedure completions with parameters insert empty brackets';
    'includeWorkspaceFoldersAsUnitPaths': Result := 'Workspace folders will be added to unit paths (i.e. -Fu)';
    'includeWorkspaceFoldersAsIncludePaths': Result := 'Workspace folders will be added to include paths (i.e. -Fi)';
    'excludeWorkspaceFolders': Result := 'Workspace folder paths to exclude from workspace path collection';
    'checkSyntax': Result := 'Syntax will be checked when file opens or saves';
    'publishDiagnostics': Result := 'Syntax errors will be published as diagnostics';
    'workspaceSymbols': Result := 'Enable workspace symbols';
    'documentSymbols': Result := 'Enable document symbols';
    'minimalisticCompletions': Result := 'Completions contain a minimal amount of extra information';
    'showSyntaxErrors': Result := 'Syntax errors as shown in the UI with ''window/showMessage''';
    'ignoreTextCompletions': Result := 'Ignores completion items like "begin" and "var"';
    'config': Result := 'Config file or directory to read settings from';
    'scanFilePatterns': Result := 'File patterns for workspace scanning (array of glob patterns, e.g. ["*.pas", "*.pp"])';
    'checkInactiveRegions': Result := 'Check inactive regions';
    'flatSymbolMode': Result := 'Force flat symbol mode (SymbolInformation[]) instead of hierarchical';
    'excludeSymbols': Result := 'Array of symbol types to exclude: sectionContainers, interfaceMethodDecls, implClassDefs, fields, properties, constants, enumMembers';
    else
      Result := '';
  end;
end;

constructor TServerSettings.Create;
begin
  inherited;

  fFPCOptions := TStringList.Create;
  fExcludeWorkspaceFolders := TStringList.Create;
  fScanFilePatterns := TStringList.Create;
  fExcludeSymbols := TStringList.Create;
  fExcludedSymbolSet := [];

  // default settings
  {$IFDEF USE_SQLITE}
  symbolDatabase := '';
  {$ENDIF}
  maximumCompletions := 200;
  overloadPolicy := TOverloadPolicy.Suffix;
  // default file patterns for workspace scanning
  fScanFilePatterns.Add('*.pas');
  fScanFilePatterns.Add('*.pp');
  fScanFilePatterns.Add('*.p');
  fScanFilePatterns.Add('*.inc');
  fScanFilePatterns.Add('*.lpr');
  fScanFilePatterns.Add('*.dpr');

  // options
  insertCompletionsAsSnippets := true;
  includeWorkspaceFoldersAsUnitPaths := true;
  includeWorkspaceFoldersAsIncludePaths := true;
  documentSymbols := true;
  ignoreTextCompletions := true;
  workspaceSymbols := true;
  minimalisticCompletions := false;
  checkInactiveRegions := true;
  
  // errors/diagnostics
  checkSyntax := false;
  publishDiagnostics := false;
  showSyntaxErrors := false;
end;

destructor TServerSettings.Destroy;
begin
  FreeAndNil(fFPCOptions);
  FreeAndNil(fExcludeWorkspaceFolders);
  FreeAndNil(fScanFilePatterns);
  FreeAndNil(fExcludeSymbols);
  inherited Destroy;
end;


{ TConfigEnvironmentSettings }

constructor TConfigEnvironmentSettings.Create;

  procedure MaybeSet(aEnvVar : String; var aVar : String);

  begin
    if GetEnvironmentVariableUTF8(aEnvVar)<>'' then
      aVar:=GetEnvironmentVariableUTF8(aEnvVar);
  end;

begin
  MaybeSet('PP',fpp);
  MaybeSet('FPCDIR',ffpcDir);
  MaybeSet('LAZARUSDIR',fLazarusDir);
  MaybeSet('FPCTARGET',ffpcTarget);
  MaybeSet('FPCTARGETCPU',ffpcTargetCPU);
end;

procedure TConfigEnvironmentSettings.Assign(aSource : TPersistent);
var
  src : TConfigEnvironmentSettings absolute aSource;
begin
  if aSource is TConfigEnvironmentSettings then
    begin
      fpcDir:=src.fpcDir;
      fpcTarget:=src.fpcTarget;
      fpcTargetCPU:=src.fpcTargetCPU;
      lazarusDir:=src.lazarusDir;
      pp:=src.pp;
    end
  else
    inherited Assign(aSource);
end;

finalization
  _ServerSettings.Free;
  _ClientInfo.Free;
  _EnvironmentSettings.Free;
end.
