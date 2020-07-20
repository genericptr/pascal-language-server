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

unit settings;

{$mode objfpc}{$H+}
{$scopedenums on}

interface
uses
  Classes, FGL;

type
  TOverloadPolicy = ( Duplicates = 1,  // duplicate function names appear in the list
                      Ignore = 2,      // after the original definition ignore others
                      Suffix = 3       // add a suffix which denotes the overload count
                      );

type
  TServerOptions = class(TPersistent)
  private
    fBooleans: array[0..32] of Boolean;
  published
    // procedure completions with parameters are inserted as snippets
    property insertCompletionsAsSnippets: Boolean read fBooleans[0] write fBooleans[0];
    // procedure completions with parameters (non-snippet) insert
    // empty brackets (and insert as snippet)
    property insertCompletionProcedureBrackets: Boolean read fBooleans[1] write fBooleans[1];
    // workspaces folders will be added to unit paths (i.e. -Fu)
    property includeWorkspaceFoldersAsUnitPaths: Boolean read fBooleans[2] write fBooleans[2];
    // workspaces folders will be added to include paths (i.e. -Fi)
    property includeWorkspaceFoldersAsIncludePaths: Boolean read fBooleans[3] write fBooleans[3];
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
  public
    procedure AfterConstruction; override;
  end;

  TMacroMap = specialize TFPGMap<ShortString, String>;

  TServerSettings = class(TPersistent)
  private
    fOptions: TServerOptions;
    fProgram: String;
    fSymbolDatabase: String;
    fFPCOptions: TStrings;
    fCodeToolsConfig: String;
    fMaximumCompletions: Integer;
    fOverloadPolicy: TOverloadPolicy;
  published
    // General options
    property options: TServerOptions read fOptions write fOptions;
    // Path to the main program file for resolving references
    // if not available the path of the current document will be used
    property &program: String read fProgram write fProgram;
    // Path to SQLite3 database for symbols
    property symbolDatabase: String read fSymbolDatabase write fSymbolDatabase;
    // FPC compiler options (passed to Code Tools)
    property fpcOptions: TStrings read fFPCOptions write fFPCOptions;
    // Optional codetools.config file to load settings from
    property codeToolsConfig: String read fCodeToolsConfig write fCodeToolsConfig;
    // Maximum number of completion items to be returned
    // if the threshold is reached then CompletionList.isIncomplete = true
    property maximumCompletions: Integer read fMaximumCompletions write fMaximumCompletions;
    // Policy which determines how overloaded document symbols are displayed
    property overloadPolicy: TOverloadPolicy read fOverloadPolicy write fOverloadPolicy;

    function CanProvideWorkspaceSymbols: boolean;
  public
    procedure AfterConstruction; override;
    procedure ReplaceMacros(Macros: TMacroMap);
  end;

var
  ServerSettings: TServerSettings = nil;

implementation
uses
  SysUtils;

{ TServerOptions }

procedure TServerOptions.AfterConstruction;
begin

  // default settings
  checkSyntax := false;
  insertCompletionsAsSnippets := true;
  includeWorkspaceFoldersAsUnitPaths := true;
  includeWorkspaceFoldersAsIncludePaths := true;
  workspaceSymbols := false;
  documentSymbols := true;
  publishDiagnostics := false;
  minimalisticCompletions := false;
  showSyntaxErrors := true;

  inherited;
end;


{ TServerSettings }

procedure TServerSettings.ReplaceMacros(Macros: TMacroMap);

  function ReplaceMacro(const S: String): String;
  var
    I: Integer;
  begin
    { support multiple formats: 
      1) $macro
      2) $MACRO
      3) $(macro)
      4) $(MACRO)
    }
    Result := S;
    for I := 0 to Macros.Count - 1 do
      begin
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
  symbolDatabase := ReplaceMacro(symbolDatabase);

  for I := 0 to fpcOptions.Count - 1 do
    begin
      ExpandedOption := ReplaceMacro(fpcOptions[I]);
      fpcOptions.Delete(I);
      fpcOptions.Insert(I, ExpandedOption);
    end;
end;

function TServerSettings.CanProvideWorkspaceSymbols: boolean;
begin
  result := options.workspaceSymbols and 
            (symbolDatabase <> '') and 
            FileExists(ExpandFileName(symbolDatabase));
end;

procedure TServerSettings.AfterConstruction;
begin
  inherited;

  options := TServerOptions.Create;
  FPCOptions := TStringList.Create;

  // default settings
  symbolDatabase := '';
  maximumCompletions := 500;
  overloadPolicy := TOverloadPolicy.Suffix;
end;

end.