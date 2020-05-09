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

interface
uses
  Classes;

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
    property includeWorkspaceFoldersAsUnitPaths: Boolean read fBooleans[2] write fBooleans[2];
    property includeWorkspaceFoldersAsIncludePaths: Boolean read fBooleans[3] write fBooleans[3];

    // syntax will be checked when file opens or saves
    property checkSyntax: Boolean read fBooleans[4] write fBooleans[4];

    // syntax errors will be published as diagnostics
    property publishDiagnostics: Boolean read fBooleans[5] write fBooleans[5];

  public
    procedure AfterConstruction; override;
  end;

  TServerSettings = class(TPersistent)
  private
    fOptions: TServerOptions;
    fProgram: String;
    fSymbolDatabase: String;
    fFPCOptions: TStrings;
  published
    property options: TServerOptions read fOptions write fOptions;

    // path to the main program file for resolving references
    // if not available the path of the current document will be used
    property &program: String read fProgram write fProgram;

    // path to SQLite3 database for symbols
    property symbolDatabase: String read fSymbolDatabase write fSymbolDatabase;

    // FPC compiler options (passed to Code Tools)
    // common options required for the parser:
    //   -Fu<x>     Add <x> to unit path
    //   -Fi<x>     Add <x> to include path
    //   -d<x>      Defines the symbol <x>
    //   -Mfpc      Free Pascal dialect (default)
    //   -Mobjfpc   FPC mode with Object Pascal support
    //   -Mdelphi   Delphi 7 compatibility mode
    property FPCOptions: TStrings read fFPCOptions write fFPCOptions;

  public
    procedure AfterConstruction; override;
  end;

var
  ServerSettings: TServerSettings = nil;

implementation

procedure TServerOptions.AfterConstruction;
begin
  includeWorkspaceFoldersAsUnitPaths := true;
  includeWorkspaceFoldersAsIncludePaths := true;

  inherited;
end;

procedure TServerSettings.AfterConstruction;
begin
  inherited;

  if options = nil then
    options := TServerOptions.Create;

  FPCOptions := TStringList.Create;
end;

end.