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
    fBooleans: array[0..32] of boolean;
  published
    // procedure completions with parameters are inserted as snippets
    property insertCompletionsAsSnippets: boolean read fBooleans[0] write fBooleans[0];
    // procedure completions with parameters (non-snippet) insert
    // empty brackets (and insert as snippet)
    property insertCompletionProcedureBrackets: boolean read fBooleans[1] write fBooleans[1];
    property includeWorkspaceFoldersAsUnitPaths: boolean read fBooleans[2] write fBooleans[2];
    property includeWorkspaceFoldersAsIncludePaths: boolean read fBooleans[3] write fBooleans[3];
    // syntax will be checked when file opens or saves
    property checkSyntax: boolean read fBooleans[4] write fBooleans[4];
    // syntax errors will be published as diagnostics
    property publishDiagnostics: boolean read fBooleans[5] write fBooleans[5];
  public
    procedure AfterConstruction; override;
  end;

  TServerSettings = class(TPersistent)
  private
    fOptions: TServerOptions;
    fProgram: String;
    fFPCOptions: TStrings;
  published
    property options: TServerOptions read fOptions write fOptions;
    property &program: String read fProgram write fProgram;
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