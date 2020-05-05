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
    fInsertCompletionsAsSnippets: boolean;
    fInsertCompletionProcedureBrackets: boolean;
    fIncludeWorkspaceFoldersAsUnitPaths: boolean;
    fIncludeWorkspaceFoldersAsIncludePaths: boolean;
    fCheckSyntax: boolean;
    fPublishDiagnostics: boolean;
  published
    // procedure completions with parameters are inserted as snippets
    property insertCompletionsAsSnippets: boolean read fInsertCompletionsAsSnippets write fInsertCompletionsAsSnippets;
    // procedure completions with parameters (non-snippet) insert
    // empty brackets (and insert as snippet)
    property insertCompletionProcedureBrackets: boolean read fInsertCompletionProcedureBrackets write fInsertCompletionProcedureBrackets;
    property includeWorkspaceFoldersAsUnitPaths: boolean read fIncludeWorkspaceFoldersAsUnitPaths write fIncludeWorkspaceFoldersAsUnitPaths;
    property includeWorkspaceFoldersAsIncludePaths: boolean read fIncludeWorkspaceFoldersAsIncludePaths write fIncludeWorkspaceFoldersAsIncludePaths;
    // syntax will be checked when file opens or saves
    property checkSyntax: boolean read fCheckSyntax write fCheckSyntax;
    // syntax errors will be published as diagnostics
    property publishDiagnostics: boolean read fPublishDiagnostics write fPublishDiagnostics;
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

procedure TServerSettings.AfterConstruction;
begin
  inherited;

  ServerSettings := self;

  Options := TServerOptions.Create;
  FPCOptions := TStringList.Create;
end;

end.