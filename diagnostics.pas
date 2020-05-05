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

unit diagnostics;

{$mode objfpc}{$H+}

interface

uses
  Classes, 
  CodeToolManager, CodeCache,
  lsp, basic;

type

  { TPublishDiagnosticsParams }

  TPublishDiagnosticsParams = class(TPersistent)
  private
    fUri: TDocumentUri;
    fVersion: integer;
    fDiagnostics: TDiagnosticItems;
  published
    // The URI for which diagnostic information is reported.
    property uri: TDocumentUri read fUri write fUri;

    // The version number of the document the diagnostics are published for.
    // todo: this must be optional
    //property version: integer read fVersion write fVersion;

    // An array of diagnostic information items.
    property diagnostics: TDiagnosticItems read fDiagnostics write fDiagnostics;
  public
    procedure AfterConstruction; override;
  end;

  { TPublishDiagnostics }

  { Diagnostics notification are sent from the server to the client to signal results of validation runs.

    Diagnostics are “owned” by the server so it is the server’s responsibility to clear them if necessary. 
    The following rule is used for VS Code servers that generate diagnostics:

    if a language is single file only (for example HTML) then diagnostics are cleared by the server when the file is closed.
    if a language has a project system (for example C#) diagnostics are not cleared when a file closes. When a project is 
    opened all diagnostics for all files are recomputed (or read from a cache).
    When a file changes it is the server’s responsibility to re-compute diagnostics and push them to the client. If the 
    computed set is empty it has to push the empty array to clear former diagnostics. Newly pushed diagnostics always replace 
    previously pushed diagnostics. There is no merging that happens on the client side. }

  TPublishDiagnostics = class(TNotificationMessage)
  private
    fParams: TPublishDiagnosticsParams;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure Clear(fileName: string);
  end;

{ Code Tools Error Handling }

procedure CheckSyntax(Code: TCodeBuffer);
procedure PublishCodeToolsError;

implementation
uses
  SysUtils, settings;

{ Publish the last code tools error as a diagnostics }
procedure PublishCodeToolsError;
var
  notification: TPublishDiagnostics;
begin
  if not (TServerOption.PublishDiagnostics in ServerSettings.Options) then
    begin
      writeln(stderr, 'Syntax Error -> '+CodeToolBoss.ErrorCode.FileName+': "'+CodeToolBoss.ErrorMessage+'" @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
      flush(stderr);
      exit;
    end;

  notification := TPublishDiagnostics.Create;
  notification.Add(CodeToolBoss.ErrorCode.FileName, 
                   CodeToolBoss.ErrorMessage, 
                   CodeToolBoss.ErrorLine - 1, 
                   CodeToolBoss.ErrorColumn - 1, 
                   // TODO: code tools error ID is too large (int64), what should we do?
                   1{CodeToolBoss.ErrorID},
                   TDiagnosticSeverity.Error);
  notification.Send;
  notification.Free;
end;

{ Checks syntax for code buffer and publishes errors as diagnostics }

procedure CheckSyntax(Code: TCodeBuffer); 
var
  Tool: TCodeTool;
  notification: TPublishDiagnostics;
begin
  if not (TServerOption.CheckSyntax in ServerSettings.Options) then
    exit;
  if not CodeToolBoss.Explore(Code,Tool,true) then
    PublishCodeToolsError
  else if TServerOption.PublishDiagnostics in ServerSettings.Options then
    begin
      // todo: when we have a document store we can check to see
      // if we actually have any errors.
      notification := TPublishDiagnostics.Create;
      notification.Clear(Code.FileName);
      notification.Send;
      notification.Free;
    end;
end;

{ TPublishDiagnostics }

procedure TPublishDiagnostics.Clear(fileName: string);
begin
  fParams.uri := PathToURI(fileName);
  fParams.diagnostics.Clear;
end;

procedure TPublishDiagnostics.Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
var
  diagnostic: TDiagnostic;
begin
  fParams.uri := PathToURI(fileName);

  diagnostic := TDiagnostic(fParams.diagnostics.Add);
  diagnostic.range := TRange.Create(line, column);
  diagnostic.severity := severity;
  diagnostic.code := code;
  diagnostic.source := 'Free Pascal Compiler';
  diagnostic.message := message;
end;

constructor TPublishDiagnostics.Create;
begin  
  fParams := TPublishDiagnosticsParams.Create;
  params := fParams;
  method := 'textDocument/publishDiagnostics';
end;

destructor TPublishDiagnostics.Destroy; 
begin
  fParams.Free;
  inherited;
end;

{ TPublishDiagnosticsParams }

procedure TPublishDiagnosticsParams.AfterConstruction;
begin
  inherited;
  diagnostics := TDiagnosticItems.Create;
end;

end.