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
  lsp, basic, window;

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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure Clear(fileName: string);
  end;

{ Code Tools Error Handling }

procedure CheckSyntax(Code: TCodeBuffer);
procedure PublishDiagnostic(UserMessage: String = '');
procedure ClearDiagnostics(Code: TCodeBuffer);

implementation
uses
  SysUtils, settings;

{ Publish the last code tools error as a diagnostics }
procedure PublishDiagnostic(UserMessage: String = '');
var
  Notification: TPublishDiagnostics;
  ShowMessage: TShowMessageNotification;
  FileName, 
  MessageString: String;
begin
  
  if UserMessage <> '' then
    writeln(stderr, UserMessage)
  else
    begin
      if CodeToolBoss.ErrorCode <> nil then
        MessageString := CodeToolBoss.ErrorCode.FileName+': "'+CodeToolBoss.ErrorMessage+'" @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn)
      else if CodeToolBoss.ErrorMessage <> '' then
        MessageString := '"'+CodeToolBoss.ErrorMessage+'" @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn)
      else
        // there's no error to show so bail
        // probably PublishDiagnostic should not have been called
        exit;

      // print the erro to StdErr
      writeln(StdErr, 'Syntax Error -> '+MessageString);

      // Show message in the gui also
      if ServerSettings.showSyntaxErrors then
        begin
          ShowMessage := TShowMessageNotification.Create(TMessageType.Error, '⚠️ '+MessageString);
          ShowMessage.Send;
          ShowMessage.Free;
        end;
    end;
  Flush(stderr);

  if not ServerSettings.publishDiagnostics then
    exit;

  if UserMessage <> '' then
    begin
      Notification := TPublishDiagnostics.Create;
      Notification.Add('', 
                       UserMessage, 
                       0, 
                       0, 
                       // TODO: code tools error ID is too large (int64), what should we do?
                       1{CodeToolBoss.ErrorID},
                       TDiagnosticSeverity.Error);
      Notification.Send;
      Notification.Free;
    end
  else if CodeToolBoss.ErrorCode <> nil then
    begin
      Notification := TPublishDiagnostics.Create;
      Notification.Add(CodeToolBoss.ErrorCode.FileName, 
                       CodeToolBoss.ErrorMessage, 
                       CodeToolBoss.ErrorLine - 1, 
                       CodeToolBoss.ErrorColumn - 1, 
                       // TODO: code tools error ID is too large (int64), what should we do?
                       1{CodeToolBoss.ErrorID},
                       TDiagnosticSeverity.Error);
      Notification.Send;
      Notification.Free;
    end;
end;

{ Checks syntax for code buffer and publishes errors as diagnostics }

procedure CheckSyntax(Code: TCodeBuffer); 
var
  Tool: TCodeTool;
  Notification: TPublishDiagnostics;
begin
  if not ServerSettings.checkSyntax then
    exit;

  if not CodeToolBoss.Explore(Code,Tool,true) then
    PublishDiagnostic
  else if ServerSettings.publishDiagnostics then
    begin
      // todo: when we have a document store we can check to see
      // if we actually have any errors.
      Notification := TPublishDiagnostics.Create;
      Notification.Clear(Code.FileName);
      Notification.Send;
      Notification.Free;
    end;
end;

procedure ClearDiagnostics(Code: TCodeBuffer);
var
  Notification: TPublishDiagnostics;
begin
  if ServerSettings.publishDiagnostics then
    begin
      // todo: when we have a document store we can check to see
      // if we actually have any errors.
      Notification := TPublishDiagnostics.Create;
      Notification.Clear(Code.FileName);
      Notification.Send;
      Notification.Free;
    end;
end;

{ TPublishDiagnostics }

procedure TPublishDiagnostics.Clear(fileName: string);
begin
  TPublishDiagnosticsParams(params).uri := PathToURI(fileName);
  TPublishDiagnosticsParams(params).diagnostics.Clear;
end;

procedure TPublishDiagnostics.Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
var
  Diagnostic: TDiagnostic;
begin
  TPublishDiagnosticsParams(params).uri := PathToURI(fileName);

  Diagnostic := TDiagnostic(TPublishDiagnosticsParams(params).diagnostics.Add);
  Diagnostic.range := TRange.Create(line, column);
  Diagnostic.severity := severity;
  Diagnostic.code := code;
  Diagnostic.source := 'Free Pascal Compiler';
  Diagnostic.message := message;
end;

constructor TPublishDiagnostics.Create;
begin  
  params := TPublishDiagnosticsParams.Create;
  method := 'textDocument/publishDiagnostics';
end;

destructor TPublishDiagnostics.Destroy; 
begin
  params.Free;
  inherited;
end;

{ TPublishDiagnosticsParams }

procedure TPublishDiagnosticsParams.AfterConstruction;
begin
  inherited;
  diagnostics := TDiagnosticItems.Create;
end;

end.