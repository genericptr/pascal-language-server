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

unit LSP.Diagnostics;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, 
  { Code Tools }
  CodeToolManager, CodeCache,
  { Protocol }
  LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Window, LSP.Messages;

type

  { TPublishDiagnosticsParams }

  TPublishDiagnosticsParams = class(TLSPStreamable)
  private
    fUri: TDocumentUri;
    fDiagnostics: TDiagnosticItems;
    procedure SetDiagnostics(AValue: TDiagnosticItems);
  published
    // The URI for which diagnostic information is reported.
    property uri: TDocumentUri read fUri write fUri;

    // The version number of the document the diagnostics are published for.
    // todo: this must be optional
    //property version: integer read fVersion write fVersion;

    // An array of diagnostic information items.
    property diagnostics: TDiagnosticItems read fDiagnostics write SetDiagnostics;
  public
    Constructor Create; override;
    Destructor Destroy; override;
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
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure Clear(fileName: string);
  end;

procedure CheckSyntax(aTransport : TMessageTransport; Code: TCodeBuffer);
procedure PublishDiagnostic(aTransport : TMessageTransport; UserMessage: String = '');
procedure ClearDiagnostics(aTransport : TMessageTransport; Code: TCodeBuffer);

implementation

uses
  pastree,  pparser, PasLS.Parser,
  SysUtils, PasLS.Settings;

{ Publish the last code tools error as a diagnostics }
procedure PublishDiagnostic(aTransport : TMessageTransport; UserMessage: String = '');
var
  Notification: TPublishDiagnostics;
  ShowMessage: TShowMessageNotification;
  MessageString: String;

begin
  Notification:=Nil;
  ShowMessage:=Nil;
  try
    if UserMessage <> '' then
      aTransport.SendDiagnostic(UserMessage)
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

        aTransport.SendDiagnostic('Syntax Error -> %s',[MessageString]);

        // Show message in the gui also
        if ServerSettings.showSyntaxErrors then
          begin
            ShowMessage := TShowMessageNotification.Create(TMessageType.Error, '⚠️ '+MessageString);
            ShowMessage.Send(aTransport);
          end;
      end;

    if not ServerSettings.publishDiagnostics then
      exit;

    if (UserMessage <> '') then
      begin
        Notification := TPublishDiagnostics.Create;
        Notification.Add('',
                         UserMessage,
                         0,
                         0,
                         // TODO: code tools error ID is too large (int64), what should we do?
                         1{CodeToolBoss.ErrorID},
                         TDiagnosticSeverity.Error);
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
        Notification.Send(aTransport);
      end;
  finally
    Notification.Free;
    ShowMessage.Free;
  end;
end;


function StrictSyntaxCheck(aTransport : TMessageTransport; Code: TCodeBuffer) : Boolean;

     Procedure SendError(E : Exception);

     var
       MessageString,aFileName : String;
       aCol,aRow : Integer;
       ShowMessage : TShowMessageNotification;
       Notification : TPublishDiagnostics;
       EParse: EParserError absolute E;

     begin
       ShowMessage:=nil;
       Notification:=Nil;
       try
         MessageString:=E.Message;
         if E is EParserError then
           begin
           aCol:=EParse.Column;
           aRow:=EParse.Row;
           aFileName:=EParse.Filename;
           end;
         aTransport.SendDiagnostic('Syntax Error -> %s',[MessageString]);
         // Show message in the gui also
         if ServerSettings.showSyntaxErrors then
           begin
             ShowMessage := TShowMessageNotification.Create(TMessageType.Error, '⚠️ '+MessageString);
             try
               ShowMessage.Send(aTransport);
             finally
               ShowMessage.Free;
             end;
           end;
         if not ServerSettings.publishDiagnostics then
          exit;
         // Publish diagnostic
         Notification := TPublishDiagnostics.Create;
         try
           Notification.Add(aFileName,
                           MessageString,
                           aRow,
                           aCol,
                           // TODO: code tools error ID is too large (int64), what should we do?
                           1{CodeToolBoss.ErrorID},
                           TDiagnosticSeverity.Error);
           Notification.Send(aTransport);
         finally
           Notification.Free;
         end;
       except
         On Ex : Exception do
           TLSPContext.Log('Error %s sending diagnostic: %s',[Ex.ClassName,Ex.Message]);
       end;
     end;

Var
  Module : TPasModule;

begin
  Result:=False;
  Module:=nil;
  try
    try
      Module:=ParseSource([Code.Filename],Code,EnvironmentSettings.fpcTarget,EnvironmentSettings.fpcTargetCPU,[]);
      Result:=True;
    except
      on e : exception do
        SendError(E);
    end;
  finally
    Module.Free;
  end;
end;

procedure CheckSyntax(aTransport : TMessageTransport; Code: TCodeBuffer);
var
  Tool: TCodeTool;
  Notification: TPublishDiagnostics;
begin
  if not ServerSettings.checkSyntax then
    exit;

  if not CodeToolBoss.Explore(Code,Tool,true) then
    PublishDiagnostic(aTransport)
  else if ServerSettings.publishDiagnostics then
    begin
      if StrictSyntaxCheck(aTransport,Code) then
        begin
          // todo: when we have a document store we can check to see
          // if we actually have any errors.
          Notification := TPublishDiagnostics.Create;
          try
            Notification.Clear(Code.FileName);
            Notification.Send(aTransport);
          finally
            Notification.Free;
          end;
        end;
    end;
end;

procedure ClearDiagnostics(aTransport : TMessageTransport; Code: TCodeBuffer);
var
  Notification: TPublishDiagnostics;
begin
  if ServerSettings.publishDiagnostics then
    begin
      // todo: when we have a document store we can check to see
      // if we actually have any errors.
      Notification := TPublishDiagnostics.Create;
      Notification.Clear(Code.FileName);
      Notification.Send(aTransport);
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
  Diagnostic.range.SetRange(line, column);
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

procedure TPublishDiagnosticsParams.SetDiagnostics(AValue: TDiagnosticItems);
begin
  if fDiagnostics=AValue then Exit;
  fDiagnostics.Assign(AValue);
end;

constructor TPublishDiagnosticsParams.Create;
begin
  inherited;
  fdiagnostics := TDiagnosticItems.Create;
end;

destructor TPublishDiagnosticsParams.Destroy;
begin
  FreeAndNil(fDiagnostics);
  inherited Destroy;
end;

end.
