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
  Classes, Types,
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
  private
    function GetDiagnosticParams: TPublishDiagnosticsParams;
  public
    constructor Create; override;
    destructor Destroy; override;
    function HaveDiagnostics : Boolean;
    Property DiagnosticParams : TPublishDiagnosticsParams Read GetDiagnosticParams;
    procedure Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure Clear(fileName: string);
  end;

  { TDiagnosticsHandler }

  TDiagnosticsHandler = Class
  private
    procedure AddCodeToolError(Diagnostics: TPublishDiagnostics; aTransport: TMessageTransport);
    procedure AddUserDiagnostic(Diagnostics: TPublishDiagnostics; aTransport: TMessageTransport; UserMessage: String);
    procedure ClearDiagnostics(aTransport: TMessageTransport; Code: TCodeBuffer);
    procedure ShowErrorMessage(aTransport: TMessageTransport;  const MessageString: String);
    function StrictSyntaxCheck(aDiagnostics : TPublishDiagnostics; aTransport: TMessageTransport; Code: TCodeBuffer): Boolean;
    function CodeToolsCheckSyntax(aDiagnostics: TPublishDiagnostics; aTransport: TMessageTransport; Code: TCodeBuffer): boolean;
  Public
    procedure CheckSyntax(aTransport : TMessageTransport; Code: TCodeBuffer);
    procedure PublishDiagnostic(aTransport : TMessageTransport; UserMessage: String = '');
  end;

Function DiagnosticsHandler : TDiagnosticsHandler;

implementation

uses
  pastree,  pparser, PasLS.Parser,
  SysUtils, PasLS.Settings;

var
  _DiagnosticsHandler :  TDiagnosticsHandler;

Function DiagnosticsHandler : TDiagnosticsHandler;

begin
  if _DiagnosticsHandler=Nil then
    _DiagnosticsHandler:=TDiagnosticsHandler.Create;
  Result:=_DiagnosticsHandler;
end;

Procedure TDiagnosticsHandler.AddUserDiagnostic(Diagnostics : TPublishDiagnostics; aTransport: TMessageTransport; UserMessage : String);

begin
  // Message on stdErr
  aTransport.SendDiagnostic(UserMessage);
  // Actual diagnostic
  Diagnostics.Add('',
                   UserMessage,
                   0,
                   0,
                   // TODO: code tools error ID is too large (int64), what should we do?
                   1{CodeToolBoss.ErrorID},
                   TDiagnosticSeverity.Error);

end;

Procedure TDiagnosticsHandler.ShowErrorMessage(aTransport : TMessageTransport; const MessageString : String);

var
  ShowMessage: TShowMessageNotification;

begin
  ShowMessage:=TShowMessageNotification.Create(TMessageType.Error, '⚠️ '+MessageString);
  try
    ShowMessage.Send(aTransport);
  finally
    ShowMessage.Free;
  end;
end;


Procedure TDiagnosticsHandler.AddCodeToolError(Diagnostics : TPublishDiagnostics; aTransport: TMessageTransport);

Var
  MessageString : String;
  aLine,aCol : Integer;
  aFileName : string;
  aErrorMessage : String;

begin
  aErrorMessage:=CodeToolBoss.ErrorMessage;
  if aErrorMessage='' then
    exit;
  aLine:=CodeToolBoss.ErrorLine;
  aCol:=CodeToolBoss.ErrorColumn;
  if CodeToolBoss.ErrorCode<> nil then
    begin
    aFileName:=CodeToolBoss.ErrorCode.FileName;
    MessageString:=aFileName+': ';
    end
  else
    begin
    aFileName:='';
    MessageString:='';
    end;
  MessageString := MessageString+Format('"%s" @ %d:%d;',[aErrorMessage,aLine,aCol]);
  // Message on stdErr
  aTransport.SendDiagnostic('Syntax Error -> %s',[MessageString]);
  // Show message in the gui also
  if ServerSettings.showSyntaxErrors then
    ShowErrorMessage(aTransport, MessageString);
  if aFileName<>'' then
    Diagnostics.Add(aFileName,
                    aErrorMessage,
                    aLine - 1,
                    aCol - 1,
                    // TODO: code tools error ID is too large (int64), what should we do?
                    1{CodeToolBoss.ErrorID},
                    TDiagnosticSeverity.Error);
end;

{ Publish the last code tools error as a diagnostics }
procedure TDiagnosticsHandler.PublishDiagnostic(aTransport : TMessageTransport; UserMessage: String = '');
var
  Notification: TPublishDiagnostics;

begin
  Notification:=TPublishDiagnostics.Create;
  try
    if UserMessage <> '' then
      AddUserDiagnostic(Notification,aTransport,UserMessage)
    else if (CodeToolBoss.ErrorCode<>Nil) then
      AddCodeToolError(Notification,aTransport);
    if not ServerSettings.publishDiagnostics then
      exit;
    if Notification.HaveDiagnostics then
      Notification.Send(aTransport);
  finally
    Notification.Free;
  end;
end;


function TDiagnosticsHandler.StrictSyntaxCheck(aDiagnostics : TPublishDiagnostics; aTransport : TMessageTransport; Code: TCodeBuffer) : Boolean;

     Procedure SendError(E : Exception);

     var
       MessageString,aFileName : String;
       aCol,aRow : Integer;
       EParse: EParserError absolute E;

     begin
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
           ShowErrorMessage(aTransport,MessageString);
         if not ServerSettings.publishDiagnostics then
           exit;
         aDiagnostics.Add(aFileName,
                          MessageString,
                          aRow-1,
                          aCol-1,
                          // TODO: code tools error ID is too large (int64), what should we do?
                          1{CodeToolBoss.ErrorID},
                          TDiagnosticSeverity.Error);
       except
         On Ex : Exception do
           TLSPContext.Log('Error %s sending diagnostic: %s',[Ex.ClassName,Ex.Message]);
       end;
     end;

Var
  Module : TPasModule;
  SourceParser : TSourceParser;
  Args : TStringDynArray;
  I : Integer;

begin
  Args:=[];
  Result:=False;
  Module:=nil;
  SourceParser:=Nil;
  try
    try
      SourceParser:=TSourceParser.Create;
      SourceParser.Code:=Code;
      SourceParser.OSTarget:=EnvironmentSettings.fpcTarget;
      SourceParser.CPUTarget:=EnvironmentSettings.fpcTargetCPU;
      SourceParser.Options:=[];
      SetLength(Args,ServerSettings.fpcOptions.Count+1);
      for I:=0 to ServerSettings.fpcOptions.Count-1 do
        Args[i]:=ServerSettings.fpcOptions[i];
      Args[Length(Args)-1]:=Code.Filename;
      SourceParser.CommandLine:=Args;
      Module:=SourceParser.ParseSource;
      Result:=True;
    except
      on e : exception do
        SendError(E);
    end;
  finally
    SourceParser.Free;
    Module.Free;
  end;
end;

procedure TDiagnosticsHandler.CheckSyntax(aTransport : TMessageTransport; Code: TCodeBuffer);

Var
  Diagnostics : TPublishDiagnostics;
  CodeOK : Boolean;

begin
  if not ServerSettings.checkSyntax then
    exit;
  // All diagnostics in 1 message.
  Diagnostics := TPublishDiagnostics.Create;
  try
    // Check code. These routines will possibly send messages to a window or stdout, depending on settings.
    CodeOk:=CodeToolsCheckSyntax(Diagnostics,aTransport,Code);
    if CodeOK then
      CodeOK:=StrictSyntaxCheck(Diagnostics,aTransport,Code);
    // If we need to publish settings, then send the diagnostics.
    if ServerSettings.publishDiagnostics then
      begin
      if CodeOK then
        Diagnostics.Clear(Code.FileName);
      Diagnostics.Send(aTransport);
      end;
  finally
    Diagnostics.Free;
  end;
end;

function TDiagnosticsHandler.CodeToolsCheckSyntax(aDiagnostics: TPublishDiagnostics; aTransport : TMessageTransport; Code: TCodeBuffer): boolean;

var
  Tool: TCodeTool;

begin
  // Check for errors.
  Result:=CodeToolBoss.Explore(Code,Tool,true);

  if not Result then
    // Errors found ? Publish them.
    AddCodeToolError(aDiagnostics,aTransport);
end;

procedure TDiagnosticsHandler.ClearDiagnostics(aTransport : TMessageTransport; Code: TCodeBuffer);
var
  Diagnostics: TPublishDiagnostics;
begin
  if not ServerSettings.publishDiagnostics then
    Exit;
  Diagnostics:=TPublishDiagnostics.Create;
  try
    Diagnostics.Clear(Code.FileName);
    Diagnostics.Send(aTransport);
  finally
    Diagnostics.Free;
  end;
end;

{ TPublishDiagnostics }

procedure TPublishDiagnostics.Clear(fileName: string);
begin
  DiagnosticParams.uri := PathToURI(fileName);
  DiagnosticParams.diagnostics.Clear;
end;

procedure TPublishDiagnostics.Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
var
  Diagnostic: TDiagnostic;
begin
  DiagnosticParams.uri := PathToURI(fileName);
  Diagnostic := DiagnosticParams.diagnostics.Add;
  Diagnostic.range.SetRange(line, column);
  Diagnostic.severity := severity;
  Diagnostic.code := code;
  Diagnostic.source := 'Free Pascal Compiler';
  Diagnostic.message := message;
end;

function TPublishDiagnostics.GetDiagnosticParams: TPublishDiagnosticsParams;

begin
  Result:=Params as TPublishDiagnosticsParams;
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

function TPublishDiagnostics.HaveDiagnostics: Boolean;
begin
  Result:=DiagnosticParams.diagnostics.Count>0;
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

Initialization

Finalization
  _DiagnosticsHandler.Free;
end.
