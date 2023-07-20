// Pascal Language Server
// Copyright 2020 Arjan Adriaanse
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
unit PasLS.Diagnostics;

{$mode objfpc}{$H+}

interface


uses
  { RTL }
  Classes, Types,
  { Code Tools }
  CodeToolManager, CodeCache,
  { Protocol }
  LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Window, LSP.Messages, LSP.Diagnostics;

Type
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
    procedure SendDiagnosticMessage(aTransport : TMessageTransport; UserMessage: String = '');
  end;

Function DiagnosticsHandler : TDiagnosticsHandler;
procedure PublishCodeToolsError(aTransport : TMessageTransport; const aMessage : string);

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

procedure PublishCodeToolsError(aTransport: TMessageTransport;
  const aMessage: string);
begin
  DiagnosticsHandler.SendDiagnosticMessage(aTransport,aMessage);
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

procedure TDiagnosticsHandler.SendDiagnosticMessage(aTransport : TMessageTransport; UserMessage: String = '');
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

Type

  { TErrorReporter }

  TErrorReporter = class
  private
    FErrorCount: Integer;
    FHandler : TDiagnosticsHandler;
    FParser : TSourceParser;
    FDiagnostics : TPublishDiagnostics;
    FTransport : TMessageTransport;
  Protected
    procedure ReportError(Sender: TObject; const aError, aFileName: string; aCode, aLine, aCol: Integer);
  Public
    Constructor Create(aHandler : TDiagnosticsHandler; aParser : TSourceParser;aDiagnostics : TPublishDiagnostics; aTransport : TMessageTransport);
    Property ErrorCount : Integer Read FErrorCount;
  end;

{ TErrorReporter }

constructor TErrorReporter.Create(aHandler: TDiagnosticsHandler;
  aParser: TSourceParser; aDiagnostics: TPublishDiagnostics;
  aTransport: TMessageTransport);
begin
  FHandler:=aHandler;
  FParser:=aParser;
  FDiagnostics:=aDiagnostics;
  FTransport:=aTransport;
  FParser.OnError:=@ReportError;
end;

procedure TErrorReporter.ReportError(Sender: TObject; const aError,
  aFileName: string; aCode, aLine, aCol: Integer);

var
  S : String;

begin
  Inc(FErrorCount);
  S:=Format('%s(%d,%d) : %s',[aFileName,aLine,aCol,aError]);
  FTransport.SendDiagnostic(S);
  if ServerSettings.showSyntaxErrors then
    FHandler.ShowErrorMessage(FTransport,S);
  if ServerSettings.publishDiagnostics then
    FDiagnostics.Add(aFileName,
                     aError,
                     aLine-1,
                     aCol-1,
                     aCode,
                     TDiagnosticSeverity.Error);
end;

function TDiagnosticsHandler.StrictSyntaxCheck(aDiagnostics : TPublishDiagnostics; aTransport : TMessageTransport; Code: TCodeBuffer) : Boolean;

Var
  Module : TPasModule;
  SourceParser : TSourceParser;
  Args : TStringDynArray;
  I : Integer;
  Reporter : TErrorReporter;

begin
  Args:=[];
  Result:=False;
  Module:=nil;
  SourceParser:=Nil;
  Reporter:=Nil;
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
      Reporter:=TErrorReporter.Create(Self,SourceParser,aDiagnostics,aTransport);
      Module:=SourceParser.ParseSource;
      Result:=Reporter.ErrorCount=0;
    except
      on e : exception do
        Reporter.ReportError(Self,E.Message,Code.FileName,-1,0,0);
    end;
  finally
    Reporter.Free;
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


Initialization

Finalization
  _DiagnosticsHandler.Free;
end.

