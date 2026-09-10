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
unit PasLS.Diagnostics;

{$mode objfpc}{$H+}

interface


uses
  { RTL }
  Classes, Types, fgl,
  { Code Tools }
  CodeToolManager, CodeCache, CodeTree, CodeAtom, 
  BasicCodeTools, PascalReaderTool, PascalParserTool,
  { Protocol }
  PasLS.CodeUtils,
  LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Window, LSP.Messages, LSP.Diagnostics;

Type
  { TDiagnosticsHandler }

  TDiagnosticsHandler = Class
  private
    fPublishDiagnostics: TPublishDiagnostics;

    procedure AddCodeToolError(aTransport: TMessageTransport);
    procedure AddUserDiagnostic(aTransport: TMessageTransport; UserMessage: String);
    procedure ShowErrorMessage(aTransport: TMessageTransport;  const MessageString: String);
    function StrictSyntaxCheck(aTransport: TMessageTransport; Code: TCodeBuffer): Boolean;
    function CodeToolsCheckSyntax(aTransport: TMessageTransport; Code: TCodeBuffer): boolean;
  Public
    constructor Create;
    destructor Destroy; override;
    procedure CheckSyntax(aTransport : TMessageTransport; Code: TCodeBuffer);
    procedure SendDiagnosticMessage(aTransport : TMessageTransport; UserMessage: String = '');
    procedure AddParserError(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
  end;

  TIdentifierGatherer = class
  private
    FIdentifiers: TCodeXYPositions;
    {$if FPC_FULLVERSION >= 30301}
    procedure OnIdentifierFound(Sender: TPascalParserTool;
      IdentifierCleanPos: integer; Range: TEPRIRange;
      Node: TCodeTreeNode; Data: Pointer; var Abort: boolean;
      RefsStart: integer);
    {$else}
    procedure OnIdentifierFound(Sender: TPascalParserTool;
          IdentifierCleanPos: integer; Range: TEPRIRange;
          Node: TCodeTreeNode; Data: Pointer; var Abort: boolean);
    {$endif}

  public
    constructor Create(AIdentifiers: TCodeXYPositions);
    procedure Gather(Tool: TPascalReaderTool);
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

constructor TDiagnosticsHandler.Create;
begin
  inherited;

  fPublishDiagnostics := TPublishDiagnostics.Create;
end;

destructor TDiagnosticsHandler.Destroy;
begin
  fPublishDiagnostics.Free;
  
  inherited;
end;

Procedure TDiagnosticsHandler.AddUserDiagnostic(aTransport: TMessageTransport; UserMessage : String);

begin
  // Clear previous user message on new message
  fPublishDiagnostics.ClearUserMessages;
  // Message on stdErr
  aTransport.SendDiagnostic(UserMessage);
  // Actual diagnostic
  fPublishDiagnostics.AddUserMessage(
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


Procedure TDiagnosticsHandler.AddCodeToolError(aTransport: TMessageTransport);

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
    fPublishDiagnostics.AddCodeToolError(aFileName,
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
  fileName: string;

begin
  if UserMessage <> '' then
  begin
    AddUserDiagnostic(aTransport,UserMessage);
    fileName := '';
  end
  else 
  if (CodeToolBoss.ErrorCode<>Nil) then
    begin
      AddCodeToolError(aTransport);
      fileName:=CodeToolBoss.ErrorCode.FileName;
    end;
  if not ServerSettings.publishDiagnostics then
    exit;
  fPublishDiagnostics.SendDiagnostics(fileName, aTransport);
end;

Type

  { TErrorReporter }

  TErrorReporter = class
  private
    FErrorCount: Integer;
    FHandler : TDiagnosticsHandler;
    FParser : TSourceParser;
    FTransport : TMessageTransport;
  Protected
    procedure ReportError(Sender: TObject; const aError, aFileName: string; aCode, aLine, aCol: Integer);
  Public
    Constructor Create(aHandler : TDiagnosticsHandler; aParser : TSourceParser; aTransport : TMessageTransport);
    Property ErrorCount : Integer Read FErrorCount;
  end;

{ TErrorReporter }

constructor TErrorReporter.Create(aHandler: TDiagnosticsHandler;
  aParser: TSourceParser; 
  aTransport: TMessageTransport);
begin
  FHandler:=aHandler;
  FParser:=aParser;
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
    FHandler.AddParserError(aFileName,
                     aError,
                     aLine-1,
                     aCol-1,
                     aCode,
                     TDiagnosticSeverity.Error);
end;

function TDiagnosticsHandler.StrictSyntaxCheck(aTransport : TMessageTransport; Code: TCodeBuffer) : Boolean;

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
      Reporter:=TErrorReporter.Create(Self,SourceParser,aTransport);
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
  CodeOK : Boolean;

begin
  if not ServerSettings.checkSyntax then
    exit;
  // Check code. These routines will possibly send messages to a window or stdout, depending on settings.
  fPublishDiagnostics.ClearCodeToolErrors(Code.Filename);
  fPublishDiagnostics.ClearParserError(Code.Filename);
  
  CodeOk:=CodeToolsCheckSyntax(aTransport,Code);
  if CodeOK then
      CodeOK:=StrictSyntaxCheck(aTransport,Code);
  // If we need to publish settings, then send the diagnostics.
  if ServerSettings.publishDiagnostics then
    fPublishDiagnostics.SendDiagnostics(Code.Filename, aTransport);
end;

function TDiagnosticsHandler.CodeToolsCheckSyntax(aTransport : TMessageTransport; Code: TCodeBuffer): boolean;

var
  Tool: TCodeTool;
  Node: TCodeTreeNode;
  IdentifiersPos: TCodeXYPositions;
  Gatherer: TIdentifierGatherer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  i: Integer;

begin
  // Check for errors.
  Result:=CodeToolBoss.Explore(Code,Tool,true);

  if not Result then
      // Errors found ? Publish them.
      AddCodeToolError(aTransport);

  try
    IdentifiersPos := TCodeXYPositions.Create;
    Gatherer := TIdentifierGatherer.Create(IdentifiersPos);
    Gatherer.Gather(Tool);

    for i := 0 to IdentifiersPos.Count - 1 do
      begin
        with IdentifiersPos.Items[i]^ do
          begin
            if CodeToolBoss.FindMainDeclaration(Code, X, Y, NewCode, NewX, NewY, NewTopLine) then
              Continue
            else
              AddCodeToolError(aTransport);
          end;
      end
  finally
    Gatherer.Free;
    IdentifiersPos.Free;
  end;
end;

procedure TDiagnosticsHandler.AddParserError(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
begin
  fPublishDiagnostics.AddParserError(fileName, message, line, column, code, severity);
end;

constructor TIdentifierGatherer.Create(AIdentifiers: TCodeXYPositions);
begin
  FIdentifiers := AIdentifiers;
end;

{$if FPC_FULLVERSION >= 30301}
procedure TIdentifierGatherer.OnIdentifierFound(Sender: TPascalParserTool;
  IdentifierCleanPos: integer; Range: TEPRIRange;
  Node: TCodeTreeNode; Data: Pointer; var Abort: boolean;
  RefsStart: integer);
{$else}
procedure TIdentifierGatherer.OnIdentifierFound(Sender: TPascalParserTool;
  IdentifierCleanPos: integer; Range: TEPRIRange;
  Node: TCodeTreeNode; Data: Pointer; var Abort: boolean);
{$endif}


var
  IdentifierStr: string;
  CodeTool: TCodeTool;
  IdentifierPos: TCodeXYPosition;
  NewTopLine: Integer;
begin
  if not (Sender is TCodeTool) then
    Exit;
  
  CodeTool := TCodeTool(Sender);
  if CodeTool.CleanPosToCaretAndTopLine(IdentifierCleanPos, IdentifierPos, NewTopLine) then
    begin
      IdentifierStr := GetIdentifier(@Sender.Src[IdentifierCleanPos]);
      if IdentifierStr <> '' then
        begin
          FIdentifiers.Add(IdentifierPos);
        end;
    end;
end;
 
procedure TIdentifierGatherer.Gather(Tool: TPascalReaderTool);
begin
{$if FPC_FULLVERSION >= 30301}
  Tool.ForEachIdentifier(true, @OnIdentifierFound, nil, 0);
{$else}
  Tool.ForEachIdentifier(true, @OnIdentifierFound, nil);
{$endif}
end;

Initialization

Finalization
  _DiagnosticsHandler.Free;
end.

