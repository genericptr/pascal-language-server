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
  { Protocol }
  LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Messages;

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
    fUserMessages: TDiagnosticItems;
    fCodeToolErrors: TUriDiagnostics;
    fParserErrors: TUriDiagnostics;

    function GetDiagnosticParams: TPublishDiagnosticsParams;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SendDiagnostics(fileName: string; aTransport : TMessageTransport);
    Property DiagnosticParams : TPublishDiagnosticsParams Read GetDiagnosticParams;
    procedure AddCodeToolError(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure AddParserError(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure AddUserMessage(message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
    procedure ClearCodeToolErrors(fileName: string);
    procedure ClearParserError(fileName: string);
    procedure ClearUserMessages;
    procedure Clear(fileName: string);
  end;


implementation

uses SysUtils;

{ TPublishDiagnostics }

procedure TPublishDiagnostics.ClearUserMessages;
begin
  DiagnosticParams.uri := '';
  fUserMessages.Clear;
end;

procedure TPublishDiagnostics.ClearCodeToolErrors(fileName: string);
var
  CodeToolErrorsDiagnostics: TDiagnosticItems;
begin
  DiagnosticParams.uri := PathToURI(fileName);
  if not fCodeToolErrors.
    TryGetData(DiagnosticParams.uri, CodeToolErrorsDiagnostics)
  then
    begin
      CodeToolErrorsDiagnostics := TDiagnosticItems.Create;
      fCodeToolErrors.Add(DiagnosticParams.uri, CodeToolErrorsDiagnostics);
    end;

  CodeToolErrorsDiagnostics.Clear;
end;

procedure TPublishDiagnostics.ClearParserError(fileName: string);
var
  CodeToolErrorsDiagnostics: TDiagnosticItems;
begin
  DiagnosticParams.uri := PathToURI(fileName);
  if not fParserErrors.
    TryGetData(DiagnosticParams.uri, CodeToolErrorsDiagnostics)
  then
    begin
      CodeToolErrorsDiagnostics := TDiagnosticItems.Create;
      fParserErrors.Add(DiagnosticParams.uri, CodeToolErrorsDiagnostics);
    end;

  CodeToolErrorsDiagnostics.Clear;
end;

procedure TPublishDiagnostics.AddUserMessage(
    message: string;
    line, column, code: integer;
    severity: TDiagnosticSeverity
  );
var
  Diagnostic: TDiagnostic;
begin
  DiagnosticParams.uri := '';
  Diagnostic := fUserMessages.Add;
  Diagnostic.range.SetRange(line, column);
  Diagnostic.severity := severity;
  Diagnostic.code := code;
  Diagnostic.source := 'Free Pascal Compiler';
  Diagnostic.message := message;
end;

procedure TPublishDiagnostics.AddCodeToolError(
    fileName, message: string;
    line, column, code: integer;
    severity: TDiagnosticSeverity
  );
var
  CodeToolErrorsDiagnostics: TDiagnosticItems;
  Diagnostic: TDiagnostic;
  i: Integer;
begin
  DiagnosticParams.uri := PathToURI(fileName);
  if not fCodeToolErrors.
    TryGetData(DiagnosticParams.uri, CodeToolErrorsDiagnostics)
  then
    begin
      CodeToolErrorsDiagnostics := TDiagnosticItems.Create;
      fCodeToolErrors.Add(DiagnosticParams.uri, CodeToolErrorsDiagnostics);
    end;

  i := 0;
  while i < CodeToolErrorsDiagnostics.Count do
    begin
      Diagnostic := CodeToolErrorsDiagnostics.Items[i];
      if Diagnostic.range.InRange(line, column) then
        Break;
      Inc(i);
    end;

  if i >= CodeToolErrorsDiagnostics.Count then
    begin
      Diagnostic := CodeToolErrorsDiagnostics.Add;
    end;
    
  Diagnostic.range.SetRange(line, column);
  Diagnostic.severity := severity;
  Diagnostic.code := code;
  Diagnostic.source := 'Free Pascal Compiler';
  Diagnostic.message := message;
end;

procedure TPublishDiagnostics.AddParserError(
    fileName, message: string;
    line, column, code: integer;
    severity: TDiagnosticSeverity
  );
var
  CodeToolErrorsDiagnostics: TDiagnosticItems;
  Diagnostic: TDiagnostic;
begin
  DiagnosticParams.uri := PathToURI(fileName);
  if not fParserErrors.
    TryGetData(DiagnosticParams.uri, CodeToolErrorsDiagnostics)
  then
    begin
      CodeToolErrorsDiagnostics := TDiagnosticItems.Create;
      fParserErrors.Add(DiagnosticParams.uri, CodeToolErrorsDiagnostics);
    end;

  Diagnostic := CodeToolErrorsDiagnostics.Add;
  Diagnostic.range.SetRange(line, column);
  Diagnostic.severity := severity;
  Diagnostic.code := code;
  Diagnostic.source := 'Free Pascal Compiler';
  Diagnostic.message := message;
end;

procedure TPublishDiagnostics.Clear(fileName: string);
begin
  DiagnosticParams.uri := PathToURI(fileName);
  DiagnosticParams.diagnostics.Clear;
end;

procedure TPublishDiagnostics.Add(fileName, message: string; line, column, code: integer; severity: TDiagnosticSeverity);
var
  Diagnostic: TDiagnostic;
begin
  if Length(fileName) = 0 then
    DiagnosticParams.uri := ''
  else
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

procedure TPublishDiagnostics.SendDiagnostics(
    fileName: string;
    aTransport: TMessageTransport
  );
var
  Diagnostic, sentDiagnostic: TDiagnostic;
  IsHaveDiagnostics: Boolean;

  procedure IterateDiagnosticItems(uriDiagnostics: TUriDiagnostics);
  var
    DiagnosticItems: TDiagnosticItems;
  begin
    if not uriDiagnostics.
      TryGetData(PathToURI(fileName), DiagnosticItems) 
    then
      begin
        DiagnosticItems := TDiagnosticItems.Create;
        uriDiagnostics.Add(PathToURI(fileName), DiagnosticItems);
      end;

    for TCollectionItem(Diagnostic) in DiagnosticItems do
      begin
        if not IsHaveDiagnostics then
          begin
            IsHaveDiagnostics := True;
            Clear(fileName);
          end;
        sentDiagnostic := DiagnosticParams.diagnostics.Add;
        sentDiagnostic.Assign(Diagnostic);
      end;
  end;
begin
  IsHaveDiagnostics := False;
  DiagnosticParams.diagnostics.Clear;
    // loop over all fCodeToolErrors[fileName] and fParserErrors[fileName]
    // add to DiagnosticParams.diagnostics
  IterateDiagnosticItems(fCodeToolErrors);
  IterateDiagnosticItems(fParserErrors);

    // if fUserMessages.count > 0 add to DiagnosticParams.diagnostics
  if Length(fileName) = 0 then
    for TCollectionItem(Diagnostic) in fUserMessages do
      begin
        if not IsHaveDiagnostics then
          begin
            IsHaveDiagnostics := True;
            Clear(fileName);
          end;
        sentDiagnostic := DiagnosticParams.diagnostics.Add;
        sentDiagnostic.Assign(Diagnostic);
      end;

  Send(aTransport);
end;

constructor TPublishDiagnostics.Create;
begin  
  fUserMessages := TDiagnosticItems.Create;
  fCodeToolErrors := TUriDiagnostics.Create(True);
  fParserErrors := TUriDiagnostics.Create(True);
  
  params := TPublishDiagnosticsParams.Create;
  method := 'textDocument/publishDiagnostics';
end;

destructor TPublishDiagnostics.Destroy; 
begin
  params.Free;
  fCodeToolErrors.Free;
  fUserMessages.Free;
  fParserErrors.Free;
  
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
