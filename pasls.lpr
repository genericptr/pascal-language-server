// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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

program pasls;

{$mode objfpc}{$H+}

uses
  { RTL }
  SysUtils, fpjson, jsonparser, jsonscanner,

  { LSP }
  lsp, general,

  { Protocols }
  basic, synchronization, completion, gotoDeclaration, gotoDefinition, 
  gotoImplementation, hover, signatureHelp, references, codeAction, 
  documentHighlight, documentSymbol, workspace, window;

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';

type
  TTestNotification = class(specialize TLSPNotification<TShowMessageParams>)
    procedure Process(var Params: TShowMessageParams); override;
  end;

procedure TTestNotification.Process(var Params: TShowMessageParams);
begin
  writeln('got params: ', Params.ClassName)
end;

procedure TestNotifications;
var
  params: TShowMessageParams;
  notification: TTestNotification;
  data: TJSONData;
  Dispatcher: TLSPDispatcher;
begin
  params := TShowMessageParams.Create;
  params.&type := TMessageType.Error;
  params.message := 'Some Error Message';

  Dispatcher := TLSPDispatcher.Create(nil);

  notification := TTestNotification.Create(nil);
  //function TLSPDispatcher.ExecuteMethod(const AClassName, AMethodName: TJSONStringType;
  //  Params, ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData;

  data := specialize TLSPStreaming<TShowMessageParams>.ToJSON(params);
  writeln(data.AsJSON);
  data := Dispatcher.Execute(data);
  if data <> nil then
    writeln(data.AsJSON);
end;

var
  Dispatcher: TLSPDispatcher;
  Header, Name, Value, Content: string;
  I, Length: Integer;
  Request, Response: TJSONData;
  ShowMessage: TShowMessageNotification;
  VerboseDebugging: boolean = false;
begin
  //TestNotifications;
  //halt;
  Dispatcher := TLSPDispatcher.Create(nil);
  TJSONData.CompressedJSON := True;
  SetTextLineEnding(Input, #13#10);
  SetTextLineEnding(Output, #13#10);
  
  while not EOF do
  begin
    ReadLn(Header);
    while Header <> '' do
    begin
      I := Pos(':', Header);
      Name := Copy(Header, 1, I - 1);
      Delete(Header, 1, i);
      Value := Trim(Header);
      if Name = 'Content-Length' then Length := StrToInt(Value);
      ReadLn(Header);
    end;

    Content := '';
    SetLength(Content, Length);
    I := 1;
    while I <= Length do
    begin
      Read(Content[I]);
      Inc(I);
    end;

    Request := TJSONParser.Create(Content, DefaultOptions).Parse;

    // log request payload
    if VerboseDebugging then
      begin
        writeln(StdErr, Request.FormatJSON);
        Flush(StdErr);
      end;
      
    Response := Dispatcher.Execute(Request);
    if Assigned(Response) then
    begin
      if not IsResponseValid(Response) then
        begin
          Writeln(StdErr, 'invalid response -> ', response.AsJSON);
          Flush(StdErr);
          continue;
        end;
      Content := Response.AsJSON;
      WriteLn('Content-Type: ', ContentType);
      WriteLn('Content-Length: ', Content.Length);
      WriteLn;
      Write(Content);
      Flush(Output);
      
      // log response payload
      if VerboseDebugging then
        begin
          writeln(StdErr, Content);
          Flush(StdErr);
        end;

      Response.Free;
    end;

    Request.Free;
  end;
end.
