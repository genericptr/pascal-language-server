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
  basic, synchronization, completion, hover, gotoDeclaration, 
  gotoImplementation, signatureHelp, references, codeAction,
  diagnostics;

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';

var
  Dispatcher: TLSPDispatcher;
  Header, Name, Value, Content: string;
  I, Length: Integer;
  Request, Response: TJSONData;
begin
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
    Response := Dispatcher.Execute(Request);
    if Assigned(Response) then
    begin
      Content := Response.AsJSON;
      WriteLn('Content-Type: ', ContentType);
      WriteLn('Content-Length: ', Content.Length);
      WriteLn;
      Write(Content);
      Flush(Output);
    end;
  end;
end.
