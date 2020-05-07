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
  TestCodeTools,
  { RTL }
  SysUtils, fpjson, jsonparser, jsonscanner,

  { LSP }
  lsp, general, 

  { Protocols }
  basic, synchronization, completion, hover, gotoDeclaration, 
  gotoImplementation, signatureHelp, references, codeAction,
  documentHighlight, documentSymbol, workspace;

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';

var
  Dispatcher: TLSPDispatcher;
  Header, Name, Value, Content: string;
  I, Length: Integer;
  Request, Response: TJSONData;
begin
  ////TestDocumentSymbols;
  //// '/Users/ryanjoseph/Developer/ObjectivePascal/MacOS_10_10'
  ////TestProjectSymbols(['/Users/ryanjoseph/Developer/ObjectivePascal/iOS_8_0']);
  //TestProjectSymbols(['/Users/ryanjoseph/Desktop/FPCLS-Test']);
  //TestProjectSymbols([
  //  '/Users/ryanjoseph/Desktop/Projects/Games/ProceduralRPG',
  //  '/Users/ryanjoseph/Developer/Projects/FPC/GLCanvas',
  //  '/Users/ryanjoseph/Developer/Projects/FPC/GLPT',
  //  '/Users/ryanjoseph/Desktop/Projects/Games/RPGEngine/engine',
  //  '/Users/ryanjoseph/Desktop/Projects/Games/RPGEngine/rpg',
  //  '/Users/ryanjoseph/Desktop/Projects/Games/ProceduralRPG/sources'
  //  ]);
  //exit;

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
      // invalid responses without id's or null id's must not be sent to server, i.e:
      // {"jsonrpc":"2.0","error":{"code":-32603,"message":"Access violation"},"id":null}
      if (Response is TJSONObject) and 
        ((TJSONObject(Response).Find('id') = nil) or 
          TJSONObject(Response).Nulls['id']) then
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
    end;
  end;
end.
