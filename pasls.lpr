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
  documentHighlight, documentSymbol, workspace, window, executeCommand;

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';

{$macro on}
{$if FPC_FULLVERSION>=30301}
var
  LastMessageID: LongInt = 0;

procedure SendMessage(dispatcher: TLSPDispatcher; method, params: String);
var
  Content: String;
  Request, Response: TJSONData;
begin
  writeln('▶️ ', method);
  Content := '{"jsonrpc": "2.0","id": '+LastMessageID.ToString+', "method": "'+method+'","params": '+params+'}';
  Inc(LastMessageID);

  Request := TJSONParser.Create(Content, DefaultOptions).Parse;

  Response := Dispatcher.Execute(Request);
  if Assigned(Response) then
    begin
      writeln(Response.FormatJSON);
      Response.Free;
    end;

  Request.Free;
  Dispatcher.Free;
end;

procedure ExecuteCommandLineMessages;
var
  i: integer;
  method, path: String;
  dispatcher: TLSPDispatcher;
begin
  if ParamCount > 0 then
    begin
      i := 1;
      dispatcher := TLSPDispatcher.Create(nil);

      while i <= ParamCount do
        begin
          if ParamCount < i + 1 then
            begin
              writeln('Invalid parameter count of '+ParamCount.ToString+' (must be pairs of 2)');
              Halt(1);
            end;

          method := ParamStr(i + 0);
          path := ExpandFileName(ParamStr(i + 1));
          if not FileExists(path) then
            begin
              writeln('Command path "'+path+'" can''t be found');
              Halt(1);
            end;

          SendMessage(dispatcher, method, GetFileAsString(path));
          Inc(i, 2);
        end;
      Halt;
    end;
end;
{$endif}

var
  Dispatcher: TLSPDispatcher;
  Header, Name, Value, Content: string;
  I, Length: Integer;
  Request, Response: TJSONData;
  ShowMessage: TShowMessageNotification;
  VerboseDebugging: boolean = false;
begin
  // Show help for the server
  if ParamStr(1) = '-h' then
    begin
      writeln('Pascal Language Server [',{$INCLUDE %DATE%},']');
      Halt;
    end;

  {$if FPC_FULLVERSION>=30301}
  ExecuteCommandLineMessages;
  {$endif}

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
