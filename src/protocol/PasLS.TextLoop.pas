// Pascal Language Server
// Copyright 2023 Michael Van Canneyt

// LSP Text/File based protocol - in particular, Standard Input/Output/Error files.

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

unit PasLS.TextLoop;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LSP.Base, fpjson;

Procedure SetupTextLoop;
Procedure RunMessageLoop(var aInput,aOutput,aError : Text; aContext : TLSPContext);
procedure DebugSendMessage(var aFile : Text; aContext : TLSPContext; const aMethod, aParams: String);

implementation

Procedure SetupTextLoop;

begin
  TJSONData.CompressedJSON := True;
  SetTextLineEnding(Input, #13#10);
  SetTextLineEnding(Output, #13#10);
end;


procedure DebugSendMessage(var aFile : Text; aContext : TLSPContext; const aMethod, aParams: String);

var
  Content: TJSONStringType;
  Request: TJSONData;
  Response: TJSONData;

begin
  Response:=Nil;
  Writeln(aFile,'▶️ ', aMethod);
  Content := '{"jsonrpc": "2.0","id": '+aContext.NextMessageID.ToString+', "method": "'+aMethod+'","params": '+aParams+'}';
  Request := GetJSON(Content, True);
  try
    Response := aContext.Execute(Request);
    if Assigned(Response) then
      begin
      writeln(aFile,'◀️ response: ');
      writeln(aFile,Response.FormatJSON);
      Flush(aFile);
      end;
  finally
    Request.Free;
    Response.Free;
  end;
end;



Function ReadRequest(var aFile : text; aContext : TLSPContext) : TJSONData;

Var
  Header,Name,Value: String;
  Content : TJSONStringType;
  I,ContentLength : Integer;
  P : PJSONCharType;

begin
  Result:=Nil;
  aContext.Log('Reading request');
  ReadLn(aFile,Header);
  while Header <> '' do
    begin
      aContext.Log('Read header: %s',[Header]);
      I := Pos(':', Header);
      Name := Copy(Header, 1, I - 1);
      Delete(Header, 1, i);
      Value := Trim(Header);
      if Name = 'Content-Length' then
        ContentLength := StrToIntDef(Value,0);
      ReadLn(aFile,Header);
    end;
  Content:='';
  SetLength(Content,ContentLength);
  P:=PJSONCharType(Content);
  for I:=1 to ContentLength do
    begin
    Read(aFile,P^);
    inc(P);
    end;
  if Content<>'' then
    Result:=GetJSON(Content, True);
end;

Procedure SendResponse(var aFile,aError : text; aContext : TLSPContext; aResponse : TJSONData; aFreeResponse : Boolean = True);

Var
  Content : TJSONStringType;

begin
  try
    if not IsResponseValid(aResponse) then
      begin
      aContext.Log('Response not valid: %s',[aResponse.AsJSON]);
      Writeln(aError, 'invalid response -> ', aResponse.AsJSON);
      Flush(aError);
      exit;
      end;
    Content := aResponse.AsJSON;
    WriteLn(aFile,'Content-Type: ', LSPContentType);
    WriteLn(aFile,'Content-Length: ', Length(Content));
    WriteLn(aFile);
    Write(aFile,Content);
    Flush(aFile);
    aContext.Log('Wrote response to request');
  finally
    if aFreeResponse then
      aResponse.Free;
  end;
end;

Procedure RunMessageLoop(var aInput,aOutput,aError : Text; aContext : TLSPContext);

var
  Request, Response: TJSONData;
  VerboseDebugging: boolean = false;

begin
  Request:=Nil;
  try
    while not EOF(aInput) do
      begin
      Request:=ReadRequest(aInput,aContext);
      // log request payload
      if VerboseDebugging then
        begin
          Writeln(aError, Request.FormatJSON);
          Flush(aError);
        end;
      Response := aContext.Execute(Request);
      if Assigned(Response) then
        begin
        // log response payload
        if VerboseDebugging then
          begin
          writeln(aError, Response.asJSON);
          Flush(aError);
          end;
        SendResponse(aOutput,aError,aContext, Response,True);
        end
      else
        aContext.Log('No response to request');
      FreeAndNil(Request);
      end;
  finally
    Request.Free;
  end;
end;



end.

