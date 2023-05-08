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
  Classes, SysUtils, LSP.Base, LSP.Messages, fpjson;

Type

  { TTextLSPContext }
  PText = ^Text;

  { TLSPTextTransport }

  TLSPTextTransport = class(TMessageTransport)
    FOutput : PText;
    FError : PText;
  Protected
    Procedure DoSendMessage(aMessage: TJSONData); override;
    Procedure DoSendDiagnostic(const aMessage: UTF8String); override;
  Public
    constructor Create(aOutput,aError : PText); reintroduce;
    Procedure EmitMessage(aMessage: TJSONStringType);
  end;



Procedure SetupTextLoop(var aInput,aOutput,aError : Text);
Procedure RunMessageLoop(var aInput,aOutput,aError : Text; aContext : TLSPContext);
procedure DebugSendMessage(var aFile : Text; aContext : TLSPContext; const aMethod, aParams: String);

implementation

Procedure SetupTextLoop(var aInput,aOutput,aError : Text);

begin
  TJSONData.CompressedJSON := True;
  SetTextLineEnding(aInput, #13#10);
  SetTextLineEnding(aOutput, #13#10);
  SetTextLineEnding(aError, #13#10);
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

Procedure SendResponse(aTransport : TMessageTransport; aContext : TLSPContext; aResponse : TJSONData; aFreeResponse : Boolean = True);

Var
  Content : TJSONStringType;

begin
  try
    if not IsResponseValid(aResponse) then
      begin
      aContext.Log('Response not valid: %s',[aResponse.AsJSON]);
      aTransport.SendDiagnostic('invalid response -> '+aResponse.AsJSON);
      exit;
      end;
    Content := aResponse.AsJSON;
    (aTransport as TLSPTextTransport).EmitMessage(Content);
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
        SendResponse(aContext.Transport, aContext, Response,True);
        end
      else
        aContext.Log('No response to request');
      FreeAndNil(Request);
      end;
  finally
    Request.Free;
  end;
end;

{ TTextLSPContext }

constructor TLSPTextTransport.Create(aOutput, aError: PText);
begin
  FOutput:=aOutput;
  FError:=aError;
end;

procedure TLSPTextTransport.EmitMessage(aMessage: TJSONStringType);
begin
  Try
    WriteLn(Foutput^,'Content-Type: ', ContentType);
    WriteLn(Foutput^,'Content-Length: ', Length(aMessage));
    WriteLn(Foutput^);
    Write(Foutput^,aMessage);
    Flush(Foutput^);
  except
    on e : exception do
      DoLog('Exception %s during output: %s',[E.ClassName,E.Message]);
  end;
end;

procedure TLSPTextTransport.DoSendMessage(aMessage: TJSONData);

Var
  Content : TJSONStringType;

begin
  Content:=aMessage.AsJSON;
  EmitMessage(Content);
end;

procedure TLSPTextTransport.DoSendDiagnostic(const aMessage: UTF8String);
begin
  Try
    WriteLn(FError^,aMessage);
    Flush(FError^);
  except
    on e : exception do
      DoLog('Exception %s during diagnostic output: %s',[E.ClassName,E.Message]);
  end;
end;



end.

