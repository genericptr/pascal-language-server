program paslsproxy;

// Pascal Language Server dispatcher
// Copyright 2023 Michael Van Canneyt

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

{$mode objfpc}{$H+}

uses
  { RTL }
  SysUtils, Classes, fpjson, jsonparser, jsonscanner,

  { LSP }

  LSP.Base, PasLS.TextLoop, PasLS.SocketDispatcher,

  ssockets,

  { Pasls }
  memUtils;


Function ExecuteCommandLineMessages(aContext : TLSPContext) : Boolean;

var
  i: integer;
  method, path : String;

begin
  Result:=True;
  if ParamCount=0 then
    exit;
  if (ParamCount mod 2)= 1 then
    begin
    writeln('Invalid parameter count of '+ParamCount.ToString+' (must be pairs of 2)');
    Exit(false);
    end;
  I:=1;
  while i <= ParamCount do
    begin
    method := ParamStr(i);
    path := ExpandFileName(ParamStr(i + 1));
    if not FileExists(path) then
      begin
      writeln('Command path "',path,'" can''t be found');
      exit(false)
      end;
    DebugSendMessage(output,aContext, method, GetFileAsString(path));
    Inc(i, 2);
  end;
end;

Function SetupSocket : TSocketStream;

begin
  // Todo: Add some code to start the socket server, e.g. when the file does not exist.
  Result:=TInetsocket.Create('127.0.0.1',9898);
end;

var
  aContext : TLSPContext;
  aSocket : TSocketStream;

begin
  // Show help for the server
  if ParamStr(1) = '-h' then
    begin
    writeln('Pascal Language Server [',{$INCLUDE %DATE%},']');
    Halt;
    end;
  SetupTextLoop;
  aSocket:=SetupSocket;
  try
    aContext:=TLSPContext.Create(TLSPClientSocketDispatcher.Create(aSocket),True);
    if not ExecuteCommandLineMessages(aContext) then
      exit;
    RunMessageLoop(Input,Output,StdErr,aContext);
   Finally
     aContext.Free;
     aSocket.Free;
     DrainAutoReleasePool;
   end;
end.

