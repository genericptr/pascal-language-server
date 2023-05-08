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

program pasls;

{$mode objfpc}{$H+}

uses
  { RTL }

  SysUtils, Classes, FPJson, JSONParser, JSONScanner,
  { Protocol }
  LSP.AllCommands, PasLS.Settings,
  LSP.Base, LSP.Basic, PasLS.TextLoop, PasLS.LSConfig;

Type

  { TLSPLogContext }

  TLSPLogContext = Class(TLSPContext)
  Public
    procedure DoTransportLog(sender : TObject; Const Msg : UTF8String);

  end;

Function ExecuteCommandLineMessages(aContext : TLSPContext) : Boolean;

var
  i: integer;
  method, path : String;

begin
  Result:=True;
  if ParamCount=0 then
    exit;
  if (ParamCount div 2)= 1 then
    begin
    writeln('Invalid parameter count of '+ParamCount.ToString+' (must be pairs of 2)');
    Exit(false);
    end;
  TLSPContext.Log('Command-line Message loop');
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

Procedure ConfigEnvironment(aConfig : TLSPServerConfig);

begin
  With EnvironmentSettings do
    begin
    pp:=aConfig.Compiler;
    fpcDir:=aConfig.FPCDir;
    lazarusDir:=aConfig.LazarusDir;
    fpcTarget:=aConfig.TargetOS;
    fpcTargetCPU:=aConfig.TargetCPU;
    end;
end;

var
  aTransport: TLSPTextTransport;
  aContext: TLSPLogContext;
  aDisp: TLSPLocalDispatcher;
  aCfg: TLSPServerConfig;

{ TLSPLogContext }

procedure TLSPLogContext.DoTransportLog(sender: TObject; const Msg: UTF8String);
begin
  Log('Transport log: '+Msg);
end;


begin
  // Show help for the server
  if ParamStr(1) = '-h' then
    begin
    writeln('Pascal Language Server [',{$INCLUDE %DATE%},']');
    Halt;
    end;
  aContext := nil;
  aCfg := TLSPServerConfig.Create;
  try
    aCfg.LoadFromFile(aCfg.DefaultConfigFile);
    if aCfg.LogFile<>'' then
      TLSPContext.LogFile := aCfg.LogFile;
    ConfigEnvironment(aCfg);
    SetupTextLoop(Input,Output,StdErr);
    aTransport:=TLSPTextTransport.Create(@Output,@StdErr);
    aDisp:=TLSPLocalDispatcher.Create(aTransport,True);
    aContext:=TLSPLogContext.Create(aTransport,aDisp,True);
    aTransport.OnLog := @aContext.DoTransportLog;
    if not ExecuteCommandLineMessages(aContext) then
      exit;
    RunMessageLoop(Input,Output,StdErr,aContext);
   Finally
     aContext.Free;
     aCfg.Free;
   end;
end.
