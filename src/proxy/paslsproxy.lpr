program paslsproxy;

// Pascal Language Server proxy dispatcher
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
  ssockets, custapp, types,

  { LSP }

  LSP.Base, PasLS.TextLoop, PasLS.SocketDispatcher,

  { Pasls }
  memUtils, PasLSProxy.Config;

Type

  { TLSPProxyApplication }

  TLSPProxyApplication = Class(TCustomApplication)
  Private
    const
      ShortOptions = 'htp:u:c:l:';
      LongOptions : Array of string = ('help','test','port:','unix:','config:','log:');
  Private
    FConfig : TLSPProxyConfig;
    function ParseOptions(out aParams : TStringDynArray): Boolean;
    function SetupSocket: TSocketStream;
  protected
    procedure DoRun; override;
    Function ExecuteCommandLineMessages(aContext : TLSPContext; aParams : Array of string) : Boolean;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aError: String); virtual;
  end;

function TLSPProxyApplication.ExecuteCommandLineMessages(aContext: TLSPContext;
  aParams: array of string): Boolean;

var
  i, len: integer;
  method, path : String;

begin
  Result:=True;
  len:=Length(aParams);
  if len =0 then
    exit;
  if (Len mod 2)= 1 then
    begin
    writeln('Invalid parameter count of '+ParamCount.ToString+' (must be pairs of 2)');
    Exit(false);
    end;
  I:=1;
  while (i<=Len) do
    begin
    method := aParams[i];
    path := ExpandFileName(aParams[i+1]);
    if not FileExists(path) then
      begin
      writeln('Command path "',path,'" can''t be found');
      exit(false)
      end;
    DebugSendMessage(output,aContext, method, GetFileAsString(path));
    Inc(i, 2);
    end;
end;

constructor TLSPProxyApplication.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);
  FConfig:=TLSPProxyConfig.Create;
  StopOnException:=True;
end;

destructor TLSPProxyApplication.Destroy;
begin
  FreeAndNil(FConfig);
  inherited Destroy;
end;

procedure TLSPProxyApplication.Usage(const aError: String);

begin
  Writeln('Pascal Language Server Proxy [',{$INCLUDE %DATE%},']');
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h  --help           This help message');
  Writeln('-c  --config=FILE    Read configuration from file FILE. Default is to read from ',TLSPProxyConfig.DefaultConfigFile);
  Writeln('-l  --log=FILE       Set log file in which to write all log messages');
  Writeln('-p  --port=NNN       Listen on port NNN (default: ',DefaultSocketPort);
  Writeln('-t  --test           Interpret non-option arguments as call/param file pairs and send to server');
  Writeln('-u  --unix=FILE      Listen on unix socket FILE (only on unix-like systems. Default: ',DefaultSocketUnix,')');
  Writeln('Only one of -p or -u may be specified, if none is specified then the default is to listen on port 9898');
end;


function TLSPProxyApplication.ParseOptions(out aParams: TStringDynArray): Boolean;
var
  FN : String;
begin
  Result:=False;
  FN:=GetOptionValue('c','config');
  if FN='' then
    FN:=TLSPProxyConfig.DefaultConfigFile;
  FConfig.LoadFromFile(FN);
{$IFDEF UNIX}
  if HasOption('u','unix') then
    FConfig.Unix:=GetOptionValue('u','unix');
{$ENDIF}
  if HasOption('p','port') then
    FConfig.Port:=StrToInt(GetOptionValue('p','port'));
  if HasOption('l','log') then
    FConfig.LogFile:=GetOptionValue('l','log');
  if HasOption('t','test') then
    aParams:=GetNonOptions(ShortOptions,LongOptions)
  else
    aParams:=[];
  Result:=True;
end;

function TLSPProxyApplication.SetupSocket: TSocketStream;


begin
  Result:=Nil;
  SetupTextLoop;
  TLSPContext.LogFile:=FConfig.LogFile;
{$IFDEF UNIX}
  // Todo: Add some code to start the socket server, e.g. when the file does not exist.
  if FConfig.Unix<>'' then
    Result:=TUnixSocket.Create(FConfig.Unix);
{$ENDIF}
  if Result=Nil then
    Result:=TInetsocket.Create('127.0.0.1',FConfig.Port);
end;

procedure TLSPProxyApplication.DoRun;

var
  aContext : TLSPContext;
  aSocket : TSocketStream;
  aMsg : String;
  lParams : TStringDynArray;

begin
  Terminate;
  lParams:=[];
  aMsg:=CheckOptions(ShortOptions, LongOptions);
  if HasOption('h','help') then
    begin
    Usage(aMsg);
    exit;
    end;
  if not ParseOptions(lParams) then
    exit;
  aSocket:=SetupSocket;
  try
    aContext:=TLSPContext.Create(TLSPClientSocketDispatcher.Create(aSocket),True);
    if length(lParams)>0 then
      if not ExecuteCommandLineMessages(aContext,lParams) then
        exit;
    RunMessageLoop(Input,Output,StdErr,aContext);
  Finally
    aContext.Free;
    aSocket.Free;
    DrainAutoReleasePool;
  end;
end;

var
  Application: TLSPProxyApplication;

begin
  Application:=TLSPProxyApplication.Create(nil);
  Application.Title:='Pascal LSP Server proxy application';
  Application.Run;
  Application.Free;
end.

end.

