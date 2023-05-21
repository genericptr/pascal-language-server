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
  {$ifdef unix}
  cwstring,
  {$endif}
  { RTL }
  SysUtils, Classes, fpjson, jsonparser, jsonscanner,
  ssockets, custapp, types,

  { LSP }

  LSP.Messages, LSP.Base, PasLS.TextLoop, PasLS.SocketDispatcher,

  { Pasls }
   PasLSProxy.Config;

Type

  { TLSPProxyApplication }

  TLSPProxyApplication = Class(TCustomApplication)
  Private
    const
      ShortOptions = 'htp:u:c:l:';
      LongOptions : Array of string = ('help','test','port:','unix:','config:','log:');
    procedure DoHandleDiagnostic(Sender: TObject; const aFrame: TLSPFrame);
  Private
    FConfig : TLSPProxyConfig;
    FContext : TLSPContext;
    function ParseOptions(out aParams : TStringDynArray): Boolean;
    function SetupTransport: TLSPSocketTransport;
  protected
    procedure DoRun; override;
    Function ExecuteCommandLineMessages(aContext : TLSPContext; aParams : Array of string) : Boolean;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aError: String); virtual;
  end;

procedure TLSPProxyApplication.DoHandleDiagnostic(Sender: TObject; const aFrame: TLSPFrame);

Type
  PFile = ^Text;

var
  aFile : PFile;
  aMsg : String;

begin
  aMsg:=aFrame.PayloadString;
  FContext.Log('Out of band message of type %s: %s',[aFrame.MessageType.AsString,aMsg]);
  case aFrame.MessageType of
    lptmDiagnostic:
      begin
      aFile:=@StdErr;
      aMsg:=aMsg+sLineBreak;
      end;
    lptmMessage:
      begin
      aFile:=@Output;
      WriteLn(aFile^,'Content-Type: ', ContentType);
      WriteLn(aFile^,'Content-Length: ', Length(aMsg));
      WriteLn(aFile^);
      end;
  else
    aFile:=Nil;
  end;
  Write(aFile^,aMsg);
  Flush(aFile^);
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
    writeln(StdErr,'Invalid parameter count of '+ParamCount.ToString+' (must be pairs of 2)');
    Exit(false);
    end;
  I:=0;
  while (i<Len) do
    begin
    method := aParams[i];
    path := ExpandFileName(aParams[i+1]);
    if not FileExists(path) then
      begin
      writeln(StdErr,'Command path "',path,'" can''t be found');
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
  if aError<>'' then
    Writeln('Error: ',aError);
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
  ExitCode:=Ord(aError<>'');
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

function TLSPProxyApplication.SetupTransport: TLSPSocketTransport;

var
  aSock : TSocketStream;

begin
  Result:=Nil;
  aSock:=Nil;
  SetupTextLoop(Input,Output,StdErr);
  TLSPContext.LogFile:=FConfig.LogFile;
{$IFDEF UNIX}
  // Todo: Add some code to start the socket server, e.g. when the file does not exist.
  if FConfig.Unix<>'' then
    aSock:=TUnixSocket.Create(FConfig.Unix);
{$ENDIF}
  if aSock=Nil then
    aSock:=TInetsocket.Create('127.0.0.1',FConfig.Port);
  Result:=TLSPSocketTransport.Create(aSock);
  Result.OnHandleFrame:=@DoHandleDiagnostic;
end;

procedure TLSPProxyApplication.DoRun;

var
  aMsg : String;
  lParams : TStringDynArray;
  aTrans : TLSPSocketTransport;
  aDisp : TLSPClientSocketDispatcher;

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
  aTrans:=SetupTransport;
  try
    aDisp:=TLSPClientSocketDispatcher.Create(aTrans);
    FContext:=TLSPContext.Create(aTrans,aDisp,True);
    if length(lParams)>0 then
      if not ExecuteCommandLineMessages(FContext,lParams) then
        exit;
    RunMessageLoop(Input,Output,StdErr,FContext);
  Finally
    FreeAndNil(FContext);
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

