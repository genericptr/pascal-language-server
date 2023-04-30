program paslssock;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, IniFiles, LSP.AllCommands,
  PasLSSock.Config, PasLS.SocketDispatcher;

type

  { TPasLSPSocketServerApp }

  TPasLSPSocketServerApp = class(TCustomApplication)
  Private
    FConfig : TLSPSocketServerConfig;
    function ParseOptions: Boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aError: String); virtual;
  end;


{ TPasLSPSocketServerApp }

function TPasLSPSocketServerApp.ParseOptions : Boolean;

var
  FN : String;
begin
  Result:=False;
  FN:=GetOptionValue('c','config');
  if FN='' then
    FN:=TLSPSocketServerConfig.DefaultConfigFile;
  FConfig.LoadFromFile(FN);
{$IFDEF UNIX}
  if HasOption('u','unix') then
    FConfig.Unix:=GetOptionValue('u','unix');
{$ENDIF}
  if HasOption('p','port') then
    FConfig.Port:=StrToInt(GetOptionValue('p','port'));
  if HasOption('t','threaded') then
    FConfig.Threaded:=True;
  if HasOption('s','single-connect') then
    FConfig.SingleConnect:=True;
  Result:=True;
end;

procedure TPasLSPSocketServerApp.DoRun;

Const
  ShortOpts = 'hp:u:c:ts';
  LongOpts : array of string = ('help','port','unix','config','threaded','single-connect');


var
  ErrorMsg: String;
  Disp : TLSPServerSocketDispatcher;

begin
  Terminate;
  // quick check parameters
  ErrorMsg:=CheckOptions(ShortOpts,LongOpts);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  if not ParseOptions then
    exit;
  if FConfig.Port>0 then
    Disp:=TLSPServerTCPSocketDispatcher.Create(FConfig.Port)
  else
    Disp:=TLSPServerUnixSocketDispatcher.Create(FConfig.Unix);
  Try
    Disp.SingleConnect:=FConfig.SingleConnect;
    Disp.InitSocket;
    Disp.RunLoop;
  finally
    Disp.Free;
  end;
end;

constructor TPasLSPSocketServerApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FConfig:=TLSPSocketServerConfig.Create;
end;

destructor TPasLSPSocketServerApp.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure TPasLSPSocketServerApp.Usage(const aError : String);
begin
  if aError<>'' then
    Writeln('Error : ',aError);
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h  --help           This help message');
  Writeln('-c  --config=FILE    Read configuration from file FILE. Default is to read from ',TLSPSocketServerConfig.DefaultConfigFile);
  Writeln('-p  --port=NNN       Listen on port NNN');
  Writeln('-s  --single-connect Handle one connection and then exit');
  Writeln('-t  --threaded       Use threading for connections.');
  Writeln('-u  --unix=FILE      Listen on unix socket FILE (only on unix-like systems)');
  Writeln('Only one of -p or -u may be specified, if none is specified then the default is to listen on port 9898');
end;

var
  Application: TPasLSPSocketServerApp;
begin
  Application:=TPasLSPSocketServerApp.Create(nil);
  Application.Title:='Pascal LSP socket server application';
  Application.Run;
  Application.Free;
end.
