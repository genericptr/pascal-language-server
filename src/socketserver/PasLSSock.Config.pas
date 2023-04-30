unit PasLSSock.Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Inifiles;

Const
  DefaultSocketUnix = '';
  DefaultSocketPort = 9898;
  DefaultSingleConnect = False;
  DefaultThreaded = False;

Type
  { TLSPSocketServerConfig }

  TLSPSocketServerConfig = Class(TObject)
  private
    FPort: Word;
    FSingleConnect: Boolean;
    FThreaded: Boolean;
    FUnix: String;
  Public
    Constructor Create; virtual;
    Procedure Reset; virtual;
    class Function DefaultConfigFile : String;
    Procedure LoadFromFile(const aFileName : String);
    Procedure SaveToFile(const aFileName : String);
    Procedure LoadFromIni(aIni : TCustomIniFile); virtual;
    Procedure SaveToIni(aIni : TCustomIniFile); virtual;
  Public
    Property Port : Word Read FPort Write FPort;
    Property Unix : String Read FUnix Write FUnix;
    Property SingleConnect : Boolean Read FSingleConnect Write FSingleConnect;
    Property Threaded : Boolean Read FThreaded Write FThreaded;
  end;


implementation

Const
  SServer = 'Server';
  KeyPort = 'Port';
  KeyUnix = 'Unix';
  KeySingleConnect = 'SingleConnect';
  KeyThreaded = 'Threaded';

{ TLSPSocketServerConfig }

constructor TLSPSocketServerConfig.Create;
begin
  Reset;
end;

procedure TLSPSocketServerConfig.Reset;
begin
  FPort:=DefaultSocketPort;
  FUnix:=DefaultSocketUnix;
  FSingleConnect:=DefaultSingleConnect;
  FThreaded:=DefaultThreaded;
end;

class function TLSPSocketServerConfig.DefaultConfigFile: String;
begin
{$IFNDEF UNIX}
  Result:='/etc/paslssock.cfg';
{$ELSE}
  Result:=ChangeFileExt(ParamStr(0),'.ini');
{$ENDIF}
end;

procedure TLSPSocketServerConfig.LoadFromFile(const aFileName: String);

Var
  Ini : TCustomIniFile;

begin
  Ini:=TMemIniFile.Create(aFileName);
  try
    LoadFromIni(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TLSPSocketServerConfig.SaveToFile(const aFileName: String);
Var
  Ini : TCustomIniFile;

begin
  Ini:=TMemIniFile.Create(aFileName);
  try
    SaveToIni(Ini);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TLSPSocketServerConfig.LoadFromIni(aIni: TCustomIniFile);
begin
  With aIni do
    begin
    FPort:=ReadInteger(SServer,KeyPort,FPort);
    FUnix:=ReadString(SServer,KeyUnix,FUnix);
    FSingleConnect:=ReadBool(SServer,KeySingleConnect,SingleConnect);
    FThreaded:=ReadBool(SServer,KeyThreaded,Threaded);
    end;
end;

procedure TLSPSocketServerConfig.SaveToIni(aIni: TCustomIniFile);
begin
  With aIni do
    begin
    WriteInteger(SServer,KeyPort,FPort);
    WriteString(SServer,KeyUnix,FUnix);
    WriteBool(SServer,KeySingleConnect,SingleConnect);
    WriteBool(SServer,KeyThreaded,Threaded);
    end;
end;


end.

