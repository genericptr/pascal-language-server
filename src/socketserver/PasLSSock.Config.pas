// Pascal Language Server
// Copyright 2023 Michael Van Canneyt

// Socket-based protocol server - configuration options

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
  DefaultLogFile = '';

Type
  { TLSPSocketServerConfig }

  TLSPSocketServerConfig = Class(TObject)
  private
    FLogFile: String;
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
    Property LogFile : String Read FLogFile Write FLogFile;
  end;


implementation

Const
  SServer = 'Server';
  KeyPort = 'Port';
  KeyUnix = 'Unix';
  KeySingleConnect = 'SingleConnect';
  KeyThreaded = 'Threaded';
  KeyLogFile = 'LogFile';

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
  LogFile:=DefaultLogFile;
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
    FLogFile:=ReadString(SServer,KeyLogFile,LogFile);
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
    WriteString(SServer,KeyLogFile,LogFile);
    end;
end;


end.

