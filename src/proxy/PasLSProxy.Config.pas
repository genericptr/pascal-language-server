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

unit PasLSProxy.Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;


Const
  DefaultSocketUnix = '';
  DefaultSocketPort = 9898;
  DefaultLogFile = '';

Type
  { TLSPProxyConfig }

  TLSPProxyConfig = Class(TObject)
  private
    FLogFile: String;
    FPort: Word;
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
    Property LogFile : String Read FLogFile Write FLogFile;
  end;


implementation

Const
  SProxy = 'Proxy';
  KeyPort = 'Port';
  KeyUnix = 'Unix';
  KeyLogFile = 'LogFile';

{ TLSPProxyConfig }

constructor TLSPProxyConfig.Create;
begin
  Reset;
end;

procedure TLSPProxyConfig.Reset;
begin
  FPort:=DefaultSocketPort;
  FUnix:=DefaultSocketUnix;
  LogFile:=DefaultLogFile;
end;

class function TLSPProxyConfig.DefaultConfigFile: String;
begin
{$IFDEF UNIX}
  Result:='/etc/paslssock.cfg';
{$ELSE}
  Result:=ChangeFileExt(ParamStr(0),'.ini');
{$ENDIF}
end;

procedure TLSPProxyConfig.LoadFromFile(const aFileName: String);

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

procedure TLSPProxyConfig.SaveToFile(const aFileName: String);
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

procedure TLSPProxyConfig.LoadFromIni(aIni: TCustomIniFile);
begin
  With aIni do
    begin
    FPort:=ReadInteger(SProxy,KeyPort,FPort);
    FUnix:=ReadString(SProxy,KeyUnix,FUnix);
    FLogFile:=ReadString(SProxy,KeyLogFile,LogFile);
    end;
end;

procedure TLSPProxyConfig.SaveToIni(aIni: TCustomIniFile);
begin
  With aIni do
    begin
    WriteInteger(SProxy,KeyPort,FPort);
    WriteString(SProxy,KeyUnix,FUnix);
    WriteString(SProxy,KeyLogFile,LogFile);
    end;
end;


end.

