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
  DefaultCompiler = 'fpc';
  DefaultLazarusDir = '';
  DefaultFPCDir = '';
  DefaultTargetOS = {$i %FPCTARGETOS%};
  DefaultTargetCPU = {$i %FPCTARGETCPU%};

Type
  { TLSPSocketServerConfig }

  TLSPSocketServerConfig = Class(TObject)
  private
    FCompiler: string;
    FFPCDir: string;
    FLazarusDir: string;
    FLogFile: String;
    FPort: Word;
    FSingleConnect: Boolean;
    FTargetCPU: string;
    FTargetOS: string;
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
    property Compiler : string read FCompiler write FCompiler;
    property FPCDir : string Read FFPCDir Write FFPCDir;
    property LazarusDir : string read FLazarusDir write FLazarusDir;
    property TargetOS : string read FTargetOS write FTargetOS;
    property TargetCPU : string read FTargetCPU write FTargetCPU;
  end;


implementation

Const
  SServer = 'Server';
  KeyPort = 'Port';
  KeyUnix = 'Unix';
  KeySingleConnect = 'SingleConnect';
  KeyThreaded = 'Threaded';
  KeyLogFile = 'LogFile';

  SCodeTools = 'CodeTools';
  KeyCompiler = 'Compiler';
  KeyFPCDir = 'FPCDir';
  KeyLazarusDir = 'LazarusDir';
  KeyTargetCPU = 'TargetCPU';
  KeyTargetOS = 'TargetOS';

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
  Compiler:=DefaultCompiler;
  FPCDir:=DefaultFPCDir;
  LazarusDir:=DefaultLazarusDir;
  TargetCPU:=DefaultTargetCPU;
  TargetOS:=DefaultTargetOS;
end;

class function TLSPSocketServerConfig.DefaultConfigFile: String;
begin
{$IFDEF UNIX}
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
    Compiler:=ReadString(SCodeTools,KeyCompiler,Compiler);
    FPCDir:=ReadString(SCodetools,KeyFPCDir,FPCDir);
    LazarusDir:=ReadString(SCodetools,KeyLazarusDir,LazarusDir);
    TargetCPU:=ReadString(SCodetools,KeyTargetCPU,TargetCPU);
    TargetOS:=ReadString(SCodetools,KeyTargetOS,TargetOS);
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
    WriteString(SCodeTools,KeyCompiler,Compiler);
    WriteString(SCodetools,KeyFPCDir,FPCDir);
    WriteString(SCodetools,KeyLazarusDir,LazarusDir);
    WriteString(SCodetools,KeyTargetCPU,TargetCPU);
    WriteString(SCodetools,KeyTargetOS,TargetOS);
    end;
end;


end.

