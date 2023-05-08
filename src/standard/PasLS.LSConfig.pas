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

unit PasLS.LSConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Inifiles;

Const
  DefaultLogFile = '';
  DefaultCompiler = 'fpc';
  DefaultLazarusDir = '';
  DefaultFPCDir = '';
  DefaultTargetOS = {$i %FPCTARGETOS%};
  DefaultTargetCPU = {$i %FPCTARGETCPU%};

Type
  { TLSPServerConfig }

  TLSPServerConfig = Class(TObject)
  private
    FCompiler: string;
    FFPCDir: string;
    FLazarusDir: string;
    FLogFile: String;
    FTargetCPU: string;
    FTargetOS: string;
  Public
    Constructor Create; virtual;
    Procedure Reset; virtual;
    class Function DefaultConfigFile : String;
    Procedure LoadFromFile(const aFileName : String);
    Procedure SaveToFile(const aFileName : String);
    Procedure LoadFromIni(aIni : TCustomIniFile); virtual;
    Procedure SaveToIni(aIni : TCustomIniFile); virtual;
  Public
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
  KeyLogFile = 'LogFile';

  SCodeTools = 'CodeTools';
  KeyCompiler = 'Compiler';
  KeyFPCDir = 'FPCDir';
  KeyLazarusDir = 'LazarusDir';
  KeyTargetCPU = 'TargetCPU';
  KeyTargetOS = 'TargetOS';

{ TLSPServerConfig }

constructor TLSPServerConfig.Create;
begin
  Reset;
end;

procedure TLSPServerConfig.Reset;
begin
  LogFile:=DefaultLogFile;
  Compiler:=DefaultCompiler;
  FPCDir:=DefaultFPCDir;
  LazarusDir:=DefaultLazarusDir;
  TargetCPU:=DefaultTargetCPU;
  TargetOS:=DefaultTargetOS;
end;

class function TLSPServerConfig.DefaultConfigFile: String;
begin
{$IFDEF UNIX}
  Result:='/etc/pasls.cfg';
{$ELSE}
  Result:=ChangeFileExt(ParamStr(0),'.ini');
{$ENDIF}
end;

procedure TLSPServerConfig.LoadFromFile(const aFileName: String);

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

procedure TLSPServerConfig.SaveToFile(const aFileName: String);
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

procedure TLSPServerConfig.LoadFromIni(aIni: TCustomIniFile);
begin
  With aIni do
    begin
    FLogFile:=ReadString(SServer,KeyLogFile,LogFile);
    Compiler:=ReadString(SCodeTools,KeyCompiler,Compiler);
    FPCDir:=ReadString(SCodetools,KeyFPCDir,FPCDir);
    LazarusDir:=ReadString(SCodetools,KeyLazarusDir,LazarusDir);
    TargetCPU:=ReadString(SCodetools,KeyTargetCPU,TargetCPU);
    TargetOS:=ReadString(SCodetools,KeyTargetOS,TargetOS);
    end;
end;

procedure TLSPServerConfig.SaveToIni(aIni: TCustomIniFile);
begin
  With aIni do
    begin
    WriteString(SServer,KeyLogFile,LogFile);
    WriteString(SCodeTools,KeyCompiler,Compiler);
    WriteString(SCodetools,KeyFPCDir,FPCDir);
    WriteString(SCodetools,KeyLazarusDir,LazarusDir);
    WriteString(SCodetools,KeyTargetCPU,TargetCPU);
    WriteString(SCodetools,KeyTargetOS,TargetOS);
    end;
end;


end.

