// Pascal Language Server
// Copyright 2026

unit PasLS.Command.SelectMainProgram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON,
  LSP.BaseTypes, PasLS.Commands;

type

  { TSelectMainProgramCommand }

  TSelectMainProgramCommand = class(TCustomCommand)
  protected
    function DoExecute(aArguments: TJSONArray): TLSPStreamable; override;
  public
    class function CommandName: string; override;
  end;

implementation

uses
  PasLS.Settings;

function TSelectMainProgramCommand.DoExecute(aArguments: TJSONArray): TLSPStreamable;
var
  MainProgram: String;
begin
  Result := nil;
  if (aArguments = nil) or (aArguments.Count = 0) then
    Exit;

  MainProgram := ExpandFileName(aArguments.Strings[0]);
  if not FileExists(MainProgram) then
  begin
    Transport.SendDiagnostic('Main program file "'+MainProgram+'" can''t be found.');
    Exit;
  end;

  ServerSettings.&program := MainProgram;
  if SaveProjectMainProgram(MainProgram) then
    Transport.SendDiagnostic('Saved main program to '+ProjectConfigFile)
  else
    Transport.SendDiagnostic('Main program selected for this session, but project config context is unavailable.');
end;

class function TSelectMainProgramCommand.CommandName: string;
begin
  Result := 'pasls.selectMainProgram';
end;

initialization
  TSelectMainProgramCommand.Register;

end.
