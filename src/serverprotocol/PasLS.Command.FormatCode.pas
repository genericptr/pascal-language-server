unit PasLS.Command.FormatCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpjson,
  LSP.BaseTypes, LSP.Base, LSP.Basic, PasLS.Commands;

Type

  { TFormatCommand }

  TFormatCommand = Class(TCustomCommand)
  Protected
    Function DoExecute(aArguments: TJSONArray): TLSPStreamable; override;
  Public
    Class Function CommandName : string; override;
  end;

implementation

uses PasLS.Formatter;

{ TFormatCommand }

function TFormatCommand.DoExecute(aArguments: TJSONArray): TLSPStreamable;

var
  Formatter : TFileFormatter;
  FilePath,ConfPath : String;
begin
  Result:=Nil;
  FilePath := UriToPath(aArguments.Strings[0]);
  ConfPath := UriToPath(aArguments.Strings[1]);
  Formatter:=TFileFormatter.Create(Transport);
  try
    Formatter.Process(FilePath,ConfPath);
  finally
    Formatter.Free;
  end;
end;

class function TFormatCommand.CommandName: string;
begin
  Result:='pasls.formatCode';
end;

initialization
  TFormatCommand.Register;
end.

