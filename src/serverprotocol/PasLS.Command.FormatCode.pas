// Pascal Language Server
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

