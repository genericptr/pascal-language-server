// Pascal Language Server
// Copyright 2020 Arjan Adriaanse
// Copyright 2020 Ryan Joseph

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
unit PasLS.Command.RemoveEmptyMethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON,
  { LSP }
  LSP.Streaming, LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Messages, PasLS.Commands;


Type

  { TRemoveEmptymethodsCommand }

  TRemoveEmptymethodsCommand = Class(TCustomCommand)
  private
    procedure RemoveEmptymethods(DocumentURI: TDocumentUri; aPos : TPosition);
  Protected
    Function DoExecute(aArguments: TJSONArray): TLSPStreamable; override;
  Public
    Class Function CommandName : string; override;
  end;

implementation

uses PasLS.RemoveEmptyMethods;

{ TRemoveEmptymethodsCommand }

procedure TRemoveEmptymethodsCommand.RemoveEmptymethods(
  DocumentURI: TDocumentUri; aPos: TPosition);

var
  Rem: TRemoveEmptyMethods;

begin
  Rem:=TRemoveEmptyMethods.Create(Transport);
  try
    Rem.Execute(documentURI,aPos);
  finally
    Rem.Free;
  end;
end;

function TRemoveEmptymethodsCommand.DoExecute(aArguments: TJSONArray
  ): TLSPStreamable;

var
  documentURI : String;
  position : TPosition;

begin
  Result:=nil;
  documentURI := aArguments.Strings[0];
  position := specialize TLSPStreaming<TPosition>.ToObject(aArguments.Objects[1].AsJSON);
  try
    RemoveEmptymethods(documentUri,Position);
  finally
    Position.Free;
  end;
end;

class function TRemoveEmptymethodsCommand.CommandName: string;
begin
  Result:='pasls.removeEmptyMethods';
end;

initialization
  TRemoveEmptymethodsCommand.Register;
end.

