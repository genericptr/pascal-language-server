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

