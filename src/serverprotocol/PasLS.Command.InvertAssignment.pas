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
unit PasLS.Command.InvertAssignment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON,
  { LSP }
  LSP.Streaming, LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Messages, PasLS.Commands;


Type

  { TInvertAssignmentCommand }

  TInvertAssignmentCommand = Class(TCustomCommand)
  private
    procedure InvertAssignment(DocumentURI: TDocumentUri; Range: TRange);
  Protected
    Function DoExecute(aArguments: TJSONArray): TLSPStreamable; override;
  Public
    Class Function CommandName : string; override;
  end;

implementation

uses PasLS.InvertAssign, PasLS.ApplyEdit, CodeToolManager, CodeCache, FindDeclarationTool;

{ TInvertAssignmentCommand }

procedure TInvertAssignmentCommand.InvertAssignment(DocumentURI: TDocumentUri; Range: TRange);

var
  Path,S,SL : String;
  Code : TCodeBuffer;
  I : TInvertAssignment;

begin
  Path := UriToPath(DocumentURI);
  Code := CodeToolBoss.FindFile(Path);
  if Assigned(Code) then
    begin
    S:='';
    if (Range.start.line<Range.&end.line) then
      begin
      S:=Code.GetLines(Range.start.line+1,Range.&end.line);
      if Range.start.character>0 then
        Delete(S,1,Range.start.character);
      end;
    SL:=Code.GetLine(Range.&end.line);
    S:=S+Copy(SL,1,Range.&end.Character+1);
    I:=TInvertAssignment.Create;
    try
      S:=I.InvertAssignment(S);
      DoApplyEdit(Transport,DocumentURI,S,Range);
    finally
      I.Free;
    end;
    end;
end;

function TInvertAssignmentCommand.DoExecute(aArguments: TJSONArray
  ): TLSPStreamable;

var
  documentURI : String;
  range : TRange;
  ePos,sPos : TPosition;
begin
  Result:=Nil;
  documentURI := aArguments.Strings[0];
  Range:=Nil;
  ePos:=Nil;
  sPos:=specialize TLSPStreaming<TPosition>.ToObject(aArguments.Objects[1].AsJSON);
  try
    ePos:=specialize TLSPStreaming<TPosition>.ToObject(aArguments.Objects[2].AsJSON);
    Range:=TRange.Create;
    Range.Start:=sPos;
    Range.&End:=ePos;
    InvertAssignment(documentURI,Range);
  finally
    sPos.Free;
    ePos.Free;
    Range.Free;
  end;
end;

class function TInvertAssignmentCommand.CommandName: string;
begin
  Result:='pasls.invertAssignment';
end;

initialization
  TInvertAssignmentCommand.Register;
end.

