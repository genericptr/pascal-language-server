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
unit PasLS.Command.CompleteCode;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, SysUtils, fpJSON,
  { LSP }
  LSP.Streaming, LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Messages, PasLS.Commands;


Type

  { TCompleteCodeCommand }

  TCompleteCodeCommand = Class(TCustomCommand)
  private
    procedure CompleteCode(DocumentURI: TDocumentUri; line, column: integer);
  Protected
    Function DoExecute(aArguments: TJSONArray): TLSPStreamable; override;
  Public
    Class Function CommandName : string; override;
  end;

implementation

uses PasLS.ApplyEdit, CodeToolManager, CodeCache, SourceChanger, FindDeclarationTool;

procedure TCompleteCodeCommand.CompleteCode(DocumentURI: TDocumentUri; line, column: integer);

var
  Path: String;
  Code, NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: Integer;
  ARange: TRange;

begin
  // https://wiki.lazarus.freepascal.org/Lazarus_IDE_Tools#Code_Completion

  with CodeToolBoss.SourceChangeCache.BeautifyCodeOptions do
    begin
     ClassHeaderComments := false;
     ClassImplementationComments := false;
     ForwardProcBodyInsertPolicy := fpipInFrontOfMethods;
    end;

  //Code := CodeToolBoss.LoadFile(URI.path + URI.Document, false, false);
  Path := UriToPath(DocumentURI);
  Code := CodeToolBoss.FindFile(Path);
  Transport.SendDiagnostic(' ▶️ complete code: '+ Path + ' Code: ' + BoolToStr(assigned(Code),'True','False'));
  if not CodeToolBoss.CompleteCode(Code, column, line, {TopLine}line, NewCode, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine, false) then
    begin
    Transport.SendDiagnostic( '🔴 CompleteCode Failed');
    Exit;
    end;

  Transport.SendDiagnostic(' ✅ Sucesss NewX: %d NewY: %d NewTopLine: %d BlockTopLine: %d BlockBottomLine: %d', [NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine]);
  //procedure AbsoluteToLineCol(Position: integer; out Line, Column: integer);
  With Code[Code.Count - 1] do
    Transport.SendDiagnostic( 'Position: %d : %d - Length: %d', [Position, Code.GetLineStart(Position),Len]);
  // TODO: we need to get character offsets and get the text out of the source
  aRange := TRange.Create(0, 0, MaxInt, MaxInt);
  try
    DoApplyEdit(Transport,DocumentURI, Code.Source, aRange);
  finally
    aRange.Free;
  end;
  // TODO: we can do this in one pass with multiple TTextEdits!
  // move the cursor
  //ApplyEdit(DocumentURI, '', TRange.Create({NewY, NewX}0,0));

  // TODO: goto line next
  //pascal-language-server: ✅ Sucesss NewX:3 NewY:83 NewTopLine: 81 BlockTopLine: 81 BlockBottomLine: 84
  //range := TRange.Create(NewY, NewX, )
end;

{ TCompleteCodeCommand }

function TCompleteCodeCommand.DoExecute(aArguments: TJSONArray): TLSPStreamable;

var
  DocumentURI : String;
  Position : TPosition;

begin
  Result:=Nil;
  documentURI := aArguments.Strings[0];
  position := specialize TLSPStreaming<TPosition>.ToObject(aArguments.Objects[1].AsJSON);
  try
    CompleteCode(documentURI, position.line, position.character);
  finally
    Position.Free;
  end;
end;

class function TCompleteCodeCommand.CommandName: string;
begin
  Result:='pasls.completeCode';
end;


Initialization
  TCompleteCodeCommand.Register;
end.

