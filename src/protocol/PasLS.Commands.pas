// Pascal Language Server
// Copyright 2022 Ryan Joseph

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

unit PasLS.Commands;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface
uses
  { RTL }
  SysUtils, Classes, URIParser, FPJSON,
  { LSP }
  LSP.BaseTypes, LSP.Basic, LSP.Messages ;

procedure CompleteCode(aTRansport : TMessageTransport; documentURI: TDocumentUri; line, column: integer);

implementation
uses
  { CodeTools }
  CodeToolManager, CodeCache,
  FindDeclarationTool,
  SourceChanger,
  { Protocols }
  LSP.Workspace;

procedure ApplyEdit(aTransport : TMessageTransport; documentURI, Text: String; range: TRange);

var
  params: TApplyWorkspaceEditParams;
  edit: TWorkspaceEdit;
  textEdit: TTextEdit;
  Msg: TWorkspaceApplyEditRequest;
  textDocumentEdit: TTextDocumentEdit;
begin
  Msg:=nil;
  params := TApplyWorkspaceEditParams.Create;
  try
    // TODO: & breaks the syntax coloring in code block
    //params.&label := 'hello';

    edit:=params.edit;

    textDocumentEdit:=edit.documentChanges.Add;
    textDocumentEdit.textDocument.uri := documentURI;
    textDocumentEdit.textDocument.version := TOptionalInteger.Create(0); // send nil since we know we have the current master copy

    textEdit := textDocumentEdit.edits.Add;
    textEdit.range:=range;
    textEdit.newText:=Text;
    // TODO: the class should know it's method name
    Msg:=TWorkspaceApplyEditRequest.Create(aTransport);
    Msg.Execute(params, 'workspace/applyEdit');
  finally
    Params.Free;
    Msg.Free;
  end;
end;

procedure CompleteCode(aTransport : TMessageTransport; documentURI: TDocumentUri; line, column: integer);
var
  Path: String;
  Code, NewCode: TCodeBuffer;
  URI: TURI;
  NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: Integer;
  aRange : TRange;

begin
  // https://wiki.lazarus.freepascal.org/Lazarus_IDE_Tools#Code_Completion

  with CodeToolBoss.SourceChangeCache.BeautifyCodeOptions do
    begin
     ClassHeaderComments:=false;
     ClassImplementationComments:=false;
     ForwardProcBodyInsertPolicy:=fpipInFrontOfMethods;
    end;

  URI :=  ParseURI(documentURI);
  //Code := CodeToolBoss.LoadFile(URI.path + URI.Document, false, false);
  Path := URI.path + URI.Document;
  Code := CodeToolBoss.FindFile(Path);
  aTransport.SendDiagnostic(' ▶️ complete code: '+ Path + ' Code: ' + BoolToStr(assigned(Code),'True','False'));
  if CodeToolBoss.CompleteCode(Code, column, line, {TopLine}line, NewCode, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine, false) then
    begin
      aTransport.SendDiagnostic(' ✅ Sucesss NewX: %d NewY: %d NewTopLine: %d BlockTopLine: %d BlockBottomLine: %d', [NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine]);
      //procedure AbsoluteToLineCol(Position: integer; out Line, Column: integer);
      With Code[Code.Count - 1] do
        aTransport.SendDiagnostic( 'Position: %d : %d - Length: %d', [Position, Code.GetLineStart(Position),Len]);
      // TODO: we need to get character offsets and get the text out of the source
      aRange:=TRange.Create(0, 0, MaxInt, MaxInt);
      try
        ApplyEdit(aTransport,documentURI, Code.Source, aRange);
      finally
        aRange.Free;
      end;
      // TODO: we can do this in one pass with multiple TTextEdits!
      // move the cursor
      //ApplyEdit(documentURI, '', TRange.Create({NewY, NewX}0,0));

      // TODO: goto line next
      //pascal-language-server: ✅ Sucesss NewX:3 NewY:83 NewTopLine: 81 BlockTopLine: 81 BlockBottomLine: 84
      //range := TRange.Create(NewY, NewX, )
    end
  else
    aTransport.SendDiagnostic( '🔴 CompleteCode Failed');
end;

end.
