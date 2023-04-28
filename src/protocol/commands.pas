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

unit Commands;

{$mode objfpc}{$H+}

interface
uses
  { RTL }
  SysUtils, Classes, URIParser, FPJSON,
  { LSP }
  LSP.Basic;

procedure CompleteCode(documentURI: TDocumentUri; line, column: integer);

implementation
uses
  { CodeTools }
  CodeToolManager, CodeToolsConfig, CodeCache, IdentCompletionTool, 
  BasicCodeTools, CodeTree, FindDeclarationTool, PascalParserTool,
  SourceChanger,
  { Protocols }
  LSP.Workspace;

procedure ApplyEdit(documentURI, Text: String; range: TRange);
var
  params: TApplyWorkspaceEditParams;
  edit: TWorkspaceEdit;
  request: TRequestMessage;
  textEdit: TTextEdit;
  textDocumentEdit: TTextDocumentEdit;
begin
  params := TApplyWorkspaceEditParams.Create;

  // TODO: & breaks the syntax coloring in code block
  //params.&label := 'hello';

  edit := TWorkspaceEdit.Create;

  textDocumentEdit := TTextDocumentEdit(edit.documentChanges.Add);
  textDocumentEdit.textDocument := TVersionedTextDocumentIdentifier.Create;
  textDocumentEdit.textDocument.uri := documentURI;
  textDocumentEdit.textDocument.version := nil; // send nil since we know we have the current master copy

  textEdit := TTextEdit(textDocumentEdit.edits.Add);
  textEdit.range := range;
  textEdit.newText := Text;

  params.edit := edit;

  // TODO: the class should know it's method name
  TWorkspaceApplyEditRequest.Execute(params, 'workspace/applyEdit');
end;

procedure CompleteCode(documentURI: TDocumentUri; line, column: integer);
var
  Path: String;
  Code, NewCode: TCodeBuffer;
  URI: TURI;
  NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: Integer;
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
  writeln(StdErr, '▶️ complete code: ', Path, ' Code: ', assigned(Code));
  if CodeToolBoss.CompleteCode(Code, column, line, {TopLine}line, NewCode, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine, false) then
    begin
      writeln(StdErr, '✅ Sucesss NewX:',NewX, ' NewY:', NewY, ' NewTopLine: ', NewTopLine, ' BlockTopLine: ', BlockTopLine, ' BlockBottomLine: ', BlockBottomLine);
      //procedure AbsoluteToLineCol(Position: integer; out Line, Column: integer);
      writeln(StdErr, '- Position: ', Code[Code.Count - 1].Position, ': ', Code.GetLineStart(Code[Code.Count - 1].Position));
      writeln(StdErr, '- Length: ', Code[Code.Count - 1].Len);

      // TODO: we need to get character offsets and get the text out of the source
      ApplyEdit(documentURI, Code.Source, TRange.Create(0, 0, MaxInt, MaxInt));
      // TODO: we can do this in one pass with multiple TTextEdits!
      // move the cursor
      //ApplyEdit(documentURI, '', TRange.Create({NewY, NewX}0,0));

      // TODO: goto line next
      //pascal-language-server: ✅ Sucesss NewX:3 NewY:83 NewTopLine: 81 BlockTopLine: 81 BlockBottomLine: 84
      //range := TRange.Create(NewY, NewX, )
    end
  else
    writeln(StdErr, '🔴 CompleteCode Failed');

  Flush(StdErr);
end;

end.