
unit documentHighlight;

{$mode objfpc}{$H+}
{$scopedenums on}

interface

uses
  Classes, 
  URIParser, CodeToolManager, CodeCache, IdentCompletionTool, BasicCodeTools, CodeTree,
  lsp, basic;

type
  TDocumentHighlightKind = (
      // A textual occurrence.
      Text = 1,
      // Read-access of a symbol, like reading a variable.
      Read = 2,
      // Write-access of a symbol, like writing to a variable.
      Write = 3
    );

  { TDocumentHighlight }
  TDocumentHighlight = class(TCollectionItem)
  private
    fRange: TRange;
    fKind: TDocumentHighlightKind;
  published
    // The range this highlight applies to.
    property range: TRange read fRange write fRange;

    // The highlight kind, default is DocumentHighlightKind.Text.
    property kind: TDocumentHighlightKind read fKind write fKind;
  end;

  TDocumentHighlightItems = specialize TGenericCollection<TDocumentHighlight>;

  { DocumentHighlightParams }

  TDocumentHighlightParams = class(TTextDocumentPositionParams)
  end;

  { TDocumentHighlightRequest }

  TDocumentHighlightRequest = class(specialize TLSPRequest<TDocumentHighlightParams, TDocumentHighlightItems>)
    function Process(var Params: TDocumentHighlightParams): TDocumentHighlightItems; override;
  end;

implementation

(*
  function TStandardCodeTool.FindBlockCounterPart(
    const CursorPos: TCodeXYPosition;
    out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
  // jump from bracket-open to bracket-close or 'begin' to 'end'
  // or 'until' to 'repeat' ...
  var CleanCursorPos: integer;
  begin
    Result:=false;
    BeginParsingAndGetCleanPos(lsrEnd,CursorPos,CleanCursorPos);
    // read word at cursor
    MoveCursorToCleanPos(CleanCursorPos);
    if Src[CurPos.StartPos] in ['(','[','{'] then begin
      // jump forward to matching bracket
      ReadNextAtom;
      if not ReadForwardTilAnyBracketClose then exit;
    end else if Src[CurPos.StartPos] in [')',']','}'] then begin
      // jump backward to matching bracket
      ReadNextAtom;
      if not ReadBackwardTilAnyBracketClose then exit;
    end else begin
      if Src[CurPos.StartPos] in [';','.'] then dec(CurPos.StartPos);
      while (CurPos.StartPos>2) and IsWordChar[Src[CurPos.StartPos-1]] do
        dec(CurPos.StartPos);
      MoveCursorToCleanPos(CurPos.StartPos);
      ReadNextAtom;
      if CurPos.EndPos=CurPos.StartPos then exit;
      // read till block keyword counterpart
      if UpAtomIs('BEGIN') or UpAtomIs('CASE') or UpAtomIs('ASM')
      or UpAtomIs('RECORD') or UpAtomIs('TRY') or UpAtomIs('REPEAT') then begin
        // read forward till END, FINALLY, EXCEPT
        ReadTilBlockEnd(true,false);
      end else if UpAtomIs('END') or UpAtomIs('FINALLY') or UpAtomIs('EXCEPT')
      or UpAtomIs('UNTIL') then
      begin
        // read backward till BEGIN, CASE, ASM, RECORD, REPEAT
        ReadBackTilBlockEnd(true);
      end else
        exit;
    end;
    // CursorPos now contains the counter block keyword
    Result:=CleanPosToCaretAndTopLine(CurPos.StartPos,NewPos,NewTopLine);
  end;
*)

function TDocumentHighlightRequest.Process(var Params: TDocumentHighlightParams): TDocumentHighlightItems;
var
  URI: TURI;
  Code: TCodeBuffer;
  X, Y: Integer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  Item: TDocumentHighlight;
  Identifier: String;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    X := position.character;
    Y := position.line;
    Result := TDocumentHighlightItems.Create;

    //writeln(stderr, 'documentHighlight: ', URI.Path, ' -> ', Y+1,':',X+1, ' ', CodeToolBoss.CurCodeTool.CLassName);

    // TODO: how can we get the range of the only the block identifiers insead of the whole range?

    //if CodeToolBoss.FindBlockStart(Code, X + 1, Y + 1, NewCode, NewX, NewY, NewTopLine, false) then
    //  begin
    //    Result := TDocumentHighlightItems.Create;
    //    writeln(stderr, '* block: ', NewY,':',NewX,':',NewTopLine);
    //    if CodeToolBoss.GetIdentifierAt(Code,NewX,NewY,Identifier) then
    //      writeln(StdErr, 'Start -> ',Identifier)
    //    else
    //      Identifier := '';
    //    Item := TDocumentHighlight(Result.Add);
    //    Item.kind := TDocumentHighlightKind.Text;
    //    Item.range := TRange.Create(NewY - 1, NewX - 1, NewY - 1, NewX + Length(Identifier) - 1);
    //  end;

    if CodeToolBoss.FindBlockCounterPart(Code, X + 1, Y + 1, NewCode, NewX, NewY, NewTopLine) then
      begin
        //writeln(stderr, '* block: ', NewY,':',NewX,':',NewTopLine);
        //if CodeToolBoss.GetIdentifierAt(Code,NewX,NewY,Identifier) then
        //  writeln(StdErr, 'Start -> ',Identifier);
        //if CodeToolBoss.GetIdentifierAt(Code,X + 1,Y + 1,Identifier) then
        //  writeln(StdErr, 'End -> ',Identifier);
        Item := TDocumentHighlight(Result.Add);
        Item.kind := TDocumentHighlightKind.Text;
        Item.range := TRange.Create(NewY - 1, NewX - 1, Y, X);
      end;

    Flush(stderr);
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/documentHighlight', TDocumentHighlightRequest);
end.