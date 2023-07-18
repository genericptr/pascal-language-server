unit PasLS.DocumentHighlight;


{$mode objfpc}{$H+}
{$scopedenums on}

interface

uses
  { RTL }
  SysUtils, Classes,
  { Code Tools }
  CodeToolManager, CodeCache,
  { Protocol }
  LSP.Base, LSP.Basic,
  { Other }
  LSP.BaseTypes, PasLS.CodeUtils, LSP.DocumentHighlight;

Type
  { TDocumentHighlightRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentHighlight

    The document highlight request is sent from the client to the server to resolve a
    document highlights for a given text document position. For programming languages
    this usually highlights all references to the symbol scoped to this file.
    However we kept `textDocument/documentHighlight` and `textDocument/references`
    separate requests since the first one is allowed to be more fuzzy.
    Symbol matches usually have a DocumentHighlightKind of Read or Write whereas fuzzy or
    textual matches use Textas the kind. }

  TDocumentHighlightRequest = class(specialize TLSPRequest<TDocumentHighlightParams, TDocumentHighlightItems>)
    function Process(var Params: TDocumentHighlightParams): TDocumentHighlightItems; override;
  end;

implementation


function TDocumentHighlightRequest.Process(var Params: TDocumentHighlightParams): TDocumentHighlightItems;
var
  Code: TCodeBuffer;
  X, Y: Integer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;

begin
  Result:=TDocumentHighlightItems.Create;
  with Params do
  begin
    Code := CodeToolBoss.FindFile(textDocument.LocalPath);
    X := position.character + 1;
    Y := position.line + 1;

    if CodeToolBoss.FindBlockCounterPart(Code, X, Y, NewCode, NewX, NewY, NewTopLine) then
      begin
        // Show start/end indentifier if the range spans more than 1 line
        if NewY - Y <> 0 then
          begin
            TDocumentHighlight.Create(Result,TDocumentHighlightKind.Text, GetIdentifierRangeAtPos(NewCode, NewX, NewY - 1));
            TDocumentHighlight.Create(Result,TDocumentHighlightKind.Text, GetIdentifierRangeAtPos(NewCode, X, Y - 1))
          end
        else
          begin
            // TODO: make this an option to show single line ranges?
            //Item := TDocumentHighlight(Result.Add);
            //Item.kind := TDocumentHighlightKind.Text;
            //Item.range := TRange.Create(NewY - 1, NewX - 1, Y - 1, X - 1);
          end;
      end
    else
  end;
end;


end.

