
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

    if CodeToolBoss.FindBlockCounterPart(Code, X + 1, Y + 1, NewCode, NewX, NewY, NewTopLine) then
      begin
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