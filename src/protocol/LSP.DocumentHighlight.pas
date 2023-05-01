// Pascal Language Server
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

unit LSP.DocumentHighlight;

{$mode objfpc}{$H+}
{$scopedenums on}

interface
uses
  { RTL }
  Classes, 
  { Code Tools }
  URIParser, CodeToolManager, CodeCache,
  { Protocol }
  LSP.Base, LSP.Basic,
  { Other }
  PasLS.CodeUtils;

type
  TDocumentHighlightKind = (
      __UNUSED__,
      Text, // A textual occurrence.
      Read, // Read-access of a symbol, like reading a variable.
      Write // Write-access of a symbol, like writing to a variable.
    );

  { TDocumentHighlight }

  TDocumentHighlight = class(TPersistent)
  private
    fRange: TRange;
    fKind: TDocumentHighlightKind;
  published
    // The range this highlight applies to.
    property range: TRange read fRange write fRange;

    // The highlight kind, default is DocumentHighlightKind.Text.
    property kind: TDocumentHighlightKind read fKind write fKind;
  public
    constructor Create(_kind: TDocumentHighlightKind; _range: TRange);
  end;

  TDocumentHighlightItems = array of TDocumentHighlight;

  { DocumentHighlightParams }

  TDocumentHighlightParams = class(TTextDocumentPositionParams)
  end;

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

constructor TDocumentHighlight.Create(_kind: TDocumentHighlightKind; _range: TRange);
begin
  kind := _kind;
  range := _range;
end;

function TDocumentHighlightRequest.Process(var Params: TDocumentHighlightParams): TDocumentHighlightItems;
var
  URI: TURI;
  Code: TCodeBuffer;
  X, Y: Integer;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;

begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    X := position.character + 1;
    Y := position.line + 1;

    if CodeToolBoss.FindBlockCounterPart(Code, X, Y, NewCode, NewX, NewY, NewTopLine) then
      begin
        // Show start/end indentifier if the range spans more than 1 line
        if NewY - Y <> 0 then
          begin
            result := [
              TDocumentHighlight.Create(TDocumentHighlightKind.Text, GetIdentifierRangeAtPos(NewCode, NewX, NewY - 1)),
              TDocumentHighlight.Create(TDocumentHighlightKind.Text, GetIdentifierRangeAtPos(NewCode, X, Y - 1))
            ];
          end
        else
          begin
            // TODO: make this an option to show single line ranges?
            //Item := TDocumentHighlight(Result.Add);
            //Item.kind := TDocumentHighlightKind.Text;
            //Item.range := TRange.Create(NewY - 1, NewX - 1, Y - 1, X - 1);
            Result := nil;
          end;
      end
    else
      result := nil;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/documentHighlight', TDocumentHighlightRequest);
end.
