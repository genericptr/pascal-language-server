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

unit LSP.InlayHint;

{$mode objfpc}{$H+}
{$scopedenums on}

interface
uses
  { RTL }
  Classes, SysUtils,
  LSP.Base, LSP.Basic;

type
  
  { TInlayHintKind }
  TInlayHintKind = (
    // TODO: do we need this for optionals?
    __UNUSED__,
    _Type,      // An inlay hint that for a type annotation.
    Parameter   // An inlay hint that is for a parameter.
  );

  TOptionalInlayHintKind = specialize TOptional<TInlayHintKind>;

  { TInlayHint
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#inlayHint

    Inlay hint information. }

  TInlayHint = class(TCollectionItem)
  private
    fPosition: TPosition;
    fLabel: String; // string | InlayHintLabelPart[] (not supported now)
    fKind: TOptionalInlayHintKind;
    fTextEdits: TTextEdits;
    fTooltip: String;
  published
    // The position of this hint.
    property position: TPosition read fPosition write fPosition;
    // The label of this hint. A human readable string or an array of
    // InlayHintLabelPart label parts.
    //
    // *Note* that neither the string nor the label part can be empty.
    property &label: String read fLabel write fLabel;
    // The kind of this hint. Can be omitted in which case the client
    // should fall back to a reasonable default.
    property kind: TOptionalInlayHintKind read fKind write fKind;
    // Optional text edits that are performed when accepting this inlay hint.
    //
    // *Note* that edits are expected to change the document so that the inlay
    // hint (or its nearest variant) is now part of the document and the inlay
    // hint itself is now obsolete.
    //
    // Depending on the client capability `inlayHint.resolveSupport` clients
    // might resolve this property late using the resolve request.
    property textEdits: TTextEdits read fTextEdits write fTextEdits;
    // The tooltip text when you hover over this item.
    //
    // Depending on the client capability `inlayHint.resolveSupport` clients
    // might resolve this property late using the resolve request.
    property tooltip: String read fTooltip write fTooltip;
  public
    destructor Destroy; override;
  end;

  TInlayHints = specialize TGenericCollection<TInlayHint>;

  { TInlayHintParams
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#inlayHintParams

    A parameter literal used in inlay hint requests. }

  TInlayHintParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentIdentifier;
    fRange: TRange;
  published
    // The text document.
    property textDocument: TTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The visible document range for which inlay hints should be computed.
    property range: TRange read fRange write fRange;
  public
    destructor Destroy; override;
  end;

  { TInlayHintRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint

    The inlay hints request is sent from the client to the server to compute inlay hints for a given [text document, range] tuple that may be rendered in the editor in place with other text. }

  TInlayHintRequest = class(specialize TLSPRequest<TInlayHintParams, TInlayHints>)
    function Process(var Params: TInlayHintParams): TInlayHints; override;
  end;

implementation

{ TInlayHint }

destructor TInlayHint.Destroy;
begin
  FreeAndNil(fPosition);
  FreeAndNil(fKind);
  inherited;
end;

{ TInlayHintParams }

destructor TInlayHintParams.Destroy;
begin
  FreeAndNil(fTextDocument);
  FreeAndNil(fRange);
  inherited;
end;

{ TInlayHintRequest }

function TInlayHintRequest.Process(var Params: TInlayHintParams): TInlayHints;
var
  hint: TInlayHint;
begin with Params do
  //hint := TInlayHint(result.Add);
  //hint.position := TPosition.Create(0, 0);
  //hint.&label := 'number';
  //hint.tooltip := 'paramter name tooltip';
  result := TInlayHints.Create;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/inlayHint', TInlayHintRequest);
end.