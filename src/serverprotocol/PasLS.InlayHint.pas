unit PasLS.InlayHint;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, SysUtils,
  { LSP Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.InlayHint;

Type
  { TInlayHintRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint

    The inlay hints request is sent from the client to the server to compute inlay hints for a given [text document, range] tuple that may be rendered in the editor in place with other text. }

  TInlayHintRequest = class(specialize TLSPRequest<TInlayHintParams, TInlayHints>)
    function Process(var Params: TInlayHintParams): TInlayHints; override;
  end;


implementation

{ TInlayHintRequest }

function TInlayHintRequest.Process(var Params: TInlayHintParams): TInlayHints;
{var
  hint: TInlayHint;}
begin with Params do
  //hint := TInlayHint(result.Add);
  //hint.position := TPosition.Create(0, 0);
  //hint.&label := 'number';
  //hint.tooltip := 'paramter name tooltip';
  result := TInlayHints.Create;
end;

end.

