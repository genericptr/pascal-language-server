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

