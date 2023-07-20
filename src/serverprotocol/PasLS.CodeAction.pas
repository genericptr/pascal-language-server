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

unit PasLS.CodeAction;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  SysUtils, Classes,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Streaming, LSP.CodeAction;

Type
  { TCodeActionRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction

    The code action request is sent from the client to the server to compute commands for a
    given text document and range. These commands are typically code fixes to either fix
    problems or to beautify/refactor code. The result of a textDocument/codeAction request
    is an array of Command literals which are typically presented in the user interface.
    To ensure that a server is useful in many clients the commands specified in a code actions
    should be handled by the server and not by the client (see workspace/executeCommand and
    ServerCapabilities.executeCommandProvider). If the client supports providing edits with a
    code action then that mode should be used. }

  TCodeActionRequest = class(specialize TLSPRequest<TCodeActionParams, TCodeActionItems>)
    function Process(var Params: TCodeActionParams): TCodeActionItems; override;
  end;


implementation

function TCodeActionRequest.Process(var Params: TCodeActionParams): TCodeActionItems;
begin with Params do
  begin
    Result := nil;
  end;
end;


end.

