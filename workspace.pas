// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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

unit workspace;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  lsp, basic, documentSymbol;

type
  
  { The parameters of a Workspace Symbol Request. }

  TWorkspaceSymbolParams = class(TPersistent)
  private
    fQuery: string;
  published
    // A query string to filter symbols by. Clients may send an empty
    // string here to request all symbols.
    property query: string read fQuery write fQuery;
  end;

  { TWorkspaceSymbolRequest }

  { The workspace symbol request is sent from the client to the server to 
    list project-wide symbols matching the query string. }

  TWorkspaceSymbolRequest = class(specialize TLSPRequest<TWorkspaceSymbolParams, TSymbolInformationItems>)
    function Process(var Params: TWorkspaceSymbolParams): TSymbolInformationItems; override;
  end;

implementation
uses
  SysUtils;

function TWorkspaceSymbolRequest.Process(var Params: TWorkspaceSymbolParams): TSymbolInformationItems;
begin with Params do
  begin
    writeln(stderr, 'workspace symbols for ', query);

    Result := TSymbolManager.SharedManager.Reload('/Users/ryanjoseph/Desktop/FPCLS-Test/units/node.pas');
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('workspace/symbol', TWorkspaceSymbolRequest);
end.