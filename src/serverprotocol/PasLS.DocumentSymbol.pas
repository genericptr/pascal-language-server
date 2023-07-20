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
unit PasLS.DocumentSymbol;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes,  FPJson, FPJsonRPC,
  { Code Tools }
  CodeToolManager, LinkScanner,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Streaming, LSP.DocumentSymbol;

Type
  { The document symbol request is sent from the client to the server. The returned result is either:

    * SymbolInformation[] which is a flat list of all symbols found in a given text document.
      Then neither the symbol’s location range nor the symbol’s container name should be used to infer a hierarchy.
    * DocumentSymbol[] which is a hierarchy of symbols found in a given text document. }

    TDocumentSymbolRequest = class(specialize TLSPRequest<TDocumentSymbolParams, TLSPStreamable>)
      function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    end;


implementation

uses
  { RTL }
  SysUtils, FileUtil, DateUtils, fpjsonrtti,
  { Code Tools }

  FindDeclarationTool, KeywordFuncLists,
  { Protocol }
  PasLS.Symbols;


{ TDocumentSymbolRequest }

function TDocumentSymbolRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: TDocumentSymbolParams;
  Path: String;
begin
  Input := specialize TLSPStreaming<TDocumentSymbolParams>.ToObject(Params);
  try
    Path := Input.textDocument.LocalPath;
    Result := SymbolManager.FindDocumentSymbols(Path);
    if not Assigned(Result) then
      Result := TJSONNull.Create;
  finally
    Input.Free;
  end;
end;


end.

