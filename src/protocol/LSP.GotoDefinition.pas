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

unit LSP.GotoDefinition;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, URIParser, 
  { Code Tools }
  CodeToolManager, CodeCache,
  { Protocol }
  LSP.Base, LSP.Basic;

type
  
  { TGotoDefinition }
  
  TGotoDefinition = class(specialize TLSPRequest<TTextDocumentPositionParams, TLocation>)
    function Process(var Params: TTextDocumentPositionParams): TLocation; override;
  end;

implementation
uses
  LSP.Diagnostics;
  
function TGotoDefinition.Process(var Params: TTextDocumentPositionParams): TLocation;
var
  URI: TURI;
  Code: TCodeBuffer;
  NewCode: TCodeBuffer;
  X, Y: Integer;
  NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: integer;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    X := position.character;
    Y := position.line;
    { 
      NOTE: currently goto definition is supported as goto declaration
      
      There is a definition for the following identifiers:
        
        - Methods

      There is no definition for the following identifiers:
  
        - Function forwards
        - Functions in the interface section
        - External functions
        - Class forwards
        - External ObjC classes

        https://www.cprogramming.com/declare_vs_define.html
        https://stackoverflow.com/questions/1410563/what-is-the-difference-between-a-definition-and-a-declaration
    }
    if CodeToolBoss.FindDeclaration(Code, X + 1, Y + 1, NewCode, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine) then
      begin
        Result := TLocation.Create(NewCode.Filename,NewY - 1, NewX - 1,0)
      end
    else
      begin
        Result := nil;
        PublishDiagnostic;
      end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/definition', TGotoDefinition);
end.

