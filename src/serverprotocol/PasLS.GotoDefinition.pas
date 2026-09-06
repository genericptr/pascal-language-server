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

unit PasLS.GotoDefinition;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, sysutils,
  { Code Tools }
  CodeToolManager, CodeCache, BasicCodeTools, CodeTree,
  { Protocol }
  LSP.Base, LSP.Basic;

type
  
  { TGotoDefinition }
  
  TGotoDefinition = class(specialize TLSPRequest<TTextDocumentPositionParams, TLocation>)
    function Process(var Params: TTextDocumentPositionParams): TLocation; override;
  end;

implementation

uses
  PasLS.Diagnostics, PasLS.CodeUtils;
  
function TGotoDefinition.Process(var Params: TTextDocumentPositionParams): TLocation;
var
  Code: TCodeBuffer;
  NewCode: TCodeBuffer;
  X, Y, AbsPos: Integer;
  NewX, NewY, NewTopLine: integer;

begin with Params do
  begin
    Code := CodeToolBoss.FindFile(textDocument.localPath);
    X := position.character;
    Y := position.line;

    { 
      NOTE: Use FindMainDeclaration to skip forward declarations and find
      the main/complete declaration. This is the correct behavior for
      "Go to Definition" as opposed to "Go to Declaration".
      
      For example, with:
        type
          IDocList = interface;  // forward declaration
          ...
          IDocList = interface(IDocAny)  // main declaration
            ...
          end;
      
      FindMainDeclaration returns the main declaration location.
    }
    if IsIdentifier(Code, X + 1, Y + 1) then
      begin
        if CodeToolBoss.FindMainDeclaration(Code, X + 1, Y + 1, NewCode, NewX, NewY, NewTopLine) then
          begin
            Result := TLocation.Create;
            Result.uri := PathToURI(NewCode.Filename);
            Result.range := GetIdentifierRangeAtPos(NewCode, NewX, NewY - 1);
          end
        else
          begin
            Result := nil;
            PublishCodeToolsError(Transport,'');
          end;
      end
    else
      begin
        Result := nil;
        PublishCodeToolsError(Transport,'');
      end;
  end;
end;

end.

