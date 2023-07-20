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
unit PasLS.Hover;

{$mode objfpc}{$H+}

interface

uses
  { Code Tools }
  CodeToolManager, CodeCache,
  { Protocol }
  LSP.BaseTypes,LSP.Base, LSP.Hover, LSP.Basic;

Type
  { THoverRequest }

  THoverRequest = class(specialize TLSPRequest<TTextDocumentPositionParams, THoverResponse>)
    function Process(var Params: TTextDocumentPositionParams): THoverResponse; override;
  end;


implementation

uses
  SysUtils;

function THoverRequest.Process(var Params: TTextDocumentPositionParams): THoverResponse;
var

  Code: TCodeBuffer;
  X, Y: Integer;
  Hint: String;
begin with Params do
  begin
    Code := CodeToolBoss.FindFile(textDocument.LocalPath);
    X := position.character;
    Y := position.line;

    try
      Hint := CodeToolBoss.FindSmartHint(Code, X + 1, Y + 1);
      // empty hint string means nothing was found
      if Hint = '' then
        exit(nil);
    except
      on E: Exception do
        begin
          LogError('Hover Error',E);
          exit(nil);
        end;
    end;

    // https://facelessuser.github.io/sublime-markdown-popups/
    // Wrap hint in markdown code
    Hint:='```pascal'+#10+Hint+#10+'```';

    Result := THoverResponse.Create;
    Result.contents.PlainText:=False;
    Result.contents.value:=Hint;
    Result.range.SetRange(Y, X);
  end;
end;


end.

