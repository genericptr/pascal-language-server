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

unit LSP.Hover;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, URIParser, 
  { Code Tools }
  CodeToolManager, CodeCache, IdentCompletionTool, BasicCodeTools,
  { Protocol }
  LSP.Base, LSP.Basic;

type
  
  { THoverResponse }

  THoverResponse = class(TPersistent)
  private
    fContents: TMarkupContent;
    fRange: TRange;
  published
    // The hover's content
    property contents: TMarkupContent read fContents write fContents;

    // An optional range is a range inside a text document
    // that is used to visualize a hover, e.g. by changing the background color.
    property range: TRange read fRange write fRange;
  end;

  { THoverRequest }
  
  THoverRequest = class(specialize TLSPRequest<TTextDocumentPositionParams, THoverResponse>)
    function Process(var Params: TTextDocumentPositionParams): THoverResponse; override;
  end;

implementation
uses
  SysUtils, 
  LSP.Diagnostics;

{ THoverRequest }

function THoverRequest.Process(var Params: TTextDocumentPositionParams): THoverResponse;
var
  URI: TURI;
  Code: TCodeBuffer;
  X, Y: Integer;
  Hint: String;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
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
          writeln(StdErr, 'Hover Error: ', E.ClassName, ' ', E.Message);
          flush(StdErr);
          exit(nil);
        end;
    end;

    // https://facelessuser.github.io/sublime-markdown-popups/
    // Wrap hint in markdown code
    Hint:='```pascal'+#10+Hint+#10+'```';

    Result := THoverResponse.Create;
    Result.contents := TMarkupContent.Create(Hint, false);
    Result.range := TRange.Create(Y, X);
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/hover', THoverRequest);
end.

