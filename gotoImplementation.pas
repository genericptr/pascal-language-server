
unit gotoImplementation;

{$mode objfpc}{$H+}

interface

uses
  Classes, URIParser, CodeToolManager, CodeCache, BasicCodeTools,
  lsp, basic;

type
  
  { TGotoImplementation }
  
  TGotoImplementation = class(specialize TLSPRequest<TTextDocumentPositionParams, TLocation>)
    function Process(var Params: TTextDocumentPositionParams): TLocation; override;
  end;

implementation

function TGotoImplementation.Process(var Params: TTextDocumentPositionParams): TLocation;
var
  URI: TURI;
  Code: TCodeBuffer;
  NewCode: TCodeBuffer;
  X, Y: Integer;
  NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: integer;
  RevertableJump: boolean;
begin with Params do
  begin
    Result := nil;
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    X := position.character;
    Y := position.line;

    if CodeToolBoss.JumpToMethod(Code, X + 1, Y + 1, 
      NewCode, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine, RevertableJump) then
      begin
        Result := TLocation.Create;
        Result.URI := PathToURI(NewCode.Filename);
        Result.Range := TRange.Create(NewY - 1, NewX - 1);
      end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/implementation', TGotoImplementation);
end.

