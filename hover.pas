
unit hover;

{$mode objfpc}{$H+}

interface

uses
  Classes, URIParser, CodeToolManager, CodeCache, IdentCompletionTool, BasicCodeTools,
  lsp, basic;

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
    Result := THoverResponse.Create;

    Hint := CodeToolBoss.FindSmartHint(Code, X + 1, Y + 1);
    if Hint = '' then
      begin
        if CodeToolBoss.ErrorMessage <> '' then
          begin
            writeln(StdErr, CodeToolBoss.ErrorMessage);
            Flush(StdErr);
          end;
        exit(nil);
      end;

    Result.contents := TMarkupContent.Create(Hint);
    Result.range := TRange.Create(Y, X);
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/hover', THoverRequest);
end.

