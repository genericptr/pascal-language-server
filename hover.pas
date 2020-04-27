
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
uses
  fpjson,
  diagnostics;

procedure PublishDiagnostic(fileName: string; line, column: integer);
var
  notification: TPublishDiagnostics;
  params: TPublishDiagnosticsParams;
  diagnostic: TDiagnostic;
  Request, Response: TJSONData;
  Content: string;
begin
  params := TPublishDiagnosticsParams.Create;
  params.uri := PathToURI(fileName);
  diagnostic := TDiagnostic(params.diagnostics.Add);
  with diagnostic do
    begin
      range := TRange.Create(line, column);
      severity := TDiagnosticSeverity.Information;
      code := '100';
      source := 'Free Pascal Compiler';
      message := 'This is a diagnostic hint';
      tags := [];
      relatedInformation := nil;
    end;

  (*
    interface NotificationMessage extends Message {
      /**
       * The method to be invoked.
       */
      method: string;

      /**
       * The notification's params.
       */
      params?: array | object;
    }
  *)
end;

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

    Hint := CodeToolBoss.FindSmartHint(Code, X + 1, Y + 1);
    if Hint = '' then
      begin
        if CodeToolBoss.ErrorMessage <> '' then
          begin
            writeln(StdErr, 'Parse error: ', CodeToolBoss.ErrorMessage);
            Flush(StdErr);
          end;
        exit(nil);
      end;

    Result := THoverResponse.Create;
    Result.contents := TMarkupContent.Create(Hint);
    Result.range := nil;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/hover', THoverRequest);
end.

