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

