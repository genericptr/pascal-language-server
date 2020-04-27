
unit references;

{$mode objfpc}{$H+}

interface

uses
  Classes, URIParser, CodeToolManager, CodeCache, IdentCompletionTool,
  lsp, basic;

type

  { TReferenceContext }

  TReferenceContext = class(TPersistent)
  private
    fIncludeDeclaration: boolean;
  published
    // Include the declaration of the current symbol.
    property includeDeclaration: boolean read fIncludeDeclaration write fIncludeDeclaration;
  end;

  { TReferenceParams }
  

  TReferenceParams = class(TTextDocumentPositionParams)
  private
    // todo: extends WorkDoneProgressParams, PartialResultParam
    fContext: TReferenceContext;
  published
    property context: TReferenceContext read fContext write fContext;
  end;

  { TReferencesRequest }

  { The references request is sent from the client to the server to resolve 
    project-wide references for the symbol denoted by the given text document position. }

  TReferencesRequest = class(specialize TLSPRequest<TReferenceParams, TLocationItems>)
    function Process(var Params: TReferenceParams): TLocationItems; override;
  end;

implementation
uses
  SysUtils, FindDeclarationTool, CTUnitGraph;

{ TReferencesRequest }

function TReferencesRequest.Process(var Params: TReferenceParams): TLocationItems;
var
  URI: TURI;
  Code: TCodeBuffer;
  X, Y: Integer;
  List: TFPList;
  Cache: TFindIdentifierReferenceCache;
  Item: Pointer;
  Pos: TCodeXYPosition;
  Loc: TLocationItem;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    X := position.character;
    Y := position.line;
    (*
      function FindReferences(IdentifierCode: TCodeBuffer;
            X, Y: integer; SearchInCode: TCodeBuffer; SkipComments: boolean;
            var ListOfPCodeXYPosition: TFPList;
            var Cache: TFindIdentifierReferenceCache  // you must free Cache
            ): boolean;
    *)
    Result := nil;
    List := nil;
    Cache := nil;
    try
      // todo: what should SearchInCode be? I think the whole project...
      if CodeToolBoss.FindReferences(Code, X + 1, Y + 1, Code, true, List, Cache) then
        begin
          Result := TLocationItems.Create;
          for Item in List do
            begin
              Pos := PCodeXYPosition(Item)^;
              Loc := TLocationItem(Result.Add);
              Loc.URI := PathToURI(Pos.Code.FileName);
              Loc.Range := TRange.Create(Pos.Y - 1, Pos.X - 1);
            end;
          //Cache.Free;
        end
      else
        begin
          if CodeToolBoss.ErrorMessage <> '' then
            begin
              writeln(StdErr, 'Parse error: ', CodeToolBoss.ErrorMessage);
              Flush(StdErr);
            end;
        end;
    except
      on E: Exception do
        begin
          writeln(stderr, 'FindReferences Error: ', E.ClassName, ' ', E.Message);
          Flush(StdErr);
        end;
    end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/references', TReferencesRequest);
end.

