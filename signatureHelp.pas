
unit signatureHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, URIParser, CodeToolManager, CodeCache, IdentCompletionTool,
  lsp, basic;

type

  { TParameterInformation }

  { Represents a parameter of a callable-signature. A parameter can
    have a label and a doc-comment. }

  TParameterInformation = class(TCollectionItem)
  private
    fLabel: string;
    fDocumentation: TMarkupContent;
  published
    // The label of this parameter information.
    //
    // Either a string or an inclusive start and exclusive end offsets within its containing
    // signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
    // string representation as `Position` and `Range` does.
    //
    // *Note*: a label of type string should be a substring of its containing signature label.
    // Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.
    property &label: string read fLabel write fLabel;

    // The human-readable doc-comment of this parameter. Will be shown
    // in the UI but can be omitted.
    property documentation: TMarkupContent read fDocumentation write fDocumentation;
  end;

  TParameterInformationCollection = specialize TGenericCollection<TParameterInformation>;

  { TSignatureInformation }

  { Represents the signature of something callable. A signature
    can have a label, like a function-name, a doc-comment, and
    a set of parameters. }

  TSignatureInformation = class(TCollectionItem)
  private
    fLabel: string;
    fDocumentation: TMarkupContent;
    fParameters: TParameterInformationCollection;
  published
    // The label of this signature. Will be shown in
    // the UI.
    property &label: string read fLabel write fLabel;

    // The human-readable doc-comment of this signature. Will be shown
    // in the UI but can be omitted.
    property documentation: TMarkupContent read fDocumentation write fDocumentation;

    // The parameters of this signature.
    property parameters: TParameterInformationCollection read fParameters write fParameters;
  end;

  TSignatureInformationCollection = specialize TGenericCollection<TSignatureInformation>;

  { TSignatureHelp }

  TSignatureHelp = class(TPersistent)
  private
    fSignatures: TSignatureInformationCollection;
    fActiveSignature: integer;
    fActiveParameter: integer;
  published
    // One or more signatures.
    property signatures: TSignatureInformationCollection read fSignatures write fSignatures;

    // The active signature. If omitted or the value lies outside the
    // range of `signatures` the value defaults to zero or is ignored if
    // `signatures.length === 0`. Whenever possible implementors should
    // make an active decision about the active signature and shouldn't
    // rely on a default value.
    // In future version of the protocol this property might become
    // mandatory to better express this.
    property activeSignature: integer read fActiveSignature write fActiveSignature;

    // The active parameter of the active signature. If omitted or the value
    // lies outside the range of `signatures[activeSignature].parameters`
    // defaults to 0 if the active signature has parameters. If
    // the active signature has no parameters it is ignored.
    // In future version of the protocol this property might become
    // mandatory to better express the active parameter if the
    // active signature does have any.
    property activeParameter: integer read fActiveParameter write fActiveParameter;

  end;

  { TSignatureHelp }

  { Signature help represents the signature of something
   callable. There can be multiple signature but only one
   active and only one active parameter. }
  TSignatureHelpRequest = class(specialize TLSPRequest<TTextDocumentPositionParams, TSignatureHelp>)
    function Process(var Params: TTextDocumentPositionParams): TSignatureHelp; override;
  end;

implementation
uses
  SysUtils, FindDeclarationTool, CodeTree, PascalParserTool;

{ TSignatureHelpRequest }

function TSignatureHelpRequest.Process(var Params: TTextDocumentPositionParams): TSignatureHelp;

  function ExtractProcHead(CurContext: TCodeContextInfoItem): String;
  var
    Code: String;
    CurExprType: TExpressionType;
    CodeNode, ChildNode: TCodeTreeNode;
    CodeTool: TFindDeclarationTool;
    ParamList: TCodeTreeNode;
  begin
    CurExprType:=CurContext.Expr;
    Code:=ExpressionTypeDescNames[CurExprType.Desc];
    if CurExprType.Context.Node<>nil then
      begin
        CodeNode:=CurExprType.Context.Node;
        CodeTool:=CurExprType.Context.Tool;
        case CodeNode.Desc of
          ctnProcedure:
            begin
              Code:=CodeTool.ExtractProcHead(CodeNode,
                  [phpWithVarModifiers,phpWithParameterNames,phpWithDefaultValues,
                   phpWithResultType]);

              // todo: get param name/type pairs
              //ParamList:=CodeTool.GetProcParamList(CodeNode);
              //if ParamList <> nil then
              //  begin
              //    ChildNode:=ParamList.FirstChild;
              //    while ChildNode<>nil do
              //      begin
              //        Code += ChildNode.DescAsString+' ';
              //        ChildNode:=ChildNode.NextBrother;
              //      end;
              //  end;
            end;
        end;
      end;
    Result := Code;
  end;

var
  URI: TURI;
  Code: TCodeBuffer;
  X, Y, I, ItemIndex: Integer;
  Hint: String;
  CodeContext: TCodeContextInfo;
  Item: TCodeContextInfoItem;
  Signature: TSignatureInformation;
  Parameters: TParameterInformationCollection;
  Parameter: TParameterInformation;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    X := position.character;
    Y := position.line;

    writeln(stderr, 'signature at ', x,',', y);
    flush(stderr);

    CodeContext := nil;
    try
      if not CodeToolBoss.FindCodeContext(Code, X + 1, Y + 1, CodeContext) or (CodeContext = nil) or (CodeContext.Count = 0) then
        exit(nil);

      // todo: this can contain overloads I think
      Result := TSignatureHelp.Create;
      Result.signatures := TSignatureInformationCollection.Create;

      for ItemIndex := 0 to CodeContext.Count - 1 do
        begin
          Item := CodeContext[ItemIndex];
          writeln(stderr, 'signature for ', CodeContext.ProcName, ' param=', CodeContext.ParameterIndex, ' head=',ExtractProcHead(Item));
          flush(stderr);

          // The labels of each function parameter must be a substring of the label of the entire function for signatureHelp
          // So it should look something like DoThis(Param 0, Param 1, Param 2)
          Signature := TSignatureInformation(Result.signatures.Add);
          Signature.&label := ExtractProcHead(Item);

          //Signature.&label := CodeContext.ProcName+'(param1: integer; param2: integer; param3: integer)';
          // TODO:Item.Params is for compiler functions!
          //if Item.Params <> nil then
          //  begin
          //    Parameters := TParameterInformationCollection.Create;
          //    for I := 0 to Item.Params.Count - 1 do
          //      begin
          //        Parameter := TParameterInformation(Parameters.Add);
          //        Parameter.&label := Item.Params[I];
          //      end;
          //    Signature.parameters := Parameters;
          //  end;
        end;
        
        Result.activeSignature := 0;
        Result.activeParameter := CodeContext.ParameterIndex - 1;

    finally
      CodeContext.Free;
    end;

  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/signatureHelp', TSignatureHelpRequest);
end.

