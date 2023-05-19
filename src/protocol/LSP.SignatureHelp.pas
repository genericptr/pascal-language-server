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

unit LSP.SignatureHelp;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes,
  { Code Tools }
  CodeToolManager, CodeCache, IdentCompletionTool,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes;

type

  { TParameterInformation }

  { Represents a parameter of a callable-signature. A parameter can
    have a label and a doc-comment. }

  TParameterInformation = class(TCollectionItem)
  private
    fLabel: string;
    fDocumentation: TMarkupContent;
    procedure SetDocumentation(AValue: TMarkupContent);
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
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
    property documentation: TMarkupContent read fDocumentation write SetDocumentation;
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
    procedure SetDocumentation(AValue: TMarkupContent);
    procedure SetParameters(AValue: TParameterInformationCollection);
  Public
    Constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    // The label of this signature. Will be shown in
    // the UI.
    property &label: string read fLabel write fLabel;

    // The human-readable doc-comment of this signature. Will be shown
    // in the UI but can be omitted.
    property documentation: TMarkupContent read fDocumentation write SetDocumentation;

    // The parameters of this signature.
    property parameters: TParameterInformationCollection read fParameters write SetParameters;
  end;

  TSignatureInformationCollection = specialize TGenericCollection<TSignatureInformation>;

  { TSignatureHelp
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#signatureHelp
    
    Signature help represents the signature of something callable.
    There can be multiple signature but only one active and only one active parameter. }

  TSignatureHelp = class(TLSPStreamable)
  private
    fSignatures: TSignatureInformationCollection;
    fActiveSignature: integer;
    fActiveParameter: integer;
    procedure SetSignatures(AValue: TSignatureInformationCollection);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
  published
    // One or more signatures.
    property signatures: TSignatureInformationCollection read fSignatures write SetSignatures;

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

  { TSignatureHelpRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_signatureHelp

    Signature help represents the signature of something
    callable. There can be multiple signature but only one
    active and only one active parameter. }

  TSignatureHelpRequest = class(specialize TLSPRequest<TTextDocumentPositionParams, TSignatureHelp>)
    function Process(var Params: TTextDocumentPositionParams): TSignatureHelp; override;
  end;

implementation

uses
  { RTL }
  SysUtils, PasLS.CodeUtils,
  { Code Tools}
  FindDeclarationTool, CodeTree, PascalParserTool,
  { Protocol }
  LSP.Diagnostics;

{ TParameterInformation }

procedure TParameterInformation.SetDocumentation(AValue: TMarkupContent);
begin
  if fDocumentation=AValue then Exit;
  fDocumentation.Assign(AValue);
end;

constructor TParameterInformation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fDocumentation:=TMarkupContent.Create;
end;

destructor TParameterInformation.Destroy;
begin
  FreeAndNil(fDocumentation);
  Inherited;
end;

procedure TParameterInformation.Assign(Source: TPersistent);
var
  Src: TParameterInformation absolute Source;
begin
  if Source is TParameterInformation then
    begin
    fLabel:=Src.fLabel;
    Documentation.Assign(Src.documentation);
    end
  else
    inherited Assign(Source);
end;

{ TSignatureInformation }

procedure TSignatureInformation.SetDocumentation(AValue: TMarkupContent);
begin
  if fDocumentation=AValue then Exit;
  fDocumentation.Assign(AValue);
end;

procedure TSignatureInformation.SetParameters(
  AValue: TParameterInformationCollection);
begin
  if fParameters=AValue then Exit;
  fParameters.Assign(AValue);
end;

constructor TSignatureInformation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fDocumentation:=TMarkupContent.Create;
  fParameters:=TParameterInformationCollection.Create;
end;

destructor TSignatureInformation.Destroy;
begin
  FreeAndNil(fDocumentation);
  FreeAndNil(fParameters);
  inherited Destroy;
end;

{ TSignatureHelp }

procedure TSignatureHelp.SetSignatures(AValue: TSignatureInformationCollection);
begin
  if fSignatures=AValue then Exit;
  fSignatures.Assign(AValue);
end;

constructor TSignatureHelp.Create;
begin
  inherited Create;
  fSignatures:=TSignatureInformationCollection.Create;
end;

destructor TSignatureHelp.Destroy;
begin
  FreeAndNil(fSignatures);
  inherited Destroy;
end;

{ TSignatureHelpRequest }

function TSignatureHelpRequest.Process(var Params: TTextDocumentPositionParams): TSignatureHelp;

  procedure ExtractProcParts(CurContext: TCodeContextInfoItem; out Code: String; out ParamList: TStringList);
  var
    Params, ResultType: String;
    CurExprType: TExpressionType;
    CodeNode: TCodeTreeNode;
    CodeTool: TFindDeclarationTool;
    i: integer;
  begin
    ParamList := nil;
    CurExprType := CurContext.Expr;
    Code := ExpressionTypeDescNames[CurExprType.Desc];
    if CurExprType.Context.Node <> nil then
      begin
        CodeNode := CurExprType.Context.Node;
        CodeTool := CurExprType.Context.Tool;
        case CodeNode.Desc of
          ctnProcedure:
            begin
              ResultType := CodeTool.ExtractProcHead(CodeNode, [
                phpWithoutClassName,   // skip classname
                phpWithoutName,        // skip function name
                phpWithoutGenericParams,// skip <> after proc name
                phpWithoutParamList,   // skip param list
                phpWithoutParamTypes,  // skip colon, param types and default values
                phpWithoutBrackets,    // skip start- and end-bracket of parameter list
                phpWithoutSemicolon,   // skip semicolon at end
                phpWithResultType]);

              Params := CodeTool.ExtractProcHead(CodeNode,
                        [phpWithoutName, 
                         phpWithoutBrackets, 
                         phpWithoutSemicolon,
                         phpWithVarModifiers,
                         phpWithParameterNames,
                         phpWithDefaultValues]);
              
              if Params <> '' then
                begin
                  ParamList := ParseParamList(Params);
                  // rebuild the param list into a single string
                  Params := '(';
                  for i := 0 to ParamList.Count - 1 do
                    begin
                      Params += ParamList[i];
                      if I < ParamList.Count - 1 then
                        Params += '; '; 
                    end;
                  Params += ')';
                end;

              Code := Params+ResultType;
            end;
        end;
      end;
  end;

var

  Code: TCodeBuffer;
  X, Y, I, ItemIndex: Integer;
  CodeContext: TCodeContextInfo;
  Item: TCodeContextInfoItem;
  Signature: TSignatureInformation;

  Parameter: TParameterInformation;
  Head: String;
  ParamList: TStringList;
begin
  Result:=Nil;
  with Params do
  begin
    Code := CodeToolBoss.FindFile(URIToPath(textDocument.uri));
    X := position.character;
    Y := position.line;
    CodeContext := nil;
    try
      if not CodeToolBoss.FindCodeContext(Code, X + 1, Y + 1, CodeContext) or (CodeContext = nil) or (CodeContext.Count = 0) then
        begin
          PublishDiagnostic(Transport);
          exit(nil);
        end;

      Result := TSignatureHelp.Create;

      // TODO: how do we know which one is active given the current parameters?
      Result.activeSignature := 0;
      
      for ItemIndex := 0 to CodeContext.Count - 1 do
        begin
          Item := CodeContext[ItemIndex];
          ExtractProcParts(Item, Head, ParamList);

          Signature := Result.signatures.Add;
          Signature.&label := CodeContext.ProcName+Head;

          if ParamList <> nil then
            begin
              for I := 0 to ParamList.Count - 1 do
                begin
                  Parameter := Signature.Parameters.Add;
                  Parameter.&label := ParamList[I];
                end;
              ParamList.Free;
            end;
        end;
        
        Result.activeParameter := CodeContext.ParameterIndex - 1;
    except
      on E: Exception do
        begin
          Transport.SendDiagnostic('Signature Error: %s %s',[E.ClassName,E.Message]);
        end;
    end;

    FreeAndNil(CodeContext);
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/signatureHelp', TSignatureHelpRequest);
end.

