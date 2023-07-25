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
unit PasLS.SignatureHelp;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes,
  { Code Tools }
  CodeToolManager, CodeCache, IdentCompletionTool,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.SignatureHelp;

Type
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
  PasLS.Diagnostics;


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
          PublishCodeToolsError(Transport,'');
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

end.

