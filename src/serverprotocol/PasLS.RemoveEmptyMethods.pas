unit PasLS.RemoveEmptyMethods;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, SysUtils,
  { Codetools }
  CodeToolManager, CodeCache,
  { LSP }
  LSP.Messages, LSP.Basic, LSP.Base;

Type

  { TRemoveEmptyMethods }

  TRemoveEmptyMethods = Class(TObject)
  private
    FTransport: TMessageTransport;
  Public
    Constructor Create(aTransport : TMessageTransport);
    Procedure Execute(const aDocumentURI : String; aPosition: TPosition); virtual;
    Property Transport : TMessageTransport Read FTransport;
  end;

implementation

uses
  { codetools }
  PascalParserTool,  CodeToolsStructs,
  { LSP }
  PasLS.ApplyEdit;

{ TRemoveEmptyMethods }

constructor TRemoveEmptyMethods.Create(aTransport: TMessageTransport);
begin
  FTransport:=aTransport;
end;

procedure TRemoveEmptyMethods.Execute(const aDocumentURI: String;
  aPosition: TPosition);

Const
  Attributes =
      [phpAddClassName,phpDoNotAddSemicolon,phpWithoutParamList,
       phpWithoutBrackets,phpWithoutClassKeyword,phpWithoutSemicolon];

var
  aList : TFPList;
  allEmpty : Boolean;
  aX,aY : Integer;
  Msg : String;
  RemovedProcHeads: TStrings;
  Code : TCodeBuffer;
  aRange : TRange;

begin
  Code:=CodeToolBoss.FindFile(URIToPath(aDocumentUri));
  if Code=Nil then
    begin
    Transport.SendDiagnostic('Cannot find file %s',[aDocumentURI]);
    exit;
    end;
  aY:=aPosition.line+1;
  aX:=aPosition.character+1;
  RemovedProcHeads:=Nil;
  aList:=TFPList.Create;
  try
    // check whether cursor is in a class
    if not CodeToolBoss.FindEmptyMethods(Code,'',aX,aY,AllPascalClassSections,aList,AllEmpty) then
      begin
      Msg:=CodeToolBoss.ErrorMessage;
      if Msg='' then
        Msg:='No class at caret position';
      Transport.SendDiagnostic('Cannot find empty methods in file %s: %s',[aDocumentURI,Msg]);
      exit;
      end;
    if not CodeToolBoss.RemoveEmptyMethods(Code,'',aX,aY,AllPascalClassSections,AllEmpty, Attributes, RemovedProcHeads) then
      Transport.SendDiagnostic('Failed to remove empty methods in file %s',[aDocumentURI])
    else
      begin
      aRange := TRange.Create(0, 0, MaxInt, MaxInt);
      try
        DoApplyEdit(Transport,aDocumentURI, Code.Source, aRange);
      finally
        aRange.Free;
      end;
      end;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(aList);
    RemovedProcHeads.Free;
  end;
end;



end.

