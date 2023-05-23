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

unit LSP.References;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  SysUtils, Classes, 
  { Code Tools }
  CodeToolManager, CodeCache, CTUnitGraph,
  { LazUtils }
  LazFileUtils, Laz_AVL_Tree,
  { Protocol }
  LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.General, LSP.Messages;

type

  { TReferenceContext }

  TReferenceContext = class(TLSPStreamable)
  private
    fIncludeDeclaration: boolean;
  published
    // Include the declaration of the current symbol.
    property includeDeclaration: boolean read fIncludeDeclaration write fIncludeDeclaration;
  end;

  { TReferenceParams }
  
  TReferenceParams = class(TTextDocumentPositionParams)
  private
    fContext: TReferenceContext;
    procedure SetContext(AValue: TReferenceContext);
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    property context: TReferenceContext read fContext write SetContext;
  end;

  { TReferencesRequest }

  { The references request is sent from the client to the server to resolve 
    project-wide references for the symbol denoted by the given text document position. }

  TReferencesRequest = class(specialize TLSPRequest<TReferenceParams, TLocationItems>)
    procedure FindReferences(Filename, MainFilename: String; X, Y: Integer; Items: TLocationItems);
    function Process(var Params: TReferenceParams): TLocationItems; override;
  end;


implementation

uses
  PasLS.Settings, LSP.Diagnostics;

{ TReferenceParams }

procedure TReferenceParams.SetContext(AValue: TReferenceContext);
begin
  if fContext=AValue then Exit;
  fContext.Assign(AValue);
end;

constructor TReferenceParams.Create;
begin
  inherited Create;
  fContext:=TReferenceContext.Create;
end;

destructor TReferenceParams.Destroy;
begin
  FreeAndNil(fContext);
  inherited Destroy;
end;

procedure TReferenceParams.Assign(Source: TPersistent);
var
  Src: TReferenceParams absolute Source;
begin
  if Source is TReferenceParams then
    begin
      Context.Assign(Src.context);
    end
  else
    inherited Assign(Source);
end;
  
procedure TReferencesRequest.FindReferences(Filename, MainFilename: String; X, Y: Integer; Items: TLocationItems);
var
  DeclCode, StartSrcCode, Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
  DeclX, DeclY, DeclTopLine, i: Integer;
  Identifier: string;
  Graph: TUsesGraph;
  Cache: TFindIdentifierReferenceCache;
  TreeOfPCodeXYPosition: TAVLTree;
  ANode, Node: TAVLTreeNode;
  CodePos: PCodeXYPosition;
  Files: TStringList;
  Completed: boolean;
  UGUnit: TUGUnit;
  Loc: TLocationItem;
begin

  // Step 1: load the file
  StartSrcCode:=CodeToolBoss.LoadFile(Filename,false,false);

  // Step 2: find the main declaration
  if not CodeToolBoss.FindMainDeclaration(StartSrcCode,
    X,Y,
    DeclCode,DeclX,DeclY,DeclTopLine) then
  begin
    PublishCodeToolsError(Transport,'FindMainDeclaration failed in '+StartSrcCode.FileName+' at '+IntToStr(Y)+':'+IntToStr(X));
    ExitCode:=-1;
    exit;
  end;

  // Step 3: get identifier
  CodeToolBoss.GetIdentifierAt(DeclCode,DeclX,DeclY,Identifier);
  DoLog('Found identifier: %s',[Identifier]);

  // Step 4: collect all modules of program
  Files:=TStringList.Create;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=nil;
  Cache:=nil;
  try
    Files.Add(DeclCode.Filename);
    if CompareFilenames(DeclCode.Filename,StartSrcCode.Filename)<>0 then
      Files.Add(DeclCode.Filename);

    // parse all used units
    Graph:=CodeToolBoss.CreateUsesGraph;
    try
      Graph.AddStartUnit(MainFilename);
      Graph.AddTargetUnit(DeclCode.Filename);
      Graph.Parse(true,Completed);
      Node:=Graph.FilesTree.FindLowest;
      while Node<>nil do begin
        UGUnit:=TUGUnit(Node.Data);
        Files.Add(UGUnit.Filename);
        Node:=Node.Successor;
      end;
    finally
      Graph.Free;
    end;

    // Step 5: find references in all files
    for i:=0 to Files.Count-1 do begin
      DoLog('Searching "%s"...',[Files[i]]);
      Code:=CodeToolBoss.LoadFile(Files[i],true,false);
      if Code=nil then begin
        DoLog('unable to load "%s"',[Files[i]]);
        continue;
      end;
      // search references
      CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      if not CodeToolBoss.FindReferences(
        DeclCode,DeclX,DeclY,
        Code, true, ListOfPCodeXYPosition, Cache) then
      begin
        PublishCodeToolsError(Transport,'FindReferences failed in "'+Code.Filename+'"');
        continue;
      end;
      if ListOfPCodeXYPosition=nil then continue;
      // In order to show all references after any parser error, they are
      // collected in a tree
      if TreeOfPCodeXYPosition=nil then
        TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
      CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                              TreeOfPCodeXYPosition,true,false);
    end;

    // Step 6: show references
    if TreeOfPCodeXYPosition=nil then begin
      // No references found
      exit;
    end;
    ANode:=TreeOfPCodeXYPosition.FindHighest;
    while ANode<>nil do begin
      CodePos:=PCodeXYPosition(ANode.Data);
      Loc := Items.Add;
      Loc.URI := PathToURI(CodePos^.Code.Filename);
      Loc.Range.SetRange(CodePos^.Y - 1, CodePos^.X - 1);
   {   With CodePos^ do
        DoLog('Found: %s @ %d,%d', [Code.Filename, Y,X]);}
      ANode:=TreeOfPCodeXYPosition.FindPrecessor(ANode);
    end;

  finally
    Files.Free;
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    Cache.Free;
  end;
end;

{ TReferencesRequest }

function TReferencesRequest.Process(var Params: TReferenceParams): TLocationItems;
var
  Path: String;
  X, Y: Integer;

begin with Params do
  begin
    Path := textDocument.LocalPath;
    X := position.character;
    Y := position.line;

    Result := TLocationItems.Create;
    // if the main program file was provided via initializationOptions -> program
    // then use this unit as the root for searching, otherwise default to the
    // current text document
    if ServerSettings.&program <> '' then
      FindReferences(Path, ServerSettings.&program, X + 1, Y + 1, Result)
    else
      FindReferences(Path, Path, X + 1, Y + 1, Result);
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/references', TReferencesRequest);
end.

