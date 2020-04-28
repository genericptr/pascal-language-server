
unit references;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  SysUtils, Classes, 
  { CodeTools }
  URIParser, CodeToolManager, CodeCache, CTUnitGraph,
  { LazUtils }
  LazFileUtils, Laz_AVL_Tree,
  { LSP }
  lsp, basic, general;

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

procedure FindReferences(Filename, MainFilename: String; X, Y: Integer; Items: TLocationItems);
var
  DeclCode, StartSrcCode, Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
  DeclX, DeclY, DeclTopLine, i: Integer;
  Identifier, CurLine: string;
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
    writeln(StdErr, 'CodeToolBoss.FindMainDeclaration failed: ',CodeToolBoss.ErrorMessage);
    ExitCode:=-1;
    exit;
  end;

  // Step 3: get identifier
  CodeToolBoss.GetIdentifierAt(DeclCode,DeclX,DeclY,Identifier);
  writeln(StdErr, 'Found declaration: ',Identifier);

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
      Code:=CodeToolBoss.LoadFile(Files[i],true,false);
      if Code=nil then begin
        writeln(stderr, 'unable to load "',Files[i],'"');
        continue;
      end;
      // search references
      CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      if not CodeToolBoss.FindReferences(
        DeclCode,DeclX,DeclY,
        Code, true, ListOfPCodeXYPosition, Cache) then
      begin
        writeln(stderr, 'FindReferences failed in "',Code.Filename,'"');
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
      Loc := TLocationItem(Items.Add);
      Loc.URI := PathToURI(CodePos^.Code.Filename);
      Loc.Range := TRange.Create(CodePos^.Y - 1, CodePos^.X - 1);
      ANode:=TreeOfPCodeXYPosition.FindPrecessor(ANode);
    end;

  finally
    Files.Free;
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    Cache.Free;
    Flush(stderr);
  end;
end;

{ TReferencesRequest }

function TReferencesRequest.Process(var Params: TReferenceParams): TLocationItems;
var
  URI: TURI;
  Path, Root: String;
  X, Y: Integer;
  List: TFPList;
  Cache: TFindIdentifierReferenceCache;
  Item: Pointer;
  Pos: TCodeXYPosition;
  Loc: TLocationItem;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Path := URI.Path + URI.Document;
    X := position.character;
    Y := position.line;

    List := nil;
    Cache := nil;
    Result := TLocationItems.Create;
    // if the main program file was provided via initializationOptions -> program
    // then use this unit as the root for searching, otherwise default to the
    // current text document
    if MainProgramFile <> '' then
      Root := MainProgramFile
    else
      Root := Path;
    FindReferences(Path, Path, X + 1, Y + 1, Result);
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/references', TReferencesRequest);
end.

