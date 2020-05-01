// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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

unit documentSymbol;

{$mode objfpc}{$H+}

interface

uses
  Classes, URIParser,
  lsp, basic;

type
  TSymbolKind = (
    SymbolKindFile = 1,
    SymbolKindModule = 2,
    SymbolKindNamespace = 3,
    SymbolKindPackage = 4,
    SymbolKindClass = 5,
    SymbolKindMethod = 6,
    SymbolKindProperty = 7,
    SymbolKindField = 8,
    SymbolKindConstructor = 9,
    SymbolKindEnum = 10,
    SymbolKindInterface = 11,
    SymbolKindFunction = 12,
    SymbolKindVariable = 13,
    SymbolKindConstant = 14,
    SymbolKindString = 15,
    SymbolKindNumber = 16,
    SymbolKindBoolean = 17,
    SymbolKindArray = 18,
    SymbolKindObject = 19,
    SymbolKindKey = 20,
    SymbolKindNull = 21,
    SymbolKindEnumMember = 22,
    SymbolKindStruct = 23,
    SymbolKindEvent = 24,
    SymbolKindOperator = 25,
    SymbolKindTypeParameter = 26
  );

  { TDocumentSymbol }

  { Represents programming constructs like variables, classes, interfaces etc. that 
    appear in a document. Document symbols can be hierarchical and they have two ranges: 
    one that encloses its definition and one that points to its most interesting range,
    e.g. the range of an identifier. }

  TDocumentSymbol = class;
  TDocumentSymbolItems = specialize TGenericCollection<TDocumentSymbol>;

  TDocumentSymbol = class(TCollectionItem)
  private
    fName: string;
    fDetail: string;
    fDeprecated: boolean;
    fRange: TRange;
    fSelectionRange: TRange;
    fChildren: TDocumentSymbolItems;
  published
    // The name of this symbol. Will be displayed in the user interface and therefore must not be
    // an empty string or a string only consisting of white spaces.
    property name: string read fName write fName;
    // More detail for this symbol, e.g the signature of a function.
    property detail: string read fDetail write fDetail;
    // Indicates if this symbol is deprecated.
    property deprecated: boolean read fDeprecated write fDeprecated;
    // The range enclosing this symbol not including leading/trailing whitespace but everything else
    // like comments. This information is typically used to determine if the clients cursor is
    // inside the symbol to reveal in the symbol in the UI.
    property range: TRange read fRange write fRange;
    // The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.
    // Must be contained by the `range`.
    property selectionRange: TRange read fSelectionRange write fSelectionRange;
    // Children of this symbol, e.g. properties of a class.
    property children: TDocumentSymbolItems read fChildren write fChildren;
  end;


  { TSymbolInformation }

  { Represents information about programming constructs like variables, classes, interfaces etc. }

  TSymbolInformation = class(TCollectionItem)
  private
   fName: string;
   fKind: TSymbolKind;
   fDeprecated: boolean;
   fLocation: TLocation;
   fContainerName: string;
  published
    // The name of this symbol.
    property name: string read fName write fName;
    // The kind of this symbol.
    property kind: TSymbolKind read fKind write fKind;
    // Indicates if this symbol is deprecated.
    property deprecated: boolean read fDeprecated write fDeprecated;
    // The location of this symbol. The location's range is used by a tool
    // to reveal the location in the editor. If the symbol is selected in the
    // tool the range's start information is used to position the cursor. So
    // the range usually spans more then the actual symbol's name and does
    // normally include things like visibility modifiers.
    // 
    // The range doesn't have to denote a node range in the sense of a abstract
    // syntax tree. It can therefore not be used to re-construct a hierarchy of
    // the symbols.
    property location: TLocation read fLocation write fLocation;
    // The name of the symbol containing this symbol. This information is for
    // user interface purposes (e.g. to render a qualifier in the user interface
    // if necessary). It can't be used to re-infer a hierarchy for the document
    // symbols.
    property containerName: string read fContainerName write fContainerName;
  end;

  TSymbolInformationItems = specialize TGenericCollection<TSymbolInformation>;


  { TDocumentSymbolParams }

  TDocumentSymbolParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentIdentifier;
  published
    // The text document.
    property textDocument: TTextDocumentIdentifier read fTextDocument write fTextDocument;
  end;

  { TDocumentSymbolRequest }

  { The document symbol request is sent from the client to the server. The returned result is either:

    * SymbolInformation[] which is a flat list of all symbols found in a given text document. 
      Then neither the symbol’s location range nor the symbol’s container name should be used to infer a hierarchy.
    * DocumentSymbol[] which is a hierarchy of symbols found in a given text document. }

  TDocumentSymbolRequest = class(specialize TLSPRequest<TDocumentSymbolParams, TPersistent>)
    function Process(var Params: TDocumentSymbolParams): TPersistent; override;
  end;

implementation
uses
  // rtl
  SysUtils, FGL,
  // code tools
  CodeToolManager, CodeToolsConfig, CodeCache, IdentCompletionTool, 
  BasicCodeTools, CodeTree, FindDeclarationTool, PascalParserTool,
  KeywordFuncLists;

{
  Find all symbols in global/interface scope:

  - public class methods
  - global variables, types and constants
  - functions
  - classes, records, objects
}

type
  TSymbol = class
    Name: string;
    Kind: TSymbolKind;
    constructor Create(_Name: String; _kind: TSymbolKind);
  end;
  TSymbolList = specialize TFPGList<TSymbol>;

constructor TSymbol.Create(_Name: String; _Kind: TSymbolKind);
begin
  Name := _name;
  Kind := _Kind;
end;

type
  TDocumentSymbols = class
    private
      Code: TCodeBuffer;
      Symbols: TSymbolList; 

      function HandleProcedure(node: TCodeTreeNode): String;
      procedure HandleMembers(constref StructIdentifier: String; Node: TCodeTreeNode); 
      procedure HandleDefinitions(var node: TCodeTreeNode); 
      function GetIdentifier(Identifier: PChar; const aSkipAmp, IncludeDot: Boolean): string;
      procedure WalkTree(node: TCodeTreeNode); 
    public
      constructor Create;
  end;

constructor TDocumentSymbols.Create;
begin
  Symbols := TSymbolList.Create; 
end;

function TDocumentSymbols.GetIdentifier(Identifier: PChar; const aSkipAmp, IncludeDot: Boolean): string;
var len: integer;
begin
  if (Identifier=nil) then begin
    Result:='';
    exit;
  end;
  if IsIdentStartChar[Identifier^] or ((Identifier^='&') and (IsIdentStartChar[Identifier[1]])) then begin
    len:=0;
    if (Identifier^='&') then
    begin
      if aSkipAmp then
        inc(Identifier)
      else
        inc(len);
    end;
    while (IsIdentChar[Identifier[len]]) or (IncludeDot and (Identifier[len] = '.')) do inc(len);
    SetLength(Result,len);
    if len>0 then
      Move(Identifier[0],Result[1],len);
  end else
    Result:='';
end;

function TDocumentSymbols.HandleProcedure(node: TCodeTreeNode): String;
var
  Identifier: String = '';
begin
  while node <> nil do
    begin
      if Node.Desc = ctnProcedureHead then
        begin
          Identifier := GetIdentifier(@Code.Tool.Scanner.CleanedSrc[node.StartPos], true, true);
          result := Identifier;
          break;
        end;
      node := node.NextBrother;
    end;
end;

procedure TDocumentSymbols.HandleMembers(constref StructIdentifier: String; Node: TCodeTreeNode); 
var
  Identifier: String;
  Symbol: TSymbol;
begin
  while Node <> nil do
    begin
      //writeln('  MEMBER: ',node.Desc, ': ', node.DescAsString, ' -> ', node.ChildCount);

      case Node.Desc of
        ctnProcedure:
          begin
            Identifier := HandleProcedure(Node.FirstChild);
            if Identifier <> '' then
              begin
                Identifier := StructIdentifier+'.'+Identifier;
                Symbol := TSymbol.Create(Identifier, SymbolKindFunction);
                Symbols.Add(Symbol);
                //writeln('  PROC: ',Identifier);
              end;
          end;

        // hidden visibility
        ctnClassPrivate,
        ctnClassProtected:
          ;

        otherwise
          HandleMembers(StructIdentifier, Node.FirstChild);
      end;

      Node := Node.NextBrother;
    end;
end;

procedure TDocumentSymbols.HandleDefinitions(var node: TCodeTreeNode); 
var
  Identifier: string;
  Symbol: TSymbol;
begin
  while node <> nil do
    begin
      //writeln('DEF: ', node.DescAsString, ' -> ', node.ChildCount);

      case Node.Desc of
        ctnVarDefinition:
          begin
            // todo: handle var - get type!
            Identifier:=GetIdentifier(@Code.Source[node.StartPos], false, false);
            //writeln('  VAR: ',Identifier);
            Symbol := TSymbol.Create(Identifier, SymbolKindVariable);
            Symbols.Add(Symbol);
          end;
        ctnConstDefinition:
          begin
            Identifier:=GetIdentifier(@Code.Source[node.StartPos], false, false);
            //writeln('  CONST: ',Identifier);
            Symbol := TSymbol.Create(Identifier, SymbolKindConstant);
            Symbols.Add(Symbol);
          end;
        ctnTypeDefinition,
        ctnEnumIdentifier:
          begin
            Identifier:=GetIdentifier(@Code.Source[node.StartPos], false, false);
            writeln('  TYPE: ',Identifier);
            // todo: what wrong kind
            Symbol := TSymbol.Create(Identifier, SymbolKindTypeParameter);
            Symbols.Add(Symbol);
          end;
      end;

      // recurse into object types
      if node.ChildCount > 0 then
        case node.Desc of
          ctnClass,
          ctnRecordType,
          ctnObject,
          ctnClassInterface,
          ctnObjCClass,
          ctnObjCCategory,
          ctnObjCProtocol,
          ctnTypeHelper,
          ctnRecordHelper,
          ctnClassHelper:
            begin
              Identifier:=GetIdentifier(@Code.Source[node.Parent.StartPos], false, false);
              HandleMembers(Identifier, node.FirstChild);
            end;
          otherwise
            HandleDefinitions(node.FirstChild);
        end;

      node := node.NextBrother;
    end;
end;

procedure TDocumentSymbols.WalkTree(node: TCodeTreeNode); 
var
  Identifier: string;
  Symbol: TSymbol;
begin
  while node <> nil do
    begin
      //writeln(node.Desc, ': ', node.DescAsString, ' -> ', node.ChildCount);

      if node.ChildCount > 0 then
        case Node.Desc of
          
          // procedures/functions
          ctnProcedure:
            begin
              Identifier := HandleProcedure(node.FirstChild);
              if Identifier <> '' then
                begin
                  Symbol := TSymbol.Create(Identifier, SymbolKindFunction);
                  Symbols.Add(Symbol);
                  //writeln('  GLOBAL PROC: ',Identifier);
                end;
            end;

          // var/types/consts
          ctnVarDefinition,
          ctnTypeDefinition,
          ctnConstDefinition:
            begin
              HandleDefinitions(node);
              continue;
            end;

          otherwise
            WalkTree(node.FirstChild);  
        end;              

      node := node.NextBrother;
    end;
end;

{ TDocumentSymbolRequest }

function TDocumentSymbolRequest.Process(var Params: TDocumentSymbolParams): TPersistent;
var
  URI: TURI;
  Path: String;
  Item: TSymbolInformation;
  Items: TSymbolInformationItems;
  Code: TCodeBuffer;
  DocumentSymbols: TDocumentSymbols;
  Symbol: TSymbol;
  Tool: TCodeTool;
  i: integer;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Path := URI.Path + URI.Document;
    writeln(stderr, 'document symbols: ', Path);
    flush(stderr);

    Items := TSymbolInformationItems.Create;

    Code := CodeToolBoss.LoadFile(Path,true,false);
    
    if not CodeToolBoss.Explore(Code,Tool,false,true) then
      begin
        writeln(StdErr, 'parser error -> '+CodeToolBoss.ErrorMessage+' @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
        flush(stderr);
        exit;
      end;

    DocumentSymbols := TDocumentSymbols.Create;
    DocumentSymbols.Code := Code;
    DocumentSymbols.WalkTree(CodeToolBoss.CurCodeTool.Tree.Root);

    for Symbol in DocumentSymbols.Symbols do
      begin
        Item := TSymbolInformation(Items.Add);
        Item.name := Symbol.Name;
        Item.kind := Symbol.Kind;
        // todo: constructor
        //Item.location := TLocation.Create;
        //Item.location.uri := PathToURI(Path);
        //Item.location.range := TRange.Create(27, 1, 45, 1);
      end;
    
    DocumentSymbols.Free;

    result := Items;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/documentSymbol', TDocumentSymbolRequest);
end.
