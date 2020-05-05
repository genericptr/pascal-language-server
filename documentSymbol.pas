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

unit documentSymbol;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, URIParser,
  CodeToolManager, CodeCache, CodeTree,
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
  protected
    procedure Assign(source: TPersistent); override;
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


  { TSymbolTableEntry }
  TSymbolTableEntry = class
    private
      Key: ShortString;
      Items: TSymbolInformationItems;
      Code: TCodeBuffer;
    public
      constructor Create(_Code: TCodeBuffer);
      destructor Destroy; override;
      procedure Clear;
  end;

  { TSymbolExtractor }

  TSymbolExtractor = class
    private
      Code: TCodeBuffer;
      Tool: TCodeTool;
      Entry: TSymbolTableEntry;
      UsedIdentifiers: TFPHashList;

      procedure WalkTree(node: TCodeTreeNode); 
      function FindProcHead(Node: TCodeTreeNode): TCodeTreeNode;
      function AddSymbol(Pos: LongInt; Kind: TSymbolKind): TSymbolInformation; overload;
      function AddSymbol(Pos: LongInt; Name: String; Kind: TSymbolKind): TSymbolInformation; overload;
      function GetIdentifier(Identifier: PChar; const aSkipAmp, IncludeDot: Boolean): string;
      function GetIdentifierAtPos(StartPos: Longint; aSkipAmp: Boolean = true; IncludeDot: Boolean = false): String; inline;
    public
      constructor Create(_Entry: TSymbolTableEntry; _Code: TCodeBuffer; _Tool: TCodeTool);
      destructor Destroy; override;
  end;

  { TSymbolManager }

  TSymbolManager = class
    private
      class var _Manager: TSymbolManager;
    private
      SymbolTable: TFPHashObjectList;
      ErrorList: TStringList;

      function Load(Path: String): TCodeBuffer;
      procedure RemoveFile(FileName: String);
      procedure AddError(Message: String); 
    public
      class function SharedManager: TSymbolManager;

      constructor Create;
      destructor Destroy; override;

      function CollectSymbols: TSymbolInformationItems;
      procedure ClearErrors;

      function Find(Path: String): TSymbolInformationItems;
      function Reload(Code: TCodeBuffer): TSymbolInformationItems; overload;
      function Reload(Path: String): TSymbolInformationItems; overload;
      procedure Scan(Path: String);
  end;

function SymbolKindToString(kind: TSymbolKind): ShortString;
function SymbolKindFromString(kind: ShortString): TSymbolKind;

implementation
uses
  SysUtils, FileUtil,
  CodeToolsConfig, IdentCompletionTool, 
  BasicCodeTools, FindDeclarationTool, PascalParserTool,
  KeywordFuncLists;

function SymbolKindToString(kind: TSymbolKind): ShortString;
begin
  case kind of
    SymbolKindFile: result := 'File';
    SymbolKindModule: result := 'Module';
    SymbolKindNamespace: result := 'Namespace';
    SymbolKindPackage: result := 'Package';
    SymbolKindClass: result := 'Class';
    SymbolKindMethod: result := 'Method';
    SymbolKindProperty: result := 'Property';
    SymbolKindField: result := 'Field';
    SymbolKindConstructor: result := 'Constructor';
    SymbolKindEnum: result := 'Enum';
    SymbolKindInterface: result := 'Interface';
    SymbolKindFunction: result := 'Function';
    SymbolKindVariable: result := 'Variable';
    SymbolKindConstant: result := 'Constant';
    SymbolKindString: result := 'String';
    SymbolKindNumber: result := 'Number';
    SymbolKindBoolean: result := 'Boolean';
    SymbolKindArray: result := 'Array';
    SymbolKindObject: result := 'Object';
    SymbolKindKey: result := 'Key';
    SymbolKindNull: result := 'Null';
    SymbolKindEnumMember: result := 'EnumMember';
    SymbolKindStruct: result := 'Struct';
    SymbolKindEvent: result := 'Event';
    SymbolKindOperator: result := 'Operator';
    SymbolKindTypeParameter: result := 'TypeParameter'
  end;
end;

function SymbolKindFromString(kind: ShortString): TSymbolKind;
begin
  if (kind = 'File') then result := SymbolKindFile
  else if (kind = 'Module') then result := SymbolKindModule
  else if (kind = 'Namespace') then result := SymbolKindNamespace
  else if (kind = 'Package') then result := SymbolKindPackage
  else if (kind = 'Class') then result := SymbolKindClass
  else if (kind = 'Method') then result := SymbolKindMethod
  else if (kind = 'Property') then result := SymbolKindProperty
  else if (kind = 'Field') then result := SymbolKindField
  else if (kind = 'Constructor') then result := SymbolKindConstructor
  else if (kind = 'Enum') then result := SymbolKindEnum
  else if (kind = 'Interface') then result := SymbolKindInterface
  else if (kind = 'Function') then result := SymbolKindFunction
  else if (kind = 'Variable') then result := SymbolKindVariable
  else if (kind = 'Constant') then result := SymbolKindConstant
  else if (kind = 'String') then result := SymbolKindString
  else if (kind = 'Number') then result := SymbolKindNumber
  else if (kind = 'Boolean') then result := SymbolKindBoolean
  else if (kind = 'Array') then result := SymbolKindArray
  else if (kind = 'Object') then result := SymbolKindObject
  else if (kind = 'Key') then result := SymbolKindKey
  else if (kind = 'Null') then result := SymbolKindNull
  else if (kind = 'EnumMember') then result := SymbolKindEnumMember
  else if (kind = 'Struct') then result := SymbolKindStruct
  else if (kind = 'Event') then result := SymbolKindEvent
  else if (kind = 'Operator') then result := SymbolKindOperator
  else if (kind = 'TypeParameter') then result := SymbolKindTypeParameter
end;

{ TSymbolInformation }

procedure TSymbolInformation.Assign(Source: TPersistent);
begin
  Name := TSymbolInformation(Source).Name;
  Kind := TSymbolInformation(Source).Kind;
  Deprecated := TSymbolInformation(Source).Deprecated;
  Location := TSymbolInformation(Source).Location;
  ContainerName := TSymbolInformation(Source).ContainerName;
end;


{ TSymbolTableEntry }

procedure TSymbolTableEntry.Clear;
begin
  Items.Clear;
end;

destructor TSymbolTableEntry.Destroy; 
begin
  Items.Free;
  inherited;
end;

constructor TSymbolTableEntry.Create(_Code: TCodeBuffer);
begin
  Code := _Code;
  Key := ExtractFileName(Code.FileName);
  Items := TSymbolInformationItems.Create;
end;

{ TSymbolExtractor }

function TSymbolExtractor.GetIdentifier(Identifier: PChar; const aSkipAmp, IncludeDot: Boolean): string;
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

function TSymbolExtractor.GetIdentifierAtPos(StartPos: Longint; aSkipAmp: Boolean = true; IncludeDot: Boolean = false): String;
var
  Source: String;
begin
  Source := Tool.Scanner.CleanedSrc;
  Result := GetIdentifier(@Source[StartPos], aSkipAmp, IncludeDot);
end;

function TSymbolExtractor.FindProcHead(Node: TCodeTreeNode): TCodeTreeNode;
begin
  Assert(Node.Desc = ctnProcedure, 'Must start from procedure node');
  result := nil;
  Node := Node.FirstChild;
  while Node <> nil do
    begin
      if Node.Desc = ctnProcedureHead then
        exit(Node);
      Node := Node.NextBrother;
    end;
end;

function TSymbolExtractor.AddSymbol(Pos: LongInt; Kind: TSymbolKind): TSymbolInformation;
var
  Identifier: string;
begin
  Identifier := GetIdentifierAtPos(Pos, true, true);
  Result := AddSymbol(Pos, Identifier, Kind);
end;

function TSymbolExtractor.AddSymbol(Pos: LongInt; Name: String; Kind: TSymbolKind): TSymbolInformation;
var
  Symbol: TSymbolInformation;
  CodePos: TCodePosition;
  Line, Column: Integer;
begin
  Tool.CleanPosToCodePos(Pos, CodePos);
  Code.AbsoluteToLineCol(CodePos.P, Line, Column);

  Symbol := TSymbolInformation(Entry.Items.Add);
  Symbol.name := Name;
  Symbol.kind := Kind;
  Symbol.location := TLocation.Create(Code.FileName, Line - 1, Column - 1, Length(Name));

  Result := Symbol;
end;

procedure TSymbolExtractor.WalkTree(Node: TCodeTreeNode); 
var
  ProcHead: String;
  Symbol: TSymbolInformation;
begin
  while Node <> nil do
    begin
      //writeln(Node.DescAsString, ' -> ', Node.ChildCount);

      case Node.Desc of
        
        // top-level procedures/functions
        ctnProcedure:
          begin
            // todo: how do we know if this is a method or function?
            ProcHead := Tool.ExtractProcName(Node, []);
            if UsedIdentifiers.Find(ProcHead) = nil then
              begin
                // todo: for TDocumentSymbol there is a detail field 
                // which we can fill with the signature
                //ProcHead := Tool.ExtractProcHead(Node,
                //                             [phpWithResultType,
                //                             phpWithoutClassKeyword,
                //                             phpWithoutClassName,
                //                             phpWithoutSemicolon,
                //                             phpDoNotAddSemicolon,
                //                             phpWithParameterNames
                //                             ]);
                Symbol := AddSymbol(Node.StartPos, ProcHead, SymbolKindFunction);
                UsedIdentifiers.Add(ProcHead, Symbol);
              end;
            // stop searching so we don't get into parameter lists
            Node := Node.NextBrother;
            continue;
          end;

        ctnVarDefinition:
          AddSymbol(Node.StartPos, SymbolKindVariable);
        ctnConstDefinition:
          AddSymbol(Node.StartPos, SymbolKindConstant);
        ctnEnumIdentifier:
          AddSymbol(Node.StartPos, SymbolKindEnum);
        ctnTypeDefinition:
          begin
            // todo: how do we know if this is a class type?
            AddSymbol(Node.StartPos, SymbolKindTypeParameter);
          end;

        // object members
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
            // TODO: we only do this when we return TDocumentSymbol which
            // support a tree structure (using children)
            Node := Node.NextBrother;
            continue;
          end;
      end;              

      if Node.ChildCount > 0 then
        WalkTree(Node.FirstChild);  

      Node := Node.NextBrother;
    end;
end;

constructor TSymbolExtractor.Create(_Entry: TSymbolTableEntry; _Code: TCodeBuffer; _Tool: TCodeTool);
begin
  Entry := _Entry;
  Code := _Code;
  Tool := _Tool;
  UsedIdentifiers := TFPHashList.Create;
end;

destructor TSymbolExtractor.Destroy; 
begin
  UsedIdentifiers.Free;
  inherited;
end;

{ TSymbolManager }

procedure TSymbolManager.RemoveFile(FileName: String);
var
  Index: integer;
begin
  Index := SymbolTable.FindIndexOf(FileName);
  if Index <> -1 then
    SymbolTable.Delete(Index);
end;

function TSymbolManager.CollectSymbols: TSymbolInformationItems;
var
  i: integer;
  Item: TCollectionItem;
  Symbol: TSymbolInformation;
  Entry: TSymbolTableEntry;
begin
  Result := TSymbolInformationItems.Create;
  for i := 0 to SymbolTable.Count - 1 do
    begin
      Entry := TSymbolTableEntry(SymbolTable[i]);
      if Entry <> nil then
        for Item in Entry.Items do
          begin
            Symbol := TSymbolInformation.Create(Result);
            Symbol.Assign(Item);
          end;
    end;
end;

procedure TSymbolManager.ClearErrors; 
begin
  ErrorList.Clear;
end;

procedure TSymbolManager.AddError(Message: String); 
begin
  // todo: diagnostics is not really working right now
  // so just log to stderr instead
  //ErrorList.Add(Message);
  writeln(StdErr, Message);
  Flush(stderr);
end;

procedure TSymbolManager.Scan(Path: String);
var
  Files: TStringList;
  FileName: String;
begin
  Files := TStringList.Create;
  try
    FindAllFiles(Files, Path, '*.pas;*.pp;*.p;*.inc', true);
    for FileName in Files do
      begin
        Reload(FileName);
      end;
    writeln('loaded ', SymbolTable.Count, ' entries.');
  finally
    Files.Free;
  end;

end;

//type
//  TSymbolManagerThread = class(TThread)
//    private
//      class var Pending: TStringList;
//      class var QueueLock: TCriticalSection;
//    protected
//      class procedure AddPending(Path: String);
//      procedure Execute; override;
//  end;

//class procedure TSymbolManagerThread.AddPending(Path: String);
//begin
//  QueueLock.Enter;
//  Pending.Add(Path);
//  QueueLock.Leave;
//  Wakup;
//end;

//procedure TSymbolManagerThread.Execute;
//var
//  Path: String;
//begin
//  QueueLock.Enter;
//  Path := Pending.Last;
//  Pending.Delete(Pending.Count - 1);
//  writeln('execute Path');
//  QueueLock.Leave;
//end;

function TSymbolManager.Load(Path: String): TCodeBuffer;
begin
  Result := CodeToolBoss.FindFile(Path);  

  // the file hasn't been loaded yet
  if Result = nil then
    Result := CodeToolBoss.LoadFile(Path, true, false);     
   
  if Result = nil then
    AddError('file '+Path+' can''t be loaded');
end;

function TSymbolManager.Find(Path: String): TSymbolInformationItems;
var
  Entry: TSymbolTableEntry;
  FileName: ShortString;
begin
  // TODO: make a TSymbolTableEntry.FileKey to standardize this
  FileName := ExtractFileName(Path);
  Entry := TSymbolTableEntry(SymbolTable.Find(FileName));
  if Entry <> nil then
    Result := Entry.Items
  else
    Result := nil;
end;

function TSymbolManager.Reload(Code: TCodeBuffer): TSymbolInformationItems;
var
  FileName: ShortString;
  Entry: TSymbolTableEntry;
  Tool: TCodeTool;
  Extractor: TSymbolExtractor;
begin
  if not CodeToolBoss.Explore(Code, Tool, false, false) then
    begin
      // todo: these errors are overwhelming on startup so we probably need a better way
      //AddError(CodeToolBoss.ErrorMessage+' @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
      exit(nil);
    end;

  FileName := ExtractFileName(Code.FileName);

  // find a symbol table entry for the file
  Entry := TSymbolTableEntry(SymbolTable.Find(FileName));
  if Entry = nil then
    begin
      Entry := TSymbolTableEntry.Create(Code);
      SymbolTable.Add(FileName, Entry);
    end;

  // clear existing items before reloading
  Entry.Clear;

  // now that we have a symbol table entry we can extract
  // relevant symbols from the node tree
  Extractor := TSymbolExtractor.Create(Entry, Code, Tool);  
  Extractor.WalkTree(Tool.Tree.Root);
  Extractor.Free;

  result := Entry.Items;
end;

function TSymbolManager.Reload(Path: String): TSymbolInformationItems;
var
  Code: TCodeBuffer;
begin
  Code := Load(Path);
  if Code = nil then
    exit(nil);
  Result := Reload(Code);
end;

class function TSymbolManager.SharedManager: TSymbolManager;
begin
  if _Manager = nil then
    _Manager := TSymbolManager.Create;
  result := _Manager;
end;

constructor TSymbolManager.Create;
begin
  SymbolTable := TFPHashObjectList.Create;
  ErrorList := TStringList.Create;
end;

destructor TSymbolManager.Destroy; 
begin
  ErrorList.Free;
  SymbolTable.Free;
  inherited;
end;

{ TDocumentSymbolRequest }

function TDocumentSymbolRequest.Process(var Params: TDocumentSymbolParams): TPersistent;
var
  URI: TURI;
  Path: String;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Path := URI.Path + URI.Document;
    result := TSymbolManager.SharedManager.Find(Path);
  end;
end;

initialization
  //TSymbolExtractorThread.Pending := TStringList.Create;
  //TSymbolExtractorThread.QueueLock := TCriticalSection.Create;

  LSPHandlerManager.RegisterHandler('textDocument/documentSymbol', TDocumentSymbolRequest);
end.
