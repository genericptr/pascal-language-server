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
{define SYMBOL_DEBUG}

interface

uses
  Classes, Contnrs, URIParser, fpjson, fpjsonrpc, SQLite3,
  CodeToolManager, CodeCache, CodeTree, LinkScanner,
  lsp, basic, codeUtils;

type
  TSymbolKind = (
    __UNUSED__,
    _File,
    _Module,
    _Namespace,
    _Package,
    _Class,
    _Method,
    _Property,
    _Field,
    _Constructor,
    _Enum,
    _Interface,
    _Function,
    _Variable,
    _Constant,
    _String,
    _Number,
    _Boolean,
    _Array,
    _Object,
    _Key,
    _Null,
    _EnumMember,
    _Struct,
    _Event,
    _Operator,
    _TypeParameter
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
    fKind: TSymbolKind;
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
    // The kind of this symbol.
    property kind: TSymbolKind read fKind write fKind;
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
    fDeprecated: TOptionalBoolean;
    fLocation: TLocation;
    fContainerName: string;
  published
    // The name of this symbol.
    property name: string read fName write fName;
    // The kind of this symbol.
    property kind: TSymbolKind read fKind write fKind;
    // Indicates if this symbol is deprecated.
    property deprecated: TOptionalBoolean read fDeprecated write fDeprecated;
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
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
  end;

  { TSymbol }
  
  type
    TSymbolFlag = (ForwardClassDefinition);
    TSymbolFlags = set of TSymbolFlag;

  { Extra symbol container for storage }

  TSymbol = class(TSymbolInformation)
  private
    function GetFullName: String;
  public
    RawJSON: String;
    Flags: TSymbolFlags;
    OverloadCount: integer;
    function Path: String;
    function IsGlobal: boolean;
    property FullName: String read GetFullName;
    constructor Create; overload;
  end;
  TSymbolItems = specialize TGenericCollection<TSymbol>;

  { TSymbolTableEntry }

  TSymbolTableEntry = class
  private
    Key: ShortString;
    Symbols: TSymbolItems;
    Code: TCodeBuffer;
    fRawJSON: String;
    function GetRawJSON: String; inline;
  public
    Modified: Boolean;
  public
    constructor Create(_Code: TCodeBuffer);
    destructor Destroy; override;
    procedure Clear;
    procedure SerializeSymbols;
    function AddSymbol(Name: String; Kind: TSymbolKind; FileName: String; Line, Column, RangeLen: Integer): TSymbol;
    function RequestReload: boolean;
    function Count: integer; inline;
    property RawJSON: String read GetRawJSON;
  end;

  { TSymbolExtractor }

  TSymbolExtractor = class
  private
    Code: TCodeBuffer;
    Tool: TCodeTool;
    Entry: TSymbolTableEntry;
    OverloadMap: TFPHashList;
    RelatedFiles: TFPHashList;
    IndentLevel: integer;
    CodeSection: TCodeTreeNodeDesc;
  private
    procedure PrintNodeDebug(Node: TCodeTreeNode; Deep: boolean = false);
    function AddSymbol(Node: TCodeTreeNode; Kind: TSymbolKind): TSymbol; overload;
    function AddSymbol(Node: TCodeTreeNode; Kind: TSymbolKind; Name: String; Container: String = ''): TSymbol; overload;
    procedure ExtractCodeSection(Node: TCodeTreeNode);
    procedure ExtractProcedure(ParentNode, Node: TCodeTreeNode);
    procedure ExtractTypeDefinition(TypeDefNode, Node: TCodeTreeNode); 
    procedure ExtractObjCClassMethods(ClassNode, Node: TCodeTreeNode);
  public
    constructor Create(_Entry: TSymbolTableEntry; _Code: TCodeBuffer; _Tool: TCodeTool);
    destructor Destroy; override;
  end;

  { TSQLiteDatabase }

  TSQLiteDatabase = class
  protected
    Database: psqlite3;
    function SingleQuery(Stat: String): boolean;
    function Exec(Stat: String): boolean;
    procedure LogError(errmsg: pansichar); virtual;
  end;

  { TSymbolDatabase }

  TSymbolDatabase = class(TSQLiteDatabase)
  private
    procedure LogError(errmsg: pansichar); override;
  public
    constructor Create(Path: String);

    { Symbols }
    function FindAllSymbols(Path: String): TJSONSerializedArray;
    function FindSymbols(Query: String): TJSONSerializedArray;
    procedure ClearSymbols(Path: String); 
    procedure InsertSymbol(Symbol: TSymbol); 
    procedure InsertSymbols(Collection: TSymbolItems; StartIndex, EndIndex: Integer); 

    { Files }
    procedure TouchFile(Path: String);
    function FileModified(Path: String): boolean;
    procedure InsertFile(Path: String);
  end;

  { TSymbolManager }

  TSymbolManager = class
  private
    SymbolTable: TFPHashObjectList;
    ErrorList: TStringList;
    fDatabase: TSymbolDatabase;

    function Load(Path: String): TCodeBuffer;
    procedure RemoveFile(FileName: String);
    procedure AddError(Message: String); 
    function GetEntry(Code: TCodeBuffer): TSymbolTableEntry;
    function GetDatabase: TSymbolDatabase; inline;
    property Database: TSymbolDatabase read GetDatabase;
  public

    { Constructors }
    constructor Create;
    destructor Destroy; override;

    { Searching }
    function FindDocumentSymbols(Path: String): TJSONSerializedArray;
    function FindWorkspaceSymbols(Query: String): TJSONSerializedArray;
    function CollectSerializedSymbols: TJSONSerializedArray;

    { Errors }
    procedure ClearErrors;

    { Loading }
    procedure Reload(Code: TCodeBuffer; Always: Boolean = false); overload;
    procedure Reload(Path: String; Always: Boolean = false); overload;
    procedure Scan(Path: String; SearchSubDirs: Boolean);
    procedure FileModified(Code: TCodeBuffer);
  end;

var
  SymbolManager: TSymbolManager = nil;

function SymbolKindToString(kind: TSymbolKind): ShortString;
function SymbolKindFromString(kind: ShortString): TSymbolKind;

implementation
uses
  SysUtils, FileUtil, DateUtils, fpjsonrtti, 
  CodeToolsConfig, IdentCompletionTool, CodeAtom,
  BasicCodeTools, FindDeclarationTool, PascalParserTool, KeywordFuncLists,
  settings, diagnostics;

function SymbolKindToString(kind: TSymbolKind): ShortString;
begin
  case kind of
    TSymbolKind._File: result := 'File';
    TSymbolKind._Module: result := 'Module';
    TSymbolKind._Namespace: result := 'Namespace';
    TSymbolKind._Package: result := 'Package';
    TSymbolKind._Class: result := 'Class';
    TSymbolKind._Method: result := 'Method';
    TSymbolKind._Property: result := 'Property';
    TSymbolKind._Field: result := 'Field';
    TSymbolKind._Constructor: result := 'Constructor';
    TSymbolKind._Enum: result := 'Enum';
    TSymbolKind._Interface: result := 'Interface';
    TSymbolKind._Function: result := 'Function';
    TSymbolKind._Variable: result := 'Variable';
    TSymbolKind._Constant: result := 'Constant';
    TSymbolKind._String: result := 'String';
    TSymbolKind._Number: result := 'Number';
    TSymbolKind._Boolean: result := 'Boolean';
    TSymbolKind._Array: result := 'Array';
    TSymbolKind._Object: result := 'Object';
    TSymbolKind._Key: result := 'Key';
    TSymbolKind._Null: result := 'Null';
    TSymbolKind._EnumMember: result := 'EnumMember';
    TSymbolKind._Struct: result := 'Struct';
    TSymbolKind._Event: result := 'Event';
    TSymbolKind._Operator: result := 'Operator';
    TSymbolKind._TypeParameter: result := 'TypeParameter'
  end;
end;

function SymbolKindFromString(kind: ShortString): TSymbolKind;
begin
  if (kind = 'File') then result := TSymbolKind._File
  else if (kind = 'Module') then result := TSymbolKind._Module
  else if (kind = 'Namespace') then result := TSymbolKind._Namespace
  else if (kind = 'Package') then result := TSymbolKind._Package
  else if (kind = 'Class') then result := TSymbolKind._Class
  else if (kind = 'Method') then result := TSymbolKind._Method
  else if (kind = 'Property') then result := TSymbolKind._Property
  else if (kind = 'Field') then result := TSymbolKind._Field
  else if (kind = 'Constructor') then result := TSymbolKind._Constructor
  else if (kind = 'Enum') then result := TSymbolKind._Enum
  else if (kind = 'Interface') then result := TSymbolKind._Interface
  else if (kind = 'Function') then result := TSymbolKind._Function
  else if (kind = 'Variable') then result := TSymbolKind._Variable
  else if (kind = 'Constant') then result := TSymbolKind._Constant
  else if (kind = 'String') then result := TSymbolKind._String
  else if (kind = 'Number') then result := TSymbolKind._Number
  else if (kind = 'Boolean') then result := TSymbolKind._Boolean
  else if (kind = 'Array') then result := TSymbolKind._Array
  else if (kind = 'Object') then result := TSymbolKind._Object
  else if (kind = 'Key') then result := TSymbolKind._Key
  else if (kind = 'Null') then result := TSymbolKind._Null
  else if (kind = 'EnumMember') then result := TSymbolKind._EnumMember
  else if (kind = 'Struct') then result := TSymbolKind._Struct
  else if (kind = 'Event') then result := TSymbolKind._Event
  else if (kind = 'Operator') then result := TSymbolKind._Operator
  else if (kind = 'TypeParameter') then result := TSymbolKind._TypeParameter
end;

function GetFileKey(Path: String): ShortString;
begin
  result := ExtractFileName(Path);
end;

function IndentLevelString(level: integer): ShortString;
var
  i: integer;
begin
  result := '';
  for i := 0 to level - 1 do
    result += '  ';
end;

{ TSymbol }

function TSymbol.Path: String;
var
  URI: TURI;
begin
  URI := ParseURI(location.uri);
  Result := URI.Path + URI.Document;
end;

function TSymbol.IsGlobal: boolean;
begin
  result := containerName <> '';
end;

function TSymbol.GetFullName: String;
begin
  if containerName <> '' then
    Result := containerName+'.'+Name
  else
    Result := Name;
end;

constructor TSymbol.Create;
begin
  // we need this dummy constructor for serializing
  Create(nil);
end;

{ TSymbolTableEntry }

function TSymbolTableEntry.GetRawJSON: String;
var
  JSON: TJSONSerializedArray;
begin
  if (fRawJSON = '') and (SymbolManager.Database <> nil) then
    begin
      JSON := SymbolManager.Database.FindAllSymbols(Code.FileName);
      fRawJSON := JSON.AsJson;
      JSON.Free;
    end;
  Result := fRawJSON;
end;

function TSymbolTableEntry.Count: integer;
begin
  result := Symbols.Count;
end;

function TSymbolTableEntry.RequestReload: boolean;
var
  Database: TSymbolDatabase;
  Path: String;
  JSON: TJSONSerializedArray;
begin
  if Modified then
    exit(true);
  Database := SymbolManager.Database;
  Path := Code.FileName;
  Result := false;

  if Database <> nil then
    begin
      Database.InsertFile(Path);
      if Database.FileModified(Path) then
        begin
          Database.TouchFile(Path);
          Result := true;
        end;
    end
  else
    Result := true;
end;

function TSymbolTableEntry.AddSymbol(Name: String; Kind: TSymbolKind; FileName: String; Line, Column, RangeLen: Integer): TSymbol;
var
  Symbol: TSymbol;
begin
  Symbol := TSymbol(Symbols.Add);
  Symbol.name := Name;
  Symbol.kind := Kind;
  Symbol.location := TLocation.Create(FileName, Line - 1, Column - 1, RangeLen);

  { TODO: In the latest version of LSP they claim container name is supported 
    so consider adding some context for the hierarchy }
  //Symbol.containerName := 'Interface > TClass > Function';

  result := Symbol;
end;

procedure TSymbolTableEntry.SerializeSymbols;
const
  BATCH_COUNT = 1000;
var
  SerializedItems: TJSONArray;
  i, Start, Next, Total: Integer;
  Symbol: TSymbol;
begin
  SerializedItems := specialize TLSPStreaming<TSymbolItems>.ToJSON(Symbols) as TJSONArray;
  
  //writeln(stderr, 'serialize ', key, ': ', SerializedItems.count);flush(stderr);

  for i := 0 to SerializedItems.Count - 1 do
    begin
      Symbol := TSymbol(Symbols.Items[i]);
      Symbol.RawJSON := SerializedItems[i].AsJson;
    end;

  // if a database is available then insert serialized symbols in batches
  if SymbolManager.Database <> nil then
    begin
      Next := 0;
      Start := 0;
      Total := SerializedItems.Count;
      while Start < Total do
        begin
          Next := Start + BATCH_COUNT;
          if Next >= Total then
            Next := Total - 1;
          SymbolManager.Database.InsertSymbols(Symbols, Start, Next);
          Start := Next + 1;
        end;
    end;

  fRawJSON := SerializedItems.AsJSON;

  SerializedItems.Free;
  Symbols.Clear;
end;

procedure TSymbolTableEntry.Clear;
begin
  Modified := false;
  Symbols.Clear;
  if SymbolManager.Database <> nil then
    SymbolManager.Database.ClearSymbols(Code.FileName);
end;

destructor TSymbolTableEntry.Destroy; 
begin
  FreeAndNil(Symbols);
  inherited;
end;

constructor TSymbolTableEntry.Create(_Code: TCodeBuffer);
begin
  Code := _Code;
  Key := ExtractFileName(Code.FileName);
  Symbols := TSymbolItems.Create;
end;

{ TSymbolExtractor }

procedure TSymbolExtractor.PrintNodeDebug(Node: TCodeTreeNode; Deep: boolean);
var
  Child: TCodeTreeNode;
begin
  {$ifdef SYMBOL_DEBUG}
  writeln(IndentLevelString(IndentLevel), Node.DescAsString, ' (', GetIdentifierAtPos(Tool, Node.StartPos, true, true), ') -> ', Node.ChildCount);
  if Deep then
    begin
      Child := Node.FirstChild;
      Inc(IndentLevel);
      while Child <> nil do
        begin
          if Child.ChildCount > 0 then
            PrintNodeDebug(Child.FirstChild, true)
          else
            PrintNodeDebug(Child);
          Child := Child.NextBrother;
        end;
      Dec(IndentLevel);
    end;
  {$endif}
end;

function TSymbolExtractor.AddSymbol(Node: TCodeTreeNode; Kind: TSymbolKind): TSymbol;
begin
  result := AddSymbol(Node, Kind, GetIdentifierAtPos(Tool, Node.StartPos, true, true));
end;

function TSymbolExtractor.AddSymbol(Node: TCodeTreeNode; Kind: TSymbolKind; Name: String; Container: String): TSymbol;
var
  CodePos: TCodeXYPosition;
  FileName: String;
begin
  {$ifdef SYMBOL_DEBUG}
  writeln(IndentLevelString(IndentLevel + 1), '* ', Name);
  {$endif}

  Tool.CleanPosToCaret(Node.StartPos, CodePos);
  
  // clear existing symbols in symbol database
  // we don't know which include files are associated
  // with each unit so we need to check each time
  // a symbol is added
  if SymbolManager.Database <> nil then
    begin
      FileName := ExtractFileName(CodePos.Code.FileName);
      if RelatedFiles.Find(FileName) = nil then
        begin
          SymbolManager.Database.ClearSymbols(CodePos.Code.FileName);
          RelatedFiles.Add(FileName, @CodePos);
        end;
    end;
    
  Result := Entry.AddSymbol(Name, Kind, CodePos.Code.FileName, CodePos.Y, CodePos.X, Node.EndPos - Node.StartPos);
end;

procedure TSymbolExtractor.ExtractObjCClassMethods(ClassNode, Node: TCodeTreeNode);
var
  Child: TCodeTreeNode;
  ExternalClass: boolean = false;
  TypeName: String;
begin
  while Node <> nil do
    begin
      PrintNodeDebug(Node);

      case Node.Desc of
        ctnClassExternal:
          begin
            ExternalClass := true;
            Tool.MoveCursorToCleanPos(Node.EndPos);
            Tool.ReadNextAtom;
            // objective-c forward class definition, i.e:
            // ACAccount = objcclass external;
            if Tool.CurPos.Flag = cafSemicolon then
              begin
                // todo: we need to assign this to the symbol so we don't show it in indexes
                //Include(ClassSymbol.Flags, TSymbolFlag.ForwardClassDefinition);
                break;
              end;
          end;
        ctnProcedure:
          begin
            AddSymbol(Node, TSymbolKind._Method, Tool.ExtractProcName(Node, []));
          end;
        ctnClassPublic,ctnClassPublished,ctnClassPrivate,ctnClassProtected,
        ctnClassRequired,ctnClassOptional:
          if ExternalClass then
            begin
              // if the class is external then search methods
              //if ExternalClass then
              //  ExtractObjCClassMethods(ClassNode, Node.FirstChild);
              TypeName := GetIdentifierAtPos(Tool, ClassNode.StartPos, true, true);
              Child := Node.FirstChild;
              while Child <> nil do
                begin
                  PrintNodeDebug(Child);
                  AddSymbol(Node, TSymbolKind._Method, TypeName+'.'+Tool.ExtractProcName(Child, []));
                  Child := Child.NextBrother;
                end;
            end;
      end;

      Node := Node.NextBrother;
    end;
end;

procedure TSymbolExtractor.ExtractTypeDefinition(TypeDefNode, Node: TCodeTreeNode); 
var
  Child: TCodeTreeNode;
begin
  while Node <> nil do
    begin
      PrintNodeDebug(Node);

      case Node.Desc of
        ctnClass,ctnClassHelper,ctnRecordHelper,ctnTypeHelper:
          begin
            AddSymbol(TypeDefNode, TSymbolKind._Class);
          end;
        ctnObject,ctnRecordType:
          begin
            AddSymbol(TypeDefNode, TSymbolKind._Struct);
          end;
        ctnObjCClass,ctnObjCCategory,ctnObjCProtocol:
          begin
            // todo: ignore forward defs!
            AddSymbol(TypeDefNode, TSymbolKind._Class);
            Inc(IndentLevel);
            ExtractObjCClassMethods(TypeDefNode, Node.FirstChild);
            Dec(IndentLevel);
          end;
        ctnSpecialize:
          begin
            // todo: is this a class/record???
            PrintNodeDebug(Node.FirstChild, true);
            AddSymbol(TypeDefNode, TSymbolKind._Class);
          end;
        ctnEnumerationType:
          begin
            AddSymbol(TypeDefNode, TSymbolKind._Enum);
            Child := Node.FirstChild;
            while Child <> nil do
              begin
                PrintNodeDebug(Child);
                // todo: make an option to show enum members in doc symbols
                //AddSymbol(Child, TSymbolKind._EnumMember, TypeName+'.'+GetIdentifierAtPos(Child.StartPos, true, true));
                Child := Child.NextBrother;
              end;
          end;
        otherwise
          begin
            AddSymbol(TypeDefNode, TSymbolKind._TypeParameter);
          end;
      end;

      Node := Node.NextBrother;
    end;
end;

procedure TSymbolExtractor.ExtractProcedure(ParentNode, Node: TCodeTreeNode);
var
  Child: TCodeTreeNode;
  Name: ShortString;
  Symbol: TSymbol;
begin
  PrintNodeDebug(Node);
    
  if ParentNode <> nil then
    Name := Tool.ExtractProcName(ParentNode, [])+'.'+Tool.ExtractProcName(Node, [])
  else
    Name := Tool.ExtractProcName(Node, []);

  Symbol := TSymbol(OverloadMap.Find(Name));
  if Symbol <> nil then
    begin
      { TODO: when newest LSP version is released on package control
        we can include the container name to be implementation
        and at least make this an option
        if the overloaded name is found in the implementation section
        then just ignore it so we have only function in the list }
      //if CodeSection = ctnImplementation then
      //  exit;

      Inc(Symbol.overloadCount);
      case ServerSettings.overloadPolicy of
        TOverloadPolicy.Duplicates:
          ;
        TOverloadPolicy.Suffix:
          Name := Name+'$'+IntToStr(Symbol.OverloadCount);
        TOverloadPolicy.Ignore:
          exit;
      end;
    end;

  Symbol := AddSymbol(Node, TSymbolKind._Function, Name);
  OverloadMap.Add(Symbol.name, Symbol);

  // recurse into procedures to find nested procedures

  if not Tool.ProcNodeHasSpecifier(Node, psForward) and
     not Tool.ProcNodeHasSpecifier(Node, psExternal) then
    begin
      Child := Node.FirstChild;
      while Child <> nil do
        begin
          if Child.Desc = ctnProcedure then
            begin
              Inc(IndentLevel);
              ExtractProcedure(Node, Child);
              Dec(IndentLevel);
            end;
          Child := Child.NextBrother;
        end;
    end;
end;

procedure TSymbolExtractor.ExtractCodeSection(Node: TCodeTreeNode); 
var
  Symbol: TSymbol = nil;
  Child: TCodeTreeNode;
  Scanner: TLinkScanner;
  LinkIndex: Integer;
begin
  while Node <> nil do
    begin
      PrintNodeDebug(Node);

      // ignore nodes from include files
      //   for main code files ignore include nodes
      //   for include files ignore main code nodes
      Scanner := Tool.Scanner;
      LinkIndex := Scanner.LinkIndexAtCleanPos(Node.StartPos);
      if (LinkIndex >= 0) and (Scanner.LinkP[LinkIndex]^.Code <> nil) and
        not (Node.Desc in AllCodeSections) and 
        (Code.FileName <> TCodeBuffer(Scanner.LinkP[LinkIndex]^.Code).FileName) then
        begin
          Node := Node.NextBrother;
          continue;
        end;

      // recurse into code sections
      if (Node.Desc in AllCodeSections) and (Node.ChildCount > 0) then
        begin
          case Node.Desc of
            ctnInterface:
              AddSymbol(Node, TSymbolKind._Namespace, kSymbolName_Interface);
            ctnImplementation:
              AddSymbol(Node, TSymbolKind._Namespace, kSymbolName_Implementation);
          end;
          CodeSection := Node.Desc;
          Inc(IndentLevel);
          ExtractCodeSection(Node.FirstChild);
          Dec(IndentLevel);
          Node := Node.NextBrother;
          continue;
        end;

      case Node.Desc of


        // todo: make constants an option?
        //ctnConstSection:
        //  begin
        //    Inc(IndentLevel);
        //    Child := Node.FirstChild;
        //    while Child <> nil do
        //      begin
        //        AddSymbol(Child, TSymbolKind._Constant);
        //        PrintNodeDebug(Child);
        //        Child := Child.NextBrother;
        //      end;
        //    Dec(IndentLevel);
        //  end;

        ctnTypeSection:
          begin
            Inc(IndentLevel);
            Child := Node.FirstChild;
            while Child <> nil do
              begin
                if Child.Desc = ctnTypeDefinition then
                  begin
                    PrintNodeDebug(Child);
                    Inc(IndentLevel);
                    ExtractTypeDefinition(Child, Child.FirstChild);
                    Dec(IndentLevel);
                  end;
                Child := Child.NextBrother;
              end;
            Dec(IndentLevel);
          end;

        ctnProcedure:
          ExtractProcedure(nil, Node);
      end;

      Node := Node.NextBrother;
    end;
end;

constructor TSymbolExtractor.Create(_Entry: TSymbolTableEntry; _Code: TCodeBuffer; _Tool: TCodeTool);
begin
  Entry := _Entry;
  Code := _Code;
  Tool := _Tool;
  OverloadMap := TFPHashList.Create;
  RelatedFiles := TFPHashList.Create;
end;

destructor TSymbolExtractor.Destroy; 
begin
  OverloadMap.Free;
  RelatedFiles.Free;
  inherited;
end;

{ TSQLiteDatabase }

procedure TSQLiteDatabase.LogError(errmsg: pansichar); 
begin
end;

function TSQLiteDatabase.SingleQuery(Stat: String): boolean;
var
  statement: psqlite3_stmt;
  errmsg: pansichar;
begin
  Result := false;
  if sqlite3_prepare_v2(database, @Stat[1], -1, @statement, @errmsg) = SQLITE_OK then
    begin
      Result := sqlite3_step(statement) = SQLITE_ROW;
      sqlite3_finalize(statement);
    end
  else
    LogError(errmsg);
end;

function TSQLiteDatabase.Exec(Stat: String): boolean;
var
  errmsg: pansichar;
begin
  result := sqlite3_exec(database, @Stat[1], nil, nil, @errmsg) = SQLITE_OK;
  if not result then
    LogError(errmsg);
end;

{ TSymbolDatabase }

procedure AddField(var Source: String; Value: String; Terminate: Boolean = true); 
begin
  Source += ''''+Value+'''';
  if Terminate then
    Source += ',';
end;

procedure AddField(var Source: String; Value: Integer; Terminate: Boolean = true); 
begin
  Source += IntToStr(Value);
  if Terminate then
    Source += ',';
end;

const
  SYMBOL_ENTRY_NAME = 0;
  SYMBOL_ENTRY_PATH = 1;
  SYMBOL_ENTRY_JSON = 2;

function TSymbolDatabase.FindAllSymbols(Path: String): TJSONSerializedArray;
var
  Stat: String;
  statement: psqlite3_stmt;
  Symbol: TSymbol;
  errmsg: pansichar;
  Contents: TLongString;
begin
  Result := nil;
  Stat := 'SELECT * FROM symbols WHERE path LIKE ''%'+Path+'%'''#0;
  if sqlite3_prepare_v2(database, @Stat[1], -1, @statement, @errmsg) = SQLITE_OK then
    begin
      Contents.Clear;
      Contents.Add('[');

      while sqlite3_step(statement) = SQLITE_ROW do
        begin
          Contents.Add(sqlite3_column_text(statement, SYMBOL_ENTRY_JSON));
          Contents.Add(',');
        end;

      if Contents.Last = ',' then
        Contents.Rewind;
      Contents.Add(']');

      Result := TJSONSerializedArray.Create(Contents.S);
      sqlite3_finalize(statement);
    end
  else
    LogError(errmsg);
end;

function TSymbolDatabase.FindSymbols(Query: String): TJSONSerializedArray;
var
  Stat: String;
  statement: psqlite3_stmt;
  Symbol: TSymbol;
  errmsg: pansichar;
  Contents: TLongString;
begin
  Result := nil;
  if Query = '' then
    Stat := 'SELECT * FROM symbols'#0
  else
    Stat := 'SELECT * FROM symbols WHERE name LIKE ''%'+Query+'%'''#0;
  if sqlite3_prepare_v2(database, @Stat[1], -1, @statement, @errmsg) = SQLITE_OK then
    begin
      Contents.Clear;
      Contents.Add('[');

      while sqlite3_step(statement) = SQLITE_ROW do
        begin
          Contents.Add(sqlite3_column_text(statement, SYMBOL_ENTRY_JSON));
          Contents.Add(',');
        end;

      if Contents.Last = ',' then
        Contents.Rewind;
      Contents.Add(']');

      Result := TJSONSerializedArray.Create(Contents.S);
      sqlite3_finalize(statement);
    end
  else
    LogError(errmsg);
end;

procedure TSymbolDatabase.LogError(errmsg: pansichar); 
begin
  // sql errors are fatal right now
  writeln(stderr, errmsg);
  flush(stderr);
  halt(-1);
end;

function TSymbolDatabase.FileModified(Path: String): boolean;
var
  Stat: String;
begin
  Stat := 'SELECT * FROM entries WHERE path = '''+Path+''' AND date != '+IntToStr(FileAge(Path));
  Result := SingleQuery(Stat);
end;

procedure TSymbolDatabase.InsertFile(Path: String);
var
  Stat: String;
  errmsg: pansichar;
begin
  Stat := 'INSERT OR IGNORE INTO entries VALUES (';
    AddField(Stat, Path);
    AddField(Stat, 0, false);
  Stat += ')'#0;
  Exec(Stat);
end;

procedure TSymbolDatabase.TouchFile(Path: String);
var
  Stat: String;
  errmsg: pansichar;
begin
  Stat := 'UPDATE entries SET date = '+IntToStr(FileAge(Path))+' WHERE path = '''+Path+''''#0;
  Exec(Stat);
end;

procedure TSymbolDatabase.ClearSymbols(Path: String);
var
  Stat: String;
begin
  Stat := 'DELETE FROM symbols WHERE path = '''+Path+'''';
  Exec(Stat);
end;

procedure TSymbolDatabase.InsertSymbols(Collection: TSymbolItems; StartIndex, EndIndex: Integer);
var
  Stat: String;
  errmsg: pansichar;
  Symbol: TSymbol;
  i: integer;
begin
  Stat := 'INSERT INTO symbols VALUES ';

  for i := StartIndex to EndIndex do
    begin
      Symbol := TSymbol(Collection.Items[i]);
      Stat += '(';

      AddField(Stat, Symbol.name);
      AddField(Stat, Symbol.Path);
      AddField(Stat, Symbol.RawJSON, false);

      Stat += ')';
      if i < EndIndex then
        Stat += ',';
    end;

  Stat += #0;

  Exec(Stat);
end;

procedure TSymbolDatabase.InsertSymbol(Symbol: TSymbol);
var
  Stat: String;
  errmsg: pansichar;
begin
  Stat := 'INSERT INTO symbols VALUES (';

  AddField(Stat, Symbol.name);
  AddField(Stat, Symbol.Path);
  AddField(Stat, Symbol.RawJSON, false);

  Stat += ')'#0;

  if sqlite3_exec(database, @Stat[1], nil, nil, @errmsg) <> SQLITE_OK then
    LogError(errmsg);
end;

constructor TSymbolDatabase.Create(Path: String);
const
  CREATE_ENTRY_TABLE = 'CREATE TABLE IF NOT EXISTS entries ('+
                       'path varchar(1023),'+
                       'date integer,'+
                       'UNIQUE(path)'+
                       ')'#0;
  CREATE_SYMBOL_TABLE = 'CREATE TABLE IF NOT EXISTS symbols ('+
                       'name varchar(255),'+
                       'path varchar(1023),'+
                       'json text'+
                       ')'#0;
begin
  // give the user some feedback on where it's loading from
  writeln(Stderr, 'Loading symbol database at ', Path);
  Flush(Stderr);

  Path += #0;
  if sqlite3_open(@Path[1], @Database) <> SQLITE_OK then
    begin
      writeln(stderr, 'Failed to load database ',Path);
      Flush(stderr);
      Assert(False, 'Failed to load database '+Path);
      halt(-1);
    end;

  Exec(CREATE_SYMBOL_TABLE);
  Exec(CREATE_ENTRY_TABLE);
end;

{ TSymbolManager }

function TSymbolManager.GetDatabase: TSymbolDatabase;
begin
  if (fDatabase = nil) and 
    (ServerSettings.symbolDatabase <> '') then 
    fDatabase := TSymbolDatabase.Create(ExpandFileName(ServerSettings.symbolDatabase));
  Result := fDatabase;
end;


procedure TSymbolManager.RemoveFile(FileName: String);
var
  Index: integer;
begin
  Index := SymbolTable.FindIndexOf(FileName);
  if Index <> -1 then
    SymbolTable.Delete(Index);
end;

function TSymbolManager.FindWorkspaceSymbols(Query: String): TJSONSerializedArray;
// note: there is a bug in Sublime Text which requires the workspace
// symbols command to send a query string so we define a wildcard
// to replace an empty query (which should return all symbols)
const
  WILDCARD_QUERY = '*';
begin
  if (Database <> nil) and (Query <> '') {and (Query <> WILDCARD_QUERY)} then
    begin
      if Query = WILDCARD_QUERY then
        Query := '';
      result := Database.FindSymbols(Query);
    end
  else
    result := CollectSerializedSymbols;
end;

{
  note: a full database query of 3MB takes 20ms so what does this really get us??
}
function TSymbolManager.CollectSerializedSymbols: TJSONSerializedArray;
var
  i, j: integer;
  Entry: TSymbolTableEntry;
  Contents: TLongString;
begin
  writeln(stderr, 'collecting serialized symbols...');
  Flush(stderr);

  Contents.Clear;
  Contents.Add('[');

  for i := 0 to SymbolTable.Count - 1 do
    begin
      Entry := TSymbolTableEntry(SymbolTable[i]);
      if Entry.RawJSON <> '' then
        if Contents.Add(Entry.RawJSON, 1, Length(Entry.RawJSON) - 2) then
          Contents.Add(',');
    end;
  
  Contents.Rewind;
  Contents.Add(']');

  Result := TJSONSerializedArray.Create(Contents.S);
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

procedure TSymbolManager.FileModified(Code: TCodeBuffer);
var
  Entry: TSymbolTableEntry;
begin
  Entry := GetEntry(Code);
  if Entry <> nil then
    Entry.Modified := true;
end;

procedure TSymbolManager.Scan(Path: String; SearchSubDirs: Boolean);
var
  Files: TStringList;
  FileName: String;
begin
  Files := TStringList.Create;
  try
    // TODO: make this an initializationOption
    FindAllFiles(Files, Path, '*.pas;*.pp;*.p', SearchSubDirs);
    for FileName in Files do
      Reload(FileName);
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
  if Result <> nil then
    exit;

  Result := CodeToolBoss.LoadFile(Path, true, false);     
  if Result = nil then
    AddError('file '+Path+' can''t be loaded');
end;

function TSymbolManager.FindDocumentSymbols(Path: String): TJSONSerializedArray;
var
  Entry: TSymbolTableEntry;
  FileName: ShortString;
  Code: TCodeBuffer;
begin

  // get the main code in case we're dealing with includes
  Code := CodeToolBoss.FindFile(Path);
  //Code := CodeToolBoss.GetMainCode(Code);
  if Code = nil then
    exit(nil);

  FileName := GetFileKey(Code.FileName);
  Entry := TSymbolTableEntry(SymbolTable.Find(FileName));

  {
    ack! we're checking if the entry was modified (locally)
    but then inside making the actual file mode check
    we need to unify these checks into one place

    1) did the file date change against the database date (ie. changed externally)
    2) is the code buffer modified? see TCodeBuffer methods instead of keeping our own

  }
  // the symtable entry was explicitly modified so it needs to be reloaded
  //if Entry.Modified then
    Reload(Path, False);

  if Entry <> nil then
    Result := TJSONSerializedArray.Create(Entry.RawJSON)
  else
    Result := nil;
end;

function TSymbolManager.GetEntry(Code: TCodeBuffer): TSymbolTableEntry;
var
  Entry: TSymbolTableEntry;
  Key: ShortString;
begin
  Key := GetFileKey(Code.FileName);
  Entry := TSymbolTableEntry(SymbolTable.Find(Key));
  if Entry = nil then
    begin
      Entry := TSymbolTableEntry.Create(Code);
      SymbolTable.Add(Key, Entry);
    end;
  result := Entry;
end;

procedure TSymbolManager.Reload(Code: TCodeBuffer; Always: Boolean = false);
var
  Entry: TSymbolTableEntry;
  Tool: TCodeTool = nil;
  Extractor: TSymbolExtractor;
  MainCode: TCodeBuffer;
  StartTime: TDateTime;
begin
  StartTime := Now;
  //Code := CodeToolBoss.GetMainCode(Code);
  
  // check if the file mod dates have changed
  Entry := GetEntry(Code);
  if not Always and not Entry.RequestReload then
    exit;

  if not CodeToolBoss.Explore(Code, Tool, false, false) then
    begin
      {$ifdef SYMBOL_DEBUG}
      writeln(StdErr, ExtractFileName(Code.FileName), ' -> ', CodeToolBoss.ErrorMessage+' @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
      {$endif}
      // todo: these errors are overwhelming on startup so we probably need a better way
      //AddError(ExtractFileName(Code.FileName)+' -> '+CodeToolBoss.ErrorMessage+' @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
      exit;
    end;

  // clear existing items before reloading
  Entry.Clear;

  // now that we have a symbol table entry we can extract
  // relevant symbols from the node tree
  Extractor := TSymbolExtractor.Create(Entry, Code, Tool);
  Extractor.ExtractCodeSection(Tool.Tree.Root);
  Extractor.Free;

  Entry.SerializeSymbols;

  writeln(StdErr, 'Reloaded ', Code.FileName, ' in ', MilliSecondsBetween(Now,StartTime),'ms');
  Flush(StdErr);
end;

procedure TSymbolManager.Reload(Path: String; Always: Boolean = false);
var
  Code: TCodeBuffer;
begin
  Code := Load(Path);
  if Code = nil then
    exit;
  Reload(Code, Always);
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

function TDocumentSymbolRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: TDocumentSymbolParams;
  URI: TURI;
  Path: String;
begin
  Input := specialize TLSPStreaming<TDocumentSymbolParams>.ToObject(Params);
  URI := ParseURI(Input.textDocument.uri);
  Path := URI.Path + URI.Document;
  Result := SymbolManager.FindDocumentSymbols(Path);
  if not Assigned(Result) then
    Result := TJSONNull.Create;
  Input.Free;
end;

initialization
  //TSymbolExtractorThread.Pending := TStringList.Create;
  //TSymbolExtractorThread.QueueLock := TCriticalSection.Create;
  LSPHandlerManager.RegisterHandler('textDocument/documentSymbol', TDocumentSymbolRequest);
end.
