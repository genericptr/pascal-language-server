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
  CodeToolManager, CodeCache, CodeTree,
  lsp, basic, codeUtils;

type
  TSymbolKind = (
{}    SymbolKindFile = 1,
{}    SymbolKindModule = 2,
{}    SymbolKindNamespace = 3,
{}    SymbolKindPackage = 4,
{}    SymbolKindClass = 5,
{}    SymbolKindMethod = 6,
{}    SymbolKindProperty = 7,
{}    SymbolKindField = 8,
{}    SymbolKindConstructor = 9,
{}    SymbolKindEnum = 10,
{}    SymbolKindInterface = 11,
{}    SymbolKindFunction = 12,
{}    SymbolKindVariable = 13,
{}    SymbolKindConstant = 14,
{}    SymbolKindString = 15,
{}    SymbolKindNumber = 16,
{}    SymbolKindBoolean = 17,
{}    SymbolKindArray = 18,
{}    SymbolKindObject = 19,
{}    SymbolKindKey = 20,
{}    SymbolKindNull = 21,
{}    SymbolKindEnumMember = 22,
{}    SymbolKindStruct = 23,
{}    SymbolKindEvent = 24,
{}    SymbolKindOperator = 25,
{}    SymbolKindTypeParameter = 26
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
    fDeprecated: TOptionalBoolean;
    fLocation: TLocation;
    fContainerName: TOptionalString;
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
    property containerName: TOptionalString read fContainerName write fContainerName;
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
    UsedIdentifiers: TFPHashList;
    IndentLevel: integer;
  private
    procedure WalkTree(node: TCodeTreeNode); 
    procedure ExtractClassMethods(ClassSymbol: TSymbol; Node: TCodeTreeNode);
    function FindProcHead(Node: TCodeTreeNode): TCodeTreeNode;
    function AddSymbol(StartPos, EndPos: LongInt; Kind: TSymbolKind): TSymbol; overload;
    function AddSymbol(StartPos, EndPos: LongInt; Name: String; Kind: TSymbolKind): TSymbol; overload;
    function GetIdentifier(Identifier: PChar; const aSkipAmp, IncludeDot: Boolean): string;
    function GetIdentifierAtPos(StartPos: Longint; aSkipAmp: Boolean = true; IncludeDot: Boolean = false): String; inline;
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
    procedure Reload(Path: String); overload;
    procedure Scan(Path: String; SearchSubDirs: Boolean);
  end;

var
  SymbolManager: TSymbolManager = nil;

function SymbolKindToString(kind: TSymbolKind): ShortString;
function SymbolKindFromString(kind: ShortString): TSymbolKind;

implementation
uses
  SysUtils, FileUtil, fpjsonrtti,
  CodeToolsConfig, IdentCompletionTool, CodeAtom,
  BasicCodeTools, FindDeclarationTool, PascalParserTool, KeywordFuncLists,
  settings;

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

function GetFileKey(Path: String): ShortString;
begin
  result := ExtractFileName(Path);
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
  result := ContainerName <> nil;
end;

function TSymbol.GetFullName: String;
begin
  if ContainerName <> nil then
    Result := containerName.Value+'.'+Name
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
        end
      else
        begin
          Result := False;
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
  SerializedItems := specialize TLSPStreaming<TSymbolInformationItems>.ToJSON(Symbols) as TJSONArray;
  
  //writeln('serialize ', key, ': ', SerializedItems.count);

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

  // todo: insert entry json into database
  // if we are going to support empty query strings
  fRawJSON := SerializedItems.AsJSON;

  SerializedItems.Free;

  // free the symbols also once they're serialized
  FreeAndNil(Symbols);
end;

procedure TSymbolTableEntry.Clear;
begin
  if Symbols <> nil then
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

function TSymbolExtractor.GetIdentifier(Identifier: PChar; const aSkipAmp, IncludeDot: Boolean): string;
// todo:  we need to include these also
const
  OperatorNameCharacters = ['+', '*', '-', '/', '<', '>', '=', ':'];
var
  len: integer;
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

function TSymbolExtractor.AddSymbol(StartPos, EndPos: LongInt; Kind: TSymbolKind): TSymbol;
var
  Identifier: string;
begin
  Identifier := GetIdentifierAtPos(StartPos, true, true);
  Result := AddSymbol(StartPos, EndPos, Identifier, Kind);
end;

function TSymbolExtractor.AddSymbol(StartPos, EndPos: LongInt; Name: String; Kind: TSymbolKind): TSymbol;
var
  CodePos: TCodeXYPosition;
begin
  Tool.CleanPosToCaret(StartPos, CodePos);
  Result := Entry.AddSymbol(Name, Kind, CodePos.Code.FileName, CodePos.Y, CodePos.X, EndPos - StartPos);
end;

function IndentLevelString(level: integer): ShortString;
var
  i: integer;
begin
  result := '';
  for i := 0 to level - 1 do
    result += '  ';
end;

procedure TSymbolExtractor.ExtractClassMethods(ClassSymbol: TSymbol; Node: TCodeTreeNode);
var
  ProcHead, ContainerName: String;
  Symbol: TSymbolInformation;
  ExternalClass: boolean;
begin
  Inc(IndentLevel);
  ExternalClass := false;

  while Node <> nil do
    begin
      {$ifdef SYMBOL_DEBUG}
      writeln(IndentLevelString(IndentLevel - 1), Node.DescAsString, ' -> ', Node.ChildCount);
      {$endif}

      case Node.Desc of
        ctnClassExternal:
          begin
            Tool.MoveCursorToCleanPos(Node.EndPos);
            Tool.ReadNextAtom;
            // objective-c forward class definition, i.e:
            // ACAccount = objcclass external;
            // which we should skip
            if Tool.CurPos.Flag = cafSemicolon then
              begin
                Include(ClassSymbol.Flags, TSymbolFlag.ForwardClassDefinition);
                break;
              end;
            ExternalClass := true;
          end;
        ctnProcedure:
          begin
            ProcHead := Tool.ExtractProcName(Node, []);
            //if UsedIdentifiers.Find(ProcHead) = nil then
              begin
                Symbol := AddSymbol(Node.StartPos, Node.EndPos + Length(ClassSymbol.Name), ProcHead, SymbolKindMethod);
                Symbol.containerName := TOptionalString.Create(ClassSymbol.Name);
              end;
            // stop searching so we don't get into parameter lists
            Node := Node.NextBrother;
            continue;
          end;
        ctnClassPublic,
        ctnClassPublished:
          begin
            // if the class is external then search methods
            if ExternalClass then
              ExtractClassMethods(ClassSymbol, Node.FirstChild);
          end;
      end;

      Node := Node.NextBrother;
    end;
  Dec(IndentLevel);
end;

procedure TSymbolExtractor.WalkTree(Node: TCodeTreeNode); 
var
  ProcHead: String;
  Symbol: TSymbol;
begin
  IndentLevel += 1;

  while Node <> nil do
    begin
      {$ifdef SYMBOL_DEBUG}
      writeln(IndentLevelString(IndentLevel - 1), Node.DescAsString, ' -> ', Node.ChildCount);
      {$endif}

      case Node.Desc of
        
        // top-level procedures/functions
        ctnProcedure:
          begin
            ProcHead := Tool.ExtractProcName(Node, []);
            //if UsedIdentifiers.Find(ProcHead) = nil then
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
                Symbol := AddSymbol(Node.StartPos, Node.EndPos, ProcHead, SymbolKindFunction);
                //UsedIdentifiers.Add(ProcHead, Symbol);
              end;

            // stop searching so we don't get into parameter lists
            Node := Node.NextBrother;
            continue;
          end;

        ctnVarDefinition:
          AddSymbol(Node.StartPos, Node.EndPos, SymbolKindVariable);
        ctnConstDefinition:
          AddSymbol(Node.StartPos, Node.EndPos, SymbolKindConstant);
        ctnEnumIdentifier:
          AddSymbol(Node.StartPos, Node.EndPos, SymbolKindEnum);
        ctnTypeDefinition:
          begin
            // todo: how do we know if this is a class type?
            Symbol := AddSymbol(Node.StartPos, Node.EndPos, SymbolKindTypeParameter);
          end;

        // object members
        ctnClass,
        ctnRecordType,
        ctnObject,
        ctnClassInterface,
        ctnTypeHelper,
        ctnRecordHelper,
        ctnClassHelper:
          begin
            // TODO: we only do this when we return TDocumentSymbol which
            // support a tree structure (using children)
            Node := Node.NextBrother;
            continue;
          end;

        // external objective-c classes need to extract methods
        // because there is no implementation for them
        ctnObjCClass,
        ctnObjCCategory,
        ctnObjCProtocol:
          begin
            ExtractClassMethods(Symbol, Node.FirstChild);
            Node := Node.NextBrother;
            continue;
          end;
      end;              

      if Node.ChildCount > 0 then
        WalkTree(Node.FirstChild);

      Node := Node.NextBrother;
    end;

  IndentLevel -= 1;
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
      //if Symbol.containerName <> nil then
      //  AddField(Stat, Symbol.containerName.value)
      //else
      //  AddField(Stat, '');
      AddField(Stat, Symbol.Path);
      //AddField(Stat, Symbol.location.range.start.line);
      //AddField(Stat, Symbol.location.range.start.character);
      //AddField(Stat, Symbol.location.range.&end.line);
      //AddField(Stat, Symbol.location.range.&end.character);
      //AddField(Stat, Integer(Symbol.kind));
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
  //if Symbol.containerName <> nil then
  //  AddField(Stat, Symbol.containerName.value)
  //else
  //  AddField(Stat, '');
  AddField(Stat, Symbol.Path);
  //AddField(Stat, Symbol.location.range.start.line);
  //AddField(Stat, Symbol.location.range.start.character);
  //AddField(Stat, Symbol.location.range.&end.line);
  //AddField(Stat, Symbol.location.range.&end.character);
  //AddField(Stat, Integer(Symbol.kind));
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
                       //'json text,'+
                       'UNIQUE(path)'+
                       ')'#0;
  CREATE_SYMBOL_TABLE = 'CREATE TABLE IF NOT EXISTS symbols ('+
                       'name varchar(255),'+
                       //'container_name varchar(255),'+
                       'path varchar(1023),'+
                       //'start_line integer,'+
                       //'start_character integer,'+
                       //'end_line integer,'+
                       //'end_character integer,'+
                       //'kind integer,'+
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
begin
  if (Database <> nil) and (Query <> '') then
    result := Database.FindSymbols(Query)
  else
    begin
      // todo: search workspace symbols without database
      result := CollectSerializedSymbols;
    end;
end;


function TSymbolManager.CollectSerializedSymbols: TJSONSerializedArray;
var
  i, j: integer;
  Entry: TSymbolTableEntry;
  Contents: TLongString;
begin
  Contents.Clear;
  Contents.Add('[');

  for i := 0 to SymbolTable.Count - 1 do
    begin
      Entry := TSymbolTableEntry(SymbolTable[i]);
      if Entry.RawJSON <> '' then
        begin
          Contents.Add(Entry.RawJSON, 1, Length(Entry.RawJSON) - 2);
          Contents.Add(',');
        end;
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
  Code, MainCode: TCodeBuffer;
begin

  // get the main code in case we're dealing with includes
  Code := CodeToolBoss.FindFile(Path);
  MainCode := CodeToolBoss.GetMainCode(Code);
  if MainCode = nil then
    exit(nil);

  FileName := GetFileKey(MainCode.FileName);
  Entry := TSymbolTableEntry(SymbolTable.Find(FileName));

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
  Tool: TCodeTool;
  Extractor: TSymbolExtractor;
  MainCode: TCodeBuffer;
begin
  Tool := nil;

  // if the main code is not the current code
  // then we're dealing with an include file
  MainCode := CodeToolBoss.GetMainCode(Code);
  if CompareCodeBuffers(MainCode, Code) <> 0 then
    exit;
  
  // check if the file mod dates have changed
  Entry := GetEntry(Code);
  if not Always and not Entry.RequestReload then
    exit;

  if not CodeToolBoss.Explore(Code, Tool, false, false) then
    begin
      {$ifdef SYMBOL_DEBUG}
      writeln(ExtractFileName(Code.FileName), ' -> ', CodeToolBoss.ErrorMessage+' @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
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
  Extractor.WalkTree(Tool.Tree.Root);
  Extractor.Free;

  Entry.SerializeSymbols;

  //writeln(stderr, 'reloaded ', Code.FileName);
  //flush(stderr);
end;

procedure TSymbolManager.Reload(Path: String);
var
  Code: TCodeBuffer;
begin
  Code := Load(Path);
  if Code = nil then
    exit;
  Reload(Code);
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
