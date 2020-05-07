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
{define SYMBOL_EXTRACTOR_DEBUG}

interface

uses
  Classes, Contnrs, URIParser, FPJSON,
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
  protected
    procedure Assign(source: TPersistent); override;
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
    function Process(var Params: TDocumentSymbolParams): TPersistent; override;
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
      function IsGlobal: boolean;
      property FullName: String read GetFullName;
  end;
  TSymbolItems = specialize TGenericCollection<TSymbol>;

  { TSymbolTableEntry }

  TSymbolTableEntry = class
    private
      Key: ShortString;
      Items: TSymbolItems;
      Code: TCodeBuffer;
      SerializedItems: TJSONArray;
      fRawJSON: String;
      function GetSerializedSymbol(index: integer): TSymbol; inline;
      function GetRawJSON: String;
    public
      constructor Create(_Code: TCodeBuffer);
      destructor Destroy; override;
      procedure Clear;
      procedure Serialize;
      procedure WriteToFile(Directory: String);
      function AddSymbol(Name: String; Kind: TSymbolKind; FileName: String; Line, Column: Integer): TSymbol;
      function Count: integer; inline;
      property Symbols[Index: integer]: TSymbol read GetSerializedSymbol; default;
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
      function AddSymbol(Pos: LongInt; Kind: TSymbolKind): TSymbol; overload;
      function AddSymbol(Pos: LongInt; Name: String; Kind: TSymbolKind): TSymbol; overload;
      function GetIdentifier(Identifier: PChar; const aSkipAmp, IncludeDot: Boolean): string;
      function GetIdentifierAtPos(StartPos: Longint; aSkipAmp: Boolean = true; IncludeDot: Boolean = false): String; inline;
    public
      constructor Create(_Entry: TSymbolTableEntry; _Code: TCodeBuffer; _Tool: TCodeTool);
      destructor Destroy; override;
  end;

  { TSymbolManager }

  TSymbolManager = class
    private
      SymbolTable: TFPHashObjectList;
      ErrorList: TStringList;

      function Load(Path: String): TCodeBuffer;
      procedure RemoveFile(FileName: String);
      procedure AddError(Message: String); 
    public
      constructor Create;
      destructor Destroy; override;

      function CollectSymbols: TSymbolInformationItems;
      function CollectSerializedSymbols(Duplicates: boolean = true): TJSONWrapper;
      procedure ClearErrors;

      function Find(Path: String): TSymbolItems;
      function Reload(Code: TCodeBuffer): TSymbolItems; overload;
      function Reload(Path: String): TSymbolItems; overload;
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

{ TSymbol }

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

{ TSymbolTableEntry }

function TSymbolTableEntry.GetSerializedSymbol(index: integer): TSymbol;
var
  Symbol: TSymbol;
begin
  Assert(SerializedItems <> nil, 'symbol table must be serialized first.');

  Symbol := TSymbol(Items.Items[index]);
  if Symbol.RawJSON = '' then
    Symbol.RawJSON := SerializedItems[index].AsJson;
  result := Symbol;
end;

function TSymbolTableEntry.GetRawJSON: String;
begin
  Assert(SerializedItems <> nil, 'symbol table must be serialized first.');

  if fRawJSON = '' then
    begin
      fRawJSON := SerializedItems.AsJson;
      // TODO: trim [] from string. how?
      //fRawJSON := Trim(fRawJSON, '[]');
    end;
  result := fRawJSON;
end;

function TSymbolTableEntry.Count: integer;
begin
  if SerializedItems <> nil then
    result := SerializedItems.Count
  else
    result := 0;
end;

function TSymbolTableEntry.AddSymbol(Name: String; Kind: TSymbolKind; FileName: String; Line, Column: Integer): TSymbol;
var
  Symbol: TSymbol;
begin
  Symbol := TSymbol(Items.Add);
  Symbol.name := Name;
  Symbol.kind := Kind;
  Symbol.location := TLocation.Create(FileName, Line - 1, Column - 1, Length(Name));

  result := Symbol;
end;

procedure TSymbolTableEntry.Serialize;
begin
  //writeln('Serialize ', Key, ' ', Items.Count, ' items...');
  SerializedItems := specialize TLSPStreaming<TSymbolInformationItems>.ToJSON(Items) as TJSONArray;
end;

procedure _WriteToFile (contents: AnsiString; path: string);
var
  f: TextFile;
begin
  try
    AssignFile(f, path);
    Rewrite(f);
    Write(f, contents);
    CloseFile(f);
  except
    on E:Exception do
      writeln(path, ': ', E.Message);
  end;
end;

procedure TSymbolTableEntry.WriteToFile(Directory: String);
begin
  _WriteToFile(SerializedItems.FormatJSON, Directory+'/'+Key+'.json');
  halt;
end;

procedure TSymbolTableEntry.Clear;
begin
  Items.Clear;
  FreeAndNil(SerializedItems);
end;

destructor TSymbolTableEntry.Destroy; 
begin
  Items.Free;
  FreeAndNil(SerializedItems);
  inherited;
end;

constructor TSymbolTableEntry.Create(_Code: TCodeBuffer);
begin
  Code := _Code;
  Key := ExtractFileName(Code.FileName);
  Items := TSymbolItems.Create;
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

function TSymbolExtractor.AddSymbol(Pos: LongInt; Kind: TSymbolKind): TSymbol;
var
  Identifier: string;
begin
  Identifier := GetIdentifierAtPos(Pos, true, true);
  Result := AddSymbol(Pos, Identifier, Kind);
end;

function TSymbolExtractor.AddSymbol(Pos: LongInt; Name: String; Kind: TSymbolKind): TSymbol;
var
  CodePos: TCodePosition;
  Line, Column: Integer;
begin
  Tool.CleanPosToCodePos(Pos, CodePos);
  Code.AbsoluteToLineCol(CodePos.P, Line, Column);
  Result := Entry.AddSymbol(Name, Kind, Code.FileName, Line, Column);
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
      {$ifdef SYMBOL_EXTRACTOR_DEBUG}
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
                Symbol := AddSymbol(Node.StartPos, ProcHead, SymbolKindMethod);
                Symbol.containerName := TOptionalString.Create(ClassSymbol.Name);
                //UsedIdentifiers.Add(ProcHead, Symbol);
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
      {$ifdef SYMBOL_EXTRACTOR_DEBUG}
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
                Symbol := AddSymbol(Node.StartPos, ProcHead, SymbolKindFunction);
                //UsedIdentifiers.Add(ProcHead, Symbol);
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
            Symbol := AddSymbol(Node.StartPos, SymbolKindTypeParameter);
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
  UsedIdentifiers: TFPHashList;
begin
  Result := TSymbolInformationItems.Create;
  UsedIdentifiers := TFPHashList.Create;
  for i := 0 to SymbolTable.Count - 1 do
    begin
      Entry := TSymbolTableEntry(SymbolTable[i]);
      if Entry <> nil then
        for Item in Entry.Items do
          if UsedIdentifiers.Find(TSymbolInformation(Item).name) = nil then
            begin
              Symbol := TSymbolInformation.Create(Result);
              Symbol.Assign(Item);
              UsedIdentifiers.Add(Symbol.name, Symbol);
            end;
    end;
  UsedIdentifiers.Free;
end;

function TSymbolManager.CollectSerializedSymbols(Duplicates: boolean = true): TJSONWrapper;
var
  i, j: integer;
  Symbol: TSymbol;
  Entry: TSymbolTableEntry;
  UsedIdentifiers: TFPHashList = nil;
  Count: integer;
begin
  if not Duplicates then
    UsedIdentifiers := TFPHashList.Create;

  Result := TJSONWrapper.Create;

  Result.Contents.Add('[');

  Count := 0;
  for i := 0 to SymbolTable.Count - 1 do
    begin
      Entry := TSymbolTableEntry(SymbolTable[i]);
      if Duplicates then
        begin
          if Entry.Count > 0 then
            begin
              Result.Contents.Add(Entry.RawJSON, 1, Length(Entry.RawJSON) - 2);
              Result.Contents.Add(',');
              Inc(Count, Entry.Count);
            end;
        end
      else
        begin
          for j := 0 to Entry.Count - 1 do
            begin
              Symbol := Entry[j];
              if Symbol.IsGlobal and (UsedIdentifiers.Find(Symbol.name) <> nil) then
                continue;
              Result.Contents.Add(Symbol.RawJSON);
              Result.Contents.Add(',');
              if Symbol.IsGlobal then
                UsedIdentifiers.Add(Symbol.Name, Symbol);
              Inc(Count);
            end;
        end;
    end;
  
  Result.Contents.Rewind;
  Result.Contents.Add(']');
  Result.ItemCount := Count;

  FreeAndNil(UsedIdentifiers);
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
    FindAllFiles(Files, Path, '*.pas;*.pp;*.p;*.inc', SearchSubDirs);
    for FileName in Files do
      Reload(FileName);
    writeln('loaded ', SymbolTable.Count, ' entries in /', ExtractFileName(Path));
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

function TSymbolManager.Find(Path: String): TSymbolItems;
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

function TSymbolManager.Reload(Code: TCodeBuffer): TSymbolItems;
var
  FileName: ShortString;
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
    exit(nil);


  if not CodeToolBoss.Explore(Code, Tool, false, false) then
    begin
      {$ifdef SYMBOL_EXTRACTOR_DEBUG}
      writeln(ExtractFileName(Code.FileName), ' -> ', CodeToolBoss.ErrorMessage+' @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
      {$endif}
      // todo: these errors are overwhelming on startup so we probably need a better way
      //AddError(ExtractFileName(Code.FileName)+' -> '+CodeToolBoss.ErrorMessage+' @ '+IntToStr(CodeToolBoss.ErrorLine)+':'+IntToStr(CodeToolBoss.ErrorColumn));
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

  Entry.Serialize;

  result := Entry.Items;
end;

function TSymbolManager.Reload(Path: String): TSymbolItems;
var
  Code: TCodeBuffer;
begin
  Code := Load(Path);
  if Code = nil then
    exit(nil);
  Result := Reload(Code);
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
    result := SymbolManager.Find(Path);
  end;
end;

initialization
  SymbolManager := TSymbolManager.Create;
  //TSymbolExtractorThread.Pending := TStringList.Create;
  //TSymbolExtractorThread.QueueLock := TCriticalSection.Create;

  LSPHandlerManager.RegisterHandler('textDocument/documentSymbol', TDocumentSymbolRequest);
end.
