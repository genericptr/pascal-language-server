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

unit PasLS.Symbols;

{$mode objfpc}{$H+}
{define SYMBOL_DEBUG}

interface
uses
  { RTL }
  Classes, Contnrs, fpjson, fpjsonrpc, SQLite3,
  { Code Tools }
  CodeToolManager, CodeCache, CodeTree, LinkScanner,
  { Protocols }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.DocumentSymbol,
  { Other }
  PasLS.CodeUtils, LSP.Streaming, LSP.Messages;

type

  { TSymbol }

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
    function AddSymbol(Name: String; Kind: TSymbolKind; FileName: String; Line, Column: Integer;EndLine,EndCol: Integer): TSymbol;
    function RequestReload: boolean;
    function Count: integer; inline;
    property RawJSON: String read GetRawJSON;
  end;

  { TSymbolBuilder }

  { Dual-mode symbol builder supporting both SymbolInformation (legacy)
    and DocumentSymbol (LSP 3.10+) output formats }

  TSymbolMode = (
    smFlat,        // Output SymbolInformation[] with Class.Method naming (Lazarus style)
    smHierarchical // Output DocumentSymbol[] with nested children (LSP 3.10+)
  );

type

  TSymbolBuilder = class
  private
    FMode: TSymbolMode;
    FEntry: TSymbolTableEntry;
    FTool: TCodeTool;

    // For hierarchical mode: map className -> TDocumentSymbolEx
    FClassMap: TFPHashObjectList;
    FRootSymbols: TDocumentSymbolExItems;

    // For tracking current hierarchy
    FCurrentClass: TDocumentSymbolEx;
    // Last added function/method (for nested function support)
    FLastAddedFunction: TDocumentSymbolEx;

    // For Interface/Implementation namespaces (hierarchical mode)
    FInterfaceSymbol: TDocumentSymbolEx;
    FImplementationSymbol: TDocumentSymbolEx;
    FCurrentSectionSymbol: TDocumentSymbolEx;

    function FindOrCreateClass(const AClassName: String; Node: TCodeTreeNode; IsImplementationContainer: Boolean = False): TDocumentSymbolEx;
    procedure SetNodeRange(Symbol: TDocumentSymbolEx; Node: TCodeTreeNode);
    function GetCurrentContainer: TDocumentSymbolExItems;
    function AddFlatSymbol(Node: TCodeTreeNode; const Name: String; Kind: TSymbolKind; const ContainerName: String = ''): TSymbol;
  public
    constructor Create(AEntry: TSymbolTableEntry; ATool: TCodeTool; AMode: TSymbolMode);
    destructor Destroy; override;

    // Section management (hierarchical mode)
    procedure BeginInterfaceSection(Node: TCodeTreeNode);
    procedure BeginImplementationSection(Node: TCodeTreeNode);

    // Add symbols based on mode
    function AddClass(Node: TCodeTreeNode; const Name: String): TSymbol;
    function AddMethod(Node: TCodeTreeNode; const AClassName, AMethodName: String): TSymbol;
    function AddGlobalFunction(Node: TCodeTreeNode; const Name: String): TSymbol;
    function AddStruct(Node: TCodeTreeNode; const Name: String): TSymbol;
    function AddProperty(Node: TCodeTreeNode; const AClassName, APropertyName: String): TSymbol;
    function AddField(Node: TCodeTreeNode; const AClassName, AFieldName: String): TSymbol;
    // Add nested function as child of parent
    function AddNestedFunction(Parent: TDocumentSymbolEx; Node: TCodeTreeNode; const Name, ParentPath: String): TDocumentSymbolEx;

    // Serialization
    procedure SerializeSymbols;

    property Mode: TSymbolMode read FMode;
    property CurrentClass: TDocumentSymbolEx read FCurrentClass write FCurrentClass;
    property RootSymbols: TDocumentSymbolExItems read FRootSymbols;
    property LastAddedFunction: TDocumentSymbolEx read FLastAddedFunction;
  end;

  { TSymbolExtractor }

  TSymbolExtractor = class
  private
    Code: TCodeBuffer;
    Tool: TCodeTool;
    Entry: TSymbolTableEntry;
    Builder: TSymbolBuilder;
    OverloadMap: TFPHashList;
    RelatedFiles: TFPHashList;
    IndentLevel: integer;
    CodeSection: TCodeTreeNodeDesc;
    function ShouldExcludeInterfaceDecl: Boolean;
    function ShouldExcludeImplClass: Boolean;
  private
    procedure PrintNodeDebug(Node: TCodeTreeNode; Deep: boolean = false);
    procedure AdjustEndPosition(Node: TCodeTreeNode; var EndPos: TCodeXYPosition);
    function AddSymbol(Node: TCodeTreeNode; Kind: TSymbolKind): TSymbol; overload;
    function AddSymbol(Node: TCodeTreeNode; Kind: TSymbolKind; Name: String; Container: String = ''): TSymbol; overload;
    procedure ExtractCodeSection(Node: TCodeTreeNode);
    function ExtractProcedure(ParentNode, Node: TCodeTreeNode):TSymbol;
    procedure ProcessNestedFunctions(Node: TCodeTreeNode; ParentSymbol: TDocumentSymbolEx; const ParentPath: String);
    procedure ExtractTypeDefinition(TypeDefNode, Node: TCodeTreeNode); 
    procedure ExtractSymbols(ClassNode, Node: TCodeTreeNode);
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
    FTransport: TMessageTransport;
  protected
    procedure DoLog(const Msg : String); overload;
    procedure DoLog(const Fmt : String; const Args : Array of const); overload;
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
    Property Transport : TMessageTransport Read FTransport Write FTransport;
  end;

  { TSymbolManager }

  TSymbolManager = class
  private
    fTransport: TMessageTransport;
    SymbolTable: TFPHashObjectList;
    ErrorList: TStringList;
    fDatabase: TSymbolDatabase;

    function Load(Path: String): TCodeBuffer;
    procedure AddError(Message: String);
    function GetEntry(Code: TCodeBuffer): TSymbolTableEntry;
    function GetDatabase: TSymbolDatabase;
    procedure setTransport(AValue: TMessageTransport);
    property Database: TSymbolDatabase read GetDatabase;
  Protected
    Procedure DoLog(const Msg : String); overload;
    Procedure DoLog(const Fmt : String; const Args : Array of const); overload;
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

    { File Management }
    procedure RemoveFile(FileName: String);

    Property Transport : TMessageTransport Read fTransport Write setTransport;
  end;

var
  SymbolManager: TSymbolManager = nil;

// Client capabilities and configuration storage
var
  ClientSupportsDocumentSymbol: Boolean = False;

function GetSymbolMode: TSymbolMode;
procedure SetClientCapabilities(SupportsDocumentSymbol: Boolean);

{ Adjusts EndPos for LSP Range specification (end position must be exclusive).
  For section nodes, moves back one line to not include next section.
  For other nodes, moves forward one character to make end exclusive. }
procedure AdjustEndPositionForLSP(Node: TCodeTreeNode; var EndPos: TCodeXYPosition);

implementation
uses
  { RTL }
  SysUtils, FileUtil, DateUtils, fpjsonrtti,
  { Code Tools }
  CodeAtom,
  FindDeclarationTool, KeywordFuncLists,PascalParserTool,
  { Protocol }
  PasLS.Settings, PasLS.ClientProfile;

function GetSymbolMode: TSymbolMode;
begin
  // Priority 1: Client profile forces flat mode
  if TClientProfile.Current.HasFeature(cfFlatSymbolMode) then
    Exit(smFlat);

  // Priority 2: Auto mode - use hierarchical if client supports it and server enables it
  if ClientSupportsDocumentSymbol and ServerSettings.documentSymbols then
    Result := smHierarchical
  else
    Result := smFlat;
end;

procedure SetClientCapabilities(SupportsDocumentSymbol: Boolean);
begin
  ClientSupportsDocumentSymbol := SupportsDocumentSymbol;
end;

procedure AdjustEndPositionForLSP(Node: TCodeTreeNode; var EndPos: TCodeXYPosition);
var
  LineText: String;
begin
  if Node.Desc in AllCodeSections then
    begin
      // For section nodes (interface/implementation), CodeTools EndPos points to
      // the start of the NEXT section (or end of file). We need to move back
      // one line so we don't include the next section's first line.
      if EndPos.Y > 1 then
        begin
          Dec(EndPos.Y);
          // Set X to end of the previous line (past last char for exclusive end)
          if (EndPos.Code <> nil) and (EndPos.Y <= EndPos.Code.LineCount) then
            EndPos.X := Length(EndPos.Code.GetLine(EndPos.Y - 1, false)) + 1
          else
            EndPos.X := 1;
        end;
    end
  else
    begin
      // For non-section nodes, move EndPos one position forward to make it exclusive
      if (EndPos.Code <> nil) and (EndPos.Y > 0) and (EndPos.Y <= EndPos.Code.LineCount) then
        begin
          LineText := EndPos.Code.GetLine(EndPos.Y - 1, false);
          if EndPos.X <= Length(LineText) then
            Inc(EndPos.X)
          else
            // EndPos.X already points past the last character on this line.
            // Keep it on the same line - don't move to next line.
            // This prevents single-line symbols from having range extend to next line.
            EndPos.X := Length(LineText) + 1;
        end;
    end;
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

begin
  Result:=Location.LocalPath;
end;

function TSymbol.IsGlobal: boolean;
begin
  result := Assigned(containerName) and (containerName.Value <> '');
end;

function TSymbol.GetFullName: String;
begin
  if Assigned(containerName) and (containerName.Value <> '') then
    Result := containerName.Value+'.'+Name
  else
    Result := Name;
end;

constructor TSymbol.Create;
begin
  // we need this dummy constructor for serializing
  Create(nil);
end;

{ TSymbolBuilder }

constructor TSymbolBuilder.Create(AEntry: TSymbolTableEntry; ATool: TCodeTool; AMode: TSymbolMode);
begin
  FEntry := AEntry;
  FTool := ATool;
  FMode := AMode;
  FCurrentClass := nil;

  if FMode = smHierarchical then
    begin
      FClassMap := TFPHashObjectList.Create(False); // Don't own objects - they're owned by FRootSymbols
      FRootSymbols := TDocumentSymbolExItems.Create;
    end;
end;

destructor TSymbolBuilder.Destroy;
begin
  if FMode = smHierarchical then
    begin
      FreeAndNil(FClassMap);
      FreeAndNil(FRootSymbols);
    end;
  inherited;
end;

procedure TSymbolBuilder.SetNodeRange(Symbol: TDocumentSymbolEx; Node: TCodeTreeNode);
var
  StartPos, EndPos, NamePos: TCodeXYPosition;
begin
  if (FTool = nil) or (Symbol = nil) or (Node = nil) then
    Exit;

  FTool.CleanPosToCaret(Node.StartPos, StartPos);
  FTool.CleanPosToCaret(Node.EndPos, EndPos);

  // Use shared adjustment logic for LSP Range specification
  AdjustEndPositionForLSP(Node, EndPos);

  Symbol.range.SetRange(StartPos.Y - 1, StartPos.X - 1, EndPos.Y - 1, EndPos.X - 1);

  // For procedure/function nodes, find the actual name position
  // Node.StartPos points to keyword ("procedure"/"function"), but we need
  // the name position for selectionRange to highlight correctly in editors
  if Node.Desc = ctnProcedure then
    begin
      FTool.MoveCursorToProcName(Node, True); // True = skip className prefix
      FTool.CleanPosToCaret(FTool.CurPos.StartPos, NamePos);
      Symbol.selectionRange.SetRange(NamePos.Y - 1, NamePos.X - 1, NamePos.Y - 1, NamePos.X - 1 + Length(Symbol.name));
    end
  else
    Symbol.selectionRange.SetRange(StartPos.Y - 1, StartPos.X - 1, StartPos.Y - 1, StartPos.X - 1 + Length(Symbol.name));
end;

function TSymbolBuilder.GetCurrentContainer: TDocumentSymbolExItems;
begin
  // In hierarchical mode, return the current section's children if available
  if (FMode = smHierarchical) and (FCurrentSectionSymbol <> nil) then
    Result := TDocumentSymbolExItems(FCurrentSectionSymbol.children)
  else
    Result := FRootSymbols;
end;

function TSymbolBuilder.AddFlatSymbol(Node: TCodeTreeNode; const Name: String; Kind: TSymbolKind; const ContainerName: String = ''): TSymbol;
var
  CodePos, EndPos: TCodeXYPosition;
begin
  Result := nil;
  if (FTool <> nil) and (Node <> nil) then
    begin
      FTool.CleanPosToCaret(Node.StartPos, CodePos);
      FTool.CleanPosToCaret(Node.EndPos, EndPos);
      // Adjust EndPos for LSP Range specification (end position must be exclusive)
      AdjustEndPositionForLSP(Node, EndPos);
      Result := FEntry.AddSymbol(Name, Kind,
                                 CodePos.Code.FileName,
                                 CodePos.Y, CodePos.X,
                                 EndPos.Y, EndPos.X);
      // Set containerName for LSP semantics in workspace/symbol
      if (Result <> nil) and (ContainerName <> '') then
        Result.containerName := TOptionalString.Create(ContainerName);
    end;
end;

procedure TSymbolBuilder.BeginInterfaceSection(Node: TCodeTreeNode);
begin
  if FMode <> smHierarchical then
    Exit;

  // Create interface namespace symbol
  FInterfaceSymbol := TDocumentSymbolEx.Create(FRootSymbols);
  FInterfaceSymbol.name := kSymbolName_Interface;
  FInterfaceSymbol.kind := TSymbolKind._Namespace;
  SetNodeRange(FInterfaceSymbol, Node);
  FCurrentSectionSymbol := FInterfaceSymbol;
end;

procedure TSymbolBuilder.BeginImplementationSection(Node: TCodeTreeNode);
begin
  if FMode <> smHierarchical then
    Exit;

  // Create implementation namespace symbol
  FImplementationSymbol := TDocumentSymbolEx.Create(FRootSymbols);
  FImplementationSymbol.name := kSymbolName_Implementation;
  FImplementationSymbol.kind := TSymbolKind._Namespace;
  SetNodeRange(FImplementationSymbol, Node);
  FCurrentSectionSymbol := FImplementationSymbol;
end;

function TSymbolBuilder.FindOrCreateClass(const AClassName: String; Node: TCodeTreeNode; IsImplementationContainer: Boolean = False): TDocumentSymbolEx;
var
  Container: TDocumentSymbolExItems;
  Key: String;
begin
  Result := nil;

  if FMode <> smHierarchical then
    Exit;

  // F1 Scheme: Classes exist in both Interface and Implementation namespaces
  // Use section-specific key to distinguish between interface declaration and implementation methods
  // Note: Must check for nil first, otherwise nil = nil is True for program files
  if (FInterfaceSymbol <> nil) and (FCurrentSectionSymbol = FInterfaceSymbol) then
    Key := 'interface.' + AClassName
  else if (FImplementationSymbol <> nil) and (FCurrentSectionSymbol = FImplementationSymbol) then
    Key := 'implementation.' + AClassName
  else
    begin
      // Program files: distinguish between declaration and implementation container
      if IsImplementationContainer then
        Key := AClassName + '.impl'
      else
        Key := AClassName;
    end;

  // Check if class already exists in current section
  Result := TDocumentSymbolEx(FClassMap.Find(Key));

  if Result = nil then
    begin
      // Create class in current section's namespace
      Container := GetCurrentContainer;

      Result := TDocumentSymbolEx.Create(Container);
      Result.name := AClassName;
      Result.kind := TSymbolKind._Class;

      // Set ranges using the node
      if Node <> nil then
        SetNodeRange(Result, Node);

      // Add reference to class map for lookup with section-specific key
      FClassMap.Add(Key, Result);
    end;
end;

function TSymbolBuilder.AddClass(Node: TCodeTreeNode; const Name: String): TSymbol;
begin
  case FMode of
    smFlat:
      begin
        // Flat mode: add class to Entry.Symbols
        Result := AddFlatSymbol(Node, Name, TSymbolKind._Class);
      end;

    smHierarchical:
      begin
        // Hierarchical mode: Create class in current section's namespace
        // - Interface section: class declaration
        // - Implementation section: class with method implementations (rare)
        FCurrentClass := FindOrCreateClass(Name, Node);
        // Also add to flat symbol list for database/workspace symbol
        Result := AddFlatSymbol(Node, Name, TSymbolKind._Class);
      end;
  end;
end;

function TSymbolBuilder.AddMethod(Node: TCodeTreeNode; const AClassName, AMethodName: String): TSymbol;
var
  ClassSymbol: TDocumentSymbolEx;
  MethodSymbol: TDocumentSymbolEx;
begin
  case FMode of
    smFlat:
      begin
        // Flat mode: Class.Method naming, no containerName (Lazarus style)
        Result := AddFlatSymbol(Node, AClassName + '.' + AMethodName, TSymbolKind._Method);
      end;

    smHierarchical:
      begin
        // Hierarchical mode: Add method as child of class
        ClassSymbol := FindOrCreateClass(AClassName, nil, True);
        if ClassSymbol <> nil then
          begin
            MethodSymbol := TDocumentSymbolEx.Create(ClassSymbol.children);
            MethodSymbol.name := AMethodName;
            MethodSymbol.kind := TSymbolKind._Method;
            SetNodeRange(MethodSymbol, Node);
            FLastAddedFunction := MethodSymbol;

            // Initialize or extend class range to include method
            if (ClassSymbol.range.start.line = 0) and (ClassSymbol.range.&end.line = 0) then
              begin
                // First method - initialize class range
                ClassSymbol.range.start.line := MethodSymbol.range.start.line;
                ClassSymbol.range.start.character := MethodSymbol.range.start.character;
                ClassSymbol.range.&end.line := MethodSymbol.range.&end.line;
                ClassSymbol.range.&end.character := MethodSymbol.range.&end.character;
                ClassSymbol.selectionRange := ClassSymbol.range;
              end
            else
              begin
                // Extend class range to include this method
                if MethodSymbol.range.start.line < ClassSymbol.range.start.line then
                  begin
                    ClassSymbol.range.start.line := MethodSymbol.range.start.line;
                    ClassSymbol.range.start.character := MethodSymbol.range.start.character;
                  end;
                if MethodSymbol.range.&end.line > ClassSymbol.range.&end.line then
                  begin
                    ClassSymbol.range.&end.line := MethodSymbol.range.&end.line;
                    ClassSymbol.range.&end.character := MethodSymbol.range.&end.character;
                  end;
              end;
          end;

        // Add to flat symbol list for database/workspace symbol with LSP semantics
        // name=MethodName, containerName=ClassName
        Result := AddFlatSymbol(Node, AMethodName, TSymbolKind._Method, AClassName);
      end;
  end;
end;

function TSymbolBuilder.AddGlobalFunction(Node: TCodeTreeNode; const Name: String): TSymbol;
var
  GlobalSymbol: TDocumentSymbolEx;
begin
  case FMode of
    smFlat:
      begin
        // Flat mode: add function to Entry.Symbols
        Result := AddFlatSymbol(Node, Name, TSymbolKind._Function);
      end;

    smHierarchical:
      begin
        // Hierarchical mode: Add to current container (Interface or Implementation namespace)
        GlobalSymbol := TDocumentSymbolEx.Create(GetCurrentContainer);
        GlobalSymbol.name := Name;
        GlobalSymbol.kind := TSymbolKind._Function;
        SetNodeRange(GlobalSymbol, Node);
        FLastAddedFunction := GlobalSymbol;
        // Also add to flat symbol list for database/workspace symbol
        Result := AddFlatSymbol(Node, Name, TSymbolKind._Function);
      end;
  end;
end;

function TSymbolBuilder.AddStruct(Node: TCodeTreeNode; const Name: String): TSymbol;
var
  StructSymbol: TDocumentSymbolEx;
begin
  case FMode of
    smFlat:
      begin
        // Flat mode: add struct to Entry.Symbols
        Result := AddFlatSymbol(Node, Name, TSymbolKind._Struct);
      end;

    smHierarchical:
      begin
        // Hierarchical mode: Add struct to current container
        StructSymbol := TDocumentSymbolEx.Create(GetCurrentContainer);
        StructSymbol.name := Name;
        StructSymbol.kind := TSymbolKind._Struct;
        SetNodeRange(StructSymbol, Node);
        // Also add to flat symbol list for database/workspace symbol
        Result := AddFlatSymbol(Node, Name, TSymbolKind._Struct);
      end;
  end;
end;

function TSymbolBuilder.AddProperty(Node: TCodeTreeNode; const AClassName, APropertyName: String): TSymbol;
var
  ClassSymbol: TDocumentSymbolEx;
  PropertySymbol: TDocumentSymbolEx;
begin
  case FMode of
    smFlat:
      begin
        // Flat mode: Class.Property naming, no containerName (Lazarus style)
        Result := AddFlatSymbol(Node, AClassName + '.' + APropertyName, TSymbolKind._Property);
      end;

    smHierarchical:
      begin
        // Hierarchical mode: add property to class's children
        ClassSymbol := FindOrCreateClass(AClassName, Node);
        if ClassSymbol <> nil then
          begin
            PropertySymbol := TDocumentSymbolEx.Create(ClassSymbol.children);
            PropertySymbol.name := APropertyName;
            PropertySymbol.kind := TSymbolKind._Property;
            SetNodeRange(PropertySymbol, Node);
          end;
        // Add to flat symbol list with LSP semantics
        Result := AddFlatSymbol(Node, APropertyName, TSymbolKind._Property, AClassName);
      end;
  end;
end;

function TSymbolBuilder.AddField(Node: TCodeTreeNode; const AClassName, AFieldName: String): TSymbol;
var
  ClassSymbol: TDocumentSymbolEx;
  FieldSymbol: TDocumentSymbolEx;
begin
  case FMode of
    smFlat:
      begin
        // Flat mode: Class.Field naming, no containerName (Lazarus style)
        Result := AddFlatSymbol(Node, AClassName + '.' + AFieldName, TSymbolKind._Field);
      end;

    smHierarchical:
      begin
        // Hierarchical mode: add field to class's children
        ClassSymbol := FindOrCreateClass(AClassName, Node);
        if ClassSymbol <> nil then
          begin
            FieldSymbol := TDocumentSymbolEx.Create(ClassSymbol.children);
            FieldSymbol.name := AFieldName;
            FieldSymbol.kind := TSymbolKind._Field;
            SetNodeRange(FieldSymbol, Node);
          end;
        // Add to flat symbol list with LSP semantics
        Result := AddFlatSymbol(Node, AFieldName, TSymbolKind._Field, AClassName);
      end;
  end;
end;

function TSymbolBuilder.AddNestedFunction(Parent: TDocumentSymbolEx; Node: TCodeTreeNode; const Name, ParentPath: String): TDocumentSymbolEx;
var
  FullPath: String;
begin
  Result := nil;
  FullPath := ParentPath + '.' + Name;

  case FMode of
    smFlat:
      begin
        // Flat mode: full path name, no containerName (Lazarus style)
        AddFlatSymbol(Node, FullPath, TSymbolKind._Function);
      end;

    smHierarchical:
      begin
        if Parent = nil then
          Exit;

        // Create nested function as child of parent
        Result := TDocumentSymbolEx.Create(Parent.children);
        Result.name := Name;
        Result.kind := TSymbolKind._Function;
        SetNodeRange(Result, Node);

        // Add to flat symbol list with LSP semantics
        // name=NestedName, containerName=ParentPath
        AddFlatSymbol(Node, Name, TSymbolKind._Function, ParentPath);
      end;
  end;
end;

procedure TSymbolBuilder.SerializeSymbols;
const
  BATCH_COUNT = 1000;
var
  SerializedItems: TJSONArray;
  i, Start, Next, Total: Integer;
  Symbol: TSymbol;
begin
  case FMode of
    smFlat:
      begin
        // Use existing serialization for flat SymbolInformation[]
        FEntry.SerializeSymbols;
      end;

    smHierarchical:
      begin
        // Serialize DocumentSymbol hierarchy for textDocument/documentSymbol
        SerializedItems := specialize TLSPStreaming<TDocumentSymbolExItems>.ToJSON(FRootSymbols) as TJSONArray;
        try
          FEntry.fRawJSON := SerializedItems.AsJSON;
        finally
          SerializedItems.Free;
        end;

        // Serialize flat SymbolInformation[] for database insertion and workspace/symbol
        SerializedItems := specialize TLSPStreaming<TSymbolItems>.ToJSON(FEntry.Symbols) as TJSONArray;
        try
          // Set RawJSON for each symbol (needed for database insertion)
          for i := 0 to SerializedItems.Count - 1 do
            begin
              Symbol := FEntry.Symbols.Items[i];
              Symbol.RawJSON := SerializedItems[i].AsJson;
            end;

          // Insert symbols into database if available
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
                  SymbolManager.Database.InsertSymbols(FEntry.Symbols, Start, Next);
                  Start := Next + 1;
                end;
            end;
        finally
          SerializedItems.Free;
        end;
      end;
  end;
end;

{ TSymbolTableEntry }

function TSymbolTableEntry.GetRawJSON: String;
var
  JSON: TJSONSerializedArray;
begin
  if (fRawJSON = '') and (SymbolManager.Database <> nil) then
    begin
      JSON := SymbolManager.Database.FindAllSymbols(Code.FileName);
      try
        fRawJSON := JSON.AsJson;
      finally
        JSON.Free;
      end;
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

function TSymbolTableEntry.AddSymbol(Name: String; Kind: TSymbolKind; FileName: String; Line, Column: Integer;EndLine,EndCol: Integer): TSymbol;

var
  Symbol: TSymbol;

begin
  Symbol := Symbols.Add;
  Symbol.name := Name;
  Symbol.kind := Kind;
  Symbol.location.URI:=PathToURI(FileName);
  Symbol.location.Range.SetRange(Line-1,Column-1,EndLine-1 ,EndCol-1);
  { TODO: In the latest version of LSP container name is supported
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
  Try
    for i := 0 to SerializedItems.Count - 1 do
      begin
        Symbol := Symbols.Items[i];
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
  Finally
    SerializedItems.Free;
    Symbols.Clear;
  end;
end;

procedure TSymbolTableEntry.Clear;
begin
  Modified := false;
  Symbols.Clear;
  if (SymbolManager.Database <> nil) and (Code <> nil) then
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
{var
  Child: TCodeTreeNode;}
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

procedure TSymbolExtractor.AdjustEndPosition(Node: TCodeTreeNode; var EndPos: TCodeXYPosition);
begin
  AdjustEndPositionForLSP(Node, EndPos);
end;

function TSymbolExtractor.ShouldExcludeInterfaceDecl: Boolean;
begin
  Result := (Builder.Mode = smFlat) and
            (CodeSection = ctnInterface) and
            TClientProfile.Current.HasFeature(cfExcludeInterfaceMethodDecls);
end;

function TSymbolExtractor.ShouldExcludeImplClass: Boolean;
begin
  Result := (Builder.Mode = smFlat) and
            (CodeSection = ctnImplementation) and
            TClientProfile.Current.HasFeature(cfExcludeImplClassDefs);
end;

function TSymbolExtractor.AddSymbol(Node: TCodeTreeNode; Kind: TSymbolKind; Name: String; Container: String): TSymbol;
var
  CodePos, EndPos: TCodeXYPosition;
  FileName: String;
begin
  {$ifdef SYMBOL_DEBUG}
  writeln(IndentLevelString(IndentLevel + 1), '* ', Name);
  {$endif}

  Tool.CleanPosToCaret(Node.StartPos, CodePos);
  Tool.CleanPosToCaret(Node.EndPos, EndPos);

  // Adjust EndPos for LSP Range specification (end position must be exclusive)
  AdjustEndPosition(Node, EndPos);

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

  Result := Entry.AddSymbol(Name, Kind, CodePos.Code.FileName, CodePos.Y, CodePos.X, EndPos.Y,EndPos.X);
end;

procedure TSymbolExtractor.ExtractSymbols(ClassNode, Node: TCodeTreeNode);
var
  Child: TCodeTreeNode;
  ExternalClass: boolean = false;
  TypeName, PropertyName, FieldName: String;
  i: Integer;
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
          if not ShouldExcludeInterfaceDecl then
          begin
            // Use Builder.AddMethod for consistent handling in both modes
            TypeName := GetIdentifierAtPos(Tool, ClassNode.StartPos, true, true);
            Builder.AddMethod(Node, TypeName, Tool.ExtractProcName(Node, []));
          end;
        ctnProperty:
          if ServerSettings.includeFieldsInSymbols then
            begin
              // For property, skip the "property" keyword to get the actual property name
              Tool.MoveCursorToCleanPos(Node.StartPos);
              Tool.ReadNextAtom; // Skip "property" keyword
              Tool.ReadNextAtom; // Move to property name
              TypeName := GetIdentifierAtPos(Tool, ClassNode.StartPos, true, true);
              // Extract property name from current atom
              PropertyName := Copy(Tool.Scanner.CleanedSrc, Tool.CurPos.StartPos,
                                   Tool.CurPos.EndPos - Tool.CurPos.StartPos);
              Builder.AddProperty(Node, TypeName, PropertyName);
            end;
        ctnVarDefinition:
          if ServerSettings.includeFieldsInSymbols then
            begin
              // Extract field (class member variable)
              TypeName := GetIdentifierAtPos(Tool, ClassNode.StartPos, true, true);
              // For field, extract identifier without the colon
              FieldName := GetIdentifierAtPos(Tool, Node.StartPos, true, true);
              // Remove trailing colon if present
              i := Pos(':', FieldName);
              if i > 0 then
                FieldName := Copy(FieldName, 1, i - 1);
              Builder.AddField(Node, TypeName, FieldName);
            end;
        ctnClassPublic,ctnClassPublished,ctnClassPrivate,ctnClassProtected,
        ctnClassRequired,ctnClassOptional:
          if ExternalClass then
            begin
              TypeName := GetIdentifierAtPos(Tool, ClassNode.StartPos, true, true);
              Child := Node.FirstChild;
              while Child <> nil do
                begin
                  PrintNodeDebug(Child);
                  if not ShouldExcludeInterfaceDecl then
                    AddSymbol(Node, TSymbolKind._Method, TypeName+'.'+Tool.ExtractProcName(Child, []));
                  Child := Child.NextBrother;
                end;
            end
          else
            begin
              // For regular Pascal classes, recurse into visibility sections
              Inc(IndentLevel);
              ExtractSymbols(ClassNode, Node.FirstChild);
              Dec(IndentLevel);
            end;
      end;

      Node := Node.NextBrother;
    end;
end;

// Helper to clean type name - removes trailing operators like '=' from 'TMyClass='
function CleanTypeName(const AName: String): String;
var
  Len: Integer;
const
  OpChars = ['+', '*', '-', '/', '<', '>', '=', ':'];
begin
  Result := AName;
  Len := Length(Result);
  while (Len > 0) and (Result[Len] in OpChars) do
    Dec(Len);
  SetLength(Result, Len);
end;

procedure TSymbolExtractor.ExtractTypeDefinition(TypeDefNode, Node: TCodeTreeNode);
var
  Child: TCodeTreeNode;
  TypeName: String;
begin
  while Node <> nil do
    begin
      PrintNodeDebug(Node);

      case Node.Desc of
        ctnClass,ctnClassHelper,ctnRecordHelper,ctnTypeHelper:
          begin
            // Skip forward declarations (e.g., "TMyClass = class;")
            // Use ctnsForwardDeclaration flag, not FirstChild check
            // (empty class "TMyClass = class end;" has no children but is NOT forward)
            if (Node.SubDesc and ctnsForwardDeclaration) > 0 then
              begin
                Node := Node.NextBrother;
                continue;
              end;
            // Skip implementation class definitions when filter is enabled (smFlat mode only)
            if ShouldExcludeImplClass then
              begin
                Node := Node.NextBrother;
                continue;
              end;
            TypeName := CleanTypeName(GetIdentifierAtPos(Tool, TypeDefNode.StartPos, true, true));
            Builder.AddClass(TypeDefNode, TypeName);
            Inc(IndentLevel);
            ExtractSymbols(TypeDefNode, Node.FirstChild);
            Dec(IndentLevel);
          end;
        ctnObject,ctnRecordType:
          begin
            TypeName := CleanTypeName(GetIdentifierAtPos(Tool, TypeDefNode.StartPos, true, true));
            Builder.AddStruct(TypeDefNode, TypeName);
          end;
        ctnObjCClass,ctnObjCCategory,ctnObjCProtocol:
          begin
            // todo: ignore forward defs!
            TypeName := CleanTypeName(GetIdentifierAtPos(Tool, TypeDefNode.StartPos, true, true));
            Builder.AddClass(TypeDefNode, TypeName);
            Inc(IndentLevel);
            ExtractSymbols(TypeDefNode, Node.FirstChild);
            Dec(IndentLevel);
          end;
        ctnSpecialize:
          begin
            // todo: is this a class/record???
            PrintNodeDebug(Node.FirstChild, true);
            TypeName := CleanTypeName(GetIdentifierAtPos(Tool, TypeDefNode.StartPos, true, true));
            Builder.AddClass(TypeDefNode, TypeName);
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

function TSymbolExtractor.ExtractProcedure(ParentNode, Node: TCodeTreeNode):TSymbol;
var
  Child: TCodeTreeNode;
  Name,ContainerName,Key: ShortString;
  Symbol: TSymbol;
begin
  result := nil;
  PrintNodeDebug(Node);

  containerName:=Tool.ExtractClassNameOfProcNode(Node);
  Name := Tool.ExtractProcName(Node, [phpWithoutClassName]);

  key:=IntToStr(CodeSection)+'.'+containerName+'.'+Name;
  Symbol := TSymbol(OverloadMap.Find(key));

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
        otherwise
          ;
      end;
    end;

  // Create symbol for overload tracking metadata only
  // Builder will handle actual addition to Entry.Symbols or FRootSymbols
  Symbol := TSymbol.Create(nil);
  Symbol.name := Name;
  Symbol.kind := TSymbolKind._Function;
  if containerName <> '' then
    Symbol.containerName := TOptionalString.Create(containerName);

  OverloadMap.Add(Key, Symbol);

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
  result := Symbol;
end;

procedure TSymbolExtractor.ProcessNestedFunctions(Node: TCodeTreeNode; ParentSymbol: TDocumentSymbolEx; const ParentPath: String);
var
  Child: TCodeTreeNode;
  NestedSymbol: TDocumentSymbolEx;
  Name, NestedPath: String;
begin
  // In hierarchical mode, we need a parent symbol for hierarchy
  if (Builder.Mode = smHierarchical) and (ParentSymbol = nil) then
    Exit;

  // Skip forward/external declarations
  if Tool.ProcNodeHasSpecifier(Node, psForward) or
     Tool.ProcNodeHasSpecifier(Node, psExternal) then
    Exit;

  // Find nested procedures in the node's children
  Child := Node.FirstChild;
  while Child <> nil do
    begin
      if Child.Desc = ctnProcedure then
        begin
          Name := Tool.ExtractProcName(Child, [phpWithoutClassName]);
          NestedSymbol := Builder.AddNestedFunction(ParentSymbol, Child, Name, ParentPath);
          // Recursively process nested functions within this nested function
          NestedPath := ParentPath + '.' + Name;
          ProcessNestedFunctions(Child, NestedSymbol, NestedPath);
        end;
      Child := Child.NextBrother;
    end;
end;

procedure TSymbolExtractor.ExtractCodeSection(Node: TCodeTreeNode);
var
  Symbol, MethodSymbol, LastClassSymbol: TSymbol;
  Child: TCodeTreeNode;
  Scanner: TLinkScanner;
  LinkIndex: Integer;
  IsImplementation:Boolean;
begin
  IsImplementation:=(Node.Parent<>nil) and  (Node.Parent.Desc=ctnImplementation);
  LastClassSymbol:=nil;

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
              begin
                // For hierarchical mode, create Interface namespace
                Builder.BeginInterfaceSection(Node);
                // For flat mode, add namespace symbol (unless filtered)
                if Builder.Mode = smFlat then
                  if not TClientProfile.Current.HasFeature(cfExcludeSectionContainers) then
                    AddSymbol(Node, TSymbolKind._Namespace, kSymbolName_Interface);
              end;
            ctnImplementation:
              begin
                // For hierarchical mode, create Implementation namespace
                Builder.BeginImplementationSection(Node);
                // For flat mode, add namespace symbol (unless filtered)
                if Builder.Mode = smFlat then
                  if not TClientProfile.Current.HasFeature(cfExcludeSectionContainers) then
                    AddSymbol(Node, TSymbolKind._Namespace, kSymbolName_Implementation);
              end;
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
           begin

            Symbol:= ExtractProcedure(nil, Node);

            if (Symbol<>nil) then
              begin
                // Use Builder to add methods or global functions based on containerName
                if Assigned(Symbol.containerName) then
                  begin
                    // This is a class method
                    // Capture result to get the actual symbol with proper location
                    MethodSymbol := Builder.AddMethod(Node, Symbol.containerName.Value, Symbol.name);
                    // Process nested functions - parent path is ClassName.MethodName
                    ProcessNestedFunctions(Node, Builder.LastAddedFunction,
                      Symbol.containerName.Value + '.' + Symbol.name);

                    // In flat mode, we also need to track class symbols for range updates
                    if (Builder.Mode = smFlat) and not ShouldExcludeImplClass then
                      begin
                        if (LastClassSymbol=nil) or (Symbol.containerName.Value<>LastClassSymbol.name) then
                          LastClassSymbol:=AddSymbol(Node,TSymbolKind._Class,Symbol.containerName.Value)
                        else if MethodSymbol <> nil then
                          LastClassSymbol.location.range.&end:=MethodSymbol.location.range.&end;
                      end;
                  end
                else
                  begin
                    // This is a global function
                    // F1 Scheme: Add to current section's namespace
                    // - Interface section: function declaration
                    // - Implementation section: function implementation
                    if not ShouldExcludeInterfaceDecl then
                    begin
                      Builder.AddGlobalFunction(Node, Symbol.name);
                      // Process nested functions - parent path is function name
                      ProcessNestedFunctions(Node, Builder.LastAddedFunction, Symbol.name);
                    end;
                  end;
              end;

          end;
      end;

      Node := Node.NextBrother;
    end;
end;

constructor TSymbolExtractor.Create(_Entry: TSymbolTableEntry; _Code: TCodeBuffer; _Tool: TCodeTool);
begin
  Entry := _Entry;
  Code := _Code;
  Tool := _Tool;
  Builder := TSymbolBuilder.Create(Entry, Tool, GetSymbolMode);
  OverloadMap := TFPHashList.Create;
  RelatedFiles := TFPHashList.Create;
end;

destructor TSymbolExtractor.Destroy;
begin
  Builder.SerializeSymbols;
  Builder.Free;
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

Type
  TSymbolEntryType = (
    SYMBOL_ENTRY_NAME, // 0
    SYMBOL_ENTRY_PATH, // 1
    SYMBOL_ENTRY_JSON  // 2
  );

function TSymbolDatabase.FindAllSymbols(Path: String): TJSONSerializedArray;
var
  Stat: String;
  statement: psqlite3_stmt;
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
          Contents.Add(sqlite3_column_text(statement, Ord(SYMBOL_ENTRY_JSON)));
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
          Contents.Add(sqlite3_column_text(statement, Ord(SYMBOL_ENTRY_JSON)));
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

procedure TSymbolDatabase.DoLog(const Msg: String);
begin
  if Assigned(Transport) then
    Transport.SendDiagnostic(Msg);
end;

procedure TSymbolDatabase.DoLog(const Fmt: String; const Args: array of const);
begin
  if Assigned(Transport) then
    Transport.SendDiagnostic(Fmt,Args);
end;

procedure TSymbolDatabase.LogError(errmsg: pansichar);

var
  S : String;

begin
  // sql errors are fatal right now
  S:=errmsg;
  DoLog(S);
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
  DoLog('Loading symbol database at %s',[Path]);
  Path += #0;
  if sqlite3_open(@Path[1], @Database) <> SQLITE_OK then
    begin
      DoLog('Failed to load database %s',[Path]);
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
    begin
    fDatabase := TSymbolDatabase.Create(ExpandFileName(ServerSettings.symbolDatabase));
    fDatabase.Transport:=fTransport;
    end;
  Result := fDatabase;
end;

procedure TSymbolManager.setTransport(AValue: TMessageTransport);
begin
  if fTransport=AValue then Exit;
  fTransport:=AValue;
  if assigned(fDatabase) then
    fDatabase.Transport:=fTransport;
end;

procedure TSymbolManager.DoLog(const Msg: String);
begin
  if Assigned(Transport) then
    Transport.SendDiagnostic(Msg);
end;

procedure TSymbolManager.DoLog(const Fmt: String; const Args: array of const);
begin
  if Assigned(Transport) then
    Transport.SendDiagnostic(Fmt,Args);
end;

procedure TSymbolManager.RemoveFile(FileName: String);
var
  Index: integer;
  Entry: TSymbolTableEntry;
begin
  Entry := TSymbolTableEntry(SymbolTable.Find(FileName));
  if Entry <> nil then
  begin
    // Clear symbols from database if enabled
    if (Database <> nil) and (Entry.Code <> nil) then
      Database.ClearSymbols(Entry.Code.FileName);

    // Remove entry from in-memory symbol table
    Index := SymbolTable.FindIndexOf(FileName);
    if Index <> -1 then
      SymbolTable.Delete(Index);
  end;
end;

function TSymbolManager.FindWorkspaceSymbols(Query: String): TJSONSerializedArray;
begin
  if Database <> nil then
    result := Database.FindSymbols(Query)
  else
    result := CollectSerializedSymbols;
end;

function TSymbolManager.CollectSerializedSymbols: TJSONSerializedArray;
var
  i : integer;
  Entry: TSymbolTableEntry;
  Contents: TLongString;
begin
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
  DoLog(Message);
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

  if (Entry <> nil) and (Entry.RawJSON <> '') then
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
    end
  else if Entry.Code <> Code then
    begin
      // Update Entry.Code to point to the new buffer
      // This handles the case when a file is moved/renamed:
      // the filename (key) is the same but the Code buffer is different
      Entry.Code := Code;
    end;
  result := Entry;
end;

procedure TSymbolManager.Reload(Code: TCodeBuffer; Always: Boolean = false);
var
  Entry: TSymbolTableEntry;
  Tool: TCodeTool = nil;
  Extractor: TSymbolExtractor;
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
      With CodeToolBoss do
        DoLog('%s -> %s @ %d:%d', [ExtractFileName(Code.FileName), ErrorMessage,ErrorLine,CodeToolBoss.ErrorColumn]);
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
  try
    Extractor.ExtractCodeSection(Tool.Tree.Root);
  finally
    Extractor.Free;  // This calls Builder.SerializeSymbols in the destructor
  end;

  // Note: Entry.fRawJSON is already set by Builder.SerializeSymbols in Extractor.Destroy
  // Don't call Entry.SerializeSymbols here as it would overwrite with flat format!

  DoLog('Reloaded %s in %d ms', [Code.FileName, MilliSecondsBetween(Now,StartTime)]);
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
  SymbolTable := TFPHashObjectList.Create(True);
  ErrorList := TStringList.Create;
end;

destructor TSymbolManager.Destroy; 
begin
  ErrorList.Free;
  SymbolTable.Free;
  inherited;
end;

finalization
  SymbolManager.Free;
end.
