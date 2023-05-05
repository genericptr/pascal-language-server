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

unit LSP.DocumentSymbol;

{$mode objfpc}{$H+}
{define SYMBOL_DEBUG}

interface

uses
  { RTL }
  Classes,  URIParser, FPJson, FPJsonRPC,
  { Code Tools }
  CodeToolManager, LinkScanner,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Streaming;

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

  TDocumentSymbolParams = class(TLSPStreamable)
  private
    fTextDocument: TTextDocumentIdentifier;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    // The text document.
    property textDocument: TTextDocumentIdentifier read fTextDocument write fTextDocument;
  end;

  { TDocumentSymbolRequest }

  { The document symbol request is sent from the client to the server. The returned result is either:

    * SymbolInformation[] which is a flat list of all symbols found in a given text document. 
      Then neither the symbol’s location range nor the symbol’s container name should be used to infer a hierarchy.
    * DocumentSymbol[] which is a hierarchy of symbols found in a given text document. }

  TDocumentSymbolRequest = class(specialize TLSPRequest<TDocumentSymbolParams, TLSPStreamable>)
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
  end;

function SymbolKindToString(kind: TSymbolKind): ShortString;
function SymbolKindFromString(kind: ShortString): TSymbolKind;

implementation
uses
  { RTL }
  SysUtils, FileUtil, DateUtils, fpjsonrtti, 
  { Code Tools }

  FindDeclarationTool, KeywordFuncLists,
  { Protocol }
  PasLS.Symbols;

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
    TSymbolKind._TypeParameter: result := 'TypeParameter';
    otherwise
      result := '???';
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

{ TDocumentSymbolParams }

constructor TDocumentSymbolParams.Create;
begin
  inherited Create;
  fTextDocument:=TTextDocumentIdentifier.Create;
end;

destructor TDocumentSymbolParams.Destroy;
begin
  FreeAndNil(fTextDocument);
  inherited Destroy;
end;

procedure TDocumentSymbolParams.Assign(Source: TPersistent);
var
  Src: TDocumentSymbolParams absolute Source;
begin
  if Source is TDocumentSymbolParams then
    begin
      textDocument.Assign(Src.textDocument);
    end
  else
    inherited Assign(Source);
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
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/documentSymbol', TDocumentSymbolRequest);
end.
