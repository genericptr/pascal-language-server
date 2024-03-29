// Pascal Language Server
// Copyright 2020 Arjan Adriaanse
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

unit LSP.Basic;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$scopedenums on}

interface
uses
  FPJson,
  Classes, SysUtils, LSP.BaseTypes, LSP.Messages;

type


  { TDocumentUri }

  TDocumentUri = string;

  { TPosition }

  TPosition = class(TLSPStreamable)
  private
    fLine: Integer;
    fCharacter: Integer;
  public
    constructor Create; override;
    constructor Create(l, c: integer); overload;
    procedure Assign(Source : TPersistent); override;
  published
    // Line position in a document (zero-based).
    property line: Integer read fLine write fLine;
    // Character offset on a line in a document (zero-based). Assuming
    // that the line is represented as a string, the `character` value
    // represents the gap between the `character` and `character + 1`.
    //
    // If the character value is greater than the line length it
    // defaults back to the line length.
    property character: Integer read fCharacter write fCharacter;
  end;

  { TRange }

  TRange = class(TLSPStreamable)
  private
    fStart: TPosition;
    fEnd: TPosition;
    procedure SetEnd(AValue: TPosition);
    procedure SetStart(AValue: TPosition);
  published
    // The range's start position.
    property start: TPosition read fStart write SetStart;
    // The range's end position.
    property &end: TPosition read fEnd write SetEnd;
  public
    Constructor Create; override;
    constructor Create(line, column: integer); overload;
    constructor Create(line, column, len: integer); overload;
    constructor Create(startLine, startColumn: integer; endLine, endColumn: integer); overload;
    Procedure  SetRange(line, column: integer; len: integer = 0); overload;
    Procedure  SetRange(startLine, startColumn: integer; endLine, endColumn: integer); overload;
    Destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
    function ToString: String; override;
  end;

  { TLocation }

  TLocation = class(TLSPStreamable)
  private
    fUri: TDocumentUri;
    fRange: TRange;
    procedure SetRange(AValue: TRange);
  public
    constructor Create; override;
    constructor Create(Path: String; Line, Column, Span: Integer); overload;
    Destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    function LocalPath : String;
  published
    property uri: TDocumentUri read fUri write fUri;
    property range: TRange read fRange write SetRange;
  end;

  { TLocation }

  { TLocationItem }

  TLocationItem = class(TCollectionItem)
  private
    fUri: TDocumentUri;
    fRange: TRange;
    procedure SetRange(AValue: TRange);
  Public
    Constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    property uri: TDocumentUri read fUri write fUri;
    property range: TRange read fRange write SetRange;
  end;
  
  TLocationItems = specialize TGenericCollection<TLocationItem>;

  { TLocationLink }

  TLocationLink = class(TLSPStreamable)
  private
    fOriginSelectionRange: TRange;
    fTargetUri: TDocumentUri;
    fTargetRange: TRange;
    fTargetSelectionRange: TRange;
    procedure SetOriginSelectionRange(AValue: TRange);
    procedure SetTargetRange(AValue: TRange);
    procedure SetTargetSelectionRange(AValue: TRange);
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(aSource : TPersistent); override;
  published
    // Span of the origin of this link.
    //
    // Used as the underlined span for mouse interaction. Defaults to
    // the word range at the mouse position.
    property originSelectionRange: TRange read fOriginSelectionRange write SetOriginSelectionRange;
    // The target resource identifier of this link.
    property targetUri: TDocumentUri read fTargetUri write fTargetUri;
    // The full target range of this link. If the target for example
    // is a symbol then target range is the range enclosing this
    // symbol not including leading/trailing whitespace but everything
    // else like comments. This information is typically used to
    // highlight the range in the editor.
    property targetRange: TRange read fTargetRange write SetTargetRange;
    // The range that should be selected and revealed when this link
    // is being followed, e.g the name of a function.  Must be
    // contained by the the `targetRange`. See also
    // `DocumentSymbol#range`
    property targetSelectionRange: TRange read fTargetSelectionRange write SetTargetSelectionRange;
  end;

  { TTextDocumentIdentifier }

  TTextDocumentIdentifier = class(TLSPStreamable)
  private
    fUri: TDocumentUri;
  Public
    Procedure Assign(aSource : TPersistent); override;
    function LocalPath : String;
  published
    property uri: TDocumentUri read fUri write fUri;
  end;

  { TVersionedTextDocumentIdentifier
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#versionedTextDocumentIdentifier

    An identifier to denote a specific version of a text document. 
    This information usually flows from the client to the server. }

  TVersionedTextDocumentIdentifier = class(TTextDocumentIdentifier)
  private
    fVersion: TOptionalInteger;
  public
    Procedure Assign(aSource : TPersistent); override;
    Destructor Destroy; override;
  published
    // The version number of this document. If a versioned text
    // document identifier is sent from the server to the client and
    // the file is not open in the editor (the server has not received
    // an open notification before) the server can send `null` to
    // indicate that the version is known and the content on disk is
    // the master (as speced with document content ownership).
    //
    // The version number of a document will increase after each
    // change, including undo/redo. The number doesn't need to be
    // consecutive.
    property version: TOptionalInteger read fVersion write fVersion;
  end;

  { TTextEdit }

  TTextEdit = class(TCollectionItem)
  private
    fRange: TRange;
    fNewText: String;
    procedure SetRange(AValue: TRange);
  published
    // The range of the text document to be manipulated. To insert
    // text into a document create a range where start === end.
    property range: TRange read fRange write SetRange;
    // The string to be inserted. For delete operations use an empty
    // string.
    property newText: String read fNewText write fNewText;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    Procedure Assign(aSource : TPersistent); override;
  end;

  TTextEdits = specialize TGenericCollection<TTextEdit>;

  { TTextDocumentEdit }

  TTextDocumentEdit = class(TCollectionItem)
  private
    fTextDocument: TVersionedTextDocumentIdentifier;
    fEdits: TTextEdits;
    procedure SetEdits(AValue: TTextEdits);
    procedure SetTextDocument(AValue: TVersionedTextDocumentIdentifier);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  published
    // The text document to change.
    property textDocument: TVersionedTextDocumentIdentifier read fTextDocument write SetTextDocument;
    // The edits to be applied.
    property edits: TTextEdits read fEdits write SetEdits;
  end;

  TTextDocumentEdits = specialize TGenericCollection<TTextDocumentEdit>;

  { TTextDocumentItem }

  TTextDocumentItem = class(TLSPStreamable)
  private
    fUri: TDocumentUri;
    fLanguageId: string;
    fVersion: Integer;
    fText: string;
  Public
    procedure Assign(Source : TPersistent); override;
    function LocalPath : String;
  published
    // The text document's URI.
    property uri: TDocumentUri read fUri write fUri;
    // The text document's language identifier.
    property languageId: string read fLanguageId write fLanguageId;
    // The version number of this document (it will increase after
    // each change, including undo/redo).
    property version: Integer read fVersion write fVersion;
    // The content of the opened text document.
    property text: string read fText write fText;
  end;

  { TTextDocumentPositionParams }

  TTextDocumentPositionParams = class(TLSPStreamable)
  private
    fTextDocument: TTextDocumentIdentifier;
    fPosition: TPosition;
    procedure SetPosition(AValue: TPosition);
    procedure SetTextDocument(AValue: TTextDocumentIdentifier);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    function LocalPath : String;
  published
    // The text document.
    property textDocument: TTextDocumentIdentifier read fTextDocument write SetTextDocument;
    // The position inside the text document.
    property position: TPosition read fPosition write SetPosition;
  end;
  
  { TMarkupKind }

  { Describes the content type that a client supports in various
    result literals like `Hover`, `ParameterInfo` or
    `CompletionItem`.
  
    Please note that `MarkupKinds` must not start with a `$`. This
    kinds are reserved for internal usage. }
  
  TMarkupKind = Class
  public const
    PlainText = 'plaintext';   // Plain text is supported as a content format
    Markdown = 'markdown';     // Markdown is supported as a content format
  end;

  { TMarkupContent }

  { A `MarkupContent` literal represents a string value which content is interpreted base on its
    kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
    
    If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
    See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
       
    *Please Note* that clients might sanitize the return markdown. A client could decide to
    remove HTML from the markdown to avoid script execution. }

  TMarkupContent = class(TLSPStreamable)
  private
    fKind: string;
    fValue: string;
    function GetPlainText: Boolean;
    procedure SetPlainText(AValue: Boolean);
  published
    // The type of the Markup
    property kind: string read FKind write FKind;
    // The content itself
    property value: string read fValue write fValue;
  public
    constructor create; override;
    constructor Create(content: string; _plainText: boolean = true);
    Property PlainText : Boolean Read GetPlainText Write SetPlainText;
    procedure Assign(Source : TPersistent) ; override;
  end;

  { TDiagnostic }


  TDiagnosticSeverity = ( __UNUSED__,
                          Error,
                          Warning,
                          Information,
                          Hint
                          );

  TDiagnosticTag = ( // Unused or unnecessary code.
                     // Clients are allowed to render diagnostics with this tag faded out instead of having
                     // an error squiggle.
                     Unnecessary = 1,

                     //Deprecated or obsolete code.
                     //Clients are allowed to rendered diagnostics with this tag strike through.
                     Deprecated = 2
                     );
  TDiagnosticTags = set of TDiagnosticTag;

  // Represents a related message and source code location for a diagnostic. This should be
  // used to point to code locations that cause or are related to a diagnostics, e.g when duplicating
  // a symbol in a scope.

  { TDiagnosticRelatedInformation }

  TDiagnosticRelatedInformation = class (TCollectionItem)
  private
    fLocation: TLocation;
    fMessage: string;
    procedure SetLocation(AValue: TLocation);
  Public
    Constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // The location of this related diagnostic information.
    property location: TLocation read fLocation write SetLocation;
    // The message of this related diagnostic information.
    property message: string read fMessage write fMessage;
  end;

  TDiagnosticRelatedInformationItems = specialize TGenericCollection<TDiagnosticRelatedInformation>;

  // Represents a diagnostic, such as a compiler error or warning. Diagnostic objects are only valid in the scope of a resource.

  TDiagnostic = class (TCollectionItem)
  private
    fRange: TRange;
    fSeverity: TDiagnosticSeverity;
    fCode: integer;
    fSource: string;
    fMessage: string;
    procedure SetRange(AValue: TRange);
  Public
    Constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // The range at which the message applies.
    property range: TRange read fRange write SetRange;
    // The diagnostic's severity. Can be omitted. If omitted it is up to the
    // client to interpret diagnostics as error, warning, info or hint.
    property severity: TDiagnosticSeverity read fSeverity write fSeverity;
    // The diagnostic's code, which might appear in the user interface.
    property code: integer read fCode write fCode;
    // A human-readable string describing the source of this
    // diagnostic, e.g. 'typescript' or 'super lint'.
    property source: string read fSource write fSource;
    // The diagnostic's message.
    property message: string read fMessage write fMessage;

    // Additional metadata about the diagnostic.
    // @since 3.15.0
    // TODO: must be optional
    //property tags: TDiagnosticTags read fTags write fTags;

    // An array of related diagnostic information, e.g. when symbol-names within
    // a scope collide all definitions can be marked via this property. (OPTIONAL)
    // TODO: must be optional
    //property relatedInformation: TDiagnosticRelatedInformationItems read fRelatedInformation write fRelatedInformation;
  end;

  TDiagnosticItems = specialize TGenericCollection<TDiagnostic>;

  { TCommand
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command

    Represents a reference to a command. 
    Provides a title which will be used to represent a command in the UI. 
    Commands are identified by a string identifier. 
    The recommended way to handle commands is to implement their execution on the server 
    side if the client and server provides the corresponding capabilities. 
    Alternatively the tool extension code could handle the command. 
    The protocol currently doesn’t specify a set of well-known commands. }

  TCommand = class(TLSPStreamable)
  private
    fTitle: string;
    fCommand: string;
    fArguments: TStrings;
    procedure SetArguments(AValue: TStrings);
  public
    Constructor Create; override;
    destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // Title of the command, like `save`.
    property title: string read fTitle write fTitle;
    // The identifier of the actual command handler.
    property command: string read fCommand write fCommand;
    // Arguments that the command handler should be invoked with.
    property arguments: TStrings read fArguments write SetArguments;
  end;

  TAbstractMessage = LSP.Messages.TAbstractMessage;
  TRequestMessage = LSP.Messages.TRequestMessage;


  { TWorkspaceEdit
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceEdit

    A workspace edit represents changes to many resources managed in the workspace. 
    The edit should either provide changes or documentChanges. 
    If the client can handle versioned document edits and if documentChanges are present, 
    the latter are preferred over changes. }
  
  TWorkspaceEdit = class(TLSPStreamable)
  private
    fChanges: TCollection;
    fDocumentChanges: TTextDocumentEdits;
  published
    // Holds changes to existing resources.
    property changes: TCollection read fChanges write fChanges;
    // Depending on the client capability
    // `workspace.workspaceEdit.resourceOperations` document changes are either
    // an array of `TextDocumentEdit`s to express changes to n different text
    // documents where each text document edit addresses a specific version of
    // a text document. Or it can contain above `TextDocumentEdit`s mixed with
    // create, rename and delete file / folder operations.
    // 
    // Whether a client supports versioned document edits is expressed via
    // `workspace.workspaceEdit.documentChanges` client capability.
    // 
    // If a client neither supports `documentChanges` nor
    // `workspace.workspaceEdit.resourceOperations` then only plain `TextEdit`s
    // using the `changes` property are supported.
    // 
    // TextDocumentEdit[] | (TextDocumentEdit | CreateFile | RenameFile | DeleteFile)[]
    property documentChanges: TTextDocumentEdits read fDocumentChanges write fDocumentChanges;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;


{ Utilities }

function PathToURI(path: String): TDocumentUri;
function URIToPath(aURI: TDocumentUri): String;

{ Optional Operators }


implementation

uses URIParser;
// uses LSP.Base;


function PathToURI(path: String): TDocumentUri;
begin
  result := 'file://'+path;
end;

function URIToPath(aURI: TDocumentUri): String;

var
  lURI : TUri;

begin
  Result := '';
  if aUri = '' then
    exit;
  lURI := ParseURI(aURI);
  if Not SameText(lURI.Protocol,'file') then
    Raise EConvertError.Create('URI is not a file, cannot convert to path');
  Result:=lURI.path + lURI.Document;
end;

{ TWorkspaceEdit }

procedure TWorkspaceEdit.AfterConstruction;
begin
  inherited;
  documentChanges := TTextDocumentEdits.Create;
end;

destructor TWorkspaceEdit.Destroy;
begin
  documentChanges.Free;
  inherited;
end;

{ TTextEdit }

procedure TTextEdit.SetRange(AValue: TRange);
begin
  if fRange=AValue then Exit;
  fRange.Assign(AValue);
end;

constructor TTextEdit.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRange:=TRange.Create;
end;

destructor TTextEdit.Destroy;
begin
  FreeAndNil(fRange);
  inherited;
end;

procedure TTextEdit.Assign(aSource : TPersistent);

var
  Src : TTextEdit absolute aSource;

begin
  if aSource is TTextEdit then
    begin
    Range:=Src.Range;
    NewText:=Src.newText;
    end
  else
    inherited Assign(aSource);
end;

{ TTextDocumentEdit }

procedure TTextDocumentEdit.SetEdits(AValue: TTextEdits);
begin
  if fEdits=AValue then Exit;
  fEdits.assign(AValue);
end;

procedure TTextDocumentEdit.SetTextDocument(
  AValue: TVersionedTextDocumentIdentifier);
begin
  if fTextDocument=AValue then Exit;
  fTextDocument.Assign(AValue);
end;

constructor TTextDocumentEdit.Create(ACollection: TCollection);
begin
  inherited;
  Fedits := TTextEdits.Create;
  fTextDocument:=TVersionedTextDocumentIdentifier.Create;
end;


destructor TTextDocumentEdit.Destroy;
begin
  FEdits.Free;
  fTextDocument.Free;
  inherited;
end;

procedure TTextDocumentEdit.Assign(Source : TPersistent);

var
  Src : TTextDocumentEdit absolute Source;

begin
  if Source is TTextDocumentEdit then
    begin
    Edits:=Src.Edits;
    TextDocument:=Src.textDocument;
    end
  else
    inherited Assign(Source);
end;

{ TTextDocumentItem }

procedure TTextDocumentItem.Assign(Source : TPersistent);

var
  Src : TTextDocumentItem absolute Source;

begin
  if Source is TTextDocumentItem then
    begin
    Uri:=Src.Uri;
    LanguageId:=Src.languageId;
    Version:=Src.version;
    Text:=Src.Text;
    end
  else
    inherited Assign(Source);
end;

function TTextDocumentItem.LocalPath: String;
begin
  Result:=URIToPath(Uri);
end;

{ TTextDocumentPositionParams }

procedure TTextDocumentPositionParams.SetPosition(AValue: TPosition);
begin
  if fPosition=AValue then Exit;
  fPosition.Assign(AValue);
end;

procedure TTextDocumentPositionParams.SetTextDocument(
  AValue: TTextDocumentIdentifier);
begin
  if fTextDocument=AValue then Exit;
  fTextDocument.Assign(AValue);
end;

constructor TTextDocumentPositionParams.Create;
begin
  fTextDocument:=TTextDocumentIdentifier.Create;
  fPosition:=TPosition.Create;
end;

destructor TTextDocumentPositionParams.Destroy;
begin
  FreeAndNil(fTextDocument);
  FreeAndNil(fPosition);
  inherited Destroy;
end;

procedure TTextDocumentPositionParams.Assign(Source : TPersistent);

var
  Src : TTextDocumentPositionParams absolute source;

begin
  if Source is TTextDocumentPositionParams then
    begin
    textDocument:=Src.textDocument;
    position:=Src.position;
    end
  else
    inherited Assign(Source);
end;

function TTextDocumentPositionParams.LocalPath: String;
begin
  if Assigned(TextDocument) then
    Result:=URIToPath(textDocument.LocalPath)
  else
    Result:='';
end;


{ TCommand }

procedure TCommand.SetArguments(AValue: TStrings);
begin
  if fArguments=AValue then Exit;
  fArguments.Assign(AValue);
end;

constructor TCommand.Create;
begin
  fArguments:=TStringList.Create;
end;

destructor TCommand.destroy;
begin
  FreeAndNil(fArguments);
  inherited destroy;
end;

procedure TCommand.Assign(Source : TPersistent);

var
  Src : TCommand absolute Source;

begin
  if Source is TCommand then
    begin
    Title:=Src.Title;
    Command:=Src.Command;
    Arguments:=Src.arguments;
    end
  else
    inherited Assign(Source);
end;

{ TPosition }

constructor TPosition.Create;
begin
  Create(0,0);
end;

constructor TPosition.Create(l, c: integer);
begin
  Inherited Create;
  line := l;
  character := c;
end;

procedure TPosition.Assign(Source : TPersistent);

var
  Src : TPosition absolute source;

begin
  if Source is TPosition then
    begin
    Line:=Src.line;
    Character:=Src.character;
    end
  else
    inherited Assign(Source);
end;

{ TLocation }

procedure TLocation.SetRange(AValue: TRange);
begin
  if fRange=AValue then Exit;
  fRange.Assign(AValue);
end;

constructor TLocation.Create;
begin
  Create('',0,0,0);
end;

constructor TLocation.Create(Path: String; Line, Column, Span: Integer);
begin
  uri := PathToURI(Path);
  fRange := TRange.Create(Line, Column, Span);
end;

destructor TLocation.Destroy;
begin
  FreeAndNil(Frange);
  inherited Destroy;
end;

procedure TLocation.Assign(Source : TPersistent);

var
  Src : TLocation absolute source;

begin
  if Source is TLocation then
    begin
    Uri:=Src.Uri;
    Range:=Src.Range;
    end
  else
    inherited Assign(Source);
end;

function TLocation.LocalPath: String;
begin
  Result:=URIToPath(uri);
end;

{ TLocationItem }

procedure TLocationItem.SetRange(AValue: TRange);
begin
  if fRange=AValue then Exit;
  fRange.Assign(AValue);
end;

constructor TLocationItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRange:=TRange.Create;
end;

destructor TLocationItem.destroy;
begin
  FreeAndNil(fRange);
  inherited destroy;
end;

procedure TLocationItem.Assign(Source : TPersistent);

var
  Src : TLocationItem absolute source;

begin
  if Source is TLocationItem then
    begin
    Uri:=Src.uri;
    Range:=Src.range;
    end
  else
    inherited Assign(source);
end;

{ TLocationLink }

procedure TLocationLink.SetOriginSelectionRange(AValue: TRange);
begin
  if fOriginSelectionRange=AValue then Exit;
  fOriginSelectionRange.Assign(AValue);
end;

procedure TLocationLink.SetTargetRange(AValue: TRange);
begin
  if fTargetRange=AValue then Exit;
  fTargetRange.Assign(AValue);
end;

procedure TLocationLink.SetTargetSelectionRange(AValue: TRange);
begin
  if fTargetSelectionRange=AValue then Exit;
  fTargetSelectionRange.Assign(AValue);
end;

constructor TLocationLink.Create;
begin
  Inherited Create;
  fOriginSelectionRange:=TRange.Create;
  fTargetRange:=TRange.Create;
  fTargetSelectionRange:=TRange.Create;
end;

destructor TLocationLink.Destroy;
begin
  FreeAndNil(fOriginSelectionRange);
  FreeAndNil(fTargetRange);
  FreeAndNil(fTargetSelectionRange);
  inherited Destroy;
end;

procedure TLocationLink.Assign(aSource : TPersistent);

var
  Src :  TLocationLink absolute aSource;

begin
  if aSource is TLocationLink then
    begin
    OriginSelectionRange:=Src.OriginSelectionRange;
    TargetUri:=Src.TargetUri;
    TargetRange:=Src.TargetRange;
    TargetSelectionRange:=Src.TargetSelectionRange;
    end
  else
    inherited Assign(aSource);
end;

{ TTextDocumentIdentifier }

procedure TTextDocumentIdentifier.Assign(aSource : TPersistent);

Var
  Src : TTextDocumentIdentifier absolute aSource;

begin
  if (aSource is TTextDocumentIdentifier) then
    begin
    Uri:=Src.Uri;
    end
  else
    inherited Assign(aSource);
end;

function TTextDocumentIdentifier.LocalPath: String;
begin
  Result:=URIToPath(Uri)
end;

{ TVersionedTextDocumentIdentifier }

procedure TVersionedTextDocumentIdentifier.Assign(aSource : TPersistent);

Var
  Src : TVersionedTextDocumentIdentifier absolute aSource;

begin
  if (aSource is TVersionedTextDocumentIdentifier) then
    begin
    Version:=Src.Version;
    end;
  inherited Assign(aSource);
end;

destructor TVersionedTextDocumentIdentifier.Destroy;
begin
  FreeAndNil(fVersion);
  inherited Destroy;
end;

{ TRange }

procedure TRange.SetEnd(AValue: TPosition);
begin
  if fEnd=AValue then Exit;
  fEnd.Assign(AValue);
end;

procedure TRange.SetStart(AValue: TPosition);
begin
  if fStart=AValue then Exit;
  fStart.Assign(AValue);
end;

constructor TRange.Create;
begin
  fStart := TPosition.Create(0,0);
  fEnd := TPosition.Create(0,0);
end;

constructor TRange.Create(line, column: integer);
begin
  Create;
  SetRange(Line,Column,0);
end;

constructor TRange.Create(line, column, len: integer);
begin
  Create;
  SetRange(Line,Column,Len);
end;

constructor TRange.Create(startLine, startColumn: integer; endLine, endColumn: integer); overload;
begin
  Create;
  SetRange(startLine, startColumn,endLine, endColumn);
end;

procedure TRange.SetRange(line, column: integer; len: integer);
begin
  SetRange(Line,Column,Line,Column+Len);
end;

procedure TRange.SetRange(startLine, startColumn: integer; endLine,
  endColumn: integer);
begin
  fStart.Line:=StartLine;
  fStart.Character:=startColumn;
  fEnd.Line:=endLine;
  fEnd.Character:=endColumn;
end;

destructor TRange.destroy;
begin
  FreeAndNil(fStart);
  FreeAndNil(fEnd);
  inherited destroy;
end;

procedure TRange.Assign(Source: TPersistent);
var
  Src : TRange absolute Source;
begin
  if Source is TRange then
    begin
    Self.start.Assign(Src.start);
    Self.&end.Assign(Src.&end);
    end
  else
    inherited Assign(Source);
end;

function TRange.ToString: String;
begin
  result := 'start: ['+Start.line.ToString+':'+start.character.ToString+'], end: ['+&end.line.ToString+':'+&end.character.ToString+']';
end;

{ TMarkupContent }

function TMarkupContent.GetPlainText: Boolean;
begin
  Result:=(Kind=TMarkupKind.PlainText)
end;

procedure TMarkupContent.SetPlainText(AValue: Boolean);
begin
  if aValue then
    kind := TMarkupKind.PlainText
  else
    kind := TMarkupKind.Markdown;
end;

constructor TMarkupContent.create;
begin
  inherited create;
  kind:=TMarkupKind.PlainText
end;

constructor TMarkupContent.Create(content: string; _plainText: boolean = true);
begin
  Create;
  value := content;
  PlainText:=_plainText;
end;

procedure TMarkupContent.Assign(Source : TPersistent);

var
  Src : TMarkupContent absolute source;

begin
  if Source is TMarkupContent then
    begin
    Kind:=Src.Kind;
    Value:=Src.Value;
    end
  else
    inherited Assign(Source);
end;

{ TDiagnosticRelatedInformation }

procedure TDiagnosticRelatedInformation.SetLocation(AValue: TLocation);
begin
  if fLocation=AValue then Exit;
  fLocation.Assign(AValue);
end;

constructor TDiagnosticRelatedInformation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fLocation:=TLocation.Create('',0,0,0);
end;

destructor TDiagnosticRelatedInformation.destroy;
begin
  FreeAndNil(fLocation);
  inherited destroy;
end;

procedure TDiagnosticRelatedInformation.Assign(Source : TPersistent);

var
  Src : TDiagnosticRelatedInformation absolute source;

begin
  if source is TDiagnosticRelatedInformation then
    begin
    Location:=Src.location;
    message:=Src.message;
    end
  else
    inherited Assign(Source);
end;

procedure TDiagnostic.SetRange(AValue: TRange);
begin
  if fRange=AValue then Exit;
  fRange.Assign(AValue);
end;

constructor TDiagnostic.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRange:=TRange.Create;
end;

destructor TDiagnostic.destroy;
begin
  FreeAndNil(fRange);
  inherited destroy;
end;

procedure TDiagnostic.Assign(Source : TPersistent);
var
  Src : TDiagnostic absolute source;
begin
  if Source is TDiagnostic then
    begin
    Range:=Src.Range;
    Severity:=Src.severity;
    Code:=Src.Code;
    self.Source:=Src.Source;
    Message:=Src.Source;
    end
  else
    inherited Assign(Source);
end;


end.
  
