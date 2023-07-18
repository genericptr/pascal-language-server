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

unit LSP.Completion;

{$mode objfpc}{$H+}
{$scopedenums on}

interface

uses
  Classes, DateUtils,
  LSP.Base, LSP.Basic, LSP.BaseTypes;

type

  { TCompletionTriggerKind }

  // How a completion was triggered
  TCompletionTriggerKind = (
    __UNUSED__,
    // Completion was triggered by typing an identifier (24x7 code
    // complete), manual invocation (e.g Ctrl+Space) or via API.
    Invoked,
    // Completion was triggered by a trigger character specified by
    // the `triggerCharacters` properties of the
    // `CompletionRegistrationOptions`.
    TriggerCharacter,
    // Completion was re-triggered as the current completion list is
    // incomplete.
    TriggerForIncompleteCompletions);

  { TCompletionContext }

  // Contains additional information about the context in which a
  // completion request is triggered.
  TCompletionContext = class(TLSPStreamable)
  private
    fTriggerKind: TCompletionTriggerKind;
    fTriggerCharacter: string;
  Public
    Procedure Assign(Source : TPersistent); override;
  published
    // How the completion was triggered.
    property triggerKind: TCompletionTriggerKind read fTriggerKind write fTriggerKind;
    // The trigger character (a single character) that has trigger
    // code complete.  Is undefined if `triggerKind !==
    // CompletionTriggerKind.TriggerCharacter`
    property triggerCharacter: string read fTriggerCharacter write fTriggerCharacter;
  end;

  { TCompletionParams }

  TCompletionParams = class(TTextDocumentPositionParams)
  private
    fContext: TCompletionContext;
    procedure SetContext(AValue: TCompletionContext);
  public
    constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // The completion context. This is only available if the client
    // specifies to send this using
    // `ClientCapabilities.textDocument.completion.contextSupport ===
    // true`
    property context: TCompletionContext read fContext write SetContext;
  end;

  { TInsertTextFormat }

  // Defines whether the insert text in a completion item should be
  // interpreted as plain text or a snippet.
  TInsertTextFormat = (
    __UNUSED__,
    // The primary text to be inserted is treated as a plain string.
    PlainText,
    // The primary text to be inserted is treated as a snippet.
    //
    // A snippet can define tab stops and placeholders with `$1`, `$2`
    // and `${3:foo}`. `$0` defines the final tab stop, it defaults to
    // the end of the snippet. Placeholders with equal identifiers are
    // linked, that is typing in one will update others too.
    Snippet
  );

  { TCompletionItemTag }

  // Completion item tags are extra annotations that tweak the
  // rendering of a completion item.
  //
  // @since 3.15.0
  TCompletionItemTag = (
    // Render a completion as obsolete, usually using a strike-out.
    Deprecated = 1);

  TCompletionItemTags = set of TCompletionItemTag;

  { TCompletionItemKind }

  // The kind of a completion entry.
  TCompletionItemKind = (
    __UNUSED__,
    TextItem,
    MethodItem,
    FunctionItem,
    ConstructorItem,
    FieldItem,
    VariableItem,
    ClassItem,
    InterfaceItem,
    ModuleItem,
    PropertyItem,
    UnitItem,
    ValueItem,
    EnumItem,
    KeywordItem,
    SnippetItem,
    ColorItem,
    FileItem,
    ReferenceItem,
    FolderItem,
    EnumMemberItem,
    ConstantItem,
    StructItem,
    EventItem,
    OperatorItem,
    TypeParameterItem
  );

  { TCompletionItem }

  TCompletionItem = class(TCollectionItem)
  private
    fLabel: string;
    fKind: TCompletionItemKind;
    fTags: TCompletionItemTags;
    fDetail: string;
    fDocumentation: TMarkupContent;
    fPreselect: Boolean;
    fSortText: string;
    fFilterText: string;
    fInsertText: string;
    fInsertTextFormat: TInsertTextFormat;
    fTextEdit: TTextEdit;
    fAdditionalTextEdits: TTextEdits;
    fCommitCharacters: TStrings;
  Public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
  public
    // the following private fields are for private us and not part of LSP
    overloadCount: integer;
    procedure SetAdditionalTextEdits(AValue: TTextEdits);
    procedure SetCommitCharacters(AValue: TStrings);
    procedure SetDocumentation(AValue: TMarkupContent);
    procedure SetTextEdit(AValue: TTextEdit);
  published
    // The label of this completion item. By default also the text
    // that is inserted when selecting this completion.
    property &label: string read fLabel write fLabel;
    // The kind of this completion item. Based of the kind an icon is
    // chosen by the editor. The standardized set of available values
    // is defined in `CompletionItemKind`.
    property kind: TCompletionItemKind read fKind write fKind;
    // Tags for this completion item.
    //
    // @since 3.15.0
    property tags: TCompletionItemTags read fTags write fTags;
    // A human-readable string with additional information about this
    // item, like type or symbol information.
    property detail: string read fDetail write fDetail;
    // A human-readable string that represents a doc-comment.
    property documentation: TMarkupContent read fDocumentation write SetDocumentation;
    // Select this item when showing.
    //
    // *Note* that only one completion item can be selected and that
    // the tool / client decides which item that is. The rule is that
    // the *first* item of those that match best is selected.
    property preselect: Boolean read fPreselect write fPreselect;
    // A string that should be used when comparing this item
    // with other items. When `falsy` the label is used.
    property sortText: string read fSortText write fSortText;
    // A string that should be used when filtering a set of
    // completion items. When `falsy` the label is used.
    property filterText: string read fFilterText write fFilterText;
    // A string that should be inserted into a document when selecting
    // this completion. When `falsy` the label is used.
    //
    // The `insertText` is subject to interpretation by the client
    // side.  Some tools might not take the string literally. For
    // example VS Code when code complete is requested in this example
    // `con<cursor position>` and a completion item with an
    // `insertText` of `console` is provided it will only insert
    // `sole`. Therefore it is recommended to use `textEdit` instead
    // since it avoids additional client side interpretation.
    property insertText: string read fInsertText write fInsertText;
    // The format of the insert text. The format applies to both the
    // `insertText` property and the `newText` property of a provided
    // `textEdit`. If omitted defaults to
    // `InsertTextFormat.PlainText`.
    property insertTextFormat: TInsertTextFormat read fInsertTextFormat write fInsertTextFormat;
    // An edit which is applied to a document when selecting this
    // completion. When an edit is provided the value of `insertText`
    // is ignored.
    //
    // *Note:* The range of the edit must be a single line range and
    // it must contain the position at which completion has been
    // requested.
    property textEdit: TTextEdit read fTextEdit write SetTextEdit;
    // An optional array of additional text edits that are applied
    // when selecting this completion. Edits must not overlap
    // (including the same insert position) with the main edit nor
    // with themselves.
    //
    // Additional text edits should be used to change text unrelated
    // to the current cursor position (for example adding an import
    // statement at the top of the file if the completion item will
    // insert an unqualified type).
    property additionalTextEdits: TTextEdits read fAdditionalTextEdits write SetAdditionalTextEdits;
    // An optional set of characters that when pressed while this
    // completion is active will accept it first and then type that
    // character. *Note* that all commit characters should have
    // `length=1` and that superfluous characters will be ignored.
    property commitCharacters: TStrings read fCommitCharacters write SetCommitCharacters;
  end;

  TCompletionItems = specialize TGenericCollection<TCompletionItem>;

  { TCompletionList
    Represents a collection of completion items to be presented in the editor. }

  TCompletionList = class(TLSPStreamable)
  private
    fIsIncomplete: Boolean;
    fItems: TCompletionItems;
    procedure SetItems(AValue: TCompletionItems);
  Public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  published
    // This list it not complete. Further typing should result in
    // recomputing this list.
    property isIncomplete: Boolean read fIsIncomplete write fIsIncomplete;
    // The completion items.
    property items: TCompletionItems read fItems write SetItems;
  end;


implementation

uses
  SysUtils,
  LSP.Messages;

{ TCompletionContext }

procedure TCompletionContext.Assign(Source: TPersistent);
var
  Src: TCompletionContext absolute Source;
begin
  if Source is TCompletionContext  then
    begin
    fTriggerKind:=Src.triggerKind;
    fTriggerCharacter:=Src.triggerCharacter;
    end
  else
    inherited Assign(Source);
end;

{ TCompletionParams }

procedure TCompletionParams.SetContext(AValue: TCompletionContext);
begin
  if fContext=AValue then Exit;
  fContext.Assign(AValue);
end;

constructor TCompletionParams.Create;
begin
  inherited Create;
  fContext:=TCompletionContext.Create;
end;

destructor TCompletionParams.Destroy;
begin
  FreeAndNil(fContext);
  inherited Destroy;
end;

procedure TCompletionParams.Assign(Source: TPersistent);
var
  Src: TCompletionParams absolute Source;
begin
  if Source is TCompletionParams then
    begin
      Context.Assign(Src.context)
    end
  else
    inherited Assign(Source);
end;

{ TCompletionItem }

procedure TCompletionItem.SetAdditionalTextEdits(AValue: TTextEdits);
begin
  if fAdditionalTextEdits=AValue then Exit;
  if fAdditionalTextEdits=Nil then
    fAdditionalTextEdits:=TTextEdits.Create;
  fAdditionalTextEdits.Assign(AValue);
end;

procedure TCompletionItem.SetCommitCharacters(AValue: TStrings);
begin
  if fCommitCharacters=AValue then Exit;
  fCommitCharacters.Assign(AValue);
end;

procedure TCompletionItem.SetDocumentation(AValue: TMarkupContent);
begin
  if fDocumentation=AValue then Exit;
  fDocumentation.Assign(AValue);
end;


procedure TCompletionItem.SetTextEdit(AValue: TTextEdit);
begin
  if fTextEdit=AValue then Exit;
  if fTextEdit=Nil then
    fTextedit:=TTextEdit.Create(Nil);
  fTextEdit.Assign(AValue);
end;

constructor TCompletionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fDocumentation:=TMarkupContent.Create;
  fCommitCharacters:=TStringList.Create;
end;

destructor TCompletionItem.destroy;
begin
  FreeAndNil(fDocumentation);
  FreeAndNil(fTextEdit);
  FreeAndNil(fAdditionalTextEdits);
  FreeAndNil(fCommitCharacters);
  inherited destroy;
end;

{ TCompletionList }

procedure TCompletionList.SetItems(AValue: TCompletionItems);
begin
  if fItems=AValue then Exit;
  fItems.Assign(AValue);
end;

constructor TCompletionList.Create;
begin
  inherited Create;
  fItems:=TCompletionItems.Create;
end;

destructor TCompletionList.Destroy;
begin
  FreeAndnil(fItems);
  inherited Destroy;
end;

procedure TCompletionList.Assign(Source: TPersistent);
var
  src: TCompletionList absolute source;
begin
  if Source is TCompletionList then
    begin
      isIncomplete:=Src.isIncomplete;
      Items.Assign(Src.Items);
    end
  else
    inherited Assign(Source);
end;

end.

