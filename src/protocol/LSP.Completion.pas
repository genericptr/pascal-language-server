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
  Classes, DateUtils, URIParser, 
  CodeToolManager, CodeCache, IdentCompletionTool, BasicCodeTools, CodeTree,
  LSP.Base, LSP.Basic;

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
  TCompletionContext = class(TPersistent)
  private
    fTriggerKind: TCompletionTriggerKind;
    fTriggerCharacter: string;
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
  published
    // The completion context. This is only available if the client
    // specifies to send this using
    // `ClientCapabilities.textDocument.completion.contextSupport ===
    // true`
    property context: TCompletionContext read fContext write fContext;
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
  private
    // the following private fields are for private us and not part of LSP
    overloadCount: integer;
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
    property documentation: TMarkupContent read fDocumentation write fDocumentation;
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
    property textEdit: TTextEdit read fTextEdit write fTextEdit;
    // An optional array of additional text edits that are applied
    // when selecting this completion. Edits must not overlap
    // (including the same insert position) with the main edit nor
    // with themselves.
    //
    // Additional text edits should be used to change text unrelated
    // to the current cursor position (for example adding an import
    // statement at the top of the file if the completion item will
    // insert an unqualified type).
    property additionalTextEdits: TTextEdits read fAdditionalTextEdits write fAdditionalTextEdits;
    // An optional set of characters that when pressed while this
    // completion is active will accept it first and then type that
    // character. *Note* that all commit characters should have
    // `length=1` and that superfluous characters will be ignored.
    property commitCharacters: TStrings read fCommitCharacters write fCommitCharacters;
  end;

  TCompletionItems = specialize TGenericCollection<TCompletionItem>;

  { TCompletionList
    Represents a collection of completion items to be presented in the editor. }

  TCompletionList = class(TPersistent)
  private
    fIsIncomplete: Boolean;
    fItems: TCompletionItems;
  published
    // This list it not complete. Further typing should result in
    // recomputing this list.
    property isIncomplete: Boolean read fIsIncomplete write fIsIncomplete;
    // The completion items.
    property items: TCompletionItems read fItems write fItems;
  end;

  { TCompletion }

  TCompletion = class(specialize TLSPRequest<TCompletionParams, TCompletionList>)
    function Process(var Params: TCompletionParams): TCompletionList; override;
  end;

implementation

uses
  SysUtils, Contnrs,
  PasLS.CodeUtils, LSP.Diagnostics, PasLS.Settings;

function KindForIdentifier(Identifier: TIdentifierListItem): TCompletionItemKind;
begin
  // the identifier has no node so we consider this a text item
  if Identifier.Node = nil then
    exit(TCompletionItemKind.TextItem);

  // get completion item kind from identifier node
  case Identifier.Node.Desc of
    ctnUnit,
    ctnUseUnit,
    ctnUseUnitClearName,
    ctnUseUnitNamespace:
      result := TCompletionItemKind.ModuleItem;
    ctnClass,
    ctnObject,
    ctnObjCClass,
    ctnObjCCategory,
    ctnObjCProtocol,
    ctnCPPClass,
    ctnTypeHelper,
    ctnRecordHelper:
      result := TCompletionItemKind.ClassItem;
    ctnRecordType:
      result := TCompletionItemKind.StructItem;
    ctnClassInterface,
    ctnDispinterface:
      result := TCompletionItemKind.InterfaceItem;
    ctnTypeSection,
    ctnVarSection,
    ctnConstSection,
    ctnResStrSection,
    ctnLabelSection,
    ctnPropertySection,
    ctnUsesSection,
    ctnRequiresSection,
    ctnContainsSection,
    ctnExportsSection:
      result := TCompletionItemKind.FolderItem; {todo: not sure?}
    ctnProcedure:
      begin
        if not ServerSettings.minimalisticCompletions and IsNodeObjectMember(Identifier.Node) then
          result := TCompletionItemKind.MethodItem
        else
          result := TCompletionItemKind.FunctionItem;
      end;
    ctnTypeDefinition:
      result := TCompletionItemKind.TypeParameterItem;
    ctnGenericType,
    ctnGenericParameter:
      result := TCompletionItemKind.TypeParameterItem; {todo: generics of class/recrod??}
    ctnProperty,
    ctnGlobalProperty:
      result := TCompletionItemKind.PropertyItem;
    ctnVarDefinition:
      begin
        if not ServerSettings.minimalisticCompletions and IsNodeObjectMember(Identifier.Node) then
          result := TCompletionItemKind.FieldItem
        else
          result := TCompletionItemKind.VariableItem;
      end;
    ctnConstDefinition:
      result := TCompletionItemKind.ConstantItem;
    ctnEnumerationType:
      result := TCompletionItemKind.EnumItem;
    ctnEnumIdentifier:
      result := TCompletionItemKind.EnumMemberItem;
    otherwise
      //writeln(StdErr, 'Default kind for '+Identifier.Identifier, ' (', Identifier.Node.DescAsString, ')');
      //PrintIdentifierTree(Identifier);
      result := TCompletionItemKind.KeywordItem;
  end;   
end;

{ TCompletionItemHelper }

type
  TCompletionItemHelper = class helper for TCompletionItem
    private
      procedure SetPrimaryText(text: string);
      procedure SetSecondaryText(text: string);
    public
      property primaryText: string write SetPrimaryText;
      property secondaryText: string write SetSecondaryText;
  end;

procedure TCompletionItemHelper.SetPrimaryText(text: string);
begin
  if ClientInfo.name = 'Sublime Text LSP' then
    begin
      filterText := text;
    end
  else
    begin
      filterText := text;
      &label := text;
    end;
end;

procedure TCompletionItemHelper.SetSecondaryText(text: string);
begin
  if ClientInfo.name = 'Sublime Text LSP' then
    begin
      &label := text;
    end
  else
    begin
      // todo: append to details?
    end;
end;

{ TCompletion }

function TCompletion.Process(var Params: TCompletionParams): TCompletionList;
var
  URI: TURI;
  Code: TCodeBuffer;
  X, Y, PStart, PEnd, Count, I: Integer;
  Line: String;
  Completions: TCompletionItems;
  Identifier: TIdentifierListItem;
  Completion: TCompletionItem;

  OverloadMap: TFPHashList;

  IdentContext, IdentDetails: ShortString;
  ObjectMember: boolean;
  Kind: TCompletionItemKind;
begin with Params do
  begin

    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    X := position.character;
    Y := position.line;
    Line := Code.GetLine(Y);
    GetIdentStartEndAtPosition(Line, X + 1, PStart, PEnd);
    CodeToolBoss.IdentifierList.Prefix := Copy(Line, PStart, PEnd - PStart);

    OverloadMap := TFPHashList.Create;
    Completions := TCompletionItems.Create;
    Result := TCompletionList.Create;

    try
      if CodeToolBoss.GatherIdentifiers(Code, X + 1, Y + 1) then
        begin
          Count := CodeToolBoss.IdentifierList.GetFilteredCount;
          IdentContext := '';
          IdentDetails := '';
          for I := 0 to Count - 1 do
            begin

              // make sure we don't exceed the maximum completions count
              if (ServerSettings.maximumCompletions > -1) and (I >= ServerSettings.maximumCompletions) then
                begin
                  Result.isIncomplete := true;
                  break;
                end;

              Identifier := CodeToolBoss.IdentifierList.FilteredItems[I];

              if not ServerSettings.minimalisticCompletions then
                IdentContext := IdentifierContext(Identifier, IdentDetails, ObjectMember);

              if Identifier.IsProcNodeWithParams then
                begin
                  //SnippetText := ParseParamList(RawList, True);
                  //SnippetText := '$0';

                  // the completion is overloaded so increment the overload count
                  Completion := TCompletionItem(OverloadMap.Find(Identifier.Identifier));
                  if Completion <> nil then
                    begin
                      Inc(Completion.overloadCount);
                      if Completion.overloadCount = 1 then
                        Completion.secondaryText := '+'+IntToStr(Completion.overloadCount)+' overload'
                      else
                        Completion.secondaryText := '+'+IntToStr(Completion.overloadCount)+' overloads';
                      continue;
                    end;

                  Kind := KindForIdentifier(Identifier);

                  if (ServerSettings.ignoreTextCompletions) and (Kind = TCompletionItemKind.TextItem) then
                    continue;

                  Completion := TCompletionItem(Completions.Add);
                  Completion.primaryText := Identifier.Identifier;
                  Completion.secondaryText := IdentContext;
                  Completion.kind := Kind;

                  if not ServerSettings.minimalisticCompletions then
                    begin
                      // todo: make showing parameters in details as an option?
                      //Completion.detail := IdentDetails+' ('+Identifier.ParamNameList+')';
                      Completion.detail := IdentDetails;
                      if ServerSettings.insertCompletionsAsSnippets then
                        begin
                          Completion.insertText := Identifier.Identifier+'($0)';
                          Completion.insertTextFormat := TInsertTextFormat.Snippet;
                        end
                      else
                        begin
                          Completion.insertText := Identifier.Identifier;
                          Completion.insertTextFormat := TInsertTextFormat.PlainText;
                        end;
                    end;

                  Completion.sortText := IntToStr(I);
                  OverloadMap.Add(Identifier.Identifier, Completion);
                end
              else
                begin
                  Kind := KindForIdentifier(Identifier);

                  if (ServerSettings.ignoreTextCompletions) and (Kind = TCompletionItemKind.TextItem) then
                    continue;

                  Completion := TCompletionItem(Completions.Add);
                  if not ServerSettings.minimalisticCompletions then
                    begin
                      Completion.secondaryText := IdentContext;
                      Completion.primaryText := Identifier.Identifier;
                      Completion.detail := IdentDetails;
                      Completion.insertText := Identifier.Identifier;
                      Completion.insertTextFormat := TInsertTextFormat.PlainText;
                    end
                  else
                    begin
                      Completion.primaryText := Identifier.Identifier;
                      Completion.secondaryText := Identifier.Identifier;
                    end;
                  Completion.kind := Kind;
                  Completion.sortText := IntToStr(I);
                end;
            end;
        end else begin
          PublishDiagnostic;
          Result.isIncomplete := true;
        end;
    except
      on E: Exception do
        begin
          writeln(StdErr, 'Completion Error: ', E.ClassName, ' ', E.Message);
          Flush(StdErr);
          Result.isIncomplete := true;
        end;
    end;

    // todo: make this a verbosity option
    //writeln(StdErr, 'got completions ', Completions.Count, ' in ', MilliSecondsBetween(Now, GatherTime), 'ms and processed in ', MilliSecondsBetween(Now, StartTime),'ms');
    //Flush(StdErr);
      
    Result.items := Completions;
  end;

  FreeAndNil(OverloadMap);
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/completion', TCompletion);
end.

