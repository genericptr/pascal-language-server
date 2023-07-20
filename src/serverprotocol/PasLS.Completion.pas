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
unit PasLS.Completion;

{$mode objfpc}{$H+}
{$scopedenums on}

interface

uses
  Classes, DateUtils,
  CodeToolManager, CodeCache, IdentCompletionTool, BasicCodeTools, CodeTree,
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Completion;

Type
  { TCompletion }

  TCompletion = class(specialize TLSPRequest<TCompletionParams, TCompletionList>)
    function KindForIdentifier(Identifier: TIdentifierListItem): TCompletionItemKind;
    function Process(var Params: TCompletionParams): TCompletionList; override;
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



implementation

uses
  SysUtils, Contnrs,
  PasLS.CodeUtils, PasLS.Diagnostics, PasLS.Settings, LSP.Messages;

procedure TCompletionItemHelper.SetPrimaryText(text: string);

begin
  if ClientInfo.name = TClients.SublimeTextLSP then
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
  if ClientInfo.name = TClients.SublimeTextLSP then
    begin
      &label := text;
    end
  else
    begin
      // todo: append to details?
    end;
end;


{ TCompletion }
function TCompletion.KindForIdentifier(Identifier: TIdentifierListItem): TCompletionItemKind;
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
      // Transport.SendDiagnostic('Default kind for %s (%s)', [Identifier.Identifier, Identifier.Node.DescAsString]);
      //PrintIdentifierTree(Identifier);
      result := TCompletionItemKind.KeywordItem;
  end;
end;


function TCompletion.Process(var Params: TCompletionParams): TCompletionList;
var

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
    Code := CodeToolBoss.FindFile(textDocument.LocalPath);
    X := position.character;
    Y := position.line;
    Line := Code.GetLine(Y);
    GetIdentStartEndAtPosition(Line, X + 1, PStart, PEnd);
    CodeToolBoss.IdentifierList.Prefix := Copy(Line, PStart, PEnd - PStart);

    OverloadMap := TFPHashList.Create;

    Result := TCompletionList.Create;
    // Alias
    Completions:=Result.items;

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

                  Completion := Completions.Add;
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

                  Completion := Completions.Add;
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
          PublishCodeToolsError(Self.Transport,'');
          Result.isIncomplete := true;
        end;
    except
      on E: Exception do
        begin
          LogError('Completion', E);
          Result.isIncomplete := true;
        end;
    end;

    // todo: make this a verbosity option
    // DoLog('got completions %d in %d ms and processed in %d ms', [Completions.Count,MilliSecondsBetween(Now, GatherTime),MilliSecondsBetween(Now, StartTime));

  end;

  FreeAndNil(OverloadMap);
end;

end.

