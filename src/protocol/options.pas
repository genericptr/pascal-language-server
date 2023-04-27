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

unit options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, 
  basic, workDoneProgress;

type

  { TTextDocumentSyncKind }

  // Defines how the host (editor) should sync document changes to the
  // language server.
  TTextDocumentSyncKind = (
    // Documents should not be synced at all.
    None = 0,
    // Documents are synced by always sending the full content
    Full = 1,
    // Documents are synced by sending the full content on open.
    // After that only incremental updates to the document are send.
    Incremental = 2);

  { TSaveOptions }

  TSaveOptions = class(TPersistent)
  private
    fIncludeText: Boolean;
  published
    // The client is supposed to include the content on save.
    property includeText: Boolean read fIncludeText write fIncludeText;
  public
    constructor Create(_includeText: boolean);
  end;

  { TTextDocumentSyncOptions }

  TTextDocumentSyncOptions = class(TPersistent)
  private
    fOpenClose: Boolean;
    fChange: TTextDocumentSyncKind;
    fWillSave: Boolean;
    fWillSaveWaitUntil: Boolean;
    fSave: TSaveOptions;
  public
    constructor Create;
  published
    // Open and close notifications are sent to the server. If omitted
    // open close notification should not be sent.
    property openClose: Boolean read fOpenClose write fOpenClose;
    // Change notifications are sent to the server. See
    // TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
    // TextDocumentSyncKind.Incremental. If omitted it defaults to
    // TextDocumentSyncKind.None.
    property change: TTextDocumentSyncKind read fChange write fChange;
    // If present will save wait until requests are sent to the server. If omitted the request should not be
    // sent.
    property willSave: Boolean read fWillSave write fWillSave;
    // If present will save wait until requests are sent to the server. If omitted the request should not be
    // sent.
    property willSaveWaitUntil: Boolean read fWillSaveWaitUntil write fWillSaveWaitUntil;
    // If present save notifications are sent to the server. If omitted the notification should not be
    // sent.
    property save: TSaveOptions read fSave write fSave;
  end;

  { TSignatureHelpOptions }
  
  TSignatureHelpOptions = class(TPersistent)
  private
    fTriggerCharacters: TStrings;
  published
    // The characters that trigger signature help automatically.
    property triggerCharacters: TStrings read fTriggerCharacters write fTriggerCharacters;
  end;

  { TCompletionOptions }

  TCompletionOptions = class(TPersistent)
  private
    fTriggerCharacters: TStrings;
    fAllCommitCharacters: TStrings;
    fResolveProvider: Boolean;
  public
    constructor Create;
  published
    // Most tools trigger completion request automatically without
    // explicitly requesting it using a keyboard shortcut
    // (e.g. Ctrl+Space). Typically they do so when the user starts to
    // type an identifier. For example if the user types `c` in a
    // JavaScript file code complete will automatically pop up present
    // `console` besides others as a completion item. Characters that
    // make up identifiers don't need to be listed here.
    //
    // If code complete should automatically be trigger on characters
    // not being valid inside an identifier (for example `.` in
    // JavaScript) list them in `triggerCharacters`.
    property triggerCharacters: TStrings read fTriggerCharacters write fTriggerCharacters;
    // The list of all possible characters that commit a
    // completion. This field can be used if clients don't support
    // individual commit characters per completion item. See
    // `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`.
    //
    // If a server provides both `allCommitCharacters` and commit
    // characters on an individual completion item the ones on the
    // completion item win.
    //
    // @since 3.2.0
    property allCommitCharacters: TStrings read fAllCommitCharacters write fAllCommitCharacters;

    // The server provides support to resolve additional information
    // for a completion item.
    property resolveProvider: Boolean read fResolveProvider write fResolveProvider;
  end;

  { TWorkDoneProgressOptions }
  
  TWorkDoneProgressOptions = class(TPersistent)
  private
    fworkDoneProgress: TOptionalBoolean;
  published
    property workDoneProgress: TOptionalBoolean read fworkDoneProgress write fworkDoneProgress;
  end;

  { TExecuteCommandOptions }

  TExecuteCommandOptions = class(TWorkDoneProgressOptions)
  private
    fCommands: TStrings;
  published
    // The commands to be executed on the server
    property commands: TStrings read fCommands write fCommands;
  public
    constructor Create(_commands: TStringArray);
  end;

  { TInlayHintOptions }

  TInlayHintOptions = class(TWorkDoneProgressOptions)
  private
    fResolveProvider: TOptionalBoolean;
  published
    // The server provides support to resolve additional information for an inlay hint item.
    property resolveProvider: TOptionalBoolean read fResolveProvider write fResolveProvider;
  end;

implementation

{ TExecuteCommandOptions }

constructor TExecuteCommandOptions.Create(_commands: TStringArray);
var
  command: String;
begin
  commands := TStringList.Create;
  for command in _commands do
    commands.Add(command);
end;

{ TSaveOptions }

constructor TSaveOptions.Create(_includeText: boolean);
begin
  includeText := _includeText;
end;

{ TTextDocumentSyncOptions}

constructor TTextDocumentSyncOptions.Create;
begin
  openClose := True;
  change := TTextDocumentSyncKind.Full;
end;

{ TCompletionOptions }

constructor TCompletionOptions.Create;
begin
  resolveProvider := False;
end;

end.

