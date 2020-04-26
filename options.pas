// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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
  Classes;

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

  { TTextDocumentSyncOptions }

  TTextDocumentSyncOptions = class(TPersistent)
  private
    fOpenClose: Boolean;
    fChange: TTextDocumentSyncKind;
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
    fResolveProvider: Boolean;
  public
    constructor Create;
  published
    // The characters that trigger completion automatically.
    property triggerCharacters: TStrings read fTriggerCharacters write fTriggerCharacters;
    // The server provides support to resolve additional
    // information for a completion item.
    property resolveProvider: Boolean read fResolveProvider write fResolveProvider;
  end;

implementation

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

