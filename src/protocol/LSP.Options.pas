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

unit LSP.Options;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  SysUtils, Classes,
  { Protocol }
  LSP.Basic;

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
  public
    constructor Create(_includeText: boolean);
    Procedure Assign(aSource : TPersistent); override;
  published
    // The client is supposed to include the content on save.
    property includeText: Boolean read fIncludeText write fIncludeText;
  end;

  { TTextDocumentSyncOptions }

  TTextDocumentSyncOptions = class(TPersistent)
  private
    fOpenClose: Boolean;
    fChange: TTextDocumentSyncKind;
    fWillSave: Boolean;
    fWillSaveWaitUntil: Boolean;
    fSave: TSaveOptions;
    procedure SetSave(AValue: TSaveOptions);
  public
    constructor Create;
    destructor destroy; override;
    Procedure Assign(aSource : TPersistent); override;
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
    property save: TSaveOptions read fSave write SetSave;
  end;

  { TSignatureHelpOptions }
  
  TSignatureHelpOptions = class(TPersistent)
  private
    fTriggerCharacters: TStrings;
    procedure SetTriggerCharacters(AValue: TStrings);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // The characters that trigger signature help automatically.
    property triggerCharacters: TStrings read fTriggerCharacters write SetTriggerCharacters;
  end;

  { TCompletionOptions }

  TCompletionOptions = class(TPersistent)
  private
    fTriggerCharacters: TStrings;
    fAllCommitCharacters: TStrings;
    fResolveProvider: Boolean;
  public
    constructor Create;
    destructor destroy; override;
    procedure Assign(Source : TPersistent); override;
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
  Public
    Procedure Assign(Source : TPersistent) ; override;
  published
    property workDoneProgress: TOptionalBoolean read fworkDoneProgress write fworkDoneProgress;
  end;

  { TExecuteCommandOptions }

  TExecuteCommandOptions = class(TWorkDoneProgressOptions)
  private
    fCommands: TStrings;
    procedure SetCommands(AValue: TStrings);
  public
    constructor Create(_commands: TStringArray);
    destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // The commands to be executed on the server
    property commands: TStrings read fCommands write SetCommands;
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

procedure TExecuteCommandOptions.SetCommands(AValue: TStrings);
begin
  if fCommands=AValue then Exit;
  fCommands.Assign(AValue);
end;

constructor TExecuteCommandOptions.Create(_commands: TStringArray);
var
  command: String;
begin
  FCommands := TStringList.Create;
  for command in _commands do
    commands.Add(command);
end;

destructor TExecuteCommandOptions.destroy;
begin
  FreeAndNil(FCommands);
  inherited destroy;
end;

procedure TExecuteCommandOptions.Assign(Source: TPersistent);

var
  Src : TExecuteCommandOptions absolute Source;

begin
  if Source is TExecuteCommandOptions then
    begin
    Commands.Assign(Src.commands);
    end
  else
    inherited Assign(Source);
end;

{ TSaveOptions }

constructor TSaveOptions.Create(_includeText: boolean);
begin
  includeText := _includeText;
end;

procedure TSaveOptions.Assign(aSource: TPersistent);

var
  Src : TSaveOptions absolute aSource;

begin
  if aSource is TSaveOptions then
    begin
    includeText:=Src.includeText;
    end
  else
    inherited Assign(aSource);
end;

{ TTextDocumentSyncOptions}

procedure TTextDocumentSyncOptions.SetSave(AValue: TSaveOptions);
begin
  if fSave=AValue then Exit;
  fSave.Assign(AValue);
end;

constructor TTextDocumentSyncOptions.Create;
begin
  openClose := True;
  change := TTextDocumentSyncKind.Full;
  fSave:=TSaveOptions.Create(False);
end;

destructor TTextDocumentSyncOptions.destroy;
begin
  FreeAndNil(fSave);
  inherited destroy;
end;

procedure TTextDocumentSyncOptions.Assign(aSource: TPersistent);

var
  Src : TTextDocumentSyncOptions absolute aSource;

begin
  if  aSource is TTextDocumentSyncOptions then
    begin
    OpenClose:=Src.openClose;
    Change:=Src.Change;
    WillSave:=Src.WillSave;
    WillSaveWaitUntil:=Src.WillSaveWaitUntil;
    Save:=Src.Save;
    end
  else
    inherited Assign(aSource);
end;

{ TSignatureHelpOptions }

procedure TSignatureHelpOptions.SetTriggerCharacters(AValue: TStrings);
begin
  if fTriggerCharacters=AValue then Exit;
  fTriggerCharacters.Assign(AValue);
end;

constructor TSignatureHelpOptions.Create;
begin
  fTriggerCharacters:=TStringList.Create;
end;

destructor TSignatureHelpOptions.Destroy;
begin
  FreeAndNil(fTriggerCharacters);
  inherited Destroy;
end;

procedure TSignatureHelpOptions.Assign(Source: TPersistent);

var
  Src : TSignatureHelpOptions absolute Source;

begin
  if Source is TSignatureHelpOptions then
    begin
    triggerCharacters:=Src.triggerCharacters;
    end
  else
    inherited Assign(Source);
end;

{ TCompletionOptions }

constructor TCompletionOptions.Create;
begin
  resolveProvider := False;
  fTriggerCharacters:=TStringList.Create;
  fAllCommitCharacters:=TStringList.Create;
end;

destructor TCompletionOptions.destroy;
begin
  FreeAndNil(fTriggerCharacters);
  FreeAndNil(fAllCommitCharacters);
  inherited destroy;
end;

procedure TCompletionOptions.Assign(Source: TPersistent);

var
  Src : TCompletionOptions absolute source;

begin
  if Source is TCompletionOptions then
    begin
    resolveProvider:=Src.resolveProvider;
    TriggerCharacters:=Src.triggerCharacters;
    AllCommitCharacters:=Src.allCommitCharacters;
    end
  else
    inherited Assign(Source);
end;

{ TWorkDoneProgressOptions }

procedure TWorkDoneProgressOptions.Assign(Source: TPersistent);

Var
  Src : TWorkDoneProgressOptions absolute source;

begin
  if Source is TWorkDoneProgressOptions then
    begin
    workDoneProgress:=Src.workDoneProgress;
    end
  else
    inherited Assign(Source);
end;

end.

