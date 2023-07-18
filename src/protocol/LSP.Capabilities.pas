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

unit LSP.Capabilities;

{$mode objfpc}{$H+}

interface
uses
  { RTL }
  SysUtils, Classes,
  { Protocol }
  LSP.BaseTypes, LSP.Options;

type

  { TWorkspaceClientCapabilities }

  TWorkspaceClientCapabilities = class(TLSPStreamable)
  private
    fApplyEdit: Boolean;
    fWorkspaceFolders: boolean;
    fConfiguration: boolean;
  Public
    Procedure Assign(Source : TPersistent); override;
  published
    // The client supports applying batch edits to the workspace by supporting
    // the request 'workspace/applyEdit'
    property applyEdit: Boolean read fApplyEdit write fApplyEdit;

    // The client has support for workspace folders.
    property workspaceFolders: Boolean read fWorkspaceFolders write fWorkspaceFolders;

    // The client supports `workspace/configuration` requests.
    property configuration: Boolean read fConfiguration write fConfiguration;
  end;

  { TWorkspaceFoldersServerCapabilities }

  TWorkspaceFoldersServerCapabilities = class(TLSPStreamable)
  private
    fSupported: boolean;
    fChangeNotifications: boolean;
  Public
    Procedure Assign(Source : TPersistent); override;
  published
    // The server has support for workspace folders
    property supported: Boolean read fSupported write fSupported;
    // Whether the server wants to receive workspace folder
    // change notifications.
    // 
    // If a string is provided, the string is treated as an ID
    // under which the notification is registered on the client
    // side. The ID can be used to unregister for these events
    // using the `client/unregisterCapability` request.
    property changeNotifications: boolean read fChangeNotifications write fChangeNotifications;
  end;

  { TWorkspaceServerCapabilities }

  TWorkspaceServerCapabilities = class(TLSPStreamable)
  private
    fWorkspaceFolders: TWorkspaceFoldersServerCapabilities;
    procedure SetWorkspaceFolders(AValue: TWorkspaceFoldersServerCapabilities);
  public
    constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // The server supports workspace folder.
    property workspaceFolders: TWorkspaceFoldersServerCapabilities read fWorkspaceFolders write SetWorkspaceFolders;
  end;

  { TTextDocumentClientCapabilities }

  TTextDocumentClientCapabilities = class(TLSPStreamable)
  end;

  { TClientCapabilities }

  TClientCapabilities = class(TLSPStreamable)
  private
    fWorkspace: TWorkspaceClientCapabilities;
    fTextDocument: TTextDocumentClientCapabilities;
    procedure SetTextDocument(AValue: TTextDocumentClientCapabilities);
    procedure SetWorkspace(AValue: TWorkspaceClientCapabilities);
  Public
    constructor Create; override;
    destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    property workspace: TWorkspaceClientCapabilities read fWorkspace write SetWorkspace;
    property textDocument: TTextDocumentClientCapabilities read fTextDocument write SetTextDocument;
  end;

  { TServerCapabilities }

  TServerCapabilities = class(TLSPStreamable)
  private
    fTextDocumentSync: TTextDocumentSyncOptions;
    fWorkspace: TWorkspaceServerCapabilities;
    fCompletionProvider: TCompletionOptions;
    fHoverProvider: boolean;
    fDefinitionProvider: boolean;
    fDeclarationProvider: boolean;
    fReferencesProvider: boolean;
    fImplementationProvider: boolean;
    fCodeActionProvider: boolean;
    fDocumentHighlightProvider: boolean;
    fDocumentSymbolProvider: boolean;
    fWorkspaceSymbolProvider: boolean;
    fSignatureHelpProvider: TSignatureHelpOptions;
    fExecuteCommandProvider: TExecuteCommandOptions;
    fInlayHintProvider: TInlayHintOptions;
    procedure SetCompletionProvider(AValue: TCompletionOptions);
    procedure SetExecuteCommandProvider(AValue: TExecuteCommandOptions);
    procedure SetInlayHintProvider(AValue: TInlayHintOptions);
    procedure SetSignatureHelpProvider(AValue: TSignatureHelpOptions);
    procedure SetTextDocumentSync(AValue: TTextDocumentSyncOptions);
    procedure SetWorkspace(AValue: TWorkspaceServerCapabilities);
  public
    constructor Create; override;
    destructor destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    property textDocumentSync: TTextDocumentSyncOptions read fTextDocumentSync write SetTextDocumentSync;
    property workspace: TWorkspaceServerCapabilities read fWorkspace write SetWorkspace;
    property completionProvider: TCompletionOptions read fCompletionProvider write SetCompletionProvider;
    property hoverProvider: boolean read fHoverProvider write fHoverProvider;
    property definitionProvider: boolean read fDefinitionProvider write fDefinitionProvider;
    property declarationProvider: boolean read fDeclarationProvider write fDeclarationProvider;
    property referencesProvider: boolean read fReferencesProvider write fReferencesProvider;
    property implementationProvider: boolean read fImplementationProvider write fImplementationProvider;
    property codeActionProvider: boolean read fCodeActionProvider write fCodeActionProvider;
    property documentHighlightProvider: boolean read fDocumentHighlightProvider write fDocumentHighlightProvider;
    property documentSymbolProvider: boolean read fDocumentSymbolProvider write fDocumentSymbolProvider;
    property workspaceSymbolProvider: boolean read fWorkspaceSymbolProvider write fWorkspaceSymbolProvider;
    property signatureHelpProvider: TSignatureHelpOptions read fSignatureHelpProvider write SetSignatureHelpProvider;
    property executeCommandProvider: TExecuteCommandOptions read fExecuteCommandProvider write SetExecuteCommandProvider;
    property inlayHintProvider: TInlayHintOptions read fInlayHintProvider write SetInlayHintProvider;
  end;

implementation

{ TWorkspaceClientCapabilities }

procedure TWorkspaceClientCapabilities.Assign(Source : TPersistent);

var
  Src : TWorkspaceClientCapabilities absolute Source;

begin
  if Source is TWorkspaceClientCapabilities then
    begin
    ApplyEdit:=Src.ApplyEdit;
    WorkspaceFolders:=Src.workspaceFolders;
    Configuration:=Src.configuration;
    end
  else
    inherited Assign(Source);
end;

{ TWorkspaceFoldersServerCapabilities }

procedure TWorkspaceFoldersServerCapabilities.Assign(Source : TPersistent);

var
  Src : TWorkspaceFoldersServerCapabilities absolute source;

begin
  if Source is TWorkspaceFoldersServerCapabilities then
    begin
    Supported:=Src.supported;
    ChangeNotifications:=Src.changeNotifications;
    end
  else
    inherited Assign(Source);
end;

{ TWorkspaceServerCapabilities }

procedure TWorkspaceServerCapabilities.SetWorkspaceFolders(
  AValue: TWorkspaceFoldersServerCapabilities);
begin
  if fWorkspaceFolders=AValue then Exit;
  fWorkspaceFolders.Assign(AValue);
end;

constructor TWorkspaceServerCapabilities.Create;
begin
  Inherited;
  FWorkspaceFolders := TWorkspaceFoldersServerCapabilities.Create;
end;

destructor TWorkspaceServerCapabilities.Destroy;
begin
  FreeAndNil(FWorkspaceFolders);
  inherited Destroy;
end;

procedure TWorkspaceServerCapabilities.Assign(Source : TPersistent);

var
  Src : TWorkspaceServerCapabilities absolute Source;

begin
  If Source is TWorkspaceServerCapabilities then
    begin
    workspaceFolders:=Src.workspaceFolders;
    end
  else
    inherited Assign(Source);
end;

{ TClientCapabilities }

procedure TClientCapabilities.SetTextDocument(
  AValue: TTextDocumentClientCapabilities);
begin
  if fTextDocument=AValue then Exit;
  fTextDocument.Assign(AValue);
end;

procedure TClientCapabilities.SetWorkspace(AValue: TWorkspaceClientCapabilities
  );
begin
  if fWorkspace=AValue then Exit;
  fWorkspace.Assign(AValue);
end;

constructor TClientCapabilities.Create;
begin
  Inherited;
  fWorkspace:=TWorkspaceClientCapabilities.Create;
  fTextDocument:=TTextDocumentClientCapabilities.Create;
end;

destructor TClientCapabilities.destroy;
begin
  FreeAndNil(fWorkspace);
  FreeAndNil(fTextDocument);
  inherited destroy;
end;

procedure TClientCapabilities.Assign(Source : TPersistent);

Var
  CP : TClientCapabilities absolute source;

begin
  if Source is TClientCapabilities then
    begin
    Workspace:=cp.Workspace;
    TextDocument:=cp.textDocument;
    end
  else
    inherited Assign(Source);
end;

{ TServerCapabilities }

procedure TServerCapabilities.SetCompletionProvider(AValue: TCompletionOptions);
begin
  if fCompletionProvider=AValue then Exit;
  fCompletionProvider.Assign(AValue);
end;

procedure TServerCapabilities.SetExecuteCommandProvider(
  AValue: TExecuteCommandOptions);
begin
  if fExecuteCommandProvider=AValue then Exit;
  fExecuteCommandProvider.Assign(AValue);
end;

procedure TServerCapabilities.SetInlayHintProvider(AValue: TInlayHintOptions);
begin
  if fInlayHintProvider=AValue then Exit;
  // May not have been created during constructor, so we need to create it here.
  if fInlayHintProvider=Nil then
    fInlayHintProvider:=TInlayHintOptions.Create;
  fInlayHintProvider.Assign(AValue);
end;

procedure TServerCapabilities.SetSignatureHelpProvider(
  AValue: TSignatureHelpOptions);
begin
  if fSignatureHelpProvider=AValue then Exit;
  fSignatureHelpProvider.Assign(AValue);
end;

procedure TServerCapabilities.SetTextDocumentSync(
  AValue: TTextDocumentSyncOptions);
begin
  if fTextDocumentSync=AValue then Exit;
  fTextDocumentSync.Assign(AValue);
end;

procedure TServerCapabilities.SetWorkspace(AValue: TWorkspaceServerCapabilities
  );
begin
  if fWorkspace=AValue then Exit;
  fWorkspace.Assign(AValue);
end;

constructor TServerCapabilities.Create;
begin
  inherited Create;
  ftextDocumentSync := TTextDocumentSyncOptions.Create;
  fworkspace := TWorkspaceServerCapabilities.Create;
  fcompletionProvider := TCompletionOptions.Create;
  fsignatureHelpProvider := TSignatureHelpOptions.Create;
  fexecuteCommandProvider := TExecuteCommandOptions.Create;
  ftextDocumentSync.change := TTextDocumentSyncKind.Full;
end;

destructor TServerCapabilities.destroy;

begin
  FreeAndNil(fTextDocumentSync);
  FreeAndNil(fWorkspace);
  FreeAndNil(fCompletionProvider);
  FreeAndNil(fSignatureHelpProvider);
  FreeAndNil(fExecuteCommandProvider);
  FreeAndNil(fInlayHintProvider);
  inherited destroy;
end;


procedure TServerCapabilities.Assign(Source : TPersistent);

var
  Src : TServerCapabilities absolute source;

begin
  if Source is TServerCapabilities then
    begin
    TextDocumentSync:=TextDocumentSync;
    Workspace:=Src.Workspace;
    CompletionProvider:=Src.CompletionProvider;
    HoverProvider:= Src.HoverProvider;
    DefinitionProvider:=Src.DefinitionProvider;
    DeclarationProvider:=Src.DeclarationProvider;
    ReferencesProvider:=Src.ReferencesProvider;
    ImplementationProvider:=Src.ImplementationProvider;
    CodeActionProvider:=Src.CodeActionProvider;
    DocumentHighlightProvider:=Src.DocumentHighlightProvider;
    DocumentSymbolProvider:=Src.DocumentSymbolProvider;
    WorkspaceSymbolProvider:=Src.WorkspaceSymbolProvider;
    SignatureHelpProvider:=Src.SignatureHelpProvider;
    ExecuteCommandProvider:=Src.ExecuteCommandProvider;
    InlayHintProvider:=Src.inlayHintProvider;
    end
  else
    inherited Assign(Source);
end;

end.

