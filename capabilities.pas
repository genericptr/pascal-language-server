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

unit capabilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, options, documentSymbol, settings;

type

  { TWorkspaceClientCapabilities }

  TWorkspaceClientCapabilities = class(TPersistent)
  private
    fApplyEdit: Boolean;
    fWorkspaceFolders: boolean;
    fConfiguration: boolean;
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

  TWorkspaceFoldersServerCapabilities = class(TPersistent)
  private
    fSupported: boolean;
    fChangeNotifications: boolean;
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

  TWorkspaceServerCapabilities = class(TPersistent)
  private
    fWorkspaceFolders: TWorkspaceFoldersServerCapabilities;
  published
    // The server supports workspace folder.
    property workspaceFolders: TWorkspaceFoldersServerCapabilities read fWorkspaceFolders write fWorkspaceFolders;
  public
    constructor Create;
  end;

  { TTextDocumentClientCapabilities }

  TTextDocumentClientCapabilities = class(TPersistent)
  private
  published
  end;

  { TClientCapabilities }

  TClientCapabilities = class(TPersistent)
  private
    fWorkspace: TWorkspaceClientCapabilities;
    fTextDocument: TTextDocumentClientCapabilities;
  published
    property workspace: TWorkspaceClientCapabilities read fWorkspace write fWorkspace;
    property textDocument: TTextDocumentClientCapabilities read fTextDocument write fTextDocument;
  end;

  { TServerCapabilities }

  TServerCapabilities = class(TPersistent)
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
  public
    constructor Create(settings: TServerSettings);
  published
    property textDocumentSync: TTextDocumentSyncOptions read fTextDocumentSync write fTextDocumentSync;
    property workspace: TWorkspaceServerCapabilities read fWorkspace write fWorkspace;
    property completionProvider: TCompletionOptions read fCompletionProvider write fCompletionProvider;
    property hoverProvider: boolean read fHoverProvider write fHoverProvider;
    property definitionProvider: boolean read fDefinitionProvider write fDefinitionProvider;
    property declarationProvider: boolean read fDeclarationProvider write fDeclarationProvider;
    property referencesProvider: boolean read fReferencesProvider write fReferencesProvider;
    property implementationProvider: boolean read fImplementationProvider write fImplementationProvider;
    property codeActionProvider: boolean read fCodeActionProvider write fCodeActionProvider;
    property documentHighlightProvider: boolean read fDocumentHighlightProvider write fDocumentHighlightProvider;
    property documentSymbolProvider: boolean read fDocumentSymbolProvider write fDocumentSymbolProvider;
    property workspaceSymbolProvider: boolean read fWorkspaceSymbolProvider write fWorkspaceSymbolProvider;
    property signatureHelpProvider: TSignatureHelpOptions read fSignatureHelpProvider write fSignatureHelpProvider;
  end;

implementation

{ TWorkspaceServerCapabilities }

constructor TWorkspaceServerCapabilities.Create;
begin
  workspaceFolders := TWorkspaceFoldersServerCapabilities.Create;
end;

{ TServerCapabilities }

constructor TServerCapabilities.Create(settings: TServerSettings);
var
  triggerCharacters: TStringList;
begin
  textDocumentSync := TTextDocumentSyncOptions.Create;

  textDocumentSync.save := TSaveOptions.Create;
  textDocumentSync.save.includeText := false;

  workspace := TWorkspaceServerCapabilities.Create;
  workspace.workspaceFolders.supported := true;
  workspace.workspaceFolders.changeNotifications := true;

  hoverProvider := true;
  declarationProvider := true;
  implementationProvider := true;
  referencesProvider := true;
  documentHighlightProvider := true;

  documentSymbolProvider := Assigned(SymbolManager);

  // note(ryan): workspace symbols are so broken in the protocol I'm 
  // going to ignore them for now until something changes
  workspaceSymbolProvider := false;//settings.CanProvideWorkspaceSymbols;
  
  completionProvider := TCompletionOptions.Create;
  triggerCharacters := TStringList.Create;
  triggerCharacters.Add('.');
  triggerCharacters.Add('^');
  completionProvider.triggerCharacters := triggerCharacters;

  signatureHelpProvider := TSignatureHelpOptions.Create;
  triggerCharacters := TStringList.Create;
  triggerCharacters.Add('(');
  triggerCharacters.Add(')');
  triggerCharacters.Add(',');
  signatureHelpProvider.triggerCharacters := triggerCharacters;
end;

end.

