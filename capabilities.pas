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
  Classes, options;

type

  { TWorkspaceClientCapabilities }

  TWorkspaceClientCapabilities = class(TPersistent)
  private
    fApplyEdit: Boolean;
  published
    property applyEdit: Boolean read fApplyEdit write fApplyEdit;
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
    fCompletionProvider: TCompletionOptions;
    fHoverProvider: boolean;
    fDefinitionProvider: boolean;
    fDeclarationProvider: boolean;
    fReferencesProvider: boolean;
    fImplementationProvider: boolean;
    fCodeActionProvider: boolean;
    fDocumentHighlightProvider: boolean;
    fSignatureHelpProvider: TSignatureHelpOptions;
  public
    constructor Create;
  published
    property textDocumentSync: TTextDocumentSyncOptions read fTextDocumentSync write fTextDocumentSync;
    property completionProvider: TCompletionOptions read fCompletionProvider write fCompletionProvider;
    property hoverProvider: boolean read fHoverProvider write fHoverProvider;
    property definitionProvider: boolean read fDefinitionProvider write fDefinitionProvider;
    property declarationProvider: boolean read fDeclarationProvider write fDeclarationProvider;
    property referencesProvider: boolean read fReferencesProvider write fReferencesProvider;
    property implementationProvider: boolean read fImplementationProvider write fImplementationProvider;
    property codeActionProvider: boolean read fCodeActionProvider write fCodeActionProvider;
    property documentHighlightProvider: boolean read fDocumentHighlightProvider write fDocumentHighlightProvider;
    property signatureHelpProvider: TSignatureHelpOptions read fSignatureHelpProvider write fSignatureHelpProvider;
  end;

implementation

{ TServerCapabilities }

constructor TServerCapabilities.Create;
var
  triggerCharacters: TStringList;
begin
  textDocumentSync := TTextDocumentSyncOptions.Create;

  hoverProvider := true;
  declarationProvider := true;
  implementationProvider := true;
  referencesProvider := true;
  documentHighlightProvider := true;
  
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

