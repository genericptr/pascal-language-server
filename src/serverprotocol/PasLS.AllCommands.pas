// Pascal Language Server - Include all command units in a single place, so all commands are available by using this unit.
// Copyright 2023 Michael Van Canneyt

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

unit PasLS.AllCommands;

{$mode objfpc}{$H+}

interface

Procedure RegisterAllCommands;

Implementation

uses
  LSP.Base,
  // General init etc.
  PasLS.General,
  // Document
  PasLS.Hover,
  PasLS.GotoDeclaration,
  PasLS.GotoDefinition,
  PasLS.Completion,
  PasLS.GotoImplementation,
  PasLS.SignatureHelp,
  PasLS.References,
  PasLS.Synchronization,
  PasLS.CodeAction,
  PasLS.DocumentHighlight,
  PasLS.DocumentSymbol,
  PasLS.InlayHint,
  // Workspace
  PasLS.Workspace,
  // Custom commands
  PasLS.ExecuteCommand,
  PasLS.Command.FormatCode,
  PasLS.Command.CompleteCode,
  PasLS.Command.InvertAssignment,
  PasLS.Command.RemoveEmptyMethods;

procedure RegisterAllCommands;

begin
  // General
  LSPHandlerManager.RegisterHandler('initialize', TInitialize);
  LSPHandlerManager.RegisterHandler('initialized', TInitialized);
  LSPHandlerManager.RegisterHandler('shutdown', TShutdown);
  LSPHandlerManager.RegisterHandler('exit', TExit);
  LSPHandlerManager.RegisterHandler('$/cancelRequest', TCancel);
  // textDocument
  LSPHandlerManager.RegisterHandler('textDocument/declaration', TGotoDeclaraction);
  LSPHandlerManager.RegisterHandler('textDocument/definition', TGotoDefinition);
  LSPHandlerManager.RegisterHandler('textDocument/completion', TCompletion);
  LSPHandlerManager.RegisterHandler('textDocument/implementation', TGotoImplementation);
  LSPHandlerManager.RegisterHandler('textDocument/references', TReferencesRequest);
  LSPHandlerManager.RegisterHandler('textDocument/signatureHelp', TSignatureHelpRequest);
  LSPHandlerManager.RegisterHandler('textDocument/didOpen', TDidOpenTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didClose', TDidCloseTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didChange', TDidChangeTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didSave', TDidSaveTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/codeAction', TCodeActionRequest);
  LSPHandlerManager.RegisterHandler('textDocument/documentHighlight', TDocumentHighlightRequest);
  LSPHandlerManager.RegisterHandler('textDocument/hover', THoverRequest);
  LSPHandlerManager.RegisterHandler('textDocument/inlayHint', TInlayHintRequest);
  LSPHandlerManager.RegisterHandler('textDocument/documentSymbol', TDocumentSymbolRequest);
  // WorkSpace
  LSPHandlerManager.RegisterHandler('workspace/didChangeConfiguration', TDidChangeConfiguration);
  LSPHandlerManager.RegisterHandler('workspace/didChangeWorkspaceFolders', TDidChangeWorkspaceFolders);
  LSPHandlerManager.RegisterHandler('workspace/symbol', TWorkspaceSymbolRequest);
  LSPHandlerManager.RegisterHandler('workspace/executeCommand', TExecuteCommandRequest);
end;

end.

