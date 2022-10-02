// Pascal Language Server
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

unit codeAction;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, URIParser, 
  lsp, basic;

type
  
  { TCodeActionKind }

  { The kind of a code action.
    Kinds are a hierarchical list of identifiers separated by `.`, e.g. `"refactor.extract.function"`.
    The set of kinds is open and client needs to announce the kinds it supports to the server during
    initialization. }

  TCodeActionKind = record
  public const
    // Empty kind.
    Empty = '';
    // Base kind for quickfix actions: 'quickfix'.
    QuickFix = 'quickfix';
    // Base kind for refactoring actions: 'refactor'.
    Refactor = 'refactor';
    // Base kind for refactoring extraction actions: 'refactor.extract'.
    RefactorExtract = 'refactor.extract';
    // Base kind for refactoring inline actions: 'refactor.inline'.
    RefactorInline = 'refactor.inline';
    // Base kind for refactoring rewrite actions: 'refactor.rewrite'.
    RefactorRewrite = 'refactor.rewrite';
    // Base kind for source actions: `source`.
    // Source code actions apply to the entire file.
    Source = 'source';
    // Base kind for an organize imports source action: `source.organizeImports`.
    SourceOrganizeImports = 'source.organizeImports';
  private
    value: string;
  end;

  { TCodeAction }

  TCodeAction = class(TCollectionItem)
  private
    fTitle: string;
    fKind: TCodeActionKind;
    fDiagnostics: TDiagnosticItems;
    fIsPreferred: boolean;
    fEdit: TWorkspaceEdit;
    fCommand: TCommand;
  published
    // A short, human-readable, title for this code action.
    property title: string read fTitle write fTitle;
    // The kind of the code action.
    // Used to filter code actions.
    property kind: string read fKind.value write fKind.value;
    // The diagnostics that this code action resolves.
    property diagnostics: TDiagnosticItems read fDiagnostics write fDiagnostics;
    // Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
    // by keybindings.
    // 
    // A quick fix should be marked preferred if it properly addresses the underlying error.
    // A refactoring should be marked preferred if it is the most reasonable choice of actions to take.
    // 
    // @since 3.15.0
    property isPreferred: boolean read fIsPreferred write fIsPreferred;
    // The workspace edit this code action performs.
    property edit: TWorkspaceEdit read fEdit write fEdit;
    // A command this code action executes. If a code action
    // provides an edit and a command, first the edit is
    // executed and then the command.
    property command: TCommand read fCommand write fCommand;
  end;

  TCodeActionItems = specialize TGenericCollection<TCodeAction>;

  { TCodeActionContext }

  { Contains additional diagnostic information about the context in which
    a code action is run. }

  TCodeActionContext = class(TPersistent)
  private
    fDiagnostics: TDiagnosticItems;
    fOnly: TStrings;
  published
    // An array of diagnostics known on the client side overlapping the range provided to the
    // `textDocument/codeAction` request. They are provided so that the server knows which
    // errors are currently presented to the user for the given range. There is no guarantee
    // that these accurately reflect the error state of the resource. The primary parameter
    // to compute code actions is the provided range.
    property diagnostics: TDiagnosticItems read fDiagnostics write fDiagnostics;

    // (OPTIONAL) Requested kind of actions to return.
    // Actions not of this kind are filtered out by the client before being shown. So servers
    // can omit computing them.
    property only: TStrings read fOnly write fOnly;
  end;

  { TCodeActionParams }

  TCodeActionParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentIdentifier;
    fRange: TRange;
    fContext: TCodeActionContext;
  published
    // The document in which the command was invoked.
    property textDocument: TTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The range for which the command was invoked.
    property range: TRange read fRange write fRange;
    // Context carrying additional information.
    property context: TCodeActionContext read fContext write fContext;
  end;

  { TCodeActionRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction

    The code action request is sent from the client to the server to compute commands for a 
    given text document and range. These commands are typically code fixes to either fix 
    problems or to beautify/refactor code. The result of a textDocument/codeAction request 
    is an array of Command literals which are typically presented in the user interface. 
    To ensure that a server is useful in many clients the commands specified in a code actions 
    should be handled by the server and not by the client (see workspace/executeCommand and 
    ServerCapabilities.executeCommandProvider). If the client supports providing edits with a 
    code action then that mode should be used. }
  
  TCodeActionRequest = class(specialize TLSPRequest<TCodeActionParams, TCodeActionItems>)
    function Process(var Params: TCodeActionParams): TCodeActionItems; override;
  end;

implementation

function TCodeActionRequest.Process(var Params: TCodeActionParams): TCodeActionItems;
begin with Params do
  begin
    Result := nil;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/codeAction', TCodeActionRequest);
end.

