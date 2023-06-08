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

unit LSP.AllCommands;

{$mode objfpc}{$H+}

interface

uses
  LSP.GotoDeclaration,
  LSP.GotoDefinition,
  LSP.Completion,
  LSP.GotoImplementation,
  LSP.Hover,
  LSP.SignatureHelp,
  LSP.References,
  LSP.CodeAction,
  LSP.DocumentHighlight,
  LSP.DocumentSymbol,
  LSP.Workspace,
  LSP.ExecuteCommand,
  LSP.InlayHint,
  LSP.General,
  LSP.Synchronization,
  // Custom commands
  PasLS.Command.FormatCode,
  PasLS.Command.CompleteCode,
  PasLS.Command.InvertAssignment,
  PasLS.Command.RemoveEmptyMethods
  ;

implementation

end.

