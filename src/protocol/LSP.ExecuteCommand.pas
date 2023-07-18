// Pascal Language Server
// Copyright 2022 Ryan Joseph

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

unit LSP.ExecuteCommand;

{$mode objfpc}{$H+}

interface
uses
  { RTL }
  SysUtils, Classes, FPJSON,
  { Protocol }
  LSP.Base, LSP.WorkDoneProgress;

type
  { TExecuteCommandParams
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#executeCommandParams

    The arguments are typically specified when a command is returned from the server to the client.
    Example requests that return a command are `textDocument/codeAction` or `textDocument/codeLens`. }

  TExecuteCommandParams = class(TWorkDoneProgressParams)
    private
      fCommand: String;
      fArguments: TJSONArray;
    published
      // The identifier of the actual command handler.
      property command: String read fCommand write fCommand;
      // Arguments that the command should be invoked with.
      property arguments: TJSONArray read fArguments write fArguments;
    public
      destructor Destroy; override;
  end;


implementation


destructor TExecuteCommandParams.Destroy;
begin
  FreeAndNil(fArguments);
  inherited;
end;

end.
