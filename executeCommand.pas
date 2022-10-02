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

unit executeCommand;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, Classes, FPJSON,
  lsp, basic, workDoneProgress;


type
  // TODO: this can't support objects and arrays so we need a new type
  TLSPAny = String; { LSPObject | LSPArray | string | integer | uinteger | decimal | boolean | null }

type
  { TExecuteCommandParams
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#executeCommandParams

    The arguments are typically specified when a command is returned from the server to the client.
    Example requests that return a command are `textDocument/codeAction` or `textDocument/codeLens`. }

  TExecuteCommandParams = class(TWorkDoneProgressParams)
    private
      fCommand: String;
      fArguments: TStrings;
    published
      // The identifier of the actual command handler.
      property command: String read fCommand write fCommand;
      // Arguments that the command should be invoked with.
      property arguments: TStrings read fArguments write fArguments;
    public
      procedure AfterConstruction; override;
      destructor Destroy; override;
  end;

  { TExecuteCommandRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_executeCommand

    The `workspace/executeCommand` request is sent from the client to the server to trigger 
    command execution on the server. In most cases the server creates a `WorkspaceEdit` structure 
    and applies the changes to the workspace using the request `workspace/applyEdit` which is sent 
    from the server to the client.

    Response:

      result: LSPAny | null
      error: code and message set in case an exception happens during the request.
  }

  TExecuteCommandRequest = class(specialize TLSPRequest<TExecuteCommandParams, TPersistent>)
    function Process(var Params: TExecuteCommandParams): TPersistent; override;
  end;

implementation
uses
  workspace;

procedure TExecuteCommandParams.AfterConstruction;
begin
  inherited;

  arguments := TStringList.Create;
end;

destructor TExecuteCommandParams.Destroy;
begin
  arguments.Free;

  inherited;
end;

function TExecuteCommandRequest.Process(var Params: TExecuteCommandParams): TPersistent;
begin with Params do
  begin
    // TODO: we need a real command dispatch and commands.pas to hold them
    case command of
      'pasls.do_stuff': 
        ;//ApplyEdit(arguments[0]);
    end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('workspace/executeCommand', TExecuteCommandRequest);
end.