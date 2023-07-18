unit PasLS.ExecuteCommand;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  SysUtils, Classes, FPJSON,
  { Protocol }
  LSP.BaseTypes, LSP.Base, LSP.Streaming, LSP.WorkDoneProgress, LSP.ExecuteCommand;

Type
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

  TExecuteCommandRequest = class(specialize TLSPRequest<TExecuteCommandParams, TLSPStreamable>)
    function Process(var Params: TExecuteCommandParams): TLSPStreamable; override;
  end;


implementation

uses
  PasLS.Commands;

function TExecuteCommandRequest.Process(var Params: TExecuteCommandParams): TLSPStreamable;
var
  aCommandClass : TCustomCommandClass;
  aCommand : TCustomCommand;

begin
  result := nil;
  aCommandClass:=CommandFactory.FindCommandClass(Params.command);
  if aCommandClass<>Nil then
    try
      aCommand:=aCommandClass.Create(Self.Transport);
      result:=aCommand.Execute(Params.Arguments);
    finally
      aCommand.Free;
    end;
   { case command of
      'pasls.formatCode':
        begin
        end;
      'pasls.invertAssignment':
        begin
        end;
    end;}

end;

initialization


end.

