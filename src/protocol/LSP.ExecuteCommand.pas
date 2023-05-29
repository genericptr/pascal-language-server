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
  LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Streaming, LSP.WorkDoneProgress;

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
  PasLS.Commands,
  PasLS.RemoveEmptyMethods;

destructor TExecuteCommandParams.Destroy;
begin
  FreeAndNil(fArguments);

  inherited;
end;

function TExecuteCommandRequest.Process(var Params: TExecuteCommandParams): TLSPStreamable;
var
  documentURI,configURI: TDocumentUri;
  spos,epos,position: TPosition;
  Range : TRange;
  rem : TRemoveEmptyMethods;

begin with Params do
  begin
    result := nil;

    case command of
      'pasls.completeCode': 
        begin
          documentURI := arguments.Strings[0];
          position := specialize TLSPStreaming<TPosition>.ToObject(arguments.Objects[1].AsJSON);
          try
            CompleteCode(Transport,documentURI, position.line, position.character);
          finally
            Position.Free;
          end;
        end;
      'pasls.formatCode':
        begin
          documentURI := arguments.Strings[0];
          configURI := arguments.Strings[1];
          PrettyPrint(Transport,documentURI,ConfigURI);
        end;
      'pasls.invertAssignment':
        begin
          documentURI := arguments.Strings[0];
          Range:=Nil;
          ePos:=Nil;
          sPos:=specialize TLSPStreaming<TPosition>.ToObject(arguments.Objects[1].AsJSON);
          try
            ePos:=specialize TLSPStreaming<TPosition>.ToObject(arguments.Objects[2].AsJSON);
            Range:=TRange.Create;
            Range.Start:=sPos;
            Range.&End:=ePos;
            InvertAssignment(Transport,documentURI,Range);
          finally
            sPos.Free;
            ePos.Free;
            Range.Free;
          end;
        end;
      'pasls.removeEmptyMethods':
        begin
          documentURI := arguments.Strings[0];
          rem:=nil;
          position := specialize TLSPStreaming<TPosition>.ToObject(arguments.Objects[1].AsJSON);
          try
            rem:=TRemoveEmptyMethods.Create(Transport);
            Rem.Execute(documentURI,position);
          finally
            Position.Free;
            Rem.Free;
          end;
        end;
    end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('workspace/executeCommand', TExecuteCommandRequest);
end.
