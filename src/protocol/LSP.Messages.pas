unit LSP.Messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LSP.Streaming, fpJSON, LSP.BaseTypes;

Type

  { TAbstractMessage
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#abstractMessage

    A general message as defined by JSON-RPC. The language server
    protocol always uses “2.0” as the jsonrpc version. }

  TAbstractMessage = class(TPersistent)
  private
    function GetJSONRPC: String;
  published
    property jsonrpc: String read GetJSONRPC;
  public
    procedure Send;
  end;

  { TRequestMessage
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage

    A request message to describe a request between the client and the server.
    Every processed request must send a response back to the sender of the request. }

  TRequestMessage = class(TAbstractMessage)
  protected
    fID: TOptionalAny; // integer | string
    fMethod: string;
    fParams: TPersistent;
  published
    // The request id.
    property id: TOptionalAny read fID write fID;
    // The method to be invoked.
    property method: string read fMethod write fMethod;
    // The notification's params.
    property params: TPersistent read fParams write fParams;
  end;

  { TNotificationMessage
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage

    A notification message. A processed notification message
    must not send a response back. They work like events. }

  TNotificationMessage = class(TAbstractMessage)
  protected
    fMethod: string;
    fParams: TPersistent;
  published
    // The method to be invoked.
    property method: string read fMethod write fMethod;
    // The notification's params.
    property params: TPersistent read fParams write fParams;
  end;

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';


implementation

{ TAbstractMessage }

function TAbstractMessage.GetJSONRPC: String;
begin
  result := '2.0';
end;

procedure TAbstractMessage.Send;
var
  Data: TJSONData;
  Content: String;
begin
  Data := specialize
  TLSPStreaming<TAbstractMessage>.ToJSON(self);
  if Data <> nil then
    begin
      Content := Data.AsJSON;

      WriteLn('Content-Type: ', ContentType);
      WriteLn('Content-Length: ', Content.Length);
      WriteLn;
      Write(Content);
      Flush(Output);

      Data.Free;
    end;
end;


end.

