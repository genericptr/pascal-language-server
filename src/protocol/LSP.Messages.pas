unit LSP.Messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LSP.Streaming, fpJSON, LSP.BaseTypes;

Type
  // We cannot assume stdout to send out-of-band messages.

  { TMessageTransport }

  TMessageTransport = class
    Procedure SendMessage(aMessage : TJSONData); virtual; abstract;
    Procedure SendDiagnostic(const aMessage : UTF8String); virtual; abstract;
    Procedure SendDiagnostic(const Fmt : String; const args : Array of const); overload;
  end;

  { TAbstractMessage
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#abstractMessage

    A general message as defined by JSON-RPC. The language server
    protocol always uses “2.0” as the jsonrpc version. }

  TAbstractMessage = class(TLSPStreamable)
  private
    function GetJSONRPC: String;
  published
    property jsonrpc: String read GetJSONRPC;
  public
    procedure Send(aTransport : TMessageTransport);
  end;

  { TRequestMessage
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage

    A request message to describe a request between the client and the server.
    Every processed request must send a response back to the sender of the request. }

  TRequestMessage = class(TAbstractMessage)
  protected
    fID: TOptionalAny; // integer | string
    fMethod: string;
    fParams: TLSPStreamable;
  published
    // The request id.
    property id: TOptionalAny read fID write fID;
    // The method to be invoked.
    property method: string read fMethod write fMethod;
    // The notification's params.
    property params: TLSPStreamable read fParams write fParams;
  end;

  { TNotificationMessage
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage

    A notification message. A processed notification message
    must not send a response back. They work like events. }

  TNotificationMessage = class(TAbstractMessage)
  protected
    fMethod: string;
    fParams: TLSPStreamable;
  published
    // The method to be invoked.
    property method: string read fMethod write fMethod;
    // The notification's params.
    property params: TLSPStreamable read fParams write fParams;
  end;



const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';


implementation

{ TMessageTransport }

procedure TMessageTransport.SendDiagnostic(const Fmt: String;
  const args: array of const);
begin
  SendDiagnostic(Format(Fmt,Args));
end;

{ TAbstractMessage }

function TAbstractMessage.GetJSONRPC: String;
begin
  result := '2.0';
end;

procedure TAbstractMessage.Send(aTransport : TMessageTransport);
var
  Data: TJSONData;

begin
  Data := specialize
  TLSPStreaming<TAbstractMessage>.ToJSON(self);
  if Data <> nil then
    begin
      aTransport.SendMessage(Data);
      Data.Free;
    end;
end;


end.

