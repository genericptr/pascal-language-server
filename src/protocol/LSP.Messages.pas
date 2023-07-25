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

unit LSP.Messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LSP.Streaming, fpJSON, LSP.BaseTypes;

Type
  // We cannot assume stdout to send out-of-band messages.

  { TMessageTransport }
  TMessageLog = procedure(sender : TObject; Const Msg : UTF8String) of object;

  TMessageTransport = class
  Protected
    Procedure DoSendMessage(aMessage : TJSONData); virtual; abstract;
    Procedure DoSendDiagnostic(const aMessage : UTF8String); virtual; abstract;
    Procedure DoLog(Const Msg : UTF8String); overload;
    Procedure DoLog(Const Fmt : UTF8String; Const args : array of const); overload;
  Public
    Class Var OnLog : TMessageLog;
  Public
    Procedure SendMessage(aMessage : TJSONData);
    Procedure SendDiagnostic(const aMessage : UTF8String);
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
    // The notification's params. Not freed when message is freed.
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

procedure TMessageTransport.DoLog(const Msg: UTF8String);
begin
  If Assigned(OnLog) then
    OnLog(Self,Msg);
end;

procedure TMessageTransport.DoLog(const Fmt: UTF8String;
  const args: array of const);
begin
  DoLog(Format(Fmt,Args));
end;

procedure TMessageTransport.SendMessage(aMessage: TJSONData);
begin
  DoLog('Sending message: %s',[aMessage.AsJSON]);
  DoSendMessage(aMessage);
end;

procedure TMessageTransport.SendDiagnostic(const aMessage: UTF8String);
begin
  DoLog('Sending diagnostic: %s',[aMessage]);
  DoSendDiagnostic(aMessage);
end;

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

