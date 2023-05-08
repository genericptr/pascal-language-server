// Pascal Language Server
// Copyright 2023 Michael Van Canneyt

// Socket-based protocol, used between LSP proxy and socket server - based LSP.

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

unit PasLS.SocketDispatcher;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, fpjson, ssockets,
  LSP.Base, LSP.Messages;


{
  The Protocol is simple message exchange:
  Client sends message of certain type (see below)
  Server answsers with message, with same ID.
}

Const
  LSPProtocolVersion = 1;

Type
  TLSPProtocolMessageType = (lpmtRequest,lpmtResponse, lptmMessage, lptmDiagnostic);

  { TLSPProtocolMessageTypeHelper }

  TLSPProtocolMessageTypeHelper = Type Helper for TLSPProtocolMessageType
  private
    function GetAsString: String;
  Public
    Property AsString : String Read GetAsString;
  end;

  { TLSPFrame }

  TLSPFrame = Packed Record
  private
    function GetMessageType: TLSPProtocolMessageType;
    function GetPayloadString: UTF8String;
    procedure SetMessageType(AValue: TLSPProtocolMessageType);
  public
    Version : Byte;
    MsgType : Byte; // Sent as byte
    ID : cardinal; // In network order;
    PayloadLen : cardinal; // In network order
    PayLoad : TBytes;
    Property MessageType : TLSPProtocolMessageType Read GetMessageType Write SetMessageType;
    Property PayloadString : UTF8String Read GetPayloadString;
  end;

  ELSPSocket = Class(Exception);

  { TSocketDispatcher }

  { TLSPSocketDispatcher }

  { TLSPSocketTransport }

  THandleFrameEvent = Procedure(Sender : TObject; const aFrame : TLSPFrame) of object;
  TLSPSocketTransport = class(TMessageTransport)
  private
    FOnHandleFrame: THandleFrameEvent;
    FSocket: TSocketStream;
    FSocketClosed: Boolean;
    FID : Integer;
  Protected
    Procedure DoHandleFrame(const aFrame : TLSPFrame); virtual;
  Public
    Constructor Create(aSocket: TSocketStream);
    Destructor Destroy; override;
    function NextID : Cardinal;
    // TMessageTRansport methods
    Procedure DoSendMessage(aMessage: TJSONData); override;
    Procedure DoSendDiagnostic(const aMessage : UTF8String); override;
    function ReadFrame(Out Msg: TLSPFrame) : Boolean;
    function SendFrame(const Msg: TLSPFrame) : Boolean;
    // Read frames till a frame with type aType is read.
    function ReceiveJSON(aType: TLSPProtocolMessageType): TJSONData;
    function SendJSON(aType: TLSPProtocolMessageType; aJSON : TJSONData) : Boolean;
    // Socket is owned by this transport instance.
    Property Socket : TSocketStream Read FSocket;
    // True when last read indicated socket is closed.
    Property SocketClosed : Boolean Read FSocketClosed;
    // Handle unexpected frames
    Property OnHandleFrame : THandleFrameEvent Read FOnHandleFrame Write FOnHandleFrame;
  end;

  TLSPSocketDispatcher = Class(TLSPBaseDispatcher)
  private
    FTransport : TLSPSocketTransport;
    function GetSocket: TSocketStream;
  Protected
    function GetTransport: TMessageTransport; override;
    function HandleException(aException: Exception; IsReceive : Boolean): Boolean; virtual;
    // Owned by this instance
    Property SocketTransport: TLSPSocketTransport Read FTransport;
  public
    // Transport is owned by this instance, will be freed when this is freed.
    Constructor Create(aTransport : TLSPSocketTransport); reintroduce;
    Destructor Destroy; override;
    Property Socket : TSocketStream Read GetSocket;

  end;

  TLSPClientSocketDispatcher = Class(TLSPSocketDispatcher)
  Public
    function ExecuteRequest(aRequest: TJSONData): TJSONData; override;
  end;


  { TLSPServerSocketConnectionDispatcher }

  TLSPServerSocketConnectionDispatcher = Class(TLSPSocketDispatcher)
  Private
    FOnDestroy: TNotifyEvent;
    FTerminated : Boolean;
    FContext : TLSPContext;
  Protected
    procedure DoMethodResult(Sender: TObject; aResponse: TObject;
      const aID: String; aResult: TJSONData);
    procedure DoMethodError(Sender: TObject; aResponse: TObject;
      const aID: String; aError: TJSONData);
  Public
    function ExecuteRequest(aRequest: TJSONData): TJSONData; override;
    Constructor Create(aTransport : TLSPSocketTransport); reintroduce;
    Destructor Destroy; override;
    Procedure RunLoop; virtual;
    Procedure Terminate; virtual;
    Property Terminated : Boolean Read FTerminated;
    Property OnDestroy: TNotifyEvent Read FOnDestroy Write FOnDestroy;
  end;

  { TLSPServerSocketDispatcher }

  TThreadMode = (tmNone,tmThreadPerConnection);
  TLSPServerSocketDispatcher = Class
  Private
    FSingleConnect: Boolean;
    FSocket: TSocketServer;
    FThreadMode: TThreadMode;
    FConns : TFPList;
  Protected
    procedure TerminateConnections; virtual;
    procedure RemoveConn(Sender: TObject); virtual;
    procedure AddConnection(aConn : TLSPServerSocketConnectionDispatcher); virtual;
    procedure HandleConnection(Sender: TObject; Data: TSocketStream); virtual;
    function CreateDispatcher(Data: TSocketStream): TLSPServerSocketConnectionDispatcher; virtual;
    Procedure SetServer(aSocket : TSocketServer);
    Property Connections : TFPList Read FConns;
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure InitSocket; virtual; abstract;
    Procedure RunLoop;
    Procedure Terminate;
    Property Socket : TSocketServer Read FSocket;
    Property ThreadMode : TThreadMode Read FThreadMode Write FThreadMode;
    Property SingleConnect : Boolean Read FSingleConnect Write FSingleConnect;
  end;

{$IFDEF UNIX}
  { TLSPServerUnixSocketDispatcher }

  TLSPServerUnixSocketDispatcher = Class (TLSPServerSocketDispatcher)
  private
    FPath: String;
  Public
    Constructor Create(aPath : String); reintroduce;
    Procedure InitSocket; override;
    Property Path : String Read FPath;
  end;
{$ENDIF}

  { TLSPServerTCPSocketDispatcher }

  TLSPServerTCPSocketDispatcher = Class (TLSPServerSocketDispatcher)
  private
    FPort: Integer;
  Public
    Constructor Create(aPort : Word); reintroduce;
    Procedure InitSocket; override;
    Property Port : Integer Read FPort;
  end;

  { TLSPThread }

  TLSPThread = Class(TThread)
  Private
    FContext : TLSPServerSocketConnectionDispatcher;
  Protected
    Procedure DoTerminate; override;
  Public
    Constructor Create(aContext : TLSPServerSocketConnectionDispatcher);
    Procedure Execute; override;

    Property Context: TLSPServerSocketConnectionDispatcher Read FContext;
  end;

implementation

uses typinfo, sockets;

{ TLSPProtocolMessageTypeHelper }

function TLSPProtocolMessageTypeHelper.GetAsString: String;
begin
  Result:=GetEnumName(TypeInfo(TLSPProtocolMessageType),Ord(Self));
end;

{ TLSPFrame }

function TLSPFrame.GetMessageType: TLSPProtocolMessageType;
begin
  Result:=TLSPProtocolMessageType(MsgType);
end;

function TLSPFrame.GetPayloadString: UTF8String;
begin
  Result:=TEncoding.UTF8.GetAnsiString(Payload);
end;

procedure TLSPFrame.SetMessageType(AValue: TLSPProtocolMessageType);
begin
  MsgType:=Ord(aValue);
end;

{ TLSPSocketTransport }

procedure TLSPSocketTransport.DoHandleFrame(const aFrame: TLSPFrame);
begin
  if Assigned(FOnHandleFrame) then
    FOnHandleFrame(Self,aFrame);
end;

constructor TLSPSocketTransport.Create(aSocket: TSocketStream);
begin
  FSocket:=aSocket;
end;

destructor TLSPSocketTransport.Destroy;
begin
  FreeAndNil(FSocket);
  inherited Destroy;
end;

procedure TLSPSocketTransport.DoSendMessage(aMessage: TJSONData);
begin
  SendJSON(lptmMessage,aMessage);
end;

procedure TLSPSocketTransport.DoSendDiagnostic(const aMessage: UTF8String);
Var
  Msg : TLSPFrame;

begin
  Msg.Version:=LSPProtocolVersion;
  Msg.MsgType:=Ord(lptmDiagnostic);
  Msg.ID:=NextID;
  Msg.PayLoad:=TEncoding.UTF8.GetAnsiBytes(aMessage);
  Msg.PayloadLen:=Length(Msg.PayLoad);
  SendFrame(Msg);
end;

{ TSocketDispatcher }

constructor TLSPSocketDispatcher.Create(aTransport: TLSPSocketTransport);
begin
  FTransport:=aTransport;
end;

destructor TLSPSocketDispatcher.Destroy;
begin
  FreeAndNil(FTransport);
  inherited Destroy;
end;

function TLSPSocketDispatcher.GetSocket: TSocketStream;
begin
  Result:=FTransport.Socket;
end;

function TLSPSocketDispatcher.GetTransport: TMessageTransport;
begin
  Result:=FTransport;
end;

function TLSPSocketTransport.NextID: Cardinal;
begin
  Result:=InterlockedIncrement(FID);
end;

function TLSPSocketTransport.SendFrame(const Msg : TLSPFrame) : Boolean;

Var
  N : Cardinal;

begin
  Result:=False;
  N:=0;
  if Socket.Write(Msg.Version,SizeOf(Byte))=0 then
    begin
    FSocketClosed:=True;
    exit;
    end;
  try
    Socket.WriteBuffer(Msg.MsgType,SizeOf(Byte));
    N:=htonl(Msg.ID);
    Socket.WriteBuffer(N,SizeOf(cardinal));
    N:=htonl(Msg.PayloadLen);
    Socket.WriteBuffer(N,SizeOf(cardinal));
    Socket.WriteBuffer(Msg.Payload[0],Msg.PayloadLen);
  except
    // Rather crude
    FSocketClosed:=True;
  end;
end;

function TLSPSocketTransport.ReadFrame(out Msg: TLSPFrame): Boolean;

Var
  N : Cardinal;

begin
  Result:=False;
  N:=0;
  Msg:=Default(TLSPFrame);
  if Socket.Read(Msg.Version,SizeOf(Byte))=0 then
    begin
    FSocketClosed:=True;
    exit;
    end;
  Socket.ReadBuffer(Msg.MsgType,SizeOf(Byte));
  Socket.ReadBuffer(N,SizeOf(cardinal));
  Msg.ID:=ntohl(N);
  Socket.ReadBuffer(N,SizeOf(cardinal));
  Msg.PayloadLen:=ntohl(N);
  SetLength(Msg.Payload,Msg.PayloadLen);
  if Msg.PayloadLen>0 then
    Socket.ReadBuffer(Msg.Payload[0],Msg.PayloadLen);
  Result:=(Msg.Version=LSPProtocolVersion);
end;


function TLSPSocketTransport.SendJSON(aType: TLSPProtocolMessageType;
  aJSON: TJSONData): Boolean;

Var
  Msg : TLSPFrame;
  JS : TJSONStringType; // Tmp var for debugging purposes.

begin
  Msg.Version:=LSPProtocolVersion;
  Msg.MsgType:=Ord(aType);
  Msg.ID:=NextID;
  if Assigned(aJSON) then
    JS:=aJSON.AsJSON
  else
    JS:='';
  Msg.PayLoad:=TEncoding.UTF8.GetAnsiBytes(JS);
  Msg.PayloadLen:=Length(Msg.PayLoad);
  SendFrame(Msg);
  Result:=True;
end;

function TLSPSocketTransport.ReceiveJSON(aType: TLSPProtocolMessageType): TJSONData;

Var
  Msg : TLSPFrame;
  JSON : TJSONStringType;

begin
  Result:=nil;
  Repeat
    if Not ReadFrame(Msg) then
      Exit;
    if (Ord(aType)<>Msg.MsgType) then
      DoHandleFrame(Msg)
    else
      begin
      if (Msg.PayloadLen<>0) then
        begin
        JSON:=TEncoding.UTF8.GetAnsiString(Msg.Payload);
        Result:=GetJSON(JSON,True);
        end;
      end;
  Until (Ord(aType)=Msg.MsgType) or SocketClosed;
end;

function TLSPSocketDispatcher.HandleException(aException : Exception; IsReceive : Boolean) : Boolean;

Const
  Stage : Array[Boolean] of string = ('sending','receiving');

begin
  Writeln('Exception ',aException.ClassName,' during ',Stage[IsReceive],' : ',aException.Message);
  Result:=True;
end;

function TLSPClientSocketDispatcher.ExecuteRequest(aRequest: TJSONData): TJSONData;


begin
  Result:=Nil;
  if SocketTransport.SendJSON(lpmtRequest,aRequest) then
    Result:=SocketTransport.ReceiveJSON(lpmtResponse);
end;

{ TLSPServerSocketConnectionDispatcher }

procedure TLSPServerSocketConnectionDispatcher.DoMethodResult(Sender: TObject;
  aResponse: TObject; const aID: String; aResult: TJSONData);
begin
  FContext.Log('Result of request "%s" : %s',[aID,aResult.AsJSON]);
end;

procedure TLSPServerSocketConnectionDispatcher.DoMethodError(Sender: TObject;
  aResponse: TObject; const aID: String; aError: TJSONData);
begin
  FContext.Log('Client reported error for request "%s" : %s',[aID,aError.AsJSON]);
end;

function TLSPServerSocketConnectionDispatcher.ExecuteRequest(aRequest: TJSONData
  ): TJSONData;
begin
  Result:=FContext.Execute(aRequest);
end;

constructor TLSPServerSocketConnectionDispatcher.Create(aTransport : TLSPSocketTransport);
Var
  lDisp : TLSPLocalDispatcher;

begin
  inherited Create(aTransport);
  lDisp:=TLSPLocalDispatcher.Create(aTransport,False);
  lDisp.OnMethodResult:=@DoMethodResult;
  lDisp.OnMethodError:=@DoMethodError;
  FContext:=TLSPContext.Create(aTransport,lDisp,True);
end;

destructor TLSPServerSocketConnectionDispatcher.Destroy;
begin
  If Assigned(FOnDestroy) then
    FOnDestroy(Self);
  FreeAndNil(FContext);
  inherited Destroy;
end;

procedure TLSPServerSocketConnectionDispatcher.RunLoop;

Var
  Req,Resp : TJSONData;

begin
  Req:=Nil;
  Resp:=Nil;
  try
    While not Terminated do
      begin
      Req:=SocketTransport.ReceiveJSON(lpmtRequest);
      if Assigned(Req) then
        begin
        Resp:=FContext.Execute(req);
        if not SocketTransport.SendJSON(lpmtResponse,Resp) then
          Terminate;
        end;
      FreeAndNil(Resp);
      FreeAndNil(Req);
      if SocketTransport.SocketClosed then
        Terminate;
      end;
  finally
    Req.Free;
    Resp.Free;
  end;
end;

procedure TLSPServerSocketConnectionDispatcher.Terminate;
begin
  FTerminated:=True;
end;

{ TLSPServerSocketDispatcher }

Function TLSPServerSocketDispatcher.CreateDispatcher(Data: TSocketStream) : TLSPServerSocketConnectionDispatcher;

Var
  Trans : TLSPSocketTransport;

begin
  Trans:=TLSPSocketTransport.Create(Data);
  Result:=TLSPServerSocketConnectionDispatcher.Create(Trans);
end;

procedure TLSPServerSocketDispatcher.HandleConnection(Sender: TObject;
  Data: TSocketStream);

var
  Conn : TLSPServerSocketConnectionDispatcher;

begin
  Conn:=CreateDispatcher(Data);
  try
    AddConnection(Conn);
    Case ThreadMode of
      tmNone:
        Conn.RunLoop;
      tmThreadPerConnection :
        begin
        TLSPThread.Create(Conn);
        Conn:=Nil;
        end;
    end;
  finally
    Conn.Free;
  end;
  if FSingleConnect then
    Terminate;
end;

procedure TLSPServerSocketDispatcher.SetServer(aSocket: TSocketServer);
begin
  FSocket:=aSocket;
  FSocket.OnConnect:=@HandleConnection;
end;

constructor TLSPServerSocketDispatcher.Create;
begin
  FConns:=TFPList.Create;
end;

destructor TLSPServerSocketDispatcher.Destroy;
begin
  FreeAndNil(FSocket);
  FreeAndNil(FConns);
  inherited Destroy;
end;

procedure TLSPServerSocketDispatcher.TerminateConnections;

Var
  I : Integer;

begin
  For I:=FConns.Count-1 downto 0 do
    TLSPServerSocketConnectionDispatcher(FConns[i]).Terminate;
end;

procedure TLSPServerSocketDispatcher.RemoveConn(Sender: TObject);
begin
  FConns.Remove(Sender);
end;

procedure TLSPServerSocketDispatcher.AddConnection(
  aConn: TLSPServerSocketConnectionDispatcher);
begin
  aConn.OnDestroy:=@RemoveConn;
  FConns.Add(aConn);
end;

procedure TLSPServerSocketDispatcher.RunLoop;

begin
  if not assigned(FSocket) then
    Raise ELSPSocket.Create('Cannot run loop: Socket not assigned');
  FSocket.StartAccepting;
end;

procedure TLSPServerSocketDispatcher.Terminate;
begin
  if not assigned(FSocket) then
    Exit;
  TerminateConnections;
  FSocket.StopAccepting(True);
end;

{$IFDEF UNIX}
{ TLSPServerUnixSocketDispatcher }

constructor TLSPServerUnixSocketDispatcher.Create(aPath: String);
begin
  Inherited Create;
  FPath:=aPath;
end;

procedure TLSPServerUnixSocketDispatcher.InitSocket;
begin
  SetServer(TUnixServer.Create(FPath));
  Socket.ReuseAddress:=True;
end;

{ TLSPServerTCPSocketDispatcher }

constructor TLSPServerTCPSocketDispatcher.Create(aPort: Word);
begin
  Inherited Create;
  FPort:=aPort;
end;

procedure TLSPServerTCPSocketDispatcher.InitSocket;
begin
  SetServer(TInetServer.Create(FPort));
  Socket.ReuseAddress:=True;
end;
{$ENDIF}

{ TLSPThread }

procedure TLSPThread.DoTerminate;
begin
  inherited DoTerminate;
  FContext.Terminate;
end;

constructor TLSPThread.Create(aContext: TLSPServerSocketConnectionDispatcher);
begin
  FContext:=aContext;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;

procedure TLSPThread.Execute;
begin
  try
    FContext.RunLoop;
  finally
    FreeAndNil(FContext)
  end;
end;

end.

