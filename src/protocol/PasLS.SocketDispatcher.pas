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

interface

uses
  Classes, SysUtils, fpjson, ssockets,
  LSP.Base;

{
  The Protocol is simple message exchange:
  Client sends message of certain type (see below)
  Server answsers with message, with same ID.
}

Const
  LSPProtocolVersion = 1;

Type
  TLSPProtocolMessageType = (lpmtRequest,lpmtResponse);


  TLSPFrame = Packed Record
    Version : Byte;
    MsgType : Byte; // Sent as byte
    ID : cardinal; // In network order;
    PayloadLen : cardinal; // In network order
    PayLoad : TBytes;
  end;

  ELSPSocket = Class(Exception);

  { TSocketDispatcher }

  { TLSPSocketDispatcher }

  TLSPSocketDispatcher = Class(TLSPBaseDispatcher)
  private
    FSocket: TSocketStream;
    FID : Integer;
    FSocketClosed: Boolean;
  Protected
    function NextID : Cardinal;
    function ReadFrame(Out Msg: TLSPFrame) : Boolean;
    function SendFrame(const Msg: TLSPFrame) : Boolean;
    function ReceiveJSON(aType: TLSPProtocolMessageType): TJSONData;
    function SendJSON(aType: TLSPProtocolMessageType; aJSON : TJSONData) : Boolean;
    function HandleException(aException: Exception; IsReceive : Boolean): Boolean; virtual;
  public
    Constructor Create(aSocket : TSocketStream); virtual;
    Destructor Destroy; override;
    Property Socket : TSocketStream Read FSocket;
    Property SocketClosed : Boolean Read FSocketClosed;
  end;

  TLSPClientSocketDispatcher = Class(TLSPSocketDispatcher)
  Public
    function ExecuteRequest(aRequest: TJSONData): TJSONData; override;
  end;

  TLSPSocketContext = class(TLSPContext);

  { TLSPServerSocketConnectionDispatcher }

  TLSPServerSocketConnectionDispatcher = Class(TLSPSocketDispatcher)
  Private
    FContext : TLSPSocketContext;
    FOnDestroy: TNotifyEvent;
    FTerminated : Boolean;
  Public
    function ExecuteRequest(aRequest: TJSONData): TJSONData; override;
    Constructor Create(aSocket: TSocketStream); override;
    Destructor Destroy; override;
    Procedure RunLoop; virtual;
    Procedure Terminate; virtual;
    Property Context : TLSPSocketContext Read FContext Write FContext;
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

uses sockets;

{ TSocketDispatcher }

constructor TLSPSocketDispatcher.Create(aSocket: TSocketStream);
begin
  FSocket:=aSocket;
end;

destructor TLSPSocketDispatcher.Destroy;
begin
  FreeAndNil(FSocket);
  inherited Destroy;
end;

function TLSPSocketDispatcher.NextID: Cardinal;
begin
  Result:=InterlockedIncrement(FID);
end;

function TLSPSocketDispatcher.SendFrame(const Msg : TLSPFrame) : Boolean;

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

function TLSPSocketDispatcher.ReadFrame(out Msg: TLSPFrame): Boolean;

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


function TLSPSocketDispatcher.SendJSON(aType: TLSPProtocolMessageType;
  aJSON: TJSONData): Boolean;

Var
  Msg : TLSPFrame;
  JS : TJSONStringType; // Tmp var for debugging purposes.

begin
  Result:=False;
  try
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
  except
    On E : Exception do
      HandleException(E,False);
  end;
end;

function TLSPSocketDispatcher.ReceiveJSON(aType : TLSPProtocolMessageType) : TJSONData;

Var
  Msg : TLSPFrame;
  JSON : TJSONStringType;

begin
  Result:=nil;
  try
    if Not ReadFrame(Msg) then
      Exit;
    if (Msg.MsgType<>ord(aType)) then
      Exit;
    if (Msg.PayloadLen=0) then
      Exit;
    JSON:=TEncoding.UTF8.GetAnsiString(Msg.Payload);
    Result:=GetJSON(JSON,True);
  except
    On E : Exception do
      HandleException(E,True);
  end;
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
  if SendJSON(lpmtRequest,aRequest) then
    Result:=ReceiveJSON(lpmtResponse);
end;

{ TLSPServerSocketConnectionDispatcher }

function TLSPServerSocketConnectionDispatcher.ExecuteRequest(aRequest: TJSONData
  ): TJSONData;
begin
  Result:=FContext.Execute(aRequest);
end;

constructor TLSPServerSocketConnectionDispatcher.Create(aSocket: TSocketStream);
begin
  inherited Create(aSocket);
  FContext:=TLSPSocketContext.Create(TLSPLocalDispatcher.Create,True);
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
      Req:=ReceiveJSON(lpmtRequest);
      if Assigned(Req) then
        begin
        Resp:=FContext.Execute(req);
        if not SendJSON(lpmtResponse,Resp) then
          Terminate;
        end;
      FreeAndNil(Resp);
      FreeAndNil(Req);
      if SocketClosed then
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

begin
  Result:=TLSPServerSocketConnectionDispatcher.Create(Data);
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

