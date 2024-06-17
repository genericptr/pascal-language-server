// Pascal Language Server
// Copyright 2023 Michael Van Canneyt

// LSP Text/File based protocol - in particular, Standard Input/Output/Error files.

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

unit PasLS.TextLoop;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ssockets, LSP.Base, LSP.Messages, fpjson;

Type
  TCreateLSPContextEvent = function(OutStream, LogStream: THandleStream): TLSPContext;

  { TTextLSPContext }
  PText = ^Text;

  { TLSPTextTransport }

  TLSPTextTransport = class(TMessageTransport)
    FOutput : THandleStream;
    // Logging occurs to stdout (tcpip), or stderr (pipes)
    FLog : THandleStream;
  Protected
    Procedure DoSendMessage(aMessage: TJSONData); override;
    Procedure DoSendDiagnostic(const aMessage: UTF8String); override;
  Public
    constructor Create(aOutput,aLog : THandleStream); reintroduce;
    Procedure EmitMessage(aMessage: TJSONStringType);
  end;

Procedure SetupTextLoop();
Procedure RunMessageLoop(aDoCreateContext: TCreateLSPContextEvent; aTcpip: Boolean; aListenIpAddress: string; aListenPort: Integer);
procedure DebugSendMessage(var aFile : Text; aContext : TLSPContext; const aMethod, aParams: String);

implementation

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';
  CRLF = #13#10;

type
  TTcpipConnectionThread = class;

  { TRunLoop }

  // To be able to handle both TCP/IP and pipes, the THandleStream and
  // TSocketStream classes are used.
  //
  // To handle the different nature of TCP/IP running as a server and piped
  // communication two threads are used.
  //
  // The first thread is only started in case TCP/IP is used and this thread
  // just waits for an incoming TCP/IP connection. (the client) Only one
  // connection is accepted and the thread immediately ends afterwards.
  // (In principle PasLS could be extended to handle multiple clients/sessions
  // simultaneously, but this is not implemented)
  //
  // The second thread is started at application start (pipe) or on an incoming
  // connection (TCP/IP) and waits in the background for incoming LSP messages
  // and send those to the main-thread to be handled.
  TRunLoop = class
  private
    FDoCreateContext: TCreateLSPContextEvent;
    FListenIp: string;
    FListenPort: Integer;
    // We only allow one connection, this boolean is set when this connection is
    // made
    FHasConnection: Boolean;

    FMustStop: Boolean;
    FContext: TLSPContext;
    FIO: TLSPTextTransport;
    FLogStream: THandleStream;

    procedure HandleNewConnection(aSender: TObject; aData: TSocketStream);
    procedure StopExecution();
    procedure ListenForIncomingConnections();
    procedure InitializeLSPTextTransport(aOutStream, aLogStream: THandleStream);
  public
    constructor Create(aDoCreateContext: TCreateLSPContextEvent; aListenIpAddress: string; aListenPort: Integer);
    procedure Execute(aTcpip: Boolean);
    property DoCreateContext: TCreateLSPContextEvent read FDoCreateContext;
  end;

  { TTcpipConnectionThread }

  TTcpipConnectionThread = class(TThread)
  private
    FInStream: THandleStream;
    FContent: UnicodeString;
    FContext: TLSPContext;
    FIO: TLSPTextTransport;
    FRunLoop: TRunLoop;
  protected
    // Processes all incoming LSP messages within the main thread and sends a
    // LSP-response when applicable
    procedure ProcessMessage();
    // Waits for a new incoming LSP message and returns the message as a array of bytes
    function AwaitMessage(aInStream: THandleStream; aVerboseOutput: Boolean): TBytes;
  public
    constructor Create(aInStream: THandleStream; aContext: TLSPContext; aIO: TLSPTextTransport; aRunLoop: TRunLoop);
    destructor Destroy; override;
    // Main execution loop that runs in a background thread and waits for incoming
    // messages (blocking). Once a message is received it is signaled to be
    // processed in the main-thread.
    procedure Execute; override;
  end;

Procedure SetupTextLoop();

begin
  TJSONData.CompressedJSON := True;
end;

procedure DebugSendMessage(var aFile : Text; aContext : TLSPContext; const aMethod, aParams: String);

var
  Content: TJSONStringType;
  Request: TJSONData;
  Response: TJSONData;

begin
  Response:=Nil;
  Writeln(aFile,'▶️ ', aMethod);
  Content := '{"jsonrpc": "2.0","id": '+aContext.NextMessageID.ToString+', "method": "'+aMethod+'","params": '+aParams+'}';
  Request := GetJSON(Content, True);
  try
    Response := aContext.Execute(Request);
    if Assigned(Response) then
      begin
      writeln(aFile,'◀️ response: ');
      writeln(aFile,Response.FormatJSON);
      Flush(aFile);
      end;
  finally
    Request.Free;
    Response.Free;
  end;
end;

Procedure SendResponse(aTransport : TMessageTransport; aContext : TLSPContext; aResponse : TJSONData; aFreeResponse : Boolean = True);

Var
  Content : TJSONStringType;

begin
  try
    if not IsResponseValid(aResponse) then
      begin
      aContext.Log('Response not valid: %s',[aResponse.AsJSON]);
      aTransport.SendDiagnostic('invalid response -> '+aResponse.AsJSON);
      exit;
      end;
    Content := aResponse.AsJSON;
    (aTransport as TLSPTextTransport).EmitMessage(Content);
    aContext.Log('Wrote response to request');
  finally
    if aFreeResponse then
      aResponse.Free;
  end;
end;

{ TTcpipConnectionThread }

// Should always run in main thread
procedure TTcpipConnectionThread.ProcessMessage();
var
  Request, Response: TJSONData;
  VerboseDebugging: boolean = false;
begin
  if FContent = '' then
    begin
    // Empty content means disconnect
    FContext.Log('Lost connection, stop.');
    FRunLoop.StopExecution;
    end
  else
    begin
    try
      // log request payload
      if VerboseDebugging then
        FContext.Log(FContent);
      Request:=GetJSON(FContent, True);

      Response := FContext.Execute(Request);
      if Assigned(Response) then
        begin
        // log response payload
        if VerboseDebugging then
          FContext.Log(Response.AsJSON);

        SendResponse(FIO, FContext, Response, True)
        end
      else
        FContext.Log('No response to request');
    finally
      Request.Free;
    end;
    end;
end;

function TTcpipConnectionThread.AwaitMessage(aInStream: THandleStream; aVerboseOutput: Boolean): TBytes;

var
  ContentSize: Integer;

  procedure ParseLine(Line: AnsiString);
  var
    I: Integer;
    Value, Name: String;
  begin
    FContext.Log('Read header: %s',[Line]);

    I := Pos(':', Line);
    Name := Copy(Line, 1, I - 1);
    Delete(Line, 1, i);
    Value := Trim(Line);
    if Name = 'Content-Length' then
      ContentSize := StrToIntDef(Value,0);
  end;

var
  Buf: array[1..1023] of Byte;
  s: AnsiString;
  BytesRead: Integer;
  Line: AnsiString;
  PosCrLf: SizeInt;
begin
  Line := '';
  ContentSize:=0;
  s := '';
  FContext.Log('Reading request');
  repeat
  PosCrLf := Pos(CRLF, s);
  if PosCrLf > 0 then
    begin
    Line := Copy(s, 1, PosCrLf-1);
    s := Copy(s, PosCrLf+2, MaxInt);
    ParseLine(Line);
    end
  else
    begin
    // TInetSocket raises an exception when it is closed and then tried
    // to read from. We don't want the exception but return without any response.
    if (aInStream is TInetSocket) then
      if TInetSocket(aInStream).Closed then
        Exit([]);

    BytesRead := aInStream.Read(Buf, SizeOf(Buf));
    if BytesRead=0 then
      begin
      FContext.Log('Lost connection');
      Exit([]);
      end;
    s := s + TEncoding.ASCII.GetAnsiString(@Buf[1], 0, BytesRead);
    end;
  until (Line='') and (ContentSize>0);
  Result := TEncoding.ASCII.GetAnsiBytes(s);
  BytesRead:=Length(Result);
  if BytesRead < ContentSize then
    begin
    SetLength(Result, ContentSize);
    aInStream.ReadBuffer(Result, BytesRead, ContentSize-BytesRead);
    end;
end;

procedure TTcpipConnectionThread.Execute;
var
  IncomingBytes: TBytes;
begin
  repeat
  IncomingBytes := AwaitMessage(FInStream, True);
  FContent := TEncoding.UTF8.GetString(IncomingBytes);
  // Handle the message (or absence of a message) in the main thread
  Synchronize(@ProcessMessage);

  // If IncomingBytes is empty, AwaitMessage discovered a disconnect
  until Length(IncomingBytes) = 0;
end;

constructor TTcpipConnectionThread.Create(aInStream: THandleStream; aContext: TLSPContext; aIO: TLSPTextTransport; aRunLoop: TRunLoop);
begin
  FInStream := aInStream;
  FContext:=aContext;
  FIO:=aIO;
  FRunLoop:=aRunLoop;
  inherited Create(False);
end;

destructor TTcpipConnectionThread.Destroy;
begin
  FInStream.Free;
  inherited Destroy;
end;

Procedure RunMessageLoop(aDoCreateContext: TCreateLSPContextEvent; aTcpip: Boolean; aListenIpAddress: string; aListenPort: Integer);

var
  RunLoop: TRunLoop;

begin
  RunLoop := TRunLoop.Create(aDoCreateContext, aListenIpAddress, aListenPort);
  try
    RunLoop.Execute(aTcpip);
  finally
    RunLoop.Free;
  end;
end;

{ TTextLSPContext }

constructor TLSPTextTransport.Create(aOutput, aLog: THandleStream);
begin
  FOutput:=aOutput;
  FLog:=aLog;
end;

procedure TLSPTextTransport.EmitMessage(aMessage: TJSONStringType);
var
  Message: string;
begin
  Try
    Message:='Content-Type: '+ ContentType+CRLF;
    Message:=Message+'Content-Length: '+IntToStr(Length(aMessage))+CRLF+CRLF;
    FOutput.WriteBuffer(Message[1], Length(Message));
    FOutput.WriteBuffer(aMessage[1], Length(aMessage));
  except
    on e : exception do
      DoLog('Exception %s during output: %s',[E.ClassName,E.Message]);
  end;
end;

procedure TLSPTextTransport.DoSendMessage(aMessage: TJSONData);

Var
  Content : TJSONStringType;

begin
  Content:=aMessage.AsJSON;
  EmitMessage(Content);
end;

procedure TLSPTextTransport.DoSendDiagnostic(const aMessage: UTF8String);
begin
  Try
    FLog.WriteBuffer(aMessage[1], Length(aMessage));
    FLog.WriteBuffer(string(LineEnding)[1], Length(LineEnding));
  except
    on e : exception do
      DoLog('Exception %s during diagnostic output: %s',[E.ClassName,E.Message]);
  end;
end;

{ TRunLoop }

procedure TRunLoop.HandleNewConnection(aSender: TObject; aData: TSocketStream);
var
  ConnThread: TTcpipConnectionThread;
begin
  FHasConnection:=True;
  // StdOut is not used for (piped) communication, so log to stdout
  FLogStream := THandleStream.Create(StdOutputHandle);
  InitializeLSPTextTransport(aData, FLogStream);

  FContext.Log('New incoming connection');

  ConnThread := TTcpipConnectionThread.create(aData, FContext, FIO, Self);
  ConnThread.FreeOnTerminate:=True;
end;

procedure TRunLoop.StopExecution();
begin
  FMustStop:=True;
end;

procedure TRunLoop.ListenForIncomingConnections();
var
  ServerSocket: TInetServer;
begin
  try
    ServerSocket := TInetServer.Create(FListenIp, FListenPort, TSocketHandler.Create);
    try
      ServerSocket.OnConnect:=@HandleNewConnection;
      // One connection, when the connection the language server stops.
      // (In theory more scenario's are possible, but keep it simple for now)
      ServerSocket.MaxConnections:=1;
      ServerSocket.ReuseAddress:=True;
      ServerSocket.StartAccepting;
    finally
      ServerSocket.Free;
    end;
  except
    on E: Exception do
      WriteLn('Network problem. ', E.Message);
  end;
  // When there is no connection, stop the application. When there is a connection,
  // stop listening but keep the connection-thread (and application) running.
  if not FHasConnection then
    TThread.Synchronize(TThread.CurrentThread, @StopExecution);
end;

procedure TRunLoop.InitializeLSPTextTransport(aOutStream, aLogStream: THandleStream);
begin
  FContext := FDoCreateContext(aOutStream, aLogStream);
  if FContext.Transport is TLSPTextTransport then
    FIO:=FContext.Transport as TLSPTextTransport
  else
    FIO:=TLSPTextTransport.Create(aOutStream, aLogStream);
end;

constructor TRunLoop.Create(aDoCreateContext: TCreateLSPContextEvent; aListenIpAddress: string; aListenPort: Integer);
begin
  FDoCreateContext:=aDoCreateContext;
  FListenIp:=aListenIpAddress;
  FListenPort:=aListenPort;
end;

procedure TRunLoop.Execute(aTcpip: Boolean);
var
  InStream, OutStream: THandleStream;
  ConnThread: TTcpipConnectionThread;
begin
  InStream:=nil;
  OutStream:=nil;
  try
    if aTcpip then
      // The biggest difference with a tcpip-server (listening) socket is that multiple
      // connections can come in. So we have to wait for a connection before
      // the real connection can be made using a TTcpipConnectionThread thread.
      // So ListenForIncomingConnections is called in the background that will
      // create a TTcpipConnectionThread when a (new) connection is made.
      TThread.ExecuteInThread(@ListenForIncomingConnections)
    else
      begin
      InStream := THandleStream.Create(StdInputHandle);
      OutStream := THandleStream.Create(StdOutputHandle);
      FLogStream := THandleStream.Create(StdErrorHandle);

      InitializeLSPTextTransport(OutStream, FLogStream);

      ConnThread := TTcpipConnectionThread.Create(InStream, FContext, FIO, Self);
      ConnThread.FreeOnTerminate:=True;
      end;

    repeat
    CheckSynchronize(-1);
    until FMustStop;
  finally
    InStream.Free;
    OutStream.Free;
    FLogStream.Free;
    if Assigned(FContext) and (FIO<>FContext.Transport) then
      fIO.Free;
  end;
end;

end.

