// Pascal Language Server
// Copyright 2020 Arjan Adriaanse
// Copyright 2020 Ryan Joseph

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

unit LSP.Base;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  { RTL }
  Classes, SysUtils, TypInfo, RTTIUtils,
  { JSON-RPC }
  fpjson, fpjsonrtti, fpjsonrpc,
  { Pasls }
  LSP.BaseTypes, LSP.Streaming, LSP.Messages;

const
  LSPContentType = 'application/vscode-jsonrpc; charset=utf-8';

Type
  { TLSPRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage

    A request message to describe a request between the client and the server. 
    Every processed request must send a response back to the sender of the request. }


  { TLSPContextCustomJSONRPCHandler }

  TLSPContextCustomJSONRPCHandler = class(TCustomJSONRPCHandler)
  private
    FTransport: TMessageTransport;
  Public
    Procedure DoLog(Const Msg : String); overload;
    Procedure DoLog(Const Fmt : String; Const Args : Array of const); overload;
    Procedure LogError(Const Msg : String; E : Exception);
    Property Transport : TMessageTransport Read FTransport Write FTransport;
  end;

  generic TLSPRequest<T: TLSPStreamable; U> = class(TLSPContextCustomJSONRPCHandler)
  protected
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    function Process(var Params : T): U; virtual; abstract;
  end;

  { TOutGoingID }

  TOutGoingID = Class
    // One ID for all outgoing requests
  Private
    Class Var
    OutgoingRequestIndex : Integer;
  Private
     FTransport : TMessageTransport;
  Public
    Constructor Create(aTransport : TMessageTransport);
    Property Transport : TMessageTransport Read FTransport;
  end;

  { TLSPOutgoingRequest
    A request from the server to the client }

  generic TLSPOutgoingRequest<T: TLSPStreamable> = class(TOutGoingID)
  public
    procedure Execute(Params: T; Method: String);
  end;

  { TLSPNotification
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage

    A notification message. A processed notification message must not send a response back. 
    They work like events. }

  generic TLSPNotification<T: TLSPStreamable> = class(TLSPContextCustomJSONRPCHandler)
  protected
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    procedure Process(var Params : T); virtual; abstract;
  end;

  { TLSPDispatcher }

  { TLSPBaseDispatcher }

  // A dispatcher is responsible for handling a request and returning the result.
  TLSPBaseDispatcher = class(TObject)
  protected
    Function GetTransport : TMessageTransport; virtual; abstract;
  Public
    Function ExecuteRequest(aRequest : TJSONData): TJSONData; virtual; abstract;
    Property Transport : TMessageTransport Read GetTransport;
  end;


  { TJSONRPCDispatcher }
  // Wrapper class for TCustomJSONRPCDispatcher, to handle a bug in fpc's JSON-rpc
  TJSONRPCDispatcher = class(TCustomJSONRPCDispatcher)
  private
    FTransport: TMessageTransport;
  protected
    Function ExecuteHandler(H: TCustomJSONRPCHandler; Params, ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    function ExecuteMethod(const AClassName, AMethodName: TJSONStringType;
      Params, ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
  public
    constructor Create(AOwner: TComponent); override;
    Property Transport : TMessageTransport Read FTransport Write FTransport;
  end;

  { TLSPLocalDispatcher }

  // Dispatches the request locally through a TJSONRPCDispatcher instance.

  TLSPLocalDispatcher = class(TLSPBaseDispatcher)
  Private
    FJSONDispatcher : TJSONRPCDispatcher;
    FOwnsTransport: Boolean;
    FTransport: TMessageTransport;
  Protected
    function GetTransport: TMessageTransport; override;
  Public
    Constructor Create(aTransport : TMessageTransport; aOwnsTransport : Boolean = False);
    Destructor Destroy; override;
    function ExecuteRequest(aRequest : TJSONData): TJSONData; override;
    Property Transport : TMessageTransport Read FTransport;
    // Dispatcher owns transport !
    Property OwnsTransport : Boolean Read FOwnsTransport Write FOwnsTransport;
  end;


  { TLSPContext }

  TLSPContext = Class (TObject)
  Private
    Class var _LogFile: TFileStream;
  private
    FDispatcher: TLSPBaseDispatcher;
    FDispatcherOwner: Boolean;
    FLastMessageID: Integer;
    FTransport: TMessageTransport;
    Class function GetLogFile: String; static;
    procedure SetDispatcher(AValue: TLSPBaseDispatcher);
    Class procedure SetLogFile(const AValue: String); static;
  Protected
    Class Procedure DoLog(const Msg : String);
    Class Procedure DoLog(const Fmt : String; Const Args : Array of const);
  Public
    Class Destructor Done;
  Public
    Constructor Create(aTransport : TMessageTransport; aDispatcher : TLSPBaseDispatcher; aOwner : Boolean); virtual;
    Constructor Create(aOwner : Boolean);
    destructor Destroy; override;
    Function Execute(aRequest : TJSONData) : TJSONData;
    function NextMessageID: Integer;
    // logging
    class Function HaveLog : Boolean;
    Class Procedure Log(const Msg : String);
    Class Procedure Log(const Fmt : String; Const Args : Array of const);
    Class Property LogFile : String Read GetLogFile Write SetLogFile;
  published
    Property LastMessageID: Integer Read FLastMessageID;
    // The transport class for sending messages;
    Property Transport : TMessageTransport Read FTransport;
    // The dispatcher that actually executes requests
    Property Dispatcher : TLSPBaseDispatcher Read FDispatcher Write SetDispatcher;
    // Do we own the dispatcher.
    Property DispatcherOwner : Boolean Read FDispatcherOwner Write FDispatcherOwner;
  end;

{ LSPHandlerManager }

function LSPHandlerManager: TCustomJSONRPCHandlerManager;

function IsResponseValid(Response: TJSONData): boolean;

implementation


{ Utilities }

function IsResponseValid(Response: TJSONData): boolean;
begin
  result := true;
  // invalid responses without id's or null id's must not be sent to client, i.e:
  // {"jsonrpc":"2.0","error":{"code":-32603,"message":"Access violation"},"id":null}
  if (Response is TJSONObject) and 
    ((TJSONObject(Response).Find('id') = nil) or 
      TJSONObject(Response).Nulls['id']) then
    result := false;
end;

{ TLSPLocalDispatcher }

function TLSPLocalDispatcher.GetTransport: TMessageTransport;
begin
  Result:=FTransport;
end;

constructor TLSPLocalDispatcher.Create(aTransport: TMessageTransport;
  aOwnsTransport: Boolean);
begin
  FTransport:=aTransport;
  FOwnsTransport:=aOwnsTransport;
  FJSONDispatcher:=TJSONRPCDispatcher.Create(Nil);
  FJSONDispatcher.Transport:=Self.Transport;
end;

destructor TLSPLocalDispatcher.Destroy;
begin
  FreeAndNil(FJSONDispatcher);
  inherited Destroy;
end;

function TLSPLocalDispatcher.ExecuteRequest(aRequest: TJSONData): TJSONData;

begin
  Result:=FJSONDispatcher.Execute(aRequest);
end;

{ TLSPContextCustomJSONRPCHandler }

procedure TLSPContextCustomJSONRPCHandler.DoLog(const Msg: String);
begin
  if Assigned(Transport) then
      Transport.SendDiagnostic(Msg);
end;

procedure TLSPContextCustomJSONRPCHandler.DoLog(const Fmt: String;
  const Args: array of const);
begin
  if Assigned(Transport) then
      Transport.SendDiagnostic(Fmt,Args);
end;

procedure TLSPContextCustomJSONRPCHandler.LogError(const Msg: String;
  E: Exception);
begin
  DoLog('% : Error %s with message "%s"',[Msg,E.ClassName,E.Message]);
end;



{ TLSPRequest }

function TLSPRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;

var
  Input: T;
  Output: U;
  Streamer: TLSPStreamer;
  AObject: TObject;
  ArrayTypeInfo: PTypeInfo;
  ElementType: PTypeInfo;

begin
  if (Transport=Nil) then
    Raise EArgumentNilException.Create('No transport available');
  Streamer:=Nil;
  Input := specialize TLSPStreaming<T>.ToObject(Params);
  try
    Output := Process(Input);
    if Output = nil then
      begin
        Result := TJSONNull.Create;
        exit;
      end;
    Streamer := TLSPStreamer.Create(nil);
    Streamer.Options := Streamer.Options + [jsoEnumeratedAsInteger, jsoSetEnumeratedAsInteger, jsoTStringsAsArray];
    if GetTypeKind(U) = tkDynArray then
      begin
        ArrayTypeInfo := PTypeInfo(TypeInfo(U));

        // Class kinds are in ElType2 (not sure why)
        ElementType := GetTypeData(ArrayTypeInfo)^.ElType;
        if ElementType = nil then
          ElementType := GetTypeData(ArrayTypeInfo)^.ElType2;

        case ElementType^.Kind of
          tkClass:
            begin

              Result := specialize TLSPStreaming<U>.ToJSON(TObjectArray(Output));

              // Free all objects
              for AObject in TObjectArray(Output) do
                AObject.Free;
            end;
          otherwise
            raise EUnknownErrorCode.Create('Dynamic array element type "'+Integer(ElementType^.Kind).ToString+'" is not supported for responses.');
        end;
      end
    else if GetTypeKind(U) = tkClass then
      begin
        Result := specialize TLSPStreaming<U>.ToJSON(TObject(Output));
        TObject(Output).Free;
      end;

    if Result = nil then
      Result := TJSONNull.Create;

  finally
    Input.Free;
    Streamer.Free;
  end;
end;

{ TOutGoingID }

constructor TOutGoingID.Create(aTransport: TMessageTransport);
begin
  FTransport:=aTransport;
end;

{ TLSPOutgoingRequest }


procedure TLSPOutgoingRequest.Execute(Params: T; Method: String);

var
  Message: TRequestMessage;

begin
  Message := TRequestMessage.Create;
  try
    Message.id := '_'+OutgoingRequestIndex.ToString;
    Inc(OutgoingRequestIndex);
    Message.params := Params;
    Message.method := Method;
    Message.Send(Self.Transport);
  finally
    Message.Free;
  end;
end;

{ TLSPNotification }

function TLSPNotification.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: T;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);
  try
    Process(Input);
  finally
    Input.Free;
  end;
  result := nil;
end;

{ TLSPBaseDispatcher }


{ TLSPDispatcher }

function TJSONRPCDispatcher.ExecuteHandler(H: TCustomJSONRPCHandler; Params,
  ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
begin
  if H is TLSPContextCustomJSONRPCHandler then
    TLSPContextCustomJSONRPCHandler(H).Transport:=Self.FTransport;
  Result:=Inherited ExecuteHandler(H,Params,ID,AContext);
end;

function TJSONRPCDispatcher.ExecuteMethod(const AClassName, AMethodName: TJSONStringType;
    Params, ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData;

{$IFDEF VER3_2}
Var
  lID : TJSONData;
{$ENDIF}

begin
  try
{$IFDEF VER3_2}
    lID:=ID;
    if lID=Nil then
      lID:=TJSONIntegerNumber.Create(0);
    try
      Result := inherited ExecuteMethod(AClassName, AMethodName, Params, lID, AContext);
    finally
      if lID<>ID then
        lID.Free;
    end;
{$ELSE}
   Result := inherited ExecuteMethod(AClassName, AMethodName, Params, ID, AContext);
{$ENDIF}
  except
    on E: LSPException do // handle errors specific to LSP
      Exit(CreateJSON2Error(E.Message, E.Code, ID.Clone, TransactionProperty))
    else raise;
  end;
end;


constructor TJSONRPCDispatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := [jdoSearchRegistry, jdoJSONRPC2, jdoNotifications, jdoStrictNotifications];
end;


{ LSPHandlerManager }

function LSPHandlerManager: TCustomJSONRPCHandlerManager;
begin
  Result := JSONRPCHandlerManager;
end;


procedure TLSPContext.SetDispatcher(AValue: TLSPBaseDispatcher);
begin
  if FDispatcher=AValue then Exit;
  if FDispatcherOwner then
    FreeAndNil(FDispatcher);
  FDispatcher:=AValue;
end;

class function TLSPContext.GetLogFile: String;
begin
  Result:='';
  if assigned(_LogFile) then
    Result:=_LogFile.FileName;
end;

class procedure TLSPContext.SetLogFile(const AValue: String);
begin
  if aValue=GetLogFile then exit;
  FreeAndNil(_LogFile);
  if aValue<>'' then
    _LogFile:=TFileStream.Create(aValue,fmCreate);
end;

class procedure TLSPContext.DoLog(const Msg: String);

Var
  NL : String;

begin
  if HaveLog and (Length(Msg)>0) then
    begin
    _LogFile.WriteBuffer(Msg[1],Length(Msg));
    NL:=sLineBreak;
    _LogFile.WriteBuffer(NL[1],Length(NL));
    end;
end;

class procedure TLSPContext.DoLog(const Fmt: String;
  const Args: array of const);
begin
  DoLog(Format(Fmt,Args));
end;

constructor TLSPContext.Create(aTransport: TMessageTransport;
  aDispatcher: TLSPBaseDispatcher; aOwner: Boolean);
begin
  Create(aOwner);
  Dispatcher:=aDispatcher;
end;

constructor TLSPContext.Create(aOwner: Boolean);
begin
  FDispatcherOwner:=aOwner;
end;

destructor TLSPContext.Destroy;
begin
  Dispatcher:=Nil;
  inherited Destroy;
end;

class destructor TLSPContext.Done;
begin
  FreeAndNil(_LogFile);
end;

function TLSPContext.NextMessageID: Integer;
begin
  Result:=InterlockedIncrement(FLastMessageID);
end;

class function TLSPContext.HaveLog: Boolean;
begin
  Result:=assigned(_LogFile);
end;

function TLSPContext.Execute(aRequest: TJSONData): TJSONData;

begin
  If HaveLog then
    DoLog('Executing request: %s',[aRequest.AsJSON]);
  try
    Result:=Dispatcher.ExecuteRequest(aRequest);
    If HaveLog then
      if Result<>Nil then
        DoLog('Request response: %s',[Result.AsJSON])
      else
        DoLog('Request returned no response.');
  except
    on E : Exception do
      begin
      if HaveLog then
        DoLog('Error %d while executing request: %s',[E.ClassName,E.Message]);
      Raise;
      end;
  end;
end;


class procedure TLSPContext.Log(const Msg: String);
begin
  If HaveLog then
    DoLog(Msg);
end;

class procedure TLSPContext.Log(const Fmt: String; const Args: array of const);
begin
  If HaveLog then
    DoLog(Fmt,Args);
end;


end.
