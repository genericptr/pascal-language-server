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

unit lsp;

{$mode objfpc}{$H+}

interface
uses
  { RTL }
  Classes, SysUtils, TypInfo, 
  { JSON-RPC }
  fpjson, fpjsonrtti, fpjsonrpc,
  { Pasls }
  basic;

type

  { TLSPStreamer }

  TLSPStreamer = class(TJSONStreamer)
  protected
    function StreamClassProperty(const AObject: TObject): TJSONData; override;
  end;

  { TLSPDeStreamer }

  TLSPDeStreamer = class(TJSONDeStreamer)
  protected
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;
                                PropData: TJSONData); override;
  end;

  { TLSPStreaming }

  generic TLSPStreaming<T: TPersistent> = class
  private
    class var Streamer: TLSPStreamer;
    class var DeStreamer: TLSPDeStreamer;
    class procedure GetObject(Sender: TOBject; AObject: TObject;
                              Info: PPropInfo; AData: TJSONObject;
                              DataName: TJSONStringType; var AValue: TObject); static;
  public
    class constructor Create;
    class function ToObject(const JSON: TJSONData): T; static;
    class function ToObject(const JSON: TJSONStringType): T;
    class function ToJSON(AObject: TObject): TJSONData; static;
  end;

  { TLSPProcessor }

  generic TLSPProcess<T, U> = function (var Params : T): U;

  generic TLSPProcessor<T, U: TPersistent> = class
  public
    class function Process(AProcess: specialize TLSPProcess<T, U>; const Params: TJSONData): TJSONData; static;
  end;

  { TLSPRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage

    A request message to describe a request between the client and the server. 
    Every processed request must send a response back to the sender of the request. }

  generic TLSPRequest<T, U: TPersistent> = class(TCustomJSONRPCHandler)
  protected
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    function Process(var Params : T): U; virtual; abstract;
  end;

  { TLSPOutgoingRequest
    A request from the server to the client }

  generic TLSPOutgoingRequest<T: TPersistent; U: TPersistent> = class
  private class var
    OutgoingRequestIndex: Integer;
  public
    class procedure Execute(Params: T; Method: String);
  end;

  { TLSPNotification
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage

    A notification message. A processed notification message must not send a response back. 
    They work like events. }

  generic TLSPNotification<T: TPersistent> = class(TCustomJSONRPCHandler)
  protected
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    procedure Process(var Params : T); virtual; abstract;
  end;

  { TLSPDispatcher }

  TLSPDispatcher = class(TCustomJSONRPCDispatcher)
  protected
    function ExecuteMethod(const AClassName, AMethodName: TJSONStringType;
      Params, ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { LSPException }

  LSPException = class(Exception)
  public
    function Code: Integer; virtual; abstract;
  end;

  EServerNotInitialized = class(LSPException)
  public
    function Code: Integer; override;
  end;

  EUnknownErrorCode = class(LSPException)
  public
    function Code: Integer; override;
  end;

  // Defined by the protocol.
  ERequestCancelled = class(LSPException)
  public
    function Code: Integer; override;
  end;

  EContentModified = class(LSPException)
  public
    function Code: Integer; override;
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

{ TLSPStreamer }

function TLSPStreamer.StreamClassProperty(const AObject: TObject): TJSONData;
var
  C: TClass;
  OptionalVariant: TOptionalVariantBase;
  OptionalObject: TOptionalObjectBase;
begin
  if not Assigned(AObject) then
    begin
      result := nil;
      Exit;
    end;
  C := AObject.ClassType;
  if C.InheritsFrom(TOptionalVariantBase) then
    begin
      OptionalVariant := TOptionalVariantBase(AObject);
      if OptionalVariant.HasValue then
        Result := StreamVariant(OptionalVariant.Value)
      else
        Result := nil
    end
  else if C.InheritsFrom(TOptionalObjectBase) then
    begin
      OptionalObject := TOptionalObjectBase(AObject);
      if OptionalObject.HasValue then
        if OptionalObject.Value = nil then
          Result := TJSONNull.Create
        else
          Result := ObjectToJSON(OptionalObject.Value)
      else
        Result := nil
    end
  else
    Result := inherited StreamClassProperty(AObject)
end;

{ TLSPDeStreamer }

procedure TLSPDeStreamer.DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData);
var
  C: TClass;
  Optional: TObject;
  OptionalVariant: TOptionalVariantBase;
  OptionalObject: TOptionalObjectBase;
  ElementType: PTypeInfo;
  PropArray: TJSONArray;
  I: Integer;
var
  VariantArray: array of variant;
  ObjectArray: array of TObject;
begin
  if (PropInfo^.PropType^.Kind = tkDynArray) and (PropData is TJSONArray) then
    begin
      // Class kinds are in ElType2 (not sure why)
      ElementType := GetTypeData(PropInfo^.PropType)^.ElType;
      if ElementType = nil then
        ElementType := GetTypeData(PropInfo^.PropType)^.ElType2;

      // Something went wrong, bail!
      if ElementType = nil then
        // TODO: raise LSPException
        exit;

      PropArray := TJSONArray(PropData);

      case ElementType^.Kind of
        tkVariant:
          begin
            SetLength(VariantArray, PropArray.Count);
            for I := 0 to PropArray.Count - 1 do
              VariantArray[I] := PropArray[I].Value;
            SetDynArrayProp(AObject, PropInfo, Pointer(VariantArray));
          end;
        tkClass:
          begin
            C:=GetTypeData(ElementType)^.ClassType;
            // Invalid class type
            if not C.InheritsFrom(TPersistent) then
              // TODO: raise LSPException
              exit;

            SetLength(ObjectArray, PropArray.Count);

            for I := 0 to PropArray.Count - 1 do
              if PropArray[I] is TJSONObject then
                begin
                  ObjectArray[I] := C.Create;
                  JSONToObject(TJSONObject(PropArray[I]), ObjectArray[I]);
                end
              else
                ; // TODO: raise LSPException (must be object in payload!)
            SetDynArrayProp(AObject, PropInfo, Pointer(ObjectArray));
          end;
        otherwise
          ; // TODO: support more types
      end;
    end
  else if PropInfo^.PropType^.Kind = tkClass then
    begin
      C := GetTypeData(PropInfo^.PropType)^.ClassType;
      if C.InheritsFrom(TOptionalVariantBase) then
        begin
          Optional := C.Create;
          OptionalVariant := TOptionalVariantBase(Optional);
          SetObjectProp(AObject, PropInfo, Optional);
          OptionalVariant.Value := JSONToVariant(PropData);
        end
      else if C.InheritsFrom(TOptionalObjectBase) then
        begin
          Optional := C.Create;
          OptionalObject := TOptionalObjectBase(Optional);
          SetObjectProp(AObject, PropInfo, Optional);
          if PropData.JSONType = jtNull then
            OptionalObject.Value := nil
          else
            begin
              OptionalObject.Value := OptionalObject.ValueClass.Create;
              JSONToObject(PropData as TJSONObject, OptionalObject.Value);
            end;
        end
      else if C.InheritsFrom(TJSONData) then
        begin
          // Clone raw JSON data
          SetObjectProp(AObject, PropInfo, TJSONData(PropData.Clone));
        end
      else
        inherited DoRestoreProperty(AObject, PropInfo, PropData)
    end
  else
    inherited DoRestoreProperty(AObject, PropInfo, PropData)
end;

{ TLSPStreaming }

class procedure TLSPStreaming.GetObject(Sender: TOBject; AObject: TObject;
                                        Info: PPropInfo; AData: TJSONObject;
                                        DataName: TJSONStringType; var AValue: TObject);
var
  C: TClass;
begin
  C := GetTypeData(Info^.PropType)^.ClassType;
  if C.InheritsFrom(TPersistent) then AValue := C.Create;
end;

class constructor TLSPStreaming.Create;
begin
  Streamer := TLSPStreamer.Create(nil);
  Streamer.Options := Streamer.Options +
    [jsoEnumeratedAsInteger, jsoSetEnumeratedAsInteger, jsoTStringsAsArray];

  DeStreamer := TLSPDeStreamer.Create(nil);
  DeStreamer.Options := DeStreamer.Options + [jdoIgnorePropertyErrors, jdoIgnoreNulls];
  DeStreamer.OnGetObject := @GetObject;
end;

class function TLSPStreaming.ToObject(const JSON: TJSONData): T;
begin
  Result := T.Create;
  DeStreamer.JSONToObject(JSON as TJSONObject, Result);
end;

class function TLSPStreaming.ToObject(const JSON: TJSONStringType): T;
begin
  Result := T.Create;
  DeStreamer.JSONToObject(JSON, Result);
end;

class function TLSPStreaming.ToJSON(AObject: TObject): TJSONData;
begin
  // TODO: intercept here for dynamic arrays

  if AObject.InheritsFrom(TCollection) then
    Result := Streamer.StreamCollection(TCollection(AObject))
  else
    Result := Streamer.ObjectToJSON(AObject);
end;

{ TLSPProcessor }

class function TLSPProcessor.Process(AProcess: specialize TLSPProcess<T, U>; const Params: TJSONData): TJSONData;
var
  Input: T;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);
  Result := specialize TLSPStreaming<U>.ToJSON(AProcess(Input));
  Input.Free;
end;

{ TLSPRequest }

function TLSPRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: T;
  Output: TObject;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);

  Output := Process(Input);

  if Output = nil then
    begin
      Result := TJSONNull.Create;
      Input.Free;
      exit;
    end;

  Result := specialize TLSPStreaming<U>.ToJSON(Output);
  
  if not Assigned(Result) then
    Result := TJSONNull.Create;

  Input.Free;
  Output.Free;
end;

{ TLSPOutgoingRequest }

class procedure TLSPOutgoingRequest.Execute(Params: T; Method: String);
var
  Request: TLSPOutgoingRequest;
  Message: TRequestMessage;
  ID: TGUID;
begin
  Message := TRequestMessage.Create;
  Message.id := '_'+OutgoingRequestIndex.ToString;
  Inc(OutgoingRequestIndex);
  Message.params := Params;
  Message.method := Method;
  Message.Send;
end;

{ TLSPNotification }

function TLSPNotification.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: T;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);
  Process(Input);
  Input.Free;

  result := nil;
end;

{ TLSPDispatcher }

function TLSPDispatcher.ExecuteMethod(const AClassName, AMethodName: TJSONStringType;
    Params, ID: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
begin
  try
    Result := inherited ExecuteMethod(AClassName, AMethodName, Params, ID, AContext);
  except
    on E: LSPException do // handle errors specific to LSP
      Exit(CreateJSON2Error(E.Message, E.Code, ID.Clone, TransactionProperty))
    else raise;
  end;
end;

constructor TLSPDispatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := [jdoSearchRegistry, jdoJSONRPC2, jdoNotifications, jdoStrictNotifications];
end;

{ LSPException }

function EServerNotInitialized.Code: Integer;
begin
  result := -32002;
end;

function EUnknownErrorCode.Code: Integer;
begin
  result := -32001;
end;

function ERequestCancelled.Code: Integer;
begin
  result := -32800;
end;

function EContentModified.Code: Integer;
begin
  result := -32801;
end;

{ LSPHandlerManager }

function LSPHandlerManager: TCustomJSONRPCHandlerManager;
begin
  Result := JSONRPCHandlerManager;
end;

end.

