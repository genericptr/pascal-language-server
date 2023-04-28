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
  Classes, SysUtils, TypInfo, RTTIUtils, Contnrs,  types,
  { JSON-RPC }
  fpjson, fpjsonrtti, fpjsonrpc,
  { Pasls }
  basic, memUtils;

type
  TObjectArray = Array of TObject;

  { TLSPStreamer }

  TLSPStreamer = class(TJSONStreamer)
  protected
    function StreamClassProperty(const AObject: TObject): TJSONData; override;
    function ObjectToJSON(Const AObject: TObject): TJSONObject;
    function StreamProperty(Const AObject: TObject; PropertyInfo: PPropInfo): TJSONData;
  end;

  { TLSPDeStreamer }

  TLSPDeStreamer = class(TJSONDeStreamer)
  protected
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;
                                PropData: TJSONData); override;
  end;

  { TLSPStreaming }

  generic TLSPStreaming<T> = class
  type
    PObject = ^TObject;
  private
    class var Streamer: TLSPStreamer;
    class var DeStreamer: TLSPDeStreamer;
    class procedure GetObject(Sender: TOBject; AObject: TObject;
                              Info: PPropInfo; AData: TJSONObject;
                              DataName: TJSONStringType; var AValue: TObject); static;
  public
    class constructor Create;
    class function ToObject(const JSON: TJSONData): T; overload;
    class function ToObject(const JSON: TJSONStringType): T; overload;
    class function ToJSON(AObject: TObject): TJSONData; overload;
    class function ToJSON(AArray: array of TObject): TJSONData; overload;
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

  generic TLSPRequest<T: TPersistent; U> = class(TCustomJSONRPCHandler)
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

{ NOTE: This is copied from `fpjsonrtti.pas` until the library supports streaming dynamic arrays }
function TLSPStreamer.ObjectToJSON(Const AObject: TObject): TJSONObject;
Var
  PIL : TPropInfoList;
  PD : TJSONData;
  I : Integer;
begin
  Result:=Nil;
  If (AObject=Nil) then
    Exit;
  Result:=TJSONObject.Create;
  try
    If Assigned(BeforeStreamObject) then
      BeforeStreamObject(Self,AObject,Result);
    If AObject is TStrings then
      Result.Add('Strings',StreamTStrings(Tstrings(AObject)))
    else If AObject is TCollection then
      Result.Add('Items',StreamCollection(TCollection(AObject)))
    else If AObject is TObjectList then
      Result.Add('Objects',StreamObjectList(TObjectList(AObject)))
    else if (jsoStreamTlist in Options) and (AObject is TList) then
      Result.Add('Objects', StreamTList(TList(AObject)))
    else
      begin
      PIL:=TPropInfoList.Create(AObject,tkProperties);
      try
        For I:=0 to PIL.Count-1 do
          begin
          PD:=StreamProperty(AObject,PIL.Items[i]);
            If (PD<>Nil) then begin
              if jsoLowerPropertyNames in Options then
                Result.Add(LowerCase(PIL.Items[I]^.Name),PD)
              else
            Result.Add(PIL.Items[I]^.Name,PD);
          end;
          end;
      finally
        FReeAndNil(Pil);
      end;
      // TODO: StreamChildren is private so we can't support it
      If (jsoStreamChildren in Options) and (AObject is TComponent) then
        begin
          //Result.Add(ChildProperty,StreamChildren(TComponent(AObject)));
          writeln(StdErr, 'jsoStreamChildren not supported');
          Flush(StdErr);
          Halt(-1);
        end;
      If Assigned(AfterStreamObject) then
        AfterStreamObject(Self,AObject,Result);
      end;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TLSPStreamer.StreamProperty(Const AObject: TObject; PropertyInfo: PPropInfo): TJSONData;
type
  TVariantArray = array of Variant;
  TObjectArray = array of TObject;
var
  PropArray: TJSONArray;
  ElementType: PTypeInfo;
  VariantArray: TVariantArray;
  ObjectArray: TObjectArray;
  i: integer;
begin
  Result := nil;

  if PropertyInfo^.PropType^.Kind = tkDynArray then
    begin
      // Class kinds are in ElType2 (not sure why)
      ElementType := GetTypeData(PropertyInfo^.PropType)^.ElType;
      if ElementType = nil then
        ElementType := GetTypeData(PropertyInfo^.PropType)^.ElType2;

      PropArray := TJSONArray.Create;

      case ElementType^.Kind of
        tkVariant:
          begin
            VariantArray := TVariantArray(GetDynArrayProp(AObject, PropertyInfo));
            for i := 0 to High(VariantArray) do
              PropArray.Add(StreamVariant(VariantArray[i]));
            Result := PropArray;
          end;
        tkClass:
          begin
            ObjectArray := TObjectArray(GetDynArrayProp(AObject, PropertyInfo));
            for i := 0 to High(ObjectArray) do
              PropArray.Add(StreamClassProperty(ObjectArray[i]));
            result := PropArray;
          end;
        otherwise
          raise EUnknownErrorCode.Create('Dynamic array element type "'+Integer(ElementType^.Kind).ToString+'" is not supported for responses.');
      end;
    end
  else
    Result := inherited StreamProperty(AObject, PropertyInfo);
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
            C := GetTypeData(ElementType)^.ClassType;

            SetLength(ObjectArray, PropArray.Count);
            for I := 0 to PropArray.Count - 1 do
              if PropArray[I] is TJSONObject then
                begin
                  ObjectArray[I] := C.Create;
                  JSONToObject(TJSONObject(PropArray[I]), ObjectArray[I]);
                end
              else
                raise EUnknownErrorCode.Create('Dynamic array element class must be TJSONObject.');

            SetDynArrayProp(AObject, PropInfo, Pointer(ObjectArray));
          end;
        otherwise
          raise EUnknownErrorCode.Create('Dynamic array element type "'+Integer(ElementType^.Kind).ToString+'" is not supported for responses.');
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
  DeStreamer.JSONToObject(JSON as TJSONObject, PObject(@Result)^);
  PObject(@Result)^.AutoRelease;
end;

class function TLSPStreaming.ToObject(const JSON: TJSONStringType): T;
begin
  Result := T.Create;
  DeStreamer.JSONToObject(JSON, PObject(@Result)^);
  PObject(@Result)^.AutoRelease;
end;

class function TLSPStreaming.ToJSON(AObject: TObject): TJSONData;
begin
  if AObject.InheritsFrom(TCollection) then
    Result := Streamer.StreamCollection(TCollection(AObject))
  else
    Result := Streamer.ObjectToJSON(AObject);
end;

class function TLSPStreaming.ToJSON(AArray: array of TObject): TJSONData;
var
  AObject: TObject;
  JArray: TJSONArray;
begin
  JArray := TJSONArray.Create;
  for AObject in AArray do
    JArray.Add(Streamer.ObjectToJSON(AObject));
  result := JArray;
end;

{ TLSPProcessor }

class function TLSPProcessor.Process(AProcess: specialize TLSPProcess<T, U>; const Params: TJSONData): TJSONData;
var
  Input: T;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);
  Result := specialize TLSPStreaming<U>.ToJSON(AProcess(Input));
end;

{ TLSPRequest }

function TLSPRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
type
//  TObjectArray = array of TObject;
  PObjectArray = ^TObjectArray;
  PObject = ^TObject;
var
  Input: T;
  Output: U;
  Streamer: TLSPStreamer;
  ObjectArray: TObjectArray;
  AObject: TObject;
  ArrayTypeInfo: PTypeInfo;
  ElementType: PTypeInfo;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);
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
            Result := specialize TLSPStreaming<U>.ToJSON(PObjectArray(@Output)^);

            // Free all objects
            for AObject in PObjectArray(@Output)^ do
              AObject.Free;
          end;
        otherwise
          raise EUnknownErrorCode.Create('Dynamic array element type "'+Integer(ElementType^.Kind).ToString+'" is not supported for responses.');
      end;
    end
  else
    begin
      Result := specialize TLSPStreaming<U>.ToJSON(PObject(@Output)^);
      PObject(@Output)^.Free;
    end;
  
  if Result = nil then
    Result := TJSONNull.Create;
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
