// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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
  Classes, SysUtils, TypInfo, fpjson, fpjsonrtti, fpjsonrpc;

type

  { TLSPStreaming }

  generic TLSPStreaming<T: TPersistent> = class
  private
    class var JSONStreamer: TJSONStreamer;
    class var JSONDeStreamer: TJSONDeStreamer;
    class procedure GetObject(Sender: TOBject; AObject: TObject; Info: PPropInfo;
      AData: TJSONObject; DataName: TJSONStringType; var AValue: TObject); static;
  public
    class constructor Create;
    class function ToObject(const JSON: TJSONData): T; static;
    class function ToJSON(AObject: TObject): TJSONData; static;
  end;

  { TLSPProcessor }

  generic TLSPProcess<T, U> = function (var Params : T): U;

  generic TLSPProcessor<T, U: TPersistent> = class
  public
    class function Process(AProcess: specialize TLSPProcess<T, U>; const Params: TJSONData): TJSONData; static;
  end;

  { TLSPRequest }

  generic TLSPRequest<T, U: TPersistent> = class(TCustomJSONRPCHandler)
  protected
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
    function Process(var Params : T): U; virtual; abstract;
  end;

  { TLSPNotification }

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

implementation

{ TLSPStreaming }

class procedure TLSPStreaming.GetObject(Sender: TOBject; AObject: TObject; Info: PPropInfo;
  AData: TJSONObject; DataName: TJSONStringType; var AValue: TObject);
var
  C: TClass;
begin
  C := GetTypeData(Info^.PropType)^.ClassType;
  if C.InheritsFrom(TPersistent) then
    AValue := C.Create;
end;

{ RegisterLSPHandler }

class constructor TLSPStreaming.Create;
begin
  JSONStreamer := TJSONStreamer.Create(nil);
  JSONStreamer.Options := JSONStreamer.Options +
    [jsoEnumeratedAsInteger, jsoSetEnumeratedAsInteger, jsoTStringsAsArray];
  JSONDeStreamer := TJSONDeStreamer.Create(nil);
  JSONDeStreamer.OnGetObject := @GetObject;
end;

class function TLSPStreaming.ToObject(const JSON: TJSONData): T;
begin
  Result := T.Create;
  JSONDeStreamer.JSONToObject(JSON as TJSONObject, Result);
end;

class function TLSPStreaming.ToJSON(AObject: TObject): TJSONData;
begin
  if AObject.InheritsFrom(TCollection) then
    Result := JSONStreamer.StreamCollection(TCollection(AObject))
  else
    Result := JSONStreamer.ObjectToJSON(AObject);
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
var
  Input: T;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);
  Result := specialize TLSPStreaming<U>.ToJSON(Process(Input));
  if not Assigned(Result) then Result := TJSONNull.Create;
end;

{ TLSPNotification }

function TLSPNotification.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: T;
begin
  Input := specialize TLSPStreaming<T>.ToObject(Params);
  Process(Input);
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

