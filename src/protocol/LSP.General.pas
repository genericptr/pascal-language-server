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

unit LSP.General;

{$mode objfpc}{$H+}
{$modeswitch arrayoperators}

interface

uses
  { RTL }
  SysUtils, Classes, typinfo,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.Capabilities;

type

  { TWorkspaceFolder }

  TWorkspaceFolder = class(TCollectionItem)
  private
    fUri: TDocumentUri;
    fName: String;
  Public
    Procedure assign(Source : TPersistent); override;
  published
    // The associated URI for this workspace folder.
    property uri: TDocumentUri read fUri write fUri;
    // The name of the workspace folder. Used to refer to this
    // workspace folder in the user interface.
    property name: string read fName write fName;
  end;
  TWorkspaceFolderItems = specialize TGenericCollection<TWorkspaceFolder>;

  { TVoidParams }

  TVoidParams = class(TLSPStreamable);

  { TClientInfo }

  TClientInfo = class(TLSPStreamable)
  private
    fName: string;
    fVersion: string;
  Public
    Procedure Assign(aSource : TPersistent); override;
  published
    // The name of the client as defined by the client.
    property name: string read fName write fName;
    // The client's version as defined by the client.
    property version: string read fVersion write fVersion;
  end;



  { TInitializationOptions }

  TInitializationOptions = class(TLSPStreamable)
  end;

  { TInitializeParams }

  TInitializeParams = class(TLSPStreamable)
  private
    fProcessId: integer;
    fClientInfo: TClientInfo;
    fRootUri: string;
    fCapabilities: TClientCapabilities;
    fTrace: string;
    fInitializationOptions: TInitializationOptions;
    fWorkspaceFolders: TWorkspaceFolderItems;
    procedure SetCapabilities(AValue: TClientCapabilities);
    procedure SetClientInfo(AValue: TClientInfo);
    procedure SetInitializationOptions(AValue: TInitializationOptions);
    procedure SetWorkspaceFolders(AValue: TWorkspaceFolderItems);
  protected
    function createInitializationOptions : TInitializationOptions; virtual;
    function createClientInfo: TClientInfo; virtual;
    function createClientCapabilities : TClientCapabilities; virtual;
    function CreateWorkspaceFolders: TWorkspaceFolderItems; virtual;
  published
    // The process Id of the parent process that started
    // the server. Is null if the process has not been started by another process.
    // If the parent process is not alive then the server should exit (see exit notification) its process.
    property processId: integer read fProcessId write fProcessId;
    // Information about the client
    property clientInfo: TClientInfo read fClientInfo write SetClientInfo;
    // The rootUri of the workspace. Is null if no
    // folder is open. If both `rootPath` and `rootUri` are set
    // `rootUri` wins.
    property rootUri: string read fRootUri write fRootUri;
    // User provided initialization options.
    property initializationOptions: TInitializationOptions read fInitializationOptions write SetInitializationOptions;
    // The capabilities provided by the client (editor or tool)
    property capabilities: TClientCapabilities read fCapabilities write SetCapabilities;
    // The initial trace setting. If omitted trace is disabled ('off').
    property trace: string read fTrace write fTrace;
    // The workspace folders configured in the client when the server starts.
    // This property is only available if the client supports workspace folders.
    // It can be `null` if the client supports workspace folders but none are
    // configured.
    property workspaceFolders: TWorkspaceFolderItems read fWorkspaceFolders write SetWorkspaceFolders;
  public
    constructor create; override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  end;

  { TInitializeResult }

  TInitializeResult = class(TLSPStreamable)
  private
    fCapabilities: TServerCapabilities;
    procedure SetCapabilities(AValue: TServerCapabilities);
  protected
    function createCapabilities: TServerCapabilities; virtual;
  Public
    constructor Create; override;
    destructor Destroy; override;
  published
    property capabilities: TServerCapabilities read fCapabilities write SetCapabilities;
  end;



  { TCancelParams }

  TCancelParams = class(TLSPStreamable)
  private
    fId: Integer;
  published
    property id: Integer read fId write fId;
  end;


implementation


{ TWorkspaceFolder }

procedure TWorkspaceFolder.assign(Source : TPersistent);

var
  WF : TWorkspaceFolder absolute source;

begin
  if source is TWorkspaceFolder then
    begin
    Uri:=wf.uri;
    Name:=wf.name;

    end
  else
    inherited assign(Source);
end;

{ TInitializeParams }

procedure TInitializeParams.SetCapabilities(AValue: TClientCapabilities);
begin
  if fCapabilities=AValue then Exit;
  fCapabilities.Assign(AValue);
end;

procedure TInitializeParams.SetClientInfo(AValue: TClientInfo);
begin
  if fClientInfo=AValue then Exit;
  fClientInfo.Assign(AValue);
end;

procedure TInitializeParams.SetInitializationOptions(
  AValue: TInitializationOptions);
begin
  if fInitializationOptions=AValue then Exit;
  fInitializationOptions.Assign(AValue);
end;

procedure TInitializeParams.SetWorkspaceFolders(AValue: TWorkspaceFolderItems);
begin
  if fWorkspaceFolders=AValue then Exit;
  fWorkspaceFolders.Assign(AValue);
end;

function TInitializeParams.createInitializationOptions: TInitializationOptions;
begin
  Result := TInitializationOptions.Create;
end;

function TInitializeParams.createClientInfo: TClientInfo;
begin
  Result := TClientInfo.Create;
end;

function TInitializeParams.createClientCapabilities: TClientCapabilities;
begin
  Result := TClientCapabilities.Create;
end;

function TInitializeParams.CreateWorkspaceFolders: TWorkspaceFolderItems;
begin
  Result := TWorkspaceFolderItems.Create
end;

constructor TInitializeParams.create;
begin
  inherited;
  fclientInfo := CreateClientInfo;
  finitializationOptions := createInitializationOptions;
  fworkspaceFolders := CreateWorkspaceFolders;
  fCapabilities:= createClientCapabilities;
end;


destructor TInitializeParams.Destroy; 
begin
  FreeAndNil(fCapabilities);
  FreeAndNil(finitializationOptions);
  FreeAndNil(fclientInfo);
  FreeAndNil(fworkspaceFolders);
  inherited;
end;

procedure TInitializeParams.Assign(Source : TPersistent);

var
  Src : TInitializeParams absolute Source;

begin
  if Source is TInitializeParams then
    begin
    processId:=Src.ProcessID;
    clientInfo:=Src.ClientInfo;
    rootUri:=Src.RootUri;
    initializationOptions:=Src.initializationOptions;
    capabilities:=Src.Capabilities;
    trace:=Src.Trace;
    workspaceFolders:=Src.workspaceFolders;
    end
  else
    inherited Assign(Source);
end;

{ TInitializeResult }

procedure TInitializeResult.SetCapabilities(AValue: TServerCapabilities);
begin
  if fCapabilities=AValue then Exit;
  fCapabilities.Assign(AValue);
end;

function TInitializeResult.createCapabilities: TServerCapabilities;
begin
  Result := TServerCapabilities.Create()
end;

constructor TInitializeResult.Create;
begin
  fCapabilities := createCapabilities;
end;

destructor TInitializeResult.Destroy;
begin
  FreeAndNil(fCapabilities);
  inherited Destroy;
end;

{ TClientInfo }

procedure TClientInfo.Assign(aSource : TPersistent);

var
  Src : TClientInfo absolute aSource;

begin
  if aSource is TClientInfo then
    begin
    Name:=Src.name;
    Version:=Src.version;
    end
  else
    inherited Assign(aSource);
end;

end.

