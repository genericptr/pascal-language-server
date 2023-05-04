// Pascal Language Server
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

unit LSP.Workspace;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, fpjson, fpjsonrpc,
  { Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.General, LSP.DocumentSymbol,
  PasLS.Settings, PasLS.Symbols, LSP.Streaming;

type
  
  { TWorkspaceFoldersChangeEvent }

  TWorkspaceFoldersChangeEvent = class(TLSPStreamable)
  private
    fAdded: TWorkspaceFolderItems;
    fRemoved: TWorkspaceFolderItems;
  Public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // The array of added workspace folders
    property added: TWorkspaceFolderItems read fAdded write fAdded;
    // The array of the removed workspace folders
    property removed: TWorkspaceFolderItems read fRemoved write fRemoved;
  end;

  { TDidChangeWorkspaceFoldersParams }

  TDidChangeWorkspaceFoldersParams = class(TLSPStreamable)
  private
    fEvent: TWorkspaceFoldersChangeEvent;
    procedure SetEvent(AValue: TWorkspaceFoldersChangeEvent);
  Public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property event: TWorkspaceFoldersChangeEvent read fEvent write SetEvent;
  end;

  { TDidChangeWorkspaceFolders }

  { The workspace/didChangeWorkspaceFolders notification is sent from the client to the server 
    to inform the server about workspace folder configuration changes. The notification is sent 
    by default if both client capability workspace.workspaceFolders and the server capability 
    workspace.workspaceFolders.supported are true; or if the server has registered itself to 
    receive this notification. To register for the workspace/didChangeWorkspaceFolders send 
    a client/registerCapability request from the server to the client. The registration parameter 
    must have a registrations item of the following form, where id is a unique id used to 
    unregister the capability (the example uses a UUID): } 

  TDidChangeWorkspaceFolders = class(specialize TLSPNotification<TDidChangeWorkspaceFoldersParams>)
    procedure Process(var Params : TDidChangeWorkspaceFoldersParams); override;
  end;

  { The parameters of a Workspace Symbol Request. }

  { TWorkspaceSymbolParams }

  TWorkspaceSymbolParams = class(TLSPStreamable)
  private
    fQuery: string;
  Public
    Procedure Assign(Source: TPersistent); override;
  published
    // A query string to filter symbols by. Clients may send an empty
    // string here to request all symbols.
    property query: string read fQuery write fQuery;
  end;

  { TWorkspaceSymbolRequest }

  { The workspace symbol request is sent from the client to the server to 
    list project-wide symbols matching the query string. }

  TWorkspaceSymbolRequest = class(specialize TLSPRequest<TWorkspaceSymbolParams, TSymbolInformationItems>)
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
  end;

  { TDidChangeConfigurationParams }

  TDidChangeConfigurationParams = class(TLSPStreamable)
  private
    fSettings: TServerSettings;
    procedure SetSettings(AValue: TServerSettings);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    property settings: TServerSettings read fSettings write SetSettings;
  end;

  { TDidChangeConfiguration }

  { A notification sent from the client to the server to signal the change of configuration settings. }

  TDidChangeConfiguration = class(specialize TLSPNotification<TDidChangeConfigurationParams>)
    procedure Process(var Params: TDidChangeConfigurationParams); override;
  end;

  { TApplyWorkspaceEditParams }

  TApplyWorkspaceEditParams = class(TLSPStreamable)
  private
    fLabel: TOptionalString;
    fEdit: TWorkspaceEdit;
    procedure SetEdit(AValue: TWorkspaceEdit);
    procedure SetLabel(AValue: TOptionalString);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
  published
    // An optional label of the workspace edit. This label is
    // presented in the user interface for example on an undo
    // stack to undo the workspace edit.
    // Once set, the instance owns the value
    property &label: TOptionalString read fLabel write SetLabel;
    //  The edits to apply.
    property edit: TWorkspaceEdit read fEdit write SetEdit;
  end;

  { TApplyWorkspaceEditResult
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#applyWorkspaceEditResult }

  TApplyWorkspaceEditResult = class(TLSPStreamable)
  private
    fApplied: boolean;
    fFailureReason: TOptionalString;
    fFailedChange: TOptionalNumber;
  Public
    Destructor Destroy; override;
  published
    // Indicates whether the edit was applied or not.
    property applied: Boolean read fApplied write fApplied;
    // An optional textual description for why the edit was not applied.
    // This may be used by the server for diagnostic logging or to provide
    // a suitable error for a request that triggered the edit.
    property failureReason: TOptionalString read fFailureReason write fFailureReason;
    // Depending on the client's failure handling strategy `failedChange`
    // might contain the index of the change that failed. This property is
    // only available if the client signals a `failureHandling` strategy
    // in its client capabilities.
    property failedChange: TOptionalNumber read fFailedChange write fFailedChange;
  end;

  { TWorkspaceApplyEditRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit
    
    The `workspace/applyEdit` request is sent from the server to the client to 
    modify resource on the client side. }

  TWorkspaceApplyEditRequest = class(specialize TLSPOutgoingRequest<TApplyWorkspaceEditParams>);

implementation
uses
  SysUtils, DateUtils;

{ TDidChangeConfiguration }

procedure TDidChangeConfiguration.Process(var Params: TDidChangeConfigurationParams);
begin
end;

{ TApplyWorkspaceEditParams }

procedure TApplyWorkspaceEditParams.SetEdit(AValue: TWorkspaceEdit);
begin
  if fEdit=AValue then Exit;
  fEdit.Assign(AValue);
end;

procedure TApplyWorkspaceEditParams.SetLabel(AValue: TOptionalString);
begin
  if fLabel=AValue then Exit;
  FreeAndNil(fLabel);
  fLabel:=AValue;
end;

constructor TApplyWorkspaceEditParams.Create;
begin
  inherited Create;
  fEdit:=TWorkspaceEdit.Create;
end;

destructor TApplyWorkspaceEditParams.Destroy;
begin
  &Label:=Nil;
  edit:=Nil;
  inherited Destroy;
end;

procedure TApplyWorkspaceEditParams.Assign(Source: TPersistent);
var
  Src: TApplyWorkspaceEditParams absolute source;
begin
  if Source is TApplyWorkspaceEditParams then
    begin
    Edit.Assign(Src.Edit);
    if Assigned(Src.&Label) then
      fLabel.Value:=Src.&Label.Value;
    end
  else
    inherited Assign(Source);
end;

{ TApplyWorkspaceEditResult }

destructor TApplyWorkspaceEditResult.Destroy;
begin
  FreeAndnil(fFailureReason);
  FreeAndnil(fFailedChange);
  inherited Destroy;
end;

{ TWorkspaceFoldersChangeEvent }

constructor TWorkspaceFoldersChangeEvent.Create;
begin
  inherited Create;
  fAdded:=TWorkspaceFolderItems.Create;
  fRemoved:=TWorkspaceFolderItems.Create;
end;

destructor TWorkspaceFoldersChangeEvent.Destroy;
begin
  FreeAndNil(fAdded);
  FreeAndNil(fRemoved);
  inherited Destroy;
end;

procedure TWorkspaceFoldersChangeEvent.Assign(Source: TPersistent);
var
  Src: TWorkspaceFoldersChangeEvent absolute Source;
begin
  if Source is TWorkspaceFoldersChangeEvent then
    begin
      Added.Assign(Src.Added);
      Removed.Assign(Src.Removed);
    end
  else
    inherited Assign(Source);
end;

{ TDidChangeWorkspaceFoldersParams }

procedure TDidChangeWorkspaceFoldersParams.SetEvent(
  AValue: TWorkspaceFoldersChangeEvent);
begin
  if fEvent=AValue then Exit;
  fEvent.Assign(AValue);
end;

constructor TDidChangeWorkspaceFoldersParams.Create;
begin
  inherited Create;
  fEvent:=TWorkspaceFoldersChangeEvent.Create;
end;

destructor TDidChangeWorkspaceFoldersParams.Destroy;
begin
  FreeAndNil(fEvent);
  inherited Destroy;
end;

procedure TDidChangeWorkspaceFoldersParams.Assign(Source: TPersistent);
var
  Src: TDidChangeWorkspaceFoldersParams absolute Source;
begin
  if Source is TDidChangeWorkspaceFoldersParams then
    begin
      Event.Assign(Src.event)
    end
  else
    inherited Assign(Source);
end;

{ TDidChangeWorkspaceFolders }

procedure TDidChangeWorkspaceFolders.Process(var Params : TDidChangeWorkspaceFoldersParams);
begin
end;

{ TWorkspaceSymbolParams }

procedure TWorkspaceSymbolParams.Assign(Source: TPersistent);
var
  Src: TWorkspaceSymbolParams;

begin
  if Source is TWorkspaceSymbolParams then
    begin
      Query:=Src.query;
    end
  else
    inherited Assign(Source);
end;

{ TWorkspaceSymbolRequest }

function TWorkspaceSymbolRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: TWorkspaceSymbolParams;

begin
  Input := specialize TLSPStreaming<TWorkspaceSymbolParams>.ToObject(Params);
  Result := SymbolManager.FindWorkspaceSymbols(Input.query);
  if not Assigned(Result) then
    Result := TJSONNull.Create;
end;

{ TDidChangeConfigurationParams }

procedure TDidChangeConfigurationParams.SetSettings(AValue: TServerSettings);
begin
  if fSettings=AValue then Exit;
  fSettings.Assign(AValue);
end;

constructor TDidChangeConfigurationParams.Create;
begin
  inherited Create;
  fSettings:=TServerSettings.Create;
end;

destructor TDidChangeConfigurationParams.Destroy;
begin
  FreeAndNil(FSettings);
  inherited Destroy;
end;

procedure TDidChangeConfigurationParams.Assign(Source: TPersistent);
var
  Src: TDidChangeConfigurationParams absolute source;
begin
  if Source is TDidChangeConfigurationParams then
    begin
       Settings.Assign(Src.settings);
    end
  else
    inherited Assign(Source);
end;


initialization
  LSPHandlerManager.RegisterHandler('workspace/didChangeConfiguration', TDidChangeConfiguration);
  LSPHandlerManager.RegisterHandler('workspace/didChangeWorkspaceFolders', TDidChangeWorkspaceFolders);
  LSPHandlerManager.RegisterHandler('workspace/symbol', TWorkspaceSymbolRequest);
end.
