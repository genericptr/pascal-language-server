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
  LSP.Base, LSP.Basic, LSP.General, LSP.DocumentSymbol, 
  Settings, Symbols;

type
  
  { TWorkspaceFoldersChangeEvent }

  TWorkspaceFoldersChangeEvent = class(TPersistent)
  private
    fAdded: TWorkspaceFolderItems;
    fRemoved: TWorkspaceFolderItems;
  published
    // The array of added workspace folders
    property added: TWorkspaceFolderItems read fAdded write fAdded;
    // The array of the removed workspace folders
    property removed: TWorkspaceFolderItems read fRemoved write fRemoved;
  end;

  { TDidChangeWorkspaceFoldersParams }

  TDidChangeWorkspaceFoldersParams = class(TPersistent)
  private
    fEvent: TWorkspaceFoldersChangeEvent;
  published
    property event: TWorkspaceFoldersChangeEvent read fEvent write fEvent;
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

  TWorkspaceSymbolParams = class(TPersistent)
  private
    fQuery: string;
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

  TDidChangeConfigurationParams = class(TPersistent)
  private
    fSettings: TServerSettings;
  published
    property settings: TServerSettings read fSettings write fSettings;
  end;

  { TDidChangeConfiguration }

  { A notification sent from the client to the server to signal the change of configuration settings. }

  TDidChangeConfiguration = class(specialize TLSPNotification<TDidChangeConfigurationParams>)
    procedure Process(var Params: TDidChangeConfigurationParams); override;
  end;

  { TApplyWorkspaceEditParams }

  TApplyWorkspaceEditParams = class(TPersistent)
  private
    fLabel: TOptionalString;
    fEdit: TWorkspaceEdit;
  published
    // An optional label of the workspace edit. This label is
    // presented in the user interface for example on an undo
    // stack to undo the workspace edit.
    property &label: TOptionalString read fLabel write fLabel;
    //  The edits to apply.
    property edit: TWorkspaceEdit read fEdit write fEdit;
  end;

  { TApplyWorkspaceEditResult
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#applyWorkspaceEditResult }

  TApplyWorkspaceEditResult = class(TPersistent)
  private
    fApplied: boolean;
    fFailureReason: TOptionalString;
    fFailedChange: TOptionalNumber;
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

  TWorkspaceApplyEditRequest = class(specialize TLSPOutgoingRequest<TApplyWorkspaceEditParams, TApplyWorkspaceEditResult>);

implementation
uses
  SysUtils, DateUtils;

{ TDidChangeConfiguration }

procedure TDidChangeConfiguration.Process(var Params: TDidChangeConfigurationParams);
begin
end;

{ TDidChangeWorkspaceFolders }

procedure TDidChangeWorkspaceFolders.Process(var Params : TDidChangeWorkspaceFoldersParams);
begin
end;

{ TWorkspaceSymbolRequest }

function TWorkspaceSymbolRequest.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;
var
  Input: TWorkspaceSymbolParams;
  StartTime: TDateTime;
begin
  Input := specialize TLSPStreaming<TWorkspaceSymbolParams>.ToObject(Params);
  StartTime := Now;
  Result := SymbolManager.FindWorkspaceSymbols(Input.query);
  Flush(stderr);

  if not Assigned(Result) then
    Result := TJSONNull.Create;
end;

initialization
  LSPHandlerManager.RegisterHandler('workspace/didChangeConfiguration', TDidChangeConfiguration);
  LSPHandlerManager.RegisterHandler('workspace/didChangeWorkspaceFolders', TDidChangeWorkspaceFolders);
  LSPHandlerManager.RegisterHandler('workspace/symbol', TWorkspaceSymbolRequest);
end.