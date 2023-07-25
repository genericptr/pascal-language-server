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
unit PasLS.Workspace;

{$mode objfpc}{$H+}

interface

uses
  { RTL}
  Classes, SysUtils, fpJSON,
  { LSP Protocol }
  LSP.Base, LSP.Basic, LSP.BaseTypes, LSP.General, LSP.DocumentSymbol, fpjsonrpc,
  PasLS.Settings, PasLS.Symbols, LSP.Streaming, LSP.Workspace;

Type
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
  { TWorkspaceSymbolRequest }

  { The workspace symbol request is sent from the client to the server to
    list project-wide symbols matching the query string. }

  TWorkspaceSymbolRequest = class(specialize TLSPRequest<TWorkspaceSymbolParams, TSymbolInformationItems>)
    function DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData; override;
  end;
  { TDidChangeConfiguration }

  { A notification sent from the client to the server to signal the change of configuration settings. }

  TDidChangeConfiguration = class(specialize TLSPNotification<TDidChangeConfigurationParams>)
    procedure Process(var Params: TDidChangeConfigurationParams); override;
  end;

  { TWorkspaceApplyEditRequest
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit

    The `workspace/applyEdit` request is sent from the server to the client to
    modify resource on the client side. }

  TWorkspaceApplyEditRequest = class(specialize TLSPOutgoingRequest<TApplyWorkspaceEditParams>);

implementation

{ TDidChangeWorkspaceFolders }

procedure TDidChangeWorkspaceFolders.Process(var Params : TDidChangeWorkspaceFoldersParams);
begin
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

{ TDidChangeConfiguration }

procedure TDidChangeConfiguration.Process(var Params: TDidChangeConfigurationParams);
begin
end;


end.

