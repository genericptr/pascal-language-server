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

unit LSP.Window;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  SysUtils, Classes,
  { Code Tools }
  CodeToolManager,
  { Protocol }
  LSP.Base, LSP.BaseTypes, LSP.Messages;

type
  
  { TMessageType }

  TMessageType = (
    __UNUSED__,
    Error,   // An error message.
    Warning, // A warning message.
    Info,    // An information message.
    Log      // A log message.
  );

  { TShowMessageParams }

  TShowMessageParams = class(TLSPStreamable)
  private
    fType: TMessageType;
    fMessage: string;
  published
    // The message type.
    property &type: TMessageType read fType write fType;
    // The actual message.
    property message: string read fMessage write fMessage;
  end;

  { TShowMessageNotification
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#window_showMessage
    
    The show message notification is sent from a server to a client to ask 
    the client to display a particular message in the user interface. }

  TShowMessageNotification = class(TNotificationMessage)
  public
    constructor Create; override;
    constructor Create(_type: TMessageType; Message: String);
    destructor Destroy; override;
  end;

  TMessageActionItem = class(TCollectionItem)
  private
    fTitle: string;
  published
    // A short title like 'Retry', 'Open Log' etc.
    property title: string read fTitle write fTitle;
  end;

  TMessageActionItems = specialize TGenericCollection<TMessageActionItem>;

  { TShowMessageRequestParams }

  TShowMessageRequestParams = class(TShowMessageParams)
  private
    fActions: TMessageActionItems;
    procedure SetActions(AValue: TMessageActionItems);
  published
    Constructor Create; override;
    Destructor Destroy; override;
    // The message action items to present.
    property actions: TMessageActionItems read fActions write SetActions;
  end;

implementation

{ TShowMessageNotification }

constructor TShowMessageNotification.Create;
begin
  inherited Create;
  params := TShowMessageParams.Create;
end;

constructor TShowMessageNotification.Create(_type: TMessageType; Message: String);
begin
  Create;
  TShowMessageParams(params).&type := _type;
  TShowMessageParams(params).message := Message;
  method := 'window/showMessage';
end;

destructor TShowMessageNotification.Destroy;
begin
  params.Free;
  inherited;
end;

{ TShowMessageRequstParams }

procedure TShowMessageRequestParams.SetActions(AValue: TMessageActionItems);
begin
  if fActions=AValue then Exit;
  fActions.Assign(AValue);
end;

constructor TShowMessageRequestParams.Create;
begin
  inherited Create;
  fActions:=TMessageActionItems.Create;
end;

destructor TShowMessageRequestParams.Destroy;
begin
  FreeAndNil(fActions);
  inherited Destroy;
end;

end.
