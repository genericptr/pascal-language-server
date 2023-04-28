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
  Classes, 
  { Code Tools }
  CodeToolManager, CodeCache,
  { Protocol }
  LSP.Base, LSP.Basic;

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

  TShowMessageParams = class(TPersistent)
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

  { TShowMessageRequstParams }

  TShowMessageRequstParams = class(TShowMessageParams)
  private
    fActions: TMessageActionItems;
  published
    // The message action items to present.
    property actions: TMessageActionItems read fActions write fActions;
  end;

implementation

{ TShowMessageNotification }

constructor TShowMessageNotification.Create(_type: TMessageType; Message: String);
begin
  params := TShowMessageParams.Create;
  TShowMessageParams(params).&type := _type;
  TShowMessageParams(params).message := Message;
  method := 'window/showMessage';
end;

destructor TShowMessageNotification.Destroy;
begin
  params.Free;
  inherited;
end;

end.