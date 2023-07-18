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

unit LSP.References;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  SysUtils, Classes, 
  { Protocol }
  LSP.BaseTypes, LSP.Base, LSP.Basic, LSP.Messages;

type

  { TReferenceContext }

  TReferenceContext = class(TLSPStreamable)
  private
    fIncludeDeclaration: boolean;
  published
    // Include the declaration of the current symbol.
    property includeDeclaration: boolean read fIncludeDeclaration write fIncludeDeclaration;
  end;

  { TReferenceParams }
  
  TReferenceParams = class(TTextDocumentPositionParams)
  private
    fContext: TReferenceContext;
    procedure SetContext(AValue: TReferenceContext);
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    property context: TReferenceContext read fContext write SetContext;
  end;



implementation


{ TReferenceParams }

procedure TReferenceParams.SetContext(AValue: TReferenceContext);
begin
  if fContext=AValue then Exit;
  fContext.Assign(AValue);
end;

constructor TReferenceParams.Create;
begin
  inherited Create;
  fContext:=TReferenceContext.Create;
end;

destructor TReferenceParams.Destroy;
begin
  FreeAndNil(fContext);
  inherited Destroy;
end;

procedure TReferenceParams.Assign(Source: TPersistent);
var
  Src: TReferenceParams absolute Source;
begin
  if Source is TReferenceParams then
    begin
      Context.Assign(Src.context);
    end
  else
    inherited Assign(Source);
end;
  
end.

