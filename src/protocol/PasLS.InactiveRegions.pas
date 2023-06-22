// Pascal Language Server
// Copyright 2023 Michael Van Canneyt

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

unit PasLS.InactiveRegions;

{$mode objfpc}{$H+}

interface

uses
  { Rtl }
  SysUtils, Classes,
  { Pasls }
  LSP.Base, LSP.BaseTypes, LSP.Messages;

type
  { TInputRegion }
  TInputRegion = class(TCollectionItem)
  private
    fStartline:Integer;
    fStartCol:Integer;
    fEndline:Integer;
    fEndCol:Integer;
  Public
    Procedure Assign(aSource : TPersistent); override;
  published
    property startLine: Integer read fStartline write fStartline;
    property startCol: Integer read fStartCol write fStartCol;
    property endLine: Integer read fEndline write fEndline;
    property endCol: Integer read fEndCol write fEndCol;
  end;
   
  TRegionsItems = specialize TGenericCollection<TInputRegion>;

  { TInactiveRegionParams }
  
  TInactiveRegionParams=class(TLSPStreamable)
  private
    fUri:string;
    fFileVersion:Integer;
    fRegions:TRegionsItems;
    procedure SetRegions(AValue: TRegionsItems);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
  published
    property uri : string read fUri write fUri;
    property fileVersion : Integer read fFileVersion write fFileVersion;
    property regions : TRegionsItems read fRegions write SetRegions;
  end;


  { TInactiveRegionsNotification }
  { The  message notification is sent from a server to a client to ask 
    the client to display a inactive region in the user interface. }

  TInactiveRegionsNotification = class(TNotificationMessage)
  private
    function GetParams: TInactiveRegionParams;
  public
    constructor Create; override;
    Property InactiveRegionParams: TInactiveRegionParams Read GetParams;
    destructor Destroy; override;
  end;

implementation

{ TInputRegion }

procedure TInputRegion.Assign(aSource: TPersistent);

var
  Reg : TInputRegion absolute aSource;

begin
  if (aSource is TInputRegion) then
    begin
    Startline:=Reg.Startline;
    StartCol:=Reg.StartCol;
    Endline:=Reg.EndLine;
    EndCol:=Reg.EndCol;
    end
  else
    inherited Assign(aSource);
end;

{ TInactiveRegionParams }

procedure TInactiveRegionParams.SetRegions(AValue: TRegionsItems);
begin
  if fRegions=AValue then Exit;
  fRegions.Assign(AValue);
end;

constructor TInactiveRegionParams.Create;
begin
  inherited Create;
  fRegions:=TRegionsItems.Create;
end;

destructor TInactiveRegionParams.Destroy;
begin
  FreeAndNil(fRegions);
  inherited Destroy;
end;

function TInactiveRegionsNotification.GetParams: TInactiveRegionParams;
begin
  Result:=(Inherited Params) as TInactiveRegionParams
end;

constructor TInactiveRegionsNotification.Create;
begin
  params := TInactiveRegionParams.Create;
  method := 'pasls.inactiveRegions';
end;

destructor TInactiveRegionsNotification.Destroy;
begin
  FreeAndNil(fparams);
  inherited;
end;

end.
