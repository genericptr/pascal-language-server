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
unit LSP.BaseTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TAnyArray = array of Variant;

  { TOptional }

  generic TOptional<T> = class
  private
    fHasValue: Boolean;
    fValue: T;
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    property HasValue: Boolean read fHasValue;
    property Value: T read GetValue write SetValue;
    procedure Clear;
  end;

  { TOptionalVariantBase }

  TOptionalVariantBase = class(specialize TOptional<Variant>);

  { TOptionalVariant }

  generic TOptionalVariant<T> = class(TOptionalVariantBase)
  private
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    constructor Create; overload;
    constructor Create(AValue: T); overload;
    property Value: T read GetValue write SetValue;
  end;

  { TOptionalObjectBase }

  TOptionalObjectBase = class(specialize TOptional<TObject>)
  public
    function ValueClass: TClass; virtual; abstract;
  end;

  { TOptionalObject }

  generic TOptionalObject<T> = class(TOptionalObjectBase)
  private
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    constructor Create;
    constructor Create(AValue: T);
    function ValueClass: TClass; override;
    property Value: T read GetValue write SetValue;
  end;

  TOptionalBoolean = specialize TOptionalVariant<Boolean>;
  TOptionalString = specialize TOptionalVariant<String>;
  TOptionalInteger = specialize TOptionalVariant<Integer>;
  TOptionalAny = specialize TOptionalVariant<Variant>; // any type except structures (objects or arrays)
  TOptionalNumber = TOptionalInteger;

  { TGenericCollection }

  generic TGenericCollection<T> = class(TCollection)
  private
    function GetI(Index : Integer): T;
    procedure SetI(Index : Integer; AValue: T);
  public
    constructor Create;
    Function Add : T; reintroduce;
    Property Items[Index : Integer] : T Read GetI Write SetI;
  end;

  { TLSPStreamable }

  TLSPStreamable = class(TPersistent)
  Public
    // We need a virtual constructor
    Constructor Create; virtual;
  end;
  TLSPStreamableClass = Class of TLSPStreamable;


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

operator :=(right: Boolean): TOptionalAny;
operator :=(right: Integer): TOptionalAny;
operator :=(right: String): TOptionalAny;

operator :=(right: Boolean): TOptionalBoolean;
operator :=(right: Integer): TOptionalInteger;
operator :=(right: String): TOptionalString;


implementation

{ Utilities }

operator :=(right: Boolean): TOptionalAny;
begin
  result := TOptionalAny.Create(right);
end;

operator :=(right: Integer): TOptionalAny;
begin
  result := TOptionalAny.Create(right);
end;

operator :=(right: String): TOptionalAny;
begin
  result := TOptionalAny.Create(right);
end;

operator :=(right: Boolean): TOptionalBoolean;
begin
  result := TOptionalBoolean.Create(right);
end;

operator :=(right: Integer): TOptionalInteger;
begin
  result := TOptionalInteger.Create(right);
end;

operator :=(right: String): TOptionalString;
begin
  result := TOptionalString.Create(right);
end;


{ TOptional }

function TOptional.GetValue: T;
begin
  if fHasValue then Result := fValue
  else Exception.Create('no value');
end;

procedure TOptional.SetValue(AValue: T);
begin
  fValue := AValue;
  fHasValue := True;
end;

procedure TOptional.Clear;
begin
  fHasValue := False;
end;

{ TOptionalVariant }

function TOptionalVariant.GetValue: T;
begin
  Result := T(inherited Value);
end;

procedure TOptionalVariant.SetValue(AValue: T);
begin
  inherited Value := AValue;
end;

constructor TOptionalVariant.Create;
begin
  inherited Create;
end;

constructor TOptionalVariant.Create(AValue: T);
begin
  Create;
  SetValue(AValue);
end;

{ TOptionalObject }

function TOptionalObject.GetValue: T;
begin
  Result := T(inherited Value);
end;

procedure TOptionalObject.SetValue(AValue: T);
begin
  inherited Value := AValue;
end;

constructor TOptionalObject.Create;
begin
  inherited Create;
end;

constructor TOptionalObject.Create(AValue: T);
begin
  Create;
  SetValue(AValue);
end;

function TOptionalObject.ValueClass: TClass;
begin
  Result := T;
end;

{ TGenericCollection }

function TGenericCollection.GetI(Index : Integer): T;
begin
  Result:=T(Inherited Items[Index]);
end;

procedure TGenericCollection.SetI(Index : Integer; AValue: T);
begin
  Inherited Items[Index]:=aValue;
end;

constructor TGenericCollection.Create;
begin
  inherited Create(T);
end;

function TGenericCollection.Add: T;
begin
  Result:=T(Inherited add);
end;

{ TLSPStreamable }

constructor TLSPStreamable.Create;
begin
  Inherited Create;
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

end.

