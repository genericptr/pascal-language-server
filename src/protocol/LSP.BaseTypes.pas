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

