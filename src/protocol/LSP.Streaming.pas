unit LSP.Streaming;

{$mode objfpc}{$H+}

interface

uses
  RTTIUtils, fpjson, fpjsonrtti, typinfo, contnrs,
  Classes, SysUtils, LSP.BaseTypes ;

type
  TObjectArray = Array of TObject;



  { TLSPStreamer }

  TLSPStreamer = class(TJSONStreamer)
  protected
    function StreamClassProperty(const AObject: TObject): TJSONData; override;
    function ObjectToJSON(Const AObject: TObject): TJSONObject;
    function StreamProperty(Const AObject: TObject; PropertyInfo: PPropInfo): TJSONData;
  end;

  { TLSPDeStreamer }

  TLSPDeStreamer = class(TJSONDeStreamer)
  protected
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;
                                PropData: TJSONData); override;
  end;

  { TLSPStreaming }

  generic TLSPStreaming<T: TPersistent> = class
  type
    PObject = ^TObject;
  private
    class var Streamer: TLSPStreamer;
    class var DeStreamer: TLSPDeStreamer;
    class procedure GetObject(Sender: TOBject; AObject: TObject;
                              Info: PPropInfo; AData: TJSONObject;
                              DataName: TJSONStringType; var AValue: TObject); static;
  public
    class constructor Create;
    class destructor Done;
    class function ToObject(const JSON: TJSONData): T; overload;
    class function ToObject(const JSON: TJSONStringType): T; overload;
    class function ToJSON(AObject: TObject): TJSONData; overload;
    class function ToJSON(AArray: array of TObject): TJSONData; overload;
  end;

implementation

{ TLSPStreamer }

function TLSPStreamer.StreamClassProperty(const AObject: TObject): TJSONData;
var
  C: TClass;
  OptionalVariant: TOptionalVariantBase;
  OptionalObject: TOptionalObjectBase;
begin
  if not Assigned(AObject) then
    begin
      result := nil;
      Exit;
    end;
  C := AObject.ClassType;
  if C.InheritsFrom(TOptionalVariantBase) then
    begin
      OptionalVariant := TOptionalVariantBase(AObject);
      if OptionalVariant.HasValue then
        Result := StreamVariant(OptionalVariant.Value)
      else
        Result := nil
    end
  else if C.InheritsFrom(TOptionalObjectBase) then
    begin
      OptionalObject := TOptionalObjectBase(AObject);
      if OptionalObject.HasValue then
        if OptionalObject.Value = nil then
          Result := TJSONNull.Create
        else
          Result := ObjectToJSON(OptionalObject.Value)
      else
        Result := nil
    end
  else
    Result := inherited StreamClassProperty(AObject)
end;

{ NOTE: This is copied from `fpjsonrtti.pas` until the library supports streaming dynamic arrays }
function TLSPStreamer.ObjectToJSON(Const AObject: TObject): TJSONObject;
Var
  PIL : TPropInfoList;
  PD : TJSONData;
  I : Integer;
begin
  Result:=Nil;
  If (AObject=Nil) then
    Exit;
  Result:=TJSONObject.Create;
  try
    If Assigned(BeforeStreamObject) then
      BeforeStreamObject(Self,AObject,Result);
    If AObject is TStrings then
      Result.Add('Strings',StreamTStrings(Tstrings(AObject)))
    else If AObject is TCollection then
      Result.Add('Items',StreamCollection(TCollection(AObject)))
    else If AObject is TObjectList then
      Result.Add('Objects',StreamObjectList(TObjectList(AObject)))
    else if (jsoStreamTlist in Options) and (AObject is TList) then
      Result.Add('Objects', StreamTList(TList(AObject)))
    else
      begin
      PIL:=TPropInfoList.Create(AObject,tkProperties);
      try
        For I:=0 to PIL.Count-1 do
          begin
          PD:=StreamProperty(AObject,PIL.Items[i]);
            If (PD<>Nil) then begin
              if jsoLowerPropertyNames in Options then
                Result.Add(LowerCase(PIL.Items[I]^.Name),PD)
              else
            Result.Add(PIL.Items[I]^.Name,PD);
          end;
          end;
      finally
        FreeAndNil(Pil);
      end;
      // TODO: StreamChildren is private so we can't support it
      If (jsoStreamChildren in Options) and (AObject is TComponent) then
          //Result.Add(ChildProperty,StreamChildren(TComponent(AObject)));
          Raise Exception.Create('jsoStreamChildren not supported');
      If Assigned(AfterStreamObject) then
        AfterStreamObject(Self,AObject,Result);
      end;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TLSPStreamer.StreamProperty(Const AObject: TObject; PropertyInfo: PPropInfo): TJSONData;
type
  TVariantArray = array of Variant;
  TObjectArray = array of TObject;
var
  PropArray: TJSONArray;
  ElementType: PTypeInfo;
  VariantArray: TVariantArray;
  ObjectArray: TObjectArray;
  i: integer;
begin
  Result := nil;

  if PropertyInfo^.PropType^.Kind = tkDynArray then
    begin
      // Class kinds are in ElType2 (not sure why)
      ElementType := GetTypeData(PropertyInfo^.PropType)^.ElType;
      if ElementType = nil then
        ElementType := GetTypeData(PropertyInfo^.PropType)^.ElType2;

      PropArray := TJSONArray.Create;

      case ElementType^.Kind of
        tkVariant:
          begin
            VariantArray := TVariantArray(GetDynArrayProp(AObject, PropertyInfo));
            for i := 0 to High(VariantArray) do
              PropArray.Add(StreamVariant(VariantArray[i]));
            Result := PropArray;
          end;
        tkClass:
          begin
            ObjectArray := TObjectArray(GetDynArrayProp(AObject, PropertyInfo));
            for i := 0 to High(ObjectArray) do
              PropArray.Add(StreamClassProperty(ObjectArray[i]));
            result := PropArray;
          end;
        otherwise
          raise EUnknownErrorCode.Create('Dynamic array element type "'+Integer(ElementType^.Kind).ToString+'" is not supported for responses.');
      end;
    end
  else
    Result := inherited StreamProperty(AObject, PropertyInfo);
end;

{ TLSPDeStreamer }

procedure TLSPDeStreamer.DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData);
var
  C: TClass;
  Optional: TObject;
  OptionalVariant: TOptionalVariantBase;
  OptionalObject: TOptionalObjectBase;
  ElementType: PTypeInfo;
  PropArray: TJSONArray;
  I: Integer;
var
  VariantArray: array of variant;
  ObjectArray: array of TObject;
begin
  VariantArray:=[];
  ObjectArray:=[];
  if (PropInfo^.PropType^.Kind = tkDynArray) and (PropData is TJSONArray) then
    begin
      // Class kinds are in ElType2 (not sure why)
      ElementType := GetTypeData(PropInfo^.PropType)^.ElType;
      if ElementType = nil then
        ElementType := GetTypeData(PropInfo^.PropType)^.ElType2;

      PropArray := TJSONArray(PropData);

      case ElementType^.Kind of
        tkVariant:
          begin
            SetLength(VariantArray, PropArray.Count);
            for I := 0 to PropArray.Count - 1 do
              VariantArray[I] := PropArray[I].Value;
            SetDynArrayProp(AObject, PropInfo, Pointer(VariantArray));
          end;
        tkClass:
          begin
            C := GetTypeData(ElementType)^.ClassType;

            SetLength(ObjectArray, PropArray.Count);
            for I := 0 to PropArray.Count - 1 do
              if PropArray[I] is TJSONObject then
                begin
                  ObjectArray[I] := C.Create;
                  JSONToObject(TJSONObject(PropArray[I]), ObjectArray[I]);
                end
              else
                raise EUnknownErrorCode.Create('Dynamic array element class must be TJSONObject.');

            SetDynArrayProp(AObject, PropInfo, Pointer(ObjectArray));
          end;
        otherwise
          raise EUnknownErrorCode.Create('Dynamic array element type "'+Integer(ElementType^.Kind).ToString+'" is not supported for responses.');
      end;
    end
  else if PropInfo^.PropType^.Kind = tkClass then
    begin
      C := GetTypeData(PropInfo^.PropType)^.ClassType;
      if C.InheritsFrom(TOptionalVariantBase) then
        begin
          Optional := C.Create;
          OptionalVariant := TOptionalVariantBase(Optional);
          SetObjectProp(AObject, PropInfo, Optional);
          OptionalVariant.Value := JSONToVariant(PropData);
        end
      else if C.InheritsFrom(TOptionalObjectBase) then
        begin
          Optional := C.Create;
          OptionalObject := TOptionalObjectBase(Optional);
          SetObjectProp(AObject, PropInfo, Optional);
          if PropData.JSONType = jtNull then
            OptionalObject.Value := nil
          else
            begin
              OptionalObject.Value := OptionalObject.ValueClass.Create;
              JSONToObject(PropData as TJSONObject, OptionalObject.Value);
            end;
        end
      else if C.InheritsFrom(TJSONData) then
        begin
          // Clone raw JSON data
          SetObjectProp(AObject, PropInfo, TJSONData(PropData.Clone));
        end
      else
        inherited DoRestoreProperty(AObject, PropInfo, PropData)
    end
  else
    inherited DoRestoreProperty(AObject, PropInfo, PropData)
end;


{ TLSPStreaming }

class procedure TLSPStreaming.GetObject(Sender: TOBject; AObject: TObject;
                                        Info: PPropInfo; AData: TJSONObject;
                                        DataName: TJSONStringType; var AValue: TObject);
var
  C: TClass;
begin
  C := GetTypeData(Info^.PropType)^.ClassType;
  if C.InheritsFrom(TLSPStreamable) then
    AValue := TLSPStreamableClass(C).Create;
end;

class constructor TLSPStreaming.Create;
begin
  Streamer := TLSPStreamer.Create(nil);
  Streamer.Options := Streamer.Options +
    [jsoEnumeratedAsInteger, jsoSetEnumeratedAsInteger, jsoTStringsAsArray];

  DeStreamer := TLSPDeStreamer.Create(nil);
  DeStreamer.Options := DeStreamer.Options + [jdoIgnorePropertyErrors, jdoIgnoreNulls];
  DeStreamer.OnGetObject := @GetObject;
end;

class destructor TLSPStreaming.Done;
begin
  Streamer.Free;
  Destreamer.Free;
end;

class function TLSPStreaming.ToObject(const JSON: TJSONData): T;
begin
  Result := T.Create;
  DeStreamer.JSONToObject(JSON as TJSONObject, TObject(Result));
end;

class function TLSPStreaming.ToObject(const JSON: TJSONStringType): T;
begin
  Result := T.Create;
  DeStreamer.JSONToObject(JSON, TObject(Result));
end;

class function TLSPStreaming.ToJSON(AObject: TObject): TJSONData;
begin
  if AObject.InheritsFrom(TCollection) then
    Result := Streamer.StreamCollection(TCollection(AObject))
  else
    Result := Streamer.ObjectToJSON(AObject);
end;

class function TLSPStreaming.ToJSON(AArray: array of TObject): TJSONData;
var
  AObject: TObject;
  JArray: TJSONArray;
begin
  JArray := TJSONArray.Create;
  for AObject in AArray do
    JArray.Add(Streamer.ObjectToJSON(AObject));
  result := JArray;
end;


end.

