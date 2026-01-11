unit Tests.Streaming;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson,
  LSP.Streaming, LSP.General;

type

  { TTestLSPStreaming }

  TTestLSPStreaming = class(TTestCase)
  published
    // Test that ToObject handles nil JSON (params omitted in JSON-RPC)
    procedure TestToObjectWithNilJSON;
    // Test that ToObject handles TJSONNull (params: null in JSON-RPC)
    procedure TestToObjectWithJSONNull;
    // Test that ToObject still works with valid TJSONObject
    procedure TestToObjectWithValidJSONObject;
  end;

implementation

type
  TVoidParamsStreaming = specialize TLSPStreaming<TVoidParams>;

{ TTestLSPStreaming }

procedure TTestLSPStreaming.TestToObjectWithNilJSON;
var
  Params: TVoidParams;
begin
  // JSON-RPC 2.0 allows params to be omitted, which results in nil
  // This should not crash, but return a default-constructed TVoidParams
  Params := TVoidParamsStreaming.ToObject(TJSONData(nil));
  try
    AssertNotNull('ToObject(nil) should return valid object', Params);
  finally
    Params.Free;
  end;
end;

procedure TTestLSPStreaming.TestToObjectWithJSONNull;
var
  Params: TVoidParams;
  NullJSON: TJSONNull;
begin
  // LSP shutdown request can send "params": null
  // This should not crash, but return a default-constructed TVoidParams
  NullJSON := TJSONNull.Create;
  try
    Params := TVoidParamsStreaming.ToObject(NullJSON);
    try
      AssertNotNull('ToObject(TJSONNull) should return valid object', Params);
    finally
      Params.Free;
    end;
  finally
    NullJSON.Free;
  end;
end;

procedure TTestLSPStreaming.TestToObjectWithValidJSONObject;
var
  Params: TVoidParams;
  JSONObj: TJSONObject;
begin
  // Normal case: valid JSON object should still work
  JSONObj := TJSONObject.Create;
  try
    Params := TVoidParamsStreaming.ToObject(JSONObj);
    try
      AssertNotNull('ToObject(TJSONObject) should return valid object', Params);
    finally
      Params.Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

initialization
  RegisterTest(TTestLSPStreaming);
end.
