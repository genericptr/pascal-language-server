unit Tests.Basic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  LSP.Basic;

type

  { TTestRange }

  TTestRange= class(TTestCase)
  private
    FRange: TRange;
    procedure SetRange(AValue: TRange);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure CheckRange(Msg : string; aStartLine,aStartChar,aEndLine,aEndChar : Integer);
    // Created in startup, will be freed in teardown.
    Property Range : TRange Read FRange Write SetRange;
  Public
    class Procedure CheckRange(Msg : string; aRange: TRange; aStartLine,aStartChar,aEndLine,aEndChar : Integer);
  published
    procedure TestHookUp;
    procedure TestAssign;
    procedure TestConstructorLen;
    procedure TestConstructorAllPos;
    procedure TestSetRangeLen;
    Procedure TestSetRangeAllPos;
    Procedure TestToString;
  end;

implementation

procedure TTestRange.TestHookUp;
begin
  AssertNotNull('Have range',Range);
  CheckRange('No args in constructor.',0,0,0,0);
end;

procedure TTestRange.TestAssign;

Var
  B : TRange;

begin
  B:=TRange.Create;
  B.Start.line:=12;
  B.Start.character:=13;
  B.&end.line:=14;
  B.&end.character:=15;
  try
    Range.Assign(B);
  finally
    b.Free;
  end;
  CheckRange('Assign',12,13,14,15);
end;

procedure TTestRange.TestConstructorLen;

begin
  Range:=TRange.Create(10,11,15);
  CheckRange('Constructor with pos and length',10,11,10,26);
end;

procedure TTestRange.TestConstructorAllPos;
begin
  Range:=TRange.Create(10,11,12,13);
  CheckRange('Constructor with explicit start and end',10,11,12,13);
end;

procedure TTestRange.TestSetRangeLen;
begin
  Range.SetRange(10,11,15);
  CheckRange('Setrange with pos and length',10,11,10,26);
end;

procedure TTestRange.TestSetRangeAllPos;
begin
  Range.SetRange(10,11,12,13);
  CheckRange('Constructor with explicit start and end',10,11,12,13);
end;

procedure TTestRange.TestToString;

Const
  aResult = 'start: [10:11], end: [12:13]';

begin
  Range.SetRange(10,11,12,13);
  AssertEquals('ToString',aResult,Range.ToString);
end;

procedure TTestRange.SetRange(AValue: TRange);
begin
  if FRange=AValue then Exit;
  FreeAndNil(FRange);
  FRange:=AValue;
end;

procedure TTestRange.SetUp;
begin
  Range:=TRange.Create;
end;

procedure TTestRange.TearDown;
begin
  FreeAndNil(FRange);
end;

procedure TTestRange.CheckRange(Msg: string; aStartLine, aStartChar, aEndLine, aEndChar: Integer);
begin
  CheckRange(Msg,Range, aStartLine, aStartChar, aEndLine, aEndChar);
end;

class procedure TTestRange.CheckRange(Msg: string; aRange: TRange; aStartLine,
  aStartChar, aEndLine, aEndChar: Integer);
begin
  AssertEquals(Msg+': Start line',aStartLine,aRange.Start.line);
  AssertEquals(Msg+': Start character',aStartChar,aRange.Start.character);
  AssertEquals(Msg+': End line',aEndLine,aRange.&end.line);
  AssertEquals(Msg+': End character',aEndChar,aRange.&end.character);
end;

initialization

  RegisterTest(TTestRange);
end.

