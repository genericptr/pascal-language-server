// Pascal Language Server
// Copyright 2022 Ryan Joseph

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

{$mode objfpc}
{$modeswitch autoderef}

unit memUtils;
interface
uses
  Math, Types, SysUtils, FGL;

type
  TAutoRetainHelpers = class helper for TObject
    constructor Instance;
    function Retain: TObject;
    function Release: TObject;
    function AutoRelease: TObject;
  end;

procedure ReleaseAndNil(var obj: TObject);
procedure DrainAutoReleasePool;

implementation

type

  { TAutoReleasePool }

  TAutoReleasePool = class(specialize TFPGList<TObject>)
    constructor Create;
    destructor Destroy; override;
    procedure Drain;
  end;

  { TAutoWrapper }

  TAutoWrapper = record
    obj: TObject;
    refCount: integer;
  end;
  PAutoWrapper = ^TAutoWrapper;
  TAutoWrapperMap = specialize TFPGMap<SizeUInt, TAutoWrapper>;

  { TAutoWrapperMapHelper }

  TAutoWrapperMapHelper = class helper for TAutoWrapperMap
    function GetRef(const key: SizeUInt): PAutoWrapper; inline;
  end;

  TAutoReleasePoolList = specialize TFPGList<TAutoReleasePool>;

var
  AutoRetainMap: TAutoWrapperMap;
  AutoReleaseStack: TAutoReleasePoolList;

{*****************************************************************************
 *                                 Functions
 *****************************************************************************}

procedure ReleaseAndNil(var obj: TObject);
begin
  if assigned(obj) then
    begin
      obj.Release;
      obj := nil;
    end;
end;

procedure DrainAutoReleasePool;
var
  obj: TObject;
begin
  for obj in AutoReleaseStack.Last do
    obj.Release;
  AutoReleaseStack.Last.Clear;
end;

{*****************************************************************************
 *                                TAutoReleasePool
 *****************************************************************************}

procedure TAutoReleasePool.Drain;
var
  obj: TObject; 
begin
  for obj in self do
    obj.Release;
  Clear;
end;

destructor TAutoReleasePool.Destroy; 
begin
  Drain;
  AutoReleaseStack.Delete(AutoReleaseStack.Count - 1);
  inherited;
end;

constructor TAutoReleasePool.Create;
begin
  AutoReleaseStack.Add(self);
  inherited;
end;

{*****************************************************************************
 *                               TAutoWrapperMapHelper
 *****************************************************************************}

function TAutoWrapperMapHelper.GetRef(const key: SizeUInt): PAutoWrapper;
begin
  result := TFPSMap(self).KeyData[@key];
end;

{*****************************************************************************
 *                                TAutoRetainHelpers
 *****************************************************************************}

constructor TAutoRetainHelpers.Instance;
begin
  NewInstance;
  AfterConstruction;
  AutoRelease;
end;

function TAutoRetainHelpers.AutoRelease: TObject;
begin
  Assert(AutoReleaseStack.Count > 0, 'No auto release pools are open!');
  // retain and then add to open pool
  AutoReleaseStack.Last.Add(Retain);
  result := self;
end;

function TAutoRetainHelpers.Retain: TObject;
var
  wrapper: TAutoWrapper;
  ref: PAutoWrapper;
  key: SizeUInt;
begin
  key := SizeUInt(self);
  if not AutoRetainMap.TryGetData(key, wrapper) then
    begin
      wrapper.obj := self;
      wrapper.refCount := 1;
      AutoRetainMap[key] := wrapper;
    end
  else
    begin
      ref := AutoRetainMap.GetRef(key);
      ref.refCount += 1;
    end;
  result := self;
end;

function TAutoRetainHelpers.Release: TObject;
var
  wrapper: PAutoWrapper;
begin
  wrapper := AutoRetainMap.GetRef(SizeUInt(self));
  dec(wrapper.refCount);
  if wrapper.refCount = 0 then
    begin
      wrapper.obj.Free;
      AutoRetainMap.Remove(SizeUInt(self));
    end;
  result := self;
end;

begin
  AutoRetainMap := TAutoWrapperMap.Create;
  AutoRetainMap.Sorted := true;
  AutoRetainMap.Duplicates := dupAccept;
  AutoReleaseStack := TAutoReleasePoolList.Create;
  TAutoReleasePool.Create;
end.