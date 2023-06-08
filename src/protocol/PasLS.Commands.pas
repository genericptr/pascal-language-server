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

unit PasLS.Commands;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface
uses
  { RTL }
  SysUtils, Classes, Types, FPJSON,
  { LSP }
  LSP.BaseTypes, LSP.Messages ;

Type

  { TCustomCommand }

  TCustomCommand = class
  private
    FTransport: TMessageTransport;
  Protected
    function DoExecute(aArguments : TJSONArray) : TLSPStreamable; virtual; abstract;
  Public
    class Function CommandName : String; virtual;
    class Procedure Register;
    class procedure UnRegister;
    Constructor Create(aTransport : TMessageTransport); virtual;
    function Execute(aArguments : TJSONArray) : TLSPStreamable;
    Property Transport : TMessageTransport Read FTransport;
  end;
  TCustomCommandClass = class of TCustomCommand;

  { TCommandDef }

  TCommandDef = Class(TCollectionItem)
  private
    FClass: TCustomCommandClass;
    function GetCommandName: String;
  Public
    property CommandClass : TCustomCommandClass Read FClass Write FClass;
    Property CommandName : String Read GetCommandName;
  end;

  { TCommandDefs }

  TCommandDefs = Class(TCollection)
  private
    FCommandCount: Integer;
    function GetDef(aIndex: Integer): TCommandDef;
    procedure SetDef(aIndex: Integer; AValue: TCommandDef);
  Public
    function Add(aCommandClass : TCustomCommandClass) : TCommandDef;
    function IndexOfCommandClass(aCommandClass : TCustomCommandClass) : Integer;
    function IndexOfCommand(const aName : String) : Integer;
    function FindCommand(const aName: String): TCommandDef;
    function FindCommandClass(aCommandClass : TCustomCommandClass): TCommandDef;
    function Remove(aCommandClass : TCustomCommandClass) : Boolean;
    Property Commands[aIndex: Integer] : TCommandDef Read GetDef Write SetDef; default;
    Property CommandCount : Integer Read FCommandCount;
  end;

  { TCommandFactory }

  TCommandFactory = class
  private
    class var _instance : TCommandFactory;
  private
    FList : TCommandDefs;
  public
    Class Constructor Init;
    Class Destructor Done;
    Constructor Create;
    Destructor Destroy; override;
    Procedure RegisterCommand(aCommand : TCustomCommandClass);
    Procedure UnregisterCommand(aCommand : TCustomCommandClass);
    Function FindCommand(const aName : String) : TCommandDef;
    Function FindCommandClass(const aName : String) : TCustomCommandClass;
    Procedure GetCommandList(L : TStrings);
    Function GetCommandNames : TStringDynArray;
    Class Property Instance : TCommandFactory Read _Instance;
  end;

Function CommandFactory : TCommandFactory;


implementation
uses
  CodeToolManager,
  FindDeclarationTool,

  { Protocols }
  LSP.Workspace, PasLS.Settings;

function CommandFactory: TCommandFactory;
begin
  Result:=TCommandFactory.Instance;
end;




{ TCustomCommand }

class function TCustomCommand.CommandName: String;
begin
  Result:=ClassName;
  if Result[1]='T' then
    Delete(Result,1,1);
end;

class procedure TCustomCommand.Register;
begin
  TCommandFactory.Instance.RegisterCommand(Self);
end;

class procedure TCustomCommand.UnRegister;
begin
  TCommandFactory.Instance.UnRegisterCommand(Self);
end;

constructor TCustomCommand.Create(aTransport: TMessageTransport);
begin
  FTransport:=aTransport;
end;

function TCustomCommand.Execute(aArguments: TJSONArray): TLSPStreamable;
begin
  // Maybe later we can add some logging etc. here.
  Result:=DoExecute(aArguments);
end;

{ TCommandDef }

function TCommandDef.GetCommandName: String;
begin
  Result:='';
  if Assigned(CommandClass) then
    Result:=CommandClass.CommandName;
end;

{ TCommandDefs }

function TCommandDefs.GetDef(aIndex: Integer): TCommandDef;
begin
  Result:=Items[aIndex] as TCommandDef;
end;

procedure TCommandDefs.SetDef(aIndex: Integer; AValue: TCommandDef);
begin
  Items[aIndex]:=aValue;
end;

function TCommandDefs.Add(aCommandClass: TCustomCommandClass): TCommandDef;
begin
  if IndexOfCommand(aCommandClass.CommandName)<>-1 then
    Raise EListError.CreateFmt('Duplicate command: %s',[aCommandClass.CommandName]);
  Result:=(Inherited Add) as TCommandDef;
  Result.CommandClass:=aCommandClass;
end;

function TCommandDefs.IndexOfCommandClass(aCommandClass: TCustomCommandClass
  ): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (GetDef(Result).CommandClass<>aCommandClass) do
    Dec(Result);
end;

function TCommandDefs.IndexOfCommand(const aName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetDef(Result).CommandName,aName) do
    Dec(Result);
end;

function TCommandDefs.FindCommand(const aName: String): TCommandDef;

var
  Idx : Integer;

begin
  Result:=nil;
  Idx:=IndexOfCommand(aName);
  If Idx<>-1 then
    Result:=Commands[Idx];
end;

function TCommandDefs.FindCommandClass(aCommandClass: TCustomCommandClass
  ): TCommandDef;
var
  Idx : Integer;

begin
  Result:=nil;
  Idx:=IndexOfCommandClass(aCommandClass);
  If Idx<>-1 then
    Result:=Commands[Idx];
end;

function TCommandDefs.Remove(aCommandClass: TCustomCommandClass): Boolean;

var
  Def : TCommandDef;

begin
  Def:=FindCommandClass(aCommandClass);
  Result:=Def<>Nil;
  if Result then
    Def.Free;
end;

{ TCommandFactory }

class constructor TCommandFactory.Init;
begin
  _Instance:=TCommandFactory.Create
end;

class destructor TCommandFactory.Done;
begin
  FreeAndNil(_Instance);
end;

constructor TCommandFactory.Create;
begin
  FList:=TCommandDefs.Create(TCommandDef);
end;

destructor TCommandFactory.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TCommandFactory.RegisterCommand(aCommand: TCustomCommandClass);

begin
  FList.Add(aCommand);
end;

procedure TCommandFactory.UnregisterCommand(aCommand: TCustomCommandClass);
begin
  FList.Remove(aCommand);
end;

function TCommandFactory.FindCommand(const aName: String): TCommandDef;
begin
  Result:=FList.FindCommand(aName);
end;

function TCommandFactory.FindCommandClass(const aName: String): TCustomCommandClass;

var
  aDef : TCommandDef;

begin
  Result:=nil;
  aDef:=FindCommand(aName);
  if Assigned(aDef) then
    Result:=aDef.CommandClass;
end;

procedure TCommandFactory.GetCommandList(L: TStrings);

Var
  i : Integer;

begin
  For I:=0 to FList.Count-1 do
    L.Add(FList[i].CommandName);
end;

function TCommandFactory.GetCommandNames: TStringDynArray;

var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    GetCommandList(L);
    Result:=L.ToStringArray;
  finally
    L.Free;
  end;
end;

end.
