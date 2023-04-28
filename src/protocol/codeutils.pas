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

unit codeUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  { RTL }
  SysUtils, Classes, FPJSON,
  { CodeTools }
  CodeCache, CodeTree, PascalReaderTool, PascalParserTool, IdentCompletionTool, BasicCodeTools,
  { Pasls }
  basic;

type
  
  { TIdentifierListItemHelper }

  TIdentifierListItemHelper = class helper for TIdentifierListItem
    function ParamPairList: string;
    function ParamNameList: string;
    function ParamTypeList: string;
  end;

  { TLongString }

  TLongString = record
  private
    LastPos: LongInt;
  public
    S: AnsiString;
    procedure Clear;
    procedure Rewind;
    procedure Add(part: AnsiString); overload;
    function Add(Part: AnsiString; Offset, Len: Integer): Boolean; overload;
    function Last: Char; inline;
  end;

  { TJSONSerializedArray }

  type
    TJSONSerializedArray = class (TJSONString)
    protected
      function GetAsJSON: TJSONStringType; override;
    end;

{ Functions }
function GetIdentifierAtPos(Tool: TPascalReaderTool; StartPos: Longint; aSkipAmp: Boolean = true; IncludeDot: Boolean = false; IncludeOps: Boolean = false): String;
function GetIdentifierRangeAtPos(Code: TCodeBuffer; X, Y: Integer): TRange;
function FindIdentifierClass(Identifier: TIdentifierListItem): ShortString;
function IsNodeObjectMember(Node: TCodeTreeNode): Boolean;
function IdentifierContext(Identifier: TIdentifierListItem; out DetailString: ShortString; out ObjectMember: boolean): ShortString;

function ParseParamList(RawList: String): TStringList; overload;
function ParseParamList(RawList: String; AsSnippet: boolean): String; overload;

function ConvertBytesToHumanReadable(bytes: cardinal): ShortString;

implementation

function GetIdentifierRangeAtPos(Code: TCodeBuffer; X, Y: Integer): TRange;
var
  Line: String;
  IdentStart, IdentEnd: Integer;
begin
  Line := Code.GetLine(Y);
  GetIdentStartEndAtPosition(Line, X, IdentStart, IdentEnd);
  result := TRange.Create(Y, IdentStart - 1, Y, IdentEnd - 1);
end;

function FindIdentifierClass(Identifier: TIdentifierListItem): ShortString;
var
  Node: TCodeTreeNode;
begin
  result := '';
  Node := Identifier.Node;
  while Node <> nil do
    begin
      if Node.Desc = ctnClass then
        begin
          result := Identifier.Tool.ExtractClassName(Node, false);
          exit;
        end;
      Node := Node.Parent;
    end;
end;

function IsNodeObjectMember(Node: TCodeTreeNode): Boolean;
begin
  result := false;
  while Node <> nil do
    begin
      if Node.Desc in AllClassObjects then
        exit(true);
      Node := Node.Parent;
    end;
end;

function GetIdentifierAtPos(Tool: TPascalReaderTool; StartPos: Longint; aSkipAmp: Boolean; IncludeDot: Boolean; IncludeOps: Boolean): String;
  
  function GetIdentifier(Identifier: PChar; aSkipAmp, IncludeDot, IncludeOps: Boolean): string;
  const
    OpChar =         ['+', '*', '-', '/', '<', '>', '=', ':'];
    IdentStartChar = ['a'..'z','A'..'Z','_'] + OpChar;
    IdentChar =      ['a'..'z','A'..'Z','_','0'..'9'] + OpChar;
  var
    len: integer;
  begin
    if (Identifier=nil) then begin
      Result:='';
      exit;
    end;
    if (Identifier^ in IdentStartChar) or ((Identifier^='&') and ((Identifier[1] in IdentStartChar))) then begin
      len:=0;
      if (Identifier^='&') then
      begin
        if aSkipAmp then
          inc(Identifier)
        else
          inc(len);
      end;
      while (Identifier[len] in IdentChar) or (IncludeDot and (Identifier[len] = '.')) do inc(len);
      SetLength(Result,len);
      if len>0 then
        Move(Identifier[0],Result[1],len);
    end else
      Result:='';
  end;

var
  Source: String;
begin
  Source := Tool.Scanner.CleanedSrc;
  Result := GetIdentifier(@Source[StartPos], aSkipAmp, IncludeDot, IncludeOps);
end;

function IdentifierContext(Identifier: TIdentifierListItem; out DetailString: ShortString; out ObjectMember: boolean): ShortString;
var
  Node: TCodeTreeNode;
  Container, TypeName, UnitName: ShortString;
begin
  result := '';
  DetailString := '';
  ObjectMember := false;

  if Identifier.Node = nil then
    exit;

  case Identifier.Node.Desc of
    ctnProcedure,
    ctnVarDefinition,
    ctnTypeDefinition,
    ctnProperty,
    ctnConstDefinition,
    ctnEnumerationType:
      begin
        TypeName := '';
        UnitName := ExtractFileName(Identifier.Tool.MainFilename);

        // find type for variables
        if Identifier.Node.Desc = ctnVarDefinition then
          begin
            Node := Identifier.Tool.FindTypeNodeOfDefinition(Identifier.Node);
            if Node <> nil then
              TypeName := GetIdentifierAtPos(Identifier.Tool, Node.StartPos);
          end;

        Node := Identifier.Node;
        while Node <> nil do
          begin
            if Node.Desc in AllClassObjects then
              begin
                Container := Identifier.Tool.ExtractClassName(Node, false);
                if TypeName <> '' then
                  begin
                    result := TypeName+' -> '+Container;
                    DetailString := Container+'.'+Identifier.Identifier+': '+TypeName+' ('+UnitName+')';
                  end
                else
                  begin
                    result := Container;
                    DetailString := Container+'.'+Identifier.Identifier+' ('+UnitName+')';
                  end;
                ObjectMember := true;
                exit;
              end;
            Node := Node.Parent;
          end;

        if TypeName <> '' then
          result := TypeName;

        // show unit name for global procedures 
        if Identifier.Node.Desc = ctnProcedure then
          DetailString := 'Unit '+UnitName
        // find type definition
        else if Identifier.Node.Desc = ctnTypeDefinition then
          //DetailString := Identifier.Tool.ExtractNode(Identifier.Node, [])
          DetailString := 'Unit '+UnitName
        // show contants values as details
        else if Identifier.Node.Desc = ctnConstDefinition then
          DetailString := Identifier.Tool.ExtractNode(Identifier.Node, []);
      end;
    ctnEnumIdentifier:
      begin
        Node := Identifier.Node;
        while Node <> nil do
          begin
            if Node.Desc = ctnTypeDefinition then
              begin
                //result := Identifier.Tool.ExtractNode(Node, []);
                Node := Identifier.Tool.FindTypeNodeOfDefinition(Identifier.Node);
                if Node <> nil then
                  result := GetIdentifierAtPos(Identifier.Tool, Node.StartPos);

                DetailString := 'Enum ('+result+')';
                ObjectMember := true;
                exit;
              end;
            Node := Node.Parent;
          end;
      end;
  end;
end;

procedure PrintIdentifierTree(Identifier: TIdentifierListItem);
var
  Node: TCodeTreeNode;
  Details: ShortString;
  ObjectMember: boolean;
begin
  writeln(StdErr, Identifier.Identifier, ' -> ', IdentifierContext(Identifier, Details, ObjectMember));
  Node := Identifier.Node;
  while Node <> nil do
    begin
      writeln(StdErr, '  ', Node.DescAsString, ' children=', Node.ChildCount);
      writeln(StdErr);
      Node := Node.Parent;
    end;
end;

{ TJSONSerializedArray }

function TJSONSerializedArray.GetAsJSON: TJSONStringType;
begin
  // already serialized so return the raw string
  result := GetAsString;
end;

{ TLongString }

procedure TLongString.Clear;
begin
  S := '';
end;

function TLongString.Last: Char;
begin
  result := S[High(S)];
end;

procedure TLongString.Rewind;
begin
  SetLength(S, LastPos);
  LastPos := Length(S);
end;

function TLongString.Add(Part: AnsiString; Offset, Len: Integer): Boolean;
begin
  Assert(Offset + Len <= Length(Part), IntToStr(Offset)+'-'+IntToStr(Len)+' is > '+IntToStr(Length(Part)));
  // zero length inserts do nothing
  if (Len = 0) or ((Len - Offset) = 0) then
    exit(false);
  LastPos := Length(S);
  SetLength(S, LastPos + Len);
  Move(Part[Offset + 1], S[LastPos + 1], Len);
  Result := true;
end;

procedure TLongString.Add(Part: AnsiString); 
begin
  LastPos := Length(S);
  S += Part;
end;

{ TIdentifierListItemHelper }

function TIdentifierListItemHelper.ParamPairList: string;
var
  ANode: TCodeTreeNode;
begin
  Result:='';
  ANode:=Node;
  if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
    Result:=Tool.ExtractProcHead(ANode,
       [phpWithoutClassKeyword,
        phpWithoutClassName,
        phpWithoutName,
        phpWithParameterNames,
        phpWithoutBrackets,
        phpWithoutSemicolon,
        phpDoNotAddSemicolon
        ]);
  end;
end;

function TIdentifierListItemHelper.ParamTypeList: string;
var
  ANode: TCodeTreeNode;
begin
  Result:='';
  ANode:=Node;
  if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
    Result:=Tool.ExtractProcHead(ANode,
       [phpWithoutClassKeyword,
        phpWithoutClassName,
        phpWithoutName,
        phpWithoutBrackets,
        phpWithoutSemicolon,
        phpDoNotAddSemicolon
        ]);
    Result:=StringReplace(Result, ',', '', []);
  end;
end;

function TIdentifierListItemHelper.ParamNameList: string;
var
  ANode: TCodeTreeNode;
begin
  Result:='';
  ANode:=Node;
  if (ANode<>nil) and (ANode.Desc=ctnProcedure) then begin
    Result:=Tool.ExtractProcHead(ANode,
       [phpWithoutBrackets, 
       phpWithoutClassKeyword,
       phpWithoutClassName,
       phpWithoutName,
       phpWithoutSemicolon,
       phpDoNotAddSemicolon,
       phpWithoutParamTypes,
       phpWithParameterNames
       ]);
  end;
end;

{ Functions }

function ConvertBytesToHumanReadable(bytes: cardinal): ShortString;
const
  kMaxUnits = 9;
var
  units: array[0..kMaxUnits - 1] of char = ('b', 'k', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y');
  multiplier: integer = 1000; // 1024 is 10.5 and lower
  exponent: integer = 0;
begin
  while (bytes >= multiplier) and (exponent < kMaxUnits) do
    begin
      bytes := trunc(bytes / multiplier);
      exponent += 1;
    end;
  result := IntToStr(bytes)+units[exponent];
end;

function SplitString (s: string; delimiter: char): TStringArray;
var
  i: integer;
  c: char;
  part: string = '';
  parts: TStringArray;
begin
  SetLength(parts, 0);
  for i := 1 to Length(s) do
    begin
      c := s[i];
      if (c = delimiter) or (i = Length(s)) then
        begin
          if (i = Length(s)) then
            part += c;
          SetLength(parts, Length(parts) + 1);
          parts[High(parts)] := part;
          part := '';
        end
      else
        part += c;
    end;
  result := parts;
end;

function ParseParamList(RawList: String): TStringList;
const
  kPairDelimiter = ': ';
var
  Text: String = '';
  I, J: Integer;
  Types, Names, Pair: TStringArray;
begin
  Result := TStringList.Create;
  // split types
  Types := SplitString(RawList, ';');
  for I := 0 to Length(Types) - 1 do
    begin
      // split name/type pair
      Pair := SplitString(Types[I], ':');
      if Length(Pair) <> 2 then
        continue;

      // split names
      Names := SplitString(Pair[0], ',');
      for J := 0 to Length(Names) - 1 do
        Result.Add(Names[J]+kPairDelimiter+Pair[1]);
      if Length(Names) > 0 then
        continue;

      Result.Add(Pair[0]+kPairDelimiter+Pair[1]);
    end;
end;

function ParseParamList(RawList: String; AsSnippet: boolean): String;
const
  kParamDelimiter = ', ';
  kPairDelimiter = ': ';
var
  Text: String = '';
  CurrentIndex: Integer = 0;
  I, J: Integer;
  Types, Names, Pair: TStringArray;
begin
  // split types
  Types := SplitString(RawList, ';');
  for I := 0 to Length(Types) - 1 do
    begin
      // split name/type pair
      Pair := SplitString(Types[I], ':');
      if Length(Pair) <> 2 then
        continue;

      // split names
      Names := SplitString(Pair[0], ',');
      for J := 0 to Length(Names) - 1 do
        begin
          if AsSnippet then
            Text += '${'+IntToStr(CurrentIndex + 1)+':'+Names[J]+kPairDelimiter+Pair[1]+'}'+kParamDelimiter
          else
            Text += Names[J]+kPairDelimiter+Pair[1]+kParamDelimiter;
          Inc(CurrentIndex);
        end;
      if Length(Names) > 0 then
        continue;

      if AsSnippet then
        Text += '${'+IntToStr(CurrentIndex + 1)+':'+Pair[0]+kPairDelimiter+Pair[1]+'}'+kParamDelimiter
      else
        Text += Pair[0]+kPairDelimiter+Pair[1]+kParamDelimiter;
      Inc(CurrentIndex);
    end;
  
  // trim trailing comma
  Text := Trim(Text);
  if (Length(Text) > 0) and (Text[Length(Text)] = ',') then
    SetLength(Text, Length(Text) - 1);

  Result := Text;
end;

end.

