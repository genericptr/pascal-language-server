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
  SysUtils, Classes, fpjson,
  IdentCompletionTool;


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
    procedure Add(Part: AnsiString; Offset, Len: Integer); overload;
  private
    class operator Initialize(var self: TLongString);
  end;

  type
    TJSONWrapper = class (TJSONString)
    protected
      function GetAsJSON: TJSONStringType; override;
    public
      Contents: TLongString;
      ItemCount: Integer;
      constructor Create; override;
    end;

{ Functions }

function FindIdentifierParent(Identifier: TIdentifierListItem): ShortString;

function ParseParamList(RawList: String): TStringList; overload;
function ParseParamList(RawList: String; AsSnippet: boolean): String; overload;

function ConvertBytesToHumanReadable(bytes: cardinal): ShortString;

implementation
uses
  CodeTree, PascalParserTool;

function TJSONWrapper.GetAsJSON: TJSONStringType;
begin
  result := Contents.S;
end;

constructor TJSONWrapper.Create;
begin
end;

{ LongString }

procedure TLongString.Clear;
begin
  S := '';
end;

procedure TLongString.Rewind;
begin
  SetLength(S, LastPos);
  LastPos := Length(S);
end;

procedure TLongString.Add(Part: AnsiString; Offset, Len: Integer); 
begin
  Assert(Offset + Len <= Length(Part), IntToStr(Offset)+'-'+IntToStr(Len)+' is > '+IntToStr(Length(Part)));
  if (Len - Offset) = 0 then
    exit;
  LastPos := Length(S);
  SetLength(S, LastPos + Len);
  Move(Part[Offset + 1], S[LastPos + 1], Len);
end;

procedure TLongString.Add(Part: AnsiString); 
begin
  LastPos := Length(S);
  S += Part;
end;

class operator TLongString.Initialize(var self: TLongString);
begin
  self.Clear;
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

function FindIdentifierParent(Identifier: TIdentifierListItem): ShortString;
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

