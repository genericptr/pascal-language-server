
unit codeUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

function ParseParamList(RawList: String): TStringList; overload;
function ParseParamList(RawList: String; AsSnippet: boolean): String; overload;

implementation

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

