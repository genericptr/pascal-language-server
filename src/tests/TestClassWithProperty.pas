unit TestClassWithProperty;

{$mode objfpc}{$H+}

interface

type
  TUser = class
  private
    FName: String;
    FAge: Integer;
  public
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
    procedure PrintInfo;
    function GetFullName: String;
  end;

implementation

procedure TUser.PrintInfo;
begin
  writeln('User: ', FName, ', Age: ', FAge);
end;

function TUser.GetFullName: String;
begin
  Result := FName;
end;

end.
