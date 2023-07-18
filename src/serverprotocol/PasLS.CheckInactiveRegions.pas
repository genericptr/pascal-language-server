// Pascal Language Server
// Copyright 2023 Michael Van Canneyt

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
//
// Adapted from fork at:
// https://github.com/coolchyni/pascal-language-server
//  (codetoolsutil unit)
//
unit PasLS.CheckInactiveRegions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, LazUtils, LazUtilities,
  // Codetools
  ExprEval,DefineTemplates,CodeToolManager,CodeCache,LinkScanner,sourcelog,
  BasicCodeTools,
  //pasls
  LSP.Messages;

type

  { TCheckInactiveRegions }

  TCheckInactiveRegions = class
  private
    FTransport: TMessageTransport;
  Public
    Constructor Create(aTransport : TMessageTransport);
    procedure Execute(Code:TCodeBuffer;uri:String);
    Property Transport : TMessageTransport Read FTransport;
  end;

Procedure CheckInactiveRegions(aTransport : TMessageTransport; aCode : TCodeBuffer; aURI : String);


implementation

uses PasLS.Settings, PasLS.InactiveRegions;

Procedure CheckInactiveRegions(aTransport : TMessageTransport; aCode : TCodeBuffer; aURI : String);
begin
  if ServerSettings.CheckInactiveRegions then
    With TCheckInactiveRegions.Create(aTransport) do
      try
        aTransport.SendDiagnostic('Checking inactive regions');
        Execute(aCode,aURI);
      finally
        Free;
      end;
end;

constructor TCheckInactiveRegions.Create(aTransport: TMessageTransport);
begin
  FTransport:=aTransport;
end;

procedure TCheckInactiveRegions.Execute(Code:TCodeBuffer;uri:String);

  Procedure GetDirectivePos(Scanner: TLinkScanner;Dir : PLSDirective; Out Line,Col : integer);
  var
    acode:Pointer;
    cursorPos:Integer;
  begin
    Scanner.CleanedPosToCursor(Dir^.CleanPos,cursorPos,acode);
    TSourceLog(acode).AbsoluteToLineCol(cursorPos,line,col);
  end;

var   
  Notification: TInactiveRegionsNotification;
  Regions: TRegionsItems;
  CurrentRegion: TInputRegion;
  Scanner: TLinkScanner;
  Dir: PLSDirective;
  i, line,col: Integer;
  DirectiveText : string;

begin
  if (Code=nil) or Not CodeToolBoss.ExploreUnitDirectives(Code,Scanner) then
    exit;
  Notification := TInactiveRegionsNotification.Create;
  try
    Notification.InactiveRegionParams.uri:=uri;
    // Easy access
    Regions:=Notification.InactiveRegionParams.regions;
    CurrentRegion:=nil;
    for i:=0 to Scanner.DirectiveCount-1 do
      begin
        Dir:=Scanner.Directives[i];
        if (Dir^.Code<>Pointer(Code)) then
          Continue;
        GetDirectivePos(Scanner,Dir,Line,Col);
        DirectiveText:=ExtractCommentContent(Scanner.CleanedSrc,Dir^.CleanPos,Scanner.NestedComments);
        Case Dir^.State of
        lsdsInactive:
          if Not Assigned(CurrentRegion) then
            begin
              CurrentRegion:=Regions.Add;
              CurrentRegion.startline:=line;
              CurrentRegion.startCol:=col+length(DirectiveText)+2;
              CurrentRegion.endline:=999999; // will be corrected when end of region is found
            end;
        lsdsActive:
          if Assigned(CurrentRegion) then
            begin
              CurrentRegion.endline:=line;
              CurrentRegion.endCol:=col;
              CurrentRegion:=Nil;
            end;
        lsdsSkipped:
           ;
        end;
      end;
    // We must always send: if after an edit the previous ranges become invalid, we need to notify the client.
    Notification.Send(Transport);
  finally
    Notification.Free;
  end;
end;

end.
