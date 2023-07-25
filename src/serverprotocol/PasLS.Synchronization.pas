// Pascal Language Server
// Copyright 2020 Arjan Adriaanse
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
unit PasLS.Synchronization;

{$mode objfpc}{$H+}

interface

uses
  Classes, DateUtils,
  CodeToolManager, CodeCache,
  LSP.BaseTypes, LSP.Base, LSP.Basic, PasLS.Symbols, LSP.Synchronization;

Type
  { TDidOpenTextDocument }

  TDidOpenTextDocument = class(specialize TLSPNotification<TDidOpenTextDocumentParams>)
    procedure Process(var Params : TDidOpenTextDocumentParams); override;
  end;

  { TDidSaveTextDocument }

  TDidSaveTextDocument = class(specialize TLSPNotification<TDidSaveTextDocumentParams>)
    procedure Process(var Params : TDidSaveTextDocumentParams); override;
  end;


  { TDidCloseTextDocument }

  TDidCloseTextDocument = class(specialize TLSPNotification<TDidCloseTextDocumentParams>)
    procedure Process(var Params : TDidCloseTextDocumentParams); override;
  end;
  { TDidChangeTextDocument }

  TDidChangeTextDocument = class(specialize TLSPNotification<TDidChangeTextDocumentParams>)
    procedure Process(var Params : TDidChangeTextDocumentParams); override;
  end;


implementation

uses PasLS.CheckInactiveRegions, PasLS.Diagnostics;

{ TDidChangeTextDocument }

procedure TDidChangeTextDocument.Process(var Params : TDidChangeTextDocumentParams);
var
  Code: TCodeBuffer;
  Change: TCollectionItem;
{  Range: TRange;
  StartPos, EndPos: integer;}


begin with Params do
  begin
    Code := CodeToolBoss.FindFile(textDocument.LocalPath);
    for Change in contentChanges do
      begin
        // note(ryan): can't get this working yet
        // and I'm not even sure if it's worth it
{
        Range := TTextDocumentContentChangeEvent(Change).range;
        if Range <> nil then
          begin
            //Code.LineColToPosition(Range.start.line + 1, Range.start.character + 1, StartPos);
            //Code.LineColToPosition(Range.&end.line + 1, Range.&end.character + 1, EndPos);
            DoLog('insert: %d -> %d  text=%s',[StartPos,EndPos, TTextDocumentContentChangeEvent(Change).text]);
            //Code.Replace(StartPos, EndPos - StartPos, TTextDocumentContentChangeEvent(Change).text);
          end
        else }
        Code.Source := TTextDocumentContentChangeEvent(Change).text;

        // Ryan, uncomment this to have a syntax check at
        // CheckSyntax(Self.Transport,Code);

        //if SymbolManager <> nil then
        //  SymbolManager.FileModified(Code);
      end;
      // DoLog( 'Synched text in %d ms',[MilliSecondsBetween(Now, StartTime)]);
    end;
end;

{ TDidCloseTextDocument }

procedure TDidCloseTextDocument.Process(var Params : TDidCloseTextDocumentParams);


begin with Params do
  begin
    // URI := ParseURI(textDocument.uri);
    // TODO: clear errors
    // TODO: if the file was manually loaded (i.e. not in search paths)
    // then we may want to remove it from the symbol table so it doesn't cause clutter
  end;
end;


{ TDidSaveTextDocument }

procedure TDidSaveTextDocument.Process(var Params : TDidSaveTextDocumentParams);
var
  Code: TCodeBuffer;
begin

  Code := CodeToolBoss.FindFile(Params.textDocument.LocalPath);
  if SymbolManager <> nil then
    SymbolManager.FileModified(Code);
  DiagnosticsHandler.CheckSyntax(Transport,Code);
  CheckInactiveRegions(Transport, Code, Params.textDocument.uri);
  // ClearDiagnostics(Transport,Code);

end;


{ TDidOpenTextDocument }

procedure TDidOpenTextDocument.Process(var Params : TDidOpenTextDocumentParams);
var

  Path: String;
  Code: TCodeBuffer;
begin with Params do
  begin
    Path := textDocument.LocalPath;

    Code := CodeToolBoss.FindFile(Path);
    if Code <> nil then
      Code.Source := textDocument.text;

    // the file was not found in search paths so
    // it need to be loaded from disk
    if Code = nil then
      Code := CodeToolBoss.LoadFile(Path, False, False);
      
    DiagnosticsHandler.CheckSyntax(Transport,Code);

    CheckInactiveRegions(Transport, Code, textDocument.uri);
    //if SymbolManager <> nil then
    //  SymbolManager.FileModified(Code);
    if SymbolManager <> nil then
      SymbolManager.Reload(Code, True);
  end;
end;

end.

