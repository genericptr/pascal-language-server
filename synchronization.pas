// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

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

unit synchronization;

{$mode objfpc}{$H+}

interface

uses
  Classes, DateUtils, URIParser, 
  CodeToolManager, CodeCache,
  lsp, basic;

type

  { TDidOpenTextDocumentParams }

  TDidOpenTextDocumentParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentItem;
  published
    // The document that was opened.
    property textDocument: TTextDocumentItem read fTextDocument write fTextDocument;
  end;

  { TDidOpenTextDocument }

  TDidOpenTextDocument = class(specialize TLSPNotification<TDidOpenTextDocumentParams>)
    procedure Process(var Params : TDidOpenTextDocumentParams); override;
  end;

  { TDidSaveTextDocumentParams }

  TDidSaveTextDocumentParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentItem;
    fText: string;
  published
    // The document that was saved.
    property textDocument: TTextDocumentItem read fTextDocument write fTextDocument;
    // Optional the content when saved. Depends on the includeText value
    // when the save notification was requested.
    property text: string read fText write fText;
  end;

  { TDidSaveTextDocument }

  TDidSaveTextDocument = class(specialize TLSPNotification<TDidSaveTextDocumentParams>)
    procedure Process(var Params : TDidSaveTextDocumentParams); override;
  end;

  { TDidCloseTextDocumentParams }

  TDidCloseTextDocumentParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentItem;
  published
    // The document that was closed.
    property textDocument: TTextDocumentItem read fTextDocument write fTextDocument;
  end;

  { TDidCloseTextDocument }

  TDidCloseTextDocument = class(specialize TLSPNotification<TDidCloseTextDocumentParams>)
    procedure Process(var Params : TDidCloseTextDocumentParams); override;
  end;

  { TTextDocumentContentChangeEvent }

  // An event describing a change to a text document. If range and
  // rangeLength are omitted the new text is considered to be the full
  // content of the document.
  TTextDocumentContentChangeEvent = class(TCollectionItem)
  private
    fText: string;
    fRange: TRange;
  published
    // The range of the document that changed.
    property range: TRange read fRange write fRange;
    // The new text of the whole document.
    property text: string read fText write fText;
  end;

  { TDidChangeTextDocumentParams }

  TDidChangeTextDocumentParams = class(TPersistent)
  private
    fTextDocument: TVersionedTextDocumentIdentifier;
    fContentChanges: TCollection;
  public
    constructor Create;
  published
    // The document that did change. The version number points to the
    // version after all provided content changes have been applied.
    property textDocument: TVersionedTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The actual content changes. The content changes describe single
    // state changes to the document. So if there are two content
    // changes c1 (at array index 0) and c2 (at array index 1) for a
    // document in state S then c1 moves the document from S to S' and
    // c2 from S' to S''. So c1 is computed on the state S and c2 is
    // computed on the state S'.
    //
    // To mirror the content of a document using change events use the
    // following approach:
    // - start with the same initial content
    // - apply the 'textDocument/didChange' notifications in the order
    //   you recevie them.
    // - apply the `TextDocumentContentChangeEvent`s in a single
    //   notification in the order you receive them.
    property contentChanges: TCollection read fContentChanges write fContentChanges;
  end;

  { TDidChangeTextDocument }

  TDidChangeTextDocument = class(specialize TLSPNotification<TDidChangeTextDocumentParams>)
    procedure Process(var Params : TDidChangeTextDocumentParams); override;
  end;

implementation
uses
  SysUtils, codeUtils, settings, diagnostics, documentSymbol;

{ TDidChangeTextDocumentParams }

constructor TDidChangeTextDocumentParams.Create;
begin
  contentChanges := TCollection.Create(TTextDocumentContentChangeEvent);
end;

{ TDidOpenTextDocument }

procedure TDidOpenTextDocument.Process(var Params : TDidOpenTextDocumentParams);
var
  URI: TURI;
  Path: String;
  Code, MainCode: TCodeBuffer;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Path := URI.Path + URI.Document;

    Code := CodeToolBoss.FindFile(Path);
    if Code <> nil then
      Code.Source := textDocument.text;

    // the file was not found in search paths so
    // it need to be loaded from disk
    if Code = nil then
      Code := CodeToolBoss.LoadFile(Path, False, False);
      
    CheckSyntax(Code);

    if SymbolManager <> nil then
      begin
        MainCode := CodeToolBoss.GetMainCode(Code);
        if MainCode <> nil then
          SymbolManager.Reload(MainCode, True);
      end;
  end;
end;

{ TDidSaveTextDocument }

procedure TDidSaveTextDocument.Process(var Params : TDidSaveTextDocumentParams);
var
  URI: TURI;
  Code: TCodeBuffer;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    CheckSyntax(Code);
    ClearDiagnostics(Code);
  end;
end;

{ TDidCloseTextDocument }

procedure TDidCloseTextDocument.Process(var Params : TDidCloseTextDocumentParams);
var
  URI: TURI;
  Code: TCodeBuffer;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    // TODO: clear errors
    // TODO: if the file was manually loaded (i.e. not in search paths)
    // then we may want to remove it from the symbol table so it doesn't cause clutter
  end;
end;


{ TDidChangeTextDocument }

procedure TDidChangeTextDocument.Process(var Params : TDidChangeTextDocumentParams);
var
  URI: TURI;
  Code, MainCode: TCodeBuffer;
  Change: TCollectionItem;
  Range: TRange;
  StartPos, EndPos: integer;
  StartTime: TDateTime;
begin with Params do
  begin
    StartTime := Now;
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    for Change in contentChanges do
      begin
        // note(ryan): can't get this working yet
        // and I'm not even sure if it's worth it
        Range := TTextDocumentContentChangeEvent(Change).range;
        if Range <> nil then
          begin
            //Code.LineColToPosition(Range.start.line + 1, Range.start.character + 1, StartPos);
            //Code.LineColToPosition(Range.&end.line + 1, Range.&end.character + 1, EndPos);
            writeln(StdErr, 'insert: ', StartPos,' -> ',EndPos, ' text=',TTextDocumentContentChangeEvent(Change).text);
            //Code.Replace(StartPos, EndPos - StartPos, TTextDocumentContentChangeEvent(Change).text);
          end
        else
          Code.Source := TTextDocumentContentChangeEvent(Change).text;

        if SymbolManager <> nil then
          begin
            MainCode := CodeToolBoss.GetMainCode(Code);
            if MainCode <> nil then
              SymbolManager.FileModified(MainCode);
          end;
      end;
      //writeln(StdErr, 'Synched text in ', MilliSecondsBetween(Now, StartTime),'ms');
      //Flush(StdErr);
    end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/didOpen', TDidOpenTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didClose', TDidCloseTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didChange', TDidChangeTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didSave', TDidSaveTextDocument);
end.
