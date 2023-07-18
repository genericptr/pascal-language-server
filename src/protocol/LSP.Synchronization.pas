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

unit LSP.Synchronization;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  LSP.BaseTypes, LSP.Base, LSP.Basic;

type
  { TDidOpenTextDocumentParams }

  TDidOpenTextDocumentParams = class(TLSPStreamable)
  private
    fTextDocument: TTextDocumentItem;
    procedure SetTextDocument(AValue: TTextDocumentItem);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    // The document that was opened.
    property textDocument: TTextDocumentItem read fTextDocument write SetTextDocument;
  end;


  { TDidSaveTextDocumentParams }

  TDidSaveTextDocumentParams = class(TLSPStreamable)
  private
    fTextDocument: TTextDocumentItem;
    fText: string;
    procedure SetTextDocument(AValue: TTextDocumentItem);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    // The document that was saved.
    property textDocument: TTextDocumentItem read fTextDocument write SetTextDocument;
    // Optional the content when saved. Depends on the includeText value
    // when the save notification was requested.
    property text: string read fText write fText;
  end;

  { TDidCloseTextDocumentParams }

  TDidCloseTextDocumentParams = class(TLSPStreamable)
  private
    fTextDocument: TTextDocumentItem;
    procedure SetTextDocument(AValue: TTextDocumentItem);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    // The document that was closed.
    property textDocument: TTextDocumentItem read fTextDocument write SetTextDocument;
  end;

  { TTextDocumentContentChangeEvent }

  // An event describing a change to a text document. If range and
  // rangeLength are omitted the new text is considered to be the full
  // content of the document.
  TTextDocumentContentChangeEvent = class(TCollectionItem)
  private
    fText: string;
    fRange: TRange;
    procedure SetRange(AValue: TRange);
  Public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    // The range of the document that changed.
    property range: TRange read fRange write SetRange;
    // The new text of the whole document.
    property text: string read fText write fText;
  end;

  TContentChanges = specialize TGenericCollection<TTextDocumentContentChangeEvent>;

  { TDidChangeTextDocumentParams }

  TDidChangeTextDocumentParams = class(TLSPStreamable)
  private
    fTextDocument: TVersionedTextDocumentIdentifier;
    fContentChanges: TContentChanges;
    procedure SetContentChanges(AValue: TContentChanges);
    procedure SetTextDocument(AValue: TVersionedTextDocumentIdentifier);
  public
    constructor Create; override;
    Destructor Destroy; override;
  published
    // The document that did change. The version number points to the
    // version after all provided content changes have been applied.
    property textDocument: TVersionedTextDocumentIdentifier read fTextDocument write SetTextDocument;
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
    property contentChanges: TContentChanges read fContentChanges write SetContentChanges;
  end;



implementation

uses
  SysUtils;


{ TDidChangeTextDocumentParams }

procedure TDidChangeTextDocumentParams.SetContentChanges(AValue: TContentChanges
  );
begin
  if fContentChanges=AValue then Exit;
  fContentChanges.Assign(AValue);
end;

procedure TDidChangeTextDocumentParams.SetTextDocument(
  AValue: TVersionedTextDocumentIdentifier);
begin
  if fTextDocument=AValue then Exit;
  fTextDocument.Assign(AValue);
end;

constructor TDidChangeTextDocumentParams.Create;
begin
  fTextDocument := TVersionedTextDocumentIdentifier.Create;
  fcontentChanges := TContentChanges.Create;
end;

destructor TDidChangeTextDocumentParams.Destroy;
begin
  FreeAndNil(fTextDocument);
  FreeAndNil(fContentChanges);
  inherited Destroy;
end;

{ TDidOpenTextDocumentParams }

procedure TDidOpenTextDocumentParams.SetTextDocument(AValue: TTextDocumentItem);
begin
  if fTextDocument=AValue then Exit;
  fTextDocument.Assign(aValue);
end;

constructor TDidOpenTextDocumentParams.Create;
begin
  inherited Create;
  fTextDocument:=TTextDocumentItem.Create;
end;

destructor TDidOpenTextDocumentParams.Destroy;
begin
  FreeAndNil(fTextDocument);
  inherited Destroy;
end;

procedure TDidOpenTextDocumentParams.Assign(Source: TPersistent);
var
  Src: TDidOpenTextDocumentParams absolute source;
begin
  if Source is TDidOpenTextDocumentParams then
    begin
      TextDocument.Assign(Src.textDocument)
    end
  else
    inherited Assign(Source);
end;


{ TDidSaveTextDocumentParams }

procedure TDidSaveTextDocumentParams.SetTextDocument(AValue: TTextDocumentItem);
begin
  if fTextDocument=AValue then Exit;
  fTextDocument.Assign(AValue);
end;

constructor TDidSaveTextDocumentParams.Create;
begin
  inherited Create;
  fTextDocument:=TTextDocumentItem.Create;
end;

destructor TDidSaveTextDocumentParams.Destroy;
begin
  FreeAndNil(fTextDocument);
  inherited Destroy;
end;

procedure TDidSaveTextDocumentParams.Assign(Source: TPersistent);
var
  Src: TDidSaveTextDocumentParams absolute source;
begin
  if Source is TDidSaveTextDocumentParams then
    begin
      TextDocument.Assign(Src.textDocument)
    end
  else
    inherited Assign(Source);
end;


{ TDidCloseTextDocumentParams }

procedure TDidCloseTextDocumentParams.SetTextDocument(AValue: TTextDocumentItem
  );
begin
  if fTextDocument=AValue then Exit;
  fTextDocument:=AValue;
end;

constructor TDidCloseTextDocumentParams.Create;
begin
  inherited Create;
  fTextDocument:=TTextDocumentItem.Create;
end;

destructor TDidCloseTextDocumentParams.Destroy;
begin
  FreeAndNil(fTextDocument);
  inherited Destroy;
end;

procedure TDidCloseTextDocumentParams.Assign(Source: TPersistent);
var
  Src: TDidCloseTextDocumentParams absolute source;
begin
  if Source is TDidCloseTextDocumentParams then
    begin
      TextDocument.Assign(Src.textDocument)
    end
  else
  inherited Assign(Source);
end;


{ TTextDocumentContentChangeEvent }

procedure TTextDocumentContentChangeEvent.SetRange(AValue: TRange);
begin
  if fRange=AValue then Exit;
  fRange.Assign(AValue);
end;

constructor TTextDocumentContentChangeEvent.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fRange := TRange.Create;
end;

destructor TTextDocumentContentChangeEvent.Destroy;
begin
  FreeAndNil(fRange);
  inherited Destroy;
end;


end.
