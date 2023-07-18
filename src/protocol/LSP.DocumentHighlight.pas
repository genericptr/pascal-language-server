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

unit LSP.DocumentHighlight;

{$mode objfpc}{$H+}
{$scopedenums on}

interface
uses
  { RTL }
  SysUtils, Classes,
  { Protocol }
  LSP.Base, LSP.Basic,
  { Other }
  LSP.BaseTypes;

type
  TDocumentHighlightKind = (
      __UNUSED__,
      Text, // A textual occurrence.
      Read, // Read-access of a symbol, like reading a variable.
      Write // Write-access of a symbol, like writing to a variable.
    );

  { TDocumentHighlight }

  TDocumentHighlight = class(TCollectionItem)
  private
    fRange: TRange;
    fKind: TDocumentHighlightKind;
    procedure SetRange(AValue: TRange);
  published
    // The range this highlight applies to.
    property range: TRange read fRange write SetRange;

    // The highlight kind, default is DocumentHighlightKind.Text.
    property kind: TDocumentHighlightKind read fKind write fKind;
  public
    constructor Create(aCollection : TCollection); override;
    // _range will be owned.
    constructor Create(aCollection : TCollection; _kind: TDocumentHighlightKind; _range: TRange);
    Destructor Destroy; override;
  end;

  TDocumentHighlightItems = Specialize TGenericCollection<TDocumentHighlight>;

  { DocumentHighlightParams }

  TDocumentHighlightParams = class(TTextDocumentPositionParams)
  end;


implementation

procedure TDocumentHighlight.SetRange(AValue: TRange);
begin
  if fRange=AValue then Exit;
  fRange.Assign(AValue);
end;

constructor TDocumentHighlight.Create(aCollection: TCollection);
begin
  Create(aCollection,TDocumentHighlightKind.__UNUSED__,TRange.Create);
end;

constructor TDocumentHighlight.Create(aCollection : TCollection; _kind: TDocumentHighlightKind; _range: TRange);
begin
  Inherited Create(aCollection);
  kind := _kind;
  FRange := _range;
end;

destructor TDocumentHighlight.Destroy;
begin
  FreeAndNil(Frange);
  inherited Destroy;
end;

end.
