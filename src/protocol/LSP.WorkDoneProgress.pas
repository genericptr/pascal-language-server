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

unit LSP.WorkDoneProgress;

{$mode objfpc}{$H+}
{$scopedenums on}

interface
uses
  { RTL }
  Classes;

type
  TProgressToken = String; { integer | string }

type
  TWorkDoneProgressParams = class(TPersistent)
    private
      fWorkDoneToken: TProgressToken;
    published
      // An optional token that a server can use to report work done progress.
      property workDoneToken: TProgressToken read fWorkDoneToken write fWorkDoneToken;
  end;

implementation

end.
