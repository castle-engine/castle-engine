{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Simple utilities for LCL + Kambi VRML engine. }
unit KambiLCLUtils;

interface

uses FileFilters, Dialogs;

{ Convert file filters (encoded as for TFileFiltersList.AddFiltersFromString)
  into LCL OpenDialog.Filter, OpenDialog.FilterIndex. }
procedure FileFiltersToOpenDialog(const FileFilters: string;
  OpenDialog: TOpenDialog);

{ Convert file filters into LCL OpenDialog.Filter, OpenDialog.FilterIndex. }
procedure FileFiltersToOpenDialog(FFList: TFileFiltersList;
  OpenDialog: TOpenDialog);

implementation

uses KambiClassUtils;

procedure FileFiltersToOpenDialog(const FileFilters: string;
  OpenDialog: TOpenDialog);
var
  FFList: TFileFiltersList;
begin
  FFList := TFileFiltersList.Create;
  try
    FFList.AddFiltersFromString(FileFilters);
    FileFiltersToOpenDialog(FFList, OpenDialog);
  finally FreeWithContentsAndNil(FFList) end;
end;

procedure FileFiltersToOpenDialog(FFList: TFileFiltersList;
  OpenDialog: TOpenDialog);
var
  Filter: TFileFilter;
  I, J: Integer;
  LCLFilter: string;
begin
  LCLFilter := '';

  for I := 0 to FFList.Count - 1 do
  begin
    Filter := FFList[I];
    LCLFilter += Filter.Name + '|';

    for J := 0 to Filter.Patterns.Count - 1 do
    begin
      if J <> 0 then LCLFilter += ';';
      LCLFilter += Filter.Patterns[J];
    end;

    LCLFilter += '|';
  end;

  OpenDialog.Filter := LCLFilter;
  { LCL FilterIndex counts from 1. }
  OpenDialog.FilterIndex := FFList.DefaultFilter + 1;
end;

end.
