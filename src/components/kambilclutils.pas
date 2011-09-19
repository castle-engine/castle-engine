{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for cooperation between LCL and "Castle Game Engine". }
unit KambiLCLUtils;

interface

uses FileFilters, Dialogs;

{ Convert file filters into LCL OpenDialog.Filter, OpenDialog.FilterIndex.
  FileFilters must be encoded as for TFileFilterList.AddFiltersFromString.

  @groupBegin }
procedure FileFiltersToOpenDialog(const FileFilters: string;
  OpenDialog: TOpenDialog);
procedure FileFiltersToOpenDialog(const FileFilters: string;
  out LCLFilter: string; out FilterIndex: Integer);
{ @groupEnd }

{ Convert file filters into LCL OpenDialog.Filter, OpenDialog.FilterIndex. }
procedure FileFiltersToOpenDialog(FFList: TFileFilterList;
  out LCLFilter: string; out LCLFilterIndex: Integer);

{ Make each '&' inside string '&&', this way the string will not contain
  special '&x' sequences when used as a TMenuItem.Caption and such. }
function SQuoteLCLCaption(const S: string): string;

implementation

uses SysUtils, KambiClassUtils;

procedure FileFiltersToOpenDialog(const FileFilters: string;
  OpenDialog: TOpenDialog);
var
  LCLFilter: string;
  FilterIndex: Integer;
begin
  FileFiltersToOpenDialog(FileFilters, LCLFilter, FilterIndex);
  OpenDialog.Filter := LCLFilter;
  OpenDialog.FilterIndex := FilterIndex;
end;

procedure FileFiltersToOpenDialog(const FileFilters: string;
  out LCLFilter: string; out FilterIndex: Integer);
var
  FFList: TFileFilterList;
begin
  FFList := TFileFilterList.Create(true);
  try
    FFList.AddFiltersFromString(FileFilters);
    FileFiltersToOpenDialog(FFList, LCLFilter, FilterIndex);
  finally FreeAndNil(FFList) end;
end;

procedure FileFiltersToOpenDialog(FFList: TFileFilterList;
  out LCLFilter: string; out LCLFilterIndex: Integer);
var
  Filter: TFileFilter;
  I, J: Integer;
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

  { LCL FilterIndex counts from 1. }
  LCLFilterIndex := FFList.DefaultFilter + 1;
end;

function SQuoteLCLCaption(const S: string): string;
begin
  Result := StringReplace(S, '&', '&&', [rfReplaceAll]);
end;

end.
