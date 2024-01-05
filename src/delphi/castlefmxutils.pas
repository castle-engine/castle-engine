{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for Delphi FMX (FireMonkey). }
unit CastleFmxUtils;

{$I castleconf.inc}

interface

uses FMX.Dialogs,
  CastleFileFilters;

{ Convert file filters into FMX Dialog.Filter, Dialog.FilterIndex.
  Suitable for both open and save dialogs (in FMX, TSaveDialog
  descends from TOpenDialog).

  Input filters are either given as a string FileFilters
  (encoded just like for TFileFilterList.AddFiltersFromString),
  or as TFileFilterList instance.

  Output filters are set as appropriate properties of given Dialog instance.

  When AllFields is false, then filters starting with "All " in the name,
  like "All files", "All images", are not included in the output.

  @groupBegin }
procedure FileFiltersToDialog(const FileFilters: string;
  const Dialog: TOpenDialog; const AllFields: boolean = true); overload;
procedure FileFiltersToDialog(FFList: TFileFilterList;
  const Dialog: TOpenDialog; const AllFields: boolean = true); overload;
{ @groupEnd }

implementation

procedure FileFiltersToDialog(const FileFilters: string;
  const Dialog: TOpenDialog; const AllFields: boolean = true);
var
  OutFilter: String;
  OutFilterIndex: Integer;
begin
  TFileFilterList.LclFmxFiltersFromString(FileFilters,
    OutFilter, OutFilterIndex, AllFields);
  Dialog.Filter := OutFilter;
  Dialog.FilterIndex := OutFilterIndex;
end;

procedure FileFiltersToDialog(FFList: TFileFilterList;
  const Dialog: TOpenDialog; const AllFields: boolean = true);
var
  OutFilter: String;
  OutFilterIndex: Integer;
begin
  FFList.LclFmxFilters(OutFilter, OutFilterIndex, AllFields);
  Dialog.Filter := OutFilter;
  Dialog.FilterIndex := OutFilterIndex;
end;

end.