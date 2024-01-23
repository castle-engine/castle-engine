{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ File filters, for TCastleWindow.FileDialog and Lazarus file dialogs. }
unit CastleFileFilters;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections;

type
  TFileFilter = class
  private
    FName: string;
    FPatterns: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Patterns: TStringList read FPatterns;
  end;

  TFileFilterList = class({$ifdef FPC}specialize{$endif} TObjectList<TFileFilter>)
  private
    FDefaultFilter: Integer;
  public
    constructor Create(AFreeObjects: Boolean);

    { Add one file filter, selectable by user.
      @param(Name Is a name displayed for for user.)
      @param(Patterns Each string in Patterns is a pattern
        using * and ? matching characters.) }
    procedure AddFilter(const Name: string; const Patterns: array of string);

    { Index of default filter, selected by default for user when using
      this filters list. }
    property DefaultFilter: Integer
      read FDefaultFilter write FDefaultFilter default 0;

    { An easy way to add multiple filters encoded in a single string.
      Filters are separated by '|' (bar character).
      Each filter has a name, separated from patterns list also by '|'.
      If filter name starts with '*', it's set as a default filter.
      Patterns are separated by ';' (semicolon character).

      As you can see, this prevents some special characters from appearing
      in names and patterns. For maximum flexibility, don't use this
      function, use AddFilter.

      For example @code(All files (*)|*|All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png)

      Not finished pairs of name + pattern at the end of the string are ignored.
      In particular, empty string is unfinished (actually, it contains
      an empty Name, and is unfinished because there is no |, so no matching
      Pattern) so empty string causes no filters to be added. }
    procedure AddFiltersFromString(const FiltersStr: string);

    { One of the filtes (excluding "catch all" filters) matches given URL.
      This excludes masks like "*" and "*.*" (the latter should not really
      be used, because it will not match all files on Unix). }
    function Matches(const Url: String): Boolean; overload;

    { Whether the filters described in FiltersStr (like for
      @link(AddFiltersFromString)) match the given URL. }
    class function Matches(const FiltersStr, Url: String): Boolean; overload;

    { Writes all recognized extensions (without * and *.*) separated
      by semicolon. }
    function AllExtensions: String;

    { Convert file filters into LCL or FMX Dialog.Filter, Dialog.FilterIndex.
      When AllFields is false, then filters starting with "All " in the name,
      like "All files", "All images", are not included in the output. }
    procedure LclFmxFilters(
      out OutFilter: string; out OutFilterIndex: Integer; const AllFields: Boolean = true);

    { Convert file filters into LCL or FMX Dialog.Filter, Dialog.FilterIndex.
      The filters are provided here just like for AddFiltersFromString.
      Effectively we convert one String encoding of filters
      (as for CGE AddFiltersFromString)
      into very similar encoding of filters suitable for LCL or FMX. }
    class procedure LclFmxFiltersFromString(const FileFilters: string;
      out OutFilter: string; out OutFilterIndex: Integer; const AllFields: Boolean = true);
  end;

implementation

uses StrUtils, CastleStringUtils, CastleUriUtils, CastleUtils;

{ TFileFilter ---------------------------------------------------------------- }

constructor TFileFilter.Create;
begin
  inherited;
  FPatterns := TStringList.Create;
end;

destructor TFileFilter.Destroy;
begin
  FreeAndNil(FPatterns);
  inherited;
end;

{ TFileFilterList ----------------------------------------------------------- }

constructor TFileFilterList.Create(AFreeObjects: Boolean);
begin
  inherited;
  FDefaultFilter := 0;
end;

procedure TFileFilterList.AddFilter(const Name: string;
  const Patterns: array of string);
var
  Filter: TFileFilter;
  I: Integer;
begin
  Filter := TFileFilter.Create;
  Add(Filter);
  Filter.FName := Name;
  for I := 0 to System.High(Patterns) do
    Filter.FPatterns.Append(Patterns[I]);
end;

procedure TFileFilterList.AddFiltersFromString(const FiltersStr: string);

  procedure AddFilterFromPair(Name: string; const Patterns: string);
  var
    Filter: TFileFilter;
    LastSeparator, NextSeparator: Integer;
    Part: string;
  begin
    if SCharIs(Name, 1, '*') then
    begin
      DefaultFilter := Count;
      System.Delete(Name, 1, 1);
    end;

    Filter := TFileFilter.Create;
    Add(Filter);

    Filter.Name := Name;

    { tests: Writeln('new name: "', Filter.FName, '"'); }

    LastSeparator := 0;
    repeat
      { calcualate NextSeparator and set Part }
      NextSeparator := PosEx(';', Patterns, LastSeparator + 1);
      if NextSeparator = 0 then
        Part := SEnding(Patterns, LastSeparator + 1) else
        Part := CopyPos(Patterns, LastSeparator + 1, NextSeparator - 1);

      { new Part is a next pattern, add it }
      Filter.Patterns.Append(Part);
      { tests: Writeln('new pattern: "', Part, '"'); }

      { advance LastSeparator for next loop roll }
      LastSeparator := NextSeparator;
    until LastSeparator = 0;
  end;

var
  LastSeparator, NextSeparator: Integer;
  NamePart: Boolean;
  Part, Name, Patterns: string;
begin
  NamePart := true;
  LastSeparator := 0;
  repeat
    { calcualate NextSeparator and set Name or Patterns }
    NextSeparator := PosEx('|', FiltersStr, LastSeparator + 1);
    if NextSeparator = 0 then
      Part := SEnding(FiltersStr, LastSeparator + 1) else
      Part := CopyPos(FiltersStr, LastSeparator + 1, NextSeparator - 1);
    if NamePart then
      Name := Part else
      Patterns := Part;

    { actually make use of Name + Patterns to create new filter }
    if not NamePart then
      AddFilterFromPair(Name, Patterns);

    { advance LastSeparator and NamePart for next loop roll }
    LastSeparator := NextSeparator;
    NamePart := not NamePart;
  until LastSeparator = 0;
end;

function TFileFilterList.Matches(const Url: String): Boolean;
var
  UrlName, Pattern: String;
  Filter: TFileFilter;
begin
  UrlName := ExtractURIName(Url);
  for Filter in Self do
    for Pattern in Filter.Patterns do
      if (Pattern <> '*') and
         (Pattern <> '*.*') and
         IsWild(UrlName, Pattern, FileNameCaseSensitive) then
        Exit(true);
  Result := false;
end;

class function TFileFilterList.Matches(const FiltersStr, Url: String): Boolean;
var
  Filters: TFileFilterList;
begin
  Filters := TFileFilterList.Create(true);
  try
    Filters.AddFiltersFromString(FiltersStr);
    Result := Filters.Matches(Url);
  finally FreeAndNil(Filters) end;
end;

function TFileFilterList.AllExtensions: String;
var
  Pattern: String;
  Filter: TFileFilter;
begin
  Result := '';
  for Filter in Self do
    for Pattern in Filter.Patterns do
    begin
      if (Pattern = '*') or (Pattern = '*.*') then
        continue;
      Result := SAppendPart(Result, ';', Pattern);
    end;
end;

procedure TFileFilterList.LclFmxFilters(
  out OutFilter: string; out OutFilterIndex: Integer; const AllFields: Boolean);
var
  Filter: TFileFilter;
  I, J: Integer;
begin
  OutFilter := '';

  { initialize OutFilterIndex.
    Will be corrected for AllFields=false case, and will be incremented
    (because LCL/FMX FilterIndex counts from 1) later. }

  OutFilterIndex := DefaultFilter;

  for I := 0 to Count - 1 do
  begin
    Filter := Items[I];
    if (not AllFields) and IsPrefix('All ', Filter.Name) then
    begin
      { then we don't want to add this to OutFilter.
        We also need to fix OutFilterIndex, to shift it. }
      if I = DefaultFilter then
        OutFilterIndex := 0 else
      if I < DefaultFilter then
        Dec(OutFilterIndex);
      Continue;
    end;

    OutFilter := OutFilter + Filter.Name + '|';

    for J := 0 to Filter.Patterns.Count - 1 do
    begin
      if J <> 0 then OutFilter := OutFilter + ';';
      { TODO: We're considering this, to improve display of rows with very long
        filters list, like "All Scenes" or "Images" when opening scenes,
        e.g. in CastleFmxPlayAnimation.

      if J > 5 then
      begin
        OutFilter := OutFilter + '...';
        Break;
      end;
      }
      OutFilter := OutFilter + Filter.Patterns[J];
    end;

    OutFilter := OutFilter + '|';
  end;

  { LCL/FMX FilterIndex counts from 1.

    Except Delphi/Linux (FMXLinux) that seems (maybe a bug?) to count from 0.
    Testcase; CastleFmxPlayAnimation, press "Load scene" button,
    default filter should be "All Scenes". }
  {$if not (defined(DELPHI) and defined(LINUX))}
  Inc(OutFilterIndex);
  {$endif}
end;

class procedure TFileFilterList.LclFmxFiltersFromString(const FileFilters: string;
  out OutFilter: string; out OutFilterIndex: Integer; const AllFields: Boolean);
var
  FFList: TFileFilterList;
begin
  FFList := TFileFilterList.Create(true);
  try
    FFList.AddFiltersFromString(FileFilters);
    FFList.LclFmxFilters(OutFilter, OutFilterIndex, AllFields);
  finally FreeAndNil(FFList) end;
end;

end.
