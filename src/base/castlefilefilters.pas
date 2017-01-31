{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ File filters, for TCastleWindowCustom.FileDialog and Lazarus file dialogs. }
unit CastleFileFilters;

{$I castleconf.inc}

interface

uses SysUtils, Classes, FGL;

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

  TFileFilterList = class(specialize TFPGObjectList<TFileFilter>)
  private
    FDefaultFilter: Integer;
  public
    constructor Create(AFreeObjects: boolean);

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
  end;

implementation

uses StrUtils, CastleStringUtils;

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

constructor TFileFilterList.Create(AFreeObjects: boolean);
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
  NamePart: boolean;
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

end.
