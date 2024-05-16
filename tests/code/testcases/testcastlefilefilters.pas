// -*- compile-command: "./test_single_testcase.sh TTestCastleFileFilters" -*-
{
  Copyright 2011-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleFileFilters unit. }
unit TestCastleFileFilters;

interface

uses CastleTester;

type
  TTestCastleFileFilters = class(TCastleTestCase)
  published
    procedure TestFileFilterFromString;
  end;

implementation

uses SysUtils, Classes, CastleFileFilters;

{ Account for hack done in TFileFilterList.LclFmxFiltersFromString
  to account for FMX Linux bug: filter index is 0-based then. }
{$ifndef FPC}
  {$ifdef LINUX}
    {$define CASTLE_ASSERT_FILTER_BUMPED_1}
  {$endif}
{$endif}

procedure TTestCastleFileFilters.TestFileFilterFromString;
var
  LCLFilter: string;
  LCLFilterIndex: Integer;
begin
  TFileFilterList.LclFmxFiltersFromString('All files (*)|*|*All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg',
    LCLFilter, LCLFilterIndex);
  AssertTrue(LCLFilter = 'All files (*)|*|All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg|');
  AssertEquals(2 {$ifdef CASTLE_ASSERT_FILTER_BUMPED_1} -1 {$endif}, LCLFilterIndex);

  TFileFilterList.LclFmxFiltersFromString('All files (*)|*|*All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg',
    LCLFilter, LCLFilterIndex, false);
  AssertTrue(LCLFilter = 'PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg|');
  AssertEquals(1 {$ifdef CASTLE_ASSERT_FILTER_BUMPED_1} -1 {$endif}, LCLFilterIndex);

  TFileFilterList.LclFmxFiltersFromString('All files (*)|*|All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|*JPEG images (*.jpg)|*.jpg',
    LCLFilter, LCLFilterIndex, false);
  AssertTrue(LCLFilter = 'PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg|');
  AssertEquals(2 {$ifdef CASTLE_ASSERT_FILTER_BUMPED_1} -1 {$endif}, LCLFilterIndex);
end;

initialization
  RegisterTest(TTestCastleFileFilters);
end.
