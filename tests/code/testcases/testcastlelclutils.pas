// -*- compile-command: "./test_single_testcase.sh TTestCastleLCLUtils" -*-
{
  Copyright 2011-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleLCLUtils unit. }
unit TestCastleLCLUtils;

interface

uses FpcUnit, TestUtils, TestRegistry;

type
  TTestCastleLCLUtils = class(TTestCase)
  published
    procedure TestCastleLCLUtils;
  end;

implementation

uses SysUtils, Classes, CastleLCLUtils;

procedure TTestCastleLCLUtils.TestCastleLCLUtils;
var
  LCLFilter: string;
  LCLFilterIndex: Integer;
begin
  FileFiltersToDialog('All files (*)|*|*All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg',
    LCLFilter, LCLFilterIndex);
  AssertTrue(LCLFilter = 'All files (*)|*|All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg|');
  AssertTrue(LCLFilterIndex = 2);

  FileFiltersToDialog('All files (*)|*|*All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg',
    LCLFilter, LCLFilterIndex, false);
  AssertTrue(LCLFilter = 'PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg|');
  AssertTrue(LCLFilterIndex = 1);

  FileFiltersToDialog('All files (*)|*|All images (*.png;*.jpg)|*.png;*.jpg|PNG images (*.png)|*.png|*JPEG images (*.jpg)|*.jpg',
    LCLFilter, LCLFilterIndex, false);
  AssertTrue(LCLFilter = 'PNG images (*.png)|*.png|JPEG images (*.jpg)|*.jpg|');
  AssertTrue(LCLFilterIndex = 2);
end;

initialization
  RegisterTest(TTestCastleLCLUtils);
end.
