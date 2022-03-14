// -*- compile-command: "./test_single_testcase.sh TTestCastleControls" -*-
{
  Copyright 2012-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleControls unit. }
unit TestCastleControls;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleControls = class(TCastleTestCase)
  published
    procedure TestFloatSliderRoundAndClamp;
    procedure TestAssigningImageURL;
  end;

implementation

uses CastleVectors, CastleControls, CastleImages;

procedure TTestCastleControls.TestFloatSliderRoundAndClamp;
const
  Epsilon = 0.001;
var
  F: TCastleFloatSlider;
begin
  F := TCastleFloatSlider.Create(nil);
  try
    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := 0;
    AssertSameValue(12, F.RoundAndClamp(12), Epsilon);
    AssertSameValue(11, F.RoundAndClamp(11), Epsilon);
    AssertSameValue(10.1, F.RoundAndClamp(10.1), Epsilon);
    AssertSameValue(10.9, F.RoundAndClamp(10.9), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(10), Epsilon);
    AssertSameValue(20, F.RoundAndClamp(20), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(5), Epsilon);
    AssertSameValue(20, F.RoundAndClamp(25), Epsilon);

    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := 3;
    AssertSameValue(12, F.RoundAndClamp(12), Epsilon);
    AssertSameValue(12, F.RoundAndClamp(11), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(10.1), Epsilon);
    AssertSameValue(12, F.RoundAndClamp(10.9), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(10), Epsilon);
    AssertSameValue(20, F.RoundAndClamp(20), Epsilon);
    AssertSameValue(15, F.RoundAndClamp(14), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(5), Epsilon);
    AssertSameValue(20, F.RoundAndClamp(25), Epsilon);

    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := -3;
    AssertSameValue(12, F.RoundAndClamp(12), Epsilon);
    AssertSameValue(12, F.RoundAndClamp(11), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(10.1), Epsilon);
    AssertSameValue(12, F.RoundAndClamp(10.9), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(10), Epsilon);
    AssertSameValue(20, F.RoundAndClamp(20), Epsilon);
    AssertSameValue(15, F.RoundAndClamp(14), Epsilon);
    AssertSameValue(10, F.RoundAndClamp(5), Epsilon);
    AssertSameValue(20, F.RoundAndClamp(25), Epsilon);

    F.Min := -20;
    F.Max := -10;
    F.MultipleOf := 3;
    AssertSameValue(-12, F.RoundAndClamp(-12), Epsilon);
    AssertSameValue(-12, F.RoundAndClamp(-11), Epsilon);
    AssertSameValue(-10, F.RoundAndClamp(-10.1), Epsilon);
    AssertSameValue(-12, F.RoundAndClamp(-10.9), Epsilon);
    AssertSameValue(-10, F.RoundAndClamp(-10), Epsilon);
    AssertSameValue(-20, F.RoundAndClamp(-20), Epsilon);
    AssertSameValue(-15, F.RoundAndClamp(-14), Epsilon);
    AssertSameValue(-10, F.RoundAndClamp(-5), Epsilon);
    AssertSameValue(-20, F.RoundAndClamp(-25), Epsilon);
  finally FreeAndNil(F) end;
end;

procedure TTestCastleControls.TestAssigningImageURL;
var
  C, C2: TCastleImageControl;
begin
  C := TCastleImageControl.Create(nil);
  try
    C2 := TCastleImageControl.Create(nil);
    try
      C.URL := 'castle-data:/images/alpha.png';
      C.URL := 'castle-data:/images/alpha.png';

      C2.URL := 'castle-data:/images/alpha.png';
      C2.URL := '';

      C.Image := LoadImage('castle-data:/images/alpha.png');
      C.Image := C.Image;
    finally FreeAndNil(C2) end;
  finally FreeAndNil(C) end;
end;

initialization
  RegisterTest(TTestCastleControls);
end.
