{
  Copyright 2012-2017 Michalis Kamburelis.

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
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleBaseTestCase;

type
  TTestCastleControls = class(TCastleBaseTestCase)
  published
    procedure TestFloatSliderRoundAndClamp;
  end;

implementation

uses CastleVectors, CastleControls;

procedure TTestCastleControls.TestFloatSliderRoundAndClamp;
const
  EqualityEpsilon = 0.001;
var
  F: TCastleFloatSlider;
begin
  F := TCastleFloatSlider.Create(nil);
  try
    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := 0;
    AssertFloatsEqual(12, F.RoundAndClamp(12), EqualityEpsilon);
    AssertFloatsEqual(11, F.RoundAndClamp(11), EqualityEpsilon);
    AssertFloatsEqual(10.1, F.RoundAndClamp(10.1), EqualityEpsilon);
    AssertFloatsEqual(10.9, F.RoundAndClamp(10.9), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(10), EqualityEpsilon);
    AssertFloatsEqual(20, F.RoundAndClamp(20), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(5), EqualityEpsilon);
    AssertFloatsEqual(20, F.RoundAndClamp(25), EqualityEpsilon);

    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := 3;
    AssertFloatsEqual(12, F.RoundAndClamp(12), EqualityEpsilon);
    AssertFloatsEqual(12, F.RoundAndClamp(11), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(10.1), EqualityEpsilon);
    AssertFloatsEqual(12, F.RoundAndClamp(10.9), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(10), EqualityEpsilon);
    AssertFloatsEqual(20, F.RoundAndClamp(20), EqualityEpsilon);
    AssertFloatsEqual(15, F.RoundAndClamp(14), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(5), EqualityEpsilon);
    AssertFloatsEqual(20, F.RoundAndClamp(25), EqualityEpsilon);

    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := -3;
    AssertFloatsEqual(12, F.RoundAndClamp(12), EqualityEpsilon);
    AssertFloatsEqual(12, F.RoundAndClamp(11), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(10.1), EqualityEpsilon);
    AssertFloatsEqual(12, F.RoundAndClamp(10.9), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(10), EqualityEpsilon);
    AssertFloatsEqual(20, F.RoundAndClamp(20), EqualityEpsilon);
    AssertFloatsEqual(15, F.RoundAndClamp(14), EqualityEpsilon);
    AssertFloatsEqual(10, F.RoundAndClamp(5), EqualityEpsilon);
    AssertFloatsEqual(20, F.RoundAndClamp(25), EqualityEpsilon);

    F.Min := -20;
    F.Max := -10;
    F.MultipleOf := 3;
    AssertFloatsEqual(-12, F.RoundAndClamp(-12), EqualityEpsilon);
    AssertFloatsEqual(-12, F.RoundAndClamp(-11), EqualityEpsilon);
    AssertFloatsEqual(-10, F.RoundAndClamp(-10.1), EqualityEpsilon);
    AssertFloatsEqual(-12, F.RoundAndClamp(-10.9), EqualityEpsilon);
    AssertFloatsEqual(-10, F.RoundAndClamp(-10), EqualityEpsilon);
    AssertFloatsEqual(-20, F.RoundAndClamp(-20), EqualityEpsilon);
    AssertFloatsEqual(-15, F.RoundAndClamp(-14), EqualityEpsilon);
    AssertFloatsEqual(-10, F.RoundAndClamp(-5), EqualityEpsilon);
    AssertFloatsEqual(-20, F.RoundAndClamp(-25), EqualityEpsilon);
  finally FreeAndNil(F) end;
end;

initialization
  RegisterTest(TTestCastleControls);
end.
