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
var
  F: TCastleFloatSlider;
begin
  F := TCastleFloatSlider.Create(nil);
  try
    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := 0;
    AssertFloatsEqual(12, F.RoundAndClamp(12));
    AssertFloatsEqual(11, F.RoundAndClamp(11));
    AssertFloatsEqual(10.1, F.RoundAndClamp(10.1));
    AssertFloatsEqual(10.9, F.RoundAndClamp(10.9));
    AssertFloatsEqual(10, F.RoundAndClamp(10));
    AssertFloatsEqual(20, F.RoundAndClamp(20));
    AssertFloatsEqual(10, F.RoundAndClamp(5));
    AssertFloatsEqual(20, F.RoundAndClamp(25));

    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := 3;
    AssertFloatsEqual(12, F.RoundAndClamp(12));
    AssertFloatsEqual(12, F.RoundAndClamp(11));
    AssertFloatsEqual(10, F.RoundAndClamp(10.1));
    AssertFloatsEqual(12, F.RoundAndClamp(10.9));
    AssertFloatsEqual(10, F.RoundAndClamp(10));
    AssertFloatsEqual(20, F.RoundAndClamp(20));
    AssertFloatsEqual(15, F.RoundAndClamp(14));
    AssertFloatsEqual(10, F.RoundAndClamp(5));
    AssertFloatsEqual(20, F.RoundAndClamp(25));

    F.Min := 10;
    F.Max := 20;
    F.MultipleOf := -3;
    AssertFloatsEqual(12, F.RoundAndClamp(12));
    AssertFloatsEqual(12, F.RoundAndClamp(11));
    AssertFloatsEqual(10, F.RoundAndClamp(10.1));
    AssertFloatsEqual(12, F.RoundAndClamp(10.9));
    AssertFloatsEqual(10, F.RoundAndClamp(10));
    AssertFloatsEqual(20, F.RoundAndClamp(20));
    AssertFloatsEqual(15, F.RoundAndClamp(14));
    AssertFloatsEqual(10, F.RoundAndClamp(5));
    AssertFloatsEqual(20, F.RoundAndClamp(25));

    F.Min := -20;
    F.Max := -10;
    F.MultipleOf := 3;
    AssertFloatsEqual(-12, F.RoundAndClamp(-12));
    AssertFloatsEqual(-12, F.RoundAndClamp(-11));
    AssertFloatsEqual(-10, F.RoundAndClamp(-10.1));
    AssertFloatsEqual(-12, F.RoundAndClamp(-10.9));
    AssertFloatsEqual(-10, F.RoundAndClamp(-10));
    AssertFloatsEqual(-20, F.RoundAndClamp(-20));
    AssertFloatsEqual(-15, F.RoundAndClamp(-14));
    AssertFloatsEqual(-10, F.RoundAndClamp(-5));
    AssertFloatsEqual(-20, F.RoundAndClamp(-25));
  finally FreeAndNil(F) end;
end;

initialization
  RegisterTest(TTestCastleControls);
end.
