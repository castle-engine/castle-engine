{
  Copyright 2013-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleRectangles;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleRectangles;

type
  TTestRectangles = class(TTestCase)
  published
    procedure TestRectangles;
    procedure TestScaleEmpty;
    procedure TestCollidesDisc;
  end;

implementation

uses CastleVectors;

procedure TTestRectangles.TestRectangles;
var
  R: TRectangle;
begin
  R := Rectangle(-100, -200, 20, 30);
  AssertTrue(R.Contains(-100, -200));
  AssertTrue(R.Contains(-100 + 19, -200 + 29));
  AssertTrue(not R.Contains(-100 + 20, -200 + 30));

  R.Grow(5);
  AssertEquals(20, R.Width);
  R := R.Grow(5);
  AssertEquals(  30, R.Width );
  AssertEquals(  40, R.Height);
  AssertEquals(-105, R.Left  );
  AssertEquals(-205, R.Bottom);
  R := R.Grow(-5);
  AssertEquals(  20, R.Width );
  AssertEquals(  30, R.Height);
  AssertEquals(-100, R.Left  );
  AssertEquals(-200, R.Bottom);
  R := R.Grow(-10);
  AssertEquals(   0, R.Width );
  AssertEquals(  10, R.Height);
  AssertEquals( -90, R.Left  );
  AssertEquals(-190, R.Bottom);
  AssertFalse(R.Contains(-100, -200));
  AssertFalse(R.Contains(-100 + 19, -200 + 29));
  AssertFalse(R.Contains(-100 + 20, -200 + 30));
end;

procedure TTestRectangles.TestScaleEmpty;
var
  R: TRectangle;
begin
  R := Rectangle(10, 20, 0, 50);
  AssertEquals(0, R.ScaleWidthAround0(2));
  AssertEquals(100, R.ScaleHeightAround0(2)); // correctly scaled, even though R.Width = 0
  AssertEquals(0, R.ScaleAround0(2).Width);
  AssertEquals(100, R.ScaleAround0(2).Height); // correctly scaled, even though R.Width = 0
  AssertEquals(10, R.ScaleAround0(2).Left); // untouched by ScaleAround0, since R.Width = 0
  AssertEquals(10, R.ScaleAround0(123).Left); // untouched by ScaleAround0, since R.Width = 0
  AssertEquals(40, R.ScaleAround0(2).Bottom); // correctly scaled, even though R.Width = 0

  // analogous to above test, but swap Width and Height
  R := Rectangle(20, 10, 50, 0);
  AssertEquals(100, R.ScaleWidthAround0(2)); // correctly scaled, even though R.Height = 0
  AssertEquals(0, R.ScaleHeightAround0(2));
  AssertEquals(100, R.ScaleAround0(2).Width); // correctly scaled, even though R.Height = 0
  AssertEquals(0, R.ScaleAround0(2).Height);
  AssertEquals(10, R.ScaleAround0(2).Bottom); // untouched by ScaleAround0, since R.Height = 0
  AssertEquals(10, R.ScaleAround0(123).Bottom); // untouched by ScaleAround0, since R.Height = 0
  AssertEquals(40, R.ScaleAround0(2).Left); // correctly scaled, even though R.Height = 0

  R := Rectangle(10, 20, 0, 50);
  AssertEquals(0, R.ScaleAroundMiddle(2).Width);
  AssertEquals(100, R.ScaleAroundMiddle(2).Height); // correctly scaled, even though R.Width = 0
  AssertEquals(10, R.ScaleAroundMiddle(2).Left); // untouched by ScaleAroundMiddle, since R.Width = 0
  AssertEquals(10, R.ScaleAroundMiddle(123).Left); // untouched by ScaleAroundMiddle, since R.Width = 0
  AssertEquals(-5, R.ScaleAroundMiddle(2).Bottom); // correctly scaled, even though R.Width = 0
end;

procedure TTestRectangles.TestCollidesDisc;
var
  R: TFloatRectangle;
begin
  R := FloatRectangle(10, 20, 30, 40);
  // left = 10, right = 40, bottom = 20, top = 60

  { circles far outside }

  AssertFalse(R.CollidesDisc(Vector2Single(0 , 0), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(20, 0), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(50, 0), 1));

  AssertFalse(R.CollidesDisc(Vector2Single(0,  100), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(20, 100), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(50, 100), 1));

  AssertFalse(R.CollidesDisc(Vector2Single(0, 10), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(0, 40), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(0, 70), 1));

  AssertFalse(R.CollidesDisc(Vector2Single(100, 10), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(100, 40), 1));
  AssertFalse(R.CollidesDisc(Vector2Single(100, 70), 1));

  { circles collide, when one range inside }

  AssertFalse(R.CollidesDisc(Vector2Single(-10, 10), 15));
  AssertTrue(R.CollidesDisc(Vector2Single(20, 10), 15));
  AssertFalse(R.CollidesDisc(Vector2Single(60, 10), 15));

  AssertFalse(R.CollidesDisc(Vector2Single(-10, 70), 15));
  AssertTrue(R.CollidesDisc(Vector2Single(20, 70), 15));
  AssertFalse(R.CollidesDisc(Vector2Single(60, 70), 15));

  AssertFalse(R.CollidesDisc(Vector2Single(0, 0), 15));
  AssertTrue(R.CollidesDisc(Vector2Single(0, 40), 15));
  AssertFalse(R.CollidesDisc(Vector2Single(0, 80), 15));

  AssertFalse(R.CollidesDisc(Vector2Single(50, 0), 15));
  AssertTrue(R.CollidesDisc(Vector2Single(50, 40), 15));
  AssertFalse(R.CollidesDisc(Vector2Single(50, 80), 15));

  { circles collide, both ranges inside }

  AssertTrue(R.CollidesDisc(Vector2Single(20, 40), 1));

  R := FloatRectangle(0, 0, 10, 10);

  AssertFalse(R.CollidesDisc(Vector2Single(-1, -1), 0.9));
  AssertFalse(R.CollidesDisc(Vector2Single(-1,  5), 0.9));
  AssertFalse(R.CollidesDisc(Vector2Single(-1, 11), 0.9));

  AssertFalse(R.CollidesDisc(Vector2Single(-1, -1), 1.1));
  AssertTrue(R.CollidesDisc(Vector2Single(-1,  5), 1.1));
  AssertFalse(R.CollidesDisc(Vector2Single(-1, 11), 1.1));
end;

initialization
  RegisterTest(TTestRectangles);
end.
