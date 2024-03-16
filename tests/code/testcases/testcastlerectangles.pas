// -*- compile-command: "./test_single_testcase.sh TTestRectangles" -*-
{
  Copyright 2013-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleRectangles unit. }
unit TestCastleRectangles;

interface

uses
  Classes, SysUtils,
  CastleTester, CastleRectangles;

type
  TTestRectangles = class(TCastleTestCase)
  published
    procedure TestRectangles;
    procedure TestScaleEmpty;
    procedure TestCollidesDisc;
    procedure TestAdd;
    // procedure TestRightTop;
    procedure TestAlign;
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
  AssertEquals(0, R.ScaleAroundCenter(2).Width);
  AssertEquals(100, R.ScaleAroundCenter(2).Height); // correctly scaled, even though R.Width = 0
  AssertEquals(10, R.ScaleAroundCenter(2).Left); // untouched by ScaleAroundCenter, since R.Width = 0
  AssertEquals(10, R.ScaleAroundCenter(123).Left); // untouched by ScaleAroundCenter, since R.Width = 0
  AssertEquals(-5, R.ScaleAroundCenter(2).Bottom); // correctly scaled, even though R.Width = 0
end;

procedure TTestRectangles.TestCollidesDisc;
var
  R: TFloatRectangle;
begin
  R := FloatRectangle(10, 20, 30, 40);
  // left = 10, right = 40, bottom = 20, top = 60

  { circles far outside }

  AssertFalse(R.CollidesDisc(Vector2(0 , 0), 1));
  AssertFalse(R.CollidesDisc(Vector2(20, 0), 1));
  AssertFalse(R.CollidesDisc(Vector2(50, 0), 1));

  AssertFalse(R.CollidesDisc(Vector2(0,  100), 1));
  AssertFalse(R.CollidesDisc(Vector2(20, 100), 1));
  AssertFalse(R.CollidesDisc(Vector2(50, 100), 1));

  AssertFalse(R.CollidesDisc(Vector2(0, 10), 1));
  AssertFalse(R.CollidesDisc(Vector2(0, 40), 1));
  AssertFalse(R.CollidesDisc(Vector2(0, 70), 1));

  AssertFalse(R.CollidesDisc(Vector2(100, 10), 1));
  AssertFalse(R.CollidesDisc(Vector2(100, 40), 1));
  AssertFalse(R.CollidesDisc(Vector2(100, 70), 1));

  { circles collide, when one range inside }

  AssertFalse(R.CollidesDisc(Vector2(-10, 10), 15));
  AssertTrue(R.CollidesDisc(Vector2(20, 10), 15));
  AssertFalse(R.CollidesDisc(Vector2(60, 10), 15));

  AssertFalse(R.CollidesDisc(Vector2(-10, 70), 15));
  AssertTrue(R.CollidesDisc(Vector2(20, 70), 15));
  AssertFalse(R.CollidesDisc(Vector2(60, 70), 15));

  AssertFalse(R.CollidesDisc(Vector2(0, 0), 15));
  AssertTrue(R.CollidesDisc(Vector2(0, 40), 15));
  AssertFalse(R.CollidesDisc(Vector2(0, 80), 15));

  AssertFalse(R.CollidesDisc(Vector2(50, 0), 15));
  AssertTrue(R.CollidesDisc(Vector2(50, 40), 15));
  AssertFalse(R.CollidesDisc(Vector2(50, 80), 15));

  { circles collide, both ranges inside }

  AssertTrue(R.CollidesDisc(Vector2(20, 40), 1));

  R := FloatRectangle(0, 0, 10, 10);

  AssertFalse(R.CollidesDisc(Vector2(-1, -1), 0.9));
  AssertFalse(R.CollidesDisc(Vector2(-1,  5), 0.9));
  AssertFalse(R.CollidesDisc(Vector2(-1, 11), 0.9));

  AssertFalse(R.CollidesDisc(Vector2(-1, -1), 1.1));
  AssertTrue(R.CollidesDisc(Vector2(-1,  5), 1.1));
  AssertFalse(R.CollidesDisc(Vector2(-1, 11), 1.1));
end;

procedure TTestRectangles.TestAdd;
var
  R: TFloatRectangle;
begin
  R := TFloatRectangle.Empty;

  R.Include(Vector2(10, 20)); // without assignment, R.Add does nothing
  AssertTrue(R.IsEmpty);

  R := R.Include(Vector2(10, 20));
  AssertFalse(R.IsEmpty);
  AssertSameValue(10, R.Left);
  AssertSameValue(20, R.Bottom);
  AssertSameValue(0, R.Width);
  AssertSameValue(0, R.Height);

  R.Include(Vector2(0, 40)); // without assignment, R.Add does nothing
  AssertSameValue(10, R.Left);
  AssertSameValue(20, R.Bottom);
  AssertSameValue(0, R.Width);
  AssertSameValue(0, R.Height);

  R := R.Include(Vector2(0, 40));
  AssertSameValue(0, R.Left);
  AssertSameValue(20, R.Bottom);
  AssertSameValue(10, R.Width);
  AssertSameValue(20, R.Height);

  R := R.Include(Vector2(5, 30)); // does not change R, since already inside
  AssertSameValue(0, R.Left);
  AssertSameValue(20, R.Bottom);
  AssertSameValue(10, R.Width);
  AssertSameValue(20, R.Height);

  R := R.Include(Vector2(-10, 30)); // changes R only horizontally
  AssertSameValue(-10, R.Left);
  AssertSameValue(20, R.Bottom);
  AssertSameValue(20, R.Width);
  AssertSameValue(20, R.Height);

  R := R.Include(Vector2(5, -50)); // changes R only vertically
  AssertSameValue(-10, R.Left);
  AssertSameValue(-50, R.Bottom);
  AssertSameValue(20, R.Width);
  AssertSameValue(90, R.Height);

  R := R.Include(Vector2(5, -25)); // should not change R
  AssertSameValue(-10, R.Left);
  AssertSameValue(-50, R.Bottom);
  AssertSameValue(20, R.Width);
  AssertSameValue(90, R.Height);
end;

{
procedure TTestRectangles.TestRightTop;
var
  RInt: TRectangle;
  R: TFloatRectangle;
begin
  RInt.Left := 10;
  RInt.Bottom := 50;
  RInt.Right := 100;
  RInt.Top := 1000;
  AssertEquals(90, RInt.Width);
  AssertEquals(950, RInt.Height);
  AssertEquals(10, RInt.Left);
  AssertEquals(50, RInt.Bottom);
  AssertEquals(100, RInt.Right);
  AssertEquals(1000, RInt.Top);

  R.Left := 10;
  R.Bottom := 50;
  R.Right := 100;
  R.Top := 1000;
  AssertSameValue(90, R.Width);
  AssertSameValue(950, R.Height);
  AssertSameValue(10, R.Left);
  AssertSameValue(50, R.Bottom);
  AssertSameValue(100, R.Right);
  AssertSameValue(1000, R.Top);
end;
}

procedure TTestRectangles.TestAlign;
var
  WindowRect, ImageRect, TextRect: TRectangle;
  UIScale: Single;
begin
  WindowRect := Rectangle(0, 0, 1920, 1080);
  ImageRect := Rectangle(0, 0, 237, 50);
  UIScale := 1.2;
  TextRect := ImageRect.
    ScaleAroundCenter(UIScale).
    Align(hpMiddle, WindowRect, hpMiddle).
    Align(vpMiddle, WindowRect, vpMiddle);
  // Writeln(TextRect.ToString); // 818x510 284x60
  AssertEquals(818, TextRect.Left);
  AssertEquals(510, TextRect.Bottom);
  AssertEquals(284, TextRect.Width);
  AssertEquals(60, TextRect.Height);
end;

initialization
  RegisterTest(TTestRectangles);
end.
