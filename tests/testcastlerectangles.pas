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
  end;

implementation

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

initialization
  RegisterTest(TTestRectangles);
end.
