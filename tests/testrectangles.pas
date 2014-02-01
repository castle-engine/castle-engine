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

unit TestRectangles;

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
  R, R1: TRectangle;
begin
  R := Rectangle(-100, -200, 20, 30);
  Assert(R.Contains(-100, -200));
  Assert(R.Contains(-100 + 19, -200 + 29));
  Assert(not R.Contains(-100 + 20, -200 + 30));

  R.Grow(5);
  Assert(R.Width = 20);
  R := R.Grow(5);
  Assert(R.Width = 30);
  Assert(R.Height = 40);
  Assert(R.Left = -105);
  Assert(R.Bottom = -205);
  R := R.Grow(-5);
  Assert(R.Width = 20);
  Assert(R.Height = 30);
  Assert(R.Left = -100);
  Assert(R.Bottom = -200);
  R := R.Grow(-10);
  Assert(R.Width = 0);
  Assert(R.Height = 10);
  Assert(R.Left = -90);
  Assert(R.Bottom = -190);
  Assert(not R.Contains(-100, -200));
  Assert(not R.Contains(-100 + 19, -200 + 29));
  Assert(not R.Contains(-100 + 20, -200 + 30));
end;

initialization
  RegisterTest(TTestRectangles);
end.
