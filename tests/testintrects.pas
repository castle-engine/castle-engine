{
  Copyright 2004-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestIntRects;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestIntRects = class(TTestCase)
    procedure TestIntRects;
  end;

implementation

uses CastleVectors, IntRects;

procedure TTestIntRects.TestIntRects;
const 
  R1       :TIntRect = ((2, 4), (3, 5));
  EqualToR1: TIntRect = ((2, 4), (3, 5));
  R2: TIntRect = ((2, 4), (10, 5));
  R3: TIntRect = ((2, 4), (10, 50));
  R4: TIntRect = ((2, 4), (10, 6));
var
  R: TIntRect;
begin
 AssertTrue(RectsEqual(R1, R1));
 AssertTrue(RectsEqual(R1, EqualToR1));
 AssertTrue(not RectsEqual(R1, R2));
 
 AssertTrue(RectsEqual(IntRect(2, 4, 3, 5), R1));
 
 AssertTrue(IntRectToNiceStr(R1) = '(2,4)-(3,5)');
 
 AssertTrue(RectWidth(R1) = 1);
 AssertTrue(RectHeight(R1) = 1);
 AssertTrue(RectWidth(R2) = 8);
 AssertTrue(RectHeight(R2) = 1);
 
 AssertTrue(RectsEqual(IntRect(0, 2, 5, 7), GrowRect(R1, 2)));
 AssertTrue(RectsEqual(IntRect(3, 5, 9, 49), GrowRect(R3, -1)));
 
 R := CenteredRect(R4, 4, 10);
 AssertTrue(RectWidth(R) = 4);
 AssertTrue(RectHeight(R) = 10);
 AssertTrue(RectsEqual(IntRect(4, 0, 8, 10), R));
 
 AssertTrue(PointInRect(Vector2Integer(4, 4), R4));
 AssertTrue(PointInRect(Vector2Integer(2, 4), R4));
 AssertTrue(not PointInRect(Vector2Integer(2, 6), R4));
 AssertTrue(not PointInRect(Vector2Integer(2, 10), R4));
 
 AssertTrue(PointInRect(4, 4, R4));
 AssertTrue(PointInRect(2, 4, R4));
 AssertTrue(not PointInRect(2, 6, R4));
 AssertTrue(not PointInRect(2, 10, R4));
end;

initialization
 RegisterTest(TTestIntRects);
end.
