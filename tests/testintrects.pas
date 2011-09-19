{
  Copyright 2004-2010 Michalis Kamburelis.

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

uses VectorMath, IntRects;

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
 Assert(RectsEqual(R1, R1));
 Assert(RectsEqual(R1, EqualToR1));
 Assert(not RectsEqual(R1, R2));
 
 Assert(RectsEqual(IntRect(2, 4, 3, 5), R1));
 
 Assert(IntRectToNiceStr(R1) = '(2,4)-(3,5)');
 
 Assert(RectWidth(R1) = 1);
 Assert(RectHeight(R1) = 1);
 Assert(RectWidth(R2) = 8);
 Assert(RectHeight(R2) = 1);
 
 Assert(RectsEqual(IntRect(0, 2, 5, 7), GrowRect(R1, 2)));
 Assert(RectsEqual(IntRect(3, 5, 9, 49), GrowRect(R3, -1)));
 
 R := CenteredRect(R4, 4, 10);
 Assert(RectWidth(R) = 4);
 Assert(RectHeight(R) = 10);
 Assert(RectsEqual(IntRect(4, 0, 8, 10), R));
 
 Assert(PointInRect(Vector2Integer(4, 4), R4));
 Assert(PointInRect(Vector2Integer(2, 4), R4));
 Assert(not PointInRect(Vector2Integer(2, 6), R4));
 Assert(not PointInRect(Vector2Integer(2, 10), R4));
 
 Assert(PointInRect(4, 4, R4));
 Assert(PointInRect(2, 4, R4));
 Assert(not PointInRect(2, 6, R4));
 Assert(not PointInRect(2, 10, R4));
end;

initialization
 RegisterTest(TTestIntRects);
end.