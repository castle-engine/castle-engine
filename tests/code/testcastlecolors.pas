// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleColors" -*-
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

{ Test CastleColors unit. }
unit TestCastleColors;

{$I castleconf.inc}

{ $define SPEED_TESTS}

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase, {$else}CastleTester,{$endif} CastleVectors, CastleColors;

type
  TTestCastleColors = class(TCastleTestCase)
  published
    procedure TestHSV;
    procedure TestLerpInHsv;
  end;

implementation

uses CastleUtils, CastleStringUtils, CastleTimeUtils;

procedure TTestCastleColors.TestHSV;
var
  RGB: TVector3Byte;
  HSV: TVector3;
  R, G, B: Integer;
  {$ifdef SPEED_TESTS}
  Operations: Integer;
  Time: Double;
  {$endif}
begin
  {$ifdef SPEED_TESTS}
  ProcessTimerBegin;
  {$endif}

  for R := 0 to 255 div 5 do
    for G := 0 to 255 div 5 do
      for B := 0 to 255 div 5 do
      begin
        RGB[0] := R * 5;
        RGB[1] := G * 5;
        RGB[2] := B * 5;
        HSV := RgbToHsv(RGB);
        { test trip to HSV and back returns the same }
        AssertTrue(TVector3Byte.Equals(RGB, HsvToRgbByte(HSV)));
        { test HSV components are in appropriate ranges }
        AssertTrue(Between(HSV[0], 0, 6));
        AssertTrue(Between(HSV[1], 0, 1));
        AssertTrue(Between(HSV[2], 0, 1));
      end;

  {$ifdef SPEED_TESTS}
  Operations := 255 div 5 + 1;
  Operations := Sqr(Operations) * Operations;
  Time := ProcessTimerEnd;

  { With FPC 2.4.4, speed is quite amazing:
    with -dRELEASE:
    HSV trip (RGB and back): average time is 0.00 secs per 1 operation (total 0.00 secs for 140608 operations)
    with -dDEBUG:
    HSV trip (RGB and back): average time is 0.00 secs per 1 operation (total 0.03 secs for 140608 operations)
  }

  Writeln(Format('HSV trip (RGB and back): average time is %f secs per 1 operation (total %f secs for %d operations)', [Time / Operations, Time, Operations]));
  {$endif}
end;

procedure TTestCastleColors.TestLerpInHsv;
const
  PureRed: TVector3 = (Data: (1, 0, 0));
  PureBlue: TVector3 = (Data: (0, 0, 1));
var
  I: Integer;
  C: TVector3;
  H: Single;
begin
  for I := 1 to 10 do
  begin
    C := LerpRgbInHsv(I / 10, BlackRGB, PureBlue);
    { interpolating from pure black to blue,
      all colors along the way should keep hue = blue }
    H := RgbToHsv(C)[0];
    AssertTrue(RgbToHsv(PureBlue)[0] = H);
//    Writeln(C.ToString, ' ', VectorToNiceStr(RgbToHsv(C)));
  end;

  for I := 0 to 10 do
  begin
    C := LerpRgbInHsv(I / 10, PureRed, PureBlue);
    { interpolate from hue 0 to hue 4 --- go down through 0 }
    H := RgbToHsv(C)[0];
    AssertTrue((H = 0.0) or Between(H, 4, 6));
//    Writeln(C.ToString, ' ', VectorToNiceStr(RgbToHsv(C)));
  end;

  for I := 0 to 10 do
  begin
    C := LerpRgbInHsv(I / 10, PureBlue, PureRed);
    { interpolate from hue 4 to hue 0 --- go up through 6 }
    H := RgbToHsv(C)[0];
    AssertTrue((H = 0.0) or Between(H, 4, 6));
//    Writeln(C.ToString, ' ', VectorToNiceStr(RgbToHsv(C)));
  end;

  AssertVectorEquals(LerpRgbInHsv(0, PureBlue, PureRed), PureBlue);
  AssertVectorEquals(LerpRgbInHsv(1, PureBlue, PureRed), PureRed);
  AssertVectorEquals(LerpRgbInHsv(0, PureRed, PureBlue), PureRed);
  AssertVectorEquals(LerpRgbInHsv(1, PureRed, PureBlue), PureBlue);
  AssertVectorEquals(LerpRgbInHsv(0, BlackRGB, PureRed), BlackRGB);
  AssertVectorEquals(LerpRgbInHsv(1, BlackRGB, PureRed), PureRed);
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCastleColors);
{$endif}
end.
