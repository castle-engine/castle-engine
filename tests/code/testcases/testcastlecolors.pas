// -*- compile-command: "./test_single_testcase.sh TTestCastleColors" -*-
{
  Copyright 2011-2023 Michalis Kamburelis.

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

{ $define SPEED_TESTS}

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase, {$else}CastleTester,{$endif} CastleVectors, CastleColors;

type
  TTestCastleColors = class(TCastleTestCase)
  published
    procedure TestHsv;
    procedure TestHsvRgba;
    procedure TestLerpInHsv;
    procedure TestColorPickerCodeRgb;
    procedure TestColorPickerCodeRgba;
  end;

implementation

uses CastleUtils, CastleStringUtils, CastleTimeUtils, CastleScene, CastleControls;

procedure TTestCastleColors.TestHsv;
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
        RGB.X := R * 5;
        RGB.Y := G * 5;
        RGB.Z := B * 5;
        HSV := RgbToHsv(RGB);
        { test trip to HSV and back returns the same }
        AssertTrue(TVector3Byte.Equals(RGB, HsvToRgbByte(HSV)));
        { test HSV components are in appropriate ranges }
        AssertTrue(Between(HSV.X, 0, 6));
        AssertTrue(Between(HSV.Y, 0, 1));
        AssertTrue(Between(HSV.Z, 0, 1));
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
  PureRed : TVector3 = (X: 1; Y: 0; Z: 0);
  PureBlue: TVector3 = (X: 0; Y: 0; Z: 1);
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
    H := RgbToHsv(C).X;
    AssertTrue(RgbToHsv(PureBlue).X = H);
//    Writeln(C.ToString, ' ', VectorToNiceStr(RgbToHsv(C)));
  end;

  for I := 0 to 10 do
  begin
    C := LerpRgbInHsv(I / 10, PureRed, PureBlue);
    { interpolate from hue 0 to hue 4 --- go down through 0 }
    H := RgbToHsv(C).X;
    AssertTrue((H = 0.0) or Between(H, 4, 6));
//    Writeln(C.ToString, ' ', VectorToNiceStr(RgbToHsv(C)));
  end;

  for I := 0 to 10 do
  begin
    C := LerpRgbInHsv(I / 10, PureBlue, PureRed);
    { interpolate from hue 4 to hue 0 --- go up through 6 }
    H := RgbToHsv(C).X;
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

procedure TTestCastleColors.TestHsvRgba;
var
  RGBA, NewRGBA: TCastleColor;
  Hsv: TVector3;
begin
  RGBA := Vector4(0.1, 0.5, 0.7, 0.9);
  Hsv := RgbToHsv(RGBA.XYZ);
  NewRGBA := HsvToRgba(Hsv, 0.9);
  AssertVectorEquals(RGBA, NewRGBA);
end;

procedure TTestCastleColors.TestColorPickerCodeRgb;
var
  MyControl: TCastleBackground;
// Define a constant with hard-coded color value like this:
const
  MyColor: TCastleColorRGB = (X: 0.125; Y: 0.125; Z: 0.373);
begin
  MyControl := TCastleBackground.Create(nil);
  try
    // Set colors from a hard-coded value like this:
    MyControl.SkyTopColor := MyColor;
    MyControl.SkyTopColor := Vector3(0.125, 0.125, 0.373);
    MyControl.SkyTopColor := HsvToRgb(Vector3(4.000, 0.667, 0.373));
    MyControl.SkyTopColor := HexToColorRGB('20205F');
  finally FreeAndNil(MyControl) end;
end;

procedure TTestCastleColors.TestColorPickerCodeRgba;
var
  MyControl: TCastleImageControl;
// Define a constant with hard-coded color value like this:
const
  MyColor: TCastleColor = (X: 0.318; Y: 0.286; Z: 0.000; W: 0.598);
begin
  MyControl := TCastleImageControl.Create(nil);
  try
    // Set colors from a hard-coded value like this:
    MyControl.Color := MyColor;
    MyControl.Color := Vector4(0.318, 0.286, 0.000, 0.598);
    MyControl.Color := HsvToRgba(Vector3(0.901, 1.000, 0.318), 0.598);
    MyControl.Color := HexToColor('51490099');
  finally FreeAndNil(MyControl) end;
end;

initialization
  RegisterTest(TTestCastleColors);
end.
