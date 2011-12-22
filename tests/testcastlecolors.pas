{
  Copyright 2011-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleColors;

{ $define SPEED_TESTS}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, VectorMath, CastleColors;

type
  TTestCastleColors = class(TTestCase)
  published
    procedure TestHSV;
  end;

implementation

uses CastleUtils, CastleStringUtils, CastleTimeUtils;

procedure TTestCastleColors.TestHSV;
var
  RGB: TVector3Byte;
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
        Assert(VectorsPerfectlyEqual(RGB, HsvToRgbByte(RgbToHsv(RGB))));
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

initialization
  RegisterTest(TTestCastleColors);
end.
