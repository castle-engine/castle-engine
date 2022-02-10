// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestSpaceFillingCurves" -*-
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleSpaceFillingCurves unit. }
unit TestCastleSpaceFillingCurves;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry,
  CastleTestCase;

type
  TTestSpaceFillingCurves = class(TCastleTestCase)
  published
    procedure TestSpaceFillingCurves;
  end;

implementation

uses CastleVectors, CastleImages, CastleInternalSpaceFillingCurves, CastleUtils,
  CastleColors;

const
  Red3Byte  : TVector3Byte = (X: 255; Y: 0; Z: 0);
  Green3Byte: TVector3Byte = (X: 0; Y: 255; Z: 0);
  Red4Byte  : TVector4Byte = (X: 255; Y: 0; Z: 0; W: 255);
  Green4Byte: TVector4Byte = (X: 0; Y: 255; Z: 0; W: 255);

procedure TTestSpaceFillingCurves.TestSpaceFillingCurves;

  procedure TestCurve(CurveClass: TSpaceFillingCurveClass; Width, Height: Cardinal);
  { sprawdz ze curve wypelnia caly obrazek, i tylko caly obrazek,
    i kazdy pixel obrazka jest wypelniony dokladnie raz. }
  var Img: TCastleImage;
      Curve: TSpaceFillingCurve;
      pixCoords: TVector2Cardinal;
      pix: PVector3Byte;
      OutFileName: string;
  begin
   Curve := nil;
   Img := TRGBImage.Create(Width, Height);
   try
    try
     Curve := CurveClass.Create(Width, Height);

     Img.Clear(Green4Byte);

     while not Curve.EndOfPixels do
     begin
      pixCoords := Curve.NextPixel;
      AssertTrue(Between(pixCoords[0], 0, Width-1));
      AssertTrue(Between(pixCoords[1], 0, Height-1));
      pix := PVector3Byte(Img.PixelPtr(pixCoords[0], pixCoords[1]));
      { kazdy pix moze byc podany tylko raz, czyli teraz pix powinien
	byc zielony. }
      AssertTrue(CompareMem(pix, @Green3Byte, SizeOf(TVector3Byte)));
      pix^ := Red3Byte;
     end;

     { na koncu caly obrazek powinien byc czerwony }
     AssertTrue(Img.IsClear(Red4Byte));
    except
     OutFileName := GetTempDir + '/test_space_filling_curves.ppm';
     SaveImage(Img, OutFileName);
     Writeln(Format('and it failed at curveClass %s, Width %d, Height %d',
       [CurveClass.ClassName, Width, Height]), nl,
       'dump written to ' + OutFileName);
     raise;
    end;
   finally
    Curve.Free;
    FreeAndNil(Img)
   end;
  end;

  procedure TestCurves(Width, Height: Cardinal);
  begin
   TestCurve(TSwapScanCurve, Width, Height);
   TestCurve(THilbertCurve, Width, Height);
   TestCurve(TPeanoCurve, Width, Height);
  end;

var i: integer;
begin
 for i := 1 to 20 do TestCurves(Random(100), Random(100));
 { perfidne testy sprawdzajace czy nasze curve'y radza sobie tez gdy jeden
   z Width, Height = 0 }
 TestCurves(0, 0);
 TestCurves(10, 0);
 TestCurves(0, 10);
end;

initialization
 RegisterTest(TTestSpaceFillingCurves);
end.
