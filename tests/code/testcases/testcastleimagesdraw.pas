// -*- compile-command: "./test_single_testcase.sh TTestImagesDraw" -*-
{
  Copyright 2015-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleImagesDraw;

interface

uses
  {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry, CastleTestCase,{$else}
  CastleTester, {$endif}CastleImages;

type
  TTestImagesDraw = class(TCastleTestCase)
  private
    Rgb, Rgb2: TRGBImage;
    RgbAlpha, RgbAlpha2: TRGBAlphaImage;
    Gray, Gray2: TGrayscaleImage;
    GrayAlpha, GrayAlpha2: TGrayscaleAlphaImage;
    function BlendBytes(const Dest, Source, Opacity: Byte): Byte;
    //function AddBytes(const Dest, Source, Opacity: Byte): Byte;
    function AddBytesPremultiplied(const Dest, Source: Byte): Byte;
    procedure ResetImages;
    procedure FreeImages;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestDrawToRgb;
    procedure TestDrawToRgbAlpha;
    procedure TestDrawToGrayscale;
    procedure TestDrawToGrayscaleAlpha;
  end;

implementation

uses SysUtils, CastleVectors, CastleColors;

function TTestImagesDraw.BlendBytes(const Dest, Source, Opacity: Byte): Byte; {$ifdef FPC}{$ifdef SUPPORTS_INLINE} inline; {$endif}{$endif}
var
  W: Word;
begin
  W :=
    Word(Dest  ) * (255 - Opacity) div 255 +
    Word(Source) * Opacity         div 255;
  if W > 255 then W := 255;
  Result := W;
end;

(*
function TTestImagesDraw.AddBytes(const Dest, Source, Opacity: Byte): Byte; {$ifdef FPC}{$ifdef SUPPORTS_INLINE} inline; {$endif}{$endif}
var
  W: Word;
begin
  W := Dest + Word(Source) * Opacity div 255;
  if W > 255 then W := 255;
  Result := W;
end;
*)

function TTestImagesDraw.AddBytesPremultiplied(const Dest, Source: Byte): Byte; {$ifdef FPC}{$ifdef SUPPORTS_INLINE} inline; {$endif}{$endif}
var
  W: Word;
begin
  W := Dest + Source;
  if W > 255 then W := 255;
  Result := W;
end;

procedure TTestImagesDraw.Setup;
begin
  inherited;

  Rgb := nil;
  RgbAlpha := nil;
  Gray := nil;
  GrayAlpha := nil;

  ResetImages;
end;

procedure TTestImagesDraw.TearDown;
begin
  FreeImages;
  inherited;
end;

procedure TTestImagesDraw.FreeImages;
begin
  FreeAndNil(Rgb);
  FreeAndNil(Rgb2);
  FreeAndNil(RgbAlpha);
  FreeAndNil(RgbAlpha2);
  FreeAndNil(Gray);
  FreeAndNil(Gray2);
  FreeAndNil(GrayAlpha);
  FreeAndNil(GrayAlpha2);
end;

procedure TTestImagesDraw.ResetImages;
begin
  FreeImages;

  Rgb := TRGBImage.Create(2, 2);
  Rgb.PixelPtr(0, 0)^ := Vector3Byte(1, 2, 3);
  Rgb.PixelPtr(0, 1)^ := Vector3Byte(4, 5, 6);
  Rgb.PixelPtr(1, 0)^ := Vector3Byte(11, 22, 33);
  Rgb.PixelPtr(1, 1)^ := Vector3Byte(44, 55, 66);

  Rgb2 := Rgb.MakeCopy as TRGBImage;

  RgbAlpha := TRGBAlphaImage.Create(2, 2);
  RgbAlpha.PixelPtr(0, 0)^ := Vector4Byte(7, 8, 9, 128);
  RgbAlpha.PixelPtr(0, 1)^ := Vector4Byte(3, 2, 1, 128);
  RgbAlpha.PixelPtr(1, 0)^ := Vector4Byte(77, 88, 99, 128);
  RgbAlpha.PixelPtr(1, 1)^ := Vector4Byte(33, 22, 11, 128);

  RgbAlpha2 := RgbAlpha.MakeCopy as TRGBAlphaImage;

  Gray := TGrayscaleImage.Create(2, 2);
  Gray.PixelPtr(0, 0)^ := 100;
  Gray.PixelPtr(0, 1)^ := 101;
  Gray.PixelPtr(1, 0)^ := 102;
  Gray.PixelPtr(1, 1)^ := 103;

  Gray2 := Gray.MakeCopy as TGrayscaleImage;

  GrayAlpha := TGrayscaleAlphaImage.Create(2, 2);
  GrayAlpha.PixelPtr(0, 0)^ := Vector2Byte(200, 128);
  GrayAlpha.PixelPtr(0, 1)^ := Vector2Byte(201, 128);
  GrayAlpha.PixelPtr(1, 0)^ := Vector2Byte(202, 128);
  GrayAlpha.PixelPtr(1, 1)^ := Vector2Byte(203, 128);

  GrayAlpha2 := GrayAlpha.MakeCopy as TGrayscaleAlphaImage;
end;

procedure TTestImagesDraw.TestDrawToRgb;
begin
  { draw from rgb }
  ResetImages;
  Rgb2.DrawTo(Rgb, 0, 1, dmBlend);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 1)^);

  ResetImages;
  Rgb2.DrawTo(Rgb, 0, 1, dmAdd);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(4 + 1, 5 + 2, 6 + 3), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(44 + 11, 55 + 22, 66 + 33), Rgb.PixelPtr(1, 1)^);

  { draw from rgb + alpha }
  ResetImages;
  RgbAlpha.DrawTo(Rgb, 0, 1, dmBlend);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(BlendBytes(4, 7, 128), BlendBytes(5, 8, 128), BlendBytes(6, 9, 128)), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(BlendBytes(44, 77, 128), BlendBytes(55, 88, 128), BlendBytes(66, 99, 128)), Rgb.PixelPtr(1, 1)^);

  ResetImages;
  RgbAlpha.DrawTo(Rgb, 0, 1, dmAdd);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(4 + 7 div 2, 5 + 8 div 2, 6 + 9 div 2), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(44 + 77 div 2, 55 + 88 div 2, 66 + 99 div 2), Rgb.PixelPtr(1, 1)^);

  { draw from grayscale }
  ResetImages;
  Gray.DrawTo(Rgb, 0, 1, dmBlend);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(100, 100, 100), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(102, 102, 102), Rgb.PixelPtr(1, 1)^);

  ResetImages;
  Gray.DrawTo(Rgb, 0, 1, dmAdd);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(4 + 100, 5 + 100, 6 + 100), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(44 + 102, 55 + 102, 66 + 102), Rgb.PixelPtr(1, 1)^);

  { draw from grayscale + alpha }
  ResetImages;
  GrayAlpha.DrawTo(Rgb, 0, 1, dmBlend);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(BlendBytes(4, 200, 128), BlendBytes(5, 200, 128), BlendBytes(6, 200, 128)), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(BlendBytes(44, 202, 128), BlendBytes(55, 202, 128), BlendBytes(66, 202, 128)), Rgb.PixelPtr(1, 1)^);

  ResetImages;
  GrayAlpha.DrawTo(Rgb, 0, 1, dmAdd);
  AssertVectorEquals(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector3Byte(4 + 200 div 2, 5 + 200 div 2, 6 + 200 div 2), Rgb.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector3Byte(44 + 202 div 2, 55 + 202 div 2, 66 + 202 div 2), Rgb.PixelPtr(1, 1)^);
end;

procedure TTestImagesDraw.TestDrawToRgbAlpha;
begin
  { draw from rgb }
  ResetImages;
  Rgb.DrawTo(RgbAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(1, 2, 3, 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(11, 22, 33, 128), RgbAlpha.PixelPtr(1, 1)^);

  ResetImages;
  Rgb.DrawTo(RgbAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(3 + 1, 2 + 2, 1 + 3, 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(33 + 11, 22 + 22, 11 + 33, 128), RgbAlpha.PixelPtr(1, 1)^);

  { draw from rgb + alpha }
  ResetImages;
  RgbAlpha2.DrawTo(RgbAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(BlendBytes(3, 7, 128), BlendBytes(2, 8, 128), BlendBytes(1, 9, 128), 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(BlendBytes(33, 77, 128), BlendBytes(22, 88, 128), BlendBytes(11, 99, 128), 128), RgbAlpha.PixelPtr(1, 1)^);

  ResetImages;
  RgbAlpha2.DrawTo(RgbAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(3 + 7 div 2, 2 + 8 div 2, 1 + 9 div 2, 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(33 + 77 div 2, 22 + 88 div 2, 11 + 99 div 2, 128), RgbAlpha.PixelPtr(1, 1)^);

  { draw from grayscale }
  ResetImages;
  Gray.DrawTo(RgbAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(100, 100, 100, 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(102, 102, 102, 128), RgbAlpha.PixelPtr(1, 1)^);

  ResetImages;
  Gray.DrawTo(RgbAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(3 + 100, 2 + 100, 1 + 100, 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(33 + 102, 22 + 102, 11 + 102, 128), RgbAlpha.PixelPtr(1, 1)^);

  { draw from grayscale + alpha }
  ResetImages;
  GrayAlpha.DrawTo(RgbAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(BlendBytes(3, 200, 128), BlendBytes(2, 200, 128), BlendBytes(1, 200, 128), 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(BlendBytes(33, 202, 128), BlendBytes(22, 202, 128), BlendBytes(11, 202, 128), 128), RgbAlpha.PixelPtr(1, 1)^);

  ResetImages;
  GrayAlpha.DrawTo(RgbAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector4Byte(7, 8, 9, 128), RgbAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector4Byte(3 + 200 div 2, 2 + 200 div 2, 1 + 200 div 2, 128), RgbAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector4Byte(77, 88, 99, 128), RgbAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector4Byte(33 + 202 div 2, 22 + 202 div 2, 11 + 202 div 2, 128), RgbAlpha.PixelPtr(1, 1)^);
end;

procedure TTestImagesDraw.TestDrawToGrayscale;
begin
  { draw from rgb + alpha }
  ResetImages;
  RgbAlpha.DrawTo(Gray, 0, 1, dmBlend);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(BlendBytes(101, GrayscaleValue(Vector3Byte(7, 8, 9)), 128), Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(BlendBytes(103, GrayscaleValue(Vector3Byte(77, 88, 99)), 128), Gray.PixelPtr(1, 1)^);

  ResetImages;
  RgbAlpha.DrawTo(Gray, 0, 1, dmAdd);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(101 + GrayscaleValue(Vector3Byte(7, 8, 9)) div 2, Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(103 + GrayscaleValue(Vector3Byte(77, 88, 99)) div 2, Gray.PixelPtr(1, 1)^);

  { draw from rgb }
  ResetImages;
  Rgb.DrawTo(Gray, 0, 1, dmBlend);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(GrayscaleValue(Vector3Byte(1, 2, 3)), Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(GrayscaleValue(Vector3Byte(11, 22, 33)), Gray.PixelPtr(1, 1)^);

  ResetImages;
  Rgb.DrawTo(Gray, 0, 1, dmAdd);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(101 + GrayscaleValue(Vector3Byte(1, 2, 3)), Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(103 + GrayscaleValue(Vector3Byte(11, 22, 33)), Gray.PixelPtr(1, 1)^);

  { draw from grayscale }
  ResetImages;
  Gray2.DrawTo(Gray, 0, 1, dmBlend);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(100, Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(102, Gray.PixelPtr(1, 1)^);

  ResetImages;
  Gray2.DrawTo(Gray, 0, 1, dmAdd);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(101 + 100, Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(103 + 102, Gray.PixelPtr(1, 1)^);

  { draw from grayscale + alpha }
  ResetImages;
  GrayAlpha.DrawTo(Gray, 0, 1, dmBlend);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(BlendBytes(101, 200, 128), Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(BlendBytes(103, 202, 128), Gray.PixelPtr(1, 1)^);

  ResetImages;
  GrayAlpha.DrawTo(Gray, 0, 1, dmAdd);
  AssertEquals(100, Gray.PixelPtr(0, 0)^);
  AssertEquals(101 + 200 div 2, Gray.PixelPtr(0, 1)^);
  AssertEquals(102, Gray.PixelPtr(1, 0)^);
  AssertEquals(103 + 202 div 2, Gray.PixelPtr(1, 1)^);
end;

procedure TTestImagesDraw.TestDrawToGrayscaleAlpha;
begin
  { draw from rgb }
  ResetImages;
  Rgb.DrawTo(GrayAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(GrayscaleValue(Vector3Byte(1, 2, 3)), 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(GrayscaleValue(Vector3Byte(11, 22, 33)), 128), GrayAlpha.PixelPtr(1, 1)^);

  ResetImages;
  Rgb.DrawTo(GrayAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(201 + GrayscaleValue(Vector3Byte(1, 2, 3)), 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(203 + GrayscaleValue(Vector3Byte(11, 22, 33)), 128), GrayAlpha.PixelPtr(1, 1)^);

  { draw from rgb + alpha }
  ResetImages;
  RgbAlpha.DrawTo(GrayAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(BlendBytes(201, GrayscaleValue(Vector3Byte(7, 8, 9)), 128), 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(BlendBytes(203, GrayscaleValue(Vector3Byte(77, 88, 99)), 128), 128), GrayAlpha.PixelPtr(1, 1)^);

  ResetImages;
  RgbAlpha.DrawTo(GrayAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(201 + GrayscaleValue(Vector3Byte(7, 8, 9)) div 2, 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(203 + GrayscaleValue(Vector3Byte(77, 88, 99)) div 2, 128), GrayAlpha.PixelPtr(1, 1)^);

  { draw from grayscale }
  ResetImages;
  Gray.DrawTo(GrayAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(100, 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(102, 128), GrayAlpha.PixelPtr(1, 1)^);

  ResetImages;
  Gray.DrawTo(GrayAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(AddBytesPremultiplied(201, 100), 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(AddBytesPremultiplied(203, 102), 128), GrayAlpha.PixelPtr(1, 1)^);

  { draw from grayscale + alpha }
  ResetImages;
  GrayAlpha2.DrawTo(GrayAlpha, 0, 1, dmBlend);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(BlendBytes(201, 200, 128), 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(BlendBytes(203, 202, 128), 128), GrayAlpha.PixelPtr(1, 1)^);

  ResetImages;
  GrayAlpha2.DrawTo(GrayAlpha, 0, 1, dmAdd);
  AssertVectorEquals(Vector2Byte(200, 128), GrayAlpha.PixelPtr(0, 0)^);
  AssertVectorEquals(Vector2Byte(AddBytesPremultiplied(201, 200), 128), GrayAlpha.PixelPtr(0, 1)^);
  AssertVectorEquals(Vector2Byte(202, 128), GrayAlpha.PixelPtr(1, 0)^);
  AssertVectorEquals(Vector2Byte(AddBytesPremultiplied(203, 202), 128), GrayAlpha.PixelPtr(1, 1)^);
end;

initialization
  RegisterTest(TTestImagesDraw);
end.
