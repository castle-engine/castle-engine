{ -*- compile-command: "./compile_console.sh" -*- }
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

unit TestImages;

interface

uses
  fpcunit, testutils, testregistry, CastleBaseTestCase;

type
  TTestImages = class(TCastleBaseTestCase)
  published
    procedure TestLoadImage;
    procedure TestImageClassBestForSavingToFormat;
    procedure TestClear;
    procedure TestVector3ToRGBE;
    procedure TestRGBEToRGBTranslating;
    procedure TestResize;
    //procedure TestMimeTypesAndExtsCount;
    procedure TestDraw;
  end;

implementation

uses SysUtils, CastleVectors, CastleImages;

procedure TTestImages.TestLoadImage;
const ImagesPath = 'data/images/';

  procedure DoTest(const fname: string;
    const AllowedImageClasses: array of TEncodedImageClass;
    DestClass: TCastleImageClass);
  var Img: TCastleImage;
  begin
   Img := LoadImage(ImagesPath+ fname, AllowedImageClasses);
   try
    AssertTrue(Img is DestClass);
   finally FreeAndNil(Img) end;
  end;

{ Unused:
  procedure DoFailTest(const fname: string;
    const AllowedImageClasses: array of TEncodedImageClass);
  var Img: TCastleImage;
  begin
   try
    Img := LoadImage(ImagesPath+ fname, AllowedImageClasses);
   except on E: EUnableToLoadImage do Exit end;
   try
    raise Exception.Create('Fail test passed - Er, I mean, failed.');
   finally FreeAndNil(Img) end;
  end;
}

  procedure TestsImageInRGBFormat(const fname: string);
  begin
   { zaladuj obrazek w formacie rgb. Dopoki TRGBImage jest w AllowedImageClasses
     wszystko powinno zawsze isc OK i wynik powinien miec typ TRGBImage. }
   DoTest('rgb.ppm', [TRGBImage], TRGBImage);
   DoTest('rgb.ppm', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], TRGBImage);
  end;

begin
 TestsImageInRGBFormat('rgb.ppm');
 { png jest obslugiwane inaczej niz typowe formaty rgb, wiec lepiej sprawdzic
   je osobno. }
 TestsImageInRGBFormat('no_alpha.png');

 { zaladuj obrazek z alpha }
 DoTest('alpha.png', [TRGBImage], TRGBImage);
 DoTest('alpha.png', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], TRGBAlphaImage);
 DoTest('alpha.png', [TRGBFloatImage], TRGBFloatImage);

 { zaladuj obrazek z rgbe }
 DoTest('rgbe.rgbe', [TRGBImage], TRGBImage);
 DoTest('rgbe.rgbe', [TRGBImage, TRGBAlphaImage], TRGBImage);
 DoTest('rgbe.rgbe', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], TRGBFloatImage);
 DoTest('rgbe.rgbe', [TRGBAlphaImage], TRGBAlphaImage);

 { zaladuj obrazek z grayscale }
 DoTest('alpha_grayscale.png', [], TGrayscaleAlphaImage);
 DoTest('alpha_grayscale.png', [TGrayscaleImage], TGrayscaleImage);
 DoTest('alpha_grayscale.png', [TRGBImage], TRGBImage);
 DoTest('alpha_grayscale.png', [TRGBAlphaImage], TRGBAlphaImage);
end;

procedure TTestImages.TestImageClassBestForSavingToFormat;
begin
 AssertTrue(ImageClassBestForSavingToFormat('ala.rgbe') = TRGBFloatImage);
 AssertTrue(ImageClassBestForSavingToFormat('blah.rgbe') = TRGBFloatImage);
 AssertTrue(ImageClassBestForSavingToFormat('ala.foo') = TRGBImage);
 AssertTrue(ImageClassBestForSavingToFormat('blah.png') = TRGBImage);
 AssertTrue(ImageClassBestForSavingToFormat('ala.bmp') = TRGBImage);

 // AssertTrue(ImageClassBestForSavingToFormat(ifRGBE) = TRGBFloatImage);
 // AssertTrue(ImageClassBestForSavingToFormat(ifPNG) = TRGBImage);
end;

procedure TTestImages.TestClear;
{ Test Clear and IsClear methods of TRGBImage and TRGBAlphaImage }
var ImgR: TRGBImage;
    ImgA: TRGBAlphaImage;
begin
 ImgR := TRGBImage.Create(100, 100);
 try
  { 55 and 66 (4th component) should be ignored for TRGBImage }
  ImgR.Clear(Vector4Byte(22, 33, 44, 55));
  AssertTrue(ImgR.IsClear(Vector4Byte(22, 33, 44, 66)));

  ImgR.SetColorRGB(0, 0, Vector3Single(0.5, 0.6, 0.7));
  AssertTrue(not ImgR.IsClear(Vector4Byte(22, 33, 44, 66)));
 finally FreeAndNil(ImgR) end;

 ImgA := TRGBAlphaImage.Create(100, 100);
 try
  ImgA.Clear(Vector4Byte(22, 33, 44, 55));
  { 55 and 66 (4th component) should NOT be ignored for TRGBAlphaImage }
  AssertTrue(not ImgA.IsClear(Vector4Byte(22, 33, 44, 66)));
  AssertTrue(ImgA.IsClear(Vector4Byte(22, 33, 44, 55)));

  ImgA.SetColorRGB(0, 0, Vector3Single(0.5, 0.6, 0.7));
  AssertTrue(not ImgA.IsClear(Vector4Byte(22, 33, 44, 55)));
 finally FreeAndNil(ImgA) end;
end;

procedure TTestImages.TestVector3ToRGBE;
const RightRGBE: TVector4Byte=(154, 10, 51, 127);
var NewRGBE: TVector4Byte;
begin
 NewRGBE := Vector3ToRGBE(Vector3Single(0.3, 0.02, 0.1));
 AssertTrue(CompareMem(@NewRGBE, @RightRGBE, SizeOf(TVector4Byte)));
end;

procedure TTestImages.TestRGBEToRGBTranslating;

  procedure CheckRGBEToRGBTranslating(const UpperValue: Single);
  var rgbe: TVector4Byte;
      rgb, newrgb: TVector3Single;
      i: Integer;
  begin
   for i := 1 to 1000 do
   begin
    rgb[0] := Random*UpperValue;
    rgb[1] := Random*UpperValue;
    rgb[2] := Random*UpperValue;

    rgbe := Vector3ToRGBE(rgb);
    newrgb := VectorRGBETo3Single(rgbe);
    if not VectorsEqual(rgb, newrgb, UpperValue/256) then
     raise Exception.Create('Error -'+
       ' rgb '+VectorToNiceStr(rgb)+
       ' rgbe '+VectorToNiceStr(rgbe)+
       ' newrgb '+VectorToNiceStr(newrgb) );
   end;
  end;

begin
 CheckRGBEToRGBTranslating(1.0);
 CheckRGBEToRGBTranslating(10.0);
 CheckRGBEToRGBTranslating(10000.0);
end;

procedure TTestImages.TestResize;
var
  Orig, OrigResized, OrigResized2: TCastleImage;

  procedure SimpleTest(const Interpolation: TResizeInterpolation);
  begin
    { check that both MakeResized and Resize with unchanged size make
      no modifications }

    OrigResized := Orig.MakeResized(Orig.Width, Orig.Height, Interpolation);
    AssertTrue(Orig.IsEqual(OrigResized));

    OrigResized2 := Orig.MakeCopy;
    OrigResized2.Resize(Orig.Width, Orig.Height, Interpolation);
    AssertTrue(Orig.IsEqual(OrigResized2));

    FreeAndNil(OrigResized);
    FreeAndNil(OrigResized2);

    { check that MakeResized and Resize have the same algorithm }

    OrigResized := Orig.MakeResized(1000, 100, Interpolation);
    AssertTrue(not Orig.IsEqual(OrigResized));

    OrigResized2 := Orig.MakeCopy;
    OrigResized2.Resize(1000, 100, Interpolation);
    AssertTrue(not Orig.IsEqual(OrigResized2));

    AssertTrue(OrigResized.IsEqual(OrigResized2));

    FreeAndNil(OrigResized);
    FreeAndNil(OrigResized2);
  end;

begin
  Orig := LoadImage('data/images/no_alpha.png', []);

  SimpleTest(riNearest);
  SimpleTest(riBilinear);

  { check that (on non-trivial data) the two interpolations actually give
    different results }

  OrigResized  := Orig.MakeResized(1000, 100, riNearest);
  OrigResized2 := Orig.MakeResized(1000, 100, riBilinear);
  AssertTrue(not OrigResized.IsEqual(OrigResized2));

  FreeAndNil(OrigResized);
  FreeAndNil(OrigResized2);
  FreeAndNil(Orig);
end;

(*
procedure TTestImages.TestMimeTypesAndExtsCount;
var
  I: TImageFormat;
  M: TImageFormatInfoMimeTypesCount;
  E: TImageFormatInfoExtsCount;
begin
  for I := Low(I) to High(I) do
  begin
    for M := Low(M) to ImageFormatInfos[I].MimeTypesCount do
    begin
      AssertTrue('' <> ImageFormatInfos[I].MimeTypes[M]);
      { This doesn't have to be true in the long run?
        I'm not sure whether mime types have to be lowercase,
        right now our engine treats them as case sensitive. }
      AssertEquals(LowerCase(ImageFormatInfos[I].MimeTypes[M]), ImageFormatInfos[I].MimeTypes[M]);
    end;
    if ImageFormatInfos[I].MimeTypesCount < High(M) then
      for M := ImageFormatInfos[I].MimeTypesCount + 1 to High(M) do
        AssertEquals('', ImageFormatInfos[I].MimeTypes[M]);

    for E := Low(E) to ImageFormatInfos[I].ExtsCount do
    begin
      AssertTrue('' <> ImageFormatInfos[I].Exts[E]);
      AssertEquals(LowerCase(ImageFormatInfos[I].Exts[E]), ImageFormatInfos[I].Exts[E]);
    end;
    if ImageFormatInfos[I].ExtsCount < High(E) then
      for E := ImageFormatInfos[I].ExtsCount + 1 to High(E) do
        AssertEquals('', ImageFormatInfos[I].Exts[E]);
  end;
end;
*)

procedure TTestImages.TestDraw;
var
  Rgb: TRGBImage;
  RgbAlpha: TRGBAlphaImage;
  Gray: TGrayscaleImage;
  GrayAlpha: TGrayscaleAlphaImage;

  function BlendBytes(const Dest, Source, Opacity: Byte): Byte; inline;
  var
    W: Word;
  begin
    W :=
      Word(Dest  ) * (255 - Opacity) div 255 +
      Word(Source) * Opacity         div 255;
    if W > 255 then W := 255;
    Result := W;
  end;

  function AddBytes(const Dest, Source, Opacity: Byte): Byte; inline;
  var
    W: Word;
  begin
    W := Dest + Word(Source) * Opacity div 255;
    if W > 255 then W := 255;
    Result := W;
  end;

  procedure FreeImages;
  begin
    FreeAndNil(Rgb);
    FreeAndNil(RgbAlpha);
    FreeAndNil(Gray);
    FreeAndNil(GrayAlpha);
  end;

  procedure ResetImages;
  begin
    FreeImages;

    Rgb := TRGBImage.Create(2, 2);
    Rgb.PixelPtr(0, 0)^ := Vector3Byte(1, 2, 3);
    Rgb.PixelPtr(0, 1)^ := Vector3Byte(4, 5, 6);
    Rgb.PixelPtr(1, 0)^ := Vector3Byte(11, 22, 33);
    Rgb.PixelPtr(1, 1)^ := Vector3Byte(44, 55, 66);

    RgbAlpha := TRGBAlphaImage.Create(2, 2);
    RgbAlpha.PixelPtr(0, 0)^ := Vector4Byte(7, 8, 9, 128);
    RgbAlpha.PixelPtr(0, 1)^ := Vector4Byte(3, 2, 1, 128);
    RgbAlpha.PixelPtr(1, 0)^ := Vector4Byte(77, 88, 99, 128);
    RgbAlpha.PixelPtr(1, 1)^ := Vector4Byte(33, 22, 11, 128);

    Gray := TGrayscaleImage.Create(2, 2);
    Gray.PixelPtr(0, 0)^ := 100;
    Gray.PixelPtr(0, 1)^ := 101;
    Gray.PixelPtr(1, 0)^ := 102;
    Gray.PixelPtr(1, 1)^ := 103;

    GrayAlpha := TGrayscaleAlphaImage.Create(2, 2);
    GrayAlpha.PixelPtr(0, 0)^ := Vector2Byte(200, 128);
    GrayAlpha.PixelPtr(0, 1)^ := Vector2Byte(201, 128);
    GrayAlpha.PixelPtr(1, 0)^ := Vector2Byte(202, 128);
    GrayAlpha.PixelPtr(1, 1)^ := Vector2Byte(203, 128);
  end;

begin
  Rgb := nil;
  RgbAlpha := nil;
  Gray := nil;
  GrayAlpha := nil;
  try
    { draw rgba over rgb }
    ResetImages;
    RgbAlpha.DrawTo(Rgb, 0, 1, dmBlend);
    AssertVectorsEqual(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
    AssertVectorsEqual(Vector3Byte(BlendBytes(4, 7, 128), BlendBytes(5, 8, 128), BlendBytes(6, 9, 128)), Rgb.PixelPtr(0, 1)^);
    AssertVectorsEqual(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
    AssertVectorsEqual(Vector3Byte(BlendBytes(44, 77, 128), BlendBytes(55, 88, 128), BlendBytes(66, 99, 128)), Rgb.PixelPtr(1, 1)^);

    ResetImages;
    RgbAlpha.DrawTo(Rgb, 0, 1, dmAdd);
    AssertVectorsEqual(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
    AssertVectorsEqual(Vector3Byte(4 + 7 div 2, 5 + 8 div 2, 6 + 9 div 2), Rgb.PixelPtr(0, 1)^);
    AssertVectorsEqual(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
    AssertVectorsEqual(Vector3Byte(44 + 77 div 2, 55 + 88 div 2, 66 + 99 div 2), Rgb.PixelPtr(1, 1)^);

    { draw grayscale over rgb }
    ResetImages;
    Gray.DrawTo(Rgb, 0, 1, dmBlend);
    AssertVectorsEqual(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
    AssertVectorsEqual(Vector3Byte(100, 100, 100), Rgb.PixelPtr(0, 1)^);
    AssertVectorsEqual(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
    AssertVectorsEqual(Vector3Byte(102, 102, 102), Rgb.PixelPtr(1, 1)^);

    ResetImages;
    Gray.DrawTo(Rgb, 0, 1, dmAdd);
    AssertVectorsEqual(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
    AssertVectorsEqual(Vector3Byte(4 + 100, 5 + 100, 6 + 100), Rgb.PixelPtr(0, 1)^);
    AssertVectorsEqual(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
    AssertVectorsEqual(Vector3Byte(44 + 102, 55 + 102, 66 + 102), Rgb.PixelPtr(1, 1)^);

    { draw grayscale + alpha over rgb }
    ResetImages;
    GrayAlpha.DrawTo(Rgb, 0, 1, dmBlend);
    AssertVectorsEqual(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
    AssertVectorsEqual(Vector3Byte(BlendBytes(4, 200, 128), BlendBytes(5, 200, 128), BlendBytes(6, 200, 128)), Rgb.PixelPtr(0, 1)^);
    AssertVectorsEqual(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
    AssertVectorsEqual(Vector3Byte(BlendBytes(44, 202, 128), BlendBytes(55, 202, 128), BlendBytes(66, 202, 128)), Rgb.PixelPtr(1, 1)^);

    ResetImages;
    GrayAlpha.DrawTo(Rgb, 0, 1, dmAdd);
    AssertVectorsEqual(Vector3Byte(1, 2, 3), Rgb.PixelPtr(0, 0)^);
    AssertVectorsEqual(Vector3Byte(4 + 200 div 2, 5 + 200 div 2, 6 + 200 div 2), Rgb.PixelPtr(0, 1)^);
    AssertVectorsEqual(Vector3Byte(11, 22, 33), Rgb.PixelPtr(1, 0)^);
    AssertVectorsEqual(Vector3Byte(44 + 202 div 2, 55 + 202 div 2, 66 + 202 div 2), Rgb.PixelPtr(1, 1)^);

  finally FreeImages end;
end;

initialization
 RegisterTest(TTestImages);
end.
