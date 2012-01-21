{
  Copyright 2004-2012 Michalis Kamburelis.

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
  fpcunit, testutils, testregistry;

type
  TTestImages = class(TTestCase)
  published
    procedure TestLoadAnyImage;
    procedure TestImageClassBestForSavingToFormat;
    procedure TestClear;
    procedure TestVector3ToRGBE;
    procedure TestRGBEToRGBTranslating;
  end;

implementation

uses SysUtils, VectorMath, Images;

procedure TTestImages.TestLoadAnyImage;
{ testuje czy mechanizm argumentow dla LoadAnyImage (AllowedImageClasses,
  ForbiddenConvs) dziala dobrze w kazdym przypadku. }
const ImagesPath = 'data' + PathDelim +  'images' + PathDelim;

  procedure DoTest(const fname: string;
    const AllowedImageClasses: array of TImageClass;
    const ForbiddenConvs: TImageLoadConversions; DestClass: TImageClass);
  var Img: TImage;
  begin
   Img := LoadImage(ImagesPath+ fname, AllowedImageClasses, ForbiddenConvs);
   try
    Assert(Img is DestClass);
   finally FreeAndNil(Img) end;
  end;

  procedure DoFailTest(const fname: string;
    const AllowedImageClasses: array of TImageClass;
    const ForbiddenConvs: TImageLoadConversions);
  var Img: TImage;
  begin
   try
    Img := LoadImage(ImagesPath+ fname, AllowedImageClasses, ForbiddenConvs);
   except on E: EUnableToLoadImage do Exit end;
   try
    raise Exception.Create('Fail test passed - Er, I mean, failed.');
   finally FreeAndNil(Img) end;
  end;

  procedure TestsImageInRGBFormat(const fname: string);
  begin
   { zaladuj obrazek w formacie rgb. Dopoki TRGBImage jest w AllowedImageClasses
     wszystko powinno zawsze isc OK i wynik powinien miec typ TRGBImage. }
   DoTest('rgb.ppm', [TRGBImage], [], TRGBImage);
   DoTest('rgb.ppm', [TRGBImage], [ilcAlphaAdd], TRGBImage);
   DoTest('rgb.ppm', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], [], TRGBImage);
   DoTest('rgb.ppm', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], [ilcAlphaAdd], TRGBImage);
   DoTest('rgb.ppm', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], AllImageLoadConversions, TRGBImage);

   DoFailTest('rgb.ppm', [TRGBAlphaImage, TRGBFloatImage], AllImageLoadConversions);
   DoFailTest('rgb.ppm', [TRGBAlphaImage, TRGBFloatImage], [ilcAlphaAdd, ilcFloatPrecAdd]);
   DoTest('rgb.ppm', [TRGBAlphaImage, TRGBFloatImage], [ilcAlphaAdd], TRGBFloatImage);
   DoTest('rgb.ppm', [TRGBAlphaImage, TRGBFloatImage], [ilcFloatPrecAdd], TRGBAlphaImage);
   DoTest('rgb.ppm', [TRGBAlphaImage, TRGBFloatImage], [], TRGBAlphaImage);
  end;

begin
 TestsImageInRGBFormat('rgb.ppm');
 { png jest obslugiwane inaczej niz typowe formaty rgb, wiec lepiej sprawdzic
   je osobno. }
 TestsImageInRGBFormat('no_alpha.png');

 { zaladuj obrazek z alpha }
 DoTest('alpha.png', [TRGBImage], [], TRGBImage);
 DoTest('alpha.png', [TRGBImage], [ilcAlphaAdd], TRGBImage);
 DoFailTest('alpha.png', [TRGBImage], [ilcAlphaDelete]);
 DoTest('alpha.png', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], [], TRGBAlphaImage);
 DoTest('alpha.png', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], [ilcAlphaAdd], TRGBAlphaImage);
 DoTest('alpha.png', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], AllImageLoadConversions, TRGBAlphaImage);
 DoTest('alpha.png', [TRGBFloatImage], [], TRGBFloatImage);
 DoTest('alpha.png', [TRGBFloatImage], [ilcAlphaAdd], TRGBFloatImage);
 DoFailTest('alpha.png', [TRGBFloatImage], [ilcFloatPrecAdd]);
 DoFailTest('alpha.png', [TRGBFloatImage], [ilcAlphaDelete]);
 DoFailTest('alpha.png', [TRGBFloatImage], AllImageLoadConversions);

 { zaladuj obrazek z rgbe }
 DoTest('rgbe.rgbe', [TRGBImage], [], TRGBImage);
 DoTest('rgbe.rgbe', [TRGBImage, TRGBAlphaImage], [], TRGBImage);
 DoTest('rgbe.rgbe', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], [], TRGBFloatImage);
 DoTest('rgbe.rgbe', [TRGBImage, TRGBAlphaImage, TRGBFloatImage], AllImageLoadConversions, TRGBFloatImage);
 DoFailTest('rgbe.rgbe', [TRGBImage, TRGBAlphaImage], [ilcFloatPrecDelete]);
 DoTest('rgbe.rgbe', [TRGBAlphaImage], [], TRGBAlphaImage);
 DoFailTest('rgbe.rgbe', [TRGBAlphaImage], [ilcFloatPrecDelete]);
 DoFailTest('rgbe.rgbe', [TRGBAlphaImage], [ilcAlphaAdd]);

 { zaladuj obrazek z grayscale }
 DoTest('alpha_grayscale.png', [], [], TGrayscaleAlphaImage);
 DoTest('alpha_grayscale.png', [TGrayscaleImage], [], TGrayscaleImage);
 DoTest('alpha_grayscale.png', [TRGBImage], [], TRGBImage);
 DoTest('alpha_grayscale.png', [TRGBAlphaImage], [], TRGBAlphaImage);
 DoTest('alpha_grayscale.png', [], AllImageLoadConversions, TGrayscaleAlphaImage);
 DoFailTest('alpha_grayscale.png', [TGrayscaleImage], AllImageLoadConversions);
 DoFailTest('alpha_grayscale.png', [TRGBImage], [ilcAlphaDelete])
end;

procedure TTestImages.TestImageClassBestForSavingToFormat;
begin
 Assert(ImageClassBestForSavingToFormat('ala.rgbe') = TRGBFloatImage);
 Assert(ImageClassBestForSavingToFormat('blah.rgbe') = TRGBFloatImage);
 Assert(ImageClassBestForSavingToFormat('ala.foo') = TRGBImage);
 Assert(ImageClassBestForSavingToFormat('blah.png') = TRGBImage);
 Assert(ImageClassBestForSavingToFormat('ala.bmp') = TRGBImage);

 Assert(ImageClassBestForSavingToFormat(ifRGBE) = TRGBFloatImage);
 Assert(ImageClassBestForSavingToFormat(ifPNG) = TRGBImage);
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
  Assert(ImgR.IsClear(Vector4Byte(22, 33, 44, 66)));

  ImgR.SetColorRGB(0, 0, Vector3Single(0.5, 0.6, 0.7));
  Assert(not ImgR.IsClear(Vector4Byte(22, 33, 44, 66)));
 finally FreeAndNil(ImgR) end;

 ImgA := TRGBAlphaImage.Create(100, 100);
 try
  ImgA.Clear(Vector4Byte(22, 33, 44, 55));
  { 55 and 66 (4th component) should NOT be ignored for TRGBAlphaImage }
  Assert(not ImgA.IsClear(Vector4Byte(22, 33, 44, 66)));
  Assert(ImgA.IsClear(Vector4Byte(22, 33, 44, 55)));

  ImgA.SetColorRGB(0, 0, Vector3Single(0.5, 0.6, 0.7));
  Assert(not ImgA.IsClear(Vector4Byte(22, 33, 44, 55)));
 finally FreeAndNil(ImgA) end;
end;

procedure TTestImages.TestVector3ToRGBE;
const RightRGBE: TVector4Byte=(154, 10, 51, 127);
var NewRGBE: TVector4Byte;
begin
 NewRGBE := Vector3ToRGBE(Vector3Single(0.3, 0.02, 0.1));
 Assert(CompareMem(@NewRGBE, @RightRGBE, SizeOf(TVector4Byte)));
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

initialization
 RegisterTest(TTestImages);
end.
