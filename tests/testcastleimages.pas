{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleImages;

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

  ImgR.Colors[0, 0, 0] := Vector4Single(0.5, 0.6, 0.7, 1.0);
  AssertTrue(not ImgR.IsClear(Vector4Byte(22, 33, 44, 66)));
 finally FreeAndNil(ImgR) end;

 ImgA := TRGBAlphaImage.Create(100, 100);
 try
  ImgA.Clear(Vector4Byte(22, 33, 44, 55));
  { 55 and 66 (4th component) should NOT be ignored for TRGBAlphaImage }
  AssertTrue(not ImgA.IsClear(Vector4Byte(22, 33, 44, 66)));
  AssertTrue(ImgA.IsClear(Vector4Byte(22, 33, 44, 55)));

  ImgA.Colors[0, 0, 0] := Vector4Single(0.5, 0.6, 0.7, 1.0);
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

initialization
 RegisterTest(TTestImages);
end.
