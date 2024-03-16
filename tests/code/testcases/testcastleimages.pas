// -*- compile-command: "./test_single_testcase.sh TTestImages" -*-
{
  Copyright 2004-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleImages. }
unit TestCastleImages;

{$ifndef FPC}
  {$pointermath on}
{$endif}

interface

uses CastleTester;

type
  TTestImages = class(TCastleTestCase)
  published
    procedure TestBasicImageLoad;
    procedure TestLoadImage;
    procedure TestImageClassBestForSavingToFormat;
    procedure TestClear;
    procedure TestVector3ToRGBE;
    procedure TestRGBEToRGBTranslating;
    procedure TestResize;
    //procedure TestMimeTypesAndExtsCount;
    procedure TestLoadSavePreserveAlpha;
    procedure TestInternalDetectClassPNG;
    procedure TestLoadAnchors;
    procedure TestPreserveTreatAsAlpha;
  end;

implementation

uses SysUtils, Classes,
  CastleVectors, CastleImages, CastleFilesUtils, CastleDownload, CastleUriUtils,
  CastleInternalPng, CastleLog;

procedure TTestImages.TestBasicImageLoad;
var
  Img: TCastleImage;
begin
  Img := LoadImage('castle-data:/test_texture.png');
  try
    AssertEquals(256, Img.Width);
    AssertEquals(256, Img.Height);
  finally FreeAndNil(Img) end;
end;

procedure TTestImages.TestLoadImage;

  procedure DoTest(const fname: string;
    const AllowedImageClasses: array of TEncodedImageClass;
    DestClass: TCastleImageClass);
  var
    Img: TCastleImage;
  begin
    Img := LoadImage('castle-data:/images/' + fname, AllowedImageClasses);
    try
      if not (Img is DestClass) then
        Fail(Format('We expect %s class but have %s', [
          DestClass.ClassName,
          Img.ClassName
        ]));
    finally FreeAndNil(Img) end;
  end;

{ Unused:
  procedure DoFailTest(const fname: string;
    const AllowedImageClasses: array of TEncodedImageClass);
  var Img: TCastleImage;
  begin
   try
    Img := LoadImage('castle-data:/' + fname, AllowedImageClasses);
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

  ImgR.Colors[0, 0, 0] := Vector4(0.5, 0.6, 0.7, 1.0);
  AssertTrue(not ImgR.IsClear(Vector4Byte(22, 33, 44, 66)));
 finally FreeAndNil(ImgR) end;

 ImgA := TRGBAlphaImage.Create(100, 100);
 try
  ImgA.Clear(Vector4Byte(22, 33, 44, 55));
  { 55 and 66 (4th component) should NOT be ignored for TRGBAlphaImage }
  AssertTrue(not ImgA.IsClear(Vector4Byte(22, 33, 44, 66)));
  AssertTrue(ImgA.IsClear(Vector4Byte(22, 33, 44, 55)));

  ImgA.Colors[0, 0, 0] := Vector4(0.5, 0.6, 0.7, 1.0);
  AssertTrue(not ImgA.IsClear(Vector4Byte(22, 33, 44, 55)));
 finally FreeAndNil(ImgA) end;
end;

procedure TTestImages.TestVector3ToRGBE;
const
  RightRGBE: TVector4Byte= (X: 154; Y: 10; Z: 51; W: 127);
var
  NewRGBE: TVector4Byte;
begin
 NewRGBE := Vector3ToRGBE(Vector3(0.3, 0.02, 0.1));
 AssertTrue(CompareMem(@NewRGBE, @RightRGBE, SizeOf(TVector4Byte)));
end;

procedure TTestImages.TestRGBEToRGBTranslating;

  procedure CheckRGBEToRGBTranslating(const UpperValue: Single);
  var
    rgbe: TVector4Byte;
    rgb, newrgb: TVector3;
    i: Integer;
  begin
   for i := 1 to 1000 do
   begin
    rgb.X := Random*UpperValue;
    rgb.Y := Random*UpperValue;
    rgb.Z := Random*UpperValue;

    rgbe := Vector3ToRGBE(rgb);
    newrgb := VectorRGBETo3Single(rgbe);
    if not TVector3.Equals(rgb, newrgb, UpperValue/256) then
     raise Exception.Create('Error -'+
       ' rgb '+rgb.ToString+
       ' rgbe '+rgbe.ToString+
       ' newrgb '+newrgb.ToString );
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
  Orig := LoadImage('castle-data:/images/no_alpha.png', []);

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

procedure TTestImages.TestLoadSavePreserveAlpha;

  procedure TestImage(const Url: String);
  var
    Img, Img2: TRGBAlphaImage;
    TempImageFileName: string;
  begin
    try
      TempImageFileName := GetTempFileNamePrefix + 'load_save_test.png';
      try
        Img := LoadImage(Url, [TRGBAlphaImage]) as TRGBAlphaImage;
        try
          SaveImage(Img, TempImageFileName);

          { load TempImageFileName, check does Img equal Img2 }
          Img2 := LoadImage(TempImageFileName, [TRGBAlphaImage]) as TRGBAlphaImage;
          try
            AssertImagesEqual(Img, Img2);
          finally FreeAndNil(Img2) end;
        finally FreeAndNil(Img) end;
      finally CheckDeleteFile(TempImageFileName, true) end;
    except
      { enhance EAssertionFailedError message with image URL }
      on E: EAssertionFailedError do
      begin
        E.Message := 'In image ' + Url + ': ' + E.Message;
        raise;
      end;
    end;
  end;

begin
  TestImage('castle-data:/images/load-save-alpha-test/1.png');
  TestImage('castle-data:/images/load-save-alpha-test/2.png');
  TestImage('castle-data:/images/load-save-alpha-test/3.png');
  TestImage('castle-data:/images/load-save-alpha-test/4.png');
  TestImage('castle-data:/images/load-save-alpha-test/5.png');
end;

procedure TTestImages.TestInternalDetectClassPNG;
var
  Stream: TStream;
  ImageClass: TEncodedImageClass;
begin
  Stream := Download('castle-data:/png_with_alpha_trns.png');
  try
    ImageClass := InternalDetectClassPNG(Stream);
    AssertTrue(ImageClass <> nil);
    AssertEquals('TRGBAlphaImage', ImageClass.ClassName);
  finally FreeAndNil(Stream) end;
end;

procedure TTestImages.TestLoadAnchors;
var
  Img: TEncodedImage;
begin
  AssertEquals('image/png', UriMimeType('castle-data:/sprite-sheets/cocos2d_wolf/wolf.png'));

  Img := LoadImage('castle-data:/sprite-sheets/cocos2d_wolf/wolf.png');
  try
    AssertEquals(256, Img.Width);
    AssertEquals(256, Img.Height);
  finally FreeAndNil(Img) end;

  Img := LoadEncodedImage('castle-data:/sprite-sheets/cocos2d_wolf/wolf.png');
  try
    AssertEquals(256, Img.Width);
    AssertEquals(256, Img.Height);
  finally FreeAndNil(Img) end;

  { since UriMimeType ignores anchors, so LoadImage should too }

  AssertEquals('image/png', UriMimeType('castle-data:/sprite-sheets/cocos2d_wolf/wolf.png#some-anchor'));

  Img := LoadImage('castle-data:/sprite-sheets/cocos2d_wolf/wolf.png#some-anchor');
  try
    AssertEquals(256, Img.Width);
    AssertEquals(256, Img.Height);
  finally FreeAndNil(Img) end;

  Img := LoadEncodedImage('castle-data:/sprite-sheets/cocos2d_wolf/wolf.png#some-anchor');
  try
    AssertEquals(256, Img.Width);
    AssertEquals(256, Img.Height);
  finally FreeAndNil(Img) end;
end;

procedure TTestImages.TestPreserveTreatAsAlpha;

  procedure AssertGrayscaleConstant(const Img: TGrayscaleImage; const Pixel: Byte);
  var
    X, Y, I: Integer;
  begin
    for X := 0 to Img.Width - 1 do
      for Y := 0 to Img.Height - 1 do
        AssertVectorEquals(Vector4(Pixel/255, Pixel/255, Pixel/255, 1.0), Img.Colors[X, Y, 0]);
    // alternative, faster check:
    for I := 0 to (Img.Width * Img.Height) - 1 do
      AssertEquals(Pixel, Img.Pixels[I]);
  end;

var
  Img1, Img2: TGrayscaleImage;
begin
  Img1 := TGrayscaleImage.Create(2, 2);
  try
    Img1.Clear(128);
    AssertFalse(Img1.TreatAsAlpha);

    AssertEquals(2, Img1.Width);
    AssertEquals(2, Img1.Height);
    AssertEquals(1, Img1.Depth);
    AssertGrayscaleConstant(Img1, 128);

    Img2 := TGrayscaleImage.Create;
    try
      Img2.Assign(Img1);
      AssertFalse(Img2.TreatAsAlpha);

      AssertEquals(2, Img2.Width);
      AssertEquals(2, Img2.Height);
      AssertEquals(1, Img2.Depth);
      AssertGrayscaleConstant(Img2, 128);
    finally FreeAndNil(Img2) end;
  finally FreeAndNil(Img1) end;

  Img1 := TGrayscaleImage.Create(2, 2);
  try
    Img1.Clear(128);
    AssertFalse(Img1.TreatAsAlpha);
    AssertEquals(2, Img1.Width);
    AssertEquals(2, Img1.Height);
    AssertEquals(1, Img1.Depth);
    Img1.TreatAsAlpha := true;
    Img1.ColorWhenTreatedAsAlpha := Vector3Byte(1, 2, 3);

    Img2 := TGrayscaleImage.Create;
    try
      Img2.Assign(Img1);
      AssertTrue(Img2.TreatAsAlpha);
      AssertEquals(1, Img2.ColorWhenTreatedAsAlpha.X);
      AssertEquals(2, Img2.ColorWhenTreatedAsAlpha.Y);
      AssertEquals(3, Img2.ColorWhenTreatedAsAlpha.Z);
      AssertEquals(1, Img2.GrayscaleColorWhenTreatedAsAlpha);

      AssertEquals(2, Img2.Width);
      AssertEquals(2, Img2.Height);
      AssertEquals(1, Img2.Depth);
      AssertGrayscaleConstant(Img2, 128);
    finally FreeAndNil(Img2) end;

    Img2 := Img1.MakeCopy as TGrayscaleImage;
    try
      AssertTrue(Img2.TreatAsAlpha);
      AssertEquals(1, Img2.ColorWhenTreatedAsAlpha.X);
      AssertEquals(2, Img2.ColorWhenTreatedAsAlpha.Y);
      AssertEquals(3, Img2.ColorWhenTreatedAsAlpha.Z);
      AssertEquals(1, Img2.GrayscaleColorWhenTreatedAsAlpha);

      AssertEquals(2, Img2.Width);
      AssertEquals(2, Img2.Height);
      AssertEquals(1, Img2.Depth);
      AssertGrayscaleConstant(Img2, 128);
    finally FreeAndNil(Img2) end;

    Img2 := Img1.MakeResized(3, 3) as TGrayscaleImage;
    try
      AssertTrue(Img2.TreatAsAlpha);
      AssertEquals(1, Img2.ColorWhenTreatedAsAlpha.X);
      AssertEquals(2, Img2.ColorWhenTreatedAsAlpha.Y);
      AssertEquals(3, Img2.ColorWhenTreatedAsAlpha.Z);
      AssertEquals(1, Img2.GrayscaleColorWhenTreatedAsAlpha);

      AssertEquals(3, Img2.Width);
      AssertEquals(3, Img2.Height);
      AssertEquals(1, Img2.Depth);
      AssertGrayscaleConstant(Img2, 128);
    finally FreeAndNil(Img2) end;

    Img2 := Img1.MakeExtracted(0, 0, 1, 1) as TGrayscaleImage;
    try
      AssertTrue(Img2.TreatAsAlpha);
      AssertEquals(1, Img2.ColorWhenTreatedAsAlpha.X);
      AssertEquals(2, Img2.ColorWhenTreatedAsAlpha.Y);
      AssertEquals(3, Img2.ColorWhenTreatedAsAlpha.Z);
      AssertEquals(1, Img2.GrayscaleColorWhenTreatedAsAlpha);

      AssertEquals(1, Img2.Width);
      AssertEquals(1, Img2.Height);
      AssertEquals(1, Img2.Depth);
      AssertGrayscaleConstant(Img2, 128);
    finally FreeAndNil(Img2) end;

  finally FreeAndNil(Img1) end;
end;

initialization
  RegisterTest(TTestImages);
end.
