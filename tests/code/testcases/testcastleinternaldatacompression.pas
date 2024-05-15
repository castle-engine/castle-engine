{
  Copyright 2023-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalDataCompression unit. }
unit TestCastleInternalDataCompression;

interface

uses Classes, SysUtils,
  CastleTester, CastleUtils, CastleClassUtils;

type
  TTestCastleInternalDataCompression = class(TCastleTestCase)
  published
    procedure TestChannelsSplitCombine;
    procedure TestChannelsSplitCombineMultiChannels;
    procedure TestRleCompression;
    procedure TestPascalEncodedMatchesOriginal;
  end;

implementation

uses CastleInternalDataCompression, CastleLog, CastleStringUtils, CastleImages,
  V3DSceneImages;

{$I test_font_image_to_compress.inc}

{ TTestCastleInternalDataCompression ----------------------------------------- }

procedure TTestCastleInternalDataCompression.TestChannelsSplitCombine;
var
  Combined: TMemoryStream;
  Split: TChannelsSplit;
begin
  Split := DataChannelsSplit(@FontImagePixels, SizeOf(FontImagePixels), 1);
  try
    Combined := TMemoryStream.Create;
    try
      Combined.Size := SizeOf(FontImagePixels);
      DataChannelsCombine(Combined.Memory, Combined.Size, 1, Split);

      AssertEquals(SizeOf(FontImagePixels), Combined.Size);
      AssertTrue(CompareMemDebug(Combined.Memory, @FontImagePixels, Combined.Size));
      AssertTrue(CompareMem     (Combined.Memory, @FontImagePixels, Combined.Size));
    finally FreeAndNil(Combined) end;
  finally
    Split.DataFree;
    FreeAndNil(Split);
  end;
end;

procedure TTestCastleInternalDataCompression.TestChannelsSplitCombineMultiChannels;

  procedure Test(Image: TCastleImage);
  var
    Split: TChannelsSplit;
    NewImage: TCastleImage;
  begin
    //Writeln('Testing ', Image.Url, ' of class ', Image.ClassName);
    Split := DataChannelsSplit(Image.RawPixels, Image.Size, Image.PixelSize);
    try
      NewImage := TCastleImageClass(Image.ClassType).Create(Image.Width, Image.Height);
      try
        DataChannelsCombine(NewImage.RawPixels, NewImage.Size, NewImage.PixelSize, Split);
        AssertImagesEqual(Image, NewImage);
      finally FreeAndNil(NewImage) end;
    finally
      Split.DataFree;
      FreeAndNil(Split);
    end;

    FreeAndNil(Image);
  end;

begin
  Test(LoadImage('castle-data:/images/f023ours.jpg'));
  Test(LoadImage('castle-data:/images/alpha.png'));
  Test(LoadImage('castle-data:/images/alpha_grayscale.png'));
  Test(LoadImage('castle-data:/images/rgbe.rgbe'));
  Test(LoadImage('castle-data:/images/open.png'));

  Test(LoadImage('castle-data:/images/f023ours.jpg', [TGrayscaleImage]) as TGrayscaleImage);
  Test(LoadImage('castle-data:/images/f023ours.jpg', [TGrayscaleAlphaImage]) as TGrayscaleAlphaImage);
  Test(LoadImage('castle-data:/images/f023ours.jpg', [TRGBImage]) as TRGBImage);
  Test(LoadImage('castle-data:/images/f023ours.jpg', [TRGBAlphaImage]) as TRGBAlphaImage);
  Test(LoadImage('castle-data:/images/f023ours.jpg', [TRGBFloatImage]) as TRGBFloatImage);
end;

procedure TTestCastleInternalDataCompression.TestPascalEncodedMatchesOriginal;

  procedure Test(const ImageOriginal, ImagePascalEncoded: TCastleImage);
  begin
    AssertImagesEqual(ImageOriginal, ImagePascalEncoded);
    //SaveImage(ImageOriginal, 'castle-data:/images/open_original_copy.png');
    ImageOriginal.Free;
  end;

begin
  Test(LoadImage('castle-data:/images/open.png'), Open);
  //SaveImage(Open, 'castle-data:/images/open_pascal_encoded.png');
end;

procedure TTestCastleInternalDataCompression.TestRleCompression;
var
  Initial, Compressed, Decompressed: TMemoryStream;
begin
  Initial := TMemoryStream.Create;
  try
    Initial.Size := SizeOf(FontImagePixels);
    Move(FontImagePixels[0], Initial.Memory^, Initial.Size);

    Compressed := TMemoryStream.Create;
    try
      RleCompress(Initial.Memory, Initial.Size, Compressed);

      WritelnLog('Compressed %d (%s) to %d (%s), ratio %f', [
        Initial.Size, SizeToStr(Initial.Size),
        Compressed.Size, SizeToStr(Compressed.Size),
        Compressed.Size / Initial.Size
      ]);

      Compressed.Position := 0;

      Decompressed := TMemoryStream.Create;
      try
        RleDecompress(Compressed.Memory, Compressed.Size, Decompressed);
        AssertEquals(Initial.Size, Decompressed.Size);
        AssertTrue(CompareMemDebug(Decompressed.Memory, Initial.Memory, Initial.Size));
        AssertTrue(CompareMem     (Decompressed.Memory, Initial.Memory, Initial.Size));
        {$ifdef FPC}
        AssertEquals(0, CompareByte(Decompressed.Memory^, Initial.Memory^, Initial.Size));
        {$endif}
      finally FreeAndNil(Decompressed) end;
    finally FreeAndNil(Compressed) end;
  finally FreeAndNil(Initial) end;
end;

initialization
  RegisterTest(TTestCastleInternalDataCompression);
end.
