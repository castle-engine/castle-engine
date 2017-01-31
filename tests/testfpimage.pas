{
  Copyright 2011-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestFPImage;

interface

uses
  fpcunit, testutils, testregistry;

type
  TTestFPImage = class(TTestCase)
  published
    procedure TestJPEG;
  end;

implementation

uses Classes, FPImage, FPReadJPEG, SysUtils, CastleVectors, CastleImages;

procedure TTestFPImage.TestJPEG;

  procedure DoIt(const S: string; const GoodWidth, GoodHeight: Cardinal);
  var
    Stream: TMemoryStream;
    Reader: TFPCustomImageReader;
    Image: TFPMemoryImage;
  begin
    Stream := nil;
    Reader := nil;
    Image := nil;
    try
  //    Stream := TFileStream.Create(S, fmOpenRead);
      Stream := TMemoryStream.Create;
      Stream.LoadFromFile(S);
      Stream.Position := 0;

      Reader := TFPReaderJPEG.Create;
      Image := TFPMemoryImage.Create(0, 0);

      Image.UsePalette := false;
      Image.LoadFromStream(Stream, Reader);
      { test size, to test image was actually read }
      AssertTrue(GoodWidth = Image.Width);
      AssertTrue(GoodHeight = Image.Height);
    finally
      Image.Free;
      Reader.Free;
      Stream.Free;
    end;
  end;

begin
  { Reading this image fails with FPImage under FPC <= 2.4.0 }
  DoIt('data/images/f023ours.jpg', 512, 512);
end;

initialization
  RegisterTest(TTestFPImage);
end.
