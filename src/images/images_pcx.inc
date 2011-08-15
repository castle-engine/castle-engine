{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ PCX loader using FPImage.

  FPImage docs:
  http://lazarus-ccr.sourceforge.net/docs/fcl/fpimage/index.html
  http://wiki.freepascal.org/fcl-image
  Also http://free-pascal-general.1045716.n5.nabble.com/Why-is-FPImage-dog-slow-in-reading-jpeg-s-td4439450.html
  is informative, explains UsePalette stuff.
}

function LoadPCX(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
var
  FPImage: TFPMemoryImage;
  X, Y: Integer;
  ResultPixels: PVector3Byte;
  Reader: TFPCustomImageReader;
  Color: TFPColor;
begin
  FPImage := TFPMemoryImage.Create(0, 0);
  try
    FPImage.UsePalette := false;

    Reader := TFPReaderPCX.Create;
    try
      FPImage.LoadFromStream(Stream, Reader);
    finally FreeAndNil(Reader) end;

    Result := TRGBImage.Create(FPImage.Width, FPImage.Height);
    try
      ResultPixels := TRGBImage(Result).RGBPixels;
      for Y := FPImage.Height - 1 downto 0 do
        for X := 0 to FPImage.Width - 1 do
        begin
          Color := FPImage.Colors[X, Y];
          ResultPixels^[0] := Color.Red shr 8;
          ResultPixels^[1] := Color.Green shr 8;
          ResultPixels^[2] := Color.Blue shr 8;
          Inc(ResultPixels);
        end;
    except FreeAndNil(Result) end;
  finally FreeAndNil(FPImage) end;
end;
