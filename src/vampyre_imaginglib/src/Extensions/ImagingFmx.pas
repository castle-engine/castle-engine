{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ Functions and classes for interoperability between Imaging and
  FireMonkey framework.}
unit ImagingFmx;

{$I ImagingOptions.inc}

interface

uses
  Types,
  SysUtils,
  ImagingTypes,
  Imaging,
  ImagingFormats,
  ImagingClasses,
  ImagingUtility,
  UITypes,
  Fmx.Types,
  Fmx.Utils,
  Fmx.Graphics;

{ Converts image from TImageData record to FMX bitmap. Bitmap must be already instantiated.}
procedure ConvertImageDataToFmxBitmap(const Image: TImageData; Bitmap: TBitmap);
{ Converts FMX bitmap to TImageData. Image Data must already instantiated.}
procedure ConvertFmxBitmapToImageData(const Bitmap: TBitmap; Image: TImageData);

{ Converts image from TBaseImage instance to FMX bitmap. Bitmap must be already instantiated.}
procedure ConvertImageToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap);

{ Copies rectangular area of pixels from TImageData record to existing FMX bitmap.}
procedure CopyRectToFmxBitmap(const Image: TImageData; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt); overload;
{ Copies rectangular area of pixels from TBaseImage instance to existing FMX bitmap.}
procedure CopyRectToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt); overload;

implementation

procedure ConvertFmxBitmapToImageData(const Bitmap: TBitmap; Image: TImageData);
var
  Color32: TColor32Rec;
  MapData: TBitmapData;
  SourceData: PAlphaColorRec;
  TargetData: PByte;
  X, Y, Bpp, SrcWidthBytes: Integer;
  TargetInfo: TImageFormatInfo;
begin
    Bitmap.Map(TMapAccess.maRead, MapData);
    GetImageFormatInfo(Image.Format, TargetInfo);

    Bpp := TargetInfo.BytesPerPixel;
    SrcWidthBytes := Image.Width * Bpp;
    TargetData := @PByteArray(Image.Bits)[0];

    for Y := 0 to Pred(Bitmap.Height) do
     for X:= 0 to Pred(Bitmap.Width) do
      begin
        SourceData:= @PAlphaColorRecArray(MapData.Data)[Y * (MapData.Pitch div 4) + X];
        case TargetInfo.Format of
          ifIndex8:
            begin
              Image.Palette[TargetData^].R := SourceData^.R;
              Image.Palette[TargetData^].G := SourceData^.G;
              Image.Palette[TargetData^].B := SourceData^.B;
              Image.Palette[TargetData^].A := SourceData^.A;
            end;
          ifGray8:
              TargetData^ := SourceData.R;
          ifA8Gray8:
            begin
              TargetData^ := SourceData.R;
              PWordRec(TargetData).High := SourceData.A;
            end;
          ifGray16:
              PWord(TargetData)^ := SourceData.R;
          ifR8G8B8:
            begin
              PColor24Rec(TargetData)^.R := SourceData.R;
              PColor24Rec(TargetData)^.G := SourceData.G;
              PColor24Rec(TargetData)^.B := SourceData.B;
            end;
          ifA8R8G8B8:
            begin
              PColor32Rec(TargetData)^.A := SourceData^.B;
              PColor32Rec(TargetData)^.G := SourceData^.R;
              PColor32Rec(TargetData)^.R := SourceData^.G;
              PColor32Rec(TargetData)^.B := SourceData^.A;
            end;
          ifR16G16B16:
            begin
              PColor48Rec(TargetData).R := Round(SourceData.R * $FFFF / 255);
              PColor48Rec(TargetData).G := Round(SourceData.G * $FFFF / 255);
              PColor48Rec(TargetData).B := Round(SourceData.B * $FFFF / 255);
            end;
          ifA16R16G16B16:
            begin
              PColor64Rec(TargetData).R  := Round(SourceData.R * $FFFF / 255);
              PColor64Rec(TargetData).G  := Round(SourceData.G * $FFFF / 255);
              PColor64Rec(TargetData).B  := Round(SourceData.B * $FFFF / 255);
              PColor64Rec(TargetData).A  := Round(SourceData.A * $FFFF / 255);
            end;
        else
          Color32.R := SourceData^.R;
          Color32.G := SourceData^.G;
          Color32.B := SourceData^.B;
          Color32.A := SourceData^.A;
          TargetInfo.SetPixel32(TargetData,@TargetInfo, Image.Palette,Color32);
        end;
        Inc(TargetData, Bpp);
      end;
     Bitmap.Unmap(MapData);
end;

procedure ConvertImageDataToFmxBitmap(const Image: TImageData; Bitmap: TBitmap);
begin
  Assert(TestImage(Image));
  Bitmap.SetSize(Image.Width, Image.Height);
  CopyRectToFmxBitmap(Image, Bitmap, 0, 0, Image.Width, Image.Height, 0, 0);
end;

procedure ConvertImageToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap);
begin
  ConvertImageDataToFmxBitmap(Image.ImageDataPointer^, Bitmap);
end;

procedure ConvertToAlphaColorRec(SrcPix: PByte; DestPix: PAlphaColorRec;
  const SrcInfo: TImageFormatInfo; SrcPalette: PPalette32);
var
   Color32:TColor32Rec;
begin
  case SrcInfo.Format of
    ifIndex8:
      begin
        DestPix^.R := SrcPalette[SrcPix^].R;
        DestPix^.G := SrcPalette[SrcPix^].G;
        DestPix^.B := SrcPalette[SrcPix^].B;
        DestPix^.A := SrcPalette[SrcPix^].A;
      end;
    ifGray8:
      begin
        DestPix.R := SrcPix^;
        DestPix.G := SrcPix^;
        DestPix.B := SrcPix^;
        DestPix.A := 255;
      end;
    ifA8Gray8:
      begin
        DestPix.R := SrcPix^;
        DestPix.G := SrcPix^;
        DestPix.B := SrcPix^;
        DestPix.A := PWordRec(SrcPix).High;
      end;
    ifGray16:
      begin
        DestPix.R := PWord(SrcPix)^ shr 8;
        DestPix.G := DestPix.R;
        DestPix.B := DestPix.R;
        DestPix.A := 255;
      end;
    ifR8G8B8:
      begin
        DestPix.R := PColor24Rec(SrcPix)^.R;
        DestPix.G := PColor24Rec(SrcPix)^.G;
        DestPix.B := PColor24Rec(SrcPix)^.B;
        DestPix.A := 255;
      end;
    ifA8R8G8B8:
      begin
        DestPix^.R := PColor32Rec(SrcPix)^.R;
        DestPix^.G := PColor32Rec(SrcPix)^.G;
        DestPix^.B := PColor32Rec(SrcPix)^.B;
        DestPix^.A := PColor32Rec(SrcPix)^.A;
      end;
    ifR16G16B16:
      begin
        DestPix.R := PColor48Rec(SrcPix).R shr 8;
        DestPix.G := PColor48Rec(SrcPix).G shr 8;
        DestPix.B := PColor48Rec(SrcPix).B shr 8;
        DestPix.A := 255;
      end;
    ifA16R16G16B16:
      begin
        DestPix.R := PColor64Rec(SrcPix).R shr 8;
        DestPix.G := PColor64Rec(SrcPix).G shr 8;
        DestPix.B := PColor64Rec(SrcPix).B shr 8;
        DestPix.A := PColor64Rec(SrcPix).A shr 8;
      end;
  else
    Color32:=SrcInfo.GetPixel32(SrcPix, @SrcInfo, SrcPalette);
    DestPix^.R := Color32.R;
    DestPix^.G := Color32.G;
    DestPix^.B := Color32.B;
    DestPix^.A := Color32.A;
  end;
end;

procedure CopyRectToFmxBitmap(const Image: TImageData; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt);
var
  TempImage: TImageData;
  X, Y, Bpp, SrcWidthBytes, MoveBytes: Integer;
  SrcPtr: PByte;
  Info: TImageFormatInfo;
  MapData: TBitmapData;
  DstPtr: PAlphaColorRec;
  ARGB: TAlphaColorRec;
begin
  Assert(TestImage(Image) and not Bitmap.IsEmpty);

  ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, Image.Width, Image.Height,
    Rect(0, 0, Bitmap.Width, Bitmap.Height));
  GetImageFormatInfo(Image.Format, Info);

  if not Info.IsSpecial then
  begin
    Bpp := Info.BytesPerPixel;
    SrcWidthBytes := Image.Width * Bpp;
    MoveBytes := Width * Bpp;
    SrcPtr := @PByteArray(Image.Bits)[SrcY * SrcWidthBytes + SrcX * Bpp];
    Bitmap.Map(TMapAccess.maReadWrite, MapData);

    for Y := 0 to Height - 1 do
    begin
      if Info.Format = ifA8R8G8B8 then
      begin
       for X := 0 to Pred(Width) do
        begin
          DstPtr := @PColor32RecArray(MapData.Data)[Y * (MapData.Pitch div 4) + X];
          Move(SrcPtr^, ARGB, 4);
          DstPtr^.A := ARGB.A;
          DstPtr^.R := ARGB.R;
          DstPtr^.G := ARGB.G;
          DstPtr^.B := ARGB.B;
          Inc(SrcPtr, 4);
        end;
      end
      else
      begin
        for X := 0 to Width - 1 do
        begin
          DstPtr := @PColor32RecArray(MapData.Data)[Y * (MapData.Pitch div 4)+X];
          ConvertToAlphaColorRec(SrcPtr, DstPtr, Info, Image.Palette);
          Inc(SrcPtr, Bpp);
        end;
      end;
    end;
  end
  else
  begin
    InitImage(TempImage);
    CloneImage(Image, TempImage);
    ConvertImage(TempImage, ifA8R8G8B8);
    try
      CopyRectToFmxBitmap(TempImage, Bitmap, SrcX, SrcY, Width, Height, DstX, DstY);
    finally
      FreeImage(TempImage);
    end;
  end;
  Bitmap.UnMap(MapData);
end;

procedure CopyRectToFmxBitmap(Image: TBaseImage; Bitmap: TBitmap;
  SrcX, SrcY, Width, Height, DstX, DstY: LongInt);
begin
  CopyRectToFmxBitmap(Image.ImageDataPointer^, Bitmap,
    SrcX, SrcY, Width, Height, DstX, DstY);
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77.1 Changes/Bug Fixes ---------------------------------
    - Removed support for old FMX versions (XE2 etc.)
    - Support for current FMX version (XE4+) contributed by Ken Schafer.

  -- 0.77 Changes/Bug Fixes -----------------------------------
    - Unit created with initial stuff, working with FMX1 in Delphi XE2.
 }

end.
