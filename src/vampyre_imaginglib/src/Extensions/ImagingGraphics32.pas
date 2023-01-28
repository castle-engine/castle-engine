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

{ Unit functions for converting and copying images between Imaging and Graphics32 library.}
unit ImagingGraphics32;

{$I ImagingOptions.inc}

interface

uses
  Types, GR32, ImagingTypes, Imaging, ImagingFormats, ImagingUtility, ImagingClasses;

{ Converts image from TImageData record to GR32's bitmap. Bitmap32 must be already
  instantiated.}
procedure ConvertImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
{ Converts image from TBaseImage instance to GR32's bitmap. Bitmap32 must be already
  instantiated.}
procedure ConvertImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);

{ Converts image data from GR32's bitmap to TImageData record.}
procedure ConvertBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);
{ Converts image data from GR32's bitmap to existing TBaseImage instance.}
procedure ConvertBitmap32ToImage(Bitmap32: TCustomBitmap32; Image: TBaseImage);

{ Copies pixels from TImageData record (with all the necessary conversion if
  the format is not 32bit) to existing GR32's bitmap. Both Image and Bitmap32 must
  have the same width and height. }
procedure CopyImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
{ Copies pixels from TBaseImage instance (with all the necessary conversion if
  the format is not 32bit) to existing GR32's bitmap. Both Image and Bitmap32 must
  have the same width and height. }
procedure CopyImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);

{ Copies rectangular area of pixels from TImageData record to existing GR32's bitmap.}
procedure CopyRectToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer); overload;
{ Copies rectangular area of pixels from TBaseImage instance to existing GR32's bitmap.}
procedure CopyRectToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer); overload;

{ Maps GR32 bitmap on TImageData record so that they'll both share
  the same pixels in memory (Bitmap32.Bits and Image.Bits point to the same
  memory address). Useful if you wan to e.g. save Bitmap32 using Imaging
  and don't want to needlessly duplicate the entire image in memory.
  Note that you must not call FreeImage on Image after the mapping or
  the memory of Bitmap32 would be freed too.}
procedure MapBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);

implementation

procedure ConvertImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
begin
  Assert(TestImage(Image));
  Bitmap32.SetSize(Image.Width, Image.Height);
  CopyImageDataToBitmap32(Image, Bitmap32);
end;

procedure ConvertImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);
begin
  ConvertImageDataToBitmap32(Image.ImageDataPointer^, Bitmap32);
end;

procedure ConvertBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);
begin
  Assert(not Bitmap32.Empty);
  NewImage(Bitmap32.Width, Bitmap32.Height, ifA8R8G8B8, Image);
  Move(Bitmap32.Bits^, Image.Bits^, Image.Size);
end;

procedure ConvertBitmap32ToImage(Bitmap32: TCustomBitmap32; Image: TBaseImage);
begin
  ConvertBitmap32ToImageData(Bitmap32, Image.ImageDataPointer^);
end;

procedure CopyImageDataToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32);
begin
  Assert(TestImage(Image) and (Image.Width = Bitmap32.Width) and (Image.Height = Bitmap32.Height));
  CopyRectToBitmap32(Image, Bitmap32, 0, 0, Image.Width, Image.Height, 0, 0);
end;

procedure CopyImageToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32);
begin
  CopyImageDataToBitmap32(Image.ImageDataPointer^, Bitmap32);
end;

procedure CopyRectToBitmap32(const Image: TImageData; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer);
var
  TempImage: TImageData;
  X, Y, Bpp, SrcWidthBytes, DstWidth, MoveBytes: Integer;
  DstPtr: PColor32Rec;
  SrcPtr: PByte;
  Info: TImageFormatInfo;
begin
  Assert(TestImage(Image) and not Bitmap32.Empty);

  ClipCopyBounds(SrcX, SrcY, Width, Height, DstX, DstY, Image.Width, Image.Height,
    Rect(0, 0, Bitmap32.Width, Bitmap32.Height));

  if Image.Format in [ifIndex8, ifGray8, ifA8Gray8, ifGray16, ifR8G8B8, ifA8R8G8B8,
    ifR16G16B16, ifA16R16G16B16] then
  begin
    GetImageFormatInfo(Image.Format, Info);
    Bpp := Info.BytesPerPixel;
    SrcWidthBytes := Image.Width * Bpp;
    DstWidth := Bitmap32.Width;
    MoveBytes := Width * Bpp;
    SrcPtr := @PByteArray(Image.Bits)[SrcY * SrcWidthBytes + SrcX * Bpp];
    DstPtr := @PColor32RecArray(Bitmap32.Bits)[DstY * DstWidth + DstX];

    for Y := 0 to Height - 1 do
    begin
      case Image.Format of
        ifIndex8:
          for X := 0 to Width - 1 do
          begin
            DstPtr^ := Image.Palette[SrcPtr^];
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifGray8:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := SrcPtr^;
            DstPtr.G := SrcPtr^;
            DstPtr.B := SrcPtr^;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifA8Gray8:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := SrcPtr^;
            DstPtr.G := SrcPtr^;
            DstPtr.B := SrcPtr^;
            DstPtr.A := PWordRec(SrcPtr).High;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifGray16:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := PWord(SrcPtr)^ shr 8;
            DstPtr.G := DstPtr.R;
            DstPtr.B := DstPtr.R;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifR8G8B8:
          for X := 0 to Width - 1 do
          begin
            DstPtr.Color24Rec := PColor24Rec(SrcPtr)^;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifA8R8G8B8:
          begin
            Move(SrcPtr^, DstPtr^, MoveBytes);
            Inc(DstPtr, Width);
            Inc(SrcPtr, MoveBytes);
          end;
        ifR16G16B16:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := PColor48Rec(SrcPtr).R shr 8;
            DstPtr.G := PColor48Rec(SrcPtr).G shr 8;
            DstPtr.B := PColor48Rec(SrcPtr).B shr 8;
            DstPtr.A := 255;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
        ifA16R16G16B16:
          for X := 0 to Width - 1 do
          begin
            DstPtr.R := PColor64Rec(SrcPtr).R shr 8;
            DstPtr.G := PColor64Rec(SrcPtr).G shr 8;
            DstPtr.B := PColor64Rec(SrcPtr).B shr 8;
            DstPtr.A := PColor64Rec(SrcPtr).A shr 8;
            Inc(DstPtr);
            Inc(SrcPtr, Bpp);
          end;
      end;

      Inc(SrcPtr, SrcWidthBytes - MoveBytes);
      Inc(DstPtr, DstWidth - Width);
    end;
  end
  else
  begin
    InitImage(TempImage);
    CloneImage(Image, TempImage);
    ConvertImage(TempImage, ifA8R8G8B8);
    try
      CopyRectToBitmap32(TempImage, Bitmap32, SrcX, SrcY, Width, Height, DstX, DstY);
    finally
      FreeImage(TempImage);
    end;
  end;
end;

procedure CopyRectToBitmap32(Image: TBaseImage; Bitmap32: TCustomBitmap32;
  SrcX, SrcY, Width, Height, DstX, DstY: Integer);
begin
  CopyRectToBitmap32(Image.ImageDataPointer^, Bitmap32,
    SrcX, SrcY, Width, Height, DstX, DstY);
end;

procedure MapBitmap32ToImageData(Bitmap32: TCustomBitmap32; var Image: TImageData);
begin
  Assert(not Bitmap32.Empty);
  FreeImage(Image);

  Image.Width := Bitmap32.Width;
  Image.Height := Bitmap32.Height;
  Image.Format := ifA8R8G8B8;
  Image.Size := Image.Width * Image.Height * 4;

  Image.Bits := Bitmap32.Bits;
end;

{
  File Notes:

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Created with initial stuff.
}

end.
