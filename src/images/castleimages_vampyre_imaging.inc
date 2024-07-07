{%MainUnit castleimages.pas}
{
  Copyright 2021-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle images format using Vampyre Imaging:
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io/ }

{$ifdef USE_VAMPYRE_IMAGING}

function Load_VampyreImaging(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
var
  Img: TSingleImage;

  { Copy row-by-row data from Img to Target, assuming exactly the same pixel format,
    and the same order within rows.
    Flips rows (as TCastleImage wants rows from bottom to top). }
  procedure CopyRowsFlip(const Target: TCastleImage);
  var
    I: Integer;
  begin
    for I := 0 to Img.Height - 1 do
      Move(Img.ScanLine[I]^, Target.RowPtr(Img.Height - 1 - I)^, Target.PixelSize * Img.Width);
  end;

  { Copy whole data from Img to Target, assuming exactly the same memory format. }
  procedure CopyWhole(const Target: TEncodedImage);
  begin
    if Target.Size <> Img.ImageDataPointer^.Size then
      raise EImageLoadError.CreateFmt('Reading image from Vampyre Imaging failed, expected size %d but got size %d, for format %s', [
        Target.Size,
        Img.ImageDataPointer^.Size,
        GetFormatName(Img.Format)
      ]);
    Move(Img.ImageDataPointer^.Bits^, Target.RawPixels^, Target.Size);
  end;

begin
  Img := TSingleImage.CreateFromStream(Stream);
  try
    // WritelnLog('Loaded image from Vampyre Imaging, format %s, size %dx%d', [
    //   GetFormatName(Img.Format),
    //   Img.Width,
    //   Img.Height
    // ]);
    // happens e.g. when reading TIFF, but TIFF dynamic library not available
    if Img.Format = ifUnknown then
      raise EImageLoadError.Create('Reading image using Vampyre Imaging failed, unknown or unsupported format');
    case Img.Format of
      ifGray8: Result := TGrayscaleImage.Create(Img.Width, Img.Height);
      ifA8Gray8: Result := TGrayscaleAlphaImage.Create(Img.Width, Img.Height);
      ifA8R8G8B8:
        begin
          Result := TRGBAlphaImage.Create(Img.Width, Img.Height);
          SwapChannels(Img.ImageDataPointer^, ChannelRed, ChannelBlue);
        end;
      ifR8G8B8:
        begin
          Result := TRGBImage.Create(Img.Width, Img.Height);
          SwapChannels(Img.ImageDataPointer^, ChannelRed, ChannelBlue);
        end;
      // TODO: tcDxt1_RGBA or tcDxt1_RGB
      ifDXT1: Result := TGPUCompressedImage.Create(Img.Width, Img.Height, 1, tcDxt1_RGBA);
      ifDXT3: Result := TGPUCompressedImage.Create(Img.Width, Img.Height, 1, tcDxt3);
      ifDXT5: Result := TGPUCompressedImage.Create(Img.Width, Img.Height, 1, tcDxt5);

      // example: RGBE/HDR images
      ifR32G32B32F:
        begin
          Result := TRGBFloatImage.Create(Img.Width, Img.Height);
          SwapChannels(Img.ImageDataPointer^, ChannelRed, ChannelBlue);
        end;
      ifB32G32R32F: Result := TRGBFloatImage.Create(Img.Width, Img.Height);

      { Example of ifR16G16B16:
        16-bit PNGs from https://www.motionforgepictures.com/height-maps/ .
        Convert these to TRGBFloatImage to keep precision beyond 8 bits,
        useful for terrains. }
      ifR16G16B16, ifB16G16R16:
        begin
          WritelnLog('Converting Vampyre Imaging format %s -> %s', [
            GetFormatName(Img.Format),
            GetFormatName(ifB32G32R32F)
          ]);
          Img.Format := ifB32G32R32F; // calls ConvertImage and actually changes content
          Result := TRGBFloatImage.Create(Img.Width, Img.Height);
        end;

      { A few grayscale formats that have precision beyond 8-bit,
        very useful for terrains. }
      ifR32F: Result := TGrayscaleFloatImage.Create(Img.Width, Img.Height);
      ifR16F, ifGray16, ifGray32:
        begin
          WritelnLog('Converting Vampyre Imaging format %s -> %s', [
            GetFormatName(Img.Format),
            GetFormatName(ifR32F)
          ]);
          Img.Format := ifR32F; // calls ConvertImage and actually changes content
          Result := TGrayscaleFloatImage.Create(Img.Width, Img.Height);
        end;

      else
        begin
          WritelnLog('Converting Vampyre Imaging format %s -> %s', [
            GetFormatName(Img.Format),
            GetFormatName(ifA8R8G8B8)
          ]);

          { This allows to read indexed (palette), like ifIndex8,
            or 16-bit (ifA16R16G16B16F, ifA16R16G16B16) through standard
            8-bit ARGB.

            Testcase: vampyre_imaginglib/src/Demos/Data/Tigers.tga
            Testcase: vampyre_imaginglib/src/Demos/Data/Tigers.tif }
          Img.Format := ifA8R8G8B8; // calls ConvertImage and actually changes content

          Result := TRGBAlphaImage.Create(Img.Width, Img.Height);
          SwapChannels(Img.ImageDataPointer^, ChannelRed, ChannelBlue);
        end;
    end;

    if Result is TCastleImage then
      CopyRowsFlip(TCastleImage(Result))
    else
      CopyWhole(Result);
  finally FreeAndNil(Img) end;

  { TODO: honor AllowedImageClasses, to fix the loaded data type
    using Vampyre transformations, *before* LoadEncodedImage
    sees it. LoadEncodedImage will also fix the loaded data type, but it is slower. }
end;

procedure Save_VampyreImaging(const CastleImage: TEncodedImage; const Stream: TStream; const Ext: String);
var
  Img: TSingleImage;

  { Copy row-by-row data from Source to Img, assuming exactly the same pixel format,
    and the same order within rows.
    Flips rows (as TCastleImage wants rows from bottom to top). }
  procedure CopyRowsFlip(const Source: TCastleImage);
  var
    I: Integer;
  begin
    for I := 0 to Img.Height - 1 do
      Move(Source.RowPtr(Img.Height - 1 - I)^, Img.ScanLine[I]^, Source.PixelSize * Img.Width);
  end;

  { Copy whole data from Img to Source, assuming exactly the same memory format. }
  procedure CopyWhole(const Source: TEncodedImage);
  begin
    if Source.Size <> Img.ImageDataPointer^.Size then
      raise EImageSaveError.CreateFmt('Saving image to Vampyre Imaging failed, expected size %d but got size %d, for format %s', [
        Source.Size,
        Img.ImageDataPointer^.Size,
        GetFormatName(Img.Format)
      ]);
    Move(Source.RawPixels^, Img.ImageDataPointer^.Bits^, Source.Size);
  end;

var
  Format: TImageFormat;
begin
  { calculate Format }
  if CastleImage is TGrayscaleImage then
    Format := ifGray8
  else
  if CastleImage is TGrayscaleAlphaImage then
    Format := ifA8Gray8
  else
  if CastleImage is TRGBImage then
    Format := ifR8G8B8
  else
  if CastleImage is TRGBAlphaImage then
    Format := ifA8R8G8B8
  else
  if CastleImage is TGPUCompressedImage then
  begin
    case TGPUCompressedImage(CastleImage).Compression of
      tcDxt1_RGB, tcDxt1_RGBA: Format := ifDXT1;
      tcDxt3: Format := ifDXT3;
      tcDxt5: Format := ifDXT5;
      else raise EImageSaveError.CreateFmt('Cannot save image with GPU compression %s using Vampyre Imaging', [
        TextureCompressionToString(TGPUCompressedImage(CastleImage).Compression)
      ]);
    end;
  end else
  if CastleImage is TRGBFloatImage then
    Format := ifR32G32B32F
  else
  begin
    raise EImageSaveError.CreateFmt('Cannot save image with format %s using Vampyre Imaging', [
      CastleImage.ClassName
    ]);
  end;

  Img := TSingleImage.CreateFromParams(CastleImage.Width, CastleImage.Height, Format);
  try
    if CastleImage is TCastleImage then
      CopyRowsFlip(TCastleImage(CastleImage))
    else
      CopyWhole(CastleImage);
    if Format in [ifR8G8B8, ifA8R8G8B8, ifR32G32B32F] then
      SwapChannels(Img.ImageDataPointer^, ChannelRed, ChannelBlue);
    if not Img.SaveToStream(Ext, Stream) then
      raise EImageSaveError.CreateFmt('Cannot save image to file format %s using Vampyre Imaging', [
        Ext
      ]);
  finally FreeAndNil(Img) end;
end;

{$endif}
