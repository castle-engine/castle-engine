{%MainUnit castleimages.pas}
{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
function LoadGIF(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$elseif defined(FPC)}
  Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderGIF.Create, ifGIF);
  {$else}
  raise EImageLoadError.Create('Cannot read GIF (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

function LoadTGA(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$elseif defined(FPC)}
  Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderTarga.Create, ifTGA);
  {$else}
  raise EImageLoadError.Create('Cannot read TGA (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

{$ifdef CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}
function LoadTIFF(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$else}
  raise EImageLoadError.Create('Cannot read TIFF (Vampyre Imaging Library not available at compile-time)');
  Result := nil; // just silence FPC warning
  {$endif}
end;

function LoadJP2(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$else}
  raise EImageLoadError.Create('Cannot read JP2 (Vampyre Imaging Library not available at compile-time)');
  Result := nil; // just silence FPC warning
  {$endif}
end;
{$endif CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}

function LoadJPEG(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$elseif defined(FPC)}
  Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderJPEG.Create, ifJPEG);
  {$else}
  raise EImageLoadError.Create('Cannot read JPEG (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

function LoadXPM(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  // FpImage XPM reader crashes on examples/images_videos/image_display/data/images/test_texture.xpm
  // {$elseif defined(FPC)}
  // Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderXPM.Create, ifXPM);
  {$else}
  raise EImageLoadError.Create('Cannot read XPM (Vampyre Imaging Library not available at compile-time)');
  Result := nil; // silence FPC 3.2.2 warning
  {$endif}
end;

function LoadPSD(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$elseif defined(FPC)}
  Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderPSD.Create, ifPSD);
  {$else}
  raise EImageLoadError.Create('Cannot read PSD (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

function LoadPCX(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$elseif defined(FPC)}
  Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderPCX.Create, ifPCX);
  {$else}
  raise EImageLoadError.Create('Cannot read PCX (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

{ Load PNM image (PNM, PGM, PBM, PPM). }
function LoadPNM(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$elseif defined(FPC)}
  // Known to fail on PPM like examples/images_videos/image_display/data/images/sky_red_left.ppm
  // TODO: submit FPC bug
  Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderPNM.Create, ifPNM);
  {$else}
  raise EImageLoadError.Create('Cannot read PNM (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

function LoadBMP(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$elseif defined(FPC)}
  Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderBMP.Create, ifBMP);
  {$else}
  raise EImageLoadError.Create('Cannot read BMP (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

function LoadRGBE(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)}
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$else}
  raise EImageLoadError.Create('Cannot read RGBE (Vampyre Imaging Library not available at compile-time)');
  {$endif}
end;

{ Saving image (format-specific) --------------------------------------------

  Each file format may have specialized SaveXxx that allows
  you to give some parameters special for given format.

  Each format must also have procedure with two parameters
  (Img: TEncodedImage; Stream: TStream), this will be used with
  ImageFormatsInfo[].
  This means that below we must use overloading instead of
  default parameters, since pointers to given procedures must be
  compatible with @link(TImageSaveFunc).

  SaveXxx should
    raise EImageSaveError.CreateFmt('Saving to XXX image class %s not possible', [Img.ClassName]);
  when Img doesn't have acceptable class.
  Also, list of handled image classes should be reflected in SavedClasses
  in ImageFormatsInfo[] for this format.
}

procedure SaveJPEG(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'jpg');
  {$elseif defined(FPC)}
  Save_FpImage(Img, Stream, TFPWriterJPEG.Create);
  {$else}
  raise Exception.Create('Cannot save JPEG (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

procedure SaveGIF(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'gif');
  {$else}
  raise EImageSaveError.Create('Cannot save GIF (Vampyre Imaging Library not available at compile-time)');
  {$endif}
end;

procedure SaveTGA(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'tga');
  {$else}
  raise EImageSaveError.Create('Cannot save TGA (Vampyre Imaging Library not available at compile-time)');
  {$endif}
end;

procedure SaveXPM(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'xpm');
  {$else}
  raise EImageSaveError.Create('Cannot save XPM (Vampyre Imaging Library not available at compile-time)');
  {$endif}
end;

procedure SavePNM(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'pnm');
  {$elseif defined(FPC)}
  Save_FpImage(Img, Stream, TFPWriterPNM.Create);
  {$else}
  raise EImageSaveError.Create('Cannot save BMP (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

procedure SaveBMP(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'bmp');
  {$elseif defined(FPC)}
  Save_FpImage(Img, Stream, TFPWriterBMP.Create);
  {$else}
  raise EImageSaveError.Create('Cannot save BMP (neither FpImage nor Vampyre Imaging Library available at compile-time)');
  {$endif}
end;

{$ifdef CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}
procedure SaveTIFF(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'tif');
  {$else}
  raise EImageSaveError.Create('Cannot save TIFF (Vampyre Imaging Library not available at compile-time)');
  {$endif}
end;

procedure SaveJP2(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'jp2');
  {$else}
  raise EImageSaveError.Create('Cannot save JP2 (Vampyre Imaging Library not available at compile-time)');
  {$endif}
end;
{$endif CASTLE_ENABLE_PLATFORM_SPECIFIC_IMAGE_FORMATS}

procedure SaveRGBE(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'hdr');
  {$else}
  raise EImageSaveError.Create('Cannot save RGBE (Vampyre Imaging Library not available at compile-time)');
  {$endif}
end;
