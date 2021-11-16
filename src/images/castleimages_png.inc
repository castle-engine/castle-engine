{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle PNG format. }

function LoadPNG(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Result := Load_VampyreImaging(Stream, AllowedImageClasses);
  {$else}
    {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)} // use LibPng
    if CastlePngInitialized then
    begin
      // Necessary on Android, where InitializePNGUsingLibpng in "initialization" was too early.
      InitializePNGUsingLibpng;
      Result := LoadPNG_LibPng(Stream, AllowedImageClasses);
    end else
    {$endif}
    {$ifdef FPC} // fallback LibPng -> FpImage (FPC only)
      Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPNG.Create, ifPNG)
    {$else} // fallback LibPng -> PngImage (Delphi only)
      Result := LoadPNG_PngImage(Stream, AllowedImageClasses);
    {$endif}
  {$endif}
end;

procedure SavePNG(const Img: TEncodedImage; const Stream: TStream);

  {$ifdef FPC}
  procedure SavePNG_FPImage(const Img: TEncodedImage; const Stream: TStream);
  var
    Writer: TFPWriterPNG;
  begin
    Writer := TFPWriterPNG.Create;
    { without this, Writer.UseAlpha is always false and TFPWriterPNG never
      stores alpha channel of png image. }
    Writer.UseAlpha := Img.HasAlpha;
    { By default, we produce 16-bit PNG. Which usually is useless
      (as our inputs is 8-bit), and makes "nvcompress" on macOS fail
      on PNG files produced by our "downscaling". }
    Writer.WordSized := false;
    SaveFpImage(Img, Stream, Writer);
  end;
  {$endif}

begin
  {$if defined(USE_VAMPYRE_IMAGING)} // if available, prefer to use Vampyre
  Save_VampyreImaging(Img, Stream, 'jpg');
  {$else}
    {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
    if CastlePngInitialized then
    begin
      // Necessary on Android, where InitializePNGUsingLibpng in "initialization" was too early.
      InitializePNGUsingLibpng;
      SavePNG_LibPng(Img, Stream);
    end else
    {$endif}
    {$ifdef FPC}
      SavePNG_FPImage(Img, Stream);
    {$else}
      raise Exception.Create('LibPNG not initialized');
    {$endif}
  {$endif}
end;
