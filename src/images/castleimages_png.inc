{%MainUnit castleimages.pas}
{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle PNG format. }

var
  WarningNoLibPngDone: Boolean;

{ Lack of LibPng is important to report, as it causes PNG loading to be much slower.

  Note that this warning shall be done only if you load PNG,
  lack of LibPng is harmless if your application doesn't load PNG (and in this case
  our build tool would not copy libpng DLL into the project on Windows,
  so CastlePngInitialized will rightly fail). }
procedure WarningNoLibPng;
begin
  if not WarningNoLibPngDone then
  begin
    WarningNoLibPngDone := true;

    {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
    { Do not make any warning when InternalDisableDynamicLibraries,
      build tool deliberately uses this (see tools/build-tool/code/tooldisabledynamiclibraries.pas)
      and it is harmless, there's no problem with build tool not using faster LibPng. }
    if not InternalDisableDynamicLibraries then
    begin
      if not CastleZLibInitialized then
        WritelnWarning('Initializing dynamic Zlib library failed.' + NL +
          '  Note that it may also cause failures to initialize libraries that require Zlib, like LibPng.'
          {$ifdef MSWINDOWS} +
          NL +
          '  Make sure you have copied the required DLL files alongside the EXE file, with the correct CPU architecture (32-bit vs 64-bit).' + NL +
          '  We advise to build your applications using Castle Game Engine editor or (command-line) build tool, that will automatically place the required DLL files.'
          {$endif}
        );

      if not CastlePngInitialized then
        WritelnWarning('Initializing dynamic LibPng library failed.' + NL +
          '  Loading PNG will use a slower approach.'
          {$ifdef MSWINDOWS} +
          NL +
          '  Make sure you have copied the required DLL files alongside the EXE file, with the correct CPU architecture (32-bit vs 64-bit).' + NL +
          '  We advise to build your applications using Castle Game Engine editor or (command-line) build tool, that will automatically place the required DLL files.'
          {$endif}
        );
    end;
    {$else}
    WritelnWarning('Initializing LibPng failed, LibPng support not compiled-in.' + NL +
      '  Loading PNG will use a slower approach.');
    {$endif}
  end;
end;

function LoadPNG(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  // use LibPng, this is the preferred (fastest) option
  {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
  if CastlePngInitialized then
  begin
    // Necessary on Android, where InitializePNGUsingLibpng in "initialization" was too early.
    InitializePNGUsingLibpng;
    Result := LoadPNG_LibPng(Stream, AllowedImageClasses);
  end else
  {$endif}
  begin
    WarningNoLibPng;
    {$if defined(USE_VAMPYRE_IMAGING)} // fallback LibPng -> Vampyre
    Result := Load_VampyreImaging(Stream, AllowedImageClasses);
    {$elseif defined(FPC)} // fallback LibPng -> FpImage (FPC only)
    Result := Load_FpImage(Stream, AllowedImageClasses, TFPReaderPNG.Create, ifPNG)
    {$else} // fallback LibPng -> PngImage (Delphi only)
    Result := LoadPNG_PngImage(Stream, AllowedImageClasses);
    {$endif}
  end;
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
    Save_FpImage(Img, Stream, Writer);
  end;
  {$endif}

begin
  {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
  if CastlePngInitialized then
  begin
    // Necessary on Android, where InitializePNGUsingLibpng in "initialization" was too early.
    InitializePNGUsingLibpng;
    SavePNG_LibPng(Img, Stream);
  end else
  {$endif}
  begin
    {$if defined(USE_VAMPYRE_IMAGING)} // fallback LibPng -> Vampyre
    Save_VampyreImaging(Img, Stream, 'png');
    {$elseif defined(FPC)} // fallback LibPng -> FpImage
    SavePNG_FPImage(Img, Stream);
    {$else}
    raise EImageSaveError.Create('Cannot save PNG: LibPNG not initialized, and Vampyre and FpImage not available at compile-time');
    {$endif}
  end;
end;
