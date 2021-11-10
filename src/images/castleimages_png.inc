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

{ Handle PNG format using either LibPng or FPImage. }

function LoadPNG(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
  if CastlePngInitialized then
  begin
    // Necessary on Android, where InitializePNGUsingLibpng in "initialization" was too early.
    InitializePNGUsingLibpng;
    Result := LoadPNG_LibPng(Stream, AllowedImageClasses);
  end else
  {$endif}
  {$ifdef FPC}
    Result := LoadPNG_FPImage(Stream, AllowedImageClasses);
  {$else}
    Result := LoadPNG_PngImage(Stream, AllowedImageClasses);
  {$endif}
end;

procedure SavePNG(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
  if CastlePngInitialized then
  begin
    // Necessary on Android, where InitializePNGUsingLibpng in "initialization" was too early.
    InitializePNGUsingLibpng;
    SavePNG_LibPng(Img, Stream)
  end else
  {$endif}
  {$ifdef FPC}
    SavePNG_FPImage(Img, Stream);
  {$else}
    raise Exception.Create('LibPNG not initialized');
    // TODO: Delphi solution
  {$endif}
end;
