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
    Result := LoadPNG_LibPng(Stream, AllowedImageClasses)
  else
  {$endif}
    Result := LoadPNG_FPImage(Stream, AllowedImageClasses);
end;

procedure SavePNG(const Img: TEncodedImage; const Stream: TStream);
begin
  {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
  if CastlePngInitialized then
    SavePNG_LibPng(Img, Stream)
  else
  {$endif}
    SavePNG_FPImage(Img, Stream);
end;
