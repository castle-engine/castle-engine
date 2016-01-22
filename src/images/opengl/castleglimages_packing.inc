{
  Copyright 2001-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: packing helpers. }

{$ifdef read_interface}

type
  TUnpackNotAlignedData = record
    Alignment: Cardinal;
  end;
  TPackNotAlignedData = TUnpackNotAlignedData;

{ Save/restore OpenGL pixel store for unpacking / packing given TCastleImage.
  Before you pass this image to some OpenGL procedures
  (like glDrawPixels for unpacking, glReadPixels for packing),
  call BeforeXxx, and later call AfterXxx to restore original state.
  These will take care of setting/restoring pixel alignment.
  @groupBegin }
procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TCastleImage);
procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TCastleImage);
procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TCastleImage);
procedure AfterPackImage(const packData: TPackNotAlignedData; image: TCastleImage);
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TCastleImage);
begin
  unpackData.Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);
  if unpackData.Alignment = 0 then
    raise Exception.Create('OpenGL context is probably not initialized yet: glGetInteger(GL_UNPACK_ALIGNMENT) returned 0');
  if (image.Width * Image.PixelSize mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
end;

procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TCastleImage);
begin
  if (image.Width * Image.PixelSize mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, unpackData.Alignment);
end;

procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TCastleImage);
begin
  packData.Alignment := glGetInteger(GL_PACK_ALIGNMENT);
  if (image.Width * Image.PixelSize mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
end;

procedure AfterPackImage(const packData: TPackNotAlignedData; image: TCastleImage);
begin
  if (image.Width * Image.PixelSize mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, packData.Alignment);
end;

{$endif read_implementation}
