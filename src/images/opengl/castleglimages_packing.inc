{
  Copyright 2001-2017 Michalis Kamburelis.

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

{ Save / restore OpenGL pixel store for unpacking / packing given TCastleImage.
  Before you pass this image to some OpenGL procedures
  (like glDrawPixels for unpacking, glReadPixels for packing),
  call BeforeXxx, and later call AfterXxx to restore original state.
  These will take care of setting / restoring pixel alignment.
  @groupBegin }
procedure BeforeUnpackImage(const Image: TCastleImage);
procedure AfterUnpackImage(const Image: TCastleImage);
procedure BeforePackImage(const Image: TCastleImage);
procedure AfterPackImage(const Image: TCastleImage);
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

const
  DefaultGLUnpackAlignment = 4;

{ We assume here that nothing else changes glPixelStorei,
  so it's always Default*Alignment at start.
  We can avoid querying it with glGetInteger,
  or storing in per-context or global variable, this way. }

procedure BeforeUnpackImage(const Image: TCastleImage);
begin
  if (Image.Width * Image.PixelSize mod DefaultGLUnpackAlignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
end;

procedure AfterUnpackImage(const Image: TCastleImage);
begin
  if (Image.Width * Image.PixelSize mod DefaultGLUnpackAlignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, DefaultGLUnpackAlignment);
end;

const
  DefaultGLPackAlignment = 4;

procedure BeforePackImage(const Image: TCastleImage);
begin
  if (Image.Width * Image.PixelSize mod DefaultGLPackAlignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
end;

procedure AfterPackImage(const Image: TCastleImage);
begin
  if (Image.Width * Image.PixelSize mod DefaultGLPackAlignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, DefaultGLPackAlignment);
end;

{$endif read_implementation}
