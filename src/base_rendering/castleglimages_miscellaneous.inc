{%MainUnit castleglimages.pas}
{
  Copyright 2001-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: miscellaneous stuff. }
{$ifdef read_interface}

{$ifdef read_interface_const}
const
  PixelsImageClasses: array [0..8] of TEncodedImageClass = (
    // 8-bit
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage,

    // compressed
    TGPUCompressedImage,

    // floating-point
    TGrayscaleFloatImage,
    TGrayscaleAlphaFloatImage,
    TRGBFloatImage,
    TRGBAlphaFloatImage
  );
{$endif read_interface_const}

{$ifdef read_interface_func}
{ Return appropriate OpenGL format and type constants
  for given TCastleImage descendant. If you will pass here Img
  that is not a descendant of one of TextureImageClassesAll
  or PixelsImageClasses, they will raise EImageClassNotSupportedForOpenGL.

  ImageGLInternalFormat works with TGPUCompressedImage classes also, returning
  appropriate enum, suitable for glCompressedTexImage2D.

  @raises(EImageClassNotSupportedForOpenGL When Img class is not supported
    by OpenGL.)

  @groupBegin }
function ImageGLFormat(const Img: TCastleImage): TGLenum;
function ImageGLInternalFormat(const Img: TEncodedImage): TGLenum;
function ImageGLType(const Img: TCastleImage): TGLenum;
{ @groupEnd }
{$endif read_interface_func}

{$ifdef read_interface_type}
//type - don't add type Delphi can't parse that correctly
  TGLTextureId = TGLTexture;

  ETextureLoadError = class(Exception);
  EImageClassNotSupportedForOpenGL = class(ETextureLoadError);
  ECannotLoadCompressedTexture = class(ETextureLoadError);
  EInvalidImageForOpenGLTexture = class(ETextureLoadError);
{$endif read_interface_type}

{$ifdef read_interface_func}

{ Delete texture object and set it to GLObjectNone.

  - Before doing anything, checks is it GLObjectNone.
  - After freeing, sets it to GLObjectNone.
  - Also takes care to call TextureMemoryProfiler.Deallocate . }
procedure FreeTexture(var Tex: TGLTextureId);

{ Call glTexParameterf to set GL_TEXTURE_MAX_ANISOTROPY_EXT on given texture
  target.

  Takes care to check for appropriate OpenGL extension (if not present,
  does nothing), and to query OpenGL limit for Anisotropy (eventually
  clamping provided Anisotropy down). }
procedure TexParameterMaxAnisotropy(const target: TGLenum; const Anisotropy: TGLfloat);
{$endif read_interface_func}

{$endif read_interface}

{$ifdef read_implementation}

{ Missing from FPC OpenGL units. }
const
  GL_TEXTURE_SWIZZLE_RGBA = $8E46;

{ Get OpenGL enums for "format" and "internalFormat" for OpenGL routines like
  https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml
  that load uncompressed images. }
procedure UncompressedGLFormatAndInternalFormat(const Img: TCastleImage;
  out ImageFormat: TGLenum;
  out ImageInternalFormat: TGLenum;
  out ImageSwizzle: TVector4Integer);

{ Missing from FPC OpenGL units. }
const
  GL_R8 = $8229;
  GL_RG = $8227;
  GL_RG8 = $822B;

{ For all image types, we have

  - Modern enum, required by OpenGL "core" profile,
    see https://www.khronos.org/opengl/wiki/Image_Format .
    We can use it if GLFeatures.TextureModernFormatsAndSwizzle .

  - Compatibility enum: name accepted by OpenGL ES 2 (
    https://registry.khronos.org/OpenGL-Refpages/es2.0/xhtml/glTexImage2D.xml )
    and latest OpenGL in compatibility mode.

  - Moreover, when GLFeatures.EnableFixedFunction, we adjust to OpenGL 1.x where
    the "internal format" for glTexImage2d was just a number,
    like 2,3,4.

    In OpenGLES >= 2 and in OpenGL >= 3.2 "core" profile this is not allowed
    anymore.
}

{$ifdef CASTLE_WEBGL}
{ TODO: web: WebGL does not support swizzle, http://www.g-truc.net/post-0734.html
  Below are dummy values (takes from CastleGL, but they don't really matter,
  should not be used by anything) just to make this code compile. }
const
  GL_GREEN = $1904;
  GL_BLUE = $1905;
{$endif}

begin
  { ImageSwizzle passing depends on it. }
  Assert(SizeOf(Integer) = SizeOf(TGLint));

  if Img is TRGBImage then
  begin
    ImageSwizzle := Vector4Integer(GL_RED, GL_GREEN, GL_BLUE, GL_ONE);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RGB;
      ImageInternalFormat := GL_RGB8;
    end else
    begin
      ImageFormat := GL_RGB;
      if GLFeatures.EnableFixedFunction then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
  if Img is TRGBAlphaImage then
  begin
    ImageSwizzle := Vector4Integer(GL_RED, GL_GREEN, GL_BLUE, GL_ALPHA);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RGBA;
      ImageInternalFormat := GL_RGBA8;
    end else
    begin
      ImageFormat := GL_RGBA;
      if GLFeatures.EnableFixedFunction then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
  if Img is TGrayscaleImage then
  begin
    { Use texture swizzle to load texture 1st channel (red) -> as luminance (fill RGB) or alpha. }
    if TGrayscaleImage(Img).TreatAsAlpha then
      ImageSwizzle := Vector4Integer(GL_ONE, GL_ONE, GL_ONE, GL_RED)
    else
      ImageSwizzle := Vector4Integer(GL_RED, GL_RED, GL_RED, GL_ONE);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RED;
      ImageInternalFormat := GL_R8;
    end else
    begin
      ImageFormat := Iff(TGrayscaleImage(Img).TreatAsAlpha, GL_ALPHA, GL_LUMINANCE);
      if GLFeatures.EnableFixedFunction and (not TGrayscaleImage(Img).TreatAsAlpha) then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
  if Img is TGrayscaleAlphaImage then
  begin
    { Use texture swizzle to load texture
      - 1st channel (red) -> RBG
      - 2nd channel (green) -> alpha }
    ImageSwizzle := Vector4Integer(GL_RED, GL_RED, GL_RED, GL_GREEN);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RG;
      ImageInternalFormat := GL_RG8;
    end else
    begin
      ImageFormat := GL_LUMINANCE_ALPHA;
      if GLFeatures.EnableFixedFunction then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
  if Img is TGrayscaleFloatImage then
  begin
    ImageSwizzle := Vector4Integer(GL_RED, GL_RED, GL_RED, GL_ONE);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RED;
      ImageInternalFormat := GL_R32F;
    end else
    begin
      ImageFormat := GL_LUMINANCE;
      if GLFeatures.EnableFixedFunction then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
  if Img is TGrayscaleAlphaFloatImage then
  begin
    { Use texture swizzle to load texture
      - 1st channel (red) -> RBG
      - 2nd channel (green) -> alpha }
    ImageSwizzle := Vector4Integer(GL_RED, GL_RED, GL_RED, GL_GREEN);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RG;
      ImageInternalFormat := GL_RG32F;
    end else
    begin
      ImageFormat := GL_LUMINANCE_ALPHA;
      if GLFeatures.EnableFixedFunction then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
  if Img is TRGBFloatImage then
  begin
    ImageSwizzle := Vector4Integer(GL_RED, GL_GREEN, GL_BLUE, GL_ONE);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RGB;
      ImageInternalFormat := GL_RGB32F;
    end else
    begin
      ImageFormat := GL_RGB;
      if GLFeatures.EnableFixedFunction then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
  if Img is TRGBAlphaFloatImage then
  begin
    ImageSwizzle := Vector4Integer(GL_RED, GL_GREEN, GL_BLUE, GL_ALPHA);
    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      ImageFormat := GL_RGBA;
      ImageInternalFormat := GL_RGBA32F;
    end else
    begin
      ImageFormat := GL_RGBA;
      if GLFeatures.EnableFixedFunction then
        ImageInternalFormat := Img.ColorComponentsCount
      else
        ImageInternalFormat := ImageFormat;
    end;
  end else
    raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to OpenGL', [Img.ClassName]);
end;

function ImageGLFormat(const Img: TCastleImage): TGLenum;
var
  IgnoredImageInternalFormat: TGLenum;
  IgnoredImageSwizzle: TVector4Integer;
begin
  UncompressedGLFormatAndInternalFormat(Img, Result, IgnoredImageInternalFormat, IgnoredImageSwizzle);
end;

function ImageGLInternalFormat(const Img: TEncodedImage): TGLenum;
var
  IgnoredImageFormat: TGLenum;
  IgnoredImageSwizzle: TVector4Integer;
begin
  if Img is TCastleImage then
  begin
    UncompressedGLFormatAndInternalFormat(TCastleImage(Img), IgnoredImageFormat, Result, IgnoredImageSwizzle);
  end else
  if Img is TGPUCompressedImage then
  begin
    Result := TextureCompressionToOpenGL(TGPUCompressedImage(Img).Compression);
  end else
    raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to OpenGL',
      [Img.ClassName]);
end;

function ImageGLType(const Img: TCastleImage): TGLenum;
begin
  if (Img is TRGBImage) or
     (Img is TRGBAlphaImage) or
     (Img is TGrayscaleImage) or
     (Img is TGrayscaleAlphaImage) then
    Result := GL_UNSIGNED_BYTE
  else
  if (Img is TGrayscaleFloatImage) or
     (Img is TGrayscaleAlphaFloatImage) or
     (Img is TRGBFloatImage) or
     (Img is TRGBAlphaFloatImage) then
    Result := GL_FLOAT
  else
    raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to OpenGL',
      [Img.ClassName]);
end;

function ImageGLData(const Img: TEncodedImage): {$ifdef CASTLE_WEBGL} IJSTypedArray {$else} Pointer {$endif};
{$ifdef CASTLE_WEBGL}
var
  I: TCastleImage;
  X, Y, Z, C, ImageDataCount: Integer;
  ResultUInt8: IJSUint8Array;
  ResultFloat32: IJSFloat32Array;
begin
  if Img is TCastleImage then
  begin
    I := TCastleImage(Img);
    ImageDataCount := Img.Width * Img.Height * Img.Depth * I.ColorComponentsCount;
    case ImageGLType(I) of
      GL_UNSIGNED_BYTE:
        begin
          ResultUInt8 := TJSUInt8Array.Create(ImageDataCount);

          { Unoptimized version, for reference. Copies one-by-one and
            enables to change the order of any dimension / color component,
            to match WebGL expectations.
            Not necessary right now in the end. }
          {
          for Z := 0 to Img.Depth - 1 do
            for Y := 0 to Img.Height - 1 do
              for X := 0 to Img.Width - 1 do
                for C := 0 to I.ColorComponentsCount - 1 do
                  ResultUInt8[
                    Z * Img.Width * Img.Height * I.ColorComponentsCount +
                    Y * Img.Width * I.ColorComponentsCount +
                    X * I.ColorComponentsCount +
                    C] :=
                  PByte(I.PixelPtr(X, Y, Z))[C];
          }
          ResultUInt8.CopyFromMemory(Img.RawPixels, ImageDataCount);

          Result := ResultUInt8;
        end;
      GL_FLOAT:
        begin
          ResultFloat32 := TJSFloat32Array.Create(ImageDataCount);

          { Unoptimized version, for reference.
            See above.
          }
          {
          for Z := 0 to Img.Depth - 1 do
            for Y := 0 to Img.Height - 1 do
              for X := 0 to Img.Width - 1 do
                for C := 0 to I.ColorComponentsCount - 1 do
                  ResultFloat32[
                    Z * Img.Width * Img.Height * I.ColorComponentsCount +
                    Y * Img.Width * I.ColorComponentsCount +
                    X * I.ColorComponentsCount +
                    C] :=
                  PSingle(I.PixelPtr(X, Y, Z))[C];
          }
          ResultFloat32.CopyFromMemory(Img.RawPixels, ImageDataCount * SizeOf(Single));

          Result := ResultFloat32;
        end;
      else
        raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to WebGL', [
          Img.ClassName
        ]);
    end;
  end else
    raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to WebGL', [
      Img.ClassName
    ]);
{$else}
begin
  Result := Img.RawPixels;
{$endif}
end;

procedure FreeTexture(var Tex: TGLTextureId);
begin
  if Tex <> GLObjectNone then
  begin
    TextureMemoryProfiler.Deallocate(Tex);
    glDeleteTexture(Tex);
    Tex := GLObjectNone;
  end;
end;

{ Internal utility in this unit, sets read buffer on desktop OpenGL. }
procedure SetReadBuffer(const Buffer: TGLEnum);
begin
  {$ifndef OpenGLES}
  glReadBuffer(Buffer);
  {$endif}
end;

{ Internal utility in this unit, sets draw buffer on desktop OpenGL. }
procedure SetDrawBuffer(const Buffer: TGLEnum);
begin
  {$ifndef OpenGLES}
  glDrawBuffer(Buffer);
  {$endif}
end;

{ Anisotropy ----------------------------------------------------------------- }

procedure TexParameterMaxAnisotropy(const target: TGLenum; const Anisotropy: TGLfloat);
begin
  if GLFeatures.TextureFilterAnisotropic then
    glTexParameterf(Target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      Min(GLFeatures.MaxTextureMaxAnisotropy, Anisotropy));
end;

{$endif read_implementation}
