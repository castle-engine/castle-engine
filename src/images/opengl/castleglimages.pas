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

{ Using images in OpenGL (as textures and as normal images).

  For non-OpenGL image management, see CastleImages and CastleTextureImages units.
  They contain functions and classes to load, save and process
  images.

  This unit has functions and classes to:

  @unorderedList(
    @item(Load images as OpenGL textures.
      You usually do not use these directly, instead TCastleScene
      automatically uses these to load and render textures as part of 3D models.

      A lot of utilities included: for 2D textures (see LoadGLTexture),
      cube maps (see glTextureCubeMap), 3D textures (see glTextureImage3D).
      These functions wrap OpenGL calls like glTexImage2D to handle
      our images (TEncodedImage (and descendant TCastleImage), TCompositeImage),
      and to automatically set texture parameters, mipmaps and such.)

    @item(Load and draw images in 2D.
      This is useful to implement various 2D controls.
      See TGLImage class and friends.)

    @item(Save the current OpenGL screen contents to our TCastleImage.
      You usually use this through TCastleWindowCustom.SaveScreen
      or TCastleControl.SaveScreen,
      based on SaveScreen_NoFlush in this unit.)

    @item(Render to texture, see TGLRenderToTexture class.
      This is our abstraction over OpenGL framebuffer (or glCopyTexSubImage
      for ancient GPUs).)
  )

  This unit hides from your some details about OpenGL images handling.
  For example, you don't have to worry about "pixel store alignment",
  we handle it here internally when transferring images between memory and GPU.
  You also don't have to worry about texture sizes being power of 2,
  or about maximum texture sizes --- we will resize textures if necessary.

  Routines in this unit that take TCastleImage or TEncodedImage parameter
  are limited to TextureImageClassesAll (for routines dealing with textures)
  or PixelsImageClasses (for routines dealing with images drawn on 2D screen).
}
unit CastleGLImages;

{$I castleconf.inc}

interface

uses SysUtils, FGL, CastleGL,
  CastleImages, CastleVectors, CastleGLUtils, CastleTimeUtils,
  CastleVideos, CastleCompositeImage, CastleRectangles, CastleGLShaders, CastleColors;

const
  PixelsImageClasses: array [0..3] of TEncodedImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

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

type
  TGLTextureId = TGLuint;

  ETextureLoadError = class(Exception);
  EImageClassNotSupportedForOpenGL = class(ETextureLoadError);
  ECannotLoadCompressedTexture = class(ETextureLoadError);
  EInvalidImageForOpenGLTexture = class(ETextureLoadError);

{ If Tex <> 0 then it does glDeleteTextures on Tex and sets Tex to 0.
  In other words, this is a simple wrapper over glDeleteTextures that
  @orderedList(
    @item checks if Tex really should be deleted
    @item sets Tex to 0 to not free it once again
  ) }
procedure glFreeTexture(var Tex: TGLTextureId);

{ Call glTexParameterf to set GL_TEXTURE_MAX_ANISOTROPY_EXT on given texture
  target.

  Takes care to check for appropriate OpenGL extension (if not present,
  does nothing), and to query OpenGL limit for Anisotropy (eventually
  clamping provided Anisotropy down). }
procedure TexParameterMaxAnisotropy(const target: TGLenum; const Anisotropy: TGLfloat);

{$define read_interface}
{$I castleglimages_filter.inc}
{$I castleglimages_wrap.inc}
{$I castleglimages_tglimage.inc}
{$I castleglimages_load_2d.inc}
{$I castleglimages_load_3d.inc}
{$I castleglimages_load_cubemap.inc}
{$I castleglimages_savescreen.inc}
{$I castleglimages_rendertotexture.inc}
{$I castleglimages_texturememoryprofiler.inc}
{$I castleglimages_video.inc}
{$I castleglimages_sprite.inc}
{$I castleglimages_packing.inc}
{$undef read_interface}

implementation

uses Math,
  CastleUtils, CastleLog, CastleGLVersion, CastleTextureImages,
  CastleApplicationProperties, CastleStringUtils, CastleURIUtils;

function ImageGLFormat(const Img: TCastleImage): TGLenum;
begin
  if Img is TRGBImage then
    Result := GL_RGB else
  if Img is TRGBAlphaImage then
    Result := GL_RGBA else
  if Img is TGrayscaleImage then
  begin
    if TGrayscaleImage(Img).TreatAsAlpha then
      Result := GL_ALPHA else
      Result := GL_LUMINANCE;
  end else
  if Img is TGrayscaleAlphaImage then
    Result := GL_LUMINANCE_ALPHA else
  if Img is TRGBFloatImage then
    Result := GL_RGB else
    raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to OpenGL', [Img.ClassName]);
end;

function ImageGLInternalFormat(const Img: TEncodedImage): TGLenum;
const
  { From https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc.txt }
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG  = $8C00;
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG  = $8C01;
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;

  { https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc2.txt }
  GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG = $9138;
  GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG = $9137;

  { https://www.khronos.org/registry/gles/extensions/AMD/AMD_compressed_ATC_texture.txt }
  GL_ATC_RGB_AMD                     = $8C92;
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD     = $8C93;
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = $87EE;

  { https://www.khronos.org/registry/gles/extensions/OES/OES_compressed_ETC1_RGB8_texture.txt }
  GL_ETC1_RGB8_OES = $8D64;

  {$ifdef OpenGLES}
  { Copied from desktop OpenGL.
    Matching
    https://www.khronos.org/registry/gles/extensions/NV/NV_texture_compression_s3tc.txt }
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;
  {$endif}

begin
  if Img is TCastleImage then
  begin
    if (Img is TGrayscaleImage) and TGrayscaleImage(Img).TreatAsAlpha then
      Result := GL_ALPHA else
      Result := {$ifdef OpenGLES} ImageGLFormat(TCastleImage(Img))
                {$else} TCastleImage(Img).ColorComponentsCount
                {$endif};
  end else
  if Img is TGPUCompressedImage then
  begin
    case TGPUCompressedImage(Img).Compression of
      tcDxt1_RGB        : Result := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
      tcDxt1_RGBA       : Result := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
      tcDxt3            : Result := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
      tcDxt5            : Result := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;

      tcPvrtc1_4bpp_RGB : Result := GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG;
      tcPvrtc1_2bpp_RGB : Result := GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG;
      tcPvrtc1_4bpp_RGBA: Result := GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;
      tcPvrtc1_2bpp_RGBA: Result := GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG;
      tcPvrtc2_4bpp     : Result := GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG;
      tcPvrtc2_2bpp     : Result := GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG;

      tcATITC_RGB                   : Result := GL_ATC_RGB_AMD;
      tcATITC_RGBA_InterpolatedAlpha: Result := GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD;
      tcATITC_RGBA_ExplicitAlpha    : Result := GL_ATC_RGBA_EXPLICIT_ALPHA_AMD;

      tcETC1            : Result := GL_ETC1_RGB8_OES;
      else raise EImageClassNotSupportedForOpenGL.Create('TGPUCompressedImage.Compression not supported by OpenGL');
    end;
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
    Result := GL_UNSIGNED_BYTE else
  if Img is TRGBFloatImage then
    Result := GL_FLOAT else
    raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to OpenGL',
      [Img.ClassName]);
end;

procedure glFreeTexture(var Tex: TGLTextureId);
begin
  if Tex <> 0 then
  begin
    TextureMemoryProfiler.Deallocate(Tex);
    glDeleteTextures(1, @Tex);
    Tex := 0;
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
  if GLFeatures.EXT_texture_filter_anisotropic then
    glTexParameterf(Target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      Min(GLFeatures.MaxTextureMaxAnisotropyEXT, Anisotropy));
end;

{ includes ------------------------------------------------------------------- }

{$define read_implementation}
{$I castleglimages_filter.inc}
{$I castleglimages_wrap.inc}
{$I castleglimages_tglimage.inc}
{$I castleglimages_load_2d.inc}
{$I castleglimages_load_3d.inc}
{$I castleglimages_load_cubemap.inc}
{$I castleglimages_savescreen.inc}
{$I castleglimages_rendertotexture.inc}
{$I castleglimages_texturememoryprofiler.inc}
{$I castleglimages_video.inc}
{$I castleglimages_sprite.inc}
{$I castleglimages_packing.inc}
{$undef read_implementation}

{ initialization / finalization ---------------------------------------------- }

procedure ContextClose;
begin
  TextureMemoryProfiler.CheckLeaks;
  TGLImageCore.StaticGLContextClose;
end;

initialization
  ApplicationProperties.OnGLContextClose.Add(@ContextClose);
finalization
  FreeAndNil(BoundFboStack);
  FreeAndNil(FTextureMemoryProfiler);
end.
