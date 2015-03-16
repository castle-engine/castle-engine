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
      our images (TEncodedImage (and descendant TCastleImage), TDDSImage),
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
  CastleImages, CastleVectors, CastleGLUtils,
  CastleVideos, CastleDDS, CastleRectangles, CastleGLShaders, CastleColors;

const
  PixelsImageClasses: array [0..3] of TCastleImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

{ Return appropriate OpenGL format and type constants
  for given TCastleImage descendant. If you will pass here Img
  that is not a descendant of one of TextureImageClassesAll
  or PixelsImageClasses, they will raise EImageClassNotSupportedForOpenGL.

  ImageGLInternalFormat works with TS3TCImage classes also, returning
  appropriate GL_COMPRESSED_*_S3TC_*_EXT, suitable for glCompressedTexImage2D.

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
  ECannotLoadS3TCTexture = class(ETextureLoadError);
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
{$I castleglimages_mipmaps.inc}
{$I castleglimages_tglimage.inc}
{$I castleglimages_load_2d.inc}
{$I castleglimages_load_3d.inc}
{$I castleglimages_load_cubemap.inc}
{$I castleglimages_savescreen.inc}
{$I castleglimages_rendertotexture.inc}
{$I castleglimages_texturememoryprofiler.inc}
{$I castleglimages_video.inc}
{$undef read_interface}

implementation

uses CastleUtils, CastleLog, CastleGLVersion, CastleWarnings, CastleTextureImages,
  CastleUIControls, CastleStringUtils;

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
begin
  if Img is TCastleImage then
  begin
    if (Img is TGrayscaleImage) and TGrayscaleImage(Img).TreatAsAlpha then
      Result := GL_ALPHA else
      Result := {$ifdef OpenGLES} ImageGLFormat(TCastleImage(Img))
                {$else} TCastleImage(Img).ColorComponentsCount
                {$endif};
  end else
  if Img is TS3TCImage then
  begin
    {$ifdef OpenGLES}
    raise EImageClassNotSupportedForOpenGL.Create('S3TC compression not supported by OpenGL ES');
    {$else}
    case TS3TCImage(Img).Compression of
      s3tcDxt1_RGB : Result := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
      s3tcDxt1_RGBA: Result := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
      s3tcDxt3     : Result := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
      s3tcDxt5     : Result := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
      else raise EImageClassNotSupportedForOpenGL.Create('TS3TCImage.Compression not supported by OpenGL');
    end;
    {$endif}
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
{$I castleglimages_mipmaps.inc}
{$I castleglimages_tglimage.inc}
{$I castleglimages_load_2d.inc}
{$I castleglimages_load_3d.inc}
{$I castleglimages_load_cubemap.inc}
{$I castleglimages_savescreen.inc}
{$I castleglimages_rendertotexture.inc}
{$I castleglimages_texturememoryprofiler.inc}
{$I castleglimages_video.inc}
{$undef read_implementation}

{ initialization / finalization ---------------------------------------------- }

procedure ContextClose;
{$ifdef GLImageUseShaders}
var
  AlphaTestShader: boolean;
  ColorTreatment: TGLImage.TColorTreatment;
{$endif}
begin
  TextureMemoryProfiler.CheckLeaks;

  glFreeBuffer(TGLImage.PointVbo);
  {$ifdef GLImageUseShaders}
  for AlphaTestShader in boolean do
    for ColorTreatment in TGLImage.TColorTreatment do
      FreeAndNil(TGLImage.GLSLProgram[AlphaTestShader, ColorTreatment]);
  {$endif}
end;

initialization
  OnGLContextClose.Add(@ContextClose);
finalization
  FreeAndNil(BoundFboStack);
  FreeAndNil(FTextureMemoryProfiler);
end.
