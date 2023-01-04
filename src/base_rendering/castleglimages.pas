{
  Copyright 2001-2023 Michalis Kamburelis.

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
      See TDrawableImage class and friends.)

    @item(Save the current OpenGL screen contents to our TCastleImage.
      You usually use this through TCastleWindow.SaveScreen
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

uses SysUtils, Generics.Collections, Classes,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleImages, CastleVectors, CastleGLUtils, CastleTimeUtils,
  CastleTextureImages, CastleVideos, CastleInternalCompositeImage, CastleRectangles,
  CastleGLShaders, CastleColors, CastleUtils, CastleRenderOptions;

{$define read_interface}

  {$define read_interface_type}
    type
    {$I castleglimages_miscellaneous.inc}
    {$I castleglimages_filter.inc}
    {$I castleglimages_wrap.inc}
    {$I castleglimages_drawableimage.inc}
    {$I castleglimages_drawableimagecache.inc}
    {$I castleglimages_persistentimage.inc}
    {$I castleglimages_load_2d.inc}
    {$I castleglimages_load_3d.inc}
    {$I castleglimages_load_cubemap.inc}
    {$I castleglimages_savescreen.inc}
    {$I castleglimages_rendertotexture.inc}
    {$I castleglimages_texturememoryprofiler.inc}
    {$I castleglimages_video.inc}
    {$I castleglimages_sprite.inc}
    {$I castleglimages_packing.inc}
  {$undef read_interface_type}

  {$define read_interface_const}
    {$I castleglimages_miscellaneous.inc}
    {$I castleglimages_filter.inc}
    {$I castleglimages_wrap.inc}
    {$I castleglimages_drawableimage.inc}
    {$I castleglimages_drawableimagecache.inc}
    {$I castleglimages_persistentimage.inc}
    {$I castleglimages_load_2d.inc}
    {$I castleglimages_load_3d.inc}
    {$I castleglimages_load_cubemap.inc}
    {$I castleglimages_savescreen.inc}
    {$I castleglimages_rendertotexture.inc}
    {$I castleglimages_texturememoryprofiler.inc}
    {$I castleglimages_video.inc}
    {$I castleglimages_sprite.inc}
    {$I castleglimages_packing.inc}
  {$undef read_interface_const}

  {$define read_interface_func}
    {$I castleglimages_miscellaneous.inc}
    {$I castleglimages_filter.inc}
    {$I castleglimages_wrap.inc}
    {$I castleglimages_drawableimage.inc}
    {$I castleglimages_drawableimagecache.inc}
    {$I castleglimages_persistentimage.inc}
    {$I castleglimages_load_2d.inc}
    {$I castleglimages_load_3d.inc}
    {$I castleglimages_load_cubemap.inc}
    {$I castleglimages_savescreen.inc}
    {$I castleglimages_rendertotexture.inc}
    {$I castleglimages_texturememoryprofiler.inc}
    {$I castleglimages_video.inc}
    {$I castleglimages_sprite.inc}
    {$I castleglimages_packing.inc}
  {$undef read_interface_func}

{$undef read_interface}

implementation

uses Math, Generics.Defaults,
  CastleLog, CastleGLVersion,
  CastleApplicationProperties, CastleStringUtils, CastleURIUtils,
  CastleRenderContext;

{$define read_implementation}

{$I castleglimages_miscellaneous.inc}
{$I castleglimages_filter.inc}
{$I castleglimages_wrap.inc}
{$I castleglimages_drawableimage.inc}
{$I castleglimages_drawableimagecache.inc}
{$I castleglimages_persistentimage.inc}
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

procedure ContextOpen;
begin
  DecompressTexture := @GLDecompressTexture;
end;

procedure ContextClose;
begin
  // Use @ on both sides to compare on Delphi, https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Procedural_Types_(Delphi)
  if {$ifndef FPC}@{$endif} DecompressTexture = @GLDecompressTexture then
    DecompressTexture := nil;
  TextureMemoryProfiler.CheckLeaks;
  TDrawableImage.StaticGLContextClose;
end;

initialization
  ApplicationProperties.OnGLContextOpen.Add(@ContextOpen);
  ApplicationProperties.OnGLContextClose.Add(@ContextClose);
finalization
  FreeAndNil(BoundFboStack);
  FreeAndNil(FTextureMemoryProfiler);
  FreeAndNil(FDrawableImageCache);
end.
