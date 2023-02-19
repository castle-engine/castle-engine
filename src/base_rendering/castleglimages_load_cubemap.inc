{%MainUnit castleglimages.pas}
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

{ Part of CastleGLImages unit: cubemaps sizing and loading. }

{$ifdef read_interface}

{$ifdef read_interface_func}
function IsCubeMapTextureSized(const Size: Cardinal): boolean; overload;
function ResizeToCubeMapTextureSize(const Size: Cardinal): Cardinal; overload;

{ Comfortably load all six cube map texture images.
  Think about this as doing glTexImage2D(Side, ...) for each cube side.
  It takes care of (almost?) everything you need to prepare OpenGL cube map
  texture.

  It automatically takes care to adjust the texture size to
  appropriate size, honoring the "power of two" requirement and
  the GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB limit of OpenGL. So texture image
  may be resized (preferably up) internally before loading.
  Although, if texture is compressed, we cannot resize it
  --- so @link(ECannotLoadCompressedTexture) will be raised if texture is not appropriate
  size.

  It takes care about OpenGL unpack parameters. Just don't worry about it.

  Pass TextureIdForProfiler only for profiling purposes (for TextureMemoryProfiler).
  This procedure assumes that the texture is already bound.

  If mipmaps are requested:

  @orderedList(
    @item(First of all, if CompositeForMipmaps is non-nil and has mipmaps defined,
      we will load them from this CompositeForMipmaps image.
      CompositeForMipmaps must have CompositeType = ctCubeMap.)

    @item(Otherwise, we'll try to generate images using OpenGL GenerateMipmap.)

    @item(As a last resort, if GenerateMipmap is not available,
      we will fallback to generating mipmaps on CPU.)
  )

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason.
    This includes @link(ECannotLoadCompressedTexture) if the compressed
    texture cannot be
    loaded for whatever reason (not availble appropriate extension,
    not correct texture size, mipmaps requested and
    CompositeForMipmaps/glGenerateMipmap not available).
    This includes EInvalidImageForOpenGLTexture if Image class is invalid
    for an OpenGL texture.)

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)
}
procedure glTextureCubeMap(const TextureIdForProfiler: TGLTextureId;
  const PositiveX, NegativeX,
        PositiveY, NegativeY,
        PositiveZ, NegativeZ: TEncodedImage;
  const CompositeForMipmaps: TCompositeImage;
  const Mipmaps: boolean;
  out DisableMipmaps: Boolean);
{$endif read_interface_func}

{$endif read_interface}

{$ifdef read_implementation}

{ ----------------------------------------------------------------------------
  Adjusting image size for cube map texture. }

function IsCubeMapTextureSized(const Size: Cardinal): boolean; overload;
begin
  Result :=
    (not GLFeatures.TextureCubeMap) or
    (
      IsPowerOf2(Size) and
      (Size > 0) and
      (Size <= GLFeatures.MaxCubeMapTextureSize)
    );
end;

function IsCubeMapTextureSized(const R: TEncodedImage): boolean; overload;
begin
  Result :=
    (not GLFeatures.TextureCubeMap) or
    (
      (r.Width = r.Height) { must be square } and
      IsPowerOf2(r.Width) and
      (r.Width > 0) and
      (r.Width <= GLFeatures.MaxCubeMapTextureSize)
    );
end;

function ResizeToCubeMapTextureSize(const r: TCastleImage): TCastleImage; overload; forward;

procedure ResizeForCubeMapTextureSize(var r: TCastleImage);
var
  newR: TCastleImage;
begin
  if not IsCubeMapTextureSized(r) then
  begin
    newR := ResizeToCubeMapTextureSize(r);
    FreeAndNil(r);
    r := newR;
  end;
end;

function ResizeToCubeMapTextureSize(const Size: Cardinal): Cardinal; overload;
begin
  Result := Size;
  if GLFeatures.TextureCubeMap then
  begin
    if Size <= 0 then
      Result := 1
    else
    if Size > GLFeatures.MaxCubeMapTextureSize then
      Result := GLFeatures.MaxCubeMapTextureSize
    else
    if IsPowerOf2(Size) then
      Result := Size
    else
      { Result jakie otrzymamy below jest na pewno < MaxTexSize bo
        skoro Size <= MaxTexSize i not IsPowerOf2(Size) to Size < MaxTexSize
        a MaxTexSize samo jest potega dwojki. }
      Result := 1 shl (Biggest2Exponent(Size) + 1);
  end;
end;

function ResizeToCubeMapTextureSize(const r: TCastleImage): TCastleImage; overload;
var
  Size: Cardinal;
begin
  if GLFeatures.TextureCubeMap then
  begin
    Size := Max(r.Width, r.Height);
    Size := ResizeToCubeMapTextureSize(Size);

    WritelnLog('Texture loading', Format('Resizing image for cube map texture from (%d, %d) to (%d, %d)',
      [R.Width, R.Height, Size, Size]));

    result := r.MakeResized(Size, Size, riBilinear);
  end else
    result := r.MakeCopy;
end;

{ Cube map texture loading --------------------------------------------------- }

{ Comfortably load a single image for one cube map texture side.

  This is pretty much like glTexImages2DForCubeMap,
  except it operates only on one side of the cube.
  Target should be one of the six cube map texture targets:
  GL_TEXTURE_CUBE_MAP_POSITIVE/NEGATIVE_X/Y/Z_ARB.

  Also, this cannot load mipmaps from Composite or use GenerateMipmap
  (GenerateMipmap call must be done for whole cube map texture target).
  So this can create mipmaps only by CastleBuild2DMipmaps. It will also
  fail with @link(ECannotLoadCompressedTexture) if mipmaps will be requested ---
  we cannot generate mipmaps for compressed textures here. If you want to use
  more modern GenerateMipmap, you should use higher-level
  glTexImages2DForCubeMap (takes all six images), or pass Mipmaps = @false
  and do it yourself.

  Level must be 0, unless you provide mipmaps.

  WasCompressed will be set to @true if this is compressed
  (otherwise we leave it unchanged).

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason.
    This includes @link(ECannotLoadCompressedTexture) if the compressed texture cannot be
    loaded for whatever reason (not availble appropriate extension,
    not correct texture size, mipmaps requested).
    This includes EInvalidImageForOpenGLTexture if Image class is invalid
    for an OpenGL texture.) }
procedure glTextureCubeMapSide(
  const Target: TGLenum; const Image: TEncodedImage; const Level: TGLuint; const Mipmaps: boolean;
  var LoadedSize: Int64; var WasCompressed: Boolean);

  { Load Image through glCompressedTexImage2D[ARB].
    This checks existence of OpenGL extensions for compressed texture,
    and checks Image sizes.
    It also takes care of pixel packing, although actually nothing needs
    be done about it when using compressed textures.

    @raises(ECannotLoadCompressedTexture If texture size is bad or OpenGL
      extensions are missing or mipmaps were required.) }
  procedure LoadCompressed(const Image: TGPUCompressedImage);
  begin
    if not (Image.Compression in GLFeatures.TextureCompression) then
      raise ECannotLoadCompressedTexture.CreateFmt('Cannot load textures compressed using %s, your OpenGL/OpenGLES does not support the necessary extensions',
        [TextureCompressionInfo[Image.Compression].Name]);

    if not IsCubeMapTextureSized(Image) then
      raise ECannotLoadCompressedTexture.CreateFmt('Cannot load compressed textures: texture size is %d x %d, it''s not correct for a cube map, and we cannot resize compressed textures',
        [Image.Width, Image.Height]);

    if Mipmaps then
      raise ECannotLoadCompressedTexture.Create('Cannot create mipmaps on CPU for compressed images');

    { Pixel packing parameters (stuff changed by Before/AfterUnpackImage)
      doesn't affect loading compressed textures, as far as I understand.
      So no need to call it. }
    glCompressedTexImage2D(Target, Level, ImageGLInternalFormat(Image),
      Image.Width, Image.Height, 0, Image.Size, Image.RawPixels);
    LoadedSize := LoadedSize + Image.Size;
    WasCompressed := true;
  end;

  procedure LoadMipmapped(const Image: TCastleImage);
  begin
    { This should only be called by glTextureCubeMap when mipmaps are needed
      but GenerateMipmap was not available.
      We have no choice but to use old CastleBuild2DMipmaps.

      Note: SGIS_generate_mipmap doesn't work on cube map texture,
      testing on ATI Mobility Radeon X1600 (fglrx, Linux, on Mac Book Pro). }

    CastleBuild2DMipmaps(Target, Image, LoadedSize);
  end;

  { Calls glTexImage2D for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    Takes care of Image size --- makes sure that image has the right size
    (power of 2, within OpenGL required sizes). }
  procedure LoadNormal(const Image: TCastleImage);
  var
    ImgGood: TCastleImage;
  begin
    if IsCubeMapTextureSized(Image) then
      CastleTexImage2D(Target, Level, Image, LoadedSize)
    else
    begin
      ImgGood := ResizeToCubeMapTextureSize(Image);
      try
        CastleTexImage2D(Target, Level, ImgGood, LoadedSize);
      finally ImgGood.Free end;
    end;
  end;

begin
  if Image is TGPUCompressedImage then
    LoadCompressed(TGPUCompressedImage(Image))
  else
  if Image Is TCastleImage then
  begin
    if Mipmaps then
      LoadMipmapped(TCastleImage(Image))
    else
      LoadNormal(TCastleImage(Image));
  end else
    raise EInvalidImageForOpenGLTexture.CreateFmt('Cannot load to OpenGL texture image class %s', [Image.ClassName]);
end;

procedure glTextureCubeMap(const TextureIdForProfiler: TGLTextureId;
  const PositiveX, NegativeX,
        PositiveY, NegativeY,
        PositiveZ, NegativeZ: TEncodedImage;
  const CompositeForMipmaps: TCompositeImage;
  const Mipmaps: boolean;
  out DisableMipmaps: Boolean);
var
  LoadedSize: Int64;

  { Check should we load mipmaps from Composite. }
  function HasMipmapsFromComposite(Composite: TCompositeImage): boolean;
  begin
    Result := (Composite <> nil) and Composite.Mipmaps;
    if not Result then Exit;

    if Composite.CompositeType <> ctCubeMap then
    begin
      WritelnWarning('Texture', 'Composite (DDS, KTX...) image contains mipmaps, but not for CubeMap texture');
      Exit(false);
    end;

    if not GLFeatures.TextureMaxLevel then
    begin
      WritelnWarning('Texture', 'Cannot load composite (DDS, KTX...) image containing mipmaps, because OpenGL(ES) does not support GL_TEXTURE_MAX_LEVEL');
      Exit(false);
    end;
  end;

  { Load mipmaps from Composite. Assume HasMipmapsFromComposite was true. }
  procedure LoadMipmapsFromComposite(const Composite: TCompositeImage);

    procedure LoadMipmapsFromCompositeSide(const GLSide: TGLenum;
      const CompositeSide: TCubeMapSide);
    var
      I: Integer;
      IgnoredWasCompressed: Boolean;
    begin
      IgnoredWasCompressed := false;
      for I := 1 to Composite.MipmapsCount - 1 do
        glTextureCubeMapSide(GLSide,
          Composite.CubeMapImage(CompositeSide, I), I, false, LoadedSize, IgnoredWasCompressed);
    end;

  begin
    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAX_LEVEL, Composite.MipmapsCount - 1);
    LoadMipmapsFromCompositeSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X, csPositiveX);
    LoadMipmapsFromCompositeSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, csNegativeX);
    LoadMipmapsFromCompositeSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, csPositiveY);
    LoadMipmapsFromCompositeSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, csNegativeY);
    LoadMipmapsFromCompositeSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, csPositiveZ);
    LoadMipmapsFromCompositeSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, csNegativeZ);
  end;

var
  WasCompressed: Boolean;
begin
  LoadedSize := 0;
  WasCompressed := false;
  DisableMipmaps := false;

  if Mipmaps and (HasMipmapsFromComposite(CompositeForMipmaps) or HasGenerateMipmap) then
  begin
    { Load six cube faces without mipmaps, then generate them all
      in one go with GenerateMipmap. }
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X, PositiveX, 0, false, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, NegativeX, 0, false, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, PositiveY, 0, false, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, NegativeY, 0, false, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, PositiveZ, 0, false, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, NegativeZ, 0, false, LoadedSize, WasCompressed);
    if HasMipmapsFromComposite(CompositeForMipmaps) then
      LoadMipmapsFromComposite(CompositeForMipmaps) else
    begin
      if WasCompressed then
      begin
        { We cannot generate mipmaps when sides used GPU compression.
          See https://docs.gl/es2/glGenerateMipmap ,
          "GL_INVALID_OPERATION is generated if the zero level array is stored in a compressed internal format."
        }
        WritelnWarning('Cubemap is loaded from a GPU-compressed texture, without mipmaps. Disabling mipmaps usage, as we cannot use GenerateMipmap on GPU-compressed cube map.');
        DisableMipmaps := true;
      end else
      begin
        GenerateMipmap(GL_TEXTURE_CUBE_MAP);
        { TODO: this is cubemap mipmaps size assuming that provided
          cubemap images were already power of 2, and within good dimensions. }
        LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(PositiveX);
        LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(NegativeX);
        LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(PositiveY);
        LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(NegativeY);
        LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(PositiveZ);
        LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(NegativeZ);
      end;
    end;
  end else
  begin
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X, PositiveX, 0, Mipmaps, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, NegativeX, 0, Mipmaps, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, PositiveY, 0, Mipmaps, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, NegativeY, 0, Mipmaps, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, PositiveZ, 0, Mipmaps, LoadedSize, WasCompressed);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, NegativeZ, 0, Mipmaps, LoadedSize, WasCompressed);
  end;

  TextureMemoryProfiler.Allocate(TextureIdForProfiler, PositiveX.URL,
    'cubemap-' + PositiveX.ClassName, LoadedSize, Mipmaps,
    PositiveX.Width, PositiveX.Height, 1);
end;

{$endif read_implementation}
