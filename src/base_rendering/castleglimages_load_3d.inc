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

{ Part of CastleGLImages unit: 3D textures sizing and loading. }

{$ifdef read_interface}

{$ifdef read_interface_func}
{ Comfortably load a 3D texture.
  Think about this as doing glTexImage3D(...) for you.
  It also sets texture minification, magnification filters and creates
  mipmaps if necessary.

  It checks OpenGL 3D texture size requirements, and throws exceptions
  if not satisfied.

  It takes care about OpenGL unpack parameters. Just don't worry about it.

  If Filter uses mipmaps, then all mipmap levels will be loaded.

  @orderedList(
    @item(
      As a first try, if CompositeForMipmaps is non-nil
      and has mipmaps (CompositeForMipmaps.Mipmaps), we will load these mipmaps.
      CompositeForMipmaps must be a 3D texture (CompositeType = ctVolume).)

    @item(Otherwise, we'll generate mipmaps.

      GenerateMipmap functionality will be required for this.
      When it is not available on this OpenGL implementation,
      we will change minification filter to simple linear and make WritelnWarning.
      So usually you just don't have to worry about this.)
  )

  Pass TextureIdForProfiler only for profiling purposes (for TextureMemoryProfiler).
  This procedure assumes that the texture is already bound.

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason,
    for example it's size is not correct for OpenGL 3D texture (we cannot
    automatically resize 3D textures, at least for now).
    Or it's compressed (although we support here TEncodedImage,
    OpenGL doesn't have any 3D texture compression available.))

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)
}
procedure glTextureImage3D(const TextureIdForProfiler: TGLTextureId;
  const Image: TEncodedImage;
  Filter: TTextureFilter; CompositeForMipmaps: TCompositeImage);
{$endif read_interface_func}

{$endif read_interface}

{$ifdef read_implementation}

function IsTexture3DSized(const Size: Cardinal): boolean; overload;
begin
  Result :=
    (not GLFeatures.Texture3D) or
    (
      IsPowerOf2(Size) and
      (Size > 0) and
      (Size <= GLFeatures.MaxTexture3DSize)
    );
end;

function IsTexture3DSized(const R: TCastleImage): boolean; overload;
begin
  if GLFeatures.Texture3D then
  begin
    Result :=
      IsPowerOf2(R.Width ) and (R.Width  > 0) and (R.Width  <= GLFeatures.MaxTexture3DSize) and
      IsPowerOf2(R.Height) and (R.Height > 0) and (R.Height <= GLFeatures.MaxTexture3DSize) and
      IsPowerOf2(R.Depth ) and (R.Depth  > 0) and (R.Depth  <= GLFeatures.MaxTexture3DSize);
  end else
    Result := true;
end;

procedure glTextureImage3D(const TextureIdForProfiler: TGLTextureId;
  const Image: TEncodedImage;
  Filter: TTextureFilter; CompositeForMipmaps: TCompositeImage);
var
  ImageInternalFormat: TGLuint;
  ImageFormat: TGLuint;
  LoadedSize: Int64;

  { Calls glTexImage3D for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    Takes care of Image size --- makes sure that image has the right size
    (power of 2, within OpenGL required sizes). }
  procedure glTexImage3DImage(Image: TCastleImage; Level: TGLuint);

    { This is like glTexImage3DImage, but it doesn't take care
      of Image size. }
    procedure Core(Image: TCastleImage);
    begin
      BeforeUnpackImage(Image);
      try
        if GLFeatures.Texture3D then
          glTexImage3D(GL_TEXTURE_3D, Level, ImageInternalFormat,
            Image.Width, Image.Height, Image.Depth, 0, ImageFormat, ImageGLType(Image),
            Image.RawPixels);
        LoadedSize := LoadedSize + Image.Size;
      finally AfterUnpackImage(Image) end;
    end;

  begin
    if not IsTexture3DSized(Image) then
      raise ETextureLoadError.CreateFmt('Image is not properly sized for a 3D texture, sizes must be a power-of-two and <= GL_MAX_3D_TEXTURE_SIZE (%d). Sizes are: %d x %d x %d',
        [ GLFeatures.MaxTexture3DSize, Image.Width, Image.Height, Image.Depth ]);

    Core(Image);
  end;

  { Check should we load mipmaps from Composite. Load them, if yes. }
  function LoadMipmapsFromComposite(Composite: TCompositeImage): boolean;
  var
    I: Integer;
  begin
    Result := (Composite <> nil) and Composite.Mipmaps;
    if Result and (Composite.CompositeType <> ctVolume) then
    begin
      WritelnWarning('Texture', 'Composite image contains mipmaps, but not for 3D (volume) texture');
      Result := false;
    end;

    if Result and (not GLFeatures.TextureMaxLevel) then
    begin
      WritelnWarning('Texture', 'Cannot load Composite image containing mipmaps, because OpenGL(ES) does not support GL_TEXTURE_MAX_LEVEL');
      Result := false;
    end;

    if Result then
    begin
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAX_LEVEL, Composite.MipmapsCount - 1);
      for I := 1 to Composite.MipmapsCount - 1 do
        if Composite.Images[I] is TCastleImage then
          glTexImage3DImage(TCastleImage(Composite.Images[I]), I) else
          raise ETextureLoadError.CreateFmt('Image class %s cannot be loaded to OpenGL 3D texture. OpenGL doesn''t allow any 3D texture compression formats',
            [Image.ClassName]);
    end;
  end;

var
  ImageSwizzle: TVector4Integer;
begin
  if not (Image is TCastleImage) then
    raise ETextureLoadError.CreateFmt('Image class %s cannot be loaded to OpenGL 3D texture. OpenGL doesn''t allow any 3D texture compression formats',
      [Image.ClassName]);

  UncompressedGLFormatAndInternalFormat(TCastleImage(Image), ImageFormat, ImageInternalFormat, ImageSwizzle);
  LoadedSize := 0;

  glTexImage3DImage(TCastleImage(Image), 0);

  if not GLFeatures.Texture3D then
    raise Exception.Create('3D textures not supported by OpenGL(ES)');

  if GLFeatures.TextureModernFormatsAndSwizzle then
    glTexParameteriv(GL_TEXTURE_3D, GL_TEXTURE_SWIZZLE_RGBA, @ImageSwizzle);

  if Filter.NeedsMipmaps then
  begin
    if not LoadMipmapsFromComposite(CompositeForMipmaps) then
    try
      GenerateMipmap(GL_TEXTURE_3D);
      LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(Image);
    except
      on E: EGenerateMipmapNotAvailable do
      begin
        Filter.Minification := minLinear;
        WritelnWarning('Texture', 'Creating mipmaps for 3D textures requires GenerateMipmap functionality, will fallback to GL_LINEAR minification: ' + E.Message);
      end;
    end;
  end;

  TextureMemoryProfiler.Allocate(TextureIdForProfiler, Image.URL, Image.ClassName,
    LoadedSize, Filter.NeedsMipmaps, Image.Width, Image.Height, Image.Depth);

  SetTextureFilter(GL_TEXTURE_3D, Filter);
end;

{$endif read_implementation}
