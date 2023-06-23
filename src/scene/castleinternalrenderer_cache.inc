{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Types used by TRendererCache. }

{$ifdef read_interface}
//type
  { Cache to share prepared renderer (OpenGL) resources,
    like textures and shaders. This allows to share OpenGL resources
    between multiple X3D nodes, in turn between multiple TCastleScenes.
    This may save a lot of OpenGL memory (and time to upload textures to GPU)
    when the same texture is referenced from various models,
    or the same shader is used by various models.

    This cache is used by TRenderer, but the cache has no connections to
    any particular TRenderer.
    So the cache can have a longer lifetime than TRenderer.
    In practice, we use now a single global instance of this cache
    in @link(RendererCache).
    In constrast, there can be many TRenderer instances (one per viewport),
    and each TCastleScene and TGLShape during it's life may switch
    between viewports (so also between renderers).

    The cache can only be used in OpenGL contexts sharing resources.
    In CGE, all OpenGL contexts (created by TCastleWindow and TCastleControl)
    share resources. }
  TRendererCache = class
  private
    FFreeWhenEmpty: Boolean;
    FFreeWhenEmptyInstanceToNil: PObject;
    TextureImageCaches: TTextureImageCacheList;
    TextureVideoCaches: TTextureVideoCacheList;
    TextureCubeMapCaches: TTextureCubeMapCacheList;
    Texture3DCaches: TTexture3DCacheList;
    TextureDepthOrFloatCaches: TTextureDepthOrFloatCacheList;
    ShapeCaches: array [{ DisableSharedCache } Boolean] of TShapeCacheList;
    ProgramCaches: TShaderProgramCacheList;

    function Empty: Boolean;

    { If we are empty, and FFreeWhenEmpty, then free self.
      Beware: calling this procedure may free the current instance.
      Do not access anything from this instance after calling this method. }
    procedure CheckFreeWhenEmpty;

    { Load given texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function TextureImage_IncReference(
      const TextureImage: TEncodedImage;
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const CompositeForMipmaps: TCompositeImage;
      const GUITexture: boolean;
      const FlipVertically: Boolean): TGLuint;

    procedure TextureImage_DecReference(
      const TextureGLName: TGLuint);

    function TextureVideo_IncReference(
      const TextureVideo: TVideo;
      const TextureFullUrl: string;
      const FlipVertically: Boolean;
      const Filter: TTextureFilter;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const GUITexture: boolean): TGLVideo3D;

    procedure TextureVideo_DecReference(
      const TextureVideo: TGLVideo3D);

    { Load given cube texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function TextureCubeMap_IncReference(
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const PositiveX, NegativeX,
            PositiveY, NegativeY,
            PositiveZ, NegativeZ: TEncodedImage;
      const CompositeForMipmaps: TCompositeImage): TGLuint;

    procedure TextureCubeMap_DecReference(
      const TextureGLName: TGLuint);

    { Requires GLFeatures.TextureDepthCompare (not just TextureDepth!) before calling this. }
    function TextureDepth_IncReference(
      const TextureFullUrl: string;
      const TextureWrap: TTextureWrap2D;
      const Width, Height: Cardinal): TGLuint;

    procedure TextureDepth_DecReference(
      const TextureGLName: TGLuint);

    { Increase / decrease reference to a float texture.
      Required GLFeatures.TextureFloat before calling this.
      Precision32 = @true requires 32-bit full Single floats,
      Precision32 = @false requires 16-bit (half) floats. }
    function TextureFloat_IncReference(
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const TextureWrap: TTextureWrap2D;
      const Width, Height: Cardinal;
      const Precision32: boolean): TGLuint;
    procedure TextureFloat_DecReference(
      const TextureGLName: TGLuint);

    { Load given 3D texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function Texture3D_IncReference(
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const TextureWrap: TTextureWrap3D;
      const Image: TEncodedImage;
      const Composite: TCompositeImage): TGLuint;

    procedure Texture3D_DecReference(
      const TextureGLName: TGLuint);
  public
    constructor Create;
    destructor Destroy; override;

    { Shape cache. We return TShapeCache, either taking an existing
      instance from cache or creating and adding a new one.
      Caller is responsible for checking are Arrays / Vbo zero and
      eventually initializing and setting. }
    function Shape_IncReference(const Shape: TX3DRendererShape;
      const RenderOptions: TCastleRenderOptions): TShapeCache;

    procedure Shape_DecReference(const Shape: TX3DRendererShape;
      var ShapeCache: TShapeCache);

    { Shader program cache. We return TShaderProgramCache,
      either taking an existing instance from cache or creating and adding
      a new one. If we create a new one, we will use Shader to initialize
      program hash and to create and link actual TX3DGLSLProgram instance. }
    function Program_IncReference(
      Shader: TShader; const ShapeNiceName: string): TShaderProgramCache;

    procedure Program_DecReference(var ProgramCache: TShaderProgramCache);

    { Free (and Self <> nil) when the cache will be empty (maybe immediately,
      maybe later).

      Beware: calling this procedure may free the current instance.
      Do not access anything from this instance after calling this method.

      After this procedure, any Xxx_DecReference call also may free
      the cache instance. }
    procedure FreeWhenEmpty(const InstanceToNil: PObject);
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TRendererCache -------------------------------------------- }

constructor TRendererCache.Create;
var
  B: Boolean;
begin
  inherited;
  TextureImageCaches := TTextureImageCacheList.Create;
  TextureVideoCaches := TTextureVideoCacheList.Create;
  TextureCubeMapCaches := TTextureCubeMapCacheList.Create;
  Texture3DCaches := TTexture3DCacheList.Create;
  TextureDepthOrFloatCaches := TTextureDepthOrFloatCacheList.Create;
  for B := Low(Boolean) to High(Boolean) do
    ShapeCaches[B] := TShapeCacheList.Create;
  ProgramCaches := TShaderProgramCacheList.Create;
end;

destructor TRendererCache.Destroy;

{.$define ONLY_WARN_ON_CACHE_LEAK}

{$ifdef ONLY_WARN_ON_CACHE_LEAK}
  procedure Assert(const B: boolean; const S: string = '');
  begin
    if not B then
      WritelnWarning('VRML/X3D', 'GLRendererContextCache warning: ' + S);
  end;
{$endif}

var
  B: Boolean;
begin
  if TextureImageCaches <> nil then
  begin
    Assert(TextureImageCaches.Count = 0, 'Some references to texture images still exist' +
      ' when freeing TRendererCache');
    FreeAndNil(TextureImageCaches);
  end;

  if TextureVideoCaches <> nil then
  begin
    Assert(TextureVideoCaches.Count = 0, 'Some references to texture videos still exist' +
      ' when freeing TRendererCache');
    FreeAndNil(TextureVideoCaches);
  end;

  if TextureCubeMapCaches <> nil then
  begin
    Assert(TextureCubeMapCaches.Count = 0, 'Some references to texture cubemaps still exist' +
      ' when freeing TRendererCache');
    FreeAndNil(TextureCubeMapCaches);
  end;

  if Texture3DCaches <> nil then
  begin
    Assert(Texture3DCaches.Count = 0, 'Some references to texture 3D still exist' +
      ' when freeing TRendererCache');
    FreeAndNil(Texture3DCaches);
  end;

  if TextureDepthOrFloatCaches <> nil then
  begin
    Assert(TextureDepthOrFloatCaches.Count = 0, 'Some references to depth or float texture still exist' +
      ' when freeing TRendererCache');
    FreeAndNil(TextureDepthOrFloatCaches);
  end;

  for B := Low(Boolean) to High(Boolean) do
    if ShapeCaches[B] <> nil then
    begin
      Assert(ShapeCaches[B].Count = 0, Format('%d references to shapes still exist on ShapeCaches[%s] when freeing TRendererCache', [
        ShapeCaches[B].Count,
        BoolToStr(B, true)
        // ShapeCaches[B][0].ToString // not printed, risks further SEGFAULT during log output in case invalid reference remained on the list
      ]));
      FreeAndNil(ShapeCaches[B]);
    end;

  if ProgramCaches <> nil then
  begin
    Assert(ProgramCaches.Count = 0, 'Some references to GLSL programs still exist' +
      ' when freeing TRendererCache');
    FreeAndNil(ProgramCaches);
  end;

  inherited;
end;

function TRendererCache.TextureImage_IncReference(
  const TextureImage: TEncodedImage;
  const TextureFullUrl: string;
  const Filter: TTextureFilter;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage;
  const GUITexture: boolean;
  const FlipVertically: Boolean): TGLuint;
var
  I: Integer;
  TextureCached: TTextureImageCache;
begin
  for I := 0 to TextureImageCaches.Count - 1 do
  begin
    TextureCached := TextureImageCaches[I];

    { Once I had an idea to make here comparison with
      TextureImage = TextureCached^.Image. Since we have TTexturesVideosCache,
      so images from the same URL would have the same reference, so this
      would work perfectly, and make comparison with TextureURL obsolete, right ?

      But there's a problem with this: Image reference may be freed while
      the corresponding texture is still cached. In fact, it's normal in
      "The Castle", if you use FreeResources([frTexturesInNodes]) feature.
      Which means that Image reference may become invalid, and, worse,
      another Image may be potentially assigned the same reference.

      What would be needed is to automatically set cached Image reference
      to nil (and implement to not use Image reference if it's nil) if
      Image instance is freed. Something like FreeNotification.

      But still, the same FreeResources([frTexturesInNodes]) would prevent
      the texture from sharing, if we would free the texture prematurely
      and later load the same texture, with to different TCastleImage instance.

      For now, I don't use this idea, and rely on TextureFullUrl. }

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.FlipVertically = FlipVertically) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = TextureAnisotropy) and
       (TextureCached.Wrap = TextureWrap) and
       (TextureCached.GUITexture = GUITexture) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'TextureImage %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  { Initialize Result first, before calling TextureImageCaches.Add.
    That's because in case LoadGLTexture raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureImage_DecReference later). }
  Result := LoadGLTexture(TextureImage, Filter, TextureWrap, CompositeForMipmaps, GUITexture);

  TexParameterMaxAnisotropy(GL_TEXTURE_2D, TextureAnisotropy);

  TextureCached := TTextureImageCache.Create;
  TextureImageCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.FlipVertically := FlipVertically;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := TextureAnisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GUITexture := GUITexture;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'TextureImage %s: %d', [TextureFullUrl, 1]);
end;

procedure TRendererCache.TextureImage_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureImageCaches.Count - 1 do
    if TextureImageCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureImageCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'TextureImage %s: %d', [TextureImageCaches[I].FullUrl,
                                    TextureImageCaches[I].References]);
      if TextureImageCaches[I].References = 0 then
      begin
        glFreeTexture(TextureImageCaches[I].GLName);
        TextureImageCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TRendererCache.TextureImage_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TRendererCache.TextureVideo_IncReference(
  const TextureVideo: TVideo;
  const TextureFullUrl: string;
  const FlipVertically: Boolean;
  const Filter: TTextureFilter;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const GUITexture: boolean): TGLVideo3D;
var
  I: Integer;
  TextureCached: TTextureVideoCache;
begin
  for I := 0 to TextureVideoCaches.Count - 1 do
  begin
    TextureCached := TextureVideoCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.FlipVertically = FlipVertically) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = TextureAnisotropy) and
       (TextureCached.Wrap = TextureWrap) and
       (TextureCached.GUITexture = GUITexture) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'TextureVideo %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLVideo);
    end;
  end;

  { Initialize Result first, before calling TextureVideoCaches.Add.
    That's because in case TGLVideo3D.Create raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureVideo_DecReference later). }
  Result := TGLVideo3D.Create(TextureVideo, Filter, TextureAnisotropy, TextureWrap, GUITexture);

  TextureCached := TTextureVideoCache.Create;
  TextureVideoCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.FlipVertically := FlipVertically;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := TextureAnisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GUITexture := GUITexture;
  TextureCached.References := 1;
  TextureCached.GLVideo := Result;

  if LogRendererCache then
    WritelnLog('++', 'TextureVideo %s: %d', [TextureFullUrl, 1]);
end;

procedure TRendererCache.TextureVideo_DecReference(
  const TextureVideo: TGLVideo3D);
var
  I: Integer;
begin
  for I := 0 to TextureVideoCaches.Count - 1 do
    if TextureVideoCaches[I].GLVideo = TextureVideo then
    begin
      Dec(TextureVideoCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'TextureVideo %s: %d', [
          TextureVideoCaches[I].FullUrl,
          TextureVideoCaches[I].References
        ]);
      if TextureVideoCaches[I].References = 0 then
      begin
        FreeAndNil(TextureVideoCaches[I].GLVideo);
        TextureVideoCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TRendererCache.TextureVideo_DecReference: no reference ' +
    'found to texture %s', [PointerToStr(TextureVideo)]);
end;

function TRendererCache.TextureCubeMap_IncReference(
  const TextureFullUrl: string;
  const Filter: TTextureFilter;
  const Anisotropy: TGLfloat;
  const PositiveX, NegativeX,
        PositiveY, NegativeY,
        PositiveZ, NegativeZ: TEncodedImage;
  const CompositeForMipmaps: TCompositeImage): TGLuint;
var
  I: Integer;
  TextureCached: TTextureCubeMapCache;
  DisableMipmaps: Boolean;
begin
  if TextureFullUrl <> '' then // never share texture with FullUrl = '', e.g. from GeneratedCubeMapTexture
    for I := 0 to TextureCubeMapCaches.Count - 1 do
    begin
      TextureCached := TextureCubeMapCaches[I];

      if (TextureCached.FullUrl = TextureFullUrl) and
         (TextureCached.Filter = Filter) and
         (TextureCached.Anisotropy = Anisotropy) then
      begin
        Inc(TextureCached.References);
        if LogRendererCache then
          WritelnLog('++', 'Cube map %s: %d', [
            TextureFullUrl,
            TextureCached.References
          ]);
        Exit(TextureCached.GLName);
      end;
    end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_CUBE_MAP, Result);

  SetTextureFilter(GL_TEXTURE_CUBE_MAP, Filter);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GLFeatures.CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GLFeatures.CLAMP_TO_EDGE);

  glTextureCubeMap(Result,
    PositiveX, NegativeX,
    PositiveY, NegativeY,
    PositiveZ, NegativeZ,
    CompositeForMipmaps,
    Filter.NeedsMipmaps,
    DisableMipmaps);

  if DisableMipmaps then
  begin
    Filter.DisableMipmaps;
    SetTextureFilter(GL_TEXTURE_CUBE_MAP, Filter);
  end;

  TexParameterMaxAnisotropy(GL_TEXTURE_CUBE_MAP, Anisotropy);

  TextureCached := TTextureCubeMapCache.Create;
  TextureCubeMapCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := Anisotropy;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'Cube map %s: %d', [TextureFullUrl, 1]);
end;

procedure TRendererCache.TextureCubeMap_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureCubeMapCaches.Count - 1 do
    if TextureCubeMapCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureCubeMapCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Cube map %s: %d', [
          TextureCubeMapCaches[I].FullUrl,
          TextureCubeMapCaches[I].References
        ]);
      if TextureCubeMapCaches[I].References = 0 then
      begin
        glFreeTexture(TextureCubeMapCaches[I].GLName);
        TextureCubeMapCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TRendererCache.TextureCubeMap_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TRendererCache.Texture3D_IncReference(
  const TextureFullUrl: string;
  const Filter: TTextureFilter;
  const Anisotropy: TGLfloat;
  const TextureWrap: TTextureWrap3D;
  const Image: TEncodedImage;
  const Composite: TCompositeImage): TGLuint;
var
  I: Integer;
  TextureCached: TTexture3DCache;
begin
  for I := 0 to Texture3DCaches.Count - 1 do
  begin
    TextureCached := Texture3DCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = Anisotropy) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'Texture3D %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  if not GLFeatures.Texture3D then
    raise Exception.Create('3D textures not supported by OpenGL(ES)');

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_3D, Result);

  glTextureImage3d(Result, Image, Filter, Composite);

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, TextureWrap.Data[2]);

  TexParameterMaxAnisotropy(GL_TEXTURE_3D, Anisotropy);

  TextureCached := TTexture3DCache.Create;
  Texture3DCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := Anisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'Texture3D %s: %d', [TextureFullUrl, 1]);
end;

procedure TRendererCache.Texture3D_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to Texture3DCaches.Count - 1 do
    if Texture3DCaches[I].GLName = TextureGLName then
    begin
      Dec(Texture3DCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Texture3D %s: %d', [
          Texture3DCaches[I].FullUrl,
          Texture3DCaches[I].References
        ]);
      if Texture3DCaches[I].References = 0 then
      begin
        glFreeTexture(Texture3DCaches[I].GLName);
        Texture3DCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TRendererCache.Texture3D_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TRendererCache.TextureDepth_IncReference(
  const TextureFullUrl: String;
  const TextureWrap: TTextureWrap2D;
  const Width, Height: Cardinal): TGLuint;
var
  I: Integer;
  TextureCached: TTextureDepthOrFloatCache;
  Filter: TTextureFilter;
  ImageType: TGLenum;
  ImageFormat: string;
  ImageSize: Int64;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
  begin
    TextureCached := TextureDepthOrFloatCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'Depth texture %s: %d', [
          TextureFullUrl,
          TextureCached.References
        ]);
      Exit(TextureCached.GLName);
    end;
  end;

  Assert(GLFeatures.TextureDepth);
  Assert(GLFeatures.TextureDepthCompare);

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  Filter.Minification := minLinear;
  Filter.Magnification := magLinear;
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);

  { OpenGLES allows only GL_UNSIGNED_SHORT or GL_UNSIGNED_INT for depth textures.
    This was true for OES_depth_texture, looks like it is true for OpenGLES 3.0 core too,
    based on "OpenGL ES 3.2 NVIDIA 515.86.01". }
  ImageType := {$ifdef OpenGLES} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_BYTE {$endif};
  ImageFormat := {$ifdef OpenGLES} 'depth-short' {$else} 'depth-byte' {$endif};
  ImageSize := {$ifdef OpenGLES} SizeOf(TGLushort) {$else} SizeOf(TGLbyte) {$endif}
    * Width * Height;

  { Do not init any texture image. Just initialize texture sizes
    and both internal and external formats to GL_DEPTH_COMPONENT_ARB
    (will match depth buffer precision). }
  glTexImage2d(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT,
    Width, Height, 0, GL_DEPTH_COMPONENT, ImageType, nil);
  TextureMemoryProfiler.Allocate(Result, '', ImageFormat, ImageSize, false,
    Width, Height, 1);

  if GLFeatures.TextureDepthCompare then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
  end else
    WritelnWarning('VRML/X3D', 'OpenGL doesn''t support GL_ARB_shadow, we cannot set depth comparison for depth texture');

  TextureCached := TTextureDepthOrFloatCache.Create;
  TextureDepthOrFloatCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.References := 1;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'Depth texture %s: %d', [TextureFullUrl, 1]);
end;

procedure TRendererCache.TextureDepth_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
    if TextureDepthOrFloatCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureDepthOrFloatCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Depth texture %s: %d', [
          TextureDepthOrFloatCaches[I].FullUrl,
          TextureDepthOrFloatCaches[I].References
        ]);
      if TextureDepthOrFloatCaches[I].References = 0 then
      begin
        glFreeTexture(TextureDepthOrFloatCaches[I].GLName);
        TextureDepthOrFloatCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TRendererCache.TextureDepth_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TRendererCache.TextureFloat_IncReference(
  const TextureFullUrl: String;
  const Filter: TTextureFilter;
  const TextureWrap: TTextureWrap2D;
  const Width, Height: Cardinal;
  const Precision32: boolean): TGLuint;
{$ifndef OpenGLES}
var
  I: Integer;
  TextureCached: TTextureDepthOrFloatCache;
  InternalFormat: TGLenum;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
  begin
    TextureCached := TextureDepthOrFloatCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'Float texture %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);

  if Precision32 then
    InternalFormat := GL_RGB32F_ARB { same thing as GL_RGB_FLOAT32_ATI } else
    InternalFormat := GL_RGB16F_ARB { same thing as GL_RGB_FLOAT16_ATI };

  { Do not init any texture image. Just initialize texture sizes and formats. }
  glTexImage2d(GL_TEXTURE_2D, 0, InternalFormat,
    Width, Height, 0, GL_RGB, GL_FLOAT, nil);
  TextureMemoryProfiler.Allocate(Result, '', 'rgb-float',
    3 * SizeOf(TGLfloat) * Width * Height, false, Width, Height, 1);

  TextureCached := TTextureDepthOrFloatCache.Create;
  TextureDepthOrFloatCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.References := 1;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GLName := Result;
  { Hm, we probably should store Filter, Precision32
    inside TextureCached as well... Ignore this, useless for now ---
    one Node will require only one float texture anyway. }

  if LogRendererCache then
    WritelnLog('++', 'Float texture %s: %d', [TextureFullUrl, 1]);
{$else}
begin
  raise Exception.Create('Float textures not available on OpenGL ES 2.0');
  Result := 0; // silence warning
{$endif}
end;

procedure TRendererCache.TextureFloat_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
    if TextureDepthOrFloatCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureDepthOrFloatCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Float texture %s: %d', [
          TextureDepthOrFloatCaches[I].FullUrl,
          TextureDepthOrFloatCaches[I].References
        ]);
      if TextureDepthOrFloatCaches[I].References = 0 then
      begin
        glFreeTexture(TextureDepthOrFloatCaches[I].GLName);
        TextureDepthOrFloatCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TRendererCache.TextureFloat_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TRendererCache.Shape_IncReference(
  const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions): TShapeCache;
var
  FogEnabled, FogVolumetric: boolean;
  FogVolumetricDirection: TVector3;
  FogVolumetricVisibilityStart: Single;

  function IgnoreStateTransform: boolean;
  begin
    if { Force CacheIgnoresTransform to be false if our shape uses shaders.
         Shaders may depend on coordinates in eye space, which obviously
         may be different for shapes that differ even only on transform. }
      (Shape.Node <> nil) and
      (Shape.Node.Appearance <> nil) and
      (Shape.Node.Appearance.FdShaders.Count <> 0) then
      Exit(false);

    { If we use any features that (may) render shape differently
      if shape's transform (or other stuff handled outside arrays
      and castlerenderer) changes, then Result must be false. }
    Result := not FogVolumetric;
  end;

  function FogVolumetricEqual(
    const Volumetric1: boolean;
    const VolumetricDirection1: TVector3;
    const VolumetricVisibilityStart1: Single;
    const Volumetric2: boolean;
    const VolumetricDirection2: TVector3;
    const VolumetricVisibilityStart2: Single): boolean;
  begin
    Result := (Volumetric1 = Volumetric2) and
      ( (not Volumetric1) or
        ( TVector3.PerfectlyEquals(VolumetricDirection1, VolumetricDirection2) and
          (VolumetricVisibilityStart1 = VolumetricVisibilityStart2) ) );
  end;

var
  I: Integer;
  DisableSharedCache: Boolean;
  Caches: TShapeCacheList;
begin
  TRenderer.GetFog(Shape.Fog, RenderOptions, FogEnabled, FogVolumetric,
    FogVolumetricDirection, FogVolumetricVisibilityStart);

  DisableSharedCache := TGLShape(Shape).DisableSharedCache;
  Caches := ShapeCaches[DisableSharedCache];

  if not DisableSharedCache then
    for I := 0 to Caches.Count - 1 do
    begin
      Result := Caches[I];
      if (Result.Geometry = Shape.Geometry) and
         Result.RenderOptions.EqualForShapeCache(RenderOptions) and
         Result.State.Equals(Shape.State, IgnoreStateTransform) and
         FogVolumetricEqual(
           Result.FogVolumetric,
           Result.FogVolumetricDirection,
           Result.FogVolumetricVisibilityStart,
           FogVolumetric,
           FogVolumetricDirection,
           FogVolumetricVisibilityStart) then
      begin
        Inc(Result.References);
        if LogRendererCache then
          WritelnLog('++', Result.ToString);
        Exit(Result);
      end;
    end;

  { not found, so create new }

  Result := TShapeCache.Create;
  Caches.Add(Result);
  Result.RenderOptions := RenderOptions;
  Result.Geometry := Shape.Geometry;
  Result.State := Shape.State;
  Result.FogVolumetric := FogVolumetric;
  Result.FogVolumetricDirection := FogVolumetricDirection;
  Result.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
  Result.References := 1;

  if LogRendererCache then
    WritelnLog('++', Result.ToString);
end;

procedure TRendererCache.Shape_DecReference(const Shape: TX3DRendererShape;
  var ShapeCache: TShapeCache);
var
  I: Integer;
  DisableSharedCache: Boolean;
  Caches: TShapeCacheList;
begin
  DisableSharedCache := TGLShape(Shape).DisableSharedCache;
  Caches := ShapeCaches[DisableSharedCache];
  I := Caches.IndexOf(ShapeCache);

  if I <> -1 then
  begin
    Dec(ShapeCache.References);
    if LogRendererCache then
      WritelnLog('--', ShapeCache.ToString);
    if ShapeCache.References = 0 then
      Caches.Delete(I);
    ShapeCache := nil;
    CheckFreeWhenEmpty;
  end else
    raise EInternalError.Create(
      'TRendererCache.Shape_DecReference: no reference found');
end;

function TRendererCache.Program_IncReference(
  Shader: TShader; const ShapeNiceName: string): TShaderProgramCache;
var
  I: Integer;
begin
  for I := 0 to ProgramCaches.Count - 1 do
  begin
    Result := ProgramCaches[I];
    if Result.Hash = Shader.CodeHash then
    begin
      Inc(Result.References);
      if LogRendererCache then
        WritelnLog('++', 'Shader program (hash %s): %d', [Result.Hash.ToString, Result.References]);
      Exit(Result);
    end;
  end;

  Result := TShaderProgramCache.Create;
  ProgramCaches.Add(Result);
  Result.References := 1;
  Result.Hash := Shader.CodeHash;

  try
    Result.ShaderProgram := TX3DGLSLProgram.Create;
    Shader.LinkProgram(Result.ShaderProgram, ShapeNiceName);
  except on E: EGLSLError do
    begin
      FreeAndNil(Result.ShaderProgram);
      { Note: leave Result assigned and Result.Hash set,
        to avoid reinitializing this shader next time. }
      WritelnWarning('VRML/X3D', Format('Cannot use GLSL shader for shape "%s": %s',
        [ShapeNiceName, E.Message]));
    end;
  end;

  { We *must* have some GLSL shader when GLFeatures.EnableFixedFunction = false }
  if (not GLFeatures.EnableFixedFunction) and (Result.ShaderProgram = nil) then
  begin
    try
      Result.ShaderProgram := TX3DGLSLProgram.Create;
      Shader.LinkFallbackProgram(Result.ShaderProgram);
    except on E: EGLSLError do
      begin
        { We try to behave nicely when LinkFallbackProgram fails, although in practice
          Android's OpenGLES implementation may just crash... }
        FreeAndNil(Result.ShaderProgram);
        WritelnWarning('VRML/X3D', Format('Cannot use even fallback GLSL shader for shape "%s": %s',
          [ShapeNiceName, E.Message]));
      end;
    end;
  end;

  if LogRendererCache then
    WritelnLog('++', 'Shader program (hash %s): %d', [Result.Hash.ToString, Result.References]);
end;

procedure TRendererCache.Program_DecReference(var ProgramCache: TShaderProgramCache);
var
  I: Integer;
begin
  for I := 0 to ProgramCaches.Count - 1 do
  begin
    if ProgramCaches[I] = ProgramCache then
    begin
      Dec(ProgramCache.References);
      if LogRendererCache then
        WritelnLog('--', 'Shader program (hash %s): %d', [ProgramCache.Hash.ToString, ProgramCache.References]);
      if ProgramCache.References = 0 then
        ProgramCaches.Delete(I);
      ProgramCache := nil;
      CheckFreeWhenEmpty;
      Exit;
    end;
  end;

  raise EInternalError.Create(
    'TRendererCache.Program_DecReference: no reference found');
end;

function TRendererCache.Empty: Boolean;
begin
  Result :=
    (TextureImageCaches.Count = 0) and
    (TextureVideoCaches.Count = 0) and
    (TextureCubeMapCaches.Count = 0) and
    (Texture3DCaches.Count = 0) and
    (TextureDepthOrFloatCaches.Count = 0) and
    (ShapeCaches[false].Count = 0) and
    (ShapeCaches[true].Count = 0) and
    (ProgramCaches.Count = 0);
end;

procedure TRendererCache.CheckFreeWhenEmpty;
begin
  if FFreeWhenEmpty and Empty then
  begin
    FFreeWhenEmptyInstanceToNil^ := nil; // sets global RendererCache to nil
    Self.Destroy;
  end;
end;

procedure TRendererCache.FreeWhenEmpty(const InstanceToNil: PObject);
begin
  if Self <> nil then // hack to allow calling on nil instance, like standard Free
  begin
    FFreeWhenEmptyInstanceToNil := InstanceToNil;
    FFreeWhenEmpty := true;
    CheckFreeWhenEmpty;
  end;
end;

{$endif}
