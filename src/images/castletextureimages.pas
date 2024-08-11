{
  Copyright 2009-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handling of images for textures.
  This unit is not OpenGL-specific, it should be suitable for all 3D libraries.
  See CastleGLImage for OpenGL-specific handling of textures and other images.

  Texture is any TEncodedImage instance. This includes not only
  a traditional 2D/3D matrix of pixels represented as TCastleImage,
  but also a texture compressed for GPU (TGPUCompressedImage). Moreover, a texture
  may have mipmaps defined --- they are stored inside TCompositeImage
  instance (that contains a list of TEncodedImage).

  Since not everything can really deal with such flexible definition
  of a texture, we decided to separate some routines specifically
  for textures. For example, you have LoadTextureImage to load full texture
  information --- contrast this with LoadImage routine in CastleImages unit,
  that only returns TCastleImage (a "normal" way to deal with image data). }
unit CastleTextureImages;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleImages, CastleInternalCompositeImage, CastleUtils, CastleVideos, CastleRenderOptions;

const
  { Image classes that are handled by absolutely all OpenGL versions. }
  TextureImageClasses: array [0..3] of TEncodedImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage
  );

  { All image classes that may be handled by OpenGL.
    Some of them may require specific OpenGL extensions or versions
    (like GPU-compressed or float textures). }
  TextureImageClassesAll: array [0..8] of TEncodedImageClass = (
    // 8-bit images
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage,

    // GPU-compressed
    TGPUCompressedImage,

    // floating-point precision and range
    TGrayscaleFloatImage,
    TGrayscaleAlphaFloatImage,
    TRGBFloatImage,
    TRGBAlphaFloatImage
  );

{ Load image suitable for a texture.
  This will load image to memory formats supported by common
  3D libraries (like OpenGL), for example it will never return TRGBFloatImage
  (although OpenGL may support it, but we cannot be sure at this point).
  It may return texture compressed using one of the GPU compression algorithms
  (see TTextureCompression).

  If the image comes from a TCompositeImage file (DDS, KTX...), it will also return it
  (if not, Composite returned will be @nil). This allows you to e.g. use
  texture mipmaps recorded there. Note that Composite.OwnsFirstImage is set
  to @false, so you can always safely free everything by simple
  @code(FreeAndNil(Image); FreeAndNil(Composite);).

  Overloaded version without Composite parameter assumes you're
  not interested in this information (still it handles Composite files of course,
  it just doesn't return Composite object instance).

  @groupBegin }
function LoadTextureImage(const Url: String;
  out Composite: TCompositeImage;
  const LoadOptions: TLoadImageOptions = []): TEncodedImage; overload;
function LoadTextureImage(const Url: String;
  const LoadOptions: TLoadImageOptions = []): TEncodedImage; overload;
{ @groupEnd }

type
  { A cache of loaded images for textures.

    Load by TextureImage_IncReference, free by TextureImage_DecReference.
    These replace LoadTextureImage, and manual freeing of Image/Composite.

    If you used IncReference that returns Composite, then you should also
    free using DecReference that takes Composite.
    If you used IncReference without Composite parameter, then also
    free using DecReference without Composite parameter.

    The idea is that instead of @code(LoadTextureImage(...)) call
    @code(Cache.TextureImage_IncReference(...)).
    Later, instead of freeing this image, call
    @code(TextureImage_DecReference(Image)). From your point of view, things
    will work the same. But if you expect to load many textures from the
    same URL, then you will get a great speed and memory saving,
    because image will only be actually loaded once. Notes:

    @unorderedList(
      @item(Note that in case of problems with loading,
        TextureImage_IncReference may raise an exception, just like normal
        LoadTextureImage. In this case it's guaranteed that no reference will
        be incremented, of course. If LoadTextureImage_IncReference returns
        in a normal way, then it will return something non-@nil, just like
        LoadTextureImage does.)

      @item(LoadTextureImage_DecReference alwas sets Image to @nil, like FreeAndNil.)

      @item(Since detecting image alpha channel type may be a little time-consuming
        (iteration over all pixels is needed), we also do it here
        and save in cache.)
    )

    Note that before destroying this object you must free all textures,
    i.e. call LoadTextureImage_DecReference for all images allocated by
    LoadTextureImage_IncReference. @italic(This class is not a lousy way
    of avoiding memory leaks) --- it would be a bad idea, because it would
    cause sloppy programming, where memory is unnecessarily allocated for
    a long time. In fact, this class asserts in destructor that no images
    are in cache anymore, so if you compiled with assertions enabled,
    this class does the job of memory-leak detector. }
  TTexturesVideosCache = class(TVideosCache)
  private
    type
      { Internal for TTexturesVideosCache. @exclude }
      TCachedTexture = class
        References: Cardinal;
        Url: String;
        LoadOptions: TLoadImageOptions;
        Image: TEncodedImage;
        Composite: TCompositeImage;
        AlphaChannel: TAlphaChannel;
      end;
      TCachedTextureList = {$ifdef FPC}specialize{$endif} TObjectList<TCachedTexture>;
    var
      CachedTextures: TCachedTextureList;
  public
    constructor Create;
    destructor Destroy; override;

    function TextureImage_IncReference(const Url: String;
      out Composite: TCompositeImage;
      out AlphaChannel: TAlphaChannel;
      const LoadOptions: TLoadImageOptions = []): TEncodedImage; overload;
    function TextureImage_IncReference(const Url: String;
      out AlphaChannel: TAlphaChannel;
      const LoadOptions: TLoadImageOptions = []): TEncodedImage; overload;
    function TextureImage_IncReference(const Url: String;
      const LoadOptions: TLoadImageOptions = []): TEncodedImage; overload;

    procedure TextureImage_DecReference(var Image: TEncodedImage; var Composite: TCompositeImage); overload;
    procedure TextureImage_DecReference(var Image: TEncodedImage); overload;

    function Empty: boolean; override;
  end;

var
  { Log texture cache events. Allows to see how the cache performs,
    and also how alpha channel is detected.
    A @italic(lot) of log messages.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogTextureCache: boolean = false;

  { Cache of texture images, equal to X3DCache
    and automatically initialized / finalized if you use X3DNodes unit. }
  TextureCache: TTexturesVideosCache;

  { Use the auto-generated alternative downscaled images.
    This allows to conserve both GPU memory and loading time
    by using a downscaled images versions.

    The subset of your images which are affected by this must be declared inside
    the material_properties.xml file.
    And the image files must be prepared earlier by the build tool call
    @code("castle-engine auto-generate-textures").
    See the https://castle-engine.io/creating_data_auto_generated_textures.php#section_texture_scale .

    Each size (width, height, and (for 3D images) depth) is scaled
    by @code(1 / 2^(TextureLoadingScale-1)).
    So value = 1 means no scaling, value = 2 means that each size is 1/2
    (texture area is 1/4), value = 3 means that each size is 1/4 and so on.

    This mechanism will @italic(not)
    automatically downscale textures at runtime. If the downscaled texture version
    should exist, according to the material_properties.xml file,
    but it doesn't, then texture loading will simply fail.
    If you want to scale the texture at runtime, use the similar @link(GLTextureScale)
    instead.

    This mechanism is independent from GLTextureScale:

    @unorderedList(
      @item(Scaling indicated by GLTextureScale is performed at runtime,
        after loading. It happens @bold(after) the results of
        TextureLoadingScale have already been applied.)

      @item(The GLTextureScale works on a different subset of textures.

        For GLTextureScale, the usage of a texture determines if it's a GUI texture
        (which cannot be scaled) or not.
        So textures loaded through TDrawableImage, or declared as guiTexture in X3D,
        are not affected by GLTextureScale. All other textures are affected.
        It doesn't matter from where they are loaded -- so it affects also
        texture contents created by code, or downloaded from the Internet.

        In contrast, the TextureLoadingScale works (only) on all the images
        declared as having a downscaled version in material_properties.xml.
        It is not affected by how the texture will be used.)

      @item(The GLTextureScale works only on texture formats that can be scaled.
        In particular, it cannot scale textures compressed with a GPU compression
        (S3TC and such). It silently ignores them.

        In contrast, the TextureLoadingScale can cooperate with GPU-compressed textures,
        if you also compress them automatically using the material_properties.xml
        and the build tool call @code("castle-engine auto-generate-textures").
        The downscaled image versions are generated from original (uncompressed,
        unscaled) images, and are then compressed.)

      @item(The GLTextureScale scaling is usually of worse quality, since it's
        done at runtime.

        In contrast, the downscaled textures used by TextureLoadingScale
        are generated as a preprocessing step.
        The build tool @code("castle-engine auto-generate-textures") may use
        a slower but higher-quality scaling.)
    )
  }
  TextureLoadingScale: Cardinal = 1;

const
  { We recommend using CastleRenderOptions unit to get these constants.
    But for backward compatibility, they are also available here. }
  minNearest = CastleRenderOptions.minNearest;
  minLinear = CastleRenderOptions.minLinear;
  minNearestMipmapNearest = CastleRenderOptions.minNearestMipmapNearest;
  minNearestMipmapLinear = CastleRenderOptions.minNearestMipmapLinear;
  minLinearMipmapNearest = CastleRenderOptions.minLinearMipmapNearest;
  minLinearMipmapLinear = CastleRenderOptions.minLinearMipmapLinear;
  minDefault = CastleRenderOptions.minDefault;
  minFastest = CastleRenderOptions.minFastest;
  minNicest = CastleRenderOptions.minNicest;

  magNearest = CastleRenderOptions.magNearest;
  magLinear = CastleRenderOptions.magLinear;
  magDefault = CastleRenderOptions.magDefault;
  magFastest = CastleRenderOptions.magFastest;
  magNicest = CastleRenderOptions.magNicest;

  { Default frame per second for Sprite Sheet animations like Starling
    and Cocos2d. }
  DefaultSpriteSheetFramesPerSecond = 8.0;

implementation

uses SysUtils, CastleStringUtils, CastleLog, CastleUriUtils;

function LoadTextureImage(const Url: String; out Composite: TCompositeImage;
  const LoadOptions: TLoadImageOptions): TEncodedImage;
begin
  if not TCompositeImage.MatchesUrl(Url) then
  begin
    { Note: We need to enable here GPU-compressed textures, TGPUCompressedImage,
      which is (for now) in TextureImageClassesAll, but not in TextureImageClasses.
      Otherwise loading non-composite but still GPU-compressed format, like .astc,
      would always fail (or go through DecompressTexture unpacking).

      Testcase:
      - fps_game on GPU supporting ASTC (michalis: worm-linux)
      - castle-model-viewer on demo-models/texturing_advanced/astc_compressed/textures_astc_compressed.x3dv

      Loading through TCompositeImage also allows them.
    }
    Result := LoadEncodedImage(Url, TextureImageClassesAll, LoadOptions);
    Composite := nil;
  end else
  begin
    Composite := TCompositeImage.Create;
    try
      Composite.LoadFromFile(Url, LoadOptions);
      Composite.OwnsFirstImage := false;
      Result := Composite.Images[0];
    except
      FreeAndNil(Composite);
      raise;
    end;
  end;
end;

function LoadTextureImage(const Url: String;
  const LoadOptions: TLoadImageOptions): TEncodedImage;
var
  Composite: TCompositeImage;
begin
  Result := LoadTextureImage(Url, Composite, LoadOptions);
  Composite.Free;
end;

{ TTexturesVideosCache ------------------------------------------------- }

constructor TTexturesVideosCache.Create;
begin
  inherited;
  CachedTextures := TCachedTextureList.Create;
end;

destructor TTexturesVideosCache.Destroy;
begin
  if CachedTextures <> nil then
  begin
    Assert(CachedTextures.Count = 0, ' Some references to texture images still exist ' +
      'when freeing TTexturesVideosCache');
    FreeAndNil(CachedTextures);
  end;
  inherited;
end;

function TTexturesVideosCache.TextureImage_IncReference(
  const Url: String;
  out Composite: TCompositeImage; out AlphaChannel: TAlphaChannel;
  const LoadOptions: TLoadImageOptions): TEncodedImage;

  function AlphaChannelLog(const AC: TAlphaChannel): string;
  begin
    if AC <> acNone then
      Result := '. Detected as simple yes/no ("test") alpha channel: ' + BoolToStr(AC = acTest, true) else
      Result := '';
  end;

var
  I: Integer;
  C: TCachedTexture;
begin
  for I := 0 to CachedTextures.Count - 1 do
  begin
    C := CachedTextures[I];
    if (C.Url = Url) and
       (C.LoadOptions = LoadOptions) then
    begin
      Inc(C.References);

      if LogTextureCache then
        WritelnLog('++', 'Texture image %s: %d', [UriDisplay(Url), C.References]);

      Composite := C.Composite;
      AlphaChannel := C.AlphaChannel;
      Exit(C.Image);
    end;
  end;

  { Initialize Result first, before calling CachedTextures.Add.
    That's because in case LoadTextureImage raises exception,
    we don't want to add image to cache (because caller would have
    no way to call TextureImage_DecReference later). }

  Result := LoadTextureImage(Url, Composite, LoadOptions);
  AlphaChannel := Result.AlphaChannel;

  C := TCachedTexture.Create;
  CachedTextures.Add(C);
  C.References := 1;
  C.Url := Url;
  C.LoadOptions := LoadOptions;
  C.Image := Result;
  C.Composite := Composite;
  C.AlphaChannel := AlphaChannel;

  if LogTextureCache then
    WritelnLog('++', 'Texture image %s: %d%s',
      [UriDisplay(Url), 1, AlphaChannelLog(AlphaChannel)]);
end;

procedure TTexturesVideosCache.TextureImage_DecReference(
  var Image: TEncodedImage; var Composite: TCompositeImage);
var
  I: Integer;
  C: TCachedTexture;
begin
  for I := 0 to CachedTextures.Count - 1 do
  begin
    C := CachedTextures[I];
    if C.Image = Image then
    begin
      if LogTextureCache then
        WritelnLog('--', 'Texture image %s: %d', [UriDisplay(C.Url), C.References - 1]);

      { We cannot simply assert

          C.Composite = Composite

        because when textures have many references,
        some references may be with and some without Composite information.
        We don't want to force all references to the same Url to always
        have or never have Composite information. (This would be uncomfortable
        for caller, as different nodes may share textures, e.g. VRML/X3D Background
        and ImageTexture nodes. They would all be forced to remember Composite
        information this way.)

        So we have to always keep Composite information in the cache,
        and free it, regardless of whether called knows this Composite information.

        Only if passed Composite <> nil (we know caller keeps it) then we can
        check it for correctness. }

      Assert((Composite = nil) or (C.Composite = Composite), 'Image pointers match in TTexturesVideosCache, Composite pointers should match too');

      Image := nil;
      Composite := nil;

      if C.References = 1 then
      begin
        FreeAndNil(C.Image);
        FreeAndNil(C.Composite);
        CachedTextures.Delete(I);
        CheckEmpty;
      end else
        Dec(C.References);

      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TTexturesVideosCache.TextureImage_DecReference: no reference found for texture image %s',
    [PointerToStr(Image)]);
end;

function TTexturesVideosCache.TextureImage_IncReference(
  const Url: String; out AlphaChannel: TAlphaChannel;
  const LoadOptions: TLoadImageOptions): TEncodedImage;
var
  Dummy: TCompositeImage;
begin
  Result := TextureImage_IncReference(Url, Dummy, AlphaChannel, LoadOptions);
end;

function TTexturesVideosCache.TextureImage_IncReference(const Url: String;
  const LoadOptions: TLoadImageOptions): TEncodedImage;
var
  DummyAlphaChannel: TAlphaChannel;
begin
  Result := TextureImage_IncReference(Url, DummyAlphaChannel, LoadOptions);
end;

procedure TTexturesVideosCache.TextureImage_DecReference(
  var Image: TEncodedImage);
var
  Dummy: TCompositeImage;
begin
  Dummy := nil;
  TextureImage_DecReference(Image, Dummy);
end;

function TTexturesVideosCache.Empty: boolean;
begin
  Result := (inherited Empty) and (CachedTextures.Count = 0);
end;

end.
