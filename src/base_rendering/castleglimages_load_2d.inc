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

{ Part of CastleGLImages unit: 2D textures sizing and loading.
  Also decompressing GPU-compressed 2D textures, since it's technically related. }

{$ifdef read_interface}

{ ----------------------------------------------------------------------
  Adjusting image sizes to load them as textures.
  Usually you don't need these functions, LoadGLTexture* and TDrawableImage
  and such call it automatically when needed. }
{$ifdef read_interface_type}
// type (continue type section from previous include file)
  { Constraints on texture size, used by @link(ResizeToTextureSize) and @link(IsTextureSized). }
  TTextureSizing = (
    { Texture size does not have to be a power of two
      (unless GLFeatures.TextureNonPowerOfTwo = @false, in which case all textures
      must have power-of-two, and then tsAny may be scaled to satisfy it
      (but it still will not be scaled for GLTextureScale)).
      It is not affected by GLTextureScale. }
    tsAny,
    { Texture size must be a power of two.
      It is not affected by GLTextureScale, because we cannot scale it. }
    tsRequiredPowerOf2,
    { Texture size must be a power of two.
      It is affected by GLTextureScale, we can scale it. }
    tsScalablePowerOf2
  );
{$endif read_interface_type}

{$ifdef read_interface_func}
{ Resize the image to a size accepted as GL_TEXTURE_2D texture size
  for OpenGL. It tries to resize to a larger size, not smaller,
  to avoid losing image information.

  It also makes texture have power-of-two size, if Sizing <> tsAny
  (or if GLFeatures.TextureNonPowerOfTwo = @false).
  This is a must for normal textures, used for 3D rendering
  (with mipmapping and such).
  Using OpenGL non-power-of-2 textures is not good for such usage case,
  some OpenGLs crash (ATI),
  some are ultra slow (NVidia), some cause artifacts (Mesa).
  OpenGL ES explicitly limits what you can do with non-power-of-2.
  Sample model using non-power-of-2 is in inlined_textures.wrl.

  Use Sizing = tsAny only for textures that you plan to use
  for drawing GUI images by TDrawableImage.

  @groupBegin }
procedure ResizeForTextureSize(var r: TCastleImage; const Sizing: TTextureSizing);
function ResizeToTextureSize(const r: TCastleImage; const Sizing: TTextureSizing): TCastleImage; overload;
function ResizeToTextureSize(const Size: Cardinal; const Sizing: TTextureSizing): Cardinal; overload;
{ @groupEnd }

{ Does image have proper size for 2D OpenGL texture.
  See ResizeForTextureSize. Note that this checks glGet(GL_MAX_TEXTURE_SIZE),
  so requires initialized OpenGL context. }
function IsTextureSized(const r: TEncodedImage; const Sizing: TTextureSizing): boolean; overload;

function IsTextureSized(const Width, Height: Cardinal; const Sizing: TTextureSizing): boolean; overload;

var
  { Scaling for all textures loaded to OpenGL.
    This allows you to conserve GPU memory.
    Each size (width, height, and (for 3D textures) depth) is scaled
    by 1 / 2^GLTextureScale.
    So value = 1 means no scaling, value = 2 means that each size is 1/2
    (texture area is 1/4), value = 3 means that each size is 1/4 and so on.

    Note that textures used for GUI, by TDrawableImage
    (more precisely: all non-power-of-2 textures) avoid this scaling entirely.

    This allows to scale the textures at runtime.
    For an independent mechanism that allows to @bold(load) already
    downscaled textures (and scale them as a preprocessing step when
    packaging your game), see @link(TextureLoadingScale).
    The documentation of @link(TextureLoadingScale) lists all the differences
    between these two approaches to scaling. }
  GLTextureScale: Cardinal = 1;

  { Constraints the scaling done by GLTextureScale.
    Scaling caused by GLTextureScale cannot scale texture to something
    less than GLTextureMinSize. If texture size was already < GLTextureMinSize,
    it is not scaled at all by GLTextureScale.
    This must be a power of two. }
  GLTextureMinSize: Cardinal = 16;

{ Load new texture to OpenGL. Generates new texture number by glGenTextures,
  then binds this texture, and loads it's data.

  Takes care of UNPACK_ALIGNMENT inside (if needed, we'll change it and
  later revert back, so that the texture is correctly loaded).

  Sets texture minification, magnification filters and wrap parameters.

  Changes currently bound texture to this one (returned).

  If mipmaps will be needed (this is decided looking at Filter.Minification)
  we will load them too.

  @orderedList(
    @item(
      As a first try, if CompositeForMipmaps is non-nil
      and has mipmaps (CompositeForMipmaps.Mipmaps), we will load these mipmaps.
      CompositeForMipmaps must be a normal 2D texture (CompositeType = ctTexture).

      Otherwise, we'll try to generate mipmaps, using various OpenGL mechanisms.)

    @item(
      We will try using GenerateMipmap functionality to generate mipmaps on GPU.
      If not available, for uncompressed textures, we will generate mipmaps on CPU.
      For compressed textures, we will change minification filter to simple
      minLinear and make WritelnWarning.)
  )

  @param(GUITexture If @true, the texture is not forced to have sizes
    as power-of-two. For TTextureSizing, it is like tsAny instead
    of tsScalablePowerOf2.
    Also, mipmaps are always disabled, regardless of requested
    Filter value, since mipmaps cannot work without power-of-two.)

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason.
    This includes ECannotLoadCompressedTexture if the compressed texture cannot be
    loaded for whatever reason.
    This includes EInvalidImageForOpenGLTexture if Image class is invalid
    for an OpenGL texture.)

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)

  @groupBegin }
function LoadGLTexture(const image: TEncodedImage;
  const Filter: TTextureFilter;
  const Wrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage = nil;
  const GUITexture: boolean = false): TGLTextureId; overload;

function LoadGLTexture(const URL: string;
  const Filter: TTextureFilter;
  const Wrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage = nil;
  const GUITexture: boolean = false): TGLTextureId; overload;
{ @groupEnd }

{ Load OpenGL texture into already reserved texture number.
  It uses existing OpenGL texture number (TextureId). Everything else
  works exactly the same as LoadGLTexture.

  You can also use this to set "default unnamed OpenGL texture" parameters
  by passing TextureId = 0.

  @raises(ETextureLoadError Raised in the same situations as LoadGLTexture.)

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)

  @groupBegin }
procedure LoadGLGeneratedTexture(const TextureId: TGLTextureId; const image: TEncodedImage;
  Filter: TTextureFilter;
  const Wrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage = nil;
  const GUITexture: boolean = false); overload;
{ @groupEnd }

{ Decompress texture image by loading it to a temporary OpenGL(ES) texture and
  reading back. IOW, this does decompression by using current OpenGL(ES) context.

  TODO: Note that the current implementation will always raise ECannotLoadCompressedTexture
  on OpenGLES, e.g. on mobile. Same as SaveTextureContents.
  This is fixable (we need to use FBO instead of glGetTexImage on OpenGLES),
  please submit a bugreport if you need it.

  @raises(ECannotLoadCompressedTexture If we cannot decompress, for example
    because we cannot load to OpenGL this compressed texture (for example
    because appropriate OpenGL extensions are not available, or such).) }
function GLDecompressTexture(const Image: TGPUCompressedImage): TCastleImage;

{$endif read_interface_func}

{$ifdef read_interface_type}
// type (continue type section from previous include file)
  { Raised by SaveTextureContents when we cannot get texture contents. }
  ECannotSaveTextureContents = class(Exception);
{$endif read_interface_type}

{$ifdef read_interface_func}
{ Save OpenGL texture contents to TCastleImage.
  @bold(Size of the texture must match the Image size,
  otherwise terrible things (writing over unallocated memory) will happen).

  @raises(ECannotSaveTextureContents If we cannot get texture contents
    (not supported on OpenGLES).) }
procedure SaveTextureContents(const Image: TCastleImage; const Texture: TGLTextureId);
{$endif read_interface_func}

{$endif read_interface}

{$ifdef read_implementation}

{ ----------------------------------------------------------------------
  Adjusting image size for 2D texture. }

function ResizeToTextureSize(const Size: Cardinal; const Sizing: TTextureSizing): Cardinal;
begin
  { make sure it's power of 2, if necessary }
  if ((Sizing = tsAny) and GLFeatures.TextureNonPowerOfTwo) or
    IsPowerOf2(Size) then
    Result := Size else
    Result := 1 shl (Biggest2Exponent(Size) + 1);

  { apply GLTextureScale }
  if (Sizing = tsScalablePowerOf2) and
     (GLTextureScale > 1) and
     (Result > GLTextureMinSize) then
    Result := Max(Result shr (GLTextureScale - 1), GLTextureMinSize);

  { honour GLFeatures.MaxTextureSize }
  if Result > GLFeatures.MaxTextureSize then
    Result := GLFeatures.MaxTextureSize;
end;

function ResizeToTextureSize(const r: TCastleImage; const Sizing: TTextureSizing): TCastleImage;
var
  NewWidth, NewHeight: Cardinal;
begin
  NewWidth  := ResizeToTextureSize(R.Width , Sizing);
  NewHeight := ResizeToTextureSize(R.Height, Sizing);

  WritelnLog('Textures', Format('Resizing 2D texture "%s" from %dx%d to %dx%d',
    [URIDisplay(R.URL), R.Width, R.Height, NewWidth, NewHeight]));

  result := r.MakeResized(NewWidth, NewHeight, riBilinear);
end;

function IsTextureSized(const Width, Height: Cardinal;
  const Sizing: TTextureSizing): boolean;
begin
  Result :=
    (Width  = ResizeToTextureSize(Width , Sizing)) and
    (Height = ResizeToTextureSize(Height, Sizing));
end;

function IsTextureSized(const r: TEncodedImage; const Sizing: TTextureSizing): boolean;
begin
  Result := IsTextureSized(r.Width, r.Height, Sizing);
end;

procedure ResizeForTextureSize(var r: TCastleImage; const Sizing: TTextureSizing);
var
  newR: TCastleImage;
begin
  if not IsTextureSized(r, Sizing) then
  begin
    newR := ResizeToTextureSize(r, Sizing);
    FreeAndNil(r);
    r := newR;
  end;
end;

function TargetIsCubeMapSide(const Target: TGLenum): Boolean;
begin
  { We rely on the fact that 6 relevant constants are consecutive. }
  Result := Between(Target, GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z);
end;

{ Call glTexImage2D to load given image.
  Takes care of OpenGL(ES) unpacking (alignment etc.).
  Assumes that image already has the right size
  (power of 2 and within OpenGL(ES) required sizes).

  Level = 0 for base (not a mipmap sublevel) image.

  Target has the same meaning as for glTexImage2D.

  LoadedSize will be increased. }
procedure CastleTexImage2D(const Target: TGLenum; const Level: TGLint; const Image: TCastleImage;
  var LoadedSize: Int64);
var
  ImageFormat, ImageInternalFormat, SwizzleTarget: TGLenum;
  ImageSwizzle: TVector4Integer;
begin
  BeforeUnpackImage(Image);
  try
    {$ifndef OpenGLES}
    { Workaround Mesa 7.9-devel bug (at least with Intel DRI,
      on Ubuntu 10.10, observed on domek): glTexImage2D accidentally
      enables GL_TEXTURE_2D. }
    if GLFeatures.EnableFixedFunction and GLVersion.Mesa then
      glPushAttrib(GL_ENABLE_BIT);
    {$endif}

    UncompressedGLFormatAndInternalFormat(Image, ImageFormat, ImageInternalFormat, ImageSwizzle);
    glTexImage2D(Target, Level, ImageInternalFormat,
      Image.Width, Image.Height, 0, ImageFormat, ImageGLType(Image),
      Image.RawPixels);
    LoadedSize := LoadedSize + Image.Size;

    if GLFeatures.TextureModernFormatsAndSwizzle then
    begin
      { We cannot use cubemap side as swizzle target,
        we need to use GL_TEXTURE_CUBE_MAP,
        following https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexParameter.xhtml . }
      SwizzleTarget := Target;
      if TargetIsCubeMapSide(SwizzleTarget) then
        SwizzleTarget := GL_TEXTURE_CUBE_MAP;
      glTexParameteriv(SwizzleTarget, GL_TEXTURE_SWIZZLE_RGBA, @ImageSwizzle);
    end;

    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction and GLVersion.Mesa then
      glPopAttrib;
    {$endif}
  finally AfterUnpackImage(Image) end;
end;

{ Load the image, loading also mipmaps, generating mipmaps for all the levels.
  As mipmaps only make sense for power-of-2 images,
  this also resizes original image to be power-of-2, if necessary.

  This is similar to libGLU gluBuild2DMipmaps.

  Note that mipmaps are done by our code, on CPU.
  You should prefer using OpenGL(ES) GenerateMipmap feature,
  available on modern OpenGL and on all OpenGLES >= 2,
  than this routine.

  Target has the same meaning as for glTexImage2D.

  LoadedSize will be increased. }
procedure CastleBuild2DMipmaps(const Target: TGLenum; const Image: TCastleImage; var LoadedSize: Int64);
const
  Interpolation = riBilinear;
var
  W, H, Level: Cardinal;
  NewImage: TCastleImage;
begin
  (* For history: implementation using libGLU:

  BeforeUnpackImage(Image);
  try
    gluBuild2DMipmaps(Target, ImageInternalFormat,
      Image.Width, Image.Height, ImageFormat, ImageGLType(Image),
      Image.RawPixels);
    LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(Image);
  finally AfterUnpackImage(Image) end;
  *)

  if Image.Depth <> 1 then
    raise ETextureLoadError.Create('Cannot generate mipmaps on CPU for 3D texture (depth <> 1)');

  WritelnWarning('CastleBuild2DMipmaps', 'Generating mipmaps on CPU for texture "%s". This indicates very old GPU (not capable of glGenerateMipmap), and will be slow.', [
    URIDisplay(Image.URL)
  ]);

  Level := 0;
  W := ResizeToTextureSize(Image.Width , tsScalablePowerOf2);
  H := ResizeToTextureSize(Image.Height, tsScalablePowerOf2);

  // WritelnLog('CastleBuild2DMipmaps', Format('Resizing 2D texture "%s" from %dx%d to %dx%d', [
  //   URIDisplay(Image.URL),
  //   Image.Width,
  //   Image.Height,
  //   W,
  //   H
  // ]));

  NewImage := Image.MakeResized(W, H, riBilinear);
  try
    repeat
      CastleTexImage2D(Target, Level, NewImage, LoadedSize);

      if not ((W > 1) or (H > 1)) then Break; // no size > 1 ? then finish
      if W > 1 then W := W div 2;
      if H > 1 then H := H div 2;
      Inc(Level);

      // WritelnLog('CastleBuild2DMipmaps', Format('Resizing 2D texture "%s" from %dx%d to %dx%d', [
      //   URIDisplay(NewImage.URL),
      //   NewImage.Width,
      //   NewImage.Height,
      //   W,
      //   H
      // ]));
      NewImage.Resize(W, H, riBilinear);
    until false;
  finally FreeAndNil(NewImage) end;
end;

{ 2D texture loading --------------------------------------------------------- }

function LoadGLTexture(const image: TEncodedImage;
  const Filter: TTextureFilter; const Wrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage; const GUITexture: boolean): TGLTextureId;
begin
  glGenTextures(1, @result);
  LoadGLGeneratedTexture(result, image, Filter, Wrap, CompositeForMipmaps, GUITexture);
end;

function LoadGLTexture(const URL: string;
  const Filter: TTextureFilter; const Wrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage; const GUITexture: boolean): TGLTextureId;
var
  Image: TEncodedImage;
begin
  Image := LoadTextureImage(URL);
  try
    Result := LoadGLTexture(Image, Filter, Wrap, CompositeForMipmaps, GUITexture);
  finally Image.Free end;
end;

{ Load Image through glCompressedTexImage2D[ARB].
  This checks existence of OpenGL extensions for Image compression format,
  and checks Image sizes.
  It also takes care of pixel packing, although actually nothing needs
  be done about it when using compressed textures.

  Level = 0 for base (not a mipmap sublevel) image.

  @raises(ECannotLoadCompressedTexture If texture size is bad or necessary
    OpenGL extensions (for the image compression type) are missing.) }
procedure glCompressedTextureImage2D(Image: TGPUCompressedImage; Level: TGLint;
  var LoadedSize: Int64);
begin
  if not (Image.Compression in GLFeatures.TextureCompression) then
    raise ECannotLoadCompressedTexture.CreateFmt('Cannot load textures compressed using %s, your OpenGL/OpenGLES does not support the necessary extensions',
      [TextureCompressionInfo[Image.Compression].Name]);

  if TextureCompressionInfo[Image.Compression].RequiresPowerOf2 and
     not IsTextureSized(Image, tsRequiredPowerOf2) then
    raise ECannotLoadCompressedTexture.CreateFmt('Cannot load compressed textures: texture size is %d x %d, it''s not a power-of-2. Textures compressed with %s must be provided with power-of-2 sizes.',
      [Image.Width, Image.Height, TextureCompressionInfo[Image.Compression].Name]);

  { Pixel packing parameters (stuff changed by Before/AfterUnpackImage)
    doesn't affect loading compressed textures, as far as I understand.
    So no need to call it. }
  glCompressedTexImage2D(GL_TEXTURE_2D, Level, ImageGLInternalFormat(Image),
    Image.Width, Image.Height, 0, Image.Size, Image.RawPixels);
  LoadedSize := LoadedSize + Image.Size;
end;

procedure CalculateImageRightSize(
  const Image: TCastleImage; const GUITexture: boolean;
  out ImageRightSize: TCastleImage; out ImageRightSizeFree: boolean);
var
  Sizing: TTextureSizing;
begin
  if GUITexture then
    Sizing := tsAny else
    Sizing := tsScalablePowerOf2;
  if IsTextureSized(Image, Sizing) then
  begin
    ImageRightSize := Image;
    ImageRightSizeFree := false;
  end else
  begin
    ImageRightSize := ResizeToTextureSize(Image, Sizing);
    ImageRightSizeFree := true;
  end;
end;

procedure LoadGLGeneratedTexture(const TextureId: TGLTextureId;
  const Image: TEncodedImage; Filter: TTextureFilter; const Wrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage; const GUITexture: boolean);
var
  LoadedSize: Int64;

  procedure LoadNormal(const Image: TCastleImage);
  begin
    CastleTexImage2D(GL_TEXTURE_2D, 0, Image, LoadedSize);
  end;

  { Check should we load mipmaps from Composite image. Load them, if yes.

    If LoadBase this also loads the base image (mipmap level 0).

    Note that I observed a bug on NVidia GeForce FX 5200, with various driver
    versions on both Linux 32 bit, 64 bit, and Windows 32 bit:
    you cannot load the base texture level (0) *after* loading the mipmaps.
    Doing so results in mipmaps being ignored, and seemingly GL_NEAREST
    minification filtering used (ignoring our set Filter.Minification).
    This could be easily observed with
    demo_models/x3d/tex_visualize_mipmaps.x3dv,
    switching to viewpoint like "Mipmaps from DDS" or "Colored mipmaps from DDS"
    --- you could clearly see that mipmaps are ignored and ugly nearest filtering
    gets used.
    Using LoadBase automatically workarounds this. }
  function LoadMipmapsFromComposite(Composite: TCompositeImage; LoadBase: boolean): boolean;
  var
    I, FromLevel: Integer;
    ImageRightSize: TCastleImage;
    ImageRightSizeFree: boolean;
  begin
    Result := (Composite <> nil) and Composite.Mipmaps;
    if not Result then Exit;

    if Composite.CompositeType <> ctTexture then
    begin
      WritelnWarning('Texture', 'Composite (DDS, KTX...) image contains mipmaps, but not for 2D texture');
      Exit(false);
    end;

    if not GLFeatures.TextureMaxLevel then
    begin
      WritelnWarning('Texture', 'Cannot load composite (DDS, KTX...) image containing mipmaps, because OpenGL(ES) does not support GL_TEXTURE_MAX_LEVEL');
      Exit(false);
    end;

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, Composite.MipmapsCount - 1);
    if LoadBase then
      FromLevel := 0
    else
      FromLevel := 1;
    for I := FromLevel to Composite.MipmapsCount - 1 do
      if Composite.Images[I] is TCastleImage then
      begin
        CalculateImageRightSize(TCastleImage(Composite.Images[I]), GUITexture,
          ImageRightSize, ImageRightSizeFree);
        try
          CastleTexImage2D(GL_TEXTURE_2D, I, ImageRightSize, LoadedSize);
        finally
          if ImageRightSizeFree then
            FreeAndNil(ImageRightSize)
          else
            ImageRightSize := nil;
        end;
      end else
      if Composite.Images[I] is TGPUCompressedImage then
        glCompressedTextureImage2D(TGPUCompressedImage(Composite.Images[I]), I, LoadedSize)
      else
        raise EInvalidImageForOpenGLTexture.CreateFmt('Cannot load to OpenGL texture image class %s', [Image.ClassName]);
  end;

  procedure LoadMipmapped(const Image: TCastleImage);
  begin
    if not LoadMipmapsFromComposite(CompositeForMipmaps, true) then
    begin
      if HasGenerateMipmap then
      begin
        CastleTexImage2D(GL_TEXTURE_2D, 0, Image, LoadedSize);
        { hardware-accelerated mipmap generation }
        GenerateMipmap(GL_TEXTURE_2D);
        LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(Image);
      end else
      begin
        CastleBuild2DMipmaps(GL_TEXTURE_2D, Image, LoadedSize);
      end;
    end;
  end;

var
  ImageRightSize: TCastleImage;
  ImageRightSizeFree: boolean;
begin
  if GUITexture then
    Filter.DisableMipmaps;

  LoadedSize := 0;

  { bind the texture, set min, mag filters and wrap parameters }
  glBindTexture(GL_TEXTURE_2D, TextureId);
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Wrap.Data[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Wrap.Data[1]);

  { give the texture data }
  if Image is TCastleImage then
  begin
    CalculateImageRightSize(TCastleImage(Image), GUITexture,
      ImageRightSize, ImageRightSizeFree);
    try
      { Load uncompressed }
      if Filter.NeedsMipmaps then
        LoadMipmapped(ImageRightSize)
      else
        LoadNormal(ImageRightSize);

      TextureMemoryProfiler.Allocate(TextureId, Image.URL, Image.ClassName, LoadedSize,
        Filter.NeedsMipmaps, ImageRightSize.Width, ImageRightSize.Height, 1);
    finally
      if ImageRightSizeFree then
        FreeAndNil(ImageRightSize)
      else
        ImageRightSize := nil;
    end;
  end else
  if Image is TGPUCompressedImage then
  begin
    { Load compressed }
    glCompressedTextureImage2D(TGPUCompressedImage(Image), 0, LoadedSize);

    if Filter.NeedsMipmaps then
    begin
      if not LoadMipmapsFromComposite(CompositeForMipmaps, false) then
      begin
        // GenerateMipmap(GL_TEXTURE_2D);
        // LoadedSize := LoadedSize + TTextureMemoryProfiler.MipmapsSize(Image);

        { On Android doing GenerateMipmap on compressed images
          definitely fails. Tested on:
          - Qualcomm/Adreno (Nexus 5), OpenGL ES 3.0 V@127.0 AU@
          - PowerVR (Pentagram Tab 7.6)
          - Qualcomm/Adreno (older phone from PW)
          and GenerateMipmap there definitely doesn't work.

          On Nexus 5 it even makes GL_INVALID_OPERATION.
          (On PowerVR my testcase was doing GL_OUT_OF_MEMORY, so it posibly
          masked earlier GL_INVALID_OPERATION. And on "older phone from PW"
          I didn't run -dDEBUG version. IOW, it is possible that *everywhere*
          we had GL_INVALID_OPERATION error.)

          OTOH, on desktop OpenGL, with DXT5 textures,
          4.4.0 NVIDIA 340.96 on Linux, GenerateMipmap on DXT5 indeed works.
          No GL error (and glGetError is checked),
          and the mipmaps are really generated (commenting out makes them white).

          Docs (for OpenGL, GLES 2.0, GLES 3.2
            https://www.khronos.org/opengles/sdk/docs/man/xhtml/glGenerateMipmap.xml
            https://www.khronos.org/opengles/sdk/docs/man/
            https://www.khronos.org/opengles/sdk/docs/man32/
          ) all say:
            GL_INVALID_OPERATION is generated if the zero level array is stored in a compressed internal format.

          This suggests that it's safest to assume that GenerateMipmap DOES NOT
          work on compressed textures, and (if mipmaps were not provided explicitly)
          we cannot use them. }

        { Update GL_TEXTURE_MIN_FILTER, since we already initialized it earlier. }
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        { Warn, so that developer is aware of it.
          Better solution may be to
          - compress textures along with mipmaps.
          - or not request mipmaps (filtering = linear from start).
          Without doing anything, using compressed textures will be inconsistent
          with uncompressed, since the analogous code will succeed in creating
          mipmaps from uncompressed texture data. }
        WritelnWarning('Texture', 'Generating mipmaps for compressed textures not possible. Turning off mipmaps for ' + Image.URL);
      end;
    end;

    TextureMemoryProfiler.Allocate(TextureId, Image.URL, Image.ClassName, LoadedSize,
      Filter.NeedsMipmaps, Image.Width, Image.Height, 1);
  end else
    raise EInvalidImageForOpenGLTexture.CreateFmt('Cannot load to OpenGL texture image class %s', [Image.ClassName]);
end;

{ GLDecompressTexture ------------------------------------------------------------- }

function GLDecompressTexture(const Image: TGPUCompressedImage): TCastleImage;

{$ifndef OpenGLES}
var
  Tex: TGLTextureId;
  LoadedSize: Int64;
begin
  glGenTextures(1, @Tex);
  glBindTexture(GL_TEXTURE_2D, Tex);

  { Testcase that fails on Radeon chantal (ATI Radeon X1600) Linux:
    demo_models/textures/marble_with_mipmaps_s3tc.dds

    No problem on NVidia (fpc 2.2.2 kocury/linux/32, fpc 2.2.4 kocury/linux/32),
    and no problem on macOS with the same GPU (also chantal, 32bit, fpc 2.2.4).
    So I'm assuming it's fglrx-specific bug. }
  if GLVersion.Fglrx and ( (Image.Width < 4) or (Image.Height < 4) ) then
    raise ECannotDecompressTexture.CreateFmt('Cannot decompress texture: fglrx (proprietary Radeon drivers on Linux) may awfully crash when one of texture sizes is smaller than 4, and your texture size is %d x %d',
      [Image.Width, Image.Height]);

  try
    LoadedSize := 0;
    glCompressedTextureImage2D(Image, 0, LoadedSize);
    TextureMemoryProfiler.Allocate(Tex, Image.URL, Image.ClassName,
      LoadedSize, false, Image.Width, Image.Height, 1);
  except
    { catch ECannotLoadCompressedTexture and change it to ECannotDecompressTexture }
    on E: ECannotLoadCompressedTexture do
      raise ECannotDecompressTexture.Create('Cannot decompress texture: ' + E.Message);
  end;

  if Image.HasAlpha then
    Result := TRGBAlphaImage.Create(Image.Width, Image.Height, Image.Depth) else
    Result := TRGBImage.Create(Image.Width, Image.Height, Image.Depth);

  SaveTextureContents(Result, Tex);
  glFreeTexture(Tex);
end;

{$else}
begin
  { glGetTexImage is not available on OpenGLES, so the above trick
    will not work on OpenGLES. TODO: We need more elaborate way to do this
    (namely, we need to simply draw the texture to an uncompressed texture,
    e.g. using TGLRenderToTexture. }
  raise ECannotDecompressTexture.Create('Cannot decompress texture on OpenGL ES');
  Result := nil; // get rid of warning
end;
{$endif}

procedure SaveTextureContents(const Image: TCastleImage; const Texture: TGLTextureId);
{$ifndef OpenGLES}
begin
  BeforePackImage(Image);
  try
    glBindTexture(GL_TEXTURE_2D, Texture);
    glGetTexImage(GL_TEXTURE_2D, 0,
      ImageGLFormat(Image), ImageGLType(Image), Image.RawPixels);
  finally AfterPackImage(Image) end;
end;
{$else}
begin
  { glGetTexImage is not available on OpenGLES }
  raise ECannotSaveTextureContents.Create('Cannot SaveTextureContents on OpenGL ES');
end;
{$endif}

{$endif read_implementation}
