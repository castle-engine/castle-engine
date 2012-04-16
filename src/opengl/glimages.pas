{
  Copyright 2001-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Using images in OpenGL (as textures and as normal images).

  This unit implements various OpenGL utilities relates to handling images
  in TCastleImage classes. This includes
  @unorderedList(
    @item(Loading images as OpenGL textures.
      A lot of utilities: for 2D textures (see LoadGLTexture),
      cube maps (see glTextureCubeMap), 3D textures (see glTextureImage3D).
      These function can be treated like a high-level wrappers around
      glTexImage2D and analogous calls,
      setting also common related texture parameters, generating mipmaps etc.)

    @item(Drawing TCastleImage instance in OpenGL buffer.
      Wrapper around glDrawPixels and related things.
      See ImageDraw.)

    @item(Screen saving, that is saving OpenGL buffer contents to TCastleImage instance.
      Wrapper around glReadPixels and related things.
      See TCastleWindowBase.SaveScreen, based on SaveScreen_NoFlush in this unit.)

    @item(Rendering straight to texture (see TGLRenderToTexture class).
      This is our abstraction
      over OpenGL framebuffer or glCopyTexSubImage.)
  )

  See @link(Images) unit for functions to load, save, process
  images. Images unit is the non-OpenGL-related helper of this unit.

  This unit hides from you some specifics of OpenGL images handling :

  @unorderedList(
    @item(
      Don't worry about pixel store alignment, this unit handles it for you.

      Since internally our image formats have no alignment, we call
      something like
      @preformatted(
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
)
      when appropriate. Actually, we use @code(Before/After Pack/Unpack NotAlignedImage)
      procedures from CastleGLUtils unit. )

    @item(
      Don't worry about texture size being power of 2, or about maximum texture
      size.

      This unit checks OpenGL capabilities, and if needed will scale your
      texture to have power of 2 sizes that fit within OpenGL
      GL_MAX_TEXTURE_SIZE limit.

      @bold(Notes about power-of-two constraint:) Newer OpenGL allows using
      non-power-of-two textures when extension ARB_texture_non_power_of_two
      is supported (or version is >= 2.0). We can use this --- the code is
      ready, just uncomment it in TextureNonPowerOfTwo.
      But by default we don't use this, that is: we will scale textures
      to be power of 2, even if we have ARB_texture_non_power_of_two or
      new OpenGL.

      Reason: well, looks like every vendor screwed it up. Embarassing.

      @unorderedList(
        @item(On Mesa, small artifacts occur (strange cracks appears on
          non-power-of-2 texture, see inlined_textures.wrl in demo_models).
          That's the @italic(best) result compared to other vendors, see below.)

        @item(On ATI (fglrx on Linux on Mac Book Pro),
          the extension is not present and OpenGL 2.0 entry points
          are not fully present. (Although GL_VERSION claims 2.1 version,
          glBlendEquationSeparate entry point from GL 2.0 is not present.)
          For safety, I just assume that ATI is not able to make OpenGL 2.0,
          so no extension and no 2.0 means textures must be power of two.

          Just for kicks, I tried anyway to pass texture 4x3 (after all,
          GL_VERSION claims 2.1 so this should be supported), and... uhh...
          libGL segfaulted. Congrats, ATI.)

        @item(I thought: Mesa is always buggy and Radeon should not be treated
          as OpenGL 2.0, so I can live with this. Now let's try
          great NVidia, they will for sure do this right. Well, yes, it works
          correctly on NVidia (GeForce FX 5200)... but the slowdown is
          enormous. For trivial box with 4x3 texture (see
          inlined_textures.wrl in demo_models), that normally runs with
          virtually infinite speed, suddenly the speed becomes like 1 frame per second !
          Other example when the slowdown is enormous: castle/levels/castle_hall.wrl

          You can test yourself (with view3dscene using
          inlined_textures.wrl model;
          just compile view3dscene to use non-power-of-2 textures;
          contact me if you want a binary compiled as such for testing.)

          Such slowdown is not acceptable, I prefer to loose texture quality
          by scaling them to powers of 2 in this case...)
      )
    )
  )

  Internally, this unit depends on the knowledge of how pixels are stored
  in TRGBImage and similar classes. For example we know that
  TRGBImage stores image in format that OpenGL would call "GL_RGB
  using GL_UNSIGNED_BYTE, without any alignment". Which means that
  Image.RGBPixels is a pointer to array like
  @code(packed array[0..Image.Height - 1,  0..Image.Width - 1] of TVector3Byte).
  So we have rows of TVector3Byte structures, stored from lowest row to
  highest row.

  Routines in this unit that take TCastleImage or TEncodedImage paramater
  are limited to TextureImageClassesAll (for routines dealing with textures)
  or PixelsImageClasses (for routines dealing with pixel buffer, like
  glReadPixels, glDrawPixels).
  Note that *not everywhere* this is checked (especially if you
  compile with -dRELEASE) so just be sure that you're always passing
  only image instances of correct class (e.g. using
  InImageClasses(MyImage, PixelsImageClasses)).
}
unit GLImages;

interface

uses GL, GLU, GLExt, SysUtils, Images, VectorMath, CastleGLUtils, Videos, DDS;

const
  PixelsImageClasses: array [0..3] of TCastleImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

{ These functions return appropriate GL_xxx format and type
  for given TCastleImage descendant. If you will pass here Img
  that is not a descendant of one of TextureImageClassesAll
  or PixelsImageClasses, they will return GL_INVALID_ENUM
  (which is for sure different than any valid value like GL_RGB, GL_RGBA
  in OpenGL API).

  Note that some routines, like ImageDraw, do not check that
  an image is of appropriate class (e.g. PixelsImageClasses for ImageDraw).
  Instead they may use ImageGLFormat results blindly, for speed.
  So when you try to draw an invalid image class, they may
  pass GL_INVALID_ENUM to OpenGL, and you get OpenGL errors.

  ImageGLInternalFormat works with TS3TCImage classes also, returning
  appropriate GL_COMPRESSED_*_S3TC_*_EXT, suitable for glCompressedTexImage2D.

  @groupBegin }
function ImageGLFormat(const Img: TEncodedImage): TGLenum;
function ImageGLInternalFormat(const Img: TEncodedImage): TGLenum;
function ImageGLType(const Img: TCastleImage): TGLenum;
{ @groupEnd }

{ Loading images ------------------------------------------------------------- }

{ This calls @link(Images.LoadImage) and creates a display-list with
  an ImageDraw call for this image.
  Image will be loaded with AllowedImageClasses = LoadAsClass and
  ForbiddenConvs = LoadForbiddenConvs, see @link(Images.LoadImage)
  for description what these parameters mean.
  LoadAsClass may contain only classes present in PixelsImageClasses. }
function LoadImageToDisplayList(const FileName: string;
  const LoadAsClass: array of TCastleImageClass;
  const LoadForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TGLuint; overload;

{ Draws the image as 2D on screen.
  This calls OpenGL glDrawPixels command on this image.

  Don't worry about OpenGL's UNPACK_ALIGNMENT,
  we will take care here about this
  (changing it and restoring to previous value if necessary). }
procedure ImageDraw(const Image: TCastleImage);

{ Same as @link(ImageDraw), but will draw only RowsCount rows
  starting from Row0. }
procedure ImageDrawRows(const Image: TCastleImage; Row0, RowsCount: integer);

{ Draw a part of the image by glDrawPixels.

  Part of the image starts from X0, Y0 (where 0, 0 is the left/bottom
  pixel, i.e. where the normal ImageDraw starts) and spans Width/Height.
  Overloaded version without Width, Height parameters just draws the
  whole remaining image.

  Too large X0, Y0, Width, Height values are automatically detected
  and cut as appropriate, so you can safely pass any large values here.

  This will cut of some columns at the left/right and bottom/top
  by using tricks with OpenGL pixel store unpack (don't worry, the whole
  state of pixel store unpack will be taken care of and preserved
  by this). So it works fast.

  @groupBegin }
procedure ImageDrawPart(const image: TCastleImage;
  const X0, Y0, Width, Height: Cardinal); overload;
procedure ImageDrawPart(const image: TCastleImage;
  const X0, Y0: Cardinal); overload;
{ @groupEnd }

{ This creates new display list with a call to ImageDraw(Img) inside. }
function ImageDrawToDisplayList(const img: TCastleImage): TGLuint;

function ImageDrawPartToDisplayList(
  const Image: TCastleImage;
  const X0, Y0, Width, Height: Cardinal): TGLuint;

{ Saving screen to TRGBImage ----------------------------------- }

{ Note about saving images from GL_FRONT:
  in general, it's not predictable to save image from GL_FRONT OpenGL buffer
  (or any part of front buffer). That's because when our window will
  be covered by other window (of other programs or our own window
  (like other instances of TCastleWindowBase or dialog windows produced
  by TCastleWindowBase.FileDialog, in case you use CastleWindow unit)) then
  glReadPixels will return pixel array filled with contents of
  *those other windows*.

  Prefixing functions below, SaveScreen_NoFlush, with things like
    TCastleWindowBase.FlushRedisplay, or even
    TCastleWindowBase.PostRedisplay + TCastleWindowBase.FlushRedisplay, or even
    an explicit call to Draw procedure and an explicit call
      to SwapBuffers / glFlush, or oven
    only an explicit call to Draw procedure (without glFlush/swapbuffers)
  ... DOES NOT help. If we are covered by some other
  window, glReadPixels on front buffer will simply return invalid
  contents.

  This means that the only really reliable way to save screen contents
  is to draw something to BACK buffer and (without doing any swapbuffers)
  do SaveScreen_NoFlush(GL_BACK) (where ReadBuffer may be some part of back
  buffer, not necessarily only simple GL_BACK). This is only possible
  if you have double-buffered window, of course.
}

{ Save the current color buffer contents to image.
  Does glReadBuffer(ReadBuffer) and then glReadPixels with appropriate
  parameters.

  The suffix "noflush" in the name is there to remind you that this
  function grabs the current buffer contents. Usually you want to
  call something like @link(TCastleWindowBase.FlushRedisplay) right before grabbing
  from the front buffer (which isn't reliable anyway), or redraw
  (like by TCastleWindowBase.EventDraw) right before grabbing from the back buffer.

  See TCastleWindowBase.SaveScreen for more friendly ways to capture the screen.

  Version with ImageClass can save to any image format from PixelsImageClasses.

  Version with TCastleImage instance just uses this instance to save the image.
  You must pass here already created TCastleImage instance, it's class,
  Width and Height will be used when saving.

  We always take explicit Width, Height (from parameter, or from Image.Width,
  Image.Height). Guessing screen size automatically doesn't really work,
  as the viewport may change when we use custom viewports.

  @groupBegin }
function SaveScreen_NoFlush(xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TRGBImage; overload;

function SaveScreen_NoFlush(
  ImageClass: TCastleImageClass;
  xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TCastleImage; overload;

procedure SaveScreen_NoFlush(
  Image: TCastleImage;
  xpos, ypos: integer;
  ReadBuffer: TGLenum); overload;
{ @groupEnd }

{ Save the screen, except it may make the width larger,
  to make it divisible by four,
  to workaround Radeon bug TGLVersion.BuggyDrawOddWidth.

  If GLVersion.BuggyDrawOddWidth then it will eventually
  enlarge the Width to make it a multiple of 4.
  Possibly, multiple of 2 would be enough, but you don't want to risk
  with Radeon bugs...

  You can draw this image by normal ImageDraw, although you risk
  then that you will see an additional column at the right filled
  with garbage colors (due to enlarging of screen done here).
  Ideally, it would be best to draw this only by
  ImageDrawPart(0, 0, Width (given here, not Image.Width), Image.Height)
  but it may not be possible --- again, thanks to TGLVersion.BuggyDrawOddWidth. }
function SaveAlignedScreen_NoFlush(
  const XPos, YPos: Integer; Width: Cardinal; const Height: Cardinal;
  const ReadBuffer: TGLenum): TRGBImage;

{ Captures current screen and creates a display list to draw it in the future.

  Capturing the screen is done by SaveScreen_NoFlush,
  drawing of the image is done normally,
  and placed in a display list.

  Actually, this is more complicated
  (we capture the screen with SaveAlignedScreen_NoFlush,
  to workaround GLVersion.BuggyDrawOddWidth bug,
  we also have to actually draw it a little larger),
  but the intention of this procedure is to
  completely hide this completexity from you. }
function SaveScreen_ToDisplayList_NoFlush(
  const XPos, YPos: Integer; const Width, Height: Cardinal;
  const ReadBuffer: TGLenum): TGLuint;

{ ----------------------------------------------------------------------
  Adjusting image size to load them as textures. }

{ Resize the image to a size accepted as GL_TEXTURE_2D texture size
  for OpenGL. It tries to resize to a larger size, not smaller,
  to avoid losing image
  information. Usually you don't have to call this, LoadGLTexture*
  functions call it automatically when needed.

  @groupBegin }
procedure ResizeForTextureSize(var r: TCastleImage);
function ResizeToTextureSize(const r: TCastleImage): TCastleImage;
{ @groupEnd }

{ Does image have proper size for OpenGL texture (GL_TEXTURE_2D).
  That is, for passing to glTexImage2D for GL_TEXTURE_2D target.
  This checks glGet(GL_MAX_TEXTURE_SIZE),
  so requires initialized OpenGL context. }
function IsTextureSized(const r: TEncodedImage): boolean;

function IsTextureSized(const Width, Height: Cardinal): boolean;
procedure ResizeToTextureSize(var Width, Height: Cardinal);

function IsCubeMapTextureSized(const Size: Cardinal): boolean;
function ResizeToCubeMapTextureSize(const Size: Cardinal): Cardinal;

{ Texture wrapping modes ----------------------------------------------------- }

type
  { }
  TTextureWrap2D = array [0..1] of TGLenum;
  TTextureWrap3D = array [0..2] of TGLenum;

operator = (const W1, W2: TTextureWrap2D): boolean;
operator = (const W1, W2: TTextureWrap3D): boolean;

const
  Texture2DRepeat: TTextureWrap2D = (GL_REPEAT, GL_REPEAT);

{ Return wrap GL_CLAMP_TO_EDGE in both directions. }
function Texture2DClampToEdge: TTextureWrap2D;

{ Loading textures ----------------------------------------------------------- }

type
  { }
  ETextureLoadError = class(Exception);
  ECannotLoadS3TCTexture = class(ETextureLoadError);
  EInvalidImageForOpenGLTexture = class(ETextureLoadError);

function TextureMinFilterNeedsMipmaps(const MinFilter: TGLenum): boolean;

{ Load new texture to OpenGL. Generates new texture number by glGenTextures,
  then binds this texture, and loads it's data.

  Takes care of UNPACK_ALIGNMENT inside (if needed, we'll change it and
  later revert back, so that the texture is correctly loaded).

  Sets texture minification, magnification filters and wrap parameters.

  Changes currently bound texture to this one (returned).

  GrayscaleIsAlpha is meaningful only if the image is TGrayscaleImage class.
  If GrayscaleIsAlpha is @false, then we'll load GL_LUMINANCE texture
  (this basically behaves like normal RGB texture, except that it has
  only one channel and stores grayscale colors). If GrayscaleIsAlpha is @true,
  the texture will be loaded as GL_ALPHA texture (it will modify only the
  fragments alpha value, it doesn't have any "color" in the normal sense,
  it's only for opacity).

  If mipmaps will be needed (this is decided looking at MinFilter)
  we will load them too.

  @orderedList(
    @item(
      As a first try, if DDSForMipmaps is non-nil
      and has mipmaps (DDSForMipmaps.Mipmaps), we will load these mipmaps.
      DDSForMipmaps must be a normal 2D texture (DDSType = dtTexture).

      Otherwise, we'll try to generate mipmaps, using various OpenGL mechanisms.)

    @item(
      If GenerateMipmap functionality will be required to create mipmaps,
      but is not available on this OpenGL implementation,
      we will change MinFilter to simple GL_LINEAR and make OnWarning.
      So usually you just don't have to worry about this.
      Note that current implementation requires GenerateMipmap functionality
      only for S3TC textures, for normal uncompressed textures we can
      generate mipmaps on CPU or through SGIS_GENERATE_MIPMAP extension.)
  )

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason.
    This includes ECannotLoadS3TCTexture if the S3TC texture cannot be
    loaded for whatever reason.
    This includes EInvalidImageForOpenGLTexture if Image class is invalid
    for an OpenGL texture.)

  @groupBegin }
function LoadGLTexture(const image: TEncodedImage;
  MinFilter, MagFilter: TGLenum;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean = false;
  DDSForMipmaps: TDDSImage = nil): TGLuint; overload;

function LoadGLTexture(const FileName: string;
  MinFilter, MagFilter: TGLenum;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean = false;
  DDSForMipmaps: TDDSImage = nil): TGLuint; overload;
{ @groupEnd }

{ Load OpenGL texture into already reserved texture number.
  It uses existing OpenGL texture number (texnum). Everything else
  works exactly the same as LoadGLTexture.

  You can also use this to set "default unnamed OpenGL texture" parameters
  by passing TexNum = 0.

  @raises(ETextureLoadError Raised in the same situations as LoadGLTexture.)

  @groupBegin }
procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TEncodedImage;
  MinFilter, MagFilter: TGLenum;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean = false;
  DDSForMipmaps: TDDSImage = nil); overload;
{ @groupEnd }

type
  { Sequence of OpenGL textures to be played as a video. }
  TGLVideo = class
  private
    FItems: array of TGLuint;
    FCount: Integer;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FFramesPerSecond: Single;
  public
    { Constructor that initializes video from TVideo class.

      TVideo passed here must be already @link(TVideo.Loaded Loaded).

      Note that this class doesn't descend
      or keep reference to TVideo instance. The idea is that after
      creating TGLVideo instance, you can often free original TVideo
      instance (if you care only about playing the movie). This can
      conserve memory greatly, as TVideo keeps all frames in the memory,
      and so is rather memory-costly.
      (Actually, TGLVideo itself may eat a lot of texture memory,
      so be careful with large videos anyway.) }
    constructor Create(Video: TVideo;
      MinFilter, MagFilter: TGLenum;
      const Anisotropy: TGLfloat;
      const Wrap: TTextureWrap2D);

    destructor Destroy; override;

    property Count: Integer read FCount;
    function IndexFromTime(const Time: Single): Integer;
    function GLTextureFromTime(const Time: Single): TGLuint;

    { See TVideo.FramesPerSecond. }
    property FramesPerSecond: Single read FFramesPerSecond;

    { See TVideo.TimeLoop. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop;

    { See TVideo.TimeBackwards. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards;
  end;

{ Comfortably load all six cube map texture images.
  Think about this as doing glTexImage2D(Side, ...) for each cube side.
  It takes care of (almost?) everything you need to prepare OpenGL cube map
  texture.

  It automatically takes care to adjust the texture size to
  appropriate size, honoring the "power of two" requirement and
  the GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB limit of OpenGL. So texture image
  may be resized (preferably up) internally before loading.
  Although, if texture is S3TC compressed, we cannot resize it
  --- so ECannotLoadS3TCTexture will be raised if texture is not appropriate
  size.

  It takes care about OpenGL unpack parameters. Just don't worry about it.

  If mipmaps are requested:

  @orderedList(
    @item(First of all, if DDSForMipmaps is non-nil and has mipmaps defined,
      we will load them from this DDS image.
      DDSForMipmaps must have DDSType = dtCubeMap.)

    @item(Otherwise, we'll try to generate images using OpenGL GenerateMipmap.)

    @item(As a last resort, if GenerateMipmap is not available,
      we will fallback to generating mipmaps on CPU by good old
      gluBuild2DMipmaps call.)
  )

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason.
    This includes ECannotLoadS3TCTexture if the S3TC texture cannot be
    loaded for whatever reason (not availble S3TC extensions,
    not correct texture size, mipmaps requested and
    DDSForMipmaps/glGenerateMipmap not available).
    This includes EInvalidImageForOpenGLTexture if Image class is invalid
    for an OpenGL texture.)
}
procedure glTextureCubeMap(
  PositiveX, NegativeX,
  PositiveY, NegativeY,
  PositiveZ, NegativeZ: TEncodedImage;
  DDSForMipmaps: TDDSImage;
  Mipmaps: boolean);

{ Comfortably load a 3D texture.
  Think about this as doing glTexImage3D(...) for you.
  It also sets texture minification, magnification filters and creates
  mipmaps if necessary.

  It checks OpenGL 3D texture size requirements, and throws exceptions
  if not satisfied.

  It takes care about OpenGL unpack parameters. Just don't worry about it.

  If MinFilter uses mipmaps, then all mipmap levels will be loaded.

  @orderedList(
    @item(
      As a first try, if DDSForMipmaps is non-nil
      and has mipmaps (DDSForMipmaps.Mipmaps), we will load these mipmaps.
      DDSForMipmaps must be a 3D texture (DDSType = dtVolume).)

    @item(Otherwise, we'll generate mipmaps.

      GenerateMipmap functionality will be required for this.
      When it is not available on this OpenGL implementation,
      we will change MinFilter to simple GL_LINEAR and make OnWarning.
      So usually you just don't have to worry about this.)
  )

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason,
    for example it's size is not correct for OpenGL 3D texture (we cannot
    automatically resize 3D textures, at least for now).
    Or it's compressed (although we support here TEncodedImage,
    OpenGL doesn't have any 3D texture compression available.)) }
procedure glTextureImage3D(const Image: TEncodedImage;
  MinFilter, MagFilter: TGLenum;
  DDSForMipmaps: TDDSImage);

type
  EGenerateMipmapNotAvailable = class(Exception);

{ Is GenerateMipmap avaiable. This checks some GL extensions/versions that
  give us glGenerateMipmap or glGenerateMipmapEXT call, used by GenerateMipmap. }
function HasGenerateMipmap: boolean;

{ Call glGenerateMipmap (or analogous function from some OpenGL extension).

  @raises(EGenerateMipmapNotAvailable If no glGenerateMipmap version
    is available on this OpenGL version. If you don't want to get
    this exception, you can always check HasGenerateMipmap
    before calling this.) }
procedure GenerateMipmap(target: TGLenum);

{ Call glTexParameterf to set GL_TEXTURE_MAX_ANISOTROPY_EXT on given texture
  target.

  Takes care to check for appropriate OpenGL extension (if not present,
  does nothing), and to query OpenGL limit for Anisotropy (eventually
  clamping provided Anisotropy down). }
procedure TexParameterMaxAnisotropy(const target: TGLenum; const Anisotropy: TGLfloat);

{ Decompress S3TC image by loading it to temporary OpenGL texture and
  reading back. So this internally uses current OpenGL context.

  @raises(ECannotLoadS3TCTexture If cannot decompress S3TC, for example
    because we cannot load to OpenGL this S3TC texture (because OpenGL S3TC
    extensions are not available, or such).) }
function GLDecompressS3TC(Image: TS3TCImage): TCastleImage;

type
  EFramebufferError = class(Exception);
  EFramebufferSizeTooLow = class(EFramebufferError);
  EFramebufferInvalid  = class(EFramebufferError);

  TGLRenderToTextureBuffer = (tbColor, tbDepth, tbColorAndDepth, tbNone);

  { Rendering to texture with OpenGL.
    Uses framebuffer (if available), and has fallback to glCopyTexSubImage2D
    for (really) old OpenGL implementations. }
  TGLRenderToTexture = class
  private
    FWidth: Cardinal;
    FHeight: Cardinal;

    FTexture: TGLuint;
    FTextureTarget: TGLenum;
    FCompleteTextureTarget: TGLenum;
    FDepthTextureTarget: TGLenum;
    FBuffer: TGLRenderToTextureBuffer;
    FStencil: boolean;
    FDepthTexture: TGLuint;

    FGLInitialized: boolean;
    Framebuffer, RenderbufferColor, RenderbufferDepth, RenderbufferStencil: TGLuint;

    FramebufferBound: boolean;
    FColorBufferAlpha: boolean;
    FMultiSampling: Cardinal;
  public
    { Constructor. Doesn't require OpenGL context,
      and doesn't initialize the framebuffer.
      You'll have to use GLContextOpen before actually making Render. }
    constructor Create(const AWidth, AHeight: Cardinal);

    destructor Destroy; override;

    { Width and height must correspond to texture initialized width / height.
      You cannot change them when OpenGL stuff is already initialized
      (after GLContextOpen and before GLContextClose or destructor).
      @groupBegin }
    property Width: Cardinal read FWidth write FWidth;
    property Height: Cardinal read FHeight write FHeight;
    { @groupEnd }

    { Texture associated with the rendered buffer image.
      If @link(Buffer) is tbColor or tbColorAndDepth then we will capture
      here color contents. If @link(Buffer) is tbDepth then we will capture
      here depth contents (useful e.g. for shadow maps).
      If If @link(Buffer) is tbNone, this is ignored.

      We require this texture to be set to a valid texture (not 0)
      before GLContextOpen (unless Buffer is tbNone).
      Also, if you later change it,
      be careful to assign here other textures of only the same size and format.
      This allows us to call glCheckFramebufferStatusEXT (and eventually
      fallback to non-stencil version) right at GLContextOpen call, and no need
      to repeat it (e.g. at each RenderBegin).

      Changed by SetTexture. }
    property Texture: TGLuint read FTexture default 0;

    { Target of texture associated with rendered buffer.
      This is GL_TEXTURE2D for normal 2D textures, but may also be
      GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP_POSITIVE_X etc. for
      other texture types.

      Companion to @link(Texture) property, changed together by SetTexture. }
    property TextureTarget: TGLenum read FTextureTarget default GL_TEXTURE_2D;

    { Change @link(Texture) and @link(TextureTarget).

      May be changed also when OpenGL stuff (framebuffer) is already
      initialized. This is useful, as it allows you to reuse framebuffer
      setup for rendering to different textures (as long as other settings
      are Ok, like Width and Height).

      It may even be changed between RenderBegin and RenderEnd.
      In fact, this is advised, if you have to call SetTexture often:
      SetTexture call outside of RenderBegin / RenderEnd causes two
      costly BindFramebuffer calls, that may be avoided when you're
      already between RenderBegin / RenderEnd. }
    procedure SetTexture(const ATexture: TGLuint;
      const ATextureTarget: TGLenum);

    { Bind target of texture associated with rendered color buffer.
      "Bind target" means that it describes the whole texture, for example
      for cube map it should be GL_TEXTURE_CUBE_MAP_ARB. }
    property CompleteTextureTarget: TGLenum
      read FCompleteTextureTarget write FCompleteTextureTarget default GL_TEXTURE_2D;

    { Depth texture used when @link(Buffer) = tbColorAndDepth.
      Note that this is not used when @link(Buffer) = tbDepth
      (the @link(Texture) and TextureTarget are used then).
      This must be set before GLContextOpen, and not modified later
      until GLContextClose. }
    property DepthTexture: TGLuint read FDepthTexture write FDepthTexture;
    property DepthTextureTarget: TGLenum read FDepthTextureTarget write FDepthTextureTarget
      default GL_TEXTURE_2D;

    { Which buffer (color and/or depth) should we catch to the texture.

      @unorderedList(
        @item(tbColor: the @link(Texture) will contain color contents.)
        @item(tbDepth: the @link(Texture) will contain depth contents.)
        @item(tbColorAndDepth: the @link(Texture) will contain color
          contents, the @link(DepthTexture) will contain depth contents.)
        @item(tbNone: we will not capture screen contents to any texture
          at all. This is useful for rendering a screen that you want
          to manually capture to normal memory with glReadPixels
          (see also SaveScreen_NoFlush in this unit or TCastleWindowBase.SaveScreen).
          Be sure to capture the screen before RenderEnd.)
      )

      For tbDepth and tbColorAndDepth, the texture that will receive
      depth contents must have GL_DEPTH_COMPONENT* format,
      and we'll render depth buffer contents to it.

      For tbDepth, if the framebuffer is used (normal on recent GPUs),
      we will not write to the color buffer at all,
      so this is quite optimal for rendering shadow maps.

      This must be set before GLContextOpen, cannot be changed later. }
    property Buffer: TGLRenderToTextureBuffer
      read FBuffer write FBuffer default tbColor;

    { Should we require stencil buffer.

      This is usually safe, as FBO spec even requires that some format
      with stencil buffer must be available.

      However, @italic(this has a high chance to fail if you need
      @link(Buffer) = tbDepth or tbColorAndDepth).
      Reason: on GPU with packed depth and stencil buffer
      (see http://www.opengl.org/registry/specs/EXT/packed_depth_stencil.txt)
      FBO with separate depth and stencil may not be possible.
      And when your texture is GL_DEPTH_COMPONENT, this is a must.
      In the future, we could allow some flag to allow you to use texture
      with GL_DEPTH_STENCIL format, this would work with packed depth/stencil
      (actually, even require it). For now, @italic(it's advised to turn
      off @name when you use @link(Buffer) = tbDepth or tbColorAndDepth). }
    property Stencil: boolean
      read FStencil write FStencil default true;

    { Initialize OpenGL stuff (framebuffer).

      When OpenGL stuff is initialized (from GLContextOpen until
      GLContextClose or destruction) this class is tied to the current OpenGL context.

      @raises(EFramebufferSizeTooLow When required @link(Width) x @link(Height)
        is larger than maximum renderbuffer (single buffer within framebuffer)
        size.)

      @raises(EFramebufferInvalid When framebuffer is used,
        and check glCheckFramebufferStatusEXT fails. This should not happen,
        it means a programmer error. Or "unsupported" result
        of glCheckFramebufferStatusEXT (that is possible regardless of programmer)
        we have a nice fallback to non-FBO implementation.) }
    procedure GLContextOpen;

    { Release all OpenGL stuff (if anything initialized).
      This is also automatically called in destructor. }
    procedure GLContextClose;

    { Begin rendering into the texture. Commands following this will
      render to the texture image.

      When framebuffer is used, it's bound here.

      When framebuffer is not used, this doesn't do anything.
      So note that all rendering will be done to normal screen in this case. }
    procedure RenderBegin;

    { End rendering into the texture.

      When framebuffer is used, this binds the normal screen back.

      When framebuffer is not used, this does actual copying from the
      screen to the texture using glCopyTexSubImage2D. We use
      glCopyTexSubImage2D --- which means texture internal format
      should already be initialized! If you don't have any initial texture data,
      you can always initialize by glTexImage2D with @nil as pointer to data.

      During copying, we may change OpenGL bound 2D texture and read buffer.
      So their values are ignored, and may be changed arbitrarily, by this
      method.

      @param(RenderBeginFollows This allows for an optimizaion,
        to minimize the number of BindFramebuffer calls when you render
        many textures in the row using the same TGLRenderToTexture.
        If @true, then you @bold(must) call RenderBegin after this
        (before drawing anything else to OpenGL).
        We will internally leave framebuffer bound, which means that
        this RenderEnd and the very next RenderBegin will actually do nothing.)
    }
    procedure RenderEnd(const RenderBeginFollows: boolean = false);

    { Generate mipmaps for the texture.
      This will use glGenerateMipmap call, which is actually
      a part of EXT_framebuffer_object extension (or GL core together
      with framebuffer in GL core), so it will always
      raise EGenerateMipmapNotAvailable if framebuffer is not available.

      You should use HasGenerateMipmap and never call this
      if not HasGenerateMipmap, if you don't want to get this exception.

      @raises(EGenerateMipmapNotAvailable If glGenerateMipmap not available.) }
    procedure GenerateMipmap;

    { Color buffer name. Use only when Buffer = tbNone, between GLContextOpen
      and GLContextClose. This is the buffer name that you should pass to
      glReadBuffer (or our SaveScreen_NoFlush), currently it's just
      GL_COLOR_ATTACHMENT0_EXT if we actually have FBO or GL_BACK if not. }
    function ColorBuffer: TGLuint;

    { Do we require color buffer with alpha channel.
      Relevant only when Buffer = tbNone (as in all other cases,
      we do not have the color buffer --- colors either go into some texture
      or are ignored).

      This must be set before GLContextOpen, cannot be changed later. }
    property ColorBufferAlpha: boolean read FColorBufferAlpha write FColorBufferAlpha
      default false;

    { All buffers (color and such) will be created with the
      specified number of samples for multisampling.
      Values greater than 1 mean that multisampling is used, which enables
      anti-aliasing.
      Note that all your textures (in @link(Texture), @link(DepthTexture))
      must be created with the same number of samples.

      Ignored if not GLFBOMultiSampling. }
    property MultiSampling: Cardinal
      read FMultiSampling write FMultiSampling default 1;
  end;

implementation

uses CastleUtils, CastleLog, GLVersionUnit, CastleWarnings, TextureImages;

function ImageGLFormat(const Img: TEncodedImage): TGLenum;
begin
  if Img is TRGBImage then
    Result := GL_RGB else
  if Img is TRGBAlphaImage then
    Result := GL_RGBA else
  if Img is TGrayscaleImage then
    Result := GL_LUMINANCE else
  if Img is TGrayscaleAlphaImage then
    Result := GL_LUMINANCE_ALPHA else
  if Img is TRGBFloatImage then
    Result := GL_RGB else
    Result := GL_INVALID_ENUM;
end;

function ImageGLInternalFormat(const Img: TEncodedImage): TGLenum;
begin
  if Img is TCastleImage then
    Result := TCastleImage(Img).ColorComponentsCount else
  if Img is TS3TCImage then
  begin
    case TS3TCImage(Img).Compression of
      s3tcDxt1_RGB : Result := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
      s3tcDxt1_RGBA: Result := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
      s3tcDxt3     : Result := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
      s3tcDxt5     : Result := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
      else Result := GL_INVALID_ENUM;
    end;
  end else
    Result := GL_INVALID_ENUM;
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
    Result := GL_INVALID_ENUM;
end;

{ Loading images ------------------------------------------------------------- }

function LoadImageToDisplayList(const FileName: string;
  const LoadAsClass: array of TCastleImageClass;
  const LoadForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TGLuint;
var
  Img: TCastleImage;
begin
  Img := LoadImage(FileName, LoadAsClass, LoadForbiddenConvs,
    ResizeToX, ResizeToY);
  try
    Result := ImageDrawToDisplayList(Img);
  finally Img.Free end;
end;

procedure ImageDraw(const Image: TCastleImage);
var UnpackData: TUnpackNotAlignedData;
begin
 BeforeUnpackImage(UnpackData, image);
 try
  with image do
   glDrawPixels(Width, Height, ImageGLFormat(image), ImageGLType(image), RawPixels);
 finally AfterUnpackImage(UnpackData, image) end;
end;

procedure ImageDrawRows(const Image: TCastleImage; Row0, RowsCount: integer);
var UnpackData: TUnpackNotAlignedData;
begin
 BeforeUnpackImage(UnpackData, image);
 try
  with image do
   glDrawPixels(Width, RowsCount, ImageGLFormat(image), ImageGLType(image), Image.RowPtr(Row0));
 finally AfterUnpackImage(UnpackData, image) end;
end;

procedure ImageDrawPart(const image: TCastleImage;
  const X0, Y0, Width, Height: Cardinal);
var
  pixUnpack: TPixelStoreUnpack;
  W, H: cardinal;
begin
  if (X0 >= Image.Width) or
     (Y0 >= Image.Height) then
    Exit; { no need to draw anything }

  SavePixelStoreUnpack(pixUnpack);
  try
    W := Min(Image.Width  - X0, Width );
    H := Min(Image.Height - Y0, Height);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, Image.Width);
    glPixelStorei(GL_UNPACK_SKIP_PIXELS, X0);
    glPixelStorei(GL_UNPACK_SKIP_ROWS, Y0);

    { We always make Save/Load Pixel Store Unpack here, so there's
      no need to use Before/After Unpack NotAligned Image.
      However, we still have to set some alignment. We can just
      set it to 1, this will be always correct. }
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    glDrawPixels(W, H, ImageGLFormat(image), ImageGLType(image), image.RawPixels);
  finally LoadPixelStoreUnpack(pixUnpack) end;
end;

procedure ImageDrawPart(const image: TCastleImage;
  const X0, Y0: Cardinal);
begin
  ImageDrawPart(Image, X0, Y0, MaxInt, MaxInt);
end;

function ImageDrawToDisplayList(const Img: TCastleImage): TGLuint;
begin
  Result := glGenListsCheck(1, 'ImageDrawToDisplayList');
  glNewList(Result, GL_COMPILE);
  try
    ImageDraw(Img);
  finally glEndList end;
end;

function ImageDrawPartToDisplayList(
  const image: TCastleImage; const X0, Y0, Width, Height: Cardinal): TGLuint;
begin
  Result := glGenListsCheck(1, 'ImageDrawPartToDisplayList');
  glNewList(Result, GL_COMPILE);
  try
    ImageDrawPart(Image, X0, Y0, Width, Height);
  finally glEndList end;
end;

{ Saving screen to TRGBImage ------------------------------------------------ }

{ This is the basis for all other SaveScreen* functions below. }
procedure SaveScreen_NoFlush(
  Image: TCastleImage;
  xpos, ypos: integer;
  ReadBuffer: TGLenum);
var
  PackData: TPackNotAlignedData;
begin
  BeforePackNotAlignedRGBImage(packData, Image.width);
  try
    glReadBuffer(ReadBuffer);
    glReadPixels(xpos, ypos, Image.width, Image.height, ImageGLFormat(Image),
      ImageGLType(Image), Image.RawPixels);
  finally AfterPackNotAlignedRGBImage(packData, Image.width) end;
end;

function SaveScreen_NoFlush(
  ImageClass: TCastleImageClass;
  xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TCastleImage;
begin
  Result := ImageClass.Create(width, height);
  try
    SaveScreen_NoFlush(Result, xpos, ypos, ReadBuffer);
  except Result.Free; raise end;
end;

function SaveScreen_NoFlush(
  xpos, ypos, width, height: integer;
  ReadBuffer: TGLenum): TRGBImage;
begin
  Result := TRGBImage(SaveScreen_NoFlush(TRGBImage, xpos, ypos, width, height, ReadBuffer));
end;

function SaveAlignedScreen_NoFlush(
  const XPos, YPos: Integer; Width: Cardinal; const Height: Cardinal;
  const ReadBuffer: TGLenum): TRGBImage;
begin
  if GLVersion.BuggyDrawOddWidth and (Width mod 4 <> 0) then
    Width += (4 - Width mod 4);
  Result := SaveScreen_NoFlush(XPos, YPos, Width, Height, ReadBuffer);
end;

function SaveScreen_ToDisplayList_NoFlush(
  const XPos, YPos: Integer; const Width, Height: Cardinal;
  const ReadBuffer: TGLenum): TGLuint;
var
  ScreenImage: TRGBImage;
begin
  ScreenImage := SaveAlignedScreen_NoFlush(XPos, YPos, Width, Height, ReadBuffer);
  try
    { There was an idea to do here
        ImageDrawPartToDisplayList(ScreenImage, 0, 0, Width, Height);
      to draw only part of the screen when GLVersion.BuggyDrawOddWidth.
      Unfortunately, it doesn't really work, even drawing the screen
      is buggy with GLVersion.BuggyDrawOddWidth... }
    Result := ImageDrawToDisplayList(ScreenImage);
  finally FreeAndNil(ScreenImage) end;
end;

{ ----------------------------------------------------------------------
  Adjusting image size for 2D texture. }

function TextureNonPowerOfTwo: boolean;
begin
  Result := false
    { Using this makes OpenGL *sooo* slow...
      see e.g. castle/levels/castle_hall_final.wrl model or
      demo_models/inlined_textures.wrl.
      So it's better to scale textures to be power of 2. }
    {GL_ARB_texture_non_power_of_two or GL_version_2_0};
end;

function IsTextureSized(const Width, Height: Cardinal): boolean;
begin
  if TextureNonPowerOfTwo then
    Result :=
      (Width <= GLMaxTextureSize) and
      (Height <= GLMaxTextureSize) else
    Result :=
      IsPowerOf2(Width) and
      IsPowerOf2(Height) and
      (Width <= GLMaxTextureSize) and
      (Height <= GLMaxTextureSize);
end;

function IsTextureSized(const r: TEncodedImage): boolean;
begin
  Result := IsTextureSized(r.Width, r.Height);
end;

procedure ResizeForTextureSize(var r: TCastleImage);
var
  newR: TCastleImage;
begin
  if not IsTextureSized(r) then
  begin
    newR := ResizeToTextureSize(r);
    FreeAndNil(r);
    r := newR;
  end;
end;

procedure ResizeToTextureSize(var Width, Height: Cardinal);

  function BestTexSize(size: Cardinal): Cardinal;
  begin
    if size > GLMaxTextureSize then
      result := GLMaxTextureSize else
    begin
      if TextureNonPowerOfTwo or IsPowerOf2(size) then
        result := size else
        result := 1 shl (Biggest2Exponent(size)+1);
        {result jakie otrzymamy w ostatnim przypisaniu jest na pewno < GLMaxTextureSize bo
         skoro size <= GLMaxTextureSize i not IsPowerOf2(size) to size < GLMaxTextureSize a GLMaxTextureSize
         samo jest potega dwojki. }
     end;
  end;

begin
  Width  := BestTexSize(Width );
  Height := BestTexSize(Height);
end;

function ResizeToTextureSize(const r: TCastleImage): TCastleImage;
var
  NewWidth, NewHeight: Cardinal;
begin
  NewWidth  := R.Width ;
  NewHeight := R.Height;
  ResizeToTextureSize(NewWidth, NewHeight);

  if Log then
    WritelnLog('Textures', Format('Resizing 2D texture from %dx%d to %dx%d to satisfy OpenGL',
      [R.Width, R.Height, NewWidth, NewHeight]));

  result := r.MakeResized(NewWidth, NewHeight);
end;

{ ----------------------------------------------------------------------------
  Adjusting image size for cube map texture. }

function IsCubeMapTextureSized(const Size: Cardinal): boolean;
begin
  Result :=
    (not GL_ARB_texture_cube_map) or
    (
      IsPowerOf2(Size) and
      (Size > 0) and
      (Size <= GLMaxCubeMapTextureSizeARB)
    );
end;

function IsCubeMapTextureSized(const R: TEncodedImage): boolean;
begin
  Result :=
    (not GL_ARB_texture_cube_map) or
    (
      (r.Width = r.Height) { must be square } and
      IsPowerOf2(r.Width) and
      (r.Width > 0) and
      (r.Width <= GLMaxCubeMapTextureSizeARB)
    );
end;

function ResizeToCubeMapTextureSize(const r: TCastleImage): TCastleImage; forward;

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

function ResizeToCubeMapTextureSize(const Size: Cardinal): Cardinal;
begin
  Result := Size;
  if GL_ARB_texture_cube_map then
  begin
    if Size <= 0 then
      Result := 1 else
    if Size > GLMaxCubeMapTextureSizeARB then
      Result := GLMaxCubeMapTextureSizeARB else
    if IsPowerOf2(Size) then
      Result := Size else
      { Result jakie otrzymamy below jest na pewno < MaxTexSize bo
        skoro Size <= MaxTexSize i not IsPowerOf2(Size) to Size < MaxTexSize
        a MaxTexSize samo jest potega dwojki. }
      Result := 1 shl (Biggest2Exponent(Size) + 1);
  end;
end;

function ResizeToCubeMapTextureSize(const r: TCastleImage): TCastleImage;
var
  Size: Cardinal;
begin
  if GL_ARB_texture_cube_map then
  begin
    Size := Max(r.Width, r.Height);
    Size := ResizeToCubeMapTextureSize(Size);

    if Log then
      WritelnLog('Texture loading', Format('Resizing image for cube map texture from (%d, %d) to (%d, %d)',
        [R.Width, R.Height, Size, Size]));

    result := r.MakeResized(Size, Size);
  end else
    result := r.MakeCopy;
end;

{ ----------------------------------------------------------------------------
  Adjusting image size for 3d texture. }

function IsTexture3DSized(const Size: Cardinal): boolean;
begin
  Result :=
    (GL3DTextures = gsNone) or
    (
      IsPowerOf2(Size) and
      (Size > 0) and
      (Size <= GLMax3DTextureSize)
    );
end;

function IsTexture3DSized(const R: TCastleImage): boolean;
begin
  if GL3DTextures <> gsNone then
  begin
    Result :=
      IsPowerOf2(R.Width ) and (R.Width  > 0) and (R.Width  <= GLMax3DTextureSize) and
      IsPowerOf2(R.Height) and (R.Height > 0) and (R.Height <= GLMax3DTextureSize) and
      IsPowerOf2(R.Depth ) and (R.Depth  > 0) and (R.Depth  <= GLMax3DTextureSize);
  end else
    Result := true;
end;

{ Texture wrapping modes ----------------------------------------------------- }

operator = (const W1, W2: TTextureWrap2D): boolean;
begin
  Result := CompareMem(@W1, @W2, SizeOf(W1));
end;

operator = (const W1, W2: TTextureWrap3D): boolean;
begin
  Result := CompareMem(@W1, @W2, SizeOf(W1));
end;

function Texture2DClampToEdge: TTextureWrap2D;
begin
  Result[0] := CastleGL_CLAMP_TO_EDGE;
  Result[1] := Result[0];
end;

{ 2D texture loading --------------------------------------------------------- }

function LoadGLTexture(const image: TEncodedImage;
  MinFilter, MagFilter: TGLenum;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean;
  DDSForMipmaps: TDDSImage): TGLuint;
begin
  glGenTextures(1, @result);
  LoadGLGeneratedTexture(result, image, MinFilter, MagFilter, Wrap,
    GrayscaleIsAlpha, DDSForMipmaps);
end;

function LoadGLTexture(const FileName: string;
  MinFilter, MagFilter: TGLenum;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean;
  DDSForMipmaps: TDDSImage): TGLuint;
var
  Image: TEncodedImage;
begin
  Image := LoadTextureImage(FileName);
  try
    Result := LoadGLTexture(Image, MinFilter, MagFilter, Wrap,
      GrayscaleIsAlpha, DDSForMipmaps);
  finally Image.Free end;
end;

function TextureMinFilterNeedsMipmaps(const MinFilter: TGLenum): boolean;
begin
  Result :=
    ( (MinFilter = GL_NEAREST_MIPMAP_NEAREST) or
      (MinFilter = GL_LINEAR_MIPMAP_NEAREST) or
      (MinFilter = GL_NEAREST_MIPMAP_LINEAR) or
      (MinFilter = GL_LINEAR_MIPMAP_LINEAR) );
end;

{ Load Image through glCompressedTexImage2DARB.
  This checks existence of OpenGL extensions for S3TC,
  and checks Image sizes.
  It also takes care of pixel packing, although actually nothing needs
  be done about it when using compressed textures.

  Level = 0 for base (not a mipmap sublevel) image.

  @raises(ECannotLoadS3TCTexture If texture size is bad or OpenGL S3TC
    extensions are missing.) }
procedure glCompressedTextureImage2D(Image: TS3TCImage; Level: TGLint);
begin
  if not (GL_ARB_texture_compression and GL_EXT_texture_compression_s3tc) then
    raise ECannotLoadS3TCTexture.Create('Cannot load S3TC compressed textures: OpenGL doesn''t support one (or both) of ARB_texture_compression and EXT_texture_compression_s3tc extensions');

  if not IsTextureSized(Image) then
    raise ECannotLoadS3TCTexture.CreateFmt('Cannot load S3TC compressed textures: texture size is %d x %d, it''s not correct for OpenGL, and we cannot resize on CPU compressed textures',
      [Image.Width, Image.Height]);

  { Pixel packing parameters (stuff changed by Before/AfterUnpackImage)
    doesn't affect loading compressed textures, as far as I understand.
    So no need to call it. }
  glCompressedTexImage2DARB(GL_TEXTURE_2D, Level, ImageGLInternalFormat(Image),
    Image.Width, Image.Height, 0, Image.Size,
    Image.RawPixels);
end;

procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TEncodedImage;
  MinFilter, MagFilter: TGLenum;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean;
  DDSForMipmaps: TDDSImage);
var
  ImageInternalFormat: TGLuint;
  ImageFormat: TGLuint;

  { Calls glTexImage2D for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    Takes care of Image size --- makes sure that image has the right size
    (power of 2, within OpenGL required sizes).
    Level = 0 for base (not a mipmap sublevel) image. }
  procedure glTexImage2DImage(Image: TCastleImage; Level: TGLint);

    { This is like glTexImage2DImage, but it doesn't take care
      of Image size. }
    procedure Core(Image: TCastleImage);
    var
      UnpackData: TUnpackNotAlignedData;
    begin
      { Nawet jesli ladujemy obrazek o ktorym wiemy ze ma wymiary dobre
        dla glTexImage2d, musimy zadbac o jego aligment : bo co by bylo
        jesli tekstura ma szerokosc 1 lub 2  ?
        Poza tym, planuje dodac tutaj robienie borderow dla tekstury, a wtedy
        wymiar dobry dla glTexImage2d to rownie dobrze 2^n+2 (a wiec prawie zawsze
        niepodzielne na 4). }
      BeforeUnpackImage(UnpackData, Image);
      try
        { Workaround Mesa 7.9-devel bug (at least with Intel DRI,
          on Ubuntu 10.10, observed on domek): glTexImage2D accidentaly
          enables GL_TEXTURE_2D. }
        if GLVersion.IsMesa then glPushAttrib(GL_ENABLE_BIT);

        glTexImage2D(GL_TEXTURE_2D, Level, ImageInternalFormat,
          Image.Width, Image.Height, 0, ImageFormat, ImageGLType(Image),
          Image.RawPixels);

        if GLVersion.IsMesa then glPopAttrib;
      finally AfterUnpackImage(UnpackData, Image) end;
    end;

  var
    ImgGood: TCastleImage;
  begin
    if IsTextureSized(Image) then
      Core(Image) else
    begin
      ImgGood := ResizeToTextureSize(Image);
      try
        Core(ImgGood);
      finally ImgGood.Free end;
    end;
  end;

  procedure LoadNormal(const image: TCastleImage);
  begin
    { Setting GL_GENERATE_MIPMAP_SGIS to GL_FALSE isn't really needed here, AFAIK.
      It's false by default after all, and glTexParameter isn't part of the
      global state, it's part of currently bound texture state (which means
      you don't have to reset it to GL_FALSE just because you potentially
      set it to GL_TRUE on some other texture previously loaded). }
    if GL_SGIS_generate_mipmap then
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);

    glTexImage2DImage(Image, 0);
  end;

  { Check should we load mipmaps from DDS. Load them, if yes.

    If LoadBase this also loads the base image (mipmap level 0).

    Note that I observed a bug on NVidia GeForce FX 5200, with various driver
    versions on both Linux 32 bit, 64 bit, and Windows 32 bit:
    you cannot load the base texture level (0) *after* loading the mipmaps.
    Doing so results in mipmaps being ignored, and seemingly GL_NEAREST
    mininification filtering used (ignoring our set MinFilter filtering).
    This could be easily observed with
    demo_models/x3d/tex_visualize_mipmaps.x3dv,
    switching to viewpoint like "Mipmaps from DDS" or "Colored mipmaps from DDS"
    --- you could clearly see that mipmaps are ignored and ugly nearest filtering
    gets used.
    Using LoadBase automatically workarounds this. }
  function LoadMipmapsFromDDS(DDS: TDDSImage; LoadBase: boolean): boolean;
  var
    I, FromLevel: Integer;
  begin
    Result := (DDS <> nil) and DDS.Mipmaps;
    if Result and (DDS.DDSType <> dtTexture) then
    begin
      OnWarning(wtMinor, 'Texture', 'DDS image contains mipmaps, but not for 2D texture');
      Result := false;
    end;

    if Result and (not GL_version_1_2) then
    begin
      OnWarning(wtMinor, 'Texture', 'Cannot load DDS image containing mipmaps, because OpenGL 1.2 not available (GL_TEXTURE_MAX_LEVEL not available)');
      Result := false;
    end;

    if Result then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, DDS.MipmapsCount - 1);
      if LoadBase then
        FromLevel := 0 else
        FromLevel := 1;
      for I := FromLevel to DDS.MipmapsCount - 1 do
        if DDS.Images[I] is TCastleImage then
          glTexImage2DImage(TCastleImage(DDS.Images[I]), I) else
        if DDS.Images[I] is TS3TCImage then
          glCompressedTextureImage2D(TS3TCImage(DDS.Images[I]), I) else
          raise EInvalidImageForOpenGLTexture.CreateFmt('Cannot load to OpenGL texture image class %s', [Image.ClassName]);
    end;
  end;

  { Calls gluBuild2DMipmaps for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    gluBuild2DMipmaps doesn't require size to be a power of 2, so no problems
    here. }
  procedure gluBuild2DMipmapsImage(Image: TCastleImage);
  var
    UnpackData: TUnpackNotAlignedData;
  begin
    BeforeUnpackImage(UnpackData, Image);
    try
      gluBuild2DMipmaps(GL_TEXTURE_2D, ImageInternalFormat,
        Image.Width, Image.Height, ImageFormat, ImageGLType(Image),
        Image.RawPixels);
    finally AfterUnpackImage(UnpackData, Image) end;
  end;

  procedure LoadMipmapped(const image: TCastleImage);
  begin
    if not LoadMipmapsFromDDS(DDSForMipmaps, true) then
    if GL_SGIS_generate_mipmap then
    begin
      { hardware-accelerated mipmap generation.
        Thanks go to Eric Grange for mentioning it on
        [http://www.pascalgamedevelopment.com/forums/viewtopic.php?p=20514]
        Documentation is on
        [http://oss.sgi.com/projects/ogl-sample/registry/SGIS/generate_mipmap.txt] }
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
      glTexImage2DImage(Image, 0);
    end else
      gluBuild2DMipmapsImage(Image);
  end;

begin
  if (Image is TGrayscaleImage) and GrayscaleIsAlpha then
  begin
    { To treat texture as pure alpha channel, both internalFormat and format
      must be ALPHA }
    ImageInternalFormat := GL_ALPHA;
    ImageFormat := GL_ALPHA;
  end else
  begin
    ImageInternalFormat := ImageGLInternalFormat(Image);
    ImageFormat := ImageGLFormat(Image);
  end;

  { bind the texture, set min, mag filters and wrap parameters }
  glBindTexture(GL_TEXTURE_2D, texnum);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, minFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, magFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Wrap[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Wrap[1]);

  { give the texture data }
  if Image is TCastleImage then
  begin
    { Load uncompressed }
    if TextureMinFilterNeedsMipmaps(MinFilter) then
      LoadMipmapped(TCastleImage(Image)) else
      LoadNormal(TCastleImage(Image));
  end else
  if Image is TS3TCImage then
  begin
    { Load compressed }
    glCompressedTextureImage2D(TS3TCImage(Image), 0);

    if TextureMinFilterNeedsMipmaps(MinFilter) then
    begin
      if not LoadMipmapsFromDDS(DDSForMipmaps, false) then
      try
        GenerateMipmap(GL_TEXTURE_2D);
      except
        on E: EGenerateMipmapNotAvailable do
        begin
          MinFilter := GL_LINEAR;
          { Update GL_TEXTURE_MIN_FILTER, since we already initialized it earlier. }
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, MinFilter);
          OnWarning(wtMinor, 'Texture', 'Creating mipmaps for S3TC compressed textures requires GenerateMipmap functionality, will fallback to GL_LINEAR minification: ' + E.Message);
        end;
      end;
    end;
  end else
    raise EInvalidImageForOpenGLTexture.CreateFmt('Cannot load to OpenGL texture image class %s', [Image.ClassName]);
end;

{ TGLVideo ------------------------------------------------------------------- }

constructor TGLVideo.Create(Video: TVideo;
  MinFilter, MagFilter: TGLenum;
  const Anisotropy: TGLfloat;
  const Wrap: TTextureWrap2D);
var
  I: Integer;
begin
  inherited Create;

  Check(Video.Loaded, 'Video must be loaded before using TGLVideo.Create');

  FCount := Video.Count;

  SetLength(FItems, Count);
  for I := 0 to High(FItems) do
  begin
    FItems[I] := LoadGLTexture(Video.Items[I], MinFilter, MagFilter, Wrap);
    TexParameterMaxAnisotropy(GL_TEXTURE_2D, Anisotropy);
  end;

  FTimeLoop := Video.TimeLoop;
  FTimeBackwards := Video.TimeBackwards;
  FFramesPerSecond := Video.FramesPerSecond;
end;

destructor TGLVideo.Destroy;
begin
  if Count > 0 then
    glDeleteTextures(Count, @FItems[0]);

  inherited;
end;

function TGLVideo.IndexFromTime(const Time: Single): Integer;
begin
  Result := TVideo.FrameIndexFromTime(Time, Count, FramesPerSecond,
    TimeLoop, TimeBackwards);
end;

function TGLVideo.GLTextureFromTime(const Time: Single): TGLuint;
begin
  Result := FItems[IndexFromTime(Time)];
end;

{ Cube map texture loading --------------------------------------------------- }

{ Comfortably load a single image for one cube map texture side.

  This is pretty much like glTexImages2DForCubeMap,
  except it operates only on one side of the cube.
  Target should be one of the six cube map texture targets:
  GL_TEXTURE_CUBE_MAP_POSITIVE/NEGATIVE_X/Y/Z_ARB.

  Also, this cannot load mipmaps from DDS or use GenerateMipmap
  (GenerateMipmap call must be done for whole cube map texture target).
  So this can create mipmaps only by gluBuild2DMipmaps. It will also
  fail with ECannotLoadS3TCTexture if mipmaps will be requested --- we cannot
  generate mipmaps for S3TC compressed. If you want to use
  more modern GenerateMipmap, you should use higher-level
  glTexImages2DForCubeMap (takes all six images), or pass Mipmaps = @false
  and do it yourself.

  Level must be 0 is you require mipmaps.

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason.
    This includes ECannotLoadS3TCTexture if the S3TC texture cannot be
    loaded for whatever reason (not availble S3TC extensions,
    not correct texture size, mipmaps requested).
    This includes EInvalidImageForOpenGLTexture if Image class is invalid
    for an OpenGL texture.) }
procedure glTextureCubeMapSide(
  Target: TGLenum; const Image: TEncodedImage; Level: TGLuint; Mipmaps: boolean);
var
  ImageInternalFormat: TGLuint;
  ImageFormat: TGLuint;

  { Calls glTexImage2D for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    Takes care of Image size --- makes sure that image has the right size
    (power of 2, within OpenGL required sizes). }
  procedure glTexImage2DImage(Image: TCastleImage);

    { This is like glTexImage2DImage, but it doesn't take care
      of Image size. }
    procedure Core(Image: TCastleImage);
    var
      UnpackData: TUnpackNotAlignedData;
    begin
      { Nawet jesli ladujemy obrazek o ktorym wiemy ze ma wymiary dobre
        dla glTexImage2d, musimy zadbac o jego aligment : bo co by bylo
        jesli tekstura ma szerokosc 1 lub 2  ?
        Poza tym, planuje dodac tutaj robienie borderow dla tekstury, a wtedy
        wymiar dobry dla glTexImage2d to rownie dobrze 2^n+2 (a wiec prawie zawsze
        niepodzielne na 4). }
      BeforeUnpackImage(UnpackData, Image);
      try
        glTexImage2D(Target, Level, ImageInternalFormat,
          Image.Width, Image.Height, 0, ImageFormat, ImageGLType(Image),
          Image.RawPixels);
      finally AfterUnpackImage(UnpackData, Image) end;
    end;

  var
    ImgGood: TCastleImage;
  begin
    if IsCubeMapTextureSized(Image) then
      Core(Image) else
    begin
      ImgGood := ResizeToCubeMapTextureSize(Image);
      try
        Core(ImgGood);
      finally ImgGood.Free end;
    end;
  end;

  { Calls gluBuild2DMipmaps for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    gluBuild2DMipmaps doesn't require size to be a power of 2, so no problems
    here. }
  procedure gluBuild2DMipmapsImage(Image: TCastleImage);
  var
    UnpackData: TUnpackNotAlignedData;
  begin
    BeforeUnpackImage(UnpackData, Image);
    try
      gluBuild2DMipmaps(Target, ImageInternalFormat,
        Image.Width, Image.Height, ImageFormat, ImageGLType(Image),
        Image.RawPixels);
    finally AfterUnpackImage(UnpackData, Image) end;
  end;

  procedure LoadMipmapped(const image: TCastleImage);
  begin
    { Testing on ATI Mobility Radeon X1600 (fglrx, Linux, on Mac Book Pro),
      it looks like SGIS_generate_mipmap doesn't work on cube map texture
      targets: I get GL error "invalid enumerant" when trying

      glTexParameteri(Target, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);

      So I don't use SGIS_generate_mipmap, instead making mipmaps always
      by gluBuild2DMipmapsImage.
    }

    gluBuild2DMipmapsImage(Image);
  end;

  procedure LoadNormal(const image: TCastleImage);
  begin
    glTexImage2DImage(Image);
  end;

  { Load Image through glCompressedTexImage2DARB.
    This checks existence of OpenGL extensions for S3TC,
    and checks Image sizes.
    It also takes care of pixel packing, although actually nothing needs
    be done about it when using compressed textures.

    @raises(ECannotLoadS3TCTexture If texture size is bad or OpenGL S3TC
      extensions are missing or mipmaps were required.) }
  procedure LoadCompressed(const Image: TS3TCImage);
  begin
    if not (GL_ARB_texture_compression and GL_EXT_texture_compression_s3tc) then
      raise ECannotLoadS3TCTexture.Create('Cannot load S3TC compressed textures: OpenGL doesn''t support one (or both) of ARB_texture_compression and EXT_texture_compression_s3tc extensions');

    if not IsCubeMapTextureSized(Image) then
      raise ECannotLoadS3TCTexture.CreateFmt('Cannot load S3TC compressed textures: texture size is %d x %d, it''s not correct for OpenGL, and we cannot resize on CPU compressed textures',
        [Image.Width, Image.Height]);

    if Mipmaps then
      raise ECannotLoadS3TCTexture.Create('Cannot create mipmaps on CPU for S3TC compressed images');

    { Pixel packing parameters (stuff changed by Before/AfterUnpackImage)
      doesn't affect loading compressed textures, as far as I understand.
      So no need to call it. }
    glCompressedTexImage2DARB(Target, Level, ImageInternalFormat,
      Image.Width, Image.Height, 0, Image.Size,
      Image.RawPixels);
  end;

begin
  ImageInternalFormat := ImageGLInternalFormat(Image);
  ImageFormat := ImageGLFormat(Image);

  if Image is TS3TCImage then
    LoadCompressed(TS3TCImage(Image)) else
  if Image Is TCastleImage then
  begin
    if Mipmaps then
      LoadMipmapped(TCastleImage(Image)) else
      LoadNormal(TCastleImage(Image));
  end else
    raise EInvalidImageForOpenGLTexture.CreateFmt('Cannot load to OpenGL texture image class %s', [Image.ClassName]);
end;

procedure glTextureCubeMap(
  PositiveX, NegativeX,
  PositiveY, NegativeY,
  PositiveZ, NegativeZ: TEncodedImage;
  DDSForMipmaps: TDDSImage;
  Mipmaps: boolean);

  { Check should we load mipmaps from DDS. }
  function HasMipmapsFromDDS(DDS: TDDSImage): boolean;
  begin
    Result := (DDS <> nil) and DDS.Mipmaps;
    if Result and (DDS.DDSType <> dtCubeMap) then
    begin
      OnWarning(wtMinor, 'Texture', 'DDS image contains mipmaps, but not for CubeMap texture');
      Result := false;
    end;
  end;

  { Load mipmaps from DDS. Assume HasMipmapsFromDDS was true. }
  procedure LoadMipmapsFromDDS(DDS: TDDSImage);

    procedure LoadMipmapsFromDDSSide(GLSide: TGLenum; DDSSide: TDDSCubeMapSide);
    var
      I: Integer;
    begin
      for I := 1 to DDS.MipmapsCount - 1 do
        glTextureCubeMapSide(GLSide, DDS.CubeMapImage(DDSSide, I), I, false);
    end;

  begin
    if not GL_version_1_2 then
    begin
      OnWarning(wtMinor, 'Texture', 'Cannot load DDS image containing mipmaps, because OpenGL 1.2 not available (GL_TEXTURE_MAX_LEVEL not available)');
      Exit;
    end;

    glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MAX_LEVEL, DDS.MipmapsCount - 1);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB, dcsPositiveX);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB, dcsNegativeX);
    { Note Positive/Negative are swapped for Y.
      DDS cube map sides are in left-handed coordinate system, like Direct X.
      See TDDSCubeMapSide comments. }
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB, dcsNegativeY);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB, dcsPositiveY);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB, dcsPositiveZ);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB, dcsNegativeZ);
  end;

begin
  if Mipmaps and (HasMipmapsFromDDS(DDSForMipmaps) or HasGenerateMipmap) then
  begin
    { Load six cube faces without mipmaps, then generate them all
      in one go with GenerateMipmap. }
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB, PositiveX, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB, NegativeX, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB, PositiveY, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB, NegativeY, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB, PositiveZ, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB, NegativeZ, 0, false);
    if HasMipmapsFromDDS(DDSForMipmaps) then
      LoadMipmapsFromDDS(DDSForMipmaps) else
    begin
      GenerateMipmap(GL_TEXTURE_CUBE_MAP_ARB);
      if Log then
        WritelnLog('Mipmaps', 'Generating mipmaps for cube map by GenerateMipmap (GOOD)');
    end;
  end else
  begin
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB, PositiveX, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB, NegativeX, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB, PositiveY, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB, NegativeY, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB, PositiveZ, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB, NegativeZ, 0, Mipmaps);
  end;
end;

{ 3D texture loading --------------------------------------------------------- }

procedure glTextureImage3D(const Image: TEncodedImage; MinFilter, MagFilter: TGLenum;
  DDSForMipmaps: TDDSImage);
var
  ImageInternalFormat: TGLuint;
  ImageFormat: TGLuint;

  { Calls glTexImage3D for given image.
    Takes care of OpenGL unpacking (alignment etc.).
    Takes care of Image size --- makes sure that image has the right size
    (power of 2, within OpenGL required sizes). }
  procedure glTexImage3DImage(Image: TCastleImage; Level: TGLuint);

    { This is like glTexImage3DImage, but it doesn't take care
      of Image size. }
    procedure Core(Image: TCastleImage);
    var
      UnpackData: TUnpackNotAlignedData;
    begin
      BeforeUnpackImage(UnpackData, Image);
      try
        case GL3DTextures of
          gsExtension:
            glTexImage3DEXT(GL_TEXTURE_3D_EXT, Level, ImageInternalFormat,
              Image.Width, Image.Height, Image.Depth, 0, ImageFormat, ImageGLType(Image),
              Image.RawPixels);
          gsStandard:
            glTexImage3D(GL_TEXTURE_3D, Level, ImageInternalFormat,
              Image.Width, Image.Height, Image.Depth, 0, ImageFormat, ImageGLType(Image),
              Image.RawPixels);
        end;
      finally AfterUnpackImage(UnpackData, Image) end;
    end;

  begin
    if not IsTexture3DSized(Image) then
      raise ETextureLoadError.CreateFmt('Image is not properly sized for a 3D texture, sizes must be a power-of-two and <= GL_MAX_3D_TEXTURE_SIZE (%d). Sizes are: %d x %d x %d',
        [ GLMax3DTextureSize, Image.Width, Image.Height, Image.Depth ]);

    Core(Image);
  end;

  { Check should we load mipmaps from DDS. Load them, if yes. }
  function LoadMipmapsFromDDS(DDS: TDDSImage): boolean;
  var
    I: Integer;
  begin
    Result := (DDS <> nil) and DDS.Mipmaps;
    if Result and (DDS.DDSType <> dtVolume) then
    begin
      OnWarning(wtMinor, 'Texture', 'DDS image contains mipmaps, but not for 3D (volume) texture');
      Result := false;
    end;

    if Result and (not GL_version_1_2) then
    begin
      OnWarning(wtMinor, 'Texture', 'Cannot load DDS image containing mipmaps, because OpenGL 1.2 not available (GL_TEXTURE_MAX_LEVEL not available)');
      Result := false;
    end;

    if Result then
    begin
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAX_LEVEL, DDS.MipmapsCount - 1);
      for I := 1 to DDS.MipmapsCount - 1 do
        if DDS.Images[I] is TCastleImage then
          glTexImage3DImage(TCastleImage(DDS.Images[I]), I) else
          raise ETextureLoadError.CreateFmt('Image class %s cannot be loaded to OpenGL 3D texture. OpenGL doesn''t allow any 3D texture compression formats',
            [Image.ClassName]);
    end;
  end;

begin
  ImageInternalFormat := ImageGLInternalFormat(Image);
  ImageFormat := ImageGLFormat(Image);

  if not (Image is TCastleImage) then
    raise ETextureLoadError.CreateFmt('Image class %s cannot be loaded to OpenGL 3D texture. OpenGL doesn''t allow any 3D texture compression formats',
      [Image.ClassName]);

  glTexImage3DImage(TCastleImage(Image), 0);

  if TextureMinFilterNeedsMipmaps(MinFilter) then
  begin
    if not LoadMipmapsFromDDS(DDSForMipmaps) then
    try
      GenerateMipmap(GL_TEXTURE_3D);
    except
      on E: EGenerateMipmapNotAvailable do
      begin
        MinFilter := GL_LINEAR;
        OnWarning(wtMinor, 'Texture', 'Creating mipmaps for 3D textures requires GenerateMipmap functionality, will fallback to GL_LINEAR minification: ' + E.Message);
      end;
    end;
  end;

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, MagFilter);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, MinFilter);
end;

{ GenerateMipmap ------------------------------------------------------------- }

{ $define TEST_NO_GENERATE_MIPMAP}

function HasGenerateMipmap: boolean;
{$ifdef TEST_NO_GENERATE_MIPMAP}
begin
  Result := false;
{$else}
begin
  Result := (GLFramebuffer <> gsNone) and (not GLVersion.BuggyGenerateMipmap);
{$endif}
end;

procedure GenerateMipmap(target: TGLenum);
begin
  {$ifndef TEST_NO_GENERATE_MIPMAP}
  if GLFramebuffer <> gsNone then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      { To work under fglrx (confirmed on chantal (ATI Mobility Radeon X1600)),
        we have to temporarily enable target.
        At least with EXT_framebuffer_object.
        This is a known ATI drivers problem:
        http://www.opengl.org/discussion_boards/ubbthreads.php?ubb=showflat&Number=237052 }
      glEnable(Target);
      case GLFramebuffer of
        gsExtension: glGenerateMipmapEXT(Target);
        gsStandard : glGenerateMipmap   (Target);
      end;
    glPopAttrib;
  end else
  {$endif}
    raise EGenerateMipmapNotAvailable.Create('Framebuffer not supported, glGenerateMipmap[EXT] not available');
end;

{ Anisotropy ----------------------------------------------------------------- }

procedure TexParameterMaxAnisotropy(const target: TGLenum; const Anisotropy: TGLfloat);
begin
  if GL_EXT_texture_filter_anisotropic then
    glTexParameterf(Target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      Min(GLMaxTextureMaxAnisotropyEXT, Anisotropy));
end;

{ DecompressS3TC ------------------------------------------------------------- }

function GLDecompressS3TC(Image: TS3TCImage): TCastleImage;
var
  Tex: TGLuint;
  PackData: TPackNotAlignedData;
begin
  glGenTextures(1, @Tex);
  glBindTexture(GL_TEXTURE_2D, Tex);

  { Testcase that fails on Radeon chantal (ATI Radeon X1600) Linux:
    demo_models/textures/marble_with_mipmaps_s3tc.dds

    No problem on NVidia (fpc 2.2.2 kocury/linux/32, fpc 2.2.4 kocury/linux/32),
    and no problem on Mac OS X with the same GPU (also chantal, 32bit, fpc 2.2.4).
    So I'm assuming it's fglrx-specific bug. }
  if GLVersion.IsFglrx and ( (Image.Width < 4) or (Image.Height < 4) ) then
    raise ECannotDecompressS3TC.CreateFmt('Cannot decompress S3TC texture: fglrx (proprietary Radeon drivers on Linux) may awfully crash when one of texture sizes is smaller than 4, and your texture size is %d x %d',
      [Image.Width, Image.Height]);

  try
    glCompressedTextureImage2D(Image, 0);
  except
    { catch ECannotLoadS3TCTexture and change it to ECannotDecompressS3TC }
    on E: ECannotLoadS3TCTexture do
      raise ECannotDecompressS3TC.Create('Cannot decompress S3TC texture: ' + E.Message);
  end;

  case Image.Compression of
    s3tcDxt1_RGB: Result := TRGBImage.Create(Image.Width, Image.Height, Image.Depth);
    s3tcDxt1_RGBA,
    s3tcDxt3,
    s3tcDxt5: Result := TRGBAlphaImage.Create(Image.Width, Image.Height, Image.Depth);
    else raise EInternalError.Create('GLDecompressS3TC-Compression?');
  end;

  BeforePackImage(PackData, Result);
  try
    glGetTexImage(GL_TEXTURE_2D, 0,
      ImageGLFormat(Result), ImageGLType(Result), Result.RawPixels);
  finally AfterPackImage(PackData, Result) end;

  glDeleteTextures(1, @Tex);
end;

{ BindFramebuffer stack ------------------------------------------------------ }

var
  { We may want to use an FBO, while another FBO is already used.

    Right now, this situation happens only when we use view3dscene
    with --screenshot option, and we load a scene that uses a generated
    texture (like RenderedTexture or GeneratedShadowMap).

    It's important in such cases that the we should restore at the end
    previously bound FBO --- not necessarily just FBO number 0. }
  BoundFboStack: TLongWordList;

{ Use instead of glBindFramebuffer(GL_FRAMEBUFFER, Fbo),
  for non-zero Fbo. This will bind and add this Fbo to stack. }
procedure BindFramebuffer(const Fbo: TGLuint);
begin
  Assert(Fbo <> 0);
  if BoundFboStack = nil then
    BoundFboStack := TLongWordList.Create;
  BoundFboStack.Add(Fbo);

  case GLFramebuffer of
    gsExtension: glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, Fbo);
    gsStandard : glBindFramebuffer   (GL_FRAMEBUFFER    , Fbo);
  end;
end;

{ Remove the top Fbo from the stack, and bind previous (new top) Fbo.
  Binds FBO number 0 (normal OpenGL buffer) if stack becomes empty.

  PreviousFboDefaultBuffer is set to the default draw buffer suitable
  for currently (after this call) bound FBO. It's GL_BACK if we're
  now in normal rendering to window (TODO: we assume you always use double-buffer then),
  or GL_COLOR_ATTACHMENT0 if we're in another non-window FBO.
  TODO: it should be GL_NONE if we're in another non-window FBO for tbDepth.
  Without this, if you would blindly try glDrawBuffer(GL_BACK)
  after UnbindFramebuffer, and you are in another single-buffered FBO,
  OpenGL (at least NVidia and fglrx) will (rightly) report OpenGL
  "invalid enum" error. }
procedure UnbindFramebuffer(out PreviousFboDefaultBuffer: TGLenum);
var
  PreviousFbo: TGLuint;
begin
  if (BoundFboStack <> nil) and (BoundFboStack.Count <> 0) then
  begin
    BoundFboStack.Count := BoundFboStack.Count - 1;
    if BoundFboStack.Count <> 0 then
      PreviousFbo := BoundFboStack.Last else
      PreviousFbo := 0;
  end else
    PreviousFbo := 0;

  if PreviousFbo = 0 then
    PreviousFboDefaultBuffer := GL_BACK else
    PreviousFboDefaultBuffer := GL_COLOR_ATTACHMENT0;

  case GLFramebuffer of
    gsExtension: glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, PreviousFbo);
    gsStandard : glBindFramebuffer   (GL_FRAMEBUFFER    , PreviousFbo);
  end;
end;

procedure UnbindFramebuffer;
var
  PreviousFboDefaultBuffer: TGLenum;
begin
  UnbindFramebuffer(PreviousFboDefaultBuffer);
  { ignore PreviousFboDefaultBuffer }
end;

{ TGLRenderToTexture --------------------------------------------------------- }

{ Fortunately, all constants with equal meanings have also equal values
  both for EXT_framebuffer_object and ARB_framebuffer_object (as "core extension"
  in OpenGL 3). Checked for
  - FramebufferStatusToString error statuses
    (except ARB version simply removed some constans (so they will only
    occur if we happen to use EXT version))
  - GL_STENCIL_ATTACHMENT
  - GL_DEPTH_STENCIL
  - GL_DEPTH_ATTACHMENT
  - GL_FRAMEBUFFER
  - GL_COLOR_ATTACHMENT0
}

{ Wrapper around glFramebufferTexture2D }
procedure FramebufferTexture2D(const Target, Attachment, TexTarget: TGLenum;
  const Texture: TGLuint; const Level: TGLint);
begin
  case GLFramebuffer of
    gsExtension: glFramebufferTexture2DEXT(Target, Attachment, TexTarget, Texture, Level);
    gsStandard : glFramebufferTexture2D   (Target, Attachment, TexTarget, Texture, Level);
  end;
end;

constructor TGLRenderToTexture.Create(const AWidth, AHeight: Cardinal);
begin
  inherited Create;

  FTextureTarget := GL_TEXTURE_2D;
  FCompleteTextureTarget := GL_TEXTURE_2D;
  FDepthTextureTarget := GL_TEXTURE_2D;
  FStencil := true;

  FWidth := AWidth;
  FHeight := AHeight;
  FMultiSampling := 1;
end;

destructor TGLRenderToTexture.Destroy;
begin
  GLContextClose;
  inherited;
end;

procedure TGLRenderToTexture.SetTexture(
  const ATexture: TGLuint;
  const ATextureTarget: TGLenum);
begin
  if (ATexture <> FTexture) or (ATextureTarget <> FTextureTarget) then
  begin
    FTexture := ATexture;
    FTextureTarget := ATextureTarget;
    if Framebuffer <> 0 then
    begin
      if not FramebufferBound then
        BindFramebuffer(Framebuffer);
      case GLFramebuffer of
        gsExtension: glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, TextureTarget, Texture, 0);
        gsStandard : glFramebufferTexture2D   (GL_FRAMEBUFFER    , GL_COLOR_ATTACHMENT0    , TextureTarget, Texture, 0);
      end;
      if not FramebufferBound then
        UnbindFramebuffer;
    end;
  end;
end;

procedure TGLRenderToTexture.GLContextOpen;

  function FramebufferStatusToString(const Status: TGLenum): string;
  begin
    { some of these messages based on spec wording
      http://oss.sgi.com/projects/ogl-sample/registry/EXT/framebuffer_object.txt ,
      http://www.opengl.org/registry/specs/ARB/framebuffer_object.txt }
    case Status of
      GL_FRAMEBUFFER_COMPLETE                          : Result := 'Complete (no error)';
      GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT             : Result := 'INCOMPLETE_ATTACHMENT: Not all framebuffer attachment points are "framebuffer attachment complete"';
      GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     : Result := 'INCOMPLETE_MISSING_ATTACHMENT: None image attached to the framebuffer. On some GPUs/drivers (fglrx) it may also mean that desired image size is too large';
      GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT         : Result := 'INCOMPLETE_DIMENSIONS: Not all attached images have the same width and height';
      GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT            : Result := 'INCOMPLETE_FORMATS: Not all images attached to the attachment points COLOR_ATTACHMENT* have the same internal format';
      GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            : Result := 'INCOMPLETE_DRAW_BUFFER: The value of FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is NONE for some color attachment point(s) named by DRAW_BUFFERi';
      GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER            : Result := 'INCOMPLETE_READ_BUFFER: READ_BUFFER is not NONE, and the value of FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is NONE for the color attachment point named by READ_BUFFER';
      GL_FRAMEBUFFER_UNSUPPORTED                       : Result := 'UNSUPPORTED: The combination of internal formats of the attached images violates an implementation-dependent set of restrictions';
      GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            : Result := 'INCOMPLETE_MULTISAMPLE: The value of RENDERBUFFER_SAMPLES is not the same for all attached images.';
      0: Result := 'OpenGL error during CheckFramebufferStatus';
      else Result := 'Unknown FramebufferStatus error: ' + gluErrorString(Status);
    end;
  end;

  procedure GenBindRenderbuffer(var RenderbufferId: TGLuint;
    const InternalFormat: TGLenum; const Attachment: TGLenum);
  begin
    case GLFramebuffer of
      gsExtension:
        begin
          glGenRenderbuffersEXT(1, @RenderbufferId);
          glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, RenderbufferId);
          glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, InternalFormat, Width, Height);
          glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, Attachment, GL_RENDERBUFFER_EXT, RenderbufferId);
        end;
      gsStandard:
        begin
          glGenRenderbuffers(1, @RenderbufferId);
          glBindRenderbuffer   (GL_RENDERBUFFER    , RenderbufferId);
          if (MultiSampling > 1) and GLFBOMultiSampling then
            glRenderbufferStorageMultisample(GL_RENDERBUFFER, MultiSampling, InternalFormat, Width, Height) else
            glRenderbufferStorage           (GL_RENDERBUFFER,                InternalFormat, Width, Height);
          glFramebufferRenderbuffer   (GL_FRAMEBUFFER    , Attachment, GL_RENDERBUFFER    , RenderbufferId);
        end;
    end;
  end;

  { initialize RenderbufferStencil, attach it to FBO stencil }
  procedure AttachSeparateStencilRenderbuffer;
  begin
    GenBindRenderbuffer(RenderbufferStencil, GL_STENCIL_INDEX, GL_STENCIL_ATTACHMENT);
  end;

  function ColorBufferFormat: TGLenum;
  begin
    if ColorBufferAlpha then
      Result := GL_RGBA else
      Result := GL_RGB;
  end;

var
  Status: TGLenum;
  PackedDepthBufferFormat: TGLenum;
  Success: boolean;
  PreviousFboDefaultBuffer: TGLenum;
begin
  Assert(not FGLInitialized, 'You cannot call TGLRenderToTexture.GLContextInit on already OpenGL-initialized instance. Call GLContextClose first if this is really what you want.');

  if GLFramebuffer <> gsNone then
  begin
    if (Width > GLMaxRenderbufferSize) or
       (Height > GLMaxRenderbufferSize) then
      raise EFramebufferSizeTooLow.CreateFmt('Maximum renderbuffer (within framebuffer) size is %d x %d in your OpenGL implementation, while we require %d x %d',
        [ GLMaxRenderbufferSize, GLMaxRenderbufferSize, Width, Height ]);

    case GLFramebuffer of
      gsExtension: glGenFramebuffersEXT(1, @Framebuffer);
      gsStandard : glGenFramebuffers   (1, @Framebuffer);
    end;
    BindFramebuffer(Framebuffer);

    { Used for tbColor or tbNone, when we don't want to capture depth buffer,
      and so it's Ok to pack depth and stencil together.

      When EXT_packed_depth_stencil is present, and stencil is wanted
      (a very common case!, as most GPUs have EXT_packed_depth_stencil
      and for shadow volumes we want stencil) we desperately want to
      use one renderbuffer with combined depth/stencil info.
      Other possibilities may be not available at all (e.g. Radeon on chantal,
      but probably most GPUs with EXT_packed_depth_stencil).

      TODO: for core OpenGL 3, how to detect should we use packed version?
      http://www.opengl.org/registry/specs/ARB/framebuffer_object.txt
      incorporates EXT_packed_depth_stencil. }
    if Stencil and GL_EXT_packed_depth_stencil then
      PackedDepthBufferFormat := GL_DEPTH_STENCIL else
      PackedDepthBufferFormat := GL_DEPTH_COMPONENT;

    case Buffer of
      tbColor:
        begin
          FramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, TextureTarget, Texture, 0);
          GenBindRenderbuffer(RenderbufferDepth, PackedDepthBufferFormat, GL_DEPTH_ATTACHMENT);
        end;
      tbDepth:
        begin
          { Needed to consider FBO "complete" }
          glDrawBuffer(GL_NONE);
          glReadBuffer(GL_NONE);

          FramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, TextureTarget, Texture, 0);
        end;
      tbColorAndDepth:
        begin
          FramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, TextureTarget, Texture, 0);
          FramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, DepthTextureTarget, DepthTexture, 0);
        end;
      tbNone:
        begin
          GenBindRenderbuffer(RenderbufferColor, ColorBufferFormat, GL_COLOR_ATTACHMENT0);
          GenBindRenderbuffer(RenderbufferDepth, PackedDepthBufferFormat, GL_DEPTH_ATTACHMENT);
        end;
      else raise EInternalError.Create('Buffer 1?');
    end;

    { setup stencil buffer }
    if Stencil then
      case Buffer of
        tbDepth, tbColorAndDepth:
          { only separate stencil buffer possible in this case }
          AttachSeparateStencilRenderbuffer;
        tbColor, tbNone:
          if GL_EXT_packed_depth_stencil then
          case GLFramebuffer of
            gsExtension: glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, RenderbufferDepth);
            gsStandard : glFramebufferRenderbuffer   (GL_FRAMEBUFFER    , GL_STENCIL_ATTACHMENT    , GL_RENDERBUFFER    , RenderbufferDepth);
          end else
            AttachSeparateStencilRenderbuffer;
      end;

    Success := false;
    try
      case GLFramebuffer of
        gsExtension: Status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
        gsStandard : Status := glCheckFramebufferStatus   (GL_FRAMEBUFFER    );
      end;
      case Status of
        GL_FRAMEBUFFER_COMPLETE: Success := true;
        GL_FRAMEBUFFER_UNSUPPORTED: OnWarning(wtMinor, 'FBO', 'Unsupported framebuffer configuration, will fallback to glCopyTexSubImage2D approach. If your window is invisible (like for "view3dscene --screenshot"), you may get only a black screen.');
        else raise EFramebufferInvalid.CreateFmt('Framebuffer check failed: %s (FBO error number %d)',
          [ FramebufferStatusToString(Status), Status]);
      end;
    finally
      { Always, regardless of Success, unbind FBO and restore normal gl*Buffer }
      case GLFramebuffer of
        gsExtension: glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
        gsStandard : glBindRenderbuffer   (GL_RENDERBUFFER    , 0);
      end;
      UnbindFramebuffer(PreviousFboDefaultBuffer);

      if Buffer = tbDepth then
      begin
        glDrawBuffer(PreviousFboDefaultBuffer);
        glReadBuffer(PreviousFboDefaultBuffer);
      end;

      { If failure, release resources. In particular, this sets Framebuffer = 0,
        which will be a signal to other methods that FBO is not supported. }
      if not Success then
        GLContextClose;
    end;
  end;

  FGLInitialized := true;
end;

procedure TGLRenderToTexture.GLContextClose;

  procedure FreeRenderbuffer(var Buf: TGLuint);
  begin
    if Buf <> 0 then
    begin
      case GLFramebuffer of
        gsExtension: glDeleteRenderbuffersEXT(1, @Buf);
        gsStandard : glDeleteRenderbuffers   (1, @Buf);
      end;
      Buf := 0;
    end;
  end;

  procedure FreeFramebuffer(var Buf: TGLuint);
  begin
    if Buf <> 0 then
    begin
      case GLFramebuffer of
        gsExtension: glDeleteFramebuffersEXT(1, @Buf);
        gsStandard : glDeleteFramebuffers   (1, @Buf);
      end;
      Buf := 0;
    end;
  end;

begin
  FreeRenderbuffer(RenderbufferColor);
  FreeRenderbuffer(RenderbufferDepth);
  FreeRenderbuffer(RenderbufferStencil);
  FreeFramebuffer(Framebuffer);
end;

procedure TGLRenderToTexture.RenderBegin;
begin
  if Framebuffer <> 0 then
  begin
    if not FramebufferBound then
    begin
      BindFramebuffer(Framebuffer);
      FramebufferBound := true;

      if Buffer = tbDepth then
      begin
        glDrawBuffer(GL_NONE);
        glReadBuffer(GL_NONE);
      end;
    end;
    Assert(FramebufferBound);
  end;
end;

{ A debug trick, saves color or depth buffer of the generated framebuffer image
  to a filename (/tmp/framebuffer_color/depth.png, change the code below
  if you want other filename). Useful e.g. to visualize captured shadow maps. }
{ $define DEBUG_SAVE_FRAMEBUFFER_DEPTH}
{ $define DEBUG_SAVE_FRAMEBUFFER_COLOR}

procedure TGLRenderToTexture.RenderEnd(const RenderBeginFollows: boolean);

{$ifdef DEBUG_SAVE_FRAMEBUFFER_COLOR}
  procedure SaveColor(const FileName: string);
  var
    PackData: TPackNotAlignedData;
    Image: TCastleImage;
  begin
    Image := TRGBImage.Create(Width, Height);
    try
      BeforePackImage(PackData, Image);
      try
        glReadPixels(0, 0, Width, Height, ImageGLFormat(Image),
          ImageGLType(Image), Image.RawPixels);
      finally AfterPackImage(PackData, Image) end;

      SaveImage(Image, FileName);
    finally FreeAndNil(Image) end;
  end;
{$endif DEBUG_SAVE_FRAMEBUFFER_COLOR}

{$ifdef DEBUG_SAVE_FRAMEBUFFER_DEPTH}
  procedure SaveDepth(const FileName: string);
  var
    PackData: TPackNotAlignedData;
    Image: TGrayscaleImage;
  begin
    Image := TGrayscaleImage.Create(Width, Height);
    try
      BeforePackImage(PackData, Image);
      try
        glReadPixels(0, 0, Width, Height, GL_DEPTH_COMPONENT,
          ImageGLType(Image), Image.RawPixels);
      finally AfterPackImage(PackData, Image) end;

      SaveImage(Image, FileName);
    finally FreeAndNil(Image) end;
  end;
{$endif DEBUG_SAVE_FRAMEBUFFER_DEPTH}

var
  PreviousFboDefaultBuffer: TGLenum;
begin
{$ifdef DEBUG_SAVE_FRAMEBUFFER_COLOR}
  if Buffer <> tbDepth then
    SaveColor('/tmp/framebuffer_color.png');
{$endif DEBUG_SAVE_FRAMEBUFFER_COLOR}
{$ifdef DEBUG_SAVE_FRAMEBUFFER_DEPTH}
  SaveDepth('/tmp/framebuffer_depth.png');
{$endif DEBUG_SAVE_FRAMEBUFFER_DEPTH}

  if Framebuffer <> 0 then
  begin
    Assert(FramebufferBound);
    if not RenderBeginFollows then
    begin
      UnbindFramebuffer(PreviousFboDefaultBuffer);
      FramebufferBound := false;

      if Buffer = tbDepth then
      begin
        glDrawBuffer(PreviousFboDefaultBuffer);
        glReadBuffer(PreviousFboDefaultBuffer);
      end;
    end;
  end else
  if Buffer <> tbNone then
  begin
    { Actually update OpenGL texture }
    glBindTexture(CompleteTextureTarget, Texture);
    glReadBuffer(GL_BACK);
    glCopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, Width, Height);

    if Buffer = tbColorAndDepth then
    begin
      glBindTexture(DepthTextureTarget, DepthTexture);
      glReadBuffer(GL_BACK);
      glCopyTexSubImage2D(DepthTextureTarget, 0, 0, 0, 0, 0, Width, Height);
    end;
  end;
end;

procedure TGLRenderToTexture.GenerateMipmap;
begin
  glBindTexture(CompleteTextureTarget, Texture);
  GLImages.GenerateMipmap(CompleteTextureTarget);
end;

function TGLRenderToTexture.ColorBuffer: TGLuint;
begin
  if Framebuffer <> 0 then
    Result := GL_COLOR_ATTACHMENT0 else
    Result := GL_BACK;
end;

finalization
  FreeAndNil(BoundFboStack);
end.
