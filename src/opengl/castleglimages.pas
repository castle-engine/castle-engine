{
  Copyright 2001-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Using images in OpenGL (as textures and as normal images).

  For non-OpenGL image management, see CastleImages and TextureImages units.
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
      You usually use this through TCastleWindowBase.SaveScreen
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

uses CastleGL, SysUtils, CastleImages, CastleVectors, CastleGLUtils,
  CastleVideos, CastleDDS, CastleRectangles, CastleGLShaders;

const
  PixelsImageClasses: array [0..3] of TCastleImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

type
  EImageClassNotSupportedForOpenGL = class(Exception);

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

{ Loading images ------------------------------------------------------------- }

type
  { Image ready to be drawn on 2D screen. }
  TGLImage = class
  private
    const
      PointCount = 4;
    type
      TPoint = packed record
        Position: TVector2SmallInt;
        TexCoord: TVector2Single;
      end;
    var
    { Static OpenGL resources, used by all TGLImage instances. }
    PointVbo: TGLuint; static;
    { Point VBO contents, reused in every Draw. }
    Point: array [0..PointCount - 1] of TPoint; static;
    {$ifdef GLImageUseShaders}
    GLSLProgram: array [boolean { alpha test? }] of TGLSLProgram; static;
    {$endif}

    Texture: TGLuint;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FAlpha: TAlphaChannel;
    procedure AlphaBegin;
    procedure AlphaEnd;
  public
    { Prepare image for drawing.

      @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
        by OpenGL.) }
    constructor Create(const Image: TCastleImage;
      const AScalingPossible: boolean = false);

    { Load image from disk, and prepare for drawing.

      @param(LoadAsClass Force a specific image class to load.
        Must be a subset of PixelsImageClasses, as other classes cannot
        be loaded into OpenGL 2D images, otherwise you may get
        EImageClassNotSupportedForOpenGL exception.
        Pass empty set [] to load into any allowed class
        (it's equivalent to passing LoadAsClass = PixelsImageClasses).

        You can pass e.g. [TRGBImage] to force loading into an RGB image without
        an alpha channel (it will be stripped from the image if necessary).)

      @param(ResizeToX After loading, resize to given width.
        Pass 0 to not resize width.)

      @param(ResizeToY After loading, resize to given height.
        Pass 0 to not resize height.)

      @param(Interpolation If any resizing will be needed (if
        ResizeToX / ResizeToY parameters request some specific size,
        and it is different than loaded image size) then the resize
        operation will use given interpolation.)

      @raises(EImageClassNotSupportedForOpenGL When image class is not supported
        by OpenGL.)
    }
    constructor Create(const URL: string;
      const LoadAsClass: array of TCastleImageClass;
      const ResizeToX: Cardinal = 0;
      const ResizeToY: Cardinal = 0;
      const Interpolation: TResizeInterpolation = riNearest);
    constructor Create(const URL: string;
      const LoadAsClass: array of TCastleImageClass;
      const AScalingPossible: boolean);

    destructor Destroy; override;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;

    { How to treat alpha channel of the texture.

      @unorderedList(
        @item acNone means to ignore it.
        @item acSimpleYesNo means to render with alpha-test.
        @item acFullRange means to render with blending.
      )

      This is initialized based on loaded image class and data.
      This means that e.g. if you have smooth alpha channel in the image,
      it will be automatically rendered with nice blending.

      You can change the value of this property to force a specific
      rendering method, for example to force using alpha test or alpha blending
      regardless of alpha values. Or to disable alpha channel usage,
      because your image must always cover pixels underneath.

      Remember that you can also change the alpha channel existence
      at loading: use LoadAsClass parameters of LoadImage
      or TGLImage.Create to force your image to have/don't have
      an alpha channel (e.g. use LoadAsClass=[TRGBImage]
      to force RGB image without alpha, use LoadAsClass=[TRGBAlphaImage]
      to force alpha channel). }
    property Alpha: TAlphaChannel read FAlpha write FAlpha;

    { Draw the image as 2D on screen.

      The X, Y parameters determine where the left-bottom
      corner of the image will be placed (from 0 to size - 1).
      The overloaded version without X, Y parameters uses current WindowPos.

      You should only use this inside TUIControl.Draw when TUIControl.DrawStyle
      returns ds2D. This means that we require that current projection is 2D
      and lighting / depth test and such are off.

      The image is drawn in 2D. In normal circumstances
      1 pixel of the image is just placed over 1 pixel of the screen,
      and we draw the whole image. You can also use the overloaded
      version with 8 parameters where you explicitly specify the
      DrawWidth and DrawHeight of the rectangle on the screen, and explicitly choose
      the portion of the image to draw. If you want to draw scaled image
      (that is, use ImageWidth different than DrawWidth or ImageHeight
      different than DrawHeight) be sure to construct an image with
      ScalingPossible = @true (otherwise runtime scaling may look ugly).

      Note that the image position (ImageX, ImageY) is specified
      like a texture coordinate. So (0, 0) is actually
      the left-bottom corner of the left-bottom pixel,
      and (Width,Height) is the right-top corner of the right-top pixel.
      That is why image position and sizes are floats, it makes sense
      to render partial pixels this way (make sure you have
      ScalingPossible = @true to get nice scaling of image contents).
      You can also flip the image horizontally or vertically,
      e.g. use ImageX = Width and ImageWidth = -Width to mirror
      image horizontally.

      @groupBegin }
    procedure Draw;
    procedure Draw(const X, Y: Integer);
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Integer;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single);
    procedure Draw(const ScreenRectangle: TRectangle);
    procedure Draw(const ScreenRectangle: TRectangle;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single);
    { @groupEnd }

    { Draw the image on the screen, divided into 3x3 parts for corners,
      sides, and inside.

      Just like the regular @link(Draw) method, this fills a rectangle on the
      2D screen, with bottom-left corner in (X, Y), and size (DrawWidth,
      DrawHeight). The image is divided into 3 * 3 = 9 parts:

      @unorderedList(
        @item(4 corners, used to fill the corners of the screen
          rectangle. They are not stretched.)
        @item(4 sides, used to fill the sides of the screen rectangle
          between the corners. They are scaled in one dimension, to fill
          the space between corners completely.)
        @item(the inside. Used to fill the rectangular inside.
          Scaled in both dimensions as necessary.)
      )
    }
    procedure Draw3x3(const X, Y, DrawWidth, DrawHeight: Integer;
      const CornerTop, CornerRight, CornerBottom, CornerLeft: Integer);
    procedure Draw3x3(const X, Y, DrawWidth, DrawHeight: Integer;
      const Corner: TVector4Integer);
    procedure Draw3x3(const ScreenRectangle: TRectangle;
      const Corner: TVector4Integer);
  end;

{ Draw the image on 2D screen. Note that if you want to use this
  many times, it will be much faster to create TGLImage instance.

  @deprecated Deprecated, always use TGLImage to draw 2D images.

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.) }
procedure ImageDraw(const Image: TCastleImage); deprecated;

{ Saving screen to TRGBImage ----------------------------------- }

type
  TColorBuffer = (
    cbFront,
    cbBack,
    cbColorAttachment0
  );

{ Notes about saving images from cbFront buffer:

  Don't do it. It just not defined what will be returned when you read from
  the front buffer. When our OpenGL context is covered by some other window,
  then glReadPixels *may* return pixels with contents of obscuring window.
  It doesn't help to draw right before trying to save buffer contents,
  reading from front buffer is just not reliable.

  The only reliable way to save screen contents is to draw something to back
  buffer and (without doing any swapbuffers) read it from cbBack buffer.
  This is only possible if you have double-buffered window, of course.
}

{ Save the current color buffer contents to image.

  The suffix "NoFlush" is there to remind you that this
  function grabs the @italic(current) buffer contents. Usually you want to
  call something like @link(TCastleWindowBase.FlushRedisplay) right before
  doing this. In practice, usually you don't want to use this function
  --- instead use safer TCastleWindowBase.SaveScreen.

  Version with ImageClass can save to any image format from PixelsImageClasses.

  Version with TCastleImage instance just uses this instance to save the image.
  You must pass here already created TCastleImage instance, it's class,
  Width and Height will be used when saving.

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)

  @groupBegin }
function SaveScreen_NoFlush(
  const Rect: TRectangle; const ReadBuffer: TColorBuffer): TRGBImage; overload;

function SaveScreen_NoFlush(const ImageClass: TCastleImageClass;
  const Rect: TRectangle; const ReadBuffer: TColorBuffer): TCastleImage; overload;

procedure SaveScreen_NoFlush(const Image: TCastleImage;
  const Left, Bottom: Integer; const ReadBuffer: TColorBuffer); overload;
{ @groupEnd }

{ Captures current screen as a TGLImage instance, ready to be drawn on 2D screen. }
function SaveScreenToGL_NoFlush(const Rect: TRectangle;
  const ReadBuffer: TColorBuffer;
  const ScalingPossible: boolean = false): TGLImage;

{ ----------------------------------------------------------------------
  Adjusting image sizes to load them as textures.
  Usually you don't need these functions, LoadGLTexture* and TGLImage
  and such call it automatically when needed. }

{ Resize the image to a size accepted as GL_TEXTURE_2D texture size
  for OpenGL. It tries to resize to a larger size, not smaller,
  to avoid losing image information.

  It also makes texture have power-of-two size, if AllowNonPowerOfTwo
  = @false. This is a must for normal textures, used for 3D rendering
  (with mipmapping and such).
  Using OpenGL non-power-of-2 textures is not good for such usage case,
  some OpenGLs crash (ATI),
  some are ultra slow (NVidia), some cause artifacts (Mesa).
  OpenGL ES explicitly limits what you can do with non-power-of-2.
  Sample model using non-power-of-2 is in inlined_textures.wrl.

  Use AllowNonPowerOfTwo = @true only for textures that you plan to use
  for drawing GUI images by TGLImage. Of course, be sure to check
  first does OpenGL support it at all (GLTextureNonPowerOfTwo).

  @groupBegin }
procedure ResizeForTextureSize(var r: TCastleImage; const AllowNonPowerOfTwo: boolean);
function ResizeToTextureSize(const r: TCastleImage; const AllowNonPowerOfTwo: boolean): TCastleImage;
{ @groupEnd }

{ Does image have proper size for 2D OpenGL texture.
  See ResizeForTextureSize. Note that this checks glGet(GL_MAX_TEXTURE_SIZE),
  so requires initialized OpenGL context. }
function IsTextureSized(const r: TEncodedImage; const AllowNonPowerOfTwo: boolean): boolean;

function IsTextureSized(const Width, Height: Cardinal; const AllowNonPowerOfTwo: boolean): boolean;
procedure ResizeToTextureSize(var Width, Height: Cardinal; const AllowNonPowerOfTwo: boolean);

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

{ TTextureFilter ------------------------------------------------------------- }

type
  { Texture minification filter (what happens when many texture pixels
    are squeezed in one screen pixel). }
  TMinificationFilter = (
    minNearest,
    minLinear,
    minNearestMipmapNearest,
    minNearestMipmapLinear,
    minLinearMipmapNearest,
    minLinearMipmapLinear);

  { Texture magnification filter (what happens when a single texture pixel
    in stretched over many screen pixels). }
  TMagnificationFilter = (magNearest, magLinear);

  TTextureFilter = object
  public
    Magnification: TMagnificationFilter;
    Minification: TMinificationFilter;
    function NeedsMipmaps: boolean;
  end;

operator = (const V1, V2: TTextureFilter): boolean;

function TextureFilter(const Minification: TMinificationFilter;
  const Magnification: TMagnificationFilter): TTextureFilter;

{ Set current texture minification and magnification filter.

  This is just a thin wrapper for calling
@longCode(#
  glTexParameteri(Target, GL_TEXTURE_MIN_FILTER, ...);
  glTexParameteri(Target, GL_TEXTURE_MAG_FILTER, ...);
#) }
procedure SetTextureFilter(const Target: TGLenum; const Filter: TTextureFilter);

{ Loading textures ----------------------------------------------------------- }

type
  { }
  ETextureLoadError = class(Exception);
  ECannotLoadS3TCTexture = class(ETextureLoadError);
  EInvalidImageForOpenGLTexture = class(ETextureLoadError);

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

  If mipmaps will be needed (this is decided looking at Filter.Minification)
  we will load them too.

  @orderedList(
    @item(
      As a first try, if DDSForMipmaps is non-nil
      and has mipmaps (DDSForMipmaps.Mipmaps), we will load these mipmaps.
      DDSForMipmaps must be a normal 2D texture (DDSType = dtTexture).

      Otherwise, we'll try to generate mipmaps, using various OpenGL mechanisms.)

    @item(
      We will try using GenerateMipmap functionality to generate mipmaps on GPU.
      If not available, for uncompressed textures, we will generate mipmaps on CPU.
      For compressed textures, we will change minification filter to simple
      minLinear and make OnWarning.)
  )

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason.
    This includes ECannotLoadS3TCTexture if the S3TC texture cannot be
    loaded for whatever reason.
    This includes EInvalidImageForOpenGLTexture if Image class is invalid
    for an OpenGL texture.)

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)

  @groupBegin }
function LoadGLTexture(const image: TEncodedImage;
  const Filter: TTextureFilter;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean = false;
  DDSForMipmaps: TDDSImage = nil): TGLuint; overload;

function LoadGLTexture(const URL: string;
  const Filter: TTextureFilter;
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

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)

  @groupBegin }
procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TEncodedImage;
  const Filter: TTextureFilter;
  const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean = false;
  DDSForMipmaps: TDDSImage = nil); overload;
{ @groupEnd }

type
  { Video as a sequence of OpenGL textures that can be easily played.
    Use TGLVideo3D to have a list of normal OpenGL textures,
    e.g. for rendering video as texture on free 3D objects.
    Use TGLVideo2D to have a list of GUI textures (TGLImage),
    e.g. for rendering video as simple 2D control. }
  TGLVideo = class
  private
    FCount: Integer;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FFramesPerSecond: Single;
    FWidth, FHeight: Cardinal;
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
    constructor Create(Video: TVideo);

    property Count: Integer read FCount;
    function IndexFromTime(const Time: Single): Integer;

    { See TVideo.FramesPerSecond. }
    property FramesPerSecond: Single read FFramesPerSecond write FFramesPerSecond;

    { See TVideo.TimeLoop. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop;

    { See TVideo.TimeBackwards. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
  end;

  { Video expressed as a series of textures, to play as texture on any 3D object. }
  TGLVideo3D = class(TGLVideo)
  private
    FItems: array of TGLuint;
  public
    constructor Create(Video: TVideo;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const Wrap: TTextureWrap2D);
    destructor Destroy; override;

    function GLTextureFromTime(const Time: Single): TGLuint;
  end;

  { Video expressed as a series of TGLImage, to play as 2D GUI control. }
  TGLVideo2D = class(TGLVideo)
  private
    FItems: array of TGLImage;
  public
    constructor Create(Video: TVideo;
      const ScalingPossible: boolean = false);
    constructor Create(const URL: string;
      const ScalingPossible: boolean = false);
    constructor Create(const URL: string;
      const ResizeToX: Cardinal = 0;
      const ResizeToY: Cardinal = 0;
      const Interpolation: TResizeInterpolation = riBilinear);
    destructor Destroy; override;

    function GLImageFromTime(const Time: Single): TGLImage;
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

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)
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

  If Filter uses mipmaps, then all mipmap levels will be loaded.

  @orderedList(
    @item(
      As a first try, if DDSForMipmaps is non-nil
      and has mipmaps (DDSForMipmaps.Mipmaps), we will load these mipmaps.
      DDSForMipmaps must be a 3D texture (DDSType = dtVolume).)

    @item(Otherwise, we'll generate mipmaps.

      GenerateMipmap functionality will be required for this.
      When it is not available on this OpenGL implementation,
      we will change minification filter to simple linear and make OnWarning.
      So usually you just don't have to worry about this.)
  )

  @raises(ETextureLoadError If texture cannot be loaded for whatever reason,
    for example it's size is not correct for OpenGL 3D texture (we cannot
    automatically resize 3D textures, at least for now).
    Or it's compressed (although we support here TEncodedImage,
    OpenGL doesn't have any 3D texture compression available.))

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.)
}
procedure glTextureImage3D(const Image: TEncodedImage;
  Filter: TTextureFilter; DDSForMipmaps: TDDSImage);

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

procedure SetReadBuffer(const Buffer: TGLEnum);
procedure SetDrawBuffer(const Buffer: TGLEnum);

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
      for cube map it should be GL_TEXTURE_CUBE_MAP. }
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
      SaveScreen_NoFlush, currently it's just rbColorAttachment0
      if we actually have FBO or rbBack if not. }
    function ColorBuffer: TColorBuffer;

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

uses CastleUtils, CastleLog, CastleGLVersion, CastleWarnings, CastleTextureImages,
  CastleColors, CastleUIControls;

function ImageGLFormat(const Img: TCastleImage): TGLenum;
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
    raise EImageClassNotSupportedForOpenGL.CreateFmt('Image class %s cannot be loaded to OpenGL', [Img.ClassName]);
end;

function ImageGLInternalFormat(const Img: TEncodedImage): TGLenum;
begin
  if Img is TCastleImage then
    Result := {$ifdef OpenGLES} ImageGLFormat(TCastleImage(Img))
              {$else} TCastleImage(Img).ColorComponentsCount
              {$endif} else
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

{ TGLImage ------------------------------------------------------------------- }

constructor TGLImage.Create(const Image: TCastleImage;
  const AScalingPossible: boolean);
var
  UnpackData: TUnpackNotAlignedData;
  NewImage: TCastleImage;

  { Load an image to Texture, knowing that Image has already good sizes for OpenGL. }
  procedure LoadImage(const Image: TCastleImage);
  begin
    BeforeUnpackImage(UnpackData, Image);
    try
      glTexImage2D(GL_TEXTURE_2D, 0, ImageGLInternalFormat(Image),
        Image.Width, Image.Height, 0, ImageGLFormat(Image), ImageGLType(Image),
        Image.RawPixels);
    finally AfterUnpackImage(UnpackData, image) end;
  end;

var
  Filter: TTextureFilter;
  {$ifdef GLImageUseShaders}
  AlphaTestShader: boolean;
  {$endif}
begin
  inherited Create;

  // TODO: use texture cache here, like GL renderer does for textures for 3D.
  // no need to create new OpenGL texture for the same image.

  glGenTextures(1, @Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  if not IsTextureSized(Image, GLFeatures.TextureNonPowerOfTwo) then
  begin
    NewImage := ResizeToTextureSize(Image, GLFeatures.TextureNonPowerOfTwo);
    try
      LoadImage(NewImage);
    finally FreeAndNil(NewImage) end;
  end else
    LoadImage(Image);
  if AScalingPossible then
  begin
    Filter.Minification := minLinear;
    Filter.Magnification := magLinear;
  end else
  begin
    Filter.Minification := minNearest;
    Filter.Magnification := magNearest;
  end;
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GLFeatures.CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GLFeatures.CLAMP_TO_EDGE);
  FWidth := Image.Width;
  FHeight := Image.Height;
  FAlpha := Image.AlphaChannel;

  if PointVbo = 0 then
    glGenBuffers(1, @PointVbo);

  {$ifdef GLImageUseShaders}
  for AlphaTestShader := false to true do
    if GLSLProgram[AlphaTestShader] = nil then
    begin
      GLSLProgram[AlphaTestShader] := TGLSLProgram.Create;
      GLSLProgram[AlphaTestShader].AttachVertexShader({$I image.vs.inc});
      GLSLProgram[AlphaTestShader].AttachFragmentShader(
        Iff(AlphaTestShader, '#define ALPHA_TEST' + NL, '') +
        {$I image.fs.inc});
      GLSLProgram[AlphaTestShader].Link(true);
    end;
  {$endif}
end;

constructor TGLImage.Create(const URL: string;
  const LoadAsClass: array of TCastleImageClass;
  const ResizeToX, ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation);
var
  Image: TCastleImage;
begin
  if High(LoadAsClass) = -1 then
    Image := LoadImage(URL, PixelsImageClasses, ResizeToX, ResizeToY, Interpolation) else
    Image := LoadImage(URL, LoadAsClass, ResizeToX, ResizeToY, Interpolation);
  try
    Create(Image);
  finally FreeAndNil(Image) end;
end;

constructor TGLImage.Create(const URL: string;
  const LoadAsClass: array of TCastleImageClass;
  const AScalingPossible: boolean);
var
  Image: TCastleImage;
begin
  if High(LoadAsClass) = -1 then
    Image := LoadImage(URL, PixelsImageClasses) else
    Image := LoadImage(URL, LoadAsClass);
  try
    Create(Image, AScalingPossible);
  finally FreeAndNil(Image) end;
end;

destructor TGLImage.Destroy;
begin
  glFreeTexture(Texture);
  inherited;
end;

procedure TGLImage.AlphaBegin;
begin
  case Alpha of
    {$ifndef GLImageUseShaders}
    acSimpleYesNo:
      begin
        glAlphaFunc(GL_GEQUAL, 0.5); // saved by GL_COLOR_BUFFER_BIT
        glEnable(GL_ALPHA_TEST); // saved by GL_COLOR_BUFFER_BIT
      end;
    {$endif}
    acFullRange:
      begin
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // saved by GL_COLOR_BUFFER_BIT
        glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
      end;
  end;
end;

procedure TGLImage.AlphaEnd;
begin
  case Alpha of
    {$ifndef GLImageUseShaders}
    acSimpleYesNo: glDisable(GL_ALPHA_TEST);
    {$endif}
    acFullRange: glDisable(GL_BLEND);
  end;
end;

procedure TGLImage.Draw;
begin
  Draw(WindowPos[0], WindowPos[1]);
end;

procedure TGLImage.Draw(const X, Y: Integer);
begin
  Draw(X, Y, Width, Height, 0, 0, Width, Height);
end;

procedure TGLImage.Draw(const X, Y, DrawWidth, DrawHeight: Integer;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single);
var
  TexX0, TexY0, TexX1, TexY1: Single;
  {$ifdef GLImageUseShaders}
  AttribEnabled: array [0..1] of TGLuint;
  AttribLocation: TGLuint;
  Prog: TGLSLProgram;
  {$endif}
begin
  if GLFeatures.UseMultiTexturing then glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Texture);
  GLEnableTexture(et2D);

  TexX0 := ImageX / Width;
  TexY0 := ImageY / Height;
  TexX1 := (ImageX + ImageWidth ) / Width;
  TexY1 := (ImageY + ImageHeight) / Height;

  Point[0].TexCoord := Vector2Single(TexX0, TexY0);
  Point[0].Position := Vector2SmallInt(X            , Y);
  Point[1].TexCoord := Vector2Single(TexX1, TexY0);
  Point[1].Position := Vector2SmallInt(X + DrawWidth, Y);
  Point[2].TexCoord := Vector2Single(TexX1, TexY1);
  Point[2].Position := Vector2SmallInt(X + DrawWidth, Y + DrawHeight);
  Point[3].TexCoord := Vector2Single(TexX0, TexY1);
  Point[3].Position := Vector2SmallInt(X            , Y + DrawHeight);

  glBindBuffer(GL_ARRAY_BUFFER, PointVbo);
  glBufferData(GL_ARRAY_BUFFER, PointCount * SizeOf(TPoint),
    @Point[0], GL_STREAM_DRAW);

  AlphaBegin;

  {$ifdef GLImageUseShaders}
  Prog := GLSLProgram[Alpha = acSimpleYesNo];
  Prog.Enable;
  AttribEnabled[0] := Prog.VertexAttribPointer('vertex', 0, 2, GL_SHORT, GL_FALSE,
    SizeOf(TPoint), Offset(Point[0].Position, Point[0]));
  AttribEnabled[1] := Prog.VertexAttribPointer('tex_coord', 0, 2, GL_FLOAT, GL_FALSE,
    SizeOf(TPoint), Offset(Point[0].TexCoord, Point[0]));
  Prog.SetUniform('projection_matrix', ProjectionMatrix);

  {$else}
  glLoadIdentity();
  glColorv(White); // don't modify texture colors

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glVertexPointer(2, GL_SHORT,
    SizeOf(TPoint), Offset(Point[0].Position, Point[0]));
  glTexCoordPointer(2, GL_FLOAT,
    SizeOf(TPoint), Offset(Point[0].TexCoord, Point[0]));
  {$endif}

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  {$ifdef GLImageUseShaders}
  Prog.Disable;
  { attribute arrays are enabled independent from GLSL program, so we need
    to disable them separately }
  for AttribLocation in AttribEnabled do
    TGLSLProgram.DisableVertexAttribArray(AttribLocation);
  {$else}
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  {$endif}

  AlphaEnd;

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  GLEnableTexture(etNone);
end;

procedure TGLImage.Draw(const ScreenRectangle: TRectangle);
begin
  Draw(ScreenRectangle.Left, ScreenRectangle.Bottom,
    ScreenRectangle.Width, ScreenRectangle.Height,
    0, 0, Width, Height);
end;

procedure TGLImage.Draw(const ScreenRectangle: TRectangle;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single);
begin
  Draw(ScreenRectangle.Left, ScreenRectangle.Bottom,
    ScreenRectangle.Width, ScreenRectangle.Height,
    ImageX, ImageY, ImageWidth, ImageHeight);
end;

procedure TGLImage.Draw3x3(const X, Y, DrawWidth, DrawHeight: Integer;
  const CornerTop, CornerRight, CornerBottom, CornerLeft: Integer);
var
  XScreenLeft, XScreenRight, YScreenBottom, YScreenTop,
    HorizontalScreenSize, VerticalScreenSize: Integer;
  XImageLeft, XImageRight, YImageBottom, YImageTop,
    HorizontalImageSize, VerticalImageSize: Single;
  OldAlpha: TAlphaChannel;
  {$ifdef GLImageUseShaders}
  OptimizeAlpha: boolean;
  {$endif}
const
  { We tweak texture coordinates a little, to avoid bilinear filtering
    that would cause border colors to "bleed" over the texture inside.
    Something minimally > 0.5 is necessary. }
  Epsilon = 0.51;
begin
  if not ( (CornerLeft + CornerRight < Width) and
           (CornerLeft + CornerRight < DrawWidth) and
           (CornerBottom + CornerTop < Height) and
           (CornerBottom + CornerTop < DrawHeight)) then
  begin
    if Log then
      WritelnLog('Draw3x3', 'Image corners are too large to draw it: corners are %d %d %d %d, image size is %d %d, draw area size is %d %d',
        [CornerTop, CornerRight, CornerBottom, CornerLeft,
         Width, Height,
         DrawWidth, DrawHeight]);
    Exit;
  end;

  XScreenLeft := X;
  XImageLeft := 0;
  XScreenRight := X + DrawWidth - CornerRight;
  XImageRight := Width - CornerRight;

  YScreenBottom := Y;
  YImageBottom := 0;
  YScreenTop := Y + DrawHeight - CornerTop;
  YImageTop := Height - CornerTop;

  { For speed, we only apply AlphaBegin/End once.
    In case of GLImageUseShaders, this optimization is only useful
    for acFullRange, and it would actually break the acSimpleYesNo case. }
  {$ifdef GLImageUseShaders}
  OptimizeAlpha := Alpha = acFullRange;
  if OptimizeAlpha then
  {$endif}
  begin
    AlphaBegin;
    OldAlpha := Alpha;
    Alpha := acNone;
  end;

  { 4 corners }
  Draw(XScreenLeft, YScreenBottom, CornerLeft, CornerBottom,
        XImageLeft,  YImageBottom, CornerLeft, CornerBottom);
  Draw(XScreenRight, YScreenBottom, CornerRight, CornerBottom,
        XImageRight,  YImageBottom, CornerRight, CornerBottom);
  Draw(XScreenRight, YScreenTop, CornerRight, CornerTop,
        XImageRight,  YImageTop, CornerRight, CornerTop);
  Draw(XScreenLeft, YScreenTop, CornerLeft, CornerTop,
        XImageLeft,  YImageTop, CornerLeft, CornerTop);

  { 4 sides }
  HorizontalScreenSize := DrawWidth - CornerLeft - CornerRight;
  HorizontalImageSize  :=     Width - CornerLeft - CornerRight;
  VerticalScreenSize := DrawHeight - CornerTop - CornerBottom;
  VerticalImageSize  :=     Height - CornerTop - CornerBottom;

  Draw(XScreenLeft + CornerLeft, YScreenBottom, HorizontalScreenSize, CornerBottom,
        XImageLeft + CornerLeft,  YImageBottom,  HorizontalImageSize, CornerBottom);
  Draw(XScreenLeft + CornerLeft, YScreenTop, HorizontalScreenSize, CornerTop,
        XImageLeft + CornerLeft,  YImageTop,  HorizontalImageSize, CornerTop);

  Draw(XScreenLeft, YScreenBottom + CornerBottom, CornerLeft, VerticalScreenSize,
        XImageLeft,  YImageBottom + CornerBottom, CornerLeft,  VerticalImageSize);
  Draw(XScreenRight, YScreenBottom + CornerBottom, CornerRight, VerticalScreenSize,
        XImageRight,  YImageBottom + CornerBottom, CornerRight,  VerticalImageSize);

  { inside }
  Draw(X + CornerLeft          , Y + CornerBottom          , HorizontalScreenSize              , VerticalScreenSize,
           CornerLeft + Epsilon,     CornerBottom + Epsilon,  HorizontalImageSize - 2 * Epsilon,  VerticalImageSize - 2 * Epsilon);

  {$ifdef GLImageUseShaders}
  if OptimizeAlpha then
  {$endif}
  begin
    Alpha := OldAlpha;
    AlphaEnd;
  end;
end;

procedure TGLImage.Draw3x3(const X, Y, DrawWidth, DrawHeight: Integer;
  const Corner: TVector4Integer);
begin
  Draw3x3(X, Y, DrawWidth, DrawHeight,
    Corner[0], Corner[1], Corner[2], Corner[3]);
end;

procedure TGLImage.Draw3x3(const ScreenRectangle: TRectangle;
  const Corner: TVector4Integer);
begin
  Draw3x3(ScreenRectangle.Left, ScreenRectangle.Bottom,
    ScreenRectangle.Width, ScreenRectangle.Height,
    Corner[0], Corner[1], Corner[2], Corner[3]);
end;

{ Drawing images on 2D screen ------------------------------------------------ }

procedure ImageDraw(const Image: TCastleImage);
var
  GLImage: TGLImage;
begin
  GLImage := TGLImage.Create(Image);
  try
    GLImage.Draw(0, 0);
  finally FreeAndNil(GLImage) end;
end;

{ Saving screen to TRGBImage ------------------------------------------------ }

const
  ColorBufferGL: array [TColorBuffer] of TGLenum = (
    GL_FRONT,
    GL_BACK,
    GL_COLOR_ATTACHMENT0
  );

{ This is the basis for all other SaveScreen* functions below. }
procedure SaveScreen_NoFlush(const Image: TCastleImage;
  const Left, Bottom: Integer; const ReadBuffer: TColorBuffer);
var
  PackData: TPackNotAlignedData;
begin
  BeforePackNotAlignedRGBImage(packData, Image.width);
  try
    SetReadBuffer(ColorBufferGL[ReadBuffer]);
    glReadPixels(Left, Bottom, Image.width, Image.height, ImageGLFormat(Image),
      ImageGLType(Image), Image.RawPixels);
  finally AfterPackNotAlignedRGBImage(packData, Image.width) end;
end;

function SaveScreen_NoFlush(const ImageClass: TCastleImageClass;
  const Rect: TRectangle; const ReadBuffer: TColorBuffer): TCastleImage;
begin
  Result := ImageClass.Create(Rect.Width, Rect.Height);
  try
    SaveScreen_NoFlush(Result, Rect.Left, Rect.Bottom, ReadBuffer);
  except Result.Free; raise end;
end;

function SaveScreen_NoFlush(const Rect: TRectangle;
  const ReadBuffer: TColorBuffer): TRGBImage;
begin
  Result := SaveScreen_NoFlush(TRGBImage, Rect, ReadBuffer) as TRGBImage;
end;

function SaveScreenToGL_NoFlush(const Rect: TRectangle;
  const ReadBuffer: TColorBuffer;
  const ScalingPossible: boolean): TGLImage;
var
  ScreenImage: TRGBImage;
begin
  ScreenImage := SaveScreen_NoFlush(Rect, ReadBuffer);
  try
    Result := TGLImage.Create(ScreenImage, ScalingPossible);
  finally FreeAndNil(ScreenImage) end;
end;

{ ----------------------------------------------------------------------
  Adjusting image size for 2D texture. }

function IsTextureSized(const Width, Height: Cardinal;
  const AllowNonPowerOfTwo: boolean): boolean;
begin
  if AllowNonPowerOfTwo then
    Result :=
      (Width <= GLFeatures.MaxTextureSize) and
      (Height <= GLFeatures.MaxTextureSize) else
    Result :=
      IsPowerOf2(Width) and
      IsPowerOf2(Height) and
      (Width <= GLFeatures.MaxTextureSize) and
      (Height <= GLFeatures.MaxTextureSize);
end;

function IsTextureSized(const r: TEncodedImage; const AllowNonPowerOfTwo: boolean): boolean;
begin
  Result := IsTextureSized(r.Width, r.Height, AllowNonPowerOfTwo);
end;

procedure ResizeForTextureSize(var r: TCastleImage; const AllowNonPowerOfTwo: boolean);
var
  newR: TCastleImage;
begin
  if not IsTextureSized(r, AllowNonPowerOfTwo) then
  begin
    newR := ResizeToTextureSize(r, AllowNonPowerOfTwo);
    FreeAndNil(r);
    r := newR;
  end;
end;

procedure ResizeToTextureSize(var Width, Height: Cardinal; const AllowNonPowerOfTwo: boolean);

  function BestTexSize(size: Cardinal): Cardinal;
  begin
    if size > GLFeatures.MaxTextureSize then
      result := GLFeatures.MaxTextureSize else
    begin
      if AllowNonPowerOfTwo or IsPowerOf2(size) then
        result := size else
        result := 1 shl (Biggest2Exponent(size)+1);
        {result jakie otrzymamy w ostatnim przypisaniu jest na pewno < GLFeatures.MaxTextureSize bo
         skoro size <= GLFeatures.MaxTextureSize i not IsPowerOf2(size) to size < GLFeatures.MaxTextureSize a GLFeatures.MaxTextureSize
         samo jest potega dwojki. }
     end;
  end;

begin
  Width  := BestTexSize(Width );
  Height := BestTexSize(Height);
end;

function ResizeToTextureSize(const r: TCastleImage; const AllowNonPowerOfTwo: boolean): TCastleImage;
var
  NewWidth, NewHeight: Cardinal;
begin
  NewWidth  := R.Width ;
  NewHeight := R.Height;
  ResizeToTextureSize(NewWidth, NewHeight, AllowNonPowerOfTwo);

  if Log then
    WritelnLog('Textures', Format('Resizing 2D texture from %dx%d to %dx%d to satisfy OpenGL',
      [R.Width, R.Height, NewWidth, NewHeight]));

  result := r.MakeResized(NewWidth, NewHeight, riBilinear);
end;

{ ----------------------------------------------------------------------------
  Adjusting image size for cube map texture. }

function IsCubeMapTextureSized(const Size: Cardinal): boolean;
begin
  Result :=
    (GLFeatures.TextureCubeMap = gsNone) or
    (
      IsPowerOf2(Size) and
      (Size > 0) and
      (Size <= GLFeatures.MaxCubeMapTextureSize)
    );
end;

function IsCubeMapTextureSized(const R: TEncodedImage): boolean;
begin
  Result :=
    (GLFeatures.TextureCubeMap = gsNone) or
    (
      (r.Width = r.Height) { must be square } and
      IsPowerOf2(r.Width) and
      (r.Width > 0) and
      (r.Width <= GLFeatures.MaxCubeMapTextureSize)
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
  if GLFeatures.TextureCubeMap <> gsNone then
  begin
    if Size <= 0 then
      Result := 1 else
    if Size > GLFeatures.MaxCubeMapTextureSize then
      Result := GLFeatures.MaxCubeMapTextureSize else
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
  if GLFeatures.TextureCubeMap <> gsNone then
  begin
    Size := Max(r.Width, r.Height);
    Size := ResizeToCubeMapTextureSize(Size);

    if Log then
      WritelnLog('Texture loading', Format('Resizing image for cube map texture from (%d, %d) to (%d, %d)',
        [R.Width, R.Height, Size, Size]));

    result := r.MakeResized(Size, Size, riBilinear);
  end else
    result := r.MakeCopy;
end;

{ ----------------------------------------------------------------------------
  Adjusting image size for 3d texture. }

function IsTexture3DSized(const Size: Cardinal): boolean;
begin
  Result :=
    (GLFeatures.Texture3D = gsNone) or
    (
      IsPowerOf2(Size) and
      (Size > 0) and
      (Size <= GLFeatures.MaxTexture3DSize)
    );
end;

function IsTexture3DSized(const R: TCastleImage): boolean;
begin
  if GLFeatures.Texture3D <> gsNone then
  begin
    Result :=
      IsPowerOf2(R.Width ) and (R.Width  > 0) and (R.Width  <= GLFeatures.MaxTexture3DSize) and
      IsPowerOf2(R.Height) and (R.Height > 0) and (R.Height <= GLFeatures.MaxTexture3DSize) and
      IsPowerOf2(R.Depth ) and (R.Depth  > 0) and (R.Depth  <= GLFeatures.MaxTexture3DSize);
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
  Result[0] := GLFeatures.CLAMP_TO_EDGE;
  Result[1] := Result[0];
end;

{ TTextureFilter ------------------------------------------------------------- }

procedure SetTextureFilter(const Target: TGLenum; const Filter: TTextureFilter);
const
  MinFilterGL: array [TMinificationFilter] of TGLint =
  ( GL_NEAREST,
    GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_LINEAR );
  MagFilterGL: array [TMagnificationFilter] of TGLint =
  ( GL_NEAREST,
    GL_LINEAR );
begin
  glTexParameteri(Target, GL_TEXTURE_MIN_FILTER, MinFilterGL[Filter.Minification]);
  glTexParameteri(Target, GL_TEXTURE_MAG_FILTER, MagFilterGL[Filter.Magnification]);
end;

function TTextureFilter.NeedsMipmaps: boolean;
begin
  Result := Minification in
    [ minNearestMipmapNearest,
      minNearestMipmapLinear,
      minLinearMipmapNearest,
      minLinearMipmapLinear ];
end;

operator = (const V1, V2: TTextureFilter): boolean;
begin
  Result :=
    (V1.Minification  = V2.Minification ) and
    (V1.Magnification = V2.Magnification);
end;

function TextureFilter(const Minification: TMinificationFilter;
  const Magnification: TMagnificationFilter): TTextureFilter;
begin
  Result.Minification := Minification;
  Result.Magnification := Magnification;
end;

{ 2D texture loading --------------------------------------------------------- }

function LoadGLTexture(const image: TEncodedImage;
  const Filter: TTextureFilter; const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean; DDSForMipmaps: TDDSImage): TGLuint;
begin
  glGenTextures(1, @result);
  LoadGLGeneratedTexture(result, image, Filter, Wrap, GrayscaleIsAlpha, DDSForMipmaps);
end;

function LoadGLTexture(const URL: string;
  const Filter: TTextureFilter; const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean; DDSForMipmaps: TDDSImage): TGLuint;
var
  Image: TEncodedImage;
begin
  Image := LoadTextureImage(URL);
  try
    Result := LoadGLTexture(Image, Filter, Wrap, GrayscaleIsAlpha, DDSForMipmaps);
  finally Image.Free end;
end;

{$ifndef OpenGLES}
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
  if not GLFeatures.TextureCompressionS3TC then
    raise ECannotLoadS3TCTexture.Create('Cannot load S3TC compressed textures: OpenGL doesn''t support one (or both) of ARB_texture_compression and EXT_texture_compression_s3tc extensions');

  if not IsTextureSized(Image, false) then
    raise ECannotLoadS3TCTexture.CreateFmt('Cannot load S3TC compressed textures: texture size is %d x %d, it''s not correct for OpenGL, and we cannot resize on CPU compressed textures',
      [Image.Width, Image.Height]);

  { Pixel packing parameters (stuff changed by Before/AfterUnpackImage)
    doesn't affect loading compressed textures, as far as I understand.
    So no need to call it. }
  glCompressedTexImage2DARB(GL_TEXTURE_2D, Level, ImageGLInternalFormat(Image),
    Image.Width, Image.Height, 0, Image.Size,
    Image.RawPixels);
end;
{$endif}

procedure LoadGLGeneratedTexture(texnum: TGLuint; const image: TEncodedImage;
  const Filter: TTextureFilter; const Wrap: TTextureWrap2D;
  GrayscaleIsAlpha: boolean; DDSForMipmaps: TDDSImage);
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
        {$ifndef OpenGLES}
        { Workaround Mesa 7.9-devel bug (at least with Intel DRI,
          on Ubuntu 10.10, observed on domek): glTexImage2D accidentaly
          enables GL_TEXTURE_2D. }
        if GLVersion.Mesa then glPushAttrib(GL_ENABLE_BIT);
        {$endif}

        glTexImage2D(GL_TEXTURE_2D, Level, ImageInternalFormat,
          Image.Width, Image.Height, 0, ImageFormat, ImageGLType(Image),
          Image.RawPixels);

        {$ifndef OpenGLES}
        if GLVersion.Mesa then glPopAttrib;
        {$endif}
      finally AfterUnpackImage(UnpackData, Image) end;
    end;

  var
    ImgGood: TCastleImage;
  begin
    if IsTextureSized(Image, false) then
      Core(Image) else
    begin
      ImgGood := ResizeToTextureSize(Image, false);
      try
        Core(ImgGood);
      finally ImgGood.Free end;
    end;
  end;

  procedure LoadNormal(const image: TCastleImage);
  begin
    glTexImage2DImage(Image, 0);
  end;

  {$ifndef OpenGLES}
  { Check should we load mipmaps from DDS. Load them, if yes.

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

    if Result and (not GLFeatures.Version_1_2) then
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
  {$endif}

  procedure LoadMipmapped(const image: TCastleImage);
  begin
    {$ifndef OpenGLES}
    if not LoadMipmapsFromDDS(DDSForMipmaps, true) then
    if HasGenerateMipmap then
    begin
    {$endif}
      glTexImage2DImage(Image, 0);
      { hardware-accelerated mipmap generation }
      GenerateMipmap(GL_TEXTURE_2D);
    {$ifndef OpenGLES}
    end else
      gluBuild2DMipmapsImage(Image);
    {$endif}
  end;

begin
  { bind the texture, set min, mag filters and wrap parameters }
  glBindTexture(GL_TEXTURE_2D, texnum);
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Wrap[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Wrap[1]);

  { give the texture data }
  if Image is TCastleImage then
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
      ImageFormat := ImageGLFormat(TCastleImage(Image));
    end;

    { Load uncompressed }
    if Filter.NeedsMipmaps then
      LoadMipmapped(TCastleImage(Image)) else
      LoadNormal(TCastleImage(Image));
  end else
  {$ifndef OpenGLES}
  if Image is TS3TCImage then
  begin
    { Load compressed }
    glCompressedTextureImage2D(TS3TCImage(Image), 0);

    if Filter.NeedsMipmaps then
    begin
      if not LoadMipmapsFromDDS(DDSForMipmaps, false) then
      try
        GenerateMipmap(GL_TEXTURE_2D);
      except
        on E: EGenerateMipmapNotAvailable do
        begin
          { Update GL_TEXTURE_MIN_FILTER, since we already initialized it earlier. }
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
          OnWarning(wtMinor, 'Texture', 'Creating mipmaps for S3TC compressed textures requires GenerateMipmap functionality, will fallback to GL_LINEAR minification: ' + E.Message);
        end;
      end;
    end;
  end else
  {$endif}
    raise EInvalidImageForOpenGLTexture.CreateFmt('Cannot load to OpenGL texture image class %s', [Image.ClassName]);
end;

{ TGLVideo ------------------------------------------------------------------- }

constructor TGLVideo.Create(Video: TVideo);
begin
  inherited Create;
  Check(Video.Loaded, 'Video must be loaded before using TGLVideo.Create');
  FCount := Video.Count;
  FTimeLoop := Video.TimeLoop;
  FTimeBackwards := Video.TimeBackwards;
  FFramesPerSecond := Video.FramesPerSecond;
  FWidth := Video.Width;
  FHeight := Video.Height
end;

function TGLVideo.IndexFromTime(const Time: Single): Integer;
begin
  Result := TVideo.FrameIndexFromTime(Time, Count, FramesPerSecond,
    TimeLoop, TimeBackwards);
end;

{ TGLVideo3D ----------------------------------------------------------------- }

constructor TGLVideo3D.Create(Video: TVideo;
  const Filter: TTextureFilter; const Anisotropy: TGLfloat;
  const Wrap: TTextureWrap2D);
var
  I: Integer;
begin
  inherited Create(Video);

  SetLength(FItems, Count);
  for I := 0 to High(FItems) do
  begin
    FItems[I] := LoadGLTexture(Video.Items[I], Filter, Wrap);
    TexParameterMaxAnisotropy(GL_TEXTURE_2D, Anisotropy);
  end;
end;

destructor TGLVideo3D.Destroy;
begin
  if Count <> 0 then
    glDeleteTextures(Count, @FItems[0]);
  inherited;
end;

function TGLVideo3D.GLTextureFromTime(const Time: Single): TGLuint;
begin
  Result := FItems[IndexFromTime(Time)];
end;

{ TGLVideo2D ----------------------------------------------------------------- }

constructor TGLVideo2D.Create(Video: TVideo;
  const ScalingPossible: boolean = false);
var
  I: Integer;
begin
  inherited Create(Video);

  SetLength(FItems, Count);
  for I := 0 to High(FItems) do
    FItems[I] := TGLImage.Create(Video.Items[I], ScalingPossible);
end;

constructor TGLVideo2D.Create(const URL: string;
  const ScalingPossible: boolean = false);
var
  Video: TVideo;
begin
  Video := TVideo.Create;
  try
    Video.LoadFromFile(URL);
    Create(Video, ScalingPossible);
  finally FreeAndNil(Video) end;
end;

constructor TGLVideo2D.Create(const URL: string;
  const ResizeToX: Cardinal;
  const ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation);
var
  Video: TVideo;
begin
  Video := TVideo.Create;
  try
    Video.LoadFromFile(URL, ResizeToX, ResizeToY, Interpolation);
    Create(Video);
  finally FreeAndNil(Video) end;
end;

destructor TGLVideo2D.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FreeAndNil(FItems[I]);
  inherited;
end;

function TGLVideo2D.GLImageFromTime(const Time: Single): TGLImage;
begin
  Result := FItems[IndexFromTime(Time)];
end;

{ Cube map texture loading --------------------------------------------------- }

{$ifndef OpenGLES}

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

  { Load Image through glCompressedTexImage2DARB.
    This checks existence of OpenGL extensions for S3TC,
    and checks Image sizes.
    It also takes care of pixel packing, although actually nothing needs
    be done about it when using compressed textures.

    @raises(ECannotLoadS3TCTexture If texture size is bad or OpenGL S3TC
      extensions are missing or mipmaps were required.) }
  procedure LoadCompressed(const Image: TS3TCImage);
  begin
    if not GLFeatures.TextureCompressionS3TC then
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

var
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
      TODO: But GenerateMipmap should work? Test.
    }

    gluBuild2DMipmapsImage(Image);
  end;

  procedure LoadNormal(const image: TCastleImage);
  begin
    glTexImage2DImage(Image);
  end;

begin
  ImageInternalFormat := ImageGLInternalFormat(Image);
  if Image is TS3TCImage then
    LoadCompressed(TS3TCImage(Image)) else
  if Image Is TCastleImage then
  begin
    ImageFormat := ImageGLFormat(TCastleImage(Image));
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
    if not GLFeatures.Version_1_2 then
    begin
      OnWarning(wtMinor, 'Texture', 'Cannot load DDS image containing mipmaps, because OpenGL 1.2 not available (GL_TEXTURE_MAX_LEVEL not available)');
      Exit;
    end;

    glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAX_LEVEL, DDS.MipmapsCount - 1);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X, dcsPositiveX);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, dcsNegativeX);
    { Note Positive/Negative are swapped for Y.
      DDS cube map sides are in left-handed coordinate system, like Direct X.
      See TDDSCubeMapSide comments. }
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, dcsNegativeY);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, dcsPositiveY);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, dcsPositiveZ);
    LoadMipmapsFromDDSSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, dcsNegativeZ);
  end;

begin
  if Mipmaps and (HasMipmapsFromDDS(DDSForMipmaps) or HasGenerateMipmap) then
  begin
    { Load six cube faces without mipmaps, then generate them all
      in one go with GenerateMipmap. }
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X, PositiveX, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, NegativeX, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, PositiveY, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, NegativeY, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, PositiveZ, 0, false);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, NegativeZ, 0, false);
    if HasMipmapsFromDDS(DDSForMipmaps) then
      LoadMipmapsFromDDS(DDSForMipmaps) else
    begin
      GenerateMipmap(GL_TEXTURE_CUBE_MAP);
      if Log then
        WritelnLog('Mipmaps', 'Generating mipmaps for cube map by GenerateMipmap (GOOD)');
    end;
  end else
  begin
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_X, PositiveX, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, NegativeX, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, PositiveY, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, NegativeY, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, PositiveZ, 0, Mipmaps);
    glTextureCubeMapSide(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, NegativeZ, 0, Mipmaps);
  end;
end;

{$else}

procedure glTextureCubeMap(
  PositiveX, NegativeX,
  PositiveY, NegativeY,
  PositiveZ, NegativeZ: TEncodedImage;
  DDSForMipmaps: TDDSImage;
  Mipmaps: boolean);
begin
  // TODO-es Load cubemaps on OpenGL ES
end;

{$endif}


{ 3D texture loading --------------------------------------------------------- }

procedure glTextureImage3D(const Image: TEncodedImage;
  Filter: TTextureFilter; DDSForMipmaps: TDDSImage);

{$ifndef OpenGLES}
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
        case GLFeatures.Texture3D of
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
        [ GLFeatures.MaxTexture3DSize, Image.Width, Image.Height, Image.Depth ]);

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

    if Result and (not GLFeatures.Version_1_2) then
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
  if not (Image is TCastleImage) then
    raise ETextureLoadError.CreateFmt('Image class %s cannot be loaded to OpenGL 3D texture. OpenGL doesn''t allow any 3D texture compression formats',
      [Image.ClassName]);

  ImageInternalFormat := ImageGLInternalFormat(Image);
  ImageFormat := ImageGLFormat(TCastleImage(Image));

  glTexImage3DImage(TCastleImage(Image), 0);

  if Filter.NeedsMipmaps then
  begin
    if not LoadMipmapsFromDDS(DDSForMipmaps) then
    try
      GenerateMipmap(GL_TEXTURE_3D);
    except
      on E: EGenerateMipmapNotAvailable do
      begin
        Filter.Minification := minLinear;
        OnWarning(wtMinor, 'Texture', 'Creating mipmaps for 3D textures requires GenerateMipmap functionality, will fallback to GL_LINEAR minification: ' + E.Message);
      end;
    end;
  end;

  SetTextureFilter(GL_TEXTURE_3D, Filter);
end;

{$else}

begin
  // TODO-es Loading 3D textures on OpenGL ES -- use GL_OES_texture_3D extension
end;

{$endif}

{ GenerateMipmap ------------------------------------------------------------- }

{ $define TEST_NO_GENERATE_MIPMAP}

function HasGenerateMipmap: boolean;
{$ifdef TEST_NO_GENERATE_MIPMAP}
begin
  Result := false;
{$else}
begin
  Result := (GLFeatures.Framebuffer <> gsNone) and (not GLVersion.BuggyGenerateMipmap);
{$endif}
end;

procedure GenerateMipmap(target: TGLenum);
begin
  {$ifndef TEST_NO_GENERATE_MIPMAP}
  if GLFeatures.Framebuffer <> gsNone then
  begin
    {$ifndef OpenGLES}
    glPushAttrib(GL_ENABLE_BIT);
      { To work under fglrx (confirmed on chantal (ATI Mobility Radeon X1600)),
        we have to temporarily enable target.
        At least with EXT_framebuffer_object.
        This is a known ATI drivers problem:
        http://www.opengl.org/discussion_boards/ubbthreads.php?ubb=showflat&Number=237052 }
      glEnable(Target);
      case GLFeatures.Framebuffer of
        gsExtension: glGenerateMipmapEXT(Target);
        gsStandard : glGenerateMipmap   (Target);
      end;
    glPopAttrib;
    {$else}
    glGenerateMipmap(Target);
    {$endif}
  end else
  {$endif}
    raise EGenerateMipmapNotAvailable.Create('Framebuffer not supported, glGenerateMipmap[EXT] not available');
end;

{ Anisotropy ----------------------------------------------------------------- }

procedure TexParameterMaxAnisotropy(const target: TGLenum; const Anisotropy: TGLfloat);
begin
  if GLFeatures.EXT_texture_filter_anisotropic then
    glTexParameterf(Target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      Min(GLFeatures.MaxTextureMaxAnisotropyEXT, Anisotropy));
end;

{ DecompressS3TC ------------------------------------------------------------- }

function GLDecompressS3TC(Image: TS3TCImage): TCastleImage;

{$ifndef OpenGLES}
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
  if GLVersion.Fglrx and ( (Image.Width < 4) or (Image.Height < 4) ) then
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

{$else}
begin
  raise ECannotDecompressS3TC.Create('Cannot decompress S3TC texture on OpenGL ES');
  Result := nil; // get rid of warning
end;
{$endif}

procedure SetReadBuffer(const Buffer: TGLEnum);
begin
  {$ifndef OpenGLES}
  glReadBuffer(Buffer);
  {$endif}
end;

procedure SetDrawBuffer(const Buffer: TGLEnum);
begin
  {$ifndef OpenGLES}
  glDrawBuffer(Buffer);
  {$endif}
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

  case GLFeatures.Framebuffer of
    {$ifndef OpenGLES}
    gsExtension: glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, Fbo);
    {$endif}
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
  Without this, if you would blindly try SetDrawBuffer(GL_BACK)
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

  case GLFeatures.Framebuffer of
    {$ifndef OpenGLES}
    gsExtension: glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, PreviousFbo);
    {$endif}
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
  case GLFeatures.Framebuffer of
    {$ifndef OpenGLES}
    gsExtension: glFramebufferTexture2DEXT(Target, Attachment, TexTarget, Texture, Level);
    {$endif}
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
      case GLFeatures.Framebuffer of
        {$ifndef OpenGLES}
        gsExtension: glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, TextureTarget, Texture, 0);
        {$endif}
        gsStandard : glFramebufferTexture2D   (GL_FRAMEBUFFER    , GL_COLOR_ATTACHMENT0    , TextureTarget, Texture, 0);
      end;
      if not FramebufferBound then
        UnbindFramebuffer;
    end;
  end;
end;

procedure TGLRenderToTexture.GLContextOpen;

  function FramebufferStatusToString(const Status: TGLenum): string;
  {$ifndef OpenGLES}
  const
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT;
  {$endif}
  begin
    { some of these messages based on spec wording
      http://oss.sgi.com/projects/ogl-sample/registry/EXT/framebuffer_object.txt ,
      http://www.opengl.org/registry/specs/ARB/framebuffer_object.txt }
    case Status of
      GL_FRAMEBUFFER_COMPLETE                          : Result := 'Complete (no error)';
      GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT             : Result := 'INCOMPLETE_ATTACHMENT: Not all framebuffer attachment points are "framebuffer attachment complete"';
      GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     : Result := 'INCOMPLETE_MISSING_ATTACHMENT: None image attached to the framebuffer. On some GPUs/drivers (fglrx) it may also mean that desired image size is too large';
      GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS             : Result := 'INCOMPLETE_DIMENSIONS: Not all attached images have the same width and height';
      {$ifndef OpenGLES}
      GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT            : Result := 'INCOMPLETE_FORMATS: Not all images attached to the attachment points COLOR_ATTACHMENT* have the same internal format';
      GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            : Result := 'INCOMPLETE_DRAW_BUFFER: The value of FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is NONE for some color attachment point(s) named by DRAW_BUFFERi';
      GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER            : Result := 'INCOMPLETE_READ_BUFFER: READ_BUFFER is not NONE, and the value of FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE is NONE for the color attachment point named by READ_BUFFER';
      GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            : Result := 'INCOMPLETE_MULTISAMPLE: The value of RENDERBUFFER_SAMPLES is not the same for all attached images.';
      {$endif}
      GL_FRAMEBUFFER_UNSUPPORTED                       : Result := 'UNSUPPORTED: The combination of internal formats of the attached images violates an implementation-dependent set of restrictions';
      0: Result := 'OpenGL error during CheckFramebufferStatus';
      else Result := 'Unknown FramebufferStatus error: ' + GLErrorString(Status);
    end;
  end;

  procedure GenBindRenderbuffer(var RenderbufferId: TGLuint;
    const InternalFormat: TGLenum; const Attachment: TGLenum);
  begin
    case GLFeatures.Framebuffer of
      {$ifndef OpenGLES}
      gsExtension:
        begin
          glGenRenderbuffersEXT(1, @RenderbufferId);
          glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, RenderbufferId);
          glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, InternalFormat, Width, Height);
          glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, Attachment, GL_RENDERBUFFER_EXT, RenderbufferId);
        end;
      {$endif}
      gsStandard:
        begin
          glGenRenderbuffers(1, @RenderbufferId);
          glBindRenderbuffer   (GL_RENDERBUFFER    , RenderbufferId);
          {$ifndef OpenGLES}
          if (MultiSampling > 1) and GLFeatures.FBOMultiSampling then
            glRenderbufferStorageMultisample(GL_RENDERBUFFER, MultiSampling, InternalFormat, Width, Height) else
          {$endif}
            glRenderbufferStorage           (GL_RENDERBUFFER,                InternalFormat, Width, Height);
          glFramebufferRenderbuffer   (GL_FRAMEBUFFER    , Attachment, GL_RENDERBUFFER    , RenderbufferId);
        end;
    end;
  end;

  function ColorBufferFormat: TGLenum;
  begin
    if ColorBufferAlpha then
      Result := GL_RGBA else
      Result := GL_RGB;
  end;

var
  Status: TGLenum;
  DepthBufferFormatPacked, DepthAttachmentPacked: TGLenum;
  Success: boolean;
  PreviousFboDefaultBuffer: TGLenum;
begin
  Assert(not FGLInitialized, 'You cannot call TGLRenderToTexture.GLContextInit on already OpenGL-initialized instance. Call GLContextClose first if this is really what you want.');

  if (GLFeatures.Framebuffer <> gsNone) and
     (not (GLVersion.BuggyFBOCubeMap and
           Between(TextureTarget, GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z))) then
  begin
    if (Width > GLFeatures.MaxRenderbufferSize) or
       (Height > GLFeatures.MaxRenderbufferSize) then
      raise EFramebufferSizeTooLow.CreateFmt('Maximum renderbuffer (within framebuffer) size is %d x %d in your OpenGL implementation, while we require %d x %d',
        [ GLFeatures.MaxRenderbufferSize, GLFeatures.MaxRenderbufferSize, Width, Height ]);

    case GLFeatures.Framebuffer of
      {$ifndef OpenGLES}
      gsExtension: glGenFramebuffersEXT(1, @Framebuffer);
      {$endif}
      gsStandard : glGenFramebuffers   (1, @Framebuffer);
    end;
    BindFramebuffer(Framebuffer);

    { When GLPackedDepthStencil, and stencil is wanted
      (a very common case!, as most GPUs have EXT_packed_depth_stencil
      and for shadow volumes we want stencil) we desperately want to
      use one renderbuffer or one texture with combined depth/stencil info.
      Other possibilities may be not available at all (e.g. Radeon on chantal,
      but probably most GPUs with EXT_packed_depth_stencil). }
    {$ifndef OpenGLES}
    if Stencil and GLFeatures.PackedDepthStencil then
    begin
      DepthBufferFormatPacked := GL_DEPTH_STENCIL;
      DepthAttachmentPacked := GL_DEPTH_STENCIL_ATTACHMENT;
    end else
    // TODO-es This is probably needed on gles too?
    // we have GL_DEPTH_STENCIL_OES, but what is the equivalent of GL_DEPTH_STENCIL_ATTACHMENT?
    {$endif}
    begin
      DepthBufferFormatPacked := GL_DEPTH_COMPONENT;
      DepthAttachmentPacked := GL_DEPTH_ATTACHMENT;
    end;

    case Buffer of
      tbColor:
        begin
          FramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, TextureTarget, Texture, 0);
          GenBindRenderbuffer(RenderbufferDepth, DepthBufferFormatPacked, DepthAttachmentPacked);
        end;
      tbDepth:
        begin
          { Needed to consider FBO "complete" }
          SetDrawBuffer(GL_NONE);
          SetReadBuffer(GL_NONE);

          FramebufferTexture2D(GL_FRAMEBUFFER, DepthAttachmentPacked, TextureTarget, Texture, 0);
        end;
      tbColorAndDepth:
        begin
          FramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, TextureTarget, Texture, 0);
          FramebufferTexture2D(GL_FRAMEBUFFER, DepthAttachmentPacked, DepthTextureTarget, DepthTexture, 0);
        end;
      tbNone:
        begin
          GenBindRenderbuffer(RenderbufferColor, ColorBufferFormat, GL_COLOR_ATTACHMENT0);
          GenBindRenderbuffer(RenderbufferDepth, DepthBufferFormatPacked, DepthAttachmentPacked);
        end;
      else raise EInternalError.Create('Buffer 1?');
    end;

    { setup separate stencil buffer }
    if Stencil and not GLFeatures.PackedDepthStencil then
      { initialize RenderbufferStencil, attach it to FBO stencil }
      GenBindRenderbuffer(RenderbufferStencil, GL_STENCIL_INDEX, GL_STENCIL_ATTACHMENT);

    Success := false;
    try
      case GLFeatures.Framebuffer of
        {$ifndef OpenGLES}
        gsExtension: Status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
        {$endif}
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
      case GLFeatures.Framebuffer of
        {$ifndef OpenGLES}
        gsExtension: glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
        {$endif}
        gsStandard : glBindRenderbuffer   (GL_RENDERBUFFER    , 0);
      end;
      UnbindFramebuffer(PreviousFboDefaultBuffer);

      if Buffer = tbDepth then
      begin
        SetDrawBuffer(PreviousFboDefaultBuffer);
        SetReadBuffer(PreviousFboDefaultBuffer);
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
      case GLFeatures.Framebuffer of
        {$ifndef OpenGLES}
        gsExtension: glDeleteRenderbuffersEXT(1, @Buf);
        {$endif}
        gsStandard : glDeleteRenderbuffers   (1, @Buf);
      end;
      Buf := 0;
    end;
  end;

  procedure FreeFramebuffer(var Buf: TGLuint);
  begin
    if Buf <> 0 then
    begin
      case GLFeatures.Framebuffer of
        {$ifndef OpenGLES}
        gsExtension: glDeleteFramebuffersEXT(1, @Buf);
        {$endif}
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
        SetDrawBuffer(GL_NONE);
        SetReadBuffer(GL_NONE);
      end;
    end;
    Assert(FramebufferBound);
  end;
end;

{ A debug trick, saves color or depth buffer of the generated framebuffer image
  to a URL (file:///tmp/framebuffer_color/depth.png, change the code below
  if you want other URL). Useful e.g. to visualize captured shadow maps. }
{ $define DEBUG_SAVE_FRAMEBUFFER_DEPTH}
{ $define DEBUG_SAVE_FRAMEBUFFER_COLOR}

procedure TGLRenderToTexture.RenderEnd(const RenderBeginFollows: boolean);

{$ifdef DEBUG_SAVE_FRAMEBUFFER_COLOR}
  procedure SaveColor(const URL: string);
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

      SaveImage(Image, URL);
    finally FreeAndNil(Image) end;
  end;
{$endif DEBUG_SAVE_FRAMEBUFFER_COLOR}

{$ifdef DEBUG_SAVE_FRAMEBUFFER_DEPTH}
  procedure SaveDepth(const URL: string);
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

      SaveImage(Image, URL);
    finally FreeAndNil(Image) end;
  end;
{$endif DEBUG_SAVE_FRAMEBUFFER_DEPTH}

var
  PreviousFboDefaultBuffer: TGLenum;
begin
{$ifdef DEBUG_SAVE_FRAMEBUFFER_COLOR}
  if Buffer <> tbDepth then
    SaveColor('file:///tmp/framebuffer_color.png');
{$endif DEBUG_SAVE_FRAMEBUFFER_COLOR}
{$ifdef DEBUG_SAVE_FRAMEBUFFER_DEPTH}
  SaveDepth('file:///tmp/framebuffer_depth.png');
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
        SetDrawBuffer(PreviousFboDefaultBuffer);
        SetReadBuffer(PreviousFboDefaultBuffer);
      end;
    end;
  end else
  if Buffer <> tbNone then
  begin
    { Actually update OpenGL texture }
    glBindTexture(CompleteTextureTarget, Texture);
    SetReadBuffer(GL_BACK);
    glCopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, Width, Height);

    if Buffer = tbColorAndDepth then
    begin
      glBindTexture(DepthTextureTarget, DepthTexture);
      SetReadBuffer(GL_BACK);
      glCopyTexSubImage2D(DepthTextureTarget, 0, 0, 0, 0, 0, Width, Height);
    end;
  end;
end;

procedure TGLRenderToTexture.GenerateMipmap;
begin
  glBindTexture(CompleteTextureTarget, Texture);
  CastleGLImages.GenerateMipmap(CompleteTextureTarget);
end;

function TGLRenderToTexture.ColorBuffer: TColorBuffer;
begin
  if Framebuffer <> 0 then
    Result := cbColorAttachment0 else
    Result := cbBack;
end;

procedure WindowClose(const Container: IUIContainer);
begin
  glFreeBuffer(TGLImage.PointVbo);
  {$ifdef GLImageUseShaders}
  FreeAndNil(TGLImage.GLSLProgram[false]);
  FreeAndNil(TGLImage.GLSLProgram[true]);
  {$endif}
end;

initialization
  OnGLContextClose.Add(@WindowClose);
finalization
  FreeAndNil(BoundFboStack);
end.
