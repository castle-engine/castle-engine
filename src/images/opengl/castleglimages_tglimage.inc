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

{ Part of CastleGLImages unit: drawing 2D images on screen (TGLImage class). }

{$ifdef read_interface}

type
  { Image ready to be drawn on 2D screen.

    @bold(An instance of this class can only exist when the OpenGL context is open.)
    So usually you want to create it in @link(TUIControl.GLContextOpen)
    and destroy in @link(TUIControl.GLContextClose).
    Or create in @link(TCastleWindowCustom.OnOpen) and destroy
    in @link(TCastleWindowCustom.OnClose).

    If you want more comfort, use @link(TGLImageManaged) or even a full-featured
    2D control @link(TCastleImageControl). }
  TGLImage = class
  strict private
    const
      PointCount = 4;
    type
      TPoint = packed record
        Position: TVector2SmallInt;
        TexCoord: TVector2Single;
      end;
      {$ifdef GLImageUseShaders}
      TColorTreatment = (
        { Use texture color to draw. }
        ctTexture,
        { Use constant color * texture (on all RGBA channels). }
        ctColorMultipliesTexture,
        { Use constant color for RGB,
          for alpha use constant color.a * texture.
          Useful when texture has no alpha information. }
        ctColorMultipliesTextureAlpha);
      {$endif}
    var
    { Static OpenGL resources, used by all TGLImage instances. }
    PointVbo: TGLuint; static;
    { Point VBO contents, reused in every Draw. }
    Point: array [0..PointCount - 1] of TPoint; static;
    {$ifdef GLImageUseShaders}
    TextureHasOnlyAlpha: boolean;
    GLSLProgram: array [boolean { alpha test? }, TColorTreatment] of TGLSLProgram; static;
    {$endif}

    FWidth: Cardinal;
    FHeight: Cardinal;
    FAlpha: TAlphaChannel;
    FIgnoreTooLargeCorners: boolean;
    FColor: TCastleColor;
    FSmoothScaling: boolean;
    FScaleCorners: Single;
    FCenterX, FCenterY, FRotation: Single;
    ScreenCenterX, ScreenCenterY: Single;
    UseScreenCenter: boolean;
    procedure AlphaBegin;
    procedure AlphaEnd;
    procedure SetSmoothScaling(const Value: boolean);
    function CurrentFilter: TTextureFilter;
  private
    StaticResourcesReady: boolean; static;
    Texture: TGLTextureId;
    { Create / destroy static stuff for rendering. }
    class procedure StaticGLContextOpen;
    class procedure StaticGLContextClose;
    procedure GLContextOpen(const AImage: TEncodedImage);
    procedure GLContextClose;
  strict protected
    function GetWidth: Cardinal; virtual;
    function GetHeight: Cardinal; virtual;
  public
    { Prepare image for drawing.

      @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
        by OpenGL.) }
    constructor Create(const Image: TEncodedImage;
      const ASmoothScaling: boolean);
    constructor Create(const Image: TEncodedImage); deprecated 'use Create(Image, ASmoothScaling) and specify ASmoothScaling explicitly';

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
      const ASmoothScaling: boolean = true);
    constructor Create(const URL: string;
      const LoadAsClass: array of TEncodedImageClass;
      const ResizeToX: Cardinal = 0;
      const ResizeToY: Cardinal = 0;
      const Interpolation: TResizeInterpolation = riNearest);
    constructor Create(const URL: string;
      const LoadAsClass: array of TEncodedImageClass;
      const ASmoothScaling: boolean);

    destructor Destroy; override;

    property Width: Cardinal read GetWidth;
    property Height: Cardinal read GetHeight;

    { Rectangle representing the inside of this image.
      Always (Left,Bottom) are zero, and (Width,Height) correspond to image
      sizes. }
    function Rect: TRectangle;

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

      You should only use this inside TUIControl.Render when TUIControl.RenderStyle
      returns rs2D. This means that we require that current projection is 2D
      and lighting / depth test and such are off.

      The image is drawn in 2D. In normal circumstances
      1 pixel of the image is just placed over 1 pixel of the screen,
      and we draw the whole image. You can also use the overloaded
      version with 8 parameters where you explicitly specify the
      DrawWidth and DrawHeight of the rectangle on the screen, and explicitly choose
      the portion of the image to draw. If you want to draw scaled image
      (that is, use ImageWidth different than DrawWidth or ImageHeight
      different than DrawHeight) consider constructing an image with
      SmoothScaling = @true (otherwise the scaling will look pixelated).

      Note that the image position (ImageX, ImageY) is specified
      like a texture coordinate. So (0, 0) is actually
      the left-bottom corner of the left-bottom pixel,
      and (Width,Height) is the right-top corner of the right-top pixel.
      That is why image position and sizes are floats, it makes sense
      to render partial pixels this way (make sure you have
      SmoothScaling = @true to get nice scaling of image contents).
      You can also flip the image horizontally or vertically,
      e.g. use ImageX = Width and ImageWidth = -Width to mirror
      image horizontally.

      @groupBegin }
    procedure Draw; overload;
    procedure Draw(const X, Y: Single); overload;
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Single); overload;
    procedure Draw(const Pos: TVector2Integer); overload;
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Integer;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single); overload; virtual;
    procedure Draw(const ScreenRectangle: TRectangle); overload;
    procedure Draw(const ScreenRectangle: TRectangle;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single); overload;
    procedure Draw(const ScreenRectangle: TRectangle;
      const ImageRect: TRectangle); overload;
    { @groupEnd }

    procedure DrawFlipped(const ScreenRectangle: TRectangle;
      const FlipHorizontal, FlipVertical: boolean);

    { In case of @link(Draw3x3) or @link(Draw3x1), the corners
      on screen are scaled by this amount. This is especially useful
      for UI scaling, see @link(TUIContainer.UIScaling)
      and @link(TUIControl.UIScale). }
    property ScaleCorners: Single
      read FScaleCorners write FScaleCorners default 1;

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

    { Draw the image on the screen, divided into 3x1 parts:
      unscaled left and right sides, and scaled inside.

      Similar to @link(Draw3x3), but image is divided into 3 parts, not 9. }
    procedure Draw3x1(const X, Y, DrawWidth, DrawHeight: Integer;
      const SideRight, SideLeft: Integer);
    procedure Draw3x1(const X, Y, DrawWidth, DrawHeight: Integer;
      const Side: TVector2Integer);
    procedure Draw3x1(const ScreenRectangle: TRectangle;
      const Side: TVector2Integer);

    { Ignore (do not log to CastleLog) situations when Draw3x3 or Draw3x1
      cannot work
      because corners are larger than draw area size.
      Set this to @true when it's perfectly possible (you do not want to even
      see it in log) for Draw3x3 and Draw3x1 calls to fail because corners
      are larger than draw area size. }
    property IgnoreTooLargeCorners: boolean
      read FIgnoreTooLargeCorners write FIgnoreTooLargeCorners default false;

    { Color to multiply the texture contents (all RGBA channels).
      By default this is White, which is (1, 1, 1, 1) as RGBA,
      and it means that texture contents are not actually modified.
      This case is also optimized when possible, to no multiplication will
      actually happen.

      Note that the alpha of this color does not determine our alpha rendering
      mode (it cannot, as you can change this color at any point,
      and after creation the @link(Alpha) is never automatically updated).
      If you set a color with alpha <> 1, consider setting also @link(Alpha)
      property to whatever you need.

      Note that if use TGrayscaleImage with TGrayscaleImage.TreatAsAlpha
      (which means that texture does not contain any RGB information),
      then only this color's RGB values determine the drawn RGB color. }
    property Color: TCastleColor read FColor write FColor;

    { Load the given image contents.
      Use this to efficiently replace the TGLImage contents on GPU.
      Updates the @link(Width), @link(Height), @link(Alpha) to correspond
      to new image. }
    procedure Load(const AImage: TEncodedImage); virtual;

    { Prepare all the possible resources.
      For descendants that load some stuf on demand
      (@link(TGLImageOnDemand) and @link(TGLImageManaged))
      this method can be used to force loading it now (if possible).
      This is only useful if you want to avoid slowdown when loading it later,
      and prefer to do the loading now. }
    procedure PrepareResources; virtual;

    { Is the image scaling mode smooth (bilinear filtering)
      or not (nearest-pixel filtering).
      This is important if you draw the image
      such that one image pixel does not perfectly fit one screen pixel.

      If you will not ever do any scaling, then set
      this to @false. It may cause minimally faster drawing,
      and avoids any possible artifacts from bilinear filtering.

      If you will do scaling, you usually want to set this to @true,
      unless you deliberately want a pixelated look (for pixelart).

      Note that switching this property after this object is constructed
      is possible, but costly. }
    property SmoothScaling: boolean read FSmoothScaling write SetSmoothScaling;

    property ScalingPossible: boolean read FSmoothScaling write SetSmoothScaling;
      deprecated 'use SmoothScaling';

    { X coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterX: Single read FCenterX write FCenterX default 0.5;

    { Y coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterY: Single read FCenterY write FCenterY default 0.5;

    { Rotation in radians. Increase to rotate in a counter-clockwise direction.
      Hint: use function like ArcTan2 from Math unit to convert 2D direction
      into a rotation angle. }
    property Rotation: Single
      read FRotation write FRotation default 0;
  end;

  { Image ready to be drawn on 2D screen, with automatically
    managed OpenGL resources.

    This is a descendant of TGLImage that is easier to use:
    it can be created and destroyed at any moment (no need to worry about
    whether OpenGL context is created), and an instance can survive
    OpenGL context close + open.

    The catch is that this keeps the image pixels in normal memory all the time. }
  TGLImageManaged = class(TGLImage)
  strict private
    FImage: TEncodedImage;
    FOwnsImage: boolean;
    procedure SetImage(const AImage: TEncodedImage);
    procedure GLContextCloseEvent(Sender: TObject);
  public
    { Prepare image for drawing. The @code(Image) instance passed here must
      exist throughout the lifetime of this instance. It can be automatically freed
      if OwnsImage = @true.

      @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
        by OpenGL.) }
    constructor Create(const AImage: TEncodedImage; const ASmoothScaling: boolean;
      const AOwnsImage: boolean);

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
      const ASmoothScaling: boolean = true);
    constructor Create(const URL: string;
      const LoadAsClass: array of TEncodedImageClass;
      const ResizeToX: Cardinal = 0;
      const ResizeToY: Cardinal = 0;
      const Interpolation: TResizeInterpolation = riNearest);
    constructor Create(const URL: string;
      const LoadAsClass: array of TEncodedImageClass;
      const ASmoothScaling: boolean);

    destructor Destroy; override;

    property Image: TEncodedImage read FImage write SetImage;
    property OwnsImage: boolean read FOwnsImage write FOwnsImage;

    procedure Load(const AImage: TEncodedImage); override;
    procedure PrepareResources; override;
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Integer;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single); override;
  end;

  { Image ready to be drawn on 2D screen, loaded on demand.

    The image is loaded from disk on demand (only once needed),
    and the OpenGL resources are also initilized on demand.
    This is very efficient for memory usage,
    but the catch is that even the image sizes are not known before the image
    will be loaded (which will happen at nearest draw, or when you
    explicitly call @link(PrepareResources)).

    Like TGLImageManaged, this image instance
    can be created and destroyed at any moment (no need to worry about
    whether OpenGL context is created), and an instance can survive
    OpenGL context close + open. }
  TGLImageOnDemand = class(TGLImage)
  strict private
    FURL: string;
    procedure GLContextCloseEvent(Sender: TObject);
  strict protected
    function GetWidth: Cardinal; override;
    function GetHeight: Cardinal; override;
  public
    constructor Create(const AURL: string;
      const ASmoothScaling: boolean = true);
    destructor Destroy; override;
    procedure PrepareResources; override;
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Integer;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single); override;
  end;

{ Draw the image on 2D screen. Note that if you want to use this
  many times, it will be much faster to create TGLImage instance.

  @deprecated Deprecated, always use TGLImage to draw 2D images.

  @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
    by OpenGL.) }
procedure ImageDraw(const Image: TCastleImage); deprecated;

{$endif read_interface}

{$ifdef read_implementation}

{ TGLImage ------------------------------------------------------------------- }

constructor TGLImage.Create(const Image: TEncodedImage;
  const ASmoothScaling: boolean);
begin
  inherited Create;

  // TODO: use texture cache here, like GL renderer does for textures for 3D.
  // no need to create new OpenGL texture for the same image.

  FColor := White;
  FScaleCorners := 1;
  FCenterX := 0.5;
  FCenterY := 0.5;
  FRotation := 0;
  { this sets CurrentFilter, that will be immediately used by Load }
  FSmoothScaling := ASmoothScaling;

  Load(Image);
end;

constructor TGLImage.Create(const Image: TEncodedImage);
begin
  Create(Image, false);
end;

constructor TGLImage.Create(const URL: string;
  const ASmoothScaling: boolean);
begin
  Create(URL, [], ASmoothScaling);
end;

constructor TGLImage.Create(const URL: string;
  const LoadAsClass: array of TEncodedImageClass;
  const ResizeToX, ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation);
var
  Image: TCastleImage;
begin
  if High(LoadAsClass) = -1 then
    Image := LoadImage(URL, PixelsImageClasses, ResizeToX, ResizeToY, Interpolation) else
    Image := LoadImage(URL, LoadAsClass, ResizeToX, ResizeToY, Interpolation);
  try
    Create(Image, false);
  finally FreeAndNil(Image) end;
end;

constructor TGLImage.Create(const URL: string;
  const LoadAsClass: array of TEncodedImageClass;
  const ASmoothScaling: boolean);
var
  Image: TEncodedImage;
begin
  if High(LoadAsClass) = -1 then
    Image := LoadEncodedImage(URL, PixelsImageClasses) else
    Image := LoadEncodedImage(URL, LoadAsClass);
  try
    Create(Image, ASmoothScaling);
  finally FreeAndNil(Image) end;
end;

destructor TGLImage.Destroy;
begin
  GLContextClose;
  inherited;
end;

procedure TGLImage.GLContextOpen(const AImage: TEncodedImage);
begin
  if AImage <> nil then
  begin
    if Texture = 0 then
      glGenTextures(1, @Texture);
    LoadGLGeneratedTexture(Texture, AImage, CurrentFilter,
      Texture2DClampToEdge, nil, true);
  end;
end;

procedure TGLImage.GLContextClose;
begin
  glFreeTexture(Texture);
end;

procedure TGLImage.Load(const AImage: TEncodedImage);
begin
  if AImage <> nil then
  begin
    FWidth := AImage.Width;
    FHeight := AImage.Height;
    FAlpha := AImage.AlphaChannel;
  end else
  begin
    FWidth := 0;
    FHeight := 0;
    FAlpha := acNone;
  end;

  {$ifdef GLImageUseShaders}
  { calculate TextureHasOnlyAlpha, useful only for GLSL image rendering }
  TextureHasOnlyAlpha :=
    (AImage <> nil) and
    (AImage is TGrayscaleImage) and
    (TGrayscaleImage(AImage).TreatAsAlpha);
  {$endif}

  { in case of TGLImage, this is the only way to call GLContextOpen.
    But in case of TGLImageManaged, GLContextOpen can be called
    from other moments too. }
  if ApplicationProperties.IsGLContextOpen then
  begin
    GLContextOpen(AImage);
    StaticGLContextOpen;
  end;
end;

class procedure TGLImage.StaticGLContextOpen;
{$ifdef GLImageUseShaders}
var
  AlphaTestShader: boolean;
  ColorTreatment: TColorTreatment;
  NewProgram: TGLSLProgram;
  VS, FS: string;
{$endif}
begin
  // Some Intel GPUs may not support VBO
  if (PointVbo = 0) and GLFeatures.VertexBufferObject then
    glGenBuffers(1, @PointVbo);

  {$ifdef GLImageUseShaders}
  { create GLSLProgram programs }
  for AlphaTestShader in boolean do
    for ColorTreatment in TColorTreatment do
      if GLSLProgram[AlphaTestShader, ColorTreatment] = nil then
      begin
        VS := {$I image.vs.inc};
        FS := Iff(AlphaTestShader, '#define ALPHA_TEST' + NL, '') +
              Iff(ColorTreatment in [ctColorMultipliesTexture, ctColorMultipliesTextureAlpha], '#define COLOR_UNIFORM' + NL, '') +
              Iff(ColorTreatment = ctColorMultipliesTextureAlpha, '#define TEXTURE_HAS_ONLY_ALPHA' + NL, '') +
              {$I image.fs.inc};
        if Log and LogShaders then
        begin
          WritelnLogMultiline('TGLImage GLSL vertex shader', VS);
          WritelnLogMultiline('TGLImage GLSL fragment shader', FS);
        end;

        NewProgram := TGLSLProgram.Create;
        NewProgram.AttachVertexShader(VS);
        NewProgram.AttachFragmentShader(FS);
        NewProgram.Link(true);
        GLSLProgram[AlphaTestShader, ColorTreatment] := NewProgram;
      end;
  {$endif}

  StaticResourcesReady := true;
end;

class procedure TGLImage.StaticGLContextClose;
{$ifdef GLImageUseShaders}
var
  AlphaTestShader: boolean;
  ColorTreatment: TGLImage.TColorTreatment;
{$endif}
begin
  glFreeBuffer(PointVbo);
  {$ifdef GLImageUseShaders}
  for AlphaTestShader in boolean do
    for ColorTreatment in TColorTreatment do
      FreeAndNil(GLSLProgram[AlphaTestShader, ColorTreatment]);
  {$endif}

  StaticResourcesReady := false;
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

procedure TGLImage.Draw(const X, Y: Single);
begin
  Draw(Round(X), Round(Y), Width, Height, 0, 0, Width, Height);
end;

procedure TGLImage.Draw(const X, Y, DrawWidth, DrawHeight: Single);
begin
  Draw(Round(X), Round(Y), Round(DrawWidth), Round(DrawHeight), 0, 0, Width, Height);
end;

procedure TGLImage.Draw(const Pos: TVector2Integer);
begin
  Draw(Pos[0], Pos[1], Width, Height, 0, 0, Width, Height);
end;

procedure TGLImage.Draw(const X, Y, DrawWidth, DrawHeight: Integer;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single);
var
  ShiftX, ShiftY: Single;

  { Rotate Point by Rotation radians around (ShiftX, ShiftY). }
  function RotatePoint2DAroundShift(const Point: TVector2SmallInt): TVector2SmallInt;
  var
    PointSingle, ResultSingle: TVector2Single;
  begin
    PointSingle := Vector2Single(Point[0] - ShiftX, Point[1] - ShiftY);
    ResultSingle := RotatePoint2D(PointSingle, Rotation);
    Result := Vector2SmallInt(Round(ResultSingle[0] + ShiftX), Round(ResultSingle[1] + ShiftY));
  end;

var
  TexX0, TexY0, TexX1, TexY1: Single;
  {$ifdef GLImageUseShaders}
  ColorTreatment: TColorTreatment;
  AttribEnabled: array [0..1] of TGLuint;
  AttribLocation: TGLuint;
  Prog: TGLSLProgram;
  {$endif}
begin
  if (DrawWidth = 0) or (DrawHeight = 0) then Exit;

  if Texture = 0 then
    raise Exception.Create('TGLImage resources are not ready when trying to render. This probably means that you created TGLImage when OpenGL context was not created yet. The simplest solution is to use TGLImageManaged instead');
  if not StaticResourcesReady then
    raise Exception.Create('TGLImage static resources are not ready when trying to render. This probably means that OpenGL context was freed, but TGLImage was not recreated');

  if GLFeatures.UseMultiTexturing then glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Texture);
  GLEnableTexture(et2D);

  TexX0 := ImageX / Width;
  TexY0 := ImageY / Height;
  TexX1 := (ImageX + ImageWidth ) / Width;
  TexY1 := (ImageY + ImageHeight) / Height;

  { Protect from X, Y and sizes going outside of the SmallInt range.
    We want to squeeze X, Y into SmallInt to make it a little more efficient
    to render, and there are no displays with resolution outside of SmallInt
    range anyway. }
  if (X < Low(SmallInt)) or
     (Y < Low(SmallInt)) or
     (X + DrawWidth  > High(SmallInt)) or
     (X + DrawHeight > High(SmallInt)) then
    Exit;

  Point[0].TexCoord := Vector2Single(TexX0, TexY0);
  Point[0].Position := Vector2SmallInt(X            , Y);
  Point[1].TexCoord := Vector2Single(TexX1, TexY0);
  Point[1].Position := Vector2SmallInt(X + DrawWidth, Y);
  Point[2].TexCoord := Vector2Single(TexX1, TexY1);
  Point[2].Position := Vector2SmallInt(X + DrawWidth, Y + DrawHeight);
  Point[3].TexCoord := Vector2Single(TexX0, TexY1);
  Point[3].Position := Vector2SmallInt(X            , Y + DrawHeight);

  if FRotation <> 0 then
  begin
    if UseScreenCenter then
    begin
      ShiftX := ScreenCenterX;
      ShiftY := ScreenCenterY;
    end else
    begin
      ShiftX := X + FCenterX * DrawWidth;
      ShiftY := Y + FCenterY * DrawHeight;
    end;
    Point[0].Position := RotatePoint2DAroundShift(Point[0].Position);
    Point[1].Position := RotatePoint2DAroundShift(Point[1].Position);
    Point[2].Position := RotatePoint2DAroundShift(Point[2].Position);
    Point[3].Position := RotatePoint2DAroundShift(Point[3].Position);
  end;

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, PointVbo);
    glBufferData(GL_ARRAY_BUFFER, PointCount * SizeOf(TPoint),
      @Point[0], GL_STREAM_DRAW);
  end;

  AlphaBegin;

  {$ifdef GLImageUseShaders}
  if TextureHasOnlyAlpha then
    ColorTreatment := ctColorMultipliesTextureAlpha else
  if not VectorsPerfectlyEqual(Color, White) then
    ColorTreatment := ctColorMultipliesTexture else
    ColorTreatment := ctTexture;

  Prog := GLSLProgram[Alpha = acSimpleYesNo, ColorTreatment];
  Prog.Enable;
  AttribEnabled[0] := Prog.VertexAttribPointer('vertex', 0, 2, GL_SHORT, GL_FALSE,
    SizeOf(TPoint), Offset(Point[0].Position, Point[0]));
  AttribEnabled[1] := Prog.VertexAttribPointer('tex_coord', 0, 2, GL_FLOAT, GL_FALSE,
    SizeOf(TPoint), Offset(Point[0].TexCoord, Point[0]));
  Prog.SetUniform('viewport_size', Viewport2DSize);
  if ColorTreatment <> ctTexture then
    Prog.SetUniform('color', Color);

  {$else}
  glLoadIdentity();
  glColorv(Color);

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  if GLFeatures.VertexBufferObject then
  begin
    glVertexPointer(2, GL_SHORT,
      SizeOf(TPoint), Offset(Point[0].Position, Point[0]));
    glTexCoordPointer(2, GL_FLOAT,
      SizeOf(TPoint), Offset(Point[0].TexCoord, Point[0]));
  end else
  begin
    glVertexPointer  (2, GL_SHORT, SizeOf(TPoint), @(Point[0].Position));
    glTexCoordPointer(2, GL_FLOAT, SizeOf(TPoint), @(Point[0].TexCoord));
  end;
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

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;

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

procedure TGLImage.DrawFlipped(const ScreenRectangle: TRectangle;
  const FlipHorizontal, FlipVertical: boolean);
var
  ImageX, ImageY, ImageWidth, ImageHeight: Single;
begin
  if FlipHorizontal then
  begin
    ImageX := Width;
    ImageWidth := -Width;
  end else
  begin
    ImageX := 0;
    ImageWidth := Width;
  end;

  if FlipVertical then
  begin
    ImageY := Height;
    ImageHeight := -Height;
  end else
  begin
    ImageY := 0;
    ImageHeight := Height;
  end;

  Draw(ScreenRectangle, ImageX, ImageY, ImageWidth, ImageHeight);
end;

procedure TGLImage.Draw(const ScreenRectangle: TRectangle;
  const ImageRect: TRectangle);
begin
  Draw(ScreenRectangle.Left, ScreenRectangle.Bottom,
    ScreenRectangle.Width, ScreenRectangle.Height,
    ImageRect.Left, ImageRect.Bottom,
    ImageRect.Width, ImageRect.Height);
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
  EpsilonT, EpsilonR, EpsilonB, EpsilonL: Single;
  DrawCornerTop, DrawCornerRight, DrawCornerBottom, DrawCornerLeft: Integer;
const
  { We tweak texture coordinates a little, to avoid bilinear filtering
    that would cause border colors to "bleed" over the texture inside.
    Something minimally > 0.5 is necessary. }
  Epsilon = 0.51;
begin
  DrawCornerTop    := Round(CornerTop    * ScaleCorners);
  DrawCornerRight  := Round(CornerRight  * ScaleCorners);
  DrawCornerBottom := Round(CornerBottom * ScaleCorners);
  DrawCornerLeft   := Round(CornerLeft   * ScaleCorners);

  if not ( (    CornerLeft +     CornerRight <     Width) and
           (DrawCornerLeft + DrawCornerRight < DrawWidth) and
           (    CornerBottom +     CornerTop <     Height) and
           (DrawCornerBottom + DrawCornerTop < DrawHeight)) then
  begin
    if Log and not IgnoreTooLargeCorners then
      WritelnLog('Draw3x3', 'Image corners are too large to draw it: corners are %d %d %d %d, image size is %d %d, draw area size is %d %d',
        [CornerTop, CornerRight, CornerBottom, CornerLeft,
         Width, Height,
         DrawWidth, DrawHeight]);
    Exit;
  end;

  XScreenLeft := X;
  XImageLeft := 0;
  XScreenRight := X + DrawWidth - DrawCornerRight;
  XImageRight  :=         Width -     CornerRight;

  YScreenBottom := Y;
  YImageBottom := 0;
  YScreenTop := Y + DrawHeight - DrawCornerTop;
  YImageTop  :=         Height -     CornerTop;

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

  { setup rotations }
  if Rotation <> 0 then
  begin
    UseScreenCenter := true; // only temporary for this Draw3x3 call
    ScreenCenterX := X + FCenterX * DrawWidth;
    ScreenCenterY := Y + FCenterY * DrawHeight;
  end;

  { 4 corners }
  Draw(XScreenLeft, YScreenBottom, DrawCornerLeft, DrawCornerBottom,
        XImageLeft,  YImageBottom,     CornerLeft,     CornerBottom);
  Draw(XScreenRight, YScreenBottom, DrawCornerRight, DrawCornerBottom,
        XImageRight,  YImageBottom,     CornerRight,     CornerBottom);
  Draw(XScreenRight, YScreenTop, DrawCornerRight, DrawCornerTop,
        XImageRight,  YImageTop,     CornerRight,     CornerTop);
  Draw(XScreenLeft, YScreenTop, DrawCornerLeft, DrawCornerTop,
        XImageLeft,  YImageTop,     CornerLeft,     CornerTop);

  { 4 sides }
  HorizontalScreenSize := DrawWidth - DrawCornerLeft - DrawCornerRight;
  HorizontalImageSize  :=     Width -     CornerLeft -     CornerRight;
  VerticalScreenSize := DrawHeight - DrawCornerTop - DrawCornerBottom;
  VerticalImageSize  :=     Height -     CornerTop -     CornerBottom;

  Draw(XScreenLeft + DrawCornerLeft, YScreenBottom, HorizontalScreenSize, DrawCornerBottom,
        XImageLeft +     CornerLeft,  YImageBottom,  HorizontalImageSize,     CornerBottom);
  Draw(XScreenLeft + DrawCornerLeft, YScreenTop, HorizontalScreenSize, DrawCornerTop,
        XImageLeft +     CornerLeft,  YImageTop,  HorizontalImageSize,     CornerTop);

  Draw(XScreenLeft, YScreenBottom + DrawCornerBottom, DrawCornerLeft, VerticalScreenSize,
        XImageLeft,  YImageBottom +     CornerBottom,     CornerLeft,  VerticalImageSize);
  Draw(XScreenRight, YScreenBottom + DrawCornerBottom, DrawCornerRight, VerticalScreenSize,
        XImageRight,  YImageBottom +     CornerBottom,     CornerRight,  VerticalImageSize);

  { inside }
  if CornerLeft > 0   then EpsilonL := Epsilon else EpsilonL := 0;
  if CornerTop > 0    then EpsilonT := Epsilon else EpsilonT := 0;
  if CornerRight > 0  then EpsilonR := Epsilon else EpsilonR := 0;
  if CornerBottom > 0 then EpsilonB := Epsilon else EpsilonB := 0;

  Draw(X + DrawCornerLeft           , Y + DrawCornerBottom           , HorizontalScreenSize, VerticalScreenSize,
               CornerLeft + EpsilonL,         CornerBottom + EpsilonB, HorizontalImageSize - (EpsilonL+EpsilonR), VerticalImageSize - (EpsilonT+EpsilonB));

  {$ifdef GLImageUseShaders}
  if OptimizeAlpha then
  {$endif}
  begin
    Alpha := OldAlpha;
    AlphaEnd;
  end;
  UseScreenCenter := false;
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

procedure TGLImage.Draw3x1(const X, Y, DrawWidth, DrawHeight: Integer;
  const SideRight, SideLeft: Integer);
var
  XScreenLeft, XScreenRight,
    HorizontalScreenSize: Integer;
  XImageLeft, XImageRight,
    HorizontalImageSize: Single;
  OldAlpha: TAlphaChannel;
  {$ifdef GLImageUseShaders}
  OptimizeAlpha: boolean;
  {$endif}
  EpsilonR, EpsilonL: Single;
const
  { We tweak texture coordinates a little, to avoid bilinear filtering
    that would cause border colors to "bleed" over the texture inside.
    Something minimally > 0.5 is necessary. }
  Epsilon = 0.51;
begin
  if not ( (SideLeft + SideRight < Width) and
           (SideLeft + SideRight < DrawWidth) ) then
  begin
    if Log and not IgnoreTooLargeCorners then
      WritelnLog('Draw3x1', 'Image sides are too large to draw it: sides are %d %d, image width is %d, draw area width is %d',
        [SideRight, SideLeft,
         Width,
         DrawWidth]);
    Exit;
  end;

  XScreenLeft := X;
  XImageLeft := 0;
  XScreenRight := X + DrawWidth - SideRight;
  XImageRight := Width - SideRight;

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

  { setup rotations }
  if Rotation <> 0 then
  begin
    UseScreenCenter := true; // only temporary for this Draw3x3 call
    ScreenCenterX := X + FCenterX * DrawWidth;
    ScreenCenterY := Y + FCenterY * DrawHeight;
  end;

  { 2 sides }
  HorizontalScreenSize := DrawWidth - SideLeft - SideRight;
  HorizontalImageSize  :=     Width - SideLeft - SideRight;

  Draw(XScreenLeft, Y, SideLeft, DrawHeight,
        XImageLeft, 0, SideLeft, Height);
  Draw(XScreenRight, Y, SideRight, DrawHeight,
        XImageRight, 0, SideRight, Height);

  { inside }
  if SideLeft > 0   then EpsilonL := Epsilon else EpsilonL := 0;
  if SideRight > 0  then EpsilonR := Epsilon else EpsilonR := 0;

  Draw(X + SideLeft           , Y, HorizontalScreenSize, DrawHeight,
           SideLeft + EpsilonL, 0, HorizontalImageSize - (EpsilonL+EpsilonR), Height);

  {$ifdef GLImageUseShaders}
  if OptimizeAlpha then
  {$endif}
  begin
    Alpha := OldAlpha;
    AlphaEnd;
  end;
  UseScreenCenter := false;
end;

procedure TGLImage.Draw3x1(const X, Y, DrawWidth, DrawHeight: Integer;
  const Side: TVector2Integer);
begin
  Draw3x1(X, Y, DrawWidth, DrawHeight,
    Side[0], Side[1]);
end;

procedure TGLImage.Draw3x1(const ScreenRectangle: TRectangle;
  const Side: TVector2Integer);
begin
  Draw3x1(ScreenRectangle.Left, ScreenRectangle.Bottom,
    ScreenRectangle.Width, ScreenRectangle.Height,
    Side[0], Side[1]);
end;

function TGLImage.CurrentFilter: TTextureFilter;
begin
  if SmoothScaling then
  begin
    Result.Minification := minLinear;
    Result.Magnification := magLinear;
  end else
  begin
    Result.Minification := minNearest;
    Result.Magnification := magNearest;
  end;
end;

procedure TGLImage.SetSmoothScaling(const Value: boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    if Texture <> 0 then { if GL resources available now --- this is needed for TGLImageManaged }
    begin
      glBindTexture(GL_TEXTURE_2D, Texture);
      SetTextureFilter(GL_TEXTURE_2D, CurrentFilter);
    end;
  end;
end;

function TGLImage.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, Width, Height);
end;

procedure TGLImage.PrepareResources;
begin
end;

function TGLImage.GetWidth: Cardinal;
begin
  Result := FWidth;
end;

function TGLImage.GetHeight: Cardinal;
begin
  Result := FHeight;
end;

{ TGLImageManaged ------------------------------------------------------------ }

constructor TGLImageManaged.Create(const AImage: TEncodedImage;
  const ASmoothScaling, AOwnsImage: boolean);
begin
  inherited Create(AImage, ASmoothScaling);
  // note that above Create will call virtual Load() which will already set our FImage
  FOwnsImage := AOwnsImage;
  ApplicationProperties.OnGLContextCloseObject.Add(@GLContextCloseEvent);
end;

constructor TGLImageManaged.Create(const URL: string;
  const ASmoothScaling: boolean);
begin
  Create(URL, [], ASmoothScaling);
end;

constructor TGLImageManaged.Create(const URL: string;
  const LoadAsClass: array of TEncodedImageClass;
  const ResizeToX, ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation);
var
  NewImage: TCastleImage;
begin
  if High(LoadAsClass) = -1 then
    NewImage := LoadImage(URL, PixelsImageClasses, ResizeToX, ResizeToY, Interpolation) else
    NewImage := LoadImage(URL, LoadAsClass, ResizeToX, ResizeToY, Interpolation);
  Create(NewImage, { ASmoothScaling } false, { AOwnsImage } true);
end;

constructor TGLImageManaged.Create(const URL: string;
  const LoadAsClass: array of TEncodedImageClass;
  const ASmoothScaling: boolean);
var
  NewImage: TEncodedImage;
begin
  if High(LoadAsClass) = -1 then
    NewImage := LoadEncodedImage(URL, PixelsImageClasses) else
    NewImage := LoadEncodedImage(URL, LoadAsClass);
  Create(NewImage, ASmoothScaling, { AOwnsImage } true);
end;

destructor TGLImageManaged.Destroy;
begin
  if FOwnsImage then
    FreeAndNil(FImage) else
    FImage := nil;
  if ApplicationProperties <> nil then
    ApplicationProperties.OnGLContextCloseObject.Remove(@GLContextCloseEvent);
  inherited;
end;

procedure TGLImageManaged.Load(const AImage: TEncodedImage);
begin
  if FOwnsImage then
    FreeAndNil(FImage) else
    FImage := nil;
  inherited Load(AImage);
  FImage := AImage;
end;

procedure TGLImageManaged.SetImage(const AImage: TEncodedImage);
begin
  { exactly like Load(), but can ignore the call is AImage reference normal.
    Load() should not do it, to allow loading the same AImage reference
    in case AImage contents change. }
  if FImage <> AImage then
    Load(AImage);
end;

procedure TGLImageManaged.Draw(const X, Y, DrawWidth, DrawHeight: Integer;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single);
begin
  // we load on demand, so do PrepareResources always
  PrepareResources;
  // PrepareResources, done while GL context is active, must make it true
  Assert(Texture <> 0);
  inherited;
end;

procedure TGLImageManaged.PrepareResources;
begin
  inherited;

  if ApplicationProperties.IsGLContextOpen then
  begin
    { initialize OpenGL resources on-demand }
    if not StaticResourcesReady then
      StaticGLContextOpen;
    if Texture = 0 then
      GLContextOpen(FImage);
  end;
end;

procedure TGLImageManaged.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
end;

{ TGLImageOnDemand ------------------------------------------------------------ }

constructor TGLImageOnDemand.Create(const AURL: string;
  const ASmoothScaling: boolean);
begin
  inherited Create(nil, ASmoothScaling);
  FURL := AURL;
  ApplicationProperties.OnGLContextCloseObject.Add(@GLContextCloseEvent);
end;

destructor TGLImageOnDemand.Destroy;
begin
  if ApplicationProperties <> nil then
    ApplicationProperties.OnGLContextCloseObject.Remove(@GLContextCloseEvent);
  inherited;
end;

procedure TGLImageOnDemand.Draw(const X, Y, DrawWidth, DrawHeight: Integer;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single);
begin
  // we load on demand, so do PrepareResources always
  PrepareResources;
  // PrepareResources, done while GL context is active, must make it true
  Assert(Texture <> 0);
  inherited;
end;

procedure TGLImageOnDemand.PrepareResources;
var
  Image: TEncodedImage;
begin
  inherited;

  if ApplicationProperties.IsGLContextOpen then
  begin
    { initialize OpenGL resources on-demand }
    if not StaticResourcesReady then
      StaticGLContextOpen;

    if Texture = 0 then
    begin
      Image := LoadEncodedImage(FURL, PixelsImageClasses);
      try
        inherited Load(Image);
      finally FreeAndNil(Image) end;
    end;
  end;
end;

procedure TGLImageOnDemand.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
end;

function TGLImageOnDemand.GetWidth: Cardinal;
begin
  // load on demand, to know current size
  PrepareResources;
  Result := inherited GetWidth;
end;

function TGLImageOnDemand.GetHeight: Cardinal;
begin
  // load on demand, to know current size
  PrepareResources;
  Result := inherited GetHeight;
end;

{ global --------------------------------------------------------------------- }

procedure ImageDraw(const Image: TCastleImage);
var
  GLImage: TGLImage;
begin
  GLImage := TGLImage.Create(Image, false);
  try
    GLImage.Draw(0, 0);
  finally FreeAndNil(GLImage) end;
end;

{$endif read_implementation}
