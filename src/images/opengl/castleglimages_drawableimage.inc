{
  Copyright 2001-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: drawing 2D images on screen (TDrawableImage class). }

{$ifdef read_interface}

type
  TGLRenderToTexture = class;

  { Image that can be drawn.
    This image can be drawn in various rendering methods and events,
    e.g. inside overridden @link(TCastleUserInterface.Render) or from events
    @link(TCastleWindowBase.OnRender) or
    @link(TCastleControlBase.OnRender).

    See the manual about drawing using TDrawableImage:
    https://castle-engine.io/manual_quick_2d_game.php ,
    https://castle-engine.io/manual_2d_ui_custom_drawn.php .
    See the description how to render 2D stuff using Castle Game Engine:
    https://github.com/castle-engine/castle-engine/wiki/2D-Games .

    An alternative way to show a 2D image is to use the full-featured
    2D control @link(TCastleImageControl). This will perform the drawing
    by itself, you only add it to
    @link(TCastleWindowBase.Controls) or
    @link(TCastleControlBase.Controls). }
  TDrawableImage = class(TNoParameterlessContructor)
  strict private
    type
      TPoint = packed record
        Position: TVector2;
        TexCoord: TVector2;
      end;
      TPointArray = packed array [0..MaxInt div SizeOf(TPoint) - 1] of TPoint;
      PPointArray = ^TPointArray;

      { Used only in shader pipeline }
      TColorTreatment = (
        { Use texture color to draw. }
        ctPureTexture,
        { Use constant color * texture (on all RGBA channels). }
        ctColorMultipliesTexture,
        { Use constant color for RGB,
          for alpha use constant color.a * texture.
          Useful when texture has no alpha information. }
        ctColorMultipliesTextureAlpha);

      TImageProgram = record
        Prog: TGLSLProgram;
        UniformViewportSize, UniformColor, UniformClipLine: TGLSLUniform;
        AttribVertex, AttribTexCoord: TGLSLAttribute;
        procedure InitializeUniformsAttributes(
          const ClipLineShader: boolean;
          const ColorTreatment: TColorTreatment);
      end;

      TVboStorage = record
        PointsVbo, IndexesVbo: TGLuint;
        PointsVboSize, IndexesVboSize: Cardinal;
      end;
      PVboStorage = ^TVboStorage;

    var
      { Static OpenGL resources, used by all TDrawableImage instances.
        Instead of using the same TVboStorage for all TDrawableImage.Draw calls,
        we rotate between a number of TVboStorage, each with it's own VBOs.

        This way we minimize the overhead caused by updating VBO that is currently
        drawn in parallel:
        https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferSubData.xhtml
        :

        """
        Consider using multiple buffer objects to avoid stalling
        the rendering pipeline during data store updates.
        If any rendering in the pipeline makes reference to data in
        the buffer object being updated by glBufferSubData,
        especially from the specific region being updated,
        that rendering must drain from the pipeline before
        the data store can be updated.
        """

        This makes a huge speedup for drawing UI on Raspberry Pi OpenGLES
        without X.
      }
      VboStorage: array [0..9] of TVboStorage; static;
      VboStorageNext: Cardinal; static;
      { Point VBO contents, reused in every Draw. }
      Points: array of TPoint; static;
      Indexes: array of {$ifdef GLIndexesShort} Word {$else} LongWord {$endif}; static;

      { Used only in shader pipeline }
      Programs: array [boolean { alpha test? }, boolean { clip? }, TColorTreatment] of TImageProgram; static;

      TextureHasOnlyAlpha: boolean;
      FWidth: Cardinal;
      FHeight: Cardinal;
      FAlpha: TAutoAlphaChannel;
      FIgnoreTooLargeCorners: boolean;
      FColor: TCastleColor;
      FSmoothScaling: boolean;
      FScaleCorners: Single;
      FRotation: Single;
      FRotationCenter: TVector2;
      ScreenRotationCenter: TVector2;
      UseScreenRotationCenter: boolean;
      FClip: boolean;
      FClipLine: TVector3;
      { The detected alpha channel of the loaded image.
        It influences the drawing mode, if @link(Alpha) is acAuto.

        This property is always acNone before the image is loaded.
        You cannot change it, you can only change the @link(Alpha) property
        that actually dictates the alpha drawing mode. }
      FImageAlpha: TAlphaChannel;
      FBlendingSourceFactor: TBlendingSourceFactor;
      FBlendingDestinationFactor: TBlendingDestinationFactor;
      FBlendingConstantColor: TVector4;
      FCustomShader: TGLSLProgram;

      FTexture: TGLTextureId;
      FImage: TEncodedImage;
      FOwnsImage: boolean;
      FURL: String;
      FRepeatS, FRepeatT: Boolean;

      FRenderToImage: TGLRenderToTexture;
      HasViewportToRestore: boolean;
      ViewportToRestore: TRectangle;

    function FinalAlpha: TAlphaChannel;
    procedure SetSmoothScaling(const Value: boolean);
    function CurrentFilter: TTextureFilter;
    function CurrentWrap: TTextureWrap2D;
    procedure SetImage(const AImage: TEncodedImage);
    procedure GLContextCloseEvent(Sender: TObject);
    procedure GLContextOpen(const AImage: TEncodedImage);
    procedure GLContextClose;
    function GetCenterX: Single;
    function GetCenterY: Single;
    procedure SetCenterX(const Value: Single);
    procedure SetCenterY(const Value: Single);
    procedure SetRepeatS(const Value: Boolean);
    procedure SetRepeatT(const Value: Boolean);
  private
    StaticResourcesReady: boolean; static;
    { Create / destroy static stuff for rendering. }
    class procedure StaticGLContextOpen;
    class procedure StaticGLContextClose;
  public
    const
      DefaultBlendingSourceFactor = bsSrcAlpha;
      DefaultBlendingDestinationFactor = bdOneMinusSrcAlpha;
      DefaultBlendingConstantColor: TVector4 = (Data: (1, 1, 1, 1));

    { Prepare image for drawing.

      @param(Image Initial image contents.
        The @code(Image) instance passed here must exist throughout
        the whole lifetime of this TDrawableImage instance (so don't free it earlier).
        It can be automatically freed by us, if OwnsImage = @true.

        It can be @nil.)

      @raises(EImageClassNotSupportedForOpenGL When Image class is not supported
        by OpenGL.) }
    constructor Create(const AImage: TEncodedImage; const ASmoothScaling: boolean;
      const AOwnsImage: boolean); overload;

    { Load image from disk, and prepare for drawing.

      @param(AURL URL (or filename) from which to load the image.
        Often you want to use here castle-data:/ protocol to load application data,
        like:

        @longCode(# Image := TDrawableImage.Create('castle-data:/textures/my_image.png', ...); #)

        See https://castle-engine.io/manual_data_directory.php
      )

      @param(ASmoothScaling The initial value of @link(SmoothScaling),
        determines whether the image scaling is smooth (bilinear filtering)
        or not (nearest-pixel filtering).
        You can always change it later through the @link(SmoothScaling)
        property. But each change has a small cost, so it's more efficient
        to just set the initial value correctly.)
    }
    constructor Create(const AURL: string;
      const ASmoothScaling: boolean = true); overload;

    { Load image from disk, and prepare for drawing.

      @param(AURL URL (or filename) from which to load the image.
        Usually you want to use here URL starting with the "castle-data:" protocol,
        to load an image from application data, like this:

        @longCode(# Image := TDrawableImage.Create('castle-data:/textures/my_image.png', ...); #)
      )

      @param(LoadAsClass Constrain the possible image classes to load into.
        This can force removing (or adding) an alpha channel,
        or converting contents to grayscale or RGB, regardless of the preferred
        image file format.
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
    constructor Create(const AURL: string;
      const LoadAsClass: array of TEncodedImageClass;
      const ResizeToX: Cardinal = 0;
      const ResizeToY: Cardinal = 0;
      const Interpolation: TResizeInterpolation = riBilinear); overload;

    { Load image from disk, and prepare for drawing.

      @param(AURL URL (or filename) from which to load the image.
        Often you want to pass here the result ot ApplicationData
        function, like:

        @longCode(# Image := TDrawableImage.Create('castle-data:/textures/my_image.png', ...); #)
      )

      @param(LoadAsClass Constrain the possible image classes to load into.
        This can force removing (or adding) an alpha channel,
        or converting contents to grayscale or RGB, regardless of the preferred
        image file format.
        Must be a subset of PixelsImageClasses, as other classes cannot
        be loaded into OpenGL 2D images, otherwise you may get
        EImageClassNotSupportedForOpenGL exception.
        Pass empty set [] to load into any allowed class
        (it's equivalent to passing LoadAsClass = PixelsImageClasses).

        You can pass e.g. [TRGBImage] to force loading into an RGB image without
        an alpha channel (it will be stripped from the image if necessary).)

      @param(ASmoothScaling The initial value of @link(SmoothScaling),
        determines whether the image scaling is smooth (bilinear filtering)
        or not (nearest-pixel filtering).
        You can always change it later through the @link(SmoothScaling)
        property. But each change has a small cost, so it's more efficient
        to just set the initial value correctly.)
    }
    constructor Create(const AURL: string;
      const LoadAsClass: array of TEncodedImageClass;
      const ASmoothScaling: boolean); overload;

    destructor Destroy; override;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;

    { Rectangle representing the inside of this image.
      Always (Left,Bottom) are zero, and (Width,Height) correspond to image
      sizes. }
    function Rect: TRectangle;

    { How to render the alpha channel of the texture.

      @unorderedList(
        @item(acAuto means to guess correct alpha channel from the loaded image
          contents, and the current @link(Color). We use @italic(alpha blending)
          if the loaded image alpha indicates so, or @code(Color[3] < 1).
          Otherwise we use @italic(alpha testing) if the loaded image
          alpha indicates so. Otherwise we don't use any alpha,
          we render image opaque.)
        @item acNone means to ignore it.
        @item acTest means to render with alpha-test.
        @item acBlending means to render with blending.
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
      or TDrawableImage.Create to force your image to have/don't have
      an alpha channel (e.g. use LoadAsClass=[TRGBImage]
      to force RGB image without alpha, use LoadAsClass=[TRGBAlphaImage]
      to force alpha channel). }
    property Alpha: TAutoAlphaChannel read FAlpha write FAlpha
      default acAuto;

    { Blending source factor, if we use blending (see @link(Alpha)). }
    property BlendingSourceFactor: TBlendingSourceFactor
      read FBlendingSourceFactor
      write FBlendingSourceFactor
      default DefaultBlendingSourceFactor;

    { Blending destination factor, if we use blending (see @link(Alpha)). }
    property BlendingDestinationFactor: TBlendingDestinationFactor
      read FBlendingDestinationFactor
      write FBlendingDestinationFactor
      default DefaultBlendingDestinationFactor;

    { For some blending source or destination factors (see
      @link(BlendingSourceFactor), @link(BlendingDestinationFactor)),
      this constant color (or it's alpha value) take part
      in the blending equation. Used only if blending (see @link(Alpha)).
      By default, opaque white. }
    property BlendingConstantColor: TVector4
      read FBlendingConstantColor
      write FBlendingConstantColor;

    { Draw the image as 2D on screen.

      The X, Y parameters determine where the left-bottom
      corner of the image will be placed (from 0 to size - 1).

      You should only use this inside TCastleUserInterface.Render.
      We require that current projection is 2D and
      lighting / depth test and such are off, which is the default state
      there.

      The image is drawn in 2D. In normal circumstances
      1 pixel of the image is just placed over 1 pixel of the screen,
      and we draw the whole image. But you can use the overloaded
      versions where you specify DrawWidth, DrawHeight or ScreenRect,
      and then the indicated image portion is stretched over the designated
      screen area. Note: if you plan to use such stretching when drawing
      the image, then you usually want to create the image with
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
      image horizontally. Although it's usually more comfortable
      to flip using DrawFlipped methods.

      @groupBegin }
    procedure Draw(const X, Y: Single); overload;
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Single); overload;
    procedure Draw(const X, Y, DrawWidth, DrawHeight: Single;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single); overload;

    procedure Draw(const Pos: TVector2Integer); overload;

    procedure Draw(const ScreenRect: TFloatRectangle); overload;
    procedure Draw(const ScreenRect, ImageRect: TFloatRectangle); overload;

    procedure Draw(const ScreenRect: TRectangle); overload;
    procedure Draw(const ScreenRect: TRectangle;
      const ImageX, ImageY, ImageWidth, ImageHeight: Single); overload;
    procedure Draw(const ScreenRect, ImageRect: TRectangle); overload;

    procedure Draw(ScreenRects, ImageRects: PFloatRectangleArray;
      const Count: Integer); overload; virtual;
    { @groupEnd }

    procedure DrawFlipped(const ScreenRect: TRectangle;
      const FlipHorizontal, FlipVertical: boolean); overload;
    procedure DrawFlipped(const ScreenRect: TFloatRectangle;
      const FlipHorizontal, FlipVertical: boolean); overload;
    procedure DrawFlipped(const ScreenRect: TFloatRectangle;
      ImageRect: TFloatRectangle;
      const FlipHorizontal, FlipVertical: boolean); overload;

    { In case of @link(Draw3x3) the corners
      on screen are scaled by this amount. This is especially useful
      for UI scaling, see @link(TUIContainer.UIScaling)
      and @link(TCastleUserInterface.UIScale). }
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
    procedure Draw3x3(const X, Y, DrawWidth, DrawHeight: Single;
      const CornerTop, CornerRight, CornerBottom, CornerLeft: Single); overload;
    procedure Draw3x3(const X, Y, DrawWidth, DrawHeight: Single;
      const Corner: TVector4Integer); overload;
    procedure Draw3x3(const ScreenRect: TRectangle;
      const Corner: TVector4Integer); overload;
    procedure Draw3x3(const ScreenRect: TFloatRectangle;
      const Corner: TVector4Integer); overload;
    procedure Draw3x3(const ScreenRect: TFloatRectangle;
      const Corner: TVector4); overload;

    (* Commented out for now: using Draw3x3 with
       CornerTop = CornerBottom = 0 should have the same effect?

    { Draw the image on the screen, divided into 3x1 parts:
      unscaled left and right sides, and scaled inside.

      Similar to @link(Draw3x3), but image is divided into 3 parts, not 9. }
    procedure Draw3x1(const X, Y, DrawWidth, DrawHeight: Single;
      const SideRight, SideLeft: Integer);
    procedure Draw3x1(const X, Y, DrawWidth, DrawHeight: Single;
      const Side: TVector2Integer);
    procedure Draw3x1(const ScreenRect: TRectangle;
      const Side: TVector2Integer);
    *)

    { Ignore (do not log to CastleLog) situations when Draw3x3
      cannot work because corners are larger than draw area size.
      Set this to @true when it's perfectly possible (you do not want to even
      see it in log) for Draw3x3 calls to fail because corners
      are larger than draw area size.

      This is @true by default now, since it's safer for UI rendering.
      Otherwise causing this problem makes a flood of errors to the log file,
      and it's easy to make this problem when your UI adjusts to the screen size.
    }
    property IgnoreTooLargeCorners: boolean
      read FIgnoreTooLargeCorners write FIgnoreTooLargeCorners default true;

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
      Use this to efficiently replace the image contents on GPU.
      Updates the @link(Width), @link(Height), @link(Alpha) to correspond
      to new image.

      It is valid (and reasonable) to call this with AImage instance equal
      to the image instance that you used when creating this TDrawableImage.
      That's because TDrawableImage @italic(does not) automatically update
      the image contents (once they were loaded to GPU) when you change
      the underlying @link(TEncodedImage) instance.

      It is valid to pass @nil as parameter.

      @italic(Do not call this method too often.) This methos copies
      the image contents from @link(TEncodedImage) to GPU, which is not something
      you want to do every frame. Doing this occasionally,
      or during the loading/preparation stage, is OK.
      Doing this often, e.g. during every @link(TCastleWindowBase.OnRender)
      or @link(TCastleWindowBase.OnUpdate), will hurt your performance.
      During the game, it's most efficient to treat the @link(TDrawableImage) instances
      as "constant" -- avoid changing their contents, avoid creating new ones. }
    procedure Load(const AImage: TEncodedImage); virtual;

    { Prepare all the possible resources.
      Since we may load OpenGL resources on-demand
      (if the TDrawableImage instance was created when OpenGL context
      was not available yet),
      this method can be used to force loading OpenGL resources now (if possible).
      This is only useful if you want to avoid slowdown when loading it later,
      and prefer to do the loading now. }
    procedure PrepareResources;

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
    property CenterX: Single read GetCenterX write SetCenterX default 0.5;
      deprecated 'use RotationCenter';

    { Y coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterY: Single read GetCenterY write SetCenterY default 0.5;
      deprecated 'use RotationCenter';

    { Center of rotation.
      Expressed as a fraction within the drawn ScreenRectangle,
      (0,0) means bottom-left corner, (1,1) means top-right corner.
      Default (0.5,0.5). }
    property RotationCenter: TVector2 read FRotationCenter write FRotationCenter;

    { Rotation in radians. Increase to rotate in a counter-clockwise direction.
      Hint: use function like ArcTan2 from Math unit to convert 2D direction
      into a rotation angle. }
    property Rotation: Single
      read FRotation write FRotation default 0;

    { Clip the image by an arbitrary 2D line defined in @link(ClipLine). }
    property Clip: boolean read FClip write FClip;

    { If @link(Clip), this is the line equation used to determine whether
      we clip the given pixel. Given a line (A, B, C) and pixel (x, y),
      the pixel is clipped (rejected) if @code(A * x + B * y + C < 0).

      The line equation works in texture coordinates.
      If the image is drawn complete on the screen,
      texture coordinates span from (0,0) at bottom-left to (1,1) top-right.

      For example ClipLine = (1, 0, -0.5) means that we reject pixels
      where @code(1 * x + 0 * y - 0.5 < 0). In other words,
      we reject pixels where @code(x < 0.5), so we reject
      the left half of the image. }
    property ClipLine: TVector3 read FClipLine write FClipLine;

    { Get the image contents from GPU to CPU.
      @raises(ECannotSaveTextureContents If we cannot get texture contents
        (not supported on OpenGLES).) }
    function GetContents(const ImageClass: TCastleImageClass): TCastleImage;

    { Internal OpenGL texture identifier.

      You cannot free it (e.g. by glFreeTexture), and you cannot change it's
      parameters (because TDrawableImage assumes it controls all the texture
      parameters). For normal drawing, you do not need this (use @link(Draw)
      or other rendering routines in this class), but it may be useful to
      integrate with external rendering systems. }
    property Texture: TGLTextureId read FTexture;

    { Use a custom shader for image rendering.

      The shader must define the same uniform variables and attributes
      as the standard TDrawableImage shader, like viewport_size, vertex, tex_coord.
      See for example our default shader code:
      https://github.com/castle-engine/castle-engine/blob/master/src/images/opengl/glsl/image.vs
      https://github.com/castle-engine/castle-engine/blob/master/src/images/opengl/glsl/image.fs .

      See also example examples/images_videos/image_render_custom_shader.lpr .

      Note that some uniforms/attributes are only used in some cases.
      For example, when TDrawableImage.Color is White (1, 1, 1, 1), then we don't pass "color" uniform.
      When TDrawableImage.Clip is @false, then we don't pass "clip_line" uniform.
      When assigning your custom shader, you take responsibility of setting
      the shader that matches the current TDrawableImage properties.

      You can (and should, if possible) reuse a single TGLSLProgram instance
      for many TDrawableImage and TCastleImageControl instances
      (as TDrawableImage.CustomShader and TCastleImageControl.CustomShader).
      You can also use your own uniforms in your shader code, of course. }
    property CustomShader: TGLSLProgram read FCustomShader write FCustomShader;

    property Image: TEncodedImage read FImage write SetImage;
    property OwnsImage: boolean read FOwnsImage write FOwnsImage;

    { Start rendering to this image, using an efficient GPU method
      ("Framebuffer Object", FBO).
      Remember that the result lives only on GPU (the underlying TEncodedImage,
      in normal RAM memory, is not updated by this drawing in any way).

      Between @link(RenderToImageBegin) and @link(RenderToImageEnd)
      you can use various rendering methods, like

      @unorderedList(
        @item(TDrawableImage.Draw (to draw @italic(other) TDrawableImage instances on this one),)
        @item(DrawRectangle, DrawCircle, DrawPrimitive2D and friends in CastleGLUtils,)
        @item(@link(TUIContainer.RenderControl Window.Container.RenderControl)
          to render any TCastleUserInterface to this image.)
      )

      Calling the RenderToImageBegin for the first time automatically
      creates a TGLRenderToTexture descendant suitable for rendering to this texture.
      It is automatically released on certain events,
      you can also explicitly release it by @link(RenderToImageFree). }
    procedure RenderToImageBegin(const SetAndRestoreViewport: boolean = true);
    procedure RenderToImageEnd;
    procedure RenderToImageFree;

    { Draw a SourceImage over this image.

      The drawing is done completely on GPU (using the @link(RenderToImageBegin)).
      This is a much faster alternative to drawing using
      @link(TCastleImage.DrawFrom) (that happens on CPU).
      The result "lives" only on GPU (the underlying TEncodedImage,
      in normal RAM memory, is not updated by this drawing in any way).

      If you use this only occasionally, it's nice to later free the resources
      by @link(RenderToImageFree). }
    procedure DrawFrom(const SourceImage: TDrawableImage;
      const DestinationRect, SourceRect: TFloatRectangle);

    { Non-empty only if the image was loaded from URL. }
    property URL: String read FURL;

    { Repeat or clamp texture in S (width) or T (height) dimensions.
      @groupBegin }
    property RepeatS: Boolean read FRepeatS write SetRepeatS default false;
    property RepeatT: Boolean read FRepeatT write SetRepeatT default false;
    { @groupEnd }
  end;

  TGLImageManaged = TDrawableImage deprecated 'use TDrawableImage';
  TGLImageOnDemand = TDrawableImage deprecated 'use TDrawableImage';
  TGLImageCore = TDrawableImage deprecated 'use TDrawableImage';
  TGLImage = TDrawableImage deprecated 'use TDrawableImage';

{$endif read_interface}

{$ifdef read_implementation}

{ TDrawableImage.TImageProgram ------------------------------------------------- }

procedure TDrawableImage.TImageProgram.InitializeUniformsAttributes(
  const ClipLineShader: boolean;
  const ColorTreatment: TColorTreatment);
begin
  UniformViewportSize := Prog.Uniform('viewport_size');
  if ColorTreatment <> ctPureTexture then
    UniformColor := Prog.Uniform('color')
  else
    UniformColor := TGLSLUniform.NotExisting;
  if ClipLineShader then
    UniformClipLine := Prog.Uniform('clip_line')
  else
    UniformClipLine := TGLSLUniform.NotExisting;
  AttribVertex := Prog.Attribute('vertex');
  AttribTexCoord := Prog.Attribute('tex_coord');
end;

{ TDrawableImage ------------------------------------------------------------------- }

constructor TDrawableImage.Create(const AImage: TEncodedImage;
  const ASmoothScaling, AOwnsImage: boolean);
begin
  inherited Create;

  // TODO: use texture cache here, like GL renderer does for textures for 3D.
  // no need to create new OpenGL texture for the same image.

  FColor := White;
  FScaleCorners := 1;
  FRotationCenter := Vector2(0.5, 0.5);
  FRotation := 0;
  { this sets CurrentFilter, that will be immediately used by Load }
  FSmoothScaling := ASmoothScaling;
  FImageAlpha := acNone;
  FAlpha := acAuto;
  FBlendingSourceFactor := DefaultBlendingSourceFactor;
  FBlendingDestinationFactor := DefaultBlendingDestinationFactor;
  FBlendingConstantColor := DefaultBlendingConstantColor;
  FIgnoreTooLargeCorners := true;

  Load(AImage);

  // note that above Create will call virtual Load() which will already set our FImage
  FOwnsImage := AOwnsImage;
  ApplicationProperties.OnGLContextCloseObject.Add(
    {$ifdef CASTLE_OBJFPC}@{$endif} GLContextCloseEvent);
end;

constructor TDrawableImage.Create(const AURL: string;
  const ASmoothScaling: boolean);
begin
  Create(AURL, [], ASmoothScaling);
end;

constructor TDrawableImage.Create(const AURL: string;
  const LoadAsClass: array of TEncodedImageClass;
  const ResizeToX, ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation);
var
  NewImage: TCastleImage;
begin
  if High(LoadAsClass) = -1 then
    NewImage := LoadImage(AURL, PixelsImageClasses, ResizeToX, ResizeToY, Interpolation)
  else
    NewImage := LoadImage(AURL, LoadAsClass, ResizeToX, ResizeToY, Interpolation);
  FURL := AURL;
  Create(NewImage, { ASmoothScaling } true, { AOwnsImage } true);
end;

constructor TDrawableImage.Create(const AURL: string;
  const LoadAsClass: array of TEncodedImageClass;
  const ASmoothScaling: boolean);
var
  NewImage: TEncodedImage;
begin
  if High(LoadAsClass) = -1 then
    NewImage := LoadEncodedImage(AURL, PixelsImageClasses)
  else
    NewImage := LoadEncodedImage(AURL, LoadAsClass);
  FURL := AURL;
  Create(NewImage, ASmoothScaling, { AOwnsImage } true);
end;

destructor TDrawableImage.Destroy;
begin
  if FOwnsImage then
    FreeAndNil(FImage)
  else
    FImage := nil;

  if ApplicationProperties <> nil then
    ApplicationProperties.OnGLContextCloseObject.Remove(
      {$ifdef CASTLE_OBJFPC}@{$endif} GLContextCloseEvent);

  RenderToImageFree;
  GLContextClose;
  inherited;
end;

procedure TDrawableImage.SetImage(const AImage: TEncodedImage);
begin
  { exactly like Load(), but can ignore the call if AImage reference
    is already set.
    Load() should not do it, to allow loading the same AImage reference
    in case AImage contents change. }
  if FImage <> AImage then
    Load(AImage);
end;

procedure TDrawableImage.GLContextOpen(const AImage: TEncodedImage);
begin
  if AImage <> nil then
  begin
    if FTexture = 0 then
      glGenTextures(1, @FTexture);
    LoadGLGeneratedTexture(FTexture, AImage, CurrentFilter,
      CurrentWrap, nil, true);
  end;
end;

procedure TDrawableImage.GLContextClose;
begin
  glFreeTexture(FTexture);
end;

procedure TDrawableImage.Load(const AImage: TEncodedImage);
begin
  if FOwnsImage and (FImage <> AImage) then
    FreeAndNil(FImage)
  else
    FImage := nil;

  if AImage <> nil then
  begin
    FWidth := AImage.Width;
    FHeight := AImage.Height;
    FImageAlpha := AImage.AlphaChannel;
  end else
  begin
    FWidth := 0;
    FHeight := 0;
    FImageAlpha := acNone;
  end;

  { calculate TextureHasOnlyAlpha, useful only for GLSL image rendering }
  TextureHasOnlyAlpha :=
    (AImage <> nil) and
    (AImage is TGrayscaleImage) and
    (TGrayscaleImage(AImage).TreatAsAlpha);

  if ApplicationProperties.IsGLContextOpen then
  begin
    GLContextOpen(AImage);
    StaticGLContextOpen;
  end;

  FImage := AImage;
end;

class procedure TDrawableImage.StaticGLContextOpen;
var
  AlphaTestShader, ClipLineShader: boolean;
  ColorTreatment: TColorTreatment;
  NewProgram: TImageProgram;
  VS, FS: string;
  I: Integer;
begin
  // Some Intel GPUs may not support VBO
  if GLFeatures.VertexBufferObject then
  begin
    for I := Low(VboStorage) to High(VboStorage) do
    begin
      if (VboStorage[I].PointsVbo = 0) then
        glGenBuffers(1, @VboStorage[I].PointsVbo);
      if (VboStorage[I].IndexesVbo = 0) then
        glGenBuffers(1, @VboStorage[I].IndexesVbo);
    end;
  end;

  if not GLFeatures.EnableFixedFunction then
  begin
    { create programs }
    for AlphaTestShader in boolean do
      for ClipLineShader in boolean do
        for ColorTreatment in TColorTreatment do
          if Programs[AlphaTestShader, ClipLineShader, ColorTreatment].Prog = nil then
          begin
            VS := Iff(ClipLineShader, '#define CLIP_LINE' + NL, '') +
                  {$I image.vs.inc};
            FS := Iff(AlphaTestShader, '#define ALPHA_TEST' + NL, '') +
                  Iff(ClipLineShader, '#define CLIP_LINE' + NL, '') +
                  Iff(ColorTreatment in [ctColorMultipliesTexture, ctColorMultipliesTextureAlpha], '#define COLOR_UNIFORM' + NL, '') +
                  Iff(ColorTreatment = ctColorMultipliesTextureAlpha, '#define TEXTURE_HAS_ONLY_ALPHA' + NL, '') +
                  {$I image.fs.inc};
            if LogShaders then
            begin
              WritelnLogMultiline('TDrawableImage GLSL vertex shader', VS);
              WritelnLogMultiline('TDrawableImage GLSL fragment shader', FS);
            end;

            NewProgram.Prog := TGLSLProgram.Create;
            NewProgram.Prog.AttachVertexShader(VS);
            NewProgram.Prog.AttachFragmentShader(FS);
            NewProgram.Prog.Link;
            NewProgram.InitializeUniformsAttributes(ClipLineShader, ColorTreatment);

            Programs[AlphaTestShader, ClipLineShader, ColorTreatment] := NewProgram;
          end;
  end;

  StaticResourcesReady := true;
end;

class procedure TDrawableImage.StaticGLContextClose;
var
  AlphaTestShader, ClipLineShader: boolean;
  ColorTreatment: TDrawableImage.TColorTreatment;
  I: Integer;
begin
  for I := Low(VboStorage) to High(VboStorage) do
  begin
    glFreeBuffer(VboStorage[I].PointsVbo);
    glFreeBuffer(VboStorage[I].IndexesVbo);
    VboStorage[I].PointsVboSize := 0;
    VboStorage[I].IndexesVboSize := 0;
  end;

  { free below regardless of EnableFixedFunction state,
    to avoid memory leaks in case we toggle EnableFixedFunction at runtime }
  // if not GLFeatures.EnableFixedFunction then
  for AlphaTestShader in boolean do
    for ClipLineShader in boolean do
      for ColorTreatment in TColorTreatment do
        FreeAndNil(Programs[AlphaTestShader, ClipLineShader, ColorTreatment].Prog);

  StaticResourcesReady := false;
end;

function TDrawableImage.FinalAlpha: TAlphaChannel;
begin
  if FAlpha = acAuto then
  begin
    Result := FImageAlpha;
    if FColor[3] < 1 then
      Result := acBlending;
  end else
    Result := FAlpha;
end;

procedure TDrawableImage.Draw(const X, Y: Single);
begin
  Draw(
    FloatRectangle(X, Y, Width, Height),
    FloatRectangle(0, 0, Width, Height));
end;

procedure TDrawableImage.Draw(const X, Y, DrawWidth, DrawHeight: Single);
begin
  Draw(
    FloatRectangle(X, Y, DrawWidth, DrawHeight),
    FloatRectangle(0, 0, Width, Height));
end;

procedure TDrawableImage.Draw(const Pos: TVector2Integer);
begin
  Draw(
    FloatRectangle(Pos[0], Pos[1], Width, Height),
    FloatRectangle(0, 0, Width, Height));
end;

procedure TDrawableImage.Draw(const X, Y, DrawWidth, DrawHeight: Single;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single);
begin
  Draw(
    FloatRectangle(X, Y, DrawWidth, DrawHeight),
    FloatRectangle(ImageX, ImageY, ImageWidth, ImageHeight));
end;

procedure TDrawableImage.Draw(const ScreenRect: TRectangle);
begin
  Draw(
    FloatRectangle(ScreenRect),
    FloatRectangle(0, 0, Width, Height));
end;

procedure TDrawableImage.Draw(const ScreenRect: TFloatRectangle);
begin
  Draw(
    ScreenRect,
    FloatRectangle(0, 0, Width, Height));
end;

procedure TDrawableImage.Draw(const ScreenRect: TRectangle;
  const ImageX, ImageY, ImageWidth, ImageHeight: Single);
begin
  Draw(
    FloatRectangle(ScreenRect),
    FloatRectangle(ImageX, ImageY, ImageWidth, ImageHeight));
end;

procedure TDrawableImage.Draw(const ScreenRect, ImageRect: TRectangle);
begin
  Draw(
    FloatRectangle(ScreenRect),
    FloatRectangle(ImageRect));
end;

procedure TDrawableImage.Draw(const ScreenRect, ImageRect: TFloatRectangle);
begin
  Draw(@ScreenRect, @ImageRect, 1);
end;

procedure TDrawableImage.Draw(
  ScreenRects, ImageRects: PFloatRectangleArray; const Count: Integer);

  procedure AlphaBegin;
  begin
    case FinalAlpha of
      {$ifndef OpenGLES}
      acTest:
        if GLFeatures.EnableFixedFunction then
        begin
          glAlphaFunc(GL_GEQUAL, 0.5);
          glEnable(GL_ALPHA_TEST);
        end;
      {$endif not OpenGLES}
      acBlending:
        begin
          if GLFeatures.BlendConstant and
            ( (BlendingSourceFactor in
               [bsConstantColor, bsOneMinusConstantColor,
                bsConstantAlpha, bsOneMinusConstantAlpha]) or
              (BlendingDestinationFactor in
               [bdConstantColor, bdOneMinusConstantColor,
                bdConstantAlpha, bdOneMinusConstantAlpha]) ) then
          begin
            glBlendColor(
              BlendingConstantColor[0],
              BlendingConstantColor[1],
              BlendingConstantColor[2],
              BlendingConstantColor[3]);
          end;

          GLBlendFunction(BlendingSourceFactor, BlendingDestinationFactor);
          glEnable(GL_BLEND);
        end;
    end;
  end;

  procedure AlphaEnd;
  begin
    case FinalAlpha of
      {$ifndef OpenGLES}
      acTest: if GLFeatures.EnableFixedFunction then glDisable(GL_ALPHA_TEST);
      {$endif not OpenGLES}
      acBlending: glDisable(GL_BLEND);
    end;
  end;

var
  Shift: TVector2;

  { Rotate Point by Rotation radians around Shift. }
  function RotatePoint2DAroundShift(const Point: TVector2): TVector2;
  begin
    Result := RotatePoint2D(Point - Shift, Rotation) + Shift;
  end;

  procedure SetupQuadPoints(const ScreenRect, ImageRect: TFloatRectangle;
    QuadPoints: PPointArray);
  var
    TexX0, TexY0, TexX1, TexY1: Single;
    X0, Y0, X1, Y1: Single;
  begin
    TexX0 := ImageRect.Left   / Width;
    TexY0 := ImageRect.Bottom / Height;
    TexX1 := (ImageRect.Left   + ImageRect.Width ) / Width;
    TexY1 := (ImageRect.Bottom + ImageRect.Height) / Height;

    X0 := ScreenRect.Left;
    Y0 := ScreenRect.Bottom;
    X1 := ScreenRect.Left   + ScreenRect.Width;
    Y1 := ScreenRect.Bottom + ScreenRect.Height;

    QuadPoints^[0].TexCoord := Vector2(TexX0, TexY0);
    QuadPoints^[0].Position := Vector2(X0,    Y0);
    QuadPoints^[1].TexCoord := Vector2(TexX1, TexY0);
    QuadPoints^[1].Position := Vector2(X1,    Y0);
    QuadPoints^[2].TexCoord := Vector2(TexX1, TexY1);
    QuadPoints^[2].Position := Vector2(X1,    Y1);
    QuadPoints^[3].TexCoord := Vector2(TexX0, TexY1);
    QuadPoints^[3].Position := Vector2(X0,    Y1);

    if FRotation <> 0 then
    begin
      if UseScreenRotationCenter then
      begin
        Shift := ScreenRotationCenter;
      end else
      begin
        Shift := Vector2(
          ScreenRect.Left   + FRotationCenter.X * ScreenRect.Width,
          ScreenRect.Bottom + FRotationCenter.Y * ScreenRect.Height);
      end;
      QuadPoints^[0].Position := RotatePoint2DAroundShift(QuadPoints^[0].Position);
      QuadPoints^[1].Position := RotatePoint2DAroundShift(QuadPoints^[1].Position);
      QuadPoints^[2].Position := RotatePoint2DAroundShift(QuadPoints^[2].Position);
      QuadPoints^[3].Position := RotatePoint2DAroundShift(QuadPoints^[3].Position);
    end;
  end;

  procedure LoadBuffer(const Target: TGLenum;
    const Vbo: TGLuint; var VboCurrentSize: Cardinal;
    const ContentsPtr: Pointer; const ContentsSize: Cardinal);
  begin
    glBindBuffer(Target, Vbo);
    { Try using glBufferSubData if possible, manual suggests it's better
      than repeatedly calling glBufferData.
      The VBO size will only increase this way, but this is probably OK,
      it's a single VBO for all our UI stuff. }
    if VboCurrentSize < ContentsSize then
    begin
      glBufferData(Target, ContentsSize, ContentsPtr, GL_DYNAMIC_DRAW);
      VboCurrentSize := ContentsSize;
    end else
      glBufferSubData(Target, 0, ContentsSize, ContentsPtr);
  end;

var
  ColorTreatment: TColorTreatment;
  Prog: TImageProgram;
  I: Integer;
  Vbo: PVboStorage;
begin
  if (Count = 0) or
     ((Count = 1) and
      ((ScreenRects^[0].Width = 0) or
       (ScreenRects^[0].Height = 0))) then
    Exit;

  // we load on demand, so do PrepareResources always
  PrepareResources;
  // PrepareResources, done while GL context is active, must make it true
  Assert(FTexture <> 0);
  Assert(StaticResourcesReady);

  if GLFeatures.UseMultiTexturing then glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, FTexture);
  GLEnableTexture(et2D);

  { calculate Indexes contents, only used when Count <> 1 }
  if Count <> 1 then
  begin
    {$ifdef GLIndexesShort}
    if Integer(Count - 1) * 4 + 3 > High(Word) then
      raise Exception.CreateFmt('Maximum index value is larger than %d (max 16-bit unsigned integer). Split your rendering into more shapes (draw calls). Or undefine GLIndexesShort in castleconf.inc to be able to render your data (but beware that it may fail on some buggy Android devices).', [High(Word)]);
    {$endif}
    SetLength(Indexes, Count * 6);
    for I := 0 to Count - 1 do
    begin
      { 1st triangle of quad }
      Indexes[I * 6    ] := I * 4;
      Indexes[I * 6 + 1] := I * 4 + 1;
      Indexes[I * 6 + 2] := I * 4 + 2;
      { 2nd triangle of quad }
      Indexes[I * 6 + 3] := I * 4;
      Indexes[I * 6 + 4] := I * 4 + 2;
      Indexes[I * 6 + 5] := I * 4 + 3;
    end;
  end;

  { calculate Point contents }
  SetLength(Points, Count * 4);
  for I := 0 to Count - 1 do
    SetupQuadPoints(ScreenRects^[I], ImageRects^[I], @Points[I * 4]);

  if GLFeatures.VertexBufferObject then
  begin
    Vbo := @VboStorage[VboStorageNext];
    Inc(VboStorageNext);
    if VboStorageNext = High(VboStorage) + 1 then
      VboStorageNext := 0;

    LoadBuffer(GL_ARRAY_BUFFER, Vbo^.PointsVbo, Vbo^.PointsVboSize,
      @Points[0], Length(Points) * SizeOf(TPoint));

    if Count <> 1 then
      LoadBuffer(GL_ELEMENT_ARRAY_BUFFER, Vbo^.IndexesVbo, Vbo^.IndexesVboSize,
        @Indexes[0], Length(Indexes) * SizeOf(Indexes[0]));
  end;

  AlphaBegin;

  if not GLFeatures.EnableFixedFunction then
  begin
    if TextureHasOnlyAlpha then
      ColorTreatment := ctColorMultipliesTextureAlpha else
    if not TCastleColor.PerfectlyEquals(Color, White) then
      ColorTreatment := ctColorMultipliesTexture else
      ColorTreatment := ctPureTexture;

    if CustomShader <> nil then
    begin
      Prog.Prog := CustomShader;
      Prog.InitializeUniformsAttributes(Clip, ColorTreatment);
    end else
      Prog := Programs[FinalAlpha = acTest, Clip, ColorTreatment];

    TGLSLProgram.Current := Prog.Prog;
    Prog.AttribVertex.EnableArray(0, 2, GL_FLOAT, GL_FALSE,
      SizeOf(TPoint), OffsetUInt(Points[0].Position, Points[0]));
    Prog.AttribTexCoord.EnableArray(0, 2, GL_FLOAT, GL_FALSE,
      SizeOf(TPoint), OffsetUInt(Points[0].TexCoord, Points[0]));

    if (RenderContext.Viewport.Left <> 0) or
       (RenderContext.Viewport.Bottom <> 0) then
      WritelnWarning('Drawing TDrawableImage when viewport (left,bottom) is not (0,0) is undefined now');
    Prog.UniformViewportSize.SetValue(Vector2(
      RenderContext.Viewport.Width,
      RenderContext.Viewport.Height));

    if ColorTreatment <> ctPureTexture then
      Prog.UniformColor.SetValue(Color);
    if Clip then
      Prog.UniformClipLine.SetValue(ClipLine);
  end else
  begin
    {$ifndef OpenGLES}
    TGLSLProgram.Current := nil;
    glLoadIdentity();
    glColorv(Color);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    if GLFeatures.VertexBufferObject then
    begin
      glVertexPointer(2, GL_FLOAT,
        SizeOf(TPoint), Offset(Points[0].Position, Points[0]));
      glTexCoordPointer(2, GL_FLOAT,
        SizeOf(TPoint), Offset(Points[0].TexCoord, Points[0]));
    end else
    begin
      glVertexPointer  (2, GL_FLOAT, SizeOf(TPoint), @(Points[0].Position));
      glTexCoordPointer(2, GL_FLOAT, SizeOf(TPoint), @(Points[0].TexCoord));
    end;
    if Clip then
    begin
      { TODO: Clip not implemented in this case yet.
        Need to transform ClipLine into screen coordinates, then do
      glEnable(GL_CLIP_PLANE0);
      glClipPlane(GL_CLIP_PLANE0, Vector4Double(
        FScreenClipLine[0], FScreenClipLine[1], 0, FScreenClipLine[2]));
      }
    end;
    {$endif not OpenGLES}
  end;

  if Count <> 1 then
  begin
    if GLFeatures.VertexBufferObject then
    begin
      { No need to call glBindBuffer below, it is already bound by
        LoadBuffer(GL_ELEMENT_ARRAY_BUFFER, ...) above. }
      //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Vbo^.IndexesVbo);
      glDrawElements(GL_TRIANGLES, Length(Indexes),
        {$ifdef GLIndexesShort} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_INT {$endif}, nil);
    end else
      glDrawElements(GL_TRIANGLES, Length(Indexes),
        {$ifdef GLIndexesShort} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_INT {$endif}, @Indexes[0]);
  end else
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  if not GLFeatures.EnableFixedFunction then
  begin
    // Prog.Prog.Disable; // not necessary, and may give some speed gain if the next drawing call will also use TDrawableImage
    { attribute arrays are enabled independent from GLSL program, so we need
      to disable them separately }
    Prog.AttribVertex.DisableArray;
    Prog.AttribTexCoord.DisableArray;
  end else
  begin
    {$ifndef OpenGLES}
    { TODO: Clip not implemented in this case yet.
    if Clip then
      glDisable(GL_CLIP_PLANE0);
    }
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    {$endif not OpenGLES}
  end;

  AlphaEnd;

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;

  GLEnableTexture(etNone);
end;

procedure TDrawableImage.DrawFlipped(const ScreenRect: TRectangle;
  const FlipHorizontal, FlipVertical: boolean);
begin
  DrawFlipped(FloatRectangle(ScreenRect), FlipHorizontal, FlipVertical);
end;

procedure TDrawableImage.DrawFlipped(const ScreenRect: TFloatRectangle;
  const FlipHorizontal, FlipVertical: boolean);
begin
  DrawFlipped(ScreenRect, FloatRectangle(Rect), FlipHorizontal, FlipVertical);
end;

procedure TDrawableImage.DrawFlipped(
  const ScreenRect: TFloatRectangle; ImageRect: TFloatRectangle;
  const FlipHorizontal, FlipVertical: boolean);
begin
  if FlipHorizontal then
  begin
    ImageRect.Left := ImageRect.Left + ImageRect.Width;
    ImageRect.Width := -ImageRect.Width;
  end;

  if FlipVertical then
  begin
    ImageRect.Bottom := ImageRect.Bottom + ImageRect.Height;
    ImageRect.Height := -ImageRect.Height;
  end;

  Draw(ScreenRect, ImageRect);
end;

procedure TDrawableImage.Draw3x3(const X, Y, DrawWidth, DrawHeight: Single;
  const CornerTop, CornerRight, CornerBottom, CornerLeft: Single);
var
  ScreenRects, ImageRects: packed array [0..8] of TFloatRectangle;
  DrawAddCount: Cardinal;

  { Add the quad to draw to ScreenRects, ImageRects.
    This looks and behaves like TDrawableImage.Draw,
    but the actual drawing is delayed to later. }
  procedure DrawAdd(const X, Y, DrawWidth, DrawHeight: Single;
    const ImageX, ImageY, ImageWidth, ImageHeight: Single);
  begin
    ScreenRects[DrawAddCount] := FloatRectangle(X, Y, DrawWidth, DrawHeight);
    ImageRects[DrawAddCount] := FloatRectangle(ImageX, ImageY, ImageWidth, ImageHeight);
    Inc(DrawAddCount);
  end;

var
  XScreenLeft, XScreenRight, YScreenBottom, YScreenTop,
    HorizontalScreenSize, VerticalScreenSize: Single;
  XImageLeft, XImageRight, YImageBottom, YImageTop,
    HorizontalImageSize, VerticalImageSize: Single;
  EpsilonT, EpsilonR, EpsilonB, EpsilonL: Single;
  DrawCornerTop, DrawCornerRight, DrawCornerBottom, DrawCornerLeft: Single;
const
  { We tweak texture coordinates a little, to avoid bilinear filtering
    that would cause border colors to "bleed" over the texture inside.
    Something minimally > 0.5 is necessary. }
  Epsilon = 0.51;
begin
  DrawCornerTop    := CornerTop    * ScaleCorners;
  DrawCornerRight  := CornerRight  * ScaleCorners;
  DrawCornerBottom := CornerBottom * ScaleCorners;
  DrawCornerLeft   := CornerLeft   * ScaleCorners;

  if not ( (    CornerLeft +     CornerRight <     Width) and
           (DrawCornerLeft + DrawCornerRight < DrawWidth) and
           (    CornerBottom +     CornerTop <     Height) and
           (DrawCornerBottom + DrawCornerTop < DrawHeight)) then
  begin
    if not IgnoreTooLargeCorners then
      WritelnLog('TDrawableImage.Draw3x3', 'Image corners are too large to draw it: corners are %f %f %f %f, image size is %d %d, draw area size is %f %f',
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

  { setup rotations }
  if Rotation <> 0 then
  begin
    UseScreenRotationCenter := true; // only temporary for this Draw3x3 call
    ScreenRotationCenter := Vector2(
      X + FRotationCenter.X * DrawWidth,
      Y + FRotationCenter.Y * DrawHeight);
  end;

  DrawAddCount := 0;

  { 4 corners }
  DrawAdd(XScreenLeft, YScreenBottom, DrawCornerLeft, DrawCornerBottom,
           XImageLeft,  YImageBottom,     CornerLeft,     CornerBottom);
  DrawAdd(XScreenRight, YScreenBottom, DrawCornerRight, DrawCornerBottom,
           XImageRight,  YImageBottom,     CornerRight,     CornerBottom);
  DrawAdd(XScreenRight, YScreenTop, DrawCornerRight, DrawCornerTop,
           XImageRight,  YImageTop,     CornerRight,     CornerTop);
  DrawAdd(XScreenLeft, YScreenTop, DrawCornerLeft, DrawCornerTop,
           XImageLeft,  YImageTop,     CornerLeft,     CornerTop);

  { 4 sides }
  HorizontalScreenSize := DrawWidth - DrawCornerLeft - DrawCornerRight;
  HorizontalImageSize  :=     Width -     CornerLeft -     CornerRight;
  VerticalScreenSize := DrawHeight - DrawCornerTop - DrawCornerBottom;
  VerticalImageSize  :=     Height -     CornerTop -     CornerBottom;

  DrawAdd(XScreenLeft + DrawCornerLeft, YScreenBottom, HorizontalScreenSize, DrawCornerBottom,
           XImageLeft +     CornerLeft,  YImageBottom,  HorizontalImageSize,     CornerBottom);
  DrawAdd(XScreenLeft + DrawCornerLeft, YScreenTop, HorizontalScreenSize, DrawCornerTop,
           XImageLeft +     CornerLeft,  YImageTop,  HorizontalImageSize,     CornerTop);

  DrawAdd(XScreenLeft, YScreenBottom + DrawCornerBottom, DrawCornerLeft, VerticalScreenSize,
           XImageLeft,  YImageBottom +     CornerBottom,     CornerLeft,  VerticalImageSize);
  DrawAdd(XScreenRight, YScreenBottom + DrawCornerBottom, DrawCornerRight, VerticalScreenSize,
           XImageRight,  YImageBottom +     CornerBottom,     CornerRight,  VerticalImageSize);

  { inside }
  if CornerLeft > 0   then EpsilonL := Epsilon else EpsilonL := 0;
  if CornerTop > 0    then EpsilonT := Epsilon else EpsilonT := 0;
  if CornerRight > 0  then EpsilonR := Epsilon else EpsilonR := 0;
  if CornerBottom > 0 then EpsilonB := Epsilon else EpsilonB := 0;

  DrawAdd(X + DrawCornerLeft           , Y + DrawCornerBottom           , HorizontalScreenSize, VerticalScreenSize,
                  CornerLeft + EpsilonL,         CornerBottom + EpsilonB, HorizontalImageSize - (EpsilonL+EpsilonR), VerticalImageSize - (EpsilonT+EpsilonB));

  Assert(DrawAddCount = 9);
  Draw(@ScreenRects, @ImageRects, 9);

  UseScreenRotationCenter := false;
end;

procedure TDrawableImage.Draw3x3(const X, Y, DrawWidth, DrawHeight: Single;
  const Corner: TVector4Integer);
begin
  Draw3x3(X, Y, DrawWidth, DrawHeight,
    Corner[0], Corner[1], Corner[2], Corner[3]);
end;

procedure TDrawableImage.Draw3x3(const ScreenRect: TRectangle;
  const Corner: TVector4Integer);
begin
  Draw3x3(ScreenRect.Left, ScreenRect.Bottom,
    ScreenRect.Width, ScreenRect.Height,
    Corner[0], Corner[1], Corner[2], Corner[3]);
end;

procedure TDrawableImage.Draw3x3(const ScreenRect: TFloatRectangle;
  const Corner: TVector4Integer);
begin
  Draw3x3(ScreenRect.Left, ScreenRect.Bottom,
    ScreenRect.Width, ScreenRect.Height,
    Corner[0], Corner[1], Corner[2], Corner[3]);
end;

procedure TDrawableImage.Draw3x3(const ScreenRect: TFloatRectangle;
  const Corner: TVector4);
begin
  Draw3x3(ScreenRect.Left, ScreenRect.Bottom,
    ScreenRect.Width, ScreenRect.Height,
    Corner[0], Corner[1], Corner[2], Corner[3]);
end;

function TDrawableImage.CurrentFilter: TTextureFilter;
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

function TDrawableImage.CurrentWrap: TTextureWrap2D;

  function RepeatToGL(const IsRepeat: Boolean): TGLenum;
  begin
    if IsRepeat then
      Result := GL_REPEAT
    else
      Result := GLFeatures.CLAMP_TO_EDGE;
  end;

begin
  Result.Data[0] := RepeatToGL(RepeatS);
  Result.Data[1] := RepeatToGL(RepeatT);
end;

procedure TDrawableImage.SetSmoothScaling(const Value: boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    if FTexture <> 0 then
    begin
      glBindTexture(GL_TEXTURE_2D, FTexture);
      SetTextureFilter(GL_TEXTURE_2D, CurrentFilter);
    end;
  end;
end;

function TDrawableImage.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, Width, Height);
end;

procedure TDrawableImage.PrepareResources;
begin
  if ApplicationProperties.IsGLContextOpen then
  begin
    { initialize OpenGL resources on-demand }
    if not StaticResourcesReady then
      StaticGLContextOpen;
    if FTexture = 0 then
      GLContextOpen(FImage);
  end;
end;

function TDrawableImage.GetContents(const ImageClass: TCastleImageClass): TCastleImage;
begin
  PrepareResources;
  if FTexture = 0 then
    raise ECannotSaveTextureContents.Create('TDrawableImage texture was not created for some reason');
  Result := ImageClass.Create(Width, Height);
  SaveTextureContents(Result, FTexture);
end;

procedure TDrawableImage.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
  RenderToImageFree;
end;

procedure TDrawableImage.RenderToImageBegin(const SetAndRestoreViewport: boolean);

  procedure RenderToImageCreate;
  begin
    FRenderToImage := TGLRenderToTexture.Create(Width, Height);
    FRenderToImage.Buffer := tbColor;
    FRenderToImage.SetTexture(FTexture, GL_TEXTURE_2D);
    FRenderToImage.GLContextOpen;
  end;

begin
  PrepareResources;
  Assert(FTexture <> 0);

  if (FRenderToImage = nil) or
     (FRenderToImage.Width <> Width) or
     (FRenderToImage.Height <> Height) then
  begin
    RenderToImageFree;
    RenderToImageCreate;
  end;

  FRenderToImage.RenderBegin;

  if SetAndRestoreViewport then
  begin
    HasViewportToRestore := true;
    ViewportToRestore := RenderContext.Viewport;
    RenderContext.Viewport := Rectangle(0, 0, Width, Height);
  end;
end;

procedure TDrawableImage.RenderToImageEnd;
begin
  FRenderToImage.RenderEnd;

  if HasViewportToRestore then
  begin
    RenderContext.Viewport := ViewportToRestore;
    HasViewportToRestore := false;
  end;
end;

procedure TDrawableImage.RenderToImageFree;
begin
  FreeAndNil(FRenderToImage);
end;

procedure TDrawableImage.DrawFrom(const SourceImage: TDrawableImage;
  const DestinationRect, SourceRect: TFloatRectangle);
begin
  RenderToImageBegin;
  SourceImage.Draw(DestinationRect, SourceRect);
  RenderToImageEnd;
end;

function TDrawableImage.GetCenterX: Single;
begin
  Result := FRotationCenter.Data[0];
end;

function TDrawableImage.GetCenterY: Single;
begin
  Result := FRotationCenter.Data[1];
end;

procedure TDrawableImage.SetCenterX(const Value: Single);
begin
  FRotationCenter.Data[0] := Value;
end;

procedure TDrawableImage.SetCenterY(const Value: Single);
begin
  FRotationCenter.Data[1] := Value;
end;

procedure TDrawableImage.SetRepeatS(const Value: Boolean);
begin
  if FRepeatS <> Value then
  begin
    FRepeatS := Value;
    if FTexture <> 0 then
    begin
      glBindTexture(GL_TEXTURE_2D, FTexture);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, CurrentWrap.Data[0]);
    end;
  end;
end;

procedure TDrawableImage.SetRepeatT(const Value: Boolean);
begin
  if FRepeatT <> Value then
  begin
    FRepeatT := Value;
    if FTexture <> 0 then
    begin
      glBindTexture(GL_TEXTURE_2D, FTexture);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, CurrentWrap.Data[1]);
    end;
  end;
end;

{$endif read_implementation}
