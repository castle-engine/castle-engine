{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ This unit contains canvas classes for drawing and applying effects.}
unit ImagingCanvases;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Types, Classes, ImagingTypes, Imaging, ImagingClasses,
  ImagingFormats, ImagingUtility;

const
  { Color constants in ifA8R8G8B8 format.}
  pcClear   = $00000000;
  pcBlack   = $FF000000;
  pcWhite   = $FFFFFFFF;
  pcMaroon  = $FF800000;
  pcGreen   = $FF008000;
  pcOlive   = $FF808000;
  pcNavy    = $FF000080;
  pcPurple  = $FF800080;
  pcTeal    = $FF008080;
  pcGray    = $FF808080;
  pcSilver  = $FFC0C0C0;
  pcRed     = $FFFF0000;
  pcLime    = $FF00FF00;
  pcYellow  = $FFFFFF00;
  pcBlue    = $FF0000FF;
  pcFuchsia = $FFFF00FF;
  pcAqua    = $FF00FFFF;
  pcLtGray  = $FFC0C0C0;
  pcDkGray  = $FF808080;

  MaxPenWidth = 256;

type
  EImagingCanvasError = class(EImagingError);
  EImagingCanvasBlendingError = class(EImagingError);

  { Fill mode used when drawing filled objects on canvas.}
  TFillMode = (
    fmSolid,  // Solid fill using current fill color
    fmClear   // No filling done
  );

  { Pen mode used when drawing lines, object outlines, and similar on canvas.}
  TPenMode = (
    pmSolid,  // Draws solid lines using current pen color.
    pmClear   // No drawing done
  );

  { Source and destination blending factors for drawing functions with blending.
    Blending formula: SrcColor * SrcFactor + DestColor * DestFactor }
  TBlendingFactor = (
    bfIgnore,           // Don't care
    bfZero,             // For Src and Dest, Factor = (0, 0, 0, 0)
    bfOne,              // For Src and Dest, Factor = (1, 1, 1, 1)
    bfSrcAlpha,         // For Src and Dest, Factor = (Src.A, Src.A, Src.A, Src.A)
    bfOneMinusSrcAlpha, // For Src and Dest, Factor = (1 - Src.A, 1 - Src.A, 1 - Src.A, 1 - Src.A)
    bfDstAlpha,         // For Src and Dest, Factor = (Dest.A, Dest.A, Dest.A, Dest.A)
    bfOneMinusDstAlpha, // For Src and Dest, Factor = (1 - Dest.A, 1 - Dest.A, 1 - Dest.A, 1 - Dest.A)
    bfSrcColor,         // For Dest,         Factor = (Src.R, Src.R, Src.B, Src.A)
    bfOneMinusSrcColor, // For Dest,         Factor = (1 - Src.R, 1 - Src.G, 1 - Src.B, 1 - Src.A)
    bfDstColor,         // For Src,          Factor = (Dest.R, Dest.G, Dest.B, Dest.A)
    bfOneMinusDstColor  // For Src,          Factor = (1 - Dest.R, 1 - Dest.G, 1 - Dest.B, 1 - Dest.A)
  );

  { Procedure for custom pixel write modes with blending.}
  TPixelWriteProc = procedure(const SrcPix: TColorFPRec; DestPtr: PByte;
    DestInfo: PImageFormatInfo; SrcFactor, DestFactor: TBlendingFactor);

  { Represents 3x3 convolution filter kernel.}
  TConvolutionFilter3x3 = record
    Kernel: array[0..2, 0..2] of LongInt;
    Divisor: LongInt;
    Bias: Single;
  end;

  { Represents 5x5 convolution filter kernel.}
  TConvolutionFilter5x5 = record
    Kernel: array[0..4, 0..4] of LongInt;
    Divisor: LongInt;
    Bias: Single;
  end;

  TPointTransformFunction = function(const Pixel: TColorFPRec;
    Param1, Param2, Param3: Single): TColorFPRec;

  TDynFPPixelArray = array of TColorFPRec;

  THistogramArray = array[Byte] of Integer;

  TSelectPixelFunction = function(var Pixels: TDynFPPixelArray): TColorFPRec;

  { Base canvas class for drawing objects, applying effects, and other.
    Constructor takes TBaseImage (or pointer to TImageData). Source image
    bits are not copied but referenced so all canvas functions affect
    source image and vice versa. When you change format or resolution of
    source image you must call UpdateCanvasState method (so canvas could
    recompute some data size related stuff).

    TImagingCanvas works for all image data formats except special ones
    (compressed). Because of this its methods are quite slow (they usually work
    with colors in ifA32R32G32B32F format). If you want fast drawing you
    can use one of fast canvas classes. These descendants of TImagingCanvas
    work only for few select formats (or only one) but they are optimized thus
    much faster.
  }
  TImagingCanvas = class(TObject)
  private
    FDataSizeOnUpdate: LongInt;
    FLineRecursion: Boolean;
    function GetPixel32(X, Y: LongInt): TColor32; virtual;
    function GetPixelFP(X, Y: LongInt): TColorFPRec; virtual;
    function GetValid: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPixel32(X, Y: LongInt; const Value: TColor32); virtual;
    procedure SetPixelFP(X, Y: LongInt; const Value: TColorFPRec); virtual;
    procedure SetPenColor32(const Value: TColor32); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPenColorFP(const Value: TColorFPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPenWidth(const Value: LongInt); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetFillColor32(const Value: TColor32); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetFillColorFP(const Value: TColorFPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetClipRect(const Value: TRect);
    procedure CheckBeforeBlending(SrcFactor, DestFactor: TBlendingFactor; DestCanvas: TImagingCanvas);
  protected
    FPData: PImageData;
    FClipRect: TRect;
    FPenColorFP: TColorFPRec;
    FPenColor32: TColor32;
    FPenMode: TPenMode;
    FPenWidth: LongInt;
    FFillColorFP: TColorFPRec;
    FFillColor32: TColor32;
    FFillMode: TFillMode;
    FNativeColor: TColorFPRec;
    FFormatInfo: TImageFormatInfo;

    { Returns pointer to pixel at given position.}
    function GetPixelPointer(X, Y: LongInt): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    { Translates given FP color to native format of canvas and stores it
      in FNativeColor field (its bit copy) or user pointer (in overloaded method).}
    procedure TranslateFPToNative(const Color: TColorFPRec); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure TranslateFPToNative(const Color: TColorFPRec; Native: Pointer); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    { Clipping function used by horizontal and vertical line drawing functions.}
    function ClipAxisParallelLine(var A1, A2, B: LongInt;
      AStart, AStop, BStart, BStop: LongInt): Boolean;
    { Internal horizontal line drawer used mainly for filling inside of objects
      like ellipses and circles.}
    procedure HorzLineInternal(X1, X2, Y: LongInt; Color: Pointer; Bpp: LongInt); virtual;
    procedure CopyPixelInternal(X, Y: LongInt; Pixel: Pointer; Bpp: LongInt); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure DrawInternal(const SrcRect: TRect; DestCanvas: TImagingCanvas;
      DestX, DestY: LongInt; SrcFactor, DestFactor: TBlendingFactor; PixelWriteProc: TPixelWriteProc);
    procedure StretchDrawInternal(const SrcRect: TRect; DestCanvas: TImagingCanvas;
      const DestRect: TRect; SrcFactor, DestFactor: TBlendingFactor;
      Filter: TResizeFilter; PixelWriteProc: TPixelWriteProc);
  public
    constructor CreateForData(ImageDataPointer: PImageData);
    constructor CreateForImage(Image: TBaseImage);
    destructor Destroy; override;

    { Call this method when you change size or format of image this canvas
      operates on (like calling ResizeImage, ConvertImage, or changing Format
      property of TBaseImage descendants).}
    procedure UpdateCanvasState; virtual;
    { Resets clipping rectangle to Rect(0, 0, ImageWidth, ImageHeight).}
    procedure ResetClipRect;

    { Clears entire canvas with current fill color (ignores clipping rectangle
      and always uses fmSolid fill mode).}
    procedure Clear;

    { Draws horizontal line with current pen settings.}
    procedure HorzLine(X1, X2, Y: LongInt); virtual;
    { Draws vertical line with current pen settings.}
    procedure VertLine(X, Y1, Y2: LongInt); virtual;
    { Draws line from [X1, Y1] to [X2, Y2] with current pen settings.}
    procedure Line(X1, Y1, X2, Y2: LongInt); virtual;
    { Draws a rectangle using current pen settings.}
    procedure FrameRect(const Rect: TRect);
    { Fills given rectangle with current fill settings.}
    procedure FillRect(const Rect: TRect); virtual;
    { Fills given rectangle with current fill settings and pixel blending.}
    procedure FillRectBlend(const Rect: TRect; SrcFactor, DestFactor: TBlendingFactor);
    { Draws rectangle which is outlined by using the current pen settings and
      filled by using the current fill settings.}
    procedure Rectangle(const Rect: TRect);
    { Draws ellipse which is outlined by using the current pen settings and
      filled by using the current fill settings. Rect specifies bounding rectangle
      of ellipse to be drawn.}
    procedure Ellipse(const Rect: TRect);
    { Fills area of canvas with current fill color starting at point [X, Y] and
      coloring its neighbors. Default flood fill mode changes color of all
      neighbors with the same color as pixel [X, Y]. With BoundaryFillMode
      set to True neighbors are recolored regardless of their old color,
      but area which will be recolored has boundary (specified by current pen color).}
    procedure FloodFill(X, Y: Integer; BoundaryFillMode: Boolean = False);         

    { Draws contents of this canvas onto another canvas with pixel blending.
      Blending factors are chosen using TBlendingFactor parameters.
      Resulting destination pixel color is:
        SrcColor * SrcFactor +  DstColor * DstFactor}
    procedure DrawBlend(const SrcRect: TRect; DestCanvas: TImagingCanvas;
      DestX, DestY: LongInt; SrcFactor, DestFactor: TBlendingFactor);
    { Draws contents of this canvas onto another one with typical alpha
      blending (Src 'over' Dest, factors are bfSrcAlpha and bfOneMinusSrcAlpha.)}
    procedure DrawAlpha(const SrcRect: TRect; DestCanvas: TImagingCanvas; DestX, DestY: LongInt); virtual;
    { Draws contents of this canvas onto another one using additive blending
      (source and dest factors are bfOne).}
    procedure DrawAdd(const SrcRect: TRect; DestCanvas: TImagingCanvas; DestX, DestY: LongInt);
    { Draws stretched and filtered contents of this canvas onto another canvas
      with pixel blending. Blending factors are chosen using TBlendingFactor parameters.
      Resulting destination pixel color is:
        SrcColor * SrcFactor +  DstColor * DstFactor}
    procedure StretchDrawBlend(const SrcRect: TRect; DestCanvas: TImagingCanvas;
      const DestRect: TRect; SrcFactor, DestFactor: TBlendingFactor;
      Filter: TResizeFilter = rfBilinear);
    { Draws contents of this canvas onto another one with typical alpha
      blending (Src 'over' Dest, factors are bfSrcAlpha and bfOneMinusSrcAlpha.)}
    procedure StretchDrawAlpha(const SrcRect: TRect; DestCanvas: TImagingCanvas;
      const DestRect: TRect; Filter: TResizeFilter = rfBilinear); virtual;
    { Draws contents of this canvas onto another one using additive blending
      (source and dest factors are bfOne).}
    procedure StretchDrawAdd(const SrcRect: TRect; DestCanvas: TImagingCanvas;
      const DestRect: TRect; Filter: TResizeFilter = rfBilinear);

    { Convolves canvas' image with given 3x3 filter kernel. You can use
      predefined filter kernels or define your own.}
    procedure ApplyConvolution3x3(const Filter: TConvolutionFilter3x3);
    { Convolves canvas' image with given 5x5 filter kernel. You can use
      predefined filter kernels or define your own.}
    procedure ApplyConvolution5x5(const Filter: TConvolutionFilter5x5);
    { Computes 2D convolution of canvas' image and given filter kernel.
      Kernel is in row format and KernelSize must be odd number >= 3. Divisor
      is normalizing value based on Kernel (usually sum of all kernel's cells).
      The Bias number shifts each color value by a fixed amount (color values
      are usually in range [0, 1] during processing). If ClampChannels
      is True all output color values are clamped to [0, 1]. You can use
      predefined filter kernels or define your own.}
    procedure ApplyConvolution(Kernel: PLongInt; KernelSize, Divisor: LongInt;
      Bias: Single = 0.0; ClampChannels: Boolean = True); virtual;

    { Applies custom non-linear filter. Filter size is diameter of pixel
      neighborhood. Typical values are 3, 5, or 7. }
    procedure ApplyNonLinearFilter(FilterSize: Integer; SelectFunc: TSelectPixelFunction);
    { Applies median non-linear filter with user defined pixel neighborhood.
      Selects median pixel from the neighborhood as new pixel
      (current implementation is quite slow).}
    procedure ApplyMedianFilter(FilterSize: Integer);
    { Applies min non-linear filter with user defined pixel neighborhood.
      Selects min pixel from the neighborhood as new pixel.}
    procedure ApplyMinFilter(FilterSize: Integer);
    { Applies max non-linear filter with user defined pixel neighborhood.
      Selects max pixel from the neighborhood as new pixel.}
    procedure ApplyMaxFilter(FilterSize: Integer);

    { Transforms pixels one by one by given function. Pixel neighbors are
      not taken into account. Param 1-3 are optional parameters
      for transform function.}
    procedure PointTransform(Transform: TPointTransformFunction;
      Param1, Param2, Param3: Single);
    { Modifies image contrast and brightness. Parameters should be
      in range <-100; 100>.}
    procedure ModifyContrastBrightness(Contrast, Brightness: Single);
    { Gamma correction of individual color channels. Range is (0, +inf),
      1.0 means no change.}
    procedure GammaCorrection(Red, Green, Blue: Single);
    { Inverts colors of all image pixels, makes negative image. Ignores alpha channel.}
    procedure InvertColors; virtual;
    { Simple single level thresholding with threshold level (in range [0, 1])
      for each color channel.}
    procedure Threshold(Red, Green, Blue: Single);
    { Adjusts the color levels of the image by scaling the
      colors falling between specified white and black points to full [0, 1] range.
      The black point specifies the darkest color in the image, white point
      specifies the lightest color, and mid point is gamma aplied to image.
      Black and white point must be in range [0, 1].}
    procedure AdjustColorLevels(BlackPoint, WhitePoint: Single; MidPoint: Single = 1.0);
    { Premultiplies color channel values by alpha. Needed for some platforms/APIs
      to display images with alpha properly.}
    procedure PremultiplyAlpha;
    { Reverses PremultiplyAlpha operation.}
    procedure UnPremultiplyAlpha;

    { Calculates image histogram for each channel and also gray values. Each
      channel has 256 values available. Channel values of data formats with higher
      precision are scaled and rounded. Example: Red[126] specifies number of pixels
      in image with red channel = 126.}
    procedure GetHistogram(out Red, Green, Blue, Alpha, Gray: THistogramArray);
    { Fills image channel with given value leaving other channels intact.
      Use ChannelAlpha, ChannelRed, etc. constants from ImagingTypes as
      channel identifier.}
    procedure FillChannel(ChannelId: Integer; NewChannelValue: Byte); overload;
    { Fills image channel with given value leaving other channels intact.
      Use ChannelAlpha, ChannelRed, etc. constants from ImagingTypes as
      channel identifier.}
    procedure FillChannelFP(ChannelId: Integer; NewChannelValue: Single); overload;

    { Color used when drawing lines, frames, and outlines of objects.}
    property PenColor32: TColor32 read FPenColor32 write SetPenColor32;
    { Color used when drawing lines, frames, and outlines of objects.}
    property PenColorFP: TColorFPRec read FPenColorFP write SetPenColorFP;
    { Pen mode used when drawing lines, object outlines, and similar on canvas.}
    property PenMode: TPenMode read FPenMode write FPenMode;
    { Width with which objects like lines, frames, etc. (everything which uses
      PenColor) are drawn.}
    property PenWidth: LongInt read FPenWidth write SetPenWidth;
    { Color used for filling when drawing various objects.}
    property FillColor32: TColor32 read FFillColor32 write SetFillColor32;
    { Color used for filling when drawing various objects.}
    property FillColorFP: TColorFPRec read FFillColorFP write SetFillColorFP;
    { Fill mode used when drawing filled objects on canvas.}
    property FillMode: TFillMode read FFillMode write FFillMode;
    { Specifies the current color of the pixels of canvas. Native pixel is
      read from canvas and then translated to 32bit ARGB. Reverse operation
      is made when setting pixel color.}
    property Pixels32[X, Y: LongInt]: TColor32 read GetPixel32 write SetPixel32;
    { Specifies the current color of the pixels of canvas. Native pixel is
      read from canvas and then translated to FP ARGB. Reverse operation
      is made when setting pixel color.}
    property PixelsFP[X, Y: LongInt]: TColorFPRec read GetPixelFP write SetPixelFP;
    { Clipping rectangle of this canvas. No pixels outside this rectangle are
      altered by canvas methods if Clipping property is True. Clip rect gets
      reset when UpdateCanvasState is called.}
    property ClipRect: TRect read FClipRect write SetClipRect;
    { Extended format information.}
    property FormatInfo: TImageFormatInfo read FFormatInfo;
    { Indicates that this canvas is in valid state. If False canvas operations
      may crash.}
    property Valid: Boolean read GetValid;

    { Returns all formats supported by this canvas class.}
    class function GetSupportedFormats: TImageFormats; virtual;
  end;

  TImagingCanvasClass = class of TImagingCanvas;

  TScanlineArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PColor32RecArray;
  PScanlineArray = ^TScanlineArray;

  { Fast canvas class for ifA8R8G8B8 format images.}
  TFastARGB32Canvas = class(TImagingCanvas)
  protected
    FScanlines: PScanlineArray;
    procedure AlphaBlendPixels(SrcPix, DestPix: PColor32Rec); {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetPixel32(X, Y: LongInt): TColor32; override;
    procedure SetPixel32(X, Y: LongInt; const Value: TColor32); override;
  public
    destructor Destroy; override;

    procedure UpdateCanvasState; override;

    procedure DrawAlpha(const SrcRect: TRect; DestCanvas: TImagingCanvas; DestX, DestY: LongInt); override;
    procedure StretchDrawAlpha(const SrcRect: TRect; DestCanvas: TImagingCanvas;
      const DestRect: TRect; Filter: TResizeFilter = rfBilinear); override;
    procedure InvertColors; override;

    property Scanlines: PScanlineArray read FScanlines;

    class function GetSupportedFormats: TImageFormats; override;
  end;

const
  { Kernel for 3x3 average smoothing filter.}
  FilterAverage3x3: TConvolutionFilter3x3 = (
    Kernel: ((1, 1, 1),
             (1, 1, 1),
             (1, 1, 1));
    Divisor: 9;
    Bias:    0);

  { Kernel for 5x5 average smoothing filter.}
  FilterAverage5x5: TConvolutionFilter5x5 = (
    Kernel: ((1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1),
             (1, 1, 1, 1, 1));
    Divisor: 25;
    Bias:     0);

  { Kernel for 3x3 Gaussian smoothing filter.}
  FilterGaussian3x3: TConvolutionFilter3x3 = (
    Kernel: ((1, 2, 1),
             (2, 4, 2),
             (1, 2, 1));
    Divisor: 16;
    Bias:     0);

  { Kernel for 5x5 Gaussian smoothing filter.}
  FilterGaussian5x5: TConvolutionFilter5x5 = (
    Kernel: ((1,  4,  6,  4, 1),
             (4, 16, 24, 16, 4),
             (6, 24, 36, 24, 6),
             (4, 16, 24, 16, 4),
             (1,  4,  6,  4, 1));
    Divisor: 256;
    Bias:     0);

  { Kernel for 3x3 Sobel horizontal edge detection filter (1st derivative approximation).}
  FilterSobelHorz3x3: TConvolutionFilter3x3 = (
    Kernel: (( 1,  2,  1),
             ( 0,  0,  0),
             (-1, -2, -1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 3x3 Sobel vertical edge detection filter (1st derivative approximation).}
  FilterSobelVert3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, 0, 1),
             (-2, 0, 2),
             (-1, 0, 1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 3x3 Prewitt horizontal edge detection filter.}
  FilterPrewittHorz3x3: TConvolutionFilter3x3 = (
    Kernel: (( 1,  1,  1),
             ( 0,  0,  0),
             (-1, -1, -1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 3x3 Prewitt vertical edge detection filter.}
  FilterPrewittVert3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, 0, 1),
             (-1, 0, 1),
             (-1, 0, 1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 3x3 Kirsh horizontal edge detection filter.}
  FilterKirshHorz3x3: TConvolutionFilter3x3 = (
    Kernel: (( 5,  5,  5),
             (-3,  0, -3),
             (-3, -3, -3));
    Divisor: 1;
    Bias:    0);

  { Kernel for 3x3 Kirsh vertical edge detection filter.}
  FilterKirshVert3x3: TConvolutionFilter3x3 = (
    Kernel: ((5, -3, -3),
             (5,  0, -3),
             (5, -3, -3));
    Divisor: 1;
    Bias:    0);

  { Kernel for 3x3 Laplace omni-directional edge detection filter
    (2nd derivative approximation).}
  FilterLaplace3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, -1, -1),
             (-1,  8, -1),
             (-1, -1, -1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 5x5 Laplace omni-directional edge detection filter
    (2nd derivative approximation).}
  FilterLaplace5x5: TConvolutionFilter5x5 = (
    Kernel: ((-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, 24, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 3x3 sharpening filter (Laplacian + original color).}
  FilterSharpen3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, -1, -1),
             (-1,  9, -1),
             (-1, -1, -1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 5x5 sharpening filter (Laplacian + original color).}
  FilterSharpen5x5: TConvolutionFilter5x5 = (
    Kernel: ((-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, 25, -1, -1),
             (-1, -1, -1, -1, -1),
             (-1, -1, -1, -1, -1));
    Divisor: 1;
    Bias:    0);

  { Kernel for 5x5 glow filter.}
  FilterGlow5x5: TConvolutionFilter5x5 = (
    Kernel: (( 1, 2,   2, 2, 1),
             ( 2, 0,   0, 0, 2),
             ( 2, 0, -20, 0, 2),
             ( 2, 0,   0, 0, 2),
             ( 1, 2,   2, 2, 1));
    Divisor: 8;
    Bias:    0);

  { Kernel for 3x3 edge enhancement filter.}
  FilterEdgeEnhance3x3: TConvolutionFilter3x3 = (
    Kernel: ((-1, -2, -1),
             (-2, 16, -2),
             (-1, -2, -1));
    Divisor: 4;
    Bias:    0);

  { Kernel for 3x3 contour enhancement filter.}
  FilterTraceContour3x3: TConvolutionFilter3x3 = (
    Kernel: ((-6, -6, -2),
             (-1, 32, -1),
             (-6, -2, -6));
    Divisor: 4;
    Bias:    240/255);

  { Kernel for filter that negates all images pixels.}
  FilterNegative3x3: TConvolutionFilter3x3 = (
    Kernel: ((0,  0, 0),
             (0, -1, 0),
             (0,  0, 0));
    Divisor: 1;
    Bias:    1);

  { Kernel for 3x3 horz/vert embossing filter.}
  FilterEmboss3x3: TConvolutionFilter3x3 = (
    Kernel: ((2,  0,  0),
             (0, -1,  0),
             (0,  0, -1));
    Divisor: 1;
    Bias:    0.5);


{ You can register your own canvas class. List of registered canvases is used
  by FindBestCanvasForImage functions to find best canvas for given image.
  If two different canvases which support the same image data format are
  registered then the one that was registered later is returned (so you can
  override builtin Imaging canvases).}
procedure RegisterCanvas(CanvasClass: TImagingCanvasClass);
{ Returns best canvas for given TImageFormat.}
function FindBestCanvasForImage(ImageFormat: TImageFormat): TImagingCanvasClass; overload;
{ Returns best canvas for given TImageData.}
function FindBestCanvasForImage(const ImageData: TImageData): TImagingCanvasClass; overload;
{ Returns best canvas for given TBaseImage.}
function FindBestCanvasForImage(Image: TBaseImage): TImagingCanvasClass; overload;

implementation

resourcestring
  SConstructorInvalidPointer = 'Invalid pointer (%p) to TImageData passed to TImagingCanvas constructor.';
  SConstructorInvalidImage = 'Invalid image data passed to TImagingCanvas constructor (%s).';
  SConstructorUnsupportedFormat = 'Image passed to TImagingCanvas constructor is in unsupported format (%s)';

var
  // list with all registered TImagingCanvas classes
  CanvasClasses: TList = nil;

procedure RegisterCanvas(CanvasClass: TImagingCanvasClass);
begin
  Assert(CanvasClass <> nil);
  if CanvasClasses = nil then
    CanvasClasses := TList.Create;
  if CanvasClasses.IndexOf(CanvasClass) < 0 then
    CanvasClasses.Add(CanvasClass);
end;

function FindBestCanvasForImage(ImageFormat: TImageFormat): TImagingCanvasClass; overload;
var
  I: LongInt;
begin
  for I := CanvasClasses.Count - 1 downto 0 do
  begin
    if ImageFormat in TImagingCanvasClass(CanvasClasses[I]).GetSupportedFormats then
    begin
      Result := TImagingCanvasClass(CanvasClasses[I]);
      Exit;
    end;
  end;
  Result := TImagingCanvas;
end;

function FindBestCanvasForImage(const ImageData: TImageData): TImagingCanvasClass;
begin
  Result := FindBestCanvasForImage(ImageData.Format);
end;

function FindBestCanvasForImage(Image: TBaseImage): TImagingCanvasClass;
begin
  Result := FindBestCanvasForImage(Image.Format);
end;

{ Canvas helper functions }

procedure PixelBlendProc(const SrcPix: TColorFPRec; DestPtr: PByte;
  DestInfo: PImageFormatInfo; SrcFactor, DestFactor: TBlendingFactor);
var
  DestPix, FSrc, FDst: TColorFPRec;
begin
  // Get set pixel color
  DestPix := DestInfo.GetPixelFP(DestPtr, DestInfo, nil);
  // Determine current blending factors
  case SrcFactor of
    bfZero:             FSrc := ColorFP(0, 0, 0, 0);
    bfOne:              FSrc := ColorFP(1, 1, 1, 1);
    bfSrcAlpha:         FSrc := ColorFP(SrcPix.A, SrcPix.A, SrcPix.A, SrcPix.A);
    bfOneMinusSrcAlpha: FSrc := ColorFP(1 - SrcPix.A, 1 - SrcPix.A, 1 - SrcPix.A, 1 - SrcPix.A);
    bfDstAlpha:         FSrc := ColorFP(DestPix.A, DestPix.A, DestPix.A, DestPix.A);
    bfOneMinusDstAlpha: FSrc := ColorFP(1 - DestPix.A, 1 - DestPix.A, 1 - DestPix.A, 1 - DestPix.A);
    bfDstColor:         FSrc := ColorFP(DestPix.A, DestPix.R, DestPix.G, DestPix.B);
    bfOneMinusDstColor: FSrc := ColorFP(1 - DestPix.A, 1 - DestPix.R, 1 - DestPix.G, 1 - DestPix.B);
  else
    Assert(False);
  end;
  case DestFactor of
    bfZero:             FDst := ColorFP(0, 0, 0, 0);
    bfOne:              FDst := ColorFP(1, 1, 1, 1);
    bfSrcAlpha:         FDst := ColorFP(SrcPix.A, SrcPix.A, SrcPix.A, SrcPix.A);
    bfOneMinusSrcAlpha: FDst := ColorFP(1 - SrcPix.A, 1 - SrcPix.A, 1 - SrcPix.A, 1 - SrcPix.A);
    bfDstAlpha:         FDst := ColorFP(DestPix.A, DestPix.A, DestPix.A, DestPix.A);
    bfOneMinusDstAlpha: FDst := ColorFP(1 - DestPix.A, 1 - DestPix.A, 1 - DestPix.A, 1 - DestPix.A);
    bfSrcColor:         FDst := ColorFP(SrcPix.A, SrcPix.R, SrcPix.G, SrcPix.B);
    bfOneMinusSrcColor: FDst := ColorFP(1 - SrcPix.A, 1 - SrcPix.R, 1 - SrcPix.G, 1 - SrcPix.B);
  else
    Assert(False);
  end;
  // Compute blending formula
  DestPix.R := SrcPix.R * FSrc.R + DestPix.R * FDst.R;
  DestPix.G := SrcPix.G * FSrc.G + DestPix.G * FDst.G;
  DestPix.B := SrcPix.B * FSrc.B + DestPix.B * FDst.B;
  DestPix.A := SrcPix.A * FSrc.A + DestPix.A * FDst.A;
  // Write blended pixel
  DestInfo.SetPixelFP(DestPtr, DestInfo, nil, DestPix);
end;

procedure PixelAlphaProc(const SrcPix: TColorFPRec; DestPtr: PByte;
  DestInfo: PImageFormatInfo; SrcFactor, DestFactor: TBlendingFactor);
var
  DestPix: TColorFPRec;
  SrcAlpha, DestAlpha: Single;
begin
  DestPix := DestInfo.GetPixelFP(DestPtr, DestInfo, nil);
  // Blend the two pixels (Src 'over' Dest alpha composition operation)
  DestPix.A := SrcPix.A + DestPix.A - SrcPix.A * DestPix.A;
  if DestPix.A = 0 then
    SrcAlpha := 0
  else
    SrcAlpha := SrcPix.A / DestPix.A;
  DestAlpha := 1.0 - SrcAlpha;
  DestPix.R := SrcPix.R * SrcAlpha + DestPix.R * DestAlpha;
  DestPix.G := SrcPix.G * SrcAlpha + DestPix.G * DestAlpha;
  DestPix.B := SrcPix.B * SrcAlpha + DestPix.B * DestAlpha;
  // Write blended pixel
  DestInfo.SetPixelFP(DestPtr, DestInfo, nil, DestPix);
end;

procedure PixelAddProc(const SrcPix: TColorFPRec; DestPtr: PByte;
  DestInfo: PImageFormatInfo; SrcFactor, DestFactor: TBlendingFactor);
var
  DestPix: TColorFPRec;
begin
  // Just add Src and Dest
  DestPix := DestInfo.GetPixelFP(DestPtr, DestInfo, nil);
  DestPix.R := SrcPix.R + DestPix.R;
  DestPix.G := SrcPix.G + DestPix.G;
  DestPix.B := SrcPix.B + DestPix.B;
  DestPix.A := SrcPix.A + DestPix.A;
  DestInfo.SetPixelFP(DestPtr, DestInfo, nil, DestPix);
end;

function CompareColors(const C1, C2: TColorFPRec): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := (C1.R * GrayConv.R + C1.G * GrayConv.G + C1.B * GrayConv.B) -
    (C2.R * GrayConv.R + C2.G * GrayConv.G + C2.B * GrayConv.B);
end;

function MedianSelect(var Pixels: TDynFPPixelArray): TColorFPRec;

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, Temp: TColorFPRec;
  begin
    repeat
      I := L;
      J := R;
      P := Pixels[(L + R) shr 1];
      repeat
        while CompareColors(Pixels[I], P) < 0 do Inc(I);
        while CompareColors(Pixels[J], P) > 0 do Dec(J);
        if I <= J then
        begin
          Temp := Pixels[I];
          Pixels[I] := Pixels[J];
          Pixels[J] := Temp;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  // First sort pixels
  QuickSort(0, High(Pixels));
  // Select middle pixel
  Result := Pixels[Length(Pixels) div 2];
end;

function MinSelect(var Pixels: TDynFPPixelArray): TColorFPRec;
var
  I: Integer;
begin
  Result := Pixels[0];
  for I := 1 to High(Pixels) do
  begin
    if CompareColors(Pixels[I], Result) < 0 then
      Result := Pixels[I];
  end;
end;

function MaxSelect(var Pixels: TDynFPPixelArray): TColorFPRec;
var
  I: Integer;
begin
  Result := Pixels[0];
  for I := 1 to High(Pixels) do
  begin
    if CompareColors(Pixels[I], Result) > 0 then
      Result := Pixels[I];
  end;
end;

function TransformContrastBrightness(const Pixel: TColorFPRec; C, B, P3: Single): TColorFPRec;
begin
  Result.A := Pixel.A;
  Result.R := Pixel.R * C + B;
  Result.G := Pixel.G * C + B;
  Result.B := Pixel.B * C + B;
end;

function TransformGamma(const Pixel: TColorFPRec; R, G, B: Single): TColorFPRec;
begin
  Result.A := Pixel.A;
  Result.R := Power(Pixel.R, 1.0 / R);
  Result.G := Power(Pixel.G, 1.0 / G);
  Result.B := Power(Pixel.B, 1.0 / B);
end;

function TransformInvert(const Pixel: TColorFPRec; P1, P2, P3: Single): TColorFPRec;
begin
  Result.A := Pixel.A;
  Result.R := 1.0 - Pixel.R;
  Result.G := 1.0 - Pixel.G;
  Result.B := 1.0 - Pixel.B;
end;

function TransformThreshold(const Pixel: TColorFPRec; R, G, B: Single): TColorFPRec;
begin
  Result.A := Pixel.A;
  Result.R := IffFloat(Pixel.R >= R, 1.0, 0.0);
  Result.G := IffFloat(Pixel.G >= G, 1.0, 0.0);
  Result.B := IffFloat(Pixel.B >= B, 1.0, 0.0);
end;

function TransformLevels(const Pixel: TColorFPRec; BlackPoint, WhitePoint, Exp: Single): TColorFPRec;
begin
  Result.A := Pixel.A;
  if Pixel.R > BlackPoint then
    Result.R := Power((Pixel.R - BlackPoint) / (WhitePoint - BlackPoint), Exp)
  else
    Result.R := 0.0;
  if Pixel.G > BlackPoint then
    Result.G := Power((Pixel.G - BlackPoint) / (WhitePoint - BlackPoint), Exp)
  else
    Result.G := 0.0;
  if Pixel.B > BlackPoint then
    Result.B := Power((Pixel.B - BlackPoint) / (WhitePoint - BlackPoint), Exp)
  else
    Result.B := 0.0;
end;

function TransformPremultiplyAlpha(const Pixel: TColorFPRec; P1, P2, P3: Single): TColorFPRec;
begin
  Result.A := Pixel.A;
  Result.R := Pixel.R * Pixel.A;
  Result.G := Pixel.G * Pixel.A;
  Result.B := Pixel.B * Pixel.A;
end;

function TransformUnPremultiplyAlpha(const Pixel: TColorFPRec; P1, P2, P3: Single): TColorFPRec;
begin
  Result.A := Pixel.A;
  if Pixel.A <> 0.0 then
  begin
    Result.R := Pixel.R / Pixel.A;
    Result.G := Pixel.G / Pixel.A;
    Result.B := Pixel.B / Pixel.A;
  end
  else
  begin
    Result.R := 0;
    Result.G := 0;
    Result.B := 0;
  end;
end;


{ TImagingCanvas class implementation }

constructor TImagingCanvas.CreateForData(ImageDataPointer: PImageData);
begin
  if ImageDataPointer = nil then
    raise EImagingCanvasError.CreateFmt(SConstructorInvalidPointer, [ImageDataPointer]);

  if not TestImage(ImageDataPointer^) then
    raise EImagingCanvasError.CreateFmt(SConstructorInvalidImage, [Imaging.ImageToStr(ImageDataPointer^)]);

  if not (ImageDataPointer.Format in GetSupportedFormats) then
    raise EImagingCanvasError.CreateFmt(SConstructorUnsupportedFormat, [Imaging.ImageToStr(ImageDataPointer^)]);

  FPData := ImageDataPointer;
  FPenWidth := 1;
  SetPenColor32(pcWhite);
  SetFillColor32(pcBlack);
  FFillMode := fmSolid;

  UpdateCanvasState;
end;

constructor TImagingCanvas.CreateForImage(Image: TBaseImage);
begin
  CreateForData(Image.ImageDataPointer);
end;

destructor TImagingCanvas.Destroy;
begin
  inherited Destroy;
end;

function TImagingCanvas.GetPixel32(X, Y: LongInt): TColor32;
begin
  Result := Imaging.GetPixel32(FPData^, X, Y).Color;
end;

function TImagingCanvas.GetPixelFP(X, Y: LongInt): TColorFPRec;
begin
  Result := Imaging.GetPixelFP(FPData^, X, Y);
end;

function TImagingCanvas.GetValid: Boolean;
begin
  Result := (FPData <> nil) and (FDataSizeOnUpdate = FPData.Size);
end;

procedure TImagingCanvas.SetPixel32(X, Y: LongInt; const Value: TColor32);
begin
  if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
    (X < FClipRect.Right) and (Y < FClipRect.Bottom) then
  begin
    Imaging.SetPixel32(FPData^, X, Y, TColor32Rec(Value));
  end;
end;

procedure TImagingCanvas.SetPixelFP(X, Y: LongInt; const Value: TColorFPRec);
begin
  if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
    (X < FClipRect.Right) and (Y < FClipRect.Bottom) then
  begin
    Imaging.SetPixelFP(FPData^, X, Y, TColorFPRec(Value));
  end;
end;

procedure TImagingCanvas.SetPenColor32(const Value: TColor32);
begin
  FPenColor32 := Value;
  TranslatePixel(@FPenColor32, @FPenColorFP, ifA8R8G8B8, ifA32R32G32B32F, nil, nil);
end;

procedure TImagingCanvas.SetPenColorFP(const Value: TColorFPRec);
begin
  FPenColorFP := Value;
  TranslatePixel(@FPenColorFP, @FPenColor32, ifA32R32G32B32F, ifA8R8G8B8, nil, nil);
end;

procedure TImagingCanvas.SetPenWidth(const Value: LongInt);
begin
  FPenWidth := ClampInt(Value, 0, MaxPenWidth);
end;

procedure TImagingCanvas.SetFillColor32(const Value: TColor32);
begin
  FFillColor32 := Value;
  TranslatePixel(@FFillColor32, @FFillColorFP, ifA8R8G8B8, ifA32R32G32B32F, nil, nil);
end;

procedure TImagingCanvas.SetFillColorFP(const Value: TColorFPRec);
begin
  FFillColorFP := Value;
  TranslatePixel(@FFillColorFP, @FFillColor32, ifA32R32G32B32F, ifA8R8G8B8, nil, nil);
end;

procedure TImagingCanvas.SetClipRect(const Value: TRect);
begin
  FClipRect := Value;
  NormalizeRect(FClipRect);
  IntersectRect(FClipRect, FClipRect, Rect(0, 0, FPData.Width, FPData.Height));
end;

procedure TImagingCanvas.CheckBeforeBlending(SrcFactor,
  DestFactor: TBlendingFactor; DestCanvas: TImagingCanvas);
begin
  if SrcFactor in [bfSrcColor, bfOneMinusSrcColor] then
    raise EImagingCanvasBlendingError.Create('Invalid source blending factor. Check the documentation for TBlendingFactor.');
  if DestFactor in [bfDstColor, bfOneMinusDstColor] then
    raise EImagingCanvasBlendingError.Create('Invalid destination blending factor. Check the documentation for TBlendingFactor.');
  if DestCanvas.FormatInfo.IsIndexed then
    raise EImagingCanvasBlendingError.Create('Blending destination canvas cannot be in indexed mode.');
end;

function TImagingCanvas.GetPixelPointer(X, Y: LongInt): Pointer;
begin
  Result := @PByteArray(FPData.Bits)[(Y * FPData.Width + X) * FFormatInfo.BytesPerPixel]
end;

procedure TImagingCanvas.TranslateFPToNative(const Color: TColorFPRec);
begin
  TranslateFPToNative(Color, @FNativeColor);
end;

procedure TImagingCanvas.TranslateFPToNative(const Color: TColorFPRec;
  Native: Pointer);
begin
  ImagingFormats.TranslatePixel(@Color, Native, ifA32R32G32B32F,
    FPData.Format, nil, FPData.Palette);
end;

procedure TImagingCanvas.UpdateCanvasState;
begin
  FDataSizeOnUpdate := FPData.Size;
  ResetClipRect;
  Imaging.GetImageFormatInfo(FPData.Format, FFormatInfo)
end;

procedure TImagingCanvas.ResetClipRect;
begin
  FClipRect := Rect(0, 0, FPData.Width, FPData.Height)
end;

procedure TImagingCanvas.Clear;
begin
  TranslateFPToNative(FFillColorFP);
  Imaging.FillRect(FPData^, 0, 0, FPData.Width, FPData.Height, @FNativeColor);
end;

function TImagingCanvas.ClipAxisParallelLine(var A1, A2, B: LongInt;
  AStart, AStop, BStart, BStop: LongInt): Boolean;
begin
  if (B >= BStart) and (B < BStop) then
  begin
    SwapMin(A1, A2);
    if A1 < AStart then A1 := AStart;
    if A2 >= AStop then A2 := AStop - 1;
    Result := True;
  end
  else
    Result := False;
end;

procedure TImagingCanvas.HorzLineInternal(X1, X2, Y: LongInt; Color: Pointer;
  Bpp: LongInt);
var
  I, WidthBytes: LongInt;
  PixelPtr: PByte;
begin
  if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
  begin
    SwapMin(X1, X2);
    X1 := Max(X1, FClipRect.Left);
    X2 := Min(X2, FClipRect.Right);
    PixelPtr := GetPixelPointer(X1, Y);
    WidthBytes := (X2 - X1) * Bpp;
    case Bpp of
      1: FillMemoryByte(PixelPtr, WidthBytes, PByte(Color)^);
      2: FillMemoryWord(PixelPtr, WidthBytes, PWord(Color)^);
      4: FillMemoryUInt32(PixelPtr, WidthBytes, PUInt32(Color)^);
    else
      for I := X1 to X2 do
      begin
        ImagingFormats.CopyPixel(Color, PixelPtr, Bpp);
        Inc(PixelPtr, Bpp);
       end;
    end;
  end;
end;

procedure TImagingCanvas.CopyPixelInternal(X, Y: LongInt; Pixel: Pointer;
  Bpp: LongInt);
begin
  if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
    (X < FClipRect.Right) and (Y < FClipRect.Bottom) then
  begin
    ImagingFormats.CopyPixel(Pixel, GetPixelPointer(X, Y), Bpp);
  end;
end;

procedure TImagingCanvas.HorzLine(X1, X2, Y: LongInt);
var
  DstRect: TRect;
begin
  if FPenMode = pmClear then Exit;
  SwapMin(X1, X2);
  if IntersectRect(DstRect, Rect(X1, Y - FPenWidth div 2, X2,
    Y + FPenWidth div 2 + FPenWidth mod 2), FClipRect) then
  begin
    TranslateFPToNative(FPenColorFP);
    Imaging.FillRect(FPData^, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, @FNativeColor);
  end;
end;

procedure TImagingCanvas.VertLine(X, Y1, Y2: LongInt);
var
  DstRect: TRect;
begin
  if FPenMode = pmClear then Exit;
  SwapMin(Y1, Y2);
  if IntersectRect(DstRect, Rect(X - FPenWidth div 2, Y1,
    X + FPenWidth div 2 + FPenWidth mod 2, Y2), FClipRect) then
  begin
    TranslateFPToNative(FPenColorFP);
    Imaging.FillRect(FPData^, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, @FNativeColor);
  end;
end;

procedure TImagingCanvas.Line(X1, Y1, X2, Y2: LongInt);
var
  Steep: Boolean;
  Error, YStep, DeltaX, DeltaY, X, Y, I, Bpp, W1, W2, Code1, Code2: LongInt;
begin
  if FPenMode = pmClear then Exit;

  // If line is vertical or horizontal just call appropriate method
  if X2 = X1 then
  begin
    VertLine(X1, Y1, Y2);
    Exit;
  end;
  if Y2 = Y1 then
  begin
    HorzLine(X1, X2, Y1);
    Exit;
  end;

  // Determine if line is steep (angle with X-axis > 45 degrees)
  Steep := Abs(Y2 - Y1) > Abs(X2 - X1);

  // If we need to draw thick line we just draw more 1 pixel lines around
  // the one we already drawn. Setting FLineRecursion assures that we
  // won't be doing recursions till the end of the world.
  if (FPenWidth > 1) and not FLineRecursion then
  begin
    FLineRecursion := True;
    W1 := FPenWidth div 2;
    W2 := W1;
    if FPenWidth mod 2 = 0 then
      Dec(W1);
    if Steep then
    begin
      // Add lines left/right
      for I := 1 to W1 do
        Line(X1, Y1 - I, X2, Y2 - I);
      for I := 1 to W2 do
        Line(X1, Y1 + I, X2, Y2 + I);
    end
    else
    begin
      // Add lines above/under
      for I := 1 to W1 do
        Line(X1 - I, Y1, X2 - I, Y2);
      for I := 1 to W2 do
        Line(X1 + I, Y1, X2 + I, Y2);
    end;
    FLineRecursion := False;
  end;

  with FClipRect do
  begin
    // Use part of Cohen-Sutherland line clipping to determine if any part of line
    // is in ClipRect
    Code1 := Ord(X1 < Left) + Ord(X1 > Right) shl 1 + Ord(Y1 < Top) shl 2 + Ord(Y1 > Bottom) shl 3;
    Code2 := Ord(X2 < Left) + Ord(X2 > Right) shl 1 + Ord(Y2 < Top) shl 2 + Ord(Y2 > Bottom) shl 3;
  end;

  if (Code1 and Code2) = 0 then
  begin
    TranslateFPToNative(FPenColorFP);
    Bpp := FFormatInfo.BytesPerPixel;

    // If line is steep swap X and Y coordinates so later we just have one loop
    // of two (where only one is used according to steepness).
    if Steep then
    begin
      SwapValues(X1, Y1);
      SwapValues(X2, Y2);
    end;
    if X1 > X2 then
    begin
      SwapValues(X1, X2);
      SwapValues(Y1, Y2);
    end;

    DeltaX := X2 - X1;
    DeltaY := Abs(Y2 - Y1);
    YStep := Iff(Y2 > Y1, 1, -1);
    Error := 0;
    Y := Y1;

    // Draw line using Bresenham algorithm. No real line clipping here,
    // just don't draw pixels outsize clip rect.
    for X := X1 to X2 do
    begin
      if Steep then
        CopyPixelInternal(Y, X, @FNativeColor, Bpp)
      else
        CopyPixelInternal(X, Y, @FNativeColor, Bpp);
      Error := Error + DeltaY;
      if Error * 2 >= DeltaX then
      begin
        Inc(Y, YStep);
        Dec(Error, DeltaX);
      end;
    end;
  end;
end;

procedure TImagingCanvas.FrameRect(const Rect: TRect);
var
  HalfPen, PenMod: LongInt;
begin
  if FPenMode = pmClear then Exit;
  HalfPen := FPenWidth div 2;
  PenMod := FPenWidth mod 2;
  HorzLine(Rect.Left - HalfPen, Rect.Right + HalfPen + PenMod - 1, Rect.Top);
  HorzLine(Rect.Left - HalfPen, Rect.Right + HalfPen + PenMod - 1, Rect.Bottom - 1);
  VertLine(Rect.Left, Rect.Top, Rect.Bottom);
  VertLine(Rect.Right - 1, Rect.Top, Rect.Bottom);
end;

procedure TImagingCanvas.FillRect(const Rect: TRect);
var
  DstRect: TRect;
begin
  if (FFillMode <> fmClear) and IntersectRect(DstRect, Rect, FClipRect) then
  begin
    TranslateFPToNative(FFillColorFP);
    Imaging.FillRect(FPData^, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, @FNativeColor);
  end;
end;

procedure TImagingCanvas.FillRectBlend(const Rect: TRect; SrcFactor,
  DestFactor: TBlendingFactor);
var
  DstRect: TRect;
  X, Y: Integer;
  Line: PByte;
begin
  if (FFillMode <> fmClear) and IntersectRect(DstRect, Rect, FClipRect) then
  begin
    CheckBeforeBlending(SrcFactor, DestFactor, Self);
    for Y := DstRect.Top to DstRect.Bottom - 1 do
    begin
      Line := @PByteArray(FPData.Bits)[(Y * FPData.Width + DstRect.Left) * FFormatInfo.BytesPerPixel];
      for X := DstRect.Left to DstRect.Right - 1 do
      begin
        PixelBlendProc(FFillColorFP, Line, @FFormatInfo, SrcFactor, DestFactor);
        Inc(Line, FFormatInfo.BytesPerPixel);
      end;
    end;
  end;
end;

procedure TImagingCanvas.Rectangle(const Rect: TRect);
begin
  FillRect(Rect);
  FrameRect(Rect);
end;

procedure TImagingCanvas.Ellipse(const Rect: TRect);
var
 RadX, RadY, DeltaX, DeltaY, R, RX, RY: LongInt;
 X1, X2, Y1, Y2, Bpp, OldY: LongInt;
 Fill, Pen: TColorFPRec;
begin
  // TODO: Use PenWidth
  X1 := Rect.Left;
  X2 := Rect.Right;
  Y1 := Rect.Top;
  Y2 := Rect.Bottom;

  TranslateFPToNative(FPenColorFP, @Pen);
  TranslateFPToNative(FFillColorFP, @Fill);
  Bpp := FFormatInfo.BytesPerPixel;

  SwapMin(X1, X2);
  SwapMin(Y1, Y2);

  RadX := (X2 - X1) div 2;
  RadY := (Y2 - Y1) div 2;

  Y1 := Y1 + RadY;
  Y2 := Y1;
  OldY := Y1;

  DeltaX := (RadX * RadX);
  DeltaY := (RadY * RadY);
  R  := RadX * RadY * RadY;
  RX := R;
  RY := 0;

  if (FFillMode <> fmClear) then
    HorzLineInternal(X1, X2, Y1, @Fill, Bpp);
  CopyPixelInternal(X1, Y1, @Pen, Bpp);
  CopyPixelInternal(X2, Y1, @Pen, Bpp);

  while RadX > 0 do
  begin
    if R > 0 then
    begin
      Inc(Y1);
      Dec(Y2);
      Inc(RY, DeltaX);
      Dec(R, RY);
    end;
    if R <= 0 then
    begin
      Dec(RadX);
      Inc(X1);
      Dec(X2);
      Dec(RX, DeltaY);
      Inc(R, RX);
    end;

    if (OldY <> Y1) and (FFillMode <> fmClear) then
    begin
      HorzLineInternal(X1, X2, Y1, @Fill, Bpp);
      HorzLineInternal(X1, X2, Y2, @Fill, Bpp);
    end;
    OldY := Y1;

    CopyPixelInternal(X1, Y1, @Pen, Bpp);
    CopyPixelInternal(X2, Y1, @Pen, Bpp);
    CopyPixelInternal(X1, Y2, @Pen, Bpp);
    CopyPixelInternal(X2, Y2, @Pen, Bpp);
  end;
end;

procedure TImagingCanvas.FloodFill(X, Y: Integer; BoundaryFillMode: Boolean);
var
  Stack: array of TPoint;
  StackPos, Y1: Integer;
  OldColor: TColor32;
  SpanLeft, SpanRight: Boolean;

  procedure Push(AX, AY: Integer);
  begin
    if StackPos < High(Stack) then
    begin
      Inc(StackPos);
      Stack[StackPos].X := AX;
      Stack[StackPos].Y := AY;
    end
    else
    begin
      SetLength(Stack, Length(Stack) + FPData.Width);
      Push(AX, AY);
    end;
  end;

  function Pop(out AX, AY: Integer): Boolean;
  begin
    if StackPos > 0 then
    begin
      AX := Stack[StackPos].X;
      AY := Stack[StackPos].Y;
      Dec(StackPos);
      Result := True;
    end
    else
      Result := False;
  end;

  function Compare(AX, AY: Integer): Boolean;
  var
    Color: TColor32;
  begin
    Color := GetPixel32(AX, AY);
    if BoundaryFillMode then
      Result := (Color <> FFillColor32) and (Color <> FPenColor32)
    else
      Result := Color = OldColor;
  end;

begin
  // Scanline Floodfill Algorithm With Stack
  // http://student.kuleuven.be/~m0216922/CG/floodfill.html

  if not PtInRect(FClipRect, Point(X, Y)) then Exit;

  SetLength(Stack, FPData.Width * 4);
  StackPos := 0;

  OldColor := GetPixel32(X, Y);

  Push(X, Y);

  while Pop(X, Y) do
  begin
    Y1 := Y;
    while (Y1 >= FClipRect.Top) and Compare(X, Y1) do
      Dec(Y1);

    Inc(Y1);
    SpanLeft := False;
    SpanRight := False;

    while (Y1 < FClipRect.Bottom) and Compare(X, Y1) do
    begin
      SetPixel32(X, Y1, FFillColor32);
      if not SpanLeft and (X > FClipRect.Left) and Compare(X - 1, Y1) then
      begin
        Push(X - 1, Y1);
        SpanLeft := True;
      end
      else if SpanLeft and (X > FClipRect.Left) and not Compare(X - 1, Y1) then
        SpanLeft := False
      else if not SpanRight and (X < FClipRect.Right - 1) and Compare(X + 1, Y1)then
      begin
        Push(X + 1, Y1);
        SpanRight := True;
      end
      else if SpanRight and (X < FClipRect.Right - 1) and not Compare(X + 1, Y1) then
        SpanRight := False;

      Inc(Y1);
    end;
  end;
end;

procedure TImagingCanvas.DrawInternal(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; DestX, DestY: LongInt; SrcFactor,
  DestFactor: TBlendingFactor; PixelWriteProc: TPixelWriteProc);
var
  X, Y, SrcX, SrcY, Width, Height, SrcBpp, DestBpp: LongInt;
  PSrc: TColorFPRec;
  SrcPointer, DestPointer: PByte;
begin
  CheckBeforeBlending(SrcFactor, DestFactor, DestCanvas);
  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;
  Width := SrcRect.Right - SrcRect.Left;
  Height := SrcRect.Bottom - SrcRect.Top;
  SrcBpp := FFormatInfo.BytesPerPixel;
  DestBpp := DestCanvas.FFormatInfo.BytesPerPixel;
  // Clip src and dst rects
  ClipCopyBounds(SrcX, SrcY, Width, Height, DestX, DestY,
    FPData.Width, FPData.Height, DestCanvas.ClipRect);

  for Y := 0 to Height - 1 do
  begin
    // Get src and dst scanlines
    SrcPointer := @PByteArray(FPData.Bits)[((SrcY + Y) * FPData.Width + SrcX) * SrcBpp];
    DestPointer := @PByteArray(DestCanvas.FPData.Bits)[((DestY + Y) * DestCanvas.FPData.Width + DestX) * DestBpp];

    for X := 0 to Width - 1 do
    begin
      PSrc := FFormatInfo.GetPixelFP(SrcPointer, @FFormatInfo, FPData.Palette);
      // Call pixel writer procedure - combine source and dest pixels
      PixelWriteProc(PSrc, DestPointer, @DestCanvas.FFormatInfo, SrcFactor, DestFactor);
      // Increment pixel pointers
      Inc(SrcPointer, SrcBpp);
      Inc(DestPointer, DestBpp);
    end;
  end;
end;

procedure TImagingCanvas.DrawBlend(const SrcRect: TRect; DestCanvas: TImagingCanvas;
  DestX, DestY: LongInt; SrcFactor, DestFactor: TBlendingFactor);
begin
  DrawInternal(SrcRect, DestCanvas, DestX, DestY, SrcFactor, DestFactor, PixelBlendProc);
end;

procedure TImagingCanvas.DrawAlpha(const SrcRect: TRect; DestCanvas: TImagingCanvas;
  DestX, DestY: LongInt);
begin
  DrawInternal(SrcRect, DestCanvas, DestX, DestY, bfIgnore, bfIgnore, PixelAlphaProc);
end;

procedure TImagingCanvas.DrawAdd(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; DestX, DestY: LongInt);
begin
  DrawInternal(SrcRect, DestCanvas, DestX, DestY, bfIgnore, bfIgnore, PixelAddProc);
end;

procedure TImagingCanvas.StretchDrawInternal(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; const DestRect: TRect;
  SrcFactor, DestFactor: TBlendingFactor; Filter: TResizeFilter;
  PixelWriteProc: TPixelWriteProc);
const
  FilterMapping: array[TResizeFilter] of TSamplingFilter =
    (sfNearest, sfLinear, DefaultCubicFilter, sfLanczos);
var
  X, Y, I, J, SrcX, SrcY, SrcWidth, SrcHeight: LongInt;
  DestX, DestY, DestWidth, DestHeight, SrcBpp, DestBpp: LongInt;
  SrcPix: TColorFPRec;
  MapX, MapY: TMappingTable;
  XMinimum, XMaximum: LongInt;
  LineBuffer: array of TColorFPRec;
  ClusterX, ClusterY: TCluster;
  Weight, AccumA, AccumR, AccumG, AccumB: Single;
  DestLine: PByte;
  FilterFunction: TFilterFunction;
  Radius: Single;
begin
  CheckBeforeBlending(SrcFactor, DestFactor, DestCanvas);
  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;
  SrcWidth := SrcRect.Right - SrcRect.Left;
  SrcHeight := SrcRect.Bottom - SrcRect.Top;
  DestX := DestRect.Left;
  DestY := DestRect.Top;
  DestWidth := DestRect.Right - DestRect.Left;
  DestHeight := DestRect.Bottom - DestRect.Top;
  SrcBpp := FFormatInfo.BytesPerPixel;
  DestBpp := DestCanvas.FFormatInfo.BytesPerPixel;
  // Get actual resampling filter and radius
  FilterFunction := SamplingFilterFunctions[FilterMapping[Filter]];
  Radius := SamplingFilterRadii[FilterMapping[Filter]];
  // Clip src and dst rects
  ClipStretchBounds(SrcX, SrcY, SrcWidth, SrcHeight, DestX, DestY, DestWidth, DestHeight,
      FPData.Width, FPData.Height, DestCanvas.ClipRect);
  // Generate mapping tables
  MapX := BuildMappingTable(DestX, DestX + DestWidth, SrcX, SrcX + SrcWidth,
    FPData.Width, FilterFunction, Radius, False);
  MapY := BuildMappingTable(DestY, DestY + DestHeight, SrcY, SrcY + SrcHeight,
    FPData.Height, FilterFunction, Radius, False);
  FindExtremes(MapX, XMinimum, XMaximum);
  SetLength(LineBuffer, XMaximum - XMinimum + 1);

  for J := 0 to DestHeight - 1 do
  begin
    ClusterY := MapY[J];
    for X := XMinimum to XMaximum do
    begin
      AccumA := 0.0;
      AccumR := 0.0;
      AccumG := 0.0;
      AccumB := 0.0;
      for Y := 0 to Length(ClusterY) - 1 do
      begin
        Weight := ClusterY[Y].Weight;
        SrcPix := FFormatInfo.GetPixelFP(@PByteArray(FPData.Bits)[(ClusterY[Y].Pos * FPData.Width + X) * SrcBpp],
          @FFormatInfo, FPData.Palette);
        AccumB := AccumB + SrcPix.B * Weight;
        AccumG := AccumG + SrcPix.G * Weight;
        AccumR := AccumR + SrcPix.R * Weight;
        AccumA := AccumA + SrcPix.A * Weight;
      end;
      with LineBuffer[X - XMinimum] do
      begin
        A := AccumA;
        R := AccumR;
        G := AccumG;
        B := AccumB;
      end;
    end;

    DestLine := @PByteArray(DestCanvas.FPData.Bits)[((J + DestY) * DestCanvas.FPData.Width + DestX) * DestBpp];

    for I := 0 to DestWidth - 1 do
    begin
      ClusterX := MapX[I];
      AccumA := 0.0;
      AccumR := 0.0;
      AccumG := 0.0;
      AccumB := 0.0;
      for X := 0 to Length(ClusterX) - 1 do
      begin
        Weight := ClusterX[X].Weight;
        with LineBuffer[ClusterX[X].Pos - XMinimum] do
        begin
          AccumB := AccumB + B * Weight;
          AccumG := AccumG + G * Weight;
          AccumR := AccumR + R * Weight;
          AccumA := AccumA + A * Weight;
        end;
      end;

      SrcPix.A := AccumA;
      SrcPix.R := AccumR;
      SrcPix.G := AccumG;
      SrcPix.B := AccumB;

      // Write resulting blended pixel
      PixelWriteProc(SrcPix, DestLine, @DestCanvas.FFormatInfo, SrcFactor, DestFactor);
      Inc(DestLine, DestBpp);
    end;
  end;
end;

procedure TImagingCanvas.StretchDrawBlend(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; const DestRect: TRect;
  SrcFactor, DestFactor: TBlendingFactor; Filter: TResizeFilter);
begin
  StretchDrawInternal(SrcRect, DestCanvas, DestRect, SrcFactor, DestFactor, Filter, PixelBlendProc);
end;

procedure TImagingCanvas.StretchDrawAlpha(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; const DestRect: TRect; Filter: TResizeFilter);
begin
  StretchDrawInternal(SrcRect, DestCanvas, DestRect, bfIgnore, bfIgnore, Filter, PixelAlphaProc);
end;

procedure TImagingCanvas.StretchDrawAdd(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; const DestRect: TRect; Filter: TResizeFilter);
begin
  StretchDrawInternal(SrcRect, DestCanvas, DestRect, bfIgnore, bfIgnore, Filter, PixelAddProc);
end;

procedure TImagingCanvas.ApplyConvolution(Kernel: PLongInt; KernelSize,
  Divisor: LongInt; Bias: Single; ClampChannels: Boolean);
var
  X, Y, I, J, PosY, PosX, SizeDiv2, KernelValue, WidthBytes, Bpp: LongInt;
  R, G, B, DivFloat: Single;
  Pixel: TColorFPRec;
  TempImage: TImageData;
  DstPointer, SrcPointer: PByte;
begin
  SizeDiv2 := KernelSize div 2;
  DivFloat := IffFloat(Divisor > 1, 1.0 / Divisor, 1.0);
  Bpp := FFormatInfo.BytesPerPixel;
  WidthBytes := FPData.Width * Bpp;

  InitImage(TempImage);
  CloneImage(FPData^, TempImage);

  try
    // For every pixel in clip rect
    for Y := FClipRect.Top to FClipRect.Bottom - 1 do
    begin
      DstPointer := @PByteArray(FPData.Bits)[Y * WidthBytes + FClipRect.Left * Bpp];

      for X := FClipRect.Left to FClipRect.Right - 1 do
      begin
        // Reset accumulators
        R := 0.0;
        G := 0.0;
        B := 0.0;

        for J := 0 to KernelSize - 1 do
        begin
          PosY := ClampInt(Y + J - SizeDiv2, FClipRect.Top, FClipRect.Bottom - 1);

          for I := 0 to KernelSize - 1 do
          begin
            PosX := ClampInt(X + I - SizeDiv2, FClipRect.Left, FClipRect.Right - 1);
            SrcPointer := @PByteArray(TempImage.Bits)[PosY * WidthBytes + PosX * Bpp];

            // Get pixels from neighborhood of current pixel and add their
            // colors to accumulators weighted by filter kernel values
            Pixel := FFormatInfo.GetPixelFP(SrcPointer, @FFormatInfo, TempImage.Palette);
            KernelValue := PUInt32Array(Kernel)[J * KernelSize + I];

            R := R + Pixel.R * KernelValue;
            G := G + Pixel.G * KernelValue;
            B := B + Pixel.B * KernelValue;
          end;
        end;

        Pixel := FFormatInfo.GetPixelFP(DstPointer, @FFormatInfo, FPData.Palette);

        Pixel.R := R * DivFloat + Bias;
        Pixel.G := G * DivFloat + Bias;
        Pixel.B := B * DivFloat + Bias;

        if ClampChannels then
          ClampFloatPixel(Pixel);

        // Set resulting pixel color
        FFormatInfo.SetPixelFP(DstPointer, @FFormatInfo, FPData.Palette, Pixel);

        Inc(DstPointer, Bpp);
      end;
    end;

  finally
    FreeImage(TempImage);
  end;
end;

procedure TImagingCanvas.ApplyConvolution3x3(const Filter: TConvolutionFilter3x3);
begin
  ApplyConvolution(@Filter.Kernel, 3, Filter.Divisor, Filter.Bias, True);
end;

procedure TImagingCanvas.ApplyConvolution5x5(const Filter: TConvolutionFilter5x5);
begin
  ApplyConvolution(@Filter.Kernel, 5, Filter.Divisor, Filter.Bias, True);
end;

procedure TImagingCanvas.ApplyNonLinearFilter(FilterSize: Integer; SelectFunc: TSelectPixelFunction);
var
  X, Y, I, J, PosY, PosX, SizeDiv2, WidthBytes, Bpp: LongInt;
  Pixel: TColorFPRec;
  TempImage: TImageData;
  DstPointer, SrcPointer: PByte;
  NeighPixels: TDynFPPixelArray;
begin
  SizeDiv2 := FilterSize div 2;
  Bpp := FFormatInfo.BytesPerPixel;
  WidthBytes := FPData.Width * Bpp;
  SetLength(NeighPixels, FilterSize * FilterSize);

  InitImage(TempImage);
  CloneImage(FPData^, TempImage);

  try
    // For every pixel in clip rect
    for Y := FClipRect.Top to FClipRect.Bottom - 1 do
    begin
      DstPointer := @PByteArray(FPData.Bits)[Y * WidthBytes + FClipRect.Left * Bpp];

      for X := FClipRect.Left to FClipRect.Right - 1 do
      begin
        for J := 0 to FilterSize - 1 do
        begin
          PosY := ClampInt(Y + J - SizeDiv2, FClipRect.Top, FClipRect.Bottom - 1);

          for I := 0 to FilterSize - 1 do
          begin
            PosX := ClampInt(X + I - SizeDiv2, FClipRect.Left, FClipRect.Right - 1);
            SrcPointer := @PByteArray(TempImage.Bits)[PosY * WidthBytes + PosX * Bpp];

            // Get pixels from neighbourhood of current pixel and store them
            Pixel := FFormatInfo.GetPixelFP(SrcPointer, @FFormatInfo, TempImage.Palette);
            NeighPixels[J * FilterSize + I] := Pixel;
          end;
        end;

        // Choose pixel using custom function
        Pixel := SelectFunc(NeighPixels);
        // Set resulting pixel color
        FFormatInfo.SetPixelFP(DstPointer, @FFormatInfo, FPData.Palette, Pixel);

        Inc(DstPointer, Bpp);
      end;
    end;

  finally
    FreeImage(TempImage);
  end;
end;

procedure TImagingCanvas.ApplyMedianFilter(FilterSize: Integer);
begin
  ApplyNonLinearFilter(FilterSize, MedianSelect);
end;

procedure TImagingCanvas.ApplyMinFilter(FilterSize: Integer);
begin
  ApplyNonLinearFilter(FilterSize, MinSelect);
end;

procedure TImagingCanvas.ApplyMaxFilter(FilterSize: Integer);
begin
  ApplyNonLinearFilter(FilterSize, MaxSelect);
end;

procedure TImagingCanvas.PointTransform(Transform: TPointTransformFunction;
  Param1, Param2, Param3: Single);
var
  X, Y, Bpp, WidthBytes: Integer;
  PixPointer: PByte;
  Pixel: TColorFPRec;
begin
  Bpp := FFormatInfo.BytesPerPixel;
  WidthBytes := FPData.Width * Bpp;

  // For every pixel in clip rect
  for Y := FClipRect.Top to FClipRect.Bottom - 1 do
  begin
    PixPointer := @PByteArray(FPData.Bits)[Y * WidthBytes + FClipRect.Left * Bpp];
    for X := FClipRect.Left to FClipRect.Right - 1 do
    begin
      Pixel := FFormatInfo.GetPixelFP(PixPointer, @FFormatInfo, FPData.Palette);

      FFormatInfo.SetPixelFP(PixPointer, @FFormatInfo, FPData.Palette,
        Transform(Pixel, Param1, Param2, Param3));

      Inc(PixPointer, Bpp);
    end;
  end;
end;

procedure TImagingCanvas.ModifyContrastBrightness(Contrast, Brightness: Single);
begin
  PointTransform(TransformContrastBrightness, 1.0 + Contrast / 100,
    Brightness / 100, 0);
end;

procedure TImagingCanvas.GammaCorrection(Red, Green, Blue: Single);
begin
  PointTransform(TransformGamma, Red, Green, Blue);
end;

procedure TImagingCanvas.InvertColors;
begin
  PointTransform(TransformInvert, 0, 0, 0);
end;

procedure TImagingCanvas.Threshold(Red, Green, Blue: Single);
begin
  PointTransform(TransformThreshold, Red, Green, Blue);
end;

procedure TImagingCanvas.AdjustColorLevels(BlackPoint, WhitePoint, MidPoint: Single);
begin
  PointTransform(TransformLevels, BlackPoint, WhitePoint, 1.0 / MidPoint);
end;

procedure TImagingCanvas.PremultiplyAlpha;
begin
  PointTransform(TransformPremultiplyAlpha, 0, 0, 0);
end;

procedure TImagingCanvas.UnPremultiplyAlpha;
begin
  PointTransform(TransformUnPremultiplyAlpha, 0, 0, 0);
end;

procedure TImagingCanvas.GetHistogram(out Red, Green, Blue, Alpha,
  Gray: THistogramArray);
var
  X, Y, Bpp: Integer;
  PixPointer: PByte;
  Color32: TColor32Rec;
begin
  FillChar(Red,   SizeOf(Red), 0);
  FillChar(Green, SizeOf(Green), 0);
  FillChar(Blue,  SizeOf(Blue), 0);
  FillChar(Alpha, SizeOf(Alpha), 0);
  FillChar(Gray,  SizeOf(Gray), 0);

  Bpp := FFormatInfo.BytesPerPixel;

  for Y := FClipRect.Top to FClipRect.Bottom - 1 do
  begin
    PixPointer := @PByteArray(FPData.Bits)[Y * FPData.Width * Bpp + FClipRect.Left * Bpp];
    for X := FClipRect.Left to FClipRect.Right - 1 do
    begin
      Color32 := FFormatInfo.GetPixel32(PixPointer, @FFormatInfo, FPData.Palette);

      Inc(Red[Color32.R]);
      Inc(Green[Color32.G]);
      Inc(Blue[Color32.B]);
      Inc(Alpha[Color32.A]);
      Inc(Gray[Round(GrayConv.R * Color32.R + GrayConv.G * Color32.G + GrayConv.B * Color32.B)]);

      Inc(PixPointer, Bpp);
    end;
  end;
end;

procedure TImagingCanvas.FillChannel(ChannelId: Integer; NewChannelValue: Byte);
var
  X, Y, Bpp: Integer;
  PixPointer: PByte;
  Color32: TColor32Rec;
begin
  Bpp := FFormatInfo.BytesPerPixel;

  for Y := FClipRect.Top to FClipRect.Bottom - 1 do
  begin
    PixPointer := @PByteArray(FPData.Bits)[Y * FPData.Width * Bpp + FClipRect.Left * Bpp];
    for X := FClipRect.Left to FClipRect.Right - 1 do
    begin
      Color32 := FFormatInfo.GetPixel32(PixPointer, @FFormatInfo, FPData.Palette);
      Color32.Channels[ChannelId] := NewChannelValue;
      FFormatInfo.SetPixel32(PixPointer, @FFormatInfo, FPData.Palette, Color32);

      Inc(PixPointer, Bpp);
    end;
  end;
end;

procedure TImagingCanvas.FillChannelFP(ChannelId: Integer; NewChannelValue: Single);
var
  X, Y, Bpp: Integer;
  PixPointer: PByte;
  ColorFP: TColorFPRec;
begin
  Bpp := FFormatInfo.BytesPerPixel;

  for Y := FClipRect.Top to FClipRect.Bottom - 1 do
  begin
    PixPointer := @PByteArray(FPData.Bits)[Y * FPData.Width * Bpp + FClipRect.Left * Bpp];
    for X := FClipRect.Left to FClipRect.Right - 1 do
    begin
      ColorFP := FFormatInfo.GetPixelFP(PixPointer, @FFormatInfo, FPData.Palette);
      ColorFP.Channels[ChannelId] := NewChannelValue;
      FFormatInfo.SetPixelFP(PixPointer, @FFormatInfo, FPData.Palette, ColorFP);

      Inc(PixPointer, Bpp);
    end;
  end;
end;

class function TImagingCanvas.GetSupportedFormats: TImageFormats;
begin
  Result := [ifIndex8..Pred(ifDXT1)];
end;

{ TFastARGB32Canvas }

destructor TFastARGB32Canvas.Destroy;
begin
  FreeMem(FScanlines);
  inherited Destroy;
end;

procedure TFastARGB32Canvas.AlphaBlendPixels(SrcPix, DestPix: PColor32Rec);
var
  SrcAlpha, DestAlpha, FinalAlpha: Integer;
begin
  FinalAlpha := SrcPix.A + 1 + (DestPix.A * (256 - SrcPix.A)) shr 8;
  if FinalAlpha = 0 then
    SrcAlpha := 0
  else
    SrcAlpha := (SrcPix.A shl 8) div FinalAlpha;
  DestAlpha := 256 - SrcAlpha;

  DestPix.A := ClampToByte(FinalAlpha);
  DestPix.R := (SrcPix.R * SrcAlpha + DestPix.R * DestAlpha) shr 8;
  DestPix.G := (SrcPix.G * SrcAlpha + DestPix.G * DestAlpha) shr 8;
  DestPix.B := (SrcPix.B * SrcAlpha + DestPix.B * DestAlpha) shr 8;
end;

procedure TFastARGB32Canvas.DrawAlpha(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; DestX, DestY: LongInt);
var
  X, Y, SrcX, SrcY, Width, Height: LongInt;
  SrcPix, DestPix: PColor32Rec;
begin
  if DestCanvas.ClassType <> Self.ClassType then
  begin
    inherited;
    Exit;
  end;

  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;
  Width := SrcRect.Right - SrcRect.Left;
  Height := SrcRect.Bottom - SrcRect.Top;
  ClipCopyBounds(SrcX, SrcY, Width, Height, DestX, DestY,
    FPData.Width, FPData.Height, DestCanvas.ClipRect);

  for Y := 0 to Height - 1 do
  begin
    SrcPix := @FScanlines[SrcY + Y, SrcX];
    DestPix := @TFastARGB32Canvas(DestCanvas).FScanlines[DestY + Y, DestX];
    for X := 0 to Width - 1 do
    begin
      AlphaBlendPixels(SrcPix, DestPix);
      Inc(SrcPix);
      Inc(DestPix);
    end;
  end;
end;

function TFastARGB32Canvas.GetPixel32(X, Y: LongInt): TColor32;
begin
  Result := FScanlines[Y, X].Color;
end;

procedure TFastARGB32Canvas.SetPixel32(X, Y: LongInt; const Value: TColor32);
begin
  if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
    (X < FClipRect.Right) and (Y < FClipRect.Bottom) then
  begin
    FScanlines[Y, X].Color := Value;
  end;
end;

procedure TFastARGB32Canvas.StretchDrawAlpha(const SrcRect: TRect;
  DestCanvas: TImagingCanvas; const DestRect: TRect; Filter: TResizeFilter);
var
  X, Y, ScaleX, ScaleY, Yp, Xp, Weight1, Weight2, Weight3, Weight4, InvFracY, T1, T2: Integer;
  FracX, FracY: Cardinal;
  SrcX, SrcY, SrcWidth, SrcHeight: LongInt;
  DestX, DestY, DestWidth, DestHeight: LongInt;
  SrcLine, SrcLine2: PColor32RecArray;
  DestPix: PColor32Rec;
  Accum: TColor32Rec;
begin
  if (Filter = rfBicubic) or (DestCanvas.ClassType <> Self.ClassType) then
  begin
    inherited;
    Exit;
  end;

  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;
  SrcWidth := SrcRect.Right - SrcRect.Left;
  SrcHeight := SrcRect.Bottom - SrcRect.Top;
  DestX := DestRect.Left;
  DestY := DestRect.Top;
  DestWidth := DestRect.Right - DestRect.Left;
  DestHeight := DestRect.Bottom - DestRect.Top;
  // Clip src and dst rects
  ClipStretchBounds(SrcX, SrcY, SrcWidth, SrcHeight, DestX, DestY, DestWidth, DestHeight,
      FPData.Width, FPData.Height, DestCanvas.ClipRect);
  ScaleX := (SrcWidth shl 16) div DestWidth;
  ScaleY := (SrcHeight shl 16) div DestHeight;

  // Nearest and linear filtering using fixed point math

  if Filter = rfNearest then
  begin
    Yp := 0;
    for Y := DestY to DestY + DestHeight - 1 do
    begin
      Xp := 0;
      SrcLine := @FScanlines[SrcY + Yp shr 16, SrcX];
      DestPix := @TFastARGB32Canvas(DestCanvas).FScanlines[Y, DestX];
      for X := 0 to DestWidth - 1 do
      begin
        AlphaBlendPixels(@SrcLine[Xp shr 16], DestPix);
        Inc(DestPix);
        Inc(Xp, ScaleX);
      end;
      Inc(Yp, ScaleY);
    end;
  end
  else
  begin
    Yp := (ScaleY shr 1) - $8000;
    for Y := DestY to DestY + DestHeight - 1 do
    begin
      DestPix := @TFastARGB32Canvas(DestCanvas).FScanlines[Y, DestX];
      if Yp < 0 then
      begin
        T1 := 0;
        FracY := 0;
        InvFracY := $10000;
      end
      else
      begin
        T1 := Yp shr 16;
        FracY := Yp and $FFFF;
        InvFracY := (not Yp and $FFFF) + 1;
      end;

      T2 := Iff(T1 < SrcHeight - 1, T1 + 1, T1);
      SrcLine :=  @Scanlines[T1 + SrcY, SrcX];
      SrcLine2 := @Scanlines[T2 + SrcY, SrcX];
      Xp := (ScaleX shr 1) - $8000;

      for X := 0 to DestWidth - 1 do
      begin
        if Xp < 0 then
        begin
          T1 := 0;
          FracX := 0;
        end
        else
        begin
          T1 := Xp shr 16;
          FracX := Xp and $FFFF;
        end;

        T2 := Iff(T1 < SrcWidth - 1, T1 + 1, T1);
        Weight2:= Integer((Cardinal(InvFracY) * FracX) shr 16); // cast to Card, Int can overflow here
        Weight1:= InvFracY - Weight2;
        Weight4:= Integer((Cardinal(FracY) * FracX) shr 16);
        Weight3:= FracY - Weight4;

        Accum.B := (SrcLine[T1].B * Weight1 + SrcLine[T2].B * Weight2 +
          SrcLine2[T1].B * Weight3 + SrcLine2[T2].B * Weight4 + $8000) shr 16;
        Accum.G := (SrcLine[T1].G * Weight1 + SrcLine[T2].G * Weight2 +
          SrcLine2[T1].G * Weight3 + SrcLine2[T2].G * Weight4 + $8000) shr 16;
        Accum.R := (SrcLine[T1].R * Weight1 + SrcLine[T2].R * Weight2 +
          SrcLine2[T1].R * Weight3 + SrcLine2[T2].R * Weight4 + $8000) shr 16;
        Accum.A := (SrcLine[T1].A * Weight1 + SrcLine[T2].A * Weight2 +
          SrcLine2[T1].A * Weight3 + SrcLine2[T2].A * Weight4 + $8000) shr 16;

        AlphaBlendPixels(@Accum, DestPix);

        Inc(Xp, ScaleX);
        Inc(DestPix);
      end;
      Inc(Yp, ScaleY);
     end;
  end;
end;

procedure TFastARGB32Canvas.UpdateCanvasState;
var
  I: LongInt;
  ScanPos: PUInt32;
begin
  inherited UpdateCanvasState;

  // Realloc and update scanline array
  ReallocMem(FScanlines, FPData.Height * SizeOf(PColor32RecArray));
  ScanPos := FPData.Bits;

  for I := 0 to FPData.Height - 1 do
  begin
    FScanlines[I] := PColor32RecArray(ScanPos);
    Inc(ScanPos, FPData.Width);
  end;
end;

class function TFastARGB32Canvas.GetSupportedFormats: TImageFormats;
begin
  Result := [ifA8R8G8B8];
end;

procedure TFastARGB32Canvas.InvertColors;
var
  X, Y: Integer;
  PixPtr: PColor32Rec;
begin
  for Y := FClipRect.Top to FClipRect.Bottom - 1 do
  begin
    PixPtr := @FScanlines[Y, FClipRect.Left];
    for X := FClipRect.Left to FClipRect.Right - 1 do
    begin
      PixPtr.R := not PixPtr.R;
      PixPtr.G := not PixPtr.G;
      PixPtr.B := not PixPtr.B;
      Inc(PixPtr);
    end;
  end;
end;

initialization
  RegisterCanvas(TFastARGB32Canvas);

finalization
  FreeAndNil(CanvasClasses);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - more more more ...
    - implement pen width everywhere
    - more objects (arc, polygon)

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Fixed bug that could raise floating point error in DrawAlpha
      and StretchDrawAlpha.
    - Fixed bug in TImagingCanvas.Line that caused not drawing
      of horz or vert lines.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Added some methods to TFastARGB32Canvas (InvertColors, DrawAlpha/StretchDrawAlpha)
    - Fixed DrawAlpha/StretchDrawAlpha destination alpha calculation.
    - Added PremultiplyAlpha and UnPremultiplyAlpha methods.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added FillChannel methods.
    - Added FloodFill method.
    - Added GetHistogram method.
    - Fixed "Invalid FP operation" in AdjustColorLevels in FPC compiled exes
      (thanks to Carlos Gonzalez).
    - Added TImagingCanvas.AdjustColorLevels method.

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Fixed error that could cause AV in linear and nonlinear filters.
    - Added blended rect filling function FillRectBlend.
    - Added drawing function with blending (DrawAlpha, StretchDrawAlpha,
        StretchDrawAdd, DrawBlend, StretchDrawBlend, ...)
    - Added non-linear filters (min, max, median).
    - Added point transforms (invert, contrast, gamma, brightness).

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added some new filter kernels for convolution.
    - Added FillMode and PenMode properties.
    - Added FrameRect, Rectangle, Ellipse, and Line methods.
    - Removed HorzLine and VertLine from TFastARGB32Canvas - new versions
      in general canvas is now as fast as those in TFastARGB32Canvas
      (only in case of A8R8G8B8 images of course). 
    - Added PenWidth property, updated HorzLine and VertLine to use it.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added TFastARGB32Canvas
    - added convolutions, hline, vline
    - unit created, initial stuff added

}

end.

