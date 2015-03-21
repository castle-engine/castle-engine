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

(*Loading, saving, and processing of 2D (and 3D) images (TCastleImage and descendants).
  Storing images in the memory, loading and saving them from/to files in various
  formats, resizing, converting to grayscale, copying and merging,
  many other image operations --- it's all here.

  The most important class here is @link(TCastleImage).
  It represents an image as a simple uncompressed array of pixels.
  Descendants of TCastleImage define what exactly is a "pixel".
  We have 8-bit color images
  (@link(TRGBAlphaImage), @link(TRGBImage),
  @link(TGrayscaleAlphaImage) and @link(TGrayscaleImage)).
  We also have an image with floating-point precision and range:
  @link(TRGBFloatImage).
  You are free to create more descendants of TCastleImage in your own units
  if you want to encode the pixel differently.

  When reading and writing image files, we understand various image
  formats. See TImageFormat documentation for a current list of supported
  formats, with comments specific to particular formats.
  The basic loading and saving procedures and LoadImage and SaveImage.

  Example usage of this unit:

@longCode(#
  var
    Image: TCastleImage;
  begin
    Image := LoadImage('image.png');
    { scale the image to be 2x smaller }
    Image.Resize(Image.Width div 2, Image.Height div 2);
    SaveImage(Image, 'newimage.png');
  end;
#)

  This unit is of course not dependent on OpenGL or any other rendering
  library. See CastleGLImages for OpenGL image operations (for textures and others).
*)

unit CastleImages;

{
  TODO:
  - implement more impressive resizing filters, at least simple
    linear like gluScaleImage
}

{$include castleconf.inc}
{$modeswitch nestedprocvars}{$H+}

interface

uses SysUtils, Classes, Math, CastleUtils, CastleVectors, CastleRectangles,
  CastlePng, CastleFileFilters, CastleClassUtils, CastleColors,
  FGL, FPImage, FPReadPCX, FPReadGIF, FPReadPSD, FPReadTGA, FPReadTiff, FPReadXPM,
  FPReadJPEG, FPWriteJPEG, FPReadPNM
  {$ifdef CASTLE_PNG_USING_FCL_IMAGE} , FPReadPNG, FPWritePNG {$endif};

type
  TAutoAlphaChannel = (acAuto, acNone, acSimpleYesNo, acFullRange);
  { See TCastleImage.AlphaChannel. }
  TAlphaChannel = acNone .. acFullRange;

const
  { Default parameters for TEncodedImage.AlphaChannel,
    decide how to detect textures alpha channel. }
  DefaultAlphaTolerance = 5;

{ Colors ------------------------------------------------------------ }

{ Check if the two RGB colors are equal, ignoring small differences.
  All three color components may differ by at most Tolerance.
  When Tolerance is 0, this is a normal (exact) comparison. }
function EqualRGB(const Color1, Color2: TVector3Byte; Tolerance: Byte): boolean;

{ TCastleImage --------------------------------------------------------------- }

type
  { Raised by @link(TCastleImage.MakeExtracted) when coordinates on image
    are wrong.
    Possibly I will use it in more routines in the future. }
  EImagePosOutOfRange = class(Exception);

  EImageLerpError = class(Exception);
  EImageLerpInvalidClasses = class(EImageLerpError);
  EImageLerpDifferentSizes = class(EImageLerpError);

  EImageAssignmentError = class(Exception);

  EImageCannotConvertFpImage = class(Exception);

  { Abstract class for an image with unspecified, possibly compressed,
    memory format. The idea is that both uncompressed images (TCastleImage)
    and images compressed for GPU (TGPUCompressedImage) are derived from this class. }
  TEncodedImage = class
  private
    FWidth, FHeight, FDepth: Cardinal;
    FURL: string;
  protected
    { Operate on this by Get/Realloc/FreeMem.
      It's always freed and nil'ed in destructor. }
    FRawPixels: Pointer;
  public
    { URL from which this image was loaded, if any. }
    property URL: string read FURL write FURL;

    destructor Destroy; override;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    property Depth: Cardinal read FDepth;

    property RawPixels: Pointer read FRawPixels;

    { Size of image contents in bytes. }
    function Size: Cardinal; virtual; abstract;

    { Is an image empty.

      @true means that RawPixels = @nil,
      and Width * Height * Depth = 0
      (so either Width = 0 or Height = 0 or Depth = 0).

      @false means that RawPixels <> nil and Width * Height * Depth <> 0
      (so all Width > 0 and Height > 0 and Depth > 0, since they are
      Cardinal (unsigned) always). }
    function IsEmpty: boolean;

    { Does an image have an alpha channel.

      You may also be interested in the AlphaChannel.
      AlphaChannel answers always atNone if HasAlpha = false,
      and always acSimpleYesNo or acFullRange if HasAlpha = true.
      But AlphaChannel may perform longer analysis of pixels
      (to differ between acSimpleYesNo and acFullRange), while this
      function always executes ultra-fast (as it's constant for each
      TCastleImage descendant).

      @italic(Descendants implementors notes:) in this class, TCastleImage,
      this returns @false. Override to return @true for images with
      alpha channel. }
    function HasAlpha: boolean; virtual;

    { @abstract(Check does an image have an alpha channel,
      and if yes analyze alpha channel: is it a single yes-no (only full
      or none values), or does it have alpha values in between?)

      This is quite useful for automatic detection how alpha textures
      should be displayed: for simple yes/no alpha, OpenGL alpha_test
      is a simple solution. For full range alpha, OpenGL blending should
      be used. Blending is a little problematic, since it requires
      special rendering order, since it doesn't cooperate nicely with
      Z-buffer. That's why we try to detect simple yes/no alpha textures,
      so that we're able to use simpler alpha test for them.

      We return "simple yes/no alpha channel" is all the alpha values
      (for every pixel) are 0, or 255, or (when AlphaTolerance <> 0)
      are close to them by AlphaTolerance. So, to be precise,
      alpha value must be <= AlphaTolerance, or >= 255 - AlphaTolerance.
      If any alpha value is between [AlphaTolerance + 1, 255 - AlphaTolerance - 1]
      then we return "full range alpha channel".
      Note that for AlphaTolerance >= 128, all images are treated as
      "simple yes/no alpha". Usually, you want to keep AlphaTolerance small.

      @italic(Descendants implementors notes:) in this class, this simply
      always returns atNone. For descendants that have alpha channel,
      implement it, honouring AlphaTolerance as described. }
    function AlphaChannel(
      const AlphaTolerance: Byte = DefaultAlphaTolerance):
      TAlphaChannel; virtual;

    { Rectangle representing the inside of this image.
      Always (Left,Bottom) are zero, and (Width,Height) correspond to image
      sizes. }
    function Rect: TRectangle;
  end;

  { Basic resize interpolation modes, fast and available for all image types. }
  TResizeInterpolation = (riNearest, riBilinear);

  { Resize interpolation modes for MakeResized with TResizeNiceInterpolation
    parameters. These are much slower than our TResizeInterpolation,
    as they are implemented by conversion to FpImage.
    However, they offer some extra quality. }
  TResizeNiceInterpolation = (
    rniNearest,
    rniBilinear,
    rniMitchel,
    rniBlackman,
    rniBlackmanSinc,
    rniBlackmanBessel,
    rniGaussian,
    rniHermite,
    rniLanczos,
    rniQuadratic,
    rniCubic,
    rniCatrom,
    rniHanning,
    rniHamming
  );

  { An abstract class representing image as a simple array of pixels.
    RawPixels is a pointer to Width * Height * Depth of pixels.

    What exactly is a "pixel" is undefined in this class. Each descendant
    of TCastleImage defines it's own pixel encoding and interpretation.
    The only requirement is that all pixels have the same size (PixelSize).
    For example, for TRGBImage a "pixel" is a TVector3Byte type
    representing a (red, green, blue) color value.

    When Depth > 1, the image is actually a 3D (not just 2D!) image.
    We call the particular 2D layers then "slices".
    Although some TCastleImage methods (and functions in other units, like CastleGLImages)
    still operate only on the 1st "slice", that is the 2D image on Depth = 0
    --- be careful. But many methods correctly take the depth into consideration.

    Pixels in RawPixels are ordered in slices, each slice is ordered in rows,
    in each row pixels are specified
    from left to right, rows are specified starting from lower row to upper.
    This means that you can think of RawPixels as

@longCode(#
  ^(packed array[0..Depth - 1, 0..Height - 1, 0..Width - 1] of TPixel)
#)

    Assuming the above definition, RawPixels^[z, y, x]
    is color of pixel at position z, x, y.

    Note that specifying rows from lower to upper follows an OpenGL standard,
    this makes using this unit with OpenGL straightforward.

    Don't ever operate on RawPixels pointer directly --- allocating, reallocating,
    freeing memory pointed to by RawPixels is handled inside this class.
    You must only worry to always free created TCastleImage instances
    (like with any class).

    Note that the only valid states of instances of this class
    are when (Width * Height * Depth > 0 and RawPixels <> nil) or
    (Width * Height * Depth = 0 and RawPixels = nil). Otherwise the fundamental
    assumption that RawPixels is a pointer to Width * Height * Depth pixels would
    be broken (as nil pointer cannot point to anything, and on the other
    side it's rather useless to have a pointer to 0 bytes (since you
    can never dereference it anyway) even if theoretically every PtrInt
    value can be treated as valid pointer to 0 bytes).

    Note about coordinates:

    @orderedList(
      @item(All X, Y, Z coordinates of pixels are 0-based
        (X in range 0..Width-1, and Y in 0..Height-1, and Z in 0..Depth-1).)

      @item(If documentation for some method does not specify otherwise,
        correctness of coordinates is *not* checked in method,
        which can lead to various errors at runtime if you will pass
        incorrect coordinates to given routine.)
    )
  }
  TCastleImage = class(TEncodedImage)
  private
    procedure NotImplemented(const AMethodName: string);
    function ToFpImage: TFPMemoryImage; virtual;
  protected
    { Check that both images have the same sizes and Second image class
      descends from First image class. If not, raise appropriate ELerpXxx
      exceptions.

      Some implementation of TRGBImage.LerpWith may require
      other checks (since LerpWith may be sometimes allowed between unequal
      classes), so this doesn't have to be used by all TRGBImage.LerpWith
      implementations (although it's comfortable for simple implementations). }
    procedure LerpSimpleCheckConditions(SecondImage: TCastleImage);

    { Like DrawFrom, but can assume that all coordinates and sizes are valid.
      Override this to add copying using some more sophisticated method
      than just memory copying. }
    procedure DrawCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer); virtual;
  public
    { Constructor without parameters creates image with Width = Height = Depth = 0
      and RawPixels = nil, so IsEmpty will return @true.

      Both constructors must be virtual, this allows to implement things
      like TCastleImage.MakeCopy. }
    constructor Create; overload; virtual;
    constructor Create(
      const AWidth, AHeight: Cardinal;
      const ADepth: Cardinal = 1); overload; virtual;

    { This is equivalent to SetSize(0, 0, 0).
      It sets Width = Height = 0 and RawPixels = nil. }
    procedure Empty;

    { Change size (Width and Height and Depth).
      Previous pixel contents (RawPixels) are lost,
      and the contents of new pixels are undefined.

      Use other method, like @link(Resize), if you want to change image size
      preserving it's contents. }
    procedure SetSize(
      const AWidth, AHeight: Cardinal;
      const ADepth: Cardinal = 1);
    procedure SetSize(const Source: TCastleImage);

    { Size of TPixel in bytes for this TCastleImage descendant. }
    class function PixelSize: Cardinal; virtual; abstract;

    { Size of image contents in bytes. }
    function Size: Cardinal; override;

    { Deprecated name for ImageSize. }
    function ImageSize: Cardinal; deprecated;

    { Number of color components in TPixel.

      E.g. RGB is 3 components and RGB+Alpha is 4 components,
      RGB+Exponent is 3 components (because it describes only
      Red, Green and Blue values (Exponent value is just used
      to correctly interpret these, it's not a 4th component)). }
    class function ColorComponentsCount: Cardinal; virtual; abstract;

    { Pointer to the (x, y, z) pixel of image.

      Note that they don't check X, Y, Z correctness in any way,
      it's your responsibility to always pass 0 <= X < Width and
      0 <= Y < Height and 0 <= Z < Depth.

      Note that this function @italic(should) be reintroduced in descendants
      to return the same value but typecasted to something better then Pointer
      (something like ^TPixel). }
    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): Pointer;

    { Pointer to the first pixel in the Y row of the image.
      Same thing as @link(PixelPtr) but always with X = 0.

      Note that this function @italic(should) be reintroduced in descendants
      to return the same value but typecasted to something better then Pointer,
      preferably something like ^(array of TPixel). }
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): Pointer;

    { This inverts RGB colors (i.e. changes each RGB component's value
      to High(Byte)-value). Doesn't touch other components,
      e.g. alpha value in case of TRGBAlphaImage descendant.

      Note that this may be not overriden in every TCastleImage descendant,
      then default implementation of this method in this class
      will raise EInternalError. This also means that you must not
      call inherited in descendants when overriding this method. }
    procedure InvertRGBColors; virtual;

    { Set the RGB color portion of the pixel.

      In case of descendants that have more then RGB components,
      other color components are not touched (e.g. in case of TRGBAlphaImage
      alpha value of given pixel is not changed).

      In case of descendants that don't have anything like RGB encoded
      inside (e.g. TGrayscaleImage), this should not be overriden and then
      default implementation of this method in this class
      will raise EInternalError. This also means that you must not
      call inherited in descendants when overriding this method.

      As usual, you are responsible for guaranting correctness of given
      X, Y coordinates because their correctness is not checked here. }
    procedure SetColorRGB(const X, Y: Integer; const v: TVector3Single); virtual;

    { Create a new object that has exactly the same class
      and the same contents as this object.
      (note: no, this function is *not* constructor, because it's implemented
      in TCastleImage, but it always returns some descendant of TCastleImage). }
    function MakeCopy: TCastleImage;

    { Change Width and Height and appropriately stretch
      image contents.

      If ResizeWidth or ResizeHeight is 0 then it means to take
      Width or Height, respectively.
      So e.g. using ResizeWidth = ResizeHeight = 0 is the same thing
      as using ResizeWidth = Width and ResizeHeight = Height and this is NOP.

      Remember that resizing may change RawPixels pointer, so all pointers
      that you aquired using functions like
      RawPixels, RGBPixels, AlphaPixels, RowPtr, PixelPtr
      may be invalid after calling Resize.

      If ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from CastleProgress to indicate progress of operation. }
    procedure Resize(ResizeWidth, ResizeHeight: Cardinal;
      const Interpolation: TResizeInterpolation = riNearest;
      const ProgressTitle: string = '');

    { Change Width and Height and appropriately stretch
      image contents.

      Preserves corners (provided in the same clockwise way
      as TGLImage.Draw3x3: top, right, bottom, left), scaling the Corners
      parameter (proportially to image scaling), and making sure that filtering
      (especially bilinear) does not "leak" colors from one image area to another.
      Effectively, the image is scaled like a 9 separate parts,
      and colors cannot bleed from part to another.

      Both ResizeWidth, ResizeHeight parameters must be provided and non-zero. }
    procedure Resize3x3(const ResizeWidth, ResizeHeight: Cardinal;
      var Corners: TVector4Integer;
      const Interpolation: TResizeInterpolation);

    { Create a new TCastleImage instance with size ResizeWidth, ResizeHeight
      and pixels copied from us and appropriately stretched.
      Class of new instance is the same as our class.

      As with @link(Resize), ResizeTo* = 0 means to use current Width/Height.
      So e.g. using MakeResized(0, 0) is the same thing as using MakeCopy.

      As with @link(Resize),
      if ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from CastleProgress to indicate progress of operation. }
    function MakeResized(ResizeWidth, ResizeHeight: Cardinal;
      const Interpolation: TResizeInterpolation = riNearest;
      const ProgressTitle: string = ''): TCastleImage;

    { Create a new TCastleImage instance with size ResizeWidth, ResizeHeight
      and pixels copied from us and appropriately stretched.
      It is not guaranteed that class of new instance is the same as our class.

      As with @link(Resize), ResizeTo* = 0 means to use current Width/Height.

      This uses slow but (potentially) pretty interpolation mode
      expressed as TResizeNiceInterpolation.
      It is implemented only for some descendants --- currently, TRGBImage
      and TRGBAlphaImage. }
    function MakeResized(ResizeWidth, ResizeHeight: Cardinal;
      const Interpolation: TResizeNiceInterpolation): TCastleImage;

    { Mirror image horizotally (that is right edge is swapped with left edge). }
    procedure FlipHorizontal;

    { Mirror image vertically. }
    procedure FlipVertical;

    { Make rotated version of the image.
      See @link(Rotate) for description of parameters. }
    function MakeRotated(Angle: Integer): TCastleImage;

    { Rotate image by Angle * 90 degrees, clockwise.
      For example, 0 does nothing. 1 rotates by 90 degrees, 2 rotates
      by 180, 3 rotates by 270. All other values (negative too) are circular
      (modulo), so e.g. 4 again does nothing, 5 rotates by 90 degrees and so on. }
    procedure Rotate(const Angle: Integer);

    { Create a new instance with the same class, and size
      TileX * Width and TileY * Height and contents being our contents
      duplicated (tiled).
      Must be TileX, TileY > 0. }
    function MakeTiled(TileX, TileY: Cardinal): TCastleImage;

    { Extract rectangular area of this image.
      X0 and Y0 are start position (lower-left corner),
      ExtractWidth, ExtractHeight specify size of area.

      This checks parameters for correctness -- if start position in not
      good or ExtractWidth/Height are too large exception
      @link(EImagePosOutOfRange) is raised. }
    function MakeExtracted(X0, Y0, ExtractWidth, ExtractHeight: Cardinal): TCastleImage;

    { Set all image pixels to the same value.
      This is implemented only in descendants that represent a pixel
      as a TVector4Byte (e.g. TRGBAlphaImage) or TVector3Byte
      (e.g. TRGBImage, 4th component is ignored in this case).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method. }
    procedure Clear(const Pixel: TVector4Byte); overload; virtual;
    procedure Clear(const Pixel: TCastleColor); overload;

    { Check do all image pixels have the same value Pixel.
      This is implemented only in descendants that represent a pixel
      as TVector4Byte or TVector3Byte (4th component is ignored in this
      case), just like method @link(Clear).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method. }
    function IsClear(const Pixel: TVector4Byte): boolean; virtual;

    { Multiply each RGB color by a matrix.
      This is a useful routine for many various conversions of image colors.
      Every pixel's RGB color is multiplied by given Matrix,
      i.e. PixelRGBColor := Matrix * PixelRGBColor.

      If some value in some channel will be < 0, it will be set to 0.
      And if it will be > High(Byte), it will be set to High(Byte).

      Examples: when
        Matrix = IdentityMatrix3Single, this is NOOP.
        Matrix = ((2, 0, 0), (0, 1, 0), (0, 0, 1))
          red channel is made lighter.
        Matrix = ((0, 0, 1), (0, 1, 0), (1, 0, 0))
          swaps red and blue channel.
        Matrix = ((0.33, 0.33, 0.33),
                  (0.33, 0.33, 0.33),
                  (0.33, 0.33, 0.33))
          is a simple conversion to grayscale (actually incorrect, even if often
          visually acceptable; actually instead of 0.33 one has to use
          GrayscaleFloat/ByteValues, this is already implemented
          in ImageTransformColorsTo1st function)

      Note: it's often more optimal to hard-code necessary color transformations
      as TColorModulatorFunc and use ModulateRGB.

      This function is only implemented for images that represent Pixel
      as RGB values, for now this means TRGBImage and TRGBAlphaImage.
      In case of TRGBAlphaImage (or any other class that represents
      colors as RGB + something more) alpha channel (i.e. "something more")
      is ignored (i.e. left without any modification).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method. }
    procedure TransformRGB(const Matrix: TMatrix3Single); virtual;

    { Process each pixel by given function.
      If ColorModulator = nil then this procedure does nothing.
      Else, every RGB color value of an image will be transformed using
      ColorModulator.

      Like TransformRGB:
      This function is only implemented for images that represent Pixel
      as RGB values, for now this means TRGBImage and TRGBAlphaImage.
      In case of TRGBAlphaImage (or any other class that represents
      colors as RGB + something more) alpha channel (i.e. "something more")
      is ignored (i.e. left without any modification).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method.  }
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); virtual;

    { Just like ModulateRGB, but this returns new image, not changing initial
      image. This means that if ColorModulator = nil this is
      equivalent to MakeCopy.

      Implemented if and only if ModulateRGB is implemented. }
     function MakeModulatedRGB(
       const ColorModulator: TColorModulatorByteFunc): TCastleImage;

    { Convert image colors to grayscale.

      Implemented if and only if ModulateRGB is implemented.
      When image has alpha channel, alpha channel value
      (or just anything beyond 3 rgb components) is ignored (not modified).

      This changes color to grayscale, but format of memory storage is the same.
      For example, for TRGBImage, they are still kept in RGB format
      (just Red = Green = Blue). If you want to convert to true Grayscale format,
      you should use TRGBImage.ToGrayscale that will create new
      TGrayscaleImage instance. }
    procedure Grayscale;

    { Convert every image color using Color*Convert function from CastleVectors.
      "Channel" parameter determines which Color*Convert function to use
      (Red, Green or Blue), must be 0, 1 or 2.

      Implemented if and only if ModulateRGB is implemented. }
    procedure ConvertToChannelRGB(Channel: Integer);

    { Converts every image color using Color*Strip function from CastleVectors.
      "Channel" parameter determines which Color*Strip function to use
      (Red, Green or Blue), must be 0, 1 or 2.

      Implemented if and only if ModulateRGB is implemented. }
    procedure StripToChannelRGB(Channel: Integer);

    { Check if given Image has the same class, the same sizes
      (Width, Height) and contains exactly the same pixel values. }
    function IsEqual(Image: TCastleImage): boolean;

    { This is like IsEqual, but is compares only given parts of the images.
      Note that it's your responsibility to make sure that given areas
      are really within the sizes of Self or Image.

      Overloaded version without SelfXxx parameters compares whole Self
      to given part of Image. Analogously, version without ImageXxx parameters
      compares whole Image to part of Self.

      @groupBegin }
    function ArePartsEqual(
      const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
      Image: TCastleImage;
      const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean; overload;

    function ArePartsEqual(
      Image: TCastleImage;
      const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean; overload;

    function ArePartsEqual(
      const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
      Image: TCastleImage): boolean; overload;
    { @groupEnd }

    { Draw one image part on another image.
      X, Y is the lower-left position on the destination image where we draw.
      Optional SourceX, SourceY, SourceWidth, SourceHeight specify
      to use only a part of the source image (without them, we take whole source
      image).
      The pixel on source image (SourceX, SourceY) will be drawn
      on destination image on (X, Y).

      The coordinates and sizes are carefully checked, so that we do not
      try to take some pixels outside of the source or destination image.

      Note that the default implementation of this function in TCastleImage
      can only directly copy the pixels, regardless
      of what information they have. This makes it very fast,
      but not suitable if the source image has some alpha channel
      and you want to apply it over a destination image with blending
      (adding scaled source to a destination color).
      Descendants with alpha channel should override @link(DrawCore)
      to handle drawing with blending.

      @raises(Exception When actual source/destination image classes are not equal.
        In this class, this method can only work when actual image classes
        are equal (that is because we directly move blocks of bytes).)

      @groupBegin }
    procedure DrawFrom(Source: TCastleImage; const X, Y: Integer);
    procedure DrawFrom(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer);
    procedure DrawTo(Destination: TCastleImage; const X, Y: Integer);
    { @groupEnd }

    { Makes linear interpolation of colors from this image and the SecondImage.
      Intuitively, every pixel in new image is set to

@preformatted(
  (1 - Value) * Self[pixel] + Value * SecondImage[pixel]
)

      Both images need to have the exact same size.
      If they are not, EImageLerpDifferentSizes is raised.

      Not all TCastleImage combinations are allowed. Every subclass is required
      to override this to at least handle Lerp between itself.
      That is, TRGBImage.Lerp has to handle Lerp with other TRGBImage,
      TRGBAlphaImage.Lerp has to handle Lerp with other TRGBAlphaImage etc.
      Other combinations may be permitted, if useful and implemented.
      EImageLerpInvalidClasses is raised if given class combinations are
      not allowed.

      In this class, this simply always raises EImageLerpInvalidClasses.

      @raises(EImageLerpDifferentSizes When SecondImage size differs
        from this image.)
      @raises(EImageLerpInvalidClasses When Lerp between this TCastleImage
        descendant class and SecondImage class is not implemented.) }
    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); virtual;

    { Mix 4 colors, with 4 weights, into a resulting color.
      All 4 Colors and OutputColor must be pointers to a pixel of current
      image class, that is they must point to PixelSize bytes of memory.

      @raises(EImageLerpInvalidClasses When mixing is not implemented
        for this image class.) }
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4Single; const Colors: TVector4Pointer); virtual;

    { Copy size and contents from Source.
      This sets our size (Width, Height and Depth)
      to match Source image, and copies pixels from the Source image,
      converting them as closely as possible.
      For example, converting RGBA to RGB will strip alpha channel,
      but copy RGB values.

      When implementing descendants: the base implementation of this method
      in TCastleImage handles only the case when Image class equals our own class.
      And raises EImageAssignmentError in other cases.
      Override this method if you want to actually handle some convertions
      when assignning.

      @raises(EImageAssignmentError If it's not possible to convert from
        Source class to us. Not every possible convertion is implemented now.)
    }
    procedure Assign(const Source: TCastleImage); virtual;

    { Append code to embed this image inside Pascal source code. }
    procedure SaveToPascalCode(const ImageName: string;
      const ShowProgress: boolean;
      var CodeInterface, CodeImplementation, CodeInitialization, CodeFinalization: string);
  end;

  TCastleImageList = specialize TFPGObjectList<TCastleImage>;

  TEncodedImageList = specialize TFPGObjectList<TEncodedImage>;

  { Possible compression of textures for GPU. }
  TGPUCompression = (
    { S3TC DXT1 compression, for RGB images with no alpha or simple yes/no alpha.
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC.

      tcDxt1_RGB and tcDxt1_RGBA are the same compression method,
      except in tcDxt1_RGB the alpha information is ignored while rendering,
      while in tcDxt1_RGBA the rendering assumes we have simple yes/no alpha.

      The difference is equivalent to OpenGL differences in treating
      @unorderedList(
        @itemSpacing compact
        @item GL_COMPRESSED_RGB_S3TC_DXT1_EXT and
        @item GL_COMPRESSED_RGBA_S3TC_DXT1_EXT.
      )
    }
    tcDxt1_RGB,

    { S3TC DXT1 compression, @bold(for RGB images with no alpha or simple yes/no alpha).
      See above tcDxt1_RGB description for details. }
    tcDxt1_RGBA,

    { S3TC DXT3 compression, @bold(for RGBA images with full alpha channel),
      best for images with sharp alpha transitions.
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC. }
    tcDxt3,

    { S3TC DXT3 compression, @bold(for RGBA images with full alpha channel),
      best for images with smooth alpha transitions.
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC. }
    tcDxt5,

    { PVRTC texture compression format.
      Supported by some Android and iOS devices.
      See http://en.wikipedia.org/wiki/PVRTC .

      TODO: Add tcPvrtc2_4bpp to mark that alpha should be ignored?
      Compression format is the same, but rendering differs?
      Or not --- TextureProperties in VRML/X3D can override alpha
      channel detection, likewise TGLImage.Alpha can be overridden. }
    tcPvrtc1_4bpp_RGB,
    tcPvrtc1_2bpp_RGB,
    tcPvrtc1_4bpp_RGBA,
    tcPvrtc1_2bpp_RGBA,
    tcPvrtc2_4bpp,
    tcPvrtc2_2bpp,

    { ATI texture compression format, @bold(without alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGB,

    { ATI texture compression format, @bold(with sharp alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGBA_ExplicitAlpha,

    { ATI texture compression format, @bold(with smooth alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGBA_InterpolatedAlpha,

    { ETC texture compression, @bold(without alpha).
      See http://en.wikipedia.org/wiki/Ericsson_Texture_Compression . }
    tcETC1
  );
  TGPUCompressions = set of TGPUCompression;

  ECannotFlipCompressedImage = class(Exception);

  { Image compressed using one of the GPU texture compression algorithms. }
  TGPUCompressedImage = class(TEncodedImage)
  private
    FCompression: TGPUCompression;
    FSize: Cardinal;
  public
    constructor Create(const AWidth, AHeight, ADepth: Cardinal;
      const ACompression: TGPUCompression);

    property Compression: TGPUCompression read FCompression;

    { Size of the whole image data inside RawPixels, in bytes. }
    function Size: Cardinal; override;

    function HasAlpha: boolean; override;
    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    { Flip compressed image vertically, losslessly.

      This works only for (some) S3TC images.
      It uses the knowledge of how S3TC compression works
      to losslessly flip the image, without re-compressing it.
      The idea is described here
      [http://users.telenet.be/tfautre/softdev/ddsload/explanation.htm].

      @raises(ECannotFlipCompressedImage
        Raised when image Height is not 1, 2, 3
        or a multiple of 4 (since the trick doesn't work in these cases,
        pixels would move between 4x4 blocks). Note that if Height
        is a power of two (as common for OpenGL textures) then it's
        always possible to make a flip.) }
    procedure FlipVertical;

    { Decompress the image.

      This uses DecompressTexture variable, so you have to initialialize it
      first (for example to CastleGLImages.GLDecompressTexture) before using this.

      @raises(ECannotDecompressTexture If we cannot decompress the texture,
        because decompressor is not set or there was some other error
        within decompressor.) }
    function Decompress: TCastleImage;

    function MakeCopy: TGPUCompressedImage;
  end;

  { Deprecated alias for TGPUCompressedImage }
  TS3TCImage = TGPUCompressedImage deprecated;

  ECannotDecompressTexture = class(Exception);

  TDecompressTextureFunction = function (Image: TGPUCompressedImage): TCastleImage;

var
  { Assign here texture decompression function that is available.
    This way the "decompressor" is pluggable, which means that
    you can even use OpenGL to decompress textures, if you're going
    to load images while some OpenGL context is active. }
  DecompressTexture: TDecompressTextureFunction;

{ TCastleImageClass and arrays of TCastleImageClasses ----------------------------- }

type
  { }
  TCastleImageClass = class of TCastleImage;
  TEncodedImageClass = class of TEncodedImage;
  TDynArrayImageClasses = array of TCastleImageClass;

  { @deprecated Deprecated name for TCastleImageClass. }
  TImageClass = TCastleImageClass deprecated;

{ Check is ImageClass one of the items in the ImageClasses array,
  or a descendant of one of them. }
function InImageClasses(ImageClass: TCastleImageClass;
  const ImageClasses: array of TCastleImageClass): boolean; overload;

{ Check is Image class one of the items in the ImageClasses array,
  or a descendant of one of them.
  This is a shortcut for InImageClasses(Image.ClassType, ImageClasses). }
function InImageClasses(Image: TCastleImage;
  const ImageClasses: array of TCastleImageClass): boolean; overload;

(*Check if both arrays contain exactly the same classes in the same order.

  May be extended in the future to do better checks and return true
  also if both array contain the same classes but in different order,
  and one array may contain the same classes duplicated any times.
  So the intention is that you should treat both arrays as sets
  (i.e. order of elements is ignored).

  The problem is that this function should be lighting fast
  (as the main purpose of it is to use it in constructions like
  setting property values, e.g.

@longCode(#
  if ImageClassesArraysEqual(Value, SomeProperty) then
  begin
    SomeProperty := Value;
    { ... do some lengthy operations to update new value of SomeProperty ... }
  end;
#)
  ), and doing smarter checks may cost us a little time.

  So for now this function returns
  @unorderedList(
    @item @true if for sure both arrays contain the same classes and
    @item @false if @italic(possibly) they don't contain the same classes.
  ) *)
function ImageClassesEqual(const Ar1, Ar2: array of TCastleImageClass): boolean;

procedure ImageClassesAssign(var Variable: TDynArrayImageClasses;
  const NewValue: array of TCastleImageClass);

{ TCastleImage basic descendants --------------------------------------------- }

type
  TRGBAlphaImage = class;
  TRGBFloatImage = class;
  TGrayscaleImage = class;
  TGrayscaleAlphaImage = class;

  { Image with pixel represented as a TVector3Byte (red, green, blue). }
  TRGBImage = class(TCastleImage)
  private
    function GetRGBPixels: PVector3Byte;
    class function FromFpImage(const FPImage: TFPMemoryImage): TRGBImage;
    function ToFpImage: TFPMemoryImage; override;
  protected
    procedure DrawCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer); override;
  public
    { This is the same pointer as RawPixels, only typecasted to PVector3Byte }
    property RGBPixels: PVector3Byte read GetRGBPixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector3Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PArray_Vector3Byte;

    procedure InvertRGBColors; override;

    procedure SetColorRGB(const x, y: Integer; const v: TVector3Single); override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure TransformRGB(const Matrix: TMatrix3Single); override;
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); override;

    { Create a new TRGBAlphaImage instance with RGB contents copied from this
      image, and alpha fully opaque. }
    function ToRGBAlphaImage: TRGBAlphaImage;

    { Convert image to an TRGBFloatImage format.

      Although float format offers superior precision compared to 8bit RGB,
      there is a slight chance of some unnoticeable loss of information
      in such convertion, since floating-point values are involved
      in calculation.

      But generally this conversion is relatively safe (contrary to
      convertion float -> 8-bit RGB, which must be lossy).

      But still you should note that doing such convertion has little
      sense since float format is useful only when you have colors that can't
      be expressed as simple 8-bit RGB. But by using this convertion
      you initially fill float image with data that does not have
      precision beyond standard 0..255 discreet range for each RGB component... }
    function ToRGBFloat: TRGBFloatImage;

    function ToGrayscale: TGrayscaleImage;

    { Draw horizontal line. Must be y1 <= y2, else it is NOOP. }
    procedure HorizontalLine(const x1, x2, y: Integer;
      const Color: TVector3Byte);
    procedure HorizontalLine(const x1, x2, y: Integer;
      const Color: TCastleColor);

    { Draw vertical line. Must be x1 <= x2, else it is NOOP. }
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TVector3Byte);
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TCastleColor);

    { Create image by merging two images according to a (third) mask image.
      This is a very special constructor.
      It creates image with the same size as MapImage.
      It also resizes ReplaceWhiteImage, ReplaceBlackImage
      to the size of MapImage.

      Then it inits color of each pixel of our image with
      combined colors of two pixels on the same coordinates from
      ReplaceWhiteImage, ReplaceBlackImage, something like

@preformatted(
  Pixel[x, y] := ReplaceWhiteImage[x, y] * S +
                 ReplaceBlackImage[x, y] * (S-1);
)

      where S = average of red, gree, blue of color MapImage[x, y].

      This means that final image will look like ReplaceWhiteImage
      in the areas where MapImage is white, and it will look like
      ReplaceBlackImage in the areas where MapImage is black. }
    constructor CreateCombined(const MapImage: TRGBImage;
      var ReplaceWhiteImage, ReplaceBlackImage: TRGBImage);

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4Single; const Colors: TVector4Pointer); override;

    procedure Assign(const Source: TCastleImage); override;
  end;

  TRGBAlphaImage = class(TCastleImage)
  private
    FPremultipliedAlpha: boolean;
    function GetAlphaPixels: PVector4Byte;
    class function FromFpImage(const FPImage: TFPMemoryImage): TRGBAlphaImage;
    function ToFpImage: TFPMemoryImage; override;
  public
    { This is the same pointer as RawPixels, only typecasted to PVector4Byte }
    property AlphaPixels: PVector4Byte read GetAlphaPixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector4Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PArray_Vector4Byte;

    procedure InvertRGBColors; override;

    procedure SetColorRGB(const x, y: Integer; const v: TVector3Single); override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    { Set alpha channel on every pixel to the same given value. }
    procedure ClearAlpha(const Alpha: Byte);

    procedure TransformRGB(const Matrix: TMatrix3Single); override;
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); override;

    { Set alpha of every pixel to either AlphaOnColor
      (when color of pixel is equal to AlphaColor with Tolerance,
      see @link(EqualRGB)) or AlphaOnNoColor. }
    procedure AlphaDecide(const AlphaColor: TVector3Byte;
      Tolerance: Byte; AlphaOnColor: Byte; AlphaOnNoColor: Byte);

    { Copy RGB contents from one image, and alpha contents from the other.
      RGB channels are copied from the RGB image,
      alpha channel is copied from the Grayscale image. Given RGB and Grayscale
      images must have the same size, and this is the resulting
      size of this image after Compose call. }
    procedure Compose(RGB: TRGBImage; AGrayscale: TGrayscaleImage);

    function HasAlpha: boolean; override;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4Single; const Colors: TVector4Pointer); override;

    { Remove alpha channel. }
    function ToRGBImage: TRGBImage;

    { Flatten to grayscale. }
    function ToGrayscaleAlphaImage: TGrayscaleAlphaImage;

    { Flatten to grayscale and remove alpha channel. }
    function ToGrayscaleImage: TGrayscaleImage;

    { Premultiply the RGB channel with alpha, to make it faster
      to use this image as source for TCastleImage.DrawTo and
      TCastleImage.DrawFrom operations. Changes @link(PremultipliedAlpha)
      from @false to @true. Unless @link(PremultipliedAlpha) was
      already @true, in which case this method does nothing --- this way
      it is safe to call this many times, we will not repeat multiplying.

      @italic(The image with premultiplied alpha can only be used
      with a subset of image routines that actually support premultiplied alpha.)
      Right now, these are only TCastleImage.DrawTo and
      TCastleImage.DrawFrom. Image with PremultipliedAlpha can be used
      as a source for drawing, and the results will be the same as without
      premultiplying, but faster. }
    procedure PremultiplyAlpha;
    property PremultipliedAlpha: boolean read FPremultipliedAlpha;
  end;

  { Image with high-precision RGB colors encoded as 3 floats. }
  TRGBFloatImage = class(TCastleImage)
  private
    function GetRGBFloatPixels: PVector3Single;
  public
    { This is the same pointer as RawPixels, only typecasted to PVector3Single }
    property RGBFloatPixels: PVector3Single read GetRGBFloatPixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector3Single;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PArray_Vector3Single;

    procedure SetColorRGB(const x, y: Integer; const v: TVector3Single); override;

    procedure Clear(const Pixel: TVector3Single); reintroduce;
    function IsClear(const Pixel: TVector3Single): boolean; reintroduce;

    { Converts TRGBFloatImage to TRGBImage.
      Colors in pixels are simply rounded using @link(Vector3Byte).
      So such convertion not only kills the floating-point
      precision in float format but also clamps color components
      to 0..1. }
    function ToRGBImage: TRGBImage;

    { Every component (red, green, blue) of every pixel
      is multiplied by Scale. }
    procedure ScaleColors(const Scale: Single);

    { Every component (red, green, blue) or every pixel
      is changed to Power(Value, Exp).
      So e.g. Exp = 1/2.2 gives commonly used gamma correction. }
    procedure ExpColors(const Exp: Single);

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4Single; const Colors: TVector4Pointer); override;
  end;

  { Grayscale image. Color is a simple Byte value. }
  TGrayscaleImage = class(TCastleImage)
  private
    FTreatAsAlpha: boolean;
    function GetGrayscalePixels: PByte;
    class function FromFpImage(const FPImage: TFPMemoryImage): TGrayscaleImage;
  public
    { This is the same pointer as RawPixels, only typecasted to PByte }
    property GrayscalePixels: PByte read GetGrayscalePixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PByte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PByteArray;

    procedure Clear(const Pixel: Byte); reintroduce;
    function IsClear(const Pixel: Byte): boolean; reintroduce;

    { Every pixels value is halved (divided by 2).
      This is done by simple bitshift, so you can be sure that all
      components are < 2^7 after this. }
    procedure HalfColors;

    { Add an alpha channel.
      The newly created alpha channel will have constant opaque alpha,
      except in the special case of TGrayscaleImage.TreatAsAlpha = @true
      (where the contents will be copied to alpha, and intensity set to white). }
    function ToGrayscaleAlphaImage: TGrayscaleAlphaImage;

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4Single; const Colors: TVector4Pointer); override;

    { Should we treat grayscale image as pure alpha channel (without any color
      information) when using this as a texture.

      This property is meaningful only for a small subset of operations,
      right now: only when creating OpenGL texture from this image.
      If @true, then the grayscale pixel data will be loaded as alpha channel
      contents (GL_ALPHA texture for OpenGL,
      it modifies only the fragments alpha value,
      it doesn't have any "color" in the normal sense).
      It is also the only way for TGrayscaleImage to return AlphaChannel <> acNone. }
    property TreatAsAlpha: boolean
      read FTreatAsAlpha write FTreatAsAlpha;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    procedure Assign(const Source: TCastleImage); override;
  end;

  { Grayscale image with an alpha channel.
    Each pixel is two bytes: grayscale + alpha. }
  TGrayscaleAlphaImage = class(TCastleImage)
  private
    function GetGrayscaleAlphaPixels: PVector2Byte;
    class function FromFpImage(const FPImage: TFPMemoryImage): TGrayscaleAlphaImage;
  public
    { This is the same pointer as RawPixels, only typecasted to PVector2Byte }
    property GrayscaleAlphaPixels: PVector2Byte read GetGrayscaleAlphaPixels;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector2Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PArray_Vector2Byte;

    procedure Clear(const Pixel: TVector2Byte); reintroduce;
    function IsClear(const Pixel: TVector2Byte): boolean; reintroduce;

    function HasAlpha: boolean; override;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4Single; const Colors: TVector4Pointer); override;

    procedure Assign(const Source: TCastleImage); override;
  end;

  { @deprecated Deprecated name for TCastleImage. }
  TImage = TCastleImage deprecated;

{ RGBE <-> 3 Single color convertion --------------------------------- }

{ Encode RGB color as Red + Green + Blue + Exponent format.
  This allows you to encode high-precision colors in 4 bytes,
  see ifRGBE image format for pointers why this is useful.

  Each component of V (red, green, blue) must be from range
  [0, +infinity), not merely from [0, 1].
  That is, V must have only nonnegative values. }
function Vector3ToRGBE(const v: TVector3Single): TVector4Byte;

{ Decode Red + Green + Blue + Exponent back into RGB (3 floats). }
function VectorRGBETo3Single(const v: TVector4Byte): TVector3Single;

{ loading image (format-specific) ---------------------------------------

  Load image from Stream.

  They must honour AllowedImageClasses, just like
  LoadImage does. Except they don't have to care about returning all TCastleImage
  descendants: see @link(TImageFormatInfo.LoadedClasses). So higher-level
  LoadImage will use them and eventually convert their result.

  An appropriate descendant of EImageLoadError will be raised
  in case of error when reading from Stream or when Stream will not
  contain correct data. }

type
  { }
  EImageLoadError = class(Exception);
  EInvalidImageFormat = class(EImageLoadError);
  EInvalidBMP = class(EInvalidImageFormat);
  EInvalidPNG = class(EInvalidImageFormat);
  EInvalidPPM = class(EInvalidImageFormat);
  EInvalidIPL = class(EInvalidImageFormat);
  EInvalidRGBE = class(EInvalidImageFormat);

  { }
  EUnableToLoadImage = class(EImageLoadError);

function LoadPNG(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadBMP(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadGIF(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadTGA(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadSGI(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadTIFF(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadJP2(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadEXR(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadJPEG(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadXPM(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadPSD(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadPCX(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

{ Load PPM image.
  Loads only the first image in .ppm file. }
function LoadPPM(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

{ Load PNM image (PNM, PGM, PBM, PPM) through FpImage.
  Note that for PPM, for now it's more advised to use our LoadPPM. }
function LoadPNM(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

function LoadIPL(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

{ Load RGBE image.
  This low-level function can load to TRGBFloatImage (preserving image data)
  or to TRGBImage (loosing floating point precision of RGBE format). }
function LoadRGBE(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

{ Load DDS image file into a single 2D image. This simply returns the first
  image found in DDS file, which should be the main image.
  If you want to investigate other images in DDS, you have to use TDDSImage
  class. }
function LoadDDS(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;

{ saving image (format-specific) --------------------------------------------

  SaveXxx. Each file format may have specialized SaveXxx that allows
  you to give some parameters special for given format.

  Each format must also have procedure with two parameters
  (Img: TCastleImage; Stream: TStream), this will be used with
  ImageFormatsInfo[].
  This means that below we must use overloading instead of
  default parameters, since pointers to given procedures must be
  compatible with @link(TImageSaveFunc).

  SaveXxx should
    raise EImageSaveError.CreateFmt('Saving to XXX image class %s not possible', [Img.ClassName]);
  when Img doesn't have acceptable class.
  Also, list of handled image classes should be reflected in SavedClasses
  in ImageFormatsInfo[] for this format.
}

{ }
procedure SaveBMP(Img: TCastleImage; Stream: TStream);
procedure SavePNG(Img: TCastleImage; Stream: TStream);
{ }
procedure SaveJPEG(Img: TCastleImage; Stream: TStream);
{ }
procedure SavePPM(Img: TCastleImage; Stream: TStream; binary: boolean); overload;
procedure SavePPM(Img: TCastleImage; Stream: TStream); { binary = true } overload;
{ }
procedure SaveRGBE(Img: TCastleImage; Stream: TStream);

procedure SaveDDS(Img: TCastleImage; Stream: TStream);

{ File formats managing ----------------------------------------------------- }

type
  { }
  TImageFormat = (
    { We handle PNG file format fully, both reading and writing,
      through the libpng library.

      This format supports a full alpha channel.
      Besides PSD, this is the only format that allows full-range
      (partial transparency) alpha channel.

      Trying to read / write PNG file when libpng is not installed
      (through LoadImage, SaveImage, LoadPNG, SavePNG and others)
      will raise exception ELibPngNotAvailable. Note that the check
      for availability of libpng is done only once you try to load/save PNG file.
      You can perfectly compile and even run your programs without
      PNG installed, until you try to load/save PNG format. }
    ifPNG,

    { We handle uncompressed BMP images. }
    ifBMP,

    ifPPM,

    { Image formats below are supported by FPImage. }
    ifJPEG, ifGIF, ifTGA, ifXPM, ifPSD, ifPCX, ifPNM,

    { We handle fully DDS (DirectDraw Surface) image format.
      See also TDDSImage class in DDS unit,
      this exposes even more features of the DDS image format. }
    ifDDS,

    { High-dynamic range image format, originally used by Radiance.
      See e.g. the pfilt and ximage programs from the Radiance package
      for processing such images.

      The float color values are encoded smartly as 4 bytes:
      3 mantisas for RGB and 1 byte for an Exponent.
      This is the Greg Ward's RGBE color encoding described in the
      "Graphic Gems" (gem II.5). This allows high floating-point-like precision,
      and possibility to encode any value >= 0 (not necessarily <= 1),
      keeping the pixel only 4 bytes long.

      Encoding a color values with float precision is very useful.
      Otherwise, when synthesized / photographed images are
      very dark / very bright, simply encoding them in traditional fixed-point
      pixel format looses color precision. So potentially important but small
      differences are lost in fixed-point formats.
      And color values are clamped to [0..1] range.
      On the other hand, keeping colors as floats preserves
      everything, and allows to process images later.

      It's most useful and natural to load/save these files as TRGBFloatImage,
      this way you keep the floating-point precision inside memory.
      However, you can also load/convert such image format
      to normal 8-bits image formats (like TRGBImage),
      if you're Ok with losing some of the precision. }
    ifRGBE,

    ifIPL,

    { Image formats below are supported
      by converting them  "under the hood" with ImageMagick.
      This is available only if this unit is compiled with FPC
      (i.e. not with Delphi) on platforms where ExecuteProcess is
      implemented. And ImageMagick must be installed and available on $PATH. }
    ifTIFF, ifSGI, ifJP2, ifEXR
  );
  TImageFormats = set of TImageFormat;

  TImageLoadFunc = function (Stream: TStream;
    const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
  TImageSaveFunc = procedure (Img: TCastleImage; Stream: TStream);

  { Possible TCastleImage classes that can be returned by Load method
    of this file format. It's assumed that appropriate Load can return
    only these classes, and any of these classes,
    and can convert between them.

    If the LoadImage will be called allowing some TCastleImage descendants
    that can be returned by Load of this format,
    then LoadImage will pretty much just pass the call to Load
    for appropriate file format.
    The above is expected to be the most common and most efficient case.
    This way necessary conversion (e.g. adding alpha channel) can be
    done at the lowest level, right inside image format handler,
    which means that e.g. you can do it per-pixel, or by libpng transforms
    in case of PNG format.

    Only when it's not possible (if, and only if, none of the AllowedImageClasses
    specified in LoadImage call can be returned by Load of this format)
    then LoadImage will try more elaborate approach. This means that
    it will try using Load of this image format, followed by
    some convertions of the image afterwards. This is generally less
    efficient, as it means that temporary image will be created during
    loading.
  }
  TImageLoadHandledClasses = (
    lcRGB,
    lcRGB_RGBA,
    lcG_GA_RGB_RGBA,
    lcRGB_RGBFloat
  );

  { Possible TCastleImage classes supported by Save method of this file format. }
  TImageSaveHandledClasses = (
    scRGB,
    scG_GA_RGB_RGBA,
    scRGB_RGBFloat
  );

  { Index of TImageFormatInfo.MimeTypes array and
    type for TImageFormatInfo.MimeTypesCount.
    Implies that TImageFormatInfo.MimeTypes is indexed from 1,
    TImageFormatInfo.MimeTypesCount must be >= 1,
    so each file format must have at least one
    (treated as "default" in some cases) MIME type. }
  TImageFormatInfoMimeTypesCount = 1..6;

  { A type to index TImageFormatInfo.Exts array and also for TImageFormatInfo.ExtsCount.
    So TImageFormatInfo.Exts array is indexed from 1,
    and TImageFormatInfo.ExtsCount must be >= 1, so each file format must have at least one
    (treated as "default" in some cases) file extension. }
  TImageFormatInfoExtsCount = 1..3;

  TImageFormatInfo = record
    { Human-readable format name.

      Note that this is supposed to be shown to normal user,
      in save dialog boxes etc. So it should be short and concise. I used to
      have here long format names like @code(JFIF, JPEG File Interchange Format) or
      @code(PNG, Portable Network Graphic), but they are too ugly, and unnecessarily
      resolving format abbrevs. For example, most users probably used JPEG,
      but not many have to know, or understand, that actually this is image format JFIF;
      these are technical and historical details that are not needed for normal usage of image
      operations.

      Saying it directly, I want to keep this FormatName short and concise.
      This is not a place to educate users what some abbrev means.
      This is a place to "name" each file format in the most natural way, which
      usually means to only slightly rephrase typical file format extension.

      In practice, I now copy descriptions from English GIMP open dialog. }
    FormatName: string;

    MimeTypesCount: TImageFormatInfoMimeTypesCount;

    { MIME types recognized as this image file format.
      First MIME type is the default for this file format
      (some procedures make use of it). }
    MimeTypes: array [TImageFormatInfoMimeTypesCount] of string;

    ExtsCount: TImageFormatInfoExtsCount;

    { File extensions for this image type.
      First file extension is default, which is used for some routines.
      Must be lowercase.

      This is used e.g. to construct file filters in open/save dialogs.
      Together with MimeTypes it is also used by URIMimeType to map
      file extension into a MIME type. An extension matching one of Exts
      values implicates the default MIME type for this format (MimeTypes[1]).

      Note that to cooperate nicely with network URLs
      (when server may report MIME type) and data URIs, most of the code
      should operate using MIME types instead of file extensions.
      So usually you are more interested in MimeTypes than Exts. }
    Exts: array [TImageFormatInfoExtsCount] of string;

    { Load method for this file format.
      @nil if cannot be loaded. }
    Load: TImageLoadFunc;

    { If Load is assigned, this describes what TCastleImage descendants
      can be returned by this Load. LoadImage will need this information,
      to make necessary convertions to other TCastleImage classes,
      when possible. }
    LoadedClasses: TImageLoadHandledClasses;

    { Save method for this file format.
      @nil if cannot be saved. }
    Save: TImageSaveFunc;
    SavedClasses: TImageSaveHandledClasses;
  end;

const
  { Information about supported image formats. }
  ImageFormatInfos: array [TImageFormat] of TImageFormatInfo =
  ( { The order on this list matters --- it determines the order of filters
      for open/save dialogs.
      First list most adviced and well-known formats, starting from lossless. }

    { Portable Network Graphic } { }
    ( FormatName: 'PNG image';
      MimeTypesCount: 1;
      MimeTypes: ('image/png', '', '', '', '', '');
      ExtsCount: 1; Exts: ('png', '', '');
      Load: @LoadPNG; LoadedClasses: {$ifdef CASTLE_PNG_USING_FCL_IMAGE} lcG_GA_RGB_RGBA {$else} lcG_GA_RGB_RGBA {$endif};
      Save: @SavePNG; SavedClasses: {$ifdef CASTLE_PNG_USING_FCL_IMAGE} scRGB { actually scRGB_RGBA } {$else} scG_GA_RGB_RGBA {$endif}; ),
    ( FormatName: 'Windows BMP image';
      MimeTypesCount: 1;
      MimeTypes: ('image/bmp', '', '', '', '', '');
      ExtsCount: 1; Exts: ('bmp', '', '');
      Load: @LoadBMP; LoadedClasses: lcRGB_RGBA;
      Save: @SaveBMP; SavedClasses: scRGB),
    { Portable Pixel Map } { }
    ( FormatName: 'PPM image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-portable-pixmap', '', '', '', '', '');
      ExtsCount: 1; Exts: ('ppm', '', '');
      Load: @LoadPPM; LoadedClasses: lcRGB;
      Save: @SavePPM; SavedClasses: scRGB; ),
    { JFIF, JPEG File Interchange Format } { }
    ( FormatName: 'JPEG image';
      MimeTypesCount: 2;
      MimeTypes: ('image/jpeg', 'image/jpg', '', '', '', '');
      ExtsCount: 3; Exts: ('jpg', 'jpeg', 'jpe');
      Load: @LoadJPEG; LoadedClasses: lcRGB_RGBA;
      Save: @SaveJPEG; SavedClasses: scRGB { actually scRGB_RGBA }),
    { Graphics Interchange Format } { }
    ( FormatName: 'GIF image';
      MimeTypesCount: 1;
      MimeTypes: ('image/gif', '', '', '', '', '');
      ExtsCount: 1; Exts: ('gif', '', '');
      Load: @LoadGIF; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'TarGA image';
      MimeTypesCount: 2;
      MimeTypes: ('image/x-targa', 'image/x-tga', '', '', '', '');
      ExtsCount: 2; Exts: ('tga', 'tpic', '');
      Load: @LoadTGA; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'XPM image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-xpixmap', '', '', '', '', '');
      ExtsCount: 1; Exts: ('xpm', '', '');
      Load: @LoadXPM; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'PSD image';
      MimeTypesCount: 4;
      MimeTypes: ('image/photoshop', 'image/x-photoshop', 'image/psd', 'application/photoshop', '', '');
      ExtsCount: 1; Exts: ('psd', '', '');
      Load: @LoadPSD; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'ZSoft PCX image';
      MimeTypesCount: 5;
      MimeTypes: ('image/pcx', 'application/pcx', 'application/x-pcx', 'image/x-pc-paintbrush', 'image/x-pcx', '');
      ExtsCount: 1; Exts: ('pcx', '', '');
      Load: @LoadPCX; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'PNM image';
      MimeTypesCount: 6;
      MimeTypes: ('image/x-portable-anymap', 'image/x-portable-graymap', 'image/x-pgm', 'image/x-portable-bitmap', 'image/pbm', 'image/x-pbm');
      ExtsCount: 3; Exts: ('pnm', 'pgm', 'pbm');
      Load: @LoadPNM; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),

    { Direct Draw Surface } { }
    ( FormatName: 'DDS image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-dds', '', '', '', '', '');
      ExtsCount: 1; Exts: ('dds', '', '');
      Load: @LoadDDS; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: @SaveDDS; SavedClasses: scG_GA_RGB_RGBA; ),

    { Image formats not well known. }

    ( FormatName: 'RGBE (RGB+Exponent) image';
      MimeTypesCount: 1;
      MimeTypes: ('image/vnd.radiance', '', '', '', '', '');
      ExtsCount: 3; Exts: ('rgbe', 'pic', 'hdr');
      Load: @LoadRGBE; LoadedClasses: lcRGB_RGBFloat;
      Save: @SaveRGBE; SavedClasses: scRGB_RGBFloat; ),
    ( FormatName: 'IPLab image';
      MimeTypesCount: 1;
      { ipl MIME type invented by Kambi, to make it unique to communicate image format for LoadImage } { }
      MimeTypes: ('image/x-ipl', '', '', '', '', '');
      ExtsCount: 1; Exts: ('ipl', '', '');
      Load: @LoadIPL; LoadedClasses: lcRGB;
      Save: nil; SavedClasses: scRGB; ),

    { Image formats loaded using ImageMagick's convert.
      Placed at the end of the list, to be at the end of open/save dialogs
      filters, since there's a large chance they will not work,
      if user didn't install ImageMagick. } { }

    ( FormatName: 'TIFF image';
      MimeTypesCount: 1;
      MimeTypes: ('image/tiff', '', '', '', '', '');
      ExtsCount: 2; Exts: ('tiff', 'tif', '');
      Load: @LoadTIFF; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'SGI image';
      MimeTypesCount: 3;
      MimeTypes: ('image/sgi', 'image/x-sgi', 'image/x-sgi-rgba', '', '', '');
      ExtsCount: 1; Exts: ('sgi', '', '');
      Load: @LoadSGI; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'JPEG 2000 image';
      MimeTypesCount: 4;
      MimeTypes: ('image/jp2', 'image/jpeg2000', 'image/jpeg2000-image', 'image/x-jpeg2000-image', '', '');
      ExtsCount: 1; Exts: ('jp2', '', '');
      Load: @LoadJP2; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'EXR image';
      MimeTypesCount: 1;
      MimeTypes: ('image/x-exr', '', '', '', '', '');
      ExtsCount: 1; Exts: ('exr', '', '');
      Load: @LoadEXR; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; )
  );

{ Find image file format with given MIME type.
  Returns @false if no format matching given MIME type. }
function MimeTypeToImageFormat(const MimeType: string;
  const OnlyLoadable, OnlySaveable: boolean; out ImgFormat: TImageFormat): boolean;

{ List available image file formats.

  This is basically for debug/info purposes, you can show this to user
  to let him know which formats are supported (and by which extensions
  they are recognized). Although almost always a better way to show
  this to user is just to use SaveImage_FileFilters with a save dialog
  like TCastleWindowCustom.FileDialog,
  this shows file types in the open/save dialog,
  so it's most natural and convenient to user.

  ListImageExtsLong produces a multiline info (separated by NL, last line not terminated
  by NL), shows all extensions and FormatName for each file format.
  Each line starts with LinePrefix.

  ListImageExtsShort writes all recognized extensions separated by comma (', ').

  @groupBegin }
function ListImageExtsLong(OnlyLoadable, OnlySaveable: boolean; const LinePrefix: string): string;
function ListImageExtsShort(OnlyLoadable, OnlySaveable: boolean): string;
{ @groupEnd }

{ loading image -------------------------------------------------------------- }

type
  { }
  EImageFormatNotSupported = class(Exception);

{ TODO: zrobic LoadImageGuess ktore zgaduje format na podstawie
  zawartosci. }

(*The ultimate procedure to load an image from a file or URL.

  URL is downloaded using CastleDownload unit.
  As always, if you all you care about is loading normal files, then just pass
  a normal filename (absolute or relative to the current directory)
  as the URL parameter.

  Simple examples:

@longCode(#
  { When you don't care what TCastleImage descendant you get: }
  Image := LoadImage('image.png');

  { When you insist on getting TRGBImage, that is 8-bit color image
    without an alpha channel. }
  ImageRGB := LoadImage('image.png', [TRGBImage]) as TRGBImage;
#)

  Image file format may be given explicitly (overloaded version with
  Format parameter), or guessed based on URL extension
  (which can be given explicitly by TypeExt,
  or automatically calculated from full URL).
  For now, we cannot guess the file format based on file contents
  or MIME type (the latter case would be sensible for http URLs).

  AllowedImageClasses says what image classes are allowed.
  As a special case, AllowedImageClasses = [] is equivalent to
  AllowedImageClasses = [TCastleImage] which says that all TCastleImage descendants
  are allowed. Then this function will do everything it can to load
  any image into the best subclass of TCastleImage, losing as little image
  information it can.

  Example: consider you're loading a PNG file. Let's suppose you're
  loading it with AllowedImageClasses = []. Then you can get
  TGrayscaleImage, TGrayscaleAlphaImage, TRGBImage, TRGBAlphaImage,
  depending on whether PNG file is grayscale or not and has alpha or not.
  Now let's suppose you specified AllowedImageClasses = [TRGBImage].
  If PNG file will not be grayscale and not have alpha channel,
  LoadImage will return TRGBImage descendant, as before.
  But if PNG fill *will* have alpha channel then
  LoadImage will simply ignore (strip) alpha channel and return you TRGBImage.

  Similar thing for grayscale: if image file was grayscale but you requested
  only TRGBImage, then grayscale may be "expanded" into full three-channel
  RGB.

  There can also happen reverse situation: you e.g. insist that
  AllowedImageClasses = [TRGBAlphaImage] but given PNG image does not
  have alpha channel. In this case LoadImage may add "dummy" alpha channel
  (everywhere equal to 1.0 or High(Byte)).
  Similar thing when you e.g. gave AllowedImageClasses = [TRGBFloatImage]
  but you're loading from PNG image. In this case you want float precision,
  but image file cannot offer it. So LoadImage can simply convert
  discreet values to appropriating floating point values.

  If at any point LoadImage will find that it's unable to satisfy
  AllowedImageClasses, it will raise @link(EUnableToLoadImage).

  @raises(EUnableToLoadImage If Image cannot be loaded into
    allowed AllowedImageClasses.)

  @raises(EImageFormatNotSupported If image file format cannot be loaded at all.
    This can happen if format is totally unknown (not recognized
    MIME type, derived from file extension in case of local files)
    or if this image format cannot be loaded at all.)

  @groupBegin *)
function LoadImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TCastleImageClass)
  :TCastleImage; overload;
function LoadImage(Stream: TStream; const MimeType: string;
  const AllowedImageClasses: array of TCastleImageClass)
  :TCastleImage; overload;

function LoadImage(const URL: string): TCastleImage; overload;
function LoadImage(const URL: string;
  const AllowedImageClasses: array of TCastleImageClass)
  :TCastleImage; overload;
function LoadImage(const URL: string;
  const AllowedImageClasses: array of TCastleImageClass;
  const ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation = riNearest): TCastleImage; overload;
{ @groupEnd }

{ saving image --------------------------------------------------------------- }

type
  { }
  EImageSaveError = class(Exception);

{ Save image to a file. Takes URL as parameter, you can give @code(file) URL
  or just a normal filename.

  File format is determined by looking at URL (guessing MIME type using
  URIMimeType), or given explicitly as MimeType,
  or just given explicitly as Format parameter.

  Image class does @bold(not)
  affect the created image file format, on the assumption that the
  "memory format" of the image (what TCastleImage descendant is used)
  can be orthogonal to the actual "file format" used to save this file.

  Tries to write the image preserving it as closely as possible in this
  image format. When it's not possible, according conversions may be done:
  floating point precision of TRGBFloatImage may be lost (if saving
  to any file format besides RGBE file, although saving to OpenEXR may also
  preserve it once implemented), alpha channel may be lost,
  grayscale may be expanded and such.

  Although not absolutely all conversions are implemented for now.
  You can be sure that
  all image formats (that allow any saving at all) can be saved
  from TRGBImage. Also TRGBFloatImage can be saved to RGBE file.
  Also PNG format supports full collection (grayscale/rgb, alpha/no alpha
  are all perfectly possible in PNG file; and TRGBFloatImage will be just converted
  to 8-bit RGB before saving to PNG).

  @raises(EImageSaveError When it's not possible to save image,
    because of Img class (memory format) and/or image file format.)

  @groupBegin }
procedure SaveImage(const img: TCastleImage; const Format: TImageFormat; Stream: TStream); overload;
procedure SaveImage(const img: TCastleImage; const MimeType: string; Stream: TStream); overload;
procedure SaveImage(const Img: TCastleImage; const URL: string); overload;
{ @groupEnd }

{ Other TCastleImage processing ---------------------------------------------------- }

{ Choose TCastleImage descendant best matching for this image file format.
  The only purpose of this for now is to pick TRGBFloatImage for RGBE files,
  chooses TRGBImage for anything else.

  For the overloaded version with URL, file format is determined
  by guessing based on file extension.

  @groupBegin }
function ImageClassBestForSavingToFormat(const Format: TImageFormat): TCastleImageClass; overload;
function ImageClassBestForSavingToFormat(const URL: string): TCastleImageClass; overload;
{ @groupEnd }

var
  { File filters if you want to choose a file that can be loaded/saved
    by appropriate functions from Images unit.

    These objects should be treated as read-only outside this unit.
    Initialization / finalization of this unit automatically take care of them.

    @groupBegin }
  LoadImage_FileFilters: TFileFilterList;
  SaveImage_FileFilters: TFileFilterList;
  { @groupEnd }

{ Maximum alpha channel type. Chooses "full range" if anything is "full range",
  otherwise choose "simple yes/no" if anything is "simple yes/no",
  otherwise returns "no alpha channel". }
procedure AlphaMaxTo1st(var A: TAlphaChannel; const B: TAlphaChannel);

function StringToAlpha(S: string; var WarningDone: boolean): TAutoAlphaChannel;

const
  AlphaToString: array [TAutoAlphaChannel] of string =
  ('AUTO', 'NONE', 'SIMPLE_YES_NO', 'FULL_RANGE');

type
  TGPUCompressionInfo = object
    Name: string;
    RequiresPowerOf2: boolean;
    AlphaChannel: TAlphaChannel;
  end;

const
  GPUCompressionInfo: array [TGPUCompression] of TGPUCompressionInfo =
  ( (Name: 'DXT1 (ignore alpha)'; RequiresPowerOf2: true ; AlphaChannel: acNone),
    (Name: 'DXT1'               ; RequiresPowerOf2: true ; AlphaChannel: acSimpleYesNo),
    (Name: 'DXT3'               ; RequiresPowerOf2: true ; AlphaChannel: acFullRange),
    (Name: 'DXT5'               ; RequiresPowerOf2: true ; AlphaChannel: acFullRange),
    { See http://community.imgtec.com/files/pvrtc-texture-compression-user-guide/
      "PVRTC2 vs PVRTC1" section --- PVRTC1 require power-of-two. } { }
    (Name: 'PVRTC1_4bpp_RGB'    ; RequiresPowerOf2: true ; AlphaChannel: acNone),
    (Name: 'PVRTC1_2bpp_RGB'    ; RequiresPowerOf2: true ; AlphaChannel: acNone),
    (Name: 'PVRTC1_4bpp_RGBA'   ; RequiresPowerOf2: true ; AlphaChannel: acFullRange),
    (Name: 'PVRTC1_2bpp_RGBA'   ; RequiresPowerOf2: true ; AlphaChannel: acFullRange),
    (Name: 'PVRTC2_4bpp'        ; RequiresPowerOf2: false; AlphaChannel: acFullRange),
    (Name: 'PVRTC2_2bpp'        ; RequiresPowerOf2: false; AlphaChannel: acFullRange),
    { TODO: unconfirmed RequiresPowerOf2 values below, using safest for now. } { }
    (Name: 'ATITC_RGB'          ; RequiresPowerOf2: true ; AlphaChannel: acNone),
    (Name: 'ATITC_RGBA_ExplicitAlpha'    ; RequiresPowerOf2: true ; AlphaChannel: acFullRange),
    (Name: 'ATITC_RGBA_InterpolatedAlpha'; RequiresPowerOf2: true ; AlphaChannel: acFullRange),
    (Name: 'ETC1'               ; RequiresPowerOf2: true ; AlphaChannel: acNone)
  );

{$undef read_interface}

implementation

uses ExtInterpolation, FPCanvas, FPImgCanv,
  CastleProgress, CastleStringUtils, CastleFilesUtils, CastleWarnings,
  CastleDDS, CastleDownload, CastleURIUtils;

{ image loading utilities --------------------------------------------------- }

{ Helper methods for implemented LoadImage. }

function ClassAllowed(ImageClass: TCastleImageClass;
  const AllowedImageClasses: array of TCastleImageClass): boolean;
begin
  Result := (High(AllowedImageClasses) = -1) or
    InImageClasses(ImageClass, AllowedImageClasses);
end;

function LoadImageParams(
  const AllowedImageClasses: array of TCastleImageClass): string;

  function ImageClassesToStr(const AllowedImageClasses: array of TCastleImageClass): string;
  var
    I: Integer;
  begin
    if High(AllowedImageClasses) = -1 then
      Result := 'all' else
    begin
      Result := '';
      for I := 0 to High(AllowedImageClasses) do
      begin
        if Result <> '' then Result += ', ';
        Result += AllowedImageClasses[I].ClassName;
      end;
    end;
  end;

begin
  Result := 'required class [' + ImageClassesToStr(AllowedImageClasses) + ']';
end;

{ file format specific ------------------------------------------------------- }

{$I images_bmp.inc}
{$ifndef CASTLE_PNG_USING_FCL_IMAGE}
  {$I images_png.inc}
{$endif}
{$I images_fpimage.inc}
{$I images_ppm.inc}
{$I images_ipl.inc}
{$I images_rgbe_fileformat.inc}
{$I images_external_tool.inc}
{$I images_dds.inc}

{ Colors ------------------------------------------------------------------ }

function EqualRGB(const Color1, Color2: TVector3Byte; Tolerance: Byte): boolean;
begin
 result:=(Abs(Smallint(Color1[0])-Color2[0]) <= tolerance) and
         (Abs(Smallint(Color1[1])-Color2[1]) <= tolerance) and
         (Abs(Smallint(Color1[2])-Color2[2]) <= tolerance);
end;

{ TEncodedImage -------------------------------------------------------------- }

destructor TEncodedImage.Destroy;
begin
  FreeMemNiling(FRawPixels);
  inherited;
end;

function TEncodedImage.IsEmpty: boolean;
begin
 Result := RawPixels = nil;
end;

function TEncodedImage.HasAlpha: boolean;
begin
  Result := false;
end;

function TEncodedImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
begin
  Result := acNone;
end;

function TEncodedImage.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, Width, Height);
end;

{ TCastleImage --------------------------------------------------------------- }

constructor TCastleImage.Create;
begin
  inherited;
  { Everything is already inited to nil and 0. }
end;

constructor TCastleImage.Create(
  const AWidth, AHeight: Cardinal;
  const ADepth: Cardinal = 1);
begin
  Create;
  SetSize(AWidth, AHeight, ADepth);
end;

procedure TCastleImage.Empty;
begin
  FreeMemNiling(FRawPixels);
  FWidth := 0;
  FHeight := 0;
  FDepth := 0;
end;

procedure TCastleImage.SetSize(const AWidth, AHeight: Cardinal;
  const ADepth: Cardinal = 1);
begin
  if (FWidth <> AWidth) or
     (FHeight <> AHeight) or
     (FDepth <> ADepth) then
  begin
    FreeMemNiling(FRawPixels);
    FWidth := AWidth;
    FHeight := AHeight;
    FDepth := ADepth;
    if (AWidth <> 0) and (AHeight <> 0) and (ADepth <> 0) then
      FRawPixels := GetMem(PixelSize * AWidth * AHeight * ADepth);
  end;
end;

procedure TCastleImage.SetSize(const Source: TCastleImage);
begin
  SetSize(Source.Width, Source.Height, Source.Depth);
end;

function TCastleImage.PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): Pointer;
begin
  Result := PointerAdd(RawPixels, PixelSize * (Width * (Height * Z + Y) + X));
end;

function TCastleImage.RowPtr(const Y: Cardinal; const Z: Cardinal = 0): Pointer;
begin
  Result := PointerAdd(RawPixels, PixelSize * (Width * (Height * Z + Y)));
end;

procedure TCastleImage.NotImplemented(const AMethodName: string);
begin
  raise EInternalError.Create(AMethodName +
    ' method not implemented for this TCastleImage descendant');
end;

procedure TCastleImage.InvertRGBColors;
begin
  NotImplemented('InvertRGBColors');
end;

procedure TCastleImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
  NotImplemented('SetColorRGB');
end;

function TCastleImage.Size: Cardinal;
begin
  Result := Width * Height * Depth * PixelSize;
end;

function TCastleImage.ImageSize: Cardinal;
begin
  Result := Size;
end;

function TCastleImage.MakeCopy: TCastleImage;
begin
  Result := TCastleImageClass(Self.ClassType).Create(Width, Height, Depth);
  Move(RawPixels^, Result.RawPixels^, Size);
  Result.FURL := URL;
end;

type
  TMixColorsFunction = procedure (const OutputColor: Pointer;
    const Weights: TVector4Single; const Colors: TVector4Pointer) of object;

{ This does the real resizing work.
  It assumes that SourceData and DestinData pointers are already allocated.
  DestinWidth, DestinHeight must not be 0. }
procedure InternalResize(PixelSize: Cardinal;
  const SourceData: Pointer; const SourceRect: TRectangle; const SourceWidth, SourceHeight: Cardinal;
  const DestinData: Pointer; const DestinRect: TRectangle; const DestinWidth, DestinHeight: Cardinal;
  const Interpolation: TResizeInterpolation;
  const MixColors: TMixColorsFunction;
  const ProgressTitle: string);
var
  DestinY: Integer;

  procedure MakeLineNearest;
  { write row DestinY of DestinData }
  var
    DestinX, SourceX, SourceY: Integer;
    SourceRow, DestinRow: PtrUInt;
  begin
    SourceY := SourceRect.ClampY(DestinY * SourceHeight div DestinHeight);
    SourceRow := PtrUInt(SourceData) + SourceWidth * SourceY * PixelSize;
    DestinRow := PtrUInt(DestinData) + DestinWidth * DestinY * PixelSize;

    for DestinX := DestinRect.Left to DestinRect.Right - 1 do
    begin
      SourceX := SourceRect.ClampX(DestinX * SourceWidth div DestinWidth);
      Move(Pointer(PtrUInt(SourceRow + SourceX * PixelSize))^,
           Pointer(PtrUInt(DestinRow + DestinX * PixelSize))^,
           PixelSize);
    end;
  end;

  procedure MakeLineBilinear;
  var
    { For every destination pixel, we consider 4 neighbor source pixels.
      - SourceX1 / SourceX2 are smaller / larger X coordinates in source.
      - SourceY1 / SourceY2 are smaller / larger Y coordinates in source.
      - SourceXFrac / SourceYFrac are fractional parts (in [0..1])
        that say how close our perfect point (from which we should take
        destination color) is to 4 neighbor pixels. }
    DestinX, SourceX1, SourceX2, SourceY1, SourceY2: Integer;
    Source1Row, Source2Row, DestinRow: PtrUInt;
    SourceXFrac, SourceYFrac: Single;
    Weights: TVector4Single;
    Colors: TVector4Pointer;
  begin
    SourceYFrac := DestinY * SourceHeight / DestinHeight;
    SourceY1 := Max(Trunc(SourceYFrac), SourceRect.Bottom);
    SourceY2 := Min(SourceY1 + 1, SourceRect.Top - 1);
    SourceYFrac := Frac(SourceYFrac);
    Source1Row := PtrUInt(SourceData) + SourceWidth * SourceY1 * PixelSize;
    Source2Row := PtrUInt(SourceData) + SourceWidth * SourceY2 * PixelSize;
    DestinRow  := PtrUInt(DestinData) + DestinWidth * DestinY  * PixelSize;

    for DestinX := DestinRect.Left to DestinRect.Right - 1 do
    begin
      SourceXFrac := DestinX * SourceWidth / DestinWidth;

      SourceX1 := Trunc(SourceXFrac);
      if SourceX1 < SourceRect.Left then
        SourceX1 := SourceRect.Left;

      SourceX2 := SourceX1 + 1;
      if SourceX2 >= SourceRect.Right then
        SourceX2 := SourceRect.Right - 1;

      SourceX1 *= PixelSize;
      SourceX2 *= PixelSize;

      SourceXFrac := Frac(SourceXFrac);
      Weights[0] := SourceXFrac * SourceYFrac;
      Colors[0] := Pointer(PtrUInt(Source2Row + SourceX2));
      Weights[1] := (1 - SourceXFrac) * SourceYFrac;
      Colors[1] := Pointer(PtrUInt(Source2Row + SourceX1));
      Weights[2] := (1 - SourceXFrac) * (1 - SourceYFrac);
      Colors[2] := Pointer(PtrUInt(Source1Row + SourceX1));
      Weights[3] :=  SourceXFrac * (1 - SourceYFrac);
      Colors[3] := Pointer(PtrUInt(Source1Row + SourceX2));
      MixColors(Pointer(PtrUInt(DestinRow + DestinX * PixelSize)), Weights, Colors);
    end;
  end;

type
  TMakeLineFunction = procedure is nested;
var
  MakeLine: TMakeLineFunction;
begin
  case Interpolation of
    riNearest : MakeLine := @MakeLineNearest;
    riBilinear: MakeLine := @MakeLineBilinear;
    else raise EInternalError.Create('Unknown Interpolation for InternalResize');
  end;

  if ProgressTitle = '' then
  begin
    for DestinY := DestinRect.Bottom to DestinRect.Top - 1 do
      MakeLine;
  end else
  begin
    Progress.Init(DestinHeight, ProgressTitle);
    try
      for DestinY := DestinRect.Bottom to DestinRect.Top - 1 do
      begin
        MakeLine;
        Progress.Step;
      end;
    finally Progress.Fini end;
  end;
end;

procedure TCastleImage.Resize(ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation;
  const ProgressTitle: string);
var
  NewPixels: Pointer;
begin
  if ((ResizeWidth <> 0) and (ResizeWidth <> Width)) or
     ((ResizeHeight <> 0) and (ResizeHeight <> Height)) then
  begin
    { Make both ResizeTo* non-zero. }
    if ResizeWidth = 0 then ResizeWidth := Width;
    if ResizeHeight = 0 then ResizeHeight := Height;

    NewPixels := GetMem(ResizeWidth * ResizeHeight * PixelSize);
    InternalResize(PixelSize,
      RawPixels, Rect, Width, Height,
      NewPixels, Rectangle(0, 0, ResizeWidth, ResizeHeight), ResizeWidth, ResizeHeight,
      Interpolation, @MixColors, ProgressTitle);
    FreeMemNiling(FRawPixels);

    FRawPixels := NewPixels;
    FWidth := ResizeWidth;
    FHeight := ResizeHeight;
  end;
end;

function TCastleImage.MakeResized(ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation;
  const ProgressTitle: string): TCastleImage;
begin
  { Make both ResizeTo* non-zero. }
  if ResizeWidth = 0 then ResizeWidth := Width;
  if ResizeHeight = 0 then ResizeHeight := Height;

  Result := TCastleImageClass(ClassType).Create(ResizeWidth, ResizeHeight);
  try
    Result.FURL := URL;
    if not IsEmpty then
      InternalResize(PixelSize,
               RawPixels,        Rect,        Width,        Height,
        Result.RawPixels, Result.Rect, Result.Width, Result.Height,
        Interpolation, @MixColors, ProgressTitle);
  except Result.Free; raise end;
end;

procedure TCastleImage.Resize3x3(const ResizeWidth, ResizeHeight: Cardinal;
  var Corners: TVector4Integer;
  const Interpolation: TResizeInterpolation);
var
  NewPixels: Pointer;
  NewCorners: TVector4Integer;
  { Position that delimit parts along X or Y, for source and destination images. }
  SourceXs, SourceYs, DestXs, DestYs: TVector4Integer;
type
  TPart = 0..2;

  procedure ResizePart(const X, Y: TPart);
  var
    SourceRect, DestRect: TRectangle;
  begin
    SourceRect := Rectangle(SourceXs[X], SourceYs[Y],
      SourceXs[Integer(X) + 1] - SourceXs[X],
      SourceYs[Integer(Y) + 1] - SourceYs[Y]);
    DestRect := Rectangle(DestXs[X], DestYs[Y],
      DestXs[Integer(X) + 1] - DestXs[X],
      DestYs[Integer(Y) + 1] - DestYs[Y]);
    InternalResize(PixelSize,
      RawPixels, SourceRect, Width, Height,
      NewPixels, DestRect, ResizeWidth, ResizeHeight,
      Interpolation, @MixColors, '');
  end;

var
  X, Y: TPart;
begin
  if (ResizeWidth <> Width) or (ResizeHeight <> Height) then
  begin
    NewCorners[0] := Corners[0] * ResizeWidth div Width;
    NewCorners[1] := Corners[1] * ResizeHeight div Height;
    NewCorners[2] := Corners[2] * ResizeWidth div Width;
    NewCorners[3] := Corners[3] * ResizeHeight div Height;

    if not ( (Corners[3] + Corners[1] < Width) and
             (Corners[2] + Corners[0] < Height) and
             (NewCorners[3] + NewCorners[1] < ResizeWidth) and
             (NewCorners[2] + NewCorners[0] < ResizeHeight) ) then
      raise Exception.CreateFmt('TCastleImage.Resize3x3: Cannot resize image with corners because corners are larger then image size. Source corners: %s, source size: %dx%d, destination corners: %s, destination size: %dx%d',
        [VectorToNiceStr(Corners), Width, Height,
         VectorToNiceStr(NewCorners), ResizeWidth, ResizeHeight]);

    SourceXs[0] := 0;
    SourceXs[1] := Corners[3];
    SourceXs[2] := Width - Corners[1];
    SourceXs[3] := Width;

    SourceYs[0] := 0;
    SourceYs[1] := Corners[2];
    SourceYs[2] := Height - Corners[0];
    SourceYs[3] := Height;

    DestXs[0] := 0;
    DestXs[1] := NewCorners[3];
    DestXs[2] := ResizeWidth - NewCorners[1];
    DestXs[3] := ResizeWidth;

    DestYs[0] := 0;
    DestYs[1] := NewCorners[2];
    DestYs[2] := ResizeHeight - NewCorners[0];
    DestYs[3] := ResizeHeight;

    NewPixels := GetMem(ResizeWidth * ResizeHeight * PixelSize);
    for X in TPart do
      for Y in TPart do
        ResizePart(X, Y);
    FreeMemNiling(FRawPixels);

    FRawPixels := NewPixels;
    FWidth := ResizeWidth;
    FHeight := ResizeHeight;
    Corners := NewCorners;
  end;
end;

function TCastleImage.MakeRotated(Angle: Integer): TCastleImage;

  procedure Rotate90;
  var
    X, Y: Integer;
  begin
    Result := TCastleImageClass(ClassType).Create(Height, Width);
    for X := 0 to Width - 1 do
      for Y := 0 to Height - 1 do
        Move(PixelPtr(X, Y)^, Result.PixelPtr(Y, Width - 1 - X)^, PixelSize);
  end;

  procedure Rotate180;
  var
    X, Y: Integer;
  begin
    Result := TCastleImageClass(ClassType).Create(Width, Height);
    for X := 0 to Width - 1 do
      for Y := 0 to Height - 1 do
        Move(PixelPtr(X, Y)^, Result.PixelPtr(Width - 1 - X, Height - 1 - Y)^, PixelSize);
  end;

  procedure Rotate270;
  var
    X, Y: Integer;
  begin
    Result := TCastleImageClass(ClassType).Create(Height, Width);
    for X := 0 to Width - 1 do
      for Y := 0 to Height - 1 do
        Move(PixelPtr(X, Y)^, Result.PixelPtr(Height - 1 - Y, X)^, PixelSize);
  end;

begin
  { convert Angle to 0..3 range }
  Angle := Angle mod 4;
  if Angle < 0 then Angle += 4;

  case Angle of
    1: Rotate90;
    2: Rotate180;
    3: Rotate270;
    { else Angle = 0, nothing to do }
  end;
end;

procedure TCastleImage.Rotate(const Angle: Integer);
var
  New: TCastleImage;
begin
  New := MakeRotated(Angle);
  try
    Assign(New);
  finally FreeAndNil(New) end;
end;

procedure TCastleImage.FlipHorizontal;
var
  ImageRow, TmpPixel, Pix1, Pix2: Pointer;
  x, y: Integer;
begin
  TmpPixel := GetMem(PixelSize);
  try
    for Y := 0 to Height-1 do
    begin
      ImageRow := RowPtr(y);
      for x := 0 to (Width-1) div 2 do
      begin
        Pix1 := PointerAdd(ImageRow, Cardinal(x) * PixelSize);
        Pix2 := PointerAdd(ImageRow, (Width-1-Cardinal(x)) * PixelSize);
        Move(Pix1^, TmpPixel^, PixelSize);
        Move(Pix2^, Pix1^, PixelSize);
        Move(TmpPixel^, Pix2^, PixelSize);
      end;
    end;
  finally FreeMem(TmpPixel) end;
end;

procedure TCastleImage.FlipVertical;
var
  TmpRow, Row1, Row2: Pointer;
  Y, RowSize: Integer;
begin
  RowSize := PixelSize * Width;
  TmpRow := GetMem(RowSize);
  try
    for Y := 0 to Height div 2 - 1 do
    begin
      Row1 := RowPtr(Y);
      Row2 := RowPtr(Height - Y - 1);
      Move(Row1^, TmpRow^, RowSize);
      Move(Row2^, Row1^, RowSize);
      Move(TmpRow^, Row2^, RowSize);
    end;
  finally FreeMem(TmpRow) end;
end;

function TCastleImage.MakeTiled(TileX, TileY: Cardinal): TCastleImage;
var
  i, j: Cardinal;
begin
  Result := TCastleImageClass(ClassType).Create(TileX * Width, TileY * Height);
  try
    { Correct but naive version:

    for i := 0 to result.Width-1 do
     for j := 0 to result.Height-1 do
      move(Image.PixelPtr(i mod Image.Width, j mod Image.Height)^,
           Result.PixelPtr( i, j)^,
           Result.PixelSize );

    This can be speeded up copying whole rows at once: }

    for i := 0 to TileX - 1 do
      for j := 0 to Result.Height - 1 do
        Move(PixelPtr(0, j mod Height)^,
             Result.PixelPtr(i * Width, j)^,
             PixelSize * Width );
  except Result.Free; raise end;
end;

function TCastleImage.MakeExtracted(X0, Y0, ExtractWidth, ExtractHeight: Cardinal): TCastleImage;
var
  y: Cardinal;
begin
  if x0 + ExtractWidth > Width then
    raise EImagePosOutOfRange.Create('x0 in MakeExtracted out of range');
  if y0 + ExtractHeight > Height then
    raise EImagePosOutOfRange.Create('y0 in MakeExtracted out of range');

  Result := TCastleImageClass(ClassType).Create(ExtractWidth, ExtractHeight);
  try
    for Y := 0 to ExtractHeight - 1 do
      Move(PixelPtr(x0, y + y0)^, Result.RowPtr(y)^, PixelSize * ExtractWidth);
  except Result.Free; raise end;
end;

procedure TCastleImage.Clear(const Pixel: TVector4Byte);
begin
  NotImplemented('Clear');
end;

procedure TCastleImage.Clear(const Pixel: TCastleColor);
begin
  Clear(Vector4Byte(Pixel));
end;

function TCastleImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  NotImplemented('IsClear');
  { code will never get here (NotImplemented always raises an exception),
    and code "Result := false;" below is only to avoid compiler warning
    that function result is undefined. }
  Result := false;
end;

procedure TCastleImage.TransformRGB(const Matrix: TMatrix3Single);
begin
  NotImplemented('TransformRGB');
end;

procedure TCastleImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
begin
  NotImplemented('ModulateRGB');
end;

function TCastleImage.MakeModulatedRGB(
  const ColorModulator: TColorModulatorByteFunc): TCastleImage;
begin
  Result := MakeCopy;
  Result.ModulateRGB(ColorModulator);
end;

procedure TCastleImage.Grayscale;
begin
  ModulateRGB(@ColorGrayscaleByte);
end;

procedure TCastleImage.ConvertToChannelRGB(Channel: Integer);
begin
  case Channel of
    0: ModulateRGB(@ColorRedConvertByte);
    1: ModulateRGB(@ColorGreenConvertByte);
    2: ModulateRGB(@ColorBlueConvertByte);
    else raise EInternalError.Create(
      'ConvertToChannelRGB: Channel must be 0, 1 or 2');
  end;
end;

procedure TCastleImage.StripToChannelRGB(Channel: Integer);
begin
  case Channel of
    0: ModulateRGB(@ColorRedStripByte);
    1: ModulateRGB(@ColorGreenStripByte);
    2: ModulateRGB(@ColorBlueStripByte);
    else raise EInternalError.Create(
      'StripToChannelRGB: Channel must be 0, 1 or 2');
  end;
end;

function TCastleImage.IsEqual(Image: TCastleImage): boolean;
begin
  Result :=
    (Image.ClassType = ClassType) and
    (Image.Width = Width) and
    (Image.Height = Height) and
    (Image.Depth = Depth) and
    (CompareMem(Image.RawPixels, RawPixels, Size));
end;

function TCastleImage.ArePartsEqual(
  const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
  Image: TCastleImage;
  const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean;
var
  Y: Integer;
  SelfPtr: Pointer;
  ImagePtr: Pointer;
  SelfRowByteWidth, ImageRowByteWidth, RowByteWidth: Cardinal;
begin
  Result :=
    (Image.ClassType = ClassType) and
    (SelfWidth = ImageWidth) and
    (SelfHeight = ImageHeight);
  if Result then
  begin
    SelfPtr := PixelPtr(SelfX0, SelfY0);
    ImagePtr := Image.PixelPtr(ImageX0, ImageY0);
    RowByteWidth := ImageWidth * PixelSize;
    SelfRowByteWidth := Self.Width * PixelSize;
    ImageRowByteWidth := Image.Width * Image.PixelSize;
    for Y := 0 to Integer(ImageHeight) - 1 do
    begin
      if not CompareMem(SelfPtr, ImagePtr, RowByteWidth) then
      begin
        Result := false;
        Exit;
      end;
      PtrUInt(SelfPtr) := PtrUInt(SelfPtr) + SelfRowByteWidth;
      PtrUInt(ImagePtr) := PtrUInt(ImagePtr) + ImageRowByteWidth;
    end;
  end;
end;

function TCastleImage.ArePartsEqual(
  Image: TCastleImage;
  const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean;
begin
  Result := ArePartsEqual(
    0, 0, Width, Height,
    Image,
    ImageX0, ImageY0, ImageWidth, ImageHeight);
end;

function TCastleImage.ArePartsEqual(
  const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
  Image: TCastleImage): boolean;
begin
  Result := ArePartsEqual(
    SelfX0, SelfY0, SelfWidth, SelfHeight,
    Image,
    0, 0, Image.Width, Image.Height);
end;

procedure TCastleImage.DrawCore(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer);
var
  Line: Integer;
  Ptr, SourcePtr: Pointer;
  RowWidth, SourceRowWidth, SourceCopyRowWidth: Cardinal;
begin
  if Source.ClassType <> ClassType then
    raise Exception.CreateFmt('Cannot draw pixels from image class %s to %s',
      [Source.ClassName, ClassName]);

  Ptr := PixelPtr(X, Y);
  RowWidth := Width * PixelSize;

  SourcePtr := Source.PixelPtr(SourceX, SourceY);
  SourceRowWidth := Source.Width * Source.PixelSize;
  SourceCopyRowWidth := SourceWidth * Source.PixelSize;

  for Line := 0 to Integer(SourceHeight) - 1 do
  begin
    Move(SourcePtr^, Ptr^, SourceCopyRowWidth);
    PtrUInt(Ptr) := PtrUInt(Ptr) + RowWidth;
    PtrUInt(SourcePtr) := PtrUInt(SourcePtr) + SourceRowWidth;
  end;
end;

procedure TCastleImage.DrawFrom(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer);
begin
  if X < 0 then
  begin
    SourceX += -X;
    SourceWidth -= -X;
    X := 0;
  end;

  if Y < 0 then
  begin
    SourceY += -Y;
    SourceHeight -= -Y;
    Y := 0;
  end;

  if SourceX < 0 then
  begin
    X += -SourceX;
    SourceWidth -= -SourceX;
    SourceX := 0;
  end;

  if SourceY < 0 then
  begin
    Y += -SourceY;
    SourceHeight -= -SourceY;
    SourceY := 0;
  end;

  SourceWidth  := Min(SourceWidth , Width  - 1 - X, Source.Width );
  SourceHeight := Min(SourceHeight, Height - 1 - Y, Source.Height);

  if (SourceWidth > 0) and
     (SourceHeight > 0) and
     (SourceX < Source.Width) and
     (SourceY < Source.Height) then
    DrawCore(Source, X, Y, SourceX, SourceY, SourceWidth, SourceHeight);
end;

procedure TCastleImage.DrawFrom(Source: TCastleImage; const X, Y: Integer);
begin
  DrawFrom(Source, X, Y, 0, 0, Source.Width, Source.Height);
end;

procedure TCastleImage.DrawTo(Destination: TCastleImage; const X, Y: Integer);
begin
  Destination.DrawFrom(Self, X, Y);
end;

procedure TCastleImage.LerpSimpleCheckConditions(SecondImage: TCastleImage);
begin
  if (Width <> SecondImage.Width) or
     (Height <> SecondImage.Height) then
    raise EImageLerpDifferentSizes.CreateFmt('Linear interpolation not possible, images have different sizes: first has %d x %d, second has %d x %d',
      [Width, Height, SecondImage.Width, SecondImage.Height]);

  if not (SecondImage is Self.ClassType) then
    raise EImageLerpInvalidClasses.CreateFmt('Linear interpolation between %s and %s class not possible',
      [ClassName, SecondImage.ClassName]);
end;

procedure TCastleImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
begin
  raise EImageLerpInvalidClasses.Create('Linear interpolation (TCastleImage.LerpWith) not possible with the base TCastleImage class');
end;

class procedure TCastleImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4Single; const Colors: TVector4Pointer);
begin
  raise EImageLerpInvalidClasses.Create('Mixing colors (TCastleImage.MixColors) not possible with the base TCastleImage class');
end;

procedure TCastleImage.Assign(const Source: TCastleImage);
begin
  if Source.ClassType = ClassType then
  begin
    SetSize(Source);
    // if Source.RawPixels = nil, then we're already freed by SetSize above
    if Source.RawPixels <> nil then
      Move(Source.RawPixels^, RawPixels^, Size);
    URL := Source.URL;
  end else
    raise EImageAssignmentError.CreateFmt('Cannot copy image contents from %s to %s',
      [Source.ClassName, ClassName]);
end;

procedure TCastleImage.SaveToPascalCode(const ImageName: string;
  const ShowProgress: boolean;
  var CodeInterface, CodeImplementation, CodeInitialization, CodeFinalization: string);
var
  NameWidth, NameHeight, NameDepth, NamePixels: string;
  pb: PByte;
  I: Integer;
begin
  { calculate Name* variables }
  NameWidth := ImageName + 'Width';
  NameHeight := ImageName + 'Height';
  NameDepth := ImageName + 'Depth';
  NamePixels := ImageName + 'Pixels';

  CodeInterface +=
    'var' +nl+
    '  ' +ImageName+ ': ' +ClassName+ ';' +nl + nl;

  CodeImplementation +=
    'const' +nl+
    '  ' +NameWidth+ ' = ' +IntToStr(Width)+ ';' +nl+
    '  ' +NameHeight+ ' = ' +IntToStr(Height)+ ';' +nl+
    '  ' +NameDepth+ ' = ' +IntToStr(Depth)+ ';' +nl+
    '  ' +NamePixels+ ': array[0 .. '
      +NameWidth+ ' * '
      +NameHeight+ ' * '
      +NameDepth+ ' * '
      +IntToStr(PixelSize) + ' - 1] of Byte = (' +nl+
    '    ';

  if ShowProgress then
    Progress.Init((Size - 1) div 12,
      Format('Generating %s (%s, alpha: %s)',
        [ImageName, ClassName, AlphaToString[AlphaChannel]]));

  pb := PByte(RawPixels);
  for I := 1 to Size - 1 do
  begin
    CodeImplementation += Format('%4d,', [pb^]);
    if (i mod 12) = 0 then
    begin
      CodeImplementation += nl + '    ';
      if ShowProgress then Progress.Step;
    end else
      CodeImplementation += ' ';
    Inc(pb);
  end;
  CodeImplementation += Format('%4d);', [pb^]) + nl + nl;

  if ShowProgress then Progress.Fini;

  CodeInitialization +=
    '  ' +ImageName+ ' := ' +ClassName+ '.Create(' +NameWidth+', ' +NameHeight+ ', ' +NameDepth+ ');' +nl+
    '  Move(' +NamePixels+ ', ' +ImageName+ '.RawPixels^, SizeOf(' +NamePixels+ '));' +nl+
    '  ' +ImageName+ '.URL := ''embedded-image:/' +ImageName+ ''';' + nl;

  CodeFinalization +=
    '  FreeAndNil(' +ImageName+ ');' +nl;
end;

{ TGPUCompressedImage ----------------------------------------------------------------- }

constructor TGPUCompressedImage.Create(
  const AWidth, AHeight, ADepth: Cardinal;
  const ACompression: TGPUCompression);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FDepth := ADepth;
  FCompression := ACompression;

  case Compression of
    { Size formula for S3TC textures:
      All DXT* compression methods compress 4x4 pixels into some constant size.
      When Width / Height is not divisible by 4, we have to round up.

      This matches what MSDN docs say about DDS with mipmaps:
      http://msdn.microsoft.com/en-us/library/bb205578(VS.85).aspx
      When mipmaps are used, DDS Width/Height must be power-of-two,
      so the base level is usually divisible by 4. But on the following mipmap
      levels the size decreases, eventually to 1x1, so this still matters.
      And MSDN says then explicitly that with DXT1, you have always
      minimum 8 bytes, and with DXT2-5 minimum 16 bytes.

      This also means that we cannot simply calculate size of mipmap in a DDS
      by looking at size of base image, and dividing by 2/4/8 as mipmap size
      decreases. We have to calculate size always rounding up to 4x4 block of pixels.
    }
    tcDxt1_RGB, tcDxt1_RGBA:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8 { 8 bytes for each 16 pixels };
    tcDxt3, tcDxt5:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 16 { 16 bytes for each 16 pixels };

    { see https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc2.txt
      for size formula size.
      Note that minimum size is 32 bytes,
      see https://developer.apple.com/library/ios/qa/qa1611/_index.html }
    tcPvrtc1_2bpp_RGB, tcPvrtc1_2bpp_RGBA:
      FSize := Max(32, (FDepth * Max(FWidth, 16) * Max(FHeight, 8) * 2 + 7) div 8);
    tcPvrtc1_4bpp_RGB, tcPvrtc1_4bpp_RGBA:
      FSize := Max(32, (FDepth * Max(FWidth,  8) * Max(FHeight, 8) * 4 + 7) div 8);

    { see https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc2.txt
      for size formula source }
    tcPvrtc2_2bpp:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 4) * 8;
    tcPvrtc2_4bpp:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8;

    { see https://www.khronos.org/registry/gles/extensions/AMD/AMD_compressed_ATC_texture.txt
      for size formula source }
    tcATITC_RGB:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8;
    tcATITC_RGBA_ExplicitAlpha,
    tcATITC_RGBA_InterpolatedAlpha:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 16;

    { size formula from
      http://en.wikipedia.org/wiki/Ericsson_Texture_Compression
      "ETC1 takes 4x4 groups of pixel data and compresses each into a single 64-bit word" }
    tcETC1:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8;

    else raise EInvalidDDS.CreateFmt('Cannot calculate size for texture compressed with %s',
      [GPUCompressionInfo[Compression].Name]);
  end;

  FRawPixels := GetMem(FSize);
end;

function TGPUCompressedImage.Size: Cardinal;
begin
  Result := FSize;
end;

function TGPUCompressedImage.HasAlpha: boolean;
begin
  Result := GPUCompressionInfo[Compression].AlphaChannel <> acNone;
end;

function TGPUCompressedImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
begin
  { Compressed data doesn't analyze alpha channel, instead
    we determine alpha channel from the compression type. }
  Result := GPUCompressionInfo[Compression].AlphaChannel;
end;

{$I images_s3tc_flip_vertical.inc}

function TGPUCompressedImage.Decompress: TCastleImage;
begin
  if Assigned(DecompressTexture) then
    Result := DecompressTexture(Self) else
    raise ECannotDecompressTexture.Create('Cannot decompress GPU-compressed texture: no decompressor initialized');
end;

function TGPUCompressedImage.MakeCopy: TGPUCompressedImage;
begin
  Result := TGPUCompressedImage.Create(Width, Height, Depth, Compression);
  Assert(Result.Size = Size);
  Move(RawPixels^, Result.RawPixels^, Size);
  Result.URL := URL;
end;

{ TCastleImageClass and arrays of TCastleImageClasses ----------------------------- }

function InImageClasses(ImageClass: TCastleImageClass;
  const ImageClasses: array of TCastleImageClass): boolean;
var
  i: Integer;
begin
  for i := 0 to High(ImageClasses) do
    if ImageClass.InheritsFrom(ImageClasses[i]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function InImageClasses(Image: TCastleImage;
  const ImageClasses: array of TCastleImageClass): boolean;
begin
  Result := InImageClasses(TCastleImageClass(Image.ClassType), ImageClasses);
end;

function ImageClassesEqual(const Ar1, Ar2: array of TCastleImageClass): boolean;
var
  i: Integer;
begin
  if High(Ar1) <> High(Ar2) then
  begin
    Result := false;
    Exit;
  end;

  for i := 0 to High(Ar1) do
    if Ar1[I] <> Ar2[I] then
    begin
      Result := false;
      Exit;
    end;

  Result := true;
end;

procedure ImageClassesAssign(var Variable: TDynArrayImageClasses;
  const NewValue: array of TCastleImageClass);
var
  i: Integer;
begin
  SetLength(Variable, High(NewValue) + 1);
  for i := 0 to High(NewValue) do
    Variable[i] := NewValue[i];
end;

{ TRGBImage ------------------------------------------------------------ }

constructor TRGBImage.CreateCombined(const MapImage: TRGBImage;
  var ReplaceWhiteImage, ReplaceBlackImage: TRGBImage);
var
  Map, White, Black, Res: PVector3Byte;
  s: single;
  i: integer;
begin
  Create(MapImage.Width, MapImage.Height);

  ReplaceWhiteImage.Resize(MapImage.Width, MapImage.Height);
  ReplaceBlackImage.Resize(MapImage.Width, MapImage.Height);

  Map := MapImage.RGBPixels;
  White := ReplaceWhiteImage.RGBPixels;
  Black := ReplaceBlackImage.RGBPixels;
  Res := RGBPixels;

  for i := 1 to Width * Height * Depth do
  begin
    s := (Map^[0] + Map^[1] + Map^[2]) / 255 / 3;
    Res^[0] := Round(s * White^[0] + (1-s) * Black^[0]);
    Res^[1] := Round(s * White^[1] + (1-s) * Black^[1]);
    Res^[2] := Round(s * White^[2] + (1-s) * Black^[2]);
    Inc(Map);
    Inc(White);
    Inc(Black);
    Inc(Res);
  end;
end;

function TRGBImage.GetRGBPixels: PVector3Byte;
begin
  Result := PVector3Byte(RawPixels);
end;

class function TRGBImage.PixelSize: Cardinal;
begin
  Result := 3;
end;

class function TRGBImage.ColorComponentsCount: Cardinal;
begin
  Result := 3;
end;

function TRGBImage.PixelPtr(const X, Y, Z: Cardinal): PVector3Byte;
begin
  Result := PVector3Byte(inherited PixelPtr(X, Y, Z));
end;

function TRGBImage.RowPtr(const Y, Z: Cardinal): PArray_Vector3Byte;
begin
  Result := PArray_Vector3Byte(inherited RowPtr(Y, Z));
end;

procedure TRGBImage.InvertRGBColors;
var
  i: Cardinal;
  prgb: PVector3byte;
begin
  prgb := RGBPixels;
  for i := 1 to Width * Height * Depth do
  begin
    prgb^[0] := High(byte)-prgb^[0];
    prgb^[1] := High(byte)-prgb^[1];
    prgb^[2] := High(byte)-prgb^[2];
    Inc(prgb);
  end;
end;

procedure TRGBImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
  PVector3Byte(PixelPtr(x, y))^ := Vector3Byte(v);
end;

procedure TRGBImage.Clear(const Pixel: TVector4Byte);
var
  P: PVector3Byte;
  I: Cardinal;
begin
  P := RGBPixels;
  for I := 1 to Width * Height * Depth do
  begin
    Move(Pixel, P^, SizeOf(TVector3Byte));
    Inc(P);
  end;
end;

function TRGBImage.IsClear(const Pixel: TVector4Byte): boolean;
var
  P: PVector3Byte;
  I: Cardinal;
begin
  P := RGBPixels;
  for I := 1 to Width * Height * Depth do
  begin
    if not CompareMem(@Pixel, P, SizeOf(TVector3Byte)) then
    begin
      Result := false;
      Exit;
    end;
    Inc(P);
  end;
  Result := true;
end;

procedure TRGBImage.TransformRGB(const Matrix: TMatrix3Single);
type PPixel = PVector3Byte;
{$I images_transformrgb_implement.inc}

procedure TRGBImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector3Byte;
{$I images_modulatergb_implement.inc}

procedure TRGBImage.DrawCore(Source: TCastleImage;
  X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer);
var
  PSource: PVector4Byte;
  PDest: PVector3Byte;
  DestX, DestY: Integer;
  SourceAlpha: TRGBAlphaImage;
  W: Word;
begin
  if Source is TRGBAlphaImage then
  begin
    SourceAlpha := TRGBAlphaImage(Source);
    if SourceAlpha.PremultipliedAlpha then
    begin
      for DestY := Y to Y + SourceHeight - 1 do
      begin
        PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
        PDest := PixelPtr(X, DestY);
        for DestX := X to X + SourceWidth - 1 do
        begin
          W := PDest^[0] + Word(PSource^[0]); if W > 255 then W := 255;
          PDest^[0] := W;
          W := PDest^[1] + Word(PSource^[1]); if W > 255 then W := 255;
          PDest^[1] := W;
          W := PDest^[2] + Word(PSource^[2]); if W > 255 then W := 255;
          PDest^[2] := W;
          Inc(PSource);
          Inc(PDest);
        end;
      end;
    end else
    begin
      for DestY := Y to Y + SourceHeight - 1 do
      begin
        PSource := Source.PixelPtr(SourceX, SourceY + DestY - Y);
        PDest := PixelPtr(X, DestY);
        for DestX := X to X + SourceWidth - 1 do
        begin
          PDest^[0] := Clamped(PDest^[0] + Round(PSource^[0] * PSource^[3] / 255), 0, 255);
          PDest^[1] := Clamped(PDest^[1] + Round(PSource^[1] * PSource^[3] / 255), 0, 255);
          PDest^[2] := Clamped(PDest^[2] + Round(PSource^[2] * PSource^[3] / 255), 0, 255);
          Inc(PSource);
          Inc(PDest);
        end;
      end;
    end;
  end else
    inherited;
end;

function TRGBImage.ToRGBAlphaImage: TRGBAlphaImage;
var
  pi: PVector3Byte;
  pa: PVector4Byte;
  i: Cardinal;
begin
  Result := TRGBAlphaImage.Create(Width, Height, Depth);
  pi := RGBPixels;
  pa := Result.AlphaPixels;
  for i := 1 to Width * Height * Depth do
  begin
    Move(pi^, pa^, SizeOf(TVector3Byte));
    pa^[3] := High(Byte);
    Inc(pi);
    Inc(pa);
  end;
end;

function TRGBImage.ToRGBFloat: TRGBFloatImage;
var
  PFloat: PVector3Single;
  PByte: PVector3Byte;
  i: Cardinal;
begin
  result := TRGBFloatImage.Create(Width, Height, Depth);
  try
    PByte := RGBPixels;
    PFloat := Result.RGBFloatPixels;
    for i := 1 to Width * Height * Depth do
    begin
      PFloat^ := Vector3Single(PByte^);
      Inc(PByte);
      Inc(PFloat);
    end;
  except Result.Free; raise end;
end;

function TRGBImage.ToGrayscale: TGrayscaleImage;
var
  pRGB: PVector3Byte;
  pGrayscale: PByte;
  I: Cardinal;
begin
  Result := TGrayscaleImage.Create(Width, Height, Depth);
  try
    pRGB := RGBPixels;
    pGrayscale := Result.GrayscalePixels;
    for i := 1 to Width * Height * Depth do
    begin
      pGrayscale^ := GrayscaleValue(pRGB^);
      Inc(pRGB);
      Inc(pGrayscale);
    end;
  except Result.Free; raise end;
end;

procedure TRGBImage.HorizontalLine(const x1, x2, y: Integer;
  const Color: TVector3Byte);
var
  P: PVector3Byte;
  i: Integer;
begin
  P := PixelPtr(x1, y);
  for i := 0 to x2 - x1 do begin P^ := Color; Inc(P) end;
end;

procedure TRGBImage.HorizontalLine(const X1, X2, Y: Integer;
  const Color: TCastleColor);
begin
  HorizontalLine(X1, X2, Y, Vector3Byte(Vector3SingleCut(Color)));
end;

procedure TRGBImage.VerticalLine(const x, y1, y2: Integer;
  const Color: TVector3Byte);
var P: PVector3Byte;
    i: Integer;
begin
 P := PixelPtr(x, y1);
 for i := 0 to y2 - y1 do
 begin
  P^ := Color;
  P := PointerAdd(P, SizeOf(TVector3Byte) * Width);
 end;
end;

procedure TRGBImage.VerticalLine(const x, y1, y2: Integer;
  const Color: TCastleColor);
begin
  VerticalLine(X, Y1, Y2, Vector3Byte(Vector3SingleCut(Color)));
end;

procedure TRGBImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector3Byte;
  SecondPtr: PVector3Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := RGBPixels;
  SecondPtr := TRGBImage(SecondImage).RGBPixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

{ $define FAST_UNSAFE_MIX_COLORS}

class procedure TRGBImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4Single; const Colors: TVector4Pointer);
var
  OutputCol: PVector3Byte absolute OutputColor;
  Cols: array [0..3] of PVector3Byte absolute Colors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^[0] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[0] + Weights[1] * Cols[1]^[0] + Weights[2] * Cols[2]^[0] + Weights[3] * Cols[3]^[0]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^[1] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[1] + Weights[1] * Cols[1]^[1] + Weights[2] * Cols[2]^[1] + Weights[3] * Cols[3]^[1]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^[2] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[2] + Weights[1] * Cols[1]^[2] + Weights[2] * Cols[2]^[2] + Weights[3] * Cols[3]^[2]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

procedure TRGBImage.Assign(const Source: TCastleImage);
var
  FloatPtr: PVector3Single;
  RgbaPtr: PVector4Byte;
  SelfPtr: PVector3Byte;
  I: Cardinal;
begin
  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    SelfPtr := RGBPixels;
    RgbaPtr := TRGBAlphaImage(Source).AlphaPixels;
    for I := 1 to Width * Height * Depth do
    begin
      Move(RgbaPtr^, SelfPtr^, SizeOf(TVector3Byte));
      Inc(SelfPtr);
      Inc(RgbaPtr);
    end;
    URL := Source.URL;
  end else

  if Source is TRGBFloatImage then
  begin
    SetSize(Source);
    SelfPtr := RGBPixels;
    FloatPtr := TRGBFloatImage(Source).RGBFloatPixels;
    for I := 1 to Width * Height * Depth do
    begin
      SelfPtr^ := Vector3Byte(FloatPtr^);
      Inc(SelfPtr);
      Inc(FloatPtr);
    end;
    URL := Source.URL;
  end else

    inherited;
end;

{ TRGBAlphaImage ------------------------------------------------------------ }

function TRGBAlphaImage.GetAlphaPixels: PVector4Byte;
begin
  Result := PVector4Byte(RawPixels);
end;

class function TRGBAlphaImage.PixelSize: Cardinal;
begin
  Result := 4;
end;

class function TRGBAlphaImage.ColorComponentsCount: Cardinal;
begin
  Result := 4;
end;

function TRGBAlphaImage.PixelPtr(const X, Y, Z: Cardinal): PVector4Byte;
begin
  Result := PVector4Byte(inherited PixelPtr(X, Y, Z));
end;

function TRGBAlphaImage.RowPtr(const Y, Z: Cardinal): PArray_Vector4Byte;
begin
  Result := PArray_Vector4Byte(inherited RowPtr(Y, Z));
end;

procedure TRGBAlphaImage.InvertRGBColors;
var
  i: Cardinal;
  palpha: PVector4byte;
begin
  palpha := AlphaPixels;
  for i := 1 to Width * Height * Depth do
  begin
    palpha^[0] := High(byte)-palpha^[0];
    palpha^[1] := High(byte)-palpha^[1];
    palpha^[2] := High(byte)-palpha^[2];
    Inc(palpha);
  end;
end;

procedure TRGBAlphaImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
  PVector3Byte(PixelPtr(x, y))^ := Vector3Byte(v);
end;

procedure TRGBAlphaImage.Clear(const Pixel: TVector4Byte);
begin
  FillDWord(RawPixels^, Width*Height, LongWord(Pixel));
end;

procedure TRGBAlphaImage.ClearAlpha(const Alpha: Byte);
var
  i: Cardinal;
  palpha: PVector4byte;
begin
  palpha := AlphaPixels;
  for i := 1 to Width * Height * Depth do
  begin
    palpha^[3] := Alpha;
    Inc(palpha);
  end;
end;

function TRGBAlphaImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsMemDWordFilled(RawPixels^, Width*Height, LongWord(Pixel));
end;

procedure TRGBAlphaImage.TransformRGB(const Matrix: TMatrix3Single);
type PPixel = PVector4Byte;
{$I images_transformrgb_implement.inc}

procedure TRGBAlphaImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector4Byte;
{$I images_modulatergb_implement.inc}

procedure TRGBAlphaImage.AlphaDecide(const AlphaColor: TVector3Byte;
  Tolerance: Byte; AlphaOnColor: Byte; AlphaOnNoColor: Byte);
var
  pa: PVector4Byte;
  i: Cardinal;
begin
  pa := AlphaPixels;
  for i := 1 to Width * Height * Depth do
  begin
    if EqualRGB(AlphaColor, PVector3Byte(pa)^, Tolerance) then
      pa^[3] := AlphaOnColor else
      pa^[3] := AlphaOnNoColor;
    Inc(pa);
  end;
end;

procedure TRGBAlphaImage.Compose(RGB: TRGBImage; AGrayscale: TGrayscaleImage);
var
  PtrAlpha: PVector4Byte;
  PtrRGB: PVector3Byte;
  PtrGrayscale: PByte;
  I: Cardinal;
begin
  Check( (RGB.Width = AGrayscale.Width) and
         (RGB.Height = AGrayscale.Height) and
         (RGB.Depth = AGrayscale.Depth),
    'For TRGBAlphaImage.Compose, RGB and alpha images must have the same sizes');

  SetSize(RGB);

  PtrAlpha := AlphaPixels;
  PtrRGB := RGB.RGBPixels;
  PtrGrayscale := AGrayscale.GrayscalePixels;

  for I := 1 to Width * Height * Depth do
  begin
    System.Move(PtrRGB^, PtrAlpha^, SizeOf(TVector3Byte));
    PtrAlpha^[3] := PtrGrayscale^;

    Inc(PtrAlpha);
    Inc(PtrRGB);
    Inc(PtrGrayscale);
  end;
end;

function TRGBAlphaImage.HasAlpha: boolean;
begin
  Result := true;
end;

function TRGBAlphaImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
var
  PtrAlpha: PVector4Byte;
  I: Cardinal;
begin
  PtrAlpha := AlphaPixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^[3] > AlphaTolerance) and
       (PtrAlpha^[3] < 255 - AlphaTolerance) then
      Exit(acFullRange);
    Inc(PtrAlpha);
  end;

  Result := acSimpleYesNo;
end;

procedure TRGBAlphaImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector4Byte;
  SecondPtr: PVector4Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := AlphaPixels;
  SecondPtr := TRGBAlphaImage(SecondImage).AlphaPixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TRGBAlphaImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4Single; const Colors: TVector4Pointer);
var
  OutputCol: PVector4Byte absolute OutputColor;
  Cols: array [0..3] of PVector4Byte absolute Colors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^[0] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[0] + Weights[1] * Cols[1]^[0] + Weights[2] * Cols[2]^[0] + Weights[3] * Cols[3]^[0]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^[1] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[1] + Weights[1] * Cols[1]^[1] + Weights[2] * Cols[2]^[1] + Weights[3] * Cols[3]^[1]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^[2] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[2] + Weights[1] * Cols[1]^[2] + Weights[2] * Cols[2]^[2] + Weights[3] * Cols[3]^[2]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^[3] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[3] + Weights[1] * Cols[1]^[3] + Weights[2] * Cols[2]^[3] + Weights[3] * Cols[3]^[3]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

function TRGBAlphaImage.ToRGBImage: TRGBImage;
begin
  Result := TRGBImage.Create(0, 0);
  Result.Assign(Self);
end;

function TRGBAlphaImage.ToGrayscaleImage: TGrayscaleImage;
begin
  Result := TGrayscaleImage.Create(0, 0);
  Result.Assign(Self);
end;

function TRGBAlphaImage.ToGrayscaleAlphaImage: TGrayscaleAlphaImage;
begin
  Result := TGrayscaleAlphaImage.Create(0, 0);
  Result.Assign(Self);
end;

procedure TRGBAlphaImage.PremultiplyAlpha;
var
  P: PVector4Byte;
  I: Integer;
begin
  if not FPremultipliedAlpha then
  begin
    FPremultipliedAlpha := true;
    P := AlphaPixels;
    for I := 1 to Width * Height * Depth do
    begin
      P^[0] := Clamped(Round(P^[0] * P^[3] / 255), 0, 255);
      P^[1] := Clamped(Round(P^[1] * P^[3] / 255), 0, 255);
      P^[2] := Clamped(Round(P^[2] * P^[3] / 255), 0, 255);
      Inc(P);
    end;
  end;
end;

{ TRGBFloatImage ------------------------------------------------------------ }

function TRGBFloatImage.GetRGBFloatPixels: PVector3Single;
begin
  Result := PVector3Single(RawPixels);
end;

class function TRGBFloatImage.PixelSize: Cardinal;
begin
  Result := SizeOf(TVector3Single);
end;

class function TRGBFloatImage.ColorComponentsCount: Cardinal;
begin
  Result := 3;
end;

function TRGBFloatImage.PixelPtr(const X, Y, Z: Cardinal): PVector3Single;
begin
  Result := PVector3Single(inherited PixelPtr(X, Y, Z));
end;

function TRGBFloatImage.RowPtr(const Y, Z: Cardinal): PArray_Vector3Single;
begin
  Result := PArray_Vector3Single(inherited RowPtr(Y, Z));
end;

procedure TRGBFloatImage.SetColorRGB(const x, y: Integer; const V: TVector3Single);
begin
  PVector3Single(PixelPtr(x, y))^ := V;
end;

procedure TRGBFloatImage.Clear(const Pixel: TVector3Single);
var
  P: PVector3Single;
  I: Cardinal;
begin
  P := RGBFloatPixels;
  for I := 1 to Width * Height * Depth do
  begin
    Move(Pixel, P^, SizeOf(TVector3Single));
    Inc(P);
  end;
end;

function TRGBFloatImage.IsClear(const Pixel: TVector3Single): boolean;
var
  P: PVector3Single;
  I: Cardinal;
begin
  P := RGBFloatPixels;
  for I := 1 to Width * Height * Depth do
  begin
    if not CompareMem(@Pixel, P, SizeOf(TVector3Single)) then
    begin
      Result := false;
      Exit;
    end;
    Inc(P);
  end;
  Result := true;
end;

function TRGBFloatImage.ToRGBImage: TRGBImage;
begin
  Result := TRGBImage.Create(0, 0);
  Result.Assign(Self);
end;

procedure TRGBFloatImage.ScaleColors(const Scale: Single);
var
  pFloat: PVector3Single;
  i: Cardinal;
begin
  PFloat := RGBFloatPixels;
  for i := 1 to Width * Height * Depth do
  begin
    PFloat^ := VectorScale(PFloat^, Scale);
    Inc(PFloat);
  end;
end;

procedure TRGBFloatImage.ExpColors(const Exp: Single);
var
  pFloat: PVector3Single;
  i: Cardinal;
begin
  PFloat := RGBFloatPixels;
  for i := 1 to Width * Height * Depth do
  begin
    PFloat^ := VectorPowerComponents(PFloat^, Exp);
    Inc(PFloat);
  end;
end;

procedure TRGBFloatImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector3Single;
  SecondPtr: PVector3Single;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := RGBFloatPixels;
  SecondPtr := TRGBFloatImage(SecondImage).RGBFloatPixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TRGBFloatImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4Single; const Colors: TVector4Pointer);
var
  OutputCol: PVector3Single absolute OutputColor;
  Cols: array [0..3] of PVector3Single absolute Colors;
begin
  OutputCol^[0] := Weights[0] * Cols[0]^[0] + Weights[1] * Cols[1]^[0] + Weights[2] * Cols[2]^[0] + Weights[3] * Cols[3]^[0];
  OutputCol^[1] := Weights[0] * Cols[0]^[1] + Weights[1] * Cols[1]^[1] + Weights[2] * Cols[2]^[1] + Weights[3] * Cols[3]^[1];
  OutputCol^[2] := Weights[0] * Cols[0]^[2] + Weights[1] * Cols[1]^[2] + Weights[2] * Cols[2]^[2] + Weights[3] * Cols[3]^[2];
end;

{ TGrayscaleImage ------------------------------------------------------------ }

function TGrayscaleImage.GetGrayscalePixels: PByte;
begin
  Result := PByte(RawPixels);
end;

class function TGrayscaleImage.PixelSize: Cardinal;
begin
  Result := 1;
end;

class function TGrayscaleImage.ColorComponentsCount: Cardinal;
begin
  Result := 1;
end;

function TGrayscaleImage.PixelPtr(const X, Y, Z: Cardinal): PByte;
begin
  Result := PByte(inherited PixelPtr(X, Y, Z));
end;

function TGrayscaleImage.RowPtr(const Y, Z: Cardinal): PByteArray;
begin
  Result := PByteArray(inherited RowPtr(Y, Z));
end;

procedure TGrayscaleImage.Clear(const Pixel: Byte);
begin
  FillChar(RawPixels^, Size, Pixel);
end;

function TGrayscaleImage.IsClear(const Pixel: Byte): boolean;
begin
  Result := IsMemCharFilled(RawPixels^, Size, Char(Pixel));
end;

procedure TGrayscaleImage.HalfColors;
var
  P: PByte;
  I: Cardinal;
begin
  P := GrayscalePixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^ := P^ shr 1;
    Inc(P);
  end;
end;

procedure TGrayscaleImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PByte;
  SecondPtr: PByte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := GrayscalePixels;
  SecondPtr := TGrayscaleImage(SecondImage).GrayscalePixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Clamped(Round(Lerp(Value, SelfPtr^, SecondPtr^)), 0, High(Byte));
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TGrayscaleImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4Single; const Colors: TVector4Pointer);
var
  OutputCol: PByte absolute OutputColor;
  Cols: array [0..3] of PByte absolute Colors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^ := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^ + Weights[1] * Cols[1]^ + Weights[2] * Cols[2]^ + Weights[3] * Cols[3]^) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

function TGrayscaleImage.ToGrayscaleAlphaImage: TGrayscaleAlphaImage;
var
  pg: PByte;
  pa: PVector2Byte;
  I: Cardinal;
begin
  Result := TGrayscaleAlphaImage.Create(Width, Height, Depth);
  pg := GrayscalePixels;
  pa := Result.GrayscaleAlphaPixels;

  if TreatAsAlpha then
  begin
    for i := 1 to Width * Height * Depth do
    begin
      pa^[0] := High(Byte);
      pa^[1] := pg^;
      Inc(pg);
      Inc(pa);
    end;
  end else
  begin
    for i := 1 to Width * Height * Depth do
    begin
      pa^[0] := pg^;
      pa^[1] := High(Byte);
      Inc(pg);
      Inc(pa);
    end;
  end;
end;

function TGrayscaleImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
var
  PtrAlpha: PByte;
  I: Cardinal;
begin
  if not TreatAsAlpha then
    Exit(inherited AlphaChannel(AlphaTolerance));

  PtrAlpha := GrayscalePixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^ > AlphaTolerance) and
       (PtrAlpha^ < 255 - AlphaTolerance) then
      Exit(acFullRange);
    Inc(PtrAlpha);
  end;

  Result := acSimpleYesNo;
end;

procedure TGrayscaleImage.Assign(const Source: TCastleImage);
var
  RgbaPtr: PVector4Byte;
  RgbPtr: PVector3Byte absolute RgbaPtr;
  SelfPtr: PByte;
  I: Cardinal;
begin
  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    SelfPtr := GrayscalePixels;
    RgbaPtr := TRGBAlphaImage(Source).AlphaPixels;
    for I := 1 to Width * Height * Depth do
    begin
      SelfPtr^ := GrayscaleValue(RgbPtr^);
      Inc(SelfPtr);
      Inc(RgbaPtr);
    end;
    URL := Source.URL;
  end else

    inherited;
end;

{ TGrayscaleAlphaImage ------------------------------------------------------------ }

function TGrayscaleAlphaImage.GetGrayscaleAlphaPixels: PVector2Byte;
begin
  Result := PVector2Byte(RawPixels);
end;

class function TGrayscaleAlphaImage.PixelSize: Cardinal;
begin
  Result := 2;
end;

class function TGrayscaleAlphaImage.ColorComponentsCount: Cardinal;
begin
  Result := 2;
end;

function TGrayscaleAlphaImage.PixelPtr(const X, Y, Z: Cardinal): PVector2Byte;
begin
  Result := PVector2Byte(inherited PixelPtr(X, Y, Z));
end;

function TGrayscaleAlphaImage.RowPtr(const Y, Z: Cardinal): PArray_Vector2Byte;
begin
  Result := PArray_Vector2Byte(inherited RowPtr(Y, Z));
end;

procedure TGrayscaleAlphaImage.Clear(const Pixel: TVector2Byte);
var
  P: PVector2Byte;
  I: Cardinal;
begin
  P := GrayscaleAlphaPixels;
  for I := 1 to Width * Height * Depth do
  begin
    Move(Pixel, P^, SizeOf(Pixel));
    Inc(P);
  end;
end;

function TGrayscaleAlphaImage.IsClear(const Pixel: TVector2Byte): boolean;
var
  P: PVector2Byte;
  I: Cardinal;
begin
  P := GrayscaleAlphaPixels;
  for I := 1 to Width * Height * Depth do
  begin
    if not CompareMem(@Pixel, P, SizeOf(Pixel)) then
    begin
      Result := false;
      Exit;
    end;
    Inc(P);
  end;
  Result := true;
end;

function TGrayscaleAlphaImage.HasAlpha: boolean;
begin
  Result := true;
end;

function TGrayscaleAlphaImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
var
  PtrAlpha: PVector2Byte;
  I: Cardinal;
begin
  PtrAlpha := GrayscaleAlphaPixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^[1] > AlphaTolerance) and
       (PtrAlpha^[1] < 255 - AlphaTolerance) then
      Exit(acFullRange);
    Inc(PtrAlpha);
  end;

  Result := acSimpleYesNo;
end;

procedure TGrayscaleAlphaImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector2Byte;
  SecondPtr: PVector2Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := GrayscaleAlphaPixels;
  SecondPtr := TGrayscaleAlphaImage(SecondImage).GrayscaleAlphaPixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TGrayscaleAlphaImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4Single; const Colors: TVector4Pointer);
var
  OutputCol: PVector2Byte absolute OutputColor;
  Cols: array [0..3] of PVector2Byte absolute Colors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^[0] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[0] + Weights[1] * Cols[1]^[0] + Weights[2] * Cols[2]^[0] + Weights[3] * Cols[3]^[0]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^[1] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(Weights[0] * Cols[0]^[1] + Weights[1] * Cols[1]^[1] + Weights[2] * Cols[2]^[1] + Weights[3] * Cols[3]^[1]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

procedure TGrayscaleAlphaImage.Assign(const Source: TCastleImage);
var
  RgbaPtr: PVector4Byte;
  RgbPtr: PVector3Byte absolute RgbaPtr;
  SelfPtr: PVector2Byte;
  I: Cardinal;
begin
  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    SelfPtr := GrayscaleAlphaPixels;
    RgbaPtr := TRGBAlphaImage(Source).AlphaPixels;
    for I := 1 to Width * Height * Depth do
    begin
      SelfPtr^[0] := GrayscaleValue(RgbPtr^);
      SelfPtr^[1] := RgbaPtr^[3];
      Inc(SelfPtr);
      Inc(RgbaPtr);
    end;
    URL := Source.URL;
  end else

    inherited;
end;

{ RGBE <-> 3 Single color convertion --------------------------------- }

const
  { do signed Exponent dodaj RGBEExponentOffset zeby zapisac exponent jako Byte }
  RGBEExponentOffset = 128;
  { RGBEMin/MaxExponent = min i max wartosci dla exponent ktore moga dac
    (Exponent + RGBEExponentOffset) w zakresie Byte.
    Czyli RGBEMinExponent + RGBEExponentOffset = 0,
          RGBEMaxExponent + RGBEExponentOffset = High(Byte),
    stad  RGBEMinExponent = -RGBEExponentOffset,
          RGBEMaxExponent = High(Byte) - RGBEExponentOffset }
  RGBEMinExponent = -RGBEExponentOffset;
  RGBEMaxExponent = High(Byte) - RGBEExponentOffset;

  { zero musi byc reprezentowane w specjalny sposob w formacie RGBE,
    podobnie jak w kazdym formacie zmiennoprzec. }
  RGBEZero: TVector4Byte=(0, 0, 0, 0);

  RGBELow :TVector4Byte=(0, 0, 0, 0); { = RGBEZero }
  RGBEHigh: TVector4Byte=(High(Byte), High(Byte), High(Byte), High(Byte));

function Vector3ToRGBE(const v: TVector3Single): TVector4Byte;
{ implementacja : jak Graphic Gems II.5 ale z poprawkami -
  - nazwy MaxVal i V sa osobne (dla czytelnosci),
  - checki czy Exponent jest w granicach RGBEMin/MaxExponent }
{ uwagi : moznaby sadzic ze Multiplier powinien byc liczony jako
    Mantissa * 255 / MaxVal (255 = High(Byte) zamiast 256),
    zeby poprawnie mapowac zakres 0..1 na zakres bajta.
    Ale,
    - po pierwsze, specyfikacja formatu RGBE (czyli Graphic Gems II.5)
      mowi zeby uzywac 256
    - po drugie, uzywanie 256 podaje nam prosty warunek na sprawdzenie
      czy czworka bajtow jest poprawnym RGBE : mianowicie, przynajmniej
      jeden z pierwszych trzech bajtow musi byc >= 128
      (czyli musi miec najstarszy bit = 1). Tym samym ten bajt jest >= 0.5
      a wiec jest poprawna mantysa. Ten prosty test na poprawnosc ma zastosowanie
      przy kodowaniu plikow rgbe przy uzyciu prostego RLE, gdzie wykorzystujemy
      takie niepoprawne czworki RGBE to kodowania specjalnych informacji.
    - po trzecie i chyba najwazniejsze, gdyby uzywac 256 to wartosc
      mantysy = 255 byla bezuzyteczna bo odpowiadalaby wartosci float = 1.0
      a mantysa zawsze musi byc ostro mniejsza od 1, z definicji.
      I to jest chyba koronny argument za mnozeniem tutaj przez 256.
}
var
  MaxVal, Multiplier: Single;
  Mantissa: Extended;
  Exponent: Integer;
begin
  MaxVal := CastleUtils.max(v[0], CastleUtils.max(v[1], v[2]));

  { rozpatrujemy tu nie tylko przypadek gdy liczba jest = 0 ale takze
    gdy jest bliska zeru. To jest standardowe zachowanie, ale uwaga -
    - w tym przypadku mogloby sie (blednie) wydawac ze mozemy tutaj zrobic
    wyjatek i sprawdzac ponizej tylko MaxVal = 0.0 (dokladna rownosc)
    a sprawdzanie bliskosci do zera zrzucic na test Exponent < RGBEMinExponent
    ponizej. ALE to nie jest prawda - test Exponent < RGBEMinExponent przejdzie
    dopiero dla niesamowicie mikroskopijnych liczb (< 1 / 2^127) podczas gdy liczby
    pomiedzy tymi "mikroskopijnie malymi" a SINGLE_EQUALITY_EPSILON ciagle
    beda powodowac problemy (bo przy liczeniu Multiplier dzielimy przez MaxVal
    wiec male MaxVal -> Float overflow). }
  if MaxVal < SingleEqualityEpsilon then begin result := RGBEZero; Exit end;

  Frexp(MaxVal, Mantissa, Exponent);

  if Exponent < RGBEMinExponent then begin result := RGBELow; Exit end;
  if Exponent > RGBEMaxExponent then begin result := RGBEHigh; Exit end;

  Multiplier := Mantissa * 256 / MaxVal;

  { MaxVal * Multiplier daje Mantissa * High(byte) a wiec cos w zakresie
    0 .. High(Byte) bo Mantissa <= 1 (de facto, Mantissa >= 0.5 wiec
    mozna podac dokladniejsze ograniczenie na Mantissa * High(byte)).
    Wszystkie pozostale v[] sa mniejsze od MaxVal wiec one tez dadza cos
    w zakresie bajta. }
  result[0] := Clamped(Round(v[0]*Multiplier), 0, High(Byte));
  result[1] := Clamped(Round(v[1]*Multiplier), 0, High(Byte));
  result[2] := Clamped(Round(v[2]*Multiplier), 0, High(Byte));

  { sprawdzajac czy Exponent in RGBEMin/MaxExponent wczesniej juz zapewnilem
    sobie ze ponizsze przypisanie jest Ok, wynik zmiesci sie w zakresie bajta. }
  result[3] := Exponent + RGBEExponentOffset;
end;

function VectorRGBETo3Single(const v: TVector4Byte): TVector3Single;
{ implementacja : jak Graphic Gems II.5.

  Multiplier wychodzi od 1/256 (a nie 1/255), nalezaloby tu wiec poczynic
  podobne uwagi co przy konwersji w druga strone, Vector3ToRGBE.
  Patrz tamtejszy komentarz. }
var
  Multiplier: Single;
begin
  if v[3] = 0 then begin result := ZeroVector3Single; Exit end;

  Multiplier := Ldexp(1/256, Integer(v[3])-RGBEExponentOffset);
  result[0] := v[0]*Multiplier;
  result[1] := v[1]*Multiplier;
  result[2] := v[2]*Multiplier;
end;

{ file formats managing ---------------------------------------------------------------- }

function MimeTypeToImageFormat(const MimeType: string;
  const OnlyLoadable, OnlySaveable: boolean; out ImgFormat: TImageFormat): boolean;
var
  I: TImageFormat;
  M: TImageFormatInfoMimeTypesCount;
begin
  for I := Low(I) to High(I) do
  begin
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[I].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[I].Save)) then
    for M := 1 to ImageFormatInfos[I].MimeTypesCount do
      if MimeType = ImageFormatInfos[I].MimeTypes[M] then
      begin
        ImgFormat := I;
        Exit(true);
      end;
  end;
  Result := false;
end;

function ListImageExtsLong(OnlyLoadable, OnlySaveable: boolean; const LinePrefix: string): string;
var
  iff: TImageFormat;
  i: integer;
begin
  result := '';

  for iff := Low(iff) to High(iff) do
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].Save)) then
    begin
      { zwrocmy uwage ze nie chcemy doklejac nl na koncu (bo zalatwieniu
        sprawy z formatem iff) bo tam nie byloby zbyt wygodnie rozpoznawac
        czy jestesmy ostatnia linia czy nie (na skutek OnlySaveable/OnlyLoadable
        nie mozna tego rozpoznac prostym sprawdzeniem iff < High(iff) }
      if result <> '' then result := result + nl;

      result := result +LinePrefix +ImageFormatInfos[iff].exts[1];
      for i := 2 to ImageFormatInfos[iff].extsCount do
        result := result + ', ' +ImageFormatInfos[iff].exts[i];
      result := result + ' - '+ImageFormatInfos[iff].formatName;
    end;
end;

function ListImageExtsShort(OnlyLoadable, OnlySaveable: boolean): string;
var
  iff: TImageFormat;
  i: integer;
begin
  result := '';

  for iff := Low(iff) to High(iff) do
    if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].Load)) and
       ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].Save)) then
    begin
      for i := 1 to ImageFormatInfos[iff].extsCount do
      begin
        if result <> '' then result := result + ', ';
        result := result + ImageFormatInfos[iff].exts[i];
      end;
    end;
end;

{ LoadImage --------------------------------------------------------------- }

{ Make sure the image has an alpha channel.
  If image doesn't have an alpha channel (it is TRGBImage or TGrayscaleImage),
  we will create new image instance (respectively, TRGBAlphaImage or TGrayscaleAlphaImage)
  that adds an alpha channel. The newly created alpha channel will have constant opaque alpha,
  except in the special case of TGrayscaleImage with TGrayscaleImage.TreatAsAlpha = @true
  (where the contents will be copied to alpha, and intensity set to white).

  If the image already had an alpha channel, then just return it. }
procedure ImageAddAlphaTo1st(var Img: TCastleImage);
var
  NewImg: TCastleImage;
begin
  if Img is TRGBImage then
  begin
    NewImg := TRGBImage(Img).ToRGBAlphaImage;
    FreeAndNil(Img);
    Img := NewImg;
  end else
  if Img is TGrayscaleImage then
  begin
    NewImg := TGrayscaleImage(Img).ToGrayscaleAlphaImage;
    FreeAndNil(Img);
    Img := NewImg;
  end;

  if not ((Img is TRGBAlphaImage) or
          (Img is TGrayscaleAlphaImage)) then
    raise EInternalError.Create(
      'ImageAddAlphaTo1st not possible for this TCastleImage descendant: ' + Img.ClassName);
end;

function LoadImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TCastleImageClass)
  :TCastleImage;

  { ClassAllowed is only a shortcut to global utility. }
  function ClassAllowed(ImageClass: TCastleImageClass): boolean;
  begin
    Result := CastleImages.ClassAllowed(ImageClass, AllowedImageClasses);
  end;

  { On input, Image must be TRGBImage and on output it will be TGrayscaleImage. }
  procedure ImageGrayscaleTo1st(var Image: TCastleImage);
  var
    NewImage: TGrayscaleImage;
  begin
    NewImage := (Image as TRGBImage).ToGrayscale;
    FreeAndNil(Image);
    Image := NewImage;
  end;

  procedure ImageRGBToFloatTo1st(var Image: TCastleImage);
  var
    NewResult: TCastleImage;
  begin
    NewResult := (Image as TRGBImage).ToRGBFloat;
    Image.Free;
    Image := NewResult;
  end;

  procedure ImageRGBToGrayscaleTo1st(var Image: TCastleImage);
  var
    NewResult: TCastleImage;
  begin
    NewResult := (Image as TRGBImage).ToGrayscale;
    Image.Free;
    Image := NewResult;
  end;

var
  Load: TImageLoadFunc;
begin
  Result := nil;
  try
    if Assigned(ImageFormatInfos[StreamFormat].Load) then
    begin
      Load := ImageFormatInfos[StreamFormat].Load;
      case ImageFormatInfos[StreamFormat].LoadedClasses of
        lcG_GA_RGB_RGBA:
          begin
            if ClassAllowed(TRGBImage) or
               ClassAllowed(TRGBAlphaImage) or
               ClassAllowed(TGrayscaleImage) or
               ClassAllowed(TGrayscaleAlphaImage) then
              Result := Load(Stream, AllowedImageClasses) else
            if ClassAllowed(TRGBFloatImage) then
            begin
              Result := Load(Stream, [TRGBImage]);
              ImageRGBToFloatTo1st(result);
            end else
              raise EUnableToLoadImage.CreateFmt('LoadImage cannot load this image file format to %s', [LoadImageParams(AllowedImageClasses)]);
          end;
        lcRGB_RGBA:
          begin
            if ClassAllowed(TRGBImage) or
               ClassAllowed(TRGBAlphaImage) then
              Result := Load(Stream, AllowedImageClasses) else
            if ClassAllowed(TGrayscaleImage) then
            begin
              Result := Load(Stream, [TRGBImage]);
              ImageRGBToGrayscaleTo1st(result);
            end else
{ TODO:     if ClassAllowed(TGrayscaleAlphaImage) then
              ... }
            if ClassAllowed(TRGBFloatImage) then
            begin
              Result := Load(Stream, [TRGBImage]);
              ImageRGBToFloatTo1st(result);
            end else
              raise EUnableToLoadImage.CreateFmt('LoadImage cannot load this image file format to %s', [LoadImageParams(AllowedImageClasses)]);
          end;
        lcRGB:
          begin
            Result := Load(Stream, [TRGBImage]);
            Assert(Result is TRGBImage);

            if not (ClassAllowed(TRGBImage)) then
            begin
              if ClassAllowed(TRGBAlphaImage) then
              begin
                ImageAddAlphaTo1st(Result);
              end else
              if ClassAllowed(TGrayscaleImage) then
              begin
                ImageGrayscaleTo1st(Result);
              end else
              { TODO:
              if ClassAllowed(TGrayscaleAlphaImage) then
              begin
                ImageAddAlphaTo1st(Result);
                ImageGrayscaleAlphaTo1st(Result);
              end else }
              if ClassAllowed(TRGBFloatImage) then
              begin
                ImageRGBToFloatTo1st(result);
              end else
                raise EUnableToLoadImage.CreateFmt('LoadImage cannot load this image file format to %s', [LoadImageParams(AllowedImageClasses)]);
            end;
          end;
        lcRGB_RGBFloat:
          begin
            if ClassAllowed(TRGBFloatImage) or
               ClassAllowed(TRGBImage) then
              Result := LoadRGBE(Stream, AllowedImageClasses) else
            begin
              Result := LoadRGBE(Stream, [TRGBImage]);
              if ClassAllowed(TRGBAlphaImage) then
              begin
                ImageAddAlphaTo1st(result);
              end else
              if ClassAllowed(TGrayscaleImage) then
              begin
                ImageGrayscaleTo1st(Result);
              end else
              if ClassAllowed(TGrayscaleAlphaImage) then
              begin
                ImageGrayscaleTo1st(Result);
                ImageAddAlphaTo1st(Result);
              end else
                raise EUnableToLoadImage.CreateFmt('LoadImage: RGBE format cannot be loaded to %s', [LoadImageParams(AllowedImageClasses)]);
            end;
          end;
        else raise EInternalError.Create('LoadImage: LoadedClasses?');
      end;
    end else
    raise EImageFormatNotSupported.Create('Can''t load image format "'+
      ImageFormatInfos[StreamFormat].FormatName+'"');

  except Result.Free; raise end;
end;

function LoadImage(Stream: TStream; const MimeType: string;
  const AllowedImageClasses: array of TCastleImageClass)
  :TCastleImage;
var
  iff: TImageFormat;
begin
  if MimeTypeToImageFormat(MimeType, true, false, iff) then
    result := LoadImage(Stream, iff, AllowedImageClasses) else
    raise EImageFormatNotSupported.Create('Unrecognized image MIME type: "'+MimeType+'"');
end;

function LoadImage(const URL: string;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
const
  SLoadError = 'Error loading image from URL "%s": %s';
var
  F: TStream;
  MimeType: string;
begin
  try
    try
      F := Download(URL, [soForceMemoryStream], MimeType);
    except
      on E: EReadError do raise EImageLoadError.Create(E.Message);
    end;

    try
      Result := LoadImage(F, MimeType, AllowedImageClasses);
      Result.FURL := URL;
    finally F.Free end;
  except
    { capture some exceptions to add URL to exception message }
    on E: EImageLoadError do
    begin
      E.Message := Format(SLoadError, [URIDisplay(URL), E.Message]);
      raise;
    end;
    on E: EImageFormatNotSupported do
    begin
      E.Message := Format(SLoadError, [URIDisplay(URL), E.Message]);
      raise;
    end;
  end;
end;

function LoadImage(const URL: string): TCastleImage;
begin
  Result := LoadImage(URL, []);
end;

function LoadImage(const URL: string;
  const AllowedImageClasses: array of TCastleImageClass;
  const ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation): TCastleImage;
begin
  Result := LoadImage(URL, AllowedImageClasses);
  Result.Resize(ResizeWidth, ResizeHeight, Interpolation);
end;

{ SaveImage na TCastleImage ---------------------------------------------------- }

procedure SaveImage(const Img: TCastleImage; const Format: TImageFormat; Stream: TStream);
var
  ImgRGB: TRGBImage;
  Save: TImageSaveFunc;
begin
  if Assigned(ImageFormatInfos[Format].Save) then
  begin
    Save := ImageFormatInfos[Format].Save;
    case ImageFormatInfos[Format].SavedClasses of
      scRGB:
        begin
          if Img is TRGBImage then
            Save(Img, Stream) else
          if Img is TRGBFloatImage then
          begin
            ImgRGB := TRGBFloatImage(Img).ToRGBImage;
            try
              SaveImage(ImgRGB, Format, Stream);
            finally ImgRGB.Free end;
          end else
            raise EImageSaveError.CreateFmt('Saving image not possible: Cannot save image class %s to this format', [Img.ClassName]);
        end;
      scG_GA_RGB_RGBA:
        begin
          if (Img is TRGBImage) or
             (Img is TRGBAlphaImage) or
             (Img is TGrayscaleImage) or
             (Img is TGrayscaleAlphaImage) then
            Save(Img, Stream) else
          if Img is TRGBFloatImage then
          begin
            ImgRGB := TRGBFloatImage(Img).ToRGBImage;
            try
              SaveImage(ImgRGB, Format, Stream);
            finally ImgRGB.Free end;
          end else
            raise EImageSaveError.CreateFmt('Saving image not possible: Cannot save image class %s to this format', [Img.ClassName]);
        end;
      scRGB_RGBFloat:
        begin
          if (Img is TRGBImage) or
             (Img is TRGBFloatImage) then
            Save(Img, Stream) else
            raise EImageSaveError.CreateFmt('Saving image not possible: Cannot save image class %s to this format', [Img.ClassName]);
        end;
      else raise EInternalError.Create('SaveImage: SavedClasses?');
    end;
  end else
    raise EImageSaveError.CreateFmt('Saving image class %s not implemented', [Img.ClassName]);
end;

procedure SaveImage(const img: TCastleImage; const MimeType: string; Stream: TStream);
var
  Format: TImageFormat;
begin
  if not MimeTypeToImageFormat(MimeType, false, true, Format) then
    raise EImageSaveError.CreateFmt('Unknown image MIME type "%s", cannot save. Make sure the filename/URL you want to save has one of the recognized extensions',
      [MimeType]);
  SaveImage(Img, Format, Stream);
end;

procedure SaveImage(const Img: TCastleImage; const URL: string);
var
  Stream: TStream;
  Format: TImageFormat;
  MimeType: string;
begin
  { Do not call SaveImage with MimeType: string parameter, instead calculate
    Format here. This way we can make better error messaage. }
  MimeType := URIMimeType(URL);
  if not MimeTypeToImageFormat(MimeType, false, true, Format) then
    raise EImageSaveError.CreateFmt('Unknown image MIME type "%s", cannot save URL "%s". Make sure the filename/URL you want to save has one of the recognized extensions',
      [MimeType, URL]);

  Stream := URLSaveStream(URL);
  try
    SaveImage(Img, Format, Stream);
  finally FreeAndNil(Stream) end;
end;

{ other image processing ------------------------------------------- }

function ImageClassBestForSavingToFormat(const URL: string): TCastleImageClass;
var
  Format: TImageFormat;
begin
  if not MimeTypeToImageFormat(URIMimeType(URL), false, true, Format) then
    Exit(TRGBImage);
  Result := ImageClassBestForSavingToFormat(Format);
end;

function ImageClassBestForSavingToFormat(const Format: TImageFormat): TCastleImageClass;
begin
  if Format = ifRGBE then
    Result := TRGBFloatImage else
    Result := TRGBImage;
end;

{ unit initialization / finalization ----------------------------------------- }

procedure InitializeImagesFileFilters;

  function CreateImagesFilters: TFileFilterList;
  begin
    Result := TFileFilterList.Create(true);
    Result.AddFilter('All Files', ['*']);
    Result.AddFilter('All Images', []);
    Result.DefaultFilter := 1;
  end;

  procedure AddImageFormat(Filters: TFileFilterList; Format: TImageFormatInfo);
  var
    F: TFileFilter;
    ExtIndex: Integer;
    Pattern: string;
  begin
    F := TFileFilter.Create;
    Filters.Add(F);
    F.Name := Format.FormatName + ' (';

    for ExtIndex := 1 to Format.ExtsCount do
    begin
      Pattern := '*.' + Format.Exts[ExtIndex];

      { add to "All images" filter }
      Filters[Filters.DefaultFilter].Patterns.Append(Pattern);

      { add to this filter }
      F.Patterns.Append(Pattern);

      { add to this filter visible name }
      if ExtIndex <> 1 then F.Name := F.Name + ', ';
      F.Name := F.Name + Pattern;
    end;

    F.Name := F.Name + ')';
  end;

var
  Format: TImageFormat;
begin
  LoadImage_FileFilters := CreateImagesFilters;
  SaveImage_FileFilters := CreateImagesFilters;

  for Format := Low(Format) to High(Format) do
  begin
    if Assigned(ImageFormatInfos[Format].Load) then
      AddImageFormat(LoadImage_FileFilters, ImageFormatInfos[Format]);
    if Assigned(ImageFormatInfos[Format].Save) then
      AddImageFormat(SaveImage_FileFilters, ImageFormatInfos[Format]);
  end;
end;

procedure AlphaMaxTo1st(var A: TAlphaChannel; const B: TAlphaChannel);
begin
  if B > A then A := B;
end;

function StringToAlpha(S: string; var WarningDone: boolean): TAutoAlphaChannel;
begin
  S := UpperCase(S);
  if S = 'AUTO' then
    Result := acAuto else
  if S = 'NONE' then
    Result := acNone else
  if S = 'SIMPLE_YES_NO' then
    Result := acSimpleYesNo else
  if S = 'FULL_RANGE' then
    Result := acFullRange else
  begin
    if not WarningDone then
    begin
      OnWarning(wtMajor, 'VRML/X3D', Format('Invalid "alphaChannel" field value "%s"', [S]));
      WarningDone := true;
    end;
    Result := acAuto;
  end;
end;

initialization
  InitializeImagesFileFilters;
  {$ifndef CASTLE_PNG_USING_FCL_IMAGE}
  InitializePNG;
  {$endif}
finalization
  FreeAndNil(LoadImage_FileFilters);
  FreeAndNil(SaveImage_FileFilters);
end.
