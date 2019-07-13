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

(*Loading, saving, and processing of images (TEncodedImage,
  TCastleImage and other classes).
  This unit deals with images, stored in normal memory (not on GPU).
  Images can be loaded and saved from/to various formats
  and processed in a lot of ways.
  For example you can resize images, you can draw one image on another,
  convert to grayscale and so on.

  The "image" as understood by this unit may have some interesting features
  useful with modern GPUs: image data may be compressed for GPU
  (@link(TGPUCompressedImage)), image data may be 3D
  (every image has @code(Depth), in addition to
  @code(Width) and @code(Height)).

  The most important class here is @link(TCastleImage).
  It represents an image as a simple uncompressed array of pixels.
  Descendants of TCastleImage define what exactly is a "pixel".
  We have 8-bit color images
  (@link(TRGBAlphaImage), @link(TRGBImage),
  @link(TGrayscaleAlphaImage) and @link(TGrayscaleImage)).
  We also have an image with floating-point precision and range:
  @link(TRGBFloatImage).

  There is also a more abstract image class @link(TEncodedImage),
  representing either uncompressed image (@link(TCastleImage))
  or an image with data compressed for GPU (@link(TGPUCompressedImage)).

  When reading and writing image files, we understand various image
  formats. See castle-view-image documentation
  ( https://castle-engine.io/castle-view-image.php )
  for a current list of supported image formats.

  The basic loading and saving procedures are LoadImage and SaveImage.
  Example usage:

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

  This unit is not dependent on OpenGL or any other rendering
  library. See @link(CastleGLImages) for OpenGL image operations
  (for textures and others).
*)

unit CastleImages;

{$include castleconf.inc}
{$ifdef CASTLE_OBJFPC} {$modeswitch nestedprocvars}{$H+} {$endif}

interface

uses SysUtils, Classes, Math, Generics.Collections,
  { FPImage and related units }
  FPImage, FPReadPCX, FPReadGIF, FPReadPSD, FPReadTGA, FPReadTiff, FPReadXPM,
  FPReadJPEG, FPWriteJPEG, FPReadPNM, FPReadPNG, CastleInternalFPWritePNG,
  { CGE units }
  CastleInternalPng, CastleUtils, CastleVectors, CastleRectangles,
  CastleFileFilters, CastleClassUtils, CastleColors;

type
  TAutoAlphaChannel = (acAuto, acNone, acTest, acBlending);
  { See TCastleImage.AlphaChannel. }
  TAlphaChannel = acNone .. acBlending;

const
  acSimpleYesNo = acBlending deprecated 'use acTest';
  acFullRange = acBlending deprecated 'use acBlending';
  { Default parameters for TEncodedImage.AlphaChannel,
    decide how to detect textures alpha channel. }
  DefaultAlphaTolerance = 5;

{ Check if the two RGB colors are equal, ignoring small differences.
  All three color components may differ by at most Tolerance.
  When Tolerance is 0, this is a normal (exact) comparison. }
function EqualRGB(const Color1, Color2: TVector3Byte; Tolerance: Byte): boolean;

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

  EImageDrawError = class(Exception);

  { An internal class to communicate image data
    between CastleImages and fcl-image efficiently.
    @exclude }
  TInternalCastleFpImage = class(TFPCompactImgRGBA8Bit)
  strict private
    function GetColors8Bit(const x, y: integer): TFPCompactImgRGBA8BitValue;
    procedure SetColors8Bit(const x, y: integer; const Value: TFPCompactImgRGBA8BitValue);
  public
    property Colors8Bit[X, Y: Integer]: TFPCompactImgRGBA8BitValue
      read GetColors8Bit write SetColors8Bit;
  end;

  { Abstract class for an image with unspecified, possibly compressed,
    memory format. The idea is that both uncompressed images (TCastleImage)
    and images compressed for GPU (TGPUCompressedImage) are derived from this class. }
  TEncodedImage = class
  private
    FWidth, FHeight, FDepth: Cardinal;
    FURL: string;
    procedure NotImplemented(const AMethodName: string);
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); virtual;
    function ToFpImage: TInternalCastleFpImage; virtual;
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

    function Dimensions: TVector3Cardinal;

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
      and always acTest or acBlending if HasAlpha = true.
      But AlphaChannel may perform longer analysis of pixels
      (to differ between acTest and acBlending), while this
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

    { Create a new image object that has exactly the same class
      and the same data (size, pixels) as this image. }
    function CreateCopy: TEncodedImage; virtual; abstract;

    { Mirror image vertically. }
    procedure FlipVertical; virtual; abstract;
  end;

  { Resize interpolation modes, see TCastleImage.Resize and TCastleImage.MakeResized. }
  TResizeInterpolation = (
    { Fastest interpolation mode. }
    riNearest,
    { Bilinear interpolation mode, quite fast. }
    riBilinear,
    { Slower but prettier interpolation modes.
      All the following interpolation modes are slower,
      not only because their equations are more complicated (and using more inputs),
      but also because their current implementation involves round-trip to FpImage
      format.
      Use these if speed is not a concern, but quality is crucial. }
    riMitchel,
    riBlackman,
    riBlackmanSinc,
    riBlackmanBessel,
    riGaussian,
    riHermite,
    riLanczos,
    riQuadratic,
    riCubic,
    riCatrom,
    riHanning,
    riHamming
  );

  TResizeInterpolationInternal = Low(TResizeInterpolation) .. riBilinear;
  TResizeInterpolationFpImage = Succ(riBilinear) .. High(TResizeInterpolation);

  { Drawing mode used by image-on-image drawing methods
    (@link(TCastleImage.DrawFrom) and @link(TCastleImage.DrawTo)). }
  TDrawMode = (
    { Normal drawing mode, where the image contents are blended using
      the opacity (alpha) of the source image. That is,

      @preformatted(
        destination.rgb := destination.rgb * (1 - source.alpha) + source.rgb * source.alpha;
        destination.alpha := destination.alpha; // never changed by this drawing mode
      )

      An image type without alpha (like TRGBImage or TGrayscaleImage)
      is always treated like it has alpha = 1.0 (fully opaque) everywhere.
      In particular, this means that when drawing @italic(an image
      without alpha over any other image), source RGB contents will
      simply replace the destination RGB contents. }
    dmBlend,

    { An advanced blending mode, capable of blending 2 images with alpha channel
      better than dmBlend.
      Based on https://en.wikipedia.org/wiki/Alpha_compositing formula for alpha-blending.
      This one is much less efficient than dmBlend and should be used only in case
      several layers of semi-transparent images should overlay one another and it
      matters to accurately account for both images alpha channel. Implemented for
      all @link(TRGBAlphaImage) and @link(TGrayscaleAlphaImage) combinations.
    }
    dmBlendSmart,

    { Multiply two images. Simply multiply source with destination, channel by channel:

      @preformatted(
        destination.rgba := destination.rgba * source.rgba;
      )

      The exception is when the source image has alpha channel,
      but destination does not. For example, when source is TRGBAlphaImage
      or TGrayscaleAlphaImage and destination is TRGBImage or TGrayscaleImage.
      In this case the multiplication is followed by a simple blending,
      to apply the effects of source alpha:

      @preformatted(
        destination.rgb :=
          source.rgb * destination.rgb * source.alpha +
                       destination.rgb * (1 - source.alpha);
      )

      Note that if source.alpha = 1 (source is opaque) that this is equivalent
      to the previous simple multiply equation. }
    dmMultiply,

    { Overwrite drawing mode, where the image contents of destination image
      is replaced with contents of source image. That is,

      @preformatted(
        destination.rgba := source.rgba;
      )
    }
    dmOverwrite,

    { Additive drawing mode, where the image contents of source image
      are added to the existing destination image. That is,

      @preformatted(
        destination.rgb := destination.rgb + source.rgb * source.alpha;
        destination.alpha := destination.alpha; // never changed by this drawing mode
      )

      So when drawing @italic(an image with alpha over an image without alpha),
      the colors will be added according to the above equation, only source
      is multiplied by alpha. To speed this operation, one can use
      @link(TRGBAlphaImage.PremultiplyAlpha) on the source image first,
      very useful if you plan to draw the same source image many times. }
    dmAdd
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
      than just memory copying (so also for handling mode other than
      dmBlend). }
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); virtual;

    function MakeResizedToFpImage(ResizeWidth, ResizeHeight: Cardinal;
      const Interpolation: TResizeInterpolation): TInternalCastleFpImage;

    function GetColors(const X, Y, Z: Integer): TCastleColor; virtual;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); virtual;
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
      const ADepth: Cardinal = 1); overload;
    procedure SetSize(const Source: TCastleImage); overload;

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

    { Inverts all colors (RGB or grayscale, but doesn't touch alpha channel).
      "Inverting" means changing color C in range [0..1] to 1-C,
      so black becomes white, white becomes black etc.

      @italic(For descendants implementors:)
      Override it if necessary, otherwise the default implementation in this class
      will raise EInternalError. }
    procedure InvertColors; virtual;

    { Get or set the color of the pixel.

      In case of descendants without alpha, we may drop this information.

      In case of grayscale descendants, when getting or setting we convert
      the color to/from grayscale as necessary. This means that setting
      RGB color on a grayscale image may lose information -- we will convert
      your color to grayscale.

      Caller is responsible for checking the correctness of given
      X, Y, Z coordinates. For speed, we may not check them inside (so nasty
      memory errors will occur in case of invalid coordinates). }
    property Colors [X, Y, Z: Integer]: TCastleColor read GetColors write SetColors;

    procedure SetColorRGB(const X, Y: Integer; const v: TVector3);
      deprecated 'use Colors[X, Y, 0] to get or set colors';

    { Create a new image object that has exactly the same class
      and the same data (size, pixels) as this image.

      (Design note: this function is *not* a constructor, because it's implemented
      in TCastleImage, but it always returns some descendant of TCastleImage.) }
    function MakeCopy: TCastleImage;

    { Create a new image object that has exactly the same class
      and the same data (size, pixels) as this image.

      Equivalent to MakeCopy, but virtual and declared as returning TEncodedImage class. }
    function CreateCopy: TEncodedImage; override;

    { Change Width and Height and appropriately stretch
      image contents.

      If ResizeWidth or ResizeHeight is 0 then it means to take
      Width or Height, respectively.
      So e.g. using ResizeWidth = ResizeHeight = 0 is the same thing
      as using ResizeWidth = Width and ResizeHeight = Height and this is NOP.

      Remember that resizing may change RawPixels pointer, so all pointers
      that you aquired using functions like
      RawPixels, Pixels, PixelsArray, RowPtr, PixelPtr
      may be invalid after calling Resize.

      If ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from CastleProgress to indicate progress of operation. }
    procedure Resize(ResizeWidth, ResizeHeight: Cardinal;
      const Interpolation: TResizeInterpolation = riBilinear;
      const ProgressTitle: string = '');

    { Change Width and Height and appropriately stretch image contents.

      This scales the image in almost the same way as standard @link(Resize).
      However, this is aware of the image corners and edges, which is good
      if you plan to use this image with @link(TDrawableImage.Draw3x3) drawing.

      The Corners parameter specifies the corners size, in the same
      clockwise order as for @link(TDrawableImage.Draw3x3): top, right, bottom, left.
      The corners will be scaled (proportially to image scaling),
      and new Corners size returned.
      Additionally it makes sure that filtering (especially bilinear)
      does not "leak" colors from one image area to another.
      Effectively, the image is scaled like a 9 separate parts,
      and colors cannot bleed from one part to another.

      Both ResizeWidth, ResizeHeight parameters must be provided and non-zero.

      For now, only a subset of TResizeInterpolation values
      are supported by this method, namely the ones in TResizeInterpolationInternal. }
    procedure Resize3x3(const ResizeWidth, ResizeHeight: Cardinal;
      var Corners: TVector4Integer;
      const Interpolation: TResizeInterpolationInternal);
      deprecated 'This method is seldom useful, and it is confused with TCastleImage.DrawFrom3x3 and TDrawableImage.Draw3x3 too often. Please report if you have a use-case when this method is useful, otherwise it may get removed from the engine one day.';

    { Create a new TCastleImage instance with size ResizeWidth, ResizeHeight
      and pixels copied from the input and appropriately stretched.
      The exact class of the new instance is the same as our class.

      As with @link(Resize) method, when the parameter ResizeTo* is 0
      it means to use current Width/Height.
      So e.g. using MakeResized(0, 0) is the same thing as using CreateCopy.

      As with @link(Resize),
      if ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from CastleProgress to indicate progress of operation. }
    function MakeResized(ResizeWidth, ResizeHeight: Cardinal;
      const Interpolation: TResizeInterpolation = riBilinear;
      const ProgressTitle: string = ''): TCastleImage;

    { Mirror image horizotally (that is right edge is swapped with left edge). }
    procedure FlipHorizontal;

    { Mirror image vertically. }
    procedure FlipVertical; override;

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

      This checks parameters for correctness -- if start position is not
      correct or ExtractWidth/Height are too large then exception
      @link(EImagePosOutOfRange) is raised. }
    function MakeExtracted(X0, Y0, ExtractWidth, ExtractHeight: Cardinal): TCastleImage;

    { Set all image pixels to the same color. }
    procedure Clear(const Pixel: TVector4Byte); overload; virtual;
    procedure Clear(const Pixel: TCastleColor); overload;

    { Check do all image pixels have the same color. }
    function IsClear(const Pixel: TVector4Byte): boolean; overload; virtual;

    { Multiply each RGB color by a matrix.
      This is a useful routine for many various conversions of image colors.
      Every pixel's RGB color is multiplied by given Matrix,
      i.e. PixelRGBColor := Matrix * PixelRGBColor.

      If some value in some channel will be < 0, it will be set to 0.
      And if it will be > High(Byte), it will be set to High(Byte).

      Examples: when
        Matrix = TMatrix3.Identity, this is NOOP.
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
          in ImageTransformColorsVar function)

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
    procedure TransformRGB(const Matrix: TMatrix3); virtual;

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
      equivalent to CreateCopy.

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

      @italic(Warning: It is not efficient to use this drawing method.)
      This drawing is performed on CPU and cannot be fast.
      If possible, use instead @link(TDrawableImage.DrawFrom),
      that performs image-on-image drawing using GPU,
      or just draw @link(TDrawableImage) straight to the screen by @link(TDrawableImage.Draw).
      See examples/images_videos/draw_images_on_gpu.lpr for an example of
      @link(TDrawableImage.DrawFrom).
      Using this method makes most sense in image manipulation tools,
      or during the loading / preparation stage of your game,
      not during actual game.

      @italic(Note for descendants implementors:)
      The default implementation of this function in TCastleImage
      can only directly copy the pixels, regardless
      of what information they have. This makes it very fast,
      but not suitable if the source image has some alpha channel
      and you want to apply it over a destination image with blending
      (adding scaled source to a destination color),
      and not suitable when Mode is <> dmBlend.
      Descendants with alpha channel should override @link(DrawFromCore)
      to handle drawing with blending (for dmBlend),
      all descendants should override @link(DrawFromCore)
      to handle drawing with Mode <> dmBlend.

      @raises(EImageDrawError When drawing cannot be performed,
        for example because drawing with this Mode,
        and/or for this Source and destination classes,
        is not implemented yet.)

      @groupBegin }
    procedure DrawFrom(Source: TCastleImage; const X, Y: Integer;
      const Mode: TDrawMode = dmBlend);
    procedure DrawFrom(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode = dmBlend);
    procedure DrawTo(Destination: TCastleImage; const X, Y: Integer;
      const Mode: TDrawMode = dmBlend);
    { @groupEnd }

    { Draw the Source image on this image,
      dividing the source image into 3x3 parts for corners,
      sides, and inside.

      The source image is divided into 3 * 3 = 9 parts:

      @unorderedList(
        @item(4 corners, used to fill the corners of the DestinationRect.
          They are not stretched.)
        @item(4 sides, used to fill the sides of the DestinationRect
          between the corners. They are scaled in one dimension, to fill
          the space between corners completely.)
        @item(the inside. Used to fill the rectangular inside.
          Scaled in both dimensions as necessary.)
      )

      The size of corners is specified in the SourceCorners parameter,
      in the order: top, right, bottom, left.
      (It's the same order as e.g. CSS margins.)

      @italic(Warning: It is not efficient to use this drawing method.)
      This drawing is performed on CPU and cannot be fast.
      If possible, use instead @link(TDrawableImage),
      and either draw it to the screen by @link(TDrawableImage.Draw3x3),
      or draw it to another @link(TDrawableImage) by combining @link(TDrawableImage.Draw3x3)
      with @link(TDrawableImage.RenderToImageBegin).
      Using this method makes most sense in image manipulation tools,
      or during the loading / preparation stage of your game,
      not during actual game.
    }
    procedure DrawFrom3x3(const DestinationRect: TRectangle;
      const Source: TCastleImage; const SourceCorners: TVector4Integer;
      const DrawMode: TDrawMode;
      const Interpolation: TResizeInterpolation = riBilinear);

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
       const Weights: TVector4; const AColors: TVector4Pointer); virtual;

    { Copy size and contents from Source.
      This sets our size (Width, Height and Depth)
      to match Source image, and copies pixels from the Source image,
      converting them as closely as possible.
      For example, converting RGBA to RGB will strip alpha channel,
      but copy RGB values.

      When implementing descendants: the base implementation of this method
      in TCastleImage handles only the case when Image class equals our own class.
      And raises EImageAssignmentError in other cases.
      Override this method if you want to actually handle some conversions
      when assignning.

      @raises(EImageAssignmentError If it's not possible to convert from
        Source class to us. Not every possible conversion is implemented now.)
    }
    procedure Assign(const Source: TCastleImage); virtual;

    { Append code to embed this image inside Pascal source code. }
    procedure SaveToPascalCode(const ImageName: string;
      const ShowProgress: boolean;
      var CodeInterface, CodeImplementation, CodeInitialization, CodeFinalization: string);

    { Set the RGB colors for transparent pixels to the nearest non-transparent
      colors. This fixes problems with black/white borders around the texture
      regions, when the texture is scaled down (by any means --
      on CPU, during rendering on GPU...).

      @bold(The algorithm implemented here, for now, is really slow
      (but also really correct).)
      It should only be used as a last resort, when your normal tool
      (like Spine atlas packer) cannot do a decent job on a given image.
      You should use this when creating assets, and save the resulting
      image to disk (avoid doing this at runtime during the game,
      since it's really really slow).

      @groupBegin }
    procedure AlphaBleed(const ProgressTitle: string = ''); virtual;
    function MakeAlphaBleed(const ProgressTitle: string = ''): TCastleImage; virtual;
    { @groupEnd }
  public
    { Draw simple geometric shapes like circles, rectangles, lines, etc.
      @groupBegin }
    procedure FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aColor: TCastleColor); virtual;
    procedure Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aWidth: single; const aColor: TCastleColor); virtual;
    procedure FillRectangle(const x1, y1, x2, y2: single;
      const aColor: TCastleColor); virtual;
    procedure Rectangle(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); virtual;
    procedure Line(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); virtual;
    procedure FloodFill(const x, y: integer; const aColor: TCastleColor;
      const aThreshold: single = 0);
    { @groupEnd }
  end;

  TCastleImageList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TCastleImage>;

  TEncodedImageList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TEncodedImage>;

  { Possible compression of textures for GPU.
    The compressed texture formats may be automatically created for you by CGE,
    see https://castle-engine.io/creating_data_auto_generated_textures.php . }
  TTextureCompression = (
    { S3TC DXT1 compression, @bold(for opaque RGB images (no alpha channel)).
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC.
      It is also supported by a small number of Android devices.

      Note that the tcDxt1_RGB and tcDxt1_RGBA are the same compression method.
      Their behaviour only differs when rendering:
      in case of tcDxt1_RGB, the alpha information is not used,
      while in case of tcDxt1_RGBA, the renderer is using alpha-testing. }
    tcDxt1_RGB,

    { S3TC DXT1 compression, @bold(for RGBA images with simple yes/no alpha channel).
      The renderer will use alpha-testing when rendering such images.
      See above tcDxt1_RGB description for details. }
    tcDxt1_RGBA,

    { S3TC DXT3 compression, @bold(for RGBA images with full alpha channel),
      best for images with sharp alpha transitions.
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC. }
    tcDxt3,

    { S3TC DXT5 compression, @bold(for RGBA images with full alpha channel),
      best for images with smooth alpha transitions.
      This compression format is often supported by desktop OpenGL implementations.
      See http://en.wikipedia.org/wiki/S3_Texture_Compression about S3TC. }
    tcDxt5,

    { PowerVR texture compression (PVRTC) format.
      Supported by some Android and iOS devices,
      using PowerVR GPU by Imagination Technologies.
      See http://en.wikipedia.org/wiki/PVRTC . }
    tcPvrtc1_4bpp_RGB,
    tcPvrtc1_2bpp_RGB,
    tcPvrtc1_4bpp_RGBA,
    tcPvrtc1_2bpp_RGBA,
    tcPvrtc2_4bpp,
    tcPvrtc2_2bpp,

    { ATI texture compression format, @bold(for RGB images without alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGB,

    { ATI texture compression format, @bold(with sharp alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGBA_ExplicitAlpha,

    { ATI texture compression format, @bold(with smooth alpha).
      Supported by some Android devices (Adreno GPU from Qualcomm). }
    tcATITC_RGBA_InterpolatedAlpha,

    { ETC texture compression, @bold(without alpha).
      See http://en.wikipedia.org/wiki/Ericsson_Texture_Compression .
      Available on almost all Android OpenGLES 2.0 devices,
      unfortunately it doesn't support alpha channel. }
    tcETC1,
    { ASTC compression with alpha - should be available on all modern mobile GPU.
      See https://www.khronos.org/registry/OpenGL/extensions/KHR/KHR_texture_compression_astc_hdr.txt}
    tcASTC_4x4_RGBA,
    tcASTC_5x4_RGBA,
    tcASTC_5x5_RGBA,
    tcASTC_6x5_RGBA,
    tcASTC_6x6_RGBA,
    tcASTC_8x5_RGBA,
    tcASTC_8x6_RGBA,
    tcASTC_8x8_RGBA,
    tcASTC_10x5_RGBA,
    tcASTC_10x6_RGBA,
    tcASTC_10x8_RGBA,
    tcASTC_10x10_RGBA,
    tcASTC_12x10_RGBA,
    tcASTC_12x12_RGBA,
    tcASTC_4x4_SRGB8_ALPHA8,
    tcASTC_5x4_SRGB8_ALPHA8,
    tcASTC_5x5_SRGB8_ALPHA8,
    tcASTC_6x5_SRGB8_ALPHA8,
    tcASTC_6x6_SRGB8_ALPHA8,
    tcASTC_8x5_SRGB8_ALPHA8,
    tcASTC_8x6_SRGB8_ALPHA8,
    tcASTC_8x8_SRGB8_ALPHA8,
    tcASTC_10x5_SRGB8_ALPHA8,
    tcASTC_10x6_SRGB8_ALPHA8,
    tcASTC_10x8_SRGB8_ALPHA8,
    tcASTC_10x10_SRGB8_ALPHA8,
    tcASTC_12x10_SRGB8_ALPHA8,
    tcASTC_12x12_SRGB8_ALPHA8
  );
  TTextureCompressions = set of TTextureCompression;

  { Image compressed using one of the GPU texture compression algorithms. }
  TGPUCompressedImage = class(TEncodedImage)
  private
    FCompression: TTextureCompression;
    FSize: Cardinal;
  public
    constructor Create(const AWidth, AHeight, ADepth: Cardinal;
      const ACompression: TTextureCompression);

    property Compression: TTextureCompression read FCompression;

    { Size of the whole image data inside RawPixels, in bytes. }
    function Size: Cardinal; override;

    function HasAlpha: boolean; override;
    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    { Flip compressed image vertically, losslessly.

      This works only for S3TC images, and only when their height is 1, 2, 3
      or a multiple of 4. Note that this is always satisfied if image height
      is a power of two (as common for textures).
      It uses the knowledge of how S3TC compression works
      to losslessly flip the image, without re-compressing it.
      The idea is described here
      [http://users.telenet.be/tfautre/softdev/ddsload/explanation.htm]. }
    procedure FlipVertical; override;

    { Decompress the image.

      This uses DecompressTexture variable, so you have to initialialize it
      first (for example to CastleGLImages.GLDecompressTexture) before using this.

      @raises(ECannotDecompressTexture If we cannot decompress the texture,
        because decompressor is not set or there was some other error
        within decompressor.) }
    function Decompress: TCastleImage;

    { Create a new image object that has exactly the same class
      and the same data (size, pixels) as this image. }
    function MakeCopy: TGPUCompressedImage;

    { Create a new image object that has exactly the same class
      and the same data (size, pixels) as this image.

      Equivalent to MakeCopy, but virtual and declared as returning TEncodedImage class. }
    function CreateCopy: TEncodedImage; override;
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

{ Check is ImageClass one of the items in the ImageClasses array,
  or a descendant of one of them. }
function InImageClasses(ImageClass: TEncodedImageClass;
  const ImageClasses: array of TEncodedImageClass): boolean; overload;

{ Check is Image class one of the items in the ImageClasses array,
  or a descendant of one of them.
  This is a shortcut for InImageClasses(Image.ClassType, ImageClasses). }
function InImageClasses(Image: TEncodedImage;
  const ImageClasses: array of TEncodedImageClass): boolean; overload;

(*Check if both arrays contain exactly the same classes in the same order.

  May be extended in the future to do better checks and return true
  also if both array contain the same classes but in different order,
  and one array may contain the same classes duplicated any times.
  So the intention is that you should treat both arrays as sets
  (i.e. order of elements is ignored).

  The problem is that this function should be lighting fast.
  As the main purpose of it is to use it in constructions like
  setting property values, e.g.

  @longCode(#
    if ImageClassesArraysEqual(Value, SomeProperty) then
    begin
      SomeProperty := Value;
      { ... do some lengthy operations to update new value of SomeProperty ... }
    end;
  #)

  Doing smarter checks may cost us a little time.

  So for now this function returns
  @unorderedList(
    @item @true if for sure both arrays contain the same classes and
    @item @false if @italic(possibly) they don't contain the same classes.
  ) *)
function ImageClassesEqual(const Ar1, Ar2: array of TEncodedImageClass): boolean;

{ TCastleImage basic descendants --------------------------------------------- }

type
  TRGBAlphaImage = class;
  TRGBFloatImage = class;
  TGrayscaleImage = class;
  TGrayscaleAlphaImage = class;

  { Image with pixel represented as a TVector3Byte (red, green, blue). }
  TRGBImage = class(TCastleImage)
  private
    function GetPixels: PVector3Byte;
    function GetPixelsArray: PVector3ByteArray;
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    function ToFpImage: TInternalCastleFpImage; override;
  protected
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); override;
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3Byte. }
    property Pixels: PVector3Byte read GetPixels;
    property RGBPixels: PVector3Byte read GetPixels; deprecated 'use Pixels';
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3ByteArray. }
    property PixelsArray: PVector3ByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector3Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector3ByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure TransformRGB(const Matrix: TMatrix3); override;
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); override;

    { Create a new TRGBAlphaImage instance with RGB contents copied from this
      image, and alpha fully opaque. }
    function ToRGBAlphaImage: TRGBAlphaImage;

    { Convert image to an TRGBFloatImage format.

      Although float format offers superior precision compared to 8bit RGB,
      there is a slight chance of some unnoticeable loss of information
      in such conversion, since floating-point values are involved
      in calculation.

      But generally this conversion is relatively safe (contrary to
      conversion float -> 8-bit RGB, which must be lossy).

      But still you should note that doing such conversion has little
      sense since float format is useful only when you have colors that can't
      be expressed as simple 8-bit RGB. But by using this conversion
      you initially fill float image with data that does not have
      precision beyond standard 0..255 discreet range for each RGB component... }
    function ToRGBFloat: TRGBFloatImage;

    function ToGrayscale: TGrayscaleImage;

    { Draw horizontal line. Must be y1 <= y2, else it is NOOP. }
    procedure HorizontalLine(const x1, x2, y: Integer;
      const Color: TVector3Byte); overload;
    procedure HorizontalLine(const x1, x2, y: Integer;
      const Color: TCastleColor); overload;

    { Draw vertical line. Must be x1 <= x2, else it is NOOP. }
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TVector3Byte); overload;
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TCastleColor); overload;

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
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    procedure Assign(const Source: TCastleImage); override;

  public
    procedure FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aColor: TCastleColor); override;
    procedure Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure FillRectangle(const x1, y1, x2, y2: single;
      const aColor: TCastleColor); override;
    procedure Rectangle(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure Line(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
  end;

  TRGBAlphaImage = class(TCastleImage)
  private
    FPremultipliedAlpha: boolean;
    function GetPixels: PVector4Byte;
    function GetPixelsArray: PVector4ByteArray;
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    function ToFpImage: TInternalCastleFpImage; override;
  protected
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); override;
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector4Byte. }
    property Pixels: PVector4Byte read GetPixels;
    property AlphaPixels: PVector4Byte read GetPixels; deprecated 'use Pixels';
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector4ByteArray. }
    property PixelsArray: PVector4ByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector4Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector4ByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    { Set alpha channel on every pixel to the same given value. }
    procedure ClearAlpha(const Alpha: Byte);

    procedure TransformRGB(const Matrix: TMatrix3); override;
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
       const Weights: TVector4; const AColors: TVector4Pointer); override;

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

    procedure AlphaBleed(const ProgressTitle: string = ''); override;
    function MakeAlphaBleed(const ProgressTitle: string = ''): TCastleImage; override;
  public
    procedure FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aColor: TCastleColor); override;
    procedure Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure FillRectangle(const x1, y1, x2, y2: single;
      const aColor: TCastleColor); override;
    procedure Rectangle(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure Line(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
  end;

  { Image with high-precision RGB colors encoded as 3 floats. }
  TRGBFloatImage = class(TCastleImage)
  private
    function GetPixels: PVector3;
    function GetPixelsArray: PVector3Array;
  protected
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3. }
    property Pixels: PVector3 read GetPixels;
    property RGBFloatPixels: PVector3 read GetPixels; deprecated 'use Pixels';
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3Array. }
    property PixelsArray: PVector3Array read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector3;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector3Array;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); overload; override;
    function IsClear(const Pixel: TVector4Byte): boolean; overload; override;

    procedure Clear(const Pixel: TVector3); overload; reintroduce;
    function IsClear(const Pixel: TVector3): boolean; overload; reintroduce;

    { Converts TRGBFloatImage to TRGBImage.
      Colors in pixels are simply rounded using @link(Vector3Byte).
      So such conversion not only kills the floating-point
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
       const Weights: TVector4; const AColors: TVector4Pointer); override;
  end;

  { Grayscale image. Color is a simple Byte value. }
  TGrayscaleImage = class(TCastleImage)
  private
    FTreatAsAlpha: boolean;
    FColorWhenTreatedAsAlpha: TVector3Byte;
    function GetPixels: PByte;
    function GetPixelsArray: PByteArray;
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    function ToFpImage: TInternalCastleFpImage; override;
  protected
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); override;
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PByte. }
    property Pixels: PByte read GetPixels;
    property GrayscalePixels: PByte read GetPixels; deprecated 'use Pixels';
    { Pointer to pixels. Same as RawPixels, only typecasted to PByteArray. }
    property PixelsArray: PByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PByte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

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
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    { Should we treat grayscale image as pure alpha channel (without any color
      information) when using this as a texture.

      This property is meaningful only for a small subset of operations.

      @orderedList(
        @item(
          When creating OpenGL texture from this image.
          If @true, then the grayscale pixel data will be loaded as alpha channel
          contents (GL_ALPHA texture for OpenGL,
          it modifies only the fragments alpha value,
          it doesn't have any "color" in the normal sense).
          It is also the only way for TGrayscaleImage to return AlphaChannel <> acNone.)

        @item(
          When using @link(DrawFrom) / @link(DrawTo) methods.
          If @true, this image is drawn like an RGBA image,
          with constant RGB color ColorWhenTreatedAsAlpha, and alpha channel
          taken from contents of this image.)
      )
    }
    property TreatAsAlpha: boolean
      read FTreatAsAlpha write FTreatAsAlpha;

    property ColorWhenTreatedAsAlpha: TVector3Byte
      read FColorWhenTreatedAsAlpha write FColorWhenTreatedAsAlpha;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    procedure Assign(const Source: TCastleImage); override;
  public
    procedure FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aColor: TCastleColor); override;
    procedure Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure FillRectangle(const x1, y1, x2, y2: single;
      const aColor: TCastleColor); override;
    procedure Rectangle(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure Line(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
  end;

  { Grayscale image with an alpha channel.
    Each pixel is two bytes: grayscale + alpha. }
  TGrayscaleAlphaImage = class(TCastleImage)
  private
    function GetPixels: PVector2Byte;
    function GetPixelsArray: PVector2ByteArray;
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    function ToFpImage: TInternalCastleFpImage; override;
  protected
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); override;
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector2Byte. }
    property Pixels: PVector2Byte read GetPixels;
    property GrayscaleAlphaPixels: PVector2Byte read GetPixels; deprecated 'use Pixels';
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector2ByteArray. }
    property PixelsArray: PVector2ByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector2Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector2ByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure Clear(const Pixel: TVector2Byte); reintroduce;
    function IsClear(const Pixel: TVector2Byte): boolean; reintroduce;

    function HasAlpha: boolean; override;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    procedure Assign(const Source: TCastleImage); override;

    // TODO: this should be implemented, just like for TRGBAlphaImage
    //procedure AlphaBleed(const ProgressTitle: string = ''); override;
    //function MakeAlphaBleed(const ProgressTitle: string = ''): TCastleImage; override;
  public
    procedure FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aColor: TCastleColor); override;
    procedure Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure FillRectangle(const x1, y1, x2, y2: single;
      const aColor: TCastleColor); override;
    procedure Rectangle(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure Line(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
  end;

{ RGBE <-> 3 Single color conversion --------------------------------- }

{ Encode RGB color as Red + Green + Blue + Exponent format.
  This allows you to encode high-precision colors in 4 bytes,
  see ifRGBE image format for pointers why this is useful.

  Each component of V (red, green, blue) must be from range
  [0, +infinity), not merely from [0, 1].
  That is, V must have only nonnegative values. }
function Vector3ToRGBE(const v: TVector3): TVector4Byte;

{ Decode Red + Green + Blue + Exponent back into RGB (3 floats). }
function VectorRGBETo3Single(const v: TVector4Byte): TVector3;

{ File formats managing ----------------------------------------------------- }

{ Does this MIME type correspond to image. }
function IsImageMimeType(const MimeType: string;
  const OnlyLoadable, OnlySaveable: boolean): boolean;

{ List available image file formats.

  This is basically for debug/info purposes, you can show this to user
  to let him know which formats are supported (and by which extensions
  they are recognized). Although almost always a better way to show
  this to user is just to use SaveImage_FileFilters with a save dialog
  like TCastleWindowBase.FileDialog,
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

{ Guess MIME type from image extension. Empty string if cannot guess. }
function ImageExtToMimeType(Ext: string): string; deprecated 'use URIMimeType';

{ loading image -------------------------------------------------------------- }

type
  { }
  EImageLoadError = class(Exception);
  EInvalidImageFormat = class(EImageLoadError);
  EUnableToLoadImage = class(EImageLoadError);

  EImageFormatNotSupported = class(Exception);

type
  TLoadImageOption = (liFlipVertically);
  TLoadImageOptions = set of TLoadImageOption;

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

  @raises(EImageLoadError If the image file cannot be processed for some reason.)

  @raises(Exception Various TStream instances (used internally by the
    @link(Download) function) may raise exceptions in case the stream cannot
    be created for reading.
    Right now, we simply let these exceptions to "pass through" from this function
    (instead of catching and re-raising).
    So be ready that this function may raise @italic(any) Exception class.
    In case of local files (file:// URLs), the typical exception class
    is EFOpenError.)

  @seealso LoadEncodedImage

  @groupBegin *)
function LoadImage(Stream: TStream; const MimeType: string;
  const AllowedImageClasses: array of TEncodedImageClass)
  :TCastleImage; overload;

function LoadImage(const URL: string): TCastleImage; overload;
function LoadImage(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass)
  :TCastleImage; overload;
function LoadImage(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass;
  const ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation = riBilinear;
  const Options: TLoadImageOptions = []): TCastleImage; overload;
{ @groupEnd }

{ Load image to TEncodedImage format.
  This allows loading image compressed with GPU, which is good for optimally
  loading it to GPU. However, the operations on GPU-compressed image are very
  limited, we generally cannot do much with GPU-compressed data except
  rendering it.

  @seealso LoadImage

  @groupBegin }
function LoadEncodedImage(Stream: TStream; const MimeType: string;
  const AllowedImageClasses: array of TEncodedImageClass;
  const Options: TLoadImageOptions = [])
  :TEncodedImage; overload;
function LoadEncodedImage(const URL: string;
  const Options: TLoadImageOptions = []): TEncodedImage; overload;
function LoadEncodedImage(URL: string;
  const AllowedImageClasses: array of TEncodedImageClass;
  const Options: TLoadImageOptions = [])
  :TEncodedImage; overload;
{ @groupEnd }

{ saving image --------------------------------------------------------------- }

type
  { Raised by SaveImage when it's not possible to save image. }
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
procedure SaveImage(const img: TEncodedImage; const MimeType: string; Stream: TStream); overload;
procedure SaveImage(const Img: TEncodedImage; const URL: string); overload;
{ @groupEnd }

{ Other TCastleImage processing ---------------------------------------------------- }

{ Choose TCastleImage descendant best matching for this image file format.
  The only purpose of this for now is to pick TRGBFloatImage for RGBE files,
  chooses TRGBImage for anything else.

  For the overloaded version with URL, file format is determined
  by guessing based on file extension.

  @groupBegin }
function ImageClassBestForSavingToFormat(const URL: string): TCastleImageClass;
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
procedure AlphaMaxVar(var A: TAlphaChannel; const B: TAlphaChannel);

function StringToAlpha(S: string; var WarningDone: boolean): TAutoAlphaChannel;

const
  AlphaToString: array [TAutoAlphaChannel] of string =
  ('AUTO', 'NONE', 'TEST', 'BLENDING');

type
  TTextureCompressionInfo = object
    Name: string;
    RequiresPowerOf2: boolean;
    AlphaChannel: TAlphaChannel;

    { When generating to DDS (that has reverted row order with respect to OpenGL),
      most of the compressed textures should be stored as flipped.
      When reading, we expect them to be already flipped.
      When loading to OpenGL, they will effectively be flipped again
      (since OpenGL expects bottom-to-top order, while we load it
      image in top-to-bottom order), thus making the image correct.

      The exceptions are DXT* formats, that we can read correctly (unflipped)
      from DDS.

      This is only a limitation of the DDS format.

      For KTX, we can generate KTX images using PowerVR Texture Tools
      that already have a correct (bottom-to-top) orientation.
      So we can have textures compressed to PVRTC1_4bpp_RGB
      with correct orientation. }
    DDSFlipped: boolean;
  end;

const
  TextureCompressionInfo: array [TTextureCompression] of TTextureCompressionInfo =
  ( (Name: 'DXT1_RGB'                    ; RequiresPowerOf2: false; AlphaChannel: acNone    ; DDSFlipped: false),
    (Name: 'DXT1_RGBA'                   ; RequiresPowerOf2: false; AlphaChannel: acTest    ; DDSFlipped: false),
    (Name: 'DXT3'                        ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: false),
    (Name: 'DXT5'                        ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: false),
    { See http://community.imgtec.com/files/pvrtc-texture-compression-user-guide/
      "PVRTC2 vs PVRTC1" section --- PVRTC1 require power-of-two. } { }
    (Name: 'PVRTC1_4bpp_RGB'             ; RequiresPowerOf2: true ; AlphaChannel: acNone    ; DDSFlipped: true),
    (Name: 'PVRTC1_2bpp_RGB'             ; RequiresPowerOf2: true ; AlphaChannel: acNone    ; DDSFlipped: true),
    (Name: 'PVRTC1_4bpp_RGBA'            ; RequiresPowerOf2: true ; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'PVRTC1_2bpp_RGBA'            ; RequiresPowerOf2: true ; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'PVRTC2_4bpp'                 ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'PVRTC2_2bpp'                 ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    { Tests show that ATITC does not need power-of-two sizes. }
    (Name: 'ATITC_RGB'                   ; RequiresPowerOf2: false; AlphaChannel: acNone    ; DDSFlipped: true),
    (Name: 'ATITC_RGBA_ExplicitAlpha'    ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ATITC_RGBA_InterpolatedAlpha'; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    { TODO: unconfirmed RequiresPowerOf2 for ETC1. } { }
    (Name: 'ETC1'                        ; RequiresPowerOf2: true ; AlphaChannel: acNone    ; DDSFlipped: true),

    (Name: 'ASTC_4x4_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_5x4_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_5x5_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_6x5_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_6x6_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_8x5_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_8x6_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_8x8_RGBA'               ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x5_RGBA'              ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x6_RGBA'              ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x8_RGBA'              ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x10_RGBA'             ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_12x10_RGBA'             ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_12x12_RGBA'             ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_4x4_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_5x4_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_5x5_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_6x5_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_6x6_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_8x5_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_8x6_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_8x8_SRGB8_ALPHA8'       ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x5_SRGB8_ALPHA8'      ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x6_SRGB8_ALPHA8'      ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x8_SRGB8_ALPHA8'      ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_10x10_SRGB8_ALPHA8'     ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_12x10_SRGB8_ALPHA8'     ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true),
    (Name: 'ASTC_12x12_SRGB8_ALPHA8'     ; RequiresPowerOf2: false; AlphaChannel: acBlending; DDSFlipped: true)
  );

{ Convert TTextureCompression enum to string. }
function TextureCompressionToString(const TextureCompression: TTextureCompression): string;

{ Convert string to TTextureCompression enum. Possible values correspond
  to names listed in TextureCompressionInfo array, they are also equal
  to enum Pascal names without leading "tc".
  Compares given strig ignoring the case.
  @raises(Exception If the string value does not name any
    TTextureCompression value.) }
function StringToTextureCompression(const S: string): TTextureCompression;

type
  { Listener type for @link(AddLoadImageListener). }
  TLoadImageEvent = procedure (var ImageUrl: string) of object;

var
  { Is the value of @link(SupportedTextureCompression) determined
    by the renderer (like OpenGL context) parameters. }
  SupportedTextureCompressionKnown: boolean;

  { Which texture compression values are supported by
    the renderer (like OpenGL context). }
  SupportedTextureCompression: TTextureCompressions;

{ All image URLs are processed by this event before loading.
  This allows to globally modify / observe your images paths,
  e.g. to use GPU compressed alternative versions.

  The URL processing is automatically used by
  @link(LoadImage), @link(LoadEncodedImage),
  @link(TCompositeImage.LoadFromFile).

  The URL processing is automatically registered by
  @link(TMaterialProperties MaterialProperties)
  to automatically use GPU compressed textures.
  See https://castle-engine.io/creating_data_material_properties.php .
  You can also use it yourself, instead or in addition
  to @link(TMaterialProperties MaterialProperties) processing.

  @italic(An example:) To work on any GPU, you want to have various
  versions of your textures (uncompressed, and also compressed with
  various GPU algorithms) in your data.
  Use this procedure to redirect all image loading to use your
  compressed versions, when they are supported by the GPU.
  By doing it like this we capture all kinds of image loading --- from TDrawableImage,
  from TCastleScene and so on.

  @longCode(#
    uses ..., CastleURIUtils, CastleGLUtils, CastleLog, CastleStringUtils,
      CastleFilesUtils;

    procedure TTextureUtils.GPUTextureAlternative(var ImageUrl: string);
    begin
      if IsPrefix('castle-data:/animation/dragon/', ImageUrl) then
      begin
        if GLFeatures = nil then
          WritelnWarning('TextureCompression', 'Cannot determine whether to use GPU compressed version for ' + ImageUrl + ' because the image is loaded before GPU capabilities are known') else
        if tcPvrtc1_4bpp_RGBA in GLFeatures.TextureCompression then
        begin
          ImageUrl := ExtractURIPath(ImageUrl) + 'compressed/pvrtc1_4bpp_rgba/' +
            ExtractURIName(ImageUrl) + '.dds';
          WritelnLog('TextureCompression', 'Using compressed alternative ' + ImageUrl);
        end;
      end;
    end;

    initialization
      AddLoadImageListener(@TTextureUtils(nil).GPUTextureAlternative);
    finalization
      RemoveLoadImageListener(@GPUTextureAlternative);
    end.
  #)
}
procedure AddLoadImageListener(const Event: TLoadImageEvent);

{ Remove listener added by @link(AddLoadImageListener). }
procedure RemoveLoadImageListener(const Event: TLoadImageEvent);

{ Process URL through events registered by @link(AddLoadImageListener).
  This is used internally by the engine. }
function ProcessImageUrl(const URL: string): string;

{$undef read_interface}

implementation

uses ExtInterpolation, FPCanvas, FPImgCanv,
  CastleProgress, CastleStringUtils, CastleFilesUtils, CastleLog,
  CastleCompositeImage, CastleDownload, CastleURIUtils, CastleTimeUtils;

{ parts ---------------------------------------------------------------------- }

{$I castleimages_file_formats.inc}
{$I castleimages_draw.inc}
{$I castleimages_paint.inc}
{$I castleimages_bmp.inc}
{$I castleimages_libpng.inc}
{$I castleimages_fpimage.inc}
{$I castleimages_png.inc} // must be included after castleimages_libpng.inc and castleimages_fpimage.inc
{$I castleimages_ppm.inc}
{$I castleimages_ipl.inc}
{$I castleimages_rgbe_fileformat.inc}
{$I castleimages_external_tool.inc}
{$I castleimages_composite.inc}

{ Colors ------------------------------------------------------------------ }

function EqualRGB(const Color1, Color2: TVector3Byte; Tolerance: Byte): boolean;
begin
 result:=(Abs(Smallint(Color1.Data[0]) - Color2.Data[0]) <= Tolerance) and
         (Abs(Smallint(Color1.Data[1]) - Color2.Data[1]) <= Tolerance) and
         (Abs(Smallint(Color1.Data[2]) - Color2.Data[2]) <= Tolerance);
end;

{ TEncodedImage -------------------------------------------------------------- }

destructor TEncodedImage.Destroy;
begin
  FreeMemNiling(FRawPixels);
  inherited;
end;

function TEncodedImage.Dimensions: TVector3Cardinal;
begin
  Result.Data[0] := Width;
  Result.Data[1] := Height;
  Result.Data[2] := Depth;
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

procedure TEncodedImage.NotImplemented(const AMethodName: string);
begin
  raise EInternalError.Create(AMethodName +
    ' method not implemented for the image class ' + ClassName);
end;

{ TCastleImage --------------------------------------------------------------- }

constructor TCastleImage.Create;
begin
  inherited;
  { Everything is already initialized to nil and 0. }
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

procedure TCastleImage.InvertColors;
begin
  NotImplemented('InvertColors');
end;

procedure TCastleImage.SetColorRGB(const X, Y: Integer; const V: TVector3);
begin
  SetColors(X, Y, 0, Vector4(V, 1));
end;

function TCastleImage.GetColors(const X, Y, Z: Integer): TCastleColor;
begin
  NotImplemented('GetColor');
  Result := TVector4.Zero; // silence warning
end;

procedure TCastleImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
begin
  NotImplemented('SetColor');
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
  Result := CreateCopy as TCastleImage;
end;

function TCastleImage.CreateCopy: TEncodedImage;
begin
  Result := TCastleImageClass(Self.ClassType).Create(Width, Height, Depth);
  Move(RawPixels^, Result.RawPixels^, Size);
  Result.FURL := URL;
end;

type
  TMixColorsFunction = procedure (const OutputColor: Pointer;
    const Weights: TVector4; const AColors: TVector4Pointer) of object;

{ This does the real resizing work.
  It assumes that SourceData and DestinData pointers are already allocated.
  DestinWidth, DestinHeight must not be 0. }
procedure InternalResize(PixelSize: Cardinal;
  const SourceData: Pointer; const SourceRect: TRectangle; const SourceWidth, SourceHeight: Cardinal;
  const DestinData: Pointer; const DestinRect: TRectangle; const DestinWidth, DestinHeight: Cardinal;
  const Interpolation: TResizeInterpolationInternal;
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
    Weights: TVector4;
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

      SourceX1 := SourceX1 * PixelSize;
      SourceX2 := SourceX2 * PixelSize;

      SourceXFrac := Frac(SourceXFrac);
      Weights.Data[0] := SourceXFrac * SourceYFrac;
      Colors.Data[0] := Pointer(PtrUInt(Source2Row + SourceX2));
      Weights.Data[1] := (1 - SourceXFrac) * SourceYFrac;
      Colors.Data[1] := Pointer(PtrUInt(Source2Row + SourceX1));
      Weights.Data[2] := (1 - SourceXFrac) * (1 - SourceYFrac);
      Colors.Data[2] := Pointer(PtrUInt(Source1Row + SourceX1));
      Weights.Data[3] :=  SourceXFrac * (1 - SourceYFrac);
      Colors.Data[3] := Pointer(PtrUInt(Source1Row + SourceX2));
      MixColors(Pointer(PtrUInt(DestinRow + DestinX * PixelSize)), Weights, Colors);
    end;
  end;

{$ifdef CASTLE_OBJFPC}

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

{$else}

  { In Delphi mode, nested procedural variables are not supported.
    Just define a local MakeLine procedure. }
  procedure MakeLine;
  begin
    case Interpolation of
      riNearest : MakeLineNearest;
      riBilinear: MakeLineBilinear;
      else raise EInternalError.Create('Unknown Interpolation for InternalResize');
    end;
  end;

begin
{$endif}
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
  NewFpImage: TInternalCastleFpImage;
begin
  if (Interpolation >= Low(TResizeInterpolationFpImage)) and
     (Interpolation <= High(TResizeInterpolationFpImage)) then
  begin
    // TODO; ProgressTitle not supported for this
    NewFpImage := MakeResizedToFpImage(ResizeWidth, ResizeHeight, Interpolation);
    try
      FromFpImage(NewFpImage);
    finally FreeAndNil(NewFpImage) end;
  end else
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
        NewPixels, CastleRectangles.Rectangle(0, 0, ResizeWidth, ResizeHeight), ResizeWidth, ResizeHeight,
        Interpolation, {$ifdef CASTLE_OBJFPC}@{$endif} MixColors, ProgressTitle);
      FreeMemNiling(FRawPixels);

      FRawPixels := NewPixels;
      FWidth := ResizeWidth;
      FHeight := ResizeHeight;
    end;
  end;
end;

function TCastleImage.MakeResized(ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation;
  const ProgressTitle: string): TCastleImage;
var
  NewFpImage: TInternalCastleFpImage;
begin
  if (Interpolation >= Low(TResizeInterpolationFpImage)) and
     (Interpolation <= High(TResizeInterpolationFpImage)) then
  begin
    // TODO; ProgressTitle not supported for this
    NewFpImage := MakeResizedToFpImage(ResizeWidth, ResizeHeight, Interpolation);
    try
      Result := CreateFromFpImage(NewFpImage, [TCastleImageClass(ClassType)]);
    finally FreeAndNil(NewFpImage) end;
  end else
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
          Interpolation, {$ifdef CASTLE_OBJFPC}@{$endif} MixColors, ProgressTitle);
    except Result.Free; raise end;
  end;
end;

procedure TCastleImage.Resize3x3(const ResizeWidth, ResizeHeight: Cardinal;
  var Corners: TVector4Integer;
  const Interpolation: TResizeInterpolationInternal);
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
    SourceRect := CastleRectangles.Rectangle(SourceXs.Data[X], SourceYs.Data[Y],
      SourceXs.Data[Integer(X) + 1] - SourceXs.Data[X],
      SourceYs.Data[Integer(Y) + 1] - SourceYs.Data[Y]);
    DestRect := CastleRectangles.Rectangle(DestXs.Data[X], DestYs.Data[Y],
      DestXs.Data[Integer(X) + 1] - DestXs.Data[X],
      DestYs.Data[Integer(Y) + 1] - DestYs.Data[Y]);
    InternalResize(PixelSize,
      RawPixels, SourceRect, Width, Height,
      NewPixels, DestRect, ResizeWidth, ResizeHeight,
      Interpolation, {$ifdef CASTLE_OBJFPC}@{$endif} MixColors, '');
  end;

var
  X, Y: TPart;
begin
  if (ResizeWidth <> Width) or (ResizeHeight <> Height) then
  begin
    NewCorners.Data[0] := Corners.Data[0] * ResizeWidth div Width;
    NewCorners.Data[1] := Corners.Data[1] * ResizeHeight div Height;
    NewCorners.Data[2] := Corners.Data[2] * ResizeWidth div Width;
    NewCorners.Data[3] := Corners.Data[3] * ResizeHeight div Height;

    if not ( (Corners.Data[3] + Corners.Data[1] < Width) and
             (Corners.Data[2] + Corners.Data[0] < Height) and
             (NewCorners.Data[3] + NewCorners.Data[1] < ResizeWidth) and
             (NewCorners.Data[2] + NewCorners.Data[0] < ResizeHeight) ) then
      raise Exception.CreateFmt('TCastleImage.Resize3x3: Cannot resize image with corners because corners are larger then image size. Source corners: %s, source size: %dx%d, destination corners: %s, destination size: %dx%d',
        [Corners.ToString, Width, Height,
         NewCorners.ToString, ResizeWidth, ResizeHeight]);

    SourceXs.Data[0] := 0;
    SourceXs.Data[1] := Corners.Data[3];
    SourceXs.Data[2] := Width - Corners.Data[1];
    SourceXs.Data[3] := Width;

    SourceYs.Data[0] := 0;
    SourceYs.Data[1] := Corners.Data[2];
    SourceYs.Data[2] := Height - Corners.Data[0];
    SourceYs.Data[3] := Height;

    DestXs.Data[0] := 0;
    DestXs.Data[1] := NewCorners.Data[3];
    DestXs.Data[2] := ResizeWidth - NewCorners.Data[1];
    DestXs.Data[3] := ResizeWidth;

    DestYs.Data[0] := 0;
    DestYs.Data[1] := NewCorners.Data[2];
    DestYs.Data[2] := ResizeHeight - NewCorners.Data[0];
    DestYs.Data[3] := ResizeHeight;

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
  if Angle < 0 then Angle := Angle + 4;

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
      for x := 0 to (Width - 1) div 2 do
      begin
        Pix1 := PointerAdd(ImageRow, Cardinal(x) * PixelSize);
        Pix2 := PointerAdd(ImageRow, (Width - 1 - Cardinal(x)) * PixelSize);
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

procedure TCastleImage.TransformRGB(const Matrix: TMatrix3);
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
  ModulateRGB({$ifdef CASTLE_OBJFPC}@{$endif} ColorGrayscaleByte);
end;

procedure TCastleImage.ConvertToChannelRGB(Channel: Integer);
begin
  case Channel of
    0: ModulateRGB({$ifdef CASTLE_OBJFPC}@{$endif} ColorRedConvertByte);
    1: ModulateRGB({$ifdef CASTLE_OBJFPC}@{$endif} ColorGreenConvertByte);
    2: ModulateRGB({$ifdef CASTLE_OBJFPC}@{$endif} ColorBlueConvertByte);
    else raise EInternalError.Create(
      'ConvertToChannelRGB: Channel must be 0, 1 or 2');
  end;
end;

procedure TCastleImage.StripToChannelRGB(Channel: Integer);
begin
  case Channel of
    0: ModulateRGB({$ifdef CASTLE_OBJFPC}@{$endif} ColorRedStripByte);
    1: ModulateRGB({$ifdef CASTLE_OBJFPC}@{$endif} ColorGreenStripByte);
    2: ModulateRGB({$ifdef CASTLE_OBJFPC}@{$endif} ColorBlueStripByte);
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
  const Weights: TVector4; const AColors: TVector4Pointer);
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

  { TODO: one day, this should just call
      DrawFrom(Source, 0, 0, Source.Width, Source.Height, dmOverwrite);
    instead of raising EImageAssignmentError.

    TCastleImage.Assign will not even need to be virtual then.

    However, for now, DrawFrom and friends do not handle the Z coordinate
    (they only draw from/to Z = 0). }
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

  CodeInterface := CodeInterface +
    'function ' +ImageName+ ': ' +ClassName+ ';' +nl + nl;

  CodeImplementation := CodeImplementation +
    'var' + NL +
    '  F' +ImageName+ ': ' +ClassName+ ';' + NL +
    'const' + NL +
    '  ' +NameWidth+ ' = ' +IntToStr(Width)+ ';' + NL +
    '  ' +NameHeight+ ' = ' +IntToStr(Height)+ ';' + NL +
    '  ' +NameDepth+ ' = ' +IntToStr(Depth)+ ';' + NL +
    '  ' +NamePixels+ ': array[0 .. '
      +NameWidth+ ' * '
      +NameHeight+ ' * '
      +NameDepth+ ' * '
      +IntToStr(PixelSize) + ' - 1] of Byte = (' + NL +
    '    ';

  if ShowProgress then
    Progress.Init((Size - 1) div 12,
      Format('Generating %s (%s, alpha: %s)',
        [ImageName, ClassName, AlphaToString[AlphaChannel]]));

  pb := PByte(RawPixels);
  for I := 1 to Size - 1 do
  begin
    CodeImplementation := CodeImplementation + Format('%4d,', [pb^]);
    if (i mod 12) = 0 then
    begin
      CodeImplementation := CodeImplementation + NL + '    ';
      if ShowProgress then Progress.Step;
    end else
      CodeImplementation := CodeImplementation + ' ';
    Inc(pb);
  end;
  CodeImplementation := CodeImplementation +
    Format('%4d);', [pb^]) + NL +
    NL +
    'function ' +ImageName+ ': ' +ClassName+ ';' + NL +
    'begin' + NL +
    '  if F' +ImageName + ' = nil then' + NL +
    '  begin' + NL +
    '    F' +ImageName+ ' := ' +ClassName+ '.Create(' +NameWidth+', ' +NameHeight+ ', ' +NameDepth+ ');' + NL +
    '    Move(' +NamePixels+ ', F' +ImageName+ '.RawPixels^, SizeOf(' +NamePixels+ '));' + NL +
    '    F' +ImageName+ '.URL := ''embedded-image:/' +ImageName+ ''';' + NL +
    '  end;' + NL +
    '  Result := F' +ImageName+ ';' + NL +
    'end;' + NL +
    NL +
    '';

  if ShowProgress then Progress.Fini;

  CodeFinalization := CodeFinalization +
    '  FreeAndNil(F' +ImageName+ ');' +nl;
end;

procedure TCastleImage.AlphaBleed(const ProgressTitle: string);
begin
  { default implementation does nothing.
    This is OK for images without alpha channel. }
end;

function TCastleImage.MakeAlphaBleed(const ProgressTitle: string): TCastleImage;
begin
  { default implementation returns a copy.
    This is OK for images without alpha channel. }
  Result := MakeCopy;
end;

procedure TCastleImage.DrawFrom3x3(const DestinationRect: TRectangle;
  const Source: TCastleImage; const SourceCorners: TVector4Integer;
  const DrawMode: TDrawMode; const Interpolation: TResizeInterpolation);

  procedure Draw(
    const DestX, DestY, DestWidth, DestHeight: Integer;
    const SourceX, SourceY, SourceWidth, SourceHeight: Integer);
  var
    Temp: TCastleImage;
  begin
    if (DestWidth = SourceWidth) and (DestHeight = SourceHeight) then
      { Optimized version -- no need to scale.
        This should happen at least for 4 corners. }
      DrawFrom(Source, DestX, DestY, SourceX, SourceY, SourceWidth, SourceHeight, DrawMode)
    else
    begin
      // create Temp with the same type as Source, to make Draw with dmOverwrite fast
      Temp := TCastleImageClass(Source.ClassType).Create(SourceWidth, SourceHeight);
      try
        Temp.DrawFrom(Source, 0, 0, SourceX, SourceY, SourceWidth, SourceHeight, dmOverwrite);
        Temp.Resize(DestWidth, DestHeight, Interpolation);
        DrawFrom(Temp, DestX, DestY, 0, 0, DestWidth, DestHeight, DrawMode);
      finally FreeAndNil(Temp) end;
    end;
  end;

var
  CornerTop, CornerRight, CornerBottom, CornerLeft: Integer;
  DestWidth, DestHeight: Integer;
  XDestLeft, XDestRight, YDestBottom, YDestTop,
    HorizontalDestSize, VerticalDestSize: Integer;
  XSourceLeft, XSourceRight, YSourceBottom, YSourceTop,
    HorizontalSourceSize, VerticalSourceSize: Integer;
begin
  CornerTop := SourceCorners[0];
  CornerRight := SourceCorners[1];
  CornerBottom := SourceCorners[2];
  CornerLeft := SourceCorners[3];

  DestWidth := DestinationRect.Width;
  DestHeight := DestinationRect.Height;

  if not ( (CornerLeft + CornerRight < Source.Width) and
           (CornerLeft + CornerRight < DestWidth) and
           (CornerBottom + CornerTop < Source.Height) and
           (CornerBottom + CornerTop < DestHeight)) then
  begin
    WritelnLog('TCastleImage.Dest3x3', 'Image corners are too large to draw it: corners are %d %d %d %d, image size is %d %d, draw area size is %d %d',
      [CornerTop, CornerRight, CornerBottom, CornerLeft,
       Source.Width, Source.Height,
       DestWidth, DestHeight]);
    Exit;
  end;

  XDestLeft := DestinationRect.Left;
  XSourceLeft := 0;
  XDestRight := DestinationRect.Left + DestWidth - CornerRight;
  XSourceRight :=                   Source.Width - CornerRight;

  YDestBottom := DestinationRect.Bottom;
  YSourceBottom := 0;
  YDestTop := DestinationRect.Bottom + DestHeight - CornerTop;
  YSourceTop :=                     Source.Height - CornerTop;

  { 4 corners }
  Draw(XDestLeft  , YDestBottom  , CornerLeft, CornerBottom,
       XSourceLeft, YSourceBottom, CornerLeft, CornerBottom);
  Draw(XDestRight  , YDestBottom  , CornerRight, CornerBottom,
       XSourceRight, YSourceBottom, CornerRight, CornerBottom);
  Draw(XDestRight  , YDestTop  , CornerRight, CornerTop,
       XSourceRight, YSourceTop, CornerRight, CornerTop);
  Draw(XDestLeft  , YDestTop  , CornerLeft, CornerTop,
       XSourceLeft, YSourceTop, CornerLeft, CornerTop);

  { 4 sides }
  HorizontalDestSize    := DestWidth    - CornerLeft - CornerRight;
  HorizontalSourceSize  := Source.Width - CornerLeft - CornerRight;
  VerticalDestSize    := DestHeight    - CornerTop - CornerBottom;
  VerticalSourceSize  := Source.Height - CornerTop - CornerBottom;

  Draw(XDestLeft   + CornerLeft, YDestBottom  , HorizontalDestSize,   CornerBottom,
       XSourceLeft + CornerLeft, YSourceBottom, HorizontalSourceSize, CornerBottom);
  Draw(XDestLeft   + CornerLeft, YDestTop  , HorizontalDestSize,   CornerTop,
       XSourceLeft + CornerLeft, YSourceTop, HorizontalSourceSize, CornerTop);

  Draw(XDestLeft   , YDestBottom   + CornerBottom, CornerLeft, VerticalDestSize,
        XSourceLeft, YSourceBottom + CornerBottom, CornerLeft, VerticalSourceSize);
  Draw(XDestRight   , YDestBottom   + CornerBottom, CornerRight, VerticalDestSize,
        XSourceRight, YSourceBottom + CornerBottom, CornerRight, VerticalSourceSize);

  Draw(DestinationRect.Left + CornerLeft, DestinationRect.Bottom + CornerBottom, HorizontalDestSize, VerticalDestSize,
                              CornerLeft,                          CornerBottom, HorizontalSourceSize, VerticalSourceSize);
end;

{ TGPUCompressedImage ----------------------------------------------------------------- }

constructor TGPUCompressedImage.Create(
  const AWidth, AHeight, ADepth: Cardinal;
  const ACompression: TTextureCompression);
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

    { size formula from
      https://www.khronos.org/registry/OpenGL/extensions/KHR/KHR_texture_compression_astc_hdr.txt }
    tcASTC_4x4_RGBA, tcASTC_4x4_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 16;
    tcASTC_5x4_RGBA, tcASTC_5x4_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 5) * DivRoundUp(FHeight, 4) * 16;
    tcASTC_5x5_RGBA, tcASTC_5x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 5) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_6x5_RGBA, tcASTC_6x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 6) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_6x6_RGBA, tcASTC_6x6_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 6) * DivRoundUp(FHeight, 6) * 16;
    tcASTC_8x5_RGBA, tcASTC_8x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_8x6_RGBA, tcASTC_8x6_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 6) * 16;
    tcASTC_8x8_RGBA, tcASTC_8x8_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 8) * 16;
    tcASTC_10x5_RGBA, tcASTC_10x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_10x6_RGBA, tcASTC_10x6_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 6) * 16;
    tcASTC_10x8_RGBA, tcASTC_10x8_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 8) * 16;
    tcASTC_10x10_RGBA, tcASTC_10x10_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 10) * 16;
    tcASTC_12x10_RGBA, tcASTC_12x10_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 12) * DivRoundUp(FHeight, 10) * 16;
    tcASTC_12x12_RGBA, tcASTC_12x12_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 12) * DivRoundUp(FHeight, 12) * 16;

    else raise EInvalidDDS.CreateFmt('Cannot calculate size for texture compressed with %s',
      [TextureCompressionInfo[Compression].Name]);
  end;

  FRawPixels := GetMem(FSize);
end;

function TGPUCompressedImage.Size: Cardinal;
begin
  Result := FSize;
end;

function TGPUCompressedImage.HasAlpha: boolean;
begin
  Result := TextureCompressionInfo[Compression].AlphaChannel <> acNone;
end;

function TGPUCompressedImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
begin
  { Compressed data doesn't analyze alpha channel, instead
    we determine alpha channel from the compression type. }
  Result := TextureCompressionInfo[Compression].AlphaChannel;
end;

{$I castleimages_s3tc_flip_vertical.inc}

function TGPUCompressedImage.Decompress: TCastleImage;
begin
  if Assigned(DecompressTexture) then
    Result := DecompressTexture(Self) else
    raise ECannotDecompressTexture.Create('Cannot decompress GPU-compressed texture: no decompressor initialized');
end;

function TGPUCompressedImage.MakeCopy: TGPUCompressedImage;
begin
  Result := CreateCopy as TGPUCompressedImage;
end;

function TGPUCompressedImage.CreateCopy: TEncodedImage;
begin
  Result := TGPUCompressedImage.Create(Width, Height, Depth, Compression);
  Assert(Result.Size = Size);
  Move(RawPixels^, Result.RawPixels^, Size);
  Result.URL := URL;
end;

{ TCastleImageClass and arrays of TCastleImageClasses ----------------------------- }

function InImageClasses(ImageClass: TEncodedImageClass;
  const ImageClasses: array of TEncodedImageClass): boolean;
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

function InImageClasses(Image: TEncodedImage;
  const ImageClasses: array of TEncodedImageClass): boolean;
begin
  Result := InImageClasses(TEncodedImageClass(Image.ClassType), ImageClasses);
end;

function ImageClassesEqual(const Ar1, Ar2: array of TEncodedImageClass): boolean;
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

  Map := MapImage.Pixels;
  White := ReplaceWhiteImage.Pixels;
  Black := ReplaceBlackImage.Pixels;
  Res := Pixels;

  for i := 1 to Width * Height * Depth do
  begin
    s := (Map^.Data[0] + Map^.Data[1] + Map^.Data[2]) / 255 / 3;
    Res^.Data[0] := Round(s * White^.Data[0] + (1-s) * Black^.Data[0]);
    Res^.Data[1] := Round(s * White^.Data[1] + (1-s) * Black^.Data[1]);
    Res^.Data[2] := Round(s * White^.Data[2] + (1-s) * Black^.Data[2]);
    Inc(Map);
    Inc(White);
    Inc(Black);
    Inc(Res);
  end;
end;

function TRGBImage.GetPixels: PVector3Byte;
begin
  Result := PVector3Byte(RawPixels);
end;

function TRGBImage.GetPixelsArray: PVector3ByteArray;
begin
  Result := PVector3ByteArray(RawPixels);
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

function TRGBImage.RowPtr(const Y, Z: Cardinal): PVector3ByteArray;
begin
  Result := PVector3ByteArray(inherited RowPtr(Y, Z));
end;

procedure TRGBImage.InvertColors;
var
  i: Cardinal;
  prgb: PVector3Byte;
begin
  prgb := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    prgb^.Data[0] := High(byte) - prgb^.Data[0];
    prgb^.Data[1] := High(byte) - prgb^.Data[1];
    prgb^.Data[2] := High(byte) - prgb^.Data[2];
    Inc(prgb);
  end;
end;

function TRGBImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector3Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.Data[0] := Pixel^.Data[0] / 255;
  Result.Data[1] := Pixel^.Data[1] / 255;
  Result.Data[2] := Pixel^.Data[2] / 255;
  Result.Data[3] := 1.0;
end;

procedure TRGBImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector3Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.Data[0] := Clamped(Round(C.Data[0] * 255), Low(Byte), High(Byte));
  Pixel^.Data[1] := Clamped(Round(C.Data[1] * 255), Low(Byte), High(Byte));
  Pixel^.Data[2] := Clamped(Round(C.Data[2] * 255), Low(Byte), High(Byte));
end;

procedure TRGBImage.Clear(const Pixel: TVector4Byte);
var
  P: PVector3Byte;
  I: Cardinal;
begin
  P := Pixels;
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
  P := Pixels;
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

procedure TRGBImage.TransformRGB(const Matrix: TMatrix3);
type PPixel = PVector3Byte;
{$I castleimages_transformrgb_implement.inc}

procedure TRGBImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector3Byte;
{$I castleimages_modulatergb_implement.inc}

function TRGBImage.ToRGBAlphaImage: TRGBAlphaImage;
var
  pi: PVector3Byte;
  pa: PVector4Byte;
  i: Cardinal;
begin
  Result := TRGBAlphaImage.Create(Width, Height, Depth);
  pi := Pixels;
  pa := Result.Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    Move(pi^, pa^, SizeOf(TVector3Byte));
    pa^.Data[3] := High(Byte);
    Inc(pi);
    Inc(pa);
  end;
end;

function TRGBImage.ToRGBFloat: TRGBFloatImage;
var
  PFloat: PVector3;
  PByte: PVector3Byte;
  i: Cardinal;
begin
  result := TRGBFloatImage.Create(Width, Height, Depth);
  try
    PByte := Pixels;
    PFloat := Result.Pixels;
    for i := 1 to Width * Height * Depth do
    begin
      PFloat^ := Vector3(PByte^);
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
    pRGB := Pixels;
    pGrayscale := Result.Pixels;
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
  HorizontalLine(X1, X2, Y, Vector3Byte(Color.XYZ));
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
  VerticalLine(X, Y1, Y2, Vector3Byte(Color.XYZ));
end;

procedure TRGBImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector3Byte;
  SecondPtr: PVector3Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TRGBImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

{ $define FAST_UNSAFE_MIX_COLORS}

class procedure TRGBImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector3Byte absolute OutputColor;
  Cols: array [0..3] of PVector3Byte absolute AColors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^.Data[0] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[0] +
    Weights.Data[1] * Cols[1]^.Data[0] +
    Weights.Data[2] * Cols[2]^.Data[0] +
    Weights.Data[3] * Cols[3]^.Data[0]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Data[1] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[1] +
    Weights.Data[1] * Cols[1]^.Data[1] +
    Weights.Data[2] * Cols[2]^.Data[1] +
    Weights.Data[3] * Cols[3]^.Data[1]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Data[2] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[2] +
    Weights.Data[1] * Cols[1]^.Data[2] +
    Weights.Data[2] * Cols[2]^.Data[2] +
    Weights.Data[3] * Cols[3]^.Data[2]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

procedure TRGBImage.Assign(const Source: TCastleImage);
var
  FloatPtr: PVector3;
  RgbaPtr: PVector4Byte;
  SelfPtr: PVector3Byte;
  I: Cardinal;
begin
  if Source is TRGBAlphaImage then
  begin
    SetSize(Source);
    SelfPtr := Pixels;
    RgbaPtr := TRGBAlphaImage(Source).Pixels;
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
    SelfPtr := Pixels;
    FloatPtr := TRGBFloatImage(Source).Pixels;
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

function TRGBAlphaImage.GetPixels: PVector4Byte;
begin
  Result := PVector4Byte(RawPixels);
end;

function TRGBAlphaImage.GetPixelsArray: PVector4ByteArray;
begin
  Result := PVector4ByteArray(RawPixels);
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

function TRGBAlphaImage.RowPtr(const Y, Z: Cardinal): PVector4ByteArray;
begin
  Result := PVector4ByteArray(inherited RowPtr(Y, Z));
end;

procedure TRGBAlphaImage.InvertColors;
var
  i: Cardinal;
  palpha: PVector4Byte;
begin
  palpha := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    palpha^.Data[0] := High(byte) - palpha^.Data[0];
    palpha^.Data[1] := High(byte) - palpha^.Data[1];
    palpha^.Data[2] := High(byte) - palpha^.Data[2];
    Inc(palpha);
  end;
end;

function TRGBAlphaImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector4Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.Data[0] := Pixel^.Data[0] / 255;
  Result.Data[1] := Pixel^.Data[1] / 255;
  Result.Data[2] := Pixel^.Data[2] / 255;
  Result.Data[3] := Pixel^.Data[3] / 255;
end;

procedure TRGBAlphaImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector4Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.Data[0] := Clamped(Round(C.Data[0] * 255), Low(Byte), High(Byte));
  Pixel^.Data[1] := Clamped(Round(C.Data[1] * 255), Low(Byte), High(Byte));
  Pixel^.Data[2] := Clamped(Round(C.Data[2] * 255), Low(Byte), High(Byte));
  Pixel^.Data[3] := Clamped(Round(C.Data[3] * 255), Low(Byte), High(Byte));
end;

procedure TRGBAlphaImage.Clear(const Pixel: TVector4Byte);
begin
  FillDWord(RawPixels^, Width*Height, LongWord(Pixel));
end;

procedure TRGBAlphaImage.ClearAlpha(const Alpha: Byte);
var
  i: Cardinal;
  palpha: PVector4Byte;
begin
  palpha := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    palpha^.Data[3] := Alpha;
    Inc(palpha);
  end;
end;

function TRGBAlphaImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsMemDWordFilled(RawPixels^, Width * Height * Depth, LongWord(Pixel));
end;

procedure TRGBAlphaImage.TransformRGB(const Matrix: TMatrix3);
type PPixel = PVector4Byte;
{$I castleimages_transformrgb_implement.inc}

procedure TRGBAlphaImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector4Byte;
{$I castleimages_modulatergb_implement.inc}

procedure TRGBAlphaImage.AlphaDecide(const AlphaColor: TVector3Byte;
  Tolerance: Byte; AlphaOnColor: Byte; AlphaOnNoColor: Byte);
var
  pa: PVector4Byte;
  i: Cardinal;
begin
  pa := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    if EqualRGB(AlphaColor, PVector3Byte(pa)^, Tolerance) then
      pa^.Data[3] := AlphaOnColor else
      pa^.Data[3] := AlphaOnNoColor;
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

  PtrAlpha := Pixels;
  PtrRGB := RGB.Pixels;
  PtrGrayscale := AGrayscale.Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    System.Move(PtrRGB^, PtrAlpha^, SizeOf(TVector3Byte));
    PtrAlpha^.Data[3] := PtrGrayscale^;

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
  PtrAlpha := Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^.Data[3] > AlphaTolerance) and
       (PtrAlpha^.Data[3] < 255 - AlphaTolerance) then
      Exit(acBlending);
    Inc(PtrAlpha);
  end;

  Result := acTest;
end;

procedure TRGBAlphaImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector4Byte;
  SecondPtr: PVector4Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TRGBAlphaImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TRGBAlphaImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector4Byte absolute OutputColor;
  Cols: array [0..3] of PVector4Byte absolute AColors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^.Data[0] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[0] +
    Weights.Data[1] * Cols[1]^.Data[0] +
    Weights.Data[2] * Cols[2]^.Data[0] +
    Weights.Data[3] * Cols[3]^.Data[0]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Data[1] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[1] +
    Weights.Data[1] * Cols[1]^.Data[1] +
    Weights.Data[2] * Cols[2]^.Data[1] +
    Weights.Data[3] * Cols[3]^.Data[1]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Data[2] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[2] +
    Weights.Data[1] * Cols[1]^.Data[2] +
    Weights.Data[2] * Cols[2]^.Data[2] +
    Weights.Data[3] * Cols[3]^.Data[2]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Data[3] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[3] +
    Weights.Data[1] * Cols[1]^.Data[3] +
    Weights.Data[2] * Cols[2]^.Data[3] +
    Weights.Data[3] * Cols[3]^.Data[3]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

function TRGBAlphaImage.ToRGBImage: TRGBImage;
begin
  Result := TRGBImage.Create(0, 0);
  Result.Assign(Self);
  Result.URL := URL + '[ToRGBImage]';
end;

function TRGBAlphaImage.ToGrayscaleImage: TGrayscaleImage;
begin
  Result := TGrayscaleImage.Create(0, 0);
  Result.Assign(Self);
  Result.URL := URL + '[ToGrayscaleImage]';
end;

function TRGBAlphaImage.ToGrayscaleAlphaImage: TGrayscaleAlphaImage;
begin
  Result := TGrayscaleAlphaImage.Create(0, 0);
  Result.Assign(Self);
  Result.URL := URL + '[ToGrayscaleAlphaImage]';
end;

procedure TRGBAlphaImage.PremultiplyAlpha;
var
  P: PVector4Byte;
  I: Integer;
begin
  if not FPremultipliedAlpha then
  begin
    FPremultipliedAlpha := true;
    P := Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      P^.Data[0] := Clamped(Round(P^.Data[0] * P^.Data[3] / 255), 0, 255);
      P^.Data[1] := Clamped(Round(P^.Data[1] * P^.Data[3] / 255), 0, 255);
      P^.Data[2] := Clamped(Round(P^.Data[2] * P^.Data[3] / 255), 0, 255);
      Inc(P);
    end;
  end;
end;

procedure TRGBAlphaImage.AlphaBleed(const ProgressTitle: string);
var
  Copy: TCastleImage;
begin
  Copy := MakeAlphaBleed(ProgressTitle);
  try
    Assert(Size = Copy.Size);
    Move(Copy.RawPixels^, RawPixels^, Size);
  finally FreeAndNil(Copy) end;
end;

function TRGBAlphaImage.MakeAlphaBleed(const ProgressTitle: string): TCastleImage;

  function FindNearestNonTransparentPixel(X, Y, Z: Integer): PVector4Byte;

    function TryPixelOpaque(const DX, DY: Integer;
      var SomePixelWithinImage: boolean): PVector4Byte;
    var
      NX, NY, GX, GY: Integer;
    begin
      NX := X + DX;
      NY := Y + DY;
      GX := Clamped(NX, 0, Width - 1);
      GY := Clamped(NY, 0, Height - 1);
      { does not search in Z.
        This is faster for 2D images.
        Also this way we do not look if we're outside Z range below,
        which would stop the search too quickly for 2D images. }
      if (NX = GX) and (NY = GY) then
        SomePixelWithinImage := true;
      Result := PixelPtr(GX, GY, Z);
      if Result^.Data[3] <> High(Byte) then
        Result := nil; // nope, tried pixel is not opaque
    end;

  var
    Distance: Cardinal;
    SomePixelWithinImage: boolean;
  begin
    Distance := 1;
    Result := nil;
    repeat
      SomePixelWithinImage := false;
      Result := TryPixelOpaque(-Distance,         0, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque( Distance,         0, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(        0, -Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(        0,  Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(-Distance, -Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(-Distance,  Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque( Distance, -Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque( Distance,  Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Inc(Distance);
    until not SomePixelWithinImage;
  end;

var
  P, NewP: PVector4Byte;
  X, Y, Z: Integer;
begin
  Result := MakeCopy;
  if ProgressTitle <> '' then
    Progress.Init(Width * Height * Depth, ProgressTitle);
  try
    for X := 0 to Width - 1 do
      for Y := 0 to Height - 1 do
        for Z := 0 to Depth - 1 do
        begin
          P := Result.PixelPtr(X, Y, Z);
          if P^.Data[3] <> High(Byte) then
          begin
            NewP := FindNearestNonTransparentPixel(X, Y, Z);
            if NewP <> nil then
              Move(NewP^, P^, SizeOf(TVector3Byte));
          end;
          if ProgressTitle <> '' then
            Progress.Step;
        end;
  finally
    if ProgressTitle <> '' then
      Progress.Fini;
  end;
end;

{ TRGBFloatImage ------------------------------------------------------------ }

function TRGBFloatImage.GetPixels: PVector3;
begin
  Result := PVector3(RawPixels);
end;

function TRGBFloatImage.GetPixelsArray: PVector3Array;
begin
  Result := PVector3Array(RawPixels);
end;

class function TRGBFloatImage.PixelSize: Cardinal;
begin
  Result := SizeOf(TVector3);
end;

class function TRGBFloatImage.ColorComponentsCount: Cardinal;
begin
  Result := 3;
end;

function TRGBFloatImage.PixelPtr(const X, Y, Z: Cardinal): PVector3;
begin
  Result := PVector3(inherited PixelPtr(X, Y, Z));
end;

function TRGBFloatImage.RowPtr(const Y, Z: Cardinal): PVector3Array;
begin
  Result := PVector3Array(inherited RowPtr(Y, Z));
end;

function TRGBFloatImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector3;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.Data[0] := Pixel^.Data[0];
  Result.Data[1] := Pixel^.Data[1];
  Result.Data[2] := Pixel^.Data[2];
  Result.Data[3] := 1.0;
end;

procedure TRGBFloatImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector3;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.Data[0] := C.Data[0];
  Pixel^.Data[1] := C.Data[1];
  Pixel^.Data[2] := C.Data[2];
end;

procedure TRGBFloatImage.Clear(const Pixel: TVector4Byte);
begin
  Clear(Vector3(
    Pixel.Data[0] * 255,
    Pixel.Data[1] * 255,
    Pixel.Data[2] * 255));
end;

function TRGBFloatImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsClear(Vector3(
    Pixel.Data[0] * 255,
    Pixel.Data[1] * 255,
    Pixel.Data[2] * 255));
end;

procedure TRGBFloatImage.Clear(const Pixel: TVector3);
var
  P: PVector3;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    Move(Pixel, P^, SizeOf(TVector3));
    Inc(P);
  end;
end;

function TRGBFloatImage.IsClear(const Pixel: TVector3): boolean;
var
  P: PVector3;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    if not CompareMem(@Pixel, P, SizeOf(TVector3)) then
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
  pFloat: PVector3;
  i: Cardinal;
begin
  PFloat := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    PFloat^ := PFloat^ * Scale;
    Inc(PFloat);
  end;
end;

function VectorPowerComponents(const V: TVector3; const Exp: Single): TVector3;
begin
  Result.Data[0] := Power(V.Data[0], Exp);
  Result.Data[1] := Power(V.Data[1], Exp);
  Result.Data[2] := Power(V.Data[2], Exp);
end;

procedure TRGBFloatImage.ExpColors(const Exp: Single);
var
  pFloat: PVector3;
  i: Cardinal;
begin
  PFloat := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    PFloat^ := VectorPowerComponents(PFloat^, Exp);
    Inc(PFloat);
  end;
end;

procedure TRGBFloatImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector3;
  SecondPtr: PVector3;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TRGBFloatImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TRGBFloatImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector3 absolute OutputColor;
  Cols: array [0..3] of PVector3 absolute AColors;
begin
  OutputCol^.Data[0] :=
    Weights.Data[0] * Cols[0]^.Data[0] +
    Weights.Data[1] * Cols[1]^.Data[0] +
    Weights.Data[2] * Cols[2]^.Data[0] +
    Weights.Data[3] * Cols[3]^.Data[0];
  OutputCol^.Data[1] :=
    Weights.Data[0] * Cols[0]^.Data[1] +
    Weights.Data[1] * Cols[1]^.Data[1] +
    Weights.Data[2] * Cols[2]^.Data[1] +
    Weights.Data[3] * Cols[3]^.Data[1];
  OutputCol^.Data[2] :=
    Weights.Data[0] * Cols[0]^.Data[2] +
    Weights.Data[1] * Cols[1]^.Data[2] +
    Weights.Data[2] * Cols[2]^.Data[2] +
    Weights.Data[3] * Cols[3]^.Data[2];
end;

procedure TRGBFloatImage.InvertColors;
var
  I: Cardinal;
  P: PVector3;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^.Data[0] := Max(1-P^.Data[0], 0.0);
    P^.Data[1] := Max(1-P^.Data[1], 0.0);
    P^.Data[2] := Max(1-P^.Data[2], 0.0);
    Inc(P);
  end;
end;

{ TGrayscaleImage ------------------------------------------------------------ }

function TGrayscaleImage.GetPixels: PByte;
begin
  Result := PByte(RawPixels);
end;

function TGrayscaleImage.GetPixelsArray: PByteArray;
begin
  Result := PByteArray(RawPixels);
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

procedure TGrayscaleImage.Clear(const Pixel: TVector4Byte);
begin
  Clear(GrayscaleValue(Pixel));
end;

function TGrayscaleImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsClear(GrayscaleValue(Pixel));
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
  P := Pixels;
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

  SelfPtr := Pixels;
  SecondPtr := TGrayscaleImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Clamped(Round(Lerp(Value, SelfPtr^, SecondPtr^)), 0, High(Byte));
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TGrayscaleImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PByte absolute OutputColor;
  Cols: array [0..3] of PByte absolute AColors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^ := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^ +
    Weights.Data[1] * Cols[1]^ +
    Weights.Data[2] * Cols[2]^ +
    Weights.Data[3] * Cols[3]^) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

function TGrayscaleImage.ToGrayscaleAlphaImage: TGrayscaleAlphaImage;
var
  pg: PByte;
  pa: PVector2Byte;
  I: Cardinal;
begin
  Result := TGrayscaleAlphaImage.Create(Width, Height, Depth);
  Result.URL := URL + '[ToGrayscaleAlphaImage]';
  pg := Pixels;
  pa := Result.Pixels;

  if TreatAsAlpha then
  begin
    for i := 1 to Width * Height * Depth do
    begin
      pa^.Data[0] := High(Byte);
      pa^.Data[1] := pg^;
      Inc(pg);
      Inc(pa);
    end;
  end else
  begin
    for i := 1 to Width * Height * Depth do
    begin
      pa^.Data[0] := pg^;
      pa^.Data[1] := High(Byte);
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

  PtrAlpha := Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^ > AlphaTolerance) and
       (PtrAlpha^ < 255 - AlphaTolerance) then
      Exit(acBlending);
    Inc(PtrAlpha);
  end;

  Result := acTest;
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
    SelfPtr := Pixels;
    RgbaPtr := TRGBAlphaImage(Source).Pixels;
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

procedure TGrayscaleImage.InvertColors;
var
  I: Cardinal;
  P: PByte;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^ := High(Byte)-P^;
    Inc(P);
  end;
end;

function TGrayscaleImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PByte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.Data[0] := Pixel^;
  Result.Data[1] := Pixel^;
  Result.Data[2] := Pixel^;
  Result.Data[3] := 1.0;
end;

procedure TGrayscaleImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PByte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^ := Clamped(Round(GrayscaleValue(C) * 255), Low(Byte), High(Byte));
end;

{ TGrayscaleAlphaImage ------------------------------------------------------------ }

function TGrayscaleAlphaImage.GetPixels: PVector2Byte;
begin
  Result := PVector2Byte(RawPixels);
end;

function TGrayscaleAlphaImage.GetPixelsArray: PVector2ByteArray;
begin
  Result := PVector2ByteArray(RawPixels);
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

function TGrayscaleAlphaImage.RowPtr(const Y, Z: Cardinal): PVector2ByteArray;
begin
  Result := PVector2ByteArray(inherited RowPtr(Y, Z));
end;

procedure TGrayscaleAlphaImage.Clear(const Pixel: TVector4Byte);
begin
  Clear(Vector2Byte(GrayscaleValue(Pixel), Pixel.Data[3]));
end;

function TGrayscaleAlphaImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsClear(Vector2Byte(GrayscaleValue(Pixel), Pixel.Data[3]));
end;

procedure TGrayscaleAlphaImage.Clear(const Pixel: TVector2Byte);
var
  P: PVector2Byte;
  I: Cardinal;
begin
  P := Pixels;
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
  P := Pixels;
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
  PtrAlpha := Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^.Data[1] > AlphaTolerance) and
       (PtrAlpha^.Data[1] < 255 - AlphaTolerance) then
      Exit(acBlending);
    Inc(PtrAlpha);
  end;

  Result := acTest;
end;

procedure TGrayscaleAlphaImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector2Byte;
  SecondPtr: PVector2Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TGrayscaleAlphaImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TGrayscaleAlphaImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector2Byte absolute OutputColor;
  Cols: array [0..3] of PVector2Byte absolute AColors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^.Data[0] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[0] +
    Weights.Data[1] * Cols[1]^.Data[0] +
    Weights.Data[2] * Cols[2]^.Data[0] +
    Weights.Data[3] * Cols[3]^.Data[0]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Data[1] := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.Data[0] * Cols[0]^.Data[1] +
    Weights.Data[1] * Cols[1]^.Data[1] +
    Weights.Data[2] * Cols[2]^.Data[1] +
    Weights.Data[3] * Cols[3]^.Data[1]) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
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
    SelfPtr := Pixels;
    RgbaPtr := TRGBAlphaImage(Source).Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      SelfPtr^.Data[0] := GrayscaleValue(RgbPtr^);
      SelfPtr^.Data[1] := RgbaPtr^.Data[3];
      Inc(SelfPtr);
      Inc(RgbaPtr);
    end;
    URL := Source.URL;
  end else

    inherited;
end;

procedure TGrayscaleAlphaImage.InvertColors;
var
  I: Cardinal;
  P: PVector2Byte;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^.Data[0] := High(Byte) - P^.Data[0];
    Inc(P);
  end;
end;

function TGrayscaleAlphaImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector2Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.Data[0] := Pixel^.Data[0] / 255;
  Result.Data[1] := Pixel^.Data[0] / 255;
  Result.Data[2] := Pixel^.Data[0] / 255;
  Result.Data[3] := Pixel^.Data[1] / 255;
end;

procedure TGrayscaleAlphaImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector2Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.Data[0] := Clamped(Round(GrayscaleValue(C) * 255), Low(Byte), High(Byte));
  Pixel^.Data[1] := Clamped(Round(C.Data[3]         * 255), Low(Byte), High(Byte));
end;

{ RGBE <-> 3 Single color conversion --------------------------------- }

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
  RGBEZero: TVector4Byte=(Data: (0, 0, 0, 0));

  RGBELow :TVector4Byte=(Data: (0, 0, 0, 0)); { = RGBEZero }
  RGBEHigh: TVector4Byte=(Data: (High(Byte), High(Byte), High(Byte), High(Byte)));

function Vector3ToRGBE(const v: TVector3): TVector4Byte;
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
  MaxVal := V.Max;

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
  if IsZero(MaxVal) then begin Result := RGBEZero; Exit end;

  Frexp(MaxVal, Mantissa, Exponent);

  if Exponent < RGBEMinExponent then begin result := RGBELow; Exit end;
  if Exponent > RGBEMaxExponent then begin result := RGBEHigh; Exit end;

  Multiplier := Mantissa * 256 / MaxVal;

  { MaxVal * Multiplier daje Mantissa * High(byte) a wiec cos w zakresie
    0 .. High(Byte) bo Mantissa <= 1 (de facto, Mantissa >= 0.5 wiec
    mozna podac dokladniejsze ograniczenie na Mantissa * High(byte)).
    Wszystkie pozostale v[] sa mniejsze od MaxVal wiec one tez dadza cos
    w zakresie bajta. }
  Result.Data[0] := Clamped(Round(v.Data[0] * Multiplier), 0, High(Byte));
  Result.Data[1] := Clamped(Round(v.Data[1] * Multiplier), 0, High(Byte));
  Result.Data[2] := Clamped(Round(v.Data[2] * Multiplier), 0, High(Byte));

  { sprawdzajac czy Exponent in RGBEMin/MaxExponent wczesniej juz zapewnilem
    sobie ze ponizsze przypisanie jest Ok, wynik zmiesci sie w zakresie bajta. }
  Result.Data[3] := Exponent + RGBEExponentOffset;
end;

function VectorRGBETo3Single(const v: TVector4Byte): TVector3;
{ Implementation like in Graphic Gems II.5.

  Note: Multiplier is from 1/256 (not 1/255).
  Same reasons as in Vector3ToRGBE implementation. }
var
  Multiplier: Single;
begin
  if v.Data[3] = 0 then begin Result := TVector3.Zero; Exit end;

  Multiplier := Ldexp(1 / 256, Integer(v.Data[3]) - RGBEExponentOffset);
  Result.Data[0] := v.Data[0] * Multiplier;
  Result.Data[1] := v.Data[1] * Multiplier;
  Result.Data[2] := v.Data[2] * Multiplier;
end;

{ TLoadImageEventList -------------------------------------------------------- }

type
  { List of TLoadImageEvent methods. }
  TLoadImageEventList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TLoadImageEvent>)
    procedure Execute(var URL: string);
  end;

procedure TLoadImageEventList.Execute(var URL: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](URL);
end;

var
  LoadImageEvents: TLoadImageEventList;

{ LoadEncodedImage ----------------------------------------------------------- }


{ Make sure the image has an alpha channel.
  If image doesn't have an alpha channel (it is TRGBImage or TGrayscaleImage),
  we will create new image instance (respectively, TRGBAlphaImage or TGrayscaleAlphaImage)
  that adds an alpha channel. The newly created alpha channel will have constant opaque alpha,
  except in the special case of TGrayscaleImage with TGrayscaleImage.TreatAsAlpha = @true
  (where the contents will be copied to alpha, and intensity set to white).

  If the image already had an alpha channel, then just return it. }
procedure ImageAddAlphaVar(var Img: TEncodedImage);
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

  if not Img.HasAlpha then
    raise EInternalError.Create(
      'ImageAddAlphaVar not possible for this image class: ' + Img.ClassName);
end;

function LoadEncodedImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TEncodedImageClass;
  const Options: TLoadImageOptions = [])
  :TEncodedImage;

  { ClassAllowed is only a shortcut to global utility. }
  function ClassAllowed(ImageClass: TEncodedImageClass): boolean;
  begin
    Result := CastleImages.ClassAllowed(ImageClass, AllowedImageClasses);
  end;

  { On input, Image must be TRGBImage and on output it will be TGrayscaleImage. }
  procedure ImageGrayscaleVar(var Image: TEncodedImage);
  var
    NewImage: TGrayscaleImage;
  begin
    NewImage := (Image as TRGBImage).ToGrayscale;
    FreeAndNil(Image);
    Image := NewImage;
  end;

  procedure ImageRGBToFloatVar(var Image: TEncodedImage);
  var
    NewResult: TEncodedImage;
  begin
    NewResult := (Image as TRGBImage).ToRGBFloat;
    Image.Free;
    Image := NewResult;
  end;

  procedure ImageRGBToGrayscaleVar(var Image: TEncodedImage);
  var
    NewResult: TEncodedImage;
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
        lcG_GA_RGB_RGBA, lcG_GA_RGB_RGBA_GPUCompressed:
          begin
            if ClassAllowed(TRGBImage) or
               ClassAllowed(TRGBAlphaImage) or
               ClassAllowed(TGrayscaleImage) or
               ClassAllowed(TGrayscaleAlphaImage) or
               ( ClassAllowed(TGPUCompressedImage) and
                 (ImageFormatInfos[StreamFormat].LoadedClasses = lcG_GA_RGB_RGBA_GPUCompressed) ) then
              Result := Load(Stream, AllowedImageClasses) else
            if ClassAllowed(TRGBFloatImage) then
            begin
              Result := Load(Stream, [TRGBImage]);
              ImageRGBToFloatVar(Result);
            end else
              raise EUnableToLoadImage.CreateFmt('LoadEncodedImage cannot load this image file format to %s', [LoadEncodedImageParams(AllowedImageClasses)]);
          end;
        lcRGB_RGBA:
          begin
            if ClassAllowed(TRGBImage) or
               ClassAllowed(TRGBAlphaImage) then
              Result := Load(Stream, AllowedImageClasses) else
            if ClassAllowed(TGrayscaleImage) then
            begin
              Result := Load(Stream, [TRGBImage]);
              ImageRGBToGrayscaleVar(Result);
            end else
{ TODO:     if ClassAllowed(TGrayscaleAlphaImage) then
              ... }
            if ClassAllowed(TRGBFloatImage) then
            begin
              Result := Load(Stream, [TRGBImage]);
              ImageRGBToFloatVar(Result);
            end else
              raise EUnableToLoadImage.CreateFmt('LoadEncodedImage cannot load this image file format to %s', [LoadEncodedImageParams(AllowedImageClasses)]);
          end;
        lcRGB:
          begin
            Result := Load(Stream, [TRGBImage]);
            Assert(Result is TRGBImage);

            if not (ClassAllowed(TRGBImage)) then
            begin
              if ClassAllowed(TRGBAlphaImage) then
              begin
                ImageAddAlphaVar(Result);
              end else
              if ClassAllowed(TGrayscaleImage) then
              begin
                ImageGrayscaleVar(Result);
              end else
              { TODO:
              if ClassAllowed(TGrayscaleAlphaImage) then
              begin
                ImageAddAlphaVar(Result);
                ImageGrayscaleAlphaVar(Result);
              end else }
              if ClassAllowed(TRGBFloatImage) then
              begin
                ImageRGBToFloatVar(Result);
              end else
                raise EUnableToLoadImage.CreateFmt('LoadEncodedImage cannot load this image file format to %s', [LoadEncodedImageParams(AllowedImageClasses)]);
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
                ImageAddAlphaVar(Result);
              end else
              if ClassAllowed(TGrayscaleImage) then
              begin
                ImageGrayscaleVar(Result);
              end else
              if ClassAllowed(TGrayscaleAlphaImage) then
              begin
                ImageGrayscaleVar(Result);
                ImageAddAlphaVar(Result);
              end else
                raise EUnableToLoadImage.CreateFmt('LoadEncodedImage: RGBE format cannot be loaded to %s', [LoadEncodedImageParams(AllowedImageClasses)]);
            end;
          end;
        else raise EInternalError.Create('LoadEncodedImage: LoadedClasses?');
      end;
    end else
    begin
      raise EImageFormatNotSupported.Create('Can''t load image format "'+
        ImageFormatInfos[StreamFormat].FormatName+'"');
    end;

    { This may be implemented at lower level, inside particular image loaders,
      some day. It would have 0 cost then. }
    if liFlipVertically in Options then
      Result.FlipVertical;

  except
    Result.Free;
    raise
  end;
end;

function LoadEncodedImage(Stream: TStream; const MimeType: string;
  const AllowedImageClasses: array of TEncodedImageClass;
  const Options: TLoadImageOptions = [])
  :TEncodedImage;
var
  iff: TImageFormat;
begin
  if MimeTypeToImageFormat(MimeType, true, false, iff) then
    result := LoadEncodedImage(Stream, iff, AllowedImageClasses, Options)
  else
    raise EImageFormatNotSupported.Create('Unrecognized image MIME type: "'+MimeType+'"');
end;

function LoadEncodedImage(URL: string;
  const AllowedImageClasses: array of TEncodedImageClass;
  const Options: TLoadImageOptions = []): TEncodedImage;
const
  SLoadError = 'Error loading image from URL "%s": %s';
var
  F: TStream;
  MimeType: string;
  TimeStart: TCastleProfilerTime;
begin
  TimeStart := Profiler.Start('Loading "' + URIDisplay(URL) + '" (CastleImages)');

  try
    try
      LoadImageEvents.Execute(URL);
      F := Download(URL, [soForceMemoryStream], MimeType);
    except
      on E: EReadError do raise EImageLoadError.Create(E.Message);
    end;

    try
      Result := LoadEncodedImage(F, MimeType, AllowedImageClasses, Options);
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

  Profiler.Stop(TimeStart);
end;

function LoadEncodedImage(const URL: string;
  const Options: TLoadImageOptions = []): TEncodedImage;
begin
  Result := LoadEncodedImage(URL, [], Options);
end;

{ LoadImage ------------------------------------------------------------------ }

function LoadImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
var
  E: TEncodedImage;
begin
  E := LoadEncodedImage(Stream, StreamFormat, AllowedImageClasses);
  if not (E is TCastleImage) then
    raise EImageLoadError.Create('Image is compressed for GPU, cannot load it to uncompressed format. You can only render such image.');
  Result := TCastleImage(E);
end;

function LoadImage(Stream: TStream; const MimeType: string;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
var
  E: TEncodedImage;
begin
  E := LoadEncodedImage(Stream, MimeType, AllowedImageClasses);
  if not (E is TCastleImage) then
    raise EImageLoadError.Create('Image is compressed for GPU, cannot load it to uncompressed format. You can only render such image.');
  Result := TCastleImage(E);
end;

function LoadImage(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;
var
  E: TEncodedImage;
begin
  E := LoadEncodedImage(URL, AllowedImageClasses);
  if not (E is TCastleImage) then
    raise EImageLoadError.CreateFmt('Image "%s" is compressed for GPU, cannot load it to uncompressed format. You can only render such image.',
      [URIDisplay(URL)]);
  Result := TCastleImage(E);
end;

function LoadImage(const URL: string): TCastleImage;
var
  E: TEncodedImage;
begin
  E := LoadEncodedImage(URL);
  if not (E is TCastleImage) then
    raise EImageLoadError.CreateFmt('Image "%s" is compressed for GPU, cannot load it to uncompressed format. You can only render such image.',
      [URIDisplay(URL)]);
  Result := TCastleImage(E);
end;

function LoadImage(const URL: string;
  const AllowedImageClasses: array of TEncodedImageClass;
  const ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation;
  const Options: TLoadImageOptions = []): TCastleImage;
var
  E: TEncodedImage;
begin
  E := LoadEncodedImage(URL, AllowedImageClasses, Options);
  if not (E is TCastleImage) then
    raise EImageLoadError.CreateFmt('Image "%s" is compressed for GPU, cannot load it to uncompressed format. You can only render such image.',
      [URIDisplay(URL)]);
  Result := TCastleImage(E);
  Result.Resize(ResizeWidth, ResizeHeight, Interpolation);
end;

{ SaveImage on TEncodedImage ---------------------------------------------------- }

procedure SaveImage(const Img: TEncodedImage; const Format: TImageFormat; Stream: TStream);
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
      scG_GA_RGB_RGBA, scG_GA_RGB_RGBA_GPUCompressed:
        begin
          if (Img is TRGBImage) or
             (Img is TRGBAlphaImage) or
             (Img is TGrayscaleImage) or
             (Img is TGrayscaleAlphaImage) or
             ( (Img is TGPUCompressedImage) and
               (ImageFormatInfos[Format].SavedClasses = scG_GA_RGB_RGBA_GPUCompressed) ) then
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

procedure SaveImage(const img: TEncodedImage; const MimeType: string; Stream: TStream);
var
  Format: TImageFormat;
begin
  if not MimeTypeToImageFormat(MimeType, false, true, Format) then
    raise EImageSaveError.CreateFmt('Unknown image MIME type "%s", cannot save. Make sure the filename/URL you want to save has one of the recognized extensions',
      [MimeType]);
  SaveImage(Img, Format, Stream);
end;

procedure SaveImage(const Img: TEncodedImage; const URL: string);
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

{ others --------------------------------------------------------------------- }

procedure AlphaMaxVar(var A: TAlphaChannel; const B: TAlphaChannel);
begin
  if B > A then A := B;
end;

function StringToAlpha(S: string; var WarningDone: boolean): TAutoAlphaChannel;
begin
  S := UpperCase(S);
  for Result := Low(Result) to High(Result) do
    if S = AlphaToString[Result] then
      Exit;

  if S = 'SIMPLE_YES_NO' then
  begin
    if not WarningDone then
    begin
      WritelnWarning('alphaChannel', '"alphaChannel" value "SIMPLE_YES_NO" is deprecated, use "TEST" instead');
      WarningDone := true;
    end;
    Exit(acTest);
  end;

  if S = 'FULL_RANGE' then
  begin
    if not WarningDone then
    begin
      WritelnWarning('alphaChannel', '"alphaChannel" value "FULL_RANGE" is deprecated, use "BLENDING" instead');
      WarningDone := true;
    end;
    Exit(acBlending);
  end;

  if not WarningDone then
  begin
    WritelnWarning('alphaChannel', Format('Invalid "alphaChannel" value "%s"', [S]));
    WarningDone := true;
  end;
  Result := acAuto;
end;

function TextureCompressionToString(const TextureCompression: TTextureCompression): string;
begin
  Result := TextureCompressionInfo[TextureCompression].Name;
end;

function StringToTextureCompression(const S: string): TTextureCompression;
var
  SLower: string;
begin
  SLower := LowerCase(S);
  for Result := Low(Result) to High(Result) do
    if SLower = LowerCase(TextureCompressionInfo[Result].Name) then
      Exit;
  raise Exception.CreateFmt('Invalid texture compression name "%s"', [S]);
end;

procedure AddLoadImageListener(const Event: TLoadImageEvent);
begin
  LoadImageEvents.Add(Event);
end;

procedure RemoveLoadImageListener(const Event: TLoadImageEvent);
begin
  LoadImageEvents.Remove(Event);
end;

function ProcessImageUrl(const URL: string): string;
begin
  Result := URL;
  LoadImageEvents.Execute(Result);
end;

{ unit initialization / finalization ----------------------------------------- }

initialization
  RegisterMimeTypes;
  InitializeImagesFileFilters;
  {$if defined(CASTLE_PNG_DYNAMIC) or defined(CASTLE_PNG_STATIC)}
  InitializePNGUsingLibpng;
  {$endif}
  LoadImageEvents := TLoadImageEventList.Create;
finalization
  FreeAndNil(LoadImage_FileFilters);
  FreeAndNil(SaveImage_FileFilters);
  FreeAndNil(LoadImageEvents);
end.
