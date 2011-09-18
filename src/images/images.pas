{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*Loading, saving, and processing of 2D (and 3D) images (TImage and descendants).
  Storing images in the memory, loading and saving them from/to files in various
  formats, resizing, converting to grayscale, copying and merging,
  many other image operations --- it's all here.

  The most important class here is @link(TImage).
  It represents an image as a simple uncompressed array of pixels.
  Descendants of TImage define what exactly is a "pixel".
  We have 8-bit color images
  (@link(TRGBAlphaImage), @link(TRGBImage),
  @link(TGrayscaleAlphaImage) and @link(TGrayscaleImage)).
  We also have an image with floating-point precision and range:
  @link(TRGBFloatImage).
  You are free to create more descendants of TImage in your own units
  if you want to encode the pixel differently.

  When reading and writing image files, we understand various image
  formats. See TImageFormat documentation for a current list of supported
  formats, with comments specific to particular formats.
  The basic loading and saving procedures and LoadImage and SaveImage.

  Example usage of this unit:

@longCode(#
  var
    Image: TImage;
  begin
    Image := LoadImage('image.png', [], []);
    { scale the image to be 2x smaller }
    Image.Resize(Image.Width div 2, Image.Height div 2);
    SaveImage(Image, 'newimage.png');
  end;
#)

  This unit is of course not dependent on OpenGL or any other rendering
  library. See GLImages for OpenGL image operations (for textures and others).
*)

unit Images;

{
  TODO:
  - implement more impressive resizing filters, at least simple
    linear like gluScaleImage
}

{$include kambiconf.inc}
{$include pngconf.inc}

interface

uses SysUtils, Classes, Math, KambiUtils, VectorMath,
  KambiPng, FileFilters, KambiClassUtils,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif},
  FPImage, FPReadPCX, 
  {$ifndef VER2_2} FPReadGIF, FPReadPSD, {$endif} FPReadTGA, FPReadTiff, FPReadXPM,
  FPReadJPEG, FPWriteJPEG, FPReadPNM;

type
  { See TImage.AlphaChannelType. }
  TAlphaChannelType = (atNone, atSimpleYesNo, atFullRange);

{ Colors ------------------------------------------------------------ }

{ Check if the two RGB colors are equal, ignoring small differences.
  All three color components may differ by at most Tolerance.
  When Tolerance is 0, this is a normal (exact) comparison. }
function EqualRGB(const Color1, Color2: TVector3Byte; Tolerance: Byte): boolean;

{ TImage ------------------------------------------------------------- }

type
  { Raised by @link(TImage.MakeExtracted) when coordinates on image
    are wrong.
    Possibly I will use it in more routines in the future. }
  EImagePosOutOfRange = class(Exception);

  EImageLerpError = class(Exception);
  EImageLerpInvalidClasses = class(EImageLerpError);
  EImageLerpDifferentSizes = class(EImageLerpError);

  { Used to potentially override AlphaChannelType detection,
    see AlphaChannelTypeOverride. }
  TDetectAlphaChannel = (daAuto, daSimpleYesNo, daFullRange);

  { Abstract class for an image with unspecified, possibly compressed,
    memory format. The idea is that both uncompressed images (TImage)
    and compressed images (TS3TCImage) are derived from this class. }
  TEncodedImage = class
  private
    FWidth, FHeight, FDepth: Cardinal;
  protected
    { Operate on this by Get/Realloc/FreeMem.
      It's always freed and nil'ed in destructor. }
    FRawPixels: Pointer;
  public
    destructor Destroy; override;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    property Depth: Cardinal read FDepth;

    property RawPixels: Pointer read FRawPixels;

    { Is an image empty.

      @true means that RawPixels = @nil,
      and Width * Height * Depth = 0
      (so either Width = 0 or Height = 0 or Depth = 0).

      @false means that RawPixels <> nil and Width * Height * Depth <> 0
      (so all Width > 0 and Height > 0 and Depth > 0, since they are
      Cardinal (unsigned) always). }
    function IsNull: boolean;

    { Does an image have an alpha channel.

      You may also be interested in the AlphaChannelType.
      AlphaChannelType answers always atNone if HasAlpha = false,
      and always atSimpleYesNo or atFullRange if HasAlpha = true.
      But AlphaChannelType may perform longer analysis of pixels
      (to differ between atSimpleYesNo and atFullRange), while this
      function always executes ultra-fast (as it's constant for each
      TImage descendant).

      @italic(Descendants implementors notes:) in this class, TImage,
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

      This method analyzes every pixel. It's alpha is considered "simple"
      if it's <= AlphaTolerance, or >= 255 - AlphaTolerance.
      So for the default AlphaTolerance, "simple" alpha means only exactly
      0 or 255 (maximum Byte values).
      The method returns true if the ratio of non-simple pixels is
      WrongPixelsTolerance. For example, default WrongPixelsTolerance = 0
      means that every pixel must have "simple" alpha channel.
      Greated WrongPixelsTolerance values may allow some tolerance,
      for example WrongPixelsTolerance = 0.01 allows 1 percent of pixels
      to fail the "simple alpha" test and the image can still be considered
      "simple yes/no alpha channel".

      In summary, default Tolerance values are 0, so exactly all pixels
      must have exactly full or exactly none alpha. Increasing
      tolerance values (for example, AlphaTolerance = 5
      and WrongPixelsTolerance = 0.01 may be good start --- still conservative
      enough, and tolerate small deviations) allows you to accept
      more images as simple yes/no alpha. Of course too large tolerance
      values have no sense --- AlphaTolerance >= 128, or WrongPixelsTolerance >= 1.0
      will cause all images to be accepted as "simple yes/no alpha".

      @italic(Descendants implementors notes:) in this class, this simply
      always returns atNone. For descendants that have alpha channel,
      implement it, honouring AlphaTolerance and WrongPixelsTolerance as
      described. }
    function AlphaChannelType(
      const AlphaTolerance: Byte = 0;
      const WrongPixelsTolerance: Single = 0.0): TAlphaChannelType; virtual;

    { Usually calls @link(AlphaChannelType), but allows you to override
      detection by TDetectAlphaChannel.

      When DetectAlphaChannel is daAuto, this is simply equivalent to
      normal AlphaChannelType.

      For other values of DetectAlphaChannel,
      when the image has any alpha channel,
      then DetectAlphaChannel decides whether this is full range or simple
      yes/no alpha channel. This means that nice algorithm of AlphaChannelType
      will not be used. This allows you to give user control over alpha
      channel detection,
      like for [http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_alpha_channel_detection]. }
    function AlphaChannelTypeOverride(
      const DetectAlphaChannel: TDetectAlphaChannel;
      const AlphaTolerance: Byte = 0;
      const WrongPixelsTolerance: Single = 0.0): TAlphaChannelType;
  end;

  { An abstract class representing image as a simple array of pixels.
    RawPixels is a pointer to Width * Height * Depth of pixels.

    What exactly is a "pixel" is undefined in this class. Each descendant
    of TImage defines it's own pixel encoding and interpretation.
    The only requirement is that all pixels have the same size (PixelSize).
    For example, for TRGBImage a "pixel" is a TVector3Byte type
    representing a (red, green, blue) color value.

    When Depth > 1, the image is actually a 3D (not just 2D!) image.
    We call the particular 2D layers then "slices".
    Although some TImage methods (and functions in other units, like GLImages)
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
    You must only worry to always free created TImage instances
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
  TImage = class(TEncodedImage)
  private
    procedure NotImplemented(const AMethodName: string);
  protected
    { Check that both images have the same sizes and Second image class
      descends from First image class. If not, raise appropriate ELerpXxx
      exceptions.

      Some implementation of TRGBImage.LerpWith may require
      other checks (since LerpWith may be sometimes allowed between unequal
      classes), so this doesn't have to be used by all TRGBImage.LerpWith
      implementations (although it's comfortable for simple implementations). }
    procedure LerpSimpleCheckConditions(SecondImage: TImage);
  public
    { Constructor without parameters creates image with Width = Height = Depth = 0
      and RawPixels = nil, so IsNull will return @true.

      Both constructors must be virtual, this allows to implement things
      like TImage.MakeCopy. }
    constructor Create; overload; virtual;
    constructor Create(
      const AWidth, AHeight: Cardinal;
      const ADepth: Cardinal = 1); overload; virtual;

    { This is equivalent to SetSize(0, 0, 0).
      It sets Width = Height = 0 and RawPixels = nil. }
    procedure Null;

    { Change Width and Height to given AWidth, AHeight.
      RawPixels is changed to point to the new memory.
      Previous image contents are lost. (use one of the other methods,
      like @link(Resize), if you want to change image size preserving
      it's contents) }
    procedure SetSize(
      const AWidth, AHeight: Cardinal;
      const ADepth: Cardinal = 1);

    { Size of TPixel in bytes for this TImage descendant. }
    class function PixelSize: Cardinal; virtual; abstract;

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

      Note that this may be not overriden in every TImage descendant,
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
      in TImage, but it always returns some descendant of TImage). }
    function MakeCopy: TImage;

    { Change Width and Height and appropriately stretch
      image contents.

      If ResizeToX or ResizeToY is 0 then it means to take
      Width or Height, respectively.
      So e.g. using ResizeToX = ResizeToY = 0 is the same thing
      as using ResizeToX = Width and ResizeToY = Height and this is NOP.

      Remember that resizing may change RawPixels pointer, so all pointers
      that you aquired using functions like
      RawPixels, RGBPixels, AlphaPixels, RowPtr, PixelPtr
      may be invalid after calling Resize.

      If ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from ProgressUnit to indicate progress of operation. }
    procedure Resize(ResizeToX, ResizeToY: Cardinal;
      const ProgressTitle: string = '');

    { Create a new TImage instance with size ResizeToX, ResizeToY
      and pixels copied from us and appropriately stretched.
      Class of new instance is the same as our class.

      As with @link(Resize), ResizeTo* = 0 means to use current Width/Height.
      So e.g. using MakeResized(0, 0) is the same thing as using MakeCopy.

      As with @link(Resize),
      if ProgressTitle <> '' this will call Progress.Init/Step/Fini
      from ProgressUnit to indicate progress of operation. }
    function MakeResized(ResizeToX, ResizeToY: Cardinal;
      const ProgressTitle: string = ''): TImage;

    { Mirror image horizotally (i.e. right edge is swapped with left edge) }
    procedure FlipHorizontal;

    { Make rotated version of the image.
      See @link(Rotate) for description of parameters. }
    function MakeRotated(Angle: Integer): TImage;

    { Rotate image by Angle * 90 degrees, clockwise.
      For example, 0 does nothing. 1 rotates by 90 degrees, 2 rotates
      by 180, 3 rotates by 270. All other values (negative too) are circular
      (modulo), so e.g. 4 again does nothing, 5 rotates by 90 degrees and so on. }
    procedure Rotate(const Angle: Integer);

    { Create a new instance with the same class, and size
      TileX * Width and TileY * Height and contents being our contents
      duplicated (tiled).
      Must be TileX, TileY > 0. }
    function MakeTiled(TileX, TileY: Cardinal): TImage;

    { Extract rectangular area of this image.
      X0 and Y0 are start position (lower-left corner),
      ExtractWidth, ExtractHeight specify size of area.

      This checks parameters for correctness -- if start position in not
      good or ExtractWidth/Height are too large exception
      @link(EImagePosOutOfRange) is raised. }
    function MakeExtracted(X0, Y0, ExtractWidth, ExtractHeight: Cardinal): TImage;

    { Set all image pixels to the same value.
      This is implemented only in descendants that represent a pixel
      as a TVector4Byte (e.g. TRGBAlphaImage) or TVector3Byte
      (e.g. TRGBImage, 4th component is ignored in this case).

      In this class this simply raises EInternalError to say 'not implemented'.
      This also means that you must not call inherited in
      descendants when overriding this method. }
    procedure Clear(const Pixel: TVector4Byte); virtual;

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
       const ColorModulator: TColorModulatorByteFunc): TImage;

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

    {$ifdef FPC}

    { Convert every image color using Color*Convert function from VectorMath.
      "Channel" parameter determines which Color*Convert function to use
      (Red, Green or Blue), must be 0, 1 or 2.

      Implemented if and only if ModulateRGB is implemented. }
    procedure ConvertToChannelRGB(Channel: Integer);

    { Converts every image color using Color*Strip function from VectorMath.
      "Channel" parameter determines which Color*Strip function to use
      (Red, Green or Blue), must be 0, 1 or 2.

      Implemented if and only if ModulateRGB is implemented. }
    procedure StripToChannelRGB(Channel: Integer);

    {$endif FPC}

    { Check if given Image has the same class, the same sizes
      (Width, Height) and contains exactly the same pixel values. }
    function IsEqual(Image: TImage): boolean;

    { This is like IsEqual, but is compares only given parts of the images.
      Note that it's your responsibility to make sure that given areas
      are really within the sizes of Self or Image.

      Overloaded version without SelfXxx parameters compares whole Self
      to given part of Image. Analogously, version without ImageXxx parameters
      compares whole Image to part of Self.

      @groupBegin }
    function ArePartsEqual(
      const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
      Image: TImage;
      const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean; overload;

    function ArePartsEqual(
      Image: TImage;
      const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean; overload;

    function ArePartsEqual(
      const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
      Image: TImage): boolean; overload;
    { @groupEnd }

    { These check that Image and Self have equal classes, and then
      copy Self to Image or Image to Self.
      X0 and Y0 is each case are the position on the destinantion image.

      Optionally you can specify dimensions of rectangle from source image
      to use (please note that they are assumed correct here; so you better
      check them, or risk invalid memory reads).

      @groupBegin }
    procedure CopyFrom(Image: TImage; const X0, Y0: Cardinal);
    procedure CopyFrom(Image: TImage; const X0, Y0: Cardinal;
      const SourceX0, SourceY0, SourceWidth, SourceHeight: Cardinal);
    procedure CopyTo(Image: TImage; const X0, Y0: Cardinal);
    { @groupEnd }

    { Makes linear interpolation of colors from this image and the SecondImage.
      Intuitively, every pixel in new image is set to

@preformatted(
  (1 - Value) * Self[pixel] + Value * SecondImage[pixel]
)

      Both images need to have the exact same size.
      If they are not, EImageLerpDifferentSizes is raised.

      Not all TImage combinations are allowed. Every subclass is required
      to override this to at least handle Lerp between itself.
      That is, TRGBImage.Lerp has to handle Lerp with other TRGBImage,
      TRGBAlphaImage.Lerp has to handle Lerp with other TRGBAlphaImage etc.
      Other combinations may be permitted, if useful and implemented.
      EImageLerpInvalidClasses is raised if given class combinations are
      not allowed.

      In this class, this simply always raises EImageLerpInvalidClasses.

      @raises(EImageLerpDifferentSizes When SecondImage size differs
        from this image.)
      @raises(EImageLerpInvalidClasses When Lerp between this TImage
        descendant class and SecondImage class is not implemented.) }
    procedure LerpWith(const Value: Single; SecondImage: TImage); virtual;
  end;

  TImageList = specialize TFPGObjectList<TImage>;

  TEncodedImageList = specialize TFPGObjectList<TEncodedImage>;

  TS3TCCompression = (
    { s3tcDxt1_RGB and s3tcDxt1_RGBA are the same compression method,
      except in s3tcDxt1_RGB the alpha information is ignored,
      while in s3tcDxt1_RGBA we have simple yes/no alpha.

      The difference is equivalent to OpenGL differences in treating
      @unorderedList(
        @itemSpacing compact
        @item GL_COMPRESSED_RGB_S3TC_DXT1_EXT and
        @item GL_COMPRESSED_RGBA_S3TC_DXT1_EXT.
      )
    }
    s3tcDxt1_RGB,
    s3tcDxt1_RGBA,

    { DXT3 and DXT5 are always treated like they had full-range alpha channel. }
    s3tcDxt3,
    s3tcDxt5);

  ECannotFlipS3TCImage = class(Exception);

  { Image encoded with S3TC compression. }
  TS3TCImage = class(TEncodedImage)
  private
    FCompression: TS3TCCompression;
    FSize: Cardinal;
  public
    constructor Create(const AWidth, AHeight: Cardinal;
      const ADepth: Cardinal;
      const ACompression: TS3TCCompression);

    property Compression: TS3TCCompression read FCompression;

    { Size of the whole image data inside RawPixels, in bytes. }
    property Size: Cardinal read FSize;

    function HasAlpha: boolean; override;
    function AlphaChannelType(
      const AlphaTolerance: Byte;
      const WrongPixelsTolerance: Single): TAlphaChannelType; override;

    { Flip compressed image vertically, losslessly.

      This usese the knowledge of how S3TC compression works,
      how the data is coded for each 4x4 block,
      to losslessly flip the image, without re-compressing it.
      The idea is described here
      [http://users.telenet.be/tfautre/softdev/ddsload/explanation.htm].

      @raises(ECannotFlipS3TCImage
        Raises ECannotFlipS3TCImage when image Height is not 1, 2, 3
        or a multiple of 4 (since the trick doesn't work in these cases,
        pixels would move between 4x4 blocks). Note that if Height
        is a power of two (as common for OpenGL textures) then it's
        always possible to make a flip.) }
    procedure FlipVertical;

    { Decompress S3TC image.

      This uses DecompressS3TC variable, so you have to initialialize it
      first (for example to GLImage.GLDecompressS3TC) before using this.

      @raises(ECannotDecompressS3TC If cannot decompress S3TC,
        because decompressor is not set and there was some other error
        within decompressor.) }
    function Decompress: TImage;

    function MakeCopy: TS3TCImage;
  end;

  ECannotDecompressS3TC = class(Exception);

  TDecompressS3TCFunction = function (Image: TS3TCImage): TImage;

var
  { Assign here S3TC decompression function that is available.
    This way the "decompressor" is pluggable, which means that
    you can even use OpenGL to decompress S3TC textures, if you're going
    to load images while some OpenGL context is active. }
  DecompressS3TC: TDecompressS3TCFunction;

{ TImageClass and arrays of TImageClasses ----------------------------- }

type
  { }
  TImageClass = class of TImage;
  TEncodedImageClass = class of TEncodedImage;
  TDynArrayImageClasses = array of TImageClass;

{ Check is ImageClass one of the items in the ImageClasses array,
  or a descendant of one of them. }
function InImageClasses(ImageClass: TImageClass;
  const ImageClasses: array of TImageClass): boolean; overload;

{ Check is Image class one of the items in the ImageClasses array,
  or a descendant of one of them.
  This is a shortcut for InImageClasses(Image.ClassType, ImageClasses). }
function InImageClasses(Image: TImage;
  const ImageClasses: array of TImageClass): boolean; overload;

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
function ImageClassesEqual(const Ar1, Ar2: array of TImageClass): boolean;

procedure ImageClassesAssign(var Variable: TDynArrayImageClasses;
  const NewValue: array of TImageClass);

{ TImage basic descendants ------------------------------------------------- }

type
  TRGBAlphaImage = class;
  TRGBFloatImage = class;
  TGrayscaleImage = class;
  TGrayscaleAlphaImage = class;

  { Image with pixel represented as a TVector3Byte (red, green, blue). }
  TRGBImage = class(TImage)
  private
    function GetRGBPixels: PVector3Byte;
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

    { Create a new TRGBAlphaImage object with RGB colors
      copied from this object, but alpha of each pixel is set
      to some random value (whatever was at that particular memory
      place at that time). }
    function ToRGBAlphaImage_AlphaDontCare: TRGBAlphaImage;

    { Like @link(ToRGBAlphaImage_AlphaDontCare), but alpha of every
      pixel is set to given Alpha. }
    function ToRGBAlphaImage_AlphaConst(Alpha: byte): TRGBAlphaImage;

    { Like @link(ToRGBAlphaImage_AlphaDontCare), but alpha of every
      pixel is set to either AlphaOnColor (when color of pixel
      is equal to AlphaColor with Tolerance, see @link(EqualRGB))
      or AlphaOnNoColor. }
    function ToRGBAlphaImage_AlphaDecide(
      const AlphaColor: TVector3Byte; Tolerance: Byte;
      AlphaOnColor: Byte; AlphaOnNoColor: Byte): TRGBAlphaImage;

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

    { Draw vertical line. Must be x1 <= x2, else it is NOOP. }
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TVector3Byte);

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

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;
  end;

  TRGBAlphaImage = class(TImage)
  private
    function GetAlphaPixels: PVector4Byte;
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

    function AlphaChannelType(
      const AlphaTolerance: Byte = 0;
      const WrongPixelsTolerance: Single = 0.0): TAlphaChannelType; override;

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;

    { Remove alpha channel, creating new TRGBImage. }
    function ToRGBImage: TRGBImage;
  end;

  { Image with high-precision RGB colors encoded as 3 floats. }
  TRGBFloatImage = class(TImage)
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

    {$ifdef FPC}
    { Every component (red, green, blue) of every pixel
      is multiplied by Scale. }
    procedure ScaleColors(const Scale: Single);

    { Every component (red, green, blue) or every pixel
      is changed to Power(Value, Exp).
      So e.g. Exp = 1/2.2 gives commonly used gamma correction. }
    procedure ExpColors(const Exp: Single);
    {$endif}

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;
  end;

  { Grayscale image. Color is a simple Byte value. }
  TGrayscaleImage = class(TImage)
  private
    function GetGrayscalePixels: PByte;
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

    { Create new TGrayscaleAlphaImage with grayscale channel copied
      from this object, and alpha channel filled with constant Alpha value. }
    function ToGrayscaleAlphaImage_AlphaConst(Alpha: byte): TGrayscaleAlphaImage;

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;
  end;

  { Grayscale image with an alpha channel.
    Each pixel is two bytes: grayscale + alpha. }
  TGrayscaleAlphaImage = class(TImage)
  private
    function GetGrayscaleAlphaPixels: PVector2Byte;
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

    function AlphaChannelType(
      const AlphaTolerance: Byte = 0;
      const WrongPixelsTolerance: Single = 0.0): TAlphaChannelType; override;

    procedure LerpWith(const Value: Single; SecondImage: TImage); override;
  end;

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

  LoadXxx: load image from Stream.

  They must honour AllowedImageClasses and ForbiddenConvs, just like
  LoadImage does. Except they don't have to care about returning all TImage
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

  TImageLoadConversion = (
    ilcAlphaDelete, ilcFloatPrecDelete, ilcRGBFlattenToGrayscale,
    ilcAlphaAdd, ilcFloatPrecAdd, ilcGrayscaleExpandToRGB);
  TImageLoadConversions = set of TImageLoadConversion;

  { }
  EUnableToLoadImage = class(EImageLoadError);

function LoadPNG(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadBMP(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadGIF(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadTGA(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadSGI(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadTIFF(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadJP2(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadEXR(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadJPEG(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadXPM(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadPSD(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

{ Load PCX image.

  Only 256-color PCX can be handled.
  This will not probably be ever improved (al least by me, Kambi),
  since I don't use PCX images anymore.
  Use PNG if you want lossless compression. }
function LoadPCX(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

{ Load PPM image.
  Loads only the first image in .ppm file. }
function LoadPPM(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

{ Load PNM image (PNM, PGM, PBM, PPM) through FpImage.
  Note that for PPM, for now it's more advised to use our LoadPPM. }
function LoadPNM(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

function LoadIPL(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

{ Load RGBE image.
  This low-level function can load to TRGBFloatImage (preserving image data)
  or to TRGBImage (loosing floating point precision of RGBE format). }
function LoadRGBE(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

{ Load DDS image file into a single 2D image. This simply returns the first
  image found in DDS file, which should be the main image.
  If you want to investigate other images in DDS, you have to use TDDSImage
  class. }
function LoadDDS(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;

{ saving image (format-specific) --------------------------------------------

  SaveXxx. Each file format may have specialized SaveXxx that allows
  you to give some parameters special for given format.

  Each format must also have procedure with two parameters
  (Img: TImage; Stream: TStream), this will be used with
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
procedure SaveBMP(Img: TImage; Stream: TStream);
procedure SavePNG(Img: TImage; Stream: TStream; interlaced: boolean); overload;
procedure SavePNG(Img: TImage; Stream: TStream); { interlaced = false } overload;
{ }
procedure SaveJPEG(Img: TImage; Stream: TStream);
{ }
procedure SavePPM(Img: TImage; Stream: TStream; binary: boolean); overload;
procedure SavePPM(Img: TImage; Stream: TStream); { binary = true } overload;
{ }
procedure SaveRGBE(Img: TImage; Stream: TStream);

procedure SaveDDS(Img: TImage; Stream: TStream);

{ File formats managing ----------------------------------------------------- }

type
  { }
  TImageFormat = (
    { We handle uncompressed BMP images. }
    ifBMP,

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

    ifPPM,
    ifIPL,

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

    { Image formats below are supported
      by converting them  "under the hood" with ImageMagick.
      This is available only if this unit is compiled with FPC
      (i.e. not with Delphi) on platforms where ExecuteProcess is
      implemented. And ImageMagick must be installed and available on $PATH. }
    ifTIFF, ifSGI, ifJP2, ifEXR,

    { Image formats below are supported by FPImage. }
    ifJPEG, ifGIF, ifTGA, ifXPM, ifPSD, ifPCX, ifPNM,

    { We handle fully DDS (DirectDraw Surface) image format.
      See also TDDSImage class in DDS unit,
      this exposes even more features of the DDS image format. }
    ifDDS);
  TImageFormats = set of TImageFormat;

  TImageLoadFunc = function (Stream: TStream;
    const AllowedImageClasses: array of TImageClass;
    const ForbiddenConvs: TImageLoadConversions): TImage;
  TImageSaveFunc = procedure (Img: TImage; Stream: TStream);

  { Possible TImage classes that can be returned by Load method
    of this file format. It's assumed that appropriate Load can return
    only these classes, and any of these classes,
    and can convert (as much as ForbiddenConvs will allow) between them.

    If the LoadImage will be called allowing some TImage descendants
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

  { Possible TImage classes supported by Save method of this file format. }
  TImageSaveHandledClasses = (
    scRGB,
    scG_GA_RGB_RGBA,
    scRGB_RGBFloat
  );

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

    ExtsCount: TImageFormatInfoExtsCount;
    { File extensions recognized as this image file format.

      These file extensions must be lowercase, without leading dot '.'.
      First extension is the default extension of this file format
      (some procedures make use of it). }
    Exts: array[TImageFormatInfoExtsCount] of string;

    { Load method for this file format.
      @nil if cannot be loaded. }
    Load: TImageLoadFunc;

    { If Load is assigned, this describes what TImage descendants
      can be returned by this Load. LoadImage will need this information,
      to make necessary convertions to other TImage classes,
      when possible. }
    LoadedClasses: TImageLoadHandledClasses;

    { Save method for this file format.
      @nil if cannot be saved. }
    Save: TImageSaveFunc;
    SavedClasses: TImageSaveHandledClasses;
  end;

const
  ImageFormatInfos :array[TImageFormat]of TImageFormatInfo =
  ( ( FormatName: 'Windows BMP image';
      ExtsCount: 1; Exts: ('bmp', '', '');
      Load: @LoadBMP; LoadedClasses: lcRGB_RGBA;
      Save: @SaveBMP; SavedClasses: scRGB),
    { Portable Network Graphic } { }
    ( FormatName: 'PNG image';
      ExtsCount: 1; Exts: ('png', '', '');
      Load: @LoadPNG; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: @SavePNG; SavedClasses: scG_GA_RGB_RGBA; ),
    { Portable Pixel Map } { }
    ( FormatName: 'PPM image';
      ExtsCount: 1; Exts: ('ppm', '', '');
      Load: @LoadPPM; LoadedClasses: lcRGB;
      Save: @SavePPM; SavedClasses: scRGB; ),
    ( FormatName: 'IPLab image';
      ExtsCount: 1; Exts: ('ipl', '', '');
      Load: @LoadIPL; LoadedClasses: lcRGB;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'RGBE (RGB+Exponent) image';
      ExtsCount: 2; Exts: ('rgbe', 'pic', '');
      Load: @LoadRGBE; LoadedClasses: lcRGB_RGBFloat;
      Save: @SaveRGBE; SavedClasses: scRGB_RGBFloat; ),

    { Loaded using ImageMagick } { }

    ( FormatName: 'TIFF image';
      ExtsCount: 1; Exts: ('tif', '', '');
      Load: @LoadTIFF; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'SGI image';
      ExtsCount: 1; Exts: ('sgi', '', '');
      Load: @LoadSGI; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'JP2 image';
      ExtsCount: 1; Exts: ('jp2', '', '');
      Load: @LoadJP2; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'EXR image';
      ExtsCount: 1; Exts: ('exr', '', '');
      Load: @LoadEXR; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),

    { Loaded using FPImage } { }

    { JFIF, JPEG File Interchange Format } { }
    ( FormatName: 'JPEG image';
      ExtsCount: 3; Exts: ('jpg', 'jpeg', 'jpe');
      Load: @LoadJPEG; LoadedClasses: lcRGB_RGBA;
      Save: @SaveJPEG; SavedClasses: scRGB { actually scRGB_RGBA }),
    { Graphics Interchange Format } { }
    ( FormatName: 'GIF image';
      ExtsCount: 1; Exts: ('gif', '', '');
      Load: @LoadGIF; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'TarGA image';
      ExtsCount: 1; Exts: ('tga', '', '');
      Load: @LoadTGA; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'XPM image';
      ExtsCount: 1; Exts: ('xpm', '', '');
      Load: @LoadXPM; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'PSD image';
      ExtsCount: 1; Exts: ('psd', '', '');
      Load: @LoadPSD; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'ZSoft PCX image';
      ExtsCount: 1; Exts: ('pcx', '', '');
      Load: @LoadPCX; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),
    ( FormatName: 'PNM image';
      ExtsCount: 3; Exts: ('pnm', 'pgm', 'pbm');
      Load: @LoadPNM; LoadedClasses: lcRGB_RGBA;
      Save: nil; SavedClasses: scRGB; ),

    { Direct Draw Surface } { }
    ( FormatName: 'DDS image';
      ExtsCount: 1; Exts: ('dds', '', '');
      Load: @LoadDDS; LoadedClasses: lcG_GA_RGB_RGBA;
      Save: @SaveDDS; SavedClasses: scG_GA_RGB_RGBA; )
  );

  DefaultSaveImageFormat: TImageFormat = ifBMP;

{ Find image file format with given file extension.
  FileExt may, but doesn't have to, contain the leading dot.
  Returns @false if no format matching given extension. }
function FileExtToImageFormat(FileExt: string;
  OnlyLoadable, OnlySaveable: boolean; out ImgFormat: TImageFormat): boolean;

{ Find image file format with given file extension, return default
  format if not found.
  Like FileExtToImageFormat, but returns DefFormat if no matching format found. }
function FileExtToImageFormatDef(const FileExt: string;
  OnlyLoadable, OnlySaveable: boolean; DefFormat: TImageFormat): TImageFormat;

{ Check do we handle image file format with given file extension.
  Like FileExtToImageFormat, except here we just check if the file extension
  is a handled format --- we are not interested in actual TImageFormat value. }
function IsFileExtToImageFormat(const FileExt: string;
  OnlyLoadable, OnlySaveable: boolean): boolean;

{ Check do we handle loading image file format with given file extension.
  Like IsFileExtToImageFormat, with OnlyLoadable = @true, OnlySaveable = @false. }
function IsFileExtLoadableImage(const FileExt: string): boolean;

type
  ENoExistingImageExt = class(Exception);

{ Find an existing filename by appending known image files extensions.
  Treat a given string S like a filename with a trailing dot and an extension.
  For each known image file extension, try to append it (with leading dot)
  and check does the file exists (as a regular file, that is by
  NormalFileExists).

  If found --- return complete file name. If not found,
  FindExistingImageExt raises ENoExistingImageExt,
  while TryFindExistingImageExt returns ''.

  @raises(ENoExistingImageExt FindExistingImageExt raises this if no existing
    image file can be found.)

  @param(OnlyLoadable If @true, will try to append only image file extensions
    that we can load.) }
function FindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;
function TryFindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;

{ List available image file formats.

  This is basically for debug/info purposes, you can show this to user
  to let him know which formats are supported (and by which extensions
  they are recognized). Although almost always a better way to show
  this to user is just to use SaveImage_FileFilters with a save dialog
  like TGLWindow.FileDialog, this shows file types in the open/save dialog,
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

const
  AllImageLoadConversions: TImageLoadConversions =
  [Low(TImageLoadConversion) .. High(TImageLoadConversion)];

{ TODO: zrobic LoadImageGuess ktore zgaduje format na podstawie
  zawartosci. }

(*The ultimate procedure to load an image from a file.

  Simple examples:

@longCode(#
  { When you don't care what TImage descendant you get: }
  Image := LoadImage('filename.png', [], []);

  { When you insist on getting TRGBImage, that is 8-bit color image
    without an alpha channel. }
  ImageRGB := LoadImage('filename.png', [TRGBImage], []) as TRGBImage;
#)

  Image file format is guess from FileName (or filename extension
  in TypeExt (may but doesn't have to contain leading dot),
  or can be just given explicitly by Format).

  AllowedImageClasses says what image classes are allowed.
  As a special case, AllowedImageClasses = [] is equivalent to
  AllowedImageClasses = [TImage] which says that all TImage descendants
  are allowed. Then this function will do everything it can to load
  any image into the best subclass of TImage, losing as little image
  information it can.

  Example: consider you're loading a PNG file. Let's suppose you're
  loading it with AllowedImageClasses = []. Then you can get
  TGrayscaleImage, TGrayscaleAlphaImage, TRGBImage, TRGBAlphaImage,
  depending on whether PNG file is grayscale or not and has alpha or not.
  Now let's suppose you specified AllowedImageClasses = [TRGBImage].
  If PNG file will not be grayscale and not have alpha channel,
  LoadImage will return TRGBImage descendant, as before.
  But if PNG fill *will* have alpha channel then

  @orderedList(

    @item(if ForbiddenConvs does not contain [ilcAlphaDelete],
      LoadImage will simply ignore (strip) alpha channel and return you TRGBImage)

    @item(if ForbiddenConvs does contain [ilcAlphaDelete],
      LoadImage will exit with exception EUnableToLoadImage.
      This is sometimes safer, since you can't accidentaly ignore alpha
      channel that was present in file.)
  )

  Similar thing for grayscale: if image file was grayscale but you requested
  only TRGBImage, then grayscale may be "expanded" into full three-channel
  RGB. Unless prevented by ilcGrayscaleExpandToRGB inside ForbiddenConvs.

  There can also happen reverse situation: you e.g. insist that
  AllowedImageClasses = [TRGBAlphaImage] but given PNG image does not
  have alpha channel. In this case LoadImage may add "dummy" alpha channel
  (everywhere equal to 1.0 or High(Byte)).
  Similar thing when you e.g. gave AllowedImageClasses = [TRGBFloatImage]
  but you're loading from PNG image. In this case you want float precision,
  but image file cannot offer it. So LoadImage can simply convert
  discreet values to appropriating floating point values.
  This is usually harmless, but sometimes it may be unwanted, since
  you're getting something in different format than was in file.
  So you can add to ForbiddenConvs ilcAlphaAdd and/or ilcFloatPrecAdd
  to prevent that.

  If at any point LoadImage will find that it's unable to satisfy
  AllowedImageClasses without doing any forbidden convertions
  in ForbiddenConvs, it will raise @link(EUnableToLoadImage).

  @raises(EUnableToLoadImage If Image cannot be loaded into
    allowed AllowedImageClasses (at least, cannot be loaded
    without using any ForbiddenConvs).)

  @raises(EImageFormatNotSupported If image file format cannot be loaded at all.
    This can happen only if format is totally unknown (e.g. not recognized
    FileName extension) or if image format has no Load method at all.)

  @groupBegin *)
function LoadImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage; overload;
function LoadImage(Stream: TStream; const typeext: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage; overload;
function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage; overload;
function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TImage; overload;
{ @groupEnd }

{ saving image --------------------------------------------------------------- }

type
  { }
  EImageSaveError = class(Exception);

{ Save image to a file.

  File format is determined by given FileName, filename extension (TypeExt)
  or just given explicitly as Format parameter.

  Image class does @bold(not)
  affect the created image file format, on the assumption that the
  "memory format" of the image (what TImage descendant is used)
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
procedure SaveImage(const img: TImage; const Format: TImageFormat; Stream: TStream); overload;
procedure SaveImage(const img: TImage; const typeext: string; Stream: TStream); overload;
procedure SaveImage(const Img: TImage; const fname: string); overload;
{ @groupEnd }

{ Other TImage processing ---------------------------------------------------- }

{ Add and set constant alpha channel of given image.
  If image doesn't have alpha channel, we will create new Img instance
  (old instance will be freed) with colors copy.
  Alpha channel is then filled with AlphaConst }
procedure ImageAlphaConstTo1st(var Img: TImage; const AlphaConst: byte);

{ Choose TImage descendant best matching for this image file format.
  The only purpose of this for now is to pick TRGBFloatImage for RGBE files,
  chooses TRGBImage for anything else.

  For the overloaded version with FileName, file format is determined
  by FileName extension.

  @groupBegin }
function ImageClassBestForSavingToFormat(ImgFormat: TImageFormat): TImageClass; overload;
function ImageClassBestForSavingToFormat(const FileName: string): TImageClass; overload;
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

{$undef read_interface}

implementation

uses ProgressUnit, KambiStringUtils, KambiFilesUtils, KambiWarnings, DDS;

{ image loading utilities --------------------------------------------------- }

{ Helper methods for implemented LoadImage. }

const
  ConvToStr: array[TImageLoadConversion]of string = (
  'delete alpha channel',
  'lose float precision',
  'flatten RGB colors to grayscale',
  'add dummy constant alpha channel',
  'add useless float precision',
  'expand grayscale to RGB (three channels)'
  );

{ Check is Conv on the ForbiddenConvs list, if it is raise an exception.
  @raises(EUnableToLoadImage If Conv is forbidden.) }
procedure DoingConversion(
  const Conv: TImageLoadConversion;
  const ForbiddenConvs: TImageLoadConversions);
begin
  if Conv in ForbiddenConvs then
    raise EUnableToLoadImage.Create('LoadImage: to load this image format '+
      'conversion "'+ConvToStr[Conv]+'" must be done, but it is forbidden here');
end;

function ClassAllowed(ImageClass: TImageClass;
  const AllowedImageClasses: array of TImageClass): boolean;
begin
  Result := (High(AllowedImageClasses) = -1) or
    InImageClasses(ImageClass, AllowedImageClasses);
end;

function LoadImageParams(
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): string;

  function ImageClassesToStr(const AllowedImageClasses: array of TImageClass): string;
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

  function ForbiddenConvsToStr(const ForbiddenConvs: TImageLoadConversions): string;
  var
    LC: TImageLoadConversion;
  begin
    if ForbiddenConvs = [] then
      Result := 'none' else
    begin
      Result := '';
      for LC := Low(LC) to High(LC) do
        if LC in ForbiddenConvs then
        begin
          if Result <> '' then Result += ', ';
          Result += ConvToStr[LC];
        end;
    end;
  end;

begin
  Result := 'required class [' + ImageClassesToStr(AllowedImageClasses) +
    '] with forbidden conversions [' + ForbiddenConvsToStr(ForbiddenConvs) + ']';
end;

{ file format specific ------------------------------------------------------- }

{$I images_bmp.inc}
{$I images_png.inc}
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

function TEncodedImage.IsNull: boolean;
begin
 Result := RawPixels = nil;
end;

function TEncodedImage.HasAlpha: boolean;
begin
  Result := false;
end;

function TEncodedImage.AlphaChannelType(
  const AlphaTolerance: Byte;
  const WrongPixelsTolerance: Single): TAlphaChannelType;
begin
  Result := atNone;
end;

function TEncodedImage.AlphaChannelTypeOverride(
  const DetectAlphaChannel: TDetectAlphaChannel;
  const AlphaTolerance: Byte = 0;
  const WrongPixelsTolerance: Single = 0.0): TAlphaChannelType;
begin
  if DetectAlphaChannel = daAuto then
    Result := AlphaChannelType(AlphaTolerance, WrongPixelsTolerance) else
  begin
    if HasAlpha then
    begin
      if DetectAlphaChannel = daFullRange then
        Result := atFullRange else
        Result := atSimpleYesNo;
    end else
      Result := atNone;
  end;
end;

{ TImage ------------------------------------------------------------ }

constructor TImage.Create;
begin
 inherited;
 { Everything is already inited to nil and 0. }
end;

constructor TImage.Create(
  const AWidth, AHeight: Cardinal;
  const ADepth: Cardinal = 1);
begin
 Create;
 SetSize(AWidth, AHeight, ADepth);
end;

procedure TImage.Null;
begin
 FreeMemNiling(FRawPixels);
 FWidth := 0;
 FHeight := 0;
 FDepth := 0;
end;

procedure TImage.SetSize(const AWidth, AHeight: Cardinal;
  const ADepth: Cardinal = 1);
begin
 FreeMemNiling(FRawPixels);
 FWidth := AWidth;
 FHeight := AHeight;
 FDepth := ADepth;
 if (AWidth <> 0) and (AHeight <> 0) and (ADepth <> 0) then
  FRawPixels := GetMem(PixelSize * AWidth * AHeight * ADepth);
end;

function TImage.PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): Pointer;
begin
 Result := PointerAdd(RawPixels, PixelSize * (Width * (Height * Z + Y) + X));
end;

function TImage.RowPtr(const Y: Cardinal; const Z: Cardinal = 0): Pointer;
begin
 Result := PointerAdd(RawPixels, PixelSize * (Width * (Height * Z + Y)));
end;

procedure TImage.NotImplemented(const AMethodName: string);
begin
 raise EInternalError.Create(AMethodName +
   ' method not implemented for this TImage descendant');
end;

procedure TImage.InvertRGBColors;
begin
 NotImplemented('InvertRGBColors');
end;

procedure TImage.SetColorRGB(const x, y: Integer; const v: TVector3Single);
begin
 NotImplemented('SetColorRGB');
end;

function TImage.MakeCopy: TImage;
begin
 Result := TImageClass(Self.ClassType).Create(Width, Height);
 Move(RawPixels^, Result.RawPixels^, Depth * Width * Height * PixelSize);
end;

{ This does the real resizing work.
  It assumes that SourceData and DestinData pointers are already allocated.
  DestinWidth, DestinHeight must not be 0. }
procedure InternalResize(PixelSize: Cardinal;
  SourceData: Pointer; SourceWidth, SourceHeight: Cardinal;
  DestinData: Pointer; DestinWidth, DestinHeight: Cardinal;
  ProgressTitle: string);

var DestinY: Cardinal;

  procedure MakeLineDestinY;
  { write row DestinY of DestinData }
  var DestinX, SourceX, SourceY: Cardinal;
      SourceRow, DestinRow: PtrUInt;
  begin
   SourceY := DestinY * SourceHeight div DestinHeight;
   SourceRow := PtrUInt(SourceData) + SourceWidth * SourceY * PixelSize;
   DestinRow := PtrUInt(DestinData) + DestinWidth * DestinY * PixelSize;

   for DestinX := 0 to DestinWidth - 1 do
   begin
    SourceX := DestinX * SourceWidth div DestinWidth;
    Move(Pointer(SourceRow + SourceX * PixelSize)^,
         Pointer(DestinRow + DestinX * PixelSize)^,
         PixelSize);
   end;
  end;

begin
 if ProgressTitle = '' then
 begin
  for DestinY := 0 to DestinHeight - 1 do MakeLineDestinY;
 end else
 begin
  Progress.Init(DestinHeight, ProgressTitle);
  try
   for DestinY := 0 to DestinHeight - 1 do
    begin MakeLineDestinY; Progress.Step end;
  finally
   Progress.Fini;
  end;
 end;
end;

procedure TImage.Resize(ResizeToX, ResizeToY: Cardinal;
  const ProgressTitle: string);
var NewPixels: Pointer;
begin
 if ((ResizeToX <> 0) and (ResizeToX <> Width)) or
    ((ResizeToY <> 0) and (ResizeToY <> Height)) then
 begin
  { Make both ResizeTo* non-zero. }
  if ResizeToX = 0 then ResizeToX := Width;
  if ResizeToY = 0 then ResizeToY := Height;

  NewPixels := GetMem(ResizeToX * ResizeToY * PixelSize);
  InternalResize(PixelSize, RawPixels, Width, Height,
    NewPixels, ResizeToX, ResizeToY, ProgressTitle);
  FreeMemNiling(FRawPixels);

  FRawPixels := NewPixels;
  FWidth := ResizeToX;
  FHeight := ResizeToY;
 end;
end;

function TImage.MakeResized(ResizeToX, ResizeToY: Cardinal;
  const ProgressTitle: string): TImage;
begin
 { Make both ResizeTo* non-zero. }
 if ResizeToX = 0 then ResizeToX := Width;
 if ResizeToY = 0 then ResizeToY := Height;

 Result := TImageClass(ClassType).Create(ResizeToX, ResizeToY);
 try
  if not IsNull then
   InternalResize(PixelSize,
            RawPixels,        Width,        Height,
     Result.RawPixels, Result.Width, Result.Height,
     ProgressTitle);
 except Result.Free; raise end;
end;

function TImage.MakeRotated(Angle: Integer): TImage;

  procedure Rotate90;
  var
    X, Y: Integer;
  begin
    Result := TImageClass(ClassType).Create(Height, Width);
    for X := 0 to Width - 1 do
      for Y := 0 to Height - 1 do
        Move(PixelPtr(X, Y)^, Result.PixelPtr(Y, Width - 1 - X)^, PixelSize);
  end;

  procedure Rotate180;
  var
    X, Y: Integer;
  begin
    Result := TImageClass(ClassType).Create(Width, Height);
    for X := 0 to Width - 1 do
      for Y := 0 to Height - 1 do
        Move(PixelPtr(X, Y)^, Result.PixelPtr(Width - 1 - X, Height - 1 - Y)^, PixelSize);
  end;

  procedure Rotate270;
  var
    X, Y: Integer;
  begin
    Result := TImageClass(ClassType).Create(Height, Width);
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

procedure TImage.Rotate(const Angle: Integer);
var
  New: TImage;
begin
  New := MakeRotated(Angle);
  try
    SetSize(New.Width, New.Height);
    Move(New.RawPixels^, RawPixels^, New.Width * New.Height * PixelSize);
  finally FreeAndNil(New) end;
end;

procedure TImage.FlipHorizontal;
var ImageRow, TmpPixel, Pix1, Pix2: Pointer;
    x, y: Integer;
begin
 TmpPixel := GetMem(PixelSize);
 try
  for y := 0 to Height-1 do
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

function TImage.MakeTiled(TileX, TileY: Cardinal): TImage;
var i, j: Cardinal;
begin
 Result := TImageClass(ClassType).Create(TileX * Width, TileY * Height);
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

function TImage.MakeExtracted(X0, Y0, ExtractWidth, ExtractHeight: Cardinal): TImage;
var y: Cardinal;
begin
 if x0 + ExtractWidth > Width then
  raise EImagePosOutOfRange.Create('x0 in MakeExtracted out of range');
 if y0 + ExtractHeight > Height then
  raise EImagePosOutOfRange.Create('y0 in MakeExtracted out of range');

 Result := TImageClass(ClassType).Create(ExtractWidth, ExtractHeight);
 try
  for y := 0 to ExtractHeight - 1 do
   Move(PixelPtr(x0, y + y0)^, Result.RowPtr(y)^, PixelSize * ExtractWidth);
 except Result.Free; raise end;
end;

procedure TImage.Clear(const Pixel: TVector4Byte);
begin
 NotImplemented('Clear');
end;

function TImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
 NotImplemented('IsClear');
 { code will never get here (NotImplemented always raises an exception),
   and code "Result := false;" below is only to avoid compiler warning
   that function result is undefined. }
 Result := false;
end;

procedure TImage.TransformRGB(const Matrix: TMatrix3Single);
begin
 NotImplemented('TransformRGB');
end;

procedure TImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
begin
 NotImplemented('ModulateRGB');
end;


function TImage.MakeModulatedRGB(
  const ColorModulator: TColorModulatorByteFunc): TImage;
begin
 Result := MakeCopy;
 Result.ModulateRGB(ColorModulator);
end;

procedure TImage.Grayscale;
begin
 ModulateRGB(@ColorGrayscaleByte);
end;

{$ifdef FPC}

procedure TImage.ConvertToChannelRGB(Channel: Integer);
begin
 case Channel of
  0: ModulateRGB(@ColorRedConvertByte);
  1: ModulateRGB(@ColorGreenConvertByte);
  2: ModulateRGB(@ColorBlueConvertByte);
  else raise EInternalError.Create(
    'ConvertToChannelRGB: Channel must be 0, 1 or 2');
 end;
end;

procedure TImage.StripToChannelRGB(Channel: Integer);
begin
 case Channel of
  0: ModulateRGB(@ColorRedStripByte);
  1: ModulateRGB(@ColorGreenStripByte);
  2: ModulateRGB(@ColorBlueStripByte);
  else raise EInternalError.Create(
    'StripToChannelRGB: Channel must be 0, 1 or 2');
 end;
end;

{$endif FPC}

function TImage.IsEqual(Image: TImage): boolean;
begin
  Result :=
    (Image.ClassType = ClassType) and
    (Image.Width = Width) and
    (Image.Height = Height) and
    (Image.Depth = Depth) and
    (CompareMem(Image.RawPixels, RawPixels, Width * Height * PixelSize));
end;

function TImage.ArePartsEqual(
  const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
  Image: TImage;
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

function TImage.ArePartsEqual(
  Image: TImage;
  const ImageX0, ImageY0, ImageWidth, ImageHeight: Cardinal): boolean;
begin
  Result := ArePartsEqual(
    0, 0, Width, Height,
    Image,
    ImageX0, ImageY0, ImageWidth, ImageHeight);
end;

function TImage.ArePartsEqual(
  const SelfX0, SelfY0, SelfWidth, SelfHeight: Cardinal;
  Image: TImage): boolean;
begin
  Result := ArePartsEqual(
    SelfX0, SelfY0, SelfWidth, SelfHeight,
    Image,
    0, 0, Image.Width, Image.Height);
end;

procedure TImage.CopyFrom(Image: TImage; const X0, Y0: Cardinal;
  const SourceX0, SourceY0, SourceWidth, SourceHeight: Cardinal);
var
  Y: Integer;
  SelfPtr: Pointer;
  ImagePtr: Pointer;
  SelfRowByteWidth, ImageRowByteWidth, CopyRowByteWidth: Cardinal;
begin
  if Image.ClassType <> ClassType then
    raise Exception.Create('Cannot copy pixels from one image to another:' +
      ' different image classes');

  SelfPtr := PixelPtr(X0, Y0);
  ImagePtr := Image.PixelPtr(SourceX0, SourceY0);
  SelfRowByteWidth := Self.Width * PixelSize;
  ImageRowByteWidth := Image.Width * Image.PixelSize;
  CopyRowByteWidth := SourceWidth * Image.PixelSize;
  for Y := 0 to Integer(SourceHeight) - 1 do
  begin
    Move(ImagePtr^, SelfPtr^, CopyRowByteWidth);
    PtrUInt(SelfPtr) := PtrUInt(SelfPtr) + SelfRowByteWidth;
    PtrUInt(ImagePtr) := PtrUInt(ImagePtr) + ImageRowByteWidth;
  end;
end;

procedure TImage.CopyFrom(Image: TImage; const X0, Y0: Cardinal);
begin
  CopyFrom(Image, X0, Y0, 0, 0, Image.Width, Image.Height);
end;

procedure TImage.CopyTo(Image: TImage; const X0, Y0: Cardinal);
begin
  Image.CopyFrom(Self, X0, Y0);
end;

procedure TImage.LerpSimpleCheckConditions(SecondImage: TImage);
begin
  if (Width <> SecondImage.Width) or
     (Height <> SecondImage.Height) then
    raise EImageLerpDifferentSizes.CreateFmt('Linear interpolation not possible, images have different sizes: first has %d x %d, second has %d x %d',
      [Width, Height, SecondImage.Width, SecondImage.Height]);

  if not (SecondImage is Self.ClassType) then
    raise EImageLerpInvalidClasses.CreateFmt('Linear interpolation between %s and %s class not possible',
      [ClassName, SecondImage.ClassName]);
end;

procedure TImage.LerpWith(const Value: Single; SecondImage: TImage);
begin
  raise EImageLerpInvalidClasses.Create('Linear interpolation (TImage.LerpWith) not possible with the base TImage class');
end;

{ TS3TCImage ----------------------------------------------------------------- }

constructor TS3TCImage.Create(const AWidth, AHeight: Cardinal;
  const ADepth: Cardinal;
  const ACompression: TS3TCCompression);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FDepth := ADepth;
  FCompression := ACompression;

  { All DXT* compression methods compress 4x4 pixels into some constant size.
    When Width / Height is not divisible by 4, we have to round up.

    This matches what MSDN docs say about DDS with mipmaps:
    http://msdn.microsoft.com/en-us/library/bb205578(VS.85).aspx
    When mipmaps are used, DDS Width/Height must be power-of-two,
    so the base level is usually divisible by 4. But on the following mipmap
    levels the size decreases, eventually to 1x1, so this still matters.
    And MSDN says then explicitly that with DXT1, you have always
    minimum 8 bytes, and with DXT2-5 minimum 16 bytes.
  }

  case Compression of
    s3tcDxt1_RGB,
    s3tcDxt1_RGBA: FSize := Depth * DivRoundUp(Width, 4) * DivRoundUp(Height, 4) * 8 { 8 bytes for each 16 pixels };
    s3tcDxt3,
    s3tcDxt5: FSize := Depth * DivRoundUp(Width, 4) * DivRoundUp(Height, 4) * 16 { 16 bytes for each 16 pixels };
    else EInternalError.Create('TS3TCImage.Create-Compression?');
  end;

  FRawPixels := GetMem(FSize);
end;

function TS3TCImage.HasAlpha: boolean;
begin
  Result := Compression in [s3tcDxt1_RGBA, s3tcDxt3, s3tcDxt5];
end;

function TS3TCImage.AlphaChannelType(
  const AlphaTolerance: Byte;
  const WrongPixelsTolerance: Single): TAlphaChannelType;
begin
  { S3TCImage doesn't analyze for alpha channel, instead simply assumes
    image is always full-range alpha it if has alpha channel. }
  case Compression of
    s3tcDxt1_RGB : Result := atNone;
    s3tcDxt1_RGBA: Result := atSimpleYesNo;
    s3tcDxt3, s3tcDxt5: Result := atFullRange;
  end;
end;

{$I images_s3tc_flip_vertical.inc}

function TS3TCImage.Decompress: TImage;
begin
  if Assigned(DecompressS3TC) then
    Result := DecompressS3TC(Self) else
    raise ECannotDecompressS3TC.Create('Cannot decompress S3TC image: no decompressor initialized');
end;

function TS3TCImage.MakeCopy: TS3TCImage;
begin
  Result := TS3TCImage.Create(Width, Height, Depth, Compression);
  Assert(Result.Size = Size);
  Move(RawPixels^, Result.RawPixels^, Size);
end;

{ TImageClass and arrays of TImageClasses ----------------------------- }

function InImageClasses(ImageClass: TImageClass;
  const ImageClasses: array of TImageClass): boolean;
var i: Integer;
begin
 for i := 0 to High(ImageClasses) do
  if ImageClass.InheritsFrom(ImageClasses[i]) then
  begin
   Result := true;
   Exit;
  end;
 Result := false;
end;

function InImageClasses(Image: TImage;
  const ImageClasses: array of TImageClass): boolean;
begin
 Result := InImageClasses(TImageClass(Image.ClassType), ImageClasses);
end;

function ImageClassesEqual(const Ar1, Ar2: array of TImageClass): boolean;
var i: Integer;
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
  const NewValue: array of TImageClass);
var i: Integer;
begin
 SetLength(Variable, High(NewValue) + 1);
 for i := 0 to High(NewValue) do
  Variable[i] := NewValue[i];
end;

{ TRGBImage ------------------------------------------------------------ }

constructor TRGBImage.CreateCombined(const MapImage: TRGBImage;
  var ReplaceWhiteImage, ReplaceBlackImage: TRGBImage);
var Map, White, Black, Res: PVector3Byte;
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

 for i := 1 to Width * Height do
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
var i: Cardinal;
    prgb: PVector3byte;
begin
 prgb := RGBPixels;
 for i := 1 to Width * Height do
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
  for I := 1 to Width * Height do
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
  for I := 1 to Width * Height do
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

function TRGBImage.ToRGBAlphaImage_AlphaDontCare: TRGBAlphaImage;
var pi: PVector3Byte;
    pa: PVector4Byte;
    i: Cardinal;
begin
 Result := TRGBAlphaImage.Create(Width, Height);
 pi := RGBPixels;
 pa := Result.AlphaPixels;
 for i := 1 to Width * Height do
 begin
  Move(pi^, pa^, SizeOf(TVector3Byte));
  {pa^[3] := <dont_care_about_this_value>}
  Inc(pi);
  Inc(pa);
 end;
end;

function TRGBImage.ToRGBAlphaImage_AlphaConst(Alpha: byte): TRGBAlphaImage;

{ Note: implementation of this *could* use ToRGBAlphaImage_AlphaDontCare,
  but doesn't, to be faster. }

var pi: PVector3Byte;
    pa: PVector4Byte;
    i: Cardinal;
begin
 Result := TRGBAlphaImage.Create(Width, Height);
 pi := RGBPixels;
 pa := Result.AlphaPixels;
 for i := 1 to Width * Height do
 begin
  Move(pi^, pa^, SizeOf(TVector3Byte));
  pa^[3] := Alpha;
  Inc(pi);
  Inc(pa);
 end;
end;

function TRGBImage.ToRGBAlphaImage_AlphaDecide(
  const AlphaColor: TVector3Byte;
  Tolerance: byte; AlphaOnColor: byte; AlphaOnNoColor: byte): TRGBAlphaImage;
begin
 Result := ToRGBAlphaImage_AlphaDontCare;
 Result.AlphaDecide(AlphaColor, Tolerance, AlphaOnColor, AlphaOnNoColor);
end;

function TRGBImage.ToRGBFloat: TRGBFloatImage;
var
  PFloat: PVector3Single;
  PByte: PVector3Byte;
  i: Cardinal;
begin
  result := TRGBFloatImage.Create(Width, Height);
  try
    PByte := RGBPixels;
    PFloat := Result.RGBFloatPixels;
    for i := 1 to Width * Height do
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
  Result := TGrayscaleImage.Create(Width, Height);
  try
    pRGB := RGBPixels;
    pGrayscale := Result.GrayscalePixels;
    for i := 1 to Width * Height do
    begin
      pGrayscale^ := GrayscaleValue(pRGB^);
      Inc(pRGB);
      Inc(pGrayscale);
    end;
  except Result.Free; raise end;
end;

procedure TRGBImage.HorizontalLine(const x1, x2, y: Integer;
  const Color: TVector3Byte);
var P: PVector3Byte;
    i: Integer;
begin
 P := PixelPtr(x1, y);
 for i := 0 to x2 - x1 do begin P^ := Color; Inc(P) end;
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

procedure TRGBImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PVector3Byte;
  SecondPtr: PVector3Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := RGBPixels;
  SecondPtr := TRGBImage(SecondImage).RGBPixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
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
var i: Cardinal;
    palpha: PVector4byte;
begin
 palpha := AlphaPixels;
 for i := 1 to Width * Height do
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
  for i := 1 to Width * Height do
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
var pa: PVector4Byte;
    i: Cardinal;
begin
 pa := AlphaPixels;
 for i := 1 to Width * Height do
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
         (RGB.Height = AGrayscale.Height),
    'For TRGBAlphaImage.Compose, RGB and alpha images must have the same sizes');

  SetSize(RGB.Width, RGB.Height);

  PtrAlpha := AlphaPixels;
  PtrRGB := RGB.RGBPixels;
  PtrGrayscale := AGrayscale.GrayscalePixels;

  for I := 1 to Width * Height do
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

function TRGBAlphaImage.AlphaChannelType(
  const AlphaTolerance: Byte;
  const WrongPixelsTolerance: Single): TAlphaChannelType;
var
  PtrAlpha: PVector4Byte;
  I, WrongPixels, AllPixels: Cardinal;
begin
  WrongPixels := 0;
  AllPixels := Width * Height;

  PtrAlpha := AlphaPixels;

  if WrongPixelsTolerance = 0 then
  begin
    for I := 1 to AllPixels do
    begin
      if (PtrAlpha^[3] > AlphaTolerance) and
         (PtrAlpha^[3] < 255 - AlphaTolerance) then
        { Special case for WrongPixelsTolerance = exactly 0.
          Avoids the cases when float "WrongPixels / AllPixels"
          may be so small that it's equal to 0, which would
          cause some wrong pixels to "slip" even with
          WrongPixelsTolerance = 0. }
        Exit(atFullRange);
      Inc(PtrAlpha);
    end;
  end else
  begin
    for I := 1 to AllPixels do
    begin
      if (PtrAlpha^[3] > AlphaTolerance) and
         (PtrAlpha^[3] < 255 - AlphaTolerance) then
      begin
        Inc(WrongPixels);
        { From the speed point of view, is it sensible to test
          WrongPixelsTolerance at each WrongPixels increment?
          On one hand, we can Exit with false faster.
          On the other hand, we lose time for checking it many times,
          if WrongPixelsTolerance is larger.
          Well, sensible WrongPixelsTolerance are very small --- so I
          think this is Ok to check this every time. }
        if WrongPixels / AllPixels > WrongPixelsTolerance then
          Exit(atFullRange);
      end;
      Inc(PtrAlpha);
    end;
  end;

  Result := atSimpleYesNo;
end;

procedure TRGBAlphaImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PVector4Byte;
  SecondPtr: PVector4Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := AlphaPixels;
  SecondPtr := TRGBAlphaImage(SecondImage).AlphaPixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

function TRGBAlphaImage.ToRGBImage: TRGBImage;
var
  SelfPtr: PVector4Byte;
  ResultPtr: PVector3Byte;
  I: Cardinal;
begin
  Result := TRGBImage.Create(Width, Height);
  SelfPtr := AlphaPixels;
  ResultPtr := Result.RGBPixels;
  for I := 1 to Width * Height do
  begin
    Move(SelfPtr^, ResultPtr^, SizeOf(TVector3Byte));
    Inc(SelfPtr);
    Inc(ResultPtr);
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
  for I := 1 to Width * Height do
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
  for I := 1 to Width * Height do
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
var
  PFloat: PVector3Single;
  PByte: PVector3Byte;
  i: Cardinal;
begin
  Result := TRGBImage.Create(Width, Height);
  try
    PByte := Result.RGBPixels;
    PFloat := RGBFloatPixels;
    for i := 1 to Width * Height do
    begin
      PByte^ := Vector3Byte(PFloat^);
      Inc(PByte);
      Inc(PFloat);
    end;
  except Result.Free; raise end;
end;

{$ifdef FPC}
procedure TRGBFloatImage.ScaleColors(const Scale: Single);
var
  pFloat: PVector3Single;
  i: Cardinal;
begin
  PFloat := RGBFloatPixels;
  for i := 1 to Width * Height do
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
  for i := 1 to Width * Height do
  begin
    PFloat^ := VectorPowerComponents(PFloat^, Exp);
    Inc(PFloat);
  end;
end;
{$endif}

procedure TRGBFloatImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PVector3Single;
  SecondPtr: PVector3Single;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := RGBFloatPixels;
  SecondPtr := TRGBFloatImage(SecondImage).RGBFloatPixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
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
  FillChar(RawPixels^, Width * Height, Pixel);
end;

function TGrayscaleImage.IsClear(const Pixel: Byte): boolean;
begin
  Result := IsMemCharFilled(RawPixels^, Width * Height, Char(Pixel));
end;

procedure TGrayscaleImage.HalfColors;
var
  P: PByte;
  I: Cardinal;
begin
  P := GrayscalePixels;
  for I := 1 to Width * Height do
  begin
    P^ := P^ shr 1;
    Inc(P);
  end;
end;

procedure TGrayscaleImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PByte;
  SecondPtr: PByte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := GrayscalePixels;
  SecondPtr := TGrayscaleImage(SecondImage).GrayscalePixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Clamped(Round(Lerp(Value, SelfPtr^, SecondPtr^)), 0, High(Byte));
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

function TGrayscaleImage.ToGrayscaleAlphaImage_AlphaConst(Alpha: byte): TGrayscaleAlphaImage;
var
  pg: PByte;
  pa: PVector2Byte;
  I: Cardinal;
begin
  Result := TGrayscaleAlphaImage.Create(Width, Height);
  pg := GrayscalePixels;
  pa := Result.GrayscaleAlphaPixels;
  for i := 1 to Width * Height do
  begin
    pa^[0] := pg^;
    pa^[1] := Alpha;
    Inc(pg);
    Inc(pa);
  end;
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
  for I := 1 to Width * Height do
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
  for I := 1 to Width * Height do
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

function TGrayscaleAlphaImage.AlphaChannelType(
  const AlphaTolerance: Byte;
  const WrongPixelsTolerance: Single): TAlphaChannelType;
var
  PtrAlpha: PVector2Byte;
  I, WrongPixels, AllPixels: Cardinal;
begin
  WrongPixels := 0;
  AllPixels := Width * Height;

  PtrAlpha := GrayscaleAlphaPixels;

  if WrongPixelsTolerance = 0 then
  begin
    for I := 1 to AllPixels do
    begin
      if (PtrAlpha^[1] > AlphaTolerance) and
         (PtrAlpha^[1] < 255 - AlphaTolerance) then
        { Special case for WrongPixelsTolerance = exactly 0.
          Avoids the cases when float "WrongPixels / AllPixels"
          may be so small that it's equal to 0, which would
          cause some wrong pixels to "slip" even with
          WrongPixelsTolerance = 0. }
        Exit(atFullRange);
      Inc(PtrAlpha);
    end;
  end else
  begin
    for I := 1 to AllPixels do
    begin
      if (PtrAlpha^[1] > AlphaTolerance) and
         (PtrAlpha^[1] < 255 - AlphaTolerance) then
      begin
        Inc(WrongPixels);
        { From the speed point of view, is it sensible to test
          WrongPixelsTolerance at each WrongPixels increment?
          On one hand, we can Exit with false faster.
          On the other hand, we lose time for checking it many times,
          if WrongPixelsTolerance is larger.
          Well, sensible WrongPixelsTolerance are very small --- so I
          think this is Ok to check this every time. }
        if WrongPixels / AllPixels > WrongPixelsTolerance then
          Exit(atFullRange);
      end;
      Inc(PtrAlpha);
    end;
  end;

  Result := atSimpleYesNo;
end;

procedure TGrayscaleAlphaImage.LerpWith(const Value: Single; SecondImage: TImage);
var
  SelfPtr: PVector2Byte;
  SecondPtr: PVector2Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := GrayscaleAlphaPixels;
  SecondPtr := TGrayscaleAlphaImage(SecondImage).GrayscaleAlphaPixels;
  for I := 1 to Width * Height do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
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
{ uwagi : moznaby sadzic ze Mnoznik powinien byc liczony jako
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
var MaxVal, Mnoznik: Single;
    Mantissa: Extended;
    Exponent: Integer;
begin
 MaxVal := KambiUtils.max(v[0], KambiUtils.max(v[1], v[2]));

 { rozpatrujemy tu nie tylko przypadek gdy liczba jest = 0 ale takze
   gdy jest bliska zeru. To jest standardowe zachowanie, ale uwaga -
   - w tym przypadku mogloby sie (blednie) wydawac ze mozemy tutaj zrobic
   wyjatek i sprawdzac ponizej tylko MaxVal = 0.0 (dokladna rownosc)
   a sprawdzanie bliskosci do zera zrzucic na test Exponent < RGBEMinExponent
   ponizej. ALE to nie jest prawda - test Exponent < RGBEMinExponent przejdzie
   dopiero dla niesamowicie mikroskopijnych liczb (< 1 / 2^127) podczas gdy liczby
   pomiedzy tymi "mikroskopijnie malymi" a SINGLE_EQUALITY_EPSILON ciagle
   beda powodowac problemy (bo przy liczeniu Mnoznik dzielimy przez MaxVal
   wiec male MaxVal -> Float overflow). }
 if MaxVal < SingleEqualityEpsilon then begin result := RGBEZero; Exit end;

 Frexp(MaxVal, Mantissa, Exponent);

 if Exponent < RGBEMinExponent then begin result := RGBELow; Exit end;
 if Exponent > RGBEMaxExponent then begin result := RGBEHigh; Exit end;

 Mnoznik := Mantissa * 256 / MaxVal;

 { MaxVal * Mnoznik daje Mantissa * High(byte) a wiec cos w zakresie
   0 .. High(Byte) bo Mantissa <= 1 (de facto, Mantissa >= 0.5 wiec
   mozna podac dokladniejsze ograniczenie na Mantissa * High(byte)).
   Wszystkie pozostale v[] sa mniejsze od MaxVal wiec one tez dadza cos
   w zakresie bajta. }
 result[0] := Clamped(Round(v[0]*Mnoznik), 0, High(Byte));
 result[1] := Clamped(Round(v[1]*Mnoznik), 0, High(Byte));
 result[2] := Clamped(Round(v[2]*Mnoznik), 0, High(Byte));

 { sprawdzajac czy Exponent in RGBEMin/MaxExponent wczesniej juz zapewnilem
   sobie ze ponizsze przypisanie jest Ok, wynik zmiesci sie w zakresie bajta. }
 result[3] := Exponent + RGBEExponentOffset;
end;

function VectorRGBETo3Single(const v: TVector4Byte): TVector3Single;
{ implementacja : jak Graphic Gems II.5.

  Mnoznik wychodzi od 1/256 (a nie 1/255), nalezaloby tu wiec poczynic
  podobne uwagi co przy konwersji w druga strone, Vector3ToRGBE.
  Patrz tamtejszy komentarz. }
var Mnoznik: Single;
begin
 if v[3] = 0 then begin result := ZeroVector3Single; Exit end;

 Mnoznik := Ldexp(1/256, Integer(v[3])-RGBEExponentOffset);
 result[0] := v[0]*Mnoznik;
 result[1] := v[1]*Mnoznik;
 result[2] := v[2]*Mnoznik;
end;

{ file formats managing ---------------------------------------------------------------- }

function FileExtToImageFormat(FileExt: string;
  OnlyLoadable, OnlySaveable: boolean; out ImgFormat: TImageFormat): boolean;
var iff: TImageFormat;
    i: integer;
begin
 if SCharIs(FileExt, 1, '.') then Delete(FileExt, 1, 1);
 FileExt := AnsiLowerCase(FileExt);
 for iff := Low(iff) to High(iff) do
 begin
  if ((not OnlyLoadable) or Assigned(ImageFormatInfos[iff].Load)) and
     ((not OnlySaveable) or Assigned(ImageFormatInfos[iff].Save)) then
  for i := 1 to ImageFormatInfos[iff].extsCount do
   if FileExt = ImageFormatInfos[iff].exts[i] then
   begin
    ImgFormat := iff;
    result := true;
    exit;
   end;
 end;
 result := false;
end;

function FileExtToImageFormatDef(const FileExt: string;
  OnlyLoadable, OnlySaveable: boolean; DefFormat: TImageFormat): TImageFormat;
begin
 if not FileExtToImageFormat(FileExt, OnlyLoadable, OnlySaveable, result) then
  result := DefFormat;
end;

function IsFileExtToImageFormat(const FileExt: string; OnlyLoadable, OnlySaveable: boolean): boolean;
var dummy: TImageFormat;
begin
 result := FileExtToImageFormat(FileExt, OnlyLoadable, OnlySaveable, dummy);
end;

function IsFileExtLoadableImage(const FileExt: string): boolean;
begin
 result := IsFileExtToImageFormat(FileExt, true, false);
end;

function TryFindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;
var iff: TImageFormat;
    i: integer;
begin
 for iff := Low(iff) to High(iff) do
  if (not OnlyLoadable) or Assigned(ImageFormatInfos[iff].Load) then
  begin
   for i := 1 to ImageFormatInfos[iff].extsCount do
   begin
    result := fname +'.' +ImageFormatInfos[iff].exts[i];
    if NormalFileExists(result) then exit;
   end;
  end;
 result := '';
end;

function FindExistingImageExt(const fname: string; OnlyLoadable: boolean): string;
begin
 result := TryFindExistingImageExt(fname, OnlyLoadable);
 if result = '' then
  raise ENoExistingImageExt.Create('No existing image extension found for image name '+fname);
end;

function ListImageExtsLong(OnlyLoadable, OnlySaveable: boolean; const LinePrefix: string): string;
var iff: TImageFormat;
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
var iff: TImageFormat;
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

function LoadImage(Stream: TStream; const StreamFormat: TImageFormat;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage;

  { ClassAllowed is only a shortcut to global utility. }
  function ClassAllowed(ImageClass: TImageClass): boolean;
  begin
    Result := Images.ClassAllowed(ImageClass, AllowedImageClasses);
  end;

  { On input, Image must be TRGBImage and on output it will be TGrayscaleImage. }
  procedure ImageGrayscaleTo1st(var Image: TImage);
  var
    NewImage: TGrayscaleImage;
  begin
    NewImage := (Image as TRGBImage).ToGrayscale;
    FreeAndNil(Image);
    Image := NewImage;
  end;

  procedure ImageRGBToFloatTo1st(var Image: TImage);
  var
    NewResult: TImage;
  begin
    NewResult := (Image as TRGBImage).ToRGBFloat;
    Image.Free;
    Image := NewResult;
  end;

const
  DummyDefaultAlpha = High(Byte);
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
              Result := Load(Stream, AllowedImageClasses, ForbiddenConvs) else
            if ClassAllowed(TRGBFloatImage) and
               (not (ilcFloatPrecAdd in ForbiddenConvs)) then
            begin
              Result := Load(Stream, [TRGBImage], ForbiddenConvs);
              ImageRGBToFloatTo1st(result);
            end else
              raise EUnableToLoadImage.CreateFmt('LoadImage cannot load this image file format to %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
          end;
        lcRGB_RGBA:
          begin
            if ClassAllowed(TRGBImage) or
               ClassAllowed(TRGBAlphaImage) then
              Result := Load(Stream, AllowedImageClasses, ForbiddenConvs) else
{TODO:            if ClassAllowed(TGrayscaleImage) or
               ClassAllowed(TGrayscaleAlphaImage) }
            if ClassAllowed(TRGBFloatImage) and
               (not (ilcFloatPrecAdd in ForbiddenConvs)) then
            begin
              Result := Load(Stream, [TRGBImage], ForbiddenConvs);
              ImageRGBToFloatTo1st(result);
            end else
              raise EUnableToLoadImage.CreateFmt('LoadImage cannot load this image file format to %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
          end;
        lcRGB:
          begin
            Result := Load(Stream, [TRGBImage], ForbiddenConvs);
            Assert(Result is TRGBImage);

            if not (ClassAllowed(TRGBImage)) then
            begin
              if ClassAllowed(TRGBAlphaImage) and
                 (not (ilcAlphaAdd in ForbiddenConvs)) then
              begin
                ImageAlphaConstTo1st(Result, DummyDefaultAlpha);
              end else
              if ClassAllowed(TGrayscaleImage) and
                 (not (ilcRGBFlattenToGrayscale in ForbiddenConvs)) then
              begin
                ImageGrayscaleTo1st(Result);
              end else
              { TODO:
              if ClassAllowed(TGrayscaleAlphaImage) and
                 (not (ilcAlphaAdd in ForbiddenConvs)) and
                 (not (ilcRGBFlattenToGrayscale in ForbiddenConvs)) then
              begin
                ImageAlphaConstTo1st(Result, DummyDefaultAlpha);
                ImageGrayscaleAlphaTo1st(Result);
              end else }
              if ClassAllowed(TRGBFloatImage) and
                 (not (ilcFloatPrecAdd in ForbiddenConvs)) then
              begin
                ImageRGBToFloatTo1st(result);
              end else
                raise EUnableToLoadImage.CreateFmt('LoadImage cannot load this image file format to %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
            end;
          end;
        lcRGB_RGBFloat:
          begin
            if ClassAllowed(TRGBFloatImage) or
               ClassAllowed(TRGBImage) then
              Result := LoadRGBE(Stream, AllowedImageClasses, ForbiddenConvs) else
            begin
              Result := LoadRGBE(Stream, [TRGBImage], ForbiddenConvs);
              if ClassAllowed(TRGBAlphaImage) and
                 (not (ilcAlphaAdd in ForbiddenConvs)) then
              begin
                ImageAlphaConstTo1st(result, DummyDefaultAlpha);
              end else
              if ClassAllowed(TGrayscaleImage) and
                 (not (ilcRGBFlattenToGrayscale in ForbiddenConvs)) then
              begin
                ImageGrayscaleTo1st(Result);
              end else
              if ClassAllowed(TGrayscaleAlphaImage) and
                 (not (ilcAlphaAdd in ForbiddenConvs)) and
                 (not (ilcRGBFlattenToGrayscale in ForbiddenConvs)) then
              begin
                ImageGrayscaleTo1st(Result);
                ImageAlphaConstTo1st(result, DummyDefaultAlpha);
              end else
                raise EUnableToLoadImage.CreateFmt('LoadImage: RGBE format cannot be loaded to %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
            end;
          end;
        else raise EInternalError.Create('LoadImage: LoadedClasses?');
      end;
    end else
    raise EImageFormatNotSupported.Create('Can''t load image format "'+
      ImageFormatInfos[StreamFormat].FormatName+'"');

  except Result.Free; raise end;
end;

function LoadImage(Stream: TStream; const typeext: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions)
  :TImage;
var iff: TImageFormat;
begin
 if FileExtToImageFormat(typeext, true, false, iff) then
  result := LoadImage(Stream, iff, AllowedImageClasses, ForbiddenConvs) else
  raise EImageFormatNotSupported.Create('Unrecognized image format : "'+typeext+'"');
end;

function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
var
  f: TStream;
begin
  {$ifdef DELPHI} Result := nil; { <- only to avoid stupid warning } {$endif}

  try
    { Even CreateReadFileStream may already raise an exception if FileName
      points to a directory. }
    try
      f := CreateReadFileStream(filename);
    except
      on E: EReadError do
        raise EImageLoadError.Create('Cannot read file: ' + E.Message);
    end;

    try
      result := LoadImage(f, ExtractFileExt(filename), AllowedImageClasses,
        ForbiddenConvs);
    finally f.Free end;
  except
    on E: EImageLoadError do begin
      E.Message := 'Error when loading image from file "'+filename+'" : '+E.Message;
      raise;
    end;
    on E: EImageFormatNotSupported do begin
      { przechwyc EImageFormatNotSupported i w tresci wyjatku wklej pelne filename }
      E.Message := 'Unrecognized image format : file "'+filename+'"';
      raise;
    end;
    else raise;
  end;
end;

function LoadImage(const filename: string;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions;
  const ResizeToX, ResizeToY: Cardinal): TImage;
begin
 result := LoadImage(filename, AllowedImageClasses, ForbiddenConvs);
 Result.Resize(ResizeToX, ResizeToY);
end;

{ SaveImage na TImage ---------------------------------------------------- }

procedure SaveImage(const Img: TImage; const Format: TImageFormat; Stream: TStream);
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

procedure SaveImage(const img: TImage; const typeext: string; Stream: TStream);
begin
 SaveImage(Img, FileExtToImageFormatDef(
   typeext, false, true, DefaultSaveImageFormat), Stream);
end;

procedure SaveImage(const Img: TImage; const fname: string);
var f: TFileStream;
begin
 f := TFileStream.Create(fname, fmCreate);
 try
  SaveImage(img, ExtractFileExt(fname), f);
 finally f.Free end;
end;

{ other image processing ------------------------------------------- }

procedure ImageAlphaConstTo1st(var Img: TImage; const AlphaConst: byte);
var
  NewImg: TImage;
begin
  if Img is TRGBImage then
  begin
    NewImg := TRGBImage(Img).ToRGBAlphaImage_AlphaConst(AlphaConst);
    FreeAndNil(Img);
    Img := NewImg;
  end else
  if Img is TGrayscaleImage then
  begin
    NewImg := TGrayscaleImage(Img).ToGrayscaleAlphaImage_AlphaConst(AlphaConst);
    FreeAndNil(Img);
    Img := NewImg;
  end;

  if not ((Img is TRGBAlphaImage) or
          (Img is TGrayscaleAlphaImage)) then
    raise EInternalError.Create(
      'ImageAlphaConstTo1st not possible for this TImage descendant: ' + Img.ClassName);
end;

function ImageClassBestForSavingToFormat(const FileName: string): TImageClass;
begin
  Result := ImageClassBestForSavingToFormat(
    FileExtToImageFormatDef(ExtractFileExt(Filename), false, true,
      DefaultSaveImageFormat));
end;

function ImageClassBestForSavingToFormat(ImgFormat: TImageFormat): TImageClass;
begin
  if ImgFormat = ifRGBE then
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

initialization
  InitializeImagesFileFilters;
  InitializePNG;
finalization
  FreeAndNil(LoadImage_FileFilters);
  FreeAndNil(SaveImage_FileFilters);
end.

(* ----------------------------------------------------------------------------------
stare comments do LoadImage :

 { w result.data zwracaja bitmape w formacie GL_RGB na GL_UNSIGNED_BYTE, bez alignowania !
   Tzn. powinno byc PixelStorei(GL_UNPACK_ALIGNMENT, 1) aby dzialaly w kazdej sytuacji.
   LoadImageData zwraca tylko wskaznik result.data.
   Jesli resizeTo[] <> 0 to dany wymiar bedzie resizowany.
   UWAGA ! Przydzielony pointer data ZAWSZE nalezy zwolnic z pamieci przez FreeMem
     (polecam moje FreeMemNiling).
   ImageProc, jesli <> nil, jest wywolywane dla zaladowanego image'a PRZED wykonaniem
     ewentualnego skalowania. Ma to zastosowanie np. gdy chcesz zaladowac stosunkowo
     maly obrazek z pliku, zamienic go np. na czarno-bialy i potem przeskalowac na
     bardzo duzy rozmiar. W takiej sytuacji duzo bardziej ekonomiczne jest wywolanie
     konwersji na black&white jeszcze PRZED skalowaniem, a wiec najlepiej przekaz
     MakeBlackAndWhite jako ImageProc. Acha, jesli chcesz to mozesz w ImageProc
     zmienic rozmiary obrazka. (chociaz dla typowego resizu pewnie wygodniej bedzie
     uzyc parametrow resizeToX, resizeToY)
 }

*)
