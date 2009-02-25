{
  Copyright 2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ DDS image file format handling. }
unit DDS;

interface

uses Classes, Images;

type
  EInvalidDDS = class(EInvalidImageFormat);

  { Type of data in DDS image file. This doesn't take into account mipmaps
    (they are orthogonal to types here). }
  TDDSType = (
    dtTexture,
    dtCubeMap,
    dtVolume);

  { Cube map sides.

    Ordered exactly like OpenGL constants
    GL_TEXTURE_CUBE_MAP_POSITIVE/NEGATIVE_X/Y/Z (see
    http://opengl.org/registry/specs/ARB/texture_cube_map.txt),
    for comfort. }
  TCubeMapSide = (
    csPositiveX,
    csNegativeX,
    csPositiveY,
    csNegativeY,
    csPositiveZ,
    csNegativeZ);
  TCubeMapSides = set of TCubeMapSide;

  { DDS image file. Basically, DDS is just a sequence of images
    (in the @link(Images) property). The interpretation of the image sequence
    depends on other fields: first of all @link(DDSType) and @link(Mipmaps). }
  TDDSImage = class
  private
    FImages: array of TImage;
    function GetImages(const Index: Integer): TImage;

    FWidth: Cardinal;
    FHeight: Cardinal;
    FDDSType: TDDSType;
    FMipmaps: boolean;
    FMipmapsCount: Cardinal;

    { Valid only when image is loaded and is dtCubeMap. }
    FCubeMapSides: TCubeMapSides;

    { Valid only when image is loaded and is dtVolume. }
    FDepth: Cardinal;

    FOwnsFirstImage: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Images sequence stored in this DDS file.

      This has always length > 0 when DDS is successfully loaded
      (that is, when LoadFromStream method finished without raising any
      exception). }
    property Images [Index: Integer]: TImage read GetImages;
    function ImagesCount: Cardinal;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;

    property DDSType: TDDSType read FDDSType;

    property Mipmaps: boolean read FMipmaps;
    { Mipmaps count.
      Always 1 when @link(Mipmaps) = @false, this is usually comfortable. }
    property MipmapsCount: Cardinal read FMipmapsCount;

    { Return given side of cube map.

      Returns @nil if DDSType is not dtCubeMap, or when the given side
      is omitted (even for dtCubeMap, not all cube map sides have to be
      specified in DDS file).

      This returns always the main mipmap level of the image,
      thus ignoring whether the DDS image has mipmaps or not. }
    function CubeMapImage(const Side: TCubeMapSide): TImage;

    { Load DDS image from any TStream.
      @raises(EInvalidDDS In case of any error in the file data.) }
    procedure LoadFromStream(Stream: TStream);

    procedure LoadFromFile(const FileName: string);

    { Close all loaded image data. Effectively, this releases all data
      loaded by LoadFromStream, reverting the object to the state right
      after creation. }
    procedure Close;

    { When @false, then closing this DDS image will not free Images[0].
      Closing happens when you call the @link(Close) method or
      destructor of this object. When this is @false, you're responsible
      to storing and freeing Images[0] later yourself, or you'll get memory
      leaks. }
    property OwnsFirstImage: boolean read FOwnsFirstImage write FOwnsFirstImage
      default true;
  end;

implementation

uses SysUtils, KambiUtils, KambiClassUtils, DataErrors, KambiStringUtils,
  VectorMath;

{ ----------------------------------------------------------------------------
  Constants and types for DDS file handling.

  This is based on MS docs on
  http://msdn.microsoft.com/en-us/library/bb943991(VS.85).aspx
  and gimp-dds/gimp-dds-2.0.7/dds.h,
  gimp-dds plugin source code (GNU GPL >= 2, so were compatible)
  from http://nifelheim.dyndns.org/~cocidius/dds/ }

const
  DDSD_CAPS                   = $00000001;
  DDSD_HEIGHT                 = $00000002;
  DDSD_WIDTH                  = $00000004;
  DDSD_PITCH                  = $00000008;
  DDSD_PIXELFORMAT            = $00001000;
  DDSD_MIPMAPCOUNT            = $00020000;
  DDSD_LINEARSIZE             = $00080000;
  DDSD_DEPTH                  = $00800000;

  DDPF_ALPHAPIXELS            = $00000001;
  DDPF_ALPHA                  = $00000002;
  DDPF_FOURCC                 = $00000004;
  DDPF_PALETTEINDEXED8        = $00000020;
  DDPF_RGB                    = $00000040;
  DDPF_LUMINANCE              = $00020000;

  DDSCAPS_COMPLEX             = $00000008;
  DDSCAPS_TEXTURE             = $00001000;
  DDSCAPS_MIPMAP              = $00400000;

  DDSCAPS2_CUBEMAP            = $00000200;
  DDSCAPS2_CUBEMAP_POSITIVEX  = $00000400;
  DDSCAPS2_CUBEMAP_NEGATIVEX  = $00000800;
  DDSCAPS2_CUBEMAP_POSITIVEY  = $00001000;
  DDSCAPS2_CUBEMAP_NEGATIVEY  = $00002000;
  DDSCAPS2_CUBEMAP_POSITIVEZ  = $00004000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ  = $00008000;
  DDSCAPS2_VOLUME             = $00200000;

type
  TDDSPixelFormat = packed record
    Size: LongWord;
    Flags: LongWord;
    FourCC: array [0 .. 3] of char;
    RGBBitCount: LongWord;
    RBitMask: LongWord;
    GBitMask: LongWord;
    BBitMask: LongWord;
    ABitMask: LongWord;
  end;
  PDDSPixelFormat = ^TDDSPixelFormat;

  { Corresponds to DDS_HEADER (Direct3D 10),
    http://msdn.microsoft.com/en-us/library/bb943982(VS.85).aspx. }
  TDDSHeader = packed record
    Size: LongWord;
    Flags: LongWord;
    Height: LongWord;
    Width: LongWord;
    PitchOrLinearSize: LongWord;
    Depth: LongWord;
    MipMapCount: LongWord;
    Reserved: array [0 .. 10] of LongWord;
    PixelFormat: TDDSPixelFormat;
    Caps1: LongWord;
    Caps2: LongWord;
    ReservedCaps: array [0 .. 1] of LongWord;
    Reserved2: LongWord;
  end;

{ TDDSImage ------------------------------------------------------------------ }

constructor TDDSImage.Create;
begin
  inherited;
  FOwnsFirstImage := true;
end;

destructor TDDSImage.Destroy;
begin
  Close;
  inherited;
end;

procedure TDDSImage.Close;
var
  I: Integer;
begin
  for I := 0 to Length(FImages) - 1 do
    if OwnsFirstImage or (I <> 0) then
      FreeAndNil(FImages[I]);
  SetLength(FImages, 0);
end;

function TDDSImage.GetImages(const Index: Integer): TImage;
begin
  Result := FImages[Index];
end;

function TDDSImage.ImagesCount: Cardinal;
begin
  Result := Length(FImages);
end;

function TDDSImage.CubeMapImage(const Side: TCubeMapSide): TImage;
begin
  { TODO }
end;

type
  TRGBAChannel = 0..3;

  { Reads DDS image rows, loading them to temporary memory.

    After reading row (by ReadRow) you can uncompress it by
    using RGBA (return value as 8-bit Byte, for Red, Green, Blue, Alpha
    according to Channel parameter)
    and NextPixel repeatedly.

    It's callers responsibility to make sure that you use
    RGBA only with channels for which masks are non-zero.

    This is not the most efficient reader (it reads rows to temporary memory,
    for starters), but it works for all covered uncompressed non-palette
    DDS pixel formats. For special pixel formats, we use specialized
    optimized readers in ReadOptimized_*. }
  TDDSRowReader = class
  private
    FPixelFormat: PDDSPixelFormat;
    FWidth, FRowBytePadding: Cardinal;
    Row: Pointer;
    Pixel: Pointer;
    PixelValue: LongWord;
    RowByteSize, PixelByteSize: Cardinal;
    ChannelMask: array [TRGBAChannel] of LongWord;
    ChannelShift: array [TRGBAChannel] of Integer;
    procedure CalculatePixelValue;
  public
    constructor Create(const PixelFormat: PDDSPixelFormat;
      const Width, RowBytePadding: Cardinal);
    destructor Destroy; override;

    procedure ReadRow(Stream: TStream);
    procedure NextPixel;
    function RGBA(Channel: TRGBAChannel): Byte;
  end;

constructor TDDSRowReader.Create(const PixelFormat: PDDSPixelFormat;
  const Width, RowBytePadding: Cardinal);

  { Calculate shift (to the right, i.e. "shr") to extract color value to 8bit. }
  function CalculateChannelShift(Mask: LongWord): Integer;
  const
    High1 = LongWord(1) shl 31;
  var
    LeadingZeros: Integer;
  begin
    if Mask = 0 then Exit(0);

    LeadingZeros := 0;
    while Mask and High1 = 0 do
    begin
      Inc(LeadingZeros);
      Mask := Mask shl 1;
    end;

    { So the mask in binary starts with LeadingZeros of 0,
      then some 1.

      We could detect how many 1 digits are present.
      We could even assume that there's a sequence of 1's and then the
      following are all zero (nothing produces DDS
      files with other values, that would be pretty strange).
      But that's not actually necessary, all we need is to know
      the position of the most significant 1 digit, and then the trivial
      equation below will work Ok. }

    Result := 24 - LeadingZeros;
  end;

var
  I: Integer;
begin
  inherited Create;
  FPixelFormat := PixelFormat;
  FWidth := Width;
  FRowBytePadding := RowBytePadding;

  { We already checked in TDDSImage.LoadFromStream that RGBitCount divides
    by 8 and is not zero. }
  PixelByteSize := FPixelFormat^.RGBBitCount div 8;

  if PixelByteSize > SizeOf(PixelValue) then
    raise EInvalidDDS.CreateFmt('Unsupported DDS pixel format: more than 32 bits per pixel (RGBitCount is %d). Please report with sample image',
      [FPixelFormat^.RGBBitCount]);

  RowByteSize := PixelByteSize * Width;
  Row := GetMem(RowByteSize);

  ChannelMask[0] := FPixelFormat^.RBitMask;
  ChannelMask[1] := FPixelFormat^.GBitMask;
  ChannelMask[2] := FPixelFormat^.BBitMask;
  ChannelMask[3] := FPixelFormat^.ABitMask;

  for I := 0 to 3 do
    ChannelShift[I] := CalculateChannelShift(ChannelMask[I]);
end;

destructor TDDSRowReader.Destroy;
begin
  FreeMem(Row);
  inherited;
end;

procedure TDDSRowReader.CalculatePixelValue;
begin
  PixelValue := 0;

  { The tricky memory part: move the meaningful bytes of current pixel
    (under Pixel^) to appropriate part of PixelValue, such that
    PixelValue can be directly and'ed with DDS masks (in ChannelMask[]).
    The copied bytes are the least significant part of LongWord value
    (DDS masks are specified like that). }

  Move(Pixel^, Pointer(PtrUInt(PtrUInt(@PixelValue)
    {$ifdef ENDIAN_BIG} + SizeOf(PixelValue) - PixelByteSize {$endif} ))^,
    PixelByteSize);
end;

procedure TDDSRowReader.ReadRow(Stream: TStream);
begin
  Stream.ReadBuffer(Row^, RowByteSize);

  if FRowBytePadding <> 0 then
    Stream.Seek(FRowBytePadding, soFromCurrent);

  Pixel := Row;
  CalculatePixelValue;
end;

procedure TDDSRowReader.NextPixel;
begin
  PtrUInt(Pixel) += PixelByteSize;
  CalculatePixelValue;
end;

function TDDSRowReader.RGBA(Channel: TRGBAChannel): Byte;
begin
  Result := (PixelValue and ChannelMask[Channel]) shr ChannelShift[Channel];
end;

procedure TDDSImage.LoadFromStream(Stream: TStream);

  procedure CheckWarn(const Check: boolean; const Message: string);
  begin
    if not Check then
      DataWarning('DDS image: ' + Message);
  end;

var
  Header: TDDSHeader;

  { Read a single, normal 2D image from DDS file. }
  function ReadImage: TImage;

    procedure ReadUncompressed(IsRGB: boolean);
    var
      RowBytePadding: Integer;

      { A couple of optimized routines for image formats that
        closely match corresponding Images unit memory format are below.
        Optimized routines already know the Result memory format (image class)
        and file format (Header.PixelFormat), they don't have to check them.

        Names for optimized routines follow the naming of DDS ms docs.
        This means that they are actually inverted in memory,
        since DDS bit masks should be treated as little-endian.
        For example for RGB image, RBitMask = $ff0000 means that red is
        the 3rd (not 1st) byte...)

        That's why DDS format RGB8 must be inverted when writing to TRGBImage.
        The format matching exactly TRGBImage is actually named BGR8 in DDS. }

      procedure ReadOptimized_G8;
      var
        Y: Integer;
      begin
        for Y := Height - 1 downto 0 do
        begin
          Stream.ReadBuffer(Result.RowPtr(Y)^, Result.PixelSize * Width);
          if RowBytePadding <> 0 then
            Stream.Seek(RowBytePadding, soFromCurrent);
        end;
      end;

      procedure ReadOptimized_AG8;
      var
        Y: Integer;
      begin
        for Y := Height - 1 downto 0 do
        begin
          Stream.ReadBuffer(Result.RowPtr(Y)^, Result.PixelSize * Width);
          if RowBytePadding <> 0 then
            Stream.Seek(RowBytePadding, soFromCurrent);
        end;
      end;

      procedure ReadOptimized_RGB8;
      var
        X, Y: Integer;
        Row: PVector3Byte;
      begin
        for Y := Height - 1 downto 0 do
        begin
          Row := Result.RowPtr(Y);
          Stream.ReadBuffer(Row^, Result.PixelSize * Width);

          { Now invert red and blue. (Since all masks are little-endian,
            RBitMask = $FF0000 means that red is the 3rd (not 1st) byte...) }
          for X := 0 to Width - 1 do
          begin
            SwapValues(Row^[2], Row^[0]);
            Inc(Row);
          end;

          if RowBytePadding <> 0 then
            Stream.Seek(RowBytePadding, soFromCurrent);
        end;
      end;

      procedure ReadOptimized_BGR8;
      var
        Y: Integer;
      begin
        for Y := Height - 1 downto 0 do
        begin
          Stream.ReadBuffer(Result.RowPtr(Y)^, Result.PixelSize * Width);
          if RowBytePadding <> 0 then
            Stream.Seek(RowBytePadding, soFromCurrent);
        end;
      end;

      procedure ReadOptimized_ARGB8;
      var
        X, Y: Integer;
        Row: PVector4Byte;
      begin
        for Y := Height - 1 downto 0 do
        begin
          Row := Result.RowPtr(Y);
          Stream.ReadBuffer(Row^, Result.PixelSize * Width);

          { Now invert ARGB to ABGR. So swap red<->blue, alpha and green are Ok. }
          for X := 0 to Width - 1 do
          begin
            SwapValues(Row^[2], Row^[0]);
            Inc(Row);
          end;

          if RowBytePadding <> 0 then
            Stream.Seek(RowBytePadding, soFromCurrent);
        end;
      end;

      procedure ReadOptimized_ABGR8;
      var
        Y: Integer;
      begin
        for Y := Height - 1 downto 0 do
        begin
          Stream.ReadBuffer(Result.RowPtr(Y)^, Result.PixelSize * Width);
          if RowBytePadding <> 0 then
            Stream.Seek(RowBytePadding, soFromCurrent);
        end;
      end;

      procedure ReadToGrayscale;
      var
        Reader: TDDSRowReader;
        Y, X: Integer;
        G: PByte;
      begin
        Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
        try
          for Y := Height - 1 downto 0 do
          begin
            Reader.ReadRow(Stream);
            G := Result.RowPtr(Y);
            for X := 0 to Width - 1 do
            begin
              G^ := Reader.RGBA(0);
              Reader.NextPixel;
              Inc(G);
            end;
          end;
        finally FreeAndNil(Reader) end;
      end;

      procedure ReadToGrayscaleAlpha;
      var
        Reader: TDDSRowReader;
        Y, X: Integer;
        GA: PVector2Byte;
      begin
        Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
        try
          for Y := Height - 1 downto 0 do
          begin
            Reader.ReadRow(Stream);
            GA := Result.RowPtr(Y);
            for X := 0 to Width - 1 do
            begin
              GA^[0] := Reader.RGBA(0);
              GA^[1] := Reader.RGBA(3);
              Reader.NextPixel;
              Inc(GA);
            end;
          end;
        finally FreeAndNil(Reader) end;
      end;

      procedure ReadToRGB;
      var
        Reader: TDDSRowReader;
        Y, X: Integer;
        RGB: PVector3Byte;
      begin
        Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
        try
          for Y := Height - 1 downto 0 do
          begin
            Reader.ReadRow(Stream);
            RGB := Result.RowPtr(Y);
            for X := 0 to Width - 1 do
            begin
              RGB^[0] := Reader.RGBA(0);
              RGB^[1] := Reader.RGBA(1);
              RGB^[2] := Reader.RGBA(2);
              Reader.NextPixel;
              Inc(RGB);
            end;
          end;
        finally FreeAndNil(Reader) end;
      end;

      procedure ReadToRGBAlpha;
      var
        Reader: TDDSRowReader;
        Y, X: Integer;
        RGBA: PVector4Byte;
      begin
        Reader := TDDSRowReader.Create(@Header.PixelFormat, Width, RowBytePadding);
        try
          for Y := Height - 1 downto 0 do
          begin
            Reader.ReadRow(Stream);
            RGBA := Result.RowPtr(Y);
            for X := 0 to Width - 1 do
            begin
              RGBA^[0] := Reader.RGBA(0);
              RGBA^[1] := Reader.RGBA(1);
              RGBA^[2] := Reader.RGBA(2);
              RGBA^[3] := Reader.RGBA(3);
              Reader.NextPixel;
              Inc(RGBA);
            end;
          end;
        finally FreeAndNil(Reader) end;
      end;

    begin
      Check(Header.PixelFormat.RGBBitCount mod 8 = 0, 'Invalid DDS pixel format: only RGBBitCount being multiple of 8 is supported. Please report with sample image');
      Check(Header.PixelFormat.RGBBitCount > 0, 'Invalid DDS pixel format: RGBBitCount must be non-zero');

      { Header.PitchOrLinearSize for uncompressed texture may indicate
        row length (pitch) in bytes. This is useful to indicate padding of lines.
        I understand that otherwise I should assume lines are not padded? }
      if (Header.Flags and DDSD_PITCH <> 0) and
         (Header.PitchOrLinearSize <> 0) then
        RowBytePadding := Max(Header.PitchOrLinearSize -
          (Header.PixelFormat.RGBBitCount div 8) * Width, 0) else
        RowBytePadding := 0;

      if IsRGB then
      begin
        if Header.PixelFormat.Flags and DDPF_ALPHAPIXELS = 0 then
        begin
          Result := TRGBImage.Create(Width, Height);

          if (Header.PixelFormat.RBitMask = $ff0000) and
             (Header.PixelFormat.GBitMask = $00ff00) and
             (Header.PixelFormat.BBitMask = $0000ff) then
            ReadOptimized_RGB8 else
          if (Header.PixelFormat.RBitMask = $0000ff) and
             (Header.PixelFormat.GBitMask = $00ff00) and
             (Header.PixelFormat.BBitMask = $ff0000) then
            ReadOptimized_BGR8 else
            ReadToRGB;
        end else
        begin
          Check(Header.PixelFormat.ABitMask <> 0, 'Invalid DDS pixel format: alpha channel flag specified (DDPF_ALPHAPIXELS), but alpha mask is zero');

          Result := TRGBAlphaImage.Create(Width, Height);

          if (Header.PixelFormat.RGBBitCount = 32) and
             (Header.PixelFormat.ABitMask = $ff000000) and
             (Header.PixelFormat.RBitMask = $00ff0000) and
             (Header.PixelFormat.GBitMask = $0000ff00) and
             (Header.PixelFormat.BBitMask = $000000ff) then
            ReadOptimized_ARGB8 else
          if (Header.PixelFormat.RGBBitCount = 32) and
             (Header.PixelFormat.ABitMask = $ff000000) and
             (Header.PixelFormat.RBitMask = $000000ff) and
             (Header.PixelFormat.GBitMask = $0000ff00) and
             (Header.PixelFormat.BBitMask = $00ff0000) then
            ReadOptimized_ABGR8 else
            ReadToRGBAlpha;
        end;
      end else
      { If not IsRGB, than I already know (it's checked earlier)
        that masks for red / green / blue are equal.
        That may be all zero (alpha only image) or no (grayscale
        with possible alpha). }
      if Header.PixelFormat.RBitMask <> 0 then
      begin
        if Header.PixelFormat.Flags and DDPF_ALPHAPIXELS = 0 then
        begin
          Result := TGrayscaleImage.Create(Width, Height);

          if (Header.PixelFormat.RGBBitCount = 8) and
             (Header.PixelFormat.RBitMask = $ff) then
            ReadOptimized_G8 else
            ReadToGrayscale;
        end else
        begin
          Result := TGrayscaleAlphaImage.Create(Width, Height);
          if (Header.PixelFormat.RGBBitCount = 16) and
             (Header.PixelFormat.ABitMask = $ff00) and
             (Header.PixelFormat.RBitMask = $00ff) then
            ReadOptimized_AG8 else
            ReadToGrayscaleAlpha;
        end;
      end else
      begin
        if Header.PixelFormat.Flags and DDPF_ALPHAPIXELS <> 0 then
          raise EInvalidDDS.Create('TODO: Unsupported pixel format for DDS: pure alpha channel') else
          raise EInvalidDDS.Create('Invalid DDS pixel format: no flag specified (so assuming grayscale inmage), but all r/g/b masks are zero');
      end;
    end;

  begin
    { There are four mutually exclusive types of DDS pixel format:
      uncompressed non-palette (DDPF_RGB) or
      uncompressed palette (DDPF_PALETTEINDEXED8) or
      compressed (DDPF_FOURCC) or
      uncompressed grayscale and/or alpha (neither flag, GIMP-DDS can write such images). }

    Result := nil;
    try
      if Header.PixelFormat.Flags and DDPF_RGB <> 0 then
      begin
        Check(Header.PixelFormat.Flags and DDPF_FOURCC = 0, 'Invalid DDS pixel format: both uncompressed (DDPF_RGB) and compressed (DDPF_FOURCC) flags specified');
        Check(Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 = 0, 'Invalid DDS pixel format: both non-palette (DDPF_RGB) and palette (DDPF_PALETTEINDEXED8) flags specified');
        ReadUncompressed(true);
      end else
      if Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 <> 0 then
      begin
        Assert(Header.PixelFormat.Flags and DDPF_RGB = 0);
        Check(Header.PixelFormat.Flags and DDPF_FOURCC = 0, 'Invalid DDS pixel format: both uncompressed (palette, DDPF_PALETTEINDEXED8) and compressed (DDPF_FOURCC) flags specified');

        raise EInvalidDDS.Create('TODO: Unsupported pixel format for DDS: palette images not supported now, please report with sample image');
      end else
      if Header.PixelFormat.Flags and DDPF_FOURCC <> 0 then
      begin
        Assert(Header.PixelFormat.Flags and DDPF_RGB = 0);
        Assert(Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 = 0);

        raise EInvalidDDS.CreateFmt('TODO: Unsupported pixel format for DDS: compressed (FourCC: %s%s%s%s) texture images not supported now, please report with sample image',
          [ SReadableForm(Header.PixelFormat.FourCC[0]),
            SReadableForm(Header.PixelFormat.FourCC[1]),
            SReadableForm(Header.PixelFormat.FourCC[2]),
            SReadableForm(Header.PixelFormat.FourCC[3]) ]);
      end else
      begin
        { No flags specified, this means uncompressed image
          without RGB: grayscale (possibly with alpha), or even only
          alpha channel. At least GIMP-DDS plugin can write such images. }
        Check(
          (Header.PixelFormat.RBitMask = Header.PixelFormat.GBitMask) and
          (Header.PixelFormat.GBitMask = Header.PixelFormat.BBitMask), 'Invalid DDS pixel format: neighter DDPF_RGB, DDPF_FOURCC, DDPF_PALETTEINDEXED8 flags specified, so this must be a grayscale and/or alpha image. But R/G/B masks are different');
        ReadUncompressed(false);
      end;
    except FreeAndNil(Result); raise end;
  end;

var
  Magic: array [0 .. 3] of char;
begin
  try
    Stream.ReadBuffer(Magic, SizeOf(Magic));
    Check(Magic = 'DDS ', 'DDS file beginning (magic number) invalid, maybe this is not really a DDS file');

    Stream.ReadBuffer(Header, SizeOf(Header));
    Assert(SizeOf(Header) = 124); { this is actually constant, as it's part of file format spec... }
    Check(Header.Size = SizeOf(Header), 'DDS header size incorrect');
    Check(Header.Flags and DDSD_CAPS <> 0, 'Missing DDSD_CAPS');
    Check(Header.Flags and DDSD_HEIGHT <> 0, 'Missing DDSD_HEIGHT');
    Check(Header.Flags and DDSD_WIDTH <> 0, 'Missing DDSD_WIDTH');
    Check(Header.Flags and DDSD_PIXELFORMAT <> 0, 'Missing DDSD_PIXELFORMAT');

    FHeight := Header.Height;
    FWidth := Header.Width;

    { calculate FMipmaps, FMipmapsCount }
    FMipmaps :=  Header.Caps1 and DDSCAPS_MIPMAP <> 0;
    if FMipmaps then
    begin
      Check(Header.Flags and DDSD_MIPMAPCOUNT <> 0, 'Missing DDSD_MIPMAPCOUNT, but caps indicate that this DDS image has mipmaps');
      Check(Header.Caps1 and DDSCAPS_COMPLEX <> 0, 'Missing DDSCAPS_COMPLEX, but caps indicate that this DDS image has mipmaps');
      FMipmapsCount := Header.MipMapCount;
    end else
      FMipmapsCount := 1;

    Check(Header.Flags and DDSCAPS_TEXTURE <> 0, 'Missing DDSCAPS_TEXTURE');

    { calculate DDSType }
    Check( (Header.Caps2 and DDSCAPS2_VOLUME = 0) or
           (Header.Caps2 and DDSCAPS2_CUBEMAP = 0),
      'DDS capabilities indicate CUBEMAP and VOLUME texture at the same time');
    if Header.Caps2 and DDSCAPS2_CUBEMAP <> 0 then
    begin
      FDDSType := dtCubeMap;

      { calculate FCubeMapSides }
      FCubeMapSides := [];
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEX <> 0 then Include(FCubeMapSides, csPositiveX);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEX <> 0 then Include(FCubeMapSides, csNegativeX);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEY <> 0 then Include(FCubeMapSides, csPositiveY);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEY <> 0 then Include(FCubeMapSides, csNegativeY);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEZ <> 0 then Include(FCubeMapSides, csPositiveZ);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEZ <> 0 then Include(FCubeMapSides, csNegativeZ);
      Check(FCubeMapSides <> [], 'DDS file is a cube map, but all six sides of the cube are missing');
    end else
    if Header.Caps2 and DDSCAPS2_VOLUME <> 0 then
    begin
      FDDSType := dtVolume;

      { calculate FDepth }
      Check(Header.Flags and DDSD_DEPTH <> 0, 'Missing DDSD_DEPTH, but DDS is a VOLUME texture');
      FDepth := Header.Depth;
    end else
      FDDSType := dtTexture;
    if FDDSType <> dtTexture then
      { This invalid situation happens on
        http://regedit.gamedev.pl/Download/Rozne/Tekstury%20narzedziowe/NoiseVolume.dds
        (from
        http://regedit.gamedev.pl/download.php5?x=Rozne%2FTekstury+narzedziowe) }
      CheckWarn(Header.Caps1 and DDSCAPS_COMPLEX <> 0, 'Missing DDSCAPS_COMPLEX, but caps indicate that this DDS image has cube map or volume');

    Close;

    Check(Header.PixelFormat.Size = SizeOf(Header.PixelFormat), 'Incorrect size of DDS pixel format record');

    { TODO: read just the 1st image for now }
    SetLength(FImages, 1);
    FImages[0] := nil; { nil, in case ReadImage will raise error }
    FImages[0] := ReadImage;
  except
    { EReadError is raised by Stream.ReadBuffer when it can't read
      specified number of bytes }
    on E: EReadError do raise EInvalidDDS.Create('Read error: ' + E.Message);
    on E: ECheckFailed do raise EInvalidDDS.Create('Wrong DDS file: ' + E.Message);
  end;
end;

procedure TDDSImage.LoadFromFile(const FileName: string);
var
  S: TStream;
begin
  S := CreateReadFileStream(FileName);
  try
    LoadFromStream(S);
  finally FreeAndNil(S) end;
end;

end.
