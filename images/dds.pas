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

  { DDS cube map sides.

    Note that if you work in right-handed coordinate system (like
    OpenGL, see http://opengl.org/registry/specs/ARB/texture_cube_map.txt)
    then you should swap the meaning of positive/negative y faces.
    That's because cube map sides are named and written in DDS file
    in a way natural for DirectX, and DirectX has left-handed coordinate system,
    which means that one axis seems reverted when you want OpenGL right-handed
    coord system.
    See [http://vrmlengine.sourceforge.net/vrml_implementation_status.php#section_dds]
    for more. }
  TDDSCubeMapSide = (
    dcsPositiveX,
    dcsNegativeX,
    dcsPositiveY,
    dcsNegativeY,
    dcsPositiveZ,
    dcsNegativeZ);
  TDDSCubeMapSides = set of TDDSCubeMapSide;

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
    FCubeMapSides: TDDSCubeMapSides;
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

    { Does this DDS file contain mipmaps.
      If @true, then all @link(Images) are guaranteed to have sizes
      being power of 2. }
    property Mipmaps: boolean read FMipmaps;
    { Mipmaps count.
      Always 1 when @link(Mipmaps) = @false, this is usually comfortable. }
    property MipmapsCount: Cardinal read FMipmapsCount;

    { Present cube map sides.
      Valid only when image is loaded and is dtCubeMap. }
    property CubeMapSides: TDDSCubeMapSides read FCubeMapSides;

    { Depth of volume (3D) texture.
      Valid only when image is loaded and is dtVolume. }
    property Depth: Cardinal read FDepth;

    { Return given side of cube map.
      Assumes DDSType = dtCubeMap and CubeMapSides = all.

      This returns always the main mipmap level of the image,
      thus ignoring whether the DDS image has mipmaps or not.

      TODO: this is actually for testing, not really for production
      (as for real usage, you will want to eventually use also mipmaps). }
    function CubeMapImage(const Side: TDDSCubeMapSide): TImage;

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

const
  AllDDSCubeMapSides = [Low(TDDSCubeMapSide) .. High(TDDSCubeMapSide)];

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

{ TDDSRowReader -------------------------------------------------------------- }

type
  TRGBAChannel = 0..3;

  TChannelInfo = record
    Mask: LongWord;
    { Shift (to the right, by ShiftR) to fit into Byte }
    ShiftToByte: Integer;
    { Shift (to the right, by ShiftR) to have the least significant bit at
      1st position. }
    ShiftToLeast: Integer;
    { How long is the sequence of 1 bits inside the mask. }
    MaskOnes: Cardinal;
  end;

  { Reads DDS image rows, loading them to temporary memory.

    After reading row (by ReadRow) you can uncompress it by
    using RGBA (returns value as 8-bit Byte, for Red, Green, Blue, Alpha
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
    Channels: array [TRGBAChannel] of TChannelInfo;
    procedure CalculatePixelValue;
  public
    constructor Create(const PixelFormat: PDDSPixelFormat;
      const Width, RowBytePadding: Cardinal);
    destructor Destroy; override;

    procedure ReadRow(Stream: TStream);
    procedure NextPixel;
    function RGBA(Channel: TRGBAChannel): Byte;
  end;

{ Shift bits to the right if Value is positive.
  Shift to the left if negative. }
function ShiftR(const Value: LongWord; const Shift: Integer): LongWord;
begin
  if Shift >= 0 then
    Result := Value shr Shift else
    Result := Value shl (-Shift);
end;

constructor TDDSRowReader.Create(const PixelFormat: PDDSPixelFormat;
  const Width, RowBytePadding: Cardinal);

  { Calculate TChannelInfo. }
  procedure CalculateChannelInfo(const Mask: LongWord; var Info: TChannelInfo);
  const
    High1 = LongWord(1) shl 31;
  var
    LeadingZeros, Ones: Integer;
    M: LongWord;
  begin
    Info.Mask := Mask;

    if Mask = 0 then
    begin
      Info.ShiftToByte := 0;
      Info.ShiftToLeast := 0;
      Info.MaskOnes := 0;
      Exit;
    end;

    M := Mask;

    LeadingZeros := 0;
    while M and High1 = 0 do
    begin
      Inc(LeadingZeros);
      M := M shl 1;
    end;

    Ones := 0;
    while M <> 0 do
    begin
      Inc(Ones);
      M := M shl 1;
    end;
    Info.MaskOnes := Ones;

    { So the mask in binary starts with LeadingZeros of 0,
      then some 1. After Ones digits, the mask is zero.

      (In other words, we have a sequence of Ones bits inside the Mask
      that start and end with digit 1. We could assume here that
      this sequence is just full of 1 (nothing produces DDS
      files with other values, that would be pretty strange).
      But that's not actually necessary, all we need is to know
      the position of the most and least significant 1 digit,
      and then the trivial equation below will work Ok. }

    Info.ShiftToByte := 24 - LeadingZeros;
    Info.ShiftToLeast := 32 - LeadingZeros - Ones;

    { Assert that after shifting, all bits above Byte are clear
      and the most significant bit of color is in the most significant bit
      of byte. }
    Assert(ShiftR(Mask, Info.ShiftToByte) and $FFFFFF80 = $80);
  end;

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

  CalculateChannelInfo(FPixelFormat^.RBitMask, Channels[0]);
  CalculateChannelInfo(FPixelFormat^.GBitMask, Channels[1]);
  CalculateChannelInfo(FPixelFormat^.BBitMask, Channels[2]);
  CalculateChannelInfo(FPixelFormat^.ABitMask, Channels[3]);
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
  with Channels[Channel] do
  begin
    Result := ShiftR(PixelValue and Mask, ShiftToByte);

    { What to with bits that are not set by PixelValue?
      That is, when the mask is less than 8 bits?

      It's important that pure white be preserved as pure white,
      and pure black as pure black. This is especially important for alpha
      channel (otherwise opaque image will suddenly turn into
      partially transparent, e.g. if you load image with 2 bits for alpha,
      and fill the remaining 6 bits with 0, then alpha = 11 (binary)
      will be turned into 11000000 (no longer completely opaque).)

      So it seems most sensible to fill the remaining bits with the contents
      of least-significant color bit. If this bit is zero, we actually
      have to do nothing, but when it's one --- we need to insert 1 bits
      into Result.

      This is done only when MaskOnes < 8, to optimize. }
    if (MaskOnes < 8) and
       (ShiftR(PixelValue, ShiftToLeast) and 1 <> 0) then
    begin
      { So the least-significant color bit is 1. }
      Result := Result or (ShiftR(not Mask, ShiftToByte) and $FF);
    end;
  end;
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

function TDDSImage.CubeMapImage(const Side: TDDSCubeMapSide): TImage;
begin
  if Mipmaps then
    Result := FImages[Cardinal(Ord(Side)) * FMipmapsCount] else
    Result := FImages[Ord(Side)];
end;

procedure TDDSImage.LoadFromStream(Stream: TStream);

  procedure CheckWarn(const Check: boolean; const Message: string);
  begin
    if not Check then
      DataWarning('DDS image: ' + Message);
  end;

var
  Header: TDDSHeader;

  { Reading header initializes many instance fields:
    FWidth, FHeight,
    FMipmaps, FMipmapsCount,
    FDDSType, FCubeMapSides, FDepth }
  procedure ReadHeader;
  var
    Magic: array [0 .. 3] of char;
  begin
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
      Check(MipmapsCount > 0, 'Specified mipmaps, but mipmap count is zero');
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
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEX <> 0 then Include(FCubeMapSides, dcsPositiveX);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEX <> 0 then Include(FCubeMapSides, dcsNegativeX);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEY <> 0 then Include(FCubeMapSides, dcsPositiveY);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEY <> 0 then Include(FCubeMapSides, dcsNegativeY);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_POSITIVEZ <> 0 then Include(FCubeMapSides, dcsPositiveZ);
      if Header.Caps2 and DDSCAPS2_CUBEMAP_NEGATIVEZ <> 0 then Include(FCubeMapSides, dcsNegativeZ);
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

    Check(Header.PixelFormat.Size = SizeOf(Header.PixelFormat), 'Incorrect size of DDS pixel format record');
  end;

  { Read actual image data, initializing FImages contents }
  procedure ReadImages;

    { Read a single, normal 2D image from DDS file. }
    function ReadImage(const Width, Height: Cardinal): TImage;

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
          The format matching exactly TRGBImage is actually named BGR8 in DDS.

          The optimized routines are much faster as they don't use TDDSRowReader,
          so they don't need a temporary row (they load directly into output
          image memory), they don't need any mask/shift operations for each channel
          of each pixel (as the pixel format already matches what is needed,
          eventual swaps BGR<->RGB are fast).
          Tests shown that optimized versions are 4-7 times faster than
          if TDDSRowReader would be used to read the same images.
        }

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

        { Read alpha-only images (may be produced by GIMP-DDS) }
        procedure ReadToGrayscaleAlphaPure;
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
                GA^[0] := 255;
                GA^[1] := Reader.RGBA(3);
                Reader.NextPixel;
                Inc(GA);
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
           (Header.PitchOrLinearSize <> 0) and
           { Use this only for level 0 of mipmap level }
           (Width = Self.Width) then
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
          { GIMP-DDS plugin doesn't set DDPF_ALPHAPIXELS, but this is wrong IMO,
            so I warn about it. }
          CheckWarn(Header.PixelFormat.Flags and DDPF_ALPHAPIXELS <> 0, 'Invalid DDS pixel format: no flag specified (so must be grayscale inmage), but all r/g/b masks are zero. We will assume this is alpha-only image, as GIMP-DDS plugin can write such files');
          Result := TGrayscaleAlphaImage.Create(Width, Height);
          ReadToGrayscaleAlphaPure;
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
    end { ReadImage };

    procedure AllocateImages(const Count: Cardinal);
    begin
      SetLength(FImages, Count);
      { zero memory, to allow easy deallocation in case an exception will
        be raised during image reading. }
      FillChar(Pointer(FImages)^, SizeOf(FImages), 0);
    end;

  var
    W, H: Cardinal;
    NextI, I: Integer;
    Side: TDDSCubeMapSide;
  begin
    { Check that Width/Height are power of two, this is needed to make
      sure images reading code is sensible (otherwise, there's no way
      to tell what are the sizes of following images). }
    if FMipmaps then
    begin
      if (not IsPowerOf2(Width)) or
         (not IsPowerOf2(Height)) then
        raise EInvalidDDS.CreateFmt('DDS image has mipmaps, but width or height are not a power of 2: %d x %d', [Width, Height]);
    end;

    case DDSType of
      dtTexture:
        begin
          if Mipmaps then
          begin
            AllocateImages(FMipmapsCount);
            W := Width;
            H := Height;
            for I := 0 to FMipmapsCount - 1 do
            begin
              FImages[I] := ReadImage(W, H);
              W := Max(1, W div 2);
              H := Max(1, H div 2);
            end;
          end else
          begin
            AllocateImages(1);
            FImages[0] := ReadImage(Width, Height);
          end;
        end;
      dtCubeMap:
        begin
          if Mipmaps then
            AllocateImages(FMipmapsCount * 6) else
            AllocateImages(6);

          NextI := 0;

          for Side := Low(Side) to High(Side) do
            if Side in FCubeMapSides then
            begin
              if Mipmaps then
              begin
                W := Width;
                H := Height;
                for I := 0 to FMipmapsCount - 1 do
                begin
                  FImages[NextI] := ReadImage(W, H);
                  Inc(NextI);
                  W := Max(1, W div 2);
                  H := Max(1, H div 2);
                end;
              end else
              begin
                FImages[NextI] := ReadImage(Width, Height);
                Inc(NextI);
              end;
            end;
        end;
      dtVolume:
        raise EInvalidDDS.Create('TODO: Volume (3D) textures not supported yet');
      else raise EInternalError.Create('DDSType?');
    end;
  end { ReadImages };

begin
  try
    Close;
    ReadHeader;
    ReadImages;
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
