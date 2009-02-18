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
  var
    RowBytePadding: Integer;
    X, Y: Integer;
    Row3: PVector3Byte;
    Row4: PVector4Byte;
  begin
    { Handle R8G8B8 uncompressed format. }
    if (Header.PixelFormat.Flags and DDPF_RGB <> 0) and
       (Header.PixelFormat.Flags and DDPF_FOURCC = 0) and
       (Header.PixelFormat.Flags and DDPF_ALPHAPIXELS = 0) and
       (Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 = 0) and
       (Header.PixelFormat.RGBBitCount = 24) and
       (Header.PixelFormat.RBitMask = $ff0000) and
       (Header.PixelFormat.GBitMask = $00ff00) and
       (Header.PixelFormat.BBitMask = $0000ff) then
    begin
      Result := TRGBImage.Create(Width, Height);

      { Header.PitchOrLinearSize for uncompressed texture may indicate
        row length (pitch) in bytes. This is useful to indicate padding of lines.
        I understand that otherwise I should assume lines are not padded? }
      if (Header.Flags and DDSD_PITCH <> 0) and
         (Header.PitchOrLinearSize <> 0) then
        RowBytePadding := Max(Header.PitchOrLinearSize - Result.PixelSize * Width, 0) else
        RowBytePadding := 0;

      for Y := Height - 1 downto 0 do
      begin
        Row3 := Result.RowPtr(Y);
        Stream.ReadBuffer(Row3^, Result.PixelSize * Width);

        { Now invert red and blue. (Since all masks are little-endian,
          RBitMask = $FF0000 means that red is the 3rd (not 1st) byte...) }
        for X := 0 to Width - 1 do
        begin
          SwapValues(Row3^[2], Row3^[0]);
          Inc(Row3);
        end;

        if RowBytePadding <> 0 then
          Stream.Seek(RowBytePadding, soFromCurrent);
      end;
    end else
    { Handle A8R8G8B8 uncompressed format. }
    if (Header.PixelFormat.Flags and DDPF_RGB <> 0) and
       (Header.PixelFormat.Flags and DDPF_FOURCC = 0) and
       (Header.PixelFormat.Flags and DDPF_ALPHAPIXELS <> 0) and
       (Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 = 0) and
       (Header.PixelFormat.RGBBitCount = 32) and
       (Header.PixelFormat.ABitMask = $ff000000) and
       (Header.PixelFormat.RBitMask = $00ff0000) and
       (Header.PixelFormat.GBitMask = $0000ff00) and
       (Header.PixelFormat.BBitMask = $000000ff) then
    begin
      Result := TRGBAlphaImage.Create(Width, Height);

      { Header.PitchOrLinearSize for uncompressed texture may indicate row length
        in bytes. This is useful to indicate padding of lines.
        I understand that otherwise I should assume lines are not padded? }
      if (Header.Flags and DDSD_PITCH <> 0) and
         (Header.PitchOrLinearSize <> 0) then
        RowBytePadding := Max(Header.PitchOrLinearSize - Result.PixelSize * Width, 0) else
        RowBytePadding := 0;

      for Y := Height - 1 downto 0 do
      begin
        Row4 := Result.RowPtr(Y);
        Stream.ReadBuffer(Row4^, Result.PixelSize * Width);

        { Now invert BGRA to ARGB. (Since all masks are little-endian). }
        for X := 0 to Width - 1 do
        begin
          SwapValues(Row4^[3], Row4^[0]);
          SwapValues(Row4^[2], Row4^[1]);
          Inc(Row4);
        end;

        if RowBytePadding <> 0 then
          Stream.Seek(RowBytePadding, soFromCurrent);
      end;
    end else
    begin
      raise EInvalidDDS.CreateFmt('Unsupported pixel format for DDS: uncompressed RGB = %s (%d bits), alpha = %s, compressed = %s (fourcc: %s%s%s%s), palette = %s, R/G/B/AMask = %s/%s/%s/%s',
        [ BoolToStr[Header.PixelFormat.Flags and DDPF_RGB <> 0],
          Header.PixelFormat.RGBBitCount,
          BoolToStr[Header.PixelFormat.Flags and DDPF_ALPHAPIXELS <> 0],
          BoolToStr[Header.PixelFormat.Flags and DDPF_FOURCC <> 0],
          SReadableForm(Header.PixelFormat.FourCC[0]),
          SReadableForm(Header.PixelFormat.FourCC[1]),
          SReadableForm(Header.PixelFormat.FourCC[2]),
          SReadableForm(Header.PixelFormat.FourCC[3]),
          BoolToStr[Header.PixelFormat.Flags and DDPF_PALETTEINDEXED8 <> 0],
          IntToHex(Header.PixelFormat.RBitMask, 8),
          IntToHex(Header.PixelFormat.GBitMask, 8),
          IntToHex(Header.PixelFormat.BBitMask, 8),
          IntToHex(Header.PixelFormat.ABitMask, 8) ]);
    end;
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
