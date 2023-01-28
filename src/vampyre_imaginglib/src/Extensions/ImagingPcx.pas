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

{ This unit contains image format loader for ZSoft Paintbrush images known as PCX.}
unit ImagingPcx;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingFormats, ImagingUtility, ImagingIO;

type
  { Class for loading  ZSoft Paintbrush images known as PCX. It is old
    format which can store 1bit, 2bit, 4bit, 8bit, and 24bit (and 32bit but is
    probably non-standard) images. Only loading is supported (you can still come
    across some PCX files) but saving is not (I don't wont this venerable format
    to spread).}
  TPCXFileFormat = class(TImageFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SPCXFormatName = 'ZSoft Paintbrush Image';
  SPCXMasks      = '*.pcx';

type
  TPCXHeader = packed record
    Id: Byte;            // Always $0A
    Version: Byte;       // 0, 2, 3, 4, 5
    Encoding: Byte;      // 0, 1
    BitsPerPixel: Byte;  // 1, 2, 4, 8
    X0, Y0: Word;        // Image window top-left
    X1, Y1: Word;        // Image window bottom-right
    DpiX: Word;
    DpiY: Word;
    Palette16: array [0..15] of TColor24Rec;
    Reserved1: Byte;
    Planes: Byte;        // 1, 3, 4
    BytesPerLine: Word;
    PaletteType: Word;   // 1: color or s/w   2: grayscale
    Reserved2: array [0..57] of Byte;
  end;

{ TPCXFileFormat }

procedure TPCXFileFormat.Define;
begin
  inherited;
  FName := SPCXFormatName;
  FFeatures := [ffLoad];

  AddMasks(SPCXMasks);
end;

function TPCXFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
const
  ifMono: TImageFormat = TImageFormat(250);
  ifIndex2: TImageFormat = TImageFormat(251);
  ifIndex4: TImageFormat = TImageFormat(252);
var
  Hdr: TPCXHeader;
  PalID, B: Byte;
  PalPCX: TPalette24Size256;
  FileDataFormat: TImageFormat;
  I, J, UncompSize, BytesPerLine, ByteNum, BitNum: LongInt;
  UncompData, RowPointer, PixelIdx: PByte;
  Pixel24: PColor24Rec;
  Pixel32: PColor32Rec;
  AlphaPlane, RedPlane, GreenPlane, BluePlane,
  Plane1, Plane2, Plane3, Plane4: PByteArray;

  procedure RleDecode(Target: PByte; UnpackedSize: LongInt);
  var
    Count: LongInt;
    Source: Byte;
  begin
    while UnpackedSize > 0 do
    with GetIO do
    begin
      GetIO.Read(Handle, @Source, SizeOf(Source));
      if (Source and $C0) = $C0 then
      begin
        // RLE data
        Count := Source and $3F;
        if UnpackedSize < Count then
          Count := UnpackedSize;
        Read(Handle, @Source, SizeOf(Source));
        FillChar(Target^, Count, Source);
        //Inc(Source);
        Inc(Target, Count);
        Dec(UnpackedSize, Count);
      end
      else
      begin
        // Uncompressed data
        Target^ := Source;
        Inc(Target);
        Dec(UnpackedSize);
      end;
    end;
  end;

begin
  Result := False;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Read PCX header and store input position (start of image data)
    Read(Handle, @Hdr, SizeOf(Hdr));
    FileDataFormat := ifUnknown;

    // Determine image's data format and find its Imaging equivalent
    // (using some custom TImageFormat constants)
    case Hdr.BitsPerPixel of
      1:
        case Hdr.Planes of
          1: FileDataFormat := ifMono;
          4: FileDataFormat := ifIndex4;
        end;
      2: FileDataFormat := ifIndex2;
      4: FileDataFormat := ifIndex4;
      8:
        case Hdr.Planes of
          1: FileDataFormat := ifIndex8;
          3: FileDataFormat := ifR8G8B8;
          4: FileDataFormat := ifA8R8G8B8;
        end;
    end;

    // No compatible Imaging format found, exit
    if FileDataFormat = ifUnknown then
      Exit;

    // Get width, height, and output data format (unsupported formats
    // like ifMono are converted later to ifIndex8)
    Width := Hdr.X1 - Hdr.X0 + 1;
    Height := Hdr.Y1 - Hdr.Y0 + 1;
    if FileDataFormat in [ifIndex8, ifR8G8B8] then
      Format := FileDataFormat
    else
      Format := ifIndex8;

    NewImage(Width, Height, Format, Images[0]);

    if not (FileDataFormat in [ifIndex8, ifR8G8B8]) then
    begin
      // other formats use palette embedded to file header
      for I := Low(Hdr.Palette16) to High(Hdr.Palette16) do
      begin
        Palette[I].A := $FF;
        Palette[I].R := Hdr.Palette16[I].B;
        Palette[I].G := Hdr.Palette16[I].G;
        Palette[I].B := Hdr.Palette16[I].R;
      end;
    end;

    // Now we determine various data sizes
    BytesPerLine := Hdr.BytesPerLine * Hdr.Planes;
    UncompSize := BytesPerLine * Height;

    GetMem(UncompData, UncompSize);
    try
      if Hdr.Encoding = 1 then
      begin
        // Image data is compressed -> read and decompress
        RleDecode(UncompData, UncompSize);
      end
      else
      begin
        // Just read uncompressed data
        Read(Handle, UncompData, UncompSize);
      end;

      if FileDataFormat in [ifR8G8B8, ifA8R8G8B8] then
      begin
        // RGB and ARGB images are stored in layout different from
        // Imaging's (and most other file formats'). First there is
        // Width red values then there is Width green values and so on
        RowPointer := UncompData;

        if FileDataFormat = ifA8R8G8B8 then
        begin
          Pixel32 := Bits;
          for I := 0 to Height - 1 do
          begin
            AlphaPlane := PByteArray(RowPointer);
            RedPlane :=   @AlphaPlane[Hdr.BytesPerLine];
            GreenPlane := @AlphaPlane[Hdr.BytesPerLine * 2];
            BluePlane :=  @AlphaPlane[Hdr.BytesPerLine * 3];
            for J := 0 to Width - 1 do
            begin
              Pixel32.A := AlphaPlane[J];
              Pixel32.R := RedPlane[J];
              Pixel32.G := GreenPlane[J];
              Pixel32.B := BluePlane[J];
              Inc(Pixel32);
            end;
            Inc(RowPointer, BytesPerLine);
          end;
        end
        else
        begin
          Pixel24 := Bits;
          for I := 0 to Height - 1 do
          begin
            RedPlane :=   PByteArray(RowPointer);
            GreenPlane := @RedPlane[Hdr.BytesPerLine];
            BluePlane :=  @RedPlane[Hdr.BytesPerLine * 2];
            for J := 0 to Width - 1 do
            begin
              Pixel24.R := RedPlane[J];
              Pixel24.G := GreenPlane[J];
              Pixel24.B := BluePlane[J];
              Inc(Pixel24);
            end;
            Inc(RowPointer, BytesPerLine);
          end;
        end;
      end
      else if FileDataFormat = ifIndex8 then
      begin
        // Just copy 8bit lines
        for I := 0 to Height - 1 do
          Move(PByteArray(UncompData)[I * Hdr.BytesPerLine], PByteArray(Bits)[I * Width], Width);
      end
      else if FileDataFormat = ifMono then
      begin
        // Convert 1bit images to ifIndex8
        Convert1To8(UncompData, Bits, Width, Height, Hdr.BytesPerLine, False);
      end
      else if FileDataFormat = ifIndex2 then
      begin
        // Convert 2bit images to ifIndex8. Note that 2bit PCX images
        // usually use (from specs, I've never seen one myself) CGA palette
        // which is not array of RGB triplets. So 2bit PCXs are loaded but
        // their colors would be wrong
        Convert2To8(UncompData, Bits, Width, Height, Hdr.BytesPerLine, False);
      end
      else if FileDataFormat = ifIndex4 then
      begin
        // 4bit images can be stored similar to RGB images (in four one bit planes)
        // or like array of nibbles (which is more common)
        if (Hdr.BitsPerPixel = 1) and (Hdr.Planes = 4) then
        begin
          RowPointer := UncompData;
          PixelIdx := Bits;
          for I := 0 to Height - 1 do
          begin
            Plane1 := PByteArray(RowPointer);
            Plane2 := @Plane1[Hdr.BytesPerLine];
            Plane3 := @Plane1[Hdr.BytesPerLine * 2];
            Plane4 := @Plane1[Hdr.BytesPerLine * 3];

            for J := 0 to Width - 1 do
            begin
              B := 0;
              ByteNum := J div 8;
              BitNum := 7 - (J mod 8);
              if (Plane1[ByteNum] shr BitNum) and $1 <> 0 then B := B or $01;
              if (Plane2[ByteNum] shr BitNum) and $1 <> 0 then B := B or $02;
              if (Plane3[ByteNum] shr BitNum) and $1 <> 0 then B := B or $04;
              if (Plane4[ByteNum] shr BitNum) and $1 <> 0 then B := B or $08;
              PixelIdx^ := B;
              Inc(PixelIdx);
            end;
            Inc(RowPointer, BytesPerLine);
          end;
        end
        else if (Hdr.BitsPerPixel = 4) and (Hdr.Planes = 1) then
        begin
          // Convert 4bit images to ifIndex8 
          Convert4To8(UncompData, Bits, Width, Height, Hdr.BytesPerLine, False);
        end
      end;

      if FileDataFormat = ifIndex8 then
      begin
        // 8bit palette is appended at the end of the file
        // with $0C identifier
        //Seek(Handle, -769, smFromEnd);
        Read(Handle, @PalID, SizeOf(PalID));
        if PalID = $0C then
        begin
          Read(Handle, @PalPCX, SizeOf(PalPCX));
          for I := Low(PalPCX) to High(PalPCX) do
          begin
            Palette[I].A := $FF;
            Palette[I].R := PalPCX[I].B;
            Palette[I].G := PalPCX[I].G;
            Palette[I].B := PalPCX[I].R;
          end;
        end
        else
          Seek(Handle, -SizeOf(PalID), smFromCurrent);
      end;

    finally
      FreeMem(UncompData);
    end;
    Result := True;
  end;
end;

function TPCXFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TPCXHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Hdr)) and
      (Hdr.Id = $0A) and
      (Hdr.Version in [0, 2, 3, 4, 5]) and
      (Hdr.Encoding in [0..1]) and
      (Hdr.BitsPerPixel in [1, 2, 4, 8]) and
      (Hdr.Planes in [1, 3, 4]) and
      (Hdr.PaletteType in [1..2]);
  end;

end;

initialization
  RegisterImageFileFormat(TPCXFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Made loader stream-safe - stream position is exactly at the end of the
      image after loading and file size doesn't need to be know during the process.
    - Initial TPCXFileFormat class implemented.

}

end.
