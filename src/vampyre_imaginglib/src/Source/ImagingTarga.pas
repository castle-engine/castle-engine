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

{ This unit contains image format loader/saver for Targa images.}
unit ImagingTarga;

{$I ImagingOptions.inc}

{ Castle Game Engine additions }
{ Needs overflow checking off.

  Testcase: open TGA from Vampyre Demos,
    Delphi 11,
    Windows 64.

  Fails at 177:
      // set position in source to real end of compressed data
      Seek(Handle, -(BufSize - (PtrUInt(Src) - PtrUInt(Buffer))),
        smFromCurrent);
}
{$Q-}

interface

uses
  ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Class for loading and saving Truevision Targa images.
    It can load/save 8bit indexed or grayscale, 16 bit RGB or grayscale,
    24 bit RGB and 32 bit ARGB images with or without RLE compression.}
  TTargaFileFormat = class(TImageFileFormat)
  protected
    FUseRLE: LongBool;
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  published
    { Controls that RLE compression is used during saving. Accessible trough
      ImagingTargaRLE option.}
    property UseRLE: LongBool read FUseRLE write FUseRLE;
  end;

implementation

const
  STargaFormatName = 'Truevision Targa Image';
  STargaMasks      = '*.tga';
  TargaSupportedFormats: TImageFormats = [ifIndex8, ifGray8, ifA1R5G5B5,
    ifR8G8B8, ifA8R8G8B8];
  TargaDefaultRLE = False;  

const
  STargaSignature = 'TRUEVISION-XFILE';

type
  { Targa file header.}
  TTargaHeader = packed record
    IDLength: Byte;
    ColorMapType: Byte;
    ImageType: Byte;
    ColorMapOff: Word;
    ColorMapLength: Word;
    ColorEntrySize: Byte;
    XOrg: SmallInt;
    YOrg: SmallInt;
    Width: SmallInt;
    Height: SmallInt;
    PixelSize: Byte;
    Desc: Byte;
  end;

  { Footer at the end of TGA file.}
  TTargaFooter = packed record
    ExtOff: UInt32;               // Extension Area Offset
    DevDirOff: UInt32;            // Developer Directory Offset
    Signature: TChar16;           // TRUEVISION-XFILE
    Reserved: Byte;               // ASCII period '.'
    NullChar: Byte;               // 0
  end;


{ TTargaFileFormat class implementation }

procedure TTargaFileFormat.Define;
begin
  inherited;
  FName := STargaFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := TargaSupportedFormats;

  FUseRLE := TargaDefaultRLE;

  AddMasks(STargaMasks);
  RegisterOption(ImagingTargaRLE, @FUseRLE);
end;

function TTargaFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Hdr: TTargaHeader;
  Foo: TTargaFooter;
  FooterFound, ExtFound: Boolean;
  I, PSize, PalSize: Integer;
  Pal: Pointer;
  FmtInfo: TImageFormatInfo;
  WordValue: Word;

  procedure LoadRLE;
  var
    I, CPixel, Cnt: LongInt;
    Bpp, Rle: Byte;
    Buffer, Dest, Src: PByte;
    BufSize: LongInt;
  begin
    with GetIO, Images[0] do
    begin
      // Allocates buffer large enough to hold the worst case
      // RLE compressed data and reads then from input
      BufSize := Width * Height * FmtInfo.BytesPerPixel;
      BufSize := BufSize + BufSize div 2 + 1;
      GetMem(Buffer, BufSize);
      Src := Buffer;
      Dest := Bits;
      BufSize := Read(Handle, Buffer, BufSize);

      Cnt := Width * Height;
      Bpp := FmtInfo.BytesPerPixel;
      CPixel := 0;
      while CPixel < Cnt do
      begin
        Rle := Src^;
        Inc(Src);
        if Rle < 128 then
        begin
          // Process uncompressed pixel
          Rle := Rle + 1;
          CPixel := CPixel + Rle;
          for I := 0 to Rle - 1 do
          begin
            // Copy pixel from src to dest
            case Bpp of
              1: Dest^ := Src^;
              2: PWord(Dest)^ := PWord(Src)^;
              3: PColor24Rec(Dest)^ := PColor24Rec(Src)^;
              4: PUInt32(Dest)^ := PUInt32(Src)^;
            end;
            Inc(Src, Bpp);
            Inc(Dest, Bpp);
          end;
        end
        else
        begin
          // Process compressed pixels
          Rle := Rle - 127;
          CPixel := CPixel + Rle;
          // Copy one pixel from src to dest (many times there)
          for I := 0 to Rle - 1 do
          begin
            case Bpp of
              1: Dest^ := Src^;
              2: PWord(Dest)^ := PWord(Src)^;
              3: PColor24Rec(Dest)^ := PColor24Rec(Src)^;
              4: PUInt32(Dest)^ := PUInt32(Src)^;
            end;
            Inc(Dest, Bpp);
          end;
          Inc(Src, Bpp);
        end;
      end;
      // set position in source to real end of compressed data
      Seek(Handle, -(BufSize - (PtrUInt(Src) - PtrUInt(Buffer))),
        smFromCurrent);
      FreeMem(Buffer);
    end;
  end;

begin
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Read targa header
    Read(Handle, @Hdr, SizeOf(Hdr));
    // Skip image ID info
    Seek(Handle, Hdr.IDLength, smFromCurrent);
    // Determine image format
    Format := ifUnknown;
    case Hdr.ImageType of
      1, 9: Format := ifIndex8;
      2, 10: case Hdr.PixelSize of
          15: Format := ifX1R5G5B5;
          16: Format := ifA1R5G5B5;
          24: Format := ifR8G8B8;
          32: Format := ifA8R8G8B8;
        end;
      3, 11: Format := ifGray8;
    end;
    // Format was not assigned by previous testing (it should be in
    // well formed targas), so formats which reflects bit dept are selected
    if Format = ifUnknown then
      case Hdr.PixelSize of
        8: Format := ifGray8;
        15: Format := ifX1R5G5B5;
        16: Format := ifA1R5G5B5;
        24: Format := ifR8G8B8;
        32: Format := ifA8R8G8B8;
      end;
    NewImage(Hdr.Width, Hdr.Height, Format, Images[0]);
    FmtInfo := GetFormatInfo(Format);

    if (Hdr.ColorMapType = 1) and (Hdr.ImageType in [1, 9]) then
    begin
      // Read palette
      PSize := Hdr.ColorMapLength * (Hdr.ColorEntrySize shr 3);
      GetMem(Pal, PSize);
      try
        Read(Handle, Pal, PSize);
        // Process palette
        PalSize := Iff(Hdr.ColorMapLength > FmtInfo.PaletteEntries,
          FmtInfo.PaletteEntries, Hdr.ColorMapLength);
        for I := 0 to PalSize - 1 do
          case Hdr.ColorEntrySize of
            24:
              with Palette[I] do
              begin
                A := $FF;
                R := PPalette24(Pal)[I].R;
                G := PPalette24(Pal)[I].G;
                B := PPalette24(Pal)[I].B;
              end;
            // I've never seen tga with these palettes so they are untested
            16:
              with Palette[I] do
              begin
                A := (PWordArray(Pal)[I] and $8000) shr 12;
                R := (PWordArray(Pal)[I] and $FC00) shr 7;
                G := (PWordArray(Pal)[I] and $03E0) shr 2;
                B := (PWordArray(Pal)[I] and $001F) shl 3;
              end;
            32:
              with Palette[I] do
              begin
                A := PPalette32(Pal)[I].A;
                R := PPalette32(Pal)[I].R;
                G := PPalette32(Pal)[I].G;
                B := PPalette32(Pal)[I].B;
              end;
          end;
      finally
        FreeMemNil(Pal);
      end;
    end;

    case Hdr.ImageType of
      0, 1, 2, 3:
        // Load uncompressed mode images
        Read(Handle, Bits, Size);
      9, 10, 11:
        // Load RLE compressed mode images
        LoadRLE;
    end;

    // Check if there is alpha channel present in A1R5GB5 images, if it is not
    // change format to X1R5G5B5
    if Format = ifA1R5G5B5 then
    begin
      if not Has16BitImageAlpha(Width * Height, Bits) then
        Format := ifX1R5G5B5;
    end;

    // We must find true end of file and set input' position to it
    // paint programs appends extra info at the end of Targas
    // some of them multiple times (PSP Pro 8)
    repeat
      ExtFound := False;
      FooterFound := False;

      if Read(Handle, @WordValue, 2) = 2 then
      begin
        // 495 = size of Extension Area
        if WordValue = 495 then
        begin
          Seek(Handle, 493, smFromCurrent);
          ExtFound := True;
        end
        else
          Seek(Handle, -2, smFromCurrent);
      end;

      if Read(Handle, @Foo, SizeOf(Foo)) = SizeOf(Foo) then
      begin
        if Foo.Signature = STargaSignature then
          FooterFound := True
        else
          Seek(Handle, -SizeOf(Foo), smFromCurrent);
      end;
    until (not ExtFound) and (not FooterFound);

    // Some editors save targas flipped
    if Hdr.Desc < 31 then
      FlipImage(Images[0]);

    Result := True;
  end;
end;

function TTargaFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  I: LongInt;
  Hdr: TTargaHeader;
  FmtInfo: TImageFormatInfo;
  Pal: PPalette24;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;

  procedure SaveRLE;
  var
    Dest: PByte;
    WidthBytes, Written, I, Total, DestSize: LongInt;

    function CountDiff(Data: PByte; Bpp, PixelCount: Longint): LongInt;
    var
      Pixel: UInt32;
      NextPixel: UInt32;
      N: LongInt;
    begin
      N := 0;
      Pixel := 0;
      NextPixel := 0;
      if PixelCount = 1 then
      begin
        Result := PixelCount;
        Exit;
      end;
      case Bpp of
        1: Pixel := Data^;
        2: Pixel := PWord(Data)^;
        3: PColor24Rec(@Pixel)^ := PColor24Rec(Data)^;
        4: Pixel := PUInt32(Data)^;
      end;
      while PixelCount > 1 do
      begin
        Inc(Data, Bpp);
        case Bpp of
          1: NextPixel := Data^;
          2: NextPixel := PWord(Data)^;
          3: PColor24Rec(@NextPixel)^ := PColor24Rec(Data)^;
          4: NextPixel := PUInt32(Data)^;
        end;
        if NextPixel = Pixel then
          Break;
        Pixel := NextPixel;
        N := N + 1;
        PixelCount := PixelCount - 1;
      end;
      if NextPixel = Pixel then
        Result := N
      else
        Result := N + 1;
    end;

    function CountSame(Data: PByte; Bpp, PixelCount: LongInt): LongInt;
    var
      Pixel: UInt32;
      NextPixel: UInt32;
      N: LongInt;
    begin
      N := 1;
      Pixel := 0;
      NextPixel := 0;
      case Bpp of
        1: Pixel := Data^;
        2: Pixel := PWord(Data)^;
        3: PColor24Rec(@Pixel)^ := PColor24Rec(Data)^;
        4: Pixel := PUInt32(Data)^;
      end;
      PixelCount := PixelCount - 1;
      while PixelCount > 0 do
      begin
        Inc(Data, Bpp);
        case Bpp of
          1: NextPixel := Data^;
          2: NextPixel := PWord(Data)^;
          3: PColor24Rec(@NextPixel)^ := PColor24Rec(Data)^;
          4: NextPixel := PUInt32(Data)^;
        end;
        if NextPixel <> Pixel then
          Break;
        N := N + 1;
        PixelCount := PixelCount - 1;
      end;
      Result := N;
    end;

    procedure RleCompressLine(Data: PByte; PixelCount, Bpp: LongInt; Dest:
      PByte; out Written: LongInt);
    const
      MaxRun = 128;
    var
      DiffCount: LongInt;
      SameCount: LongInt;
      RleBufSize: LongInt;
    begin
      RleBufSize := 0;
      while PixelCount > 0 do
      begin
        DiffCount := CountDiff(Data, Bpp, PixelCount);
        SameCount := CountSame(Data, Bpp, PixelCount);
        if (DiffCount > MaxRun) then
          DiffCount := MaxRun;
        if (SameCount > MaxRun) then
          SameCount := MaxRun;
        if (DiffCount > 0) then
        begin
          Dest^ := Byte(DiffCount - 1);
          Inc(Dest);
          PixelCount := PixelCount - DiffCount;
          RleBufSize := RleBufSize + (DiffCount * Bpp) + 1;
          Move(Data^, Dest^, DiffCount * Bpp);
          Inc(Data, DiffCount * Bpp);
          Inc(Dest, DiffCount * Bpp);
        end;
        if SameCount > 1 then
        begin
          Dest^ := Byte((SameCount - 1) or $80);
          Inc(Dest);
          PixelCount := PixelCount - SameCount;
          RleBufSize := RleBufSize + Bpp + 1;
          Inc(Data, (SameCount - 1) * Bpp);
          case Bpp of
            1: Dest^ := Data^;
            2: PWord(Dest)^ := PWord(Data)^;
            3: PColor24Rec(Dest)^ := PColor24Rec(Data)^;
            4: PUInt32(Dest)^ := PUInt32(Data)^;
          end;
          Inc(Data, Bpp);
          Inc(Dest, Bpp);
        end;
      end;
      Written := RleBufSize;
    end;

  begin
    with ImageToSave do
    begin
      // Allocate enough space to hold the worst case compression
      // result and then compress source's scanlines
      WidthBytes := Width * FmtInfo.BytesPerPixel;
      DestSize := WidthBytes * Height;
      DestSize := DestSize + DestSize div 2 + 1;
      GetMem(Dest, DestSize);
      Total := 0;
      try
        for I := 0 to Height - 1 do
        begin
          RleCompressLine(@PByteArray(Bits)[I * WidthBytes], Width,
            FmtInfo.BytesPerPixel, @PByteArray(Dest)[Total], Written);
          Total := Total + Written;
        end;
        GetIO.Write(Handle, Dest, Total);
      finally
        FreeMem(Dest);
      end;
    end;
  end;

begin
  Result := False;
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    FmtInfo := GetFormatInfo(Format);
    // Fill targa header
    FillChar(Hdr, SizeOf(Hdr), 0);
    Hdr.IDLength := 0;
    Hdr.ColorMapType := Iff(FmtInfo.PaletteEntries > 0, 1, 0);
    Hdr.Width := Width;
    Hdr.Height := Height;
    Hdr.PixelSize := FmtInfo.BytesPerPixel * 8;
    Hdr.ColorMapLength := FmtInfo.PaletteEntries;
    Hdr.ColorEntrySize := Iff(FmtInfo.PaletteEntries > 0, 24, 0);
    Hdr.ColorMapOff := 0;
    // This indicates that targa is stored in top-left format
    // as our images -> no flipping is needed.
    Hdr.Desc := 32;
    // Set alpha channel size in descriptor (mostly ignored by other software though)
    if Format = ifA8R8G8B8 then
      Hdr.Desc := Hdr.Desc or 8
    else if Format = ifA1R5G5B5 then
      Hdr.Desc := Hdr.Desc or 1;

    // Choose image type
    if FmtInfo.IsIndexed then
      Hdr.ImageType := Iff(FUseRLE, 9, 1)
    else
      if FmtInfo.HasGrayChannel then
        Hdr.ImageType := Iff(FUseRLE, 11, 3)
      else
        Hdr.ImageType := Iff(FUseRLE, 10, 2);

    Write(Handle, @Hdr, SizeOf(Hdr));

    // Write palette
    if FmtInfo.PaletteEntries > 0 then
    begin
      GetMem(Pal, FmtInfo.PaletteEntries * SizeOf(TColor24Rec));
      try
        for I := 0 to FmtInfo.PaletteEntries - 1 do
          with Pal[I] do
          begin
            R := Palette[I].R;
            G := Palette[I].G;
            B := Palette[I].B;
          end;
        Write(Handle, Pal, FmtInfo.PaletteEntries * SizeOf(TColor24Rec));
      finally
        FreeMemNil(Pal);
      end;
    end;

    if FUseRLE then
      // Save rle compressed mode images
      SaveRLE
    else
      // Save uncompressed mode images
      Write(Handle, Bits, Size);

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TTargaFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.HasGrayChannel then
    // Convert all grayscale images to Gray8 (preserve alpha of AxGrayx formats)
    ConvFormat := IffFormat(not Info.HasAlphaChannel, ifGray8, ifA8R8G8B8)
  else if Info.IsIndexed then
    // Convert all indexed images to Index8
    ConvFormat := ifIndex8
  else if Info.HasAlphaChannel then
    // Convert images with alpha channel to A8R8G8B8
    ConvFormat := ifA8R8G8B8
  else if Info.UsePixelFormat then
    // Convert 16bit images (without alpha channel) to A1R5G5B5
    ConvFormat := ifA1R5G5B5
  else
    // Convert all other formats to R8G8B8
    ConvFormat := ifR8G8B8;

  ConvertImage(Image, ConvFormat);
end;

function TTargaFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TTargaHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Hdr, SizeOf(Hdr));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Hdr)) and
      (Hdr.ImageType in [0, 1, 2, 3, 9, 10, 11]) and
      (Hdr.PixelSize in [1, 8, 15, 16, 24, 32]) and
      (Hdr.ColorEntrySize in [0, 16, 24, 32]);
  end;
end;

initialization
  RegisterImageFileFormat(TTargaFileFormat);

{
  File Notes:

 -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Made public properties for options registered to SetOption/GetOption
      functions.
    - Changed extensions to filename masks.
    - Changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit.

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - 16 bit images are usually without alpha but some has alpha
      channel and there is no indication of it - so I have added
      a check: if all pixels of image are with alpha = 0 image is treated
      as X1R5G5B5 otherwise as A1R5G5B5
    - fixed problems with some nonstandard 15 bit images
}

end.

