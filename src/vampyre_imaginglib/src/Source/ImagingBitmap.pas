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

{
  This unit contains image format loader/saver for Windows Bitmap images.
}
unit ImagingBitmap;

{$I ImagingOptions.inc}

interface

uses
  ImagingTypes, Imaging, ImagingUtility, ImagingFormats, ImagingIO;

type
  { Class for loading and saving Windows Bitmap images.
    It can load/save 8bit indexed, 16, 24, 32 bit RGB or ARGB
    images with or without RLE compression. It can also load 1/4 bit
    indexed images and OS2 bitmaps.}
  TBitmapFileFormat = class(TImageFileFormat)
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
      ImagingBitmapRLE option.}
    property UseRLE: LongBool read FUseRLE write FUseRLE;
  end;

implementation

const
  SBitmapFormatName = 'Windows Bitmap Image';
  SBitmapMasks =      '*.bmp,*.dib';
  BitmapSupportedFormats: TImageFormats = [ifIndex8, ifA1R5G5B5, ifA4R4G4B4,
    ifR5G6B5, ifR8G8B8, ifA8R8G8B8, ifX1R5G5B5, ifX4R4G4B4, ifX8R8G8B8];
  BitmapDefaultRLE = True;  

const
  { Bitmap file identifier 'BM'.}
  BMMagic: Word = 19778;

  { Constants for the TBitmapInfoHeader.Compression field.}
  BI_RGB = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;

  V3InfoHeaderSize = 40;
  V4InfoHeaderSize = 108; 

type
  { File Header for Windows/OS2 bitmap file.}
  TBitmapFileHeader = packed record
    ID: Word;           // Is always 19778 : 'BM'
    Size: UInt32;       // File size
    Reserved1: Word;
    Reserved2: Word;
    Offset: UInt32;     // Offset from start pos to beginning of image bits
  end;

  { Info Header for Windows bitmap file version 4.}
  TBitmapInfoHeader = packed record
    Size: UInt32;
    Width: Int32;
    Height: Int32;
    Planes: Word;
    BitCount: Word;
    Compression: UInt32;
    SizeImage: UInt32;
    XPelsPerMeter: Int32;
    YPelsPerMeter: Int32;
    ClrUsed: UInt32;
    ClrImportant: UInt32;
    RedMask: UInt32;
    GreenMask: UInt32;
    BlueMask: UInt32;
    AlphaMask: UInt32;
    CSType: UInt32;
    EndPoints: array[0..8] of UInt32;
    GammaRed: UInt32;
    GammaGreen: UInt32;
    GammaBlue: UInt32;
  end;

  { Info Header for OS2 bitmaps.}
  TBitmapCoreHeader = packed record
    Size: UInt32;
    Width: Word;
    Height: Word;
    Planes: Word;
    BitCount: Word;
  end;

  { Used in RLE encoding and decoding.} 
  TRLEOpcode = packed record
    Count: Byte;
    Command: Byte;
  end;
  PRLEOpcode = ^TRLEOpcode;

{ TBitmapFileFormat class implementation }

procedure TBitmapFileFormat.Define;
begin
  inherited;
  FName := SBitmapFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := BitmapSupportedFormats;

  FUseRLE := BitmapDefaultRLE;

  AddMasks(SBitmapMasks);
  RegisterOption(ImagingBitmapRLE, @FUseRLE);
end;

function TBitmapFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
  BC: TBitmapCoreHeader;
  IsOS2: Boolean;
  PalRGB: PPalette24;
  I, FPalSize, AlignedSize, StartPos, HeaderSize, AlignedWidthBytes, WidthBytes: LongInt;
  Info: TImageFormatInfo;
  Data: Pointer;

  procedure LoadRGB;
  var
    I: LongInt;
    LineBuffer: PByte;
  begin
    with Images[0], GetIO do
    begin
      // If BI.Height is < 0 then image data are stored non-flipped
      // but default in windows is flipped so if Height is positive we must
      // flip it

      if BI.BitCount < 8 then
      begin
        // For 1 and 4 bit images load aligned data, they will be converted to
        // 8 bit and unaligned later
        GetMem(Data, AlignedSize);

        if BI.Height < 0 then
          Read(Handle, Data, AlignedSize)
        else
          for I := Height - 1 downto 0 do
            Read(Handle, @PByteArray(Data)[I * AlignedWidthBytes], AlignedWidthBytes);
      end
      else
      begin
        // Images with pixels of size >= 1 Byte are read line by line and
        // copied to image bits without padding bytes
        GetMem(LineBuffer, AlignedWidthBytes);
        try
          if BI.Height < 0 then
            for I := 0 to Height - 1 do
            begin
              Read(Handle, LineBuffer, AlignedWidthBytes);
              Move(LineBuffer^, PByteArray(Bits)[I * WidthBytes], WidthBytes);
            end
          else
            for I := Height - 1 downto 0 do
            begin
              Read(Handle, LineBuffer, AlignedWidthBytes);
              Move(LineBuffer^, PByteArray(Bits)[I * WidthBytes], WidthBytes);
            end;
        finally
          FreeMemNil(LineBuffer);
        end;
      end;
    end;
  end;

  procedure LoadRLE4;
  var
    RLESrc: PByteArray;
    Row, Col, WriteRow, I: Integer;
    SrcPos: UInt32;
    DeltaX, DeltaY, Low, High: Byte;
    Pixels: PByteArray;
    OpCode: TRLEOpcode;
    NegHeightBitmap: Boolean;
  begin
    GetMem(RLESrc, BI.SizeImage);
    GetIO.Read(Handle, RLESrc, BI.SizeImage);
    with Images[0] do
    try
      Low := 0;
      Pixels := Bits;
      SrcPos := 0;
      NegHeightBitmap := BI.Height < 0;
      Row := 0; // Current row in dest image
      Col := 0; // Current column in dest image
      // Row in dest image where actual writing will be done
      WriteRow := Iff(NegHeightBitmap, Row, Height - 1 - Row);
      while (Row < Height) and (SrcPos < BI.SizeImage) do
      begin
        // Read RLE op-code
        OpCode := PRLEOpcode(@RLESrc[SrcPos])^;
        Inc(SrcPos, SizeOf(OpCode));
        if OpCode.Count = 0 then
        begin
          // A byte Count of zero means that this is a special
          // instruction.
          case OpCode.Command of
            0:
              begin
                // Move to next row
                Inc(Row);
                WriteRow := Iff(NegHeightBitmap, Row, Height - 1 - Row);
                Col := 0;
              end ;
            1: Break; // Image is finished
            2:
              begin
                // Move to a new relative position
                DeltaX := RLESrc[SrcPos];
                DeltaY := RLESrc[SrcPos + 1];
                Inc(SrcPos, 2);
                Inc(Col, DeltaX);
                Inc(Row, DeltaY);
              end
          else
            // Do not read data after EOF
            if SrcPos + OpCode.Command > BI.SizeImage then
              OpCode.Command := BI.SizeImage - SrcPos;
            // Take padding bytes and nibbles into account
            if Col + OpCode.Command > Width then
              OpCode.Command := Width - Col;
            // Store absolute data. Command code is the
            // number of absolute bytes to store
            for I := 0 to OpCode.Command - 1 do
            begin
              if (I and 1) = 0 then
              begin
                High := RLESrc[SrcPos] shr 4;
                Low := RLESrc[SrcPos] and $F;
                Pixels[WriteRow * Width + Col] := High;
                Inc(SrcPos);
              end
              else
                Pixels[WriteRow * Width + Col] := Low;
              Inc(Col);
            end;
            // Odd number of bytes is followed by a pad byte
            if (OpCode.Command mod 4) in [1, 2] then
              Inc(SrcPos);
          end;
        end
        else
        begin
          // Take padding bytes and nibbles into account
          if Col + OpCode.Count > Width then
            OpCode.Count := Width - Col;
          // Store a run of the same color value
          for I := 0 to OpCode.Count - 1 do
          begin
            if (I and 1) = 0 then
              Pixels[WriteRow * Width + Col] := OpCode.Command shr 4
            else
              Pixels[WriteRow * Width + Col] := OpCode.Command and $F;
            Inc(Col);
          end;
        end;
      end;
    finally
      FreeMem(RLESrc);
    end;
  end;

  procedure LoadRLE8;
  var
    RLESrc: PByteArray;
    SrcCount, Row, Col, WriteRow: Integer;
    SrcPos: UInt32;
    DeltaX, DeltaY: Byte;
    Pixels: PByteArray;
    OpCode: TRLEOpcode;
    NegHeightBitmap: Boolean;
  begin
    GetMem(RLESrc, BI.SizeImage);
    GetIO.Read(Handle, RLESrc, BI.SizeImage);
    with Images[0] do
    try
      Pixels := Bits;
      SrcPos := 0;
      NegHeightBitmap := BI.Height < 0;
      Row := 0; // Current row in dest image
      Col := 0; // Current column in dest image
      // Row in dest image where actual writing will be done
      WriteRow := Iff(NegHeightBitmap, Row, Height - 1 - Row);
      while (Row < Height) and (SrcPos < BI.SizeImage) do
      begin
        // Read RLE op-code
        OpCode := PRLEOpcode(@RLESrc[SrcPos])^;
        Inc(SrcPos, SizeOf(OpCode));
        if OpCode.Count = 0 then
        begin
          // A byte Count of zero means that this is a special
          // instruction.
          case OpCode.Command of
            0:
              begin
                // Move to next row
                Inc(Row);
                WriteRow := Iff(NegHeightBitmap, Row, Height - 1 - Row);
                Col := 0;
              end ;
            1: Break; // Image is finished
            2:
              begin
                // Move to a new relative position
                DeltaX := RLESrc[SrcPos];
                DeltaY := RLESrc[SrcPos + 1];
                Inc(SrcPos, 2);
                Inc(Col, DeltaX);
                Inc(Row, DeltaY);
              end
          else
            SrcCount := OpCode.Command;
            // Do not read data after EOF
            if SrcPos + OpCode.Command > BI.SizeImage then
              OpCode.Command := BI.SizeImage - SrcPos;
            // Take padding bytes into account
            if Col + OpCode.Command > Width then
              OpCode.Command := Width - Col;
            // Store absolute data. Command code is the
            // number of absolute bytes to store
            Move(RLESrc[SrcPos], Pixels[WriteRow * Width + Col], OpCode.Command);
            Inc(SrcPos, SrcCount);
            Inc(Col, OpCode.Command);
            // Odd number of bytes is followed by a pad byte
            if (SrcCount mod 2) = 1 then
              Inc(SrcPos);
          end;
        end
        else
        begin
          // Take padding bytes into account
          if Col + OpCode.Count > Width then
            OpCode.Count := Width - Col;
          // Store a run of the same color value. Count is number of bytes to store
          FillChar(Pixels [WriteRow * Width + Col], OpCode.Count, OpCode.Command);
          Inc(Col, OpCode.Count);
        end;
      end;
    finally
      FreeMem(RLESrc);
    end;
  end;

begin
  Data := nil;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  try
    FillChar(BI, SizeOf(BI), 0);
    StartPos := Tell(Handle);
    Read(Handle, @BF, SizeOf(BF));
    Read(Handle, @BI.Size, SizeOf(BI.Size));
    IsOS2 := BI.Size = SizeOf(TBitmapCoreHeader);

    // Bitmap Info reading
    if IsOS2 then
    begin
      // OS/2 type bitmap, reads info header without 4 already read bytes
      Read(Handle, @PByteArray(@BC)[SizeOf(BI.Size)],
        SizeOf(TBitmapCoreHeader) - SizeOf(BI.Size));
      with BI do
      begin
        ClrUsed := 0;
        Compression := BI_RGB;
        BitCount := BC.BitCount;
        Height := BC.Height;
        Width := BC.Width;
      end;
    end
    else
    begin
      // Windows type bitmap
      HeaderSize := Min(BI.Size - SizeOf(BI.Size), SizeOf(BI) - SizeOf(BI.Size)); // do not read more than size of BI!
      Read(Handle, @PByteArray(@BI)[SizeOf(BI.Size)], HeaderSize);
      // SizeImage can be 0 for BI_RGB images, but it is here because of:
      // I saved 8bit bitmap in Paint Shop Pro 8 as OS2 RLE compressed.
      // It wrote strange 64 Byte Info header with SizeImage set to 0
      // Some progs were able to open it, some were not.
      if BI.SizeImage = 0 then
        BI.SizeImage := BF.Size - BF.Offset;
    end;
    // Bit mask reading. Only read it if there is V3 header, V4 header has
    // masks loaded already (only masks for RGB in V3).
    if (BI.Compression = BI_BITFIELDS) and (BI.Size = V3InfoHeaderSize) then
      Read(Handle, @BI.RedMask, SizeOf(BI.RedMask) * 3);

    case BI.BitCount of
      1, 4, 8: Format := ifIndex8;
      16:
        if BI.RedMask = $0F00 then
          // Set XRGB4 or ARGB4 according to value of alpha mask
          Format := IffFormat(BI.AlphaMask = 0, ifX4R4G4B4, ifA4R4G4B4)
        else if BI.RedMask = $F800 then
          Format := ifR5G6B5
        else
          // R5G5B5 is default 16bit format (with Compression = BI_RGB or masks).
          // We set it to A1.. and later there is a check if there are any alpha values
          // and if not it is changed to X1R5G5B5
          Format := ifA1R5G5B5;
      24: Format := ifR8G8B8;
      32: Format := ifA8R8G8B8; // As with R5G5B5 there is alpha check later 
    end;

    NewImage(BI.Width, Abs(BI.Height), Format, Images[0]);
    Info := GetFormatInfo(Format);
    WidthBytes := Width * Info.BytesPerPixel;
    AlignedWidthBytes := (((Width * BI.BitCount) + 31) shr 5) * 4;
    AlignedSize := Height * LongInt(AlignedWidthBytes);

    // Palette settings and reading
    if BI.BitCount <= 8 then
    begin
      // Seek to the beginning of palette
      Seek(Handle, StartPos + SizeOf(TBitmapFileHeader) + LongInt(BI.Size),
        smFromBeginning);
      if IsOS2 then
      begin
        // OS/2 type
        FPalSize := 1 shl BI.BitCount;
        GetMem(PalRGB, FPalSize * SizeOf(TColor24Rec));
        try
          Read(Handle, PalRGB, FPalSize * SizeOf(TColor24Rec));
          for I := 0 to FPalSize - 1 do
          with PalRGB[I] do
          begin
            Palette[I].R := R;
            Palette[I].G := G;
            Palette[I].B := B;
          end;
        finally
          FreeMemNil(PalRGB);
        end;
      end
      else
      begin
        // Windows type
        FPalSize := BI.ClrUsed;
        if FPalSize = 0 then
          FPalSize := 1 shl BI.BitCount;
        Read(Handle, Palette, FPalSize * SizeOf(TColor32Rec));
      end;
      for I := 0 to Info.PaletteEntries - 1 do
        Palette[I].A := $FF;
    end;

    // Seek to the beginning of image bits
    Seek(Handle, StartPos + LongInt(BF.Offset), smFromBeginning);

    case BI.Compression of
      BI_RGB: LoadRGB;
      BI_RLE4: LoadRLE4;
      BI_RLE8: LoadRLE8;
      BI_BITFIELDS: LoadRGB;
    end;

    if BI.AlphaMask = 0 then
    begin
      // Alpha mask is not stored in file (V3) or not defined.
      // Check alpha channels of loaded images if they might contain them.
      if Format = ifA1R5G5B5 then
      begin
        // Check if there is alpha channel present in A1R5GB5 images, if it is not
        // change format to X1R5G5B5
        if not Has16BitImageAlpha(Width * Height, Bits) then
          Format := ifX1R5G5B5;
      end
      else if Format = ifA8R8G8B8 then
      begin
        // Check if there is alpha channel present in A8R8G8B8 images, if it is not
        // change format to X8R8G8B8
        if not Has32BitImageAlpha(Width * Height, Bits) then
          Format := ifX8R8G8B8;
      end;
    end;

    if BI.BitCount < 8 then
    begin
      // 1 and 4 bpp images are supported only for loading which is now
      // so we now convert them to 8bpp (and unalign scanlines).
      case BI.BitCount of
        1: Convert1To8(Data, Bits, Width, Height, AlignedWidthBytes, False);
        4:
          begin
            // RLE4 bitmaps are translated to 8bit during RLE decoding
            if BI.Compression <> BI_RLE4 then
               Convert4To8(Data, Bits, Width, Height, AlignedWidthBytes, False);
          end;
      end;
      // Enlarge palette
      ReallocMem(Palette, Info.PaletteEntries * SizeOf(TColor32Rec));
    end;

    Result := True;
  finally
    FreeMemNil(Data);
  end;
end;

function TBitmapFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  StartPos, EndPos, I, Pad, PadSize, WidthBytes: LongInt;
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
  Info: TImageFormatInfo;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;

  procedure SaveRLE8;
  const
    BufferSize = 8 * 1024;
  var
    X, Y, I, SrcPos: LongInt;
    DiffCount, SameCount: Byte;
    Pixels: PByteArray;
    Buffer: array[0..BufferSize - 1] of Byte;
    BufferPos: LongInt;

    procedure WriteByte(ByteToWrite: Byte);
    begin
      if BufferPos = BufferSize then
      begin
        // Flush buffer if necessary
        GetIO.Write(Handle, @Buffer, BufferPos);
        BufferPos := 0;
      end;
      Buffer[BufferPos] := ByteToWrite;
      Inc(BufferPos);
    end;

  begin
    BufferPos := 0;
    with GetIO, ImageToSave do
    begin
      for Y := Height - 1 downto 0 do
      begin
        X := 0;
        SrcPos := 0;
        Pixels := @PByteArray(Bits)[Y * Width];

        while X < Width do
        begin
          SameCount := 1;
          DiffCount := 0;
          // Determine run length
          while X + SameCount < Width do
          begin
            // If we reach max run length or byte with different value
            // we end this run
            if (SameCount = 255) or (Pixels[SrcPos + SameCount] <> Pixels[SrcPos]) then
              Break;
            Inc(SameCount);
          end;

          if SameCount = 1 then
          begin
            // If there are not some bytes with the same value we
            // compute how many different bytes are there
            while X + DiffCount < Width do
            begin
              // Stop diff byte counting if there two bytes with the same value
              // or DiffCount is too big
              if (DiffCount = 255) or (Pixels[SrcPos + DiffCount + 1] =
                Pixels[SrcPos + DiffCount]) then
                Break;
              Inc(DiffCount);
            end;
          end;

          // Now store absolute data (direct copy image->file) or
          // store RLE code only (number of repeats + byte to be repeated)
          if DiffCount > 2 then
          begin
            // Save 'Absolute Data' (0 + number of bytes) but only
            // if number is >2 because (0+1) and (0+2) are other special commands
            WriteByte(0);
            WriteByte(DiffCount);
            // Write absolute data to buffer
            for I := 0 to DiffCount - 1 do
              WriteByte(Pixels[SrcPos + I]);
            Inc(X, DiffCount);
            Inc(SrcPos, DiffCount);
            // Odd number of bytes must be padded
            if (DiffCount mod 2) = 1 then
              WriteByte(0);
          end
          else
          begin
            // Save number of repeats and byte that should be repeated
            WriteByte(SameCount);
            WriteByte(Pixels[SrcPos]);
            Inc(X, SameCount);
            Inc(SrcPos, SameCount);
          end;
        end;
        // Save 'End Of Line' command
        WriteByte(0);
        WriteByte(0);
      end;
      // Save 'End Of Bitmap' command
      WriteByte(0);
      WriteByte(1);
      // Flush buffer
      GetIO.Write(Handle, @Buffer, BufferPos);
    end;
  end;

begin
  Result := False;
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    Info := GetFormatInfo(Format);
    StartPos := Tell(Handle);
    FillChar(BF, SizeOf(BF), 0);
    FillChar(BI, SizeOf(BI), 0);
    // Other fields will be filled later - we don't know all values now
    BF.ID := BMMagic;
    Write(Handle, @BF, SizeOf(BF));
    if Info.HasAlphaChannel and (Info.BytesPerPixel = 2){V4 temp hack} then
      // Save images with alpha in V4 format
      BI.Size := V4InfoHeaderSize
    else
      // Save images without alpha in V3 format - for better compatibility
      BI.Size := V3InfoHeaderSize;
    BI.Width := Width;
    BI.Height := Height;
    BI.Planes := 1;
    BI.BitCount := Info.BytesPerPixel * 8;
    BI.XPelsPerMeter := 2835; // 72 dpi
    BI.YPelsPerMeter := 2835; // 72 dpi
    // Set compression
    if (Info.BytesPerPixel = 1) and FUseRLE then
      BI.Compression := BI_RLE8
    else if (Info.HasAlphaChannel or
      ((BI.BitCount = 16) and (Format <> ifX1R5G5B5))) and (Info.BytesPerPixel = 2){V4 temp hack} then
      BI.Compression := BI_BITFIELDS
    else
      BI.Compression := BI_RGB;
    // Write header (first time)
    Write(Handle, @BI, BI.Size);

    // Write mask info
    if BI.Compression = BI_BITFIELDS then
    begin
      if BI.BitCount = 16 then
      with Info.PixelFormat^ do
      begin
        BI.RedMask   := RBitMask;
        BI.GreenMask := GBitMask;
        BI.BlueMask  := BBitMask;
        BI.AlphaMask := ABitMask;
      end
      else
      begin
        // Set masks for A8R8G8B8
        BI.RedMask   := $00FF0000;
        BI.GreenMask := $0000FF00;
        BI.BlueMask  := $000000FF;
        BI.AlphaMask := $FF000000;
      end;
      // If V3 header is used RGB masks must be written to file separately.
      // V4 header has embedded masks (V4 is default for formats with alpha).
      if BI.Size = V3InfoHeaderSize then
        Write(Handle, @BI.RedMask, SizeOf(BI.RedMask) * 3);
    end;
    // Write palette
    if Palette <> nil then
      Write(Handle, Palette, Info.PaletteEntries * SizeOf(TColor32Rec));

    BF.Offset := Tell(Handle) - StartPos;

    if BI.Compression <> BI_RLE8 then
    begin
      // Save uncompressed data, scanlines must be filled with pad bytes
      // to be multiples of 4, save as bottom-up (Windows native) bitmap
      Pad := 0;
      WidthBytes := Width * Info.BytesPerPixel;
      PadSize := ((Width * BI.BitCount + 31) div 32) * 4 - WidthBytes;

      for I := Height - 1 downto 0 do
      begin
        Write(Handle, @PByteArray(Bits)[I * WidthBytes], WidthBytes);
        if PadSize > 0 then
          Write(Handle, @Pad, PadSize);
      end;
    end
    else
    begin
      // Save data with RLE8 compression
      SaveRLE8;
    end;

    EndPos := Tell(Handle);
    Seek(Handle, StartPos, smFromBeginning);
    // Rewrite header with new values
    BF.Size := EndPos - StartPos;
    BI.SizeImage := BF.Size - BF.Offset;
    Write(Handle, @BF, SizeOf(BF));
    Write(Handle, @BI, BI.Size);
    Seek(Handle, EndPos, smFromBeginning);

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TBitmapFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
    // Convert FP image to RGB/ARGB according to presence of alpha channel
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8)
  else if Info.HasGrayChannel or Info.IsIndexed then
    // Convert all grayscale and indexed images to Index8 unless they have alpha
    // (preserve it)
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifIndex8)
  else if Info.HasAlphaChannel then
    // Convert images with alpha channel to A8R8G8B8
    ConvFormat := ifA8R8G8B8
  else if Info.UsePixelFormat then
    // Convert 16bit RGB images (no alpha) to X1R5G5B5
    ConvFormat := ifX1R5G5B5
  else
    // Convert all other formats to R8G8B8
    ConvFormat := ifR8G8B8;

  ConvertImage(Image, ConvFormat);
end;

function TBitmapFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Hdr: TBitmapFileHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    ReadCount := Read(Handle, @Hdr, SizeOf(Hdr));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (Hdr.ID = BMMagic) and (ReadCount = SizeOf(Hdr));
  end;
end;

initialization
  RegisterImageFileFormat(TBitmapFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now
    - Add option to choose to save V3 or V4 headers. 

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Fixed problem with indexed BMP loading - some pal entries
      could end up with alpha=0. 

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Now saves bitmaps as bottom-up for better compatibility
      (mainly Lazarus' TImage!).
    - Fixed crash when loading bitmaps with headers larger than V4.
    - Temp hacks to disable V4 headers for 32bit images (compatibility with
      other soft).

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Removed temporary data allocation for image with aligned scanlines.
      They are now directly written to output so memory requirements are
      much lower now.
    - Now uses and recognizes BITMAPINFOHEADERV4 when loading/saving.
      Mainly for formats with alpha channels.
    - Added ifR5G6B5 to supported formats, changed converting to supported
      formats little bit.
    - Rewritten SaveRLE8 nested procedure. Old code was long and
      mysterious - new is short and much more readable.
    - MakeCompatible method moved to base class, put ConvertToSupported here.
      GetSupportedFormats removed, it is now set in constructor.
    - Rewritten LoadRLE4 and LoadRLE8 nested procedures.
      Should be less buggy an more readable (load inspired by Colosseum Builders' code).
    - Made public properties for options registered to SetOption/GetOption
      functions. 
    - Added alpha check to 32b bitmap loading too (teh same as in 16b
      bitmap loading).
    - Moved Convert1To8 and Convert4To8 to ImagingFormats
    - Changed extensions to filename masks.
    - Changed SaveData, LoadData, and MakeCompatible methods according
      to changes in base class in Imaging unit.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - fixed wrong const that caused A4R4G4B4 BMPs to load as A1R5G5B5
    - fixed the bug that caused 8bit RLE compressed bitmaps to load as
      whole black

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - 16 bit images are usually without alpha but some has alpha
      channel and there is no indication of it - so I have added
      a check: if all pixels of image are with alpha = 0 image is treated
      as X1R5G5B5 otherwise as A1R5G5B5

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - when loading 1/4 bit images with dword aligned dimensions
      there was ugly memory rewriting bug causing image corruption

}

end.

