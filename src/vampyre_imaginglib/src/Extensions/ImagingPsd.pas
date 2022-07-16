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

{ This unit contains image format loader/saver for Photoshop PSD image format.}
unit ImagingPsd;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingColors, ImagingUtility;

type
  { Class for loading and saving Adobe Photoshop PSD images.
    Loading and saving of indexed, grayscale, RGB(A), HDR (FP32), and CMYK
    (auto converted to RGB) images is supported. Non-HDR gray, RGB,
    and CMYK images can have 8bit or 16bit color channels.
    There is no support for loading mono images, duotone images are treated
    like grayscale images, and multichannel and CIE Lab images are loaded as
    RGB images but without actual conversion to RGB color space.
    Also no layer information is loaded.}
  TPSDFileFormat = class(TImageFileFormat)
  private
    FSaveAsLayer: LongBool;
  protected
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
    property SaveAsLayer: LongBool read FSaveAsLayer write FSaveAsLayer;
  end;

implementation

uses
  ImagingExtFileFormats;

const
  SPSDFormatName = 'Photoshop Image';
  SPSDMasks      = '*.psd,*.pdd';
  PSDSupportedFormats: TImageFormats = [ifIndex8, ifGray8, ifA8Gray8,
    ifR8G8B8, ifA8R8G8B8, ifGray16, ifA16Gray16, ifR16G16B16, ifA16R16G16B16,
    ifR32F, ifR32G32B32F, ifA32R32G32B32F];
  PSDDefaultSaveAsLayer = True;

const
  SPSDMagic = '8BPS';
  CompressionNone: Word = 0;
  CompressionRLE: Word  = 1;

type
  {$MINENUMSIZE 2}
  { PSD Image color mode.}
  TPSDColorMode = (
    cmMono         = 0,
    cmGrayscale    = 1,
    cmIndexed      = 2,
    cmRGB          = 3,
    cmCMYK         = 4,
    cmMultiChannel = 7,
    cmDuoTone      = 8,
    cmLab          = 9
  );

  { PSD image main header.}
  TPSDHeader = packed record
    Signature: TChar4;             // Format ID '8BPS'
    Version: Word;                 // Always 1
    Reserved: array[0..5] of Byte; // Reserved, all zero
    Channels: Word;                // Number of color channels (1-24) including alpha channels
    Rows : UInt32;                 // Height of image in pixels (1-30000)
    Columns: UInt32;               // Width of image in pixels (1-30000)
    Depth: Word;                   // Number of bits per channel (1, 8, and 16)
    Mode: TPSDColorMode;           // Color mode
  end;

  TPSDChannelInfo = packed record
    ChannelID: Word;               // 0 = Red, 1 = Green, 2 = Blue etc., -1 = Transparency mask, -2 = User mask
    Size: UInt32;                  // Size of channel data.
  end;

procedure SwapHeader(var Header: TPSDHeader);
begin
  Header.Version := SwapEndianWord(Header.Version);
  Header.Channels := SwapEndianWord(Header.Channels);
  Header.Depth := SwapEndianWord(Header.Depth);
  Header.Rows := SwapEndianUInt32(Header.Rows);
  Header.Columns := SwapEndianUInt32(Header.Columns);
  Header.Mode := TPSDColorMode(SwapEndianWord(Word(Header.Mode)));
end; 

{
  TPSDFileFormat class implementation
}

procedure TPSDFileFormat.Define;
begin
  inherited;
  FName := SPSDFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := PSDSupportedFormats;
  AddMasks(SPSDMasks);

  FSaveAsLayer := PSDDefaultSaveAsLayer;
  RegisterOption(ImagingPSDSaveAsLayer, @FSaveAsLayer);
end;

function TPSDFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Header: TPSDHeader;
  ByteCount: UInt32;
  RawPal: array[0..767] of Byte;
  Compression, PackedSize: Word;
  LineSize, ChannelPixelSize, WidthBytes,
    CurrChannel, MaxRLESize, I, Y, X: LongInt;
  Info: TImageFormatInfo;
  PackedLine, LineBuffer: PByte;
  RLELineSizes: array of Word;
  Col32: TColor32Rec;
  Col64: TColor64Rec;
  PCol32: PColor32Rec;
  PCol64: PColor64Rec;

  { PackBits RLE decode code from Mike Lischke's GraphicEx library.}
  procedure DecodeRLE(Source, Dest: PByte; PackedSize, UnpackedSize: LongInt);
  var
    Count: LongInt;
  begin
    while (UnpackedSize > 0) and (PackedSize > 0) do
    begin
      Count := ShortInt(Source^);
      Inc(Source);
      Dec(PackedSize);
      if Count < 0 then
      begin
        // Replicate next byte -Count + 1 times
        if Count = -128 then
          Continue;
        Count := -Count + 1;
        if Count > UnpackedSize then
          Count := UnpackedSize;
        FillChar(Dest^, Count, Source^);
        Inc(Source);
        Dec(PackedSize);
        Inc(Dest, Count);
        Dec(UnpackedSize, Count);
      end
      else
      begin
        // Copy next Count + 1 bytes from input
        Inc(Count);
        if Count > UnpackedSize then
          Count := UnpackedSize;
        if Count > PackedSize then
          Count := PackedSize;
        Move(Source^, Dest^, Count);
        Inc(Dest, Count);
        Inc(Source, Count);
        Dec(PackedSize, Count);
        Dec(UnpackedSize, Count);
      end;
    end;
  end;

begin
  Result := False;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Read PSD header
    Read(Handle, @Header, SizeOf(Header));
    SwapHeader(Header);

    // Determine image data format
    Format := ifUnknown;
    case Header.Mode of
      cmGrayscale, cmDuoTone:
        begin
          if Header.Depth in [8, 16] then
          begin
            if Header.Channels = 1 then
              Format := IffFormat(Header.Depth = 8, ifGray8, ifGray16)
            else if Header.Channels >= 2 then
              Format := IffFormat(Header.Depth = 8, ifA8Gray8, ifA16Gray16);
          end
          else if (Header.Depth = 32) and (Header.Channels = 1) then
            Format := ifR32F;
        end;
      cmIndexed:
        begin
          if Header.Depth = 8 then
            Format := ifIndex8;
        end;
      cmRGB, cmMultiChannel, cmCMYK, cmLab:
        begin
          if Header.Depth in [8, 16] then
          begin
            if Header.Channels = 3 then
              Format := IffFormat(Header.Depth = 8, ifR8G8B8, ifR16G16B16)
            else if Header.Channels >= 4 then
              Format := IffFormat(Header.Depth = 8, ifA8R8G8B8, ifA16R16G16B16);
          end
          else if Header.Depth = 32 then
          begin
            if Header.Channels = 3 then
              Format := ifR32G32B32F
            else if Header.Channels >= 4 then
              Format := ifA32R32G32B32F;
          end;
        end;
      cmMono:; // Not supported
    end;

    // Exit if no compatible format was found
    if Format = ifUnknown then
      Exit;

    NewImage(Header.Columns, Header.Rows, Format, Images[0]);
    Info := GetFormatInfo(Format);

    // Read or skip Color Mode Data Block (palette)
    Read(Handle, @ByteCount, SizeOf(ByteCount));
    ByteCount := SwapEndianUInt32(ByteCount);
    if Format = ifIndex8 then
    begin
      // Read palette only for indexed images
      Read(Handle, @RawPal, SizeOf(RawPal));
      for I := 0 to 255 do
      begin
        Palette[I].A := $FF;
        Palette[I].R := RawPal[I + 0];
        Palette[I].G := RawPal[I + 256];
        Palette[I].B := RawPal[I + 512];
      end;
    end
    else
      Seek(Handle, ByteCount, smFromCurrent);

    // Skip Image Resources Block
    Read(Handle, @ByteCount, SizeOf(ByteCount));
    ByteCount := SwapEndianUInt32(ByteCount);
    Seek(Handle, ByteCount, smFromCurrent);
    // Now there is Layer and Mask Information Block
    Read(Handle, @ByteCount, SizeOf(ByteCount));
    ByteCount := SwapEndianUInt32(ByteCount);
    // Skip Layer and Mask Information Block
    Seek(Handle, ByteCount, smFromCurrent);

    // Read compression flag
    Read(Handle, @Compression, SizeOf(Compression));
    Compression := SwapEndianWord(Compression);

    if Compression = CompressionRLE then
    begin
      // RLE compressed PSDs (most) have first lengths of compressed scanlines
      // for each channel stored
      SetLength(RLELineSizes, Height * Header.Channels);
      Read(Handle, @RLELineSizes[0], Length(RLELineSizes) * SizeOf(Word));
      SwapEndianWord(@RLELineSizes[0], Height * Header.Channels);
      MaxRLESize := RLELineSizes[0];
      for I := 1 to High(RLELineSizes) do
      begin
        if MaxRLESize < RLELineSizes[I] then
          MaxRLESize := RLELineSizes[I];
      end;
    end
    else
      MaxRLESize := 0;

    ChannelPixelSize := Info.BytesPerPixel div Info.ChannelCount;
    LineSize := Width * ChannelPixelSize;
    WidthBytes := Width * Info.BytesPerPixel;
    GetMem(LineBuffer, LineSize);
    GetMem(PackedLine, MaxRLESize);

    try
      // Image color channels are stored separately in PSDs so we will load
      // one by one and copy their data to appropriate addresses of dest image.
      for I := 0 to Header.Channels - 1 do
      begin
        // Now determine to which color channel of destination image we are going
        // to write pixels.
        if I <= 4 then
        begin
          // If PSD has alpha channel we need to switch current channel order -
          // PSDs have alpha stored after blue channel but Imaging has alpha
          // before red.
          if Info.HasAlphaChannel and (Header.Mode <> cmCMYK) then
          begin
            if I = Info.ChannelCount - 1 then
              CurrChannel := I
            else
              CurrChannel := Info.ChannelCount - 2 - I;
          end
          else
            CurrChannel := Info.ChannelCount - 1 - I;
        end
        else
        begin
          // No valid channel remains
          CurrChannel := -1;
        end;

        if CurrChannel >= 0 then
        begin
          for Y := 0 to Height - 1 do
          begin
            if Compression = CompressionRLE then
            begin
              // Read RLE line and decompress it
              PackedSize := RLELineSizes[I * Height + Y];
              Read(Handle, PackedLine, PackedSize);
              DecodeRLE(PackedLine, LineBuffer, PackedSize, LineSize);
            end
            else
            begin
              // Just read uncompressed line
              Read(Handle, LineBuffer, LineSize);
            end;

            // Swap endian if needed
            if ChannelPixelSize = 4 then
              SwapEndianUInt32(PUInt32(LineBuffer), Width)
            else if ChannelPixelSize = 2 then
              SwapEndianWord(PWordArray(LineBuffer), Width);

            if Info.ChannelCount > 1 then
            begin
              // Copy each pixel fragment to its right place in destination image
              for X := 0 to Width - 1 do
              begin
                Move(PByteArray(LineBuffer)[X * ChannelPixelSize],
                  PByteArray(Bits)[Y * WidthBytes + X * Info.BytesPerPixel + CurrChannel * ChannelPixelSize],
                  ChannelPixelSize);
              end;
            end
            else
            begin
              // Just copy the line
              Move(LineBuffer^, PByteArray(Bits)[Y * LineSize], LineSize);
            end;
          end;
        end
        else
        begin
          // Skip current color channel, not needed for image loading - just to
          // get stream's position to the end of PSD
          if Compression = CompressionRLE then
          begin
            for Y := 0 to Height - 1 do
              Seek(Handle, RLELineSizes[I * Height + Y], smFromCurrent);
          end
          else
            Seek(Handle, LineSize * Height, smFromCurrent);
        end;
      end;

      if Header.Mode = cmCMYK then
      begin
        // Convert CMYK images to RGB (alpha is ignored here). PSD stores CMYK
        // channels in the way that first requires subtraction from max channel value
        if ChannelPixelSize = 1 then
        begin
          PCol32 := Bits;
          for X := 0 to Width * Height - 1 do
          begin
            Col32.A := 255 - PCol32.A;
            Col32.R := 255 - PCol32.R;
            Col32.G := 255 - PCol32.G;
            Col32.B := 255 - PCol32.B;
            CMYKToRGB(Col32.A, Col32.R, Col32.G, Col32.B, PCol32.R, PCol32.G, PCol32.B);
            PCol32.A := 255;
            Inc(PCol32);
          end;
        end
        else
        begin
          PCol64 := Bits;
          for X := 0 to Width * Height - 1 do
          begin
            Col64.A := 65535 - PCol64.A;
            Col64.R := 65535 - PCol64.R;
            Col64.G := 65535 - PCol64.G;
            Col64.B := 65535 - PCol64.B;
            CMYKToRGB16(Col64.A, Col64.R, Col64.G, Col64.B, PCol64.R, PCol64.G, PCol64.B);
            PCol64.A := 65535;
            Inc(PCol64);
          end;
        end;
      end;

      Result := True;
    finally
      FreeMem(LineBuffer);
      FreeMem(PackedLine);
    end;
  end;
end;

function TPSDFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
type
  TURect = packed record
    Top, Left, Bottom, Right: UInt32;
  end;
const
  BlendMode: TChar8 = '8BIMnorm';
  LayerOptions: array[0..3] of Byte = (255, 0, 0, 0);
  LayerName: array[0..7] of AnsiChar = #7'Layer 0';
var
  MustBeFreed: Boolean;
  ImageToSave: TImageData;
  Info: TImageFormatInfo;
  Header: TPSDHeader;
  I, CurrChannel, ChannelPixelSize: LongInt;
  LayerBlockOffset, SaveOffset, ChannelInfoOffset: Integer;
  ChannelInfo: TPSDChannelInfo;
  R: TURect;
  LongVal: UInt32;
  WordVal, LayerCount: Word;
  RawPal: array[0..767] of Byte;
  ChannelDataSizes: array of Integer;

  function PackLine(Src, Dest: PByteArray; Length: Integer): Integer;
  var
    I, Remaining: Integer;
  begin
    Remaining := Length;
    Result := 0;
    while Remaining > 0 do
    begin
      I := 0;
      // Look for characters same as the first
      while (I < 128) and (Remaining - I > 0) and (Src[0] = Src[I]) do
        Inc(I);

      if I > 2 then
      begin
        Dest[0] := Byte(-(I - 1));
        Dest[1] := Src[0];
        Dest := PByteArray(@Dest[2]);

        Src := PByteArray(@Src[I]);
        Dec(Remaining, I);
        Inc(Result, 2);
      end
      else
      begin
        // Look for different characters
        I := 0;
        while (I < 128) and (Remaining - (I + 1) > 0) and
          ((Src[I] <> Src[I + 1]) or (Remaining - (I + 2) <= 0) or
          (Src[I] <> Src[I + 2])) do
        begin
          Inc(I);
        end;
        // If there's only 1 remaining, the previous WHILE doesn't catch it
        if Remaining = 1 then
          I := 1;

        if I > 0 then
        begin
          // Some distinct ones found
          Dest[0] := I - 1;
          Move(Src[0], Dest[1], I);
          Dest := PByteArray(@Dest[1 + I]);
          Src := PByteArray(@Src[I]);
          Dec(Remaining, I);
          Inc(Result, I + 1);
        end;
      end;
    end;
  end;

  procedure WriteChannelData(SeparateChannelStorage: Boolean);
  var
    I, X, Y, LineSize, WidthBytes, RLETableOffset, CurrentOffset, WrittenLineSize: Integer;
    LineBuffer, RLEBuffer: PByteArray;
    RLELengths: array of Word;
    Compression: Word;
  begin
    LineSize := ImageToSave.Width * ChannelPixelSize;
    WidthBytes := ImageToSave.Width * Info.BytesPerPixel;
    GetMem(LineBuffer, LineSize);
    GetMem(RLEBuffer, LineSize * 3);
    SetLength(RLELengths, ImageToSave.Height * Info.ChannelCount);
    RLETableOffset := 0;
    // No compression for FP32, Photoshop won't open them
    Compression := Iff(Info.IsFloatingPoint, CompressionNone, CompressionRLE);

    if not SeparateChannelStorage then
    begin
      // This is for storing background merged image. There's only one
      // compression flag and one RLE lengths table for all channels
      WordVal := Swap(Compression);
      GetIO.Write(Handle, @WordVal, SizeOf(WordVal));
      if Compression = CompressionRLE then
      begin
        RLETableOffset := GetIO.Tell(Handle);
        GetIO.Write(Handle, @RLELengths[0], SizeOf(Word) * ImageToSave.Height * Info.ChannelCount);
      end;
    end;

    for I := 0 to Info.ChannelCount - 1 do
    begin
      if SeparateChannelStorage then
      begin
        // Layer image data has compression flag and RLE lengths table
        // independent for each channel
        WordVal := Swap(CompressionRLE);
        GetIO.Write(Handle, @WordVal, SizeOf(WordVal));
        if Compression = CompressionRLE then
        begin
          RLETableOffset := GetIO.Tell(Handle);
          GetIO.Write(Handle, @RLELengths[0], SizeOf(Word) * ImageToSave.Height);
          ChannelDataSizes[I] := 0;
        end;
      end;

      // Now determine which color channel we are going to write to file.
      if Info.HasAlphaChannel then
      begin
        if I = Info.ChannelCount - 1 then
          CurrChannel := I
        else
          CurrChannel := Info.ChannelCount - 2 - I;
      end
      else
        CurrChannel := Info.ChannelCount - 1 - I;

      for Y := 0 to ImageToSave.Height - 1 do
      begin
        if Info.ChannelCount > 1 then
        begin
          // Copy each pixel fragment to its right place in destination image
          for X := 0 to ImageToSave.Width - 1 do
          begin
            Move(PByteArray(ImageToSave.Bits)[Y * WidthBytes + X * Info.BytesPerPixel + CurrChannel * ChannelPixelSize],
              PByteArray(LineBuffer)[X * ChannelPixelSize], ChannelPixelSize);
          end;
        end
        else
          Move(PByteArray(ImageToSave.Bits)[Y * LineSize], LineBuffer^, LineSize);

        // Write current channel line to file (swap endian if needed first)
        if ChannelPixelSize = 4 then
          SwapEndianUInt32(PUInt32(LineBuffer), ImageToSave.Width)
        else if ChannelPixelSize = 2 then
          SwapEndianWord(PWordArray(LineBuffer), ImageToSave.Width);

        if Compression = CompressionRLE then
        begin
          // Compress and write line
          WrittenLineSize := PackLine(LineBuffer, RLEBuffer, LineSize);
          RLELengths[ImageToSave.Height * I + Y] := SwapEndianWord(WrittenLineSize);
          GetIO.Write(Handle, RLEBuffer, WrittenLineSize);
        end
        else
        begin
          WrittenLineSize := LineSize;
          GetIO.Write(Handle, LineBuffer, WrittenLineSize);
        end;

        if SeparateChannelStorage then
          Inc(ChannelDataSizes[I], WrittenLineSize);
      end;

      if SeparateChannelStorage and (Compression = CompressionRLE) then
      begin
        // Update channel RLE lengths
        CurrentOffset := GetIO.Tell(Handle);
        GetIO.Seek(Handle, RLETableOffset, smFromBeginning);
        GetIO.Write(Handle, @RLELengths[ImageToSave.Height * I], SizeOf(Word) * ImageToSave.Height);
        GetIO.Seek(Handle, CurrentOffset, smFromBeginning);
        Inc(ChannelDataSizes[I], SizeOf(Word) * ImageToSave.Height);
      end;
    end;

    if not SeparateChannelStorage and (Compression = CompressionRLE) then
    begin
      // Update channel RLE lengths
      CurrentOffset := GetIO.Tell(Handle);
      GetIO.Seek(Handle, RLETableOffset, smFromBeginning);
      GetIO.Write(Handle, @RLELengths[0], SizeOf(Word) * ImageToSave.Height * Info.ChannelCount);
      GetIO.Seek(Handle, CurrentOffset, smFromBeginning);
    end;

    FreeMem(LineBuffer);
    FreeMem(RLEBuffer);
  end;

begin
  Result := False;
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    Info := GetFormatInfo(Format);
    ChannelPixelSize := Info.BytesPerPixel div Info.ChannelCount;

    // Fill header with proper info and save it
    FillChar(Header, SizeOf(Header), 0);
    Header.Signature := SPSDMagic;
    Header.Version := 1;
    Header.Channels := Info.ChannelCount;
    Header.Rows := Height;
    Header.Columns := Width;
    Header.Depth := Info.BytesPerPixel div Info.ChannelCount * 8;
    if Info.IsIndexed then
      Header.Mode := cmIndexed
    else if Info.HasGrayChannel or (Info.ChannelCount = 1) then
      Header.Mode := cmGrayscale
    else
      Header.Mode := cmRGB;

    SwapHeader(Header);
    Write(Handle, @Header, SizeOf(Header));

    // Write palette size and data
    LongVal := SwapEndianUInt32(IffUnsigned(Info.IsIndexed, SizeOf(RawPal), 0));
    Write(Handle, @LongVal, SizeOf(LongVal));
    if Info.IsIndexed then
    begin
      for I := 0 to Info.PaletteEntries - 1 do
      begin
        RawPal[I] := Palette[I].R;
        RawPal[I + 256] := Palette[I].G;
        RawPal[I + 512] := Palette[I].B;
      end;
      Write(Handle, @RawPal, SizeOf(RawPal));
    end;

    // Write empty resource and layer block sizes
    LongVal := 0;
    Write(Handle, @LongVal, SizeOf(LongVal));
    LayerBlockOffset := Tell(Handle);
    Write(Handle, @LongVal, SizeOf(LongVal));

    if FSaveAsLayer and (ChannelPixelSize < 4) then  // No Layers for FP32 images
    begin
      LayerCount := SwapEndianWord(Iff(Info.HasAlphaChannel, Word(-1), 1)); // Must be -1 to get transparency in Photoshop
      R.Top := 0;
      R.Left := 0;
      R.Bottom := SwapEndianUInt32(Height);
      R.Right := SwapEndianUInt32(Width);
      WordVal := SwapEndianWord(Info.ChannelCount);
      Write(Handle, @LongVal, SizeOf(LongVal));        // Layer section size, empty now
      Write(Handle, @LayerCount, SizeOf(LayerCount));  // Layer count
      Write(Handle, @R, SizeOf(R));                    // Bounds rect
      Write(Handle, @WordVal, SizeOf(WordVal));        // Channel count

      ChannelInfoOffset := Tell(Handle);
      SetLength(ChannelDataSizes, Info.ChannelCount);  // Empty channel infos
      FillChar(ChannelInfo, SizeOf(ChannelInfo), 0);
      for I := 0 to Info.ChannelCount - 1 do
        Write(Handle, @ChannelInfo, SizeOf(ChannelInfo));

      Write(Handle, @BlendMode, SizeOf(BlendMode));    // Blend mode = normal
      Write(Handle, @LayerOptions, SizeOf(LayerOptions)); // Predefined options
      LongVal := SwapEndianUInt32(16);               // Extra data size (4 (mask size) + 4 (ranges size) + 8 (name))
      Write(Handle, @LongVal, SizeOf(LongVal));
      LongVal := 0;
      Write(Handle, @LongVal, SizeOf(LongVal));        // Mask size = 0
      LongVal := 0;
      Write(Handle, @LongVal, SizeOf(LongVal));        // Blend ranges size
      Write(Handle, @LayerName, SizeOf(LayerName));    // Layer name

      WriteChannelData(True);                          // Write Layer image data

      Write(Handle, @LongVal, SizeOf(LongVal));        // Global mask info size = 0

      SaveOffset := Tell(Handle);
      Seek(Handle, LayerBlockOffset, smFromBeginning);

      // Update layer and mask section sizes
      LongVal := SwapEndianUInt32(SaveOffset - LayerBlockOffset - 4);
      Write(Handle, @LongVal, SizeOf(LongVal));
      LongVal := SwapEndianUInt32(SaveOffset - LayerBlockOffset - 8);
      Write(Handle, @LongVal, SizeOf(LongVal));

      // Update layer channel info
      Seek(Handle, ChannelInfoOffset, smFromBeginning);
      for I := 0 to Info.ChannelCount - 1 do
      begin
        ChannelInfo.ChannelID := SwapEndianWord(I);
        if (I = Info.ChannelCount - 1) and Info.HasAlphaChannel then
          ChannelInfo.ChannelID := Swap(Word(-1));
        ChannelInfo.Size := SwapEndianUInt32(ChannelDataSizes[I] + 2); // data size (incl RLE table) + comp. flag
        Write(Handle, @ChannelInfo, SizeOf(ChannelInfo));
      end;

      Seek(Handle, SaveOffset, smFromBeginning);
    end;

    // Write background merged image
    WriteChannelData(False);

    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TPSDFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
  begin
    if Info.ChannelCount = 1 then
      ConvFormat := ifR32F
    else if Info.HasAlphaChannel then
      ConvFormat := ifA32R32G32B32F
    else
      ConvFormat := ifR32G32B32F;
  end
  else if Info.HasGrayChannel then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16Gray16, ifGray16)
  else if Info.RBSwapFormat in GetSupportedFormats then
    ConvFormat := Info.RBSwapFormat
  else
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8);

  ConvertImage(Image, ConvFormat);
end;

function TPSDFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Header: TPSDHeader;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Header, SizeOf(Header));
    SwapHeader(Header);
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount >= SizeOf(Header)) and
      (Header.Signature = SPSDMagic) and
      (Header.Version = 1);
  end;
end;

initialization
  RegisterImageFileFormat(TPSDFileFormat);

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - 3 channel RGB float images are loaded and saved directly
      as ifR32G32B32F.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - PSDs are now saved with RLE compression.
    - Mask layer saving added to SaveData for images with alpha
      (shows proper transparency when opened in Photoshop). Can be
      enabled/disabled using option 
    - Fixed memory leak in SaveData.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Saving implemented.
    - Loading implemented.
    - Unit created with initial stuff!
}

end.

