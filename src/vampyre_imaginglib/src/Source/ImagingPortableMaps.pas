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

{ This unit contains loader/saver for Portable Maps file format family (or PNM).
  That includes PBM, PGM, PPM, PAM, and PFM formats.}
unit ImagingPortableMaps;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

type
  { Types of pixels of PNM images.}
  TTupleType = (ttInvalid, ttBlackAndWhite, ttGrayScale, ttRGB, ttBlackAndWhiteAlpha,
    ttGrayScaleAlpha, ttRGBAlpha, ttGrayScaleFP, ttRGBFP);

  { Record with info about PNM image used in both loading and saving functions.}
  TPortableMapInfo = record
    Width: LongInt;
    Height: LongInt;
    FormatId: AnsiChar;
    MaxVal: LongInt;
    BitCount: LongInt;
    Depth: LongInt;
    TupleType: TTupleType;
    Binary: Boolean;
    HasPAMHeader: Boolean;
    IsBigEndian: Boolean;
  end;

  { Base class for Portable Map file formats (or Portable AnyMaps or PNM).
    There are several types of PNM file formats that share common
    (simple) structure. This class can actually load all supported PNM formats.
    Saving is also done by this class but descendants (each for different PNM
    format) control it.}
  TPortableMapFileFormat = class(TImageFileFormat)
  protected
    FIdNumbers: TChar2;
    FSaveBinary: LongBool;
    FUSFormat: TFormatSettings;
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveDataInternal(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt; var MapInfo: TPortableMapInfo): Boolean;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  published  
    { If set to True images will be saved in binary format. If it is False
      they will be saved in text format (which could result in 5-10x bigger file).
      Default is value True. Note that PAM and PFM files are always saved in binary.}
    property SaveBinary: LongBool read FSaveBinary write FSaveBinary;
  end;

  { Portable Bit Map is used to store monochrome 1bit images. Raster data
    can be saved as text or binary data. Either way value of 0 represents white
    and 1 is black. As Imaging does not have support for 1bit data formats
    PBM images can be loaded but not saved. Loaded images are returned in
    ifGray8 format (witch pixel values scaled from 1bit to 8bit).}
  TPBMFileFormat = class(TPortableMapFileFormat)
  protected
    procedure Define; override;
  end;

  { Portable Gray Map is used to store grayscale 8bit or 16bit images.
    Raster data can be saved as text or binary data.}
  TPGMFileFormat = class(TPortableMapFileFormat)
  protected
    procedure Define; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  end;

  { Portable Pixel Map is used to store RGB images with 8bit or 16bit channels.
    Raster data can be saved as text or binary data.}
  TPPMFileFormat = class(TPortableMapFileFormat)
  protected
    procedure Define; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  end;

  { Portable Arbitrary Map is format that can store image data formats
    of PBM, PGM, and PPM formats with optional alpha channel. Raster data
    can be stored only in binary format. All data formats supported
    by this format are ifGray8, ifGray16, ifA8Gray8, ifA16Gray16,
    ifR8G8B8, ifR16G16R16, ifA8R8G8B8, and ifA16R16G16B16.}
  TPAMFileFormat = class(TPortableMapFileFormat)
  protected
    procedure Define; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  end;

  { Portable Float Map is unofficial extension of PNM format family which
    can store images with floating point pixels. Raster data is saved in
    binary format as array of IEEE 32 bit floating point numbers. One channel
    or RGB images are supported by PFM format (so no alpha).}
  TPFMFileFormat = class(TPortableMapFileFormat)
  protected
    procedure Define; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  end;

implementation

const
  PortableMapDefaultBinary = True;

  SPBMFormatName = 'Portable Bit Map';
  SPBMMasks =      '*.pbm';
  SPGMFormatName = 'Portable Gray Map';
  SPGMMasks =      '*.pgm';
  PGMSupportedFormats = [ifGray8, ifGray16];
  SPPMFormatName = 'Portable Pixel Map';
  SPPMMasks =      '*.ppm';
  PPMSupportedFormats = [ifR8G8B8, ifR16G16B16];
  SPAMFormatName = 'Portable Arbitrary Map';
  SPAMMasks =      '*.pam';
  PAMSupportedFormats = [ifGray8, ifGray16, ifA8Gray8, ifA16Gray16,
    ifR8G8B8, ifR16G16B16, ifA8R8G8B8, ifA16R16G16B16];
  SPFMFormatName = 'Portable Float Map';
  SPFMMasks =      '*.pfm';
  PFMSupportedFormats = [ifR32F, ifB32G32R32F];

const
  { TAB, CR, LF, and Space are used as seperators in Portable map headers and data.}
  WhiteSpaces = [#9, #10, #13, #32];
  SPAMWidth = 'WIDTH';
  SPAMHeight = 'HEIGHT';
  SPAMDepth = 'DEPTH';
  SPAMMaxVal = 'MAXVAL';
  SPAMTupleType = 'TUPLTYPE';
  SPAMEndHdr = 'ENDHDR';

  { Size of buffer used to speed up text PNM loading/saving.}
  LineBufferCapacity = 16 * 1024;

  TupleTypeNames: array[TTupleType] of string = (
    'INVALID', 'BLACKANDWHITE', 'GRAYSCALE', 'RGB',
    'BLACKANDWHITE_ALPHA', 'GRAYSCALE_ALPHA', 'RGB_ALPHA', 'GRAYSCALEFP',
    'RGBFP');

{ TPortableMapFileFormat }

procedure TPortableMapFileFormat.Define;
begin
  inherited;
  FFeatures := [ffLoad, ffSave];
  FSaveBinary := PortableMapDefaultBinary;
  FUSFormat := GetFormatSettingsForFloats;
end;

function TPortableMapFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  I, ScanLineSize, MonoSize: LongInt;
  Dest: PByte;
  MonoData: Pointer;
  Info: TImageFormatInfo;
  LineBuffer: array[0..LineBufferCapacity - 1] of AnsiChar;
  LineEnd, LinePos: LongInt;
  MapInfo: TPortableMapInfo;
  LineBreak: string;

  procedure CheckBuffer;
  begin
    if (LineEnd = 0) or (LinePos = LineEnd) then
    begin
      // Reload buffer if its is empty or its end was reached
      LineEnd := GetIO.Read(Handle, @LineBuffer[0], LineBufferCapacity);
      LinePos := 0;
    end;
  end;

  procedure FixInputPos;
  begin
    // Sets input's position to its real pos as it would be without buffering
    if LineEnd > 0 then
    begin
      GetIO.Seek(Handle, -LineEnd + LinePos, smFromCurrent);
      LineEnd := 0;
    end;
  end;

  function ReadString: string;
  var
    S: AnsiString;
    C: AnsiChar;
  begin
    // First skip all whitespace chars
    SetLength(S, 1);
    repeat
      CheckBuffer;
      S[1] := LineBuffer[LinePos];
      Inc(LinePos);
      if S[1] = '#' then
      repeat
        // Comment detected, skip everything until next line is reached
        CheckBuffer;
        S[1] := LineBuffer[LinePos];
        Inc(LinePos);
      until S[1] = #10;
    until not(S[1] in WhiteSpaces);
    // Now we have reached some chars other than white space, read them until
    // there is whitespace again
    repeat
      SetLength(S, Length(S) + 1);
      CheckBuffer;
      S[Length(S)] := LineBuffer[LinePos];
      Inc(LinePos);
      // Repeat until current char is whitespace or end of file is reached
      // (Line buffer has 0 bytes which happens only on EOF)
    until (S[Length(S)] in WhiteSpaces) or (LineEnd = 0);
    // Get rid of last char - whitespace or null
    SetLength(S, Length(S) - 1);
    // Move position to the beginning of next string (skip white space - needed
    // to make the loader stop at the right input position)
    repeat
      CheckBuffer;
      C := LineBuffer[LinePos];
      Inc(LinePos);
    until not (C in WhiteSpaces) or (LineEnd = 0);
    // Dec pos, current is the beginning of the the string
    Dec(LinePos);

    Result := string(S);
  end;

  function ReadIntValue: LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Result := StrToInt(ReadString);
  end;

  procedure FindLineBreak;
  var
    C: AnsiChar;
  begin
    LineBreak := #10;
    repeat
      CheckBuffer;
      C := LineBuffer[LinePos];
      Inc(LinePos);

      if C = #13 then
        LineBreak := #13#10;

    until C = #10;
  end;

  function ParseHeader: Boolean;
  var
    Id: TChar2;
    I: TTupleType;
    TupleTypeName: string;
    Scale: Single;
  begin
    Result := False;
    with GetIO do
    begin
      FillChar(MapInfo, SizeOf(MapInfo), 0);
      Read(Handle, @Id, SizeOf(Id));
      FindLineBreak;

      if Id[1] in ['1'..'6'] then
      begin
        // Read header for PBM, PGM, and PPM files
        MapInfo.Width := ReadIntValue;
        MapInfo.Height := ReadIntValue;

        if Id[1] in ['1', '4'] then
        begin
          MapInfo.MaxVal := 1;
          MapInfo.BitCount := 1
        end
        else
        begin
          // Read channel max value, <=255 for 8bit images, >255 for 16bit images
          // but some programs think its max colors so put <=256 here
          MapInfo.MaxVal := ReadIntValue;
          MapInfo.BitCount := Iff(MapInfo.MaxVal <= 256, 8, 16);
        end;

        MapInfo.Depth := 1;
        case Id[1] of
          '1', '4': MapInfo.TupleType := ttBlackAndWhite;
          '2', '5': MapInfo.TupleType := ttGrayScale;
          '3', '6':
            begin
              MapInfo.TupleType := ttRGB;
              MapInfo.Depth := 3;
            end;
        end;
      end
      else if Id[1] = '7' then
      begin
        // Read values from PAM header
        // WIDTH
        if (ReadString <> SPAMWidth) then Exit;
        MapInfo.Width := ReadIntValue;
        // HEIGHT
        if (ReadString <> SPAMheight) then Exit;
        MapInfo.Height := ReadIntValue;
        // DEPTH
        if (ReadString <> SPAMDepth) then Exit;
        MapInfo.Depth := ReadIntValue;
        // MAXVAL
        if (ReadString <> SPAMMaxVal) then Exit;
        MapInfo.MaxVal := ReadIntValue;
        MapInfo.BitCount := Iff(MapInfo.MaxVal <= 256, 8, 16);
        // TUPLETYPE
        if (ReadString <> SPAMTupleType) then Exit;
        TupleTypeName := ReadString;
        for I := Low(TTupleType) to High(TTupleType) do
          if SameText(TupleTypeName, TupleTypeNames[I]) then
          begin
            MapInfo.TupleType := I;
            Break;
          end;
        // ENDHDR
        if (ReadString <> SPAMEndHdr) then Exit;
      end
      else if Id[1] in ['F', 'f'] then
      begin
        // Read header of PFM file
        MapInfo.Width := ReadIntValue;
        MapInfo.Height := ReadIntValue;
        Scale := StrToFloatDef(ReadString, 0, FUSFormat);
        MapInfo.IsBigEndian := Scale > 0.0;
        if Id[1] = 'F' then
          MapInfo.TupleType := ttRGBFP
        else
          MapInfo.TupleType := ttGrayScaleFP;
        MapInfo.Depth := Iff(MapInfo.TupleType = ttRGBFP, 3, 1);
        MapInfo.BitCount := Iff(MapInfo.TupleType = ttRGBFP, 96, 32);
      end;

      FixInputPos;
      MapInfo.Binary := (Id[1] in ['4', '5', '6', '7', 'F', 'f']);

      if MapInfo.Binary  and not (Id[1] in ['F', 'f']) then
      begin
        // Mimic the behaviour of Photoshop and other editors/viewers:
        // If linereaks in file are DOS CR/LF 16bit binary values are
        // little endian, Unix LF only linebreak indicates big endian.
        MapInfo.IsBigEndian := LineBreak = #10;
      end;

      // Check if values found in header are valid
      Result := (MapInfo.Width > 0) and (MapInfo.Height > 0) and
        (MapInfo.BitCount in [1, 8, 16, 32, 96]) and (MapInfo.TupleType <> ttInvalid);
      // Now check if image has proper number of channels (PAM)
      if Result then
        case MapInfo.TupleType of
          ttBlackAndWhite, ttGrayScale:           Result := MapInfo.Depth = 1;
          ttBlackAndWhiteAlpha, ttGrayScaleAlpha: Result := MapInfo.Depth = 2;
          ttRGB:      Result := MapInfo.Depth = 3;
          ttRGBAlpha: Result := MapInfo.Depth = 4;
        end;
    end;
  end;

begin
  Result := False;
  LineEnd := 0;
  LinePos := 0;
  SetLength(Images, 1);

  with GetIO, Images[0] do
  begin
    Format := ifUnknown;
    // Try to parse file header
    if not ParseHeader then Exit;
    // Select appropriate data format based on values read from file header
    case MapInfo.TupleType of
      ttBlackAndWhite:      Format := ifGray8;
      ttBlackAndWhiteAlpha: Format := ifA8Gray8;
      ttGrayScale:          Format := IffFormat(MapInfo.BitCount = 8, ifGray8, ifGray16);
      ttGrayScaleAlpha:     Format := IffFormat(MapInfo.BitCount = 8, ifA8Gray8, ifA16Gray16);
      ttRGB:                Format := IffFormat(MapInfo.BitCount = 8, ifR8G8B8, ifR16G16B16);
      ttRGBAlpha:           Format := IffFormat(MapInfo.BitCount = 8, ifA8R8G8B8, ifA16R16G16B16);
      ttGrayScaleFP:        Format := ifR32F;
      ttRGBFP:              Format := ifB32G32R32F;
    end;
    // Exit if no matching data format was found
    if Format = ifUnknown then Exit;

    NewImage(MapInfo.Width, MapInfo.Height, Format, Images[0]);
    Info := GetFormatInfo(Format);

    // Now read pixels from file to dest image
    if not MapInfo.Binary then
    begin
      Dest := Bits;
      for I := 0 to Width * Height - 1 do
      begin
        case Format of
          ifGray8:
            begin
              Dest^ := ReadIntValue;
              if MapInfo.BitCount = 1 then
                // If source is 1bit mono image (where 0=white, 1=black)
                // we must scale it to 8bits
                Dest^ := 255 - Dest^ * 255;
            end;
          ifGray16: PWord(Dest)^ := ReadIntValue;
          ifR8G8B8:
            with PColor24Rec(Dest)^ do
            begin
              R := ReadIntValue;
              G := ReadIntValue;
              B := ReadIntValue;
            end;
          ifR16G16B16:
            with PColor48Rec(Dest)^ do
            begin
              R := ReadIntValue;
              G := ReadIntValue;
              B := ReadIntValue;
            end;
        end;
        Inc(Dest, Info.BytesPerPixel);
      end;
    end
    else
    begin
      if MapInfo.BitCount > 1 then
      begin
        if not (MapInfo.TupleType in [ttGrayScaleFP, ttRGBFP]) then
        begin
          // Just copy bytes from binary Portable Maps (non 1bit, non FP)
          Read(Handle, Bits, Size);
        end
        else
        begin
          Dest := Bits;
          // FP images are in BGR order and endian swap maybe needed.
          // Some programs store scanlines in bottom-up order but
          // I will stick with Photoshops behaviour here
          Read(Handle, Bits, Size);
          if MapInfo.IsBigEndian then
            SwapEndianUInt32(PUInt32(Dest), Size div SizeOf(UInt32));
        end;

        if MapInfo.TupleType in [ttBlackAndWhite, ttBlackAndWhiteAlpha] then
        begin
          // Black and white PAM files must be scaled to 8bits. Note that
          // in PAM files 1=white, 0=black (reverse of PBM)
          for I := 0 to Width * Height * Iff(MapInfo.TupleType = ttBlackAndWhiteAlpha, 2, 1) - 1 do
            PByteArray(Bits)[I] := PByteArray(Bits)[I] * 255;
        end
        else if MapInfo.TupleType in [ttRGB, ttRGBAlpha] then
        begin
          // Swap channels of RGB/ARGB images. Binary RGB image files use BGR order.
          SwapChannels(Images[0], ChannelBlue, ChannelRed);
        end;

        // Swap byte order if needed
        if (MapInfo.BitCount = 16) and MapInfo.IsBigEndian then
          SwapEndianWord(Bits, Width * Height * Info.BytesPerPixel div SizeOf(Word));
      end
      else
      begin
        // Handle binary PBM files (ttBlackAndWhite 1bit)
        ScanLineSize := (Width + 7) div 8;
        // Get total binary data size, read it from file to temp
        // buffer and convert the data to Gray8
        MonoSize := ScanLineSize * Height;
        GetMem(MonoData, MonoSize);
        try
          Read(Handle, MonoData, MonoSize);
          Convert1To8(MonoData, Bits, Width, Height, ScanLineSize, False);
          // 1bit mono images must be scaled to 8bit, but inverted (where 0=white, 1=black)
          for I := 0 to Width * Height - 1 do
            PByteArray(Bits)[I] := 255 - PByteArray(Bits)[I] * 255;
        finally
          FreeMem(MonoData);
        end;
      end;
    end;

    FixInputPos;

    if (MapInfo.MaxVal <> Pow2Int(MapInfo.BitCount) - 1) and
      (MapInfo.TupleType in [ttGrayScale, ttGrayScaleAlpha, ttRGB, ttRGBAlpha]) then
    begin
      Dest := Bits;
      // Scale color values according to MaxVal we got from header
      // if necessary.
      for I := 0 to Width * Height * Info.BytesPerPixel div (MapInfo.BitCount shr 3) - 1 do
      begin
        if MapInfo.BitCount = 8 then
          Dest^ := Dest^ * 255 div MapInfo.MaxVal
        else
          PWord(Dest)^ := PWord(Dest)^ * 65535 div MapInfo.MaxVal;
        Inc(Dest, MapInfo.BitCount shr 3);
      end;
    end;

    Result := True;
  end;
end;

function TPortableMapFileFormat.SaveDataInternal(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt; var MapInfo: TPortableMapInfo): Boolean;
const
  // Use Unix linebreak, for many viewers/editors it means that
  // 16bit samples are stored as big endian - so we need to swap byte order
  // before saving
  LineDelimiter  = #10;
  PixelDelimiter = #32;
var
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  Info: TImageFormatInfo;
  I, LineLength: LongInt;
  Src: PByte;
  Pixel32: TColor32Rec;
  Pixel64: TColor64Rec;
  W: Word;

  procedure WriteString(S: string; Delimiter: Char = LineDelimiter);
  begin
    SetLength(S, Length(S) + 1);
    S[Length(S)] := Delimiter;
  {$IF Defined(DCC) and Defined(UNICODE)}
    GetIO.Write(Handle, @AnsiString(S)[1], Length(S));
  {$ELSE}
    GetIO.Write(Handle, @S[1], Length(S));
  {$IFEND}
    Inc(LineLength, Length(S));
  end;

  procedure WriteHeader;
  begin
    WriteString('P' + MapInfo.FormatId);
    if not MapInfo.HasPAMHeader then
    begin
      // Write header of PGM, PPM, and PFM files
      WriteString(IntToStr(ImageToSave.Width));
      WriteString(IntToStr(ImageToSave.Height));
      case MapInfo.TupleType of
        ttGrayScale, ttRGB: WriteString(IntToStr(Pow2Int(MapInfo.BitCount) - 1));
        ttGrayScaleFP, ttRGBFP:
          begin
            // Negative value indicates that raster data is saved in little endian
            WriteString(FloatToStr(-1.0, FUSFormat));
          end;
      end;
    end
    else
    begin
      // Write PAM file header
      WriteString(Format('%s %d', [SPAMWidth, ImageToSave.Width]));
      WriteString(Format('%s %d', [SPAMHeight, ImageToSave.Height]));
      WriteString(Format('%s %d', [SPAMDepth, MapInfo.Depth]));
      WriteString(Format('%s %d', [SPAMMaxVal, Pow2Int(MapInfo.BitCount) - 1]));
      WriteString(Format('%s %s', [SPAMTupleType, TupleTypeNames[MapInfo.TupleType]]));
      WriteString(SPAMEndHdr);
    end;
  end;

begin
  Result := False;
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with GetIO, ImageToSave do
  try
    Info := GetFormatInfo(Format);
    // Fill values of MapInfo record that were not filled by
    // descendants in their SaveData methods
    MapInfo.BitCount := (Info.BytesPerPixel div Info.ChannelCount) * 8;
    MapInfo.Depth := Info.ChannelCount;
    if MapInfo.TupleType = ttInvalid then
    begin
      if Info.HasGrayChannel then
      begin
        if Info.HasAlphaChannel then
          MapInfo.TupleType := ttGrayScaleAlpha
        else
          MapInfo.TupleType := ttGrayScale;
      end
      else
      begin
        if Info.HasAlphaChannel then
          MapInfo.TupleType := ttRGBAlpha
        else
          MapInfo.TupleType := ttRGB;
      end;
    end;
    // Write file header
    WriteHeader;

    if not MapInfo.Binary then
    begin
      Src := Bits;
      LineLength := 0;
      // For each pixel find its text representation and write it to file
      for I := 0 to Width * Height  - 1 do
      begin
        case Format of
          ifGray8: WriteString(IntToStr(Src^), PixelDelimiter);
          ifGray16: WriteString(IntToStr(PWord(Src)^), PixelDelimiter);
          ifR8G8B8:
            with PColor24Rec(Src)^ do
              WriteString(SysUtils.Format('%d %d %d', [R, G, B]), PixelDelimiter);
          ifR16G16B16:
            with PColor48Rec(Src)^ do
              WriteString(SysUtils.Format('%d %d %d', [R, G, B]), PixelDelimiter);
        end;
        // Lines in text PNM images should have length <70
        if LineLength > 65 then
        begin
          LineLength := 0;
          WriteString('', LineDelimiter);
        end;
        Inc(Src, Info.BytesPerPixel);
      end;
    end
    else
    begin
      // Write binary images
      if not (MapInfo.TupleType in [ttGrayScaleFP, ttRGBFP]) then
      begin
        // Save integer binary images
        if  MapInfo.BitCount = 8 then
        begin
          if MapInfo.TupleType in [ttGrayScale, ttGrayScaleAlpha] then
          begin
            // 8bit grayscale images can be written in one Write call
            Write(Handle, Bits, Size);
          end
          else
          begin
            // 8bit RGB/ARGB images: red and blue must be swapped and
            // 3 or 4 bytes must be written
            Src := Bits;
            for I := 0 to Width * Height - 1 do
            with PColor32Rec(Src)^ do
            begin
              if MapInfo.TupleType = ttRGBAlpha then
                Pixel32.A := A;
              Pixel32.R := B;
              Pixel32.G := G;
              Pixel32.B := R;
              Write(Handle, @Pixel32, Info.BytesPerPixel);
              Inc(Src, Info.BytesPerPixel);
            end;
          end;
        end
        else
        begin
          // Images with 16bit channels: make sure that channel values are saved in big endian
          Src := Bits;
          if MapInfo.TupleType in [ttGrayScale, ttGrayScaleAlpha] then
          begin
            // 16bit grayscale image
            for I := 0 to Width * Height * Info.BytesPerPixel div SizeOf(Word) - 1 do
            begin
              W := SwapEndianWord(PWord(Src)^);
              Write(Handle, @W, SizeOf(Word));
              Inc(Src, SizeOf(Word));
            end;
          end
          else
          begin
            // RGB images with 16bit channels: swap RB and endian too
            for I := 0 to Width * Height - 1 do
            with PColor64Rec(Src)^ do
            begin
              if MapInfo.TupleType = ttRGBAlpha then
                Pixel64.A := SwapEndianWord(A);
              Pixel64.R := SwapEndianWord(B);
              Pixel64.G := SwapEndianWord(G);
              Pixel64.B := SwapEndianWord(R);
              Write(Handle, @Pixel64, Info.BytesPerPixel);
              Inc(Src, Info.BytesPerPixel);
            end;
          end;
        end;
      end
      else
      begin
        // Floating point images (no need to swap endian here - little
        // endian is specified in file header)
        Write(Handle, Bits, Size);
      end;
    end;
    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

function TPortableMapFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Id: TChar4;
  ReadCount: LongInt;
begin
  Result := False;
  if Handle <> nil then
  with GetIO do
  begin
    ReadCount := Read(Handle, @Id, SizeOf(Id));
    Seek(Handle, -ReadCount, smFromCurrent);
    Result := (Id[0] = 'P') and (Id[1] in [FIdNumbers[0], FIdNumbers[1]]) and
      (Id[2] in WhiteSpaces);
  end;
end;

{ TPBMFileFormat }

procedure TPBMFileFormat.Define;
begin
  inherited;
  FName := SPBMFormatName;
  FFeatures := [ffLoad];
  AddMasks(SPBMMasks);
  FIdNumbers := '14';
end;

{ TPGMFileFormat }

procedure TPGMFileFormat.Define;
begin
  inherited;
  FName := SPGMFormatName;
  FSupportedFormats := PGMSupportedFormats;
  AddMasks(SPGMMasks);
  RegisterOption(ImagingPGMSaveBinary, @FSaveBinary);
  FIdNumbers := '25';
end;

function TPGMFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  MapInfo: TPortableMapInfo;
begin
  FillChar(MapInfo, SizeOf(MapInfo), 0);
  if FSaveBinary then
    MapInfo.FormatId := FIdNumbers[1]
  else
    MapInfo.FormatId := FIdNumbers[0];
  MapInfo.Binary := FSaveBinary;
  Result := SaveDataInternal(Handle, Images, Index, MapInfo);
end;

procedure TPGMFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
    // All FP images go to 16bit
    ConvFormat :=  ifGray16
  else if Info.HasGrayChannel then
    // Grayscale will be 8 or 16 bit - depends on input's bitcount
    ConvFormat := IffFormat(Info.BytesPerPixel div Info.ChannelCount > 1,
      ifGray16, ifGray8)
  else if Info.BytesPerPixel > 4 then
    // Large bitcounts -> 16bit
    ConvFormat := ifGray16
  else
    // Rest of the formats -> 8bit 
    ConvFormat := ifGray8;

  ConvertImage(Image, ConvFormat);
end;

{ TPPMFileFormat }

procedure TPPMFileFormat.Define;
begin
  inherited;
  FName := SPPMFormatName;
  FSupportedFormats := PPMSupportedFormats;
  AddMasks(SPPMMasks);
  RegisterOption(ImagingPPMSaveBinary, @FSaveBinary);
  FIdNumbers := '36';
end;

function TPPMFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  MapInfo: TPortableMapInfo;
begin
  FillChar(MapInfo, SizeOf(MapInfo), 0);
  if FSaveBinary then
    MapInfo.FormatId := FIdNumbers[1]
  else
    MapInfo.FormatId := FIdNumbers[0];
  MapInfo.Binary := FSaveBinary;
  Result := SaveDataInternal(Handle, Images, Index, MapInfo);
end;

procedure TPPMFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
    // All FP images go to 48bit RGB
    ConvFormat :=  ifR16G16B16
  else if Info.HasGrayChannel then
    // Grayscale will be 24 or 48 bit RGB - depends on input's bitcount
    ConvFormat := IffFormat(Info.BytesPerPixel div Info.ChannelCount > 1,
      ifR16G16B16, ifR8G8B8)
  else if Info.BytesPerPixel > 4 then
    // Large bitcounts -> 48bit RGB
    ConvFormat := ifR16G16B16
  else
    // Rest of the formats -> 24bit RGB
    ConvFormat := ifR8G8B8;

  ConvertImage(Image, ConvFormat);
end;

{ TPAMFileFormat }

procedure TPAMFileFormat.Define;
begin
  inherited;
  FName := SPAMFormatName;
  FSupportedFormats := PAMSupportedFormats;
  AddMasks(SPAMMasks);
  FIdNumbers := '77';
end;

function TPAMFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  MapInfo: TPortableMapInfo;
begin
  FillChar(MapInfo, SizeOf(MapInfo), 0);
  MapInfo.FormatId := FIdNumbers[0];
  MapInfo.Binary := True;
  MapInfo.HasPAMHeader := True;
  Result := SaveDataInternal(Handle, Images, Index, MapInfo);
end;

procedure TPAMFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
var
  ConvFormat: TImageFormat;
begin
  if Info.IsFloatingPoint then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16R16G16B16, ifR16G16B16)
  else if Info.HasGrayChannel then
    ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16Gray16, ifGray16)
  else
  begin
    if Info.BytesPerPixel <= 4 then
      ConvFormat := IffFormat(Info.HasAlphaChannel, ifA8R8G8B8, ifR8G8B8)
    else
      ConvFormat := IffFormat(Info.HasAlphaChannel, ifA16R16G16B16, ifR16G16B16);
  end;
  ConvertImage(Image, ConvFormat);
end;

{ TPFMFileFormat }

procedure TPFMFileFormat.Define;
begin
  inherited;
  FName := SPFMFormatName;
  AddMasks(SPFMMasks);
  FIdNumbers := 'Ff';
  FSupportedFormats := PFMSupportedFormats;
end;

function TPFMFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
var
  Info: TImageFormatInfo;
  MapInfo: TPortableMapInfo;
begin
  FillChar(MapInfo, SizeOf(MapInfo), 0);
  Info := GetFormatInfo(Images[Index].Format);

  if (Info.ChannelCount > 1) or Info.IsIndexed then
    MapInfo.TupleType := ttRGBFP
  else
    MapInfo.TupleType := ttGrayScaleFP;

  if MapInfo.TupleType = ttGrayScaleFP then
    MapInfo.FormatId := FIdNumbers[1]
  else
    MapInfo.FormatId := FIdNumbers[0];

  MapInfo.Binary := True;
  Result := SaveDataInternal(Handle, Images, Index, MapInfo);
end;

procedure TPFMFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  if (Info.ChannelCount > 1) or Info.IsIndexed then
    ConvertImage(Image, ifB32G32R32F)
  else
    ConvertImage(Image, ifR32F);
end;

initialization
  RegisterImageFileFormat(TPBMFileFormat);
  RegisterImageFileFormat(TPGMFileFormat);
  RegisterImageFileFormat(TPPMFileFormat);
  RegisterImageFileFormat(TPAMFileFormat);
  RegisterImageFileFormat(TPFMFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.77.1 Changes/Bug Fixes -----------------------------------
    - Native RGB floating point format of PFM is now supported by Imaging
      so we use it now for saving instead of A32B32G32B32.
    - String to float formatting changes (don't change global settings).

  -- 0.26.3 Changes/Bug Fixes -----------------------------------
    - Fixed D2009 Unicode related bug in PNM saving.

  -- 0.24.3 Changes/Bug Fixes -----------------------------------
    - Improved compatibility of 16bit/component image loading.
    - Changes for better thread safety.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Made modifications to ASCII PNM loading to be more "stream-safe".
    - Fixed bug: indexed images saved as grayscale in PFM.
    - Changed converting to supported formats little bit.
    - Added scaling of channel values (non-FP and non-mono images) according
      to MaxVal.
    - Added buffering to loading of PNM files. More than 10x faster now
      for text files.
    - Added saving support to PGM, PPM, PAM, and PFM format.
    - Added PFM file format.
    - Initial version created.
}

end.
