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

{ This unit contains image format loader/saver for Radiance HDR/RGBE images.}
unit ImagingRadiance;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Imaging, ImagingTypes, ImagingUtility;

type
  { Radiance is a suite of tools for performing lighting simulation. It's
    development started in 1985 and it pioneered the concept of
    high dynamic range imaging. Radiance defined an image format for storing
    HDR images, now described as RGBE image format. Since it was the first
    HDR image format, this format is supported by many other software packages.

    Radiance image file consists of three sections: a header, resolution string,
    followed by the pixel data. Each pixel is stored as 4 bytes, one byte
    mantissa for each r, g, b and a shared one byte exponent.
    The pixel data may be stored uncompressed or using run length encoding.

    Imaging translates RGBE pixels to original float values and stores them
    in ifR32G32B32F data format. It can read both compressed and uncompressed
    files, and saves files as compressed.}
  THdrFileFormat = class(TImageFileFormat)
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
  end;

implementation

uses
  Math, ImagingIO;

const
  SHdrFormatName = 'Radiance HDR/RGBE';
  SHdrMasks      = '*.hdr';
  HdrSupportedFormats: TImageFormats = [ifR32G32B32F];

type
  TSignature = array[0..9] of AnsiChar;
  THdrFormat = (hfRgb, hfXyz);

  THdrHeader = record
    Format: THdrFormat;
    Width: Integer;
    Height: Integer;
  end;

  TRgbe = packed record
    R, G, B, E: Byte;
  end;
  TDynRgbeArray = array of TRgbe;

const
  RadianceSignature: TSignature = '#?RADIANCE';
  RgbeSignature: TSignature = '#?RGBE';
  SFmtRgbeRle = '32-bit_rle_rgbe';
  SFmtXyzeRle = '32-bit_rle_xyze';

resourcestring
  SErrorBadHeader = 'Bad HDR/RGBE header format.';
  SWrongScanLineWidth = 'Wrong scanline width.';
  SXyzNotSupported = 'XYZ color space not supported.';

{ THdrFileFormat }

procedure THdrFileFormat.Define;
begin
  inherited;
  FName := SHdrFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := HdrSupportedFormats;

  AddMasks(SHdrMasks);
end;

function THdrFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Header: THdrHeader;
  IO: TIOFunctions;

  function ReadHeader: Boolean;
  const
    CommentIds: TAnsiCharSet = ['#', '!'];
  var
    Line: AnsiString;
    HasResolution: Boolean;
    Count, Idx: Integer;
    ValStr, NativeLine: string;
    ValFloat: Double;
  begin
    Result := False;
    HasResolution := False;
    Count := 0;

    repeat
      if not ReadLine(IO, Handle, Line) then
        Exit;

      Inc(Count);
      if Count > 16 then // Too long header for HDR
        Exit;

      if Length(Line) = 0 then
        Continue;
      if Line[1] in CommentIds then
        Continue;

      NativeLine := string(Line);

      if StrMaskMatch(NativeLine, 'Format=*') then
      begin
        // Data format parsing
        ValStr := Copy(NativeLine, 8, MaxInt);
        if ValStr = SFmtRgbeRle then
          Header.Format := hfRgb
        else if ValStr = SFmtXyzeRle then
          Header.Format := hfXyz
        else
          Exit;
      end;

      if StrMaskMatch(NativeLine, 'Gamma=*') then
      begin
        ValStr := Copy(NativeLine, 7, MaxInt);
        if TryStrToFloat(ValStr, ValFloat, GetFormatSettingsForFloats) then
          FMetadata.SetMetaItem(SMetaGamma, ValFloat);
      end;

      if StrMaskMatch(NativeLine, 'Exposure=*') then
      begin
        ValStr := Copy(NativeLine, 10, MaxInt);
        if TryStrToFloat(ValStr, ValFloat, GetFormatSettingsForFloats) then
          FMetadata.SetMetaItem(SMetaExposure, ValFloat);
      end;

      if StrMaskMatch(NativeLine, '?Y * ?X *') then
      begin
        Idx := Pos('X', NativeLine);
        ValStr := SubString(NativeLine, 4, Idx - 2);
        if not TryStrToInt(ValStr, Header.Height) then
          Exit;
        ValStr := Copy(NativeLine, Idx + 2, MaxInt);
        if not TryStrToInt(ValStr, Header.Width) then
          Exit;

        if (NativeLine[1] = '-') then
          Header.Height := -Header.Height;
        if (NativeLine[Idx - 1] = '-') then
          Header.Width := -Header.Width;

        HasResolution := True;
      end;

    until HasResolution;
    Result := True;
  end;

  procedure DecodeRgbe(const Src: TRgbe; Dest: PColor96FPRec); {$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    Mult: Single;
  begin
    if Src.E > 0 then
    begin
      Mult := Math.Ldexp(1, Src.E - 128);
      Dest.R := Src.R / 255 * Mult;
      Dest.G := Src.G / 255 * Mult;
      Dest.B := Src.B / 255 * Mult;
    end
    else
    begin
      Dest.R := 0;
      Dest.G := 0;
      Dest.B := 0;
    end;
  end;

  procedure ReadCompressedLine(Width, Y: Integer; var DestBuffer: TDynRgbeArray);
  var
    Pos: Integer;
    I, X, Count: Integer;
    Code, Value: Byte;
    LineBuff: TDynByteArray;
    Rgbe: TRgbe;
    Ptr: PByte;
  begin
    SetLength(LineBuff, Width);
    IO.Read(Handle, @Rgbe, SizeOf(Rgbe));

    if ((Rgbe.B shl 8) or Rgbe.E) <> Width  then
      RaiseImaging(SWrongScanLineWidth);

    for I := 0 to 3 do
    begin
      Pos := 0;
      while Pos < Width do
      begin
        IO.Read(Handle, @Code, SizeOf(Byte));
        if Code > 128 then
        begin
          Count := Code - 128;
          IO.Read(Handle, @Value, SizeOf(Byte));
          FillMemoryByte(@LineBuff[Pos], Count, Value);
        end
        else
        begin
          Count := Code;
          IO.Read(Handle, @LineBuff[Pos], Count * SizeOf(Byte));
        end;
        Inc(Pos, Count);
      end;

      Ptr := @PByteArray(@DestBuffer[0])[I];
      for X := 0 to Width - 1 do
      begin
        Ptr^ := LineBuff[X];
        Inc(Ptr, 4);
      end;
    end;
  end;

  procedure ReadPixels(var Image: TImageData);
  var
    Y, X, SrcLineLen: Integer;
    Dest: PColor96FPRec;
    Compressed: Boolean;
    Rgbe: TRgbe;
    Buffer: TDynRgbeArray;
  begin
    Dest := Image.Bits;
    Compressed := not ((Image.Width < 8) or (Image.Width > $7FFFF));
    SrcLineLen := Image.Width * SizeOf(TRgbe);

    IO.Read(Handle, @Rgbe, SizeOf(Rgbe));
    IO.Seek(Handle, -SizeOf(Rgbe), smFromCurrent);

    if (Rgbe.R <> 2) or (Rgbe.G <> 2) or ((Rgbe.B and 128) > 0) then
      Compressed := False;

    SetLength(Buffer, Image.Width);

    for Y := 0 to Image.Height - 1 do
    begin
      if Compressed then
        ReadCompressedLine(Image.Width, Y, Buffer)
      else
        IO.Read(Handle, @Buffer[0], SrcLineLen);

      for X := 0 to Image.Width - 1 do
      begin
        DecodeRgbe(Buffer[X], Dest);
        Inc(Dest);
      end;
    end;
  end;

begin
  IO := GetIO;
  SetLength(Images, 1);

  // Read header, allocate new image and, then read and convert the pixels
  if not ReadHeader then
    RaiseImaging(SErrorBadHeader);
  if (Header.Format = hfXyz) then
    RaiseImaging(SXyzNotSupported);

  NewImage(Abs(Header.Width), Abs(Header.Height), ifR32G32B32F, Images[0]);
  ReadPixels(Images[0]);

  // Flip/mirror the image as needed (height < 0 is default top-down)
  if Header.Width < 0 then
    MirrorImage(Images[0]);
  if Header.Height > 0 then
    FlipImage(Images[0]);

  Result := True;
end;

function THdrFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
const
  LineEnd = #$0A;
  SPrgComment = '#Made with Vampyre Imaging Library';
  SSizeFmt = '-Y %d +X %d';
var
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  IO: TIOFunctions;

  procedure SaveHeader;
  begin
    WriteLine(IO, Handle, RadianceSignature, LineEnd);
    WriteLine(IO, Handle, SPrgComment, LineEnd);
    WriteLine(IO, Handle, 'FORMAT=' + SFmtRgbeRle, LineEnd + LineEnd);
    WriteLine(IO, Handle, AnsiString(Format(SSizeFmt, [ImageToSave.Height, ImageToSave.Width])), LineEnd);
  end;

  procedure EncodeRgbe(const Src: TColor96FPRec; var DestR, DestG, DestB, DestE: Byte); {$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    V, M: {$IFDEF FPC}Float{$ELSE}Extended{$ENDIF};
    E: Integer;
  begin
    V := Src.R;
    if (Src.G > V) then
      V := Src.G;
    if (Src.B > V) then
      V := Src.B;

    if V < 1e-32 then
    begin
      DestR := 0;
      DestG := 0;
      DestB := 0;
      DestE := 0;
    end
    else
    begin
      Frexp(V, M, E);
      V := M * 256.0 / V;
      DestR := ClampToByte(Round(Src.R * V));
      DestG := ClampToByte(Round(Src.G * V));
      DestB := ClampToByte(Round(Src.B * V));
      DestE := ClampToByte(E + 128);
    end;
  end;

  procedure WriteRleLine(const Line: array of Byte; Width: Integer);
  const
    MinRunLength = 4;
  var
    Cur, BeginRun, RunCount, OldRunCount, NonRunCount: Integer;
    Buf: array[0..1] of Byte;
  begin
    Cur := 0;
    while Cur < Width do
    begin
      BeginRun := Cur;
      RunCount := 0;
      OldRunCount := 0;
      while (RunCount < MinRunLength) and (BeginRun < Width) do
      begin
        Inc(BeginRun, RunCount);
        OldRunCount := RunCount;
        RunCount := 1;
        while (BeginRun + RunCount < Width) and (RunCount < 127) and (Line[BeginRun] = Line[BeginRun + RunCount]) do
          Inc(RunCount);
      end;
      if (OldRunCount > 1) and (OldRunCount = BeginRun - Cur) then
      begin
        Buf[0] := 128 + OldRunCount;
        Buf[1] := Line[Cur];
        IO.Write(Handle, @Buf, 2);
        Cur := BeginRun;
      end;
      while Cur < BeginRun do
      begin
        NonRunCount := Min(128, BeginRun - Cur);
        Buf[0] := NonRunCount;
        IO.Write(Handle, @Buf, 1);
        IO.Write(Handle, @Line[Cur], NonRunCount);
        Inc(Cur, NonRunCount);
      end;
      if RunCount >= MinRunLength then
      begin
        Buf[0] := 128 + RunCount;
        Buf[1] := Line[BeginRun];
        IO.Write(Handle, @Buf, 2);
        Inc(Cur, RunCount);
      end;
    end;
  end;

  procedure SavePixels;
  var
    Y, X, I, Width: Integer;
    SrcPtr: PColor96FPRecArray;
    Components: array of array of Byte;
    StartLine: array[0..3] of Byte;
  begin
    Width := ImageToSave.Width;
    // Save using RLE, each component is compressed separately
    SetLength(Components, 4, Width);

    for Y := 0 to ImageToSave.Height - 1 do
    begin
      SrcPtr := @PColor96FPRecArray(ImageToSave.Bits)[ImageToSave.Width * Y];

      // Identify line as using "new" RLE scheme (separate components)
      StartLine[0] := 2;
      StartLine[1] := 2;
      StartLine[2] := Width shr 8;
      StartLine[3] := Width and $FF;
      IO.Write(Handle, @StartLine, SizeOf(StartLine));

      for X := 0 to Width - 1 do
      begin
        EncodeRgbe(SrcPtr[X], Components[0, X], Components[1, X],
          Components[2, X], Components[3, X]);
      end;

      for I := 0 to 3 do
        WriteRleLine(Components[I], Width);
    end;
  end;

begin
  Result := False;
  IO := GetIO;
  // Makes image to save compatible with Jpeg saving capabilities
  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  with ImageToSave do
  try
    // Save header
    SaveHeader;
    // Save uncompressed pixels
    SavePixels;
    // Castle Game Engine fix, submitted upstream, see https://github.com/galfar/imaginglib/pull/23
    Result := True;
  finally
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure THdrFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  ConvertImage(Image, ifR32G32B32F);
end;

function THdrFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  FileSig: TSignature;
  ReadCount: Integer;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @FileSig, SizeOf(FileSig));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (ReadCount = SizeOf(FileSig)) and
      ((FileSig = RadianceSignature) or CompareMem(@FileSig, @RgbeSignature, 6));
  end;
end;

initialization
  RegisterImageFileFormat(THdrFileFormat);

{
  File Notes:

  -- 0.77.1 ---------------------------------------------------
    - Added RLE compression to saving.
    - Added image saving.
    - Unit created with initial stuff (loading only).

}

end.
