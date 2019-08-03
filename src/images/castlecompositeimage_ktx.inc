{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ KTX loading. }

{ TKTXHandler ----------------------------------------------------------------- }

type
  TKTXHandler = class(TCompositeFormatHandler)
  strict private
    const
      // See https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
      CorrectFileId: array [0..11] of Byte =
      ($AB, $4B, $54, $58, $20, $31, $31, $BB, $0D, $0A, $1A, $0A);

    type
      TKTXHeader = record
        FileId: array [0..High(CorrectFileId)] of Byte;
        Endianness,
        GlType,
        GlTypeSize,
        GlFormat,
        GlInternalFormat,
        GlBaseInternalFormat,
        Width,
        Height,
        Depth,
        NumberOfArrayElements,
        NumberOfFaces,
        NumberOfMipmapLevels,
        BytesOfKeyValueData: LongWord;
      end;
    const
      // See /usr/include/GL/gl.h , /usr/include/GL/glext.h
      //GL_BYTE           = $1400;
      GL_UNSIGNED_BYTE  = $1401;
      //GL_SHORT          = $1402;
      GL_UNSIGNED_SHORT = $1403;
      //GL_INT            = $1404;
      //GL_UNSIGNED_INT   = $1405;
      GL_FLOAT          = $1406;
      //GL_DOUBLE         = $140A;
      GL_HALF_FLOAT     = $140B;

      GL_RED             = $1903;
      //GL_GREEN           = $1904;
      //GL_BLUE            = $1905;
      //GL_ALPHA           = $1906;
      GL_RGB             = $1907;
      GL_RGBA            = $1908;
      GL_LUMINANCE       = $1909;
      //GL_LUMINANCE_ALPHA = $190A;

      GL_LUMINANCE8_EXT = $8040;
      GL_BGR            = $80E0;
      GL_BGRA           = $80E1;

      GL_R16            = $822A;
      GL_RG8            = $822B;
      GL_RG             = $8227;
      //GL_R16F           = $822D;
      GL_R32F           = $822E;
      GL_R16UI          = $8234;

      GL_RED_INTEGER    = $8D94;

    var
      Header: TKTXHeader;
      SwapEndianness: boolean;
      RowsTopToBottom, SlicesFrontToBack: boolean;

    procedure ReadKeyValues(const Stream: TStream);
    procedure ReadOrientation(const Value: string);

    { The KTX specification says
      "Uncompressed texture data matches a GL_UNPACK_ALIGNMENT of 4." }
    function RowBytePadding(const Image: TCastleImage): Integer;

    procedure ReadOneImage(const Stream: TStream);

    { Optionally invert Y, depending on RowsTopToBottom. }
    function TargetY(const Y: Integer): Integer;

    { Optionally invert Z, depending on SlicesFrontToBack. }
    function TargetZ(const Z: Integer): Integer;

    procedure ReadG(const Stream: TStream);
    procedure ReadG16(const Stream: TStream);
    procedure ReadGA(const Stream: TStream);
    procedure ReadRGB(const Stream: TStream);
    procedure ReadBGR(const Stream: TStream);
    procedure ReadRGBA(const Stream: TStream);
    procedure ReadBGRA(const Stream: TStream);
    procedure ReadFloatImage(const Stream: TStream);
    procedure ReadCompressed(const Stream: TStream; const Compression: TTextureCompression);
  public
    procedure LoadFromStream(const Stream: TStream; const URL: string); override;
  end;

function TKTXHandler.TargetY(const Y: Integer): Integer;
begin
  if RowsTopToBottom then
    Result := Height - Y - 1
  else
    Result := Y;
end;

function TKTXHandler.TargetZ(const Z: Integer): Integer;
begin
  if SlicesFrontToBack then
    Result := Depth - Z - 1
  else
    Result := Z;
end;

procedure TKTXHandler.ReadG(const Stream: TStream);
var
  Y, Z: Integer;
  Row: PByteArray;
  Image: TGrayscaleImage;
  Padding: Integer;
begin
  Image := TGrayscaleImage.Create(Width, Height, Depth);
  Images.Add(Image);

  Padding := RowBytePadding(Image);

  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := Image.RowPtr(TargetY(Y), TargetZ(Z));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      if Padding <> 0 then
        Stream.Seek(Padding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadG16(const Stream: TStream);
var
  RowData: TWordList;
  TargetRow: PByteArray;
  X, Y, Z: Integer;
  Image: TGrayscaleImage;
  Padding: Integer;
begin
  Image := TGrayscaleImage.Create(Width, Height, Depth);
  Images.Add(Image);

  Padding := RowBytePadding(Image);

  RowData := TWordList.Create;
  try
    RowData.Count := Image.Width;

    for Z := 0 to Image.Depth - 1 do
      for Y := 0 to Image.Height - 1 do
      begin
        Stream.ReadBuffer(RowData.List^, Image.Width * SizeOf(Word));
        { convert row of Words to Bytes }
        TargetRow := Image.RowPtr(TargetY(Y), TargetZ(Z));
        for X := 0 to Image.Width - 1 do
          TargetRow^[X] := RowData.List^[X] shr 8;
         if Padding <> 0 then
           Stream.Seek(Padding, soFromCurrent);
       end;
  finally FreeAndNil(RowData) end;
end;

procedure TKTXHandler.ReadGA(const Stream: TStream);
var
  Y, Z: Integer;
  Row: PVector2ByteArray;
  Image: TGrayscaleAlphaImage;
  Padding: Integer;
begin
  Image := TGrayscaleAlphaImage.Create(Width, Height, Depth);
  Images.Add(Image);

  Padding := RowBytePadding(Image);

  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := Image.RowPtr(TargetY(Y), TargetZ(Z));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      if Padding <> 0 then
        Stream.Seek(Padding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadRGB(const Stream: TStream);
var
  Y, Z: Integer;
  Row: PVector3ByteArray;
  Image: TRGBImage;
  Padding: Integer;
begin
  Image := TRGBImage.Create(Width, Height, Depth);
  Images.Add(Image);

  Padding := RowBytePadding(Image);

  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := Image.RowPtr(TargetY(Y), TargetZ(Z));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      if Padding <> 0 then
        Stream.Seek(Padding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadBGR(const Stream: TStream);
var
  X, Y, Z: Integer;
  Row: PVector3Byte;
  Image: TRGBImage;
  Padding: Integer;
begin
  Image := TRGBImage.Create(Width, Height, Depth);
  Images.Add(Image);

  Padding := RowBytePadding(Image);

  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := PVector3Byte(Image.RowPtr(TargetY(Y), TargetZ(Z)));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      { Now invert red and blue. }
      for X := 0 to Image.Width - 1 do
      begin
        SwapValues(Row^.Data[2], Row^.Data[0]);
        Inc(Row);
      end;
      if Padding <> 0 then
        Stream.Seek(Padding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadRGBA(const Stream: TStream);
var
  Y, Z: Integer;
  Row: PVector4ByteArray;
  Image: TRGBAlphaImage;
begin
  Image := TRGBAlphaImage.Create(Width, Height, Depth);
  // when Image.PixelSize is 4, there is never any padding
  Assert(RowBytePadding(Image) = 0);
  Images.Add(Image);

  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := Image.RowPtr(TargetY(Y), TargetZ(Z));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
    end;
end;

procedure TKTXHandler.ReadBGRA(const Stream: TStream);
var
  X, Y, Z: Integer;
  Row: PVector4Byte;
  Image: TRGBAlphaImage;
begin
  Image := TRGBAlphaImage.Create(Width, Height, Depth);
  // when Image.PixelSize is 4, there is never any padding
  Assert(RowBytePadding(Image) = 0);
  Images.Add(Image);

  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := PVector4Byte(Image.RowPtr(TargetY(Y), TargetZ(Z)));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      { Now invert red and blue. }
      for X := 0 to Image.Width - 1 do
      begin
        SwapValues(Row^.Data[2], Row^.Data[0]);
        Inc(Row);
      end;
    end;
end;

procedure TKTXHandler.ReadFloatImage(const Stream: TStream);
var
  Floats: TSingleList;
  X, Y, Z: Integer;
  Pixel: PVector3;
  PixelSource: PSingle;
  Image: TRGBFloatImage;
begin
  Image := TRGBFloatImage.Create(Width, Height, Depth);
  // when Image.PixelSize is 4, there is never any padding
  Assert(RowBytePadding(Image) = 0);
  Images.Add(Image);

  Floats := TSingleList.Create;
  try
    Floats.Count := Image.Width * Image.Height * Image.Depth;
    Stream.ReadBuffer(Floats.List^, Floats.Count * SizeOf(Single));
    PixelSource := PSingle(Floats.List);

    // move Floats data to Image.Pixels
    for Z := 0 to Image.Depth - 1 do
      for Y := 0 to Image.Height - 1 do
      begin
        Pixel := PVector3(Image.RowPtr(TargetY(Y), TargetZ(Z)));
        for X := 0 to Image.Width - 1 do
        begin
          Pixel^[0] := PixelSource^;
          Pixel^[1] := PixelSource^;
          Pixel^[2] := PixelSource^;
          Inc(PixelSource);
          Inc(Pixel);
        end;
      end;
  finally FreeAndNil(Floats) end;
end;

procedure TKTXHandler.ReadOrientation(const Value: string);
begin
  if (Value = 'S=r,T=d') or
     (Value = 'S=r,T=d,R=o') then
  begin
    RowsTopToBottom := true;
    SlicesFrontToBack := false;
  end else
  if (Value = 'S=r,T=u') or
     (Value = 'S=r,T=u,R=o') then
  begin
    RowsTopToBottom := false;
    SlicesFrontToBack := false;
  end else
  { This orientation is not adviced by KTX specification,
    but is used by PowerVR texture tools if you do "vertical flip" at encoding. }
  if Value = 'S=r,T=u,R=i' then
  begin
    RowsTopToBottom := false;
    SlicesFrontToBack := true;
  end else
  if Value = 'S=r,T=d,R=i' then
  begin
    RowsTopToBottom := true;
    SlicesFrontToBack := true;
  end else
    WritelnWarning('KTX', 'Unrecognized KTX orientation "%s"', [Value]);

  WritelnLog('KTX', 'Got KTX orientation "%s". RowsTopToBottom: %s, SlicesFrontToBack: %s.',
    [Value,
     BoolToStr(RowsTopToBottom, true),
     BoolToStr(SlicesFrontToBack, true)
    ]);
end;

procedure TKTXHandler.ReadKeyValues(const Stream: TStream);
var
  RemainingToRead, KeyAndValueByteSize, ValuePadding: LongWord;
  KeyAndValue: array of AnsiChar;
  Key, Value: AnsiString;
begin
  RemainingToRead := Header.BytesOfKeyValueData;
  while RemainingToRead > 0 do
  begin
    Stream.ReadBuffer(KeyAndValueByteSize, SizeOf(KeyAndValueByteSize));
    if SwapEndianness then
      KeyAndValueByteSize := Swap32(KeyAndValueByteSize);
    if KeyAndValueByteSize > 0 then
    begin
      SetLength(KeyAndValue, KeyAndValueByteSize);
      Stream.ReadBuffer(KeyAndValue[0], KeyAndValueByteSize);
      ValuePadding := 3 - ((KeyAndValueByteSize + 3) mod 4);
      if ValuePadding <> 0 then
        Stream.Seek(ValuePadding, soFromCurrent);

      Key := PChar(@(KeyAndValue[0]));
      SetLength(Value, KeyAndValueByteSize - Length(Key) - 1);
      if Length(Value) > 0 then
      begin
        // check is Value NULL-terminated
        if KeyAndValue[KeyAndValueByteSize - 1] = #0 then
          SetLength(Value, Length(Value) - 1);
        Move(KeyAndValue[Length(Key) + 1], Value[1], Length(Value));
      end;

      { Compare ignoring case, as specification recommends using
        'KTXorientation' (as samples in https://github.com/KhronosGroup/KTX
        follow it), but PowerVR texture tools write 'KTXOrientation' }
      if SameText(Key, 'KTXorientation') then
        ReadOrientation(Value);
    end else
      ValuePadding := 0;

    RemainingToRead := RemainingToRead -
      (SizeOf(KeyAndValueByteSize) + KeyAndValueByteSize + ValuePadding);
  end;
end;

procedure TKTXHandler.ReadCompressed(const Stream: TStream; const Compression: TTextureCompression);
var
  Image: TGPUCompressedImage;
begin
  Image := TGPUCompressedImage.Create(Width, Height, Depth, Compression);
  Images.Add(Image);

  Stream.ReadBuffer(Image.RawPixels^, Image.Size);

  if RowsTopToBottom then
    Image.FlipVertical;
end;

function TKTXHandler.RowBytePadding(const Image: TCastleImage): Integer;
begin
  Result := Image.PixelSize * Image.Width mod 4;
  if Result <> 0 then
    Result := 4 - Result;
end;

procedure TKTXHandler.ReadOneImage(const Stream: TStream);
{$I gl_texture_compression_constants.inc}
var
  Compression: TTextureCompression;
begin
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_LUMINANCE) and
     (Header.GlInternalFormat = GL_LUMINANCE8_EXT) and
     (Header.GlBaseInternalFormat = GL_LUMINANCE) then
  begin
    ReadG(Stream);
  end else
  if (
       { You can generate this with
         - PowerVR Texture Tool GUI with "Encode" to R16
         - PowerVR Texture Tool CLI with "-f r16,US" encoding
       }
       (Header.GlType = GL_UNSIGNED_SHORT) and
       (Header.GlTypeSize = 2) and
       (Header.GlFormat = GL_RED_INTEGER) and
       (Header.GlInternalFormat = GL_R16UI) and
       (Header.GlBaseInternalFormat = GL_RED_INTEGER)
     ) or (
       { You can generate this with
         - PowerVR Texture Tool CLI with "-f r16,USN" encoding
       }
       (Header.GlType = GL_UNSIGNED_SHORT) and
       (Header.GlTypeSize = 2) and
       (Header.GlFormat = GL_RED) and
       (Header.GlInternalFormat = GL_R16) and
       (Header.GlBaseInternalFormat = GL_RED)
     ) then
  begin
    ReadG16(Stream);
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_RG) and
     (Header.GlInternalFormat = GL_RG8) and
     (Header.GlBaseInternalFormat = GL_RG) then
  begin
    ReadGA(Stream);
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_BGRA) and
     (Header.GlInternalFormat = GL_BGRA) and
     (Header.GlBaseInternalFormat = GL_BGRA) then
  begin
    ReadBGRA(Stream);
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_RGBA) and
     (Header.GlInternalFormat = GL_RGBA) and
     (Header.GlBaseInternalFormat = GL_RGBA) then
  begin
    ReadRGBA(Stream);
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_BGR) and
     (Header.GlInternalFormat = GL_BGR) and
     (Header.GlBaseInternalFormat = GL_BGR) then
  begin
    ReadBGR(Stream);
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_RGB) and
     (Header.GlInternalFormat = GL_RGB) and
     (Header.GlBaseInternalFormat = GL_RGB) then
  begin
    ReadRGB(Stream);
  end else
  if (Header.GlType = GL_FLOAT) and
     (Header.GlTypeSize = 4) and
     (Header.GlFormat = GL_RED) and
     (Header.GlInternalFormat = GL_R32F) and
     (Header.GlBaseInternalFormat = GL_RED) then
  begin
    ReadFloatImage(Stream);
  end else
  if Header.GlType = GL_HALF_FLOAT then
  begin
    raise EInvalidKTX.Create('Cannot handle KTX with half-float (16-bit floats) data');
  end else
  if (Header.GlType = 0) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = 0) and
     ((Header.GlBaseInternalFormat = GL_RGBA) or (Header.GlBaseInternalFormat = GL_RGB)) then // GL_RGB is needed by ETC1
  begin
    case Header.GlInternalFormat of
      GL_COMPRESSED_RGB_S3TC_DXT1_EXT          : Compression := tcDxt1_RGB;
      GL_COMPRESSED_RGBA_S3TC_DXT1_EXT         : Compression := tcDxt1_RGBA;
      GL_COMPRESSED_RGBA_S3TC_DXT3_EXT         : Compression := tcDxt3;
      GL_COMPRESSED_RGBA_S3TC_DXT5_EXT         : Compression := tcDxt5;

      GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG       : Compression := tcPvrtc1_4bpp_RGB;
      GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG       : Compression := tcPvrtc1_2bpp_RGB;
      GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG      : Compression := tcPvrtc1_4bpp_RGBA;
      GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG      : Compression := tcPvrtc1_2bpp_RGBA;
      GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG      : Compression := tcPvrtc2_4bpp;
      GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG      : Compression := tcPvrtc2_2bpp;

      GL_ATC_RGB_AMD                           : Compression := tcATITC_RGB;
      GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD       : Compression := tcATITC_RGBA_InterpolatedAlpha;
      GL_ATC_RGBA_EXPLICIT_ALPHA_AMD           : Compression := tcATITC_RGBA_ExplicitAlpha;

      GL_ETC1_RGB8_OES                         : Compression := tcETC1;

      GL_COMPRESSED_RGBA_ASTC_4x4_KHR          : Compression := tcASTC_4x4_RGBA;
      GL_COMPRESSED_RGBA_ASTC_5x4_KHR          : Compression := tcASTC_5x4_RGBA;
      GL_COMPRESSED_RGBA_ASTC_5x5_KHR          : Compression := tcASTC_5x5_RGBA;
      GL_COMPRESSED_RGBA_ASTC_6x5_KHR          : Compression := tcASTC_6x5_RGBA;
      GL_COMPRESSED_RGBA_ASTC_6x6_KHR          : Compression := tcASTC_6x6_RGBA;
      GL_COMPRESSED_RGBA_ASTC_8x5_KHR          : Compression := tcASTC_8x5_RGBA;
      GL_COMPRESSED_RGBA_ASTC_8x6_KHR          : Compression := tcASTC_8x6_RGBA;
      GL_COMPRESSED_RGBA_ASTC_8x8_KHR          : Compression := tcASTC_8x8_RGBA;
      GL_COMPRESSED_RGBA_ASTC_10x5_KHR         : Compression := tcASTC_10x5_RGBA;
      GL_COMPRESSED_RGBA_ASTC_10x6_KHR         : Compression := tcASTC_10x6_RGBA;
      GL_COMPRESSED_RGBA_ASTC_10x8_KHR         : Compression := tcASTC_10x8_RGBA;
      GL_COMPRESSED_RGBA_ASTC_10x10_KHR        : Compression := tcASTC_10x10_RGBA;
      GL_COMPRESSED_RGBA_ASTC_12x10_KHR        : Compression := tcASTC_12x10_RGBA;
      GL_COMPRESSED_RGBA_ASTC_12x12_KHR        : Compression := tcASTC_12x12_RGBA;

      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR  : Compression := tcASTC_4x4_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR  : Compression := tcASTC_5x4_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR  : Compression := tcASTC_5x5_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR  : Compression := tcASTC_6x5_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR  : Compression := tcASTC_6x6_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR  : Compression := tcASTC_8x5_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR  : Compression := tcASTC_8x6_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR  : Compression := tcASTC_8x8_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR : Compression := tcASTC_10x5_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR : Compression := tcASTC_10x6_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR : Compression := tcASTC_10x8_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR: Compression := tcASTC_10x10_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR: Compression := tcASTC_12x10_SRGB8_ALPHA8;
      GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR: Compression := tcASTC_12x12_SRGB8_ALPHA8;

      else raise EInvalidKTX.CreateFmt('GPU compression 0x%.4x not supported',
        [Header.GlInternalFormat]);
    end;

    ReadCompressed(Stream, Compression);
  end else
  begin
    raise EInvalidKTX.CreateFmt('Cannot handle this KTX data type: glType 0x%.4x, glTypeSize %d, glFormat 0x%.4x, glInternalFormat 0x%.4x, glBaseInternalFormat 0x%.4x',
      [Header.GlType,
       Header.GlTypeSize,
       Header.GlFormat,
       Header.GlInternalFormat,
       Header.GlBaseInternalFormat]);
  end;
end;

procedure TKTXHandler.LoadFromStream(const Stream: TStream; const URL: string);
var
  I: Integer;
  ImageSize: LongWord;
begin
  try
    Stream.ReadBuffer(Header, SizeOf(Header));

    for I := 0 to High(CorrectFileId) do
      if Header.FileId[I] <> CorrectFileId[I] then
        raise EInvalidKTX.Create('Invalid KTX file header');

    SwapEndianness := Header.Endianness = $01020304;
    if SwapEndianness then
    begin
      Header.Endianness            := Swap32(Header.Endianness           );
      Header.GlType                := Swap32(Header.GlType               );
      Header.GlTypeSize            := Swap32(Header.GlTypeSize           );
      Header.GlFormat              := Swap32(Header.GlFormat             );
      Header.GlInternalFormat      := Swap32(Header.GlInternalFormat     );
      Header.GlBaseInternalFormat  := Swap32(Header.GlBaseInternalFormat );
      Header.Width                 := Swap32(Header.Width                );
      Header.Height                := Swap32(Header.Height               );
      Header.Depth                 := Swap32(Header.Depth                );
      Header.NumberOfArrayElements := Swap32(Header.NumberOfArrayElements);
      Header.NumberOfFaces         := Swap32(Header.NumberOfFaces        );
      Header.NumberOfMipmapLevels  := Swap32(Header.NumberOfMipmapLevels );
      Header.BytesOfKeyValueData   := Swap32(Header.BytesOfKeyValueData  );
    end;

    if Header.Endianness <> $04030201 then
      raise EInvalidKTX.Create('Invalid KTX endianess');

    Width := Header.Width;
    // by using Max(1, ...) for Height we should support 1D textures too
    Height := Max(1, Header.Height);
    Depth := Max(1, Header.Depth);

    RowsTopToBottom := false;
    SlicesFrontToBack := false;

    // Skip BytesOfKeyValueData
    //Stream.Seek(Header.BytesOfKeyValueData, soFromCurrent);

    ReadKeyValues(Stream);

    Stream.ReadBuffer(ImageSize, SizeOf(ImageSize));
    if SwapEndianness then
      ImageSize := Swap32(ImageSize);

    ReadOneImage(Stream);

    // TODO: read mipmaps
    //WritelnLog('KTX', 'Mipmap levels: %d', [Header.NumberOfMipmapLevels]);

    { TODO: For now, we don't support cubemaps.
      But we can read multiple images in case of NumberOfArrayElements > 1. }
    if (Header.NumberOfFaces <= 1) and
       (Header.NumberOfArrayElements > 1) then
    begin
      for I := 1 to Header.NumberOfArrayElements - 1 do
        ReadOneImage(Stream);
    end;
  except
    { EReadError is raised by Stream.ReadBuffer when it can't read
      specified number of bytes }
    on E: EReadError do raise EInvalidKTX.Create('Read error: ' + E.Message);
    on E: ECheckFailed do raise EInvalidKTX.Create('Invalid KTX file: ' + E.Message);
  end;
end;
