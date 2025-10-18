{%MainUnit castleinternalcompositeimage.pas}
{
  Copyright 2009-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ KTX loading and saving.
  See https://castle-engine.io/ktx for KTX overview and how to use
  it in Castle Game Engine.

  See KTX specs https://registry.khronos.org/KTX/ .
  This file implements KTX 1, https://registry.khronos.org/KTX/specs/1.0/ktxspec.v1.html  }

{ Log do we use fast or slow speed-path for loading / saving.
  "Fast speed path" means we can use 1x ReadBuffer call to move entire data
  between TStream and TEncodedImage.RawPixels, so this is ideal situation. }
{.$define CASTLE_KTX_LOG_SPEED}

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
        BytesOfKeyValueData: UInt32;
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

      GL_RGB8           = $8051;
      GL_SRGB8          = $8C41;

      GL_RGBA8 = $8058;

      GL_R16            = $822A;
      GL_RG16           = $822C;
      GL_RGB16          = $8054;
      GL_RGBA16         = $805B;

      GL_RG8            = $822B;
      GL_RG             = $8227;
      //GL_R16F           = $822D;

      GL_R32F           = $822E;
      GL_RG32F          = $8230;
      GL_RGB32F         = $8815;
      GL_RGBA32F        = $8814;

      GL_R16UI          = $8234;
      GL_RG16UI         = $823A;
      GL_RGB16UI        = $8D77;
      GL_RGBA16UI       = $8D76;

      GL_SRGB8_ALPHA8   = $8C43;

      GL_RED_INTEGER    = $8D94;
      GL_RG_INTEGER     = $8228;
      GL_RGB_INTEGER    = $8D98;
      GL_RGBA_INTEGER   = $8D99;
      //GL_BGR_INTEGER  = $8D9A; // for now unused
      //GL_BGRA_INTEGER = $8D9B; // for now unused

    var
      Header: TKTXHeader;
      SwapEndianness: boolean;
      RowsTopToBottom, SlicesFrontToBack: boolean;

    procedure ReadKeyValues(const Stream: TStream);
    procedure ReadOrientation(const Value: String);

    { Bytes to skip after each row.

      Following the KTX specification:
      "Uncompressed texture data matches a GL_UNPACK_ALIGNMENT of 4."
      Note that our TCastleImage uses no padding
      (see src/base_rendering/castleglimages_packing.inc ),
      though in practice it is often equivalent to padding by 4
      (since if you have 8-bit RGBA data, or 32-bit float data,
      or image width divisible by 4, then "align to 4 bytes"
      is the same as "no padding at all"). }
    function RowBytePadding(const Image: TCastleImage): Integer;

    { Read one image (one TEncodedImage) from Stream. }
    procedure ReadOneImage(const Stream: TStream; const W, H, D: Cardinal);

    { Optionally invert Y, depending on RowsTopToBottom.
      H should be height of the current image. }
    function TargetY(const Y: Integer; const H: Cardinal): Integer;

    { Optionally invert Z, depending on SlicesFrontToBack.
      D should be depth of the current image. }
    function TargetZ(const Z: Integer; const D: Cardinal): Integer;

    { Read one image, assuming it is using GPU compression.

      @returns Image, for convenience. }
    function ReadOneImage_Compressed(const Stream: TStream;
      const Image: TGPUCompressedImage): TGPUCompressedImage;

    { Read one image where:
      - the memory format for each pixel is the same as the on-disk format,
      - but we potentially need to:
        - invert rows (maybe RowsTopToBottom = true),
        - slices (maybe SlicesFrontToBack = true),
        - or skip padding (maybe RowBytePadding is non-zero).

      @returns Image, for convenience. }
    function ReadOneImage_Universal(const Stream: TStream;
      const Image: TCastleImage): TCastleImage;

    { Read one image where:
      - the memory format for each pixel is 16-bit integer,
        but we need to convert it to 32-bit float,
      - and we potentially need to:
        - invert rows (maybe RowsTopToBottom = true),
        - slices (maybe SlicesFrontToBack = true),
        - or skip padding (maybe RowBytePadding is non-zero).

      @returns Image, for convenience. }
    function ReadOneImage_16ToFloat(const Stream: TStream;
      const Image: TCastleImage): TCastleImage;

    { Swap red<->blue in TRGBImage or TRGBAlphaImage.

      @returns Image, for convenience. }
    function SwapRedBlue(const Image: TCastleImage): TCastleImage;

    { Write to Stream one TEncodedImage. }
    procedure WriteOneImage(const Stream: TStream; const Image: TEncodedImage);

    {$ifdef CASTLE_KTX_LOG_SPEED}
    procedure LogSpeed(const Image: TEncodedImage; const S: String);
    {$endif}
  public
    procedure LoadFromStream(const Stream: TStream; const Url: String); override;
    procedure SaveToStream(const Stream: TStream); override;
  end;

{$ifdef CASTLE_KTX_LOG_SPEED}
procedure TKTXHandler.LogSpeed(const Image: TEncodedImage; const S: String);
begin
  WritelnLog('KTX', '%s(%dx%dx%d): %s', [
    Image.ClassName,
    Image.Width, Image.Height, Image.Depth,
    S
  ]);
end;
{$endif}

function TKTXHandler.TargetY(const Y: Integer; const H: Cardinal): Integer;
begin
  if RowsTopToBottom then
    Result := H - Y - 1
  else
    Result := Y;
end;

function TKTXHandler.TargetZ(const Z: Integer; const D: Cardinal): Integer;
begin
  if SlicesFrontToBack then
    Result := D - Z - 1
  else
    Result := Z;
end;

function TKTXHandler.ReadOneImage_Universal(const Stream: TStream;
  const Image: TCastleImage): TCastleImage;
var
  W, H, D: Cardinal;
  Y, Z: Integer;
  Row: Pointer;
  Padding: Integer;
begin
  Result := Image;
  W := Image.Width;
  H := Image.Height;
  D := Image.Depth;

  Padding := RowBytePadding(Image);

  if (Padding = 0) and (not RowsTopToBottom) and (not SlicesFrontToBack) then
  begin
    {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'FAST'); {$endif}
    Stream.ReadBuffer(Image.RawPixels^, Image.Size);
  end else
  begin
    {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'SLOW (needs to invert rows, slices, or has padding)'); {$endif}
    for Z := 0 to D - 1 do
      for Y := 0 to H - 1 do
      begin
        Row := Image.RowPtr(TargetY(Y, H), TargetZ(Z, D));
        Stream.ReadBuffer(Row^, Image.PixelSize * W);
        if Padding <> 0 then
          Stream.Seek(Padding, soFromCurrent);
      end;
  end;
end;

function TKTXHandler.ReadOneImage_16ToFloat(
  const Stream: TStream; const Image: TCastleImage): TCastleImage;
var
  W, H, D: Cardinal;
  RowData: TWordList;
  TargetRow: PFloat;
  X, Y, Z: Integer;
  Padding: Integer;
begin
  Result := Image;
  W := Image.Width;
  H := Image.Height;
  D := Image.Depth;

  {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'SLOW (needs to convert 16-bit to 32-bit float)'); {$endif}

  Padding := RowBytePadding(Image);

  RowData := TWordList.Create;
  try
    RowData.Count := W;

    for Z := 0 to D - 1 do
      for Y := 0 to H - 1 do
      begin
        Stream.ReadBuffer(RowData.L^, W * Image.ColorComponentsCount * SizeOf(Word));
        { convert row of Words to Floats }
        TargetRow := Image.RowPtr(TargetY(Y, H), TargetZ(Z, D));
        for X := 0 to W * Image.ColorComponentsCount - 1 do
          TargetRow[X] := RowData.L[X] / High(Word);
         if Padding <> 0 then
           Stream.Seek(Padding, soFromCurrent);
       end;
  finally FreeAndNil(RowData) end;
end;

function TKTXHandler.SwapRedBlue(const Image: TCastleImage): TCastleImage;
var
  W, H, D: Cardinal;
  X, Y, Z: Integer;
  PixelPtr: PVector3Byte;
begin
  Result := Image;
  W := Image.Width;
  H := Image.Height;
  D := Image.Depth;

  {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'SLOWDOWN (needs to swap red<->blue)'); {$endif}

  for Z := 0 to D - 1 do
    for Y := 0 to H - 1 do
    begin
      PixelPtr := PVector3Byte(Image.RowPtr(Y, Z));
      for X := 0 to W - 1 do
        SwapValues(PixelPtr^.Z, PixelPtr^.X);
      Inc(PByte(PixelPtr), Image.ColorComponentsCount);
    end;
end;

procedure TKTXHandler.ReadOrientation(const Value: String);
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

  {$ifdef CASTLE_KTX_LOG_SPEED}
  WritelnLog('KTX', 'Got KTX orientation "%s". RowsTopToBottom: %s, SlicesFrontToBack: %s.', [
    Value,
    BoolToStr(RowsTopToBottom, true),
    BoolToStr(SlicesFrontToBack, true)
  ]);
  {$endif CASTLE_KTX_LOG_SPEED}
end;

procedure TKTXHandler.ReadKeyValues(const Stream: TStream);
var
  RemainingToRead, KeyAndValueByteSize, ValuePadding: UInt32;
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

      Key := PAnsiChar(@(KeyAndValue[0]));
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

function TKTXHandler.ReadOneImage_Compressed(const Stream: TStream;
  const Image: TGPUCompressedImage): TGPUCompressedImage;
begin
  {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'FAST'); {$endif}
  Result := Image;
  Stream.ReadBuffer(Image.RawPixels^, Image.Size);

  if RowsTopToBottom then
  begin
    Image.FlipVertical;
    {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'SLOWDOWN (needs to vertically flip)'); {$endif}
  end;
end;

function TKTXHandler.RowBytePadding(const Image: TCastleImage): Integer;
begin
  Result := Image.PixelSize * Image.Width mod 4;
  if Result <> 0 then
    Result := 4 - Result;
end;

procedure TKTXHandler.ReadOneImage(const Stream: TStream; const W, H, D: Cardinal);
var
  Compression: TTextureCompression;

  { Do the GlType etc. fields of the header match. }
  function IsType(const GlType, GlTypeSize, GlFormat, GlInternalFormat,
    GlBaseInternalFormat: UInt32): Boolean;
  begin
    Result :=
      (Header.GlType = GlType) and
      (Header.GlTypeSize = GlTypeSize) and
      (Header.GlFormat = GlFormat) and
      (Header.GlInternalFormat = GlInternalFormat) and
      (Header.GlBaseInternalFormat = GlBaseInternalFormat);
  end;

begin
  { 8-bit image formats, no RB swap }
  if IsType(GL_UNSIGNED_BYTE, 1, GL_LUMINANCE, GL_LUMINANCE8_EXT, GL_LUMINANCE) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TGrayscaleImage.Create(W, H, D)));
  end else
  if IsType(GL_UNSIGNED_BYTE, 1, GL_RG, GL_RG8, GL_RG) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TGrayscaleAlphaImage.Create(W, H, D)));
  end else
  if IsType(GL_UNSIGNED_BYTE, 1, GL_RGB, GL_RGB, GL_RGB) or
     { The "sized internal format" GL_RGB8 is recommended by KTX 1 spec. }
     IsType(GL_UNSIGNED_BYTE, 1, GL_RGB, GL_RGB8, GL_RGB) or
     { TODO: for now SRGB8 is read the same way,
        we ignore information that it is in sRGB color-space.
        Whether the image is assumed to be in sRGB or linear color-space
        depends in CGE on the image function (whether it is placed in
        e.g. TPhysicalMaterial.BaseTexture or NormalTexture)
        and global @link(ColorSpace) variable. }
     IsType(GL_UNSIGNED_BYTE, 1, GL_RGB, GL_SRGB8, GL_RGB) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TRGBImage.Create(W, H, D)));
  end else
  if IsType(GL_UNSIGNED_BYTE, 1, GL_RGBA, GL_RGBA, GL_RGBA) or
     IsType(GL_UNSIGNED_BYTE, 1, GL_RGBA, GL_RGBA8, GL_RGBA) or
     { Testcase: rgba-reference.ktx from https://github.com/KhronosGroup/KTX-Software/tree/master/tests/testimages
       Note that we ignore sRGB here for now, we read it just like regular image.
       In CGE we don't have a control per-image for sRGB, yet. }
     IsType(GL_UNSIGNED_BYTE, 1, GL_RGBA, GL_SRGB8_ALPHA8, GL_RGBA) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TRGBAlphaImage.Create(W, H, D)));
  end else

  { 8-bit image formats, with RB swap }

  if IsType(GL_UNSIGNED_BYTE, 1, GL_BGRA, GL_BGRA, GL_BGRA) then
  begin
    Images.Add(SwapRedBlue(ReadOneImage_Universal(Stream, TRGBAlphaImage.Create(W, H, D))));
  end else
  if IsType(GL_UNSIGNED_BYTE, 1, GL_BGR, GL_BGR, GL_BGR) then
  begin
    Images.Add(SwapRedBlue(ReadOneImage_Universal(Stream, TRGBImage.Create(W, H, D))));
  end else

  { 16-bit image formats }

  if { You can generate this with
      - PowerVR Texture Tool GUI with "Encode" to R16
      - PowerVR Texture Tool CLI with "-f r16,US" encoding
     }
     IsType(GL_UNSIGNED_SHORT, 2, GL_RED_INTEGER, GL_R16UI, GL_RED_INTEGER) or
     { You can generate this with
       - PowerVR Texture Tool CLI with "-f r16,USN" encoding
     }
     IsType(GL_UNSIGNED_SHORT, 2, GL_RED, GL_R16, GL_RED) then
  begin
    Images.Add(ReadOneImage_16ToFloat(Stream, TGrayscaleFloatImage.Create(W, H, D)));
  end else
  if IsType(GL_UNSIGNED_SHORT, 2, GL_RG_INTEGER, GL_RG16UI, GL_RG_INTEGER) or
     IsType(GL_UNSIGNED_SHORT, 2, GL_RG, GL_RG16, GL_RG) then
  begin
    // TODO: Should be good, but untested
    Images.Add(ReadOneImage_16ToFloat(Stream, TGrayscaleAlphaFloatImage.Create(W, H, D)));
  end else
  if IsType(GL_UNSIGNED_SHORT, 2, GL_RGB_INTEGER, GL_RGB16UI, GL_RGB_INTEGER) or
     IsType(GL_UNSIGNED_SHORT, 2, GL_RGB, GL_RGB16, GL_RGB) then
  begin
    // TODO: Should be good, but untested
    Images.Add(ReadOneImage_16ToFloat(Stream, TRGBFloatImage.Create(W, H, D)));
  end else
  if IsType(GL_UNSIGNED_SHORT, 2, GL_RGBA_INTEGER, GL_RGBA16UI, GL_RGBA_INTEGER) or
     IsType(GL_UNSIGNED_SHORT, 2, GL_RGBA, GL_RGBA16, GL_RGBA) then
  begin
    // TODO: Should be good, but untested
    Images.Add(ReadOneImage_16ToFloat(Stream, TRGBAlphaFloatImage.Create(W, H, D)));
  end else

  { float-based image formats }

  if IsType(GL_FLOAT, SizeOf(Single), GL_RED, GL_R32F, GL_RED) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TGrayscaleFloatImage.Create(W, H, D)));
  end else
  if IsType(GL_FLOAT, SizeOf(Single), GL_RG, GL_RG32F, GL_RG) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TGrayscaleAlphaFloatImage.Create(W, H, D)));
  end else
  if IsType(GL_FLOAT, SizeOf(Single), GL_RGB, GL_RGB32F, GL_RGB) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TRGBFloatImage.Create(W, H, D)));
  end else
  if IsType(GL_FLOAT, SizeOf(Single), GL_RGBA, GL_RGBA32F, GL_RGBA) then
  begin
    Images.Add(ReadOneImage_Universal(Stream, TRGBAlphaFloatImage.Create(W, H, D)));
  end else

  { Not supported: half floats }

  if Header.GlType = GL_HALF_FLOAT then
  begin
    raise EInvalidKTX.Create('Cannot handle KTX with half-float (16-bit floats) data');
  end else

  { GPU-Compressed images }

  if (Header.GlType = 0) and
     { Header.GlTypeSize is commonly 1,
       but Header.GlTypeSize = 0 is used by Apple, see https://github.com/castle-engine/castle-image-viewer/issues/1 .
       Spec doesn't clarify it for compressed textures ( http://github.khronos.org/KTX-Specification/ )
       so just ignore checking GlTypeSize below. }
     // (Header.GlTypeSize = 1) and
     (Header.GlFormat = 0) and
     ((Header.GlBaseInternalFormat = GL_RGBA) or (Header.GlBaseInternalFormat = GL_RGB)) then // GL_RGB is needed by ETC1
  begin
    if not OpenGLToTextureCompression(Header.GlInternalFormat, Compression) then
      raise EInvalidKTX.CreateFmt('GPU compression 0x%.4x not supported', [
        Header.GlInternalFormat
      ]);

    Images.Add(ReadOneImage_Compressed(Stream, TGPUCompressedImage.Create(W, H, D, Compression)));
  end else
  begin
    if (Header.GlType = 0) and (Header.GlFormat = 0) then
    begin
      raise EInvalidKTX.CreateFmt('Cannot read data from this KTX (compressed) format.' + NL +
        'Details:' + NL +
        '  glTypeSize %d' + NL +
        '  glInternalFormat 0x%.4x' + NL +
        '  glBaseInternalFormat 0x%.4x', [
        Header.GlTypeSize,
        Header.GlInternalFormat,
        Header.GlBaseInternalFormat
      ]);
    end else
    begin
      raise EInvalidKTX.CreateFmt('Cannot read data from this KTX (uncompressed) format.' + NL +
        'Details:' + NL +
        '  glType 0x%.4x' + NL +
        '  glTypeSize %d' + NL +
        '  glFormat 0x%.4x' + NL +
        '  glInternalFormat 0x%.4x' + NL +
        '  glBaseInternalFormat 0x%.4x', [
        Header.GlType,
        Header.GlTypeSize,
        Header.GlFormat,
        Header.GlInternalFormat,
        Header.GlBaseInternalFormat
      ]);
    end;
  end;
end;

procedure TKTXHandler.LoadFromStream(const Stream: TStream; const Url: String);

  procedure ReadMipmapLevel(const W, H, D: Cardinal);
  var
    I: Integer;
    ImageSize: UInt32;
  begin
    Stream.ReadBuffer(ImageSize, SizeOf(ImageSize));
    if SwapEndianness then
      ImageSize := Swap32(ImageSize);

    ReadOneImage(Stream, W, H, D);

    { TODO: For now, we don't support cubemaps.
      But we can read multiple images in case of NumberOfArrayElements > 1. }
    if (Header.NumberOfFaces <= 1) and
       (Header.NumberOfArrayElements > 1) then
    begin
      for I := 1 to Header.NumberOfArrayElements - 1 do
        ReadOneImage(Stream, W, H, D);
    end;

    { Note: Ignoring mipPadding from KTX spec
      https://www.khronos.org/registry/KTX/specs/1.0/ktxspec_v1.html
      as it should be 0 in practice. }
  end;

var
  I: Integer;
  W, H, D: Cardinal;
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

      if Header.GlTypeSize > 1 then
        WritelnWarning('We need to swap for endianess the texture data (GlTypeSize > 1) but this is not implemented yet.');
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

    { interpret mipmap information }
    if Header.NumberOfMipmapLevels = 0 then
    begin
      WritelnWarning('TODO: Not supported KTX numberOfMipmapLevels = 0 (indicates we should generate mipmaps at KTX loading)');
      Header.NumberOfMipmapLevels := 1;
    end;
    Composite.FMipmaps := Header.NumberOfMipmapLevels > 1;
    Composite.FMipmapsCount := Header.NumberOfMipmapLevels;
    { Check that Width/Height are power of two, this is needed to make
      sure images reading code is sensible (otherwise, there's no way
      to tell what are the sizes of following images). }
    if Composite.FMipmaps then
    begin
      if (not IsPowerOf2(Width)) or
         (not IsPowerOf2(Height)) then
      begin
        WritelnWarning('KTX image has mipmaps, but width or height are not a power of 2: %d x %d. We will ignore these mipmaps, since we cannot determine their size', [Width, Height]);
        Composite.FMipmaps := false;
      end;
    end;
    //WritelnLog('KTX', 'Mipmap levels: %d', [Composite.FMipmapsCount]);

    W := Width;
    H := Height;
    D := Depth;
    for I := 0 to Composite.FMipmapsCount - 1 do
    begin
      ReadMipmapLevel(W, H, D);
      W := Max(1, W div 2);
      H := Max(1, H div 2);
      D := Max(1, D div 2);
    end;
  except
    { EReadError is raised by Stream.ReadBuffer when it can't read
      specified number of bytes }
    on E: EReadError do raise EInvalidKTX.Create('Read error: ' + E.Message);
    on E: ECheckFailed do raise EInvalidKTX.Create('Invalid KTX file: ' + E.Message);
  end;
end;

const
  { Bytes to write as padding. }
  ZeroBytes: array [0..3] of Byte = (0, 0, 0, 0);

procedure TKTXHandler.WriteOneImage(const Stream: TStream; const Image: TEncodedImage);
var
  Y, Z: Integer;
  Img: TCastleImage;
begin
  if (Image is TGPUCompressedImage) or
     (RowBytePadding(Image as TCastleImage) = 0) then
  begin
    {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'SAVE: FAST'); {$endif}
    Stream.WriteBuffer(Image.RawPixels^, Image.Size);
  end else
  begin
    {$ifdef CASTLE_KTX_LOG_SPEED} LogSpeed(Image, 'SAVE: SLOW (needs to add row padding)'); {$endif}
    Img := Image as TCastleImage;
    for Z := 0 to Image.Depth - 1 do
      for Y := 0 to Image.Height - 1 do
      begin
        Stream.WriteBuffer(Img.RowPtr(Y)^, Img.Width * Img.PixelSize);
        Stream.WriteBuffer(ZeroBytes, RowBytePadding(Img));
      end;
  end;
end;

procedure TKTXHandler.SaveToStream(const Stream: TStream);
var
  Image: TEncodedImage;

  { Calculate 5 TKTXHeader fields that correspond to the image type:
    GlType, GlTypeSize, GlFormat, GlInternalFormat, GlBaseInternalFormat. }
  procedure CalculateHeaderImageType;

    procedure HeaderSetType(const GlType, GlTypeSize, GlFormat, GlInternalFormat, GlBaseInternalFormat: UInt32);
    begin
      Header.GlType := GlType;
      Header.GlTypeSize := GlTypeSize;
      Header.GlFormat := GlFormat;
      Header.GlInternalFormat := GlInternalFormat;
      Header.GlBaseInternalFormat := GlBaseInternalFormat;
    end;

  var
    Compression: TTextureCompression;
    HasAlpha: Boolean;
  begin
    { 8-bit image formats }
    if Image is TGrayscaleImage then
      HeaderSetType(GL_UNSIGNED_BYTE, 1, GL_LUMINANCE, GL_LUMINANCE8_EXT, GL_LUMINANCE)
    else
    if Image is TGrayscaleAlphaImage then
      HeaderSetType(GL_UNSIGNED_BYTE, 1, GL_RG, GL_RG8, GL_RG)
    else
    if Image is TRGBImage then
      HeaderSetType(GL_UNSIGNED_BYTE, 1, GL_RGB, GL_RGB8, GL_RGB)
    else
    if Image is TRGBAlphaImage then
      HeaderSetType(GL_UNSIGNED_BYTE, 1, GL_RGBA, GL_RGBA8, GL_RGBA)
    else

    { float image formats }
    if Image is TGrayscaleFloatImage then
      HeaderSetType(GL_FLOAT, SizeOf(Single), GL_RED, GL_R32F, GL_RED)
    else
    if Image is TGrayscaleAlphaFloatImage then
      HeaderSetType(GL_FLOAT, SizeOf(Single), GL_RG, GL_RG32F, GL_RG)
    else
    if Image is TRGBFloatImage then
      HeaderSetType(GL_FLOAT, SizeOf(Single), GL_RGB, GL_RGB32F, GL_RGB)
    else
    if Image is TRGBAlphaFloatImage then
      HeaderSetType(GL_FLOAT, SizeOf(Single), GL_RGBA, GL_RGBA32F, GL_RGBA)
    else

    { GPU-compressed image formats }
    if Image is TGPUCompressedImage then
    begin
      Compression := TGPUCompressedImage(Image).Compression;
      HasAlpha := TextureCompressionInfo[Compression].AlphaChannel <> acNone;
      HeaderSetType(0, 1, 0, TextureCompressionToOpenGL(Compression), Iff(HasAlpha, GL_RGBA, GL_RGB));
    end else

      raise EInvalidCompositeImage.CreateFmt('Cannot save to KTX format, unsupported image class %s', [
        Image.ClassName
      ]);
  end;

const
  KeyValueName = 'KTXorientation';
  // Match KTX orientation with our optimal directions: RowsTopToBottom = false, SlicesFrontToBack = false
  KeyValueValue = 'S=r,T=u';
  KeyValueData = KeyValueName + #0 + KeyValueValue + #0;

  { We only write out single key-pair.
    Calculate properties of it, and set Header.BytesOfKeyValueData. }
  procedure CalculateHeaderKeyValuePairs(out KeyAndValueByteSize, ValuePadding: UInt32);
  begin
    KeyAndValueByteSize := Length(KeyValueData);
    // padding of KeyAndValueByteSize to 4 bytes
    ValuePadding := (4 - (KeyAndValueByteSize mod 4)) mod 4;
    Header.BytesOfKeyValueData := KeyAndValueByteSize +
      ValuePadding +
      SizeOf(KeyAndValueByteSize);
  end;

var
  KeyAndValueByteSize, ValuePadding, ImageSize, RowByteSize: UInt32;
begin
  if Images.Count = 0 then
    raise EInvalidCompositeImage.Create('Cannot save to KTX format, no images (TCompositeImage.Images.Count = 0)');
  if Images.Count > 1 then
    WritelnWarning('Saving to KTX more than 1 image not supported yet, only the 1st image in TCompositeImage.Images will be saved');
  Image := Images[0];

  { Initialize some fields. These should not be really used by writing
    routines (that can assume these values), but let's initialize them to be safe. }
  RowsTopToBottom := false;
  SlicesFrontToBack := false;
  SwapEndianness := false;

  FillChar(Header, SizeOf(Header), 0);
  Move(CorrectFileId, Header.FileId, SizeOf(Header.FileId));
  Header.Endianness := $04030201;
  Header.Width := Image.Width;
  // KTX spec says to use 0 for 1D images
  Header.Height := Iff(Image.Height = 1, 0, Image.Height);
  // KTX spec says to use 0 for 1D and 2D images
  Header.Depth := Iff(Image.Depth = 1, 0, Image.Depth);
  Header.NumberOfArrayElements := 0;
  Header.NumberOfFaces := 1; // for non-cubemaps this should be 1
  Header.NumberOfMipmapLevels := 1; // for non-mipmapped textures

  CalculateHeaderImageType;
  CalculateHeaderKeyValuePairs(KeyAndValueByteSize, ValuePadding);

  Stream.WriteBuffer(Header, SizeOf(Header));

  // write one key-value pair with KTXorientation
  Stream.WriteBuffer(KeyAndValueByteSize, SizeOf(KeyAndValueByteSize));
  Stream.WriteBuffer(KeyValueData[1], Length(KeyValueData));
  // add padding from ValuePadding
  Stream.WriteBuffer(ZeroBytes, ValuePadding);

  if Image is TCastleImage then
  begin
    { Calculate row size with padding, since KTX requires 4-byte alignment
      while our data has no alignment. }
    RowByteSize := Image.Width * TCastleImage(Image).PixelSize +
      RowBytePadding(TCastleImage(Image));
    ImageSize := RowByteSize * Image.Height * Image.Depth;
  end else
    ImageSize := Image.Size;
  Stream.WriteBuffer(ImageSize, SizeOf(ImageSize));

  WriteOneImage(Stream, Image);
end;
