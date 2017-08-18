{
  Copyright 2009-2017 Michalis Kamburelis.

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

      GL_RED_INTEGER    = $8D94;

      //GL_R16F           = $822D;
      GL_R32F           = $822E;
      GL_R16UI          = $8234;

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

    procedure ReadG(const Stream: TStream;
      const Image: TGrayscaleImage; const ARowBytePadding: Integer);
    procedure ReadG16(const Stream: TStream;
      const Image: TGrayscaleImage; const ARowBytePadding: Integer);
    procedure ReadAG(const Stream: TStream;
      const Image: TGrayscaleAlphaImage; const ARowBytePadding: Integer);
    procedure ReadRGB(const Stream: TStream;
      const Image: TRGBImage; const ARowBytePadding: Integer);
    procedure ReadBGR(const Stream: TStream;
      const Image: TRGBImage; const ARowBytePadding: Integer);
    procedure ReadRGBA(const Stream: TStream;
      const Image: TRGBAlphaImage);
    procedure ReadBGRA(const Stream: TStream;
      const Image: TRGBAlphaImage);
    procedure ReadFloatImage(const Stream: TStream;
      const Image: TRGBFloatImage);
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

procedure TKTXHandler.ReadG(const Stream: TStream;
  const Image: TGrayscaleImage; const ARowBytePadding: Integer);
var
  Y, Z: Integer;
begin
  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Stream.ReadBuffer(Image.RowPtr(TargetY(Y), Z)^, Image.PixelSize * Image.Width);
      if ARowBytePadding <> 0 then
        Stream.Seek(ARowBytePadding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadG16(const Stream: TStream;
  const Image: TGrayscaleImage; const ARowBytePadding: Integer);
var
  RowData: TWordList;
  TargetRow: PByteArray;
  X, Y, Z: Integer;
begin
  RowData := TWordList.Create;
  try
    RowData.Count := Image.Width;

    for Z := 0 to Image.Depth - 1 do
      for Y := 0 to Image.Height - 1 do
      begin
        Stream.ReadBuffer(RowData.List^, Image.Width * SizeOf(Word));
        { convert row of Words to Bytes }
        TargetRow := Image.RowPtr(TargetY(Y), Z);
        for X := 0 to Image.Width - 1 do
          TargetRow^[X] := RowData.List^[X] shr 8;
         if ARowBytePadding <> 0 then
           Stream.Seek(ARowBytePadding, soFromCurrent);
       end;
  finally FreeAndNil(RowData) end;
end;

procedure TKTXHandler.ReadAG(const Stream: TStream;
  const Image: TGrayscaleAlphaImage; const ARowBytePadding: Integer);
var
  Y, Z: Integer;
begin
  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Stream.ReadBuffer(Image.RowPtr(TargetY(Y), Z)^, Image.PixelSize * Image.Width);
      if ARowBytePadding <> 0 then
        Stream.Seek(ARowBytePadding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadRGB(const Stream: TStream;
  const Image: TRGBImage; const ARowBytePadding: Integer);
var
  Y, Z: Integer;
  Row: PVector3ByteArray;
begin
  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := Image.RowPtr(TargetY(Y), Z);
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      if ARowBytePadding <> 0 then
        Stream.Seek(ARowBytePadding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadBGR(const Stream: TStream;
  const Image: TRGBImage; const ARowBytePadding: Integer);
var
  X, Y, Z: Integer;
  Row: PVector3Byte;
begin
  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := PVector3Byte(Image.RowPtr(TargetY(Y), Z));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      { Now invert red and blue. }
      for X := 0 to Image.Width - 1 do
      begin
        SwapValues(Row^.Data[2], Row^.Data[0]);
        Inc(Row);
      end;
      if ARowBytePadding <> 0 then
        Stream.Seek(ARowBytePadding, soFromCurrent);
    end;
end;

procedure TKTXHandler.ReadRGBA(const Stream: TStream;
  const Image: TRGBAlphaImage);
var
  Y, Z: Integer;
  Row: PVector4ByteArray;
begin
  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := Image.RowPtr(TargetY(Y), Z);
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
    end;
end;

procedure TKTXHandler.ReadBGRA(const Stream: TStream;
  const Image: TRGBAlphaImage);
var
  X, Y, Z: Integer;
  Row: PVector4Byte;
begin
  for Z := 0 to Image.Depth - 1 do
    for Y := 0 to Image.Height - 1 do
    begin
      Row := PVector4Byte(Image.RowPtr(TargetY(Y), Z));
      Stream.ReadBuffer(Row^, Image.PixelSize * Image.Width);
      { Now invert red and blue. }
      for X := 0 to Image.Width - 1 do
      begin
        SwapValues(Row^.Data[2], Row^.Data[0]);
        Inc(Row);
      end;
    end;
end;

procedure TKTXHandler.ReadFloatImage(const Stream: TStream;
  const Image: TRGBFloatImage);
var
  Floats: TSingleList;
  X, Y, Z: Integer;
  Pixel: PVector3;
  PixelSource: PSingle;
begin
  Floats := TSingleList.Create;
  try
    Floats.Count := Image.Width * Image.Height * Image.Depth;
    Stream.ReadBuffer(Floats.List^, Floats.Count * SizeOf(Single));
    PixelSource := PSingle(Floats.List);

    // move Floats data to Image.Pixels
    for Z := 0 to Image.Depth - 1 do
      for Y := 0 to Image.Height - 1 do
      begin
        Pixel := PVector3(Image.RowPtr(TargetY(Y), Z));
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
  if Value = 'S=r,T=d' then
  begin
    RowsTopToBottom := true;
    SlicesFrontToBack := false;
  end else
  if Value = 'S=r,T=u' then
  begin
    RowsTopToBottom := false;
    SlicesFrontToBack := false;
  end else
  if Value = 'S=r,T=d,R=i' then
  begin
    RowsTopToBottom := true;
    SlicesFrontToBack := true;
  end else
  if Value = 'S=r,T=u,R=o' then
  begin
    RowsTopToBottom := false;
    SlicesFrontToBack := false;
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

function TKTXHandler.RowBytePadding(const Image: TCastleImage): Integer;
begin
  Result := Image.PixelSize * Image.Width mod 4;
  if Result <> 0 then
    Result := 4 - Result;
end;

procedure TKTXHandler.ReadOneImage(const Stream: TStream);
var
  GrayImage: TGrayscaleImage;
  RGBAImage: TRGBAlphaImage;
  RGBImage: TRGBImage;
  RGBFloatImage: TRGBFloatImage;
begin
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_LUMINANCE) and
     (Header.GlInternalFormat = GL_LUMINANCE8_EXT) and
     (Header.GlBaseInternalFormat = GL_LUMINANCE) then
  begin
    GrayImage := TGrayscaleImage.Create(Width, Height, Depth);
    Images.Add(GrayImage);
    ReadG(Stream, GrayImage, RowBytePadding(GrayImage));
  end else
  if (Header.GlType = GL_UNSIGNED_SHORT) and
     (Header.GlTypeSize = 2) and
     (Header.GlFormat = GL_RED_INTEGER) and
     (Header.GlInternalFormat = GL_R16UI) and
     (Header.GlBaseInternalFormat = GL_RED_INTEGER) then
  begin
    GrayImage := TGrayscaleImage.Create(Width, Height, Depth);
    Images.Add(GrayImage);
    ReadG16(Stream, GrayImage, RowBytePadding(GrayImage));
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_BGRA) and
     (Header.GlInternalFormat = GL_BGRA) and
     (Header.GlBaseInternalFormat = GL_BGRA) then
  begin
    RGBAImage := TRGBAlphaImage.Create(Width, Height, Depth);
    // when Image.PixelSize is 4, there is never any padding
    Assert(RowBytePadding(RGBAImage) = 0);
    Images.Add(RGBAImage);
    ReadBGRA(Stream, RGBAImage);
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_RGBA) and
     (Header.GlInternalFormat = GL_RGBA) and
     (Header.GlBaseInternalFormat = GL_RGBA) then
  begin
    RGBAImage := TRGBAlphaImage.Create(Width, Height, Depth);
    // when Image.PixelSize is 4, there is never any padding
    Assert(RowBytePadding(RGBAImage) = 0);
    Images.Add(RGBAImage);
    ReadRGBA(Stream, RGBAImage);
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_BGR) and
     (Header.GlInternalFormat = GL_BGR) and
     (Header.GlBaseInternalFormat = GL_BGR) then
  begin
    RGBImage := TRGBImage.Create(Width, Height, Depth);
    Images.Add(RGBImage);
    ReadBGR(Stream, RGBImage, RowBytePadding(RGBImage));
  end else
  if (Header.GlType = GL_UNSIGNED_BYTE) and
     (Header.GlTypeSize = 1) and
     (Header.GlFormat = GL_RGB) and
     (Header.GlInternalFormat = GL_RGB) and
     (Header.GlBaseInternalFormat = GL_RGB) then
  begin
    RGBImage := TRGBImage.Create(Width, Height, Depth);
    Images.Add(RGBImage);
    ReadRGB(Stream, RGBImage, RowBytePadding(RGBImage));
  end else
  if (Header.GlType = GL_FLOAT) and
     (Header.GlTypeSize = 4) and
     (Header.GlFormat = GL_RED) and
     (Header.GlInternalFormat = GL_R32F) and
     (Header.GlBaseInternalFormat = GL_RED) then
  begin
    RGBFloatImage := TRGBFloatImage.Create(Width, Height, Depth);
    // when Image.PixelSize is 4, there is never any padding
    Assert(RowBytePadding(RGBFloatImage) = 0);
    Images.Add(RGBFloatImage);
    ReadFloatImage(Stream, RGBFloatImage);
  end else
  if Header.GlType = GL_HALF_FLOAT then
  begin
    raise EInvalidKTX.Create('Cannot handle KTX with half-float (16-bit floats) data');
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
    Height := Header.Height;
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
  except
    { EReadError is raised by Stream.ReadBuffer when it can't read
      specified number of bytes }
    on E: EReadError do raise EInvalidKTX.Create('Read error: ' + E.Message);
    on E: ECheckFailed do raise EInvalidKTX.Create('Invalid KTX file: ' + E.Message);
  end;
end;
