{%MainUnit castleimages.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Support for loading and saving images in .castle-image format.

  Features of .castle-image format:

  - .castle-image is a super-simple image file format that matches
    the structure of images in CGE memory.

    After a short binary header (that specified the image class, size)
    the image data follows,
    with layout that matches the memory layout of TEncodedImage.RawPixels
    (on little-endian CPUs).

  - The format can be both loaded and saved extremely efficiently by CGE.

    That's a consequence of the above:
    we can just copy the memory contents of TEncodedImage.RawPixels
    to the file, and back.

  - It can represent any TEncodedImage descendant,
    so both TGPUCompressedImage and TCastleImage.

  - It can represent 3D images (Depth > 1).

  Limitations:

  - For now, it cannot represent
    - mipmaps of the image
    - cubemaps
    (for that, use DDS or KTX).

  - The file size is large -- it's just equal to the size of
    TEncodedImage.RawPixels.

    Which is just uncompressed image data (in case of TCastleImage) or
    GPU-compressed data (in case of TGPUCompressedImage),
    but even GPU-compression algorithms are not very efficient for size
    (that's not their point, their point is to enable decompression
    on GPU of only the necessary data).

    So, this is *not* the format to use if you care
    about file size. Use standard PNG or JPEG instead in that case.
}

type
  { Enum to express image class in .castle-image format. }
  TCastleImageFormatClass = (
    // TGPUCompressedImage
    cicGPUCompressed,

    // 8-bit images
    cicGrayscale,
    cicGrayscaleAlpha,
    cicRGB,
    cicRGBAlpha,

    // float images
    cicGrayscaleFloat,
    cicGrayscaleAlphaFloat,
    cicRGBFloat,
    cicRGBAlphaFloat
  );

  { Header that starts every .castle-image file. }
  TCastleImageFormatHeader = packed record
    { Always "CastleImage" (11 bytes). }
    Initial: array [0 .. 10] of AnsiChar; // "CastleImage"
    { Version number. Always '1' for now,
      we will bump it when we change structure. }
    Version: AnsiChar;
    { Class of the image. }
    ImageClass: UInt8; // casted to TCastleImageFormatClass
    Padding: array [0 .. 2] of Byte; // to align size to 4 bytes
    Width, Height, Depth: UInt32;
    DataSize: UInt32;
    case TCastleImageFormatClass of
      cicGPUCompressed: (
        Compression: UInt8; // casted to TTextureCompression
        Padding2: array [0 .. 2] of Byte; // to align size to 4 bytes
      );
  end;

  ECastleImageFormatLoadError = class(EImageLoadError);

{ Convert TEncodedImageClass to TCastleImageFormatClass. }
function ImageClassToEnum(const ImageClass: TEncodedImageClass): TCastleImageFormatClass;
begin
  if ImageClass = TGPUCompressedImage then
    Result := cicGPUCompressed
  else
  if ImageClass = TGrayscaleImage then
    Result := cicGrayscale
  else
  if ImageClass = TGrayscaleAlphaImage then
    Result := cicGrayscaleAlpha
  else
  if ImageClass = TRGBImage then
    Result := cicRGB
  else
  if ImageClass = TRGBAlphaImage then
    Result := cicRGBAlpha
  else
  if ImageClass = TGrayscaleFloatImage then
    Result := cicGrayscaleFloat
  else
  if ImageClass = TGrayscaleAlphaFloatImage then
    Result := cicGrayscaleAlphaFloat
  else
  if ImageClass = TRGBFloatImage then
    Result := cicRGBFloat
  else
  if ImageClass = TRGBAlphaFloatImage then
    Result := cicRGBAlphaFloat
  else
    raise EInternalError.CreateFmt('Cannot save image class "%s" to .castle-image format', [
      ImageClass.ClassName
    ]);
end;

{ Convert TCastleImageFormatClass to TEncodedImageClass. }
function ImageClassFromEnum(const ImageClassEnum: TCastleImageFormatClass): TEncodedImageClass;
begin
  case ImageClassEnum of
    cicGPUCompressed: Result := TGPUCompressedImage;
    cicGrayscale: Result := TGrayscaleImage;
    cicGrayscaleAlpha: Result := TGrayscaleAlphaImage;
    cicRGB: Result := TRGBImage;
    cicRGBAlpha: Result := TRGBAlphaImage;
    cicGrayscaleFloat: Result := TGrayscaleFloatImage;
    cicGrayscaleAlphaFloat: Result := TGrayscaleAlphaFloatImage;
    cicRGBFloat: Result := TRGBFloatImage;
    cicRGBAlphaFloat: Result := TRGBAlphaFloatImage;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('ImageClassFromEnum?');
    {$endif}
  end;
end;

{ Load image from .castle-image format. }
function Load_CastleImageFormat(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
var
  InitialStr: AnsiString;
  Header: TCastleImageFormatHeader;
  ImageClassEnum: TCastleImageFormatClass;
  ImageClass: TEncodedImageClass;
  Width, Height, Depth, DataSize: Cardinal;
  Compression: TTextureCompression;
begin
  // make sure paddings in TCastleImageFormatHeader are correct
  Assert(SizeOf(TCastleImageFormatHeader) mod 4 = 0);

  Stream.ReadBuffer(Header, SizeOf(Header));

  SetLength(InitialStr, 11);
  Move(Header.Initial, InitialStr[1], 11);
  if Header.Initial <> 'CastleImage' then
    raise ECastleImageFormatLoadError.Create('Invalid .castle-image file, bad initial bytes, expected "CastleImage"');

  if Header.Version <> '1' then
    raise ECastleImageFormatLoadError.Create('Invalid .castle-image file, bad version, expected 1');

  ImageClassEnum := TCastleImageFormatClass(Header.ImageClass);
  ImageClass := ImageClassFromEnum(ImageClassEnum);

  { We ignore AllowedImageClasses now, let outside LoadImage adjust to them.
    We would not have here any other more optimal approach. }

  Width := LEtoN(Header.Width);
  Height := LEtoN(Header.Height);
  Depth := LEtoN(Header.Depth);
  DataSize := LEtoN(Header.DataSize);
  Compression := TTextureCompression(Header.Compression);

  if ImageClass = TGPUCompressedImage then
    Result := TGPUCompressedImage.Create(Width, Height, Depth, Compression)
  else
    Result := TCastleImageClass(ImageClass).Create(Width, Height, Depth);

  if Result.Size <> DataSize then
    raise ECastleImageFormatLoadError.CreateFmt('Invalid .castle-image file, bad data size, expected %d but got (in file) %d', [
      Result.Size,
      DataSize
    ]);

  Stream.ReadBuffer(Result.RawPixels^, DataSize);
end;

{ Save image to .castle-image format. }
procedure Save_CastleImageFormat(const Img: TEncodedImage; const Stream: TStream);
var
  InitialStr: AnsiString;
  Header: TCastleImageFormatHeader;
begin
  InitialStr := 'CastleImage';
  Move(InitialStr[1], Header.Initial, 11);
  Header.Version := '1';
  Header.ImageClass := Ord(ImageClassToEnum(TEncodedImageClass(Img.ClassType)));
  Header.Width := NtoLE(Img.Width);
  Header.Height := NtoLE(Img.Height);
  Header.Depth := NtoLE(Img.Depth);
  Header.DataSize := NtoLE(Img.Size);
  if Img is TGPUCompressedImage then
    Header.Compression := Ord(TGPUCompressedImage(Img).Compression)
  else
    Header.Compression := 0; // just make it non-random in file, to not leak memory contents

  { TODO:
    - This assumes image data is endianess-independent, as we don't convert
      RawPixels between native and little-endian.
      Is this true for float-based images?
      It should be true for others? }

  { Note: We don't make an effort to zero the unimportant (padding) data in
    RawPixels. Code filling image data should never leave inside RawPixels
    random memory contents, even in the (unused) padding bytes,
    otherwise it risks security issues. }

  Stream.WriteBuffer(Header, SizeOf(Header));
  Stream.WriteBuffer(Img.RawPixels^, Img.Size);
end;
