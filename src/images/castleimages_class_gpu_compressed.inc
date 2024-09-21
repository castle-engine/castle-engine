{
  Copyright 2001-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Image compressed using one of the GPU texture compression algorithms. }
  TGPUCompressedImage = class(TEncodedImage)
  private
    FCompression: TTextureCompression;
    FSize: Cardinal;
  public
    constructor Create(const AWidth, AHeight, ADepth: Cardinal;
      const ACompression: TTextureCompression);

    property Compression: TTextureCompression read FCompression;

    { Size of the whole image data inside RawPixels, in bytes. }
    function Size: Cardinal; override;

    function HasAlpha: boolean; override;
    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    { Flip compressed image vertically, losslessly.

      This works only for S3TC images, and only when their height is 1, 2, 3
      or a multiple of 4. Note that this is always satisfied if image height
      is a power of two (as common for textures).
      It uses the knowledge of how S3TC compression works
      to losslessly flip the image, without re-compressing it.
      The idea is described here
      [http://users.telenet.be/tfautre/softdev/ddsload/explanation.htm]. }
    procedure FlipVertical; override;

    { Decompress the image.

      This uses DecompressTexture routine.
      By default, it is assigned only when OpenGL(ES) context is available
      and can decompress textures with the help of OpenGL(ES).

      @raises(ECannotDecompressTexture If we cannot decompress the texture,
        because decompressor is not set or there was some other error
        within decompressor.) }
    function Decompress: TCastleImage;

    { Create a new image object that has exactly the same class
      and the same data (size, pixels) as this image. }
    function MakeCopy: TGPUCompressedImage;

    { Create a new image object that has exactly the same class
      and the same data (size, pixels) as this image.

      Equivalent to MakeCopy, but virtual and declared as returning TEncodedImage class. }
    function CreateCopy: TEncodedImage; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TGPUCompressedImage ----------------------------------------------------------------- }

constructor TGPUCompressedImage.Create(
  const AWidth, AHeight, ADepth: Cardinal;
  const ACompression: TTextureCompression);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FDepth := ADepth;
  FCompression := ACompression;

  case Compression of
    { Size formula for S3TC textures:
      All DXT* compression methods compress 4x4 pixels into some constant size.
      When Width / Height is not divisible by 4, we have to round up.

      This matches what MSDN docs say about DDS with mipmaps:
      http://msdn.microsoft.com/en-us/library/bb205578(VS.85).aspx
      When mipmaps are used, DDS Width/Height must be power-of-two,
      so the base level is usually divisible by 4. But on the following mipmap
      levels the size decreases, eventually to 1x1, so this still matters.
      And MSDN says then explicitly that with DXT1, you have always
      minimum 8 bytes, and with DXT2-5 minimum 16 bytes.

      This also means that we cannot simply calculate size of mipmap in a DDS
      by looking at size of base image, and dividing by 2/4/8 as mipmap size
      decreases. We have to calculate size always rounding up to 4x4 block of pixels.
    }
    tcDxt1_RGB, tcDxt1_RGBA:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8 { 8 bytes for each 16 pixels };
    tcDxt3, tcDxt5:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 16 { 16 bytes for each 16 pixels };

    { see https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc2.txt
      for size formula size.
      Note that minimum size is 32 bytes,
      see https://developer.apple.com/library/ios/qa/qa1611/_index.html }
    tcPvrtc1_2bpp_RGB, tcPvrtc1_2bpp_RGBA:
      FSize := Max(32, (FDepth * Max(FWidth, 16) * Max(FHeight, 8) * 2 + 7) div 8);
    tcPvrtc1_4bpp_RGB, tcPvrtc1_4bpp_RGBA:
      FSize := Max(32, (FDepth * Max(FWidth,  8) * Max(FHeight, 8) * 4 + 7) div 8);

    { see https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_compression_pvrtc2.txt
      for size formula source }
    tcPvrtc2_2bpp:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 4) * 8;
    tcPvrtc2_4bpp:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8;

    { see https://www.khronos.org/registry/gles/extensions/AMD/AMD_compressed_ATC_texture.txt
      for size formula source }
    tcATITC_RGB:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8;
    tcATITC_RGBA_ExplicitAlpha,
    tcATITC_RGBA_InterpolatedAlpha:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 16;

    { size formula from
      http://en.wikipedia.org/wiki/Ericsson_Texture_Compression
      "ETC1 takes 4x4 groups of pixel data and compresses each into a single 64-bit word" }
    tcETC1:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 8;

    { size formula from
      https://www.khronos.org/registry/OpenGL/extensions/KHR/KHR_texture_compression_astc_hdr.txt }
    tcASTC_4x4_RGBA, tcASTC_4x4_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 4) * DivRoundUp(FHeight, 4) * 16;
    tcASTC_5x4_RGBA, tcASTC_5x4_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 5) * DivRoundUp(FHeight, 4) * 16;
    tcASTC_5x5_RGBA, tcASTC_5x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 5) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_6x5_RGBA, tcASTC_6x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 6) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_6x6_RGBA, tcASTC_6x6_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 6) * DivRoundUp(FHeight, 6) * 16;
    tcASTC_8x5_RGBA, tcASTC_8x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_8x6_RGBA, tcASTC_8x6_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 6) * 16;
    tcASTC_8x8_RGBA, tcASTC_8x8_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 8) * DivRoundUp(FHeight, 8) * 16;
    tcASTC_10x5_RGBA, tcASTC_10x5_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 5) * 16;
    tcASTC_10x6_RGBA, tcASTC_10x6_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 6) * 16;
    tcASTC_10x8_RGBA, tcASTC_10x8_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 8) * 16;
    tcASTC_10x10_RGBA, tcASTC_10x10_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 10) * DivRoundUp(FHeight, 10) * 16;
    tcASTC_12x10_RGBA, tcASTC_12x10_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 12) * DivRoundUp(FHeight, 10) * 16;
    tcASTC_12x12_RGBA, tcASTC_12x12_SRGB8_ALPHA8:
      FSize := FDepth * DivRoundUp(FWidth, 12) * DivRoundUp(FHeight, 12) * 16;

    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInvalidDDS.CreateFmt('Cannot calculate size for texture compressed with %s',
      [TextureCompressionInfo[Compression].Name]);
    {$endif}
  end;

  FRawPixels := GetMem(FSize);
end;

function TGPUCompressedImage.Size: Cardinal;
begin
  Result := FSize;
end;

function TGPUCompressedImage.HasAlpha: boolean;
begin
  Result := TextureCompressionInfo[Compression].AlphaChannel <> acNone;
end;

function TGPUCompressedImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
begin
  { Compressed data doesn't analyze alpha channel, instead
    we determine alpha channel from the compression type. }
  Result := TextureCompressionInfo[Compression].AlphaChannel;
end;

{$I castleimages_s3tc_flip_vertical.inc}

function TGPUCompressedImage.Decompress: TCastleImage;
begin
  WritelnLog('Decompressing GPU-compressed "%s", this is usually a waste of time for normal games that should load textures in format (compressed or not) suitable for current GPU', [
    Url
  ]);
  if Assigned(DecompressTexture) then
    Result := DecompressTexture(Self)
  else
    raise ECannotDecompressTexture.Create('Cannot decompress GPU-compressed texture: no decompressor initialized');
end;

function TGPUCompressedImage.MakeCopy: TGPUCompressedImage;
begin
  Result := CreateCopy as TGPUCompressedImage;
end;

function TGPUCompressedImage.CreateCopy: TEncodedImage;
begin
  Result := TGPUCompressedImage.Create(Width, Height, Depth, Compression);
  Assert(Result.Size = Size);
  Move(RawPixels^, Result.RawPixels^, Size);
  Result.Url := Url;
end;

{$endif read_implementation}