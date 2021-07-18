{
  Copyright 2021-2021 Yevhen Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  { ASTC compressed image header
    adapted from https://arm-software.github.io/vulkan-sdk/_a_s_t_c.html }
  {TAstcHeader = packed record
    Magic: array[1..4] of Byte;
    BlockdimX: Byte;
    BlockdimY: Byte;
    BlockdimZ: Byte;
    XSize: array[1..3] of Byte;
    YSize: array[1..3] of Byte;
    ZSize: array[1..3] of Byte;
  end;}
  TAstcInfo = packed record
    BlockdimX: Byte;
    BlockdimY: Byte;
    BlockdimZ: Byte;
    XSize: LongWord;
    YSize: LongWord;
    ZSize: LongWord;
  end;

function LoadASTC(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
const
  MagicAstc : Longword = $5CA1AB13;
var
  Magic: LongWord;
  Word24bit: array[0..2] of Byte;
  Info: TAstcInfo;
  Res: TGPUCompressedImage absolute Result;

  function BlockSizeToTextureCompression: TTextureCompression;
  var
    BlockSizeString: String;
  begin
    BlockSizeString := Info.BlockdimX.ToString + 'x' + Info.BlockdimY.ToString + 'x' + Info.BlockdimZ.ToString;
    case BlockSizeString of
      '4x4x1': Result := tcASTC_4x4_RGBA;
      '5x4x1': Result := tcASTC_5x4_RGBA;
      '5x5x1': Result := tcASTC_5x5_RGBA;
      '6x5x1': Result := tcASTC_6x5_RGBA;
      '6x6x1': Result := tcASTC_6x6_RGBA;
      '8x5x1': Result := tcASTC_8x5_RGBA;
      '8x6x1': Result := tcASTC_8x6_RGBA;
      '8x8x1': Result := tcASTC_8x8_RGBA;
      '10x5x1': Result := tcASTC_10x5_RGBA;
      '10x6x1': Result := tcASTC_10x6_RGBA;
      '10x8x1': Result := tcASTC_10x8_RGBA;
      '10x10x1': Result := tcASTC_10x10_RGBA;
      '12x10x1': Result := tcASTC_12x10_RGBA;
      '12x12x1': Result := tcASTC_12x12_RGBA;
      { WARNING: There doesn't seem to be any way to tell which color space
        was encoded inside the ASTC file }
      {tcASTC_4x4_SRGB8_ALPHA8,
      tcASTC_5x4_SRGB8_ALPHA8,
      tcASTC_5x5_SRGB8_ALPHA8,
      tcASTC_6x5_SRGB8_ALPHA8,
      tcASTC_6x6_SRGB8_ALPHA8,
      tcASTC_8x5_SRGB8_ALPHA8,
      tcASTC_8x6_SRGB8_ALPHA8,
      tcASTC_8x8_SRGB8_ALPHA8,
      tcASTC_10x5_SRGB8_ALPHA8,
      tcASTC_10x6_SRGB8_ALPHA8,
      tcASTC_10x8_SRGB8_ALPHA8,
      tcASTC_10x10_SRGB8_ALPHA8,
      tcASTC_12x10_SRGB8_ALPHA8,
      tcASTC_12x12_SRGB8_ALPHA8}
      else
        raise EInvalidASTC.Create('Unexpected block size in ASTC: ' + BlockSizeString);
    end;
  end;

begin
  try
    Stream.ReadLE(Magic);
    if Magic <> MagicAstc then
      raise Exception.Create('Invalid ASTC header: ' + IntToHex(QWord(Magic), 8) + ' expected: ' + IntToHex(QWord(MagicAstc), 8));

    Stream.ReadBuffer(Info.BlockdimX, 1);
    Stream.ReadBuffer(Info.BlockdimY, 1);
    Stream.ReadBuffer(Info.BlockdimZ, 1);

    Stream.ReadBuffer(Word24bit, 3);
    Info.XSize := Word24Bit[0] + Word24Bit[1] shl 8 + Word24Bit[2] shl 16;
    Stream.ReadBuffer(Word24bit, 3);
    Info.YSize := Word24Bit[0] + Word24Bit[1] shl 8 + Word24Bit[2] shl 16;
    Stream.ReadBuffer(Word24bit, 3);
    Info.ZSize := Word24Bit[0] + Word24Bit[1] shl 8 + Word24Bit[2] shl 16;

    Result := TGPUCompressedImage.Create(Info.XSize, Info.YSize, Info.ZSize,
          BlockSizeToTextureCompression);
    try
      Stream.ReadBuffer(Res.RawPixels^, Res.Size);
      //Res.FlipVertical; Not implemented
    { on unhandled error, make sure to free result }
    except
      FreeAndNil(Result);
      raise;
    end;
  except
  { EReadError is thrown by Stream.ReadBuffer when it can't read
    specified number of bytes }
     on E: EReadError do raise EInvalidASTC.Create('Read error: ' + E.Message);
     on E: ECheckFailed do raise EInvalidASTC.Create('Wrong ASTC: ' + E.Message);
  end;
end;

