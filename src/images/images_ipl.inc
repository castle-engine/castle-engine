{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

function LoadIPL(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
{ Implemented looking at iplab_read.m from
  http://www.graphics.cornell.edu/online/box/data.html . }

(*
  { Take a number from 0..31 (1111 in binary) and map it to 0..255 range.
    Parameter FiveBits is a Word, to safely multiply it in implementation. }
  function FiveBitsToByte(fivebits: Word): byte;
  begin
    Result := fivebits*255 div 31;
  end;
*)

const
  TypeFlagUChar = 0;
  TypeFlagInt16 = 1;
  TypeFlagInt32 = 2;
  TypeFlagFloat = 3;
  TypeFlagUShort = 6;
var
  IPLVersion: string;
  IPLFormat: byte;
  IPLTypeFlag: Word;
  IPLWidth, IPLHeight: LongWord;
  i, j: Integer;
  IPLPixel: Word;
  imgPixel: PVector3Byte;
begin
  SetLength(IPLVersion, 4);
  Stream.ReadBuffer(IPLVersion[1], Length(IPLVersion));
  Stream.ReadBuffer(IPLFormat, SizeOf(IPLFormat));
  Check(IPLFormat = 0, 'IPLab file format should be 0');
  Stream.ReadBuffer(IPLTypeFlag, SizeOf(IPLTypeFlag));
  Check((IPLTypeFlag = TypeFlagUShort), 'IPLab file typeflag not supported');

  Stream.ReadBuffer(IPLWidth, SizeOf(IPLWidth));
  Stream.ReadBuffer(IPLHeight, SizeOf(IPLHeight));

  { skip rest of the header }
  Stream.Seek(2105, soFromCurrent);

  result := TRGBImage.Create(IPLWidth, IPLHeight);
  try
    for j := IPLHeight-1 downto 0 do
      for i := 0 to IPLWidth-1 do
      begin
        imgPixel := TCastleImage(Result).PixelPtr(i, j);
        Stream.ReadBuffer(IPLPixel, SizeOf(IPLPixel));
        {imgPixel[0] := FiveBitsToByte((IPLPixel and $7C00) shr 10);
        imgPixel[1] := FiveBitsToByte((IPLPixel and $03E0) shr 5);
        imgPixel[2] := FiveBitsToByte(IPLPixel and $001F);}
        imgPixel^[0] := IPLPixel shr 8;
        imgPixel^[1] := imgPixel^[0];
        imgPixel^[2] := imgPixel^[0];
      end;
  except Result.Free; raise end;
end;
