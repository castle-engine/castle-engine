{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

function LoadIPL(Stream: TStream): TRGBImage;
{ napisane na podstawie iplab_read.m z www cornell box'a, zreszta napisane
  tylko po to zeby moc odczytac tamtejsze obrazki bo w MatLabie iplab_read.m
  nie dzialalo (teraz juz wiem dlaczego - byl tam blad, typeflag zajmuje dwa 
  bajty (a nie jeden), ale musialem napisac swoja wersje w Pascalu zeby dojsc 
  do tego bledu). }
  
(*  function FiveBitsToByte(fivebits: Word): byte;
  begin
   {liczbe z zkaresu 0..31 (1111 binarnie) (ale podana jako typ Word -
    ponizej bezdiemy chcieli ja mnozyc przez 255 !) mapuje na zakres 0..255}
   result := fivebits*255 div 31;
  end;  *)
  
const
  TypeFlagUChar = 0;
  TypeFlagInt16 = 1;
  TypeFlagInt32 = 2;
  TypeFlagFloat = 3;
  TypeFlagUShort = 6;
var IPLVersion: string;
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
    imgPixel := Result.PixelPtr(i, j);
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
