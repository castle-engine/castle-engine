{
  Copyright 2001-2005 Michalis Kamburelis.

  This file is part of "Kambi's images Pascal units".

  "Kambi's images Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's images Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's images Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

function LoadJPEG(Stream: TStream): TRGBImage;
var MemS: TMemoryStream;
begin
 MemS := TMemoryStream.Create;
 try
  KambiPasJpeg.LoadJPEG(Stream, MemS, true, 0, nil);
  MemS.Position := 0;
  result := LoadBMP(MemS);
 finally MemS.Free end;
end;

procedure SaveJPEG(const img: TRGBImage; Stream: TStream; quality: integer);
var MemS: TMemoryStream;
begin
 MemS := TMemoryStream.Create;
 try
  SaveBMP(img, MemS);
  MemS.Position := 0;
  KambiPasJpeg.StoreJPEG(MemS, Stream, false, quality, nil);
 finally MemS.Free end;
end;

procedure SaveJPEG(const img: TRGBImage; Stream: TStream); { quality = 90 }
begin SaveJPEG(img, Stream, 90) end; 
