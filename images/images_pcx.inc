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

{ Stary kod jeszcze z dosowego unitu image6.
  Laduje pcxy 256 kolorowe.
  Zrobiony na podstawie ksiazki "sztuczki programowania gier" czy jakos
   tak, dolaczam go do unitu Images dla czystej przyjemnosci bawienia
   sie wlasnym starym kodem - nie zamierzam usprawniac obslugi pcx'ow
   w najblizszym czasie. }

function LoadPCX(Stream: TStream): TRGBImage;

  type
    TPCXHeader = packed record
      manufacturer,
      version,
      encoding,
      bitsPerPixel: byte;
      x0, y0,
      width, height: Smallint;
      XPelsPerMeter,
      YPelsPerMeter: Smallint;
      EGAPalette: packed array[1..16]of TVector3Byte;
      reserved,
      Planes: byte;
      BytesPerLine,
      paletteType: Smallint;
      filler: array[1..58]of byte;
    end;

var head: TPCXHeader;
    bajt: byte;
    y: integer;
    X, num_end_byte: Cardinal;
    palette: packed array[0..255]of TVector3Byte;

  procedure PalettePut(x, y, palIndex: integer);
  { ustaw na wspolrzednych x, y result.data kolor palette[palIndex] }
  begin
   Result.PixelPtr(x, y)^ := palette[palIndex];
  end;

begin
 {czytaj header i sprawdz jego poprawnosc}
 Stream.ReadBuffer(head, SizeOf(head));
 with head do
 begin
  if (manufacturer <> 10) or (encoding <> 1) then
   raise EInvalidPCX.Create('Invalid PNG file header, manufacturer <> 10 or encoding <> 1');
  if (bitsPerPixel <> 8) or (planes <> 1) then
   raise EInvalidPCX.Create('Only 256 coloured (8 bits per pixel) PCX images with Planes = 1'
     +' are implemented, TODO');

  height := height-x0; {uwzgledniamy tutaj, ze moze byc np. x0, y0=(1, 1) } { TODO: test it }
  width := width-y0;
 end;

 { czytaj pelete }
 Stream.Seek(-768, soFromEnd);
 Stream.ReadBuffer(palette, SizeOf(palette));
 Stream.Position := 128;

 { TODO -- czy to jest ok ze idziemy od 0 do height /width
   zamiast do height -1 /width-1 ? }

 result := TRGBImage.Create(head.width+1, head.height+1);

 {czytamy zasadnicza zawartosc :}
 for y := Result.Height - 1 downto 0 do
 begin
  x := 0;
  while x < Result.Width do
  begin
   Stream.ReadBuffer(bajt, 1);
   if bajt < 192 then
    begin PalettePut(x, y, bajt); Inc(x) end else
    begin
     { I must secure here: num_end_byte is max Result.Width - 1.
       Otherwise my program could do wrong memory access when
       given invalid image, and this is not acceptable.
       I have a sample of such invalid image (see
       /win/images/format_tests/pcx/ANATHE_256.PCX). }
     num_end_byte := KambiUtils.Min(x + bajt - 192 - 1, Result.Width - 1);
     Stream.ReadBuffer(bajt, 1);
     while x <= num_end_byte do begin PalettePut(x, y, bajt); Inc(x); end;
    end;
  end;
 end;
end;
