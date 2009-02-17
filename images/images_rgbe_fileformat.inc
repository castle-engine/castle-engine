{
  Copyright 2003-2009 Michalis Kamburelis.

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

{ LoadRGBE i SaveRGBE napisane na podstawie opisu formatu RGBE w
  /win/docs/radiance/refer/filefmts.pdf, dostepnym w Internecie
  ze stron Radiance'a. Zaczete przy okazji zabaw w wakacje po RGK.

  Format pixela RGBE jest wyjasniony przez Grega Warda w Graphic Gems II.5.

  Proste kodowane RLE zrobione w RGBE (mowimy tu o nowej wersji
  kodowania RGBE) (mniej wiecej na podstawie Glassnera, Graphic Gems II.8) :
  - bajt > 128 oznacza ze nastepny_bajt nalezy powtorzyc bajt-128 razy
  - bajt <= 128 oznacza ze teraz nastepuje ciag bajt bajtow ktory
    nie jest spakowany, tzn. ten ciag bajtow nalezy literalnie przekopiowac
    do docelowego miejsca.
  - wartosc bajt = 0 jest zabroniona.

  Stare kodowanie RLE:
  - nieznormalizowany (nie ma mantysy >= 0.5 * 256) pixel = (1, 1, 1, RepeatCount)
    mowi zeby powtorzyc ostatni pixel RepeatCount razy. Kolejne pixele tej postaci
    podaja coraz starsze bajty liczby powtorzen, tzn. kolejne RepeatCount
    nalezy przesuwac o 8, 16, itd. bitow w lewo.
}

function LoadRGBE(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
var LoadResult: TImage absolute result;

  procedure ReadRGBERow(RGBERow: PArray_Vector4Byte);
  { odczytaj ze Stream jedna scanline (dlugosci result.Width) do RGBERow }

    function CompressedLineMarker(const Marker: TVector4Byte): boolean;
    begin
     result := (Marker[0] = 2) and (Marker[1] = 2) and ((Marker[2] and $80) = 0);
     Assert((not result) or (LoadResult.Width =
       LongWord((Marker[2] shl 8) or Marker[3])));
    end;

    procedure ReadRLECompressedScanlineChannel(Pixel: PByteArray);
    { odczytaj z aktualnego strumienia skompresowany RLE ciag bajtow.
      Kolejne bajty beda zapisywane do Pixel^[0], Pixel^[4], Pixel^[8] itd.,
      az do Pixel^[(LoadResult.Width-1)*4]. }
    var i: Integer;
        x: Cardinal;
        ByteMarker, ByteRepeat: byte;
    begin
     x := 0; { ile kolumn juz zapisalem do Pixel }
     while x < LoadResult.Width do
     begin
      Stream.ReadBuffer(ByteMarker, SizeOf(Byte));
      if ByteMarker > 128 then
      begin
       Stream.ReadBuffer(ByteRepeat, SizeOf(Byte));
       for i := 1 to ByteMarker-128 do
        begin Pixel^[x*4] := ByteRepeat; Inc(x) end;
      end else
      begin
       if ByteMarker = 0 then
        raise EInvalidRGBE.Create('ByteMarker starting RLE run is = 0');
       for i := 1 to ByteMarker do
        begin Stream.ReadBuffer(Pixel^[x*4], SizeOf(Byte)); Inc(x) end;
      end;
     end;
    end;

    procedure ReadOldRLECompressedScanline;
    { Wiemy ze pierwszy pixel zostal juz odczytany do RGBERow^[0].
      Odczytujemy reszte. DoneCount okresla ile pixeli juz zapisalimy
      do RGBERow, zawsze RGBERow^[DoneCount-1] to ostatni zapisany
      pixel (wiec jesli znajdziemy gdzies pixel z trzema mantysami = 1
      kazacy nam "powtarzac" to musimy powtarzac wlasnie RGBERow^[DoneCount-1].) }
    var RepeatCount, DoneCount, ShiftRepeatCount, i: Cardinal;
	RepeatPixel: TVector4Byte;
    begin
     DoneCount := 1;
     ShiftRepeatCount := 0; { w wyniku czytania kilku pixeli (1, 1, 1, RepeatCount)
       pod rzad bedziemy zwiekszali ShiftRepeatCount o 8. }
     while DoneCount < LoadResult.Width do
     begin
      Stream.ReadBuffer(RGBERow^[DoneCount], SizeOf(TVector4Byte));
      if (RGBERow^[DoneCount, 0] = 1) and
         (RGBERow^[DoneCount, 1] = 1) and
	 (RGBERow^[DoneCount, 2] = 1) then
      begin
       RepeatPixel := RGBERow^[DoneCount-1];
       RepeatCount := LongWord(RGBERow^[DoneCount, 3]) shl ShiftRepeatCount;
       ShiftRepeatCount := ShiftRepeatCount + 8;
       for i := 1 to RepeatCount do
       begin
        RGBERow^[DoneCount] := RepeatPixel;
	Inc(DoneCount);
       end;
      end else
      begin
       Inc(DoneCount);
       ShiftRepeatCount := 0;
      end;
     end;
    end;

  var i: Integer;
  begin
   { na pewno result.Width > 0 wiec w scanline jest min 1 pixel i mamy
     zawsze dostapne RGBERow^[0] }
   Stream.ReadBuffer(RGBERow^[0], SizeOf(RGBERow^[0]));
   if CompressedLineMarker(RGBERow^[0]) then
   begin
    for i := 0 to 3 do ReadRLECompressedScanlineChannel(@(RGBERow^[0, i]));
   end else
   begin
    { file is not RLE compressed or is RLE compressed using old fashion
      (three mantisas = 1). Moglbym odczytac tu pierwsza wersje pliku
      (gdyby nie zalezalo mi nawet na zabezpieczeniu sie przed wersja
      druga) prostym Stream.ReadBuffer(RGBERow^[0]^, 4*(LoadResult.Width-1));

      Tym niemniej w koncu zaimplementowalem stare RLE. }
    ReadOldRLECompressedScanline;
   end;
  end;

  { DoingConversion and ClassAllowed are only shortcuts to
    global utilities. }
  procedure DoingConversion(const Conv: TImageLoadConversion);
  begin
    Images.DoingConversion(Conv, ForbiddenConvs);
  end;

  function ClassAllowed(ImageClass: TImageClass): boolean;
  begin
    Result := Images.ClassAllowed(ImageClass, AllowedImageClasses);
  end;

var
  line: string;
  { jesli false to kazdy pixel to RGBE. Wpp. kazdy pixel w pliku to XYZE }
  xyzFormat: boolean;
  x, y, RGBEWidth, RGBEHeight: Integer;
  RGBERow: PArray_Vector4Byte;
  ImageRow: PArray_Vector3Byte;
begin
  { read header }
  { klauzula FORMAT= moze nie wystapic w pliku, wtedy przyjmij xyzFormat = false }
  xyzFormat := false;
  repeat
    line := StreamReadUpto_NotEOS(Stream, [#10]);
    if line='FORMAT=32-bit_rle_xyze' then xyzFormat := true;
  until line = '';

  Check(not xyzFormat, 'TODO: reading XYZE images not implemented yet. '+
    'Use ra_xyze (from Radiance programs) to convert this image to rgbe.');

  { read resolution string. As many other programs (ximage, ImageMagick's display)
    we don't support any other resolution than -Y * +X * - TODO }
  line := StreamReadUpto_NotEOS(Stream, [#10]);
  DeFormat(line, '-Y %d +X %d', [@RGBEHeight, @RGBEWidth], false);

  Result := nil;
  try
    if ClassAllowed(TRGBEImage) then
    begin
      Result := TRGBEImage.Create(RGBEWidth, RGBEHeight);
      for y := result.Height-1 downto 0 do
        ReadRGBERow( Result.RowPtr(y) );
    end else
    if ClassAllowed(TRGBImage) then
    begin
      DoingConversion(ilcFloatPrecDelete);
      Result := TRGBImage.Create(RGBEWidth, RGBEHeight);

      RGBERow := GetMem(SizeOf(TVector4Byte) * result.Width);
      try
        for y := result.Height-1 downto 0 do
        begin
          ReadRGBERow(RGBERow);
          ImageRow := PArray_Vector3Byte(Result.RowPtr(y));
          for x := 0 to result.Width-1 do
            ImageRow^[x] := Vector3Byte( VectorRGBETo3Single(RGBERow^[x]) )
        end;
      finally FreeMemNiling(Pointer(RGBERow)) end;
    end else
      raise EUnableToLoadImage.CreateFmt('Cannot load RGBE file to %s', [LoadImageParams(AllowedImageClasses, ForbiddenConvs)]);
  except Result.Free; raise end;
end;

procedure SaveRGBE(Img: TImage; Stream: TStream);
var pPixelRGB: PVector3Byte;
    x, y: Integer;
    rgbe: TVector4Byte;
begin
 if not ((Img is TRGBImage) or (Img is TRGBEImage)) then
   raise EUnableToSaveImage.CreateFmt('Saving to RGBE image class %s not possible', [Img.ClassName]);

 WriteStr(Stream,'#?RADIANCE'#10+
                 'FORMAT=32-bit_rle_rgbe'#10+
		 'SOFTWARE='+ProgramName+' by Michalis Kamburelis (Kambi)'#10+
		 #10+
		 '-Y '+IntToStr(Img.Height)+' +X '+IntToStr(Img.Width)+#10);

 { Ktos moglby pomyslec ze powinienem napisac powyzej '+Y' zamiast '-Y',
   bo przeciez taki format jest zgodny z kolejnoscia pixli w Img.
   Ale nie wszystkie programy umieja odczytywac obrazki zapisane w orientacji
   innej niz (-Y, +X) (nie umie nawet ximage Radiance'a ani display z
   ImageMagicka wiec prawde mowiac w ogole nie wiem jaki program umie;
   moje wlasne tez nie, I didn't implement it since it's not used by anything. }

 if Img is TRGBImage then
 begin
  for y := Img.Height-1 downto 0 do
  begin
   pPixelRGB := Img.RowPtr(y);
   for x := 0 to Img.Width-1 do
   begin
    rgbe := Vector3ToRGBE( Vector3Single(pPixelRGB^) );
    Stream.WriteBuffer(rgbe, SizeOf(rgbe));
    Inc(pPixelRGB);
   end;
  end;
 end else
 begin
  for y := Img.Height-1 downto 0 do
   Stream.WriteBuffer(Img.RowPtr(y)^, SizeOf(TVector4Byte) * Img.Width);
 end;
end;

{ simple helper funcs ---------------------------------------- }

procedure SaveRGBEFromByteRGB(const Img: TRGBImage; Stream: TStream);
begin
 SaveRGBE(Img, Stream);
end;
