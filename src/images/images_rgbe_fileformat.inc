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

{ LoadRGBE and SaveRGBE coded based on the RGBE format description inside
  the Radiance documentation.

  The RGBE encoding for a single pixel is explained by Greg Ward in
  Graphic Gems II.5.

  The specification of new RLE encoding in RGBE (more-or-less based on
  Glassner in Graphic Gems II.8):
  - ThisByte > 128 means to repeat NextByte by ThisByte-128 times
  - ThisByte <= 128 means that an unpacked bytes follow.
  - ThisByte = 0 is not allowed.

  The specification of old RLE encoding in RGBE:
  - a special unnormalized pixel (does not have a mantissa >= 0.5 * 256)
    looking like this: (1, 1, 1, RepeatCount) means to repeat
    the last pixel RepeatCount times. Consecutive pixels of this form
    add the more significant bytes of the repeat count,
    IOW the next (1, 1, 1, NextRepeatCount) means to repeat "NextRepeatCount shl 8"
    times, the next next (1, 1, 1, NextNextRepeatCount)
    means to repeat "NextNextRepeatCount shl 16" times and so on.
}

function LoadRGBE(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
var LoadResult: TCastleImage absolute result;

  { Read from Stream one scanline (of the length result.Width) to RGBERow. }
  procedure ReadRGBERow(RGBERow: PArray_Vector4Byte);

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

  function ClassAllowed(ImageClass: TEncodedImageClass): boolean;
  begin
    Result := CastleImages.ClassAllowed(ImageClass, AllowedImageClasses);
  end;

var
  line: string;
  { jesli false to kazdy pixel to RGBE. Wpp. kazdy pixel w pliku to XYZE }
  xyzFormat: boolean;
  x, y, RGBEWidth, RGBEHeight: Integer;
  RGBERow: PArray_Vector4Byte;
  FloatRow: PArray_Vector3Single;
  ByteRow: PArray_Vector3Byte;
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
    RGBERow := GetMem(SizeOf(TVector4Byte) * RGBEWidth);
    try
      if ClassAllowed(TRGBFloatImage) then
      begin
        Result := TRGBFloatImage.Create(RGBEWidth, RGBEHeight);
        for y := result.Height-1 downto 0 do
        begin
          ReadRGBERow(RGBERow);
          FloatRow := TCastleImage(Result).RowPtr(y);
          for x := 0 to result.Width - 1 do
            FloatRow^[x] := VectorRGBETo3Single(RGBERow^[x]);
        end;
      end else
      if ClassAllowed(TRGBImage) then
      begin
        Result := TRGBImage.Create(RGBEWidth, RGBEHeight);
        for y := result.Height-1 downto 0 do
        begin
          ReadRGBERow(RGBERow);
          ByteRow := TCastleImage(Result).RowPtr(y);
          for x := 0 to result.Width - 1 do
            ByteRow^[x] := Vector3Byte( VectorRGBETo3Single(RGBERow^[x]) );
        end;
      end else
        raise EUnableToLoadImage.CreateFmt('Cannot load RGBE file to %s', [LoadEncodedImageParams(AllowedImageClasses)]);
    finally FreeMemNiling(Pointer(RGBERow)) end;
  except Result.Free; raise end;
end;

procedure SaveRGBE(Img: TEncodedImage; Stream: TStream);
var
  PByte: PVector3Byte;
  PFloat: PVector3Single;
  x, y: Integer;
  RGBE: TVector4Byte;
begin
  if not ((Img is TRGBImage) or (Img is TRGBFloatImage)) then
    raise EImageSaveError.CreateFmt('Saving to RGBE image class %s not possible', [Img.ClassName]);

  WriteStr(Stream,'#?RADIANCE'#10 +
                  'FORMAT=32-bit_rle_rgbe'#10 +
                  'SOFTWARE=' + ApplicationName + #10 +
                  #10 +
                  '-Y '+IntToStr(Img.Height)+' +X '+IntToStr(Img.Width)+#10);

  { Theoretically, I could use above '+Y' instead of '-Y'.
    '+Y' is more natural ordering for me, consistent with OpenGL and my TEncodedImage.

    However, it turns out that many (all?) programs cannot handle orientation
    other than (-Y, +X). This applies even for ximage from Radiance,
    or display from ImageMagick. Because of this, even our own LoadRGBE above
    handles only (-Y, +X). So, honestly, it seems that there exists no program
    able to handle anything else than (-Y, +X). }

  if Img is TRGBImage then
  begin
    for y := Img.Height - 1 downto 0 do
    begin
      PByte := TCastleImage(Img).RowPtr(y);
      for x := 0 to Img.Width - 1 do
      begin
        RGBE := Vector3ToRGBE( Vector3Single(PByte^) );
        Stream.WriteBuffer(RGBE, SizeOf(RGBE));
        Inc(PByte);
      end;
    end;
  end else
  begin
    for y := Img.Height - 1 downto 0 do
    begin
      PFloat := TCastleImage(Img).RowPtr(y);
      for x := 0 to Img.Width - 1 do
      begin
        RGBE := Vector3ToRGBE(PFloat^);
        Stream.WriteBuffer(RGBE, SizeOf(RGBE));
        Inc(PFloat);
      end;
    end;
  end;
end;
