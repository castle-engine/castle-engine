{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ PPM save/load support. }

{ TODO: When ReadPPMToken is a nested procedure inside LoadPPM (as it should be),
  then loading ppms causes segfaults, only under Windows (not on Linux),
  only with FPC 2.6.2 (not with 2.6.0 or earlier). Probably some FPC 2.6.2 bug.
  Investigate, check if it's still there with FPC trunk, and eventually report.

  The segfault occurs in weird places, and seems to have something to do
  with how ReadPPMToken(Stream) returns a string.

  Occurs with both debug and release build.
  To reproduce, just try to open
  castle_game_engine/tests/data/images/rgb.ppm
  with glViewImage. Or just castle_game_engine/tests/
  (the CastleImage tests there also open rgb.ppm).
}

{ Reads next token in ppm - skips any blanks that it stands on ,
  and reads everything up to the ending whitespace (or to the end of stream).
  Returns token without those surrounding whitespaces.
  Returns '' if we're standing at the end of file.
  Ignores PPM comments (# ... to the nearest #10 or #13) before token. }
function ReadPPMToken(Stream: TStream): string;
var c: char;
begin
 try
  result:='';

  {read whitespaces and comments}
  repeat
   c := StreamReadChar(Stream);
   if c = '#' then { omit comment }
    repeat c := StreamReadChar(Stream) until c in [#10, #13];
  until not (c in WhiteSpaces);

  {read token, until whitespace}
  result := c;
  repeat
   c := StreamReadChar(Stream);
   if c in WhiteSpaces then
    break else
    result := result+c;
  until false;
 except
  on EReadError do {silent exception, just exit from this function};
 end;
end;

function LoadPPM(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
var magic: array[0..1]of char;
    IsBinary: boolean;
    MaxColVal: integer;
    x, y: Cardinal;
    line: PArray_Vector3Byte;
    ppmline: PByteArray;
    ColSize, LineSize: Cardinal;
    AllocateWidth, AllocateHeight: Cardinal;
begin
 try
  Stream.ReadBuffer(magic, 2);
  if (magic[0]<>'P') or ((magic[1]<>'3') and (magic[1]<>'6')) then
   raise ECheckFailed.CreateFmt('Not a PPM beginning : signature is %s%s (#%d #%d)',
     [magic[0], magic[1], Ord(magic[0]), Ord(magic[1])]);
  IsBinary := magic[1] = '6';
  Check(StreamReadChar(Stream) in Whitespaces, 'no whitespace after PPM signature');

  AllocateWidth := StrToInt(ReadPPMToken(Stream));
  AllocateHeight := StrToInt(ReadPPMToken(Stream));
  result := TRGBImage.Create(AllocateWidth, AllocateHeight);
  try
   { one whitespace after MaxColVal is already read by ReadPPMToken }
   MaxColVal := StrToInt(ReadPPMToken(Stream));

   if IsBinary then
   begin
    if MaxColVal < 256 then ColSize := 1 else ColSize := 2;
    LineSize := ColSize * TCastleImage(Result).PixelSize * Result.Width;

    {osobny kod dla ColSize =1 i =2 (dla szybkosci).}
    if ColSize = 1 then
    begin
     for y := result.Height-1 downto 0 do
     begin
      line := TCastleImage(Result).RowPtr(y);
      Stream.ReadBuffer(line^, LineSize);
      if MaxColVal <> 255 then
      begin
       for x := 0 to (Result.Width*3)-1 do
        PByteArray(line)^[x] := Integer(PByteArray(line)^[x])*255 div MaxColVal;
      end;
     end;
    end else
    begin
     ppmline := GetMem(LineSize);
     try
      for y := result.Height-1 downto 0 do
      begin
       line := TCastleImage(Result).RowPtr(y);
       Stream.ReadBuffer(ppmline[0], LineSize);
       for x := 0 to result.Width-1 do
       begin
        {Zawsze odczytany wynik rzutujemy na Integer zeby pomnozony * 255 nie wyszedl
         poza swoj zakres. Zamieniamy go na zakres 0..255, dzielac przez MaxColVal. }
        line^[x, 0]:=(Integer(ppmline^[x*6  ])*256+ppmline^[x*6+1])*255 div MaxColVal;
        line^[x, 1]:=(Integer(ppmline^[x*6+2])*256+ppmline^[x*6+3])*255 div MaxColVal;
        line^[x, 2]:=(Integer(ppmline^[x*6+4])*256+ppmline^[x*6+5])*255 div MaxColVal;
       end;
      end;
     finally FreeMem(ppmline) end;
    end;
   end else
   begin
    for y := result.Height-1 downto 0 do
    begin
     line := TCastleImage(Result).RowPtr(y);
     for x := 0 to result.Width-1 do
     begin
      line^[x, 0] := StrToInt(ReadPPMToken(Stream))*255 div MaxColVal;
      line^[x, 1] := StrToInt(ReadPPMToken(Stream))*255 div MaxColVal;
      line^[x, 2] := StrToInt(ReadPPMToken(Stream))*255 div MaxColVal;
     end;
    end;
   end;
  except Result.Free; raise end;
 except
  on E: EReadError do raise EInvalidPPM.Create('Read error : '+E.Message);
  on E: ECheckFailed do raise EInvalidPPM.Create('Wrong PPM file : '+E.Message);
  on E: EConvertError do raise EInvalidPPM.Create('Convert error : '+E.Message);
 end;
end;

procedure SavePPM(Img: TEncodedImage; Stream: TStream; Binary: boolean);
var line: PArray_Vector3Byte;
    x, y: cardinal;
begin
  if not (Img is TRGBImage) then
    raise EImageSaveError.CreateFmt('Saving to PPM image class %s not possible', [Img.ClassName]);

 {najbezpieczniej w ppm uzywac znakow konca linii #10.
  To dlatego ze po MaxColVal wymagany jest DOKLADNIE jeden bialy znak. }
 if binary then
  WriteStr(Stream,'P6') else
  WriteStr(Stream,'P3');
 WriteStr(Stream, Format(#10'%d %d'#10'%d'#10, [img.Width, img.Height, 255]));
 for y := img.Height-1 downto 0 do
 begin
  line := TCastleImage(Img).RowPtr(y);
  if binary then
   Stream.WriteBuffer(line^, img.Width*SizeOf(TVector3Byte)) else
  begin
   for x := 0 to img.Width-1 do
   begin
    WriteStr(Stream, Format('%d %d %d ', [line^[x, 0], line^[x, 1], line^[x, 2]]));

    {linia powinna miec maksymalnie 70 znakow. Acha.
     Kazdy nasz pixel to max 4(liczba 0..255 + spacja) * 3 = 12 znakow.
     Wiec piszmy 5 pixeli w linii - maksymalnie linia bedzie miala 5*12 = 60 znakow. }
    if (x+1) mod 5 = 0 then WriteStr(Stream, #10);
   end;
  end;
 end;
end;

procedure SavePPM(Img: TEncodedImage; Stream: TStream); { binary = true }
begin SavePPM(img, Stream, true) end;
