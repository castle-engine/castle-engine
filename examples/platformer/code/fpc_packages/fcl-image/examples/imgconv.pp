{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Image conversion example.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
program ImgConv;

{_$define UseFile}

uses FPWriteXPM, FPWritePNG, FPWriteBMP,fpreadgif,fptiffcmn,
     FPReadXPM, FPReadPNG, FPReadBMP, fpreadjpeg,fpwritejpeg,
     fpreadtga,fpwritetga,fpreadpnm,fpwritepnm, fpreadtiff, fpwritetiff,
     {$ifndef UseFile}classes,{$endif}
     FPImage, sysutils;

var img : TFPMemoryImage;
    reader : TFPCustomImageReader;
    Writer : TFPCustomimageWriter;
    ReadFile, WriteFile, WriteOptions : string;

procedure Init;
var t : char;
begin
  if paramcount = 4 then
    begin
    T := upcase (paramstr(1)[1]);
    if T = 'X' then
      Reader := TFPReaderXPM.Create
    else if T = 'B' then
      Reader := TFPReaderBMP.Create
    else if T = 'J' then
      Reader := TFPReaderJPEG.Create
    else if T = 'G' then
      Reader := TFPReaderGif.Create
    else if T = 'P' then
      Reader := TFPReaderPNG.Create
    else if T = 'T' then
      Reader := TFPReaderTarga.Create
    else if T = 'F' then
      Reader := TFPReaderTiff.Create
    else if T = 'N' then
      Reader := TFPReaderPNM.Create
    else
      begin
      Writeln('Unknown file format : ',T);
      Halt(1);
      end;
    ReadFile := paramstr(2);
    WriteOptions := paramstr(3);
    WriteFile := paramstr(4);
    end
  else
    begin
    Reader := nil;
    ReadFile := paramstr(1);
    WriteOptions := paramstr(2);
    WriteFile := paramstr(3);
    end;
  WriteOptions := uppercase (writeoptions);
  T := WriteOptions[1];
  if T = 'X' then
    Writer := TFPWriterXPM.Create
  else if T = 'B' then
    begin
    Writer := TFPWriterBMP.Create;
    TFPWriterBMP(Writer).BitsPerPixel:=32;
    end
  else if T = 'J' then
    Writer := TFPWriterJPEG.Create
  else if T = 'P' then
    Writer := TFPWriterPNG.Create
  else if T = 'T' then
    Writer := TFPWriterTARGA.Create
  else if T = 'F' then
    Writer := TFPWriterTiff.Create
  else if T = 'N' then
    Writer := TFPWriterPNM.Create
  else
    begin
    Writeln('Unknown file format : ',T);
    Halt(1);
    end;
  img := TFPMemoryImage.Create(0,0);
  img.UsePalette:=false;
end;

procedure ReadImage;
{$ifndef UseFile}var str : TStream;{$endif}
begin
  if assigned (reader) then
    img.LoadFromFile (ReadFile, Reader)
  else
    {$ifdef UseFile}
    img.LoadFromFile (ReadFile);
    {$else}
    if fileexists (ReadFile) then
      begin
      str := TFileStream.create (ReadFile,fmOpenRead);
      try
        img.loadFromStream (str);
      finally
        str.Free;
      end;
      end
    else
      writeln ('File ',readfile,' doesn''t exists!');
    {$endif}
end;

procedure WriteImage;
var t : string;
begin
  t := WriteOptions;
  writeln (' WriteImage, options=',t);
  if (t[1] = 'P') then
    with (Writer as TFPWriterPNG) do
      begin
      Grayscale := pos ('G', t) > 0;
      Indexed := pos ('I', t) > 0;
      WordSized := pos('W', t) > 0;
      UseAlpha := pos ('A', t) > 0;
      writeln ('Grayscale ',Grayscale, ' - Indexed ',Indexed,
               ' - WordSized ',WordSized,' - UseAlpha ',UseAlpha);
      end
  else if (t[1] = 'F') then
    with (Writer as TFPWriterTiff) do
      begin
      if pos ('G', t) > 0 then
         begin
         Img.Extra[TiffPhotoMetric]:='0';
         if Pos('8',T)>0 then
           Img.Extra[TiffGrayBits]:='8'
         else if Pos('16',T)>0 then
           Img.Extra[TiffGrayBits]:='16';
         Writeln(TiffPhotoMetric,': 0 ',TiffGrayBits,': ',Img.Extra[TiffGrayBits]);
         end;
      end
  else if (t[1] = 'X') then
    begin
    if length(t) > 1 then
    with (Writer as TFPWriterXPM) do
      begin
      ColorCharSize := ord(t[2]) - ord('0');
      end;
    end;
  writeln ('Options checked, now writing...');
  img.SaveToFile (WriteFile, Writer);
end;

procedure Clean;
begin
  Reader.Free;
  Writer.Free;
  Img.Free;
end;

begin
  if (paramcount <> 4) and (paramcount <> 3) then
    begin
    writeln ('Give filename to read and to write, preceded by filetype:');
    writeln ('X for XPM, P for PNG, B for BMP, J for JPEG, T for TGA,');
    writeln ('N for PNM (read only), F for TIFF, G for gif (read only)');
    writeln ('example: imgconv X hello.xpm P hello.png');
    writeln ('example: imgconv hello.xpm P hello.png');
    writeln ('Options for');
    writeln ('  PNG :  G : grayscale, A : use alpha, ');
    writeln ('         I : Indexed in palette, W : Word sized.');
    writeln ('  TIFF :  G16 write grayscale 16 bits/pixel');
    writeln ('          G8 write grayscale 16 bits/pixel');
    writeln ('  XPM :  Number of chars to use for 1 pixel');
    writeln ('  The color size of an XPM can be set after the X as 1,2,3 or 4');
    writeln ('example: imgconv hello.xpm PIA hello.png');
    writeln ('example: imgconv hello.png X2 hello.xpm');
    end
  else
    try
      writeln ('Initing');
      Init;
      writeln ('Reading image');
      ReadImage;
      writeln ('Writing image');
      WriteImage;
      writeln ('Clean up');
      Clean;
    except
      on e : exception do
        writeln ('Error: ',e.message);
    end;
end.
