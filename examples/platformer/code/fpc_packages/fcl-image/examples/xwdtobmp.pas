{
  Converts a xwd image to a bpm image

  Usage: xwdtobmp [source] [dest]

  Author: Felipe Monteiro de Carvalho

  License: Public domain
}
program xwdtobmp;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

{$ifndef fpc}
  {$define win32}
{$endif}


{$ifdef win32}
  {$apptype console}
{$endif}

uses FPWriteBMP, FPReadXWD, classes, FPImage, sysutils;

var
  img : TFPMemoryImage;
  reader : TFPCustomImageReader;
  Writer : TFPCustomimageWriter;
  ReadFile, WriteFile, WriteOptions : string;
begin
  if ParamCount <> 2 then
  begin
    WriteLn('Usage: xwdtobmp [source] [dest]');
    Exit;
  end;

  try
    writeln ('Initing');
    Reader := TFPReaderXWD.Create;
    Writer := TFPWriterBMP.Create;
    TFPWriterBMP(Writer).BitsPerPixel:=32;
    img := TFPMemoryImage.Create(0,0);
    img.UsePalette:=false;
    ReadFile := ParamStr(1);
    WriteFile := ParamStr(2);

    writeln ('Reading image');
    img.LoadFromFile (ReadFile, Reader);

    writeln ('Writing image');
    img.SaveToFile (WriteFile, Writer);

    writeln ('Clean up');
    Reader.Free;
    Writer.Free;
    Img.Free;
  except
    on e : exception do
      writeln ('Error: ',e.message);
  end;
end.

