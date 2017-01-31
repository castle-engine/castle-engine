{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Given a list of image URLs (as parameters), load and write
  some information about them. Image size, type, alpha channel
  (detailed analysis of alpha: yes/no or full range).
  Can be used as a test of our Images loading capabilities,
  or as a command-line tool similar to ImageMagick "identify". }
program image_identify;

uses SysUtils, CastleUtils, CastleImages, CastleParameters;

var
  I: Integer;
  Img: TEncodedImage;
  AlphaChannel: string;
begin
  if Parameters.High = 0 then
    raise EInvalidParams.Create('No parameters supplied, nothing to do');
  for I := 1 to Parameters.High do
  begin
    try
      Img := LoadEncodedImage(Parameters[I]);
    except
      on E: EImageLoadError do
      begin
        Writeln(Parameters[I], ': load error: ', E.Message);
        Continue;
      end;
      on E: EImageFormatNotSupported do
      begin
        Writeln(Parameters[I], ': image format not supported: ', E.Message);
        Continue;
      end;
    end;
    try
      AlphaChannel := AlphaToString[Img.AlphaChannel];
      case Img.AlphaChannel of
        acTest    : AlphaChannel += ' (only fully transparent / fully opaque parts)';
        acBlending: AlphaChannel += ' (partially transparent parts)';
      end;

      Writeln(Parameters[I], ': ', Img.Width, ' x ', Img.Height,
        ', type: ', Img.ClassName, ', alpha: ' + AlphaChannel);
    finally FreeAndNil(Img) end;
  end;
end.
