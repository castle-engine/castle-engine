{
  Copyright 2009-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Write basic information about the image.
  See README.md for details. }
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
        acTest    : AlphaChannel := AlphaChannel + ' (only fully transparent / fully opaque parts)';
        acBlending: AlphaChannel := AlphaChannel + ' (partially transparent parts)';
      end;

      Writeln(Parameters[I], ': ', Img.Width, ' x ', Img.Height,
        ', type: ', Img.ClassName, ', alpha: ' + AlphaChannel);
    finally FreeAndNil(Img) end;
  end;
end.
