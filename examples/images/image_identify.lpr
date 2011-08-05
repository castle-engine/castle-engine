{
  Copyright 2009-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Given a list of image filenames (as parameters), load and identify them.
  "To identify" means that it outputs basic image information,
  like width x height and to what memory format we load it.

  This can be used to simply test our Images unit loading capabilities
  on supplied images. }
program image_identify;

uses SysUtils, KambiUtils, Images, KambiParameters;

var
  I: Integer;
  Img: TImage;
begin
  if Parameters.High = 0 then
    raise EInvalidParams.Create('No parameters supplied, nothing to do');
  for I := 1 to Parameters.High do
  begin
    try
      Img := LoadImage(Parameters[I], [], []);
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
      Writeln(Parameters[I], ': ', Img.Width, ' x ', Img.Height, ' - ', Img.ClassName);
    finally FreeAndNil(Img) end;
  end;
end.
