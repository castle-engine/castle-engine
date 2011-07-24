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

{ Remove from DDS mipmaps the levels with one of the sizes < 4.

  This workarounds GIMP-DDS bugs related to this, it generates invalid
  images for them. }
program dds_remove_smallest_mipmaps;

uses SysUtils, KambiUtils, Images, DDS, KambiWarnings, KambiStringUtils;

var
  D: TDDSImage;
  NeedsSave: boolean;
  I: Integer;
begin
  OnWarning := @OnWarningWrite;
  Parameters.CheckHigh(1);
  D := TDDSImage.Create;
  try
    D.LoadFromFile(Parameters[1]);
    if D.DDSType <> dtTexture then
      raise EInvalidParams.Create('For now, this works only for normal (2D) textures in DSS');

    if D.Mipmaps then
    begin
      NeedsSave := false;
      I := 1; { start from 1, to never remove base image }
      while I <= D.Images.High do
      begin
        if (D.Images[I].Width < 4) or
           (D.Images[I].Height < 4) then
        begin
          Writeln(Format('Removing mipmap with size %d x %d.',
            [D.Images[I].Width, D.Images[I].Height]));
          D.Images.FreeAndNil(I);
          D.Images.Delete(I);
          D.MipmapsCount := D.MipmapsCount - 1;
          NeedsSave := true;
        end else
          Inc(I);
      end;

      if NeedsSave then
      begin
        D.SaveToFile(Parameters[1]);
        Writeln('Saved changes.');
      end else
        Writeln('No changes needed (no small mipmaps found).');
    end else
      Writeln('No mipmaps, nothing to do.');
  finally FreeAndNil(D) end;
end.
