{
  Copyright 2016-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Command-line tool to compare 2 images.
  See README.md for details. }

uses SysUtils, CastleUtils, CastleVectors, CastleImages, CastleParameters;
const
  Tolerance = 5;
var
  Image1, Image2: TRGBImage;
  X, Y: Integer;
  Ptr1: PVector3Byte;
  Ptr2: PVector3Byte;
begin
  Parameters.CheckHigh(2);

  Image1 := nil;
  Image2 := nil;
  try
    Image1 := LoadImage(Parameters[1], [TRGBImage]) as TRGBImage;
    Image2 := LoadImage(Parameters[2], [TRGBImage]) as TRGBImage;

    if (Image1.Width <> Image2.Width) or
       (Image1.Height <> Image2.Height) then
    begin
      Writeln(ErrOutput, Format('Image sizes differ: %dx%d vs %dx%d',
        [Image1.Width, Image1.Height,
         Image2.Width, Image2.Height]));
      Halt(1);
    end else
    begin
      Ptr1 := Image1.Pixels;
      Ptr2 := Image2.Pixels;
      { Count Y downward, to show respective to top in error message,
        which is more common for image editing programs. }
      for Y := Integer(Image1.Height) - 1 downto 0 do
        for X := 0 to Integer(Image1.Width) - 1 do
        begin
          if (Abs(Ptr1^[0] - Ptr2^[0]) > Tolerance) or
             (Abs(Ptr1^[1] - Ptr2^[1]) > Tolerance) or
             (Abs(Ptr1^[2] - Ptr2^[2]) > Tolerance) then
          begin
            Writeln(ErrOutput, Format('Image colors differ on pixel (%d,%d) (counted from the top-left): first image has (%d,%d,%d) vs second image (%d,%d,%d)',
              [X, Y, Ptr1^[0], Ptr1^[1], Ptr1^[2], Ptr2^[0], Ptr2^[1], Ptr2^[2]]));
            Halt(1);
          end;
          Inc(Ptr1);
          Inc(Ptr2);
        end;
    end;
  finally
    FreeAndNil(Image1);
    FreeAndNil(Image2);
  end;
end.
