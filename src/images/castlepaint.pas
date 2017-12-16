{
  Copyright 2001-2017 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(* Paint extension for CastleImages unit and must be used with it. *)

unit CastlePaint;

{$mode objfpc}{$H+}

interface

uses
  CastleImages, CastleColors;

type
  { Contains "Sorter" procedures which calls a proper native
    implementation of each routine.
    Warning: the operation is destructive, it will overwrite the current
    image content, if you want to preserve the initial image
    you will need to create copies manually.
    They are not abstract (as helper currently doesn't allow for virtual methods)
    so they cause a negligible slow-down when called run-time }
  TCastleImageHelper = class helper for TCastleImage
  public
    { draws a hollow circle at x,y with aRadius radius with antialiasing.
      aColor is treated differently depending on specific
      image type it is drawn over }
    procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);
  end;

type
  TRGBAlphaImageHelper = class helper for TRGBAlphaImage
  public
  end;

type
  TRGBImageHelper = class helper for TRGBImage
  public
  end;

type
  TGrayscaleAlphaImageHelper = class helper for TGrayscaleAlphaImage
  public
  end;

type
  TGrayscaleImageHelper = class helper for TGrayscaleImage
  public
  end;

type
  TRGBFloatImageHelper = class helper for TRGBFloatImage
  public
  end;

implementation

procedure TCastleImageHelper.Circle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
begin
  if Self is TRGBAlphaImage then
    TRGBAlphaImage(Self).Circle(x, y, aRadius, aColor)
  else
  if Self is TRGBImage then
    TRGBImage(Self).Circle(x, y, aRadius, aColor)
  else
  if Self is TGrayscaleAlphaImage then
    TRGBImage(Self).Circle(x, y, aRadius, aColor)
  else
  if Self is TGrayscaleImage then
    TRGBImage(Self).Circle(x, y, aRadius, aColor)
  else
  if Self is TRGBFloatImage then
    TRGBImage(Self).Circle(x, y, aRadius, aColor)
  else
  raise EImageDrawError.CreateFmt('Painting is not supported for %s',
        [ClassName]);
end;

end.

