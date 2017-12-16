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
  CastleImages, CastleColors, CastleVectors;

//type
  //TCastleColor = TVector4;
  //TCastleColorRGB = TVector3;
{  TCastleColorGrayscaleAlpha = TVector2;
  TCastleColorGrayscale = single;}


type
  { "native" colors (as in pointers) }
  TCastleColor4Byte = TVector4Byte;
  TCastleColor2Byte = TVector2Byte;


type
  { Contains "Sorter" procedures which calls a proper native
    implementation of each routine.
    Warning: the operation is destructive, it will overwrite the current
    image content, if you want to preserve the initial image
    you will need to create copies manually.
    They are not abstract (as helper currently doesn't allow for virtual methods)
    so they cause a negligible slow-down when called run-time }
  TCastleImageHelper = class helper for TCastleImage
  strict private
    function CastleColorToCastleColor4Byte(aColor: TCastleColor): TCastleColor4Byte;
    function CastleColorToCastleColor2Byte(aColor: TCastleColor): TCastleColor2Byte;
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
    procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
  end;

type
  TRGBImageHelper = class helper for TRGBImage
  public
    procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
  end;

type
  TGrayscaleAlphaImageHelper = class helper for TGrayscaleAlphaImage
  public
    procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
  end;

type
  TGrayscaleImageHelper = class helper for TGrayscaleImage
  public
    procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
  end;

type
  TRGBFloatImageHelper = class helper for TRGBFloatImage
  public
    procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);
  end;

implementation

function TCastleImageHelper.CastleColorToCastleColor4Byte(aColor: TCastleColor): TCastleColor4Byte;
begin
  Result[0] := Trunc(aColor[0]*256);
  Result[1] := Trunc(aColor[1]*256);
  Result[2] := Trunc(aColor[2]*256);
  Result[3] := Trunc(aColor[3]*256);
end;

function TCastleImageHelper.CastleColorToCastleColor2Byte(aColor: TCastleColor): TCastleColor2Byte;
begin
  Result[0] := GrayscaleValue(Vector3Byte(Vector3(aColor[0], aColor[1], aColor[2])));
  Result[1] := Trunc(aColor[3]*256);
end;

procedure TCastleImageHelper.Circle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
begin
  if Self is TRGBAlphaImage then
    TRGBAlphaImage(Self).Circle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TRGBImage then
    TRGBImage(Self).Circle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TGrayscaleAlphaImage then
    TGrayscaleAlphaImage(Self).Circle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TGrayscaleImage then
    TGrayscaleImage(Self).Circle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TRGBFloatImage then
    TRGBFloatImage(Self).Circle(x, y, aRadius, aColor)
  else
  raise EImageDrawError.CreateFmt('Painting is not supported for %s',
        [ClassName]);
end;

procedure TRGBAlphaImageHelper.Circle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
begin

end;

procedure TRGBImageHelper.Circle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
begin

end;

procedure TGrayscaleAlphaImageHelper.Circle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
begin

end;

procedure TGrayscaleImageHelper.Circle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
begin

end;

procedure TRGBFloatImageHelper.Circle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
begin

end;

end.

