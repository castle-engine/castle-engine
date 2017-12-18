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
    Warning: Color is treated differently depending on the image it is painted over.
    They are not abstract (as helper currently doesn't allow for virtual methods)
    so they cause a negligible slow-down when called run-time }
  TCastleImageHelper = class helper for TCastleImage
  strict private
    function BlendColor(const c1, c2: byte; const t: single): byte; {$IFDEF Supports_Inline}Inline;{$ENDIF}
    function CastleColorToCastleColor4Byte(aColor: TCastleColor): TCastleColor4Byte;
    function CastleColorToCastleColor2Byte(aColor: TCastleColor): TCastleColor2Byte;
  public
    { draws a filled circle at x,y with aRadius radius with antialiasing. }
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);
    { same as Circle, but the Circle is hollow }
    {procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);}
    { same as FillCircle but much faster, without antialiasing }
    {procedure QuickCircle(const x, y: integer; const aRadius: single;
      const aColor: TCastleColor);}

    { draws a hollow rectangle at x1,y1 - x2,y2 with antialiasing.}
    {...}
  end;

type
  TRGBAlphaImageHelper = class helper for TRGBAlphaImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
  end;

type
  TRGBImageHelper = class helper for TRGBImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
  end;

type
  TGrayscaleAlphaImageHelper = class helper for TGrayscaleAlphaImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
  end;

type
  TGrayscaleImageHelper = class helper for TGrayscaleImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
  end;

type
  TRGBFloatImageHelper = class helper for TRGBFloatImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);
  end;

implementation

{-----= Tools =-----}

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

function TCastleImageHelper.BlendColor(const c1, c2: byte; const t: single): byte; {$IFDEF Supports_Inline}Inline;{$ENDIF}
var
  tmp: integer;
begin
  tmp := Round(c1 * t + c2 * (1 - t));
  if tmp >= 255 then
    Result := 255
  else
  if tmp <= 0 then
    Result := 0
  else
    Result := tmp;
end;

{-----= "Sorting procedures =-----}

procedure TCastleImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
begin
  if Self is TRGBAlphaImage then
    TRGBAlphaImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TRGBImage then
    TRGBImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TGrayscaleAlphaImage then
    TGrayscaleAlphaImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TGrayscaleImage then
    TGrayscaleImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TRGBFloatImage then
    TRGBFloatImage(Self).FillCircle(x, y, aRadius, aColor)
  else
  raise EImageDrawError.CreateFmt('Painting is not supported for %s',
        [ClassName]);
end;

{-----= CIRCLES =-----}

procedure TRGBAlphaImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector4Byte;
begin

end;

procedure TRGBImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector3Byte;
begin

end;

procedure TGrayscaleAlphaImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
var
  p: PVector2Byte;
begin

end;

procedure TGrayscaleImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
var
  p: PByte;
begin

end;

procedure TRGBFloatImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
var
  p: PVector3;
begin

end;

end.

