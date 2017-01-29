{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Random sampling of points (directions) on a sphere and hemisphere.
  Useful e.g. for ray-tracers.

  Most of the implementation based on "Global Illumination Compendium" IV.B
  [http://www.cs.kuleuven.ac.be/~phil/GI/].
  See also images there, they help to illustrate the meaning of some
  functions here.

  We use two ways to represent points on hemisphere:

  @orderedList(
    @item(Functions @italic(without "XYZ" suffix) return vector
      of 2 floats = two angles, called phi and theta (in this order).
      Phi (in [0, 2*Pi]) is an angle from some chosen meridian.
      Theta (in [0, Pi/2] for hemisphere and [0, Pi] for sphere)
      is an angle from chosen vector pointing outward from the (hemi)sphere.
      See images in "Global Illumination Compendium",
      they are probably much easier to understand than this definition.)

    @item(Functions @italic(with "XYZ" suffix) return
      3D point x, y, z.

      @unorderedList(
        @item (0, 0, 0) is the center of (hemi)sphere,
        @item (0, 0, 1) is the chosen outward vector (i.e. Theta = 0 there),
        @item (1, 0, 0) is the direction where Phi = 0 and Theta = Pi/2,
        @item (0, 1, 0) is the direction where Phi = Pi/2 and Theta = Pi/2.
      )

      This is matching conventions in "Global Illumination Compendium",
      see there (point 21).)
  )

  Functions with Density <> Const return PdfValue for returned point,
  i.e. for density p(Theta) it's PfdValue = p(Result[1]).
  These functions try to calculate PdfValue smartly (often calculating
  PfdValue and calculating Result uses the same intermediate calculation,
  so we can save some computation). PdfValue is needed for importance sampling.
}

unit CastleSphereSampling;

{$I castleconf.inc}

interface

uses CastleVectors, CastleUtils;

{ Convert from PhiTheta representation of (hemi)sphere direction to
  XYZ representation.

  See the beginning of this unit's documentation, CastleSphereSampling,
  for more precise description of XYZ representation. }
function PhiThetaToXYZ(const PhiTheta: TVector2Single; const SphereRadius: Single)
  :TVector3Single; overload;

{ Convert from PhiTheta representation of (hemi)sphere direction to
  XYZ representation.

  This is the more advanced version where you can freely specify which
  vector is the "main outside (hemi)sphere vector", SphereTheta0.
  Points with Theta = 0 are exactly on SphereTheta0.
  It is @italic(undefined) where point like (Phi = 0, Theta = Pi/2)
  (or any other point with Theta <> 0) will be placed,
  i.e. it's not defined where's the "chosen meridian" for Phi = 0.
  However @italic(it's defined that this meridian will be determined only by
  SphereTheta0), and this is usually sufficient (since this makes sure
  that sampling and then converting to XYZ multiple points with the same
  SphereTheta0 will preserve sampled density).

  Note that the length of SphereTheta0 determines also the sphere radius. }
function PhiThetaToXYZ(const PhiTheta: TVector2Single;
  const SphereTheta0: TVector3Single): TVector3Single; overload;

{ Convert from XYZ representation of (hemi)sphere direction to PhiTheta. }
function XYZToPhiTheta(const XYZ: TVector3Single): TVector2Single;

{ Random point (direction) on unit hemisphere, sampled with
  constant density (p(Theta) = 1/2*Pi).
  @groupBegin }
function RandomHemispherePointConst: TVector2Single;
function RandomHemispherePointConstXYZ: TVector3Single;
{ @groupEnd }

{ Random point (direction) on unit hemisphere, sampled with
  density p(Theta) = cos(Theta)/Pi.
  @groupBegin }
function RandomHemispherePointCosTheta(
  out PdfValue: Single): TVector2Single;
function RandomHemispherePointCosThetaXYZ(
  out PdfValue: Single): TVector3Single;
{ @groupEnd }

{ Random point (direction) on unit hemisphere, sampled with
  density p(Theta) = (n+1) * (cos(Theta))^n / 2*Pi.
  @groupBegin }
function RandomHemispherePointCosThetaExp(const n: Single;
  out PdfValue: Single): TVector2Single;
function RandomHemispherePointCosThetaExpXYZ(const n: Single;
  out PdfValue: Single): TVector3Single;
{ @groupEnd }

implementation

{ Note: Math unit operates of Float type. We try to minimize in some routines
  below conversions Single <-> Float. }

uses Math;

function PhiThetaToXYZ(const PhiTheta: TVector2Single; const SphereRadius: Single): TVector3Single;
var
  SinPhi, CosPhi, SinTheta, CosTheta: Float;
begin
  SinCos(PhiTheta[0], SinPhi, CosPhi);
  SinCos(PhiTheta[1], SinTheta, CosTheta);

  result[0] := SphereRadius * CosPhi * SinTheta;
  result[1] := SphereRadius * SinPhi * SinTheta;
  result[2] := SphereRadius * CosTheta;
end;

function XYZToPhiTheta(const XYZ: TVector3Single): TVector2Single;
begin
  Result[0] := ArcTan2(XYZ[1], XYZ[0]);
  Result[1] := ArcTan2(Sqrt(Sqr(XYZ[0]) + Sqr(XYZ[1])), XYZ[2]);
end;

function PhiThetaToXYZ(const PhiTheta: TVector2Single; const SphereTheta0: TVector3Single): TVector3Single;
var
  NewX, NewY: TVector3Single;
  SphereRadius, NewXLen, NewYLen: Single;
begin
  result := PhiThetaToXYZ(PhiTheta, 1);

  { make NewX anything orthogonal (but not zero) to SphereTheta0. }
  if Zero(SphereTheta0[0]) and Zero(SphereTheta0[1]) then
  begin
    { then we're sure that SphereTheta0[2] <> 0, so NewX will not be zero }
    NewX[0] := 0;
    NewX[1] := -SphereTheta0[2];
    NewX[2] := SphereTheta0[1];
  end else
  begin
    NewX[0] := -SphereTheta0[1];
    NewX[1] := SphereTheta0[0];
    NewX[2] := 0;
  end;
  NewY := VectorProduct(SphereTheta0, NewX);
  { set correct lengths for NewX and NewY. We calculate NewYLen fast, without
    any Sqrt (which would happen inside VectorLen), because we know that
    NewY was calculated by VectorProduct above and that NewX and SphereTheta0
    are orthogonal. }
  SphereRadius := VectorLen(SphereTheta0);
  NewXLen := VectorLen(NewX);
  NewYLen := NewXLen * SphereRadius;

  VectorScaleVar(NewX, SphereRadius/NewXLen);
  VectorScaleVar(NewY, SphereRadius/NewYLen);

  { TODO: create MatrixMultPointVar to speed this a little bit? }
  result := MatrixMultPoint(TransformToCoordsMatrix(ZeroVector3Single,
    NewX,
    NewY,
    SphereTheta0), result);
end;

function RandomHemispherePointConst: TVector2Single;
begin
  result[0] := 2*Pi*Random;
  result[1] := ArcCos(Random);
end;

function RandomHemispherePointConstXYZ: TVector3Single;
var
  r1, r2, sqroot: Single;
  cosinus, sinus: Float;
begin
  r1 := Random;
  r2 := Random;
  SinCos(2*Pi*r1, sinus, cosinus);
  sqroot := Sqrt(1-Sqr(r2));

  result[0] := cosinus * sqroot;
  result[1] := sinus * sqroot;
  result[2] := r2;
end;

function RandomHemispherePointCosTheta(
  out PdfValue: Single): TVector2Single;
var
  SqrtR2: Float;
begin
  SqrtR2 := Sqrt(Random);

  result[0] := 2*Pi*Random;
  result[1] := ArcCos(SqrtR2);
  PdfValue := SqrtR2 / Pi;
end;

function RandomHemispherePointCosThetaXYZ(
  out PdfValue: Single): TVector3Single;
var
  SqRoot, r1, r2: Single;
  SinR1, CosR1: Float;
begin
  r1 := Random;
  r2 := Random;
  SinCos(2*Pi*r1, SinR1, CosR1);
  SqRoot := Sqrt(1-r2);

  result[0] := CosR1 * SqRoot;
  result[1] := SinR1 * SqRoot;
  result[2] := Sqrt(r2);
  PdfValue := result[2];
end;

function RandomHemispherePointCosThetaExp(const n: Single;
  out PdfValue: Single): TVector2Single;
var
  r2: Float;
begin
  r2 := Random;

  result[0] := 2*Pi*Random;
  result[1] := ArcCos(Power(r2, 1/(n+1)));
  PdfValue := (n+1) * Power(r2, n/(n+1)) / 2*Pi;
end;

function RandomHemispherePointCosThetaExpXYZ(const n: Single;
  out PdfValue: Single): TVector3Single;
var
  r1, r2, r2Power, r2Root: Single;
  SinR1, CosR1: Float;
begin
  r1 := Random;
  r2 := Random;
  SinCos(2*Pi*r1, SinR1, CosR1);
  r2Power := Power(r2, 1/(n+1));
  r2Root := Sqrt(1-Sqr(r2Power));

  result[0] := CosR1 * r2Root;
  result[1] := SinR1 * r2Root;
  result[2] := r2Power;
  PdfValue := (n+1) * Power(r2, n/(n+1)) / 2*Pi;
end;

end.
