{
  Copyright 2003-2022 Michalis Kamburelis.

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
  i.e. for density p(Theta) it's PfdValue = p(Result.Y).
  These functions try to calculate PdfValue smartly (often calculating
  PfdValue and calculating Result uses the same intermediate calculation,
  so we can save some computation). PdfValue is needed for importance sampling.
}

unit CastleInternalSphereSampling;

{$I castleconf.inc}

interface

uses CastleVectors, CastleUtils;

{ Convert from PhiTheta representation of (hemi)sphere direction to
  XYZ representation.

  See the beginning of this unit's documentation, CastleInternalSphereSampling,
  for more precise description of XYZ representation. }
function PhiThetaToXYZ(const PhiTheta: TVector2; const SphereRadius: Single)
  :TVector3; overload;

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
function PhiThetaToXYZ(const PhiTheta: TVector2;
  const SphereTheta0: TVector3): TVector3; overload;

{ Convert from XYZ representation of (hemi)sphere direction to PhiTheta. }
function XYZToPhiTheta(const XYZ: TVector3): TVector2;

{ Random point (direction) on unit hemisphere, sampled with
  constant density (p(Theta) = 1/2*Pi).
  @groupBegin }
function RandomHemispherePointConst: TVector2;
function RandomHemispherePointConstXYZ: TVector3;
{ @groupEnd }

{ Random point (direction) on unit hemisphere, sampled with
  density p(Theta) = cos(Theta)/Pi.
  @groupBegin }
function RandomHemispherePointCosTheta(
  out PdfValue: Single): TVector2;
function RandomHemispherePointCosThetaXYZ(
  out PdfValue: Single): TVector3;
{ @groupEnd }

{ Random point (direction) on unit hemisphere, sampled with
  density p(Theta) = (n+1) * (cos(Theta))^n / 2*Pi.
  @groupBegin }
function RandomHemispherePointCosThetaExp(const n: Single;
  out PdfValue: Single): TVector2;
function RandomHemispherePointCosThetaExpXYZ(const n: Single;
  out PdfValue: Single): TVector3;
{ @groupEnd }

implementation

{ Note: Math unit operates of Float type. We try to minimize in some routines
  below conversions Single <-> Float. }

uses Math;

function PhiThetaToXYZ(const PhiTheta: TVector2; const SphereRadius: Single): TVector3;
var
  SinPhi, CosPhi, SinTheta, CosTheta: Float;
begin
  SinCos(PhiTheta.X, SinPhi, CosPhi);
  SinCos(PhiTheta.Y, SinTheta, CosTheta);

  result.X := SphereRadius * CosPhi * SinTheta;
  result.Y := SphereRadius * SinPhi * SinTheta;
  result.Z := SphereRadius * CosTheta;
end;

function XYZToPhiTheta(const XYZ: TVector3): TVector2;
begin
  Result.X := ArcTan2(XYZ.Y, XYZ.X);
  Result.Y := ArcTan2(Sqrt(Sqr(XYZ.X) + Sqr(XYZ.Y)), XYZ.Z);
end;

function PhiThetaToXYZ(const PhiTheta: TVector2; const SphereTheta0: TVector3): TVector3;
var
  NewX, NewY: TVector3;
  SphereRadius, NewXLen, NewYLen: Single;
begin
  result := PhiThetaToXYZ(PhiTheta, 1);

  { make NewX anything orthogonal (but not zero) to SphereTheta0. }
  if IsZero(SphereTheta0.X) and IsZero(SphereTheta0.Y) then
  begin
    { then we're sure that SphereTheta0.Z <> 0, so NewX will not be zero }
    NewX.X := 0;
    NewX.Y := -SphereTheta0.Z;
    NewX.Z := SphereTheta0.Y;
  end else
  begin
    NewX.X := -SphereTheta0.Y;
    NewX.Y := SphereTheta0.X;
    NewX.Z := 0;
  end;
  NewY := TVector3.CrossProduct(SphereTheta0, NewX);
  { set correct lengths for NewX and NewY. We calculate NewYLen fast, without
    any Sqrt (which would happen inside VectorLen), because we know that
    NewY was calculated by TVector3.CrossProduct above and that NewX and SphereTheta0
    are orthogonal. }
  SphereRadius := SphereTheta0.Length;
  NewXLen := NewX.Length;
  NewYLen := NewXLen * SphereRadius;

  NewX := NewX * SphereRadius/NewXLen;
  NewY := NewY * SphereRadius/NewYLen;

  { TODO: create TMatrix4.MultPointVar to speed this a little bit? }
  Result := TransformToCoordsMatrix(TVector3.Zero,
    NewX,
    NewY,
    SphereTheta0).MultPoint(Result);
end;

function RandomHemispherePointConst: TVector2;
begin
  result.X := 2*Pi*Random;
  result.Y := ArcCos(Random);
end;

function RandomHemispherePointConstXYZ: TVector3;
var
  r1, r2, sqroot: Single;
  cosinus, sinus: Float;
begin
  r1 := Random;
  r2 := Random;
  SinCos(2*Pi*r1, sinus, cosinus);
  sqroot := Sqrt(1-Sqr(r2));

  result.X := cosinus * sqroot;
  result.Y := sinus * sqroot;
  result.Z := r2;
end;

function RandomHemispherePointCosTheta(
  out PdfValue: Single): TVector2;
var
  SqrtR2: Float;
begin
  SqrtR2 := Sqrt(Random);

  result.X := 2*Pi*Random;
  result.Y := ArcCos(SqrtR2);
  PdfValue := SqrtR2 / Pi;
end;

function RandomHemispherePointCosThetaXYZ(
  out PdfValue: Single): TVector3;
var
  SqRoot, r1, r2: Single;
  SinR1, CosR1: Float;
begin
  r1 := Random;
  r2 := Random;
  SinCos(2*Pi*r1, SinR1, CosR1);
  SqRoot := Sqrt(1-r2);

  result.X := CosR1 * SqRoot;
  result.Y := SinR1 * SqRoot;
  result.Z := Sqrt(r2);
  PdfValue := result.Z;
end;

function RandomHemispherePointCosThetaExp(const n: Single;
  out PdfValue: Single): TVector2;
var
  r2: Float;
begin
  r2 := Random;

  result.X := 2*Pi*Random;
  result.Y := ArcCos(Power(r2, 1/(n+1)));
  PdfValue := (n+1) * Power(r2, n/(n+1)) / 2*Pi;
end;

function RandomHemispherePointCosThetaExpXYZ(const n: Single;
  out PdfValue: Single): TVector3;
var
  r1, r2, r2Power, r2Root: Single;
  SinR1, CosR1: Float;
begin
  r1 := Random;
  r2 := Random;
  SinCos(2*Pi*r1, SinR1, CosR1);
  r2Power := Power(r2, 1/(n+1));
  r2Root := Sqrt(1-Sqr(r2Power));

  result.X := CosR1 * r2Root;
  result.Y := SinR1 * r2Root;
  result.Z := r2Power;
  PdfValue := (n+1) * Power(r2, n/(n+1)) / 2*Pi;
end;

end.
