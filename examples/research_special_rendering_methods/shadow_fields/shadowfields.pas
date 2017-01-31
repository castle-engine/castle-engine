{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shadow fields structures. }
unit ShadowFields;

interface

uses CastleCubeMaps, CastleVectors, Math, CastleSphericalHarmonics;

const
  SFSpheresCount = 16;

  ShadowFieldExt = '.shadow_field';

type
  TSFInterpolation = (siNone, siLinearRadius, siBilinear, siTrilinear);

  TShadowField = class
  private
    InterpolatedVector: TSHVectorSingle;

    function SphereIndexFromPoint(const V: TVector3Single;
      out Sphere: Cardinal): boolean;
  public
    EnvMaps: array [0..SFSpheresCount - 1,
      TCubeMapSide, 0..Sqr(CubeMapSize) - 1] of TCubeMapByte;

    SHVectors: array [0..SFSpheresCount - 1,
      TCubeMapSide, 0..Sqr(CubeMapSize) - 1] of TSHVectorSingle;

    SpheresMiddle: TVector3Single;

    { Radius of the smallest sphere of the shadow field.
      Must be >= 0 (yes, should work Ok with = 0 too). }
    FirstSphereRadius: Float;

    { Radius of the largest sphere of the shadow field.
      Must be > FirstSphereRadius. }
    LastSphereRadius: Float;

    procedure LoadFromFile(const URL: string);
    procedure SaveToFile(const URL: string);

    { Environment map, from EnvMaps, corresponding to point V in 3D space.
      This just uses IndexFromPoint, and returns appropriate environment map.

      Returns @nil if there's no environment map this far or this close
      to V (that is, when IndexFromPoint returns @false). }
    function EnvMapFromPoint(const V: TVector3Single;
      const Scale: Single): PCubeMapByte;

    { SH vector, corresponding to point V in 3D space.
      This just uses Index*FromPoint, and returns appropriate vector.

      Various Interpolation result in various Index*FromPoint versions used.
      For no interpolation, returned pointer is just a direct pointer
      to some part of SHVectors array (this way non-interpolated
      version has additional speed benefit of avoiding copying).
      For some interpolation, returned pointer points to some internal
      structure here (and is valid only until you call SHVectorFromPoint
      next time).

      SHVectorCount says how many items of sh vector are you interested in.
      When interpolation is used, it will calculate only these factors,
      which means that for SHVectorCount < MaxSHBasis you may get small
      speed gain.

      Returns @nil if there's no environment map this far or this close
      to V (that is, when Index*FromPoint returns @false). }
    function SHVectorFromPoint(
      const V: TVector3Single; const Scale: Single;
      const Interpolation: TSFInterpolation;
      const SHVectorCount: Cardinal): PSHVectorSingle;

    { Which environment map (from EnvMaps) and sh vector (from SHVectors)
      correspond to point V in 3D space.
      V is given in coordinates of this shadow field.

      @false if there's no environment map this far from
      SpheresMiddle (you should then
      assume that this object doesn't occlude anything,
      or light source doesn't make any light this far).

      @false is also returned when V is exactly at SpheresMiddle ---
      this means that V
      is too close to shadow caster to choose a suitable env map.
      It doesn't matter what you assume in this case, as this shouldn't happen... }
    function IndexFromPoint(
      V: TVector3Single; const Scale: Single;
      out Sphere: Cardinal; out Side: TCubeMapSide; out Pixel: Cardinal):
      boolean;

    { Like IndexFromPoint, but returns two SF items, you should interpolate
      between them with Ratio1/2.

      Ratio1 + Ratio2 always sum to 1. One of the ratios may be exactly 0,
      in this case you should ignore indexes (they may be invalid). }
    function Index2FromPoint(
      V: TVector3Single; const Scale: Single;
      out Sphere: TVector2Cardinal; out Side: TCubeMapSide; out Pixel: Cardinal;
      out Ratio: TVector2Single): boolean;

    { Like IndexFromPoint, but returns four SF items, you should interpolate
      between them with Ratio. This does bilinear interpolation (using closest
      sphere radius). }
    function Index4FromPoint(
      V: TVector3Single; const Scale: Single;
      out Sphere: Cardinal;
      out Side: TCubeMapSide4;
      out Pixel: TVector4Cardinal;
      out Ratio: TVector4Single): boolean;

    { Given indexes to EnvMaps array, to which point in 3D space
      they correspond? This is reverse to IndexFromPoint. }
    function PointFromIndex(const Sphere: Cardinal;
      const CubeMapSide: TCubeMapSide; const Pixel: Cardinal): TVector3Single;
  end;

implementation

uses SysUtils, CastleFilesUtils, CastleUtils, CastleZStream, Classes,
  CastleDownload;

procedure TShadowField.LoadFromFile(const URL: string);
var
  F: TStream;
begin
  F := Download(URL, [soGzip]);
  try
    F.ReadBuffer(EnvMaps, SizeOf(EnvMaps));
    F.ReadBuffer(SHVectors, SizeOf(SHVectors));
    F.ReadBuffer(SpheresMiddle, SizeOf(SpheresMiddle));
    F.ReadBuffer(FirstSphereRadius, SizeOf(FirstSphereRadius));
    F.ReadBuffer(LastSphereRadius, SizeOf(LastSphereRadius));
  finally FreeAndNil(F) end;
end;

procedure TShadowField.SaveToFile(const URL: string);
var
  F: TStream;
begin
  F := URLSaveStream(URL, [soGzip]);
  try
    F.WriteBuffer(EnvMaps, SizeOf(EnvMaps));
    F.WriteBuffer(SHVectors, SizeOf(SHVectors));
    F.WriteBuffer(SpheresMiddle, SizeOf(SpheresMiddle));
    F.WriteBuffer(FirstSphereRadius, SizeOf(FirstSphereRadius));
    F.WriteBuffer(LastSphereRadius, SizeOf(LastSphereRadius));
  finally FreeAndNil(F) end;
end;

function TShadowField.EnvMapFromPoint(const V: TVector3Single;
  const Scale: Single): PCubeMapByte;
var
  Sphere, Pixel: Cardinal;
  Side: TCubeMapSide;
begin
  if IndexFromPoint(V, Scale, Sphere, Side, Pixel) then
    Result := @EnvMaps[Sphere, Side, Pixel] else
    Result := nil;
end;

function TShadowField.SHVectorFromPoint(const V: TVector3Single;
  const Scale: Single;
  const Interpolation: TSFInterpolation;
  const SHVectorCount: Cardinal): PSHVectorSingle;

  procedure DoNone;
  var
    Sphere, Pixel: Cardinal;
    Side: TCubeMapSide;
  begin
    if IndexFromPoint(V, Scale, Sphere, Side, Pixel) then
      Result := @SHVectors[Sphere, Side, Pixel] else
      Result := nil;
  end;

  procedure DoLinearRadius;
  var
    Sphere: TVector2Cardinal;
    Pixel, LM: Cardinal;
    Side: TCubeMapSide;
    Ratio: TVector2Single;
    Vector: array [0..1] of PSHVectorSingle;
  begin
    if Index2FromPoint(V, Scale, Sphere, Side, Pixel, Ratio) then
    begin
      if Ratio[0] = 0 then
        Result := @SHVectors[Sphere[1], Side, Pixel] else
      if Ratio[1] = 0 then
        Result := @SHVectors[Sphere[0], Side, Pixel] else
      begin
        Vector[0] := @SHVectors[Sphere[0], Side, Pixel];
        Vector[1] := @SHVectors[Sphere[1], Side, Pixel];
        for LM := 0 to SHVectorCount - 1 do
          InterpolatedVector[LM] :=
            Vector[0]^[LM] * Ratio[0] +
            Vector[1]^[LM] * Ratio[1];
        Result := @InterpolatedVector;
      end;
    end else
      Result := nil;
  end;

  procedure DoBilinear(out OutVector: TSHVectorSingle;
    const Sphere: Cardinal;
    const Side: TCubeMapSide4;
    const Pixel: TVector4Cardinal;
    const Ratio: TVector4Single);
  var
    LM: Cardinal;
    Vector: array [0..3] of PSHVectorSingle;
    I: Cardinal;
  begin
    for I := 0 to 3 do
      Vector[I] := @SHVectors[Sphere, Side[I], Pixel[I]];
    for LM := 0 to SHVectorCount - 1 do
      OutVector[LM] :=
        Vector[0]^[LM] * Ratio[0] +
        Vector[1]^[LM] * Ratio[1] +
        Vector[2]^[LM] * Ratio[2] +
        Vector[3]^[LM] * Ratio[3];
  end;

  procedure DoBilinear;
  var
    Sphere: Cardinal;
    Pixel: TVector4Cardinal;
    Side: TCubeMapSide4;
    Ratio: TVector4Single;
  begin
    if Index4FromPoint(V, Scale, Sphere, Side, Pixel, Ratio) then
    begin
      DoBilinear(InterpolatedVector, Sphere, Side, Pixel, Ratio);
      Result := @InterpolatedVector;
    end else
      Result := nil;
  end;

  procedure DoTrilinear;
  var
    Sphere: TVector2Cardinal;
    LM: Cardinal;
    Pixel: TVector4Cardinal;
    Side: TCubeMapSide4;
    Ratio2: TVector2Single;
    Ratio4: TVector4Single;
    BiVector: array [0..1] of TSHVectorSingle;
    DummySphere, DummyPixel: Cardinal;
    DummySide: TCubeMapSide;
  begin
    { I call Index2FromPoint only to get Sphere and Ratio2.
      I'm not really interested here in DummySide and DummyPixel values,
      I'll ignore them.

      Analogously, I call Index4FromPoint only to get Side and Pixel and Ratio4.
      I'll ignore DummySphere. }

    if Index2FromPoint(V, Scale, Sphere, DummySide, DummyPixel, Ratio2) and
       Index4FromPoint(V, Scale, DummySphere, Side, Pixel, Ratio4) then
    begin
      if Ratio2[0] = 0 then
      begin
        { Use only Sphere[1]. So it degenerates to bilinear. }
        DoBilinear(InterpolatedVector, Sphere[1], Side, Pixel, Ratio4);
        Result := @InterpolatedVector;
      end else
      if Ratio2[1] = 0 then
      begin
        { Use only Sphere[0]. So it degenerates to bilinear. }
        DoBilinear(InterpolatedVector, Sphere[0], Side, Pixel, Ratio4);
        Result := @InterpolatedVector;
      end else
      begin
        { Actual trilinear work. Call DoBilinear twice, and interpolate
          between them. }
        DoBilinear(BiVector[0], Sphere[0], Side, Pixel, Ratio4);
        DoBilinear(BiVector[1], Sphere[1], Side, Pixel, Ratio4);
        for LM := 0 to SHVectorCount - 1 do
          InterpolatedVector[LM] :=
            BiVector[0][LM] * Ratio2[0] +
            BiVector[1][LM] * Ratio2[1];
        Result := @InterpolatedVector;
      end;
    end else
      Result := nil;
  end;

begin
  case Interpolation of
    siNone: DoNone;
    siLinearRadius: DoLinearRadius;
    siBilinear: DoBilinear;
    siTrilinear: DoTrilinear;
    else raise EInternalError.Create('TShadowField.SHVectorFromPoint interp?');
  end;
end;

function TShadowField.SphereIndexFromPoint(const V: TVector3Single;
  out Sphere: Cardinal): boolean;
var
  Distance: Float;
begin
  { calculate Sphere number using VectorLen(V) }
  Distance := VectorLen(V);
  if Distance < FirstSphereRadius then
    Sphere := 0 else
  if Distance > LastSphereRadius then
    Exit(false) else
  begin
    Distance := MapRange(Distance, FirstSphereRadius, LastSphereRadius,
      0, SFSpheresCount - 1);
    { Distace was between FirstSphereRadius and LastSphereRadius,
      so it should be now between 0 and SFSpheresCount - 1.
      But since floating point calculations are never perfect, we safeguard
      ourselves to force resulting Cadinal Sphere be in the right range. }
    if Distance < 0 then
      Sphere := 0 else
    begin
      Sphere := Round(Distance);
      MinVar(Sphere, SFSpheresCount - 1);
    end;
  end;

  Result := true;
end;

function TShadowField.IndexFromPoint(V: TVector3Single;
  const Scale: Single;
  out Sphere: Cardinal; out Side: TCubeMapSide; out Pixel: Cardinal):
  boolean;
begin
  VectorSubtractVar(V, SpheresMiddle);
  if ZeroVector(V) then
    Exit(false);

  VectorScaleVar(V, 1/Scale);

  if not SphereIndexFromPoint(V, Sphere) then Exit(false);

  DirectionToCubeMap(V, Side, Pixel);
  Result := true;
end;

function TShadowField.Index4FromPoint(
  V: TVector3Single; const Scale: Single;
  out Sphere: Cardinal;
  out Side: TCubeMapSide4;
  out Pixel: TVector4Cardinal;
  out Ratio: TVector4Single): boolean;
begin
  VectorSubtractVar(V, SpheresMiddle);
  if ZeroVector(V) then
    Exit(false);

  VectorScaleVar(V, 1/Scale);

  if not SphereIndexFromPoint(V, Sphere) then Exit(false);

  Direction4ToCubeMap(V, Side, Pixel, Ratio);
  Result := true;
end;

function TShadowField.Index2FromPoint(V: TVector3Single;
  const Scale: Single;
  out Sphere: TVector2Cardinal; out Side: TCubeMapSide; out Pixel: Cardinal;
  out Ratio: TVector2Single): boolean;
var
  Distance: Float;
begin
  VectorSubtractVar(V, SpheresMiddle);
  if ZeroVector(V) then
    Exit(false);

  VectorScaleVar(V, 1/Scale);

  { calculate Sphere number using VectorLen(V) }
  Distance := VectorLen(V);
  if Distance < FirstSphereRadius then
  begin
    Sphere[0] := 0;
    Ratio[0] := 1;
    Ratio[1] := 0; { exactly 0, so Sphere[1] will be ignored }
  end else
  if Distance > LastSphereRadius then
    Exit(false) else
  begin
    Distance := MapRange(Distance, FirstSphereRadius, LastSphereRadius,
      0, SFSpheresCount - 1);
    { Distace was between FirstSphereRadius and LastSphereRadius,
      so it should be now between 0 and SFSpheresCount - 1.
      But since floating point calculations are never perfect, we safeguard
      ourselves to force resulting Cadinal Sphere be in the right range. }
    if Distance < 0 then
    begin
      Sphere[0] := 0;
      Ratio[0] := 1;
      Ratio[1] := 0;
    end else
    begin
      Sphere[0] := Trunc(Distance);
      Ratio[1] := Frac(Distance);
      Ratio[0] := 1 - Ratio[1];
      Sphere[1] := Sphere[0] + 1;
      if Sphere[0] >= SFSpheresCount then
      begin
        { This may happen in case of floating point errors. }
        Sphere[0] := SFSpheresCount - 1;
        Ratio[0] := 1;
        Ratio[1] := 0;
      end else
      if Sphere[1] >= SFSpheresCount then
      begin
        { This may happen if Sphere[0] really was on the end. }
        Ratio[0] := 1;
        Ratio[1] := 0;
      end;
    end;

    Assert(Sphere[0] < SFSpheresCount);
  end;

  DirectionToCubeMap(V, Side, Pixel);

  Result := true;
end;

function TShadowField.PointFromIndex(const Sphere: Cardinal;
  const CubeMapSide: TCubeMapSide; const Pixel: Cardinal): TVector3Single;
var
  Distance: Float;
begin
  Distance := MapRange(Sphere, 0, SFSpheresCount - 1,
    FirstSphereRadius, LastSphereRadius);
  Result := VectorAdjustToLength(CubeMapDirection(CubeMapSide, Pixel), Distance);
  VectorAddVar(Result, SpheresMiddle);
end;

end.
