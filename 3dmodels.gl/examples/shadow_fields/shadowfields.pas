{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Shadow fields structures. }
unit ShadowFields;

interface

uses CubeEnvMap, VectorMath, Math, SphericalHarmonics;

const
  SFSpheresCount = 16;

  ShadowFieldExt = '.shadow_field';

type
  TSFInterpolation = (siNone, siLinearRadius);

  TShadowField = class
  private
    InterpolatedVector: TSHVectorSingle;
  public
    EnvMaps: array [0..SFSpheresCount - 1,
      TEnvMapSide, 0..Sqr(EnvMapSize) - 1] of TEnvMapByte;

    SHVectors: array [0..SFSpheresCount - 1,
      TEnvMapSide, 0..Sqr(EnvMapSize) - 1] of TSHVectorSingle;

    SpheresMiddle: TVector3Single;

    { Radius of the smallest sphere of the shadow field.
      Must be >= 0 (yes, should work Ok with = 0 too). }
    FirstSphereRadius: Float;

    { Radius of the largest sphere of the shadow field.
      Must be > FirstSphereRadius. }
    LastSphereRadius: Float;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    { Environment map, from EnvMaps, corresponding to point V in 3D space.
      This just uses IndexFromPoint, and returns appropriate environment map.

      Returns @nil if there's no environment map this far or this close
      to V (that is, when IndexFromPoint returns @false). }
    function EnvMapFromPoint(const V: TVector3Single): PEnvMapByte;

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
    function SHVectorFromPoint(const V: TVector3Single;
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
    function IndexFromPoint(V: TVector3Single;
      out Sphere: Cardinal; out Side: TEnvMapSide; out Pixel: Cardinal):
      boolean;

    { Like IndexFromPoint, but returns two SF items, you should interpolate
      between them with Ratio1/2.

      Ratio1 + Ratio2 always sum to 1. One of the ratios may be exactly 0,
      in this case you should ignore indexes (they may be invalid). }
    function Index2FromPoint(V: TVector3Single;
      out Sphere1: Cardinal; out Side1: TEnvMapSide; out Pixel1: Cardinal;
      out Ratio1: Single;
      out Sphere2: Cardinal; out Side2: TEnvMapSide; out Pixel2: Cardinal;
      out Ratio2: Single): boolean;

    { Given indexes to EnvMaps array, to which point in 3D space
      they correspond? This is reverse to IndexFromPoint. }
    function PointFromIndex(const Sphere: Cardinal;
      const EnvMapSide: TEnvMapSide; const Pixel: Cardinal): TVector3Single;
  end;

implementation

uses SysUtils, KambiFilesUtils, KambiUtils, KambiZStream, Classes;

procedure TShadowField.LoadFromFile(const FileName: string);
var
  F: TStream;
begin
  F := TGZFileStream.Create(FileName, gzOpenRead);
  try
    F.ReadBuffer(EnvMaps, SizeOf(EnvMaps));
    F.ReadBuffer(SHVectors, SizeOf(SHVectors));
    F.ReadBuffer(SpheresMiddle, SizeOf(SpheresMiddle));
    F.ReadBuffer(FirstSphereRadius, SizeOf(FirstSphereRadius));
    F.ReadBuffer(LastSphereRadius, SizeOf(LastSphereRadius));
  finally FreeAndNil(F) end;
end;

procedure TShadowField.SaveToFile(const FileName: string);
var
  F: TStream;
begin
  F := TGZFileStream.Create(FileName, gzOpenWrite);
  try
    F.WriteBuffer(EnvMaps, SizeOf(EnvMaps));
    F.WriteBuffer(SHVectors, SizeOf(SHVectors));
    F.WriteBuffer(SpheresMiddle, SizeOf(SpheresMiddle));
    F.WriteBuffer(FirstSphereRadius, SizeOf(FirstSphereRadius));
    F.WriteBuffer(LastSphereRadius, SizeOf(LastSphereRadius));
  finally FreeAndNil(F) end;
end;

function TShadowField.EnvMapFromPoint(const V: TVector3Single): PEnvMapByte;
var
  Sphere, Pixel: Cardinal;
  Side: TEnvMapSide;
begin
  if IndexFromPoint(V, Sphere, Side, Pixel) then
    Result := @EnvMaps[Sphere, Side, Pixel] else
    Result := nil;
end;

function TShadowField.SHVectorFromPoint(const V: TVector3Single;
  const Interpolation: TSFInterpolation;
  const SHVectorCount: Cardinal): PSHVectorSingle;
var
  Sphere1, Pixel1, Sphere2, Pixel2, LM: Cardinal;
  Side1, Side2: TEnvMapSide;
  Ratio1, Ratio2: Single;
  Vector1, Vector2: PSHVectorSingle;
begin
  case Interpolation of
    siNone:
      begin
        if IndexFromPoint(V, Sphere1, Side1, Pixel1) then
          Result := @SHVectors[Sphere1, Side1, Pixel1] else
          Result := nil;
      end;
    siLinearRadius:
      begin
        if Index2FromPoint(V,
          Sphere1, Side1, Pixel1, Ratio1,
          Sphere2, Side2, Pixel2, Ratio2) then
        begin
          if Ratio1 = 0 then
            Result := @SHVectors[Sphere2, Side2, Pixel2] else
          if Ratio2 = 0 then
            Result := @SHVectors[Sphere1, Side1, Pixel1] else
          begin
            Vector1 := @SHVectors[Sphere1, Side1, Pixel1];
            Vector2 := @SHVectors[Sphere2, Side2, Pixel2];
            for LM := 0 to SHVectorCount - 1 do
              InterpolatedVector[LM] :=
                Vector1^[LM] * Ratio1 + Vector2^[LM] * Ratio2;
            Result := @InterpolatedVector;
          end;
        end else
          Result := nil;
      end;
  end;
end;

function TShadowField.IndexFromPoint(V: TVector3Single;
  out Sphere: Cardinal; out Side: TEnvMapSide; out Pixel: Cardinal):
  boolean;
var
  Distance: Float;
begin
  VectorSubtractTo1st(V, SpheresMiddle);
  if ZeroVector(V) then
    Exit(false);

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
      MinTo1st(Sphere, SFSpheresCount - 1);
    end;
  end;

  DirectionToEnvMap(V, Side, Pixel);
  Result := true;
end;

function TShadowField.Index2FromPoint(V: TVector3Single;
  out Sphere1: Cardinal; out Side1: TEnvMapSide; out Pixel1: Cardinal;
  out Ratio1: Single;
  out Sphere2: Cardinal; out Side2: TEnvMapSide; out Pixel2: Cardinal;
  out Ratio2: Single): boolean;
var
  Distance: Float;
begin
  VectorSubtractTo1st(V, SpheresMiddle);
  if ZeroVector(V) then
    Exit(false);

  { calculate Sphere number using VectorLen(V) }
  Distance := VectorLen(V);
  if Distance < FirstSphereRadius then
  begin
    Sphere1 := 0;
    Ratio1 := 1;
    Ratio2 := 0; { exactly 0, so Sphere2 will be ignored }
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
      Sphere1 := 0;
      Ratio1 := 1;
      Ratio2 := 0;
    end else
    begin
      Sphere1 := Trunc(Distance);
      Ratio2 := Frac(Distance);
      Ratio1 := 1 - Ratio2;
      Sphere2 := Sphere1 + 1;
      if Sphere1 >= SFSpheresCount then
      begin
        { This may happen in case of floating point errors. }
        Sphere1 := SFSpheresCount - 1;
        Ratio1 := 1;
        Ratio2 := 0;
      end else
      if Sphere2 >= SFSpheresCount then
      begin
        { This may happen if Sphere1 really was on the end. }
        Ratio1 := 1;
        Ratio2 := 0;
      end;
    end;

    Assert(Sphere1 < SFSpheresCount);
  end;

  DirectionToEnvMap(V, Side1, Pixel1);

  Side2 := Side1;
  Pixel2 := Pixel1;

  Result := true;
end;

function TShadowField.PointFromIndex(const Sphere: Cardinal;
  const EnvMapSide: TEnvMapSide; const Pixel: Cardinal): TVector3Single;
var
  Distance: Float;
begin
  Distance := MapRange(Sphere, 0, SFSpheresCount - 1,
    FirstSphereRadius, LastSphereRadius);
  Result := VectorAdjustToLength(EnvMapDirection(EnvMapSide, Pixel), Distance);
  VectorAddTo1st(Result, SpheresMiddle);
end;

end.
