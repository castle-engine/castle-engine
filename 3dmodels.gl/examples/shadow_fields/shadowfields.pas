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

uses CubeEnvMap, VectorMath, Math;

const
  SFSpheresCount = 16;

type
  TEnvMap = array [TEnvMapSide, 0..Sqr(EnvMapSize) - 1] of Byte;
  PEnvMap = ^TEnvMap;

  TShadowField = class
  public
    EnvMaps: array [0..SFSpheresCount - 1,
      TEnvMapSide, 0..Sqr(EnvMapSize) - 1] of TEnvMap;

    { Radius of the smallest sphere of the shadow field.
      Must be >= 0 (yes, should work Ok with = 0 too). }
    FirstSphereRadius: Float;

    { Radius of the largest sphere of the shadow field.
      Must be > FirstSphereRadius. }
    LastSphereRadius: Float;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    { Environment map, from EnvMaps, corresponding to point V in 3D space.
      V is given in coordinates of this shadow field.

      @nil if there's no environment map this far (you should then
      assume that this object doesn't occlude anything,
      or light source doesn't make any light this far).

      @nil is also returned when V is zero --- this means that V
      is too close to shadow caster to choose a suitable env map.
      It doesn't matter what you assume in this case, as this shouldn't happen... }
    function EnvMapFromPoint(const V: TVector3Single): PEnvMap;

    { Given indexes to EnvMaps array, to which point in 3D space
      they correspond? This is somewhat reverse to EnvMapFromPoint. }
    function PointFromEnvMap(const Sphere: Cardinal;
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
    F.WriteBuffer(FirstSphereRadius, SizeOf(FirstSphereRadius));
    F.WriteBuffer(LastSphereRadius, SizeOf(LastSphereRadius));
  finally FreeAndNil(F) end;
end;

function TShadowField.EnvMapFromPoint(const V: TVector3Single): PEnvMap;
var
  Distance: Float;
  Sphere, Pixel: Cardinal;
  Side: TEnvMapSide;
begin
  if ZeroVector(V) then
    Exit(nil);

  { calculate Sphere number using VectorLen(V) }
  Distance := VectorLen(V);
  if Distance < FirstSphereRadius then
    Sphere := 0 else
  if Distance > LastSphereRadius then
    Exit(nil) else
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
  Result := @EnvMaps[Sphere, Side, Pixel];
end;

function TShadowField.PointFromEnvMap(const Sphere: Cardinal;
  const EnvMapSide: TEnvMapSide; const Pixel: Cardinal): TVector3Single;
var
  Distance: Float;
begin
  Distance := MapRange(Sphere, 0, SFSpheresCount - 1,
    FirstSphereRadius, LastSphereRadius);
  Result := VectorAdjustToLength(EnvMapDirection(EnvMapSide, Pixel), Distance);
end;

end.
