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
{ Calculate shadow field of arbitrary 3D model.

  $1 is the source model, $2 is the output file with shadow field.
  If $2 is omitted, we output to $1 with extension changed to .shadow_field
  (this is usually most comfortable).

  By default we generate OOF (object occlusion field).
  This simply means that color is white where the ray @bold(does not hit),
  that is white color = light passes through.
  If you want to generate SRF (source radiance field), for light source, use
    --light
  option. This simply reverts the colors, i.e. white is where the ray hits,
  that is white color = light shines here.

  Note that for environment maps, it's actually trivial to switch
  between OOF and SRF (just negate the colors). This could even be
  done painlessly during rendering.
  However, for spherical harmonics, there's no such easy way to
  invert the SH function. (And for double- and triple- products
  within shadow_fields program we would need this; simply reverting
  the result of function would not be good.)
}
program precompute_shadow_field;

uses SysUtils, CastleProgress, CastleProgressConsole, CastleUtils,
  CastleSceneCore, ShadowFields, CastleTimeUtils, CastleVectors,
  CastleCubeMaps, CastleSphericalHarmonics, CastleParameters,
  CastleURIUtils;

var
  Scene: TCastleSceneCore;
  LightSource: boolean;

procedure ComputeCubeMap(const Point: TVector3Single;
  var CubeMap: TCubeMapByte; var SHVector: TSHVectorSingle);
var
  Side: TCubeMapSide;
  Pixel: Cardinal;
  Color: Byte;
begin
  for Side := Low(Side) to High(Side) do
    for Pixel := 0 to Sqr(CubeMapSize) - 1 do
    begin
      if Scene.InternalOctreeVisibleTriangles.IsRayCollision(Point,
        CubeMapDirection(Side, Pixel), nil, false, nil) then
        Color := 0 else
        Color := High(Byte);
      if LightSource then Color := High(Byte)- Color;
      CubeMap[Side, Pixel] := Color;
    end;

  SHVectorFromCubeMap(SHVector, CubeMap);
end;

const
  Options: array[0..0] of TOption =
  ( (Short: 'l'; Long: 'light'; Argument: oaNone) );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0: LightSource := true;
    end;
  end;

var
  SF: TShadowField;
  Sphere, Pixel: Cardinal;
  Side: TCubeMapSide;
  BoundingSphereCenter: TVector3Single;
  BoundingSphereRadius: Single;
  InputURL, OutputURL: string;
begin
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtLeast(1);
  Parameters.CheckHighAtMost(2);

  InputURL := Parameters[1];
  if Parameters.High = 1 then
    OutputURL := ChangeURIExt(InputURL, ShadowFieldExt) else
    OutputURL := Parameters[2];
  Writeln('Output URL: ', OutputURL);

  InitializeSHBasisMap;

  Progress.UserInterface := ProgressConsoleInterface;

  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load(InputURL);
    Scene.TriangleOctreeProgressTitle := 'Building octree';
    Scene.Spatial := [ssVisibleTriangles];

    SF := TShadowField.Create;
    try
      { calculate Scene bounding sphere, and set sphere properties of SF }
      Scene.BoundingBox.BoundingSphere(BoundingSphereCenter, BoundingSphereRadius);
      { BoundingSphereFromBox3D gives me sqr of radius }
      BoundingSphereRadius := Sqrt(BoundingSphereRadius);
      { Since our BoundingSphereRadius is too large (it's bounding sphere
        over bounding box), make it smaller here.
        This should result in more spheres closer to the object,
        which should be slightly  better. }
      BoundingSphereRadius := 0.6 * BoundingSphereRadius;
      SF.SpheresMiddle := BoundingSphereCenter;
      { 0.2, 8 are advised in paper about shadow fields }
      SF.FirstSphereRadius := 0.2 * BoundingSphereRadius;
      SF.LastSphereRadius := 8 * BoundingSphereRadius;

      Writeln(Format('Shadow fields spheres: middle %s, radius (%f - %f)',
        [ VectorToNiceStr(SF.SpheresMiddle),
          SF.FirstSphereRadius, SF.LastSphereRadius ]));

      ProcessTimerBegin;

      Progress.Init(SFSpheresCount * 6 * Sqr(CubeMapSize), 'Shadow field calculation');
      try

        for Sphere := 0 to SFSpheresCount - 1 do
          for Side := Low(Side) to High(Side) do
            for Pixel := 0 to Sqr(CubeMapSize) - 1 do
            begin
              ComputeCubeMap(
                SF.PointFromIndex(Sphere, Side, Pixel),
                SF.EnvMaps[Sphere, Side, Pixel],
                SF.SHVectors[Sphere, Side, Pixel]);
              Progress.Step;
            end;

      finally Progress.Fini end;

      Writeln(Format('Shadow field computed: %d rays, %f secs',
        [ SFSpheresCount * 6 * Sqr(CubeMapSize) *
                           6 * Sqr(CubeMapSize),
          ProcessTimerEnd]));

      SF.SaveToFile(OutputURL);
    finally FreeAndNil(SF) end;
  finally FreeAndNil(Scene) end;
end.
