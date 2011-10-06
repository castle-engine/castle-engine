{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML lighting calculation. }
unit Lighting;

interface

uses VectorMath, X3DNodes, Triangle, Math, CastleUtils;


{ Apply fog to the color of the vertex.

  Given Color is assumed to contain already the sum of
@preformatted(
  material emission (VRML97Emission)
  + for each light:
    material * lighting properties (VRML97LightContribution)
)

  This procedure will apply the fog, making the linear interpolation
  between original Color and the fog color, as appropriate.

  @param(FogType Must be calculated by VRML97FogType.
    The idea is that VRML97FogType has to compare strings to calculate
    fog type from user-specified type. You don't want to do this in each
    VRML97FogTo1st call, that is e.g. called for every ray in ray-tracer.)

  @param(FogNode If the fog is not used, FogNode may be @nil.
    In this case FogType must be -1, which is always satisfied by
    VRML97FogType(nil) resulting in -1.

    Note that FogType may be -1 for other reasons too,
    for example FogNode.FogType was unknown or FogNode.FdVisibilityRange = 0.) }
procedure VRML97FogTo1st(
  var Color: TVector3Single;
  const Position, VertexPos: TVector3Single;
  FogNode: TFogNode; FogType: TFogTypeOrNone);

implementation

uses CastleWarnings;

procedure VRML97FogTo1st(var Color: TVector3Single;
  const Position, VertexPos: TVector3Single;
  FogNode: TFogNode; FogType: TFogTypeOrNone);
var
  FogVisibilityRangeScaled: Single;

  procedure ApplyDistance(const Distance: Single);
  var
    F: Single;
  begin
    case FogType of
      ftLinear: F := (FogVisibilityRangeScaled - Distance) / FogVisibilityRangeScaled;
      ftExp   : F := Exp(-Distance / (FogVisibilityRangeScaled - Distance));
    end;
    Color := Vector_Init_Lerp(F, FogNode.FdColor.Value, Color);
  end;

var
  FogVolumetricVisibilityStart: Single;
  Distance, DistanceSqr: Single;
  VertProjected: TVector3Single;
begin
  if FogType <> ftNone then
  begin
    FogVisibilityRangeScaled :=
      FogNode.FdVisibilityRange.Value * FogNode.TransformScale;

    if FogNode.FdVolumetric.Value then
    begin
      FogVolumetricVisibilityStart :=
        FogNode.FdVolumetricVisibilityStart.Value * FogNode.TransformScale;

      { Calculation of Distance for volumetric fog
        below is analogous to TVRMLMeshRenderer.DoBeforeGLVertex
        caculation. }

      VertProjected := PointOnLineClosestToPoint(
        ZeroVector3Single, FogNode.FdVolumetricDirection.Value, VertexPos);
      Distance := VectorLen(VertProjected);
      if not AreParallelVectorsSameDirection(
        VertProjected, FogNode.FdVolumetricDirection.Value) then
        Distance := -Distance;
      { Now I want
        - Distance = FogVolumetricVisibilityStart -> 0
        - Distance = FogVolumetricVisibilityStart + X -> X
          (so that Distance = FogVolumetricVisibilityStart +
          FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
      Distance -= FogVolumetricVisibilityStart;

      { When Distance < 0 our intention is to have no fog.
        So Distance < 0 should be equivalent to Distance = 0. }
      MaxTo1st(Distance, 0);

      if Distance >= FogVisibilityRangeScaled - SingleEqualityEpsilon then
        Color := FogNode.FdColor.Value else
        ApplyDistance(Distance);
    end else
    begin
      DistanceSqr := PointsDistanceSqr(Position, VertexPos);

      if DistanceSqr >= Sqr(FogVisibilityRangeScaled - SingleEqualityEpsilon) then
        Color := FogNode.FdColor.Value else
        ApplyDistance(Sqrt(DistanceSqr));

    end;
  end;
end;

end.
