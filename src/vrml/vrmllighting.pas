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
unit VRMLLighting;

interface

uses VectorMath, VRMLNodes, VRMLTriangle, Math, KambiUtils;

{ VRML 2.0 material emissiveColor for lighting equation.
  That is, the @code(O_Ergb) part of lighting equation in
  VRML 2.0 spec section "4.14.4 Lighting equations".

  This takes also into account VRML 1.0, when emissiveColor
  of VRML 1.0 material is taken.

  When LightingCalculationOn we do something special:
  we take material's diffuseColor instead of emissiveColor.
  This is supposed to be used when you use ray-tracer with
  recursion 0 (i.e., actually it's a ray-caster in this case).
  Using emissiveColor in such case would almost always
  give a completely black, useless image. }
function VRML97Emission(const IntersectNode: TTriangle;
  LightingCalculationOn: boolean): TVector3Single;

{ VRML 2.0 light contribution to the specified vertex color.
  This calculates the following equation part from VRML 2.0 spec section
  "4.14.4 Lighting equations" :

@preformatted(
  on_i * attenuation_i * spot_i * I_Lrgb
    * (ambient_i + diffuse_i + specular_i)
)

  In some cases we do something different than VRML 2.0 spec:

  @unorderedList(
    @item(
      For VRML 1.0 SpotLight, we have to calculate spot light differently
      (because VRML 1.0 SpotLight gives me dropOffRate instead of
      beamWidth), so we use spot factor equation following OpenGL equations.)

    @item(
      For VRML 1.0, we have to calculate ambientFactor in a little different way:
      see [http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#ext_light_attenuation],
      when light's ambientIntensity is < 0 then we just return 0 and
      otherwise we use material's ambientColor.)

    @item(
      VRML 97 lighting equations suggest one-sided lighting, only where
      the normal points out. In my opinion, one-sided lighting is not useful,
      and also our OpenGL rendering uses two-sides lighting.
      (One reason for OpenGL rendering is to integrate nicely with flat mirrors,
      where you have to flip normals. So OpenGL renderer always gives
      vectors from CCW, and so uses two-side to not favor any side of the face.))
  )

  We do not clamp color components to (0, 1). This would be a waste of time,
  you should clamp only at the end (or never). This also allows
  to multiply / accumulate values outside of the (0, 1) range
  during calculations. OpenGL also clamps only at the end. }
function VRML97LightContribution(const Light: TLightInstance;
  const Intersection: TVector3Single; const IntersectNode: TTriangle;
  const CamPosition: TVector3Single): TVector3Single;

{ VRML 2.0 light contribution, without knowing the camera or full material.
  We have a 3D vertex, we know it lies on a plane with given normal,
  and we have light information. We don't have a TTriangle reference,
  and we do not have any camera information. Try to calculate VRML lighting
  as close as possible to the fully correct version (see regular
  VRML97LightContribution) with this information.

  The specular lighting part must be simply ignored in this case.

  This is used by VRMLLightMap. }
function VRML97LightContribution_CameraIndependent(const Light: TLightInstance;
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3Single): TVector3Single;

type
  TVRMLFogType = type Integer;

function VRML97FogType(FogNode: TFogNode): TVRMLFogType;

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
  FogNode: TFogNode; FogType: Integer);

implementation

uses KambiWarnings;

function VRML97Emission(const IntersectNode: TTriangle;
  LightingCalculationOn: boolean): TVector3Single;
var
  M1: TMaterialNode_1;
  M2: TMaterialNode;
begin
  if IntersectNode.State.ShapeNode <> nil then
  begin
    M2 := IntersectNode.State.ShapeNode.Material;
    if M2 <> nil then
    begin
      if LightingCalculationOn then
        Result := M2.FdEmissiveColor.Value else
        Result := M2.FdDiffuseColor.Value;
    end else
    begin
      if LightingCalculationOn then
        { Default VRML 2.0 Material.emissiveColor }
        Result := ZeroVector3Single else
        { Default VRML 2.0 Material.diffuseColor }
        Result := Vector3Single(0.8, 0.8, 0.8);
    end;
  end else
  begin
    M1 := IntersectNode.State.LastNodes.Material;
    if LightingCalculationOn then
      Result := M1.EmissiveColor3Single(0) else
      Result := M1.DiffuseColor3Single(0);
  end;
end;

function VRML97LightContribution(const Light: TLightInstance;
  const Intersection: TVector3Single; const IntersectNode: TTriangle;
  const CamPosition: TVector3Single): TVector3Single;
{$I vrmllighting_97_lightcontribution.inc}

function VRML97LightContribution_CameraIndependent(const Light: TLightInstance;
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3Single)
  :TVector3Single;
{$define CAMERA_INDEP}
{$I vrmllighting_97_lightcontribution.inc}
{$undef CAMERA_INDEP}

function VRML97FogType(FogNode: TFogNode): TVRMLFogType;
begin
  if (FogNode = nil) or
     (FogNode.FdVisibilityRange.Value = 0.0) then
    Exit(-1);

  Result := ArrayPosStr(FogNode.FdFogType.Value, ['LINEAR', 'EXPONENTIAL']);
  if Result = -1 then
    OnWarning(wtMajor, 'VRML/X3D', 'Unknown fog type '''+FogNode.FdFogType.Value+'''');
end;

procedure VRML97FogTo1st(var Color: TVector3Single;
  const Position, VertexPos: TVector3Single;
  FogNode: TFogNode; FogType: Integer);
var
  FogVisibilityRangeScaled: Single;

  procedure ApplyDistance(const Distance: Single);
  var
    F: Single;
  begin
    case FogType of
      0: F := (FogVisibilityRangeScaled - Distance) / FogVisibilityRangeScaled;
      1: F := Exp(-Distance / (FogVisibilityRangeScaled - Distance));
    end;
    Color := Vector_Init_Lerp(F, FogNode.FdColor.Value, Color);
  end;

var
  FogVolumetricVisibilityStart: Single;
  Distance, DistanceSqr: Single;
  VertProjected: TVector3Single;
begin
  if FogType <> -1 then
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
