{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Light source instance in the scene. References VRML/X3D
    light source node (see @link(Node)), and keeps track of light source
    transformation in the 3D world. For the sake of speed
    also stores a couple of light's properties already multiplied
    by the transformation.

    This record may be initialized only by TAbstractLightNode.CreateLightInstance.
    Update it (when transform changes) by TAbstractLightNode.UpdateLightInstance. }
  TLightInstance = record
    Node: TAbstractLightNode;

    Transform: TMatrix4;
    TransformScale: Single;

    { Light location, already transformed by the @link(Transform) matrix.
      For TAbstractPositionalLightNode lights. }
    Location: TVector3;

    { Light direction, already normalized and
      transformed by the @link(Transform) matrix.
      For spot and directional lights. }
    Direction: TVector3;

    { Light radius, already transformed by the @link(Transform) matrix.
      For lights with radius (positional lights in VRML >= 2.0,
      that is TAbstractPositionalLightNode with HasRadius = true). }
    Radius: Single;

    { Are light location, direction (in this class and inside Node fields)
      expressed in world coordinates. If not, they are expressed in scene
      coordinates.

      This matters if you render the scene using TCastleScene,
      and transform it (by wrapping it in TCastleTransform, or by directly
      changing TCastleScene transformation properties inherited from
      TCastleTransform).
      By default (WorldCoordinates = false) we assume that light is defined
      in scene space, so it will be transformed by the whole modelview matrix
      (camera matrix with scene transformations).
      When this is true, during rendering we take care to transform this light
      only by camera matrix (not additional scene transformation). }
    WorldCoordinates: boolean;

    { Position expressed in homogeneous coordinates.
      For positional lights, the last component is always 1.
      For directional lights, the last component is always 0. }
    function Position: TVector4;

    { Light contribution to the specified vertex color.
      This can be used by software renderers (ray-tracers etc.)
      to calculate pixel color following VRML/X3D specifications.
      TX3DGraphTraverseState.Emission should be added to
      TLightInstance.Contribution (for each light),
      and resulting color should be processed by TFogNode.ApplyFog.

      We do not clamp color components to (0, 1). This would be a waste of time,
      you should clamp only at the end (or never). This also allows
      to multiply / accumulate values outside of the (0, 1) range
      during calculations. OpenGL also clamps only at the end.

      MainTextureColor is color of the "main" texture.
      See @link(TAppearanceNode.MainTexture) for definition what is this texture. }
    function Contribution(
      const Point: TVector3; const PointPlaneNormal: TVector3;
      State: TX3DGraphTraverseState;
      const CamPosition: TVector3;
      const MainTextureColor: TCastleColorRGB): TVector3;
  end;
  PLightInstance = ^TLightInstance;

  TLightInstancesList = class({$ifdef FPC}specialize{$endif} TStructList<TLightInstance>)
  public
    { Find given light node. Return -1 if not found. }
    function IndexOfNode(Node: TAbstractLightNode): integer;
    { Find light with given node name. Return @nil if not found. }
    function FindName(NodeName: string): PLightInstance;
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TLightInstance ------------------------------------------------------------- }

function TLightInstance.Contribution(
  const Point: TVector3; const PointPlaneNormal: TVector3;
  State: TX3DGraphTraverseState;
  const CamPosition: TVector3;
  const MainTextureColor: TCastleColorRGB): TVector3;

{ Note: In some cases we have to do something different than VRML 2.0/X3D spec:

  @unorderedList(
    @item(
      For VRML 1.0 SpotLight, we have to calculate spot light differently
      (because VRML 1.0 SpotLight gives me dropOffRate instead of
      beamWidth), so we use spot factor equation following OpenGL equations.)

    @item(
      VRML 97 lighting equations suggest one-sided lighting, only where
      the normal points out. In my opinion, one-sided lighting is not useful,
      and also our OpenGL rendering uses two-sides lighting.
      (One reason for OpenGL rendering is to integrate nicely with flat mirrors,
      where you have to flip normals. So OpenGL renderer always gives
      vectors from CCW, and so uses two-side to not favor any side of the face.))
  )
}

var
  { kierunek od Point do zrodla swiatla, normalized }
  LightDirNorm: TVector3;
  { wektor normalny do powierzchni (skierowany w strone Light) }
  Normal: TVector3;

  function CalculateSpot_1(const SpotLight: TSpotLightNode_1): Single;
  var
    CosSpotAngle, SpotAngle: Single;
  begin
    { Uzywamy dropOffRate VRMLa 1.0 zeby sprawic by spot byl
      bardziej "skupiony" zgodnie z rownaniami swiatla OpenGLa.
      W VRMLu 97 skupienie spota jest okreslane inaczej, przez beamWidth. }
    CosSpotAngle := TVector3.DotProduct(-LightDirNorm, Direction);
    { TODO: optimize: no need to calc ArcCos here if we would precalc
      CosCutOffAngle for every light. }
    SpotAngle := ArcCos(CosSpotAngle);
    if SpotAngle <= SpotLight.FdCutOffAngle.Value then
      Result := Power(CosSpotAngle, SpotLight.SpotExponent) else
      Result := 0;
  end;

  function CalculateSpot(const SpotLight: TSpotLightNode): Single;
  var
    CosSpotAngle, SpotAngle, SpotCO, SpotBW: Single;
  begin
    CosSpotAngle := TVector3.DotProduct(-LightDirNorm, Direction);
    SpotAngle := ArcCos(CosSpotAngle);
    SpotCO := SpotLight.FdCutOffAngle.Value;
    SpotBW := SpotLight.FdBeamWidth.Value;

    if SpotAngle < SpotCO then
    begin
      if SpotAngle > SpotBW then
        Result := (SpotAngle - SpotCO) / (SpotBW - SpotCO) else
        Result := 1;
    end else
      Result := 0;
  end;

  function AmbientFactor(const M: TMaterialInfo): TCastleColorRGB;
  var
    MaterialAmbientColor: TCastleColorRGB;
  begin
    if not (M is TPhongMaterialInfo) then
      Result := TVector3.Zero
    else
    begin
      MaterialAmbientColor := TPhongMaterialInfo(M).AmbientColor;
      Result := MaterialAmbientColor * Node.FdAmbientIntensity.Value;
    end;
  end;

  function DiffuseFactor(const M: TMaterialInfo): TCastleColorRGB;
  var
    MaterialDiffuseColor: TCastleColorRGB;
  begin
    if M is TPhongMaterialInfo then
      MaterialDiffuseColor := TPhongMaterialInfo(M).DiffuseColor
    else
    if M is TPhysicalMaterialInfo then
      MaterialDiffuseColor := TPhysicalMaterialInfo(M).BaseColor
    else
      Exit(TVector3.Zero);

    Result :=
      (MaterialDiffuseColor * MainTextureColor) *
      (Node.FdIntensity.Value * TVector3.DotProduct(Normal, LightDirNorm));
  end;

  function SpecularFactor(const M: TMaterialInfo): TCastleColorRGB;
  var
    { average of "vector to camera pos" and LightDirNorm }
    CameraAndLightHalfway: TVector3;
    MaterialSpecularColor: TVector3;
    MaterialShininessExp: Single;
  begin
    if M is TPhongMaterialInfo then
    begin
      MaterialSpecularColor := TPhongMaterialInfo(M).SpecularColor;
      MaterialShininessExp := TPhongMaterialInfo(M).ShininessExp;
      { niestety musimy znormalizowac (CamPosition - Point)
        (zeby mial taki sam wklad w sume co LightDirNorm) i wynik
        (ktory musi byc znormalizowany zeby nadawal sie do wyliczania
        cosinusa jako prostego dot product; jak zwykle, nalezy pamietac
        ze suma dwoch wektorow dlugosci 1 niekoniecznie (rzadko) jest wektorem
        dlugosci 2 (wiec skalowanie przez 1/2 nie rozwiazaloby tu problemu)) }
      CameraAndLightHalfway := (
        (CamPosition - Point).Normalize + LightDirNorm).Normalize;
      Result :=
        MaterialSpecularColor *
        (Node.FdIntensity.Value *
          Power( Max(TVector3.DotProduct(Normal, CameraAndLightHalfway), Single(0.0)),
            MaterialShininessExp));
    end else
      Result := TVector3.Zero;
  end;

var
  Spot: Single;
  Attenuat: Single;
  PosLightNode: TAbstractPositionalLightNode;
  Factors: TVector3;
  M: TMaterialInfo;
begin
  M := State.MaterialInfo;
  if M = nil then
    { Material = nil means "implicit unlit material", it has only emissive
      which will be added outside of this function. }
    Exit(TVector3.Zero);

  if (not Node.FdOn.Value) then
    Exit(TVector3.Zero);

  { TODO: arg dla attenuation powinien byc w ukladzie transformacji swiatla.
    For now lights with non-trivial attenuation put under some transformation
    that does distance scaling will not render as required by VRML 2.0 spec.
    Czyli musze implementowac InvertTransformation ? }
  if Node is TAbstractPositionalLightNode then
  begin
    PosLightNode := TAbstractPositionalLightNode(Node);
    if PosLightNode.DistanceNeededForAttenuation then
      Attenuat := PosLightNode.CalculateAttenuation( PointsDistance(Point, Location) )
    else
      Attenuat := PosLightNode.CalculateAttenuation( 0.0 );
  end else
    Attenuat := 1;

  { LightDirNorm is called "L" in the equation in VRML spec }
  if Node is TAbstractPositionalLightNode then
    LightDirNorm := (Location - Point).Normalize
  else
    LightDirNorm := -Direction;

  { wektory Normal i LightDirNorm musza wychodzic z tej samej strony
    powierzchni TriNormPlaneSingle (musimy przeciez zrobic cos co w OpenGLu
    jest okreslane jako two-sided lighting, czyli wziac albo Normal albo
    -Normal, w zaleznosci od pozycji swiatla.). }

  { Normal is called "N" in the equation in VRML spec }
  Normal := PlaneDirInDirection(PointPlaneNormal, LightDirNorm);

  { oblicz Spot - dla SpotLight okresla on jak bardzo punkt jest w spocie,
    dla innych swiatel jest zawsze = 1. }
  if Node is TSpotLightNode_1 then
    Spot := CalculateSpot_1(TSpotLightNode_1(Node))
  else
  if Node is TSpotLightNode then
    Spot := CalculateSpot(TSpotLightNode(Node))
  else
    Spot := 1;

  Factors :=
    AmbientFactor(M) +
    DiffuseFactor(M) +
    SpecularFactor(M);

  Result := (Node.FdColor.Value * (Attenuat * Spot)) * Factors;
end;

function TLightInstance.Position: TVector4;
begin
  if Node is TAbstractPositionalLightNode then
    Result := Vector4(Location, 1) else
    Result := Vector4(-Direction, 0);
end;

{ TLightInstancesList ----------------------------------------------------- }

function TLightInstancesList.IndexOfNode(Node: TAbstractLightNode): integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Node = Node then
      Exit;
  Result := -1;
end;

function TLightInstancesList.FindName(NodeName: string): PLightInstance;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := PLightInstance(Ptr(I));
    if Result^.Node.X3DName = NodeName then
      Exit;
  end;
  Result := nil;
end;

function TLightInstancesList.Equals(SecondValue: TObject): boolean;

  function LightInstanceEquals(const L1, L2: TLightInstance): boolean;
  begin
    Result := (L1.Node = L2.Node) and
      TMatrix4.PerfectlyEquals(L1.Transform, L2.Transform);

    { No need to compare things like Location or Direction,
      as they are just precalculated based on Node and Transform. }
  end;

var
  I: Integer;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TLightInstancesList) and
    (TLightInstancesList(SecondValue).Count = Count);
  if Result then
    for I := 0 to Count - 1 do
      if not LightInstanceEquals(L[I], TLightInstancesList(SecondValue).L[I]) then
        Exit(false);
end;

{$endif read_implementation}
