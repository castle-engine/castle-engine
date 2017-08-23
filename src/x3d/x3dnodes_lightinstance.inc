{
  Copyright 2002-2017 Michalis Kamburelis.

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
  TLightInstance = object
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
      and transform it by T3DTransform or direct OpenGL modelview changes.
      By default (WorldCoordinates = false) we assume that light is defined
      in scene space, so it will be transformed by the whole modelview matrix
      (camera matrix with scene transformations).
      When this is true, during rendering we take care to transform this light
      only by camera matrix (not additional scene transformation).
      Useful for example for headlight. }
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
      during calculations. OpenGL also clamps only at the end. }
    function Contribution(
      const Point: TVector3; const PointPlaneNormal: TVector3;
      State: TX3DGraphTraverseState;
      const CamPosition: TVector3;
      const DiffuseTextureColor: TCastleColorRGB): TVector3;

    { Light contribution, without knowing the camera or full material.
      We have a 3D vertex, we know it lies on a plane with given normal,
      and we have light information. Try to calculate VRML/X3D lighting
      equation as close as possible to the fully correct version (see regular
      @link(Contribution) method) with this information.

      The specular lighting part must be simply ignored in this case.  }
    function ContributionCameraIndependent(
      const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3;
      const DiffuseTextureColor: TCastleColorRGB): TVector3;
  end;
  PLightInstance = ^TLightInstance;

  TLightInstancesList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TLightInstance>)
  public
    { Find given light node. Return -1 if not found. }
    function IndexOfNode(Node: TAbstractLightNode): integer;
    { Find light with given node name. Return @nil if not found. }
    function FindName(NodeName: string): PLightInstance;
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}

    { Append List to our contents, setting every light's WorldCoordinates = @true. }
    procedure AppendInWorldCoordinates(const AList: TLightInstancesList);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TLightInstance ------------------------------------------------------------- }

function TLightInstance.Contribution(
  const Point: TVector3; const PointPlaneNormal: TVector3;
  State: TX3DGraphTraverseState;
  const CamPosition: TVector3;
  const DiffuseTextureColor: TCastleColorRGB): TVector3;
{$I x3dnodes_lightcontribution.inc}

function TLightInstance.ContributionCameraIndependent(
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3;
  const DiffuseTextureColor: TCastleColorRGB): TVector3;
{$define CAMERA_INDEP}
{$I x3dnodes_lightcontribution.inc}
{$undef CAMERA_INDEP}

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
    if List^[Result].Node = Node then
      Exit;
  Result := -1;
end;

function TLightInstancesList.FindName(NodeName: string): PLightInstance;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Ptr(I);
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
      if not LightInstanceEquals(List^[I], TLightInstancesList(SecondValue).List^[I]) then
        Exit(false);
end;

procedure TLightInstancesList.AppendInWorldCoordinates(const AList: TLightInstancesList);
var
  OldCount: Integer;
  I: Integer;
begin
  OldCount := Count;
  Count := Count + AList.Count;
  for I := 0 to AList.Count - 1 do
  begin
    List^[OldCount + I] := AList.List^[I];
    List^[OldCount + I].WorldCoordinates := true;
  end;
end;

{$endif read_implementation}
