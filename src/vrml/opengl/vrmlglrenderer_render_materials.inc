{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

procedure TVRMLGLRenderer.RenderMaterialsBegin;

  { Apply material (both VRML 1.0 and 2.0).
    Assumes MaterialLit is already calculated.
    This sets full glMaterial and glColor. }
  procedure RenderMaterial(
    const AmbientColor, DiffuseColor, SpecularColor, EmissiveColor: TVector3Single;
    const UnLitColor: TVector3Single;
    const ShininessExp, Opacity: Single);
  begin
    if Attributes.PureGeometry or (not Attributes.ControlMaterials) then Exit;

    MaterialOpacity := Opacity;

    if MaterialLit then
    begin
      glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(AmbientColor, Opacity));
      glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(DiffuseColor, Opacity));
      glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4Single(SpecularColor, Opacity));
      glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Vector4Single(EmissiveColor, Opacity));
      glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, ShininessExp);

      { Use also glColor for unlit things. This way this code will be able
        to work regardless of glEnable(GL_LIGHTING) state (even if we
        will put this on GL display list and then will call such list
        with various states of glEnable(GL_LIGHTING)). }
      glColorv(Vector4Single(DiffuseColor, Opacity));
    end else
    begin
      glColorv(Vector4Single(UnLitColor, Opacity));
    end;
  end;

  { Sets current OpenGL material and color properties for VRML 1.0 material
    (index 0). }
  procedure Material_1(M: TNodeMaterial_1);
  var
    UnlitColor: TVector3Single;
  begin
    { Usually, UnlitColor should be from EmissiveColor3Single.

      However, IndexedLineSet in VRML 1.0 is an exception.
      NodeLit result for it is determined by whether it has normals
      (this is contrary to other nodes, that are always lit or unlit;
      well, forgetting about the "OnlyEmissiveMaterial" exceptional situation
      for the moment).

      If it's determined to be unlit by NodeLit
      (so when OnlyEmissiveMaterial is @false, then only NodeLit = @false
      can force using UnlitColor), we want the unlit color to be the same
      as when it was lit but we were watching with GL_LIGHTING off.
      RenderMaterial uses DiffuseColor in such case.

      Admittedly, I'm doing it all by just guessing what user may want...
      VRML 1.0 spec doesn't specify here what should really be done, i.e. what
      unlit color should be used. }
    if (CurrentGeometry is TNodeIndexedLineSet_1) and (not M.OnlyEmissiveMaterial) then
      UnlitColor := M.DiffuseColor3Single(0) else
      UnlitColor := M.EmissiveColor3Single(0);

    RenderMaterial(
      M.AmbientColor3Single(0),
      M.DiffuseColor3Single(0),
      M.SpecularColor3Single(0),
      M.EmissiveColor3Single(0),
      UnlitColor,
      M.ShininessExp(0),
      M.Opacity(0));
  end;

  { Sets current OpenGL material and color properties for VRML >= 2.0 material.
    Material may be nil. }
  procedure Material_2(Material: TNodeMaterial_2);
  var
    UnLitColor: TVector3Single;
  begin
    if Material <> nil then
    begin
      UnLitColor := White3Single;

      { Otherwise Circle2D would be always white.
        X3D spec doesn't say anything about this, so I use diffuse color. }
      if CurrentGeometry is TNodeCircle2D then
        UnLitColor := Material.FdDiffuseColor.Value;

      RenderMaterial(
        VectorScale(Material.FdDiffuseColor.Value,
          Material.FdAmbientIntensity.Value),
        Material.FdDiffuseColor.Value,
        Material.FdSpecularColor.Value,
        Material.FdEmissiveColor.Value,
        UnLitColor,
        Material.ShininessExp,
        Material.Opacity);
    end else
    begin
      RenderMaterial(
        { Colors for lighting don't matter here }
        White3Single, White3Single, White3Single, White3Single,
        { From VRML 2.0 spec about Appearance node: when no Material is
          specified, object is unlit and unlit object color is (1, 1, 1). }
        White3Single,
        { shininess } 0, { opacity } 1);
    end;
  end;

var
  M1: TNodeMaterial_1;
  M2: TNodeMaterial_2;
  FogImmune, VRML1OnlyEmissive, VRML2Unlit: boolean;
begin
  { calculate material parameters:
    M1, M2, FogImmune, VRML1OnlyEmissive, VRML2Unlit }
  M1 := nil;
  M2 := nil;
  if CurrentGeometry is TVRMLGeometryNode_1 then
  begin
    { VRML 1.0 }
    M1 := CurrentState.LastNodes.Material;
    FogImmune := M1.FdFogImmune.Value;
    { VRML 1.0 specification states that this is a special case
      and we should treat EmissiveColor as precalculated color.
      So we can turn lighting off to improve speed (in fact, we are supposed
      to do that). }
    VRML1OnlyEmissive := M1.OnlyEmissiveMaterial;
    VRML2Unlit := false;
  end else
  begin
    { VRML 2.0 }
    if CurrentState.ShapeNode <> nil then
    begin
      M2 := CurrentState.ShapeNode.Material;
      FogImmune := (M2 <> nil) and M2.FdFogImmune.Value;
      VRML1OnlyEmissive := false;
      { VRML 2.0 spec says that when Material is not assigned, shape is unlit. }
    end else
    begin
      VRMLWarning(vwSerious, 'VRML node "' + CurrentGeometry.NodeTypeName +
        '" can be specified only in a "geometry" field of "Shape" node');
      FogImmune := false;
      VRML1OnlyEmissive := false;
    end;
    VRML2Unlit := M2 = nil;
  end;

  { calculate and apply MaterialTemporaryDisabledFog }
  MaterialTemporaryDisabledFog := FogEnabled and FogImmune;
  if MaterialTemporaryDisabledFog then
    glDisable(GL_FOG);

  { calculate and apply MaterialLit }
  MaterialLit :=
    { When doing bump mapping <> bmNone, we always turn on lighting.
      We will calculate lighting using our equations, and actually
      it's not important whether light will be enabled or not.
      But it's important that we pass material values for ambient and diffuse
      (shader uses them), so we should behave like the node is lit. }
    (ShapeBumpMappingUsed <> bmNone) or
    (
      (not VRML1OnlyEmissive) and
      (not VRML2Unlit) and
      not (
        (CurrentGeometry is TNodePointSet_1) or
        (CurrentGeometry is TNodePointSet_2) or
        { VRML 1.0 specification states that lines normals aren't generated
          --- instead, lines are drawn unlit when there are no normals specified. }
        ( (CurrentGeometry is TNodeIndexedLineSet_1) and
          (CurrentState.LastNodes.Normal.FdVector.Count = 0) ) or
        { VRML 2.0 specification says that IndexedLineSet should always be unlit,
          X3D spec adds that LineSet is also always unlit. }
        (CurrentGeometry is TNodeIndexedLineSet_2) or
        (CurrentGeometry is TNodeLineSet) or
        (CurrentGeometry is TNodeCircle2D)
      )
    );
  if not MaterialLit then
  begin
    { PushAttrib is used to save GL_LIGHTING enabled state,
      as we cannot predict it (it's up to the VRMLGLRenderer caller
      whether the light is turned on). Obviously we don't want to explicitly
      query GL_LIGHTING enabled state, push/pop is better.

      Note that we use GL_LIGHTING_BIT instead of GL_ENABLE_BIT:
      GL_ENABLE_BIT would also push/pop GL_FOG enabled state,
      and this could cause problems when mixed with fogImmune. }
    glPushAttrib(GL_LIGHTING_BIT);
    glDisable(GL_LIGHTING);
  end;

  { set material. At the end, since this uses MaterialLit, M1, M2. }
  if CurrentGeometry is TVRMLGeometryNode_1 then
    Material_1(M1) else
    Material_2(M2);
end;

procedure TVRMLGLRenderer.RenderMaterialsEnd;
begin
  if not MaterialLit then
  begin
    glPopAttrib;
    MaterialLit := true; { restore default MaterialLit value, just in case }
  end;

  { Enable fog again, if needed. This was next shape rendering will
    start with fog enabled (if it should be, according to FogEnabled). }
  if MaterialTemporaryDisabledFog then
  begin
    glEnable(GL_FOG);
    MaterialTemporaryDisabledFog := false;
  end;
end;
