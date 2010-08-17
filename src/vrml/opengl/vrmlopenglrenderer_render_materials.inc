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

{ Judge whether the node can be lit, looking only at Node (not at State,
  Material etc.).

  Assumes already calculated:
  - MeshRenderer,
  - MeshRenderer.BumpMappingMethod
    (set by TBumpMappingRenderer.Enable called from RenderTexturesBegin),
  - (MeshRenderer as TIndexedLineSet_1Renderer).NorImplementation
    (set in TIndexedLineSet_1Renderer.Create). }
function TVRMLOpenGLRenderer.NodeLit(Node: TVRMLGeometryNode): boolean;
begin
  if ( (MeshRenderer <> nil) and
       (MeshRenderer.BumpMappingMethod <> bmNone) ) then
  begin
    { When doing bump mapping by bmMultiTex*, we turn off lighting.
      Bump mapping will calculate it per-pixel anyway.

      In case of bmMultiTexDotNormalized, UnlitColor will be
      applied (mixed with original texture to give primary_color).
      In case of bmMultiTexDotNotNormalized,
      UnlitColor will be ignored (we'll use glColor to pass light dir). }

    { When doing bump mapping in bmGLSLAll, we always turn on lighting.

      We will calculate lighting using our equations, and actually
      it's not important whether light will be enabled or not.
      But it's important that we pass material values for ambient and diffuse
      (shader uses them), so we should behave like the node is lit.

      (Actually, this will not work when Material node is @nil... then we can't
      pass any ambient/diffuse lighting values, and results of rendering this
      with bump mapping are undefined. TODO: fix.)
    }

    Result := MeshRenderer.BumpMappingMethod in bmGLSLAll;
  end else
  begin
    Result := not (
      (Node is TNodePointSet_1) or
      (Node is TNodePointSet_2) or
      { VRML 1.0 specification states that lines normals aren't generated
        --- instead, lines are drawn unlit when there are no normals specified. }
      ( (Node is TNodeIndexedLineSet_1) and
        ((MeshRenderer as TIndexedLineSet_1Renderer).NorImplementation = niNone) ) or
      { VRML 2.0 specification says that IndexedLineSet should always be unlit,
        X3D spec adds that LineSet is also always unlit. }
      (Node is TNodeIndexedLineSet_2) or
      (Node is TNodeLineSet) or
      (Node is TNodeCircle2D)
    );
  end;
end;

procedure TVRMLOpenGLRenderer.Render_MaterialsBegin;
var
  M1: TNodeMaterial_1;
  M2: TNodeMaterial_2;
  FogImmune, VRML1OnlyEmissive, VRML2Unlit: boolean;
begin
  Material1BoundNumber := -1; { no VRML 1.0 material is bound now.
    Initial Render_MaterialsBegin_1 will set it to 0. }

  { calculate material parameters:
    M1, M2, FogImmune, VRML1OnlyEmissive, VRML2Unlit }
  if CurrentState.ShapeNode <> nil then
  begin
    M2 := CurrentState.ShapeNode.Material;
    FogImmune := (M2 <> nil) and M2.FdFogImmune.Value;
    VRML1OnlyEmissive := false;
    { VRML 2.0 spec says that when Material is not assigned, shape is unlit. }
    VRML2Unlit := M2 = nil;
  end else
  begin
    M1 := CurrentState.LastNodes.Material;
    FogImmune := M1.FdFogImmune.Value;
    { VRML 1.0 specification states that this is a special case
      and we should treat EmissiveColor as precalculated color.
      So we can turn lighting off to improve speed (in fact, we are supposed
      to do that). }
    VRML1OnlyEmissive := M1.OnlyEmissiveMaterial;
    VRML2Unlit := false;
  end;

  { calculate and apply MaterialFromColorEnabled }
  Assert(not MaterialFromColorEnabled);
  MaterialFromColorEnabled := (MeshRenderer <> nil) and
    (MeshRenderer.NeedsBindMaterial1InsideBeginEnd or
     MeshRenderer.NeedsSetColor) and NodeLit(CurrentGeometry);
  if MaterialFromColorEnabled then
  begin
    { For lit nodes, we want to set both unlit glColor and lit glMaterial
      in SetColor.

      Using GL_COLOR_MATERIAL is the best way to do this, this also works
      much faster and more memory-efficient (AFAIK, OpenGL is prepared
      to handle different glColor-per-vertex efficiently, but changing
      whole glMaterial-per-vertex is not efficient (as glColor is pushed
      with vertex data into the pipeline, while changing material settings
      requires flushing part of current pipeline)). }

    { When both NeedsBindMaterial1InsideBeginEnd and NeedsSetColor
      are true, what to do?

      TAbstractColMatTexCoordinateRenderer
      descends from TAbstractMatTexCoordinateRenderer,
      so NeedsSetColor takes precence over material color.
      So set MaterialFromColor following NeedsSetColor needs. }
    if MeshRenderer.NeedsSetColor then
      MaterialFromColor := GL_DIFFUSE else
    begin
      Assert(MeshRenderer.NeedsBindMaterial1InsideBeginEnd);
      if (M1 <> nil) and VRML1OnlyEmissive then
        MaterialFromColor := GL_EMISSION else
        { Then assume it's the diffuse that changes most often }
        MaterialFromColor := GL_DIFFUSE;
    end;

    glColorMaterial(GL_FRONT_AND_BACK, MaterialFromColor);
    glEnable(GL_COLOR_MATERIAL);
  end;

  { calculate and apply MaterialTemporaryDisabledFog }
  MaterialTemporaryDisabledFog := FogEnabled and FogImmune;
  if MaterialTemporaryDisabledFog then
    glDisable(GL_FOG);

  { calculate and apply MaterialLit }
  MaterialLit := (not VRML1OnlyEmissive) and NodeLit(CurrentGeometry) and
    (not VRML2Unlit);
  if not MaterialLit then
  begin
    { PushAttrib is used to save GL_LIGHTING enabled state,
      as we cannot predict it (it's up to the VRMLOpenGLRenderer caller
      whether the light is turned on). Obviously we don't want to explicitly
      query GL_LIGHTING enabled state, push/pop is better.

      Note that we use GL_LIGHTING_BIT instead of GL_ENABLE_BIT:
      GL_ENABLE_BIT would also push/pop GL_FOG enabled state,
      and this could cause problems when mixed with fogImmune. }
    glPushAttrib(GL_LIGHTING_BIT);
    glDisable(GL_LIGHTING);
  end;

  { set material. At the end, since this uses MaterialLit, M2. }
  if CurrentGeometry is TVRMLGeometryNode_1 then
    Render_BindMaterial_1(0, false) else
    Render_MaterialsBegin_2(M2);
end;

procedure TVRMLOpenGLRenderer.Render_MaterialsEnd;
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

  if MaterialFromColorEnabled then
  begin
    glDisable(GL_COLOR_MATERIAL);
    MaterialFromColorEnabled := false;
  end;
end;

{ Apply material (both VRML 1.0 and 2.0).
  Assumes MaterialLit is already calculated.

  When not InsideBeginEnd, this sets full glMaterial and glColor.
  This is Ok to be called even between glBegin/glEnd,
  but it's very unoptimal then.

  When InsideBeginEnd, we use limited but faster (when used often,
  especially between glBegin/glEnd) version. It uses GL_COLOR_MATERIAL
  (and depends on MaterialFromColorEnabled). }
procedure TVRMLOpenGLRenderer.Render_Material(
  const AmbientColor, DiffuseColor, SpecularColor, EmissiveColor: TVector3Single;
  const UnLitColor: TVector3Single;
  const ShininessExp, Opacity: Single;
  const InsideBeginEnd: boolean);
begin
  if Attributes.PureGeometry or (not Attributes.ControlMaterials) then Exit;

  Material_BoundOpacity := Opacity;

  { What does it mean when InsideBeginEnd is true but
    MaterialFromColorEnabled is false?
    Assuming that all classes correctly set NeedsBindMaterial1InsideBeginEnd
    before using InsideBeginEnd = true.
    It means that MaterialsBegin decided GL_COLOR_MATERIAL is not needed,
    because NodeLit was false.

    So don't worry about it, the later code will work fine then. }

  if InsideBeginEnd and MaterialFromColorEnabled then
  begin
    case MaterialFromColor of
      GL_DIFFUSE : glColorv(Vector4f(Attributes.ColorModulated(DiffuseColor), Opacity));
      GL_EMISSION: glColorv(Vector4f(Attributes.ColorModulated(EmissiveColor), Opacity));
      else raise EInternalError.Create('MaterialFromColor?');
    end;
  end else
  if MaterialLit then
  begin
    glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT,
      Vector4f(Attributes.ColorModulated(AmbientColor), Opacity));
    glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE,
      Vector4f(Attributes.ColorModulated(DiffuseColor), Opacity));
    glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR,
      Vector4f(Attributes.ColorModulated(SpecularColor), Opacity));
    glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION,
      Vector4f(Attributes.ColorModulated(EmissiveColor), Opacity));
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, ShininessExp);

    { Use also glColor for unlit things. This way this code will be able
      to work regardless of glEnable(GL_LIGHTING) state (even if we
      will put this on GL display list and then will call such list
      with various states of glEnable(GL_LIGHTING)). }
    glColorv(Vector4f(Attributes.ColorModulated(DiffuseColor), Opacity));
  end else
  begin
    glColorv(Vector4f(Attributes.ColorModulated(UnLitColor), Opacity));
  end;
end;

{ Sets current OpenGL material and color properties for material
  number MatNum from CurrentState.LastNodes.Material.
  This is ignored when CurrentGeometry is not TVRMLGeometryNode_1
  (then VRML 2.0 approach to materials should be used).

  Pass InsideBeginEnd to indicate that this is (possibly) called
  inside glBegin/glEnd. Then the faster (but more limited) approach
  will be used. The faster approach uses GL_COLOR_MATERIAL, instead
  of changing the whole glMaterial.

  Reasoning: Changing whole glMaterial
  is allowed but prohibitively slow within glBegin/glEnd (or just: too often,
  like for each face or vertex). It also eats a *lot* of memory in case of
  display lists (~130 MB for "castle" spider_queen and spider animations,
  on NVidia GeForce FX 5200). So using GL_COLOR_MATERIAL is required
  for sensible speed and memory use. }
procedure TVRMLOpenGLRenderer.Render_BindMaterial_1(MatNum: Integer;
  const InsideBeginEnd: boolean);
var
  UnlitColor: TVector3Single;
  M: TNodeMaterial_1;
begin
  if (not (CurrentGeometry is TVRMLGeometryNode_1)) or
     (MatNum = Material1BoundNumber) then
    Exit;

  Material1BoundNumber := MatNum;

  M := CurrentState.LastNodes.Material;

  { Usually, UnlitColor should be from EmissiveColor3Single.

    However, IndexedLineSet in VRML 1.0 is an exception. Note that
    NodeLit may sometimes decide to draw it unlit, and sometimes lit
    (this is contrary to other nodes, that are always lit or unlit;
    well, forgetting about the "OnlyEmissiveMaterial" exceptional situation
    for the moment).

    If it's determined to be unlit by NodeLit
    (so when OnlyEmissiveMaterial is @false, then only NodeLit = @false
    can force using UnlitColor), we want the unlit color to be the same
    as when it was lit but we were watching with GL_LIGHTING off.
    Render_Material uses DiffuseColor in such case.

    Admittedly, I'm doing it all by just guessing what user may want...
    VRML 1.0 spec doesn't specify here what should really be done, i.e. what
    unlit color should be used. }
  if (CurrentGeometry is TNodeIndexedLineSet_1) and (not M.OnlyEmissiveMaterial) then
    UnlitColor := M.DiffuseColor3Single(MatNum) else
    UnlitColor := M.EmissiveColor3Single(MatNum);

  Render_Material(
    M.AmbientColor3Single(MatNum),
    M.DiffuseColor3Single(MatNum),
    M.SpecularColor3Single(MatNum),
    M.EmissiveColor3Single(MatNum),
    UnlitColor,
    M.ShininessExp(MatNum),
    M.Opacity(MatNum),
    InsideBeginEnd);
end;

{ Binds current VRML 2.0 Material. }
procedure TVRMLOpenGLRenderer.Render_MaterialsBegin_2(Material: TNodeMaterial_2);
var
  UnLitColor: TVector3Single;
begin
  Assert(not (CurrentGeometry is TVRMLGeometryNode_1));

  if CurrentState.ShapeNode = nil then
  begin
    VRMLWarning(vwSerious, 'VRML node "' + CurrentGeometry.NodeTypeName +
      '" can be specified only in a "geometry" field of "Shape" node');
    Exit;
  end;

  if Material <> nil then
  begin
    UnLitColor := White3Single;

    { Otherwise Circle2D would be always white.
      X3D spec doesn't say anything about this, so I use diffuse color. }
    if CurrentGeometry is TNodeCircle2D then
      UnLitColor := Material.FdDiffuseColor.Value;

    Render_Material(
      VectorScale(Material.FdDiffuseColor.Value,
        Material.FdAmbientIntensity.Value),
      Material.FdDiffuseColor.Value,
      Material.FdSpecularColor.Value,
      Material.FdEmissiveColor.Value,
      UnLitColor,
      Material.ShininessExp,
      Material.Opacity, { inside glBegin/End } false);
  end else
  begin
    Render_Material(
      { Colors for lighting don't matter here }
      White3Single, White3Single, White3Single, White3Single,
      { From VRML 2.0 spec about Appearance node: when no Material is
        specified, object is unlit and unlit object color is (1, 1, 1). }
      White3Single,
      { shininess } 0, { opacity } 1, { inside glBegin/End } false);
  end;
end;

{ Sets current color. Initially, this is designed for setting VRML 2.0
  color, so it can be called after you called Render_BindMaterial_2.

  But it's also functional for VRML 1.0 --- although this is only for
  OnRadianceTransfer or OnVertexColor now.
  Normally, in VRML 1.0 Render_BindMaterial_1
  is the only way to change color.

  This sets the RGB part of the unlit color,
  and the RGB part of diffuseColor of lit material.
  This is good for VRML 2.0 nodes that use colors to either
  set precalculated color (when unlit, i.e. material is NULL)
  or to change diffuseColor (when lit, see specification "lighting equaltions",
  term O_Drgb).

  Actually, setting the lit material color is not done if not NodeLit
  (no point in wasting 1 OpenGL call if material will never be used).
  This optimization is implementd in Render_MaterialsBegin (although it affects
  this procedure). There we check for NodeLit before turning on
  GL_COLOR_MATERIAL, since when not NodeLit, material has never any use. }
procedure TVRMLOpenGLRenderer.SetColor(const Color: TVector3Single);
begin
  { We use Material_BoundOpacity since we do not want to change
    material opacity here, only it's RGB part. }

  if Attributes.PureGeometry or
     (not Attributes.ControlMaterials) then
    Exit;

  glColorv(Vector4f(Attributes.ColorModulated(Color), Material_BoundOpacity));
end;
