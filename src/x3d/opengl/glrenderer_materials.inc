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

var
  Lighting, MaterialAndShapeLit: boolean;
  MaterialOpacity: Single;

procedure RenderMaterialsBegin;

  { Apply material (both VRML 1.0 and 2.0).
    Assumes MaterialAndShapeLit is already calculated.
    This sets full glMaterial and glColor. }
  procedure RenderMaterial(
    const AmbientColor, DiffuseColor, SpecularColor, EmissiveColor: TVector3Single;
    const UnLitColor: TVector3Single;
    const ShininessExp, ShapeOpacity: Single);
  begin
    if Attributes.PureGeometry then Exit;

    Shader.MaterialSpecularColor := SpecularColor;
    MaterialOpacity := ShapeOpacity * Attributes.Opacity;

    if Lighting then
    begin
      glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(AmbientColor, MaterialOpacity));
      glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(DiffuseColor, MaterialOpacity));
      glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4Single(SpecularColor, MaterialOpacity));
      glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Vector4Single(EmissiveColor, MaterialOpacity));
      glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, ShininessExp);

      { If Lighting, we know that GL_LIGHTING is enabled
        (for fixed-function, or shader). So no point in setting glColor. }
    end else
    if MaterialAndShapeLit then
      { When the lighting is disabled because of Attributes.Lighting
        (not because of this shape's material), it's best to use diffuse color.
        Otherwise most of normal VRML 1.0 scenes would have black shapes
        (as that's the default emissive color, usually used for unlit)
        and all VRML 2.0 scenes would have white shapes (as that's
        the unlit color).

        VRML/X3D spec doesn't say anything about this,
        as they are written like Attributes.Lighting = always @true.
        So we can just do whatever seems natural for user that
        turned off lighting. }
      glColorv(Vector4Single(DiffuseColor, MaterialOpacity)) else
      glColorv(Vector4Single(UnLitColor, MaterialOpacity));
  end;

  { Sets current OpenGL material and color properties for VRML 1.0 material
    (index 0). }
  procedure Material_1(M: TMaterialNode_1);
  var
    UnlitColor: TVector3Single;
  begin
    { Usually, UnlitColor should be from EmissiveColor3Single.

      However, IndexedLineSet in VRML 1.0 is an exception.
      MaterialAndShapeLit result for it is determined by whether it has normals
      (this is contrary to other nodes, that are always lit or unlit;
      well, forgetting about the "OnlyEmissiveMaterial" exceptional situation
      for the moment).

      If it's determined to be unlit by TIndexedLineSetNode_1.Lit
      (that is, it has not enough normals), but OnlyEmissiveMaterial is @false
      (we have usable material diffuse), we want the unlit color to be the same
      as when it was lit but we were watching with GL_LIGHTING off.
      RenderMaterial uses DiffuseColor in such case.

      Admittedly, I'm doing it all by just guessing what user may want...
      VRML 1.0 spec doesn't specify here what should really be done, i.e. what
      unlit color should be used. }
    if (Shape.Geometry is TIndexedLineSetNode_1) and (not M.OnlyEmissiveMaterial) then
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
  procedure Material_2(Material: TMaterialNode);
  begin
    if Material <> nil then
    begin
      RenderMaterial(
        VectorScale(Material.FdDiffuseColor.Value,
          Material.FdAmbientIntensity.Value),
        Material.FdDiffuseColor.Value,
        Material.FdSpecularColor.Value,
        Material.FdEmissiveColor.Value,
        White3Single,
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
  M1: TMaterialNode_1;
  M2: TMaterialNode;
  MaterialLit: boolean;
begin
  { calculate material parameters: M1, M2, MaterialLit }
  M1 := nil;
  M2 := nil;
  if Shape.Geometry is TAbstractGeometryNode_1 then
  begin
    { VRML 1.0 }
    M1 := Shape.State.LastNodes.Material;
    { VRML 1.0 specification states that this is a special case
      and we should treat EmissiveColor as precalculated color,
      and turn lighting off to improve speed. }
    MaterialLit := not M1.OnlyEmissiveMaterial;
  end else
  begin
    { VRML >= 2.0 }
    if Shape.Node <> nil then
      M2 := Shape.Node.Material else
      OnWarning(wtMajor, 'VRML/X3D', 'VRML node "' + Shape.Geometry.NodeTypeName +
        '" can be specified only in a "geometry" field of "Shape" node');
    { VRML 2.0 spec says that when Material is not assigned, shape is unlit. }
    MaterialLit := M2 <> nil;
  end;

  { calculate MaterialAndShapeLit (like Lighting, but ignores Attributes.Lighting) }
  MaterialAndShapeLit := MaterialLit and Shape.Geometry.Lit(Shape.State);

  { calculate and apply Lighting }
  Lighting := Attributes.Lighting and MaterialAndShapeLit;
  if not Attributes.PureGeometry then
    FixedFunctionLighting := Lighting;
  if Lighting then
    Shader.EnableLighting;

  { set material. At the end, since this uses MaterialAndShapeLit, M1, M2. }
  if M1 <> nil then
    Material_1(M1) else
    Material_2(M2);
end;
