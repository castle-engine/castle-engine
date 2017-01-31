{
  Copyright 2003-2017 Michalis Kamburelis.

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
    const AmbientColor, DiffuseColor, SpecularColor, EmissiveColor: TCastleColorRGB;
    const UnLitColor: TCastleColorRGB;
    const ShininessExp, ShapeOpacity: Single);
  begin
    if Attributes.Mode <> rmFull then Exit;

    MaterialOpacity := ShapeOpacity * Attributes.Opacity;

    Shader.MaterialAmbient := Vector4Single(AmbientColor, MaterialOpacity);
    Shader.MaterialDiffuse := Vector4Single(DiffuseColor, MaterialOpacity);
    Shader.MaterialSpecular := Vector4Single(SpecularColor, MaterialOpacity);
    Shader.MaterialEmission := Vector4Single(EmissiveColor, MaterialOpacity);
    Shader.MaterialShininessExp := ShininessExp;

    if MaterialAndShapeLit then
      { When the lighting is disabled because of Attributes.Lighting
        (not because of this shape's material), it's best to use diffuse color.
        Otherwise most of normal VRML 1.0 scenes would have black shapes
        (as that's the default emissive color, usually used for unlit)
        and all VRML 2.0 scenes would have white shapes (as that's
        the unlit color) or shapes with emissiveColor (as we optimize
        case when only emissiveColor is non-zero, using then unlit color := emissiveColor).

        VRML/X3D spec doesn't say anything about this,
        as they are written like Attributes.Lighting = always @true.
        So we can just do whatever seems natural for user that
        turned off lighting. }
      Shader.MaterialUnlit := Vector4Single(DiffuseColor, MaterialOpacity) else
      Shader.MaterialUnlit := Vector4Single(UnLitColor, MaterialOpacity);

    {$ifndef OpenGLES}
    if Lighting then
    begin
      glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Shader.MaterialAmbient);
      glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Shader.MaterialDiffuse);
      glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Shader.MaterialSpecular);
      glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Shader.MaterialEmission);
      glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, Shader.MaterialShininessExp);

      { If Lighting, we know that GL_LIGHTING is enabled
        (for fixed-function, or shader). So no point in setting glColor. }
    end else
      glColorv(Shader.MaterialUnlit);
    {$endif}
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
      well, forgetting about the "PureEmissive" exceptional situation
      for the moment).

      If it's determined to be unlit by TIndexedLineSetNode_1.Lit
      (that is, it has not enough normals), but PureEmissive is @false
      (we have usable material diffuse), we want the unlit color to be the same
      as when it was lit but we were watching with GL_LIGHTING off.
      RenderMaterial uses DiffuseColor in such case.

      Admittedly, I'm doing it all by just guessing what user may want...
      VRML 1.0 spec doesn't specify here what should really be done, i.e. what
      unlit color should be used. }
    if (Shape.Geometry is TIndexedLineSetNode_1) and (not M.PureEmissive) then
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
    if (Material <> nil) and not Material.PureEmissive then
    begin
      RenderMaterial(
        VectorScale(Material.FdDiffuseColor.Value,
          Material.FdAmbientIntensity.Value),
        Material.FdDiffuseColor.Value,
        Material.FdSpecularColor.Value,
        Material.FdEmissiveColor.Value,
        WhiteRGB,
        Material.ShininessExp,
        Material.Opacity);
    end else
    if Material <> nil then
    begin
      { in this case we know that Material.PureEmissive = true,
        so this can be rendered like unlit material with color = emissiveColor. }
      RenderMaterial(
        WhiteRGB, WhiteRGB, WhiteRGB, WhiteRGB,
        Material.FdEmissiveColor.Value,
        Material.ShininessExp,
        Material.Opacity);
    end else
    begin
      RenderMaterial(
        { Colors for lighting don't matter here }
        WhiteRGB, WhiteRGB, WhiteRGB, WhiteRGB,
        { From VRML 2.0 spec about Appearance node: when no Material is
          specified, object is unlit and unlit object color is (1, 1, 1). }
        WhiteRGB,
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
    MaterialLit := not M1.PureEmissive;
  end else
  begin
    { VRML >= 2.0 }
    if Shape.Node <> nil then
      M2 := Shape.Node.Material else
      WritelnWarning('VRML/X3D', 'VRML node "' + Shape.Geometry.X3DType +
        '" can be specified only in a "geometry" field of "Shape" node');
    { VRML 2.0 spec says that when Material is not assigned, shape is unlit. }
    MaterialLit := (M2 <> nil) and not M2.PureEmissive;
  end;

  { calculate MaterialAndShapeLit (like Lighting, but ignores things
    outside this shape and material) }
  MaterialAndShapeLit := MaterialLit and Shape.Geometry.Lit(Shape.State);

  { calculate and apply Lighting }
  Lighting := Attributes.Lighting and MaterialAndShapeLit and
    (Attributes.Mode <> rmDepth);
  if Attributes.Mode <> rmPureGeometry then
    FixedFunctionLighting := Lighting;
  if Lighting then
    Shader.EnableLighting;

  { set material. At the end, since this uses MaterialAndShapeLit, M1, M2. }
  if M1 <> nil then
    Material_1(M1) else
    Material_2(M2);
end;
