{
  Copyright 2003-2018 Michalis Kamburelis.

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

  { Apply material (works for all types of materials - VRML 1.0,
    X3D and VRML 2.0, or CommonSurfaceShader).
    Assumes MaterialAndShapeLit is already calculated.
    This sets full glMaterial and glColor. }
  procedure RenderMaterial(
    const AmbientColor, DiffuseColor, SpecularColor, EmissiveColor: TCastleColorRGB;
    const UnLitColor: TCastleColorRGB;
    const ShininessExp, ShapeOpacity: Single);
  begin
    if Attributes.Mode <> rmFull then Exit;

    MaterialOpacity := ShapeOpacity * Attributes.Opacity;

    Shader.MaterialAmbient := AmbientColor;
    Shader.MaterialDiffuseAlpha := Vector4(DiffuseColor, MaterialOpacity);
    Shader.MaterialSpecular := SpecularColor;
    Shader.MaterialEmission := EmissiveColor;
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
      Shader.MaterialUnlit := Vector4(DiffuseColor, MaterialOpacity) else
      Shader.MaterialUnlit := Vector4(UnLitColor, MaterialOpacity);

    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      if Lighting then
      begin

        glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4(AmbientColor, MaterialOpacity));
        glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Shader.MaterialDiffuseAlpha);
        glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4(SpecularColor, MaterialOpacity));
        glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Vector4(EmissiveColor, MaterialOpacity));
        glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, Shader.MaterialShininessExp);

        { If Lighting, we know that GL_LIGHTING is enabled
          (for fixed-function, or shader). So no point in setting glColor. }
      end else
        glColorv(Shader.MaterialUnlit);
      {$endif}
    end;
  end;

  { Sets current OpenGL material and color properties for VRML 1.0 material
    (index 0). }
  procedure RenderMaterialVrml1(const M: TMaterialNode_1);
  var
    UnlitColor: TVector3;
    I: TMaterialInfo;
  begin
    I := M.MaterialInfo(0);

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
    if (Shape.Geometry is TIndexedLineSetNode_1) and (not I.PureEmissive) then
      UnlitColor := I.DiffuseColor else
      UnlitColor := I.EmissiveColor;

    RenderMaterial(
      I.AmbientColor,
      I.DiffuseColor,
      I.SpecularColor,
      I.EmissiveColor,
      UnlitColor,
      I.ShininessExp,
      I.Opacity);
  end;

  { Sets current OpenGL material and color properties for X3D (or VRML >= 2.0) material.
    Works with any TMaterialInfo (and follows always X3D / VRML 2.0 spec).
    Material may be nil. }
  procedure RenderMaterialX3d(const Material: TMaterialInfo);
  begin
    if (Material <> nil) and not Material.PureEmissive then
    begin
      RenderMaterial(
        Material.AmbientColor,
        Material.DiffuseColor,
        Material.SpecularColor,
        Material.EmissiveColor,
        WhiteRGB,
        Material.ShininessExp,
        Material.Opacity);
    end else
    if Material <> nil then
    begin
      Assert(Material.PureEmissive);
      //Assert(not MaterialLit);
      Assert(not MaterialAndShapeLit);
      Assert(not Lighting);
      { In this case we know that Material.PureEmissive = true,
        so this can be rendered like unlit material with color = emissiveColor. }
      RenderMaterial(
        WhiteRGB, WhiteRGB, WhiteRGB, WhiteRGB,
        Material.EmissiveColor,
        Material.ShininessExp,
        Material.Opacity);
    end else
    begin
      RenderMaterial(
        { Colors for lighting don't matter here }
        WhiteRGB, WhiteRGB, WhiteRGB, WhiteRGB,
        { From VRML 2.0 spec about Appearance node: when no Material is
          specified, object is unlit and unlit object color is (1, 1, 1).
          X3D spec also hints (at Material node) that
          Material = NULL makes "unlit white color".  }
        WhiteRGB,
        { shininess } 0, { opacity } 1);
    end;
  end;

var
  MaterialVrml1: TMaterialNode_1;
  MaterialX3d: TMaterialInfo;
  MaterialLit: boolean;
begin
  { calculate material parameters: MaterialVrml1, MaterialX3d, MaterialLit }
  MaterialVrml1 := nil;
  MaterialX3d := nil;
  if Shape.Geometry is TAbstractGeometryNode_1 then
  begin
    { VRML 1.0 }
    MaterialVrml1 := Shape.State.VRML1State.Material;
    { VRML 1.0 specification states that this is a special case
      and we should treat EmissiveColor as precalculated color,
      and turn lighting off to improve speed. }
    MaterialLit := not MaterialVrml1.PureEmissive;
  end else
  begin
    { VRML >= 2.0 }
    if Shape.Node <> nil then
    begin
      if Shape.Node.CommonSurfaceShader <> nil then
        MaterialX3d := Shape.Node.CommonSurfaceShader.MaterialInfo
      else
      if Shape.Node.Material is TPhysicalMaterialNode then
        MaterialX3d := TPhysicalMaterialNode(Shape.Node.Material).MaterialInfo
      else
      if Shape.Node.Material is TMaterialNode then
        MaterialX3d := TMaterialNode(Shape.Node.Material).MaterialInfo
      else
      if Shape.Node.Material is TTwoSidedMaterialNode then
        MaterialX3d := TTwoSidedMaterialNode(Shape.Node.Material).MaterialInfo
      else
        MaterialX3d := nil;
    end else
      WritelnWarning('VRML/X3D', 'VRML node "' + Shape.NiceName +
        '" can be specified only in the "geometry" field of the "Shape" node');

    { VRML 2.0 spec says that when Material is not assigned, shape is unlit.

      Note that our MaterialX3d may also be set by CommonSurfaceShader,
      and that is OK:
      If the CommonSurfaceShader is present, we also treat it as lit
      (this way CommonSurfaceShader works regardless if the underlying
      implementation uses shaders).  }
    MaterialLit := (MaterialX3d <> nil) and (not MaterialX3d.PureEmissive);
  end;

  { calculate MaterialAndShapeLit (like Lighting, but ignores things
    outside this shape and material) }
  MaterialAndShapeLit := MaterialLit and Shape.Geometry.Lit(Shape.State);

  { calculate and apply Lighting }
  Lighting := Attributes.Lighting and MaterialAndShapeLit and
    (Attributes.Mode = rmFull);
  FixedFunctionLighting := Lighting;
  if Lighting then
    Shader.EnableLighting;

  { set material. At the end, since this uses MaterialAndShapeLit, MaterialVrml1, MaterialX3d. }
  if MaterialVrml1 <> nil then
    RenderMaterialVrml1(MaterialVrml1)
  else
    RenderMaterialX3d(MaterialX3d);

  { override Shader.MaterialUnlit in case rmSolidColor }
  if Attributes.Mode = rmSolidColor then
    Shader.MaterialUnlit := Vector4(Attributes.SolidColor, 1);
end;
