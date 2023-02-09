{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

var
  Lighting: Boolean;

procedure RenderMaterialsBegin;

  { Is this a Phong material that has only emissiveColor non-zero. }
  function PureEmissivePhong(const Material: TMaterialInfo): Boolean;
  begin
    Result :=
      (Material is TPhongMaterialInfo) and
      TPhongMaterialInfo(Material).PureEmissive and
      { Existence of Color/ColorRGBA nodes means that we cannot rely on simple
        "only emissive" lighting calculation, because Color/ColorRGBA
        nodes replace the diffuse factor, in case of Phong Material. }
      (Shape.Geometry.InternalColor = nil) and
      (Shape.Geometry.InternalColorRGBA = nil);
  end;

  { Apply material for shader and fixed-function pipelines.
    Assumes Lighting is already calculated.
    For fixed-function pipeline, this sets full glMaterial and glColor. }
  procedure RenderMaterial(const M: TMaterialInfo);
  {$ifndef OpenGLES}
  var
    PM: TPhongMaterialInfo;
    // UM: TUnlitMaterialInfo;
  {$endif}
  begin
    Shader.Material := M;

    if RenderOptions.Mode <> rmFull then Exit;

    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      if Lighting then
      begin
        Assert(M <> nil); // Lighting is always false when M = nil

        if M is TPhongMaterialInfo then
        begin
          PM := TPhongMaterialInfo(M);
          glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4(PM.AmbientColor, M.Opacity));
          glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4(PM.DiffuseColor, M.Opacity));
          glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4(PM.SpecularColor, M.Opacity));
          glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Vector4(PM.EmissiveColor, M.Opacity));
          glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, PM.ShininessExp);
        end else
        if M is TUnlitMaterialInfo then
        begin
          Assert(false, 'UnlitMaterial should not have Lighting=true');
          // UM := TUnlitMaterialInfo(M);
          // glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4(TVector3.Zero, M.Opacity));
          // glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4(TVector3.Zero, M.Opacity));
          // glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4(TVector3.Zero, M.Opacity));
          // glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Vector4(UM.EmissiveColor, M.Opacity));
          // glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 0.0);
        end else
        begin
          // approximate PhysicalMaterial in fixed-function using Phong with diffuseColor = baseColor
          glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4(TVector3.Zero, M.Opacity));
          glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4(M.MainColor, M.Opacity));
          glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4(TVector3.Zero, M.Opacity));
          glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION, Vector4(TVector3.Zero, M.Opacity));
          glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 0.0);
        end;

        { If Lighting, we know that GL_LIGHTING is enabled
          (for fixed-function, or shader). So no point in setting glColor. }
      end else
      if M <> nil then
      begin
        { UnlitMaterial is implemented here, as it causes Lighting = false always.
          We will also use this for fixed-function pipeline when
          - user set RenderOptions.Lighting = false and we have Phong Material or PhysicalMaterial.
          - or we have Phong Material that is PureEmissive. }
        if PureEmissivePhong(M) then
          glColorv(Vector4(TPhongMaterialInfo(M).EmissiveColor, M.Opacity))
        else
          glColorv(M.MainColor);
      end else
      begin
        { In case of material = nil, behave like UnlitMaterial with emissiveColor = white. }
        glColorv(WhiteRGB);
      end;
      {$endif}
    end;
  end;

var
  Material: TMaterialInfo;
  MaterialLit, MaterialAndShapeLit: Boolean;
begin
  // calculate Material
  if Shape.Geometry is TAbstractGeometryNode_1 then
  begin
    { VRML 1.0 }
    Material := Shape.State.VRML1State.Material.MaterialInfo(0);
  end else
  begin
    { VRML >= 2.0, X3D }
    Material := Shape.State.MaterialInfo;
  end;

  // calculate MaterialLit ("whether Material requires lighting calculation")
  MaterialLit :=
    { Material = nil indicates "white unlit material". }
    (Material <> nil) and
    { Optimize materials in case of PureEmissive. }
    (not PureEmissivePhong(Material)) and
    { UnlitMaterial explicitly says that lighting calculation is not done. }
    (not (Material is TUnlitMaterialInfo));

  // calculate MaterialAndShapeLit ("whether Material or geometry requires lighting calculation")
  MaterialAndShapeLit := MaterialLit and Shape.Geometry.Lit(Shape.State);

  // calculate Lighting ("whether anything requires lighting calculation")
  Lighting := RenderOptions.Lighting and MaterialAndShapeLit and
    (RenderOptions.Mode = rmFull);
  RenderContext.FixedFunctionLighting := Lighting;
  if Lighting then
    Shader.EnableLighting;

  RenderMaterial(Material);
end;
