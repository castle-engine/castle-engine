{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels.gl Pascal units".

  "Kambi's 3dmodels.gl Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels.gl Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels.gl Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Judge whether the node can be lit, looking only at Node (not at State,
  Material etc.). }
function NodeLit(Node: TNodeGeneralShape): boolean;
begin
  Result := not (
    (Node is TNodePointSet_1) or
    (Node is TNodePointSet_2));
end;

procedure TVRMLOpenGLRenderer.Render_MaterialsBegin;
begin
  Render_Material_ForcedLightDisable := false;
  Render_Material_BoundMatNum := -1; { no material is bound now }
  Render_Material_LastFogImmune := false;

  if not (Render_Node is TNodeGeneralShape_1) then
    Render_BindMaterial_2;
end;

procedure TVRMLOpenGLRenderer.Render_MaterialsEnd;
begin
  if Render_Material_ForcedLightDisable then
  begin
    glPopAttrib;
    Render_Material_ForcedLightDisable := false;
  end;
end;

procedure TVRMLOpenGLRenderer.Render_Material(
  const Lit: boolean;
  const AmbientColor, DiffuseColor, SpecularColor,
    EmissiveColor: TVector3Single;
  const UnLitColor: TVector3Single;
  const ShininessExp, Opacity: Single;
  const FogImmune: boolean);
begin
  if Lit then
  begin
    if Render_Material_ForcedLightDisable then
    begin
      glPopAttrib;
      Render_Material_ForcedLightDisable := false;
    end;

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
    if not Render_Material_ForcedLightDisable then
    begin
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      Render_Material_ForcedLightDisable := true;
    end;

    glColorv(Vector4f(Attributes.ColorModulated(UnLitColor), Opacity));
  end;

  if FogEnabled then
  begin
    { Render_Material_LastFogImmune is used to minimize
      number of glDisable/Enable(GL_FOG) (as they can be potentially
      expensive on some OpenGL implementations, if they init some
      large state etc.) }
    if Render_Material_LastFogImmune <> FogImmune then
    begin
      if FogImmune then
        glDisable(GL_FOG) else
        glEnable(GL_FOG);
      Render_Material_LastFogImmune := FogImmune;
    end;
  end;

  { This is some old attempt at implementing transparency using
    polygon stipple. This is called "low quality transparency" in ivview.
    But this is SOOO ugly that I do not consider it really worth
    maintaining. Really, using blending we can achieve MUCH better results.
    So it's commented out below. }
  {if Opacity(MatNum) < 1.0 then
  begin
    glEnable(GL_POLYGON_STIPPLE);
    Stipple := RandomPolyStippleBy16(1-Opacity(MatNum));
    glPolygonStipple(@Stipple);
  end else
    glDisable(GL_POLYGON_STIPPLE)}
end;

{ Sets current OpenGL material and color properties for material
  number MatNum from Render_State.LastNodes.Material.
  This is ignored when Render_Node is not TNodeGeneralShape_1
  (then VRML 2.0 approach to materials should be used). }
procedure TVRMLOpenGLRenderer.Render_BindMaterial_1(MatNum: Integer);
begin
  if (not (Render_Node is TNodeGeneralShape_1)) or
     (MatNum = Render_Material_BoundMatNum) then
    Exit;

  Render_Material_BoundMatNum := MatNum;

  with Render_State.LastNodes.Material do
  begin
    Render_Material(
      { VRML 1.0 specification states that this is a special case
        and we should treat EmissiveColor as precalculated color.
        So we can turn lighting off to improve speed (in fact, we are supposed
        to do that). }
      (not OnlyEmissiveMaterial) and NodeLit(Render_Node),
      AmbientColor3Single(MatNum),
      DiffuseColor3Single(MatNum),
      SpecularColor3Single(MatNum),
      EmissiveColor3Single(MatNum),
      EmissiveColor3Single(MatNum),
      ShininessExp(MatNum),
      Opacity(MatNum),
      FdFogImmune.Value);
  end;
end;

{ Binds current VRML 2.0 Material.
 }
procedure TVRMLOpenGLRenderer.Render_BindMaterial_2;
var
  Material: TNodeMaterial_2;
begin
  Assert(not (Render_Node is TNodeGeneralShape_1));

  if Render_State.ParentShape = nil then
  begin
    VRMLNonFatalError('VRML node "' + Render_Node.NodeTypeName +
      '" can be specified only in a "geometry" field of "Shape" node');
    Exit;
  end;

  Material := Render_State.ParentShape.Material;

  if Material <> nil then
  begin
    Render_Material(
      NodeLit(Render_Node),
      VectorScale(Material.FdDiffuseColor.Value,
        Material.FdAmbientIntensity.Value),
      Material.FdDiffuseColor.Value,
      Material.FdSpecularColor.Value,
      Material.FdEmissiveColor.Value,
      { UnLit color doesn't matter here, it will not be used
        by Render_Material. }
      White3Single,
      Material.ShininessExp,
      Material.Opacity,
      Material.FdFogImmune.Value);
  end else
  begin
    Render_Material(
      false,
      { Colors for lighting don't matter here }
      White3Single, White3Single, White3Single, White3Single,
      { From VRML 2.0 spec about Appearance node: when no Material is
        specified, object is unlit and unlit object color is (1, 1, 1). }
      White3Single,
      0,
      1,
      false);
  end;
end;
