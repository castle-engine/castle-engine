{
  Copyright 2003-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Judge whether the node can be lit, looking only at Node (not at State,
  Material etc.). }
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
begin
  Render_Material_ForcedLightDisable := false;
  Render_Material_BoundMatNum := -1; { no material is bound now }
  Render_Material_LastFogImmune := false;

  Assert(not MaterialFromColorEnabled);

  if not (CurrentGeometry is TVRMLGeometryNode_1) then
    Render_BindMaterial_2;
end;

procedure TVRMLOpenGLRenderer.Render_MaterialsEnd;
begin
  if Render_Material_ForcedLightDisable then
  begin
    glPopAttrib;
    Render_Material_ForcedLightDisable := false;
  end;

  { Other RenderShape calls start with
    Render_Material_LastFogImmune = false, so they expect that the fog
    is already enabled (if FogEnabled). Therefore, we have to reenable
    here the fog if the last material had fogImmune. }
  if FogEnabled then
  begin
    if Render_Material_LastFogImmune then
    begin
      glEnable(GL_FOG);
      Render_Material_LastFogImmune := false;
    end;
  end;

  if MaterialFromColorEnabled then
  begin
    glDisable(GL_COLOR_MATERIAL);
    MaterialFromColorEnabled := false;
  end;
end;

procedure TVRMLOpenGLRenderer.Render_Material(
  const Lit: boolean;
  const AmbientColor, DiffuseColor, SpecularColor,
    EmissiveColor: TVector3Single;
  const UnLitColor: TVector3Single;
  const ShininessExp, Opacity: Single;
  const FogImmune: boolean);
{$ifdef TRANSPARENCY_BY_STIPPLE}
var
  Stipple: TPolygonStipple;
{$endif}
begin
  if Attributes.PureGeometry or (not Attributes.ControlMaterials) then Exit;

  Material_BoundOpacity := Opacity;

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
      { PushAttrib is used to save GL_LIGHTING enabled state,
        as we cannot predict it (it's up to the VRMLOpenGLRenderer caller
        whether the light is turned on). Obviously we don't want to explicitly
        query GL_LIGHTING enabled state, push/pop is better.

        Note that we use GL_LIGHTING_BIT instead of GL_ENABLE_BIT:
        GL_ENABLE_BIT would also push/pop GL_FOG enabled state,
        and this could cause problems when mixed with fogImmune. }
      glPushAttrib(GL_LIGHTING_BIT);

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

  {$ifdef TRANSPARENCY_BY_STIPPLE}
  { This is an attempt at implementing transparency using
    polygon stipple. This is called "low quality transparency" in ivview.
    But this is SOOO ugly that I do not consider it really worth
    maintaining. Really, using blending we can achieve MUCH better results. }
  if Opacity < 1.0 then
  begin
    glEnable(GL_POLYGON_STIPPLE);
    if Opacity >= 0.75 then
      Stipple := ThreeQuartersStipple else
      Stipple := HalftoneStipple;
    //Stipple := RandomPolyStippleBy16(1 - Opacity);
    glPolygonStipple(@Stipple);
  end else
    glDisable(GL_POLYGON_STIPPLE)
  {$endif}

  { MaterialFromColorEnabled should be enabled only below and disabled
    back by MaterialsEnd. For many Render_BindMaterial_1 calls, it's
    Ok if it's already enabled. }
  if (MeshRenderer <> nil) and
     MeshRenderer.NeedsSetColor and
     NodeLit(CurrentGeometry) and
     (not MaterialFromColorEnabled) then
  begin
    { For lit nodes, we want to set both unlit glColor and lit glMaterial
      in SetColor.

      Using GL_COLOR_MATERIAL is the best way to do this, this also works
      much faster and more memory-efficient (AFAIK, OpenGL is prepared
      to handle different glColor-per-vertex efficiently, but changing
      whole glMaterial-per-vertex is not efficient (as glColor is pushed
      with vertex data into the pipeline, while changing material settings
      requires flushing part of current pipeline)). }

    glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE);
    glEnable(GL_COLOR_MATERIAL);
    MaterialFromColorEnabled := true;
  end;
end;

{ Sets current OpenGL material and color properties for material
  number MatNum from CurrentState.LastNodes.Material.
  This is ignored when CurrentGeometry is not TVRMLGeometryNode_1
  (then VRML 2.0 approach to materials should be used). }
procedure TVRMLOpenGLRenderer.Render_BindMaterial_1(MatNum: Integer);
var
  UnlitColor: TVector3Single;
begin
  if (not (CurrentGeometry is TVRMLGeometryNode_1)) or
     (MatNum = Render_Material_BoundMatNum) then
    Exit;

  Render_Material_BoundMatNum := MatNum;

  with CurrentState.LastNodes.Material do
  begin
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
    if (CurrentGeometry is TNodeIndexedLineSet_1) and (not OnlyEmissiveMaterial) then
      UnlitColor := DiffuseColor3Single(MatNum) else
      UnlitColor := EmissiveColor3Single(MatNum);

    Render_Material(
      { VRML 1.0 specification states that this is a special case
        and we should treat EmissiveColor as precalculated color.
        So we can turn lighting off to improve speed (in fact, we are supposed
        to do that). }
      (not OnlyEmissiveMaterial) and NodeLit(CurrentGeometry),
      AmbientColor3Single(MatNum),
      DiffuseColor3Single(MatNum),
      SpecularColor3Single(MatNum),
      EmissiveColor3Single(MatNum),
      UnlitColor,
      ShininessExp(MatNum),
      Opacity(MatNum),
      FdFogImmune.Value);
  end;
end;

{ Binds current VRML 2.0 Material. }
procedure TVRMLOpenGLRenderer.Render_BindMaterial_2;
var
  Material: TNodeMaterial_2;
  IsNodeLit: boolean;
  UnLitColor: TVector3Single;
begin
  Assert(not (CurrentGeometry is TVRMLGeometryNode_1));

  if CurrentState.ParentShape = nil then
  begin
    VRMLNonFatalError('VRML node "' + CurrentGeometry.NodeTypeName +
      '" can be specified only in a "geometry" field of "Shape" node');
    Exit;
  end;

  Material := CurrentState.ParentShape.Material;

  IsNodeLit := NodeLit(CurrentGeometry);

  if Material <> nil then
  begin
    UnLitColor := White3Single;

    { Otherwise Circle2D would be always white.
      X3D spec doesn't say anything about this, so I use diffuse color. }
    if CurrentGeometry is TNodeCircle2D then
      UnLitColor := Material.FdDiffuseColor.Value;

    Render_Material(
      IsNodeLit,
      VectorScale(Material.FdDiffuseColor.Value,
        Material.FdAmbientIntensity.Value),
      Material.FdDiffuseColor.Value,
      Material.FdSpecularColor.Value,
      Material.FdEmissiveColor.Value,
      UnLitColor,
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
  This optimization is implementd in Render_Material (although it affects
  this procedure). There we check for NodeLit before turning on
  GL_COLOR_MATERIAL, since when not NodeLit, material has never any use. }
procedure TVRMLOpenGLRenderer.SetColor(const Color: TVector3Single);
begin
  { We use Material_BoundOpacity since we do not want to change
    material opacity here, only it's RGB part. }

  if Attributes.PureGeometry or
     (not Attributes.ControlMaterials) then
    Exit;

  glColorv(Vector4f(Attributes.ColorModulated(Color),
    Material_BoundOpacity));
end;
