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

procedure TVRMLOpenGLRenderer.Render_MaterialsBegin;
begin
 Render_Material_ForcedLightDisable := false;
 Render_Material_BoundMatNum := -1; { no material is bound now }
 Render_Material_LastFogImmune := false;
end;

procedure TVRMLOpenGLRenderer.Render_MaterialsEnd;
begin
 if Render_Material_ForcedLightDisable then
 begin
  glPopAttrib;
  Render_Material_ForcedLightDisable := false;
 end;
end;

procedure TVRMLOpenGLRenderer.Render_BindMaterial(MatNum: integer);
{ Ustawia aktualny material na material numer MatNum ze State.LastNodes.Material }
{var Stipple: TPolygonStipple;}
begin
 if MatNum = Render_Material_BoundMatNum then exit;
 Render_Material_BoundMatNum := MatNum;

 with Render_State.LastNodes.Material do
 begin
  if OnlyEmissiveMaterial then
  begin
   if not Render_Material_ForcedLightDisable then
   begin
    glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    Render_Material_ForcedLightDisable := true;
   end;

   {VRML 1.0 specification states that this is a special case
    and we should treat EmissiveColor as precalculated color.
    So we can turn lighting off to improve speed (in fact, we are supposed
    to do that). }
   glColorv(Vector4f(Attributes.ColorModulated(EmissiveColor3Single(MatNum)),
     Opacity(MatNum)));
  end else
  begin
   if Render_Material_ForcedLightDisable then
   begin
    glPopAttrib;
    Render_Material_ForcedLightDisable := false;
   end;

   { TODO: VRML97 - Ambient materialu bedziesz musial brac
     z DiffuseColor * AmbientIntensity }
   glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT,
     Vector4f(Attributes.ColorModulated(AmbientColor3Single(MatNum)), Opacity(MatNum)));
   glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE,
     Vector4f(Attributes.ColorModulated(DiffuseColor3Single(MatNum)), Opacity(MatNum)));
   glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR,
     Vector4f(Attributes.ColorModulated(SpecularColor3Single(MatNum)), Opacity(MatNum)));
   glMaterialv(GL_FRONT_AND_BACK, GL_EMISSION,
     Vector4f(Attributes.ColorModulated(EmissiveColor3Single(MatNum)), Opacity(MatNum)));
   glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, ShininessExp(MatNum));

   { Use also glColor for unlit things. This way this code will be able
     to work regardless of glEnable(GL_LIGHTING) state (even if we
     will put this on GL display list and then will call such list
     with various states of glEnable(GL_LIGHTING)). }
   glColorv(Vector4f(Attributes.ColorModulated(DiffuseColor3Single(MatNum)),
     Opacity(MatNum)));
  end;

  if FogEnabled then
  begin
    { Render_Material_LastFogImmune is used to minimize
      number of glDisable/Enable(GL_FOG) (as they can be potentially
      expensive on some OpenGL implementations, if they init some
      large state etc.) }
    if Render_Material_LastFogImmune <> FdFogImmune.Value then
    begin
      if FdFogImmune.Value then
        glDisable(GL_FOG) else
        glEnable(GL_FOG);
      Render_Material_LastFogImmune := FdFogImmune.Value;
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
end;

