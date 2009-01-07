{
  Copyright 2006,2007 Michalis Kamburelis.

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

{ VRML headlight rendered by OpenGL --- TVRMLGLHeadLight class. }
unit VRMLGLHeadLight;

interface

uses VectorMath, VRMLNodes, VRMLHeadlight;

type
  { VRML headlight rendered by OpenGL. }
  TVRMLGLHeadLight = class(TVRMLHeadLight)
  public
    { This sets properties of GL_LIGHT_GLLightNumber to render given light.

      Note that this requires that current matrix is modelview.
      Matrix @italic(may) be reset to identity by this procedure.

      If CallEnabled then it will also call glEnable(GL_LIGHT_GLLightNumber). }
    procedure Render(GLLightNumber: Cardinal; CallEnabled: boolean);

    { This is like Light.Render(GLLightNumber, true), but will call
      glDisable(GL_LIGHT_GLLightNumber) if Light is nil.

      In effect, you can call this procedure with nil or non-nil
      parameter, and you can be sure that enabled/disabled state
      of light GL_LIGHT_GLLightNumber will be set. }
    class procedure RenderOrDisable(Light: TVRMLGLHeadlight;
      GLLightNumber: Cardinal);
  end;

implementation

uses GL, GLU, GLExt, KambiGLUtils, SysUtils, Math;

procedure TVRMLGLHeadLight.Render(GLLightNumber: Cardinal; CallEnabled: boolean);
var
  GLLight: TGLenum;
  GLAmbientColor: TVector4Single;
  GLAmbientColor3: TVector3Single absolute GLAmbientColor;
  GLColor: TVector4Single;
  GLColor3: TVector3Single absolute GLColor;
begin
  GLLight := GL_LIGHT0 + GLLightNumber;

  { GL_POSITION of the light is affected by current matrix
    (i.e. current at the time of glLightv(GLLight, GL_POSITION, ...) call).
    This is headlight, so this always wants to be relative to identity
    matrix. }
  glLoadIdentity;

  if Spot then
    glLightv(GLLight, GL_POSITION, Vector4Single(0, 0, 0, 1)) else
    { The light is directional }
    glLightv(GLLight, GL_POSITION, Vector4Single(0, 0, 1, 0));

  GLAmbientColor3 := VectorScale(Color, AmbientIntensity);
  GLAmbientColor[3] := 1.0;
  glLightv(GLLight, GL_AMBIENT, GLAmbientColor);

  GLColor3 := VectorScale(Color, Intensity);
  GLColor[3] := 1.0;
  glLightv(GLLight, GL_DIFFUSE, GLColor);
  glLightv(GLLight, GL_SPECULAR, GLColor);

  glLightf(GLLight, GL_CONSTANT_ATTENUATION, Attenuation[0]);
  glLightf(GLLight, GL_LINEAR_ATTENUATION, Attenuation[1]);
  glLightf(GLLight, GL_QUADRATIC_ATTENUATION, Attenuation[2]);

  if Spot then
  begin
    glLightf(GLLight, GL_SPOT_CUTOFF, RadToDeg(SpotCutOffAngle));
    glLightf(GLLight, GL_SPOT_EXPONENT, SpotDropOffRate * 180.0);
  end else
    glLighti(GLLight, GL_SPOT_CUTOFF, 180);

  if CallEnabled then
    glEnable(GLLight);
end;

class procedure TVRMLGLHeadLight.RenderOrDisable(Light: TVRMLGLHeadlight;
  GLLightNumber: Cardinal);
begin
  if Light <> nil then
    Light.Render(GLLightNumber, true) else
    glDisable(GL_LIGHT0 + GLLightNumber);
end;

end.