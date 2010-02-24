{
  Copyright 2006-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML headlight rendered by OpenGL --- TVRMLGLHeadLight class. }
unit VRMLGLHeadLight;

interface

uses VectorMath, VRMLNodes, VRMLHeadlight, Cameras;

type
  { VRML headlight rendered by OpenGL. }
  TVRMLGLHeadLight = class(TVRMLHeadLight)
  public
    { This sets properties of GL_LIGHT_GLLightNumber to render given light.

      This requires that current OpenGL matrix is modelview.
      This always preserves current matrix value (doing push/pops if necessary).

      If HeadlightFromCurrentView = @true, then we assume
      that headlight should be done from current view. This is the usual
      meaning of "headlight". In this case, current OpenGL matrix
      value doesn't matter --- so e.g. it doesn't matter if
      you call this before or after loading camera matrix.
      (Internally, we'll just set matrix to identity and use lighting
      position like (0, 0, 0), since it's Ok to do this then.
      Of course, we'll push/pop matrix to preserve your current value.)
      Values of HeadlightPosition, HeadlightDirection also don't matter in these cases.

      If HeadlightFromCurrentView = @false, then we assume that
      the headlight shines from given HeadlightPosition and HeadlightDirection.
      The modelview matrix should contain in this case the current view
      (camera) matrix. This allows you to simulate "headlight" shining
      from other place than current OpenGL view.

      If CallEnabled then it will also call glEnable(GL_LIGHT_GLLightNumber).

      Overloaded version with camera simply uses Camera.GetCameraVectors
      to get HeadlightPosition, HeadlightDirection.
      When HeadlightFromCurrentView = @true, Camera doesn't matter
      (may be @nil).

      @groupBegin }
    procedure Render(GLLightNumber: Cardinal; CallEnabled: boolean;
      const HeadlightFromCurrentView: boolean;
      const HeadlightPosition, HeadlightDirection: TVector3Single);

    procedure Render(GLLightNumber: Cardinal; CallEnabled: boolean;
      const HeadlightFromCurrentView: boolean;
      Camera: TCamera);
    { @groupEnd }

    { This is like Light.Render(GLLightNumber, true, ...), but will call
      glDisable(GL_LIGHT_GLLightNumber) if Light is nil.

      In effect, you can call this procedure with nil or non-nil
      parameter, and you can be sure that enabled/disabled state
      of light GL_LIGHT_GLLightNumber will be set.

      @groupBegin }
    class procedure RenderOrDisable(Light: TVRMLGLHeadlight;
      GLLightNumber: Cardinal;
      const HeadlightFromCurrentView: boolean;
      const HeadlightPosition, HeadlightDirection: TVector3Single);

    class procedure RenderOrDisable(Light: TVRMLGLHeadlight;
      GLLightNumber: Cardinal;
      const HeadlightFromCurrentView: boolean;
      Camera: TCamera);
    { @groupEnd }
  end;

implementation

uses GL, GLU, KambiGLUtils, SysUtils, Math;

procedure TVRMLGLHeadLight.Render(GLLightNumber: Cardinal; CallEnabled: boolean;
  const HeadlightFromCurrentView: boolean;
  const HeadlightPosition, HeadlightDirection: TVector3Single);
var
  GLLight: TGLenum;
  GLAmbientColor: TVector4Single;
  GLAmbientColor3: TVector3Single absolute GLAmbientColor;
  GLColor: TVector4Single;
  GLColor3: TVector3Single absolute GLColor;
begin
  GLLight := GL_LIGHT0 + GLLightNumber;

  if HeadlightFromCurrentView then
  begin
    glPushMatrix;

      { GL_POSITION of the light is affected by current matrix
        (i.e. current at the time of glLightv(GLLight, GL_POSITION, ...) call).
        This is headlight, so this always wants to be relative to identity
        matrix. }
      glLoadIdentity;

      if Spot then
        glLightv(GLLight, GL_POSITION, Vector4Single(0, 0, 0, 1)) else
        { The light is directional }
        glLightv(GLLight, GL_POSITION, Vector4Single(0, 0, 1, 0));

    glPopMatrix;
  end else
  begin
    if Spot then
      glLightv(GLLight, GL_POSITION, Vector4Single(HeadlightPosition, 1)) else
      { The light is directional.
        Negate HeadlightDirection: for glLightv(.., GL_POSITION, ...)
        this is like "position in infinity from where light shines"
        (*not* direction *in* which the light shines, as our
        HeadlightDirection). }
      glLightv(GLLight, GL_POSITION, Vector4Single(VectorNegate(HeadlightDirection), 0));
  end;

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

procedure TVRMLGLHeadLight.Render(GLLightNumber: Cardinal; CallEnabled: boolean;
  const HeadlightFromCurrentView: boolean;
  Camera: TCamera);
var
  Pos, Dir, Up: TVector3Single;
begin
  Camera.GetCameraVectors(Pos, Dir, Up);
  Render(GLLightNumber, CallEnabled, HeadlightFromCurrentView, Pos, Dir);
end;

class procedure TVRMLGLHeadLight.RenderOrDisable(Light: TVRMLGLHeadlight;
  GLLightNumber: Cardinal;
  const HeadlightFromCurrentView: boolean;
  const HeadlightPosition, HeadlightDirection: TVector3Single);
begin
  if Light <> nil then
    Light.Render(GLLightNumber, true,
      HeadlightFromCurrentView, HeadlightPosition, HeadlightDirection) else
    glDisable(GL_LIGHT0 + GLLightNumber);
end;

class procedure TVRMLGLHeadLight.RenderOrDisable(Light: TVRMLGLHeadlight;
  GLLightNumber: Cardinal;
  const HeadlightFromCurrentView: boolean;
  Camera: TCamera);
var
  Pos, Dir, Up: TVector3Single;
begin
  Camera.GetCameraVectors(Pos, Dir, Up);
  RenderOrDisable(Light, GLLightNumber, HeadlightFromCurrentView, Pos, Dir);
end;

end.