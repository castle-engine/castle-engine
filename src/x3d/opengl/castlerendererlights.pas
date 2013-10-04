{
  Copyright 2003-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML/X3D lights OpenGL rendering. Internal for CastleRenderer. @exclude }
unit CastleRendererLights;

{$I castleconf.inc}

interface

uses CastleVectors, CastleGLUtils, X3DNodes, CastleRendererShader;

type
  { Modify light's properties of the light right before it's rendered.
    Currently, you can modify only the "on" state.

    By default, LightOn is the value of Light.LightNode.FdOn field.
    You can change it if you want. }
  TLightRenderEvent = procedure (const Light: TLightInstance;
    var LightOn: boolean) of object;

  { Render lights to OpenGL.
    Sets OpenGL lights properties, enabling and disabling them as needed.

    This is smart, avoiding to configure the same light many times.
    Remembers what VRML/X3D light is set on which OpenGL light,
    and this way avoids needless reconfiguring of OpenGL lights.
    This may speed up rendering, avoiding changing OpenGL state when not
    necessary.
    Assumes that nothing changes OpenGL light properties during our
    lifetime. }
  TVRMLGLLightsRenderer = class
  private
    FLightRenderEvent: TLightRenderEvent;
    LightsKnown: boolean;
    LightsDone: array of PLightInstance;
    function NeedRenderLight(Index: Integer; Light: PLightInstance): boolean;
  public
    { Statistics of how many OpenGL light setups were done
      (Statistics[true]) vs how many were avoided (Statistics[false]).
      The second number should usually be much higher, prooving
      that using a cache of LightsDone inside this class was useful. }
    Statistics: array [boolean] of Cardinal;

    constructor Create(const ALightRenderEvent: TLightRenderEvent);

    { Set OpenGL lights properties.
      Sets OpenGL fixed-function pipeline lights,
      enabling and disabling them as needed.
      Lights are also passed to TShader, calling appropriate
      TShader.EnableLight methods. So shader pipeline is also dealt with.

      Lights1 and Lights2 lists are simply glued inside.
      Lights2 may be @nil (equal to being empty). }
    procedure Render(const Lights1, Lights2: TLightInstancesList;
      const Shader: TShader);

    { Process light source properties right before rendering the light.

      This event, if assigned, must be deterministic,
      based purely on light properties. For example, it's Ok to
      make LightRenderEvent that turns off lights that have shadowVolumes = TRUE.
      It is @italic(not Ok) to make LightRenderEvent that sets LightOn to
      a random boolean value. That because caching here assumes that
      for the same Light values, LightRenderEvent will set LightOn the same. }
    property LightRenderEvent: TLightRenderEvent read FLightRenderEvent;
  end;

implementation

uses SysUtils, CastleUtils, Math, CastleRenderingCamera, CastleGL;

{ Set and enable OpenGL light properties based on VRML/X3D light.

  Requires that current OpenGL matrix is modelview.
  Always preserves the matrix value (by using up to one modelview
  matrix stack slot).

  We do not examine Light.LightNode.FdOn.Value here.

  We make no assumptions about the previous state of this OpenGL light.
  We simply always set all the parameters to fully define the required
  light behavior. Some light parameters may not be set, but only because
  they are not used --- for example, if a light is not a spot light,
  then we set GL_SPOT_CUTOFF to 180 (indicates that light has no spot),
  but don't necessarily set GL_SPOT_DIRECTION or GL_SPOT_EXPONENT
  (as OpenGL will not use them anyway). }
procedure glLightFromVRMLLight(glLightNum: Integer; const Light: TLightInstance);

{$ifndef OpenGLES}

  { SetupXxx light : setup glLight properties GL_POSITION, GL_SPOT_* }
  procedure SetupDirectionalLight(LightNode: TAbstractDirectionalLightNode);
  begin
    glLightv(glLightNum, GL_POSITION, Vector4Single(VectorNegate(LightNode.FdDirection.Value), 0));
    glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
  end;

  procedure SetupPointLight(LightNode: TAbstractPointLightNode);
  begin
    glLightv(glLightNum, GL_POSITION, Vector4Single(LightNode.FdLocation.Value, 1));
    glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
  end;

  procedure SetupSpotLight_1(LightNode: TSpotLightNode_1);
  begin
    glLightv(glLightNum, GL_POSITION, Vector4Single(LightNode.FdLocation.Value, 1));

    glLightv(glLightNum, GL_SPOT_DIRECTION, LightNode.FdDirection.Value);
    glLightf(glLightNum, GL_SPOT_EXPONENT, Clamped(LightNode.SpotExp, 0.0, 128.0));
    glLightf(glLightNum, GL_SPOT_CUTOFF,
      { Clamp to 90 for safety, see VRML 2.0 version for comments }
      Min(90, RadToDeg(LightNode.FdCutOffAngle.Value)));
  end;

  procedure SetupSpotLight(LightNode: TSpotLightNode);
  begin
    glLightv(glLightNum, GL_POSITION, Vector4Single(LightNode.FdLocation.Value, 1));

    glLightv(glLightNum, GL_SPOT_DIRECTION, LightNode.FdDirection.Value);

    { There is no way to exactly translate beamWidth to OpenGL GL_SPOT_EXPONENT.
      GL_SPOT_EXPONENT is an exponent for cosinus.
      beamWidth says to use constant intensity within beamWidth angle,
      and linear drop off to cutOffAngle.
      See [http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/chapter.opengl_rendering.html#section.vrml_lights]
      for more discussion. }

    if LightNode.FdBeamWidth.Value >= LightNode.FdCutOffAngle.Value then
      glLightf(glLightNum, GL_SPOT_EXPONENT, 0) else
      glLightf(glLightNum, GL_SPOT_EXPONENT, Clamped(
        0.5 / Max(LightNode.FdBeamWidth.Value, 0.0001), 0.0, 128.0));

    glLightf(glLightNum, GL_SPOT_CUTOFF,
      { Clamp to 90, to protect against user inputting invalid value in VRML,
        or just thing like 1.5708, which may be recalculated by
        RadToDeg to 90.0002104591, so > 90, and OpenGL raises "invalid value"
        error then... }
      Min(90, RadToDeg(LightNode.FdCutOffAngle.Value)));
  end;

var
  SetNoAttenuation: boolean;
  Attenuat: TVector3Single;
  Color3, AmbientColor3: TVector3f;
  Color4, AmbientColor4: TVector4f;
begin
  glLightNum += GL_LIGHT0;

  glPushMatrix;
  try
    if Light.WorldCoordinates then
      glLoadMatrix(RenderingCamera.Matrix);

    glMultMatrix(Light.Transform);

    if Light.Node is TAbstractDirectionalLightNode then
      SetupDirectionalLight(TAbstractDirectionalLightNode(Light.Node)) else
    if Light.Node is TAbstractPointLightNode then
      SetupPointLight(TAbstractPointLightNode(Light.Node)) else
    if Light.Node is TSpotLightNode_1 then
      SetupSpotLight_1(TSpotLightNode_1(Light.Node)) else
    if Light.Node is TSpotLightNode then
      SetupSpotLight(TSpotLightNode(Light.Node)) else
      raise EInternalError.Create('Unknown light node class');

    { setup attenuation for OpenGL light }
    SetNoAttenuation := true;

    if (Light.Node is TAbstractPositionalLightNode) then
    begin
      Attenuat := TAbstractPositionalLightNode(Light.Node).FdAttenuation.Value;
      if not ZeroVector(Attenuat) then
      begin
        SetNoAttenuation := false;
        glLightf(glLightNum, GL_CONSTANT_ATTENUATION, Attenuat[0]);
        glLightf(glLightNum, GL_LINEAR_ATTENUATION, Attenuat[1]);
        glLightf(glLightNum, GL_QUADRATIC_ATTENUATION, Attenuat[2]);
      end;
    end;

    if SetNoAttenuation then
    begin
      { lights with no Attenuation field or with Attenuation = (0, 0, 0)
         get default Attenuation = (1, 0, 0) }
      glLightf(glLightNum, GL_CONSTANT_ATTENUATION, 1);
      glLightf(glLightNum, GL_LINEAR_ATTENUATION, 0);
      glLightf(glLightNum, GL_QUADRATIC_ATTENUATION, 0);
    end;
  finally glPopMatrix end;

  { calculate Color4 = light color * light intensity }
  Color3 := VectorScale(Light.Node.FdColor.Value,
    Light.Node.FdIntensity.Value);
  Color4 := Vector4Single(Color3, 1);

  { calculate AmbientColor4 = light color * light ambient intensity }
  if Light.Node.FdAmbientIntensity.Value < 0 then
    AmbientColor4 := Color4 else
  begin
    AmbientColor3 := VectorScale(Light.Node.FdColor.Value,
      Light.Node.FdAmbientIntensity.Value);
    AmbientColor4 := Vector4Single(AmbientColor3, 1);
  end;

  glLightv(glLightNum, GL_AMBIENT, AmbientColor4);
  glLightv(glLightNum, GL_DIFFUSE, Color4);
  glLightv(glLightNum, GL_SPECULAR, Color4);

  glEnable(glLightNum);
{$else}
begin
  // TODO-es We shave to pass light to shaders in our own vars
{$endif}
end;

{ TVRMLGLLightsRenderer ----------------------------------------------- }

constructor TVRMLGLLightsRenderer.Create(
  const ALightRenderEvent: TLightRenderEvent);
begin
  inherited Create;
  FLightRenderEvent := ALightRenderEvent;
  LightsKnown := false;
  SetLength(LightsDone, GLFeatures.MaxLights);
end;

function TVRMLGLLightsRenderer.NeedRenderLight(Index: Integer; Light: PLightInstance): boolean;
begin
  Result := not (
    LightsKnown and
    ( { Light Index is currently disabled, and we want it disabled: Ok. }
      ( (LightsDone[Index] = nil) and
        (Light = nil) )
      or
      { Light Index is currently enabled, and we want it enabled,
        with the same LightNode and Transform: Ok.
        (Other TLightInstance record properties are calculated from
        LightNode and Transform, so no need to compare them). }
      ( (LightsDone[Index] <> nil) and
        (Light <> nil) and
        (LightsDone[Index]^.Node = Light^.Node) and
        (MatricesPerfectlyEqual(
          LightsDone[Index]^.Transform, Light^.Transform)) )
    ));
  if Result then
    { Update LightsDone[Index], if change required. }
    LightsDone[Index] := Light;
  Inc(Statistics[Result]);
end;

procedure TVRMLGLLightsRenderer.Render(
  const Lights1, Lights2: TLightInstancesList;
  const Shader: TShader);
var
  LightsEnabled: Cardinal;

  procedure AddList(Lights: TLightInstancesList);
  var
    I: Integer;
    LightOn: boolean;
    Light: PLightInstance;
  begin
    for I := 0 to Lights.Count - 1 do
    begin
      Light := Addr(Lights.L[I]);

      LightOn := Light^.Node.FdOn.Value;
      if Assigned(LightRenderEvent) then
        LightRenderEvent(Light^, LightOn);

      if LightOn then
      begin
        if NeedRenderLight(LightsEnabled, Light) then
          glLightFromVRMLLight(LightsEnabled, Light^);
        Shader.EnableLight(LightsEnabled, Light);
        Inc(LightsEnabled);
        if LightsEnabled >= GLFeatures.MaxLights then Exit;
      end;
    end;
  end;

var
  I: Integer;
begin
  LightsEnabled := 0;
  if LightsEnabled >= GLFeatures.MaxLights then Exit;

  AddList(Lights1);
  if LightsEnabled >= GLFeatures.MaxLights then Exit;

  if Lights2 <> nil then
  begin
    AddList(Lights2);
    if LightsEnabled >= GLFeatures.MaxLights then Exit;
  end;

  {$ifndef OpenGLES}
  { Disable remaining light for fixed-function pipeline }
  for I := LightsEnabled to GLFeatures.MaxLights - 1 do
    if NeedRenderLight(I, nil) then
      glDisable(GL_LIGHT0 + I);
  {$endif}

  LightsKnown := true;
end;

end.
