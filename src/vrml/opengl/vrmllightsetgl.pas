{
  Copyright 2003-2006,2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Renders VRML light set (TDynActiveLightArray)
  into OpenGL.)

  It is mainly used by VRMLOpenGLRenderer
  (to render lights while rendering whole VRML graph) but it can
  also be used separately : e.g. @code(lets_take_a_walk) has a file
  "lightset.wrl" that is used only to extract light nodes from it
  and then these nodes have to be "rendered", i.e. they must be activated
  into OpenGL context (as OpenGL lights) so that they can affect any
  data rendered to OpenGL afterwards (e.g. other VRML files).
  This unit can be used for exactly this kind of thing : loading VRML
  graph only to extract lights and render them as OpenGL lights.
}

unit VRMLLightSetGL;

{$I openglmac.inc}

interface

uses VectorMath, GL, GLU, GLExt, KambiGLUtils, VRMLNodes, VRMLLightSet;

type
  { Allows you to modify light's properties (currently, only the "on" state)
    of the light right before it's rendered.
    Used by glLightFromVRMLLight and many other routines in this unit.

    By default, LightOn is the value of Light.LightNode.FdOn field.
    You can change it if you want. }
  TVRMLLightRenderEvent = procedure (const Light: TActiveLight;
    var LightOn: boolean) of object;

{ Sets up OpenGL light (number glLightNum) properties based on VRMLLight
  properties. It means that it calls
    glLight*(GL_LIGHT0 + glLightNum, ..., ...)
  some number of times to initialize OpenGL light.
  It requires that current matrix = GL_MODELVIEW.
  It may call glPush/PopMatrix and do some matrix loading/multiplying operations
  (but it is guaranteed that at the end OpenGL current matrix will be
  the same as before calling this functions (i.e. current matrix
  will be restored if necessary); it is also guaranteed that it will not
  need more that one slot on MODELVIEW matrices stack).
  BTW, you can see that you can nicely wrap this procedure inside OpenGL
  display list.

  If UseLightOnProperty than we will examine VRMLLight.LightNode.FdOn.Value
  and we will do glEnable/Disable(GL_LIGHT0 + glLightNum). If not,
  we will always proceed just like VRMLLight.LightNode.FdOn.Value = true
  (but we will not call glEnable(GL_LIGHT0 + glLightNum)).
  We DO NOT have a separate procedure to do the behaviour with
  UseLightOnProperty = false and separate with UseLightOnProperty = true
  (something like a separate procedure glLightFromVRMLLightAssumeOn, and
  procedure named glLightFromVRMLLight always behaves like
  UseLightOnProperty = true) because this is so important property that we
  want to force user of this procedure (i.e. myself) to ALWAYS give an
  explicit value for UseLightOnProperty.

  It makes no assumptions
  about the current state of this OpenGL light - i.e. you don't have to care
  what is the state of glLightNum before calling glLightFromVRMLLight -
  this procedure will take care of everything and will call glLight* with
  enough parameters to fully set up the behoviour of this OpenGL light.
  (it doesn't mean that it calls glLight*(GL_LIGHT0 + glLightNum, Param, ...)
  with every possible Param value - e.g. for non-spot lights
  it sets GL_SPOT_CUTOFF to 180 and then it is undefined whether it
  will call glLight with Param in [GL_SPOT_DIRECTION, GL_SPOT_EXPONENT]
  because GL_SPOT_DIRECTION and GL_SPOT_EXPONENT meaningless if
  GL_SPOT_CUTOFF = 180; moreover, if UseLightOnProperty = true and
  light is off (VRMLLight.LightNode.FdOn.Value = false) then it is possible
  that this procedure will call glDisable(GL_LIGHT0 + glLightNum) and
  will not call ANY glLight*).

  The idea is that after calling glLightFromVRMLLight
  you can always make assumption that "this OpenGL light corresponds
  exactly to given VRML light" and nothing more.

  ColorModulatorSingle may be nil. If not nil, it will be called to filter
  Light Color.
}
procedure glLightFromVRMLLight(glLightNum: Integer; const Light: TActiveLight;
  UseLightOnProperty: boolean;
  ColorModulatorSingle: TColorModulatorSingleFunc;
  LightRenderEvent: TVRMLLightRenderEvent);

{ glLightsFromVRML dla kazdego swiatla Lights[i] zrobi
    glLightFromVRMLLight(glLightNum1 + i, Lights[i], true, ColorModulatorSingle)
  przy czym glLightNum1 + i musi byc <= glLightNum2.

  Jezeli mamy wiecej swiatel VRMLa (LightsCount) niz dostepnych swiatel
  OpenGLa (glLightNum2 - glLightNum1 +1) to (niestety) nie zaladujemy
  wszystkich swiatel VRMLa (Lights) do OpenGLa - zaladujemy tylko tyle
  ile sie zmiesci czyli glLightNum2 - glLightNum1 +1.

  W odwrotnej sytuacji (jesli mamy wiecej wolnych swiatel OpenGLa
  niz potrzebujemy) ustawimy nadmiarowe swiatla OpenGLa na Disabled.

  W rezultacie wywolanie tej procedury gwarantuje zaladowanie zadanej
  listy swiatel VRMLa (Lights, LightsCount) do swiatel OpenGLa
  glLightNum1..glLightNum2. Poczatkowy stan swiatel OpenGLa
  glLightNum1..glLightNum2 (przed wywolaniem tej procedury)
  nie jest istotny - ta procedura zdeterminuje go w pelni na podstawie
  dostepnych Lights.
}
procedure glLightsFromVRML(Lights: PArray_ActiveLight; LightsCount: Integer;
  glLightNum1, glLightNum2: Integer;
  ColorModulatorSingle: TColorModulatorSingleFunc;
  LightRenderEvent: TVRMLLightRenderEvent); overload;

procedure glLightsFromVRML(Lights: TDynActiveLightArray;
  glLightNum1, glLightNum2: Integer;
  ColorModulatorSingle: TColorModulatorSingleFunc;
  LightRenderEvent: TVRMLLightRenderEvent); overload;

type
  { Use this to render many light sets (TDynActiveLightArray) and avoid
    to configure the same light many times.

    The idea is that calling Render() is just like doing glLightsFromVRML,
    that is it sets up given OpenGL lights. But this class remembers what
    VRML light was set on what OpenGL light, and assumes that VRML lights
    don't change during TVRMLGLLightsCachingRenderer execution. So OpenGL
    light will not be configured again, if it's already configured
    correctly.

    Note that LightRenderEvent event for this must be deterministic,
    based purely on light properties. For example, it's Ok to
    make LightRenderEvent that turns off lights that have kambiShadows = TRUE.
    It is @italic(not Ok) to make LightRenderEvent that sets LightOn to
    random boolean value. IOW, caching here assumes that for the same Light
    values, LightRenderEvent will set LightOn the same. }
  TVRMLGLLightsCachingRenderer = class
  private
    FGLLightNum1, FGLLightNum2: Integer;
    FColorModulatorSingle: TColorModulatorSingleFunc;
    FLightRenderEvent: TVRMLLightRenderEvent;
    LightsKnown: boolean;
    LightsDone: array of PActiveLight;
  public
    constructor Create(const AGLLightNum1, AGLLightNum2: Integer;
      const AColorModulatorSingle: TColorModulatorSingleFunc;
      const ALightRenderEvent: TVRMLLightRenderEvent);

    procedure Render(Lights: TDynActiveLightArray);
    procedure Render(Lights: PArray_ActiveLight; LightsCount: Integer);

    property GLLightNum1: Integer read FGLLightNum1;
    property GLLightNum2: Integer read FGLLightNum2;
    property ColorModulatorSingle: TColorModulatorSingleFunc
      read FColorModulatorSingle;
    property LightRenderEvent: TVRMLLightRenderEvent read FLightRenderEvent;
  public
    { Statistics of how many OpenGL lights setups were done
      (Statistics[true]) vs how many were avoided (Statistics[false]).
      This allows you to decide is using TVRMLGLLightsCachingRenderer
      class sensible (as opposed to directly rendering with glLightsFromVRML
      calls). }
    Statistics: array [boolean] of Cardinal;
  end;

(* To jest obiekt ktory umozliwia latwe zrobienie czegos takiego jak
   lightset.wrl w lets_take_a_walk, o ktorym napisalem na poczatku tego
   modulu. This object creates Lights: TDynActiveLightArray object and loads
   to it all lights available in traversed part of the given RootNode.
   E.g. given the VRML
     #VRML V1.0 ascii
     DEF Light1 PointLight { }
     Separator { DEF Light2 PointLight { } }
     Switch { DEF Light3 PointLight { } }
   this object will init Lights to conatin Light1 and Light2 (NOT Light3).

   OpenGL contexts : between first RenderLights (when GL display list
   is calculated for the first time) to the next GLContextClose (called automatically
   by destructor and CalculateLights and sometimes by setting some properties)
   this object must be used in the same GL context.
   So usually you will find most comfortable to use this object like
   TVRMLGLScene: create and destroy it in the main program and
   call GLContextClose in the OnClose TGLWindow event (this behaviour ensures that
   Switch-Fullscreen-On/Off in TGLWindowDemo will work correctly).
*)
type
  TVRMLLightSetGL = class(TVRMLLightSet)
  private
    dlRenderLights: TGLuint; { =0 means "not initialized" }
    FGLLightNum1, FGLLightNum2: Integer;

    { This is like GLLightNum2, but it's not -1.
      Initialized by CalculateRealGLLightNum2.
      Deinitialized in GLContextClose (by setting this to invalid value = -1). }
    RealGLLightNum2: Integer;
    procedure CalculateRealGLLightNum2;
  private
    FColorModulatorSingle: TColorModulatorSingleFunc;
    procedure SetGLLightNum1(Value: Integer);
    procedure SetGLLightNum2(Value: Integer);
    procedure SetColorModulatorSingle(Value: TColorModulatorSingleFunc);
  public
    { recalculate Lights property (based on RootNode) and GLContextClose
      (GLContextClose must be called by this routine: if Lights changed then
      we have to regenerate display list for Render).  }
    procedure CalculateLights; override;

    { Wlasciwosci uzywane przez RenderLights, patrz tam po opis.
      Ustawianie tych wlasciwosci nie wiaze nas z kontekstem OpenGLa -
      - w szczegolnosci, ustawianie glLightNum2 na -1 nie powoduje
      natychmiastowego glGet(GL_MAX_LIGHT), wszystko to bedzie
      wykonywane dopiero w RenderLights.

      Standardowa uwaga do ColorModulatorSingle:
      jego dzialanie jest zapamietywane
      w RenderLights na display liscie, co oznacza ze ColorModulatorSingle
      powinien byc funkcja ktora dla tego samego argumentu zawsze odpowiada
      to samo (nie kieruje sie np. aktualnym stanem jakiejs tam innej zmiennej
      w programie). }
    property glLightNum1: Integer read FGLLightNum1 write SetGLLightNum1;
    property glLightNum2: Integer read FGLLightNum2 write SetGLLightNum2;
    property ColorModulatorSingle: TColorModulatorSingleFunc
      read FColorModulatorSingle write SetColorModulatorSingle; { = nil }

    { skrot do glLightsFromVRML(Lights, glLightNum1, glLightNum2,
      ColorModulatorSingle). Ponadto pozwala ci uzyc glLightNum2 = -1
      aby powiedziec ze wszystkie swiatla do konca sa wolne
      (czyli glLightNum2= -1 znaczy to samo co glGet(GL_MAX_LIGHT)-1).
      Ponadto uzywa w srodku display listy.

      This function creates connection between this object and current gl context. }
    procedure RenderLights;

    { This turns off all lights between glLightNum1 and glLightNum2
      (when glLightNum2 = -1 then it's interpreted as glGet(GL_MAX_LIGHT)-1).
      I.e. it calls glDisable(GL_LIGHTx) for them. }
    procedure TurnLightsOff;

    { Turn off lights not supposed to light in the shadow.

      This simply disables lights with @code(kambiShadows) field
      set to @true.
      See [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_shadows]
      for more info.

      Lights with kambiShadows = FALSE are ignored:
      they are left untouched by this method (they are
      neither disabled, nor enabled --- usually you should enable them
      as needed by RenderLights). }
    procedure TurnLightsOffForShadows;

    { close any connection between this object and current gl context.
      After calling this, you can e.g. switch to another context and use
      this object there. You can also destroy current context and
      then free this object.

      Calling GLContextClose when there is no connection between this object and
      gl context (e.g. calling it twice in a row) is a valid NOP. }
    procedure GLContextClose;

    { wartosci GLLightNum1, GLLightNum2 sa tak wazne ze wolalem nie ustawiac
      ich w konstruktorze na jakies defaultowe wartosci tylko wymagac od ciebie
      podania ich explicite przy konstruowaniu obiektu. }
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      AGLLightNum1, AGLLightNum2: Integer);

    { calls GLContextClose }
    destructor Destroy; override;
  end;

implementation

uses SysUtils, KambiUtils, Math;

procedure glLightFromVRMLLight(glLightNum: Integer; const Light: TActiveLight;
  UseLightOnProperty: boolean;
  ColorModulatorSingle: TColorModulatorSingleFunc;
  LightRenderEvent: TVRMLLightRenderEvent);

  procedure glLightFromVRMLLightAssumeOn;

    { SetupXxx light : setup glLight properties GL_POSITION, GL_SPOT_* }
    procedure SetupDirectionalLight(LightNode: TVRMLDirectionalLightNode);
    begin
     glLightv(glLightNum, GL_POSITION, Vector4f(VectorNegate(LightNode.FdDirection.Value), 0));
     glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
    end;

    procedure SetupPointLight(LightNode: TVRMLPointLightNode);
    begin
     glLightv(glLightNum, GL_POSITION, Vector4f(LightNode.FdLocation.Value, 1));
     glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
    end;

    procedure SetupSpotLight_1(LightNode: TNodeSpotLight_1);
    begin
     glLightv(glLightNum, GL_POSITION, Vector4f(LightNode.FdLocation.Value, 1));

     glLightv(glLightNum, GL_SPOT_DIRECTION, LightNode.FdDirection.Value);
     glLightf(glLightNum, GL_SPOT_EXPONENT, LightNode.SpotExp);
     glLightf(glLightNum, GL_SPOT_CUTOFF,
       { Clamp to 90 for safety, see VRML 2.0 version for comments }
       Min(90, RadToDeg(LightNode.FdCutOffAngle.Value)));
    end;

    procedure SetupSpotLight_2(LightNode: TNodeSpotLight_2);
    begin
     glLightv(glLightNum, GL_POSITION, Vector4f(LightNode.FdLocation.Value, 1));

     glLightv(glLightNum, GL_SPOT_DIRECTION, LightNode.FdDirection.Value);

     { There is no way to translate beamWidth to OpenGL's GL_SPOT_EXPONENT.
       In OpenGL spotlight, there is *no* way to specify that light
       is uniform (maximum) within beamWidth, and that light amount
       falls linearly from beamWidth to cutOffAngle.
       In OpenGL, light intensity drops off by
       cosinus(of the angle)^GL_SPOT_EXPONENT.

       No sensible way to even approximate VRML behavior ?

       We can accurately express one specific case (that is
       actually the default, in you will not give beamWidth
       value in VRML 2.0): if beamWidth >= cutOffAngle, the light
       is maximum within full cutOffAngle. This is easy to
       do, just set spot_exponent to 0, then
       cosinus(of the angle)^GL_SPOT_EXPONENT is always 1.

       For other values of beamWidth, I just set spot_exponent
       to some arbitrary value and hope that result will look sensible...

       TODO: some VRML 2.0 extension to allow specifying
       exponent directly would be useful to give user actual
       control over this. Probably just add dropOffRate field
       (like in VRML 1.0) with def value like -1 and say that
       "dropOffRate < 0 means that we should try to approx
       beamWidth, otherwise dropOffRate is used".

       Looking at how other VRML implementations handle this:
       - Seems that most of them ignore the issue, leaving spot exponent
         always 0 and ignoring beamWidth entirely.
       - One implementation
         [http://arteclab.artec.uni-bremen.de/courses/mixed-reality/material/ARToolkit/ARToolKit2.52vrml/lib/libvrml/libvrml97gl/src/vrml97gl/old_ViewerOpenGL.cpp]
         does exactly like me --- checks beamWidth < cutOffAngle
         and sets spot_exponent to 0 or 1.
       - FreeWRL
         [http://search.cpan.org/src/LUKKA/FreeWRL-0.14/VRMLRend.pm]
         uses more intelligent approach setting
         GL_SPOT_EXPONENT to 0.5/ (beamWidth + 0.1).
         Which gives
           beamWidth = 0 => GL_SPOT_EXPONENT = 5
           beamWidth = Pi/4 => GL_SPOT_EXPONENT =~ 0.5 / 0.9 =~ 1/2
           beamWidth = Pi/2 => GL_SPOT_EXPONENT =~ 0.5 / 1.67 =~ 1/3
         Honestly I don't see how it's much better than our atbitrary way... }
     if LightNode.FdBeamWidth.Value >= LightNode.FdCutOffAngle.Value then
       glLightf(glLightNum, GL_SPOT_EXPONENT, 0) else
       glLightf(glLightNum, GL_SPOT_EXPONENT, 1
         { 0.5 / (LightNode.FdBeamWidth.Value + 0.1) });

     glLightf(glLightNum, GL_SPOT_CUTOFF,
       { Clamp to 90, to protect against user inputting invalid value in VRML,
         or just thing like 1.5708, which may be recalculated by
         RadToDeg to 90.0002104591, so > 90, and OpenGL raises "invalid value"
         error then... }
       Min(90, RadToDeg(LightNode.FdCutOffAngle.Value)));
    end;

  var SetNoAttenuation: boolean;
      Attenuat: TVector3Single;
      Color3, AmbientColor3: TVector3f;
      Color4, AmbientColor4: TVector4f;
  begin
   glPushMatrix;
   try
    glMultMatrix(Light.Transform);

    if Light.LightNode is TVRMLDirectionalLightNode then
      SetupDirectionalLight(TVRMLDirectionalLightNode(Light.LightNode)) else
    if Light.LightNode is TVRMLPointLightNode then
      SetupPointLight(TVRMLPointLightNode(Light.LightNode)) else
    if Light.LightNode is TNodeSpotLight_1 then
      SetupSpotLight_1(TNodeSpotLight_1(Light.LightNode)) else
    if Light.LightNode is TNodeSpotLight_2 then
      SetupSpotLight_2(TNodeSpotLight_2(Light.LightNode)) else
      raise EInternalError.Create('Unknown light node class');

    { setup attenuation for OpenGL light }
    SetNoAttenuation := true;

    if (Light.LightNode is TVRMLPositionalLightNode) then
    begin
     Attenuat := TVRMLPositionalLightNode(Light.LightNode).FdAttenuation.Value;
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

   { calculate Color4 = light color * light intensity,
     eventually modulated. }
   Color3 := VectorScale(Light.LightNode.FdColor.Value,
     Light.LightNode.FdIntensity.Value);
   if Assigned(ColorModulatorSingle) then
     Color3 := ColorModulatorSingle(Color3);
   Color4 := Vector4f(Color3, 1);

   { calculate AmbientColor4 = light color * light ambient intensity,
     eventually modulated. }
   if Light.LightNode.FdAmbientIntensity.Value < 0 then
     AmbientColor4 := Color4 else
   begin
     AmbientColor3 := VectorScale(Light.LightNode.FdColor.Value,
       Light.LightNode.FdAmbientIntensity.Value);
     if Assigned(ColorModulatorSingle) then
       AmbientColor3 := ColorModulatorSingle(AmbientColor3);
     AmbientColor4 := Vector4f(AmbientColor3, 1);
   end;

   glLightv(glLightNum, GL_AMBIENT, AmbientColor4);
   glLightv(glLightNum, GL_DIFFUSE, Color4);
   glLightv(glLightNum, GL_SPECULAR, Color4);
  end;

var
  LightOn: boolean;
begin
  glLightNum += GL_LIGHT0;

  if UseLightOnProperty then
  begin
    LightOn := Light.LightNode.FdOn.Value;
    { Call LightRenderEvent, allowing it to change LightOn value. }
    if Assigned(LightRenderEvent) then
      LightRenderEvent(Light, LightOn);
    if LightOn then
    begin
      glLightFromVRMLLightAssumeOn;
      glEnable(glLightNum);
    end else
      glDisable(glLightNum);
  end else
    glLightFromVRMLLightAssumeOn;
end;

procedure glLightsFromVRML(
  Lights: PArray_ActiveLight; LightsCount: Integer;
  glLightNum1, glLightNum2: Integer;
  ColorModulatorSingle: TColorModulatorSingleFunc;
  LightRenderEvent: TVRMLLightRenderEvent);
var
  I: Integer;
begin
  if LightsCount >= glLightNum2-glLightNum1 + 1  then
  begin
    { use all available OpenGL lights }
    for i := 0 to GLLightNum2 - GLLightNum1 do
      glLightFromVRMLLight(GLLightNum1 + i, Lights^[i], true,
        ColorModulatorSingle, LightRenderEvent);
  end else
  begin
    { use some OpenGL lights for VRML lights, disable rest of the lights }
    for i := 0 to LightsCount - 1 do
      glLightFromVRMLLight(GLLightNum1 + i, Lights^[i], true,
        ColorModulatorSingle, LightRenderEvent);
    for i := LightsCount to GLLightNum2-GLLightNum1 do
      glDisable(GL_LIGHT0 + GLLightNum1 + i);
  end;
end;

procedure glLightsFromVRML(Lights: TDynActiveLightArray;
  GLLightNum1, GLLightNum2: Integer;
  ColorModulatorSingle: TColorModulatorSingleFunc;
  LightRenderEvent: TVRMLLightRenderEvent);
begin
  glLightsFromVRML(Lights.ItemsArray, Lights.Count, GLLightNum1, GLLightNum2,
    ColorModulatorSingle, LightRenderEvent);
end;

{ TVRMLGLLightsCachingRenderer ----------------------------------------------- }

constructor TVRMLGLLightsCachingRenderer.Create(
  const AGLLightNum1, AGLLightNum2: Integer;
  const AColorModulatorSingle: TColorModulatorSingleFunc;
  const ALightRenderEvent: TVRMLLightRenderEvent);
begin
  inherited Create;
  FGLLightNum1 := AGLLightNum1;
  FGLLightNum2 := AGLLightNum2;
  FColorModulatorSingle := AColorModulatorSingle;
  FLightRenderEvent := ALightRenderEvent;

  LightsKnown := false;
  { avoid range error when GLLightNum2 < GLLightNum1 }
  if GLLightNum2 >= GLLightNum1 then
    SetLength(LightsDone, GLLightNum2 - GLLightNum1 + 1);
end;

procedure TVRMLGLLightsCachingRenderer.Render(Lights: TDynActiveLightArray);
begin
  Render(Lights.ItemsArray, Lights.Count);
end;

procedure TVRMLGLLightsCachingRenderer.Render(Lights: PArray_ActiveLight;
  LightsCount: Integer);

  function NeedRenderLight(Index: Integer; Light: PActiveLight): boolean;
  begin
    Result := not (
      LightsKnown and
      ( { Light Index is currently disabled, and we want it disabled: Ok. }
        ( (LightsDone[Index] = nil) and
          (Light = nil) )
        or
        { Light Index is currently enabled, and we want it enabled,
          with the same LightNode and Transform: Ok.
          (Other TActiveLight record properties are calculated from
          LightNode and Transform, so no need to compare them). }
        ( (LightsDone[Index] <> nil) and
          (Light <> nil) and
          (LightsDone[Index]^.LightNode = Light^.LightNode) and
          (MatricesPerfectlyEqual(
            LightsDone[Index]^.Transform, Light^.Transform)) )
      ));
    if Result then
      { Update LightsDone[Index], if change required. }
      LightsDone[Index] := Light;
    Inc(Statistics[Result]);
  end;

var
  I: Integer;
begin
  if LightsCount >= GLLightNum2 - GLLightNum1 + 1  then
  begin
    { use all available OpenGL lights }
    for i := 0 to GLLightNum2 - GLLightNum1 do
      if NeedRenderLight(I, @(Lights^[i])) then
        glLightFromVRMLLight(GLLightNum1 + i, Lights^[i], true,
          ColorModulatorSingle, LightRenderEvent);
  end else
  begin
    { use some OpenGL lights for VRML lights, disable rest of the lights }
    for i := 0 to LightsCount - 1 do
      if NeedRenderLight(I, @(Lights^[i])) then
        glLightFromVRMLLight(GLLightNum1 + i, Lights^[i], true,
          ColorModulatorSingle, LightRenderEvent);
    for i := LightsCount to GLLightNum2-GLLightNum1 do
      if NeedRenderLight(I, nil) then
        glDisable(GL_LIGHT0 + GLLightNum1 + i);
  end;

  LightsKnown := true;
end;

{ TVRMLLightSetGL ------------------------------------------------------------ }

procedure TVRMLLightSetGL.SetGLLightNum1(Value: Integer);
begin
  if FGLLightNum1 <> Value then
  begin
    FGLLightNum1 := Value;
    GLContextClose;
  end;
end;

procedure TVRMLLightSetGL.SetGLLightNum2(Value: Integer);
begin
  if FGLLightNum2 <> Value then
  begin
    FGLLightNum2 := Value;
    GLContextClose;
  end;
end;

procedure TVRMLLightSetGL.SetColorModulatorSingle(Value: TColorModulatorSingleFunc);
begin
  if {$ifndef FPC_OBJFPC} @ {$endif} Value <>
     {$ifndef FPC_OBJFPC} @ {$endif} FColorModulatorSingle then
  begin
    {$ifndef FPC_OBJFPC} @ {$endif} FColorModulatorSingle :=
    {$ifndef FPC_OBJFPC} @ {$endif} Value;
    GLContextClose;
  end;
end;

procedure TVRMLLightSetGL.CalculateLights;
begin
  GLContextClose;
  inherited;
end;

procedure TVRMLLightSetGL.CalculateRealGLLightNum2;
begin
  if RealGLLightNum2 = -1 then
  begin
    RealGLLightNum2 := GLLightNum2;
    if RealGLLightNum2 = -1 then
      RealGLLightNum2 := glGetInteger(GL_MAX_LIGHTS) - 1;
  end;
end;

procedure TVRMLLightSetGL.RenderLights;
begin
  if dlRenderLights = 0 then
  begin
    CalculateRealGLLightNum2;

    dlRenderLights := glGenListsCheck(1, 'TVRMLLightSetGL.RenderLights');

    { As usual, I don't use here GL_COMPILE_AND_EXECUTE (because this
      can result in non-optimal display list). I use GL_COMPILE,
      and then I just call this list. }

    glNewList(dlRenderLights, GL_COMPILE);
    try
      glLightsFromVRML(Lights, glLightNum1, RealGLLightNum2,
        ColorModulatorSingle,
        { For now, LightRenderEvent is always nil here, as I didn't need
          it with TVRMLLightSetGL. There are no problems to add
          LightRenderEvent to TVRMLLightSetGL in the future. }
        nil);
    finally glEndList end;
  end;

  glCallList(dlRenderLights);
end;

procedure TVRMLLightSetGL.TurnLightsOff;
var
  I: Integer;
begin
  CalculateRealGLLightNum2;
  for I := GLLightNum1 to RealGLLightNum2 do
    glDisable(GL_LIGHT0 + I);
end;

procedure TVRMLLightSetGL.TurnLightsOffForShadows;
var
  MyLightNum, GLLightNum: Integer;
  L: PActiveLight;
begin
  CalculateRealGLLightNum2;

  L := Lights.Pointers[0];
  for MyLightNum := 0 to Lights.Count - 1 do
  begin
    GLLightNum := MyLightNum + GLLightNum1;

    if L^.LightNode.FdKambiShadows.Value then
    begin
      if GLLightNum <= RealGLLightNum2 then
        glDisable(GL_LIGHT0 + GLLightNum);
    end;

    Inc(L);
  end;
end;

procedure TVRMLLightSetGL.GLContextClose;
begin
  glFreeDisplayList(dlRenderLights);
  RealGLLightNum2 := -1;
end;

constructor TVRMLLightSetGL.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
  AGLLightNum1, AGLLightNum2: Integer);
begin
  inherited Create(ARootNode, AOwnsRootNode);
  FGLLightNum1 := AGLLightNum1;
  FGLLightNum2 := AGLLightNum2;
  RealGLLightNum2 := -1;
end;

destructor TVRMLLightSetGL.Destroy;
begin
  GLContextClose;
  inherited;
end;

end.

