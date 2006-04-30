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

{ @abstract(This unit renders VRML lights list (TDynActiveLightArray)
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

uses VectorMath, OpenGLh, KambiGLUtils, VRMLNodes, VRMLLightSet;

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
  UseLightOnProperty: boolean; ColorModulatorSingle: TColorModulatorSingleFunc);

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
  glLightNum1, glLightNum2: Integer; ColorModulatorSingle: TColorModulatorSingleFunc); overload;
procedure glLightsFromVRML(Lights: TDynActiveLightArray;
  glLightNum1, glLightNum2: Integer; ColorModulatorSingle: TColorModulatorSingleFunc); overload;

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
   is calculated for the first time) to the next CloseGL (called automatically
   by destructor and CalculateLights and sometimes by setting some properties)
   this object must be used in the same GL context.
   So usually you will find most comfortable to use this object like
   TVRMLFlatSceneGL: create and destroy it in the main program and
   call CloseGL in the OnClose TGLWindow event (this behaviour ensures that
   Switch-Fullscreen-On/Off in TGLWindowDemo will work correctly).
*)
type
  TVRMLLightSetGL = class(TVRMLLightSet)
  private
    dlRenderLights: TGLuint; { =0 means "not initialized" }
    FGLLightNum1, FGLLightNum2: Integer;

    { This is like GLLightNum2, but it's not -1.
      Initialized by CalculateRealGLLightNum2.
      Deinitialized in CloseGL (by setting this to invalid value = -1). }
    RealGLLightNum2: Integer;
    procedure CalculateRealGLLightNum2;

    FColorModulatorSingle: TColorModulatorSingleFunc;
    procedure SetGLLightNum1(Value: Integer);
    procedure SetGLLightNum2(Value: Integer);
    procedure SetColorModulatorSingle(Value: TColorModulatorSingleFunc);
  public
    { recalculate Lights property (based on RootNode) and CloseGL
      (CloseGL must be called by this routine: if Lights changed then
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

    { close any connection between this object and current gl context.
      After calling this, you can e.g. switch to another context and use
      this object there. You can also destroy current context and
      then free this object.

      Calling CloseGL when there is no connection between this object and
      gl context (e.g. calling it twice in a row) is a valid NOP. }
    procedure CloseGL;

    { wartosci GLLightNum1, GLLightNum2 sa tak wazne ze wolalem nie ustawiac
      ich w konstruktorze na jakies defaultowe wartosci tylko wymagac od ciebie
      podania ich explicite przy konstruowaniu obiektu. }
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean;
      AGLLightNum1, AGLLightNum2: Integer);

    { calls CloseGL }
    destructor Destroy; override;
  end;

implementation

uses SysUtils, KambiUtils, Math;

procedure glLightFromVRMLLight(glLightNum: Integer; const Light: TActiveLight;
  UseLightOnProperty: boolean; ColorModulatorSingle: TColorModulatorSingleFunc);

  procedure glLightFromVRMLLightAssumeOn;

    { SetupXxx light : setup glLight properties GL_POSITION, GL_SPOT_* }
    procedure SetupDirectionalLight(LightNode: TNodeDirectionalLight);
    begin
     glLightv(glLightNum, GL_POSITION, Vector4f(VectorNegate(LightNode.FdDirection.Value), 0));
     glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
    end;

    procedure SetupPointLight(LightNode: TNodePointLight);
    begin
     glLightv(glLightNum, GL_POSITION, Vector4f(LightNode.FdLocation.Value, 1));
     glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
    end;

    procedure SetupSpotLight(LightNode: TNodeSpotLight);
    begin
     glLightv(glLightNum, GL_POSITION, Vector4f(LightNode.FdLocation.Value, 1));

     glLightv(glLightNum, GL_SPOT_DIRECTION, LightNode.FdDirection.Value);
     glLightf(glLightNum, GL_SPOT_EXPONENT, LightNode.SpotExp);
     glLightf(glLightNum, GL_SPOT_CUTOFF, RadToDeg(LightNode.FdCutOffAngle.Value));
    end;

  var SetNoAttenuation: boolean;
      Attenuat: TVector3Single;
      LightColorMultIntensity3f: TVector3f;
      LightColorMultIntensity4f: TVector4f;
  begin
   glPushMatrix;
   try
    glMultMatrix(Light.Transform);

    case ArrayPosPointer(Light.LightNode.ClassType,
           [ TNodeDirectionalLight, TNodePointLight, TNodeSpotLight ]) of
     0: SetupDirectionalLight(TNodeDirectionalLight(Light.LightNode));
     1: SetupPointLight      (TNodePointLight      (Light.LightNode));
     2: SetupSpotLight       (TNodeSpotLight       (Light.LightNode));
     else raise EInternalError.Create('Unknown light node class');
    end;

    { setup attenuation for OpenGL light }
    SetNoAttenuation := true;

    if (Light.LightNode is TNodeGeneralPositionalLight) then
    begin
     Attenuat := TNodeGeneralPositionalLight(Light.LightNode).FdAttenuation.Value;
     if not IsZeroVector(Attenuat) then
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

   { evaluate LightColorMultIntensity4f }
   LightColorMultIntensity3f := VectorScale(Light.LightNode.FdColor.Value,
     Light.LightNode.FdIntensity.Value);
   if Assigned(ColorModulatorSingle) then
    LightColorMultIntensity3f := ColorModulatorSingle(LightColorMultIntensity3f);
   LightColorMultIntensity4f := Vector4f(LightColorMultIntensity3f, 1);

   { TODO: VRML97 - light ambient color powinienes brac z LightColor *
     LightAmbientIntensity, tzn. do ambient jest specjalne intensity. }
   glLightv(glLightNum, GL_AMBIENT, LightColorMultIntensity4f);
   glLightv(glLightNum, GL_DIFFUSE, LightColorMultIntensity4f);
   glLightv(glLightNum, GL_SPECULAR, LightColorMultIntensity4f);
  end;

begin
 glLightNum += GL_LIGHT0;

 if UseLightOnProperty then
 begin
  if Light.LightNode.FdOn.Value then
  begin
   glLightFromVRMLLightAssumeOn;
   glEnable(glLightNum);
  end else
   glDisable(glLightNum);
 end else
  glLightFromVRMLLightAssumeOn;
end;

procedure glLightsFromVRML(Lights: PArray_ActiveLight; LightsCount: Integer;
  glLightNum1, glLightNum2: Integer; ColorModulatorSingle: TColorModulatorSingleFunc); overload;
var i: Integer;
begin
 if LightsCount >= glLightNum2-glLightNum1+1  then
  begin
   { wykorzystujemy wszystkie dostepne swiatla OpenGLa }
   for i := 0 to glLightNum2-glLightNum1 do
    glLightFromVRMLLight(glLightNum1 + i, Lights[i], true, ColorModulatorSingle);
  end else
  begin
   { jezeli nie zamierzamy wykorzystac wszystkich swiatel OpenGL to
     niewykorzystanym swiatlom robimy Disabled (a wykorzystywanym robimy
     to co wyzej) }
   for i := 0 to LightsCount-1 do
    glLightFromVRMLLight(glLightNum1 + i, Lights[i], true, ColorModulatorSingle);
   for i := LightsCount to glLightNum2-glLightNum1 do
    glDisable(GL_LIGHT0 + glLightNum1 + i);
  end;
end;

procedure glLightsFromVRML(Lights: TDynActiveLightArray;
  glLightNum1, glLightNum2: Integer; ColorModulatorSingle: TColorModulatorSingleFunc); overload;
begin
 glLightsFromVRML(Lights.Items, Lights.Count, glLightNum1, glLightNum2,
   ColorModulatorSingle);
end;

{ TVRMLLightSetGL ------------------------------------------------------------ }

procedure TVRMLLightSetGL.SetGLLightNum1(Value: Integer);
begin
  if FGLLightNum1 <> Value then
  begin
    FGLLightNum1 := Value;
    CloseGL;
  end;
end;

procedure TVRMLLightSetGL.SetGLLightNum2(Value: Integer);
begin
  if FGLLightNum2 <> Value then
  begin
    FGLLightNum2 := Value;
    CloseGL;
  end;
end;

procedure TVRMLLightSetGL.SetColorModulatorSingle(Value: TColorModulatorSingleFunc);
begin
  if @Value <> @FColorModulatorSingle then
  begin
    @FColorModulatorSingle := @Value;
    CloseGL;
  end;
end;

procedure TVRMLLightSetGL.CalculateLights;
begin
  CloseGL;
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
        ColorModulatorSingle);
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

procedure TVRMLLightSetGL.CloseGL;
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
  CloseGL;
  inherited;
end;

end.

