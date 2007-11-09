{
  Copyright 2007 Michalis Kamburelis.

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

{ TShadowVolumesHelper class. }
unit ShadowVolumesHelper;

interface

uses VectorMath, Boxes3d, OpenGLh;

type
  { This class performs various initialization and calculations related
    to shadow volume rendering. It does everything, except the actual
    shadow volume rendering (this is handled elsewhere, for example
    in TVRMLFlatSceneGL.RenderShadowVolume).

    To use this class: call InitGLContext, InitFrustumAndLight,
    InitScene at appropriate moments. Setup your stencil buffer with
    provided SetStencilOpSeparate or SetStencilOpForFront / SetStencilOpForBack.
    Pass the instance of this to TVRMLFlatSceneGL.RenderShadowVolume. }
  TShadowVolumesHelper = class
  private
    IsFrustumLightBox: boolean;
    FrustumLightBox: TBox3d;
    FFrustum: TFrustum;
    FrustumNearPoints: TFrustumPointsDouble;

    FWrapAvailable: boolean;
    FStencilOpIncrWrap, FStencilOpDecrWrap: TGLenum;

    { These will ideally be initialized to GL_INCR/DECR_WRAP (available
      in OpenGL >= 2.0) or GL_INCR/DECR_WRAP_EXT (available if EXT_stencil_wrap).
      Actually values with and without _EXT are the same.

      If OpenGL will not have these available, then they will be equal to
      old GL_INCR/DECT constants (without wrapping).

      These are set by InitGL. WrapAvailable says whether
      they were set to _WRAP_ versions.

      @groupBegin }
    property StencilOpIncrWrap: TGLenum read FStencilOpIncrWrap;
    property StencilOpDecrWrap: TGLenum read FStencilOpDecrWrap;
    { @groupEnd }

    FSceneShadowPossiblyVisible: boolean;
    FZFail: boolean;
    FZFailAndLightCap: boolean;

    FLightPosition: TVector4Single;
  public
    property WrapAvailable: boolean read FWrapAvailable;

    { Call this when OpenGL context is initialized, this will set some things.
      For now, this sets StencilOpIncrWrap, StencilOpDecrWrap. }
    procedure InitGLContext;

    { Call this when camera frustum is known and light position (of the shadow
      casting light) is known, typically at the beginning of your drawing routine.
      You have to call this before InitScene.

      This prepares some things (so that each InitScene call doesn't have to) and
      all subsequent InitScene calls assume that Frustum and
      LightPosition are the same. }
    procedure InitFrustumAndLight(
      const Frustum: TFrustum;
      const ALightPosition: TVector4Single);

    { Light casting shadows position, initialized by InitFrustumAndLight. }
    property LightPosition: TVector4Single read FLightPosition;

    { Call this when the bounding box of shadow caster is known.

      This calculates various things related to shadow volumes rendering
      of this scene.  1. checks whether you need to render shadow of the
      object inside SceneBox, settting SceneShadowPossiblyVisible.
      2. checks whether ZFail method is needed, setting ZFail.

      TODO: this should set stencilop also, based on
      glStencilOpSeparate is available and ZFail, also avoiding resetting
      the same state if not necessary.

      This assumes that Frustum and LightPosition values given
      in InitFrustumAndLight are OK.  }
    procedure InitScene(const SceneBox: TBox3d);

    { This is an alternative version of InitScene that initializes the same
      variables as InitScene, but assumes that the scene, along with it's
      shadow, will always be visible. This means that for example
      SceneShadowPossiblyVisible, ZFail, ZFailAndLightCap will always be @true.

      Use this only if you have a scene that will really be always visible
      and z-fail will be needed. For example, level scene in FPS games
      will usually be always visible, so making optimizations in
      InitScene may be useless. Also, this is useful if for any reason you
      don't know bounding box of the scene. }
    procedure InitSceneAlwaysVisible;

    { Does the shadow need to be rendered, set by InitScene. }
    property SceneShadowPossiblyVisible: boolean
      read FSceneShadowPossiblyVisible;

    { Is the ZFail method needed, set by InitScene. }
    property ZFail: boolean read FZFail;

    { Is the ZFail with light caps method needed, set by InitScene. }
    property ZFailAndLightCap: boolean read FZFailAndLightCap;

    { These set glStencilOpSeparate (suitable for both front and
      back faces) or glStencil (suitable only for front or only for back faces)
      as appropriate.

      Use this before rendering shadow volumes.
      It uses ZFail to decide what setup is necessary.
      It also uses StencilOpIncrWrap / StencilOpDecrWrap as needed. }
    procedure SetStencilOpSeparate;
    procedure SetStencilOpForFront;
    procedure SetStencilOpForBack;
  end;

implementation

uses SysUtils, KambiUtils, KambiStringUtils, KambiLog;

procedure TShadowVolumesHelper.InitGLContext;
begin
  { calcualte WrapAvailable, StencilOpIncrWrap, StencilOpDecrWrap }
  FWrapAvailable := (GLVersion.Major >= 2) or GL_EXT_stencil_wrap;
  if WrapAvailable then
  begin
    FStencilOpIncrWrap := GL_INCR_WRAP_EXT;
    FStencilOpDecrWrap := GL_DECR_WRAP_EXT;
  end else
  begin
    FStencilOpIncrWrap := GL_INCR;
    FStencilOpDecrWrap := GL_DECR;
  end;

  { This looks hacky, but actually this is how it should be:
    - with Mesa versions 6.x (tested with 6.4.1, 6.5.1, 6.5.2),
      glStencilOpSeparate is not nil, but it doesn't work.
    - Same thing happens with NVidia legacy 96xx drivers (reported
      version is "1.5.8 NVIDIA 96.43.01").
    I guess that's OK (I mean, it's not Mesa/NVidia bug), as I should look
    for glStencilOpSeparate only if GL version is >= 2. }
  if GLVersion.Major <= 1 then
    glStencilOpSeparate := nil;

  if Log then
    WritelnLogMultiline('Shadow volumes initialization',
      Format('GL_INCR/DECR_WRAP_EXT available: %s' + nl +
             'glStencilOpSeparate available: %s',
            [ BoolToStr[WrapAvailable],
              BoolToStr[glStencilOpSeparate <> nil] ]));

  { TODO } FZFail := true;
end;

procedure TShadowVolumesHelper.InitFrustumAndLight(
  const Frustum: TFrustum;
  const ALightPosition: TVector4Single);
var
  ALightPosition3: TVector3Single absolute ALightPosition;
begin
  FFrustum := Frustum;

  CalculateFrustumPoints(FrustumNearPoints, Frustum, true);

  IsFrustumLightBox := (ALightPosition[3] = 1) and
    ( (Frustum[fpFar][0] <> 0) or
      (Frustum[fpFar][1] <> 0) or
      (Frustum[fpFar][2] <> 0) );

  { TODO: this is very lame frustum culling.
    1. First of all, this works only for positional lights.
    2. Second, this represents the sum of
       Frustum + light position as a box3d --- while a convex hull of max
       9 points would be more optimal.
    3. Third, it doesn't work for frustum with infinite far plane
       (and this is practically neededfor z-fail approach).
       (CalculateFrustumPoints, and so FrustumAndPointBoundingBox,
       cannot work with such frustum).
  }

  if IsFrustumLightBox then
    FrustumLightBox := FrustumAndPointBoundingBox(Frustum, ALightPosition3);

  FLightPosition := ALightPosition;
end;

procedure TShadowVolumesHelper.InitScene(const SceneBox: TBox3d);

  function CalculateZFail: boolean;
  begin
    Result := true; // TODO, use FrustumNearPoints
  end;

begin
  { Frustum culling for shadow volumes:
    If the light is positional (so we have FrustumLightBox),
    and Box is outside FrustumLightBox, there's
    no need to render shadow quads for this creature.
    This is a very usefull optimization in certain cases
    (level with many creatures, spread evenly on the level;
    on "The Castle" Doom level, for example, it wasn't hard to find place
    where this optimization improved FPS to 40 from 17). }
  FSceneShadowPossiblyVisible := (not IsFrustumLightBox) or
    Boxes3dCollision(SceneBox, FrustumLightBox);

  FZFail := FSceneShadowPossiblyVisible and CalculateZFail;

  FZFailAndLightCap := ZFail and
    FrustumBox3dCollisionPossibleSimple(FFrustum, SceneBox);
end;

procedure TShadowVolumesHelper.InitSceneAlwaysVisible;
begin
  FSceneShadowPossiblyVisible := true;
  FZFail := true;
  FZFailAndLightCap := true;
end;

procedure TShadowVolumesHelper.SetStencilOpSeparate;
begin
  if ZFail then
  begin
    glStencilOpSeparate(GL_FRONT, GL_KEEP, StencilOpIncrWrap, GL_KEEP);
    glStencilOpSeparate(GL_BACK , GL_KEEP, StencilOpDecrWrap, GL_KEEP);
  end else
  begin
    glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, StencilOpIncrWrap);
    glStencilOpSeparate(GL_BACK , GL_KEEP, GL_KEEP, StencilOpDecrWrap);
  end;
end;

procedure TShadowVolumesHelper.SetStencilOpForFront;
begin
  if ZFail then
    glStencilOp(GL_KEEP, StencilOpIncrWrap, GL_KEEP) else
    { For each fragment that passes depth-test, *increase* it's stencil value. }
    glStencilOp(GL_KEEP, GL_KEEP, StencilOpIncrWrap);
end;

procedure TShadowVolumesHelper.SetStencilOpForBack;
begin
  if ZFail then
    glStencilOp(GL_KEEP, StencilOpDecrWrap, GL_KEEP) else
    { For each fragment that passes depth-test, *decrease* it's stencil value. }
    glStencilOp(GL_KEEP, GL_KEEP, StencilOpDecrWrap);
end;

end.
