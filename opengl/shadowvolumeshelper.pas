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
  TStencilSetupKind = (ssSeparate, ssForFront, ssForBack);

  { This class performs various initialization and calculations related
    to shadow volume rendering. It does everything, except the actual
    shadow volume rendering (this is handled elsewhere, for example
    in TVRMLFlatSceneGL.RenderShadowVolume).

    To use this class: call InitGLContext, InitFrustumAndLight,
    InitScene at appropriate moments. Setup your stencil buffer by
    setting StencilSetupKind before InitScene.
    Pass the instance of this to TVRMLFlatSceneGL.RenderShadowVolume. }
  TShadowVolumesHelper = class
  private
    FrustumAndLightPlanes: array [0..5] of TVector4Single;
    FrustumAndLightPlanesCount: Cardinal;
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
    FLightPositionDouble: TVector4Double;

    StencilConfigurationKnown: boolean;
    StencilConfigurationKnownKind: TStencilSetupKind;
    StencilConfigurationKnownZFail: boolean;

    FStencilSetupKind: TStencilSetupKind;

    FCount: boolean;
    FCountScenes: Cardinal;
    FCountShadowsNotVisible: Cardinal;
    FCountZPass: Cardinal;
    FCountZFailNoLightCap: Cardinal;
    FCountZFailAndLightCap: Cardinal;

    procedure UpdateCount;
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
      LightPosition are the same.

      It also resets CountScenes etc. counters for debug purposes. }
    procedure InitFrustumAndLight(
      const Frustum: TFrustum;
      const ALightPosition: TVector4Single);

    { Light casting shadows position, initialized by InitFrustumAndLight. }
    property LightPosition: TVector4Single read FLightPosition;
    property LightPositionDouble: TVector4Double read FLightPositionDouble;

    { Call this when the bounding box of shadow caster is known.

      This calculates various things related to shadow volumes rendering
      of this scene.  1. checks whether you need to render shadow of the
      object inside SceneBox, settting SceneShadowPossiblyVisible.
      2. checks whether ZFail method is needed, setting ZFail.

      This assumes that Frustum and LightPosition values given
      in InitFrustumAndLight are OK.

      Also note that after InitFrustumAndLight, all InitScene will assume that
      they have complete control over glStencilOp states for the time
      of rendering shadow volumes. In other words: InitScene will setup
      some stencil configuration, depending on ZFail state and StencilSetupKind.
      For the sake of speed, we try to avoid setting the same state
      twice, so we optimize it: first InitScene after InitFrustumAndLight
      does always setup stencil confguration. Following InitScene will
      only change stencil configuration if ZFail value will actually change.
      This means that if e.g. all objects will be detected to be in z-pass
      method, then stencil configuration will be done only once.

      The above optimization works OK if you do not change StencilOp configuration
      yourself during SV rendering. }
    procedure InitScene(const SceneBox: TBox3d);

    { You can split InitScene call into these two calls,
      first InitSceneDontSetupStencil and then InitSceneOnlySetupStencil.

      This is useful only in very special cases, in particular: if you
      only have one shadow caster object in your scene. Then you
      can call InitSceneDontSetupStencil only once, before any drawing,
      and later you can call InitSceneOnlySetupStencil multiple times
      to set stencil configuration for this object.

      @groupBegin }
    procedure InitSceneDontSetupStencil(const SceneBox: TBox3d);
    procedure InitSceneOnlySetupStencil;
    { @groupEnd }

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

    { What kind of stencil settings should be set by InitScene ?

      Set ssSeparate only if glStencilOpSeparate is available.
      Otherwise, you have to use 2-pass method (render everything
      2 times, culling front one time, culling back second time) ---
      in this case use ssForFront or ssForBack as appropriate. }
    property StencilSetupKind: TStencilSetupKind
      read FStencilSetupKind  write FStencilSetupKind
      default ssSeparate;

    property Count: boolean read FCount write FCount default false;
    property CountScenes: Cardinal read FCountScenes;
    property CountShadowsNotVisible: Cardinal read FCountShadowsNotVisible;
    property CountZPass: Cardinal read FCountZPass;
    property CountZFailNoLightCap: Cardinal read FCountZFailNoLightCap;
    property CountZFailAndLightCap: Cardinal read FCountZFailAndLightCap;
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
end;

procedure TShadowVolumesHelper.InitFrustumAndLight(
  const Frustum: TFrustum;
  const ALightPosition: TVector4Single);

  procedure CalculateFrustumAndLightPlanes;
  var
    FP, LastPlane: TFrustumPlane;
  begin
    FrustumAndLightPlanesCount := 0;

    LastPlane := High(FP);
    Assert(LastPlane = fpFar);

    { if infinite far plane, then ignore it }
    if (Frustum[fpFar][0] = 0) and
       (Frustum[fpFar][1] = 0) and
       (Frustum[fpFar][2] = 0) then
      LastPlane := Pred(LastPlane);

    for FP := Low(FP) to LastPlane do
    begin
      { This checks that LightPosition is inside Frustum[FP] plane.

        When LightPosition[3] = 1, this is normal test on which side
        of plane lies a point, so then it's OK (frustum planes point inside
        the frustum). For LightPosition[3] > 0 this is also  equivalent.

        For LightPosition[3] = 0 (directional light), this check dot product
        between light direction and plane direction. So >= 0 means that they
        point in the same dir (angle < 90 degs), so the light position
        in infinity can also be considered inside this plane. }
      if Frustum[FP][0] * LightPosition[0] +
         Frustum[FP][1] * LightPosition[1] +
         Frustum[FP][2] * LightPosition[2] +
         Frustum[FP][3] * LightPosition[3] >= 0 then
      begin
        FrustumAndLightPlanes[FrustumAndLightPlanesCount] := Frustum[FP];
        Inc(FrustumAndLightPlanesCount);
      end;
    end;

    { TODO: a better convex hull between light position and frustum could
      be useful. We should add additional planes to FrustumAndLightPlanes
      for this. Some pointers on [http://www.terathon.com/gdc06_lengyel.ppt]. }
  end;

var
  ALightPosition3: TVector3Single absolute ALightPosition;
begin
  FFrustum := Frustum;
  FLightPosition := ALightPosition;
  FLightPositionDouble := Vector4Double(ALightPosition);

  CalculateFrustumPoints(FrustumNearPoints, Frustum, true);

  CalculateFrustumAndLightPlanes;

  StencilConfigurationKnown := false;

  FCountScenes := 0;
  FCountShadowsNotVisible := 0;
  FCountZPass := 0;
  FCountZFailNoLightCap := 0;
  FCountZFailAndLightCap := 0;
end;

procedure TShadowVolumesHelper.InitScene(const SceneBox: TBox3d);
begin
  InitSceneDontSetupStencil(SceneBox);
  InitSceneOnlySetupStencil;
end;

procedure TShadowVolumesHelper.InitSceneDontSetupStencil(const SceneBox: TBox3d);

  function CalculateShadowPossiblyVisible(const SceneBox: TBox3d): boolean;
  var
    I: Integer;

    function CheckPoint(const X, Y, Z: Integer): boolean;
    begin
      Result :=
        SceneBox[X][0] * FrustumAndLightPlanes[I][0] +
        SceneBox[Y][1] * FrustumAndLightPlanes[I][1] +
        SceneBox[Z][2] * FrustumAndLightPlanes[I][2] +
        FrustumAndLightPlanes[I][3] < 0;
    end;

  begin
    for I := 0 to Integer(FrustumAndLightPlanesCount) - 1 do
    begin
      if CheckPoint(0, 0, 0) and
         CheckPoint(0, 0, 1) and
         CheckPoint(0, 1, 0) and
         CheckPoint(0, 1, 1) and
         CheckPoint(1, 0, 0) and
         CheckPoint(1, 0, 1) and
         CheckPoint(1, 1, 0) and
         CheckPoint(1, 1, 1) then
        Exit(false);
    end;
    Result := true;
  end;

  function CalculateZFail: boolean;

    { Returns if SceneBox is (at least partially)
      inside the Plane (i.e. where the plane equation is <= 0).
      Also returns @true if Plane is invalid, since in this case result
      of CalculateZFail should depend on other planes. }
    function InsidePlane(const Plane: TVector4Double): boolean;

      function CalculatePoint(const X, Y, Z: Integer): Single;
      begin
        Result := Plane[0] * SceneBox[X][0] +
                  Plane[1] * SceneBox[Y][1] +
                  Plane[2] * SceneBox[Z][2] +
                  Plane[3];
      end;

    begin
      Result :=
        ( (Plane[0] = 0) and (Plane[1] = 0) and (Plane[2] = 0) ) or
        (CalculatePoint(0, 0, 0) <= 0) or
        (CalculatePoint(0, 0, 1) <= 0) or
        (CalculatePoint(0, 1, 0) <= 0) or
        (CalculatePoint(0, 1, 1) <= 0) or
        (CalculatePoint(1, 0, 0) <= 0) or
        (CalculatePoint(1, 0, 1) <= 0) or
        (CalculatePoint(1, 1, 0) <= 0) or
        (CalculatePoint(1, 1, 1) <= 0);
    end;

  var
    LightPosition3: TVector3Double absolute FLightPositionDouble;
    NearPlane: TVector4Double;
  begin
    if LightPosition[3] <> 0 then
    begin
      { Idea: calculate a pyramid between light position and near plane rectangle
        of the frustum. Assuming light point is positional and it does not
        lie on the near plane, this is simple: such pyramid has 4 side planes
        (created by two succeding near plane rectangle points and light pos),
        and 1 additional plane for near plane.

        Now, if for any such plane, SceneBox is outside, then ZFail is for sure
        not needed.

        Actually, even when the light lies exactly on near rectangle plane,
        usually this is still OK. The trouble will occur only if the light lies
        exactly on one of near rectangle points, since then an invalid
        plane (with 1st three items = 0) will be calculated. In such case,
        at most two planes out of 5 will be invalid (we assume that all 4 near
        rectangle points are for sure different, so the light pos may collide
        with only 1 of them, so only two plane calculations will lead to
        invalid plane). In such case, it's OK to simply ignore invalid planes.

        That's why InsidePlane simply checks for and ignores invalid planes.
      }

      { FrustumNearPoints meaning:
        0: left , top
        1: right, top
        2: right, bottom
        3: left , bottom }

      NearPlane := TrianglePlane(
        FrustumNearPoints[2], FrustumNearPoints[1], FrustumNearPoints[0]);

      { Now NearPlane points CCW outside of the frustum, but this is not
        necessarily what we want. We want NearPlane to point CCW outside
        from light+near plane pyramid. In other words, LightPosition should be
        on CW side of this plane. If LightPosition is on CCW side,
        flip NearPlane. Also, calculations of other side planes should
        generate flipped planes. }

      if (NearPlane[0] * LightPositionDouble[0] +
          NearPlane[1] * LightPositionDouble[1] +
          NearPlane[2] * LightPositionDouble[2] +
          NearPlane[3] * LightPositionDouble[3]) > 0 then
      begin
        VectorNegateTo1st(NearPlane);
        Result :=
          InsidePlane(NearPlane) and
          InsidePlane(TrianglePlane(FrustumNearPoints[1], FrustumNearPoints[0], LightPosition3)) and
          InsidePlane(TrianglePlane(FrustumNearPoints[2], FrustumNearPoints[1], LightPosition3)) and
          InsidePlane(TrianglePlane(FrustumNearPoints[3], FrustumNearPoints[2], LightPosition3)) and
          InsidePlane(TrianglePlane(FrustumNearPoints[0], FrustumNearPoints[3], LightPosition3));
      end else
      begin
        Result :=
          InsidePlane(NearPlane) and
          InsidePlane(TrianglePlane(FrustumNearPoints[0], FrustumNearPoints[1], LightPosition3)) and
          InsidePlane(TrianglePlane(FrustumNearPoints[1], FrustumNearPoints[2], LightPosition3)) and
          InsidePlane(TrianglePlane(FrustumNearPoints[2], FrustumNearPoints[3], LightPosition3)) and
          InsidePlane(TrianglePlane(FrustumNearPoints[3], FrustumNearPoints[0], LightPosition3));
      end;
    end else
    begin
      { For directional light, this is somewhat similar to positional lights,
        except that you have 4 planes (each one from a segment of near rectangle,
        extruded to infinity in both directions).

        The corner cases may occur when light direction is the same as direction
        of one of the segments. This will make at most 2 such planes invalid
        (as 2 pairs of segments have different direction than other 2 pairs
        of near rectangle segments). So we simply ignore such invald planes,
        so again we can use InsidePlane function. }

      { Although NearPlane for directional lights is not a plane that delimits
        the pyramid, we still calculate NearPlane to decide in which direction
        our 4 planes should be calculated, so that they point CCW outside. }
      NearPlane := TrianglePlane(
        FrustumNearPoints[2], FrustumNearPoints[1], FrustumNearPoints[0]);

      if (NearPlane[0] * LightPositionDouble[0] +
          NearPlane[1] * LightPositionDouble[1] +
          NearPlane[2] * LightPositionDouble[2]) > 0 then
      begin
        Result :=
          InsidePlane(TrianglePlane(FrustumNearPoints[0], FrustumNearPoints[1], VectorAdd(FrustumNearPoints[0], LightPosition3))) and
          InsidePlane(TrianglePlane(FrustumNearPoints[1], FrustumNearPoints[2], VectorAdd(FrustumNearPoints[1], LightPosition3))) and
          InsidePlane(TrianglePlane(FrustumNearPoints[2], FrustumNearPoints[3], VectorAdd(FrustumNearPoints[2], LightPosition3))) and
          InsidePlane(TrianglePlane(FrustumNearPoints[3], FrustumNearPoints[0], VectorAdd(FrustumNearPoints[3], LightPosition3)));
      end else
      begin
        Result :=
          InsidePlane(TrianglePlane(FrustumNearPoints[1], FrustumNearPoints[0], VectorAdd(FrustumNearPoints[1], LightPosition3))) and
          InsidePlane(TrianglePlane(FrustumNearPoints[2], FrustumNearPoints[1], VectorAdd(FrustumNearPoints[2], LightPosition3))) and
          InsidePlane(TrianglePlane(FrustumNearPoints[3], FrustumNearPoints[2], VectorAdd(FrustumNearPoints[3], LightPosition3))) and
          InsidePlane(TrianglePlane(FrustumNearPoints[0], FrustumNearPoints[3], VectorAdd(FrustumNearPoints[0], LightPosition3)));
      end;
    end;
  end;

begin
  { Frustum culling for shadow volumes.
    If the SceneBox is outside of convex hull light pos + frustum,
    the shadow of this scene is not visible for sure. }
  FSceneShadowPossiblyVisible := CalculateShadowPossiblyVisible(SceneBox);

  FZFail := FSceneShadowPossiblyVisible and CalculateZFail;

  FZFailAndLightCap := ZFail and
    FrustumBox3dCollisionPossibleSimple(FFrustum, SceneBox);

  UpdateCount;
end;

procedure TShadowVolumesHelper.UpdateCount;
begin
  { update counters }
  if Count then
  begin
    Inc(FCountScenes);

    if FSceneShadowPossiblyVisible then
    begin
      if ZFail then
      begin
        if ZFailAndLightCap then
          Inc(FCountZFailAndLightCap) else
          Inc(FCountZFailNoLightCap);
      end else
        Inc(FCountZPass);
    end else
      Inc(FCountShadowsNotVisible);
  end;
end;

procedure TShadowVolumesHelper.InitSceneOnlySetupStencil;

  procedure ActuallySetStencilConfiguration;

    { These set glStencilOpSeparate (suitable for both front and
      back faces) or glStencil (suitable only for front or only for back faces)
      as appropriate.

      Use this before rendering shadow volumes.
      It uses ZFail to decide what setup is necessary.
      It also uses StencilOpIncrWrap / StencilOpDecrWrap as needed. }

    procedure SetStencilOpSeparate;
    begin
      if ZFail then
      begin
        glStencilOpSeparate(GL_FRONT, GL_KEEP, StencilOpDecrWrap, GL_KEEP);
        glStencilOpSeparate(GL_BACK , GL_KEEP, StencilOpIncrWrap, GL_KEEP);
      end else
      begin
        glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, StencilOpIncrWrap);
        glStencilOpSeparate(GL_BACK , GL_KEEP, GL_KEEP, StencilOpDecrWrap);
      end;
    end;

    procedure SetStencilOpForFront;
    begin
      if ZFail then
        glStencilOp(GL_KEEP, StencilOpDecrWrap, GL_KEEP) else
        { For each fragment that passes depth-test, *increase* it's stencil value. }
        glStencilOp(GL_KEEP, GL_KEEP, StencilOpIncrWrap);
    end;

    procedure SetStencilOpForBack;
    begin
      if ZFail then
        glStencilOp(GL_KEEP, StencilOpIncrWrap, GL_KEEP) else
        { For each fragment that passes depth-test, *decrease* it's stencil value. }
        glStencilOp(GL_KEEP, GL_KEEP, StencilOpDecrWrap);
    end;

  begin
    case StencilSetupKind of
      ssSeparate: SetStencilOpSeparate;
      ssForFront: SetStencilOpForFront;
      ssForBack: SetStencilOpForBack;
      else raise EInternalError.Create('shadowvolumeshelper.pas 456');
    end;
  end;

begin
  if FSceneShadowPossiblyVisible then
  begin
    { setup stencil configuration. To avoid state too often changes,
      use StencilConfigurationKnown. }
    if (not StencilConfigurationKnown) or
       (StencilConfigurationKnownZFail <> ZFail) or
       (StencilConfigurationKnownKind <> StencilSetupKind) then
    begin
      ActuallySetStencilConfiguration;
      StencilConfigurationKnown := true;
      StencilConfigurationKnownKind := StencilSetupKind;
      StencilConfigurationKnownZFail := ZFail;
    end;
      { else, if configuration known and equals current, nothing to do }
  end;
end;

procedure TShadowVolumesHelper.InitSceneAlwaysVisible;
begin
  FSceneShadowPossiblyVisible := true;
  FZFail := true;
  FZFailAndLightCap := true;
  UpdateCount;
  InitSceneOnlySetupStencil;
end;

end.
