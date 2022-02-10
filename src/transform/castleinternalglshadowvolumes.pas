{
  Copyright 2007-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering shadow volumes in OpenGL (TGLShadowVolumeRenderer). }
unit CastleInternalGLShadowVolumes;

{$I castleconf.inc}

interface

uses
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleTransform, CastleVectors, CastleBoxes, CastleGLUtils, CastleFrustum;

type
  TStencilSetupKind = (ssFrontAndBack, ssFront, ssBack);

  TGLShadowVolumeRenderer = class;

  TSVRenderParamsProc = procedure (const Params: TRenderParams) of object;
  TSVRenderProc = procedure of object;

  { Shadow volume rendering in OpenGL.
    This class provides various utilities related to shadow volume rendering.
    It provides everything, except it doesn't
    actually render the 3D models or their shadow volumes (actual rendering
    is provided by T3D descendants, like TCastleScene.RenderShadowVolume).

    For general usage tutorial of this class,
    see [https://castle-engine.io/vrml_engine_doc/output/xsl/html/chapter.shadows.html] }
  TGLShadowVolumeRenderer = class(TBaseShadowVolumeRenderer)
  private
    FrustumAndLightPlanes: array [0..5] of TVector4;
    FrustumAndLightPlanesCount: Cardinal;
    FFrustum: TFrustum;
    FrustumNearPoints: TFrustumPoints;

    FWrapAvailable: boolean;
    FStencilOpIncrWrap, FStencilOpDecrWrap: TGLenum;

    { These will ideally be initialized to GL_INCR/DECR_WRAP (available
      in OpenGL >= 2.0) or GL_INCR/DECR_WRAP_EXT (available if EXT_stencil_wrap).
      Actually values with and without _EXT are the same.

      If OpenGL will not have these available, then they will be equal to
      old GL_INCR/DECT constants (without wrapping).

      These are set by GLContextOpen. WrapAvailable says whether
      they were set to _WRAP_ versions.

      @groupBegin }
    property StencilOpIncrWrap: TGLenum read FStencilOpIncrWrap;
    property StencilOpDecrWrap: TGLenum read FStencilOpDecrWrap;
    { @groupEnd }
  private
    FCasterShadowPossiblyVisible: boolean;
    FZFail: boolean;
    FZFailAndLightCap: boolean;

    FLightPosition: TVector4;

    StencilConfigurationKnown: boolean;
    StencilConfigurationKnownKind: TStencilSetupKind;
    StencilConfigurationKnownZFail: boolean;

    FStencilSetupKind: TStencilSetupKind;

    FCount: boolean;
    FCountCasters: Cardinal;
    FCountShadowsNotVisible: Cardinal;
    FCountZPass: Cardinal;
    FCountZFailNoLightCap: Cardinal;
    FCountZFailAndLightCap: Cardinal;

    FStencilTwoSided: boolean;

    procedure UpdateCount;
  public
    constructor Create;

    property WrapAvailable: boolean read FWrapAvailable;

    { Call this when OpenGL context is initialized, this will set some things.
      For now, this sets StencilOpIncrWrap, StencilOpDecrWrap. }
    procedure GLContextOpen;

    { Call this when camera frustum is known and light position (of the shadow
      casting light) is known, typically at the beginning of your drawing routine.
      You have to call this before InitCaster.

      This prepares some things (so that each InitCaster call doesn't have to) and
      all subsequent InitCaster calls assume that Frustum and
      LightPosition are the same.

      It also resets CountCasters etc. counters for debug purposes. }
    procedure InitFrustumAndLight(
      const Frustum: TFrustum;
      const ALightPosition: TVector4);

    { Light casting shadows position, initialized by InitFrustumAndLight. }
    property LightPosition: TVector4 read FLightPosition;

    { Call this when the bounding box of shadow caster is known.

      This calculates various things related to shadow volumes rendering
      of this scene.  1. checks whether you need to render shadow of the
      object inside CasterBox, settting CasterShadowPossiblyVisible.
      2. checks whether ZFail method is needed, setting ZFail.

      This assumes that Frustum and LightPosition values given
      in InitFrustumAndLight are OK.

      Also note that after InitFrustumAndLight, all InitCaster will assume that
      they have complete control over glStencilOp states for the time
      of rendering shadow volumes. In other words: InitCaster will setup
      some stencil configuration, depending on ZFail state and StencilSetupKind.
      For the sake of speed, we try to avoid setting the same state
      twice, so we optimize it: first InitCaster after InitFrustumAndLight
      does always setup stencil confguration. Following InitCaster will
      only change stencil configuration if ZFail value will actually change.
      This means that if e.g. all objects will be detected to be in z-pass
      method, then stencil configuration will be done only once.

      The above optimization works OK if you do not change StencilOp configuration
      yourself during SV rendering. }
    procedure InitCaster(const CasterBox: TBox3D);

    { Does the shadow need to be rendered, calculated by last InitCaster call. }
    property CasterShadowPossiblyVisible: boolean read FCasterShadowPossiblyVisible;

    { Is the ZFail method needed, set by InitCaster. }
    property ZFail: boolean read FZFail;

    { Is the ZFail with light caps method needed, set by InitCaster. }
    property ZFailAndLightCap: boolean read FZFailAndLightCap;

    { Is two-sided stencil test (that allows you to make SV in a single pass)
      available.

      This is initialized by GLContextOpen, and is true if OpenGL provides
      one of:
      @unorderedList(
        @item(glStencilOpSeparate (in OpenGL >= 2.0))
        @item(GL_ATI_separate_stencil extension, glStencilOpSeparateATI)
        @item(We could also handle GL_EXT_stencil_two_side extension, glActiveStencilFaceEXT.
          But, since OpenGL >= 2.0 is now common, don't try.)
      ) }
    property StencilTwoSided: boolean read FStencilTwoSided;

    { What kind of stencil settings should be set by InitCaster.

      Set ssFrontAndBack only if StencilTwoSided is @true.
      Otherwise, you have to use 2-pass method (render everything
      2 times, culling front one time, culling back second time) ---
      in this case use ssFront or ssBack as appropriate. }
    property StencilSetupKind: TStencilSetupKind
      read FStencilSetupKind  write FStencilSetupKind
      default ssFrontAndBack;

    { Statistics of shadow volumes. They are enabled by default,
      as calculating them takes practically no time.
      @groupBegin }
    property Count: boolean read FCount write FCount default true;
    property CountCasters: Cardinal read FCountCasters;
    property CountShadowsNotVisible: Cardinal read FCountShadowsNotVisible;
    property CountZPass: Cardinal read FCountZPass;
    property CountZFailNoLightCap: Cardinal read FCountZFailNoLightCap;
    property CountZFailAndLightCap: Cardinal read FCountZFailAndLightCap;
    { @groupEnd }

    { Do actual rendering with shadow volumes.

      You have to provide the appropriate callbacks that render given
      scene parts.

      Params.Transparent and Params.ShadowVolumesReceivers and Params.InShadow
      are changed here (their previous values are ignored).
      They cannot be modified by our callbacks.

      Render3D renders part of the scene.

      @unorderedList(
        @item(
          When Params.ShadowVolumesReceivers includes @true, renders things that
          may be in the shadow.
          You should use Params.InShadow to either display the version
          of the scene in the shadows (so probably darker, probably with some
          lights off) or the version that is currently lighted
          (probably brighter, with normal scene lights on).)

        @item(
          When Params.ShadowVolumesReceivers includes @true, renders things that
          must never be considered in shadow (are not shadow receivers).
          Params.InShadow is always @false when Params.ShadowVolumesReceivers is only [@false]
          (to render only stuff that is never in shadow).)
      )

      Render3D must also honour Params.Transparent,
      rendering only opaque or only transparent parts.
      For Transparent = @true, always Params.InShadow = @false.
      Shadow volumes simply don't allow transparent object
      to function properly as shadow receivers.
      Reading [http://developer.nvidia.com/object/fast_shadow_volumes.html]
      notes: they also just do separate rendering pass to render the
      partially-transparent parts, IOW they also note that transparent parts
      simply don't work at all with shadow volumes.

      RenderShadowVolumes renders shadow volumes from shadow casters.

      If DrawShadowVolumes then shadow volumes will be also actually drawn
      to color buffer (as yellow blended polygons), this is useful for debugging
      shadow volumes. }
    procedure Render(
      const Params: TRenderParams;
      const Render3D: TSVRenderParamsProc;
      const RenderShadowVolumes: TSVRenderProc;
      const DrawShadowVolumes: boolean);
  end;

implementation

uses SysUtils,
  CastleUtils, CastleStringUtils, CastleLog, CastleGLVersion,
  CastleTriangles, CastleRenderOptions, CastleRenderContext;

constructor TGLShadowVolumeRenderer.Create;
begin
  inherited;
  FCount := true;
end;

procedure TGLShadowVolumeRenderer.GLContextOpen;
begin
  { calcualte WrapAvailable, StencilOpIncrWrap, StencilOpDecrWrap }
  {$ifdef OpenGLES}
  FWrapAvailable := true;
  FStencilOpIncrWrap := GL_INCR_WRAP;
  FStencilOpDecrWrap := GL_DECR_WRAP;
  {$else}
  FWrapAvailable := GLFeatures.Version_2_0 or Load_GL_EXT_stencil_wrap;
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

  { This again looks hacky but is Ok, glStencilOpSeparateATI has the same
    call semantics as glStencilOpSeparate, in fact glStencilOpSeparate
    is just an extension promoted to standard in GL 2.0... }
  if (not Assigned(glStencilOpSeparate)) and Load_GL_ATI_separate_stencil then
  begin
    if LogShadowVolumes then
      WritelnLog('Shadow volumes',
        'Real glStencilOpSeparate not available, ' +
        'but faking it by glStencilOpSeparateATI (since ' +
        'GL_ATI_separate_stencil available)');
    glStencilOpSeparate := glStencilOpSeparateATI;
  end;
  {$endif}

  // In case of Nintendo Switch, glStencilOpSeparate isn't a function pointer
  FStencilTwoSided := {$ifdef CASTLE_NINTENDO_SWITCH} true {$else} {$ifndef FPC}@{$endif}glStencilOpSeparate <> nil {$endif};

  if LogShadowVolumes then
    WritelnLogMultiline('Shadow volumes',
      Format('GL_INCR/DECR_WRAP_EXT available: %s' + nl +
             'Two-sided stencil test available: %s',
            [ BoolToStr(WrapAvailable, true),
              BoolToStr(StencilTwoSided, true) ]));
end;

procedure TGLShadowVolumeRenderer.InitFrustumAndLight(
  const Frustum: TFrustum;
  const ALightPosition: TVector4);

  procedure CalculateFrustumAndLightPlanes;
  var
    FP, LastPlane: TFrustumPlane;
  begin
    FrustumAndLightPlanesCount := 0;

    LastPlane := High(FP);
    Assert(LastPlane = fpFar);

    { if infinite far plane, then ignore it }
    if Frustum.ZFarInfinity then
      LastPlane := Pred(LastPlane);

    for FP := Low(FP) to LastPlane do
    begin
      { This checks that LightPosition is inside Frustum.Planes[FP] plane.

        When LightPosition[3] = 1, this is normal test on which side
        of plane lies a point, so then it's OK (frustum planes point inside
        the frustum). For LightPosition[3] > 0 this is also  equivalent.

        For LightPosition[3] = 0 (directional light), this check dot product
        between light direction and plane direction. So >= 0 means that they
        point in the same dir (angle < 90 degs), so the light position
        in infinity can also be considered inside this plane. }
      if Frustum.Planes[FP][0] * LightPosition[0] +
         Frustum.Planes[FP][1] * LightPosition[1] +
         Frustum.Planes[FP][2] * LightPosition[2] +
         Frustum.Planes[FP][3] * LightPosition[3] >= 0 then
      begin
        FrustumAndLightPlanes[FrustumAndLightPlanesCount] := Frustum.Planes[FP];
        Inc(FrustumAndLightPlanesCount);
      end;
    end;

    { TODO: a better convex hull between light position and frustum could
      be useful. We should add additional planes to FrustumAndLightPlanes
      for this. Some pointers on [http://www.terathon.com/gdc06_lengyel.ppt]. }
  end;

var
  ALightPosition3: TVector3 absolute ALightPosition;
begin
  FFrustum := Frustum;
  FLightPosition := ALightPosition;

  Frustum.CalculatePoints(FrustumNearPoints);

  CalculateFrustumAndLightPlanes;

  StencilConfigurationKnown := false;

  FCountCasters := 0;
  FCountShadowsNotVisible := 0;
  FCountZPass := 0;
  FCountZFailNoLightCap := 0;
  FCountZFailAndLightCap := 0;
end;

procedure TGLShadowVolumeRenderer.InitCaster(const CasterBox: TBox3D);

  procedure DontSetupStencil(const CasterBox: TBox3D);

    function CalculateShadowPossiblyVisible(const CasterBox: TBox3D): boolean;
    var
      I: Integer;

      function CheckPoint(const X, Y, Z: Integer): boolean;
      begin
        Result :=
          CasterBox.Data[X][0] * FrustumAndLightPlanes[I][0] +
          CasterBox.Data[Y][1] * FrustumAndLightPlanes[I][1] +
          CasterBox.Data[Z][2] * FrustumAndLightPlanes[I][2] +
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

      { Returns if CasterBox is (at least partially)
        inside the Plane (i.e. where the plane equation is <= 0).
        Also returns @true if Plane is invalid, since in this case result
        of CalculateZFail should depend on other planes. }
      function InsidePlane(const Plane: TVector4): boolean;

        function CalculatePoint(const X, Y, Z: Integer): Single;
        begin
          Result := Plane[0] * CasterBox.Data[X][0] +
                    Plane[1] * CasterBox.Data[Y][1] +
                    Plane[2] * CasterBox.Data[Z][2] +
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
      LightPosition3: PVector3;
      NearPlane: TVector4;
    begin
      LightPosition3 := @FLightPosition;
      if LightPosition[3] <> 0 then
      begin
        { Idea: calculate a pyramid between light position and near plane rectangle
          of the frustum. Assuming light point is positional and it does not
          lie on the near plane, this is simple: such pyramid has 4 side planes
          (created by two succeding near plane rectangle points and light pos),
          and 1 additional plane for near plane.

          Now, if for any such plane, CasterBox is outside, then ZFail is for sure
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
          FrustumNearPoints[2].XYZ, FrustumNearPoints[1].XYZ, FrustumNearPoints[0].XYZ);

        { Now NearPlane points CCW outside of the frustum, but this is not
          necessarily what we want. We want NearPlane to point CCW outside
          from light+near plane pyramid. In other words, LightPosition should be
          on CW side of this plane. If LightPosition is on CCW side,
          flip NearPlane. Also, calculations of other side planes should
          generate flipped planes. }

        if (NearPlane.X * LightPosition.X +
            NearPlane.Y * LightPosition.Y +
            NearPlane.Z * LightPosition.Z +
            NearPlane.W * LightPosition.W) > 0 then
        begin
          NearPlane := -NearPlane;
          Result :=
            InsidePlane(NearPlane) and
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[0].XYZ, LightPosition3^)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[1].XYZ, LightPosition3^)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[2].XYZ, LightPosition3^)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[3].XYZ, LightPosition3^));
        end else
        begin
          Result :=
            InsidePlane(NearPlane) and
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[1].XYZ, LightPosition3^)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[2].XYZ, LightPosition3^)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[3].XYZ, LightPosition3^)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[0].XYZ, LightPosition3^));
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
          FrustumNearPoints[2].XYZ, FrustumNearPoints[1].XYZ, FrustumNearPoints[0].XYZ);

        if (NearPlane.X * LightPosition.X +
            NearPlane.Y * LightPosition.Y +
            NearPlane.Z * LightPosition.Z) > 0 then
        begin
          Result :=
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[1].XYZ, (FrustumNearPoints[0].XYZ + LightPosition3^))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[2].XYZ, (FrustumNearPoints[1].XYZ + LightPosition3^))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[3].XYZ, (FrustumNearPoints[2].XYZ + LightPosition3^))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[0].XYZ, (FrustumNearPoints[3].XYZ + LightPosition3^)));
        end else
        begin
          Result :=
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[0].XYZ, (FrustumNearPoints[1].XYZ + LightPosition3^))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[1].XYZ, (FrustumNearPoints[2].XYZ + LightPosition3^))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[2].XYZ, (FrustumNearPoints[3].XYZ + LightPosition3^))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[3].XYZ, (FrustumNearPoints[0].XYZ + LightPosition3^)));
        end;
      end;
    end;

  begin
    { Frustum culling for shadow volumes.
      If the CasterBox is outside of convex hull light pos + frustum,
      the shadow of this scene is not visible for sure. }
    FCasterShadowPossiblyVisible := CalculateShadowPossiblyVisible(CasterBox);

    FZFail := FCasterShadowPossiblyVisible and CalculateZFail;

    FZFailAndLightCap := ZFail and
      FFrustum.Box3DCollisionPossibleSimple(CasterBox);

    UpdateCount;
  end;

  procedure OnlySetupStencil;

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
        ssFrontAndBack: SetStencilOpSeparate;
        ssFront: SetStencilOpForFront;
        ssBack: SetStencilOpForBack;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('shadowvolumes.pas 456');
        {$endif}
      end;
    end;

  begin
    if FCasterShadowPossiblyVisible then
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

begin
  DontSetupStencil(CasterBox);
  OnlySetupStencil;
end;

procedure TGLShadowVolumeRenderer.UpdateCount;
begin
  { update counters }
  if Count then
  begin
    Inc(FCountCasters);

    if FCasterShadowPossiblyVisible then
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

procedure TGLShadowVolumeRenderer.Render(
  const Params: TRenderParams;
  const Render3D: TSVRenderParamsProc;
  const RenderShadowVolumes: TSVRenderProc;
  const DrawShadowVolumes: boolean);
{$ifdef OpenGLES}
begin
  // TODO-es
  Params.Transparent := false; Params.ShadowVolumesReceivers := [false, true]; Render3D(Params);
  Params.Transparent := true ; Params.ShadowVolumesReceivers := [false, true]; Render3D(Params);
{$else}

const
  { Which stencil bits should be tested when determining which things
    are in the scene ?

    Not only *while rendering* shadow quads but also *after this rendering*
    value in stencil buffer may be > 1 (so you need more than 1 bit
    to hold it in stencil buffer).

    Why ? It's obvious that *while rendering* (e.g. right after rendering
    all front quads) this value may be > 1. But when the point
    is in the shadow because it's inside more than one shadow
    (cast by 2 different shadow quads) then even *after rendering*
    this point will have value > 1.

    So it's important that this constant spans a couple of bits.
    More precisely, it should be the maximum number of possibly overlapping
    front shadow quads from any possible camera view. Practically speaking,
    it will always be too little (for complicated shadow casters),
    but stencil_wrap will hopefully in this case minimize artifacts. }
  StencilShadowBits = $FF;
var
  SavedCount: boolean;
  SavedDepthBufferUpdate: Boolean;
  SavedColorChannels: TColorChannels;
begin
  Params.InShadow := false;
  Params.Transparent := false;
  Params.ShadowVolumesReceivers := [false];
  Render3D(Params);

  Params.InShadow := true;
  Params.Transparent := false;
  Params.ShadowVolumesReceivers := [true];
  Render3D(Params);

  glEnable(GL_STENCIL_TEST);
    { Note that stencil buffer is set to all 0 now. }

    glPushAttrib(GL_ENABLE_BIT
      { saves Enable(GL_DEPTH_TEST), Enable(GL_CULL_FACE) });
      glEnable(GL_DEPTH_TEST);

      { Calculate shadows to the stencil buffer. }

      { Don't write anything to depth or color buffers. }
      SavedDepthBufferUpdate := RenderContext.DepthBufferUpdate;
      SavedColorChannels := RenderContext.ColorChannels;
      RenderContext.DepthBufferUpdate := false;
      RenderContext.ColorChannels := [];

        glStencilFunc(GL_ALWAYS, 0, 0);

        if StencilTwoSided then
        begin
          StencilSetupKind := ssFrontAndBack;
          RenderShadowVolumes;
        end else
        begin
          glEnable(GL_CULL_FACE);

          { Render front facing shadow shadow volume faces. }
          StencilSetupKind := ssFront;
          glCullFace(GL_BACK);
          RenderShadowVolumes;

          { Render back facing shadow shadow volume faces. }
          StencilSetupKind := ssBack;
          SavedCount := Count;
          Count := false;
          glCullFace(GL_FRONT);
          RenderShadowVolumes;
          Count := SavedCount;
        end;

      RenderContext.DepthBufferUpdate := SavedDepthBufferUpdate;
      RenderContext.ColorChannels := SavedColorChannels;
    glPopAttrib;
  glDisable(GL_STENCIL_TEST);

  { Now render everything once again, with lights turned on.
    But render only things not in shadow.

    ----------------------------------------
    Long (but maybe educational) explanation why glDepthFunc(GL_LEQUAL)
    below is crucial:

    What should I do with depth buffer ? Now it contains opaque
    never-shadowed things, and all shadowed things.
    At some time (see revision 2048 in Kambi SVN repository on kocury) I called
    glClear(GL_DEPTH_BUFFER_BIT) before rendering shadowed things for the
    second time. While this seems to work, the 2nd pass leaves the depth-buffer
    untouched on shadowed places. So it creates a whole
    lot of problems for rendering transparent things at the end:
    1. I have to render them before glClear(GL_DEPTH_BUFFER_BIT),
       since they can be occluded by any level parts (shadowed on not).
    2. I have to render them once again (but only into depth buffer,
       i.e. with temporary
       glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE))
       after glClear(GL_DEPTH_BUFFER_BIT), since they can occlude
       non-shadowed level parts.

    This is easy doable for opaque parts. But what about transparent
    things? In other words, where should we call
    Render3D(Transparent=true, ShadowVolumesReceivers=false)
    and Render3D(Transparent=true, InShadow=false, ShadowVolumesReceivers=true)?
    They should be rendered but they don't affect depth buffer.
    Well, clearly, they have to be rendered
    before glClear(GL_DEPTH_BUFFER_BIT) (for the same reason that
    opaque parts must: because they can be occluded by any level
    part, shadowed or not). But rendering non-shadowed parts may then
    cover them (since they cannot be rendered to depth buffer).
    So I should render them once again, but *only at the places
    where Render_Shadowed_Lights below passed the stencil test (=0) and the depth test*.
    But the second condition is not so easy (I would have to change stencil
    buffer once again, based on Level.Render hits).

    The simpler version, to just render them second time everywhere where
    were not in shadow, i.e. stencil test (=0) passes would be no good:
    we would render transparent things twice in the places on the level
    when they are not in shadow.

    So basically, it's all doable, but not trivial, and (more important)
    I'm losing rendering time on handling this. And without taking proper care
    about transparent parts, transparent things
    may be e.g. visible through the walls,
    in the places where shadow falls on the wall.

    This was all talking assuming that we do glClear(GL_DEPTH_BUFFER_BIT)
    before the 2nd pass. But do we really have to ? No!
    It's enough to just set depth test GL_LEQUAL (instead of default
    GL_LESS) and then the 2nd pass will naturally cover the level
    rendered in the 1st pass. That's all, it's easy to implement,
    works perfectly, and is fast.

    End of long explanation about glDepthFunc(GL_LEQUAL).
    ----------------------------------------
  }

  { for now, InternalPass is only 0 or 1, and 1 is used only here }
  Assert(Params.InternalPass = 0);
  Inc(Params.InternalPass);

  glPushAttrib(GL_DEPTH_BUFFER_BIT { for glDepthFunc });
    glDepthFunc(GL_LEQUAL);

    { setup stencil : don't modify stencil, stencil test passes only for =0 }
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glStencilFunc(GL_EQUAL, 0, StencilShadowBits);
    glEnable(GL_STENCIL_TEST);
      Inc(Params.StencilTest);
      Params.InShadow := false;
      Params.Transparent := false;
      Params.ShadowVolumesReceivers := [true];
      Render3D(Params);
      Dec(Params.StencilTest);
    glDisable(GL_STENCIL_TEST);
  glPopAttrib();

  if DrawShadowVolumes then
  begin
    SavedCount := Count;
    Count := false;
    SavedDepthBufferUpdate := RenderContext.DepthBufferUpdate;
    RenderContext.DepthBufferUpdate := false;

    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
      glEnable(GL_DEPTH_TEST);
      glDisable(GL_LIGHTING);
      glColor4f(1, 1, 0, 0.3);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      RenderShadowVolumes;
    glPopAttrib;

    RenderContext.DepthBufferUpdate := SavedDepthBufferUpdate;
    Count := SavedCount;
  end;

  Params.InShadow := false;
  Params.Transparent := true;
  Params.ShadowVolumesReceivers := [true];
  Render3D(Params);

  Params.InShadow := false;
  Params.Transparent := true;
  Params.ShadowVolumesReceivers := [false];
  Render3D(Params);
{$endif}
end;

end.
