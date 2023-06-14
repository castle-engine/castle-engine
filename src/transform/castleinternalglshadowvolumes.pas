{
  Copyright 2007-2023 Michalis Kamburelis.

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
  CastleTransform, CastleVectors, CastleBoxes, CastleGLUtils, CastleFrustum,
  CastleRenderPrimitives;

type
  TGLShadowVolumeRenderer = class;

  TSVRenderProc = procedure (const Params: TRenderParams) of object;

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

    FCasterShadowPossiblyVisible: boolean;
    FZFail: boolean;
    FZFailAndLightCap: boolean;

    FLightPosition: TVector4;

    StencilConfigurationKnown: boolean;
    StencilConfigurationKnownZFail: boolean;

    FCount: boolean;
    FCountCasters: Cardinal;
    FCountShadowsNotVisible: Cardinal;
    FCountZPass: Cardinal;
    FCountZFailNoLightCap: Cardinal;
    FCountZFailAndLightCap: Cardinal;

    FMesh: TCastleRenderUnlitMesh;
    FDebugRender: Boolean;

    procedure UpdateCount;
    procedure SetDebugRender(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    { Create resources that require rendering (OpenGL) context, like @link(Mesh). }
    procedure PrepareRenderingResources;

    { Call this when camera frustum is known and light position (of the shadow
      casting light) is known, typically at the beginning of your drawing routine.

      You have to call this before any InitCaster / GetCasterShadowPossiblyVisible
      call. All subsequent InitCaster / GetCasterShadowPossiblyVisible calls
      assume that Frustum and LightPosition remain like this.

      It also resets CountCasters and other counters for debug purposes. }
    procedure InitFrustumAndLight(
      const Frustum: TFrustum;
      const ALightPosition: TVector4);

    { Light casting shadows position, initialized by InitFrustumAndLight. }
    property LightPosition: TVector4 read FLightPosition;

    { Call this when the bounding box of shadow caster is known.

      This calculates various things related to shadow volumes rendering
      of this scene.  1. checks whether you need to render shadow of the
      object inside CasterBox, setting CasterShadowPossiblyVisible.
      2. checks whether ZFail method is needed, setting ZFail.

      This assumes that Frustum and LightPosition values given
      in InitFrustumAndLight are OK.

      Also note that after InitFrustumAndLight, all InitCaster will assume that
      they have complete control over glStencilOp states for the time
      of rendering shadow volumes. In other words: InitCaster will setup
      some stencil configuration, depending on ZFail state.
      For the sake of speed, we try to avoid setting the same state
      twice, so we optimize it: first InitCaster after InitFrustumAndLight
      does always setup stencil confguration. Following InitCaster will
      only change stencil configuration if ZFail value will actually change.
      This means that if e.g. all objects will be detected to be in z-pass
      method, then stencil configuration will be done only once.

      The above optimization works OK if you do not change StencilOp configuration
      yourself during SV rendering. }
    procedure InitCaster(const CasterBox: TBox3D);

    { Check is shadow caster's shadow is possibly visible.
      Unlike InitCaster, this doesn't change any state,
      it doesn't set any OpenGL stuff,
      it doesn't update @link(CasterShadowPossiblyVisible) property. }
    function GetCasterShadowPossiblyVisible(const CasterBox: TBox3D): Boolean;

    { Does the shadow need to be rendered, calculated by last InitCaster call. }
    property CasterShadowPossiblyVisible: boolean read FCasterShadowPossiblyVisible;

    { Is the ZFail method needed, set by InitCaster. }
    property ZFail: boolean read FZFail;

    { Is the ZFail with light caps method needed, set by InitCaster. }
    property ZFailAndLightCap: boolean read FZFailAndLightCap;

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

      RenderOnePass renders part of the scene.

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

      RenderOnePass must also honour Params.Transparent,
      rendering only opaque or only transparent parts.
      For Transparent = @true, always Params.InShadow = @false.
      Shadow volumes simply don't allow transparent object
      to function properly as shadow receivers.
      Reading [http://developer.nvidia.com/object/fast_shadow_volumes.html]
      notes: they also just do separate rendering pass to render the
      partially-transparent parts, IOW they also note that transparent parts
      simply don't work at all with shadow volumes.

      RenderShadowVolumes renders shadow volumes from shadow casters. }
    procedure Render(
      const Params: TRenderParams;
      const RenderOnePass: TSVRenderProc;
      const RenderShadowVolumes: TSVRenderProc);

    { Use this to render shadow quads. }
    property Mesh: TCastleRenderUnlitMesh read FMesh;

    { Shadow volumes will be also actually drawn to color buffer
      (as yellow blended polygons), useful for debugging. }
    property DebugRender: Boolean read FDebugRender write SetDebugRender default false;
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

procedure TGLShadowVolumeRenderer.PrepareRenderingResources;
begin
  Assert(FMesh = nil, 'Call TGLShadowVolumeRenderer.PrepareRenderingResources only once');
  FMesh := TCastleRenderUnlitMesh.Create(DebugRender);
end;

destructor TGLShadowVolumeRenderer.Destroy;
begin
  FreeAndNil(FMesh);
  inherited;
end;

procedure TGLShadowVolumeRenderer.InitFrustumAndLight(
  const Frustum: TFrustum;
  const ALightPosition: TVector4);

  procedure CalculateFrustumAndLightPlanes;
  var
    FP, LastPlane: TFrustumPlane;
    LightPos: TVector4;
  begin
    FrustumAndLightPlanesCount := 0;

    LastPlane := High(FP);
    Assert(LastPlane = fpFar);

    { if infinite far plane, then ignore it }
    if Frustum.ZFarInfinity then
      LastPlane := Pred(LastPlane);

    LightPos := LightPosition;
    { For directional lights, as LightPos consider the light source
      being at infinity at the position *from which* the light emanates.
      We want to add to FrustumAndLightPlanes the planes the are further
      from this light. }
    if LightPos.W = 0 then
      LightPos := -LightPos;

    for FP := Low(FP) to LastPlane do
    begin
      { This checks that LightPos is inside Frustum.Planes[FP] plane.
        Remember that Frustum.Planes[FP] plane direction (XYZ) points inside
        the frustum.

        For positional lights (point, spot):
        LightPosition.W <> 0 (usually LightPosition.W = 1.0), and then
        this is normal test on which side of plane (Frustum.Planes[FP])
        lies a point in homogeneous coordinates (LightPos).

        For directional lights:
        LightPosition.W = 0, and then this is a dot product between
        plane direction (Frustum.Planes[FP]) and inverted light direction (LightPos,
        equal -LightPosition in this case). So we check if light source position
        is on the inside of the plane. }
      if TVector4.DotProduct(Frustum.Planes[FP], LightPos) >= 0 then
      begin
        FrustumAndLightPlanes[FrustumAndLightPlanesCount] := Frustum.Planes[FP];
        Inc(FrustumAndLightPlanesCount);
      end;
    end;

    { TODO: a better convex hull between light position and frustum could
      be useful. We should add additional planes to FrustumAndLightPlanes
      for this. Some pointers on [http://www.terathon.com/gdc06_lengyel.ppt]. }
  end;

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

function TGLShadowVolumeRenderer.GetCasterShadowPossiblyVisible(const CasterBox: TBox3D): Boolean;
var
  I: Integer;

  function CasterCornerOutsideFrustum(const X, Y, Z: Integer): boolean;
  begin
    Result :=
      CasterBox.Data[X][0] * FrustumAndLightPlanes[I][0] +
      CasterBox.Data[Y][1] * FrustumAndLightPlanes[I][1] +
      CasterBox.Data[Z][2] * FrustumAndLightPlanes[I][2] +
      FrustumAndLightPlanes[I][3] < 0;
  end;

begin
  if CasterBox.IsEmpty then
    Exit(false);
  for I := 0 to Integer(FrustumAndLightPlanesCount) - 1 do
  begin
    if CasterCornerOutsideFrustum(0, 0, 0) and
       CasterCornerOutsideFrustum(0, 0, 1) and
       CasterCornerOutsideFrustum(0, 1, 0) and
       CasterCornerOutsideFrustum(0, 1, 1) and
       CasterCornerOutsideFrustum(1, 0, 0) and
       CasterCornerOutsideFrustum(1, 0, 1) and
       CasterCornerOutsideFrustum(1, 1, 0) and
       CasterCornerOutsideFrustum(1, 1, 1) then
      Exit(false);
  end;
  Result := true;
end;

procedure TGLShadowVolumeRenderer.InitCaster(const CasterBox: TBox3D);

  procedure DontSetupStencil(const CasterBox: TBox3D);

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
      NearPlane: TVector4;
    begin
      if LightPosition.W <> 0 then
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

        if TVector4.DotProduct(NearPlane, LightPosition) > 0 then
        begin
          NearPlane := -NearPlane;
          Result :=
            InsidePlane(NearPlane) and
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[0].XYZ, LightPosition.XYZ)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[1].XYZ, LightPosition.XYZ)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[2].XYZ, LightPosition.XYZ)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[3].XYZ, LightPosition.XYZ));
        end else
        begin
          Result :=
            InsidePlane(NearPlane) and
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[1].XYZ, LightPosition.XYZ)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[2].XYZ, LightPosition.XYZ)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[3].XYZ, LightPosition.XYZ)) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[0].XYZ, LightPosition.XYZ));
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

        if TVector3.DotProduct(NearPlane.XYZ, LightPosition.XYZ) > 0 then
        begin
          Result :=
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[1].XYZ, (FrustumNearPoints[0].XYZ + LightPosition.XYZ))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[2].XYZ, (FrustumNearPoints[1].XYZ + LightPosition.XYZ))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[3].XYZ, (FrustumNearPoints[2].XYZ + LightPosition.XYZ))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[0].XYZ, (FrustumNearPoints[3].XYZ + LightPosition.XYZ)));
        end else
        begin
          Result :=
            InsidePlane(TrianglePlane(FrustumNearPoints[1].XYZ, FrustumNearPoints[0].XYZ, (FrustumNearPoints[1].XYZ + LightPosition.XYZ))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[2].XYZ, FrustumNearPoints[1].XYZ, (FrustumNearPoints[2].XYZ + LightPosition.XYZ))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[3].XYZ, FrustumNearPoints[2].XYZ, (FrustumNearPoints[3].XYZ + LightPosition.XYZ))) and
            InsidePlane(TrianglePlane(FrustumNearPoints[0].XYZ, FrustumNearPoints[3].XYZ, (FrustumNearPoints[0].XYZ + LightPosition.XYZ)));
        end;
      end;
    end;

  begin
    { Frustum culling for shadow volumes.
      If the CasterBox is outside of convex hull light pos + frustum,
      the shadow of this scene is not visible for sure. }
    FCasterShadowPossiblyVisible := GetCasterShadowPossiblyVisible(CasterBox);

    FZFail := FCasterShadowPossiblyVisible and CalculateZFail;

    FZFailAndLightCap := ZFail and
      FFrustum.Box3DCollisionPossibleSimple(CasterBox);

    UpdateCount;
  end;

  procedure OnlySetupStencil;

    { Set glStencilOpSeparate (suitable for both front and back faces) as appropriate.
      Use this before rendering shadow volumes.
      It uses ZFail to decide what setup is necessary. }
    procedure ActuallySetStencilConfiguration;
    begin
      if ZFail then
      begin
        glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_DECR_WRAP, GL_KEEP);
        glStencilOpSeparate(GL_BACK , GL_KEEP, GL_INCR_WRAP, GL_KEEP);
      end else
      begin
        glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR_WRAP);
        glStencilOpSeparate(GL_BACK , GL_KEEP, GL_KEEP, GL_DECR_WRAP);
      end;
    end;

  begin
    if FCasterShadowPossiblyVisible then
    begin
      { setup stencil configuration. To avoid state too often changes,
        use StencilConfigurationKnown. }
      if (not StencilConfigurationKnown) or
         (StencilConfigurationKnownZFail <> ZFail) then
      begin
        ActuallySetStencilConfiguration;
        StencilConfigurationKnown := true;
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
  const RenderOnePass: TSVRenderProc;
  const RenderShadowVolumes: TSVRenderProc);

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
  SavedDepthFunc: TDepthFunction;
  SavedCullFace, SavedDepthTest: Boolean;
begin
  Assert(GLFeatures.ShadowVolumesPossible);

  Params.InShadow := false;
  Params.Transparent := false;
  Params.ShadowVolumesReceivers := [false];
  RenderOnePass(Params);

  Params.InShadow := true;
  Params.Transparent := false;
  Params.ShadowVolumesReceivers := [true];
  RenderOnePass(Params);

  glEnable(GL_STENCIL_TEST);
    { Note that stencil buffer is set to all 0 now. }
    { Calculate shadows to the stencil buffer. }

    SavedCullFace := RenderContext.CullFace;
    SavedDepthTest := RenderContext.DepthTest;
    SavedDepthBufferUpdate := RenderContext.DepthBufferUpdate;
    SavedColorChannels := RenderContext.ColorChannels;

    RenderContext.DepthTest := true;
    { Don't write anything to depth or color buffers. }
    RenderContext.DepthBufferUpdate := false;
    RenderContext.ColorChannels := [];

      glStencilFunc(GL_ALWAYS, 0, 0);
      RenderShadowVolumes(Params);

    RenderContext.DepthBufferUpdate := SavedDepthBufferUpdate;
    RenderContext.ColorChannels := SavedColorChannels;
    RenderContext.CullFace := SavedCullFace;
    RenderContext.DepthTest := SavedDepthTest;
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
    RenderOnePass(Transparent=true, ShadowVolumesReceivers=false)
    and RenderOnePass(Transparent=true, InShadow=false, ShadowVolumesReceivers=true)?
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

  SavedDepthFunc := RenderContext.DepthFunc;
  RenderContext.DepthFunc := dfLessEqual;

    { setup stencil : don't modify stencil, stencil test passes only for =0 }
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glStencilFunc(GL_EQUAL, 0, StencilShadowBits);
    glEnable(GL_STENCIL_TEST);
      Inc(Params.StencilTest);
      Params.InShadow := false;
      Params.Transparent := false;
      Params.ShadowVolumesReceivers := [true];
      RenderOnePass(Params);
      Dec(Params.StencilTest);
    glDisable(GL_STENCIL_TEST);

  RenderContext.DepthFunc := SavedDepthFunc;

  if DebugRender then
  begin
    SavedCount := Count;
    Count := false;

    SavedDepthBufferUpdate := RenderContext.DepthBufferUpdate;
    SavedDepthTest := RenderContext.DepthTest;

    RenderContext.DepthBufferUpdate := false;
    RenderContext.DepthTest := true;
    RenderContext.BlendingEnable;

    RenderShadowVolumes(Params);

    RenderContext.BlendingDisable;
    RenderContext.DepthBufferUpdate := SavedDepthBufferUpdate;
    RenderContext.DepthTest := SavedDepthTest;

    Count := SavedCount;
  end;

  Params.InShadow := false;
  Params.Transparent := true;
  Params.ShadowVolumesReceivers := [true];
  RenderOnePass(Params);

  Params.InShadow := false;
  Params.Transparent := true;
  Params.ShadowVolumesReceivers := [false];
  RenderOnePass(Params);
end;

procedure TGLShadowVolumeRenderer.SetDebugRender(const Value: Boolean);
begin
  if FDebugRender <> Value then
  begin
    FDebugRender := Value;
    if (Mesh <> nil) and (Mesh.UseColor <> Value) then
    begin
      FreeAndNil(FMesh);
      FMesh := TCastleRenderUnlitMesh.Create(DebugRender);
      FMesh.Color := Vector4(1, 1, 0, 0.3);
    end;
  end;
end;

end.
