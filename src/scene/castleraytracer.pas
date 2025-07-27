{
  Copyright 2003-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering 3D models by ray-tracing (TClassicRayTracer, TPathTracer). }
unit CastleRayTracer;

{$I castleconf.inc}
{$I octreeconf.inc}

{ TODO:
  - for classic raytracer do shadow cache
  - for classic raytracer use various space filling curves
  - now that FPC inline is stable and cross-unit, use it to inline
    various things from CastleVectors. Check speed.
}

{ Define PATHTR_USES_SHADOW_CACHE to make path tracing use shadow cache.
  Speed gain is small (sometimes it's even a little worse with
  shadow cache (see ~/ownCloud/3dmodels/rayhunter-demos/raporty/shadow-cache)),
  but in general it's a good idea. For appropriate scenes, speed gain
  is more than 110%. }
{$define PATHTR_USES_SHADOW_CACHE}

interface

uses Classes,
  CastleVectors, CastleImages, CastleInternalRays, CastleProjection, CastleUtils,
  CastleInternalBaseTriangleOctree, CastleShapes, X3DNodes,
  CastleInternalSpaceFillingCurves, CastleTriangles, CastleSceneCore,
  CastleInternalTriangleOctree;

type
  { }
  TPixelsMadeNotifierFunc = procedure(PixelsMadeCount: Cardinal; Data: Pointer);

  TRayTracerKind = (rtkClassic, rtkPathTracer);

  TRayTracer = class
  strict protected
    procedure AppendStats(const Stats: TStrings; const RenderingTime: Single); virtual;
  public
    { Spatial structure (that contains geometry with materials) to render.
      You can create it using e.g. CreateOctreeVisibleTrianglesForScene
      for TCastleScene.
      Must be set before calling @link(Execute). }
    Octree: TBaseTrianglesOctree;

    { Image where the ray-tracer result will be stored.
      Must be set before calling @link(Execute).

      We will not resize given here Image. Instead we will use it's
      current size --- so you just have to set Image size as appropriate
      before calling this method.

      For every pixel, we calculate it's color and store it by
      @code(TCastleImage.Color[X, Y, 0] := xxx) method.
      We don't modify alpha channel of the image.

      Using TRGBFloatImage class is advised if you want the full color
      information. Otherwise color precision is lost beyond 8 bits, and values
      above 1.0 are clamped. }
    Image: TCastleImage;

    { Camera view.
      CamDirection and CamUp do not have to be normalized --- we will correct
      them here if needed.
      CamUp will be automatically corrected to be orthogonal to
      CamDirection if necessary, you only make sure it's not parallel
      to CamDirection. }
    CamPosition, CamDirection, CamUp: TVector3;

    { Camera projection properties. }
    Projection: TProjection;

    { Default background color, if scene doesn't have Background node with skyColor. }
    SceneBGColor: TVector3;

    { Scene Background node. }
    Background: TAbstractBackgroundNode;

    { Callback notified (if assigned) about writing each image pixel.
      This way you can display somewhere, or store to file, partially
      generated image. This callback gets information (in PixelsMadeCount)
      about how many pixels were generated (this includes also pixels skipped
      in case FirstPixel > 0).

      The pixels are written in the order of TSwapScanCurve for
      TClassicRayTracer, and in order dependent on TPathTracer.SFCurveClass
      for TPathTracer. When shadow cache will be implemented to TClassicRayTracer,
      then configurable SFCurveClass may be done also for TClassicRayTracer.

      Remember that pixels not done yet have the same content as they
      had when you @link(Execute) method started. In other words,
      if you set PixelsMadeNotifier <> nil, then often it's
      desirable to initialize Image content with some color (e.g. black)
      before calling @link(Execute). Otherwise at the time of @link(Execute)
      call, the pixels not done yet will have undefined colors. }
    PixelsMadeNotifier: TPixelsMadeNotifierFunc;
    PixelsMadeNotifierData: Pointer;

    { Initial pixel to start rendering from. By setting this to something > 0,
      you can (re-)start rendering from the middle. Useful to finish
      the job of a previous terminated ray-tracer process.

      Must be in [0 .. Image.Width * Image.Height] range.
      Setting to @code(Image.Width * Image.Height) makes the ray-tracer
      do nothing. }
    FirstPixel: Cardinal;

    { Do ray-tracing, writing a ray-traced image into the @link(Image). }
    procedure Execute; virtual; abstract;

    { Do ray-tracing, like @link(Execute),
      additionally gathering some statistics.
      The statistics will be added to the given string list. }
    procedure ExecuteStats(const Stats: TStrings);
  end;

  { Classic Whitted-style ray-tracer.
    See [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.classic_ray_tracer.html]
    for documentation.

    Make sure that VRML2Lights in states are properly initialized if you
    plan to render VRML 2.0 nodes. TCastleSceneCore and descendants do
    this for you automatically. }
  TClassicRayTracer = class(TRayTracer)
  protected
    procedure AppendStats(const Stats: TStrings; const RenderingTime: Single); override;
  public
    { Limit for recursion depth. 0 means that only primary rays will be cast,
      1 means that primary rays and 1 ray into mirror / transmitted / shadow,
      and so on. }
    InitialDepth: Cardinal;

    { Fog to render. Set FogNode <> @nil to render a fog,
      following VRML 2.0/X3D lighting equations.
      FogNode.TransformScale is used. }
    FogNode: TFogNode;

    { Lights shining on everything, like a headlight. }
    GlobalLights: TLightInstancesList;
    OwnsGlobalLights: boolean;

    property BaseLights: TLightInstancesList read GlobalLights write GlobalLights;
      {$ifdef FPC}deprecated 'use GlobalLights';{$endif}
    property OwnsBaseLights: Boolean read OwnsGlobalLights write OwnsGlobalLights;
      {$ifdef FPC}deprecated 'use OwnsGlobalLights';{$endif}

    procedure Execute; override;
    destructor Destroy; override;
  end;

  TFastPointerList = {$ifdef FPC}TFPList{$else}TList{$endif};

  { Path tracer. See
    [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.path_tracer.html]
    for documentation. }
  TPathTracer = class(TRayTracer)
  private
    CollectedLightItems: TFastPointerList;
    procedure CollectLightItems(const Triangle: PTriangle);
  protected
    procedure AppendStats(const Stats: TStrings; const RenderingTime: Single); override;
  public
    constructor Create;
    procedure Execute; override;
  public
    { MinDepth and RRoulContinue together determine the path length.
      The path has at least MinDepth length, and then Russian roulette
      is used.

      See [https://castle-engine.io/rayhunter.php]
      documentation about "<recursion-depth>" and @--r-roul-continue
      for suggestions about how to use these parameters.
      See also [https://castle-engine.io/raytr_gallery.php]
      for some experiments with these values.

      RRoulContinue must be in 0..1 range.

      You can give RRoulContinue = 0 if you don't want to use
      Russian roulette at all (works OK because our comparison
      @code(Random < RRoulContinue) uses "<", not "<=").
      Note that this causes bias (result is darker than it should be).
      Only RRoulContinue > 0 removes bias (the expected result is the correct
      one).

      Small RRoulContinue values cause a lot of noise.
      Large RRoulContinue values cause long rendering.

      MinDepth must be >= 0. You can use MinDepth = 0 to
      disable "minimal path length", and use Russian roulette always (noisy).
      @groupBegin }
    MinDepth: Integer;
    RRoulContinue: Single;
    { @groupEnd }

    { How many paths to use. Both must be > 0.

      PrimarySamplesCount tells how many paths are used for primary ray,
      and is really useful only for anti-aliasing. You can set this to
      a few. Values above ~10 are useless, they cause much longer rendering
      without really improving the result. You can set this to 1 if you
      don't need anti-aliasing.

      NonPrimarySamplesCount is the number of paths caused by each hit
      of a primary ray. This is the main quality control for the path-tracer,
      more paths mean that colors are gathered from more random samples,
      which means that final color is more accurate. In total you have
      pixels count * PrimarySamplesCount * NonPrimarySamplesCount,
      so beware when increasing this: you really have a lot paths. }
    PrimarySamplesCount, NonPrimarySamplesCount: Cardinal;

    { How many samples are used to calculate @italic(direct)
      illumination at every path point. These are rays sent into
      random points of random light sources, to test if given light
      shines here.

      Set this to 0 to have a really naive path-tracing, that wanders
      randomly hoping to hit light source by chance. This will usually
      need an enormous amount of PrimarySamplesCount * NonPrimarySamplesCount
      to given any sensible results.

      Set this to 1 or more for a normal path-tracer. }
    DirectIllumSamplesCount: Cardinal;

    { Order of pixels filled. In theory, something like THilbertCurve
      or TPeanoCurve could speed up rendering (because shadow cache is more
      utilized) compared to TSwapScanCurve. But in practice, right now
      this doesn't give any noticeable benefit. }
    SFCurveClass: TSpaceFillingCurveClass;
  end;

{ Create spatial structure to resolve collisions in the given scene.
  Caller is responsible for freeing the result. }
function CreateOctreeVisibleTrianglesForScene(
  const Scene: TCastleSceneCore): TTriangleOctree;

implementation

uses SysUtils, Math,
  CastleInternalSphereSampling, CastleTimeUtils, CastleColors, CastleRenderOptions;

{ TOctreeIgnoreForShadowRaysAndOneItem -------------------------------------- }

type
  { Simple utility class to easily ignore all transparent, non-shadow-casting
    triangles, and, additionally, one chosen triangle.
    Useful for TrianglesToIgnoreFunc parameters of various
    TBaseTrianglesOctree methods. }
  TOctreeIgnoreForShadowRaysAndOneItem = class
  public
    OneItem: PTriangle;
    function IgnoreItem(const Sender: TObject; const Triangle: PTriangle): boolean;
    constructor Create(AOneItem: PTriangle);
  end;

function TOctreeIgnoreForShadowRaysAndOneItem.IgnoreItem(
  const Sender: TObject; const Triangle: PTriangle): boolean;
begin
  Result := (Triangle = OneItem) or Triangle^.IgnoreForShadowRays;
end;

constructor TOctreeIgnoreForShadowRaysAndOneItem.Create(
  AOneItem: PTriangle);
begin
  inherited Create;
  OneItem := AOneItem;
end;

type
  TOctreeIgnoreForShadowRays = class
    { Ignore (return @true) transparent triangles
      (with Material.Transparency > 0) and non-shadow-casting triangles
      (with Appearance.shadowCaster = FALSE).

      This is suitable for TTriangleIgnoreFunc function, you can pass
      this to RayCollision and such. }
    class function IgnoreForShadowRays(
      const Sender: TObject; const Triangle: PTriangle): boolean;
  end;

class function TOctreeIgnoreForShadowRays.IgnoreForShadowRays(
  const Sender: TObject; const Triangle: PTriangle): boolean;
begin
  Result := Triangle^.IgnoreForShadowRays;
end;

{ Checks whether Light (point or directional) shines at scene point
  LightedPoint.

  "Lights at scene" means that the light is turned on
  (field "on" is @true) and between light source and a LightedPoint
  nothing blocks the light (we check it by querying collisions using
  the octree, ignoring transparent and non-shadow-casting triangles),
  and the light source is on the same side of LightedPointPlane as
  RenderDir.

  TriangleToIgnore and IgnoreMarginAtStart work just like for
  SegmentCollision. You should usually set TriangleToIgnore to the
  triangle containing your LightedPoint and IgnoreMarginAtStart to @true,
  to avoid detecting point as shadowing itself. }
function OctreeLightNotBlocked(const Octree: TBaseTrianglesOctree;
  const Light: TLightInstance;
  const LightedPoint, LightedPointPlane, RenderDir: TVector3;
  const TriangleToIgnore: PTriangle;
  const IgnoreMarginAtStart: boolean): boolean;
var
  LightPos: TVector3;
begin
  if not Light.Node.FdOn.Value then
    Exit(false);

  if Light.Node is TAbstractDirectionalLightNode then
    { Swiatlo directional oznacza ze swiatlo polozone jest tak bardzo
      daleko ze wszystkie promienie od swiatla sa rownolegle.

      Od pozycji LightedPoint odejmujemy wydluzone Direction swiatla.

      3 * Box3DMaxSize(Octree.TreeRoot.Box) na pewno jest odlegloscia
      ktora sprawi ze LightPos bedzie poza Octree.TreeRoot.Box
      (bo gdyby nawet Octree.TreeRoot.Box byl szescianem to jego przekatna
      ma dlugosc tylko Sqrt(2) * Sqrt(2) * Box3DMaxSize(Octree.TreeRoot.Box)
      (= 2 * Box3DMaxSize(Octree.TreeRoot.Box))
      W ten sposob otrzymujemy punkt ktory na pewno lezy POZA TreeRoot.Box
      i jezeli nic nie zaslania drogi od Point do tego punktu to
      znaczy ze swiatlo oswietla Intersection. }
    LightPos := LightedPoint -
      Light.Direction.AdjustToLength(3 * Octree.InternalTreeRoot.Box.MaxSize)
  else
    LightPos := Light.Location;

  Result :=
    VectorsSamePlaneDirections(
      LightPos - LightedPoint,
      RenderDir,
      LightedPointPlane) and
    (Octree.SegmentCollision(LightedPoint, LightPos, false, TriangleToIgnore, IgnoreMarginAtStart,
      {$ifdef FPC}@{$endif} TOctreeIgnoreForShadowRays {$ifdef FPC}(nil){$endif}.IgnoreForShadowRays) = nil);
end;

{ RayDirection calculations ----------------------------------------------------- }

{ Calculate the transmitted ray created by hitting a ray
  - with direction NormRayDirection (normalized ray direction is expected here)
  - from material with angle of refraction EtaFrom
  - transmitted into the material with angle of refraction EtaTo
  - hit occurs on the plane with normal vector (i.e. normalized) PlaneNormal }
function TryTransmittedRayDirection(
  out TransmittedRayDirection: TVector3;
  const NormRayDirection: TVector3;
  const PlaneNormal: TVector3;
  const EtaFrom, EtaTo: Single): boolean;
{ Written based on Foley, page 627 }
var
  EtaTransmission, RayIDotNormal, ToBeSqrRooted: Single;
  RayI: TVector3;
  { This is the Normal pointing in the direction from where the RayDirection came
    (i.e. in the opposite of RayDirection,
    i.e. -RayDirection (note the "-") and Normal must point to the same side
    of plane with PlaneNormal) }
  Normal: TVector3;
begin
  Normal := PlaneDirNotInDirection(PlaneNormal, NormRayDirection);
  RayI := -NormRayDirection;

  RayIDotNormal := TVector3.DotProduct(RayI, Normal);

  { EtaTransmission is the ratio between angles of refraction of materials
    that change when the transmitted ray enters to the other side
    of the plane. }
  EtaTransmission := EtaFrom / EtaTo;

  ToBeSqrRooted := 1 - Sqr(EtaTransmission) * (1 - Sqr(RayIDotNormal));

  Result := ToBeSqrRooted >= 0;
  if Result then
    TransmittedRayDirection :=
      (Normal * (EtaTransmission * RayIDotNormal - Sqrt(ToBeSqrRooted)))
      - (RayI * EtaTransmission);
end;

{ Calculate the perfect reflected vector.
  Arguments NormRayDirection and PlaneNormal like for TryTransmittedRayDirection. }
function ReflectedRayDirection(
  const NormRayDirection: TVector3;
  const PlaneNormal: TVector3): TVector3;
var
  Normal, NormNegatedRayDirection: TVector3;
begin
  { Calculate Normal like in TryTransmittedRayDirection. }
  Normal := PlaneDirNotInDirection(PlaneNormal, NormRayDirection);
  NormNegatedRayDirection := -NormRayDirection;

  { We calculate ray as mirror ray to NormNegatedRayDirection.
    Calculation is just like in Foley (page 601, section (14.16)). }
  Result := (Normal * 2 * TVector3.DotProduct(Normal, NormNegatedRayDirection))
    - NormNegatedRayDirection;
end;

function GetSceneBackgroundColor(const Background: TAbstractBackgroundNode;
  const Default: TCastleColorRGB): TCastleColorRGB;
begin
  if (Background is TAbstract3DBackgroundNode) and
     (TAbstract3DBackgroundNode(Background).FdSkyColor.Count <> 0) then
    Result := TAbstract3DBackgroundNode(Background).FdSkyColor.Items[0]
  else
    Result := Default;
end;

{ TRayTracer ----------------------------------------------------------------- }

procedure TRayTracer.AppendStats(const Stats: TStrings; const RenderingTime: Single);
begin
  Stats.Append(Format('Rendering done in %f seconds.', [RenderingTime]));
  Stats.Append(Format('%d simple collision tests done (one ray tested with one triangle).',
    [TriangleCollisionTestsCounter]));
end;

procedure TRayTracer.ExecuteStats(const Stats: TStrings);
var
  TimerBegin: TProcessTimerResult;
  RenderingTime: Single;
begin
  TimerBegin := ProcessTimer;
  TriangleCollisionTestsCounter := 0;

  Execute;

  RenderingTime := ProcessTimerSeconds(ProcessTimer, TimerBegin);
  AppendStats(Stats, RenderingTime);
end;

{ TClassicRayTracer ---------------------------------------------------------- }

procedure TClassicRayTracer.Execute;
var
  FogType: TFogTypeOrNone;
  SceneBackgroundColor: TCastleColorRGB;

  { Traces the ray with given Depth.
    Returns @false if the ray didn't hit anything, otherwise
    returns @true and sets Color. }
  function Trace(const RayOrigin, RayDirection: TVector3; const Depth: Cardinal;
    const TriangleToIgnore: PTriangle; IgnoreMarginAtStart: boolean): TCastleColorRGB;
  var
    Intersection: TVector3;
    IntersectNormal: TVector3;
    IntersectNode: PTriangle;
    MaterialTransparency: Single;
    MaterialReflectionColor: TVector3;

    procedure ModifyColorByTransmittedRay;
    var
      TransmittedColor, TransmittedRayVec: TVector3;
      EtaFrom, EtaTo: Single;
    const
      EtaConst = 1.3;
    begin
      { Make transmitted ray if Transparency > 0 and there
        is no total internal reflection
        [http://en.wikipedia.org/wiki/Total_internal_reflection]. }
      if MaterialTransparency > 0 then
      begin
        { TODO: we should get an information from our model here
          (from Octree IntersectNode item). But we don't have it for now.
          So for now we just always assume that the first transmission
          has Eta = EtaConst and the next one has 1/EtaConst
          and the next one has again EtaConst etc. }
        if Odd(InitialDepth - Depth) then
          begin EtaFrom := 1; EtaTo := EtaConst end else
          begin EtaFrom := EtaConst; EtaTo := 1 end;

        if TryTransmittedRayDirection(
          TransmittedRayVec, RayDirection.Normalize,
          IntersectNormal, EtaFrom, EtaTo) then
        begin
          TransmittedColor := Trace(Intersection, TransmittedRayVec,
            Depth - 1, IntersectNode, true);
          Result :=  Result * (1 - MaterialTransparency) +
            TransmittedColor * MaterialTransparency;
        end;
      end;
    end;

    procedure ModifyColorByReflectedRay;
    var
      ReflRayDirection, ReflColor: TVector3;
      MaterialReflection: Single;
    begin
      MaterialReflection := MaterialReflectionColor.Max;
      if MaterialReflection > 0 then
      begin
        ReflRayDirection := ReflectedRayDirection(RayDirection.Normalize,
          IntersectNormal);
        ReflColor := Trace(Intersection, ReflRayDirection, Depth - 1,
          IntersectNode, true);
        Result :=
          { Scale down original result, by "1 - MaterialReflection".
            Note: scaling down by "1 - MaterialReflectionColor" would be bad,
            makes the material look like the inversion of MaterialReflectionColor.
            E.g. for MaterialReflectionColor = blue, it will make the surface
            somewhat yellowish. }
          Result * (1 - MaterialReflection) +
          (ReflColor * MaterialReflectionColor);
      end;
    end;

    function LightNotBlocked(const Light: TLightInstance): boolean;
    begin
      { Does the light get to the current surface ?

        Note that we treat partially transparent objects here
        as not casting a shadow.
        This is better that not doing anything (partially transparent objects
        making normal "blocking" shadows looks bad), but it's not really correct,
        since in reality partially transparent objects just bend
        (or rather translate, if you consider a thin partially transparent object
        like a glass that doesn't intersect any other objects)
        the ray so it can get to the light.

        We also take into account here that things with
        Appearance.shadowCaster = FALSE do not block light.

        We also take into account that the light may be on the opposide
        side of the plane than from where RayDirection came.
        In such case the light shines on IntersectNode, but from the opposite
        side, so we will not add it here. }
      Result := OctreeLightNotBlocked(Octree, Light,
        Intersection, IntersectNormal,
        -RayDirection, IntersectNode, true);
    end;

    { Calculate emission color of given shape.

      This can be used by software renderers (ray-tracers etc.)
      to calculate pixel color following VRML/X3D specifications.
      Emission should be added to
      TLightInstance.Contribution (for each light),
      and resulting color should be processed by TFogNode.ApplyFog.

      When LightingCalculationOn = @false we actually take diffuseColor
      instead of emissiveColor. This is useful if you want to force
      the scene completely unlit, usually diffuseColor is more useful for this
      (since emissiveColor is often black on everything). }
    function Emission(const M: TMaterialInfo;
      const LightingCalculationOn: boolean): TVector3;
    begin
      if M <> nil then
      begin
        if LightingCalculationOn then
          Result := M.EmissiveColor
        else
          Result := M.MainColor;
      end else
      begin
        if LightingCalculationOn then
          { Default VRML 2.0 Material.emissiveColor }
          Result := TVector3.Zero
        else
          { Default VRML 2.0 Material.diffuseColor }
          Result := Vector3(0.8, 0.8, 0.8);
      end;
    end;

  var
    i: integer;
    Lights: TLightInstancesList;
    MaterialInfo: TMaterialInfo;
    DiffuseTextureColor: TCastleColorRGB;
  begin
    IntersectNode := Octree.RayCollision(Intersection, RayOrigin, RayDirection, true,
      TriangleToIgnore, IgnoreMarginAtStart, nil);
    if IntersectNode = nil then Exit(SceneBackgroundColor);

    IntersectNormal :=
      {$ifdef CONSERVE_TRIANGLE_MEMORY}
      IntersectNode^.SceneSpace.Plane.XYZ
      {$else}
      IntersectNode^.INormalCore(Intersection)
      {$endif};

    MaterialInfo := IntersectNode^.MaterialInfo;
    if MaterialInfo is TPhongMaterialInfo then // also checks MaterialInfo <> nil
    begin
      MaterialReflectionColor := TPhongMaterialInfo(MaterialInfo).ReflectionColor;
      MaterialTransparency := TPhongMaterialInfo(MaterialInfo).Transparency;
    end else
    begin
      MaterialReflectionColor := TPhongMaterialInfo.DefaultReflectionColor;
      MaterialTransparency := TPhongMaterialInfo.DefaultTransparency;
    end;

    DiffuseTextureColor :=
      {$ifdef CONSERVE_TRIANGLE_MEMORY}
      WhiteRGB
      {$else}
      IntersectNode^.State.MainTexture.Color(IntersectNode^.ITexCoord(Intersection)).XYZ
      {$endif};

    Result := Emission(IntersectNode^.State.MaterialInfo, InitialDepth <> 0);
    with IntersectNode^ do
    begin
      if Depth > 0 then
      begin
        Lights := State.Lights;
        if Lights <> nil then
          for i := 0 to Lights.Count - 1 do
            if LightNotBlocked(Lights.L[i]) then
              Result := Result + Lights.L[i].Contribution(Intersection,
                IntersectNormal, IntersectNode^.State, CamPosition, DiffuseTextureColor);

        { Add GlobalLights contribution, just like other lights.

          Note for LightNotBlocked testing: theoretically, just using
          LightNotBlocked always (no matter Depth/InitialDepth) should
          be fully correct. But:

          1. For Depth = InitialDepth, we know that headlight is not blocked
             (since a camera sees a point, so headlight on camera also sees it).
             Avoiding shadow ray test in this case is an optimization.

          2. For directional headlights, this somewhat workarounds a problem
             with our shadow rays. LightNotBlocked treats directional
             lights as coming from infinity. While directional headlight
             doesn't come from infinity: it comes from camera position.
             For example, imagine a player with directional headlight standing
             inside a closed cube (or any closed labirynth). LightNotBlocked
             would make the headlight not visible (as it's blocked from all
             sides by the cube), since LightNotBlocked doesn't know
             that light source is standing inside the cube...

             To fix this fully correctly, we should invent new light type,
             like "directional, but not from infinity, but from given plane",
             and modify LightNotBlocked accordingly.

             For now, treating Depth = InitialDepth as special case, fixes
             the problem at least for primary rays: directional light always
             reaches them.

          The above reasoning is nice as long as GlobalLights only contain
          the headlight. Which is true in the current uses. }
        for I := 0 to GlobalLights.Count - 1 do
          if (Depth = InitialDepth) or
             LightNotBlocked(GlobalLights.L[I]) then
            Result := Result + GlobalLights.L[I].Contribution(Intersection,
              IntersectNormal, IntersectNode^.State, CamPosition, DiffuseTextureColor);

        { Calculate recursively reflected and transmitted rays.
          Note that the order of calls (first reflected or first transmitted ?)
          is important --- as they may scale our Result (not only add
          to it). }
        ModifyColorByReflectedRay;
        ModifyColorByTransmittedRay;
      end;
    end;

    { Fog calculation should be done on every
      depth of ray-tracing, not only once (i.e. for primary rays).
      Reasoning: imagine that you look through a glass window
      (and you're really close to this window, so that fog doesn't
      affect the window glass noticeably) and through this window
      you see something distant, like a forest. The forest is distant
      so fog setting should affect it. Obviously even though the window
      glass wasn't affected much by the fog, you will see a forest
      covered by the fog. So each recursive call of Trace should bring
      a color affected by the fog. }
    FogNode.ApplyFog(Result, CamPosition, Intersection, FogType);
  end;

var
  RaysWindow: TRaysWindow;

  procedure DoPixel(const x, y: Cardinal);
  var
    RayOrigin, RayDirection: TVector3;
    C: TCastleColor;
    ColRGB: TCastleColorRGB absolute C;
  begin
    RaysWindow.PrimaryRay(x, y, Image.Width, Image.Height, RayOrigin, RayDirection);
    C := Image.Colors[X, Y, 0];
    ColRGB := Trace(RayOrigin, RayDirection, InitialDepth, nil, false);
    Image.Colors[X, Y, 0] := C;
  end;

var
  PixCoord: TVector2Cardinal;
  SFCurve: TSpaceFillingCurve;
begin
  FogType := FogNode.FogTypeOrNone;

  SceneBackgroundColor := GetSceneBackgroundColor(Background, SceneBGColor);

  RaysWindow := nil;
  SFCurve := nil;
  try
    RaysWindow := TRaysWindow.CreateDescendant(CamPosition,
      CamDirection.Normalize, CamUp.Normalize, Projection);

    { Using any other kind of space filling curve doesn't have any
      noticeable impact right now for classic ray tracer, since
      classic ray tracer doesn't use shadow cache or any other similar technique
      that benefits from processing similar cases in one block.
      In the future this make come to use.
      Right now, using SFCurve just makes implementing FirstPixel
      and PixelsMadeNotifier easier. }
    SFCurve := TSwapScanCurve.Create(Image.Width, Image.Height);
    SFCurve.SkipPixels(FirstPixel);

    { generate image pixels }
    if Assigned(PixelsMadeNotifier) then
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
        PixelsMadeNotifier(SFCurve.PixelsDone, PixelsMadeNotifierData);
      end;
    end else
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
      end;
    end;

  finally
    RaysWindow.Free;
    SFCurve.Free;
  end;
end;

procedure TClassicRayTracer.AppendStats(const Stats: TStrings; const RenderingTime: Single);
var
  PrimaryRaysCount: Cardinal;
begin
  inherited;
  PrimaryRaysCount := Image.Width * Image.Height - FirstPixel;
  Stats.Append(Format('Image size is %d x %d pixels (first %d pixels skipped) which gives %d primary rays.',
    [Image.Width, Image.Height, FirstPixel, PrimaryRaysCount]));
  Stats.Append(Format('%f primary rays done per second.',
    [PrimaryRaysCount / RenderingTime]));
  Stats.Append(Format('%f simple collision tests done per one primary ray.',
    [TriangleCollisionTestsCounter /  PrimaryRaysCount ]));
end;

destructor TClassicRayTracer.Destroy;
begin
  if OwnsGlobalLights then
    FreeAndNil(GlobalLights);
  inherited;
end;

{ TPathTracer -------------------------------------------------------------- }

constructor TPathTracer.Create;
begin
  inherited;
  SFCurveClass := TSwapScanCurve;
end;

  function EmissiveColor(const Item: TTriangle): TVector3;
  var
    M: TMaterialInfo;
  begin
    M := Item.MaterialInfo;
    if M <> nil then
      Result := M.EmissiveColor
    else
      Result := TPhongMaterialInfo.DefaultEmissiveColor;
  end;

  function IsLightSource(const Item: TTriangle): boolean;
  begin
    Result := EmissiveColor(Item).LengthSqr > Sqr(SingleEpsilon);
  end;

procedure TPathTracer.CollectLightItems(const Triangle: PTriangle);
begin
  if IsLightSource(Triangle^) then
    CollectedLightItems.Add(Triangle);
end;

{ Some notes about path tracer implementation :

  - Meaning of TraceOnlyIndirect parameter for Trace():

    When (DirectIllumSamplesCount <> 0) then rendering equation at each point
    is splitted into
      L_out = L_emission + IntegralOverHemisphere( L_direct + L_indirect ),
    where
      L_indirect = L_emission + IntegralOverHemisphere( L_direct + L_indirect ),
    and L_indirect must hit something that is *not* a light source.
    Which means that L_emission is always here = (0, 0, 0) so we have
      L_indirect = IntegralOverHemisphere( L_direct + L_indirect ),

    Which means that when we do recursive calls to Trace to calculate
    L_indirect we *do not want to hit light source* since then
    L_direct would be calculated twice.

    In other words,
    - for primary rays we pass TraceOnIndirect = false
      since we calculate L_out and we want L_emission (which makes sense anyway,
      since when you look directly at the light source you obviously see it's
      light).

    - for non-primary rays, i.e. in Trace recursive call from Trace itself,
      we pass TraceOnIndirect = true. (Well, unless DirectIllumSamplesCount = 0,
      in which case we just always make normal rendering equation
      and we want to always calculate L_emission;
      So TraceOnlyIndirect is actually DirectIllumSamplesCount <> 0).

    Tests confirm that this is right. Without this (if we would remove
    the check
    "if TraceOnlyIndirect and IsLightSource(IntersectNode) then ...")
    we get a lot more noise on our image.

    Trace call with TraceOnlyIndirect = true that hits into a light source
    returns just black color.

  - Note that MinDepth and Depth (Trace parameters) are *signed* integers.
    Depth can be negative, for each recursive call we pass Depth = Depth - 1.
    When Depth <= 0 then roussian roulette will be used.
    MinDepth is also signed becasue of that, since it's a starting Depth value.
}

procedure TPathTracer.Execute;
var
  { In LightItems we have pointers to Octree.Triangles[] pointing
    to the items with emission color > 0. In other words, light sources. }
  LightItems: TFastPointerList;

  {$ifdef PATHTR_USES_SHADOW_CACHE}
  { For each light in LightItems[I], ShadowCache[I] gives the pointer
    (somewhere into Octree.Triangles[]) of the last object that blocked this
    light source (or nil if no such object was yet found during
    this path tracer execution).

    This index is updated and used in IsLightShadowed.
    The idea of "shadow cache" comes from RGK, crystalized in "Graphic Gems II". }
  ShadowCache: TFastPointerList;
  {$endif}

  SceneBackgroundColor: TCastleColorRGB;

const
  { LightEmissionArea defines what the xxx.Emission properties of light sources mean.

    The emission color of light will be scaled by the solid angle that
    the light source has for the illuminated point. This scaling will be done such
    that if the light occupies a solid angle of LightEmissionArea
    (in steradians) then the light color added to DirectIllumination will be
    exactly the Emission of the light. If the solid angle is X times larger
    than LightEmissionArea then the color that should be added to Direct Illumination
    will be X*LightEmissionArea.

    This color will be further scaled by BRDF and other things,
    so this doesn't mean that exactly such color X*Emission will be returned
    by DirectIllumination. But this will be the "basis" of that color.

    You can think of LightEmissionArea as defining in what units
    we have the Emission of light stored in the model.

    The value below was chosen experimentally wanting the cornell-box rendered
    with path tracer to look the same as the rendering on the www.cornell-box website.
    I adjusted this parameter until the path tracer images (with Russian roulette,
    because it doesn't introduce bias) had "visually" similar brightness as their
    box.jpg. }
  LightEmissionArea = 1/30;

  function IsLightShadowed(const Item: PTriangle;
    const ItemPoint: TVector3;
    const LightSourceIndiceIndex: Integer;
    LightSourcePoint: TVector3): boolean;
  { This function calculates shadow ray (or rather segment). Returns true if
    between point ItemPoint and LightSourcePoint there is some element
    with transparency = 1. Otherwise returns false.
    LightSourceIndiceIndex is the index to the LightItems[] array.
    Item is pointer to given item (somewhere in Octree.Triangles[]). }
  { TODO: transparent objects should scale light color instead of just
    letting it pass }
  var
    OctreeIgnorer: TOctreeIgnoreForShadowRaysAndOneItem;
    Shadower: PTriangle;
  {$ifdef PATHTR_USES_SHADOW_CACHE}
    CachedShadower: PTriangle;
  {$endif}
  begin
    {$ifdef PATHTR_USES_SHADOW_CACHE}
    { try to get result from ShadowCache }
    CachedShadower := ShadowCache.Items[LightSourceIndiceIndex];
    if (CachedShadower <> nil) and
       (CachedShadower <> Item) then
    begin
      Inc(TriangleCollisionTestsCounter);
      if IsTriangleSegmentCollision(CachedShadower^.SceneSpace.Triangle,
        CachedShadower^.SceneSpace.Plane, ItemPoint, LightSourcePoint) then
        Exit(true);

      { above we forget about epsilon margin around ItemPoint and
        LightSourcePoint (if there is intersection there it should be considered
        as invalid). Also below we forget about margin around
        LightSourcePoint. In my code these epsilon margins are not so
        important (only for invalid models, for valid models
        ItemIndexToIgnore and OctreeIgnorer are sufficient) so I ignore these
        shortcomings here. }
    end;
    {$endif}

    { calculate intersection using Octree }
    OctreeIgnorer := TOctreeIgnoreForShadowRaysAndOneItem.Create(
      LightItems.Items[LightSourceIndiceIndex]);
    try
      Shadower := Octree.SegmentCollision(ItemPoint, LightSourcePoint, false,
        Item, true, {$ifdef FPC}@{$endif} OctreeIgnorer.IgnoreItem);
      Result := Shadower <> nil;
      {$ifdef PATHTR_USES_SHADOW_CACHE}
      ShadowCache.Items[LightSourceIndiceIndex] := Shadower;
      {$endif}
    finally OctreeIgnorer.Free end;
  end;

  function Trace(const RayOrigin, RayDirection: TVector3;
    const Depth: Integer; const TriangleToIgnore: PTriangle;
    const IgnoreMarginAtStart: boolean; const TraceOnlyIndirect: boolean)
    : TVector3;
  { traces ray with given depth. Returns Black (0, 0, 0) if
    ray doesn't hit anything, otherwise returns calculated color. }
  var
    Intersection: TVector3;
    IntersectNode: PTriangle;
    Material: TPhongMaterialInfo; { = IntersectNode.MaterialInfo }
    IntersectNormal: TVector3;

    function TraceNonEmissivePart: TVector3;

      function TryCalculateTransmittedSpecularRayDirection(
        var TracedDir: TVector3;
        var PdfValue: Single): boolean;
      var
        TransmittedRayDirection: TVector3;
        EtaFrom, EtaTo: Single;
      const
        EtaConst = 1.3; { TODO: here I also use EtaConst, like in Classic ray-tracer }
      begin
        if Odd(MinDepth-Depth) then
          begin EtaFrom := 1; EtaTo := EtaConst end else
          begin EtaFrom := EtaConst; EtaTo := 1 end;

        Result := TryTransmittedRayDirection(TransmittedRayDirection,
          RayDirection.Normalize,
          IntersectNormal, EtaFrom, EtaTo);
        if Result then
          TracedDir := PhiThetaToXYZ(
            RandomHemispherePointCosThetaExp(
              Round(Material.TransSpecularExp),
              PdfValue),
            TransmittedRayDirection);
      end;

      function DirectIllumination: TVector3;
      { this function calculates DirectIllumination for our Intersection.
        Implementation: we use formulation (101) from GlobalIllumCompendium:

        for i = 0..DirectIllumSamplesCount-1 do
          uniformly choose LightItemIndex from 0..LightIndices.Count-1.
          uniformly (with respect to surface area of chosen light triangle)
            choose point on light as SampleLightPoint.
          if (SampleLightPoint visible from Intersection) then
            result += SurfaceArea(LightItem) * LightEmission * BRDF *
              GeometryFunction

        At the end result *= LightIndices.Count / DirectIllumSamplesCount.

        One can say that the instruction

          result += SurfaceArea(LightItem) * LightEmission * BRDF *
            GeometryFunction

        is more or less equivalent to

          result += LightEmission * BRDF * cos(LightDirNorm, IntersectNormal)
            * solid-angle-of-light

        (this is the role of SurfaceArea(LightItem) and part of GeometryFunction -
        they simply calculate solid angle; well, de facto some very good approximation
        of solid angle)

        As a result result = average color from light times average surface
        of light * number of lights = exactly direct illumination, taking into account
        that different lights have different surfaces and shine with different intensity.
      }
      { TODO: better approach: (102), i.e. choose point on light source
              with respect to its solid angle.
        TODO: even better: (103), i.e. choose light in such way that
              lights with larger surface (or rather, with larger solid
              angle) and/or with larger intensity will be chosen more often. }
      var
        LightSource: PTriangle;
        LightSourceIndiceIndex: Integer; { indeks do LightIndices[] }
        SampleLightPoint: TVector3;
        DirectColor, LightDirNorm, NegatedLightDirNorm: TVector3;
        i: integer;
      begin
        Result := TVector3.Zero;

        { need to guard against LightsItems.Count = 0 (to be able to safely
          do Random(LightsItems.Count) later) and against
          DirectIllumSamplesCount = 0 (to be able to safely divide by
          DirectIllumSamplesCount later). }
        if (LightItems.Count = 0) or (DirectIllumSamplesCount = 0) then Exit;

        for i := 0 to DirectIllumSamplesCount - 1 do
        begin
          { calculate LightSourceIndiceIndex, LightSourceIndex, LightSource }
          LightSourceIndiceIndex := Random(LightItems.Count);
          LightSource := LightItems.Items[LightSourceIndiceIndex];
          if LightSource = IntersectNode then Continue;

          { calculate SampleLightPoint.
            Better check later that SampleLightPoint is
            different from Intersection (since SampleLightPoint is random, on
            incorrectly constructed model anything can happen...)  }
          SampleLightPoint := LightSource^.SceneSpace.Triangle.RandomPoint;
          if TVector3.Equals(SampleLightPoint, Intersection) then Continue;

          { calculate LigtDirNorm (not normalized).
            If LigtDirNorm comes from different side of
            IntersectionNode.TriangleNormPlane than IntersectNormal
            it means that light is on the opposite side of plane - so
            light doesn't illuminate our pixel. }
          LightDirNorm := SampleLightPoint - Intersection;
          if not VectorsSamePlaneDirections(LightDirNorm, IntersectNormal,
            IntersectNormal) then Continue;

          { check IsLightShadowed, i.e. do shadow ray }
          if IsLightShadowed(IntersectNode, Intersection,
            LightSourceIndiceIndex, SampleLightPoint) then Continue;

          { calculate DirectColor = emission color of light }
          DirectColor := EmissiveColor(LightSource^);

          { multiply by our "pseudo-BRDF" i.e. simply by Diffuse color
            of material }
          DirectColor := DirectColor * Material.DiffuseColor;

          { calculate LightDirNorm (normalized), NegatedLightDirNorm }
          LightDirNorm := LightDirNorm.Normalize;
          NegatedLightDirNorm := -LightDirNorm;

          { Multiply DirectColor
            1) by GeometryFunction i.e.
                 cos(LightDirNorm, IntersectNormal)
                   * cos(-LightDirNorm, LightSource.SceneSpace.Normal) /
                   PointsDistanceSqr(SampleLightPoint, Intersection).
               Cosines naturally calculated using dot product.
            2) by TriangleArea

            One can notice that the term

              TriangleArea *
              cos(-LightDirNorm, LightSource.SceneSpace.Normal) /
                PointsDistanceSqr(SampleLightPoint, Intersection)

            simply calculates solid angle of light with respect to Intersection
            (well, strictly speaking some very good approximation of this solid angle).

            It can be instructive here to see how it works when we remove multiplication

              by cos(-LightDirNorm, LightSource.SceneSpace.Normal)

            (light will then shine brighter as if "sideways").

            It can also be instructive to remove:

            - division by PointsDistanceSqr(SampleLightPoint, Intersection)
            - and also multiplication by TriangleArea

            (these two things "work together",
            i.e. their ratio is important here, so removing only
            one of these values doesn't make sense).

            It would be elegant to multiply here also by
            1/LightEmissionArea. But since LightEmissionArea = const so
            I moved multiplication by LightEmissionArea to the very end of this
            function.}
          DirectColor := DirectColor *
            TVector3.DotProduct(LightDirNorm, IntersectNormal) *
            TVector3.DotProduct(NegatedLightDirNorm,
              PlaneDirInDirection(LightSource^.SceneSpace.Plane,
                NegatedLightDirNorm)) *
            LightSource^.SceneSpace.Area /
            PointsDistanceSqr(SampleLightPoint, Intersection);

          Result := Result + DirectColor;
        end;

        { only here multiply by 1/LightEmissionArea.
          Also divide by number of samples and multiply by number of lights -
          - as a result make the result approximate the sum of direct illumination contribution
          of all lights. }
        Result := Result * (LightItems.Count /
          (LightEmissionArea * DirectIllumSamplesCount));
      end;

    type
      { colors Transmittive/Reflective Diffuse/Specular }
      TColorKind = (ckRS, ckRD, ckTS, ckTD);
    var
      Colors: array[TColorKind]of TVector3;
      Weights: array[TColorKind]of Single;
      WeightsSum: Single;
      RandomCK: Single;
      PdfValue: Single;
      TracedCol, TracedDir: TVector3;
      ck: TColorKind;
    begin
      Result := TVector3.Zero;
      { all result that we calculate here we get thanks to winning in Russian
        roulette if Depth <= 0. (Need to remember this and later divide
        by RROulContinue.) }

      if (Depth > 0) or (Random < RRoulContinue) then
      begin
        { path step is importance sampling according to Modified Phong BRDF,
          see GlobalIllumComp (66), diffuse we sample with density cos(),
          specular with density cos()^N_EXP.

          As a result after getting Trace result for diffuse I no longer divide result
          by cosine() (and I should, because this is importance sampling) nor
          multiply it by cosine() (and I should, because in BRDF integral there is
          this cosine - diffuse means gathering from all light directions
          uniformly but at smaller angle less rays fall on surface,
          that's why in diffuse we have cosine). All because these cosines
          cancel out.

          Similarly for specular - I hope! TODO: Specular is not yet
          very well tested...

          As a result I completely ignore PdfValue (obtained as result of
          RandomUnitHemispeherePoint) and BRDF - simply this particular PDF distribution
          corresponds EXACTLY to how light falls. }

        { calculate Colors[] }
        Colors[ckRS] := Material.ReflSpecular;
        Colors[ckRD] := Material.ReflDiffuse;
        Colors[ckTS] := Material.TransSpecular;
        Colors[ckTD] := Material.TransDiffuse;

        { calculate Weights[] and WeightSum }
        WeightsSum := 0;
        for ck := Low(ck) to High(ck) do
        begin
          Weights[ck] := Colors[ck][0] +
                         Colors[ck][1] +
                         Colors[ck][2];
          WeightsSum := WeightsSum + Weights[ck];
        end;

        { choose one of ck: choose variable RandomCK from range 0..WeightsSum
          and then check which of the Weights[] ranges it falls into. Calculate ck. }
        RandomCK := Random * WeightsSum;
        ck := Low(ck);
        while ck < High(ck) do
        begin
          if RandomCK < Weights[ck] then break;
          RandomCK := RandomCK - Weights[ck];
          Inc(ck);
        end;

        { note: no, below cannot be replaced with test WeightsSum >
          SingleEpsilon. Even when this holds it can still turn out
          that WeightsSum is indeed much larger than zero but
          Weights[ck] itself is microscopically small (and we simply had a lot of
          luck in random selection; path tracer does so many paths, so many pixels
          etc. that it's not hard here to "accidentally" choose microscopically
          small value). }
        if Weights[ck] > SingleEpsilon then
        begin
          IntersectNormal :=
            {$ifdef CONSERVE_TRIANGLE_MEMORY}
            IntersectNode^.SceneSpace.Plane.XYZ
            {$else}
            IntersectNode^.INormalCore(Intersection)
            {$endif};
          { choose normal at Intersection pointing in the direction of RayOrigin }
          IntersectNormal := PlaneDirNotInDirection(IntersectNormal, RayDirection);

          { calculate TracedDir and PdfValue by sampling hemisphere appropriately
           (based on ck). In case of TS total internal reflection may occur
           and then we end the path. }
          case ck of
            ckTD: TracedDir := PhiThetaToXYZ(
                    RandomHemispherePointCosTheta(PdfValue),
                    -IntersectNormal);
            ckTS: if not TryCalculateTransmittedSpecularRayDirection(
                    TracedDir, PdfValue) then Exit;
            ckRD: TracedDir := PhiThetaToXYZ(
                    RandomHemispherePointCosTheta(PdfValue),
                    IntersectNormal);
            ckRS: TracedDir := PhiThetaToXYZ(
                    RandomHemispherePointCosThetaExp(
                      Round(Material.ReflSpecularExp),
                      PdfValue),
                    ReflectedRayDirection(RayDirection.Normalize,
                      IntersectNormal));
          end;

          { call Trace() recursively, so continue the path }
          TracedCol := Trace(Intersection, TracedDir, Depth - 1,
            IntersectNode, true, DirectIllumSamplesCount <> 0);

          { process TracedCol: multiply by Colors[ck], divide by chance
            of its selection among four Colors[], i.e. by
            Weights[ck]/WeightsSum (because this is importance sampling after all)
            (i.e. multiply by WeightsSum/Weights[ck], we know that denominator is
            > SingleEpsilon, we checked this earlier). }
          TracedCol := TracedCol * Colors[ck];
          TracedCol := TracedCol * (WeightsSum / Weights[ck]);

          Result := Result + TracedCol;
        end;

        { add DirectIllumination }
        Result := Result + DirectIllumination;

        { If we entered here thanks to Russian roulette (i.e. if Depth <= 0)
          then scale Result so that the estimator written here is unbiased. }
        if Depth <= 0 then Result := Result * (1/RRoulContinue);
      end;
    end;

  var
    i: Integer;
    NonEmissiveColor: TVector3;
    AnyMaterial: TMaterialInfo;
  begin
    IntersectNode := Octree.RayCollision(Intersection, RayOrigin, RayDirection, true,
      TriangleToIgnore, IgnoreMarginAtStart, nil);
    if IntersectNode = nil then Exit(SceneBackgroundColor);

    if TraceOnlyIndirect and IsLightSource(IntersectNode^) then
    begin
      Result := TVector3.Zero;
      Exit;
    end;

    AnyMaterial := IntersectNode^.MaterialInfo;
    if AnyMaterial is TPhongMaterialInfo then
      Material := TPhongMaterialInfo(AnyMaterial)
    else
      Material := nil; // TODO: path tracer treats all non-Phong materials (like Physical) as unlit

    { de facto if TraceOnlyIndirect then the line below certainly adds
      (0, 0, 0) to result. But I don't see right now how to extract
      some special optimization from this.

      We use below EmissiveColor(), not MaterialInfo.EmissiveColor,
      because this is done even when MaterialInfo is nil. }
    Result := EmissiveColor(IntersectNode^);

    if Material <> nil then
    begin
      { if MinDepth = Depth it means that our Trace returns color for primary ray.

        So we branch here into NonPrimarySamplesCount, i.e. we act
        as if we were stochastic ray tracer that branches
        into multiple rays at recursion point.

        Otherwise we go by path i.e. we act as if we were path tracer i.e.
        we don't branch into multiple rays. }
      if MinDepth = Depth then
      begin
        NonEmissiveColor := TVector3.Zero;
        for i := 0 to NonPrimarySamplesCount-1 do
          NonEmissiveColor := NonEmissiveColor + TraceNonEmissivePart;
        NonEmissiveColor := NonEmissiveColor * (1 / NonPrimarySamplesCount);
        Result := Result + NonEmissiveColor;
      end else
        Result := Result + TraceNonEmissivePart;
    end;
  end;

var
  RaysWindow: TRaysWindow;

  procedure DoPixel(const x, y: Cardinal);
  var
    PixColor, PrimaryRayOrigin, PrimaryRayDirection: TVector3;
    SampleNum: Integer;
    C: TCastleColor;
    ColRGB: TCastleColorRGB absolute C;
  begin
    { generate pixel x, y. calculate PixColor }
    if PrimarySamplesCount = 1 then
    begin
      { when PrimarySamplesCount = 1 we send one primary ray
        and this ray is NOT randomized on projection plane within pixel
        x, y but passes exactly through center of pixel x, y. }
      RaysWindow.PrimaryRay(x, y, Image.Width, Image.Height, PrimaryRayOrigin, PrimaryRayDirection);
      PixColor := Trace(PrimaryRayOrigin, PrimaryRayDirection, MinDepth, nil, false, false);
    end else
    begin
      PixColor := TVector3.Zero;
      for SampleNum := 0 to PrimarySamplesCount - 1 do
      begin
        RaysWindow.PrimaryRay(
          x + Random - 0.5, y + Random - 0.5,
          Image.Width, Image.Height, PrimaryRayOrigin, PrimaryRayDirection);
        PixColor := PixColor + Trace(PrimaryRayOrigin, PrimaryRayDirection, MinDepth, nil, false, false);
      end;
      PixColor := PixColor * (1 / PrimarySamplesCount);
    end;

    { save PixColor to Image }
    C := Image.Colors[X, Y, 0];
    ColRGB := PixColor;
    Image.Colors[X, Y, 0] := C;
  end;

var
  PixCoord: TVector2Cardinal;
  SFCurve: TSpaceFillingCurve;
begin
  SceneBackgroundColor := GetSceneBackgroundColor(Background, SceneBGColor);

  { check parameters (path tracing takes very long anyway so we can afford
    to execute a few tests at the beginning of this procedure, even when
    compiling in RELEASE version) }
  Check(PrimarySamplesCount > 0, 'PrimarySamplesCount for PathTracer must be greater than 0');
  Check(NonPrimarySamplesCount > 0, 'NonPrimarySamplesCount for PathTracer must be greater than 0');
  ClampVar(RRoulContinue, Single(0.0), Single(1.0));

  { initialize all to nil, to easily free them all in one try..finally }
  LightItems := nil;
  {$ifdef PATHTR_USES_SHADOW_CACHE} ShadowCache := nil; {$endif}
  RaysWindow := nil;
  SFCurve := nil;
  try
    { calculate LightItems }
    LightItems := TFastPointerList.Create;
    LightItems.Capacity := Octree.TrianglesCount div 4;
    CollectedLightItems := LightItems;
    Octree.EnumerateTriangles({$ifdef FPC}@{$endif} CollectLightItems);

    {$ifdef PATHTR_USES_SHADOW_CACHE}
    { calculate ShadowCache }
    ShadowCache := TFastPointerList.Create;
    ShadowCache.Count := LightItems.Count;
    { Setting TFastPointerList.Count already makes sure that new pointers are nil }
    {$endif}

    { calculate RaysWindow }
    RaysWindow := TRaysWindow.CreateDescendant(CamPosition,
      CamDirection.Normalize, CamUp.Normalize, Projection);

    { calculate SFCurve }
    SFCurve := SFCurveClass.Create(Image.Width, Image.Height);
    SFCurve.SkipPixels(FirstPixel);

    { generate image pixels }
    if Assigned(PixelsMadeNotifier) then
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
        PixelsMadeNotifier(SFCurve.PixelsDone, PixelsMadeNotifierData);
      end;
    end else
    begin
      while not SFCurve.EndOfPixels do
      begin
        PixCoord := SFCurve.NextPixel;
        DoPixel(PixCoord[0], PixCoord[1]);
      end;
    end;

  finally
    SFCurve.Free;
    RaysWindow.Free;
    {$ifdef PATHTR_USES_SHADOW_CACHE} ShadowCache.Free; {$endif}
    LightItems.Free;
  end;
end;

procedure TPathTracer.AppendStats(const Stats: TStrings; const RenderingTime: Single);
var
  PathsCount: Cardinal;
begin
  inherited;
  PathsCount := (Image.Width * Image.Height - FirstPixel) *
    PrimarySamplesCount * NonPrimarySamplesCount;
  Stats.Append(Format('Image size is %d x %d pixels (first %d pixels skipped) and we use %d (primary) x %d (non-primary) samples per pixel which gives %d paths.',
    [Image.Width, Image.Height, FirstPixel,
     PrimarySamplesCount, NonPrimarySamplesCount, PathsCount]));
  Stats.Append(Format('%f paths done per second.',
    [PathsCount / RenderingTime]));
  Stats.Append(Format('%f simple collision tests done per one path.',
    [TriangleCollisionTestsCounter /  PathsCount ]));
end;

{ globals ------------------------------------------------------------------- }

function CreateOctreeVisibleTrianglesForScene(const Scene: TCastleSceneCore): TTriangleOctree;
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  Result := TTriangleOctree.Create(DefTriangleOctreeLimits, Scene.LocalBoundingBox);
  try
    Result.Triangles.Capacity := Scene.TrianglesCount;
    ShapeList := Scene.Shapes.TraverseList(
      { OnlyActive } true,
      { OnlyVisible } true,
      { OnlyCollidable } false
    );
    for Shape in ShapeList do
      Shape.Triangulate({$ifdef FPC}@{$endif} Result.AddItemTriangle,
        { FrontFaceAlwaysCcw should not matter } false);
  except Result.Free; raise end;
end;

end.
