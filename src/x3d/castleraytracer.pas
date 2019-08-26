{
  Copyright 2003-2018 Michalis Kamburelis.

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
  CastleVectors, CastleImages, CastleRays, CastleProjection, CastleUtils,
  X3DTriangles, CastleShapes, X3DNodes, CastleSpaceFillingCurves, CastleTriangles;

type
  { }
  TPixelsMadeNotifierFunc = procedure(PixelsMadeCount: Cardinal; Data: Pointer);

  TRayTracerKind = (rtkClassic, rtkPathTracer);

  TRayTracer = class
  strict protected
    procedure AppendStats(const Stats: TStrings; const RenderingTime: Single); virtual;
  public
    { Scene to render.
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
    BaseLights: TLightInstancesList;
    OwnsBaseLights: boolean;

    procedure Execute; override;
    destructor Destroy; override;
  end;

  { Path tracer. See
    [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.path_tracer.html]
    for documentation. }
  TPathTracer = class(TRayTracer)
  private
    CollectedLightItems: TFPList;
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

implementation

uses SysUtils, Math,
  CastleSphereSampling, CastleTimeUtils, CastleColors, CastleTextureImages;

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

function GetDiffuseTexture(const Texture: TAbstractTextureNode;
  const TexCoord: TVector4): TCastleColorRGB;

  function DivUnsignedModulo(const Dividend: Integer; const Divisor: Integer): Integer;
  var
    DivResult: Smallint;
    DivRemainder: Word;
  begin
    DivUnsignedMod(Dividend, Divisor, DivResult, DivRemainder);
    Result := DivRemainder;
  end;

var
  RepeatCoord: array [0..2] of boolean;

  function SampleNearest(const Image: TCastleImage; const Pixel: TVector3): TCastleColor;
  var
    I: Integer;
    Dimensions: TVector3Cardinal;
    PixelInt: TVector3Integer;
  begin
    Dimensions := Image.Dimensions;
    for I := 0 to 2 do
    begin
      PixelInt[I] := Round(Pixel[I] - 0.5);
      if RepeatCoord[I] then
        PixelInt[I] := DivUnsignedModulo(PixelInt[I], Dimensions[I])
      else
        PixelInt[I] := Clamped(PixelInt[I], 0, Dimensions[I] - 1);
    end;
    Result := Image.Colors[PixelInt[0], PixelInt[1], PixelInt[2]];
  end;

  // This works OK, but is not needed now (as it ~2 times slower than SampleLinear2D).
  {
  function SampleLinear(const Image: TCastleImage; Pixel: TVector3): TCastleColor;
  var
    I: Integer;
    Dimensions: TVector3Cardinal;
    PixelInt: array [boolean, 0..2] of Integer;
    PixelFrac: TVector3;
  begin
    Dimensions := Image.Dimensions;
    for I := 0 to 2 do
    begin
      if RepeatCoord[I] then
        Pixel[I] := FloatModulo(Pixel[I], Dimensions[I])
      else
        Pixel[I] := Clamped(Pixel[I], 0, Dimensions[I]);
    end;

    for I := 0 to 2 do
    begin
      PixelInt[false][I] := Clamped(Floor(Pixel[I]), 0, Dimensions[I] - 1);
      PixelInt[true ][I] := Min(PixelInt[false][I] + 1, Dimensions[I] - 1);
      PixelFrac[I] := Clamped(Pixel[I] - PixelInt[false][I], 0.0, 1.0);
    end;

    Result :=
      Image.Colors[PixelInt[false][0], PixelInt[false][1], PixelInt[false][2]] * (1 - PixelFrac[0]) * (1 - PixelFrac[1]) * (1 - PixelFrac[2]) +
      Image.Colors[PixelInt[false][0], PixelInt[false][1], PixelInt[ true][2]] * (1 - PixelFrac[0]) * (1 - PixelFrac[1]) * (    PixelFrac[2]) +
      Image.Colors[PixelInt[false][0], PixelInt[ true][1], PixelInt[false][2]] * (1 - PixelFrac[0]) * (    PixelFrac[1]) * (1 - PixelFrac[2]) +
      Image.Colors[PixelInt[false][0], PixelInt[ true][1], PixelInt[ true][2]] * (1 - PixelFrac[0]) * (    PixelFrac[1]) * (    PixelFrac[2]) +
      Image.Colors[PixelInt[ true][0], PixelInt[false][1], PixelInt[false][2]] * (    PixelFrac[0]) * (1 - PixelFrac[1]) * (1 - PixelFrac[2]) +
      Image.Colors[PixelInt[ true][0], PixelInt[false][1], PixelInt[ true][2]] * (    PixelFrac[0]) * (1 - PixelFrac[1]) * (    PixelFrac[2]) +
      Image.Colors[PixelInt[ true][0], PixelInt[ true][1], PixelInt[false][2]] * (    PixelFrac[0]) * (    PixelFrac[1]) * (1 - PixelFrac[2]) +
      Image.Colors[PixelInt[ true][0], PixelInt[ true][1], PixelInt[ true][2]] * (    PixelFrac[0]) * (    PixelFrac[1]) * (    PixelFrac[2]);
  end;
  }

  function SampleLinear2D(const Image: TCastleImage; Pixel: TVector3): TCastleColor;
  var
    I: Integer;
    Dimensions: TVector3Cardinal;
    PixelInt: array [boolean, 0..1] of Integer;
    PixelFrac: TVector2;
  begin
    Dimensions := Image.Dimensions;
    for I := 0 to 1 do
    begin
      if RepeatCoord[I] then
        Pixel.Data[I] := FloatModulo(Pixel.Data[I], Dimensions[I])
      else
        Pixel.Data[I] := Clamped(Pixel.Data[I], 0, Dimensions[I]);
    end;

    for I := 0 to 1 do
    begin
      // TODO: this is not correct in case RepeatCoord[I],
      // it will not filter 100% nicely on the border pixels
      PixelInt[false][I] := Clamped(Floor(Pixel[I]), 0, Dimensions[I] - 1);
      PixelInt[true ][I] := Min(PixelInt[false][I] + 1, Dimensions[I] - 1);
      PixelFrac[I] := Clamped(Pixel[I] - PixelInt[false][I], 0.0, 1.0);
    end;

    Result :=
      Image.Colors[PixelInt[false][0], PixelInt[false][1], 0] * (1 - PixelFrac[0]) * (1 - PixelFrac[1]) +
      Image.Colors[PixelInt[ true][0], PixelInt[false][1], 0] * (    PixelFrac[0]) * (1 - PixelFrac[1]) +
      Image.Colors[PixelInt[false][0], PixelInt[ true][1], 0] * (1 - PixelFrac[0]) * (    PixelFrac[1]) +
      Image.Colors[PixelInt[ true][0], PixelInt[ true][1], 0] * (    PixelFrac[0]) * (    PixelFrac[1]);
  end;

var
  SingleTexture: TX3DNode;
  Texture2D: TAbstractTexture2DNode;
  EncodedImage: TEncodedImage;
  Image: TCastleImage;
  TexCoord3D: TVector3;
  Pixel: TVector3;
  Color: TCastleColor;
  Bilinear: boolean;
begin
  Result := WhiteRGB;

  SingleTexture := nil;
  if Texture is TMultiTextureNode { also makes sure Texture <> nil } then
  begin
    if TMultiTextureNode(Texture).FdTexture.Count > 0 then
      SingleTexture := TMultiTextureNode(Texture).FdTexture[0];
  end else
    SingleTexture := Texture;

  if SingleTexture is TAbstractTexture2DNode { also makes sure SingleTexture <> nil } then
  begin
    // TODO: 3D textures could be handled easily too!
    Texture2D := TAbstractTexture2DNode(SingleTexture);
    if Texture2D.IsTextureImage then
    begin
      EncodedImage := Texture2D.TextureImage;
      if (EncodedImage is TCastleImage) and
         (not EncodedImage.IsEmpty) and
         (not IsZero(TexCoord[3])) then
      begin
        Image := TCastleImage(EncodedImage);
        TexCoord3D := TexCoord.ToPosition;
        Pixel.Init(
          TexCoord3D[0] * Image.Width,
          TexCoord3D[1] * Image.Height,
          TexCoord3D[2] * Image.Depth
        );
        if IsNan(TexCoord[0]) then
        begin
          // TODO: bug on demo-models/bump_mapping/bump_mapping_leaf_test.wrl
          {
          Writeln(Pixel.ToRawString);
          Writeln(TexCoord3D.ToRawString);
          Writeln(TexCoord.ToRawString);
          Writeln(Image.Width, ' ', Image.Height, ' ', Image.Depth);
          }
          Exit;
        end;

        RepeatCoord[0] := Texture2D.RepeatS;
        RepeatCoord[1] := Texture2D.RepeatT;
        RepeatCoord[2] := false { Texture2D.RepeatR };

        Bilinear := not (
          (Texture2D.TextureProperties <> nil) and
          // TODO: we should look either at MagnificationFilter or MinificationFilter
          (Texture2D.TextureProperties.MagnificationFilter = magNearest)
        );
        if Bilinear then
          Color := SampleLinear2D(Image, Pixel)
        else
          Color := SampleNearest(Image, Pixel);

        Result := Color.XYZ;
      end;
    end;
  end;
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
      Result := Octree.LightNotBlocked(Light,
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
          Result := M.DiffuseColor;
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
      IntersectNode^.World.Plane.XYZ
      {$else}
      IntersectNode^.INormalWorldSpace(Intersection)
      {$endif};

    MaterialInfo := IntersectNode^.MaterialInfo;
    if MaterialInfo <> nil then
    begin
      MaterialReflectionColor := MaterialInfo.ReflectionColor;
      MaterialTransparency := MaterialInfo.Transparency;
    end else
    begin
      MaterialReflectionColor := TMaterialInfo.DefaultReflectionColor;
      MaterialTransparency := TMaterialInfo.DefaultTransparency;
    end;

    DiffuseTextureColor :=
      {$ifdef CONSERVE_TRIANGLE_MEMORY}
      WhiteRGB
      {$else}
      GetDiffuseTexture(
        IntersectNode^.State.DiffuseAlphaTexture,
        IntersectNode^.ITexCoord(Intersection))
      {$endif};

    Result := Emission(IntersectNode^.State.MaterialInfo, InitialDepth <> 0);
    with IntersectNode^ do
    begin
      if Depth > 0 then
      begin
        Lights := State.Lights;
        if Lights <> nil then
          for i := 0 to Lights.Count - 1 do
            if LightNotBlocked(Lights.List^[i]) then
              Result := Result + Lights.List^[i].Contribution(Intersection,
                IntersectNormal, IntersectNode^.State, CamPosition, DiffuseTextureColor);

        { Add BaseLights contribution, just like other lights.

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

          The above reasoning is nice as long as BaseLights only contain
          the headlight. Which is true in the current uses. }
        for I := 0 to BaseLights.Count - 1 do
          if (Depth = InitialDepth) or
             LightNotBlocked(BaseLights.List^[I]) then
            Result := Result + BaseLights.List^[I].Contribution(Intersection,
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
  if OwnsBaseLights then
    FreeAndNil(BaseLights);
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
      Result := TMaterialInfo.DefaultEmissiveColor;
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
  LightItems: TFPList;

  {$ifdef PATHTR_USES_SHADOW_CACHE}
  { For each light in LightItems[I], ShadowCache[I] gives the pointer
    (somewhere into Octree.Triangles[]) of the last object that blocked this
    light source (or nil if no such object was yet found during
    this path tracer execution).

    This index is updated and used in IsLightShadowed.
    The idea of "shadow cache" comes from RGK, crystalized in "Graphic Gems II". }
  ShadowCache: TFPList;
  {$endif}

  SceneBackgroundColor: TCastleColorRGB;

  { TODO: comments below are in Polish. }

const
  { LightEmissionArea definiuje co znacza wlasciwosci .Emission zrodel swiatla.
    Kolor emission swiatla bedzie oczywiscie skalowany przez kat brylowy jaki
    ma zrodlo swiatla dla oswietlanego punktu. Skalowanie to bedzie robione tak
    ze jezeli swiatlo bedzie zajmowalo kat brylowy LightEmissionArea
    (w steradianach) to kolor swiatla dodany do DirectIllumination bedzie wynosil
    doklanie Emission swiatla. Jesli kat brylowy bedzie a razy wiekszy (mniejszy)
    niz LightEmissionArea to kolor jaki bedzie mial byc dodany do Direct Illumination
    bedzie wynosil a*LightEmissionArea.

    Oczywiscie ten kolor bedzie dalej skalowany, przez BRDFa i inne rzeczy,
    wiec to nie oznacza ze dokladnie taki kolor a*Emission zostanie zwrocony
    przez DirectIllumination. Ale taka bedzie "podstawa" tego koloru.

    Mowiac nieco formalnie, LightEmissionArea okresla w jakich jednostkach
    mamy zapisane Emission swiatla w modelu.

    Wartosc ponizej dobralem eksperymentalnie chcac zeby cornell-box renderowane
    z path wygladalo tak samo jak rendering na stronie www.cornell-box'a.
    Dopasowywalem ten parametr tak dlugo az rysunki path (z rosyjska ruletka,
    bo ona nie wprowadza biasu) mialy "na oko" podobna jasnosc co tamtejszy
    box.jpg. }
  LightEmissionArea = 1/30;

  function IsLightShadowed(const Item: PTriangle;
    const ItemPoint: TVector3;
    const LightSourceIndiceIndex: Integer;
    LightSourcePoint: TVector3): boolean;
  { ta funkcja liczy shadow ray (a w zasadzie segment). Zwraca true jezeli
    pomiedzy punktem ItemPoint a LightSourcePoint jest jakis element
    o transparency = 1. Wpp. zwraca false.
    LightSourceIndiceIndex to indeks to tablicy LightItems[].
    Item to pointer to given item (somewhere in Octree.Triangles[]). }
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
    { sprobuj wziac wynik z ShadowCache }
    CachedShadower := ShadowCache.Items[LightSourceIndiceIndex];
    if (CachedShadower <> nil) and
       (CachedShadower <> Item) then
    begin
      Inc(TriangleCollisionTestsCounter);
      if IsTriangleSegmentCollision(CachedShadower^.World.Triangle,
        CachedShadower^.World.Plane, ItemPoint, LightSourcePoint) then
        Exit(true);

      { powyzej zapominamy o marginesie epsilonowym wokol ItemPoint i
        LightSourcePoint (jezeli tam jest przeciecie to powinno byc uznawane
        za niewazne). Zreszta ponizej zapominamy o marginesie wokol
        LightSourcePoint. W moim kodzie te marginesy epsilonowe nie sa tak
        wazne (tylko dla nieprawidlowych modeli, dla prawidlowych modeli
        wystarcza ItemIndexToIgnore i OctreeIgnorer) wiec olewam tutaj te
        niedorobki. }
    end;
    {$endif}

    { oblicz przeciecie uzywajac Octree }
    OctreeIgnorer := TOctreeIgnoreForShadowRaysAndOneItem.Create(
      LightItems.Items[LightSourceIndiceIndex]);
    try
      Shadower := Octree.SegmentCollision(ItemPoint, LightSourcePoint, false,
        Item, true, @OctreeIgnorer.IgnoreItem);
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
  { sledzi promien z zadana glebokoscia. Zwraca Black (0, 0, 0) jesli
    promien w nic nie trafia, wpp. zwraca wyliczony kolor. }
  var
    Intersection: TVector3;
    IntersectNode: PTriangle;
    MaterialInfo: TMaterialInfo; { = IntersectNode.MaterialInfo }
    IntersectNormal: TVector3;

    function TraceNonEmissivePart: TVector3;

      function TryCalculateTransmittedSpecularRayDirection(
        var TracedDir: TVector3;
        var PdfValue: Single): boolean;
      var
        TransmittedRayDirection: TVector3;
        EtaFrom, EtaTo: Single;
      const
        EtaConst = 1.3; { TODO: tu tez uzywam EtaConst, jak w Classic }
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
              Round(MaterialInfo.TransSpecularExp),
              PdfValue),
            TransmittedRayDirection);
      end;

      function DirectIllumination: TVector3;
      { ta funkcja liczy DirectIllumination dla naszego Intersection.
        Implementacja : uzywamy sformulowania (101) z GlobalIllumCompendium :

        for i = 0..DirectIllumSamplesCount-1 do
          uniformly losuj LightItemIndex sposrod 0..LightIndices.Count-1.
          uniformly (wzgledem pola powierzchni trojkata wylosowanego swiatla)
            losuj punkt na swietle jako SampleLightPoint.
          if (SampleLightPoint widoczny z Intersection) then
            result += PolePowierzchni(LightItem) * LightEmission * BRDF *
              GeometryFunction
        Na koncu result *= LightIndices.Count / DirectIllumSamplesCount.

        Mozna powiedziec ze instrukcja
          result += PolePowierzchni(LightItem) * LightEmission * BRDF *
            GeometryFunction
        jest mniej wiecej rownowazne
          result += LightEmission * BRDF * cos(LightDirNorm, IntersectNormal)
            * solid-angle-swiatla
        (taka jest rola PolePowierzchni(LightItem) i czesci GeometryFunction -
        one po prostu licza solid angle; no, de facto pewne bardzo dobre przyblizenie
        solid angle)

        W rezultacie result = sredni kolor ze swiatla razy srednia powierzchnia
          swiatla * ilosc swiatel = wlasnie direct illumination, uwzgledniajace
          ze rozne swiatla maja rozna powierzchnie i swieca z rozna intensywnoscia.
      }
      { TODO: better approach : (102), czyli losuj punkt na zrodle swiatla
              ze wzgledu na jego solid angle.
        TODO: jeszcze lepiej : (103), czyli losuj swiatlo w taki sposob ze
              swiatla o wiekszej powierzchni (a wlasciwie, o wiekszym kacie
              brylowym) i/lub o wiekszej intensywnosci beda wybierane czesciej. }
      var
        LightSource: PTriangle;
        LightSourceIndiceIndex: Integer; { indeks do LightIndices[] }
        SampleLightPoint: TVector3;
        DirectColor, LightDirNorm, NegatedLightDirNorm: TVector3;
        i: integer;
      begin
        Result := TVector3.Zero;

        { trzeba ustrzec sie tu przed LightsItems.Count = 0 (zeby moc pozniej
          spokojnie robic Random(LightsItems.Count) i przed
          DirectIllumSamplesCount = 0 (zeby moc pozniej spokojnie podzielic przez
          DirectIllumSamplesCount). }
        if (LightItems.Count = 0) or (DirectIllumSamplesCount = 0) then Exit;

        for i := 0 to DirectIllumSamplesCount - 1 do
        begin
          { calculate LightSourceIndiceIndex, LightSourceIndex, LightSource }
          LightSourceIndiceIndex := Random(LightItems.Count);
          LightSource := LightItems.Items[LightSourceIndiceIndex];
          if LightSource = IntersectNode then Continue;

          { calculate SampleLightPoint.
            Lepiej pozniej sprawdz ze SampleLightPoint jest
            rozny od Intersection (poniewaz SampleLightPoint jest losowy to na
            nieprawidlowo skonstruowanym modelu wszystko moze sie zdarzyc...)  }
          SampleLightPoint := LightSource^.World.Triangle.RandomPoint;
          if TVector3.Equals(SampleLightPoint, Intersection) then Continue;

          { calculate LigtDirNorm (nieznormalizowane).
            Jezeli LigtDirNorm wychodzi z innej strony
            IntersectionNode.TriangleNormPlane niz IntersectNormal
            to znaczy ze swiatlo jest po przeciwnej stronie plane - wiec
            swiatlo nie oswietla naszego pixela. }
          LightDirNorm := SampleLightPoint - Intersection;
          if not VectorsSamePlaneDirections(LightDirNorm, IntersectNormal,
            IntersectNormal) then Continue;

          { sprawdz IsLightShadowed, czyli zrob shadow ray }
          if IsLightShadowed(IntersectNode, Intersection,
            LightSourceIndiceIndex, SampleLightPoint) then Continue;

          { calculate DirectColor = kolor emission swiatla }
          DirectColor := EmissiveColor(LightSource^);

          { wymnoz przez naszego "niby-BRDFa" czyli po prostu przez kolor Diffuse
            materialu }
          DirectColor := DirectColor * MaterialInfo.DiffuseColor;

          { calculate LightDirNorm (znormalizowane), NegatedLightDirNorm }
          LightDirNorm.NormalizeMe;
          NegatedLightDirNorm := -LightDirNorm;

          { Wymnoz DirectColor
            1) przez GeometryFunction czyli
                 cos(LightDirNorm, IntersectNormal)
                   * cos(-LightDirNorm, LightSource.World.Normal) /
                   PointsDistanceSqr(SampleLightPoint, Intersection).
               Cosinusy naturalnie licz uzywajac dot product.
            2) przez TriangleArea

            Mozna zauwazyc ze czlon
              TriangleArea *
              cos(-LightDirNorm, LightSource.World.Normal) /
                PointsDistanceSqr(SampleLightPoint, Intersection)
            liczy po prostu solid angle swiatla with respect to Intersection
            (no, mowiac scisle pewne bardzo dobre przyblizenie tego solid angle).

            Moze byc tutaj pouczajace zobaczyc jak to dziala gdy usuniemy mnozenie
              przez cos(-LightDirNorm, LightSource.World.Normal)
              (swiatlo bedzie wtedy jasniej swiecilo jakby "w bok"),
            pouczajace moze tez byc usuniecie dzielenia przez
              PointsDistanceSqr(SampleLightPoint, Intersection) i jednoczesnie
              mnozenia przez TriangleArea (te dwie rzeczy "wspolpracuja ze soba",
              tzn. wazny jest tu wlasnie ich iloraz, dlatego usuwanie tylko
              jednej z tych wartosci nie ma sensu).

            Elegancko byloby tutaj pomnozyc jeszcze przez
              1/LightEmissionArea. Ale poniewaz LightEmissionArea = const wiec
              przenioslem mnozenie przez LightEmissionArea na sam koniec tej
              funkcji.}
          DirectColor *=
            TVector3.DotProduct(LightDirNorm, IntersectNormal) *
            TVector3.DotProduct(NegatedLightDirNorm,
              PlaneDirInDirection(LightSource^.World.Plane,
                NegatedLightDirNorm)) *
            LightSource^.World.Area /
            PointsDistanceSqr(SampleLightPoint, Intersection);

          Result := Result + DirectColor;
        end;

        { dopiero tu przemnoz przez 1/LightEmissionArea.
          Podziel tez przez ilosc probek i pomnoz przez ilosc swiatel -
          - w rezultacie spraw zeby wynik przyblizal sume wkladu direct illumination
          wszystkich swiatel. }
        Result := Result * (LightItems.Count /
          (LightEmissionArea * DirectIllumSamplesCount));
      end;

    type
      { kolory Transmittive/Reflective Diffuse/Specular }
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
      { caly result jaki tu wyliczymy dostaniemy dzieki wygranej w rosyjskiej
        ruletce jezeli Depth <= 0. (Trzeba o tym pamietac i pozniej podzielic
        przez RROulContinue.) }

      if (Depth > 0) or (Random < RRoulContinue) then
      begin
        { krok sciezki to importance sampling zgodnie z Modified Phong BRDF,
          patrz GlobalIllumComp (66), diffuse samplujemy z gestoscia cos(),
          specular z gestoscia cos()^N_EXP.

          W rezultacie po otrzymaniu wyniku Trace diffuse nie dziele juz wyniku
          przez cosinus() (a powinienem, bo to jest importance sampling) ani
          nie mnoze go przez cosinus() (a powinienem, bo w calce BRDF'a jest
          ten cosinus - diffuse oznacza zbieranie ze wszystkich kierunkow swiatla
          rownomiernie ale pod mniejszym katem na powierzchnie pada mniej promieni,
          dlatego w diffuse mamy cosinus). Wszystko dlatego ze te cosinusy sie
          skracaja.

          Podobnie dla specular - mam nadzieje ! TODO: Specular jeszcze
          nie jest zbyt dobrze przetestowane...

          W rezultacie kompletnie ignoruje PdfValue (otrzymywane w wyniku
          RandomUnitHemispeherePoint) i BRDF'a - po prostu akurat taki rozklad PDF'ow
          odpowiada DOKLADNIE temu jak wpada swiatlo. }

        { calculate Colors[] }
        Colors[ckRS] := MaterialInfo.ReflSpecular;
        Colors[ckRD] := MaterialInfo.ReflDiffuse;
        Colors[ckTS] := MaterialInfo.TransSpecular;
        Colors[ckTD] := MaterialInfo.TransDiffuse;

        { calculate Weights[] and WeightSum }
        WeightsSum := 0;
        for ck := Low(ck) to High(ck) do
        begin
          Weights[ck] := Colors[ck][0] +
                         Colors[ck][1] +
                         Colors[ck][2];
          WeightsSum := WeightsSum + Weights[ck];
        end;

        { wylosuj jedno z ck : wylosuj zmienna RandomCK z przedzialu 0..WeightsSum
          a potem zbadaj do ktorego z przedzialow Weights[] wpada. Calculate ck. }
        RandomCK := Random * WeightsSum;
        ck := Low(ck);
        while ck < High(ck) do
        begin
          if RandomCK < Weights[ck] then break;
          RandomCK := RandomCK - Weights[ck];
          Inc(ck);
        end;

        { notka : nie, ponizej nie mozna zamienic na test WeightsSum >
          SingleEpsilon. Nawet gdy to zachodzi ciagle moze sie okazac
          ze WeightsSum jest wprawdzie duzo wieksze od zera ale samo
          Weights[ck] jest mikroskopijnie male (i po prostu mielismy duzo
          szczescia w losowaniu; path tracer robi tyle sciezek, tyle pixeli
          itd. ze nietrudno tutaj "przez przypadek" wylosowac mikroskopijnie
          mala wartosc). }
        if Weights[ck] > SingleEpsilon then
        begin
          IntersectNormal :=
            {$ifdef CONSERVE_TRIANGLE_MEMORY}
            IntersectNode^.World.Plane.XYZ
            {$else}
            IntersectNode^.INormalWorldSpace(Intersection)
            {$endif};
          { choose normal at Intersection pointing in the direction of RayOrigin }
          IntersectNormal := PlaneDirNotInDirection(IntersectNormal, RayDirection);

          { calculate TracedDir i PdfValue samplujac odpowiednio polsfere
           (na podstawie ck). W przypadku TS moze wystapic calk. odbicie wewn.
           i wtedy konczymy sciezke. }
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
                      Round(MaterialInfo.ReflSpecularExp),
                      PdfValue),
                    ReflectedRayDirection(RayDirection.Normalize,
                      IntersectNormal));
          end;

          { wywolaj rekurencyjnie Trace(), a wiec idz sciezka dalej }
          TracedCol := Trace(Intersection, TracedDir, Depth - 1,
            IntersectNode, true, DirectIllumSamplesCount <> 0);

          { przetworz TracedCol : wymnoz przez Colors[ck], podziel przez szanse
            jego wyboru sposrod czterech Colors[], czyli przez
            Weights[ck]/WeightsSum (bo to w koncu jest importance sampling)
            (czyli pomnoz przez WeightsSum/Weights[ck], wiemy ze mianownik jest
            > SingleEpsilon, sprawdzilismy to juz wczesniej). }
          TracedCol := TracedCol * Colors[ck];
          TracedCol := TracedCol * (WeightsSum / Weights[ck]);

          Result := Result + TracedCol;
        end;

        { dodaj DirectIllumination }
        Result := Result + DirectIllumination;

        { Jezeli weszlismy tu dzieki rosyjskiej ruletce (a wiec jezeli Depth <= 0)
          to skaluj Result zeby zapisany tu estymator byl unbiased. }
        if Depth <= 0 then Result := Result * (1/RRoulContinue);
      end;
    end;

  var
    i: Integer;
    NonEmissiveColor: TVector3;
  begin
    IntersectNode := Octree.RayCollision(Intersection, RayOrigin, RayDirection, true,
      TriangleToIgnore, IgnoreMarginAtStart, nil);
    if IntersectNode = nil then Exit(SceneBackgroundColor);

    if TraceOnlyIndirect and IsLightSource(IntersectNode^) then
    begin
      Result := TVector3.Zero;
      Exit;
    end;

    MaterialInfo := IntersectNode^.MaterialInfo;

    { de facto jezeli TraceOnlyIndirect to ponizsza linijka na pewno dodaje
      do result (0, 0, 0). Ale nie widze w tej chwili jak z tego wyciagnac
      jakas specjalna optymalizacje.

      We use below EmissiveColor(), not MaterialInfo.EmissiveColor,
      because this is done even when MaterialInfo is nil. }
    Result := EmissiveColor(IntersectNode^);

    if MaterialInfo <> nil then
    begin
      { jezeli MinDepth = Depth to znaczy ze nasz Trace zwraca kolor dla primary ray.
        Wiec rozgaleziamy sie tutaj na NonPrimarySamplesCount, czyli dzialamy
          jakbysmy byly stochastycznym ray tracerem ktory rozgalezia sie
          na wiele promieni w punkcie rekursji.
        Wpp. idziemy sciezka czyli dzialamy jakbysmy byly path tracerem czyli
          nie rozgaleziamy sie na wiele promieni. }
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
    { generuj pixel x, y. calculate PixColor }
    if PrimarySamplesCount = 1 then
    begin
      { gdy PrimarySamplesCount = 1 to wysylamy jeden promien pierwotny
        i ten promien NIE jest losowany na rzutni w zakresie pixela
        x, y ale przechodzi dokladnie przez srodek pixela x, y. }
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

    { zapisz PixColor do Image }
    C := Image.Colors[X, Y, 0];
    ColRGB := PixColor;
    Image.Colors[X, Y, 0] := C;
  end;

var
  PixCoord: TVector2Cardinal;
  SFCurve: TSpaceFillingCurve;
begin
  SceneBackgroundColor := GetSceneBackgroundColor(Background, SceneBGColor);

  { check parameters (path tracing i tak trwa bardzo dlugo wiec mozemy sobie
    pozwolic zeby na poczatku tej procedury wykonac kilka testow, nawet gdy
    kompilujemy sie w wersji RELEASE) }
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
    LightItems := TFPList.Create;
    LightItems.Capacity := Octree.TrianglesCount div 4;
    CollectedLightItems := LightItems;
    Octree.EnumerateTriangles(@CollectLightItems);

    {$ifdef PATHTR_USES_SHADOW_CACHE}
    { calculate ShadowCache }
    ShadowCache := TFPList.Create;
    ShadowCache.Count := LightItems.Count;
    { Setting TFPList.Count already makes sure that new pointers are nil }
    {$endif}

    { calculate RaysWindow }
    RaysWindow := TRaysWindow.CreateDescendant(CamPosition,
      CamDirection.Normalize, CamUp.Normalize, Projection);

    { calculate SFCurve }
    SFCurve := SFCurveClass.Create(Image.Width, Image.Height);
    SFCurve.SkipPixels(FirstPixel);

    { generuj pixle obrazka }
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

end.
