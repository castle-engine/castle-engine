{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering background (TBackgroundRenderer) based on X3D background node. }
unit CastleInternalBackgroundRenderer;

{$I castleconf.inc}

interface

uses CastleVectors, SysUtils, CastleUtils, CastleImages, X3DNodes,
  CastleColors, CastleGLUtils, CastleTransform, CastleRectangles, CastleProjection,
  CastleInternalShapesRenderer, CastleInternalRenderer;

type
  { Background under X3D scene.

    It supports all features of X3D nodes descending
    from TAbstractBackgroundNode.

    @unorderedList(
      @itemSpacing Compact
      @item(May be a skybox: A cube with each face potentially textured
        (textures may have alpha channel),)
      @item(Behind the skybox: you can see ground sphere,
        with color rings for ground colors,)
      @item(Behind the ground sphere: you can see sky sphere,
        with color rings for sky colors,)
      @item(May also be a single full-screen quad (for TImageBackgroundNode).)
    )

    This is an abstract class.
    Do not construct it explicitly.
    The only way to create non-abstract instances of this class
    is to use @link(CreateBackgroundRenderer).
  }
  TBackgroundRenderer = class abstract
  private
    { Calculate the sky sphere radius that fits nicely in given projection near/far.

      Background spheres (for sky and ground) are rendered at given radius.
      And inside these spheres, we have a cube (to apply background textures).
      Both spheres and cube must fit nicely within your projection near/far
      to avoid any artifacts. }
    class function NearFarToSkySphereRadius(const zNear, zFar: Single): Single;
  public
    procedure Render(const RenderingCamera: TRenderingCamera;
      const Wireframe: boolean;
      const RenderRect: TFloatRectangle;
      const CurrentProjection: TProjection;
      const ShapesCollector: TShapesCollector;
      const ShapesRenderer: TShapesRenderer); virtual;
    { Change rotation.
      Initially rotation is taken from TBackgroundNode.TransformRotation
      (corresponds to the rotation of Background in the X3D file transformation). }
    procedure UpdateRotation(const Rotation: TVector4); virtual;
    procedure FreeResources; virtual;
  end;

{ Create background renderer.
  Returns @nil if this Node class is not supported. }
function CreateBackgroundRenderer(const Node: TAbstractBackgroundNode): TBackgroundRenderer;

implementation

uses Math,
  CastleLog, CastleScene, X3DFields, CastleSceneCore, CastleGLImages,
  CastleRenderContext, CastleRenderOptions, CastleInternalGLUtils;

const
  { Background rendering doesn't use the same projection near/far as other content.
    It is not necessary (background doesn't check depth at all, it has DepthTest=false)
    and it makes things simpler (we can assume some box/sphere sizes
    for background, we don't need to make them dependent on current projection near/far). }
  BgProjectionNear = 1;
  BgProjectionFar = 100;

  { Relation of a cube size and a radius of it's bounding sphere.

    Sphere surrounds the cube, such that 6 cube corners touch the sphere.
    So cube diameter = 2 * sphere radius.
    Cube diameter = sqrt(sqr(cube size) + sqr(cube face diameter)),
    and cube face diameter = sqrt(2) * cube size.
    This gives constants below. }
  SphereRadiusToCubeSize = 2 / {$ifdef FPC}Sqrt(3){$else}1.732050807568877{$endif};
  CubeSizeToSphereRadius = {$ifdef FPC}Sqrt(3){$else}1.732050807568877{$endif} / 2;

{ TBackgroundRenderer ------------------------------------------------------------ }

class function TBackgroundRenderer.NearFarToSkySphereRadius(const zNear, zFar: Single): Single;

{ Conditions are ZNear < CubeSize/2, ZFar > SphereRadius.
  So conditions for radius are

    ZNear * 2 * CubeSizeToSphereRadius < SphereRadius < ZFar

  Note that 2 * CubeSizeToSphereRadius is Sqrt(3) =~ 1.7,
  so it's possible to choose
  ZNear <= ZFar that still yield no possible radius.
}

var
  Min, Max: Single;
begin
  Min := zNear * 2 * CubeSizeToSphereRadius;
  Max := zFar;
  Result := (Min + Max) / 2;
end;

procedure TBackgroundRenderer.Render(const RenderingCamera: TRenderingCamera;
  const Wireframe: boolean;
  const RenderRect: TFloatRectangle;
  const CurrentProjection: TProjection;
  const ShapesCollector: TShapesCollector;
  const ShapesRenderer: TShapesRenderer);
begin
end;

procedure TBackgroundRenderer.UpdateRotation(const Rotation: TVector4);
begin
end;

procedure TBackgroundRenderer.FreeResources;
begin
end;

{ TBackgroundScene ----------------------------------------------------------- }

type
  { Background implementation using internal TCastleScene to render.
    In overridden constructor, load Scene contents.
    You can also use ClearColor and UseClearColor if the background
    may be realized by simple clearing of the viewport with solid color. }
  TBackgroundScene = class(TBackgroundRenderer)
  protected
    Scene: TCastleScene;
    Params: TBasicRenderParams;
    ClearColor: TCastleColor;
    UseClearColor: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(const RenderingCamera: TRenderingCamera;
      const Wireframe: boolean;
      const RenderRect: TFloatRectangle;
      const CurrentProjection: TProjection;
      const ShapesCollector: TShapesCollector;
      const ShapesRenderer: TShapesRenderer); override;
    procedure FreeResources; override;
  end;

constructor TBackgroundScene.Create;
begin
  inherited;

  Scene := TCastleScene.Create(nil);
  { We don't need depth test (we put our shapes in proper order),
    we even don't want it (because we don't clear depth buffer
    before drawing, so it may contain the depths on 3D world rendered
    in previous frame). }
  Scene.RenderOptions.DepthTest := false;
  { We may share some nodes with the main scene.
    And both scenes must have Static=false (as we will change them,
    e.g. in TBackground3D.UpdateRotation or TBackground2D.Render (UpdateProperties)).
    So, for now, just hide the warning about
    "You cannot use the same X3D node in multiple instances of TCastleScene".
    Testcase: demo-models/background/ with ImageBackground or TextureBackground }
  Scene.InternalNodeSharing := true;
  Params := TBasicRenderParams.Create;
end;

destructor TBackgroundScene.Destroy;
begin
  FreeAndNil(Scene);
  FreeAndNil(Params);
  inherited;
end;

procedure TBackgroundScene.Render(const RenderingCamera: TRenderingCamera;
  const Wireframe: boolean; const RenderRect: TFloatRectangle;
  const CurrentProjection: TProjection;
  const ShapesCollector: TShapesCollector;
  const ShapesRenderer: TShapesRenderer);
var
  SavedOcclusionCulling: Boolean;
  SavedBlendingSort, SavedOcclusionSort: TShapeSort;
begin
  inherited;

  Params.InShadow := false;
  Params.ShadowVolumesReceivers := [false, true];
  Params.RenderingCamera := RenderingCamera;
  Params.Collector := ShapesCollector;
  Params.RendererToPrepareShapes := ShapesRenderer.Renderer;

  if Wireframe then
    Scene.RenderOptions.WireframeEffect := weWireframeOnly
  else
    Scene.RenderOptions.WireframeEffect := weNormal;

  if UseClearColor then
    RenderContext.Clear([cbColor], ClearColor);

  { We don't calculate correct Params.Frustum (accounting for the fact that camera
    is rotated but never shifted during 3D background rendering) now.
    But also frustum culling for this would not be very useful,
    so just disable it by leaving Params.Frustum = nil. }
  //Params.Frustum := nil; // this is actually the default, we never set Params.Frustum
  Assert(Params.Frustum = nil);

  ShapesCollector.Clear;

  { Scene.Render in this case should call Scene.LocalRender straight away,
    as Scene has no transformation.
    And that's good, the TCastleTransform.Render could not handle any transformation
    as Params.Frustum is nil.  }
  Params.Transparent := false; Scene.Render(Params);
  Params.Transparent := true ; Scene.Render(Params);

  { Disable occlusion culling on background.
    It works correctly... but the 1-frame delay is too noticeable,
    testcase: examples/fps_game/ . }
  SavedOcclusionCulling := ShapesRenderer.OcclusionCulling;
  ShapesRenderer.OcclusionCulling := false;

  SavedBlendingSort := ShapesRenderer.BlendingSort;
  ShapesRenderer.BlendingSort := sortNone;

  SavedOcclusionSort := ShapesRenderer.OcclusionSort;
  ShapesRenderer.OcclusionSort := sortNone;

  ShapesRenderer.Render(ShapesCollector, Params);

  ShapesRenderer.OcclusionCulling := SavedOcclusionCulling;
  ShapesRenderer.BlendingSort := SavedBlendingSort;
  ShapesRenderer.OcclusionSort := SavedOcclusionSort;
end;

procedure TBackgroundScene.FreeResources;
begin
  inherited;
  Scene.FreeResources([frTextureDataInNodes]);
end;

{ TBackground3D --------------------------------------------------------------- }

type
  TBackground3D = class(TBackgroundScene)
  strict private
    Transform: TTransformNode;
  public
    constructor Create(const Node: TAbstract3DBackgroundNode);
    procedure UpdateRotation(const Rotation: TVector4); override;
    procedure Render(const RenderingCamera: TRenderingCamera;
      const Wireframe: boolean;
      const RenderRect: TFloatRectangle;
      const CurrentProjection: TProjection;
      const ShapesCollector: TShapesCollector;
      const ShapesRenderer: TShapesRenderer); override;
  end;

constructor TBackground3D.Create(
  const Node: TAbstract3DBackgroundNode);
var
  SkySphereRadius: Single;

  procedure RenderCubeSides;
  var
    CubeSize, CubeSize2: Single;

    procedure RenderTextureSide(const Side: TBackgroundSide);
    const
      Coords: array [TBackgroundSide, 0..3] of TVector3 =
      ( ((X: 1; Y: -1; Z:  1), (X:-1; Y: -1; Z:  1), (X:-1; Y:  1; Z:  1), (X: 1; Y:  1; Z:  1)), {back}
        ((X:-1; Y: -1; Z:  1), (X: 1; Y: -1; Z:  1), (X: 1; Y: -1; Z: -1), (X:-1; Y: -1; Z: -1)), {bottom}
        ((X:-1; Y: -1; Z: -1), (X: 1; Y: -1; Z: -1), (X: 1; Y:  1; Z: -1), (X:-1; Y:  1; Z: -1)), {front}
        ((X:-1; Y: -1; Z:  1), (X:-1; Y: -1; Z: -1), (X:-1; Y:  1; Z: -1), (X:-1; Y:  1; Z:  1)), {left}
        ((X: 1; Y: -1; Z: -1), (X: 1; Y: -1; Z:  1), (X: 1; Y:  1; Z:  1), (X: 1; Y:  1; Z: -1)), {right}
        ((X:-1; Y:  1; Z: -1), (X: 1; Y:  1; Z: -1), (X: 1; Y:  1; Z:  1), (X:-1; Y:  1; Z:  1))  {top}
      );
      TexCoords: array [0..3] of TVector2 = (
        (X: 0; Y: 0),
        (X: 1; Y: 0),
        (X: 1; Y: 1),
        (X: 0; Y: 1)
      );
    var
      Shape: TShapeNode;
      Appearance: TAppearanceNode;
      QuadSet: TQuadSetNode;
      Coord: TCoordinateNode;
      TexCoord: TTextureCoordinateNode;
      Texture: TAbstractTextureNode;
      V: TVector3;
    begin
      Texture := Node.Texture(Side);
      if Texture = nil then Exit;

      Coord := TCoordinateNode.Create('', Node.BaseUrl);
      for V in Coords[Side] do
        Coord.FdPoint.Items.Add(V * CubeSize2);

      TexCoord := TTextureCoordinateNode.Create('', Node.BaseUrl);
      TexCoord.FdPoint.Send(TexCoords);

      QuadSet := TQuadSetNode.Create('', Node.BaseUrl);
      QuadSet.Coord := Coord;
      QuadSet.TexCoord := TexCoord;

      Appearance := TAppearanceNode.Create('', Node.BaseUrl);
      Appearance.FdShaders.AssignValue(Node.FdShaders);
      Appearance.FdEffects.AssignValue(Node.FdEffects);
      Appearance.Texture := Texture;
      if Texture is TAbstractTexture2DNode then
      begin
        { We have to change repeat mode of this texture, even if it came from
          TTextureBackgroundNode. The only reasonable way to render background
          is to use clamp mode. More correct alternative would be creating
          a copy of node in case of TTextureBackgroundNode,
          but this would often be wasteful --- the background texture is
          probably not DEF/USEd in other places (that need repeat mode),
          and it's probably repeat=true by accident (since this is the default value). }
        TAbstractTexture2DNode(Texture).RepeatS := false;
        TAbstractTexture2DNode(Texture).RepeatT := false;
      end;

      Shape := TShapeNode.Create('', Node.BaseUrl);
      Shape.FdGeometry.Value := QuadSet;
      Shape.Appearance := Appearance;

      Transform.AddChildren(Shape);
    end;

  var
    BS: TBackgroundSide;
  begin
    CubeSize := SkySphereRadius * SphereRadiusToCubeSize;
    CubeSize2 := CubeSize / 2;
    for BS := Low(BS) to High(BS) do RenderTextureSide(BS);
  end;

var
  SphereCreated: boolean;
  SphereCoord: TMFVec3f;
  SphereCoordIndex: TMFInt32;
  SphereColor: TMFColor;

  procedure NeedsSphere;
  var
    Coord: TCoordinateNode;
    Color: TColorNode;
    Geometry: TIndexedFaceSetNode;
    Shape: TShapeNode;
  begin
    { add a mesh for sphere, if not present already }
    if not SphereCreated then
    begin
      SphereCreated := true;

      Coord := TCoordinateNode.Create('', Node.BaseUrl);
      SphereCoord := Coord.FdPoint;

      Color := TColorNode.Create('', Node.BaseUrl);
      SphereColor := Color.FdColor;

      Geometry := TIndexedFaceSetNode.Create('', Node.BaseUrl);
      Geometry.FdCoord.Value := Coord;
      Geometry.FdColor.Value := Color;
      Geometry.FdSolid.Value := false;
      SphereCoordIndex := Geometry.FdCoordIndex;

      Shape := TShapeNode.Create('', Node.BaseUrl);
      Shape.FdGeometry.Value := Geometry;

      Transform.AddChildren(Shape);
    end;
  end;

const
  { slices of rings rendered in Render*Stack }
  Slices = 24;

  { For given Angle (meaning: 0 = zenith, Pi = nadir), calculate the height
    and radius of given circle of sky sphere. }
  procedure StackCircleCalc(const Angle: Single; out Y, Radius: Single);
  var
    S, C: Extended;
  begin
    SinCos(Angle, S, C);
    Radius := S * SkySphereRadius;
    Y := C * SkySphereRadius;
  end;

  function StackTipCalc(const Angle: Single): TVector3;
  begin
    // Result := Vector3(0, Cos(Angle) * SkySphereRadius, 0);
    { simpler and more accurate version, since StackTipCalc is only called with
      Angle = 0 or Pi }
    if Angle = 0 then
      Result := Vector3(0,  SkySphereRadius, 0) else
    begin
      Assert(Angle = Single(Pi));
      Result := Vector3(0, -SkySphereRadius, 0);
    end;
  end;

  function CirclePoint(const Y, Radius: Single; const SliceIndex: Integer): TVector3;
  var
    S, C: Extended;
  begin
    SinCos(SliceIndex * 2 * Pi / Slices, S, C);
    Result := Vector3(S * Radius, Y, C * Radius);
  end;

  { Render*Stack: render one stack of sky/ground sphere.
    Angles are given in the sky connvention : 0 is zenith, Pi is nadir. }

  procedure RenderFirstStack(
    const TipColor   : TVector3; const TipAngle   : Single;
    const CircleColor: TVector3; const CircleAngle: Single);
  var
    CircleY, CircleRadius: Single;
    I, Start, Next, StartIndex, NextIndex: Integer;
  begin
    Start := SphereCoord.Count;
    Next := Start;
    Assert(Start = SphereColor.Count);
    SphereCoord.Count := Start + Slices + 1;
    SphereColor.Count := Start + Slices + 1;

    StartIndex := SphereCoordIndex.Count;
    NextIndex := StartIndex;
    SphereCoordIndex.Count := SphereCoordIndex.Count + Slices * 4;

    StackCircleCalc(CircleAngle, CircleY, CircleRadius);

    SphereCoord.Items.L[Start] := StackTipCalc(TipAngle);
    SphereColor.Items.L[Start] := TipColor;
    Inc(Next);

    for I := 0 to Slices - 1 do
    begin
      SphereCoord.Items.L[Next] := CirclePoint(CircleY, CircleRadius, I);
      SphereColor.Items.L[Next] := CircleColor;
      Inc(Next);

      SphereCoordIndex.Items.L[NextIndex    ] := Start;
      SphereCoordIndex.Items.L[NextIndex + 1] := Start + 1 + I;
      if I <> Slices - 1 then
        SphereCoordIndex.Items.L[NextIndex + 2] := Start + 2 + I else
        SphereCoordIndex.Items.L[NextIndex + 2] := Start + 1;
      SphereCoordIndex.Items.L[NextIndex + 3] := -1;
      NextIndex := NextIndex + 4;
    end;
  end;

  procedure RenderNextStack(
    const CircleColor: TVector3; const CircleAngle: Single);
  var
    CircleY, CircleRadius: Single;
    I, Start, Next, StartIndex, NextIndex: Integer;
  begin
    Start := SphereCoord.Count;
    Next := Start;
    Assert(Start = SphereColor.Count);
    SphereCoord.Count := Start + Slices;
    SphereColor.Count := Start + Slices;

    StartIndex := SphereCoordIndex.Count;
    NextIndex := StartIndex;
    SphereCoordIndex.Count := SphereCoordIndex.Count + Slices * 5;

    StackCircleCalc(CircleAngle, CircleY, CircleRadius);

    for I := 0 to Slices - 1 do
    begin
      SphereCoord.Items.L[Next] := CirclePoint(CircleY, CircleRadius, I);
      SphereColor.Items.L[Next] := CircleColor;
      Inc(Next);

      SphereCoordIndex.Items.L[NextIndex    ] := Start + I;
      if I <> Slices - 1 then
      begin
        SphereCoordIndex.Items.L[NextIndex + 1] := Start + 1 + I;
        SphereCoordIndex.Items.L[NextIndex + 2] := Start + 1 + I - Slices;
      end else
      begin
        SphereCoordIndex.Items.L[NextIndex + 1] := Start;
        SphereCoordIndex.Items.L[NextIndex + 2] := Start - Slices;
      end;
      SphereCoordIndex.Items.L[NextIndex + 3] := Start + I - Slices;
      SphereCoordIndex.Items.L[NextIndex + 4] := -1;
      NextIndex := NextIndex + 5;
    end;
  end;

  procedure RenderLastStack(
    const TipColor: TVector3; const TipAngle: Single);
  var
    I, Start, StartIndex, NextIndex: Integer;
  begin
    Start := SphereCoord.Count;
    Assert(Start = SphereColor.Count);
    SphereCoord.Count := Start + 1;
    SphereColor.Count := Start + 1;

    StartIndex := SphereCoordIndex.Count;
    NextIndex := StartIndex;
    SphereCoordIndex.Count := SphereCoordIndex.Count + Slices * 4;

    SphereCoord.Items.L[Start] := StackTipCalc(TipAngle);
    SphereColor.Items.L[Start] := TipColor;

    for I := 0 to Slices - 1 do
    begin
      SphereCoordIndex.Items.L[NextIndex    ] := Start;
      if I <> Slices - 1 then
        SphereCoordIndex.Items.L[NextIndex + 1] := Start - Slices + I + 1 else
        SphereCoordIndex.Items.L[NextIndex + 1] := Start - Slices;
      SphereCoordIndex.Items.L[NextIndex + 2] := Start - Slices + I;
      SphereCoordIndex.Items.L[NextIndex + 3] := -1;
      NextIndex := NextIndex + 4;
    end;
  end;

  procedure RenderSky;
  var
    I, ColorCount, AngleCount: Integer;
    Angle: PSingle;
    Color: PVector3;
    GroundHighestAngle: Single;
  begin
    { calculate GroundHighestAngle, will be usable to optimize rendering sky.
      GroundHighestAngle is measured in sky convention (0 = zenith, Pi = nadir).
      If there is no sky I simply set GroundHighestAngle to sthg > Pi. }
    if Node.FdGroundAngle.Count <> 0 then
      GroundHighestAngle := Pi - Node.FdGroundAngle.Items.Last else
      GroundHighestAngle := Pi + 1;

    ColorCount := Node.FdSkyColor.Count;
    AngleCount := Node.FdSkyAngle.Count;
    Color := PVector3(Node.FdSkyColor.Items.L);
    Angle := PSingle(Node.FdSkyAngle.Items.L);

    if ColorCount <= 0 then
    begin
      WritelnWarning('VRML/X3D', 'Background node incorrect: Sky must have at least one color');
      Exit;
    end else
    if AngleCount + 1 <> ColorCount then
    begin
      WritelnWarning('VRML/X3D', 'Background node incorrect: Sky must have exactly one more Color than Angles');
      { We know now that ColorCount >= 1, and of course AngleCount >= 0
        (since array always has >= 0 items). So we correct one of them to be
        smaller. }
      if AngleCount + 1 > ColorCount then
        AngleCount := ColorCount - 1 else
        ColorCount := AngleCount + 1;
    end;

    Assert(ColorCount >= 1);
    Assert(AngleCount + 1 = ColorCount);
    ClearColor := Vector4(Color[0], 1.0);

    UseClearColor := ColorCount = 1;

    if ColorCount > 1 then
    begin
      { When ColorCount >= 2, the idea of rendering is to do:
        - RenderFirstStack
        - RenderNextStack many times
        - RenderLastStack
        But we try to break this early, to not waste time rendering
        something that will be covered anyway by the ground sphere,
        using GroundHighestAngle. }

      NeedsSphere;

      RenderFirstStack(Color[0], 0,
                       Color[1], Angle[0]);
      for I := 1 to AngleCount - 1 do
      begin
        if Angle[I - 1] > GroundHighestAngle then Break;
        RenderNextStack(Color[I + 1], Angle[I]);
      end;
      { Close the tip of the sky sphere with constant color (last on Color[] table).
        Add 0.01 epsilon, in case GroundHighestAngle is very close to the
        last sky angle. Better to make RenderLastStack then, to avoid bad artifacts
        (see https://github.com/castle-engine/castle-engine/issues/79 ). }
      if Angle[AngleCount - 1] <= GroundHighestAngle + 0.01 then
        RenderLastStack(Color[ColorCount - 1], Pi);
    end;
  end;

  procedure RenderGround;
  var
    I: Integer;
    ColorCount, AngleCount: Integer;
    Angle: PSingle;
    Color: PVector3;
  begin
    ColorCount := Node.FdGroundColor.Count;
    AngleCount := Node.FdGroundAngle.Count;
    Color := PVector3(Node.FdGroundColor.Items.L);
    Angle := PSingle(Node.FdGroundAngle.Items.L);

    if AngleCount <> 0 then
    begin
      if AngleCount + 1 <> ColorCount then
      begin
        WritelnWarning('VRML/X3D', 'Background node incorrect: Ground must have exactly one more Color than Angles');
        if AngleCount + 1 > ColorCount then
          AngleCount := ColorCount - 1 else
          ColorCount := AngleCount + 1;
      end;
      Assert(AngleCount + 1 = ColorCount);

      NeedsSphere;

      RenderFirstStack(Color[0], Pi,
                       Color[1], Pi - Angle[0]);
      for I := 1 to AngleCount - 1 do
        RenderNextStack(Color[I + 1], Pi - Angle[I]);
    end;
  end;

var
  RootNode: TX3DRootNode;
begin
  inherited Create;

  SkySphereRadius := NearFarToSkySphereRadius(BgProjectionNear, BgProjectionFar);
  SphereCreated := false;

  RootNode := TX3DRootNode.Create('', Node.BaseUrl);

  Transform := TTransformNode.Create('', Node.BaseUrl);
  Transform.Rotation := Node.TransformRotation;
  RootNode.AddChildren(Transform);

  RenderSky;
  RenderGround;
  RenderCubeSides;

  Scene.Load(RootNode, true);
end;

procedure TBackground3D.UpdateRotation(const Rotation: TVector4);
begin
  inherited;
  Transform.Rotation := Rotation;
end;

procedure TBackground3D.Render(const RenderingCamera: TRenderingCamera;
  const Wireframe: boolean;
  const RenderRect: TFloatRectangle;
  const CurrentProjection: TProjection;
  const ShapesCollector: TShapesCollector;
  const ShapesRenderer: TShapesRenderer);
var
  SavedProjectionMatrix: TMatrix4;
  AspectRatio: Single;
  TempProjection: TProjection;
begin
  { Change projection.

    Reason when ptOrthographic:
    The background rendering doesn't like custom orthographic Dimensions.
    They could make the background sky box very small, such that it
    doesn't fill the screen. See e.g. x3d/empty_with_background_ortho.x3dv
    testcase. So temporary set good perspective projection.

    Reason in both cases:
    We need to set our own BgProjectionNear/Far,
    in order for SkySphereRadius be valid. }

  SavedProjectionMatrix := RenderContext.ProjectionMatrix;

  AspectRatio := RenderRect.Width / RenderRect.Height;
  if CurrentProjection.ProjectionType = ptOrthographic then
  begin
    RenderContext.ProjectionMatrix := PerspectiveProjectionMatrixDeg(
      45, AspectRatio, BgProjectionNear, BgProjectionFar)
  end else
  begin
    TempProjection := CurrentProjection;
    TempProjection.ProjectionNear := BgProjectionNear;
    TempProjection.ProjectionFar := BgProjectionFar;
    RenderContext.ProjectionMatrix := TempProjection.Matrix(AspectRatio);
  end;

  inherited;

  RenderContext.ProjectionMatrix := SavedProjectionMatrix;
end;

{ TBackground2D --------------------------------------------------------------- }

type
  TBackground2D = class(TBackgroundScene)
  strict private
    Node: TImageBackgroundNode;
    Shape: TShapeNode;
    Texture2D: TAbstractTexture2DNode;
    TexCoords: TTextureCoordinateNode;
    IndexedFaceSet: TIndexedFaceSetNode;
    Material: TUnlitMaterialNode;
  public
    constructor Create(const ANode: TImageBackgroundNode);
    destructor Destroy; override;
    procedure Render(const RenderingCamera: TRenderingCamera;
      const Wireframe: boolean;
      const RenderRect: TFloatRectangle;
      const CurrentProjection: TProjection;
      const ShapesCollector: TShapesCollector;
      const ShapesRenderer: TShapesRenderer); override;
  end;

constructor TBackground2D.Create(const ANode: TImageBackgroundNode);
var
  RootNode: TX3DRootNode;
  Coordinate: TCoordinateNode;
begin
  inherited Create;

  Node := ANode;

  if Node.Texture is TAbstractTexture2DNode then // also checks is it non-nil
    Texture2D := TAbstractTexture2DNode(Node.Texture)
  else
    Texture2D := nil;

  if Texture2D <> nil then
  begin
    RootNode := TX3DRootNode.Create;

    Coordinate := TCoordinateNode.Create;
    Coordinate.SetPoint([
      Vector3(-1, -1, 0),
      Vector3( 1, -1, 0),
      Vector3( 1,  1, 0),
      Vector3(-1,  1, 0)
    ]);

    TexCoords := TTextureCoordinateNode.Create;

    IndexedFaceSet := TIndexedFaceSetNode.CreateWithShape(Shape);
    IndexedFaceSet.Coord := Coordinate;
    IndexedFaceSet.TexCoord := TexCoords;
    IndexedFaceSet.SetCoordIndex([0, 1, 2, 3]);

    Shape.Appearance := TAppearanceNode.Create;
    Shape.Appearance.Texture := Texture2D;

    Material := TUnlitMaterialNode.Create;
    Shape.Appearance.Material := Material;

    RootNode.AddChildren(Shape);

    Scene.Load(RootNode, true);
  end;
end;

destructor TBackground2D.Destroy;
begin
  inherited;
end;

procedure TBackground2D.Render(const RenderingCamera: TRenderingCamera;
  const Wireframe: boolean;
  const RenderRect: TFloatRectangle;
  const CurrentProjection: TProjection;
  const ShapesCollector: TShapesCollector;
  const ShapesRenderer: TShapesRenderer);

  { Apply various Node properties at the beginning of each Render,
    this way we can animate them.
    See demo-models/background/background_image_animated.x3d example. }
  procedure UpdateProperties;
  begin
    Assert(Node <> nil);
    Assert(Texture2D <> nil);

    Material.EmissiveColor := Node.Color.XYZ;
    Material.Transparency := 1 - Node.Color.W;

    // TODO: check do they differ
    TexCoords.SetPoint(Node.FdTexCoords.Items);
  end;

var
  SavedProjectionMatrix, SavedModelviewMatrix: TMatrix4;
begin
  if Texture2D = nil then Exit;

  UpdateProperties;

  { Our Scene geometry assumes that modelview matrix is identity }
  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    glLoadMatrix(TMatrix4.Identity);
    {$endif}
  end;
  SavedModelviewMatrix := RenderingCamera.RotationMatrix;
  RenderingCamera.RotationMatrix := TMatrix4.Identity;

  { Our Scene geometry assumes an orthographic projection (-1,-1) - (1,1) }
  SavedProjectionMatrix := RenderContext.ProjectionMatrix;
  OrthoProjection(FloatRectangle(-1, -1, 2, 2));

  inherited;

  RenderContext.ProjectionMatrix := SavedProjectionMatrix;
  RenderingCamera.RotationMatrix := SavedModelviewMatrix;
end;

{ global routines ------------------------------------------------------------ }

function CreateBackgroundRenderer(const Node: TAbstractBackgroundNode): TBackgroundRenderer;
begin
  if Node is TAbstract3DBackgroundNode then
    Result := TBackground3D.Create(TAbstract3DBackgroundNode(Node))
  else
  if Node is TImageBackgroundNode then
    Result := TBackground2D.Create(TImageBackgroundNode(Node))
  else
    Result := nil;
end;

end.
