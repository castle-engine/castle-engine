{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Background for 3D world (TBackground). }
unit CastleBackground;

{$I castleconf.inc}

interface

uses CastleVectors, SysUtils, CastleUtils, CastleImages, X3DNodes,
  CastleColors, CastleGLUtils, CastleTransform;

type
  { Background for 3D world.
    Background defined here has the same features as VRML/X3D Background:

    @unorderedList(
      @itemSpacing Compact
      @item(skybox - a cube with each face potentially textured
        (textures may have alpha channel))
      @item a ground sphere around this, with color rings for ground colors
      @item a sky sphere around this, with color rings for sky colors
    )

    Engine users should not use this class directly. Instead TCastleSceneManager
    automatically uses this to render the background defined by
    TCastleSceneManager.MainScene. }
  TBackground = class
  private
    { TCastleScene to render the background in most cases.
      Cannot be declared as TCastleScene as it would create a circular dependency
      with CastleScene unit. }
    SceneObj: TObject;
    ParamsObj: TObject;
    ClearColor: TCastleColor;
    MatrixTransform: TMatrixTransformNode;
  public
    { Calculate (or just confirm that Proposed value is still OK)
      the sky sphere radius that fits nicely in your projection near/far.

      Background spheres (for sky and ground) are rendered at given radius.
      And inside these spheres, we have a cube (to apply background textures).
      Both spheres and cube must fit nicely within your projection near/far
      to avoid any artifacts.

      We first check is Proposed a good result value (it satisfies
      the conditions, with some safety margin). If yes, then we return
      exactly the Proposed value. Otherwise, we calculate new value
      as an average in our range.
      This way, if you already had sky sphere radius calculated
      (and prepared some OpenGL resources for it),
      and projection near/far changes very slightly
      (e.g. because bounding box slightly changed), then you don't have
      to recreate background --- if the old sky sphere radius is still OK,
      then the old background resources are still OK.

      Just pass Proposed = 0 (or anything else that is always outside
      the range) if you don't need this feature. }
    class function NearFarToSkySphereRadius(const zNear, zFar: Single;
      const Proposed: Single = 0): Single;

    constructor Create;
    destructor Destroy; override;
    procedure Update(const Node: TAbstractBackgroundNode;
      const SkySphereRadius: Single);
    procedure Render(const RenderingCamera: TRenderingCamera;
      const Wireframe: boolean);
    procedure UpdateTransform(const Transform: TMatrix4);
    procedure FreeResources;
  end;

implementation

uses CastleLog, CastleScene, X3DFields, Math, CastleSceneCore;

const
  { Relation of a cube size and a radius of it's bounding sphere.

    Sphere surrounds the cube, such that 6 cube corners touch the sphere.
    So cube diameter = 2 * sphere radius.
    Cube diameter = sqrt(sqr(cube size) + sqr(cube face diameter)),
    and cube face diameter = sqrt(2) * cube size.
    This gives constants below. }
  SphereRadiusToCubeSize = 2 / Sqrt(3);
  CubeSizeToSphereRadius = Sqrt(3) / 2;

{ TBackground ------------------------------------------------------------ }

class function TBackground.NearFarToSkySphereRadius(const zNear, zFar: Single;
  const Proposed: Single): Single;

{ Conditions are ZNear < CubeSize/2, ZFar > SphereRadius.
  So conditions for radius are

    ZNear * 2 * CubeSizeToSphereRadius < SphereRadius < ZFar

  Note that 2 * CubeSizeToSphereRadius is Sqrt(3) =~ 1.7,
  so it's possible to choose
  ZNear <= ZFar that still yield no possible radius.

  It would be possible to avoid whole need for this method
  by setting projection matrix in our own render. But then,
  you'd have to pass fovy and such parameters to the background renderer.
}

var
  Min, Max, SafeMin, SafeMax: Single;
begin
  Min := zNear * 2 * CubeSizeToSphereRadius;
  Max := zFar;

  { The new sphere radius should be in [Min...Max].
    For maximum safety (from floating point troubles), we require
    that it's within slightly smaller "safe" range. }

  SafeMin := Lerp(0.1, Min, Max);
  SafeMax := Lerp(0.9, Min, Max);

  if (Proposed >= SafeMin) and
     (Proposed <= SafeMax) then
    Result := Proposed else
    Result := (Min + Max) / 2;
end;

{$define Scene := TCastleScene(SceneObj)}
{$define Params := TBasicRenderParams(ParamsObj)}

constructor TBackground.Create;
begin
  inherited;
  Scene := TCastleScene.Create(nil);
  { We don't need depth test (we put our shapes in proper order),
    we even don't want it (because we don't clear depth buffer
    before drawing, so it may contain the depths on 3D world rendered
    in previous frame). }
  Scene.Attributes.DepthTest := false;
  Params := TBasicRenderParams.Create;
end;

destructor TBackground.Destroy;
begin
  FreeAndNil(Scene);
  FreeAndNil(Params);
  inherited;
end;

procedure TBackground.Update(const Node: TAbstractBackgroundNode;
  const SkySphereRadius: Single);
var
  RootNode: TX3DRootNode;

  procedure RenderCubeSides;
  var
    CubeSize, CubeSize2: Single;

    procedure RenderTextureSide(const Side: TBackgroundSide);
    const
      Coords: array [TBackgroundSide, 0..3] of TVector3 =
      ( ((Data: ( 1, -1,  1)), (Data: (-1, -1,  1)), (Data: (-1,  1,  1)), (Data: ( 1,  1,  1))), {back}
        ((Data: (-1, -1,  1)), (Data: ( 1, -1,  1)), (Data: ( 1, -1, -1)), (Data: (-1, -1, -1))), {bottom}
        ((Data: (-1, -1, -1)), (Data: ( 1, -1, -1)), (Data: ( 1,  1, -1)), (Data: (-1,  1, -1))), {front}
        ((Data: (-1, -1,  1)), (Data: (-1, -1, -1)), (Data: (-1,  1, -1)), (Data: (-1,  1,  1))), {left}
        ((Data: ( 1, -1, -1)), (Data: ( 1, -1,  1)), (Data: ( 1,  1,  1)), (Data: ( 1,  1, -1))), {right}
        ((Data: (-1,  1, -1)), (Data: ( 1,  1, -1)), (Data: ( 1,  1,  1)), (Data: (-1,  1,  1)))  {top}
      );
      TexCoords: array [0..3] of TVector2 = (
        (Data: (0, 0)),
        (Data: (1, 0)),
        (Data: (1, 1)),
        (Data: (0, 1))
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

      MatrixTransform.AddChildren(Shape);
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

      MatrixTransform.AddChildren(Shape);
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

    SphereCoord.Items.List^[Start] := StackTipCalc(TipAngle);
    SphereColor.Items.List^[Start] := TipColor;
    Inc(Next);

    for I := 0 to Slices - 1 do
    begin
      SphereCoord.Items.List^[Next] := CirclePoint(CircleY, CircleRadius, I);
      SphereColor.Items.List^[Next] := CircleColor;
      Inc(Next);

      SphereCoordIndex.Items.List^[NextIndex    ] := Start;
      SphereCoordIndex.Items.List^[NextIndex + 1] := Start + 1 + I;
      if I <> Slices - 1 then
        SphereCoordIndex.Items.List^[NextIndex + 2] := Start + 2 + I else
        SphereCoordIndex.Items.List^[NextIndex + 2] := Start + 1;
      SphereCoordIndex.Items.List^[NextIndex + 3] := -1;
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
      SphereCoord.Items.List^[Next] := CirclePoint(CircleY, CircleRadius, I);
      SphereColor.Items.List^[Next] := CircleColor;
      Inc(Next);

      SphereCoordIndex.Items.List^[NextIndex    ] := Start + I;
      if I <> Slices - 1 then
      begin
        SphereCoordIndex.Items.List^[NextIndex + 1] := Start + 1 + I;
        SphereCoordIndex.Items.List^[NextIndex + 2] := Start + 1 + I - Slices;
      end else
      begin
        SphereCoordIndex.Items.List^[NextIndex + 1] := Start;
        SphereCoordIndex.Items.List^[NextIndex + 2] := Start - Slices;
      end;
      SphereCoordIndex.Items.List^[NextIndex + 3] := Start + I - Slices;
      SphereCoordIndex.Items.List^[NextIndex + 4] := -1;
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

    SphereCoord.Items.List^[Start] := StackTipCalc(TipAngle);
    SphereColor.Items.List^[Start] := TipColor;

    for I := 0 to Slices - 1 do
    begin
      SphereCoordIndex.Items.List^[NextIndex    ] := Start;
      if I <> Slices - 1 then
        SphereCoordIndex.Items.List^[NextIndex + 1] := Start - Slices + I + 1 else
        SphereCoordIndex.Items.List^[NextIndex + 1] := Start - Slices;
      SphereCoordIndex.Items.List^[NextIndex + 2] := Start - Slices + I;
      SphereCoordIndex.Items.List^[NextIndex + 3] := -1;
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
    Color := Node.FdSkyColor.Items.L;
    Angle := Node.FdSkyAngle.Items.L;

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
    Color := Node.FdGroundColor.Items.L;
    Angle := Node.FdGroundAngle.Items.L;

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

begin
  RootNode := TX3DRootNode.Create('', Node.BaseUrl);
  SphereCreated := false;

  MatrixTransform := TMatrixTransformNode.Create('', Node.BaseUrl);
  MatrixTransform.FdMatrix.Value := Node.TransformRotation;
  RootNode.AddChildren(MatrixTransform);

  RenderSky;
  RenderGround;
  RenderCubeSides;

  Scene.Load(RootNode, true);
end;

procedure TBackground.Render(const RenderingCamera: TRenderingCamera;
  const Wireframe: boolean);
begin
  Params.InShadow := false;
  { since we constructed Scene ourselves,
    we know it only has ShadowVolumesReceivers=true shapes }
  Params.ShadowVolumesReceivers := true;
  Params.RenderingCamera := RenderingCamera;

  if Wireframe then
    Scene.Attributes.WireframeEffect := weWireframeOnly else
    Scene.Attributes.WireframeEffect := weNormal;

  { TODO: in the old times, we had here an optimization:
    if the background is not displayed as Wireframe,
    and it has all 6 cube sides filled with textures without
    an alpha channel, then there's no need to display sky/ground spheres,
    and no need to even clear color buffer before.
    We lose this optimization now, since we don't know now which cube sides
    are successfully loaded and which have alpha. }
  RenderContext.Clear([cbColor], ClearColor);

  { We don't calculate correct Frustum (accounting for the fact that camera
    is rotated but never shifted during background rendering) now.
    But also frustum culling for this would not be very useful,
    so just disable it. }
  Scene.InternalIgnoreFrustum := true;

  Params.Transparent := false; Scene.Render(Params);
  Params.Transparent := true ; Scene.Render(Params);
end;

procedure TBackground.FreeResources;
begin
  Scene.FreeResources([frTextureDataInNodes]);
end;

procedure TBackground.UpdateTransform(const Transform: TMatrix4);
begin
  MatrixTransform.FdMatrix.Send(Transform);
end;

end.
