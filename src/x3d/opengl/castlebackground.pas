{
  Copyright 2002-2013 Michalis Kamburelis.

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

interface

uses CastleVectors, SysUtils, CastleUtils, CastleImages, X3DNodes,
  CastleFrustum, CastleColors, CastleGLUtils;

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
    procedure Render(const Frustum: TFrustum);
  end;

implementation

uses CastleWarnings, CastleScene;

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
  CubeSize, CubeSize2: Single;
  RootNode: TX3DRootNode;
  MatrixTransform: TMatrixTransformNode;

  procedure RenderTextureSide(const Side: TBackgroundSide);
  const
    Coords: array [TBackgroundSide, 0..3] of TVector3Single =
    ( (( 1, -1,  1), (-1, -1,  1), (-1,  1,  1), ( 1,  1,  1)), {back}
      ((-1, -1,  1), ( 1, -1,  1), ( 1, -1, -1), (-1, -1, -1)), {bottom}
      ((-1, -1, -1), ( 1, -1, -1), ( 1,  1, -1), (-1,  1, -1)), {front}
      ((-1, -1,  1), (-1, -1, -1), (-1,  1, -1), (-1,  1,  1)), {left}
      (( 1, -1, -1), ( 1, -1,  1), ( 1,  1,  1), ( 1,  1, -1)), {right}
      ((-1,  1, -1), ( 1,  1, -1), ( 1,  1,  1), (-1,  1,  1))  {top}
    );
    TexCoords: array [0..3] of TVector2Single = ((0, 0), (1, 0), (1, 1), (0, 1));
  var
    Shape: TShapeNode;
    Appearance: TAppearanceNode;
    QuadSet: TQuadSetNode;
    Coord: TCoordinateNode;
    TexCoord: TTextureCoordinateNode;
    Texture: TAbstractTextureNode;
    V: TVector3Single;
  begin
    Texture := Node.Texture(Side);
    if Texture = nil then Exit;

    Coord := TCoordinateNode.Create('', Node.BaseUrl);
    for V in Coords[Side] do
      Coord.FdPoint.Items.Add(V * CubeSize2);

    TexCoord := TTextureCoordinateNode.Create('', Node.BaseUrl);
    TexCoord.FdPoint.Send(TexCoords);

    QuadSet := TQuadSetNode.Create('', Node.BaseUrl);
    QuadSet.FdCoord.Value := Coord;
    QuadSet.FdTexCoord.Value := TexCoord;

    Appearance := TAppearanceNode.Create('', Node.BaseUrl);
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

// TODO-background: do we need to set REPLACE (e.g. by MultiTexture.mode),
// or are we already optimized Ok for unlit case to just use white color?
// check.

    { Wybieramy GL_REPLACE bo scianki szescianu beda zawsze cale teksturowane
      i chcemy olac zupelnie kolor/material jaki bedzie na tych sciankach.
      Chcemy wziac to z tekstury (dlatego standardowe GL_MODULATE nie jest dobre).
      Ponadto, kanal alpha tez chcemy wziac z tekstury, tzn. szescian
      nieba ma byc przeswitujacy gdy tekstura bedzie przeswitujaca
      (dlatego GL_DECAL nie jest odpowiedni). }
//    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

    MatrixTransform.FdChildren.Add(Shape);
  end;

(*
  { For given Angle (meaning: 0 = zenith, Pi = nadir), calculate the height
    and radius of given circle of sky sphere. }
  procedure StackCircleCalc(const Angle: Single; out Y, Radius: Single);
  begin
    Radius := sin(Angle) * SkySphereRadius;
    Y := cos(Angle) * SkySphereRadius;
  end;

  { Render*Stack: render one stack of sky/ground sphere.
    Angles are given in the sky connvention : 0 is zenith, Pi is nadir.
    Colors are nterpolated from upper to lower angle from upper to lower color.
    RenderUpper/LowerStack do not need upper/lower angle: it is implicitly
    understood to be the zenith/nadir.

    TODO: ustalic we wszystkich Render*Stack ze sciany do wewnatrz sa
    zawsze CCW (albo na odwrot) i uzyc backface culling ? Czy cos na
    tym zyskamy ?
  }

  const
    { slices of rings rendered in Render*Stack }
    Slices = 24;

  procedure RenderStack(
    const UpperColor: TVector3Single; const UpperAngle: Single;
    const LowerColor: TVector3Single; const LowerAngle: Single);
  var
    UpperY, UpperRadius, LowerY, LowerRadius: Single;

    procedure Bar(const SliceAngle: Single);
    var
      SinSliceAngle, CosSliceAngle: Single;
    begin
      SinSliceAngle := Sin(SliceAngle);
      CosSliceAngle := Cos(SliceAngle);
      glColorv(LowerColor);
      glVertex3f(SinSliceAngle*LowerRadius, LowerY, CosSliceAngle*LowerRadius);
      glColorv(UpperColor);
      glVertex3f(SinSliceAngle*UpperRadius, UpperY, CosSliceAngle*UpperRadius);
    end;

  var
    i: Integer;
  begin
    StackCircleCalc(UpperAngle, UpperY, UpperRadius);
    StackCircleCalc(LowerAngle, LowerY, LowerRadius);
    glBegin(GL_QUAD_STRIP);
      Bar(0);
      for i := 1 to Slices-1 do Bar(i* 2*Pi/Slices);
      Bar(0);
    glEnd;
  end;

  procedure RenderUpperStack(
    const UpperColor: TVector3Single;
    const LowerColor: TVector3Single; const LowerAngle: Single);
  { Easy but not optimal implementation of this is
    RenderStack(UpperColor, 0, LowerColor, LowerAngle); }
  var
    LowerY, LowerRadius: Single;

    procedure Pt(const SliceAngle: Single);
    begin
      glVertex3f(Sin(SliceAngle)*LowerRadius, LowerY, Cos(SliceAngle)*LowerRadius);
    end;

  var
    i: Integer;
  begin
    StackCircleCalc(LowerAngle, LowerY, LowerRadius);
    glBegin(GL_TRIANGLE_FAN);
      glColorv(UpperColor);
      glVertex3f(0, SkySphereRadius, 0);
      glColorv(LowerColor);
      Pt(0);
      for i := 1 to Slices-1 do Pt(i* 2*Pi/Slices);
      Pt(0);
    glEnd;
  end;

  procedure RenderLowerStack(
    const UpperColor: TVector3Single; const UpperAngle: Single;
    const LowerColor: TVector3Single);
  { Easy but not optimal implementation of this is
    RenderStack(UpperColor, UpperAngle, LowerColor, Pi); }
  var
    UpperY, UpperRadius: Single;

    procedure Pt(const SliceAngle: Single);
    begin
      glVertex3f(Sin(SliceAngle)*UpperRadius, UpperY, Cos(SliceAngle)*UpperRadius);
    end;

  var
    i: Integer;
  begin
    StackCircleCalc(UpperAngle, UpperY, UpperRadius);
    glBegin(GL_TRIANGLE_FAN);
      glColorv(LowerColor);
      glVertex3f(0, -SkySphereRadius, 0);
      glColorv(UpperColor);
      Pt(0);
      for i := 1 to Slices-1 do Pt(i* 2*Pi/Slices);
      Pt(0);
    glEnd;
  end;
*)
var
  BS: TBackgroundSide;
begin
  RootNode := TX3DRootNode.Create('', Node.BaseUrl);

  MatrixTransform := TMatrixTransformNode.Create('', Node.BaseUrl);
  MatrixTransform.FdMatrix.Value := Node.TransformRotation;
  RootNode.FdChildren.Add(MatrixTransform);

  CubeSize := SkySphereRadius * SphereRadiusToCubeSize;
  CubeSize2 := CubeSize / 2;

  for BS := Low(BS) to High(BS) do RenderTextureSide(BS);

  ClearColor := Vector4Single(Node.FdSkyColor.Items[0], 1.0);

// TODO-background: implement ground and sky in new approach
(*
var
  TexturedSides: TBackgroundSides;
  i: Integer;
  GroundHighestAngle: Single;
  SomeTexturesWithAlpha: boolean;
begin
  inherited Create;

  { calculate nieboTex and SomeTexturesWithAlpha }
  SomeTexturesWithAlpha := false;
  TexturedSides := [];
  for bs := Low(bs) to High(bs) do
  begin
    Include(TexturedSides, bs);
    if Imgs.Images[bs].HasAlpha then SomeTexturesWithAlpha := true;
  end;

    { wykonujemy najbardziej elementarna optymalizacje : jesli mamy 6 tekstur
      i zadna nie ma kanalu alpha (a w praktyce jest to chyba najczestsza sytuacja)
      to nie ma sensu sie w ogole przejmowac sky i ground, tekstury je zaslonia. }
    if (TexturedSides <> BGAllSides) or SomeTexturesWithAlpha then
    begin
      { calculate GroundHighestAngle, will be usable to optimize rendering sky.
        GroundHighestAngle is measured in sky convention (0 = zenith, Pi = nadir).
        If there is no sky I simply set GroundHighestAngle to sthg > Pi. }
      if GroundAngleCount <> 0 then
        GroundHighestAngle := Pi-GroundAngle^[GroundAngleCount-1] else
        GroundHighestAngle := Pi + 1;

      { render sky }
      Assert(SkyColorCount >= 1, 'Sky must have at least one color');
      Assert(SkyAngleCount+1 = SkyColorCount, 'Sky must have exactly one more Color than Angles');

      if SkyColorCount = 1 then
      begin
        GLClear([cbColor], Vector4Single(SkyColor^[0]));
      end else
      begin
        { wiec SkyColorCount >= 2. W zasadzie rendering przebiega na zasadzie
            RenderUpperStack
            RenderStack iles razy
            RenderLowerStack
          Probujemy jednak przerwac robote w trakcie ktoregos RenderStack
          lub RenderLowerStack zeby nie tracic czasu na malowanie obszaru
          ktory i tak zamalujemy przez ground. Uzywamy do tego GroundHighestAngle.
        }
        RenderUpperStack(SkyColor^[0], SkyColor^[1], SkyAngle^[0]);
        for i := 1 to SkyAngleCount-1 do
        begin
          if SkyAngle^[i-1] > GroundHighestAngle then Break;
          RenderStack(SkyColor^[i]  , SkyAngle^[i-1],
                      SkyColor^[i+1], SkyAngle^[i]);
        end;
        { TODO: jesli ostatni stack ma SkyAngle bliskie Pi to powinnismy renderowac
          juz ostatni stack przy uzyciu RenderLowerStack. }
        if SkyAngle^[SkyAngleCount-1] <= GroundHighestAngle then
          RenderLowerStack(
            SkyColor^[SkyColorCount-1], SkyAngle^[SkyAngleCount-1],
            SkyColor^[SkyColorCount-1]);
      end;

      { render ground }
      if GroundAngleCount <> 0 then
      begin
        { jesli GroundAngleCount = 0 to nie ma ground wiec nie wymagamy wtedy
          zeby GroundColorCount = 1 (a wiec jest to wyjatek od zasady
          GroundAngleCount + 1 = GroundColorCount) }
        Assert(GroundAngleCount+1 = GroundColorCount, 'Ground must have exactly one more Color than Angles');

        RenderLowerStack(GroundColor^[1], Pi-GroundAngle^[0],
                         GroundColor^[0]);
        for i := 1 to GroundAngleCount-1 do
         RenderStack(GroundColor^[i+1], Pi-GroundAngle^[i],
                     GroundColor^[i]  , Pi-GroundAngle^[i-1]);
      end;
    end;

*)

  Scene.Load(RootNode, true);
end;

procedure TBackground.Render(const Frustum: TFrustum);
begin
  Params.InShadow := false;
  { since we constructed Scene ourselves,
    we know it only has ShadowVolumesReceivers=true shapes }
  Params.ShadowVolumesReceivers := true;

  // TODO-background: we ignore Frustum, as it contains shifted camera, not just rotated

  // TODO-background: for now we always do GLClear, although we don't have to
  GLClear([cbColor], ClearColor);

  Params.Transparent := false; Scene.Render(nil, Frustum, Params);
  Params.Transparent := true ; Scene.Render(nil, Frustum, Params);
end;

end.
