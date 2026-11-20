// -*- compile-command: "./test_single_testcase.sh TTestScene" -*-
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleScene unit. }
unit TestCastleScene;

interface

uses
  Classes, SysUtils,
  CastleTester;

type
  TTestScene = class(TCastleTestCase)
  published
    procedure TestScene;
    procedure TestSpatialUpgrade;
    procedure TestImageTransformImageWidthHeight;
    { Testcase from https://github.com/castle-engine/castle-engine/issues/664 }
    procedure TestRenderReferencesAndLightRadius;
    { Texture used by a light's Effect, when the light is in another scene
      (CastGlobalLights) than the lit shapes. }
    procedure TestGlobalLightEffectTexture;
    { Light's Effect coloring the light with PLUG_light_color_scale. }
    procedure TestGlobalLightEffectColor;
  end;

implementation

uses X3DNodes, CastleSceneCore, CastleScene, CastleBoxes, CastleVectors,
  CastleInternalRays, CastleProjection, CastleComponentSerialize,
  CastleUIControls, CastleWindow, CastleViewport, CastleInternalGLUtils,
  CastleImages, CastleColors, CastleRenderOptions, X3DFields;

procedure TTestScene.TestScene;

  procedure EmptySceneAsserts(EmptyScene: TCastleScene);
  var
    CamProjection: TProjectionType;
    CamPos, CamDir, CamUp, GravityUp: TVector3;
  begin
    AssertTrue(EmptyScene.VerticesCount = 0);
    AssertTrue(EmptyScene.TrianglesCount = 0);

    AssertTrue(EmptyScene.BoundingBox.IsEmpty);

    AssertTrue(EmptyScene.ShapesActiveCount = 0);

    AssertTrue(EmptyScene.GetViewpoint(CamProjection, CamPos, CamDir, CamUp, GravityUp) = nil);

    AssertTrue(EmptyScene.FogStack.Top = nil);

    AssertTrue(EmptyScene.InternalBackgroundRenderer = nil);
  end;

var
  EmptyScene: TCastleScene;
begin
  EmptyScene := TCastleScene.Create(nil);
  try
    EmptySceneAsserts(EmptyScene);
    EmptyScene.ChangedAll;
    EmptySceneAsserts(EmptyScene);
  finally FreeAndNil(EmptyScene) end;

  EmptyScene := TCastleScene.Create(nil);
  try
    EmptyScene.Load(TX3DRootNode.Create, true);
    EmptySceneAsserts(EmptyScene);
    EmptyScene.ChangedAll;
    EmptySceneAsserts(EmptyScene);
  finally FreeAndNil(EmptyScene) end;
end;

procedure TTestScene.TestSpatialUpgrade;
var
  SceneSpatialNone, SceneSpatialPreciseCollisions,
    SceneSpatialOnlyRendering, SceneSpatialOnlyDynamicCollisions,
    SceneSpatialOnlyVisibleTriangles, SceneSpatialOnlyStaticCollisions: TCastleScene;
  UiOwner: TComponent;
begin
  UiOwner := TComponent.Create(nil);
  try
    UserInterfaceLoad('castle-data:/designs/test_spatial_upgrade.castle-user-interface', UiOwner);

    SceneSpatialNone := UiOwner.FindRequiredComponent('SceneSpatialNone') as TCastleScene;
    SceneSpatialPreciseCollisions := UiOwner.FindRequiredComponent('SceneSpatialPreciseCollisions') as TCastleScene;
    SceneSpatialOnlyRendering := UiOwner.FindRequiredComponent('SceneSpatialOnlyRendering') as TCastleScene;
    SceneSpatialOnlyDynamicCollisions := UiOwner.FindRequiredComponent('SceneSpatialOnlyDynamicCollisions') as TCastleScene;
    SceneSpatialOnlyVisibleTriangles := UiOwner.FindRequiredComponent('SceneSpatialOnlyVisibleTriangles') as TCastleScene;
    SceneSpatialOnlyStaticCollisions := UiOwner.FindRequiredComponent('SceneSpatialOnlyStaticCollisions') as TCastleScene;

    AssertFalse(SceneSpatialNone.PreciseCollisions);
    AssertTrue(SceneSpatialPreciseCollisions.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyRendering.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyDynamicCollisions.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyVisibleTriangles.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyStaticCollisions.PreciseCollisions);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestScene.TestImageTransformImageWidthHeight;
var
  ImageTransform: TCastleImageTransform;
begin
  ImageTransform := TCastleImageTransform.Create(nil);
  try
    AssertEquals(0, ImageTransform.ImageWidth);
    AssertEquals(0, ImageTransform.ImageHeight);

    ImageTransform.Url := 'castle-data:/test_texture.png';
    AssertEquals(256, ImageTransform.ImageWidth);
    AssertEquals(256, ImageTransform.ImageHeight);

    ImageTransform.Size := Vector2(100, 200);
    ImageTransform.Scale := Vector3(0.25, 0.333333, 1);

    // ImageWidth and ImageHeight should not change because of Size and Scale
    AssertEquals(256, ImageTransform.ImageWidth);
    AssertEquals(256, ImageTransform.ImageHeight);

    // LocalBoundingBox should reflect Size
    AssertSameValue(100, ImageTransform.LocalBoundingBox.SizeX);
    AssertSameValue(200, ImageTransform.LocalBoundingBox.SizeY);

    // BoundingBox should reflect Size scaled by Scale
    AssertSameValue(100 * 0.25, ImageTransform.BoundingBox.SizeX, 0.01);
    AssertSameValue(200 * 0.333333, ImageTransform.BoundingBox.SizeY, 0.01);

    ImageTransform.Url := '';

    // make sure ImageWidth and ImageHeight are reset to 0
    AssertEquals(0, ImageTransform.ImageWidth);
    AssertEquals(0, ImageTransform.ImageHeight);

    { The box is, for now, not empty, but has zero size. Though we don't promise
      exact behavior of this in API. }
    AssertFalse(ImageTransform.LocalBoundingBox.IsEmpty);
    AssertSameValue(0, ImageTransform.LocalBoundingBox.SizeX);
    AssertSameValue(0, ImageTransform.LocalBoundingBox.SizeY);
    AssertFalse(ImageTransform.BoundingBox.IsEmpty);
    AssertSameValue(0, ImageTransform.BoundingBox.SizeX);
    AssertSameValue(0, ImageTransform.BoundingBox.SizeY);
  finally FreeAndNil(ImageTransform) end;
end;

procedure TTestScene.TestRenderReferencesAndLightRadius;

{ Testcase from
  https://github.com/castle-engine/castle-engine/issues/664

  Before the fixes, rendering would fail with
  OpenGL error (1282): The specified operation is not allowed in the current state.
}

var
  Window: TCastleWindow;
  View: TCastleView;
  Viewport1: TCastleViewport;
begin
  if not CanCreateWindowForTest then
  begin
    AbortTest;
    Exit;
  end;

  Window := CreateWindowForTest;
  try
    //Window.Visible := false; // need to be Visible to reproduce the crash
    Window.Open;

    View := TCastleView.Create(nil);
    try
      View.DesignUrl := 'castle-data:/designs/gh_664_references_and_light_radius/gameviewmain.castle-user-interface';
      Window.Container.View := View;
      Viewport1 := View.DesignedComponent('Viewport1') as TCastleViewport;

      // default view is already "bad" in design, crashes before 664 fix
      Window.Container.EventBeforeRender;
      Window.Container.EventRender;
      Window.Container.EventUpdate;
      CheckGLErrors('TestRenderReferencesAndLightRadius, frame 0');

      // this is good view, no crash
      Viewport1.Camera.SetWorldView(
        Vector3(0.00, 1.58, 0.83), // position
        Vector3(-0.72, 0.00, -0.69), // direction
        Vector3(0.00, 1.00, 0.00)  // up (current)
      );
      Window.Container.EventBeforeRender;
      Window.Container.EventRender;
      Window.Container.EventUpdate;
      CheckGLErrors('TestRenderReferencesAndLightRadius, frame 1');

      // this is bad view, crashes before 664 fix
      Viewport1.Camera.SetWorldView(
        Vector3(0.00, 1.58, 0.83), // position
        Vector3(-0.98, 0.00, -0.20), // direction
        Vector3(0.00, 1.00, 0.00)  // up (current)
      );
      Window.Container.EventBeforeRender;
      Window.Container.EventRender;
      Window.Container.EventUpdate;
      CheckGLErrors('TestRenderReferencesAndLightRadius, frame 2');

      // Interactive test:
      // while not Window.Closed do
      //   Application.ProcessAllMessages;
    finally FreeAndNil(View) end;
  finally DestroyWindowForTest(Window) end;
end;

procedure TTestScene.TestGlobalLightEffectTexture;
const
  { The light's effect multiplies the light by the texture,
    projected along the world X axis on the floor 10 units wide:
    the texture is black on the left (-X) half, white on the right (+X) half. }
  EffectCode =
    'uniform mat4 castle_CameraInverseMatrix;' + LineEnding +
    'varying vec4 castle_vertex_eye;' + LineEnding +
    'uniform sampler2D test_light_texture;' + LineEnding +
    'void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)' + LineEnding +
    '{' + LineEnding +
    '  vec4 world = castle_CameraInverseMatrix * castle_vertex_eye;' + LineEnding +
    '  scale *= texture2D(test_light_texture, vec2(world.x / 10.0 + 0.5, 0.5)).r;' + LineEnding +
    '}';
var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  LitScene, LightScene: TCastleScene;

  function BuildLitScene: TX3DRootNode;
  var
    Library_: TEffectNode;
    Shape: TShapeNode;
    Material: TMaterialNode;
    Box: TBoxNode;
    Transform: TTransformNode;
  begin
    Result := TX3DRootNode.Create;

    { Make castle_CameraInverseMatrix available to the light effect. }
    Library_ := TEffectNode.Create;
    Library_.Language := slGLSL;
    Library_.FdShaderLibraries.Items.Add('castle-shader:/EyeWorldSpace.glsl');
    Result.AddChildren(Library_);

    Material := TMaterialNode.Create;
    Material.DiffuseColor := WhiteRGB;
    Box := TBoxNode.CreateWithShape(Shape);
    Box.Size := Vector3(10, 0.2, 10);
    Shape.Material := Material;
    Transform := TTransformNode.Create;
    Transform.Translation := Vector3(0, -0.1, 0);
    Transform.AddChildren(Shape);
    Result.AddChildren(Transform);
  end;

  function BuildLightScene: TX3DRootNode;
  var
    Light: TPointLightNode;
    Effect: TEffectNode;
    Part: TEffectPartNode;
    Texture: TPixelTextureNode;
    Image: TRGBImage;
  begin
    Image := TRGBImage.Create(2, 1);
    Image.Colors[0, 0, 0] := Black;
    Image.Colors[1, 0, 0] := White;
    Texture := TPixelTextureNode.Create;
    Texture.FdImage.Value := Image;
    Texture.TextureProperties := TTexturePropertiesNode.Create;
    Texture.TextureProperties.MinificationFilter := minNearest;
    Texture.TextureProperties.MagnificationFilter := magNearest;
    Texture.TextureProperties.BoundaryModeS := bmClampToEdge;
    Texture.TextureProperties.BoundaryModeT := bmClampToEdge;

    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    Effect.AddCustomField(TSFNode.Create(Effect, true, 'test_light_texture',
      [TAbstractTextureNode], Texture));
    Part := TEffectPartNode.Create;
    Part.ShaderType := stFragment;
    Part.Contents := EffectCode;
    Effect.SetParts([Part]);

    Light := TPointLightNode.Create;
    Light.Global := true;
    Light.Location := Vector3(0, 5, 0);
    Light.Attenuation := Vector3(1, 0, 0);
    Light.Radius := 100;
    Light.SetEffects([Effect]);

    Result := TX3DRootNode.Create;
    Result.AddChildren(Light);
  end;

  { Floor color at world (X, 0, 0). }
  function FloorColor(const Image: TCastleImage; const X: Single): TCastleColor;
  var
    P: TVector2;
  begin
    P := Viewport.PositionFromWorld(Vector3(X, 0, 0));
    Result := Image.Colors[Round(P.X), Round(P.Y), 0];
  end;

var
  Image: TCastleImage;
  Left, Right: TCastleColor;
begin
  if not CanCreateWindowForTest then
  begin
    AbortTest;
    Exit;
  end;

  Window := CreateWindowForTest;
  try
    Window.Width := 400;
    Window.Height := 400;
    Window.Open;

    Viewport := TCastleViewport.Create(Window);
    Viewport.FullSize := true;
    Viewport.BackgroundColor := Black;
    Viewport.Camera.SetView(
      Vector3(0, 12, 0),
      Vector3(0, -1, 0),
      Vector3(0, 0, -1));
    Window.Controls.InsertFront(Viewport);

    LitScene := TCastleScene.Create(Window);
    LitScene.Load(BuildLitScene, true);
    LitScene.RenderOptions.PhongShading := true;
    Viewport.Items.Add(LitScene);

    { The light is in another scene, lighting LitScene as a global light. }
    LightScene := TCastleScene.Create(Window);
    LightScene.Load(BuildLightScene, true);
    LightScene.CastGlobalLights := true;
    Viewport.Items.Add(LightScene);

    Image := Window.SaveScreen;
    try
      Left := FloorColor(Image, -2.5);
      Right := FloorColor(Image, 2.5);
    finally FreeAndNil(Image) end;

    // texture is black on the left: no light
    AssertTrue(Left.X < 0.05);
    // texture is white on the right: lit (the texture was bound)
    AssertTrue(Right.X > 0.3);
    CheckGLErrors('TestGlobalLightEffectTexture');

    Window.Close;
  finally DestroyWindowForTest(Window) end;
end;

procedure TTestScene.TestGlobalLightEffectColor;
const
  { The light's effect multiplies the light color by the texture (RGB),
    with PLUG_light_color_scale, projected along the world X axis
    on the floor 10 units wide:
    the texture is red on the left (-X) half, green on the right (+X) half. }
  EffectCode =
    'uniform mat4 castle_CameraInverseMatrix;' + LineEnding +
    'varying vec4 castle_vertex_eye;' + LineEnding +
    'uniform sampler2D test_light_texture;' + LineEnding +
    'void PLUG_light_color_scale(inout vec3 color_scale, const in vec3 normal_eye, const in vec3 light_dir)' + LineEnding +
    '{' + LineEnding +
    '  vec4 world = castle_CameraInverseMatrix * castle_vertex_eye;' + LineEnding +
    '  color_scale *= texture2D(test_light_texture, vec2(world.x / 10.0 + 0.5, 0.5)).rgb;' + LineEnding +
    '}';
var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  LitScene, LightScene: TCastleScene;

  { Floor with Phong (TMaterialNode) or physical (TPhysicalMaterialNode)
    material: the plug is in both lighting models. }
  function BuildLitScene(const Physical: Boolean): TX3DRootNode;
  var
    Library_: TEffectNode;
    Shape: TShapeNode;
    Material: TMaterialNode;
    PhysicalMaterial: TPhysicalMaterialNode;
    Box: TBoxNode;
    Transform: TTransformNode;
  begin
    Result := TX3DRootNode.Create;

    { Make castle_CameraInverseMatrix available to the light effect. }
    Library_ := TEffectNode.Create;
    Library_.Language := slGLSL;
    Library_.FdShaderLibraries.Items.Add('castle-shader:/EyeWorldSpace.glsl');
    Result.AddChildren(Library_);

    Box := TBoxNode.CreateWithShape(Shape);
    Box.Size := Vector3(10, 0.2, 10);
    if Physical then
    begin
      PhysicalMaterial := TPhysicalMaterialNode.Create;
      PhysicalMaterial.BaseColor := WhiteRGB;
      PhysicalMaterial.Metallic := 0;
      PhysicalMaterial.Roughness := 1;
      Shape.Material := PhysicalMaterial;
    end else
    begin
      Material := TMaterialNode.Create;
      Material.DiffuseColor := WhiteRGB;
      Shape.Material := Material;
    end;
    Transform := TTransformNode.Create;
    Transform.Translation := Vector3(0, -0.1, 0);
    Transform.AddChildren(Shape);
    Result.AddChildren(Transform);
  end;

  function BuildLightScene: TX3DRootNode;
  var
    Light: TPointLightNode;
    Effect: TEffectNode;
    Part: TEffectPartNode;
    Texture: TPixelTextureNode;
    Image: TRGBImage;
  begin
    Image := TRGBImage.Create(2, 1);
    Image.Colors[0, 0, 0] := Red;
    Image.Colors[1, 0, 0] := Green;
    Texture := TPixelTextureNode.Create;
    Texture.FdImage.Value := Image;
    Texture.TextureProperties := TTexturePropertiesNode.Create;
    Texture.TextureProperties.MinificationFilter := minNearest;
    Texture.TextureProperties.MagnificationFilter := magNearest;
    Texture.TextureProperties.BoundaryModeS := bmClampToEdge;
    Texture.TextureProperties.BoundaryModeT := bmClampToEdge;

    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    Effect.AddCustomField(TSFNode.Create(Effect, true, 'test_light_texture',
      [TAbstractTextureNode], Texture));
    Part := TEffectPartNode.Create;
    Part.ShaderType := stFragment;
    Part.Contents := EffectCode;
    Effect.SetParts([Part]);

    Light := TPointLightNode.Create;
    Light.Global := true;
    Light.Location := Vector3(0, 5, 0);
    Light.Attenuation := Vector3(1, 0, 0);
    Light.Radius := 100;
    Light.SetEffects([Effect]);

    Result := TX3DRootNode.Create;
    Result.AddChildren(Light);
  end;

  { Floor color at world (X, 0, 0). }
  function FloorColor(const Image: TCastleImage; const X: Single): TCastleColor;
  var
    P: TVector2;
  begin
    P := Viewport.PositionFromWorld(Vector3(X, 0, 0));
    Result := Image.Colors[Round(P.X), Round(P.Y), 0];
  end;

var
  Image: TCastleImage;
  Left, Right: TCastleColor;
  Physical: Boolean;
begin
  if not CanCreateWindowForTest then
  begin
    AbortTest;
    Exit;
  end;

  Window := CreateWindowForTest;
  try
    Window.Width := 400;
    Window.Height := 400;
    Window.Open;

    Viewport := TCastleViewport.Create(Window);
    Viewport.FullSize := true;
    Viewport.BackgroundColor := Black;
    Viewport.Camera.SetView(
      Vector3(0, 12, 0),
      Vector3(0, -1, 0),
      Vector3(0, 0, -1));
    Window.Controls.InsertFront(Viewport);

    { The light is in another scene, lighting LitScene as a global light. }
    LightScene := TCastleScene.Create(Window);
    LightScene.Load(BuildLightScene, true);
    LightScene.CastGlobalLights := true;
    Viewport.Items.Add(LightScene);

    for Physical := false to true do
    begin
      LitScene := TCastleScene.Create(Window);
      LitScene.Load(BuildLitScene(Physical), true);
      LitScene.RenderOptions.PhongShading := true;
      Viewport.Items.Add(LitScene);

      Image := Window.SaveScreen;
      try
        Left := FloorColor(Image, -2.5);
        Right := FloorColor(Image, 2.5);
      finally FreeAndNil(Image) end;

      // texture is red on the left: only red light
      AssertTrue(Left.X > 0.2);
      AssertTrue(Left.Y < 0.05);
      AssertTrue(Left.Z < 0.05);
      // texture is green on the right: only green light
      AssertTrue(Right.X < 0.05);
      AssertTrue(Right.Y > 0.2);
      AssertTrue(Right.Z < 0.05);
      CheckGLErrors('TestGlobalLightEffectColor');

      FreeAndNil(LitScene);
    end;

    Window.Close;
  finally DestroyWindowForTest(Window) end;
end;

initialization
  RegisterTest(TTestScene);
end.
