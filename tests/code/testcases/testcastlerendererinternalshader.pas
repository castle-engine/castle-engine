{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test for CastleRendererInternalShader unit. }
unit TestCastleRendererInternalShader;

interface

uses
  Classes, SysUtils,
  CastleTester;

type
  TTestCastleRendererInternalShader = class(TCastleTestCase)
  published
    procedure TestFindPlugName;
    { Shapes lit by different lights, whose Effect nodes have the same code
      but different uniform values, must not share a shader program. }
    procedure TestLightEffectsNotShared;
    { TCastleAbstractLight.SetEffects, with effects using a shader library. }
    procedure TestLightComponentEffects;
  end;

implementation

uses CastleUtils, CastleRendererInternalShader,
  CastleVectors, CastleColors, CastleImages, CastleWindow, CastleViewport, CastleScene,
  CastleRenderOptions, CastleTransform, X3DNodes, X3DFields;

procedure TTestCastleRendererInternalShader.TestFindPlugName;
var
  PlugName, DeclaredParameters: String;
begin
  PlugName := FindPlugName(NL +
    'void PLUG_main_texture_apply ( inout vec4 fcol, const in vec3 unused )' + NL +
    '{' + NL +
    '}', DeclaredParameters);
  AssertEquals('main_texture_apply', PlugName);
  AssertEquals('( inout vec4 fcol, const in vec3 unused )', DeclaredParameters);

  PlugName := FindPlugName(NL +
    'void PLUG_main_texture_apply( inout vec4 fcol, const in vec3 unused )' + NL +
    '{' + NL +
    '}', DeclaredParameters);
  AssertEquals('main_texture_apply', PlugName);
  AssertEquals('( inout vec4 fcol, const in vec3 unused )', DeclaredParameters);

  // Make sure PLUG_xxx not followed by ( is not recognized
  PlugName := FindPlugName(NL +
    '/* void PLUG_main_texture_apply bla blah */', DeclaredParameters);
  AssertEquals('', PlugName);
end;

procedure TTestCastleRendererInternalShader.TestLightEffectsNotShared;
const
  { The same code for both lights: only the uniform value differs. }
  EffectCode =
    'uniform float test_light_factor;' + LineEnding +
    'void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)' + LineEnding +
    '{' + LineEnding +
    '  scale *= test_light_factor;' + LineEnding +
    '}';
var
  Window: TCastleWindow;
  Viewport: TCastleViewport;

  { A scene with a floor patch at (X, 0, 0) lit by its own point light
    (not global: it lights only this scene), whose Effect multiplies
    the light by LightFactor. }
  function CreateLitPatch(const X, LightFactor: Single): TCastleScene;
  var
    Root: TX3DRootNode;
    Shape: TShapeNode;
    Appearance: TAppearanceNode;
    Material: TMaterialNode;
    Box: TBoxNode;
    Transform: TTransformNode;
    Light: TPointLightNode;
    Effect: TEffectNode;
    Part: TEffectPartNode;
  begin
    Root := TX3DRootNode.Create;

    Material := TMaterialNode.Create;
    Material.DiffuseColor := WhiteRGB;
    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;
    Box := TBoxNode.CreateWithShape(Shape);
    Box.Size := Vector3(4, 0.2, 4);
    Shape.Appearance := Appearance;
    Transform := TTransformNode.Create;
    Transform.Translation := Vector3(X, -0.1, 0);
    Transform.AddChildren(Shape);
    Root.AddChildren(Transform);

    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'test_light_factor', LightFactor));
    Part := TEffectPartNode.Create;
    Part.ShaderType := stFragment;
    Part.Contents := EffectCode;
    Effect.SetParts([Part]);

    Light := TPointLightNode.Create;
    Light.Global := false;
    Light.Location := Vector3(X, 3, 0);
    Light.Attenuation := Vector3(1, 0, 0);
    Light.SetEffects([Effect]);
    Root.AddChildren(Light);

    Result := TCastleScene.Create(Window);
    Result.Load(Root, true);
    Result.RenderOptions.PhongShading := true;
  end;

  { Rendered color at world (X, 0, 0). }
  function ColorAt(const Image: TCastleImage; const X: Single): TCastleColor;
  var
    P: TVector2;
  begin
    P := Viewport.PositionFromWorld(Vector3(X, 0, 0));
    Result := Image.Colors[Round(P.X), Round(P.Y), 0];
  end;

var
  Image: TCastleImage;
  Dark, Lit: TCastleColor;
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

    { Same shape, same material, same light type: before including light
      effects in the shader code hash, both patches shared one shader program,
      with the uniform value of the first one rendered. }
    Viewport.Items.Add(CreateLitPatch(-3, 0));
    Viewport.Items.Add(CreateLitPatch(3, 1));

    Image := Window.SaveScreen;
    try
      Dark := ColorAt(Image, -3);
      Lit := ColorAt(Image, 3);
    finally FreeAndNil(Image) end;

    AssertTrue(Dark.X < 0.05);
    AssertTrue(Lit.X > 0.3);

    Window.Close;
  finally DestroyWindowForTest(Window) end;
end;

procedure TTestCastleRendererInternalShader.TestLightComponentEffects;
const
  { Uses a function of the EyeWorldSpace shader library:
    without the library, the shader would not link and the light
    would not be scaled at all. }
  EffectCode =
    'uniform float test_light_factor;' + LineEnding +
    'vec3 direction_eye_to_world_space(vec3 direction_eye);' + LineEnding +
    'void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)' + LineEnding +
    '{' + LineEnding +
    '  scale *= test_light_factor * length(direction_eye_to_world_space(light_dir));' + LineEnding +
    '}';
var
  Window: TCastleWindow;
  Viewport: TCastleViewport;

  { A floor patch at (X, 0, 0) lit by a TCastlePointLight above it
    (its radius doesn't reach the other patch),
    whose effect multiplies the light by LightFactor. }
  procedure AddLitPatch(const X, LightFactor: Single);
  var
    Box: TCastleBox;
    Light: TCastlePointLight;
    Effect: TEffectNode;
    Part: TEffectPartNode;
  begin
    Box := TCastleBox.Create(Window);
    Box.Size := Vector3(4, 0.2, 4);
    Box.Translation := Vector3(X, -0.1, 0);
    Viewport.Items.Add(Box);

    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    Effect.SetShaderLibraries(['castle-shader:/EyeWorldSpace.glsl']);
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'test_light_factor', LightFactor));
    Part := TEffectPartNode.Create;
    Part.ShaderType := stFragment;
    Part.Contents := EffectCode;
    Effect.SetParts([Part]);

    Light := TCastlePointLight.Create(Window);
    Light.Translation := Vector3(X, 1, 0);
    Light.Attenuation := Vector3(1, 0, 0);
    Light.Radius := 3.5;
    Light.SetEffects([Effect]);
    Viewport.Items.Add(Light);
  end;

  { Rendered color at world (X, 0, 0). }
  function ColorAt(const Image: TCastleImage; const X: Single): TCastleColor;
  var
    P: TVector2;
  begin
    P := Viewport.PositionFromWorld(Vector3(X, 0, 0));
    Result := Image.Colors[Round(P.X), Round(P.Y), 0];
  end;

var
  Image: TCastleImage;
  Dark, Lit: TCastleColor;
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

    AddLitPatch(-3, 0);
    AddLitPatch(3, 1);

    Image := Window.SaveScreen;
    try
      Dark := ColorAt(Image, -3);
      Lit := ColorAt(Image, 3);
    finally FreeAndNil(Image) end;

    AssertTrue(Dark.X < 0.05);
    AssertTrue(Lit.X > 0.3);

    Window.Close;
  finally DestroyWindowForTest(Window) end;
end;

initialization
  RegisterTest(TTestCastleRendererInternalShader);
end.
