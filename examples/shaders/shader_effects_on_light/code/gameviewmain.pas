{
  Copyright 2026 Popa Marius Adrian.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shader effect on a light source, using a texture.

  A point light, in its own scene (LightScene, with CastGlobalLights = true),
  has an Effect that multiplies the light color by an image
  (stained glass window, window panes or blinds) projected downwards from the light.
  The image is an ImageTexture in a field of the Effect node.
  The lit objects are in another scene (LitScene). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleTransform, CastleCameras,
  X3DNodes, X3DFields;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    Viewport: TCastleViewport;
    LitScene, LightScene: TCastleScene;
    Light: TPointLightNode;
    LightEffect: TEffectNode;
    { Fields of LightEffect, passed to the shader as uniforms. }
    FieldTexture: TSFNode;
    FieldLightPosition: TSFVec3f;
    Textures: array [0..2] of TImageTextureNode;
    CurrentTexture: Integer;
    LightTime: Single;
    LightMoves: Boolean;
    LabelInfo: TCastleLabel;
    function BuildLitScene: TX3DRootNode;
    function BuildLightScene: TX3DRootNode;
    procedure UpdateLight;
    procedure UpdateInfo;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleColors, CastleUtils, CastleRenderOptions;

const
  LightHeight = 6.0;
  LightOrbit = 1.5;
  TextureUrls: array [0..2] of String = (
    'castle-data:/stained_glass.png',
    'castle-data:/window.png',
    'castle-data:/blinds.png'
  );

function TViewMain.BuildLitScene: TX3DRootNode;

  procedure AddShape(const Geometry: TAbstractGeometryNode; const Translation: TVector3);
  var
    Shape: TShapeNode;
    Appearance: TAppearanceNode;
    Material: TMaterialNode;
    Transform: TTransformNode;
  begin
    Material := TMaterialNode.Create;
    Material.DiffuseColor := Vector3(0.9, 0.9, 0.85);
    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;
    Shape := TShapeNode.Create;
    Shape.Geometry := Geometry;
    Shape.Appearance := Appearance;
    Transform := TTransformNode.Create;
    Transform.Translation := Translation;
    Transform.AddChildren(Shape);
    Result.AddChildren(Transform);
  end;

var
  EyeWorldSpace: TEffectNode;
  Floor, Box: TBoxNode;
  Sphere: TSphereNode;
  Cylinder: TCylinderNode;
  Ambient: TDirectionalLightNode;
begin
  Result := TX3DRootNode.Create;

  { The light's effect needs the world position of the lit point.
    This Effect (without any code) enables the "EyeWorldSpace" shader library
    (castle_CameraInverseMatrix uniform) for all shapes in this scene. }
  EyeWorldSpace := TEffectNode.Create;
  EyeWorldSpace.Language := slGLSL;
  EyeWorldSpace.FdShaderLibraries.Items.Add('castle-shader:/EyeWorldSpace.glsl');
  Result.AddChildren(EyeWorldSpace);

  { A dim light without effects, so that the unlit parts are not black. }
  Ambient := TDirectionalLightNode.Create;
  Ambient.Direction := Vector3(-0.3, -1, -0.4);
  Ambient.Intensity := 0.25;
  Result.AddChildren(Ambient);

  Floor := TBoxNode.Create;
  Floor.Size := Vector3(16, 0.2, 16);
  AddShape(Floor, Vector3(0, -0.1, 0));

  Box := TBoxNode.Create;
  Box.Size := Vector3(1.5, 1.5, 1.5);
  AddShape(Box, Vector3(-2.5, 0.75, 1));

  Sphere := TSphereNode.Create;
  Sphere.Radius := 1;
  AddShape(Sphere, Vector3(2.5, 1, -1));

  Cylinder := TCylinderNode.Create;
  Cylinder.Radius := 0.5;
  Cylinder.Height := 2.5;
  AddShape(Cylinder, Vector3(0, 1.25, -3));
end;

function TViewMain.BuildLightScene: TX3DRootNode;
var
  Part: TEffectPartNode;
  I: Integer;
begin
  for I := 0 to High(Textures) do
  begin
    Textures[I] := TImageTextureNode.Create;
    Textures[I].SetUrl([TextureUrls[I]]);
    Textures[I].RepeatS := false;
    Textures[I].RepeatT := false;
    { Keep the textures alive while not used by the effect. }
    Textures[I].KeepExistingBegin;
  end;

  { Effect on the light. Its fields become uniforms in the shader code,
    the texture field is a sampler2D. }
  LightEffect := TEffectNode.Create;
  LightEffect.Language := slGLSL;
  FieldTexture := TSFNode.Create(LightEffect, true, 'projection_texture',
    [TAbstractTextureNode], Textures[CurrentTexture]);
  LightEffect.AddCustomField(FieldTexture);
  FieldLightPosition := TSFVec3f.Create(LightEffect, true, 'light_position',
    Vector3(0, LightHeight, 0));
  LightEffect.AddCustomField(FieldLightPosition);
  LightEffect.AddCustomField(TSFFloat.Create(LightEffect, true, 'projection_size', 0.9));
  Part := TEffectPartNode.Create;
  Part.ShaderType := stFragment;
  Part.SetUrl(['castle-data:/shaders/projected_light.fs']);
  LightEffect.SetParts([Part]);

  Light := TPointLightNode.Create;
  { Global: lights shapes in all scenes (with LightScene.CastGlobalLights). }
  Light.Global := true;
  Light.Intensity := 1.2;
  Light.Color := Vector3(1, 1, 1);
  Light.Attenuation := Vector3(1, 0, 0);
  Light.Radius := 50;
  Light.SetEffects([LightEffect]);

  Result := TX3DRootNode.Create;
  Result.AddChildren(Light);
end;

procedure TViewMain.UpdateLight;
var
  Position: TVector3;
begin
  Position := Vector3(
    Cos(LightTime * 0.4) * LightOrbit, LightHeight, Sin(LightTime * 0.4) * LightOrbit);
  Light.Location := Position;
  { Send, not just set the value, to notify the shader uniform. }
  FieldLightPosition.Send(Position);
end;

procedure TViewMain.UpdateInfo;
begin
  LabelInfo.Caption :=
    'A point light with a shader effect: a colored image projected from the light.' + NL +
    'The image is a texture in a field of the light''s Effect node.' + NL +
    Format('Image: %s (press T to change)', [TextureUrls[CurrentTexture]]) + NL +
    Format('Light moves: %s (press Space)', [Iff(LightMoves, 'yes', 'no')]) + NL +
    'Drag with mouse to rotate the view.';
end;

procedure TViewMain.Start;
var
  Navigation: TCastleExamineNavigation;
begin
  inherited;
  LightMoves := true;

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := true;
  Viewport.BackgroundColor := Vector4(0.05, 0.05, 0.08, 1);
  Viewport.Camera.SetView(Vector3(0, 10, 12), Vector3(0, -0.7, -1), Vector3(0, 1, 0));
  InsertFront(Viewport);

  Navigation := TCastleExamineNavigation.Create(FreeAtStop);
  Viewport.InsertFront(Navigation);

  LitScene := TCastleScene.Create(FreeAtStop);
  LitScene.Load(BuildLitScene, true);
  { Light effects using castle_vertex_eye need Phong shading. }
  LitScene.RenderOptions.PhongShading := true;
  Viewport.Items.Add(LitScene);

  LightScene := TCastleScene.Create(FreeAtStop);
  LightScene.Load(BuildLightScene, true);
  LightScene.CastGlobalLights := true;
  { Process events, to pass FieldLightPosition.Send to the shader. }
  LightScene.ProcessEvents := true;
  Viewport.Items.Add(LightScene);

  LabelInfo := TCastleLabel.Create(FreeAtStop);
  LabelInfo.Anchor(hpLeft, 10);
  LabelInfo.Anchor(vpTop, -10);
  LabelInfo.Color := White;
  LabelInfo.FontSize := 20;
  InsertFront(LabelInfo);

  UpdateLight;
  UpdateInfo;
end;

procedure TViewMain.Stop;
var
  I: Integer;
begin
  { Frees LightScene (and other FreeAtStop components) with its nodes,
    but not the textures kept by KeepExistingBegin. }
  inherited;
  for I := 0 to High(Textures) do
  begin
    Textures[I].KeepExistingEnd;
    FreeIfUnusedAndNil(Textures[I]);
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  if LightMoves then
  begin
    LightTime := LightTime + SecondsPassed;
    UpdateLight;
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyT) then
  begin
    CurrentTexture := (CurrentTexture + 1) mod Length(Textures);
    { Changing the texture at runtime works too. }
    FieldTexture.Send(Textures[CurrentTexture]);
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keySpace) then
  begin
    LightMoves := not LightMoves;
    UpdateInfo;
    Exit(true);
  end;
end;

end.
