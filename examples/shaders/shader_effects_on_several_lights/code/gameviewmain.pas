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

{ Shader effects on several lights, with the same code and different uniforms.

  Three lamps in one scene, each lighting its own floor tile (the light radius
  doesn't reach the other tiles, so every tile is lit by a different light).
  Every lamp has an Effect with the same GLSL code (data/shaders/rings_light.fs):
  the light shines in rings around the lamp. Each Effect has its own values
  of the uniforms "lamp_position" and "ring_count", so every tile shows
  a different number of rings. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleTransform, CastleCameras,
  X3DNodes, X3DFields;

type
  TLamp = record
    { Field "ring_count" of the lamp's Effect (a uniform in the shader). }
    RingCount: TSFFloat;
    BaseRingCount: Single;
  end;

  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Lamps: array [0..2] of TLamp;
    Animate: Boolean;
    Time: Single;
    LabelInfo: TCastleLabel;
    function CreateLamp(const Root: TX3DRootNode; const Position: TVector3;
      const Color: TVector3; const RingCount: Single): TLamp;
    procedure UpdateInfo;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleColors, CastleUtils, CastleRenderOptions;

function TViewMain.CreateLamp(const Root: TX3DRootNode; const Position: TVector3;
  const Color: TVector3; const RingCount: Single): TLamp;
var
  Effect: TEffectNode;
  Part: TEffectPartNode;
  Light: TPointLightNode;
  Tile: TBoxNode;
  Bulb: TSphereNode;
  Shape: TShapeNode;
  Appearance: TAppearanceNode;
  Material: TMaterialNode;
  BulbMaterial: TUnlitMaterialNode;
  Transform: TTransformNode;
begin
  { The floor tile lit by this lamp. }
  Material := TMaterialNode.Create;
  Material.DiffuseColor := Vector3(0.9, 0.9, 0.9);
  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;
  Tile := TBoxNode.CreateWithTransform(Shape, Transform);
  Tile.Size := Vector3(4.6, 0.2, 4.6);
  Shape.Appearance := Appearance;
  Transform.Translation := Vector3(Position.X, -0.1, Position.Z);
  Root.AddChildren(Transform);

  { A small bulb, to see where the lamp is. }
  BulbMaterial := TUnlitMaterialNode.Create;
  BulbMaterial.EmissiveColor := Color;
  Appearance := TAppearanceNode.Create;
  Appearance.Material := BulbMaterial;
  Bulb := TSphereNode.CreateWithTransform(Shape, Transform);
  Bulb.Radius := 0.15;
  Shape.Appearance := Appearance;
  Transform.Translation := Position;
  Root.AddChildren(Transform);

  { The Effect: the same code for every lamp, its own uniform values. }
  Effect := TEffectNode.Create;
  Effect.Language := slGLSL;
  Effect.AddCustomField(TSFVec3f.Create(Effect, true, 'lamp_position', Position));
  Result.RingCount := TSFFloat.Create(Effect, true, 'ring_count', RingCount);
  Effect.AddCustomField(Result.RingCount);
  Part := TEffectPartNode.Create;
  Part.ShaderType := stFragment;
  Part.SetUrl(['castle-data:/shaders/rings_light.fs']);
  Effect.SetParts([Part]);

  { The lamp. Its radius reaches only its own tile:
    each tile is lit by a different lamp. }
  Light := TPointLightNode.Create;
  Light.Global := false;
  Light.Radius := 4.5;
  Light.Location := Position;
  Light.Color := Color;
  Light.Intensity := 1.5;
  Light.Attenuation := Vector3(1, 0, 0);
  Light.SetEffects([Effect]);
  Root.AddChildren(Light);

  Result.BaseRingCount := RingCount;
end;

procedure TViewMain.UpdateInfo;
begin
  LabelInfo.Caption :=
    'Three lamps with the same shader effect code (rings around the lamp)' + NL +
    'and different uniform values: 0.5, 1 and 2 rings per unit.' + NL +
    Format('Animate the ring counts: %s (press Space)', [Iff(Animate, 'yes', 'no')]) + NL +
    'Drag with mouse to rotate the view.';
end;

procedure TViewMain.Start;
var
  Navigation: TCastleExamineNavigation;
  Root: TX3DRootNode;
  EyeWorldSpace: TEffectNode;
begin
  inherited;

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := true;
  Viewport.BackgroundColor := Vector4(0.05, 0.05, 0.08, 1);
  Viewport.Camera.SetView(Vector3(0, 12, 12), Vector3(0, -1, -1), Vector3(0, 1, 0));
  InsertFront(Viewport);

  Navigation := TCastleExamineNavigation.Create(FreeAtStop);
  Viewport.InsertFront(Navigation);

  Root := TX3DRootNode.Create;

  { The light's effect needs the world position of the lit point.
    This Effect (without any code) enables the "EyeWorldSpace" shader library
    (castle_CameraInverseMatrix uniform) for all shapes in this scene. }
  EyeWorldSpace := TEffectNode.Create;
  EyeWorldSpace.Language := slGLSL;
  EyeWorldSpace.FdShaderLibraries.Items.Add('castle-shader:/EyeWorldSpace.glsl');
  Root.AddChildren(EyeWorldSpace);

  Lamps[0] := CreateLamp(Root, Vector3(-7, 1.5, 0), Vector3(1, 0.6, 0.4), 0.5);
  Lamps[1] := CreateLamp(Root, Vector3( 0, 1.5, 0), Vector3(0.6, 1, 0.5), 1);
  Lamps[2] := CreateLamp(Root, Vector3( 7, 1.5, 0), Vector3(0.5, 0.7, 1), 2);

  Scene := TCastleScene.Create(FreeAtStop);
  Scene.Load(Root, true);
  { Light effects using castle_vertex_eye need Phong shading. }
  Scene.RenderOptions.PhongShading := true;
  { Process events, to pass RingCount.Send to the shader. }
  Scene.ProcessEvents := true;
  Viewport.Items.Add(Scene);

  LabelInfo := TCastleLabel.Create(FreeAtStop);
  LabelInfo.Anchor(hpLeft, 10);
  LabelInfo.Anchor(vpTop, -10);
  LabelInfo.Color := White;
  LabelInfo.FontSize := 20;
  InsertFront(LabelInfo);
  UpdateInfo;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  I: Integer;
begin
  inherited;
  if Animate then
  begin
    Time := Time + SecondsPassed;
    { Send (not just set Value), to update the uniform of this lamp's effect. }
    for I := 0 to High(Lamps) do
      Lamps[I].RingCount.Send(Lamps[I].BaseRingCount * (1 + 0.3 * Sin(Time * 2)));
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keySpace) then
  begin
    Animate := not Animate;
    UpdateInfo;
    Exit(true);
  end;
end;

end.
