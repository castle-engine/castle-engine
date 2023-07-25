{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, X3DNodes, X3DFields;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonRandomizeColor, ButtonChangeTexture: TCastleButton;
    SceneForColorEffect, SceneForTextureEffect: TCastleScene;
    RectCurrentColor: TCastleRectangleControl;
  private
    { Control effect affecting SceneForColorEffect }
    EffectColorField: TSFVec3f;

    { Control effect affecting SceneForTextureEffect }
    EffectTextureField: TSFNode;
    TestTexture1: TImageTextureNode;
    TestTexture2: TImageTextureNode;

    procedure ClickRandomizeColor(Sender: TObject);
    procedure ClickChangeTexture(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleVectors, CastleRenderOptions;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  { Create Effect changing a color, exposing a 'color' uniform shader variable
    using X3D field EffectColorField.

    You can change such uniform shader variable at any time.
    You can even change it very often (e.g. every frame),
    changing uniform values is cheap.
    See TViewMain.ClickRandomizeColor . }
  procedure CreateColorEffect(const Scene: TCastleScene);
  var
    Effect: TEffectNode;
    EffectPart: TEffectPartNode;
  begin
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;

    { Add custom field (maps to GLSL uniform "color"), initially white }
    EffectColorField := TSFVec3f.Create(Effect, true, 'color', Vector3(1, 1, 1));
    Effect.AddCustomField(EffectColorField);

    { Add TEffectPartNode which actually contains shader code.
      You can load shader code from file,
      you could also set it explicitly from string using EffectPart.Contents := '...'; }
    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stFragment;
    EffectPart.SetUrl(['castle-data:/shaders/color_effect.fs']);
    Effect.SetParts([EffectPart]);

    { Using "RootNode.AddChildren" applies effect for everything in the scene.
      It is possible to be more selective:

      - You can apply effect only for given TAppearanceNode
        (will be limited to shapes using this TAppearanceNode).

      - Effects can also be applied on a specific texture or light source.
        See https://github.com/castle-engine/demo-models/blob/master/compositing_shaders/grayscale_texture_effect.x3dv .

      See https://castle-engine.io/compositing_shaders.php and in particular
      https://castle-engine.io/compositing_shaders_doc/html/ about shader effects. }
    Scene.RootNode.AddChildren([Effect]);

    // adding node to Scene turns off animation set by AutoAnimation, run it again
    Scene.PlayAnimation('Walk', true);
  end;

  { Create Effect using a texture, provided as uniform shader variable 'testTexture'
    using X3D field EffectTextureField.
    The point of it is to show how to pass a texture to a shader in CGE.

    You can change the texture at runtime, it will change the texture passed to shader
    correctly. See TViewMain.ClickChangeTexture lower for example. }
  procedure CreateTextureEffect(const Scene: TCastleScene);
  var
    Effect: TEffectNode;
    EffectPartFragment, EffectPartVertex: TEffectPartNode;
  begin
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;

    TestTexture1 := TImageTextureNode.Create;
    TestTexture1.SetUrl(['castle-data:/test_textures/handpaintedwall2.png']);
    TestTexture1.KeepExistingBegin; // do not auto-free when unused

    TestTexture2 := TImageTextureNode.Create;
    TestTexture2.SetUrl(['castle-data:/test_textures/mountain.png']);
    TestTexture2.KeepExistingBegin; // do not auto-free when unused

    { Add custom field (maps to GLSL uniform "texture") }
    EffectTextureField := TSFNode.Create(Effect, true, 'testTexture', [TImageTextureNode], TestTexture1);
    Effect.AddCustomField(EffectTextureField);

    { Add 2x TEffectPartNode which actually contains shader code.
      You can load shader code from file,
      you could also set it explicitly from string using EffectPart.Contents := '...'; }
    EffectPartFragment := TEffectPartNode.Create;
    EffectPartFragment.ShaderType := stFragment;
    EffectPartFragment.SetUrl(['castle-data:/shaders/texture_effect.fs']);

    EffectPartVertex := TEffectPartNode.Create;
    EffectPartVertex.ShaderType := stVertex;
    EffectPartVertex.SetUrl(['castle-data:/shaders/texture_effect.vs']);

    Effect.SetParts([EffectPartFragment, EffectPartVertex]);

    { Using "RootNode.AddChildren" applies effect for everything in the scene.
      It is possible to be more selective:

      - You can apply effect only for given TAppearanceNode
        (will be limited to shapes using this TAppearanceNode).

      - Effects can also be applied on a specific texture or light source.
        See https://github.com/castle-engine/demo-models/blob/master/compositing_shaders/grayscale_texture_effect.x3dv .

      See https://castle-engine.io/compositing_shaders.php and in particular
      https://castle-engine.io/compositing_shaders_doc/html/ about shader effects. }
    Scene.RootNode.AddChildren([Effect]);

    // adding node to Scene turns off animation set by AutoAnimation, run it again
    Scene.PlayAnimation('Walk', true);
  end;

begin
  inherited;

  CreateColorEffect(SceneForColorEffect);

  CreateTextureEffect(SceneForTextureEffect);

  ButtonRandomizeColor.OnClick := {$ifdef FPC}@{$endif} ClickRandomizeColor;
  ButtonChangeTexture.OnClick := {$ifdef FPC}@{$endif} ClickChangeTexture;
end;

procedure TViewMain.Stop;
begin
  TestTexture1.KeepExistingEnd;
  TestTexture2.KeepExistingEnd;
  FreeIfUnusedAndNil(TestTexture1);
  FreeIfUnusedAndNil(TestTexture2);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickRandomizeColor(Sender: TObject);
var
  NewColor: TVector3;
begin
  NewColor := Vector3(Random, Random, Random);
  RectCurrentColor.Color := Vector4(NewColor, 1);
  EffectColorField.Send(NewColor);
end;

procedure TViewMain.ClickChangeTexture(Sender: TObject);
begin
  if EffectTextureField.Value = TestTexture1 then
    EffectTextureField.Send(TestTexture2)
  else
    EffectTextureField.Send(TestTexture1);

  { TODO: The GLContextClose call is needed for now, to load TestTexture2 properly to OpenGL
    next time the scene is rendered.
    The GLContextClose should not be necessary, the reload should happen automatically,
    TCastleSceneCore should realize that shapes using EffectTextureField
    (and textures used by these shapes) need to be prepared again,
    i.e. passed to PrepareShape. }
  SceneForTextureEffect.GLContextClose;
end;

end.
