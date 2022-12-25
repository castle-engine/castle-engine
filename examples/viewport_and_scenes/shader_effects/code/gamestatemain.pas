{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, X3DNodes, X3DFields;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonRandomizeColor: TCastleButton;
    SceneForEffects: TCastleScene;
    RectCurrentColor: TCastleRectangleControl;

    Effect: TEffectNode;
    EffectColorField: TSFVec3f;
    procedure ClickRandomizeColor(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleVectors, CastleRenderOptions;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  { Create Effect, add it to given Scene. }
  procedure CreateEffect(const Scene: TCastleScene);
  var
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
        (will be limited to shapes using this TAppearanceNode),
        see examples/terrain/terrainscene.pas .

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

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  ButtonRandomizeColor := DesignedComponent('ButtonRandomizeColor') as TCastleButton;
  SceneForEffects := DesignedComponent('SceneForEffects') as TCastleScene;
  RectCurrentColor := DesignedComponent('RectCurrentColor') as TCastleRectangleControl;

  CreateEffect(SceneForEffects);

  ButtonRandomizeColor.OnClick := {$ifdef FPC}@{$endif} ClickRandomizeColor;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickRandomizeColor(Sender: TObject);
var
  NewColor: TVector3;
begin
  NewColor := Vector3(Random, Random, Random);
  RectCurrentColor.Color := Vector4(NewColor, 1);
  EffectColorField.Send(NewColor);
end;

end.
