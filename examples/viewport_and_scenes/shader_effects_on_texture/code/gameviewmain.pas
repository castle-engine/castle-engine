{
  Copyright 2024-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleTimeUtils, X3DNodes,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  CastleTransform;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ScenesParent: TCastleTransform;
    SceneTexturedSampleThing2, SceneTexturedSampleThing3: TCastleScene;
  private
    WorldTime: TFloatTime;
    { Field needed only to pass information
      from SceneAddTextureEffect to NodeAddTextureEffect. }
    EffectShaderUrl: String;
    procedure NodeAddTextureEffect(Node: TX3DNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleRenderOptions;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.NodeAddTextureEffect(Node: TX3DNode);
var
  TextureNode: TAbstractSingleTextureNode;
  Effect: TEffectNode;
  EffectPart: TEffectPartNode;
begin
  // can be safely cast, as we enumerate only TAbstractSingleTextureNode
  TextureNode := Node as TAbstractSingleTextureNode;

  Effect := TEffectNode.Create;
  Effect.Language := slGLSL;

  { Add TEffectPartNode which actually contains shader code.
    You can load shader code from file,
    you could also set it explicitly from string using EffectPart.Contents := '...'; }
  EffectPart := TEffectPartNode.Create;
  EffectPart.ShaderType := stFragment;
  EffectPart.SetUrl([EffectShaderUrl]);
  Effect.SetParts([EffectPart]);

  { This applies the effect to the specific texture,
    which allows PLUG_texture_color to be available
    ( see https://castle-engine.io/compositing_shaders_doc/html/section.fragment_plugs.html ). }
  TextureNode.SetEffects([Effect]);
end;

procedure TViewMain.Start;

  { Add a shader effect that modifies effective texture RGBA. }
  procedure SceneAddTextureEffect(const Scene: TCastleScene;
    const AnEffectShaderUrl: String);
  var
    SavedAnimation: String;
  begin
    if Scene.RootNode <> nil then
    begin
      SavedAnimation := Scene.AutoAnimation;

      EffectShaderUrl := AnEffectShaderUrl;
      Scene.RootNode.EnumerateNodes(TAbstractSingleTextureNode,
        {$ifdef FPC}@{$endif} NodeAddTextureEffect, false);

      { Adding node to Scene turns off animation set by AutoAnimation, run it again.
        The need to do this may disappear in future CGE versions. }
      if SavedAnimation <> '' then
        Scene.PlayAnimation(SavedAnimation, true);
    end;
  end;

begin
  inherited;
  SceneAddTextureEffect(SceneTexturedSampleThing2, 'castle-data:/shaders/texture_effect_sepia.fs');
  SceneAddTextureEffect(SceneTexturedSampleThing3, 'castle-data:/shaders/texture_effect_invert.fs');
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Rotate all 3 scenes (by single parent ScenesParent) around X axis,
    just to show some interesting lighting. }
  WorldTime := WorldTime + SecondsPassed;
  ScenesParent.Rotation := Vector4(1, 0, 0, WorldTime);
end;

end.
