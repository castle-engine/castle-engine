{
  Copyright 2025-2025 Michalis Kamburelis.

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

{ Define the ANIMATE_RED_INTENSITY symbol to make a "pulsating red" effect
  described in the https://castle-engine.io/shaders section
  "Pass Time To Shader Effect". }
{.$define ANIMATE_RED_INTENSITY}

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleScene,
  CastleUIControls, CastleControls, CastleKeysMouse, X3DNodes,
  CastleTimeUtils, X3DFields;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MyScene: TCastleScene;
    CheckboxEnableEffect: TCastleCheckBox;
  private
    Effect: TEffectNode;
    {$ifdef ANIMATE_RED_INTENSITY}
    EffectRedIntensity: TSFFloat;
    WorldTime: TFloatTime;
    {$endif ANIMATE_RED_INTENSITY}
    procedure CheckboxEnableEffectChange(Sender: TObject);
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

procedure TViewMain.Start;

  procedure CreateSimpleEffect;
  var
    EffectPart: TEffectPartNode;
  begin
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;

    {$ifdef ANIMATE_RED_INTENSITY}
    // Add custom field (maps to GLSL uniform "red_intensity"), initially 0.0
    EffectRedIntensity := TSFFloat.Create(Effect, true, 'red_intensity', 0.0);
    Effect.AddCustomField(EffectRedIntensity);
    {$endif ANIMATE_RED_INTENSITY}

    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stFragment;
    EffectPart.SetUrl(['castle-data:/shader_color_effect.fs']);
    Effect.SetParts([EffectPart]);

    MyScene.SetEffects([Effect]);
  end;

begin
  inherited;
  CreateSimpleEffect;
  CheckboxEnableEffect.OnChange := {$ifdef FPC}@{$endif} CheckboxEnableEffectChange;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  {$ifdef ANIMATE_RED_INTENSITY}
  // Update the time
  WorldTime := WorldTime + SecondsPassed;
  // Set the red intensity based on time
  EffectRedIntensity.Send((Sin(10 * WorldTime) + 1) / 2);
  {$endif ANIMATE_RED_INTENSITY}
end;

procedure TViewMain.CheckboxEnableEffectChange(Sender: TObject);
begin
  Effect.Enabled := CheckboxEnableEffect.Checked;
end;

end.
