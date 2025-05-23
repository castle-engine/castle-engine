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

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleScene, CastleTimeUtils,
  CastleUIControls, CastleControls, CastleKeysMouse, X3DNodes, X3DFields;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonNoEffects, ButtonEffects1, ButtonEffects2, ButtonEffectsCombined: TCastleButton;
    MainBackground: TCastleBackground;
  private
    EffectColor, EffectNoise: TEffectNode;
    EffectColorIntensity: TSFFloat;
    LifeTime: TFloatTime;
    procedure ClickNoEffects(Sender: TObject);
    procedure ClickEffects1(Sender: TObject);
    procedure ClickEffects2(Sender: TObject);
    procedure ClickEffectsCombined(Sender: TObject);
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
  CastleRenderOptions;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  { Create EffectColor node.

    Exposes 'intensity' uniform shader variable
    using X3D field EffectColorIntensity.
    You can change such uniform shader variable at any time.
    You can even change it very often (e.g. every frame),
    changing uniform values is cheap. }
  procedure CreateEffectColor;
  var
    EffectPart: TEffectPartNode;
  begin
    EffectColor := TEffectNode.Create;
    EffectColor.Language := slGLSL;
    { Don't use reference counting to free this X3D node when nothing refers to it.
      We will manage it manually. }
    EffectColor.KeepExistingBegin;

    { Add custom field (maps to GLSL uniform "color"), initially white }
    EffectColorIntensity := TSFFloat.Create(EffectColor, true, 'intensity', 0.0);
    EffectColor.AddCustomField(EffectColorIntensity);

    { Add TEffectPartNode which actually contains shader code.
      You can load shader code from file,
      you could also set it explicitly from string using EffectPart.Contents := '...'; }
    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stFragment;
    EffectPart.SetUrl(['castle-data:/shaders/skybox_color_effect.fs']);
    EffectColor.SetParts([EffectPart]);
  end;

  { Create EffectNoise node. }
  procedure CreateEffectNoise;
  var
    EffectParts: array of TEffectPartNode;
  begin
    EffectNoise := TEffectNode.Create;
    EffectNoise.Language := slGLSL;
    EffectNoise.KeepExistingBegin;

    { Use 2 TEffectPartNode instance, because the GLSL noise functions
      live in a separate file (to show that we can have multiple shader parts,
      and also because they've been copied from
      https://gist.github.com/patriciogonzalezvivo/670c22f3966e662d2f83 ). }
    SetLength(EffectParts, 2);

    EffectParts[0] := TEffectPartNode.Create;
    EffectParts[0].ShaderType := stFragment;
    EffectParts[0].SetUrl(['castle-data:/shaders/skybox_noise.fs']);

    EffectParts[1] := TEffectPartNode.Create;
    EffectParts[1].ShaderType := stFragment;
    EffectParts[1].SetUrl(['castle-data:/shaders/noise.glsl']);

    EffectNoise.SetParts(EffectParts);
  end;

begin
  inherited;
  // assign OnClick handler to buttons
  ButtonNoEffects.OnClick := {$ifdef FPC}@{$endif} ClickNoEffects;
  ButtonEffects1.OnClick := {$ifdef FPC}@{$endif} ClickEffects1;
  ButtonEffects2.OnClick := {$ifdef FPC}@{$endif} ClickEffects2;
  ButtonEffectsCombined.OnClick := {$ifdef FPC}@{$endif} ClickEffectsCombined;

  CreateEffectColor;
  CreateEffectNoise;
end;

procedure TViewMain.Stop;
begin
  { Free the effect nodes. We do it gracefully: let reference counting
    handle them again (by KeepExistingEnd), and free them *now* only if
    nothing else refers to them now (by FreeIfUnusedAndNil instead of FreeAndNil).
    This means that their free will happen
    - right now, if MainBackground is not using them
    - or when MainBackground is destroyed in "inherited" of Stop, if it is using them
  }
  EffectColor.KeepExistingEnd;
  EffectNoise.KeepExistingEnd;
  FreeIfUnusedAndNil(EffectColor);
  FreeIfUnusedAndNil(EffectNoise);
  EffectColorIntensity := nil; // was (or will be) implicitly freed by EffectColor freeing
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  // Animate the effect color 'intensity' uniform, just to show that we can
  LifeTime := LifeTime + SecondsPassed;
  EffectColorIntensity.Send(Frac(LifeTime * 2.0));
end;

procedure TViewMain.ClickNoEffects(Sender: TObject);
begin
  MainBackground.SetEffects([]);
end;

procedure TViewMain.ClickEffects1(Sender: TObject);
begin
  MainBackground.SetEffects([EffectColor]);
  LifeTime := 0; // reset the lifetime, so that the color effect starts from the beginning
end;

procedure TViewMain.ClickEffects2(Sender: TObject);
begin
  MainBackground.SetEffects([EffectNoise]);
end;

procedure TViewMain.ClickEffectsCombined(Sender: TObject);
begin
  { Note: Order matters, it determines the order of calling
    the effects calculations. }
  MainBackground.SetEffects([EffectColor, EffectNoise]);
  LifeTime := 0; // reset the lifetime, so that the color effect starts from the beginning
end;

end.
