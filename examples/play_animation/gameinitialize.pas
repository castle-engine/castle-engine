{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleScene, CastleSceneCore, CastleControls, CastleLog,
  CastleFilesUtils, CastleColors, CastleUIControls, X3DLoad, CastleUtils,
  CastleApplicationProperties, CastleVectors;

var
  Window: TCastleWindow;
  Scene: TCastleScene;

  { user interface }
  ButtonOpen3D, ButtonOpen2D, ButtonOpenDialog: TCastleButton;
  AnimationsPanel: TCastleRectangleControl;
  SwitchForward, SwitchLoop: TCastleSwitchControl;
  SliderTransition: TCastleFloatSlider;

const
  Margin = 10; // used throughout the user interface

{ Handle button clicks ------------------------------------------------------- }

type
  TEventsHandler = class
    class procedure Open(const Url: string);
    class procedure ButtonOpen3DClick(Sender: TObject);
    class procedure ButtonOpen2DClick(Sender: TObject);
    class procedure ButtonOpenDialogClick(Sender: TObject);
    class procedure ButtonPlayAnimationClick(Sender: TObject);
  end;

class procedure TEventsHandler.Open(const Url: string);

  procedure RecreateAnimationsPanel;
  var
    Lab: TCastleLabel;
    W, H: Integer;
    AnimationName: string;
    Button: TCastleButton;
  begin
    FreeAndNil(AnimationsPanel);

    AnimationsPanel := TCastleRectangleControl.Create(Application);
    AnimationsPanel.Anchor(hpRight, -Margin);
    AnimationsPanel.Anchor(vpTop, -Margin);
    AnimationsPanel.Color := Vector4(1, 1, 1, 0.1);

    W := 0;
    H := Margin;

    Lab := TCastleLabel.Create(AnimationsPanel);
    Lab.Caption := 'Forward:';
    Lab.Anchor(hpLeft, Margin);
    Lab.Anchor(vpTop, -H);
    AnimationsPanel.InsertFront(Lab);

    SwitchForward := TCastleSwitchControl.Create(AnimationsPanel);
    SwitchForward.Checked := true;
    SwitchForward.Anchor(hpLeft, Lab.CalculatedWidth + 2 * Margin);
    SwitchForward.Anchor(vpTop, -H);
    AnimationsPanel.InsertFront(SwitchForward);

    H += Lab.CalculatedHeight + Margin;
    MaxVar(W, Lab.CalculatedWidth + SwitchForward.CalculatedWidth + 3 * Margin);

    Lab := TCastleLabel.Create(AnimationsPanel);
    Lab.Caption := 'Loop:';
    Lab.Anchor(hpLeft, Margin);
    Lab.Anchor(vpTop, -H);
    AnimationsPanel.InsertFront(Lab);

    SwitchLoop := TCastleSwitchControl.Create(AnimationsPanel);
    SwitchLoop.Checked := true;
    SwitchLoop.Anchor(hpLeft, Lab.CalculatedWidth + 2 * Margin);
    SwitchLoop.Anchor(vpTop, -H);
    AnimationsPanel.InsertFront(SwitchLoop);

    H += Lab.CalculatedHeight + Margin;
    MaxVar(W, Lab.CalculatedWidth + SwitchLoop.CalculatedWidth + 3 * Margin);

    Lab := TCastleLabel.Create(AnimationsPanel);
    Lab.Caption := 'Transition:';
    Lab.Anchor(hpLeft, Margin);
    Lab.Anchor(vpTop, -H);
    AnimationsPanel.InsertFront(Lab);

    SliderTransition := TCastleFloatSlider.Create(AnimationsPanel);
    SliderTransition.Min := 0;
    SliderTransition.Max := 5;
    SliderTransition.Anchor(hpLeft, Lab.CalculatedWidth + 2 * Margin);
    SliderTransition.Anchor(vpTop, -H);
    AnimationsPanel.InsertFront(SliderTransition);

    H += Lab.CalculatedHeight + Margin;
    MaxVar(W, Lab.CalculatedWidth + SliderTransition.CalculatedWidth + 3 * Margin);

    Lab := TCastleLabel.Create(AnimationsPanel);
    Lab.Caption := 'Click to play animation...';
    Lab.Anchor(hpLeft, Margin);
    Lab.Anchor(vpTop, -H);
    AnimationsPanel.InsertFront(Lab);

    H += Lab.CalculatedHeight + Margin;
    MaxVar(W, Lab.CalculatedWidth + 2 * Margin);

    for AnimationName in Scene.AnimationsList do
    begin
      Button := TCastleButton.Create(Application);
      Button.Caption := AnimationName;
      Button.Anchor(hpLeft, Margin);
      Button.Anchor(vpTop, -H);
      Button.OnClick := @TEventsHandler(nil).ButtonPlayAnimationClick;
      AnimationsPanel.InsertFront(Button);

      H += Button.CalculatedHeight + Margin;
      MaxVar(W, Button.CalculatedWidth + 2 * Margin);
    end;

    AnimationsPanel.Width := W;
    AnimationsPanel.Height := H;
    Window.Controls.InsertFront(AnimationsPanel);
  end;

begin
  Scene.Load(Url);
  { force recreating camera, to adjust to new scene bounding box }
  Window.SceneManager.Camera.Free;
  { create camera with Examine navigation (regardless of NavigationInfo inside scene) }
  Window.SceneManager.ExamineCamera;
  RecreateAnimationsPanel;
end;

class procedure TEventsHandler.ButtonOpen3DClick(Sender: TObject);
begin
  Open('../resource_animations/data/knight_single_castle_anim_frames/knight.castle-anim-frames');
end;

class procedure TEventsHandler.ButtonOpen2DClick(Sender: TObject);
begin
  Open('../2d_dragon_spine_game/data/dragon/dragon.json');
end;

class procedure TEventsHandler.ButtonOpenDialogClick(Sender: TObject);
var
  Url: string;
begin
  Url := Scene.Url;
  if Window.FileDialog('Open model', Url, true, Load3D_FileFilters) then
    Scene.Load(Url);
end;

class procedure TEventsHandler.ButtonPlayAnimationClick(Sender: TObject);
var
  AnimationName: string;
  Params: TPlayAnimationParameters;
begin
  AnimationName := (Sender as TCastleButton).Caption;
  Params := TPlayAnimationParameters.Create;
  try
    Params.Name := AnimationName;
    Params.Forward := SwitchForward.Checked;
    Params.Loop := SwitchLoop.Checked;
    Scene.PlayAnimation(Params);
  finally FreeAndNil(Params) end;
end;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  Y: Integer;
begin
  { Assign Window callbacks }
  Window.FpsShowOnCaption := true;

  { For a scalable UI (adjusts to any window size in a smart way) }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Scene := TCastleScene.Create(Application);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Y := -Margin;

  ButtonOpen3D := TCastleButton.Create(Application);
  ButtonOpen3D.Caption := 'Load sample 3D model';
  ButtonOpen3D.OnClick := @TEventsHandler(nil).ButtonOpen3DClick;
  ButtonOpen3D.Anchor(hpLeft, Margin);
  ButtonOpen3D.Anchor(vpTop, Y);
  Window.Controls.InsertFront(ButtonOpen3D);
  Y -= ButtonOpen3D.CalculatedHeight + Margin;

  ButtonOpen2D := TCastleButton.Create(Application);
  ButtonOpen2D.Caption := 'Load sample 2D model';
  ButtonOpen2D.OnClick := @TEventsHandler(nil).ButtonOpen2DClick;
  ButtonOpen2D.Anchor(hpLeft, Margin);
  ButtonOpen2D.Anchor(vpTop, Y);
  Window.Controls.InsertFront(ButtonOpen2D);
  Y -= ButtonOpen2D.CalculatedHeight + Margin;

  ButtonOpenDialog := TCastleButton.Create(Application);
  ButtonOpenDialog.Caption := 'Open any model on disk';
  ButtonOpenDialog.OnClick := @TEventsHandler(nil).ButtonOpenDialogClick;
  ButtonOpenDialog.Anchor(hpLeft, Margin);
  ButtonOpenDialog.Anchor(vpTop, Y);
  Window.Controls.InsertFront(ButtonOpenDialog);

  // pretend ButtonOpen2D was clicked
  TEventsHandler.ButtonOpen2DClick(nil);
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'play_animation';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
end.
