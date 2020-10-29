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

uses SysUtils, Math,
  CastleWindow, CastleScene, CastleSceneCore, CastleControls, CastleLog,
  CastleFilesUtils, CastleColors, CastleUIControls, X3DLoad, CastleUtils,
  CastleApplicationProperties, CastleVectors, CastleCameras, CastleViewport;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;

  { user interface }
  ButtonOpen3D, ButtonOpen2D, ButtonOpenDialog: TCastleButton;
  AnimationsPanel: TCastleRectangleControl;
  CheckboxForward, CheckboxLoop: TCastleCheckbox;
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
    AnimationName: string;
    Button: TCastleButton;
    ScrollView: TCastleScrollView;
    ScrollGroup: TCastleVerticalGroup;
    LabelAndSlider: TCastleHorizontalGroup;
  begin
    { Free previous animation controls.
      Note that everything below is owned by new AnimationsPanel instance. }
    FreeAndNil(AnimationsPanel);

    AnimationsPanel := TCastleRectangleControl.Create(Application);
    AnimationsPanel.Anchor(hpRight, -Margin);
    AnimationsPanel.Anchor(vpTop, -Margin);
    AnimationsPanel.Color := Vector4(1, 1, 1, 0.5);

    { We place TCastleScrollView inside AnimationsPanel,
      in case we have too many animations to fit. }
    ScrollView := TCastleScrollView.Create(AnimationsPanel);
    ScrollView.ScrollArea.AutoSizeToChildren := true;
    AnimationsPanel.InsertFront(ScrollView);

    ScrollGroup := TCastleVerticalGroup.Create(Application);
    ScrollGroup.Padding := Margin;
    ScrollGroup.Spacing := Margin;
    ScrollView.ScrollArea.InsertFront(ScrollGroup);

    CheckboxForward := TCastleCheckbox.Create(AnimationsPanel);
    CheckboxForward.Checked := true;
    CheckboxForward.Caption := 'Forward';
    ScrollGroup.InsertFront(CheckboxForward);

    CheckboxLoop := TCastleCheckbox.Create(AnimationsPanel);
    CheckboxLoop.Checked := true;
    CheckboxLoop.Caption := 'Loop';
    ScrollGroup.InsertFront(CheckboxLoop);

    LabelAndSlider := TCastleHorizontalGroup.Create(AnimationsPanel);
    LabelAndSlider.Spacing := Margin;
    ScrollGroup.InsertFront(LabelAndSlider);

    Lab := TCastleLabel.Create(AnimationsPanel);
    Lab.Caption := 'Transition:';
    Lab.Color := Black;
    LabelAndSlider.InsertFront(Lab);

    SliderTransition := TCastleFloatSlider.Create(AnimationsPanel);
    SliderTransition.Min := 0;
    SliderTransition.Max := 5;
    LabelAndSlider.InsertFront(SliderTransition);

    Lab := TCastleLabel.Create(AnimationsPanel);
    Lab.Caption := 'Click to play animation...';
    Lab.Color := Black;
    ScrollGroup.InsertFront(Lab);

    for AnimationName in Scene.AnimationsList do
    begin
      Button := TCastleButton.Create(AnimationsPanel);
      Button.Caption := AnimationName;
      Button.OnClick := @TEventsHandler(nil).ButtonPlayAnimationClick;
      ScrollGroup.InsertFront(Button);
    end;

    ScrollView.Width := ScrollGroup.EffectiveWidth;
    ScrollView.Height := Min(ScrollGroup.EffectiveHeight,
      Window.Container.UnscaledHeight - 2 * Margin);

    AnimationsPanel.Width := ScrollView.Width;
    AnimationsPanel.Height := ScrollView.Height;
    Window.Controls.InsertFront(AnimationsPanel);
  end;

begin
  Scene.Load(Url);
  Viewport.AssignDefaultCamera;
  RecreateAnimationsPanel;
end;

class procedure TEventsHandler.ButtonOpen3DClick(Sender: TObject);
begin
  Open('../../fps_game/data/knight_creature/knight.gltf');

  //Open('../resource_animations/data/knight_single_x3d/knight.x3dv');

  { Note: TransitionDuration is not supported for castle-anim-frames.
    TransitionDuration is supported on other model formats (in particular
    glTF, Spine, X3D support animations too). }
  //Open('../resource_animations/data/knight_single_castle_anim_frames/knight.castle-anim-frames');
end;

class procedure TEventsHandler.ButtonOpen2DClick(Sender: TObject);
begin
  Open('../../2d_dragon_spine_game/data/dragon/dragon.json');
end;

class procedure TEventsHandler.ButtonOpenDialogClick(Sender: TObject);
var
  Url: string;
begin
  Url := Scene.Url;
  if Window.FileDialog('Open model', Url, true, LoadScene_FileFilters) then
    Open(Url);
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
    Params.Forward := CheckboxForward.Checked;
    Params.Loop := CheckboxLoop.Checked;
    Params.TransitionDuration := SliderTransition.Value;
    Scene.PlayAnimation(Params);
  finally FreeAndNil(Params) end;
end;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  Y: Single;
begin
  { This is an optimization useful when you animate a hierarchy of Transform
    nodes (which often happens in case of Spine animation).
    The animation blending should work the same, regardless if this is false or true. }
  // OptimizeExtensiveTransformations := true;

  { Assign Window callbacks }
  Window.FpsShowOnCaption := true;

  { For a scalable UI (adjusts to any window size in a smart way) }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := false; // we will explicitly call AssignDefaultCamera
  Viewport.AutoNavigation := false;
  Viewport.NavigationType := ntExamine; // always Examine, regardless of scene
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Y := -Margin;

  ButtonOpen3D := TCastleButton.Create(Application);
  ButtonOpen3D.Caption := 'Load sample 3D model';
  ButtonOpen3D.OnClick := @TEventsHandler(nil).ButtonOpen3DClick;
  ButtonOpen3D.Anchor(hpLeft, Margin);
  ButtonOpen3D.Anchor(vpTop, Y);
  Window.Controls.InsertFront(ButtonOpen3D);
  Y := Y - (ButtonOpen3D.EffectiveHeight + Margin);

  ButtonOpen2D := TCastleButton.Create(Application);
  ButtonOpen2D.Caption := 'Load sample 2D model';
  ButtonOpen2D.OnClick := @TEventsHandler(nil).ButtonOpen2DClick;
  ButtonOpen2D.Anchor(hpLeft, Margin);
  ButtonOpen2D.Anchor(vpTop, Y);
  Window.Controls.InsertFront(ButtonOpen2D);
  Y := Y - (ButtonOpen2D.EffectiveHeight + Margin);

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
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
end.
