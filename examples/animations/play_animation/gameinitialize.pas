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
  CastleApplicationProperties, CastleVectors, CastleCameras, CastleViewport,
  CastleURIUtils, X3DNodes, CastleTextureImages;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  ScaleAfterLoading: Single = 1.0;

  { user interface }
  ButtonOpen3D, ButtonOpen2DSpine, ButtonOpen2DStarling, ButtonOpen2DCocos2d, ButtonOpen2DImage, ButtonOpenDialog: TCastleButton;
  AnimationsPanel, LoadOptionsPanel, LoadSamplesPanel: TCastleRectangleControl;
  CheckboxForward, CheckboxLoop, CheckboxMagFilterNearest, CheckboxMinFilterNearest: TCastleCheckbox;
  SliderTransition, SliderScale, SliderFPSLoadOpt: TCastleFloatSlider;
  CheckboxAnimationNamingLoadOpt: TCastleCheckbox;

const
  Margin = 10; // used throughout the user interface

{ Handle button clicks ------------------------------------------------------- }

type

  { TEventsHandler }

  TEventsHandler = class
    class procedure Open(const Url: String);
    class procedure ButtonOpen3DClick(Sender: TObject);
    class procedure ButtonOpen2DSpineClick(Sender: TObject);
    class procedure ButtonOpen2DStarlingClick(Sender: TObject);
    class procedure ButtonOpen2DCocos2dClick(Sender: TObject);
    class procedure ButtonOpen2DImageClick(Sender: TObject);
    class procedure ButtonOpenDialogClick(Sender: TObject);
    class procedure ButtonPlayAnimationClick(Sender: TObject);
    class procedure ScaleChanged(Sender: TObject);
    class procedure TextureMagOptionsChanged(Sender: TObject);
    class procedure TextureMinOptionsChanged(Sender: TObject);
    class procedure StarlingOptionsChanged(Sender: TObject);
  end;

class procedure TEventsHandler.Open(const Url: String);

  procedure RecreateAnimationsPanel;
  var
    Lab: TCastleLabel;
    AnimationName: String;
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

    LabelAndSlider := TCastleHorizontalGroup.Create(AnimationsPanel);
    LabelAndSlider.Spacing := Margin;
    ScrollGroup.InsertFront(LabelAndSlider);

    Lab := TCastleLabel.Create(AnimationsPanel);
    Lab.Caption := 'Scale:';
    Lab.Color := Black;
    LabelAndSlider.InsertFront(Lab);

    SliderScale := TCastleFloatSlider.Create(AnimationsPanel);
    SliderScale.Min := 0.01;
    SliderScale.Max := 5;
    SliderScale.Value := ScaleAfterLoading;
    LabelAndSlider.InsertFront(SliderScale);
    SliderScale.OnChange := @TEventsHandler(nil).ScaleChanged;

    CheckboxMagFilterNearest := TCastleCheckbox.Create(LoadOptionsPanel);
    CheckboxMagFilterNearest.Checked := Scene.RenderOptions.MagnificationFilter = magNearest;
    CheckboxMagFilterNearest.Caption := 'Nearest magnification filter';
    ScrollGroup.InsertFront(CheckboxMagFilterNearest);
    CheckboxMagFilterNearest.OnChange := @TEventsHandler(nil).TextureMagOptionsChanged;

    CheckboxMinFilterNearest := TCastleCheckbox.Create(LoadOptionsPanel);
    CheckboxMinFilterNearest.Checked := Scene.RenderOptions.MinificationFilter = minNearest;
    CheckboxMinFilterNearest.Caption := 'Nearest minification filter';
    ScrollGroup.InsertFront(CheckboxMinFilterNearest);
    CheckboxMinFilterNearest.OnChange := @TEventsHandler(nil).TextureMinOptionsChanged;

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
  { Scale reset - to make the following Viewport.AssignDefaultCamera adjust
    camera to the scene unscaled size }
  Scene.Scale := Vector3(1.0, 1.0, 1.0);
  Scene.Load(Url);
  Viewport.AssignDefaultCamera;
  RecreateAnimationsPanel;
  { Update scale for current slider value }
  ScaleChanged(nil);
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

class procedure TEventsHandler.ButtonOpen2DSpineClick(Sender: TObject);
begin
  Open('../../2d_dragon_spine_game/data/dragon/dragon.json');
end;

{ Simple funtion to parse current settings in options UI
  see ButtonOpen2DStarlingClick for more info. }
function CurrentUIStarlingSettingsToAnchor: String;
var
  AnimNaming: String;
begin
  if CheckboxAnimationNamingLoadOpt.Checked then
    AnimNaming := 'trailing-number'
  else
    AnimNaming := 'strict-underscore';

  Result := '#fps:' + FloatToStrDot(SliderFPSLoadOpt.Value) + ',anim-naming:' + AnimNaming;
end;

class procedure TEventsHandler.ButtonOpen2DStarlingClick(Sender: TObject);
begin
  { When using Starling files you can specify some options, as URL anchors.
    See https://github.com/castle-engine/castle-engine/wiki/Sprite-sheets .

    The sample Starling file below is based on one of many great assets by Kenney,
    check https://kenney.nl/ for more. }
  Open('castle-data:/starling/character_zombie_atlas.starling-xml' + CurrentUIStarlingSettingsToAnchor);
end;

class procedure TEventsHandler.ButtonOpen2DCocos2dClick(Sender: TObject);
begin
  Open('../../../tools/sprite-sheet-to-x3d/samples/wolf.plist');
end;

class procedure TEventsHandler.ButtonOpen2DImageClick(Sender: TObject);
begin
  { You can open normal images in TCastleScene, optionaly you can set rect
    from image in anchor (left, bottom, width, height).
    See https://github.com/castle-engine/castle-engine/wiki/Images . }

  { Full image }
  //Open('castle-data:/starling/character_zombie_atlas.png');
  //Open('castle-data:/starling/character_zombie_atlas.png#left:0,bottom:0,width:1024,height:2048');

  { Just first pose from our character_zombie_atlas.png }
  Open('castle-data:/starling/character_zombie_atlas.png#left:0,bottom:1756,width:182,height:259');
end;

class procedure TEventsHandler.ButtonOpenDialogClick(Sender: TObject);
var
  Url: String;
begin
  Url := Scene.Url;
  if Window.FileDialog('Open model', Url, true, LoadScene_FileFilters) then
  begin
    { In case of Starling add current settings }
    if URIMimeType(Url) = 'application/x-starling-sprite-sheet' then
      Open(Url + CurrentUIStarlingSettingsToAnchor)
    else
      Open(Url);
  end;
end;

class procedure TEventsHandler.ButtonPlayAnimationClick(Sender: TObject);
var
  AnimationName: String;
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

class procedure TEventsHandler.ScaleChanged(Sender: TObject);
begin
  Scene.Scale := Vector3(SliderScale.Value, SliderScale.Value, SliderScale.Value);
end;

class procedure TEventsHandler.TextureMagOptionsChanged(Sender: TObject);
begin
  if CheckboxMagFilterNearest.Checked then
    Scene.RenderOptions.MagnificationFilter := magNearest
  else
    Scene.RenderOptions.MagnificationFilter := magDefault;
end;

class procedure TEventsHandler.TextureMinOptionsChanged(Sender: TObject);
begin
  { There are a lot of other options like:
    - minNearestMipmapNearest,
    - minNearestMipmapLinear,
    - minLinearMipmapNearest,
    - minLinearMipmapLinear,
    - minDefault,
    - minFastest - Alias for minNearest,
    - minNicest - minLinearMipmapLinear,

    But to keep example UI simple we change only minNearest and minDefault. }

  if CheckboxMinFilterNearest.Checked then
    Scene.RenderOptions.MinificationFilter := minNearest
  else
    Scene.RenderOptions.MinificationFilter := minDefault;
end;

class procedure TEventsHandler.StarlingOptionsChanged(Sender: TObject);
var
  AnimationName: String;
  Params: TPlayAnimationParameters;
begin
  { This function is only to show that Starling loading options work.
    In real application you would just load the scene with proper "fps" at the start,
    and later change the speed at runtime by adjusting TCastleScene.TimePlayingSpeed,
    without reloading the model. }

  { Check last loaded model was Starling }
  if URIMimeType(URIDeleteAnchor(Scene.URL)) <> 'application/x-starling-sprite-sheet' then
    Exit;

  try
    { Remember current scale }
    ScaleAfterLoading := SliderScale.Value;

    { Get latest animation }
    if Scene.CurrentAnimation <> nil then
      AnimationName := Scene.CurrentAnimation.X3DName
    else
      AnimationName := '';

    { Reload model with new settings }
    Open(URIDeleteAnchor(Scene.URL) + CurrentUIStarlingSettingsToAnchor);

    { Re-run animation. }
    if AnimationName <> '' then
    begin
      Params := TPlayAnimationParameters.Create;
      try
        Params.Name := AnimationName;
        Params.Forward := CheckboxForward.Checked;
        Params.Loop := CheckboxLoop.Checked;
        Params.TransitionDuration := SliderTransition.Value;
        Scene.PlayAnimation(Params);
      finally FreeAndNil(Params) end;
    end;
  finally
    ScaleAfterLoading := 1.0;
  end;
end;


procedure CreateLoadOptionsUI;
var
  Lab: TCastleLabel;
  ScrollView: TCastleScrollView;
  ScrollGroup: TCastleVerticalGroup;
  LabelAndSlider: TCastleHorizontalGroup;
begin
  LoadOptionsPanel := TCastleRectangleControl.Create(Application);
  LoadOptionsPanel.Anchor(hpLeft, Margin);
  LoadOptionsPanel.Anchor(vpTop, -Margin);
  LoadOptionsPanel.Color := Vector4(1, 1, 1, 0.5);

  { We place TCastleScrollView inside LoadOptionsPanel,
    in case we have too many settings to fit. }
  ScrollView := TCastleScrollView.Create(LoadOptionsPanel);
  ScrollView.ScrollArea.AutoSizeToChildren := true;
  LoadOptionsPanel.InsertFront(ScrollView);

  ScrollGroup := TCastleVerticalGroup.Create(LoadOptionsPanel);
  ScrollGroup.Padding := Margin;
  ScrollGroup.Spacing := Margin;
  ScrollView.ScrollArea.InsertFront(ScrollGroup);

  Lab := TCastleLabel.Create(LoadOptionsPanel);
  Lab.Caption := 'Starling Sprite Sheet load options:';
  Lab.Color := Black;
  ScrollGroup.InsertFront(Lab);

  LabelAndSlider := TCastleHorizontalGroup.Create(LoadOptionsPanel);
  LabelAndSlider.Spacing := Margin;
  ScrollGroup.InsertFront(LabelAndSlider);

  Lab := TCastleLabel.Create(LoadOptionsPanel);
  Lab.Caption := 'FPS:';
  Lab.Color := Black;
  LabelAndSlider.InsertFront(Lab);

  SliderFPSLoadOpt := TCastleFloatSlider.Create(LoadOptionsPanel);
  SliderFPSLoadOpt.Min := 1;
  SliderFPSLoadOpt.Max := 60;
  SliderFPSLoadOpt.Value := DefaultSpriteSheetFramesPerSecond;
  LabelAndSlider.InsertFront(SliderFPSLoadOpt);
  SliderFPSLoadOpt.OnChange := @TEventsHandler(nil).StarlingOptionsChanged;

  CheckboxAnimationNamingLoadOpt := TCastleCheckbox.Create(LoadOptionsPanel);
  CheckboxAnimationNamingLoadOpt.Checked := false;
  CheckboxAnimationNamingLoadOpt.Caption := 'All trailing numbers are animation frames';
  CheckboxAnimationNamingLoadOpt.OnChange := @TEventsHandler(nil).StarlingOptionsChanged;
  ScrollGroup.InsertFront(CheckboxAnimationNamingLoadOpt);

  ScrollView.Width := ScrollGroup.EffectiveWidth;
  ScrollView.Height := Min(ScrollGroup.EffectiveHeight,
    Window.Container.UnscaledHeight - 2 * Margin);

  LoadOptionsPanel.Width := ScrollView.Width;
  LoadOptionsPanel.Height := ScrollView.Height;
  Window.Controls.InsertFront(LoadOptionsPanel);
end;

procedure CreateLoadSamplesUI;
var
  VerticalGroup: TCastleVerticalGroup;
  Lab: TCastleLabel;
  Y: Single;
begin
  Y := -LoadOptionsPanel.Height - Margin * 2;

  LoadSamplesPanel := TCastleRectangleControl.Create(Application);
  LoadSamplesPanel.Color := Vector4(1, 1, 1, 0.5);
  LoadSamplesPanel.Anchor(hpLeft, Margin);
  LoadSamplesPanel.Anchor(vpTop, Y);

  VerticalGroup := TCastleVerticalGroup.Create(LoadSamplesPanel);
  VerticalGroup.Padding := Margin;
  VerticalGroup.Spacing := Margin;
  LoadSamplesPanel.InsertFront(VerticalGroup);

  Lab := TCastleLabel.Create(LoadSamplesPanel);
  Lab.Color := Black;
  Lab.Caption := 'Load sample...';
  VerticalGroup.InsertFront(Lab);

  ButtonOpen3D := TCastleButton.Create(LoadSamplesPanel);
  ButtonOpen3D.Caption := 'glTF Skinned Animation (3D)';
  ButtonOpen3D.OnClick := @TEventsHandler(nil).ButtonOpen3DClick;
  VerticalGroup.InsertFront(ButtonOpen3D);

  ButtonOpen2DSpine := TCastleButton.Create(LoadSamplesPanel);
  ButtonOpen2DSpine.Caption := 'Spine Skeletal Animation (2D)';
  ButtonOpen2DSpine.OnClick := @TEventsHandler(nil).ButtonOpen2DSpineClick;
  VerticalGroup.InsertFront(ButtonOpen2DSpine);

  ButtonOpen2DStarling := TCastleButton.Create(LoadSamplesPanel);
  ButtonOpen2DStarling.Caption := 'Starling Sprite Sheet (2D)';
  ButtonOpen2DStarling.OnClick := @TEventsHandler(nil).ButtonOpen2DStarlingClick;
  VerticalGroup.InsertFront(ButtonOpen2DStarling);

  ButtonOpen2DCocos2d := TCastleButton.Create(LoadSamplesPanel);
  ButtonOpen2DCocos2d.Caption := 'Cocos2d Sprite Sheet (2D)';
  ButtonOpen2DCocos2d.OnClick := @TEventsHandler(nil).ButtonOpen2DCocos2dClick;
  VerticalGroup.InsertFront(ButtonOpen2DCocos2d);

  ButtonOpen2DImage := TCastleButton.Create(LoadSamplesPanel);
  ButtonOpen2DImage.Caption := 'Image 2D';
  ButtonOpen2DImage.OnClick := @TEventsHandler(nil).ButtonOpen2DImageClick;
  VerticalGroup.InsertFront(ButtonOpen2DImage);

  ButtonOpenDialog := TCastleButton.Create(LoadSamplesPanel);
  ButtonOpenDialog.Caption := 'Open any model on disk';
  ButtonOpenDialog.OnClick := @TEventsHandler(nil).ButtonOpenDialogClick;
  VerticalGroup.InsertFront(ButtonOpenDialog);

  LoadSamplesPanel.Width := VerticalGroup.EffectiveWidth;
  LoadSamplesPanel.Height := VerticalGroup.EffectiveHeight;
  Window.Controls.InsertFront(LoadSamplesPanel);
end;

{ routines ------------------------------------------------------------------- }
{ One-time initialization of resources. }
procedure ApplicationInitialize;
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

  CreateLoadOptionsUI;
  CreateLoadSamplesUI;

  // pretend ButtonOpen2DSpine was clicked
  TEventsHandler.ButtonOpen2DSpineClick(nil);
  //TEventsHandler.ButtonOpen2DStarlingClick(nil);
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
