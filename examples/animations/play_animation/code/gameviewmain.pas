﻿{
  Copyright 2018-2024 Michalis Kamburelis, Andrzej Kilijański.

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

uses SysUtils, Classes, Math,
  CastleScene, CastleSceneCore, CastleControls, CastleLog,
  CastleFilesUtils, CastleColors, CastleUIControls, X3DLoad, CastleUtils,
  CastleApplicationProperties, CastleVectors, CastleCameras, CastleViewport,
  CastleUriUtils, X3DNodes, CastleTextureImages;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    ButtonOpen3D, ButtonOpen2DSpine, ButtonOpen2DStarling, ButtonOpen2DCocos2d, ButtonOpen2DImage, ButtonOpenDialog: TCastleButton;
    SceneAnimationButtons: TCastleUserInterface;
    CheckboxForward, CheckboxLoop, CheckboxMagFilterNearest, CheckboxMinFilterNearest, CheckboxAnimationNamingLoadOpt: TCastleCheckbox;
    SliderTransition, SliderScale, SliderFPSLoadOpt: TCastleFloatSlider;
    SliderOptimization: TCastleIntegerSlider;
  private
    { Event handlers }
    procedure OpenScene(const Url: String);
    procedure ClickButtonOpen3D(Sender: TObject);
    procedure ClickButtonOpen2DSpine(Sender: TObject);
    procedure ClickButtonOpen2DStarling(Sender: TObject);
    procedure ClickButtonOpen2DCocos2d(Sender: TObject);
    procedure ClickButtonOpen2DImage(Sender: TObject);
    procedure ClickButtonOpenDialog(Sender: TObject);
    procedure ClickButtonPlayAnimation(Sender: TObject);
    procedure ChangedScale(Sender: TObject);
    procedure ChangedTextureMagOptions(Sender: TObject);
    procedure ChangedTextureMinOptions(Sender: TObject);
    procedure ChangedStarlingOptions(Sender: TObject);
    procedure ChangedOptimization(Sender: TObject);

    { Simple function to parse current settings in options UI
      see ClickButtonOpen2DStarling for more info. }
    function CurrentUIStarlingSettingsToAnchor: String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses CastleWindow, CastleComponentSerialize, CastleRenderOptions;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { attach events }
  ButtonOpen3D.OnClick := {$ifdef FPC}@{$endif} ClickButtonOpen3D;
  ButtonOpen2DSpine.OnClick := {$ifdef FPC}@{$endif} ClickButtonOpen2DSpine;
  ButtonOpen2DStarling.OnClick := {$ifdef FPC}@{$endif} ClickButtonOpen2DStarling;
  ButtonOpen2DCocos2d.OnClick := {$ifdef FPC}@{$endif} ClickButtonOpen2DCocos2d;
  ButtonOpen2DImage.OnClick := {$ifdef FPC}@{$endif} ClickButtonOpen2DImage;
  ButtonOpenDialog.OnClick := {$ifdef FPC}@{$endif} ClickButtonOpenDialog;
  SliderFPSLoadOpt.OnChange := {$ifdef FPC}@{$endif} ChangedStarlingOptions;
  CheckboxAnimationNamingLoadOpt.OnChange := {$ifdef FPC}@{$endif} ChangedStarlingOptions;
  SliderScale.OnChange := {$ifdef FPC}@{$endif} ChangedScale;
  SliderOptimization.OnChange := {$ifdef FPC}@{$endif} ChangedOptimization;
  CheckboxMagFilterNearest.OnChange := {$ifdef FPC}@{$endif} ChangedTextureMagOptions;
  CheckboxMinFilterNearest.OnChange := {$ifdef FPC}@{$endif} ChangedTextureMinOptions;

  // pretend ButtonOpen2DSpine was clicked
  ClickButtonOpen2DSpine(nil);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.OpenScene(const Url: String);

  procedure CreateAnimationsButtons;
  var
    AnimationName: String;
    Button: TCastleButton;
  begin
    { Remove previous animation controls. }
    SceneAnimationButtons.ClearControls;

    for AnimationName in Scene.AnimationsList do
    begin
      Button := TCastleButton.Create(Self);
      Button.Caption := AnimationName;
      Button.OnClick := {$ifdef FPC}@{$endif} ClickButtonPlayAnimation;
      SceneAnimationButtons.InsertFront(Button);
    end;
  end;

begin
  { Scale reset - to make the following Viewport.AssignDefaultCamera adjust
    camera to the scene unscaled size }
  Scene.Scale := Vector3(1.0, 1.0, 1.0);
  Scene.Load(Url);

  { Set blending sort following "NavigationInfo.blendingSort" info from scene.
    This means we use 2D sorting e.g. for Spine models by default. }
  if (Scene.NavigationInfoStack.Top <> nil) and
     (Scene.NavigationInfoStack.Top.BlendingSort <> sortAuto) then
    Viewport.BlendingSort := Scene.NavigationInfoStack.Top.BlendingSort
  else
    Viewport.BlendingSort := sortAuto;

  Viewport.AssignDefaultCamera;

  CreateAnimationsButtons;

  { Update scale for current SliderScale value }
  ChangedScale(nil);
end;

procedure TViewMain.ClickButtonOpen3D(Sender: TObject);
begin
  OpenScene('castle-data:/gltf_knight/knight.gltf');

  { Note: TransitionDuration is not supported for castle-anim-frames.
    TransitionDuration is supported on other model formats (in particular
    glTF, Spine, X3D support animations too). }
  //OpenScene('../../deprecated_resource_animations/data/deprecated/knight_single_castle_anim_frames/knight.castle-anim-frames');
end;

procedure TViewMain.ClickButtonOpen2DSpine(Sender: TObject);
begin
  OpenScene('castle-data:/spine_dragon/dragon.json');
end;

function TViewMain.CurrentUIStarlingSettingsToAnchor: String;
var
  AnimNaming: String;
begin
  if CheckboxAnimationNamingLoadOpt.Checked then
    AnimNaming := 'trailing-number'
  else
    AnimNaming := 'strict-underscore';

  Result := '#fps:' + FloatToStrDot(SliderFPSLoadOpt.Value) + ',anim-naming:' + AnimNaming;
end;

procedure TViewMain.ClickButtonOpen2DStarling(Sender: TObject);
begin
  { When using Starling files you can specify some options, as URL anchors.
    See https://castle-engine.io/sprite_sheets .

    The sample Starling file below is based on one of many great assets by Kenney,
    check https://kenney.nl/ for more. }
  OpenScene('castle-data:/starling/character_zombie_atlas.starling-xml' + CurrentUIStarlingSettingsToAnchor);
end;

procedure TViewMain.ClickButtonOpen2DCocos2d(Sender: TObject);
begin
  OpenScene('castle-data:/cocos2d/wolf.plist');
end;

procedure TViewMain.ClickButtonOpen2DImage(Sender: TObject);
begin
  { You can open normal images in TCastleScene, optionaly you can set rect
    from image in anchor (left, bottom, width, height).
    See https://castle-engine.io/using_images . }

  { Full image }
  //OpenScene('castle-data:/starling/character_zombie_atlas.png');
  //OpenScene('castle-data:/starling/character_zombie_atlas.png#left:0,bottom:0,width:1024,height:2048');

  { Just first pose from our character_zombie_atlas.png }
  OpenScene('castle-data:/starling/character_zombie_atlas.png#left:0,bottom:1756,width:182,height:259');
end;

procedure TViewMain.ClickButtonOpenDialog(Sender: TObject);
var
  Url: String;
begin
  Url := Scene.Url;
  if Application.MainWindow.FileDialog('Open model', Url, true, LoadScene_FileFilters) then
  begin
    { In case of Starling add current settings }
    if UriMimeType(Url) = 'application/x-starling-sprite-sheet' then
      OpenScene(Url + CurrentUIStarlingSettingsToAnchor)
    else
      OpenScene(Url);
  end;
end;

procedure TViewMain.ClickButtonPlayAnimation(Sender: TObject);
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

procedure TViewMain.ChangedScale(Sender: TObject);
begin
  Scene.Scale := Vector3(SliderScale.Value, SliderScale.Value, SliderScale.Value);
end;

procedure TViewMain.ChangedTextureMagOptions(Sender: TObject);
begin
  if CheckboxMagFilterNearest.Checked then
    Scene.RenderOptions.MagnificationFilter := magNearest
  else
    Scene.RenderOptions.MagnificationFilter := magDefault;
end;

procedure TViewMain.ChangedTextureMinOptions(Sender: TObject);
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

procedure TViewMain.ChangedStarlingOptions(Sender: TObject);
var
  AnimationName: String;
  Params: TPlayAnimationParameters;
begin
  { This function is only to show that Starling loading options work.
    In real application you would just load the scene with proper "fps" at the start,
    and later change the speed at runtime by adjusting TCastleScene.TimePlayingSpeed,
    without reloading the model. }

  { Check last loaded model was Starling }
  if UriMimeType(URIDeleteAnchor(Scene.URL)) <> 'application/x-starling-sprite-sheet' then
    Exit;

  { Get latest animation }
  if Scene.CurrentAnimation <> nil then
    AnimationName := Scene.CurrentAnimation.X3DName
  else
    AnimationName := '';

  { Reload model with new settings }
  OpenScene(URIDeleteAnchor(Scene.URL) + CurrentUIStarlingSettingsToAnchor);

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
end;

procedure TViewMain.ChangedOptimization(Sender: TObject);
begin
  { Activate a few optimizations based on the slider value.
    See examples/animations/optimize_animations_test/ for more notes
    about optimizing. }
  Viewport.DynamicBatching := SliderOptimization.Value > 0;
  InternalFastTransformUpdate := SliderOptimization.Value > 0;
  OptimizeExtensiveTransformations := SliderOptimization.Value > 0;
  case SliderOptimization.Value of
    0, 1: Scene.AnimateSkipTicks := 0;
    2: Scene.AnimateSkipTicks := 1;
  end;
end;

end.
