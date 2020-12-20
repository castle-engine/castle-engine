{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of using multiple viewports (TCastleViewport) to view the same world.
  All TCastleViewport instances have the same TCastleViewport.Items reference. }

{ If defined, then the 3D world will contain an additional animation
  of a dinosaur. It's most suitable when as the main scene you load
  data/bridge_final.x3dv .
  This shows that animation from 2nd file works fully with mirrors
  by GeneratedCubeMapTexture in 1st file, also in custom viewports. }
{ $define ADD_ANIMATION}

{$I castleconf.inc}

uses SysUtils, Classes,
  CastleWindow, X3DNodes, CastleSceneCore, CastleScene, CastleRenderOptions,
  CastleUIControls, CastleCameras, CastleQuaternions, CastleVectors,
  CastleControls, CastleLog, CastleScreenEffects, CastleViewport,
  CastleUtils, CastleGLUtils, X3DLoad, CastleGLShaders, CastleParameters,
  CastleStringUtils, CastleKeysMouse, CastleColors, CastleControlsImages,
  CastleApplicationProperties, CastleTransform
  {$ifdef ADD_ANIMATION} , CastleFilesUtils, CastleTransform {$endif};

{ TMyViewport ---------------------------------------------------------------- }

type
  { Derive our own TCastleViewport descendants, just to demo that we can. }
  TMyViewport = class(TCastleViewport)
  public
    Caption: string;
  end;

{ TWireViewport -------------------------------------------------------------- }

type
  TWireViewport = class(TMyViewport)
    procedure Render; override;
  end;

procedure TWireViewport.Render;
begin
  { To make wireframe rendering, but only in this viewport
    (not in other viewports), we temporarily switch WireframeEffect
    of the MainScene.

    In a desktop OpenGL, an alternative way to do this is to switch
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    and then go back by
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    But this is not possible on OpenGLES. }

  Items.MainScene.RenderOptions.WireframeEffect := weWireframeOnly;
  inherited;
  Items.MainScene.RenderOptions.WireframeEffect := weNormal;
end;

{ TScreenEffectDemoViewport -------------------------------------------------- }

type
  TScreenEffectDemoViewport = class(TMyViewport)
    constructor Create(AOwner: TComponent); override;
  end;

constructor TScreenEffectDemoViewport.Create(AOwner: TComponent);
var
  ScreenEffect: TScreenEffectNode;
  ComposedShader: TComposedShaderNode;
  FragmentShader: TShaderPartNode;
begin
  inherited;

  FragmentShader := TShaderPartNode.Create;
  FragmentShader.ShaderType := stFragment;
  FragmentShader.Contents :=
    'void main (void)' +NL+
    '{' +NL+
      '  vec4 left   = screen_get_color(ivec2(screen_x() - 1, screen_y()));' + NL +
      '  vec4 right  = screen_get_color(ivec2(screen_x() + 1, screen_y()));' + NL +
      '  vec4 top    = screen_get_color(ivec2(screen_x(), screen_y() - 1));' + NL +
      '  vec4 bottom = screen_get_color(ivec2(screen_x(), screen_y() + 1));' + NL +
      '  gl_FragColor = (1.0 + abs(left - right) + abs(top - bottom)) / 2.0;' + NL +
    '}';

  ComposedShader := TComposedShaderNode.Create;
  ComposedShader.SetParts([FragmentShader]);

  ScreenEffect := TScreenEffectNode.Create;
  ScreenEffect.SetShaders([ComposedShader]);

  AddScreenEffect(ScreenEffect);
end;

{ TFocusedFrame -------------------------------------------------------------- }

type
  { Draw frame around the control's rectangle, if focused (under cursor). }
  TFocusedFrame = class(TCastleUserInterface)
  public
    procedure Render; override;
    procedure SetFocused(const Value: boolean); override;
  end;

procedure TFocusedFrame.Render;
begin
  if Focused then
    Theme.Draw(RenderRect, tiActiveFrame);
end;

procedure TFocusedFrame.SetFocused(const Value: boolean);
begin
  if Value <> Focused then
    { The TFocusedFrame.Render is based on Focused value. }
    VisibleChange([chRender]);

  inherited;
end;

{ ---------------------------------------------------------------------------- }

var
  Window: TCastleWindowBase;
  RootTransform: TCastleRootTransform;
  Scene: TCastleScene;
  Viewports: array [0..3] of TMyViewport;
  ViewportFrames: array [0..3] of TFocusedFrame;
  ViewportsLabels: array [0..3] of TCastleLabel;
  OpenButton, QuitButton: TCastleButton;

const
  Margin = 5;

procedure Resize(Container: TUIContainer);
var
  W, H, TopMargin: Single;
begin
  TopMargin := OpenButton.EffectiveHeight + 2 * Margin;
  W := Window.Width / 2;
  H := (Window.Height - TopMargin) / 2;

  Viewports[0].Left   :=       Margin;
  Viewports[0].Bottom :=       Margin;
  Viewports[0].Width  := W - 2*Margin;
  Viewports[0].Height := H - 2*Margin;

  Viewports[1].Left   := W +   Margin;
  Viewports[1].Bottom :=       Margin;
  Viewports[1].Width  := W - 2*Margin;
  Viewports[1].Height := H - 2*Margin;

  Viewports[2].Left   :=       Margin;
  Viewports[2].Bottom := H +   Margin;
  Viewports[2].Width  := W - 2*Margin;
  Viewports[2].Height := H - 2*Margin;

  Viewports[3].Left   := W +   Margin;
  Viewports[3].Bottom := H +   Margin;
  Viewports[3].Width  := W - 2*Margin;
  Viewports[3].Height := H - 2*Margin;
end;

procedure CameraReinitialize;
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
  begin
    { set different camera views for all viewports, to make it interesting }
    Viewports[I].AssignDefaultCamera;
    Viewports[I].AssignDefaultNavigation;
    if (I < 3) and
       (Viewports[I].Navigation is TCastleExamineNavigation) then
      (Viewports[I].Navigation as TCastleExamineNavigation).Rotations :=
        QuatFromAxisAngle(TVector3.One[I], Pi / 2);
  end;
end;

var
  URL: string = 'castle-data:/teapot.x3dv';
  // 'castle-data:/bridge_final.x3dv';
  // '../../../demo_models/shadow_volumes/shadows_dynamic.x3dv'

type
  TDummy = class
    procedure OpenButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
  end;

procedure TDummy.OpenButtonClick(Sender: TObject);
var
  NewURL: string;
begin
  NewURL := URL;
  if Window.FileDialog('Open Scene', NewURL, true, LoadScene_FileFilters) then
  begin
    Scene.Load(NewURL);
    // In case of trouble when loading, this will raise an exception.
    // Let the default Application exception handler show it.
    URL := NewURL;
    CameraReinitialize;
  end;
end;

procedure TDummy.QuitButtonClick(Sender: TObject);
begin
  Application.Terminate;
end;

{ One-time initialization. }
procedure ApplicationInitialize;
var
  I: Integer;
  Background: TCastleRectangleControl;
  {$ifdef ADD_ANIMATION}
  Animation: TCastleScene;
  Transform: TCastleTransform;
  {$endif ADD_ANIMATION}
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Window.SetDemoOptions(keyF11, CharEscape, true);
  Window.OnResize := @Resize;

  if Parameters.High = 1 then
    URL := Parameters[1];

  Scene := TCastleScene.Create(Application);
  Scene.Load(URL);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  RootTransform := TCastleRootTransform.Create(Application);
  RootTransform.Add(Scene);
  RootTransform.MainScene := Scene;

  {$ifdef ADD_ANIMATION}
  { initialize Transform }
  Transform := TCastleTransform.Create(Application);
//  Transform.Translation := Vector3(5, 3, 60);
  RootTransform.Add(Transform);

  { initialize Animation }
  Animation := TCastleScene.Create(Application);
  Animation.Load('castle-data:/raptor.castle-anim-frames');
  Animation.ProcessEvents := true;
  Animation.Spatial := [ssRendering, ssDynamicCollisions];
  Transform.Add(Animation);
  {$endif ADD_ANIMATION}

  { one viewport shows only wireframe }
  Viewports[0] := TWireViewport.Create(Application);
  Viewports[0].Caption := 'Wireframe view';

  { shadow on one viewport }
  Viewports[1] := TMyViewport.Create(Application);
  Viewports[1].Caption := 'Shadow volumes On' + NL + 'Main Camera (controls headlight)';
  RootTransform.MainCamera := Viewports[1].Camera; // just a test of MainCamera

  Viewports[2] := TScreenEffectDemoViewport.Create(Application);
  Viewports[2].Caption := 'Screen effect shader';

  Theme.Images[tiActiveFrame] := FrameThickWhite;
  Theme.Corners[tiActiveFrame] := Vector4(3, 3, 3, 3);
  Theme.Images[tiLabel] := FrameYellowBlack;
  Theme.Corners[tiLabel] := Vector4(1, 1, 1, 1);

  for I := 0 to High(Viewports) do
  begin
    if Viewports[I] = nil then
      Viewports[I] := TMyViewport.Create(Application);

    Viewports[I].AutoCamera := false;
    Viewports[I].AutoNavigation := false;
    Viewports[I].Items := RootTransform;
    Viewports[I].FullSize := false;
    Viewports[I].ShadowVolumes := I = 1;
    { The initial Resize event will position viewports correctly }
    Window.Controls.InsertFront(Viewports[I]);

    ViewportFrames[I] := TFocusedFrame.Create(Application);
    ViewportFrames[I].FullSize := true; // fill parent control, which is the viewport
    Viewports[I].InsertFront(ViewportFrames[I]);

    ViewportsLabels[I] := TCastleLabel.Create(Application);
    ViewportsLabels[I].Caption := Viewports[I].Caption;
    ViewportsLabels[I].Color := Yellow;
    ViewportsLabels[I].Anchor(hpLeft, 15);
    ViewportsLabels[I].Anchor(vpBottom, 15);
    Viewports[I].InsertFront(ViewportsLabels[I]);
  end;

  CameraReinitialize;

  OpenButton := TCastleButton.Create(Application);
  OpenButton.Caption := 'Open Scene';
  OpenButton.OnClick := @TDummy(nil).OpenButtonClick;
  OpenButton.Anchor(hpLeft, Margin);
  OpenButton.Anchor(vpTop, -Margin);
  Window.Controls.InsertFront(OpenButton);

  QuitButton := TCastleButton.Create(Application);
  QuitButton.Caption := 'Quit';
  QuitButton.OnClick := @TDummy(nil).QuitButtonClick;
  QuitButton.Anchor(hpRight, -Margin);
  QuitButton.Anchor(vpTop, -Margin);
  Window.Controls.InsertFront(QuitButton);

  { add a background, since our viewports (deliberately, for demo)
    do not cover whole window. }
  Background := TCastleRectangleControl.Create(Application);
  Background.FullSize := true;
  Background.Color := Vector4(0.5, 0.5, 1.0, 1.0);
  Window.Controls.InsertBack(Background);
end;

{ The main program body only creates and assigns Application.MainWindow,
  and runs the application (Window.OpenAndRun).
  The actual initialization job is done inside Application.OnInitialize.

  In a cross-platform application, everything above (including
  the ApplicationInitialize) would be in a cross-platform unit.
  See https://castle-engine.io/manual_cross_platform.php . }
begin
  Window := TCastleWindowBase.Create(Application);
  Window.StencilBits := 8;

  Application.OnInitialize := @ApplicationInitialize;
  Application.MainWindow := Window;

  Window.OpenAndRun;
end.
