{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of using custom viewports (TCastleViewport) to view the same 3D world
  (scene manager in TCastleSceneManager). }

{ If defined, then the 3D world will contain an additional animation
  of a dinosaur. It's most suitable when as the main scene you load
  data/bridge_final.x3dv, then you get a setup similar to scene_manager_demos.
  This shows that animation from 2nd file works fully with mirrors
  by GeneratedCubeMapTexture in 1st file, also in custom viewports. }
{ $define ADD_ANIMATION}

{$I castleconf.inc}

uses SysUtils, CastleGL, CastleWindow, X3DNodes, CastleSceneCore, CastleScene,
  CastleUIControls, CastleCameras, CastleQuaternions, CastleVectors,
  CastleControls, CastleLog, CastleScreenEffects, CastleSceneManager,
  CastleUtils, CastleGLUtils, X3DLoad, CastleGLShaders, CastleParameters,
  CastleStringUtils, CastleKeysMouse, CastleColors, CastleControlsImages,
  CastleApplicationProperties
  {$ifdef ADD_ANIMATION} , CastleFilesUtils, Castle3D {$endif};

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
  { TODO: There is no way to make this trick work on OpenGLES.
    Wireframe rendering must be done then by really rendering different
    primitives, not by switching some PolygonMode. }
  {$ifndef OpenGLES}
  glPushAttrib(GL_POLYGON_BIT or GL_LINE_BIT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }
  {$endif}
    inherited;
  {$ifndef OpenGLES}
  glPopAttrib;
  {$endif}
end;

{ TScreenEffectDemoViewport -------------------------------------------------- }

type
  TScreenEffectDemoViewport = class(TMyViewport)
  private
    GLSLProgram: TGLSLScreenEffect;
  protected
    function GetScreenEffects(const Index: Integer): TGLSLProgram; override;
  public
    function ScreenEffectsCount: Integer; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

function TScreenEffectDemoViewport.GetScreenEffects(const Index: Integer): TGLSLProgram;
begin
  if Index = 0 then
    Result := GLSLProgram else
    Result := nil;
end;

function TScreenEffectDemoViewport.ScreenEffectsCount: Integer;
begin
  if GLSLProgram <> nil then Result := 1 else Result := 0;
end;

procedure TScreenEffectDemoViewport.GLContextOpen;
begin
  inherited;
  if TGLSLProgram.ClassSupport <> gsNone then
  begin
    GLSLProgram := TGLSLScreenEffect.Create;
    GLSLProgram.ScreenEffectShader :=
      'void main (void)' +NL+
      '{' +NL+
      '  gl_FragColor = (' +NL+
      '    screen_get_color(ivec2(screen_x() - 1, screen_y())) -' +NL+
      '    screen_get_color(ivec2(screen_x() + 1, screen_y()))' +NL+
      '  ) + vec4(1.0) / 2.0;' +NL+
      '}';
    GLSLProgram.Link;
    Writeln(GLSLProgram.DebugInfo);
  end;
end;

procedure TScreenEffectDemoViewport.GLContextClose;
begin
  FreeAndNil(GLSLProgram);
  inherited;
end;

{ TFocusedFrame -------------------------------------------------------------- }

type
  { Draw frame around the control's rectangle, if focused (under cursor). }
  TFocusedFrame = class(TUIControlSizeable)
  public
    procedure Render; override;
    procedure SetFocused(const Value: boolean); override;
  end;

procedure TFocusedFrame.Render;
begin
  if Focused then
    Theme.Draw(ScreenRect, tiActiveFrame);
end;

procedure TFocusedFrame.SetFocused(const Value: boolean);
begin
  if Value <> Focused then
    { The TFocusedFrame.Render is based on Focused value. }
    VisibleChange;

  inherited;
end;

{ ---------------------------------------------------------------------------- }

var
  Window: TCastleWindow;
  Scene: TCastleScene;
  Viewports: array [0..3] of TMyViewport;
  ViewportFrames: array [0..3] of TFocusedFrame;
  ViewportsLabels: array [0..3] of TCastleLabel;
  OpenButton, QuitButton: TCastleButton;

const
  Margin = 5;

procedure Resize(Container: TUIContainer);
var
  W, H, TopMargin: Integer;
begin
  TopMargin := OpenButton.CalculatedHeight + 2 * Margin;
  W := Window.Width div 2;
  H := (Window.Height - TopMargin) div 2;

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
    Viewports[I].Camera.Free;
    Viewports[I].Camera := Window.SceneManager.CreateDefaultCamera;
    if (I < 3) and (Viewports[I].Camera is TExamineCamera) then
      TExamineCamera(Viewports[I].Camera).Rotations :=
        QuatFromAxisAngle(UnitVector3Single[I], Pi/2);
  end;

  { scene manager needs assigned camera to make a headlight.

    Right now, one camera cannot be simultaneously on scene manager
    and viewports. So assign here new camera.
    See TODO at TCastleAbstractViewport.Camera. }
  Window.SceneManager.Camera.Free;
  Window.SceneManager.Camera := Window.SceneManager.CreateDefaultCamera;
end;

var
  URL: string = 'data/teapot.x3dv';
  // 'data/bridge_final.x3dv';
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
  if Window.FileDialog('Open 3D file', NewURL, true, Load3D_FileFilters) then
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

var
  I: Integer;
  Background: TCastleSimpleBackground;
  {$ifdef ADD_ANIMATION}
  Animation: TCastleScene;
  Transform: T3DTransform;
  {$endif ADD_ANIMATION}
begin
  if Parameters.High = 1 then
    URL := Parameters[1];

  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Scene := TCastleScene.Create(Application);
  Scene.Load(URL);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window := TCastleWindow.Create(Application);

  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;
  Window.SceneManager.DefaultViewport := false;

  {$ifdef ADD_ANIMATION}
  { initialize Transform }
  Transform := T3DTransform.Create(Window.SceneManager);
//  Transform.Translation := Vector3Single(5, 3, 60);
  Window.SceneManager.Items.Add(Transform);

  { initialize Animation }
  Animation := TCastleScene.Create(Window.SceneManager);
  Animation.Load(ApplicationData('raptor.castle-anim-frames'));
  Animation.ProcessEvents := true;
  Animation.Spatial := [ssRendering, ssDynamicCollisions];
  Transform.Add(Animation);
  {$endif ADD_ANIMATION}

  { one viewport shows only wireframe }
  Viewports[0] := TWireViewport.Create(Application);
  Viewports[0].Caption := 'Wireframe view';

  { shadow on one viewport }
  Viewports[1] := TMyViewport.Create(Application);
  Viewports[1].Caption := 'Shadow volumes On';

  Viewports[2] := TScreenEffectDemoViewport.Create(Application);
  Viewports[2].Caption := 'Screen effect shader';

  Theme.Images[tiActiveFrame] := FrameThickWhite;
  Theme.Corners[tiActiveFrame] := Vector4Integer(3, 3, 3, 3);
  Theme.Images[tiLabel] := FrameYellowBlack;
  Theme.Corners[tiLabel] := Vector4Integer(1, 1, 1, 1);

  for I := 0 to High(Viewports) do
  begin
    if Viewports[I] = nil then
      Viewports[I] := TMyViewport.Create(Application);
    Viewports[I].SceneManager := Window.SceneManager;
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
  Assert(Window.SceneManager.Viewports.Count = High(Viewports) + 1);

  CameraReinitialize;

  OpenButton := TCastleButton.Create(Application);
  OpenButton.Caption := 'Open 3D file';
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
  Background := TCastleSimpleBackground.Create(Application);
  Background.Color := Vector4Single(0.5, 0.5, 1.0, 1.0);
  Window.Controls.InsertBack(Background);

  Window.StencilBits := 8;
  Window.OnResize := @Resize;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
