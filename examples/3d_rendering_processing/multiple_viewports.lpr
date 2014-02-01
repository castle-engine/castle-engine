{
  Copyright 2010-2013 Michalis Kamburelis.

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

{ If defined, then the 3D world will contain a translated TCastlePrecalculatedAnimation
  with a dinosaur. It's most suitable when as the main scene you load
  models/bridge_final.x3dv, then you get a setup similar to scene_manager_demos.
  This shows that animated TCastlePrecalculatedAnimation works fully with mirrors
  by GeneratedCubeMapTexture, also in custom viewports. }
{ $define ADD_GL_ANIMATION}

{$I castleconf.inc}

uses SysUtils, CastleGL, CastleWindow, X3DNodes, CastleSceneCore, CastleScene, CastleSceneManager,
  CastleUIControls, CastleCameras, CastleQuaternions, CastleVectors,
  CastleControls, CastleWarnings,
  CastleUtils, CastleGLUtils, X3DLoad, CastleGLShaders, CastleParameters,
  CastleStringUtils, CastleKeysMouse, CastleColors, CastleControlsImages;

{ TMyViewport ---------------------------------------------------------------- }

type
  { Derive our own TCastleViewport descendants, just to demo that we can. }
  TMyViewport = class(TCastleViewport)
  public
    Caption: string;
    procedure SetFocused(const Value: boolean); override;
  end;

procedure TMyViewport.SetFocused(const Value: boolean);
begin
  if Value <> Focused then
    { The TMyViewport2D.Render is based on Focused value. }
    VisibleChange;

  inherited;
end;

{ TMyViewport2D -------------------------------------------------------------- }

type
  { 2D controls over the viewport. For this we need TUIControl instance
    with RenderStyle 2D. }
  TMyViewport2D = class(TUIControl)
  public
    Viewport: TMyViewport;
    procedure Render; override;
    function RenderStyle: TRenderStyle; override;
  end;

function TMyViewport2D.RenderStyle: TRenderStyle;
begin
  Result := rs2D;
end;

procedure TMyViewport2D.Render;
begin
  if Viewport.Focused then
    Theme.Draw(Viewport.Rect, tiActiveFrame);
end;

{ TWireViewport -------------------------------------------------------------- }

type
  TWireViewport = class(TMyViewport)
    procedure Render; override;
  end;

procedure TWireViewport.Render;
begin
  {$ifndef OpenGLES} //TODO-es
  glPushAttrib(GL_POLYGON_BIT or GL_LINE_BIT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }
    glLineWidth(1); { saved by GL_LINE_BIT }
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
    GLSLProgram: TGLSLProgram;
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
  if (TGLSLProgram.ClassSupport <> gsNone) and
     GLFeatures.TextureRectangle then
  begin
    GLSLProgram := TGLSLProgram.Create;
    GLSLProgram.AttachFragmentShader(
      '#extension GL_ARB_texture_rectangle : enable' +nl+
      'uniform sampler2DRect screen;' +NL+
      'void main (void)' +NL+
      '{' +NL+
      '  gl_FragColor = ( texture2DRect(screen, vec2(gl_TexCoord[0].s - 1.0, gl_TexCoord[0].t)) - texture2DRect(screen, vec2(gl_TexCoord[0].s + 1.0, gl_TexCoord[0].t)) ) + vec4(1.0) / 2.0;' +NL+
      '}');
    { For this test program, we eventually allow shader to run in software }
    GLSLProgram.Link(false);
    GLSLProgram.UniformNotFoundAction := uaIgnore;
    Writeln(GLSLProgram.DebugInfo);
  end;
end;

procedure TScreenEffectDemoViewport.GLContextClose;
begin
  FreeAndNil(GLSLProgram);
  inherited;
end;

{ ---------------------------------------------------------------------------- }

var
  Window: TCastleWindow;
  Scene: TCastleScene;
  Viewports: array [0..3] of TMyViewport;
  Viewports2D: array [0..3] of TMyViewport2D;
  ViewportsLabels: array [0..3] of TCastleLabel;
  OpenButton, QuitButton: TCastleButton;

procedure Resize(Container: TUIContainer);
const
  Margin = 5;
var
  W, H, ButtonHeight, I: Integer;
begin
  ButtonHeight := OpenButton.Height + 2*Margin;
  W := Window.Width div 2;
  H := (Window.Height - ButtonHeight) div 2;

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

  OpenButton.Left := Margin;
  OpenButton.Bottom := Window.Height - ButtonHeight + Margin;

  QuitButton.Left := Window.Width - Margin - QuitButton.Width;
  QuitButton.Bottom := OpenButton.Bottom;

  for I := Low(ViewportsLabels) to High(ViewportsLabels) do
  begin
    ViewportsLabels[I].Left := Viewports[I].Left + 10;
    ViewportsLabels[I].Bottom := Viewports[I].Bottom + 10;
  end;
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
  URL: string = 'models/teapot.x3dv';
  //  '../../../demo_models/shadow_volumes/shadows_dynamic.x3dv'

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
    try
      Scene.Load(NewURL);
    except
      on E: Exception do
      begin
        Window.MessageOk('Cannot open file "' + NewURL + '": ' + E.Message, mtError);
        Exit;
      end;
    end;
    URL := NewURL;
    CameraReinitialize;
  end;
end;

procedure TDummy.QuitButtonClick(Sender: TObject);
begin
  Application.Quit;
end;

var
  I: Integer;
  Background: TCastleSimpleBackground;
  {$ifdef ADD_GL_ANIMATION}
  Animation: TCastlePrecalculatedAnimation;
  Transform: T3DTransform;
  {$endif ADD_GL_ANIMATION}
begin
  if Parameters.High = 1 then
    URL := Parameters[1];

  OnWarning := @OnWarningWrite;

  Scene := TCastleScene.Create(Application);
  Scene.Load(URL);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window := TCastleWindow.Create(Application);

  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;
  Window.SceneManager.DefaultViewport := false;

  {$ifdef ADD_GL_ANIMATION}
  { initialize Transform }
  Transform := T3DTransform.Create(SceneManager);
  Transform.Translation := Vector3Single(5, 3, 60);
  Window.SceneManager.Items.Add(Transform);

  { initialize Animation }
  Animation := TCastlePrecalculatedAnimation.Create(SceneManager);
  Animation.LoadFromFile('models/raptor.kanim', false, true);
  Animation.FirstScene.Spatial := [ssRendering, ssDynamicCollisions];
  Transform.Child := Animation;
  {$endif ADD_GL_ANIMATION}

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
    Window.Controls.Add(Viewports[I]);

    Viewports2D[I] := TMyViewport2D.Create(Application);
    Viewports2D[I].Viewport := Viewports[I];
    Window.Controls.Add(Viewports2D[I]);

    ViewportsLabels[I] := TCastleLabel.Create(Application);
    ViewportsLabels[I].Text.Text := Viewports[I].Caption;
    ViewportsLabels[I].Color := Yellow;
    ViewportsLabels[I].Padding := 5;
    Window.Controls.Add(ViewportsLabels[I]);
  end;
  Assert(Window.SceneManager.Viewports.Count = High(Viewports) + 1);

  CameraReinitialize;

  OpenButton := TCastleButton.Create(Application);
  OpenButton.Caption := 'Open 3D file';
  OpenButton.OnClick := @TDummy(nil).OpenButtonClick;
  Window.Controls.InsertFront(OpenButton);

  QuitButton := TCastleButton.Create(Application);
  QuitButton.Caption := 'Quit';
  QuitButton.OnClick := @TDummy(nil).QuitButtonClick;
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
