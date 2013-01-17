{
  Copyright 2010-2012 Michalis Kamburelis.

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

uses SysUtils, GL, CastleWindow, X3DNodes, CastleSceneCore, CastleScene, CastleSceneManager,
  CastleUIControls, CastleCameras, CastleQuaternions, CastleVectors, CastleControls, CastleWarnings,
  CastleUtils, CastleGLUtils, X3DLoad, CastleGLShaders, CastleParameters,
  CastleStringUtils, CastleKeysMouse, CastleColors;

{ TBackground ---------------------------------------------------------------- }

type
  TBackground = class(TUIControl)
  public
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
  end;

function TBackground.DrawStyle: TUIControlDrawStyle;
begin
  { 3D, because we want to be drawn before other 3D objects }
  Result := ds3D;
end;

procedure TBackground.Draw;
begin
  glPushAttrib(GL_COLOR_BUFFER_BIT);
    glClearColor(0.5, 0.5, 1, 1); // saved by GL_COLOR_BUFFER_BIT
    glClear(GL_COLOR_BUFFER_BIT);
  glPopAttrib;
end;

{ TMyViewport ---------------------------------------------------------------- }

type
  { Derive our own TCastleViewport descendants, just to demo that we can. }
  TMyViewport = class(TCastleViewport)
  public
    Caption: string;
    procedure Draw; override;
    procedure SetFocused(const Value: boolean); override;
  end;

procedure ViewportDraw2D(ViewportPtr: Pointer);
const
  Margin = 5;
var
  Viewport: TMyViewport absolute ViewportPtr;
begin
  glLoadIdentity;

  if Viewport.Focused then
  begin
    GLRectangleBorder(0, 0, Viewport.Width, Viewport.Height, White4Single, 3);
    { line width saved by GL_LINE_BIT, although we can generally change
      it carelessly (without saving) when drawing in our engine }
  end;

  if Viewport.Caption <> '' then
  begin
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA); { saved by GL_COLOR_BUFFER_BIT }
    glEnable(GL_BLEND); { saved by GL_COLOR_BUFFER_BIT }
    glColor4f(0, 0, 0, 0.5);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); { saved by GL_POLYGON_BIT }
    glRectf(
      10 - Margin,
      10 - Margin,
      10 + UIFont.TextWidth(Viewport.Caption) + Margin,
      10 + UIFont.RowHeight + Margin);
    glDisable(GL_BLEND); { saved by GL_COLOR_BUFFER_BIT }

    glColor3f(1, 1, 0);
    SetWindowPos(Viewport.Left + 10, Viewport.Bottom + 10);
    UIFont.Print(Viewport.Caption);
  end;
end;

procedure TMyViewport.Draw;
begin
  inherited;

  glPushAttrib(GL_ENABLE_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT or GL_POLYGON_BIT);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glProjectionPushPopOrtho2D(@ViewportDraw2D, Self, 0, Width, 0, Height);
  glPopAttrib;
end;

procedure TMyViewport.SetFocused(const Value: boolean);
begin
  if Value <> Focused then
  begin
    { We base our Draw on Focused value. }
    VisibleChange;
  end;

  inherited;
end;

{ TWireViewport -------------------------------------------------------------- }

type
  TWireViewport = class(TMyViewport)
    procedure Draw; override;
  end;

procedure TWireViewport.Draw;
begin
  glPushAttrib(GL_POLYGON_BIT or GL_LINE_BIT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }
    glLineWidth(1); { saved by GL_LINE_BIT }
    inherited;
  glPopAttrib;
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
     GL_ARB_texture_rectangle then
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
  OpenButton, QuitButton: TCastleButton;

procedure Resize(Window: TCastleWindowBase);
const
  Margin = 5;
var
  W, H, ButtonHeight: Integer;
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
  FileName: string = 'models/teapot.x3dv';
  //  '../../../demo_models/shadow_volumes/shadows_dynamic.x3dv'

type
  TDummy = class
    procedure OpenButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
  end;

procedure TDummy.OpenButtonClick(Sender: TObject);
var
  NewFileName: string;
begin
  NewFileName := FileName;
  if Window.FileDialog('Open 3D file', NewFileName, true, Load3D_FileFilters) then
  begin
    try
      Scene.Load(NewFileName);
    except
      on E: Exception do
      begin
        Window.MessageOk('Cannot open file "' + NewFileName + '": ' + E.Message, mtError);
        Exit;
      end;
    end;
    FileName := NewFileName;
    CameraReinitialize;
  end;
end;

procedure TDummy.QuitButtonClick(Sender: TObject);
begin
  Application.Quit;
end;

var
  I: Integer;
  {$ifdef ADD_GL_ANIMATION}
  Animation: TCastlePrecalculatedAnimation;
  Transform: T3DTransform;
  {$endif ADD_GL_ANIMATION}
begin
  if Parameters.High = 1 then
    FileName := Parameters[1];

  OnWarning := @OnWarningWrite;

  Scene := TCastleScene.Create(Application);
  Scene.Load(FileName);
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

  Viewports[2] := TScreenEffectDemoViewport.Create(Application);
  Viewports[2].Caption := 'Screen effect shader';

  for I := 0 to High(Viewports) do
  begin
    if Viewports[I] = nil then
      Viewports[I] := TMyViewport.Create(Application);
    Viewports[I].SceneManager := Window.SceneManager;
    Viewports[I].FullSize := false;
    Viewports[I].ShadowVolumes := I = 1;
    { The initial Resize event will position viewports correctly }
    Window.Controls.Add(Viewports[I]);
  end;
  Assert(Window.SceneManager.Viewports.Count = High(Viewports) + 1);

  { shadow on one viewport }
  Viewports[1].Caption := 'Shadow volumes On';

  CameraReinitialize;

  OpenButton := TCastleButton.Create(Application);
  OpenButton.Caption := 'Open 3D file';
  OpenButton.OnClick := @TDummy(nil).OpenButtonClick;
  Window.Controls.Add(OpenButton);

  QuitButton := TCastleButton.Create(Application);
  QuitButton.Caption := 'Quit';
  QuitButton.OnClick := @TDummy(nil).QuitButtonClick;
  Window.Controls.Add(QuitButton);

  { add a background, since our viewpoints (deliberately, for demo)
    do not cover whole window. }
  Window.Controls.Add(TBackground.Create(Application));

  Window.StencilBits := 8;
  Window.OnResize := @Resize;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
