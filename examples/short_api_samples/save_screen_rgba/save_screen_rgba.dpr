{ TCastleContainer.SaveScreenRgba example.

  This example relies on OpenGL color buffer that can store alpha information,
  which you request by "Window.AlphaBits := 8".

  An alternative approach, without needing "Window.AlphaBits := xxx",
  is to initialize FBO with color framebuffer with alpha channel,
  and render using this Fbo, like this:

    Fbo := TGLRenderToTexture.Create(Window.Width, Window.Height);
    Fbo.Buffer := tbNone;
    Fbo.ColorBufferAlpha := true;
    Fbo.GLContextOpen;
    Fbo.RenderBegin;
    ...
    Fbo.RenderEnd;
}
uses SysUtils,
  CastleWindow, CastleLog, CastleVectors, CastleUIControls, CastleScene,
  CastleViewport, CastleKeysMouse, CastleImages, CastleCameras;

{ View ----------------------------------------------------------------------- }

type
  { View to contain whole UI and to handle events, like key press. }
  TMyView = class(TCastleView)
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  MyView: TMyView;

function TMyView.Press(const Event: TInputPressRelease): Boolean;
var
  Image: TRGBAlphaImage;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyF5) then
  begin
    Image := Container.SaveScreenRgba;
    try
      SaveImage(Image, 'save_screen_rgba.png');
    finally FreeAndNil(Image) end;
    Exit(true);
  end;
end;

var
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  // We must have buffer that stores alpha information, not only RGB
  Window.AlphaBits := 8;
  // Initially fill buffer with transparent white.
  // When displaying, the window will be white, but it will be saved as transparent white.
  Window.Container.BackgroundColor := Vector4(1, 1, 1, 0);
  Window.Open;

  MyView := TMyView.Create(Application);
  Window.Container.View := MyView;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  Viewport.Transparent := true; // do not fill parent with Viewport.BackgroundColor
  MyView.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/teapot.x3dv');
  Scene.PreciseCollisions := true;
  Scene.ProcessEvents := true;
  Viewport.Items.Add(Scene);

  // headlight
  Viewport.Camera.Add(TCastleDirectionalLight.Create(Application));

  // nice initial camera position
  Viewport.AssignDefaultCamera;

  Application.Run;
end.
