{ Example of using TPlayAnimationParameters.StopNotification.

  Play animation by pressing Space.

  There are 3 cases when the animation stops in this application:

  - if you press S,
  - if you just wait for animation to finish,
    since we leave TPlayAnimationParameters.Loop = false.
    (it's a short attack animation).
  - note that the animation also stops when another animation is run.
    (the OnStopNotification in this case is not called immediately
    when Scene.PlayAnimation for new animation is called,
    the actual animation change happens in nearest update).

  Regardless of why the animation stopped, AnimationStopped method is called. }

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils,
  CastleWindow, CastleSceneCore, CastleScene, CastleViewport, CastleCameras,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, X3DNodes,
  CastleKeysMouse, CastleColors, CastleNotifications;

{ TStateMain ----------------------------------------------------------------- }

type
  TStateMain = class(TUIState)
  strict private
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Notifications: TCastleNotifications;
    procedure AnimationStopped(const AScene: TCastleSceneCore; const Animation: TTimeSensorNode);
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

procedure TStateMain.Start;
begin
  inherited;

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  InsertFront(Viewport);

  Scene := TCastleScene.Create(FreeAtStop);
  Scene.Load('castle-data:/knight.gltf');
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Notifications := TCastleNotifications.Create(FreeAtStop);
  Notifications.Timeout := 2;
  Notifications.Fade := 0.25;
  Notifications.Color := Yellow;
  Notifications.FontSize := 30;
  Notifications.Anchor(vpTop, -10);
  Notifications.Anchor(hpMiddle);
  Notifications.TextAlignment := hpMiddle;
  Viewport.InsertFront(Notifications);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
var
  PlayAnimationParams: TPlayAnimationParameters;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keySpace) then
  begin
    PlayAnimationParams := TPlayAnimationParameters.Create;
    try
      PlayAnimationParams.Name := 'Attack';
      PlayAnimationParams.StopNotification := {$ifdef FPC}@{$endif} AnimationStopped;
      Scene.PlayAnimation(PlayAnimationParams);
    finally FreeAndNil(PlayAnimationParams) end;

    Notifications.Show('Playing animation...');
  end;

  if Event.IsKey(keyS) then
    Scene.StopAnimation;
end;

procedure TStateMain.AnimationStopped(const AScene: TCastleSceneCore; const Animation: TTimeSensorNode);
begin
  Notifications.Show('Stopped.');
end;

{ application initialization ------------------------------------------------- }

var
  Window: TCastleWindow;
  StateMain: TStateMain;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;
  Application.MainWindow := Window;

  StateMain := TStateMain.Create(Application);
  TUIState.Current := StateMain;

  Application.Run;
end.
