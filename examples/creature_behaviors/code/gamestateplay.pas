{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main "playing game" state, where most of the game logic takes place. }
unit GameStatePlay;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleCameras,
  CastleTransform, CastleBehaviors, CastleClassUtils;

type
  { A transformation that automatically synchronizes (in both ways)
    with a specific TCastleCamera in @link(Camera).
    This way it is a TCastleTransform instance that expresses
    a camera of a given TCastleViewport.

    In the most usual case, the camera in @link(Camera) refers
    to the same TCastleViewport that contains this TCastleCameraTransform
    instance. So you set it up like this:

    @longCode(#
      MyCameraTransform := TCastleCameraTransform.Create(...);
      MyCameraTransform.Camera := Viewport.Camera;
      Viewport.Items.Add(MyCameraTransform);
    #)

    TODO: make this automatically added to viewport.

    TODO: make this class just equal TCastleCamera?

    TODO: `RenderOnTop` available on the `TCastleCameraTransform` too, to have it ready.

    TODO: move to some core unit
  }
  TCastleCameraTransform = class(TCastleTransform)
  strict private
    InsideSynchronizeFromCamera: Cardinal;
    FCamera: TCastleCamera;
    FCameraObserver: TFreeNotificationObserver;
    procedure SynchronizeToCamera;
    procedure SynchronizeFromCamera;
    procedure CameraFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetCamera(const Value: TCastleCamera);
  protected
    procedure ChangedTransform; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render(const Params: TRenderParams); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  published
    { Camera synchronized with this TCastleCameraTransform instance. }
    property Camera: TCastleCamera read FCamera write SetCamera;
  end;

  { Main "playing game" state, where most of the game logic takes place. }
  TStatePlay = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_play.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;

    Enemies: TCastleTransformList;
    Player: TCastleCameraTransform;
    PlayerAlive: TCastleAliveBehavior;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils, Math,
  CastleSoundEngine, CastleLog, CastleStringUtils, CastleFilesUtils,
  GameStateMenu;

{ TCastleCameraTransform ----------------------------------------------------- }

constructor TCastleCameraTransform.Create(AOwner: TComponent);
begin
  inherited;
  FCameraObserver := TFreeNotificationObserver.Create(Self);
  FCameraObserver.OnFreeNotification := {$ifdef FPC}@{$endif} CameraFreeNotification;
end;

procedure TCastleCameraTransform.SetCamera(const Value: TCastleCamera);
begin
  if FCamera <> Value then
  begin
    FCamera := Value;
    FCameraObserver.Observed := Value;
  end;
end;

procedure TCastleCameraTransform.CameraFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Camera := nil;
end;

procedure TCastleCameraTransform.SynchronizeToCamera;
var
  P, D, U: TVector3;
begin
  // avoid recursive calls between SynchronizeToCamera and SynchronizeFromCamera
  if InsideSynchronizeFromCamera <> 0 then Exit;

  // synchronize Position, Direction, Up *to* Camera
  if Camera <> nil then
  begin
    GetView(P, D, U);
    if Parent <> nil then
    begin
      P := Parent.LocalToWorld(P);
      D := Parent.LocalToWorldDirection(D);
      U := Parent.LocalToWorldDirection(U);
    end;
    Camera.SetView(P, D, U);
  end;
end;

procedure TCastleCameraTransform.SynchronizeFromCamera;
var
  P, D, U: TVector3;
begin
  // synchronize Position, Direction, Up *from* Camera
  if Camera  <> nil then
  begin
    Camera.GetView(P, D, U);
    if Parent <> nil then
    begin
      P := Parent.WorldToLocal(P);
      D := Parent.WorldToLocalDirection(D);
      U := Parent.WorldToLocalDirection(U);
    end;
    Inc(InsideSynchronizeFromCamera);
    SetView(P, D, U);  // this causes ChangedTransform which causes SynchronizeToCamera
    Dec(InsideSynchronizeFromCamera);
  end;
end;

procedure TCastleCameraTransform.ChangedTransform;
begin
  inherited;
  SynchronizeToCamera;
end;

procedure TCastleCameraTransform.Render(const Params: TRenderParams);
begin
  { Do this before rendering, otherwise we could display children in unsynchronized
    position/orientation.
    That's because Camera could change after our Update, but before rendering.
    (Testcase: move/rotate using touch control
    in fps_game when you have shooting_eye.) }
  SynchronizeFromCamera;
  inherited;
end;

procedure TCastleCameraTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if not Exists then Exit;
  SynchronizeFromCamera;
end;

{ TStatePlay ----------------------------------------------------------------- }

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

procedure TStatePlay.Start;
var
  SoldierScene: TCastleScene;
  I: Integer;
  // TODO MoveAttackBehavior: TCastleMoveAttack;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  WalkNavigation := DesignedComponent('WalkNavigation') as TCastleWalkNavigation;

  Player := TCastleCameraTransform.Create(FreeAtStop);
  Player.Camera := MainViewport.Camera;
  MainViewport.Items.Add(Player);

  PlayerAlive := TCastleAliveBehavior.Create(FreeAtStop);
  Player.AddBehavior(PlayerAlive);

  { Initialize Enemies }
  Enemies := TCastleTransformList.Create(false);
  for I := 1 to 5 do
  begin
    SoldierScene := DesignedComponent('SceneSoldier' + IntToStr(I)) as TCastleScene;
    Enemies.Add(SoldierScene);

    // TODO: TCastleMoveAttack should take care of this
    SoldierScene.PlayAnimation('walk', true);

    SoldierScene.AddBehavior(TCastleAliveBehavior.Create(FreeAtStop));

    // TODO
    // MoveAttackBehavior := TCastleMoveAttack.Create(FreeAtStop);
    // MoveAttackBehavior.Enemy := PlayerAlive;
    // SoldierScene.AddBehavior(MoveAttackBehavior);
  end;
end;

procedure TStatePlay.Stop;
begin
  FreeAndNil(Enemies);
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;
var
  HitAlive: TCastleAliveBehavior;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStatePlay.Press method should be used to handle keys
    not handled in children controls.
  }

  if Event.IsMouseButton(buttonLeft) then
  begin
    SoundEngine.Play(SoundEngine.SoundFromName('shoot_sound'));

    { We clicked on enemy if
      - TransformUnderMouse indicates we hit something
      - It has a behavior of TCastleAliveBehavior. }
    if (MainViewport.TransformUnderMouse <> nil) and
       (MainViewport.TransformUnderMouse.FindBehavior(TCastleAliveBehavior) <> nil) then
    begin
      // TODO: TCastleMoveAttack should take care of this
      HitAlive := MainViewport.TransformUnderMouse.FindBehavior(TCastleAliveBehavior) as TCastleAliveBehavior;
      HitAlive.Hurt(1000, MainViewport.Camera.Direction);
      if HitAlive.Dead then
      begin
        (HitAlive.Parent as TCastleScene).PlayAnimation('die', false);
        // dead corpse no longer collides
        HitAlive.Parent.Pickable := false;
        HitAlive.Parent.Collides := false;
      end;
    end;

    Exit(true);
  end;

  if Event.IsKey(CtrlM) then
  begin
    WalkNavigation.MouseLook := not WalkNavigation.MouseLook;
    Exit(true);
  end;

  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;

  if Event.IsKey(keyEscape) then
  begin
    TUIState.Current := StateMenu;
    Exit(true);
  end;
end;

end.
