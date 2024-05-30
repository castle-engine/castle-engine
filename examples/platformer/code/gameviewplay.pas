{
  Copyright 2021-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main "playing game" view, where most of the game logic takes place. }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleSceneCore, CastleVectors,
  CastleTransform, CastleSoundEngine, X3DNodes,
  GameEnemy, GameFallingObstacle, GameDeadlyObstacle, GameMovingPlatform, ModularMovement,
  Platformer2DInAirControl, Platformer2DWalkSupport, DoubleJumpSupport, AnimationTrigger,
  CastleInputAxis, CastleInputs;

type
  TLevelBounds = class (TComponent)
  public
    Left: Single;
    Right: Single;
    Top: Single;
    Down: Single;
    constructor Create(AOwner: TComponent);override;
  end;

  TBullet = class(TCastleTransform)
  strict private
    Duration: Single;
    FRBody: TCastleRigidBody;
  public
    constructor Create(AOwner: TComponent; const BulletSpriteScene: TCastleScene); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    { Physics body. Guaranteed to be initialized (non-nil) for TBullet. }
    property RBody: TCastleRigidBody read FRBody;
  end;

  { Main "playing game" view, where most of the game logic takes place. }
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    LabelCollectedCoins: TCastleLabel;
    MainViewport: TCastleViewport;
    ScenePlayer: TCastleScene;
    PlayerRigidBody: TCastleRigidBody;
    CheckboxCameraFollow: TCastleCheckbox;
    ImageHitPoint4: TCastleImageControl;
    ImageHitPoint3: TCastleImageControl;
    ImageHitPoint2: TCastleImageControl;
    ImageHitPoint1: TCastleImageControl;
    ImageKey: TCastleImageControl;
    PlayerDoubleJumpSupport: TDoubleJumpSupport;
    PlayerModularMovement: TModularMovement;
    PlayerAnimationTrigger: TAnimationTrigger;
  strict private
    { Checks this is first Update when the InputJump occurred.
      See ../README.md for documentation about allowed keys/mouse/touch input. }
    WasInputJump: Boolean;

    { Checks this is firs Update when InputShot occurred.
      See ../README.md for documentation about allowed keys/mouse/touch input. }
    WasInputShot: Boolean;

    { Player abilities }
    PlayerCanDoubleJump: Boolean;
    PlayerCanShot: Boolean;
    PlayerCollectedCoins: Integer;
    PlayerHitPoints: Integer;
    PlayerAnimationToLoop: String;
    PlayerHasKey: Boolean;

    LevelComplete: Boolean;

    BulletSpriteScene: TCastleScene;

    { Level bounds }
    LevelBounds: TLevelBounds;

    { Enemies behaviors }
    Enemies: TEnemyList;

    { Falling obstacles (spike) behaviors }
    FallingObstacles: TFallingObstaclesList;

    { Deadly obstacles (spikes) behaviors }
    DeadlyObstacles: TDeadlyObstaclesList;

    { List of moving platforms behaviors }
    MovingPlatforms: TMovingPlatformList;

    procedure ConfigurePlayerPhysics(const Player:TCastleScene);
    procedure ConfigurePlayerAbilities(const Player:TCastleScene);
    procedure PlayerCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
    procedure PlayerCollisionExit(const CollisionDetails: TPhysicsCollisionDetails);
    procedure ConfigureBulletSpriteScene;

    procedure Shot(BulletOwner: TComponent; const Origin, Direction: TVector3);
    procedure AfterPlayerMovementUpdate(Sender: TObject);

    { Coins support }
    procedure CollectCoin;
    procedure ResetCollectedCoins;

    { Life support }
    procedure ResetHitPoints;
    procedure SetHitPoints(const HitPoints: Integer);

    { Key support }
    procedure CollectKey;
    procedure ResetCollectedKeys;

    { Special animation play function to play an animation ant then loop
      another one (specified in AnimationNameToLoop) }
    procedure PlayAnimationOnceAndLoop(Scene: TCastleScene;
      const AnimationNameToPlayOnce, AnimationNameToLoop: String);
    procedure OnAnimationStop(const Scene: TCastleSceneCore;
      const Animation: TTimeSensorNode);

    { Check input for shot via keys and mouse/touch, to support both keyboard
      and mouse and touch (on mobile) navigation. }
    function InputShot: Boolean;
    { Callback for TCastleInputAxis for moving player on touch screen. }
    procedure TouchScreenMove(const Sender: TCastleInputAxis; var Value: Single);
    { Callback for TInputShortcut for player jumping on touch screen. }
    procedure TouchScreenJump(const Sender: TInputShortcut; var IsPressed: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    { Procedure called after level is loaded }
    procedure Start; override;
    { Procedure called before level is closed }
    procedure Stop; override;
    { Procedure called after this view becomes top one }
    procedure Resume; override;
    { Procedure called on every frame }
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;

    { Public functions }
    procedure HitPlayer;
    function IsPlayerDead: Boolean;

    procedure PauseGame;
    procedure ResumeGame;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses
  SysUtils, Math,
  CastleLog,
  GameSound, GameViewMenu, GameViewGameOver, GameViewLevelComplete, GameViewPause;

{ TBullet -------------------------------------------------------------------- }

constructor TBullet.Create(AOwner: TComponent; const BulletSpriteScene: TCastleScene);
var
  Col: TCastleSphereCollider;
begin
  inherited Create(AOwner);

  Add(BulletSpriteScene);
  BulletSpriteScene.Visible := true;
  BulletSpriteScene.Translation := Vector3(0, 0, 0);

  { In this case we are adding TCastleRigidBody to TBullet(TCastleTransform)
    and not to the BulletSpriteScene in order to be able to use this scene in
    multiple bullets. }
  FRBody := TCastleRigidBody.Create(Self);
  RBody.Setup2D;
  RBody.Dynamic := true;
  RBody.CollisionDetection := cdContinuous;
  RBody.MaxLinearVelocity := 0;
  RBody.Layer := 3;

  Col := TCastleSphereCollider.Create(Self);
  { We don't set the Radius becouse we simply use Autosize }
  // Col.Radius :=  BulletSpriteScene.BoundingBox.Size.X / 2;
  { Make bullet more bouncy }
  Col.Restitution := 0.6;
  Col.Mass := 1;

  AddBehavior(Col);
  AddBehavior(RBody);
end;

procedure TBullet.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType
  );
begin
  inherited Update(SecondsPassed, RemoveMe);

  Duration := Duration + SecondsPassed;
  if Duration > 3 then
    RemoveMe := rtRemoveAndFree;
end;

{ TLevelBounds --------------------------------------------------------------- }

constructor TLevelBounds.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := -3072;
  Right := 5120;
  Top := 3072;
  Down := -800;
end;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

function TViewPlay.InputShot: Boolean;
begin
  Result :=
    Container.Pressed.Items[keySpace] or
    { Right mouse button, or 2 fingers, are held. }
    (buttonRight in Container.MousePressed) or
    (Container.TouchesCount >= 2);
end;

procedure TViewPlay.TouchScreenMove(const Sender: TCastleInputAxis;
  var Value: Single);
var
  LValue, RValue: Single;
  I: Integer;
begin
  { Note: if we would not need to support multi-touch (and only wanted
    to check 1st finger) then we would use simpler "Container.MousePosition"
    instead of "Container.TouchesCount", "Container.Touches[..].Position". }
  LValue := 0;
  RValue := 0;
  if buttonLeft in Container.MousePressed then
  begin
    for I := 0 to Container.TouchesCount - 1 do
    begin
      { Check only the lower part of the screen }
      if Container.Touches[I].Position.Y < Container.PixelsHeight * 0.5 then
      begin
        if Container.Touches[I].Position.X < Container.PixelsWidth * 0.5 then
          LValue := -1.0
        else
        if Container.Touches[I].Position.X >= Container.PixelsWidth * 0.5 then
          RValue := 1.0;
      end;
    end;
    { When we touch left and right part of the window at the same time there is no
      movement }
    Value := LValue + RValue;
  end;
end;

procedure TViewPlay.TouchScreenJump(const Sender: TInputShortcut;
  var IsPressed: Boolean);
var
  I: Integer;
begin
  { Mouse, or any finger, pressing in upper part of the screen. }
  if buttonLeft in Container.MousePressed then
    for I := 0 to Container.TouchesCount - 1 do
      if (Container.Touches[I].Position.Y >= Container.PixelsHeight * 0.5) then
      begin
        IsPressed := true;
        Exit;
      end;
end;

procedure TViewPlay.ConfigurePlayerPhysics(
  const Player: TCastleScene);
var
  RBody: TCastleRigidBody;
begin
  RBody := Player.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody<> nil then
  begin
    RBody.OnCollisionEnter := {$ifdef FPC}@{$endif}PlayerCollisionEnter;
    RBody.OnCollisionExit := {$ifdef FPC}@{$endif}PlayerCollisionExit;
  end;

  WasInputJump := false;
end;

procedure TViewPlay.ConfigurePlayerAbilities(const Player: TCastleScene);
begin
  PlayerCanDoubleJump := false;
  PlayerDoubleJumpSupport.Exists := false;
  PlayerCanShot := false;
  ResetHitPoints;

  ResetCollectedCoins;
  ResetCollectedKeys;
end;

procedure TViewPlay.PlayerCollisionEnter(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  if CollisionDetails.OtherTransform <> nil then
  begin
    if Pos('GoldCoin', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollectCoin;
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('DblJump', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      SoundEngine.Play(NamedSound('PowerUp'));
      PlayerCanDoubleJump := true;
      PlayerDoubleJumpSupport.Exists := true;
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Shot', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      SoundEngine.Play(NamedSound('PowerUp'));
      PlayerCanShot := true;
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Key', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollectKey;
      CollisionDetails.OtherTransform.Exists := false;
    end else
    if Pos('Door', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      if PlayerHasKey then
        LevelComplete := true
      else
        { Show no key message. }
        CollisionDetails.OtherTransform.Items[0].Exists := true;
    end;
  end;
end;

procedure TViewPlay.PlayerCollisionExit(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  { Hide no key message. }
  if CollisionDetails.OtherTransform <> nil then
  begin
    if (Pos('Door', CollisionDetails.OtherTransform.Name) > 0) and
       (not PlayerHasKey) then
      CollisionDetails.OtherTransform.Items[0].Exists := false;
  end;
end;

procedure TViewPlay.ConfigureBulletSpriteScene;
begin
  BulletSpriteScene := TCastleScene.Create(FreeAtStop);
  BulletSpriteScene.URL := 'castle-data:/bullet/particle_darkGrey.png';
  BulletSpriteScene.Scale := Vector3(0.5, 0.5, 0.5);
end;

procedure TViewPlay.Shot(BulletOwner: TComponent; const Origin,
  Direction: TVector3);
var
  Bullet: TBullet;
begin
  Bullet := TBullet.Create(BulletOwner, BulletSpriteScene);
  Bullet.Translation := Origin;
  Bullet.RBody.LinearVelocity := Direction * Vector3(750, 20, 0);
  MainViewport.Items.Add(Bullet);
end;

procedure TViewPlay.AfterPlayerMovementUpdate(Sender: TObject);
var
  Velocity: TVector3;
begin
  { always check is player moving the same direction }
  Velocity := PlayerRigidBody.LinearVelocity;
  if Velocity.X < -1 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else if Velocity.X > 1 then
    ScenePlayer.Scale := Vector3(1, 1, 1);

  if PlayerAnimationTrigger.Exists then
    Exit;

  { Check is there first jump frame }
  if PlayerModularMovement.IsFirstJumpingFrame then
  begin
    SoundEngine.Play(NamedSound('Jump'));
    if ScenePlayer.CurrentAnimation.X3DName <> 'hurt' then
    begin
      if ScenePlayer.CurrentAnimation.X3DName <> 'jump' then
          ScenePlayer.PlayAnimation('jump', true)
    end else
      PlayerAnimationToLoop := 'jump';
    Exit;
  end;

  { Don't change animation when player are hurt }
  if ScenePlayer.CurrentAnimation.X3DName <> 'hurt' then
  begin
    if (not PlayerModularMovement.IsPlayerOnGround) and (Velocity.Y > 0) then
    begin
      //WritelnLog('jump');
      if ScenePlayer.CurrentAnimation.X3DName <> 'jump' then
        ScenePlayer.PlayAnimation('jump', true)
    end else
    if (not PlayerModularMovement.IsPlayerOnGround) then
    begin
      //WritelnLog('fall');
      if ScenePlayer.CurrentAnimation.X3DName <> 'fall' then
        ScenePlayer.PlayAnimation('fall', true)
    end else
      if Abs(Velocity.X) > 1 then
      begin
        //WritelnLog('walk');
        if ScenePlayer.CurrentAnimation.X3DName <> 'walk' then
          ScenePlayer.PlayAnimation('walk', true);
      end else
      begin
        //WritelnLog('idle');
        if ScenePlayer.CurrentAnimation.X3DName <> 'idle' then
          ScenePlayer.PlayAnimation('idle', true);
      end;
  end;
end;

procedure TViewPlay.CollectCoin;
begin
  SoundEngine.Play(NamedSound('Coin'));
  Inc(PlayerCollectedCoins);
  LabelCollectedCoins.Caption := PlayerCollectedCoins.ToString;
end;

procedure TViewPlay.ResetCollectedCoins;
begin
  PlayerCollectedCoins := 0;
  LabelCollectedCoins.Caption := '0';
end;

procedure TViewPlay.HitPlayer;
begin
  SetHitPoints(PlayerHitPoints - 1);
  SoundEngine.Play(NamedSound('Hurt'));
  PlayAnimationOnceAndLoop(ScenePlayer, 'hurt', 'idle');
end;

function TViewPlay.IsPlayerDead: Boolean;
begin
  Result := PlayerHitPoints < 0;
end;

procedure TViewPlay.PauseGame;
begin
  MainViewport.Items.TimeScale := 0;
end;

procedure TViewPlay.ResumeGame;
begin
  MainViewport.Items.TimeScale := 1;
end;

procedure TViewPlay.ResetHitPoints;
begin
  SetHitPoints(4);
end;

procedure TViewPlay.SetHitPoints(const HitPoints: Integer);
begin
  PlayerHitPoints := HitPoints;

  if PlayerHitPoints > 3 then
    ImageHitPoint4.URL := 'castle-data:/ui/hud_heartFull.png'
  else
    ImageHitPoint4.URL := 'castle-data:/ui/hud_heartEmpty.png';

  if PlayerHitPoints > 2 then
    ImageHitPoint3.URL := 'castle-data:/ui/hud_heartFull.png'
  else
    ImageHitPoint3.URL := 'castle-data:/ui/hud_heartEmpty.png';

  if PlayerHitPoints > 1 then
    ImageHitPoint2.URL := 'castle-data:/ui/hud_heartFull.png'
  else
    ImageHitPoint2.URL := 'castle-data:/ui/hud_heartEmpty.png';

  if PlayerHitPoints > 0 then
    ImageHitPoint1.URL := 'castle-data:/ui/hud_heartFull.png'
  else
    ImageHitPoint1.URL := 'castle-data:/ui/hud_heartEmpty.png';
end;

procedure TViewPlay.CollectKey;
begin
  SoundEngine.Play(NamedSound('PowerUp'));
  PlayerHasKey := true;
  ImageKey.Exists := true;
end;

procedure TViewPlay.ResetCollectedKeys;
begin
  PlayerHasKey := false;
  ImageKey.Exists := false;
end;

procedure TViewPlay.PlayAnimationOnceAndLoop(Scene: TCastleScene;
  const AnimationNameToPlayOnce, AnimationNameToLoop: String);
var
  Parameters: TPlayAnimationParameters;
begin
  Parameters := TPlayAnimationParameters.Create;
  try
    Parameters.Loop := false;
    Parameters.Name := AnimationNameToPlayOnce;
    Parameters.Forward := true;
    Parameters.StopNotification := {$ifdef FPC}@{$endif}OnAnimationStop;
    PlayerAnimationToLoop := AnimationNameToLoop;
    Scene.PlayAnimation(Parameters);
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TViewPlay.OnAnimationStop(const Scene: TCastleSceneCore;
  const Animation: TTimeSensorNode);
begin
  Scene.PlayAnimation(PlayerAnimationToLoop, true);
end;

procedure TViewPlay.Start;
var
  { TCastleTransforms that groups objects in our level }
  PlatformsRoot: TCastleTransform;
  EnemiesRoot: TCastleTransform;
  FallingObstaclesRoot: TCastleTransform;
  DeadlyObstaclesRoot: TCastleTransform;

  { Variables used when interating each object groups }
  PlatformScene: TCastleScene;
  EnemyScene: TCastleScene;
  FallingObstacleScene: TCastleScene;
  DeadlyObstacleScene: TCastleScene;

  { Variables used to create behaviors }
  Enemy: TEnemy;
  FallingObstacle: TFallingObstacle;
  DeadlyObstacle: TDeadlyObstacle;
  MovingPlatform: TMovingPlatform;

  I: Integer;
begin
  inherited;

  { Physics layers are configured in editor but you can also make it in that way. }
  {ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[0,1] := true; // ground collide with player
  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[0,0] := true; // ground collide with ground
  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[0,2] := true; // ground collide with enemies

  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[1,1] := false; // player don't collide with player
  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[1,2] := true; // player collide with enemies
  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[1,0] := true; // player collide with ground

  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[2,0] := true;  // enemies collide with ground
  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[2,1] := true;  // enemies collide with player
  ScenePlayer.World.PhysicsProperties.LayerCollisons.Collides[2,2] := false; // enemies don't collide with each other

  ScenePlayer.RigidBody.Layer := 1;}

  LevelComplete := false;

  LevelBounds := TLevelBounds.Create(ScenePlayer.Owner);

  WasInputShot := false;

  { Configure physics and behaviors for platforms }
  MovingPlatforms := TMovingPlatformList.Create(true);
  PlatformsRoot := DesignedComponent('Platforms') as TCastleTransform;
  for I := 0 to PlatformsRoot.Count - 1 do
  begin
    PlatformScene := PlatformsRoot.Items[I] as TCastleScene;
    WritelnLog('Configure platform: ' + PlatformScene.Name);

    { We use Tag to set distance for moving platform, so when its other than 0
      we need add behavior to it. }
    if PlatformScene.Tag <> 0 then
    begin
      MovingPlatform := TMovingPlatform.Create(nil);
      PlatformScene.AddBehavior(MovingPlatform);
      MovingPlatforms.Add(MovingPlatform);
    end;
  end;

  { Configure enemies }
  Enemies := TEnemyList.Create(true);
  EnemiesRoot := DesignedComponent('Enemies') as TCastleTransform;
  for I := 0 to EnemiesRoot.Count - 1 do
  begin
    EnemyScene := EnemiesRoot.Items[I] as TCastleScene;
    {EnemyScene.RigidBody.Layer := 2;}

    if not EnemyScene.Exists then
      Continue;

    { Below using nil as Owner of TEnemy, as the Enemies list already "owns"
      instances of this class, i.e. it will free them. }
    Enemy := TEnemy.Create(nil);
    EnemyScene.AddBehavior(Enemy);
    Enemies.Add(Enemy);
  end;

  { Configure falling obstacles }
  FallingObstacles := TFallingObstaclesList.Create(true);
  FallingObstaclesRoot := DesignedComponent('FallingObstacles') as TCastleTransform;
  for I := 0 to FallingObstaclesRoot.Count - 1 do
  begin
    FallingObstacleScene := FallingObstaclesRoot.Items[I] as TCastleScene;
    { Below using nil as Owner of TFallingObstacle,
      as the FallingObstacles list already "owns" instances of this class,
      i.e. it will free them. }
    FallingObstacle := TFallingObstacle.Create(nil);
    FallingObstacleScene.AddBehavior(FallingObstacle);
    FallingObstacles.Add(FallingObstacle);
  end;

  { Configure deadly obstacles }
  DeadlyObstacles := TDeadlyObstaclesList.Create(true);
  DeadlyObstaclesRoot := DesignedComponent('DeadlyObstacles') as TCastleTransform;
  for I := 0 to DeadlyObstaclesRoot.Count - 1 do
  begin
    DeadlyObstacleScene := DeadlyObstaclesRoot.Items[I] as TCastleScene;
    { Below using nil as Owner of TFallingObstacle,
      as the DeadlyObstacles list already "owns" instances of this class,
      i.e. it will free them. }
    DeadlyObstacle := TDeadlyObstacle.Create(nil);
    DeadlyObstacleScene.AddBehavior(DeadlyObstacle);
    DeadlyObstacles.Add(DeadlyObstacle);
  end;

  { Configure physics for player - done in editor, only event in code }
  ConfigurePlayerPhysics(ScenePlayer);

  ConfigurePlayerAbilities(ScenePlayer);

  { Add player after update listener to set animations and sounds }
  ScenePlayer.AddAfterUpdateListener({$ifdef FPC}@{$endif} AfterPlayerMovementUpdate);
  { Support for touch screen by TCastleInputAxis and TInputShortcut callbacks }
  PlayerModularMovement.SidewayInputAxis.OnUpdate := {$ifdef FPC}@{$endif} TouchScreenMove;
  PlayerModularMovement.InputJump.OnIsPressedCheck := {$ifdef FPC}@{$endif} TouchScreenJump;

  { Preparation of bullet scene }
  ConfigureBulletSpriteScene;

  { Play game music }
  SoundEngine.LoopingChannel[0].Sound := NamedSound('GameMusic');

  WritelnLog('Configuration done');
end;

procedure TViewPlay.Stop;
begin
  FreeAndNil(Enemies);
  FreeAndNil(FallingObstacles);
  FreeAndNil(DeadlyObstacles);
  FreeAndNil(MovingPlatforms);
  inherited;
end;

procedure TViewPlay.Resume;
begin
  inherited Resume;

  { Play game music }
  SoundEngine.LoopingChannel[0].Sound := NamedSound('GameMusic');
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  CamPos: TVector3;
  ViewHeight: Single;
  ViewWidth: Single;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }

  { If player is dead and we did not show game over view we do that }
  if IsPlayerDead and (Container.FrontView <> ViewGameOver) then
  begin
    ScenePlayer.Exists := false;

    Container.PushView(ViewGameOver);
    Exit;
  end;

  { If level is completed and we did not show level complete we do that }
  if LevelComplete and (Container.FrontView <> ViewLevelComplete) then
  begin
    PlayerRigidBody.Exists := false;
    PauseGame;
    Container.PushView(ViewLevelComplete);
    Exit;
  end;

  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Should we move camera with player }
  if CheckboxCameraFollow.Checked then
  begin
    ViewHeight := MainViewport.Camera.Orthographic.EffectiveRect.Height;
    ViewWidth := MainViewport.Camera.Orthographic.EffectiveRect.Width;

    CamPos := MainViewport.Camera.Translation;
    CamPos.X := ScenePlayer.Translation.X;
    CamPos.Y := ScenePlayer.Translation.Y;

    { Keep the camera inside the level }
    if CamPos.Y - ViewHeight / 2 < LevelBounds.Down then
       CamPos.Y := LevelBounds.Down + ViewHeight / 2;

    if CamPos.Y + ViewHeight / 2 > LevelBounds.Top then
       CamPos.Y := LevelBounds.Top - ViewHeight / 2;

    if CamPos.X - ViewWidth / 2 < LevelBounds.Left then
       CamPos.X := LevelBounds.Left + ViewWidth / 2;

    if CamPos.X + ViewWidth / 2 > LevelBounds.Right then
       CamPos.X := LevelBounds.Right - ViewWidth / 2;

    MainViewport.Camera.Translation := CamPos;
  end;

  { Check player should shot }
  if PlayerCanShot and ScenePlayer.Exists and PlayerRigidBody.Exists then
  begin
    if InputShot then
    begin
      if WasInputShot = false  then
      begin
        SoundEngine.Play(NamedSound('Shot'));
        WasInputShot := true;

        Shot(ScenePlayer, ScenePlayer.LocalToWorld(Vector3(ScenePLayer.BoundingBox.SizeX / 2 + 5, 0, 0)),
          Vector3(ScenePlayer.Scale.X, 1, 0));
      end;
    end else
      WasInputShot := false;
  end;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewPlay.Press method should be used to handle keys
    not handled in children controls.
  }

  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;

  if Event.IsKey(keyEscape) and (Container.FrontView = ViewPlay) then
  begin
    PauseGame;
    Container.PushView(ViewPause);
    Exit(true);
  end;
end;

end.
