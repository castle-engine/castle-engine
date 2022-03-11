{
  Copyright 2021-2021 Andrzej Kilijański, Michalis Kamburelis.

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
  CastleKeysMouse, CastleViewport, CastleScene, CastleSceneCore, CastleVectors,
  CastleTransform, CastleSoundEngine, X3DNodes,
  GameEnemy, GameFallingObstacle, GameDeadlyObstacle, GameMovingPlatform;

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
  public
    constructor Create(AOwner: TComponent; BulletSpriteScene: TCastleScene); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  { Main "playing game" state, where most of the game logic takes place. }
  TStatePlay = class(TUIState)
  strict private
    { Components designed using CGE editor, loaded from state_play.castle-user-interface. }
    LabelFps: TCastleLabel;
    LabelCollectedCoins: TCastleLabel;
    MainViewport: TCastleViewport;
    ScenePlayer: TCastleScene;
    CheckboxCameraFollow: TCastleCheckbox;
    CheckboxAdvancedPlayer: TCastleCheckbox;
    ImageHitPoint4: TCastleImageControl;
    ImageHitPoint3: TCastleImageControl;
    ImageHitPoint2: TCastleImageControl;
    ImageHitPoint1: TCastleImageControl;
    ImageKey: TCastleImageControl;
    CoinsRoot: TCastleTransform;

    { Checks this is first Update when the InputJump occurred.
      See ../README.md for documentation about allowed keys/mouse/touch input. }
    WasInputJump: Boolean;

    { Checks this is firs Update when InputShot occurred.
      See ../README.md for documentation about allowed keys/mouse/touch input. }
    WasInputShot: Boolean;

    { Player abilities }
    PlayerCanDoubleJump: Boolean;
    WasDoubleJump: Boolean;
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

    procedure ConfigurePlatformPhysics(Platform: TCastleScene);
    procedure ConfigureCoinsPhysics(const Coin: TCastleScene);
    procedure ConfigurePowerUpsPhysics(const PowerUp: TCastleScene);
    procedure ConfigureGroundPhysics(const Ground: TCastleScene);
    procedure ConfigureStonePhysics(const Stone: TCastleScene);
    procedure ConfigureDoorsPhysics(const Door: TCastleScene);
    procedure ConfigureKeysPhysics(const Key: TCastleScene);

    procedure ConfigurePlayerPhysics(const Player:TCastleScene);
    procedure ConfigurePlayerAbilities(const Player:TCastleScene);
    procedure PlayerCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
    procedure PlayerCollisionExit(const CollisionDetails: TPhysicsCollisionDetails);
    procedure ChangePlayerPhysicsSettingsBasedOnGround(const Player,
      Ground: TCastleTransform);
    procedure ConfigureBulletSpriteScene;

    procedure ConfigureEnemyPhysics(const EnemyScene: TCastleScene);

    { Simplest version }
    procedure UpdatePlayerSimpleDependOnlyVelocity(const SecondsPassed: Single;
      var HandleInput: Boolean);

    { More advanced version with ray to check "Are we on ground?" }
    procedure UpdatePlayerByVelocityAndRay(const SecondsPassed: Single;
      var HandleInput: Boolean);

    { More advanced version with ray to check "Are we on ground?" and
      double jump }
    procedure UpdatePlayerByVelocityAndRayWithDblJump(const SecondsPassed: Single;
      var HandleInput: Boolean);

    { More advanced version with physics ray to check "Are we on ground?" and
      double jump }
    procedure UpdatePlayerByVelocityAndPhysicsRayWithDblJump(const SecondsPassed: Single;
      var HandleInput: Boolean);

    { More advanced version with physics ray to check "Are we on ground?",
      double jump, shot and move acceleration frame rate independed }
    procedure UpdatePlayerByVelocityAndPhysicsRayWithDblJumpShot(const SecondsPassed: Single;
      var HandleInput: Boolean);

    procedure Shot(BulletOwner: TComponent; const Origin, Direction: TVector3);

    { Coins support }
    procedure CollectCoin;
    procedure ResetCollectedCoins;

    { Life support }
    procedure ResetHitPoints;
    procedure SetHitPoints(const HitPoints: Integer);

    { Key support }
    procedure CollectKey;
    procedure ResetCollectedKeys;

    procedure PlayAnimationOnceAndLoop(Scene: TCastleScene;
      const AnimationNameToPlayOnce, AnimationNameToLoop: String);
    procedure OnAnimationStop(const Scene: TCastleSceneCore;
      const Animation: TTimeSensorNode);

    { Check pressed keys and mouse/touch, to support both keyboard
      and mouse and touch (on mobile) navigation. }
    function InputLeft: Boolean;
    function InputRight: Boolean;
    function InputJump: Boolean;
    function InputShot: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resume; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;

    { Public functions }
    procedure HitPlayer;
    function IsPlayerDead: Boolean;

    procedure PauseGame;
    procedure ResumeGame;
  end;

var
  StatePlay: TStatePlay;

implementation

uses
  SysUtils, Math,
  CastleLog,
  GameStateMenu, GameStateGameOver, GameStateLevelComplete, GameStatePause;

{ TBullet -------------------------------------------------------------------- }

constructor TBullet.Create(AOwner: TComponent; BulletSpriteScene: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TSphereCollider;
begin
  inherited Create(AOwner);

  Add(BulletSpriteScene);
  BulletSpriteScene.Visible := true;
  BulletSpriteScene.Translation := Vector3(0, 0, 0);

  RBody := TRigidBody.Create(Self);
  RBody.Setup2D;
  RBody.Dynamic := true;
  RBody.MaximalLinearVelocity := 0;


  Collider := TSphereCollider.Create(RBody);
  Collider.Radius :=  BulletSpriteScene.BoundingBox.Size.X / 2;
  { Make bullet more bouncy }
  Collider.Restitution := 0.6;
  Collider.Mass := 1;

  RigidBody := RBody;
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

{ TStatePlay ----------------------------------------------------------------- }

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

function TStatePlay.InputLeft: Boolean;
var
  I: Integer;
begin
  Result :=
    Container.Pressed.Items[keyA] or
    Container.Pressed.Items[keyArrowLeft];

  { Mouse, or any finger, pressing in left-lower part of the screen.

    Note: if we would not need to support multi-touch (and only wanted
    to check 1st finger) then we would use simpler "Container.MousePosition"
    instead of "Container.TouchesCount", "Container.Touches[..].Position". }

  if buttonLeft in Container.MousePressed then
    for I := 0 to Container.TouchesCount - 1 do
      if (Container.Touches[I].Position.X < Container.Width * 0.5) and
         (Container.Touches[I].Position.Y < Container.Height * 0.5) then
        Exit(true);
end;

function TStatePlay.InputRight: Boolean;
var
  I: Integer;
begin
  Result :=
    Container.Pressed.Items[keyD] or
    Container.Pressed.Items[keyArrowRight];

  { Mouse, or any finger, pressing in left-lower part of the screen. }
  if buttonLeft in Container.MousePressed then
    for I := 0 to Container.TouchesCount - 1 do
      if (Container.Touches[I].Position.X >= Container.Width * 0.5) and
         (Container.Touches[I].Position.Y < Container.Height * 0.5) then
        Exit(true);
end;

function TStatePlay.InputJump: Boolean;
var
  I: Integer;
begin
  Result :=
    Container.Pressed.Items[keyW] or
    Container.Pressed.Items[keyArrowUp];

  { Mouse, or any finger, pressing in upper part of the screen. }
  if buttonLeft in Container.MousePressed then
    for I := 0 to Container.TouchesCount - 1 do
      if (Container.Touches[I].Position.Y >= Container.Height * 0.5) then
        Exit(true);
end;

function TStatePlay.InputShot: Boolean;
begin
  Result :=
    Container.Pressed.Items[keySpace] or
    { Right mouse button, or 2 fingers, are held. }
    (buttonRight in Container.MousePressed) or
    (Container.TouchesCount >= 2);
end;

procedure TStatePlay.ConfigurePlatformPhysics(Platform: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TBoxCollider;
  Size: TVector3;
begin
  RBody := TRigidBody.Create(Platform);

  { Platforms that can move has Tag <> 0, so they are dynamic bodies }
  RBody.Dynamic := (Platform.Tag <> 0);

  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];

  { If Tag > 0 we move platform horizontal, if Tag < 0 we move platform
    vertical. }
  if Platform.Tag > 0 then
    RBody.LockTranslation := [1, 2]
  else if Platform.Tag < 0 then
    RBody.LockTranslation := [0, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.MaximalAngularVelocity := 0;

  Collider := TBoxCollider.Create(RBody);

  Size.X := Platform.BoundingBox.SizeX;
  Size.Y := Platform.BoundingBox.SizeY;
  Size.Z := 60;

  Collider.Size := Size;
  if Platform.Tag <> 0 then
    Collider.Friction := 100;
  Collider.Restitution := 0.0;
  Collider.Mass := 1000;

  Platform.RigidBody := RBody;
end;

procedure TStatePlay.ConfigureCoinsPhysics(const Coin: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TSphereCollider;
begin
  RBody := TRigidBody.Create(Coin);
  RBody.Dynamic := false;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.Trigger := true;

  Collider := TSphereCollider.Create(RBody);
  Collider.Radius := Coin.BoundingBox.SizeY / 8;
  Coin.RigidBody := RBody;
end;

procedure TStatePlay.ConfigurePowerUpsPhysics(const PowerUp: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TSphereCollider;
begin
  RBody := TRigidBody.Create(PowerUp);
  RBody.Dynamic := false;
  //RBody.Animated := true;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.Trigger := true;

  Collider := TSphereCollider.Create(RBody);
  Collider.Radius := PowerUp.BoundingBox.SizeY / 8;
  Collider.Friction := 0.1;
  Collider.Restitution := 0.05;

  PowerUp.RigidBody := RBody;
end;

procedure TStatePlay.ConfigureGroundPhysics(const Ground: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TBoxCollider;
  Size: TVector3;
begin
  RBody := TRigidBody.Create(Ground);
  RBody.Dynamic := false;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];

  Collider := TBoxCollider.Create(RBody);

  Size.X := Ground.BoundingBox.SizeX;
  Size.Y := Ground.BoundingBox.SizeY;
  Size.Z := 1;

  Collider.Size := Size;

  Ground.RigidBody := RBody;
end;

procedure TStatePlay.ConfigureStonePhysics(const Stone: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TBoxCollider;
  Size: TVector3;
begin
  RBody := TRigidBody.Create(Stone);
  RBody.Dynamic := false;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];

  Collider := TBoxCollider.Create(RBody);

  Size.X := Stone.BoundingBox.SizeX;
  Size.Y := Stone.BoundingBox.SizeY;
  Size.Z := 1;

  Collider.Size := Size;

  Stone.RigidBody := RBody;
end;

procedure TStatePlay.ConfigureDoorsPhysics(const Door: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TSphereCollider;
begin
  RBody := TRigidBody.Create(Door);
  RBody.Dynamic := false;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.Trigger := true;

  Collider := TSphereCollider.Create(RBody);
  Collider.Radius := Door.BoundingBox.SizeX / 2.1;
  Door.RigidBody := RBody;
end;

procedure TStatePlay.ConfigureKeysPhysics(const Key: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TSphereCollider;
begin
  RBody := TRigidBody.Create(Key);
  RBody.Dynamic := false;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.Trigger := true;

  Collider := TSphereCollider.Create(RBody);
  Collider.Radius := Key.BoundingBox.SizeX / 4;
  Key.RigidBody := RBody;
end;

procedure TStatePlay.ConfigurePlayerPhysics(const Player: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TCapsuleCollider;
begin
  RBody := TRigidBody.Create(Player);
  RBody.Dynamic := true;
  RBody.Setup2D;
  RBody.Gravity := true;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.OnCollisionEnter := {$ifdef FPC}@{$endif}PlayerCollisionEnter;
  RBody.OnCollisionExit := {$ifdef FPC}@{$endif}PlayerCollisionExit;

  Collider := TCapsuleCollider.Create(RBody);
  Collider.Radius := ScenePlayer.BoundingBox.SizeX * 0.45; // little smaller than 50%
  Collider.Height := ScenePlayer.BoundingBox.SizeY - Collider.Radius * 2;
  Collider.Friction := 0.25;
  Collider.Restitution := 0.0001;
  Collider.Mass := 50;

  {ColliderSP := TSphereCollider.Create(RBody);
  ColliderSP.Radius := ScenePlayer.BoundingBox.SizeX * 0.45;}

{  ColliderBox := TBoxCollider.Create(RBody);
  ColliderBox.Size := Vector3(ScenePlayer.BoundingBox.SizeX, ScenePlayer.BoundingBox.SizeY, 60.0);
  Collider.Friction := 0.5;
  //Collider.Restitution := 0.05;
  Collider.Mass := 50;

  WriteLnLog('Player collider: ' + FloatToStr(ColliderBox.Size.X) + ', ' +
  FloatToStr(ColliderBox.Size.Y) + ', ' + FloatToStr(ColliderBox.Size.Z));}

  Player.RigidBody := RBody;

  WasInputJump := false;
end;

procedure TStatePlay.ConfigurePlayerAbilities(const Player: TCastleScene);
begin
  PlayerCanDoubleJump := false;
  WasDoubleJump := false;
  PlayerCanShot := false;
  ResetHitPoints;

  ResetCollectedCoins;
  ResetCollectedKeys;
end;

procedure TStatePlay.PlayerCollisionEnter(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  if CollisionDetails.OtherTransform <> nil then
  begin
    if Pos('GoldCoin', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollectCoin;
      CollisionDetails.OtherTransform.Exists := false;
      //TODO: Exists in root problem workaround (https://github.com/castle-engine/castle-engine/pull/292)
      CollisionDetails.OtherTransform.RigidBody.Exists := false;
    end else
    if Pos('DblJump', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      SoundEngine.Play(SoundEngine.SoundFromName('power_up'));
      PlayerCanDoubleJump := true;
      CollisionDetails.OtherTransform.Exists := false;
      //TODO: Exists in root problem workaround (https://github.com/castle-engine/castle-engine/pull/292)
      CollisionDetails.OtherTransform.RigidBody.Exists := false;
    end else
    if Pos('Shot', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      SoundEngine.Play(SoundEngine.SoundFromName('power_up'));
      PlayerCanShot := true;
      CollisionDetails.OtherTransform.Exists := false;
      //TODO: Exists in root problem workaround (https://github.com/castle-engine/castle-engine/pull/292)
      CollisionDetails.OtherTransform.RigidBody.Exists := false;
    end else
    if Pos('Key', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      CollectKey;
      CollisionDetails.OtherTransform.Exists := false;
      //TODO: Exists in root problem workaround (https://github.com/castle-engine/castle-engine/pull/292)
      CollisionDetails.OtherTransform.RigidBody.Exists := false;
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

procedure TStatePlay.PlayerCollisionExit(
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

procedure TStatePlay.ChangePlayerPhysicsSettingsBasedOnGround(const Player,
  Ground: TCastleTransform);
begin
  { When player is on moving platform he can't have Restitution > 0.0001 because
    he will slide on it. But when he fall to other ground and Restitution is
    small the movement looks not naturally. }

  if (Ground <> nil) and (Pos('Platform', Ground.Name) > 0) and
     (Ground.Tag <> 0) then
    Player.RigidBody.Collider.Restitution := 0.0001
  else
    Player.RigidBody.Collider.Restitution := 0.05;
end;

procedure TStatePlay.ConfigureEnemyPhysics(const EnemyScene: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TSphereCollider;
begin
  RBody := TRigidBody.Create(EnemyScene);
  RBody.Dynamic := true;
  //RBody.Animated := true;
  RBody.Setup2D;
  RBody.Gravity := true;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.OnCollisionEnter := {$ifdef FPC}@{$endif}PlayerCollisionEnter;

  Collider := TSphereCollider.Create(RBody);
  Collider.Radius := EnemyScene.BoundingBox.SizeY * 0.45; // little smaller than 50%
  Collider.Friction := 0.1;
  Collider.Restitution := 0.05;

  {ColliderBox := TBoxCollider.Create(RBody);
  ColliderBox.Size := Vector3(ScenePlayer.BoundingBox.SizeX, ScenePlayer.BoundingBox.SizeY, 30.0);
  ColliderBox.Friction := 0.1;
  ColliderBox.Restitution := 0.05;}

  EnemyScene.RigidBody := RBody;
end;

procedure TStatePlay.ConfigureBulletSpriteScene;
begin
  BulletSpriteScene := TCastleScene.Create(FreeAtStop);
  BulletSpriteScene.URL := 'castle-data:/bullet/particle_darkGrey.png';
  BulletSpriteScene.Scale := Vector3(0.5, 0.5, 0.5);
end;

procedure TStatePlay.UpdatePlayerSimpleDependOnlyVelocity(
  const SecondsPassed: Single; var HandleInput: Boolean);
const
  JumpVelocity = 700;
  MaxHorizontalVelocity = 350;
var
  DeltaVelocity: TVector3;
  Vel: TVector3;
  PlayerOnGround: Boolean;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { This is not ideal you can do another jump when Player is
    on top of the jump you can make next jump, but can be nice mechanic
    for someone }
  PlayerOnGround := (Abs(Vel.Y) < 10);

  if InputJump then
  begin
    if (not WasInputJump) and PlayerOnGround then
    begin
      DeltaVelocity.Y := JumpVelocity;
      WasInputJump := true;
    end;
  end else
    WasInputJump := false;


  if InputRight and PlayerOnGround then
  begin
    DeltaVelocity.x := MaxHorizontalVelocity / 2;
  end;

  if InputLeft and PlayerOnGround then
  begin
    DeltaVelocity.x := - MaxHorizontalVelocity / 2;
  end;

  if Vel.X + DeltaVelocity.X > 0 then
    Vel.X := Min(Vel.X + DeltaVelocity.X, MaxHorizontalVelocity)
  else
    Vel.X := Max(Vel.X + DeltaVelocity.X, -MaxHorizontalVelocity);

  Vel.Y := Vel.Y + DeltaVelocity.Y;
  Vel.Z := 0;

  { Stop the player without slipping }
  if PlayerOnGround and (not InputRight) and (not InputLeft) then
    Vel.X := 0;

  ScenePlayer.RigidBody.LinearVelocity := Vel;

  { Set animation }

  { We get here 20 because vertical velocity calculated by physics engine when
    player is on platform have no 0 but some small values to up and down sometimes
    It can fail when the player goes uphill (will set jump animation) or down
    will set fall animation }
  if Vel.Y > 20 then
    ScenePlayer.PlayAnimation('jump', true)
  else
  if Vel.Y < -20 then
    ScenePlayer.PlayAnimation('fall', true)
  else
    if Abs(Vel.X) > 1 then
    begin
      if ScenePlayer.CurrentAnimation.X3DName <> 'walk' then
        ScenePlayer.PlayAnimation('walk', true);
    end
    else
      ScenePlayer.PlayAnimation('idle', true);

  if Vel.X < -1 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else if Vel.X > 1 then
    ScenePlayer.Scale := Vector3(1, 1, 1);
end;

procedure TStatePlay.UpdatePlayerByVelocityAndRay(const SecondsPassed: Single;
  var HandleInput: Boolean);
const
  JumpVelocity = 700;
  MaxHorizontalVelocity = 350;
var
  DeltaVelocity: TVector3;
  Vel: TVector3;
  PlayerOnGround: Boolean;
  Distance: Single;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { Check player is on ground }
  if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(0, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
    Distance) <> nil then
  begin
    // WriteLnLog('Distance ', FloatToStr(Distance));
    PlayerOnGround := Distance < 2;
  end else
    PlayerOnGround := false;


  { Two more checks Kraft - player should slide down when player just
    on the edge, maybe be can remove that when add Capsule collider }
  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.40, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(ScenePlayer.BoundingBox.SizeX * 0.40, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if InputJump then
  begin
    if (not WasInputJump) and PlayerOnGround then
    begin
      DeltaVelocity.Y := JumpVelocity;
      WasInputJump := true;
    end;
  end else
    WasInputJump := false;


  if InputRight and PlayerOnGround then
  begin
    DeltaVelocity.x := MaxHorizontalVelocity / 2;
  end;

  if InputLeft and PlayerOnGround then
  begin
    DeltaVelocity.x := - MaxHorizontalVelocity / 2;
  end;

  if Vel.X + DeltaVelocity.X > 0 then
    Vel.X := Min(Vel.X + DeltaVelocity.X, MaxHorizontalVelocity)
  else
    Vel.X := Max(Vel.X + DeltaVelocity.X, -MaxHorizontalVelocity);

  Vel.Y := Vel.Y + DeltaVelocity.Y;
  Vel.Z := 0;

  { Stop the player without slipping }
  if PlayerOnGround and (not InputRight) and (not InputLeft) then
    Vel.X := 0;

  ScenePlayer.RigidBody.LinearVelocity := Vel;

  { Set animation }

  { We get here 20 because vertical velocity calculated by physics engine when
    player is on platform have no 0 but some small values to up and down sometimes
    It can fail when the player goes uphill (will set jump animation) or down
    will set fall animation }
  if Vel.Y > 20 then
    ScenePlayer.PlayAnimation('jump', true)
  else
  if Vel.Y < -20 then
    ScenePlayer.PlayAnimation('fall', true)
  else
    if Abs(Vel.X) > 1 then
    begin
      if ScenePlayer.CurrentAnimation.X3DName <> 'walk' then
        ScenePlayer.PlayAnimation('walk', true);
    end
    else
      ScenePlayer.PlayAnimation('idle', true);

  if Vel.X < -1 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else if Vel.X > 1 then
    ScenePlayer.Scale := Vector3(1, 1, 1);
end;

procedure TStatePlay.UpdatePlayerByVelocityAndRayWithDblJump(
  const SecondsPassed: Single; var HandleInput: Boolean);
const
  JumpVelocity = 700;
  MaxHorizontalVelocity = 350;
var
  DeltaVelocity: TVector3;
  Vel: TVector3;
  PlayerOnGround: Boolean;
  Distance: Single;
  InSecondJump: Boolean;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  InSecondJump := false;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { Check player is on ground }
  if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(0, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
    Distance) <> nil then
  begin
    // WriteLnLog('Distance ', FloatToStr(Distance));
    PlayerOnGround := Distance < 2;
  end else
    PlayerOnGround := false;


  { Two more checks Kraft - player should slide down when player just
    on the edge, maye be can remove that when add Capsule collider }
  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.40 , -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(ScenePlayer.BoundingBox.SizeX * 0.40, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if PlayerOnGround then
    WasDoubleJump := false;

  if InputJump then
  begin
    if (not WasInputJump) and (PlayerOnGround or (PlayerCanDoubleJump and (not WasDoubleJump))) then
    begin
      if not PlayerOnGround then
      begin
        WasDoubleJump := true;
        InSecondJump := true;
        { In second jump just add diffrence betwen current Velocity and JumpVelocity }
        DeltaVelocity.Y := JumpVelocity - Vel.Y;
      end else
        DeltaVelocity.Y := JumpVelocity;
      WasInputJump := true;
    end;
  end else
    WasInputJump := false;

  if InputRight then
  begin
    if PlayerOnGround then
      DeltaVelocity.x := MaxHorizontalVelocity / 2
    else if InSecondJump then
      { When key is pressed when you make second jump you can increase
        horizontal speed }
      DeltaVelocity.x := MaxHorizontalVelocity / 3
    else
      { This add a little control when you in the air during jumping or falling }
      DeltaVelocity.x := MaxHorizontalVelocity / 20;
  end;

  if InputLeft then
  begin
    if PlayerOnGround then
      DeltaVelocity.x := - MaxHorizontalVelocity / 2
    else if InSecondJump then
      { When key is pressed when you make second jump you can increase
        horizontal speed }
      DeltaVelocity.x := - MaxHorizontalVelocity / 3
    else
      { This add a little control when you in the air during jumping or falling }
      DeltaVelocity.x := - MaxHorizontalVelocity / 20;
  end;

  if Vel.X + DeltaVelocity.X > 0 then
    Vel.X := Min(Vel.X + DeltaVelocity.X, MaxHorizontalVelocity)
  else
    Vel.X := Max(Vel.X + DeltaVelocity.X, -MaxHorizontalVelocity);

  Vel.Y := Vel.Y + DeltaVelocity.Y;
  Vel.Z := 0;

  { Stop the player without slipping }
  if PlayerOnGround and (not InputRight) and (not InputLeft) then
    Vel.X := 0;

  ScenePlayer.RigidBody.LinearVelocity := Vel;

  { Set animation }

  { We get here 20 because vertical velocity calculated by physics engine when
    player is on platform have no 0 but some small values to up and down sometimes
    It can fail when the player goes uphill (will set jump animation) or down
    will set fall animation }
  if (not PlayerOnGround) and (Vel.Y > 20) then
    ScenePlayer.PlayAnimation('jump', true)
  else
  if (not PlayerOnGround) and (Vel.Y < -20) then
    ScenePlayer.PlayAnimation('fall', true)
  else
    if Abs(Vel.X) > 1 then
    begin
      if ScenePlayer.CurrentAnimation.X3DName <> 'walk' then
        ScenePlayer.PlayAnimation('walk', true);
    end
    else
      ScenePlayer.PlayAnimation('idle', true);

  if Vel.X < -1 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else if Vel.X > 1 then
    ScenePlayer.Scale := Vector3(1, 1, 1);
end;

procedure TStatePlay.UpdatePlayerByVelocityAndPhysicsRayWithDblJump(
  const SecondsPassed: Single; var HandleInput: Boolean);
const
  JumpVelocity = 700;
  MaxHorizontalVelocity = 350;
  AirControlFactor = 20;
var
  DeltaVelocity: TVector3;
  Vel: TVector3;
  PlayerOnGround: Boolean;
  InSecondJump: Boolean;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  InSecondJump := false;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { Check player is on ground }
  PlayerOnGround := ScenePlayer.RigidBody.PhysicsRayCast(ScenePlayer.Translation,
    Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5) <> nil;

  { Two more checks Kraft - player should slide down when player just
    on the edge, but sometimes it stay and center ray dont "see" that we are
    on ground }
  if PlayerOnGround = false then
  begin
    PlayerOnGround := ScenePlayer.RigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.40, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5) <> nil;
  end;

  if PlayerOnGround = false then
  begin
    PlayerOnGround := ScenePlayer.RigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(ScenePlayer.BoundingBox.SizeX * 0.40, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5) <> nil;
  end;

  if PlayerOnGround then
    WasDoubleJump := false;

  if InputJump then
  begin
    if (not WasInputJump) and (PlayerOnGround or (PlayerCanDoubleJump and (not WasDoubleJump))) then
    begin
      if not PlayerOnGround then
      begin
        WasDoubleJump := true;
        InSecondJump := true;
        { In second jump just add diffrence betwen current Velocity and JumpVelocity }
        DeltaVelocity.Y := JumpVelocity - Vel.Y;
      end else
        DeltaVelocity.Y := JumpVelocity;
      WasInputJump := true;
    end;
  end else
    WasInputJump := false;

  if InputRight then
  begin
    if PlayerOnGround then
      DeltaVelocity.x := MaxHorizontalVelocity / 2
    else if InSecondJump then
      { When key is pressed when you make second jump you can increase
        horizontal speed }
      DeltaVelocity.x := MaxHorizontalVelocity / 3
    else
      { This add a little control when you in the air during jumping or falling }
      DeltaVelocity.x := MaxHorizontalVelocity / 20;
  end;

  if InputLeft then
  begin
    if PlayerOnGround then
      DeltaVelocity.x := - MaxHorizontalVelocity / 2
    else if InSecondJump then
      { When key is pressed when you make second jump you can increase
        horizontal speed }
      DeltaVelocity.x := - MaxHorizontalVelocity / 3
    else
      { This add a little control when you in the air during jumping or falling }
      DeltaVelocity.x := - MaxHorizontalVelocity / 20;
  end;

  if Vel.X + DeltaVelocity.X > 0 then
    Vel.X := Min(Vel.X + DeltaVelocity.X, MaxHorizontalVelocity)
  else
    Vel.X := Max(Vel.X + DeltaVelocity.X, -MaxHorizontalVelocity);

  Vel.Y := Vel.Y + DeltaVelocity.Y;
  Vel.Z := 0;

  { Stop the player without slipping }
  if PlayerOnGround and (not InputRight) and (not InputLeft) then
    Vel.X := 0;

  ScenePlayer.RigidBody.LinearVelocity := Vel;

  { Set animation }

  { We get here 20 because vertical velocity calculated by physics engine when
    player is on platform have no 0 but some small values to up and down sometimes
    It can fail when the player goes uphill (will set jump animation) or down
    will set fall animation }
  if (not PlayerOnGround) and (Vel.Y > 20) then
    ScenePlayer.PlayAnimation('jump', true)
  else
  if (not PlayerOnGround) and (Vel.Y < -20) then
    ScenePlayer.PlayAnimation('fall', true)
  else
    if Abs(Vel.X) > 1 then
    begin
      if ScenePlayer.CurrentAnimation.X3DName <> 'walk' then
        ScenePlayer.PlayAnimation('walk', true);
    end
    else
      ScenePlayer.PlayAnimation('idle', true);

  if Vel.X < -1 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else if Vel.X > 1 then
    ScenePlayer.Scale := Vector3(1, 1, 1);
end;

procedure TStatePlay.UpdatePlayerByVelocityAndPhysicsRayWithDblJumpShot(
  const SecondsPassed: Single; var HandleInput: Boolean);
const
  JumpVelocity = 680;
  MaxHorizontalVelocity = 345;
  { We need multiply any horizontal velocity speed by SecondsPassed.
    Without that when game will run 120 FPS, player will accelerated
    twice faster than on 60 FPS.
    So MaxHorizontalVelocityChange is designed and tested on 60 FPS so we need
    multiply MaxHorizontalVelocity by 60 to get it.

    It's easy to realize when you know that for 60 FPS:

    MaxHorizontalVelocityChange * SecondsPassed * 60 = 350
    21000 * (1/60) * 60 = 350
    21000 * 0.01666 * 60 = 350

    And for 120 FPS:
    21000 * (1/120) * 60 = 175
    21000 * 0.008333 * 60 = 175
    For 120 FPS every frame max speed up will be 175 but you have two times
    more frames (updates). So 175 * 2 = 350 like in 60 FPS.

    We don't need that for jump because jump is one time event not changed
    per update. If something depend from update call frequency you need make it
    depend from time passed in CGE SecondsPassed.
    }
  MaxHorizontalVelocityChange = MaxHorizontalVelocity * 60;
var
  DeltaVelocity: TVector3;
  Vel: TVector3;
  PlayerOnGround: Boolean;
  InSecondJump: Boolean;
  GroundScene: TCastleTransform;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { Check player is on ground }
  GroundScene := ScenePlayer.RigidBody.PhysicsRayCast(ScenePlayer.Translation,
    Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5);

  { Two more checks - player should slide down when player just
    on the edge, but sometimes it stay and center ray don't "see" that we are
    on ground }
  if GroundScene = nil then
  begin
    GroundScene := ScenePlayer.RigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.40, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5);
  end;

  if GroundScene = nil then
  begin
    GroundScene := ScenePlayer.RigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(ScenePlayer.BoundingBox.SizeX * 0.40, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5);
  end;

  { Fix restitution for moving platforms - when player is on moving platform
    and restitution is to big he can slide from platform because it will jumping
    a little, but in other hand when restitution is too small jumping looks
    not natural }
  ChangePlayerPhysicsSettingsBasedOnGround(ScenePlayer, GroundScene);

  { Player is on ground when RayCasts hits something }
  PlayerOnGround := (GroundScene <> nil);

  { Reset DoubleJump flag when player is on ground. }
  if PlayerOnGround then
    WasDoubleJump := false;

  { Flag for velocity calculation when second jump starts in this Update }
  InSecondJump := false;
  if InputJump then
  begin
    { Player can jump when:
      - is on ground
      - he can double jump and there was not WasDoubleJump
      - here we also check if the key has just been pressed (when it is held,
        the player should not keep jumping) }
    if (not WasInputJump) and (PlayerOnGround or (PlayerCanDoubleJump and (not WasDoubleJump))) then
    begin
      SoundEngine.Play(SoundEngine.SoundFromName('jump'));
      if not PlayerOnGround then
      begin
        WasDoubleJump := true;
        InSecondJump := true;
        { In second jump just add diffrence between current Velocity and JumpVelocity }
        DeltaVelocity.Y := JumpVelocity - Vel.Y;
      end else
        DeltaVelocity.Y := JumpVelocity;
      WasInputJump := true;
    end;
  end else
    WasInputJump := false;

  if InputRight then
  begin
    if PlayerOnGround then
      DeltaVelocity.x := MaxHorizontalVelocityChange * SecondsPassed / 2
    else if InSecondJump then
      { When key is pressed when you make second jump you can increase
        horizontal speed }
      DeltaVelocity.x := MaxHorizontalVelocityChange * SecondsPassed / 3
    else
      { This add a little control when you in the air during jumping or falling }
      DeltaVelocity.x := MaxHorizontalVelocityChange * SecondsPassed / 14;
  end;

  if InputLeft then
  begin
    if PlayerOnGround then
      DeltaVelocity.x := - MaxHorizontalVelocityChange * SecondsPassed / 2
    else if InSecondJump then
      { When key is pressed when you make second jump you can increase
        horizontal speed }
      DeltaVelocity.x := - MaxHorizontalVelocityChange * SecondsPassed / 3
    else
      { This add a little control when you in the air during jumping or falling }
      DeltaVelocity.x := - MaxHorizontalVelocityChange * SecondsPassed / 14;
  end;

  if Vel.X + DeltaVelocity.X > 0 then
    Vel.X := Min(Vel.X + DeltaVelocity.X, MaxHorizontalVelocity)
  else
    Vel.X := Max(Vel.X + DeltaVelocity.X, -MaxHorizontalVelocity);

  Vel.Y := Vel.Y + DeltaVelocity.Y;
  Vel.Z := 0;

  { Stop the player without slipping }
  if PlayerOnGround and (not InputRight) and (not InputLeft) then
    Vel.X := 0;

  { Player can't move when hurt on ground }
  if PlayerOnGround and (ScenePlayer.CurrentAnimation.X3DName = 'hurt') then
  begin
    Vel.X := 0;
    Vel.Y := 0;
  end;

  ScenePlayer.RigidBody.LinearVelocity := Vel;

  { Set animation }

  { Don't change animation when player are hurt }
  if ScenePlayer.CurrentAnimation.X3DName <> 'hurt' then
  begin
  { We get here 20 because vertical velocity calculated by physics engine when
    player is on platform have no 0 but some small values to up and down sometimes
    It can fail when the player goes uphill (will set jump animation) or down
    will set fall animation }
  if (not PlayerOnGround) and (Vel.Y > 20) then
    ScenePlayer.PlayAnimation('jump', true)
  else
  if (not PlayerOnGround) and (Vel.Y < -20) then
    ScenePlayer.PlayAnimation('fall', true)
  else
    if Abs(Vel.X) > 1 then
    begin
      if ScenePlayer.CurrentAnimation.X3DName <> 'walk' then
        ScenePlayer.PlayAnimation('walk', true);
    end
    else
      ScenePlayer.PlayAnimation('idle', true);
  end;

  { Here we use horizontal velocity to change player scene direction to moving
    direction. }
  if Vel.X < -1 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else if Vel.X > 1 then
    ScenePlayer.Scale := Vector3(1, 1, 1);

  if PlayerCanShot then
  begin
    if InputShot then
    begin
      if WasInputShot = false  then
      begin
        SoundEngine.Play(SoundEngine.SoundFromName('shot'));
        WasInputShot := true;

        Shot(ScenePlayer, ScenePlayer.LocalToWorld(Vector3(ScenePLayer.BoundingBox.SizeX / 2 + 5, 0, 0)),
          Vector3(ScenePlayer.Scale.X, 1, 0));
      end;
    end else
      WasInputShot := false;
  end;
end;

procedure TStatePlay.Shot(BulletOwner: TComponent; const Origin,
  Direction: TVector3);
var
  Bullet: TBullet;
begin
  Bullet := TBullet.Create(BulletOwner, BulletSpriteScene);
  Bullet.Translation := Origin;
  Bullet.RigidBody.LinearVelocity := Direction * Vector3(750, 20, 0);
  MainViewport.Items.Add(Bullet);
end;

procedure TStatePlay.CollectCoin;
begin
  SoundEngine.Play(SoundEngine.SoundFromName('coin'));
  Inc(PlayerCollectedCoins);
  LabelCollectedCoins.Caption := PlayerCollectedCoins.ToString;
end;

procedure TStatePlay.ResetCollectedCoins;
begin
  PlayerCollectedCoins := 0;
  LabelCollectedCoins.Caption := '0';
end;

procedure TStatePlay.HitPlayer;
begin
  SetHitPoints(PlayerHitPoints - 1);
  SoundEngine.Play(SoundEngine.SoundFromName('hurt'));
  PlayAnimationOnceAndLoop(ScenePlayer, 'hurt', 'idle');
end;

function TStatePlay.IsPlayerDead: Boolean;
begin
  Result := PlayerHitPoints < 0;
end;

procedure TStatePlay.PauseGame;
begin
  MainViewport.Items.TimeScale := 0;
end;

procedure TStatePlay.ResumeGame;
begin
  MainViewport.Items.TimeScale := 1;
end;

procedure TStatePlay.ResetHitPoints;
begin
  SetHitPoints(4);
end;

procedure TStatePlay.SetHitPoints(const HitPoints: Integer);
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

procedure TStatePlay.CollectKey;
begin
  SoundEngine.Play(SoundEngine.SoundFromName('power_up'));
  PlayerHasKey := true;
  ImageKey.Exists := true;
end;

procedure TStatePlay.ResetCollectedKeys;
begin
  PlayerHasKey := false;
  ImageKey.Exists := false;
end;

procedure TStatePlay.PlayAnimationOnceAndLoop(Scene: TCastleScene;
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

procedure TStatePlay.OnAnimationStop(const Scene: TCastleSceneCore;
  const Animation: TTimeSensorNode);
begin
  Scene.PlayAnimation(PlayerAnimationToLoop, true);
end;

procedure TStatePlay.Start;
var
  { TCastleTransforms that groups objects in our level }
  PlatformsRoot: TCastleTransform;

  GroundsRoot: TCastleTransform;
  GroundsLineRoot: TCastleTransform;
  StonesRoot: TCastleTransform;
  EnemiesRoot: TCastleTransform;
  FallingObstaclesRoot: TCastleTransform;
  DeadlyObstaclesRoot: TCastleTransform;
  PowerUps: TCastleTransform;
  DoorsRoot: TCastleTransform;
  KeysRoot: TCastleTransform;

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

  I, J: Integer;
begin
  inherited;

  LevelComplete := false;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  LabelCollectedCoins := DesignedComponent('LabelCollectedCoins') as TCastleLabel;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  CheckboxCameraFollow := DesignedComponent('CheckboxCameraFollow') as TCastleCheckbox;
  CheckboxAdvancedPlayer := DesignedComponent('CheckboxAdvancedPlayer') as TCastleCheckbox;
  ImageHitPoint1 := DesignedComponent('ImageHitPoint1') as TCastleImageControl;
  ImageHitPoint2 := DesignedComponent('ImageHitPoint2') as TCastleImageControl;
  ImageHitPoint3 := DesignedComponent('ImageHitPoint3') as TCastleImageControl;
  ImageHitPoint4 := DesignedComponent('ImageHitPoint4') as TCastleImageControl;
  ImageKey := DesignedComponent('ImageKey') as TCastleImageControl;

  ScenePlayer := DesignedComponent('ScenePlayer') as TCastleScene;

  WasInputShot := false;

  { Configure physics and behaviors for platforms }
  MovingPlatforms := TMovingPlatformList.Create(true);
  PlatformsRoot := DesignedComponent('Platforms') as TCastleTransform;
  for I := 0 to PlatformsRoot.Count - 1 do
  begin
    PlatformScene := PlatformsRoot.Items[I] as TCastleScene;
    WritelnLog('Configure platform: ' + PlatformScene.Name);
    ConfigurePlatformPhysics(PlatformScene);

    { We use Tag to set distance for moving platform, so when its other than 0
      we need add behavior to it. }
    if PlatformScene.Tag <> 0 then
    begin
      MovingPlatform := TMovingPlatform.Create(nil);
      PlatformScene.AddBehavior(MovingPlatform);
      MovingPlatforms.Add(MovingPlatform);
    end;
  end;

  { Configure physics for coins }
  CoinsRoot := DesignedComponent('Coins') as TCastleTransform;
  for I := 0 to CoinsRoot.Count - 1 do
  begin
    WritelnLog('Configure coin: ' + CoinsRoot.Items[I].Name);
    ConfigureCoinsPhysics(CoinsRoot.Items[I] as TCastleScene);
  end;

  LevelBounds := TLevelBounds.Create(CoinsRoot.Owner);

  { Configure physics for ground  }

  GroundsRoot := DesignedComponent('Grounds') as TCastleTransform;
  for I := 0 to GroundsRoot.Count - 1 do
  begin
    if (Pos('GroundLine', GroundsRoot.Items[I].Name) = 1) or
       (Pos('GroundOther', GroundsRoot.Items[I].Name) = 1) then
    begin
      GroundsLineRoot := GroundsRoot.Items[I];
      for J := 0 to GroundsLineRoot.Count - 1 do
      begin
        ConfigureGroundPhysics(GroundsLineRoot.Items[J] as TCastleScene);
      end;
    end;
  end;

  StonesRoot := DesignedComponent('Stones') as TCastleTransform;
  for I := 0 to StonesRoot.Count - 1 do
  begin
    ConfigureStonePhysics(StonesRoot.Items[I] as TCastleScene);
  end;

  PowerUps := DesignedComponent('PowerUps') as TCastleTransform;
  for I := 0 to PowerUps.Count - 1 do
  begin
    ConfigurePowerUpsPhysics(PowerUps.Items[I] as TCastleScene);
  end;

  DoorsRoot := DesignedComponent('Doors') as TCastleTransform;
  for I := 0 to DoorsRoot.Count - 1 do
  begin
    ConfigureDoorsPhysics(DoorsRoot.Items[I] as TCastleScene);
  end;

  KeysRoot := DesignedComponent('Keys') as TCastleTransform;
  for I := 0 to KeysRoot.Count - 1 do
  begin
    ConfigureKeysPhysics(KeysRoot.Items[I] as TCastleScene);
  end;

  Enemies := TEnemyList.Create(true);
  EnemiesRoot := DesignedComponent('Enemies') as TCastleTransform;
  for I := 0 to EnemiesRoot.Count - 1 do
  begin
    EnemyScene := EnemiesRoot.Items[I] as TCastleScene;

    if not EnemyScene.Exists then
      Continue;

    ConfigureEnemyPhysics(EnemyScene);
    { Below using nil as Owner of TEnemy, as the Enemies list already "owns"
      instances of this class, i.e. it will free them. }
    Enemy := TEnemy.Create(nil);
    EnemyScene.AddBehavior(Enemy);
    Enemies.Add(Enemy);
  end;

  FallingObstacles := TFallingObstaclesList.Create(true);
  FallingObstaclesRoot := DesignedComponent('FallingObstacles') as TCastleTransform;
  for I := 0 to FallingObstaclesRoot.Count - 1 do
  begin
    FallingObstacleScene := FallingObstaclesRoot.Items[I] as TCastleScene;
    { Below using nil as Owner of TFallingObstacle, as the list already "owns"
      instances of this class, i.e. it will free them. }
    FallingObstacle := TFallingObstacle.Create(nil);
    FallingObstacleScene.AddBehavior(FallingObstacle);
    FallingObstacles.Add(FallingObstacle);
  end;

  DeadlyObstacles := TDeadlyObstaclesList.Create(true);
  DeadlyObstaclesRoot := DesignedComponent('DeadlyObstacles') as TCastleTransform;
  for I := 0 to DeadlyObstaclesRoot.Count - 1 do
  begin
    DeadlyObstacleScene := DeadlyObstaclesRoot.Items[I] as TCastleScene;
    { Below using nil as Owner of TFallingObstacle, as the list already "owns"
      instances of this class, i.e. it will free them. }
    DeadlyObstacle := TDeadlyObstacle.Create(nil);
    DeadlyObstacleScene.AddBehavior(DeadlyObstacle);
    DeadlyObstacles.Add(DeadlyObstacle);
  end;

  { Configure physics for player }
  ConfigurePlayerPhysics(ScenePlayer);
  ConfigurePlayerAbilities(ScenePlayer);

  ConfigureBulletSpriteScene;

  { Play game music }
  SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('game_music');

  WritelnLog('Configuration done');
end;

procedure TStatePlay.Stop;
begin
  FreeAndNil(Enemies);
  FreeAndNil(FallingObstacles);
  FreeAndNil(DeadlyObstacles);
  FreeAndNil(MovingPlatforms);
  inherited;
end;

procedure TStatePlay.Resume;
begin
  inherited Resume;

  { Play game music }
  SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('game_music');
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  CamPos: TVector3;
  ViewHeight: Single;
  ViewWidth: Single;
begin
  inherited;
  { This virtual method is executed every frame.}

  { If player is dead and we did not show game over state we do that }
  if IsPlayerDead and (TUIState.CurrentTop <> StateGameOver) then
  begin
    ScenePlayer.Exists := false;
    //TODO: Exists in root problem workaround (https://github.com/castle-engine/castle-engine/pull/292)
    ScenePlayer.RigidBody.Exists := false;

    TUIState.Push(StateGameOver);
    Exit;
  end;

  { If level is completed and we did not show level complete we do that }
  if LevelComplete and (TUIState.CurrentTop <> StateLevelComplete) then
  begin
    ScenePlayer.RigidBody.Exists := false;
    PauseGame;
    TUIState.Push(StateLevelComplete);
    Exit;
  end;

  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if CheckboxCameraFollow.Checked then
  begin
    ViewHeight := MainViewport.Camera.Orthographic.EffectiveHeight;
    ViewWidth := MainViewport.Camera.Orthographic.EffectiveWidth;

    CamPos := MainViewport.Camera.Position;
    CamPos.X := ScenePlayer.Translation.X;
    CamPos.Y := ScenePlayer.Translation.Y;

    { Camera always stay on level }
    if CamPos.Y - ViewHeight / 2 < LevelBounds.Down then
       CamPos.Y := LevelBounds.Down + ViewHeight / 2;

    if CamPos.Y + ViewHeight / 2 > LevelBounds.Top then
       CamPos.Y := LevelBounds.Top - ViewHeight / 2;

    if CamPos.X - ViewWidth / 2 < LevelBounds.Left then
       CamPos.X := LevelBounds.Left + ViewWidth / 2;

    if CamPos.X + ViewWidth / 2 > LevelBounds.Right then
       CamPos.X := LevelBounds.Right - ViewWidth / 2;

    MainViewport.Camera.Position := CamPos;
  end;

  if CheckboxAdvancedPlayer.Checked then
    { uncomment to see less advanced versions }
    //UpdatePlayerByVelocityAndRay(SecondsPassed, HandleInput)
    //UpdatePlayerByVelocityAndRayWithDblJump(SecondsPassed, HandleInput)
    //UpdatePlayerByVelocityAndPhysicsRayWithDblJump(SecondsPassed, HandleInput)
    UpdatePlayerByVelocityAndPhysicsRayWithDblJumpShot(SecondsPassed, HandleInput)
  else
    UpdatePlayerSimpleDependOnlyVelocity(SecondsPassed, HandleInput);
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;
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

  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;

  // This will be working when exists in root will be fixed
  {if Event.IsKey(keyF6) then
    CoinsRoot.Exists := not CoinsRoot.Exists;}

  if Event.IsKey(keyEscape) and (TUIState.CurrentTop = StatePlay) then
  begin
    PauseGame;
    TUIState.Push(StatePause);
    Exit(true);
  end;
end;

end.
