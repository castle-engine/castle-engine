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

{ Main "playing game" view, where most of the game logic takes place. }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
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
    CheckboxAdvancedPlayer: TCastleCheckbox;
    ImageHitPoint4: TCastleImageControl;
    ImageHitPoint3: TCastleImageControl;
    ImageHitPoint2: TCastleImageControl;
    ImageHitPoint1: TCastleImageControl;
    ImageKey: TCastleImageControl;
  strict private
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

    procedure ConfigurePlayerPhysics(const Player:TCastleScene);
    procedure ConfigurePlayerAbilities(const Player:TCastleScene);
    procedure PlayerCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
    procedure PlayerCollisionExit(const CollisionDetails: TPhysicsCollisionDetails);
    procedure ConfigureBulletSpriteScene;

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
  ViewPlay: TViewPlay;

implementation

uses
  SysUtils, Math,
  CastleLog,
  GameSound, GameViewMenu, GameViewGameOver, GameViewLevelComplete, GameViewPause;

{ TBullet -------------------------------------------------------------------- }

constructor TBullet.Create(AOwner: TComponent; const BulletSpriteScene: TCastleScene);
var
  Collider: TCastleSphereCollider;
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

  Collider := TCastleSphereCollider.Create(Self);
  { We don't set the Radius becouse we simply use Autosize }
  // Collider.Radius :=  BulletSpriteScene.BoundingBox.Size.X / 2;
  { Make bullet more bouncy }
  Collider.Restitution := 0.6;
  Collider.Mass := 1;

  AddBehavior(Collider);
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

function TViewPlay.InputLeft: Boolean;
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

function TViewPlay.InputRight: Boolean;
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

function TViewPlay.InputJump: Boolean;
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

function TViewPlay.InputShot: Boolean;
begin
  Result :=
    Container.Pressed.Items[keySpace] or
    { Right mouse button, or 2 fingers, are held. }
    (buttonRight in Container.MousePressed) or
    (Container.TouchesCount >= 2);
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
  WasDoubleJump := false;
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

procedure TViewPlay.UpdatePlayerSimpleDependOnlyVelocity(
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
  Vel := PlayerRigidBody.LinearVelocity;

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

  PlayerRigidBody.LinearVelocity := Vel;

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

procedure TViewPlay.UpdatePlayerByVelocityAndRay(const SecondsPassed: Single;
  var HandleInput: Boolean);
const
  JumpVelocity = 700;
  MaxHorizontalVelocity = 350;
var
  DeltaVelocity: TVector3;
  Vel: TVector3;
  PlayerOnGround: Boolean;
  GroundHit: TPhysicsRayCastResult;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := PlayerRigidBody.LinearVelocity;

  { Check player is on ground }
  GroundHit := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation + Vector3(0, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0));
  if GroundHit.Hit then
  begin
    // WriteLnLog('Distance ', FloatToStr(Distance));
    PlayerOnGround := GroundHit.Distance < 2;
  end else
    PlayerOnGround := false;

  { Two more checks using physics engine - player should slide down when player is just
    on the edge.
    TODO: maybe we can remove this logic after using TCastleCapsule collider for player. }
  if not PlayerOnGround then
  begin
    GroundHit := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.30, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0));
    if GroundHit.Hit then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := GroundHit.Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if not PlayerOnGround then
  begin
    GroundHit := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation + Vector3(ScenePlayer.BoundingBox.SizeX * 0.30, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0));
    if GroundHit.Hit then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := GroundHit.Distance < 2;
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

  PlayerRigidBody.LinearVelocity := Vel;

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

procedure TViewPlay.UpdatePlayerByVelocityAndRayWithDblJump(
  const SecondsPassed: Single; var HandleInput: Boolean);
const
  JumpVelocity = 700;
  MaxHorizontalVelocity = 350;
var
  DeltaVelocity: TVector3;
  Vel: TVector3;
  PlayerOnGround: Boolean;
  GroundHit: TPhysicsRayCastResult;
  InSecondJump: Boolean;
begin
  { This method is executed every frame.}

  { When player is dead, he can't do anything }
  if IsPlayerDead then
    Exit;

  InSecondJump := false;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := PlayerRigidBody.LinearVelocity;

  { Check player is on ground }
  GroundHit := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation + Vector3(0, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0));
  if GroundHit.Hit then
  begin
    // WriteLnLog('Distance ', FloatToStr(Distance));
    PlayerOnGround := GroundHit.Distance < 2;
  end else
    PlayerOnGround := false;

  { Two more checks using physics - player should slide down when player just
    on the edge, maye be can remove that when add Capsule collider }
  if not PlayerOnGround then
  begin
    GroundHit := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.30 , -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0));
    if GroundHit.Hit then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := GroundHit.Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if not PlayerOnGround then
  begin
    GroundHit := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation + Vector3(ScenePlayer.BoundingBox.SizeX * 0.30, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0));
    if GroundHit.Hit then
    begin
      // WriteLnLog('Distance ', FloatToStr(Distance));
      PlayerOnGround := GroundHit.Distance < 2;
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

  PlayerRigidBody.LinearVelocity := Vel;

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

procedure TViewPlay.UpdatePlayerByVelocityAndPhysicsRayWithDblJump(
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
  Vel := PlayerRigidBody.LinearVelocity;

  { Check player is on ground }
  PlayerOnGround := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation,
    Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5).Hit;

  { Two more checks using physics - player should slide down when player just
    on the edge, but sometimes it stay and center ray dont "see" that we are
    on ground }
  if not PlayerOnGround then
  begin
    PlayerOnGround := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.30, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5).Hit;
  end;

  if not PlayerOnGround then
  begin
    PlayerOnGround := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(ScenePlayer.BoundingBox.SizeX * 0.30, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5).Hit;
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

  PlayerRigidBody.LinearVelocity := Vel;

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

procedure TViewPlay.UpdatePlayerByVelocityAndPhysicsRayWithDblJumpShot(
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

  if PlayerRigidBody = nil then
    Exit;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := PlayerRigidBody.LinearVelocity;

  { Check player is on ground }
  GroundScene := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation,
    Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5).Transform;

  { Two more checks - player should slide down when player just
    on the edge, but sometimes it stay and center ray don't "see" that we are
    on ground }
  if GroundScene = nil then
  begin
    GroundScene := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(-ScenePlayer.BoundingBox.SizeX * 0.30, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5).Transform;
  end;

  if GroundScene = nil then
  begin
    GroundScene := PlayerRigidBody.PhysicsRayCast(ScenePlayer.Translation
      + Vector3(ScenePlayer.BoundingBox.SizeX * 0.30, 0, 0),
      Vector3(0, -1, 0), ScenePlayer.BoundingBox.SizeY / 2 + 5).Transform;
  end;

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
      SoundEngine.Play(NamedSound('Jump'));
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

  PlayerRigidBody.LinearVelocity := Vel;

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
        SoundEngine.Play(NamedSound('Shot'));
        WasInputShot := true;

        Shot(ScenePlayer, ScenePlayer.LocalToWorld(Vector3(ScenePLayer.BoundingBox.SizeX / 2 + 5, 0, 0)),
          Vector3(ScenePlayer.Scale.X, 1, 0));
      end;
    end else
      WasInputShot := false;
  end;
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

  if CheckboxCameraFollow.Checked then
  begin
    ViewHeight := MainViewport.Camera.Orthographic.EffectiveRect.Height;
    ViewWidth := MainViewport.Camera.Orthographic.EffectiveRect.Width;

    CamPos := MainViewport.Camera.Translation;
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

    MainViewport.Camera.Translation := CamPos;
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
