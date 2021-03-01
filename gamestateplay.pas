{ Main "playing game" state, where most of the game logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStatePlay;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleTransform;

type
  TLevelBounds = class (TComponent)
  public
    Left: Single;
    Right: Single;
    Top: Single;
    Down: Single;
    constructor Create(AOwner: TComponent);override;
  end;


  { Main "playing game" state, where most of the game logic takes place. }
  TStatePlay = class(TUIState)
  strict private
    { Components designed using CGE editor, loaded from state_play.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    ScenePlayer: TCastleScene;
    CheckboxCameraFollow: TCastleCheckbox;
    CheckboxAdvancedPlayer: TCastleCheckbox;

    { Checks this is firs Update when W key (jump) was pressed }
    WasJumpKeyPressed: Boolean;

    { Player abilities }
    PlayerCanDoubleJump: Boolean;
    WasDoubleJump: Boolean;

    { Level bounds }
    LevelBounds: TLevelBounds;

    procedure ConfigurePlatformPhysics(Platform: TCastleScene);
    procedure ConfigureCoinsPhysics(const Coin: TCastleScene);
    procedure ConfigureGroundPhysics(const Ground: TCastleScene);

    procedure ConfigurePlayerPhysics(const Player:TCastleScene);
    procedure ConfigurePlayerAbilities(const Player:TCastleScene);
    procedure PlayerCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);

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


  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses
  SysUtils, Math,
  CastleLog,
  GameStateMenu;

{ TLevelBounds }

constructor TLevelBounds.Create(AOwner: TComponent);
begin
  Left := -4096;
  Right := 6144;
  Top := 4096;
  Down := -2048;
end;

{ TStatePlay ----------------------------------------------------------------- }

procedure TStatePlay.ConfigurePlatformPhysics(Platform: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TBoxCollider;
  Size: TVector3;
begin
  RBody := TRigidBody.Create(Platform);
  RBody.Dynamic := false;
  RBody.Setup2D;
  RBody.Gravity := false;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];

  Collider := TBoxCollider.Create(RBody);

  Size.X := Platform.BoundingBox.SizeX;
  Size.Y := Platform.BoundingBox.SizeY;
  Size.Z := 1;

  Collider.Size := Size;

  Platform.RigidBody := RBody;
end;

procedure TStatePlay.ConfigureCoinsPhysics(const Coin: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TSphereCollider;
begin
  RBody := TRigidBody.Create(Coin);
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
  Collider.Radius := Coin.BoundingBox.SizeY / 8;
  Collider.Friction := 0.1;
  Collider.Restitution := 0.05;

  WritelnWarning('Coin collider: ' + FloatToStr(Collider.Radius));

  Coin.RigidBody := RBody;
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

procedure TStatePlay.ConfigurePlayerPhysics(const Player: TCastleScene);
var
  RBody: TRigidBody;
  Collider: TCapsuleCollider;
  //ColliderBox: TBoxCollider;
begin
  RBody := TRigidBody.Create(Player);
  RBody.Dynamic := true;
  //RBody.Animated := true;
  RBody.Setup2D;
  RBody.Gravity := true;
  RBody.LinearVelocityDamp := 0;
  RBody.AngularVelocityDamp := 0;
  RBody.AngularVelocity := Vector3(0, 0, 0);
  RBody.LockRotation := [0, 1, 2];
  RBody.MaximalLinearVelocity := 0;
  RBody.OnCollisionEnter := @PlayerCollisionEnter;

  Collider := TCapsuleCollider.Create(RBody);
  Collider.Radius := ScenePlayer.BoundingBox.SizeX * 0.45; // little smaller than 50%
  Collider.Height := ScenePlayer.BoundingBox.SizeY - Collider.Radius * 2;
  Collider.Friction := 0.1;
  Collider.Restitution := 0.05;

  {ColliderBox := TBoxCollider.Create(RBody);
  ColliderBox.Size := Vector3(ScenePlayer.BoundingBox.SizeX, ScenePlayer.BoundingBox.SizeY, 30.0);
  ColliderBox.Friction := 0.1;
  ColliderBox.Restitution := 0.05;

  WritelnWarning('Player collider: ' + FloatToStr(ColliderBox.Size.X) + ', ' +
  FloatToStr(ColliderBox.Size.Y) + ', ' + FloatToStr(ColliderBox.Size.Z));}

  Player.RigidBody := RBody;

  WasJumpKeyPressed := false;
end;

procedure TStatePlay.ConfigurePlayerAbilities(const Player: TCastleScene);
begin
  PlayerCanDoubleJump := true;
  WasDoubleJump := false;
end;

procedure TStatePlay.PlayerCollisionEnter(
  const CollisionDetails: TPhysicsCollisionDetails);
begin
  if CollisionDetails.OtherTransform <> nil then
  begin
    if pos('GoldCoin', CollisionDetails.OtherTransform.Name) > 0 then
    begin
      WritelnWarning('Coin position ' + FloatToStr(CollisionDetails.OtherTransform.Translation.X) + ', ' +
      FloatToStr(CollisionDetails.OtherTransform.Translation.Y) + ', ' +
      FloatToStr(CollisionDetails.OtherTransform.Translation.Z));

      WritelnWarning('Player position ' + FloatToStr(ScenePlayer.Translation.X) + ', ' +
      FloatToStr(ScenePlayer.Translation.Y) + ', ' +
      FloatToStr(ScenePlayer.Translation.Z));

      CollisionDetails.OtherTransform.Exists := false;
    end;
  end;
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

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { This is not ideal you can do another jump when Player is
    on top of the jump you can make next jump, but can be nice mechanic
    for someone }
  PlayerOnGround := (Abs(Vel.Y) < 10);

  if Container.Pressed.Items[keyW] then
  begin
    if (not WasJumpKeyPressed) and PlayerOnGround then
    begin
      DeltaVelocity.Y := JumpVelocity;
      WasJumpKeyPressed := true;
    end;
  end else
    WasJumpKeyPressed := false;


  if Container.Pressed.Items[keyD] and PlayerOnGround then
  begin
    DeltaVelocity.x := MaxHorizontalVelocity / 2;
  end;

  if Container.Pressed.Items[keyA] and PlayerOnGround then
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
  if PlayerOnGround and (Container.Pressed.Items[keyD] = false) and (Container.Pressed.Items[keyA] = false) then
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

  if Vel.X < 0 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else
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

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { Check player is on ground }
  if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(0, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
    Distance) <> nil then
  begin
    // WritelnWarning('Distance ', FloatToStr(Distance));
    PlayerOnGround := Distance < 2;
  end else
    PlayerOnGround := false;


  { Two more checks Kraft - player should slide down when player just
    on the edge, maybe be can remove that when add Capsule collider }
  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(-ScenePlayer.BoundingBox.SizeX / 2, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WritelnWarning('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(ScenePlayer.BoundingBox.SizeX / 2, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WritelnWarning('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if Container.Pressed.Items[keyW] then
  begin
    if (not WasJumpKeyPressed) and PlayerOnGround then
    begin
      DeltaVelocity.Y := JumpVelocity;
      WasJumpKeyPressed := true;
    end;
  end else
    WasJumpKeyPressed := false;


  if Container.Pressed.Items[keyD] and PlayerOnGround then
  begin
    DeltaVelocity.x := MaxHorizontalVelocity / 2;
  end;

  if Container.Pressed.Items[keyA] and PlayerOnGround then
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
  if PlayerOnGround and (Container.Pressed.Items[keyD] = false) and (Container.Pressed.Items[keyA] = false) then
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

  if Vel.X < 0 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else
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

  InSecondJump := false;

  DeltaVelocity := Vector3(0, 0, 0);
  Vel := ScenePlayer.RigidBody.LinearVelocity;

  { Check player is on ground }
  if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(0, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
    Distance) <> nil then
  begin
    // WritelnWarning('Distance ', FloatToStr(Distance));
    PlayerOnGround := Distance < 2;
  end else
    PlayerOnGround := false;


  { Two more checks Kraft - player should slide down when player just
    on the edge, maye be can remove that when add Capsule collider }
  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(-ScenePlayer.BoundingBox.SizeX / 2, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WritelnWarning('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if PlayerOnGround = false then
  begin
    if ScenePlayer.RayCast(ScenePlayer.Translation + Vector3(ScenePlayer.BoundingBox.SizeX / 2, -ScenePlayer.BoundingBox.SizeY / 2, 0), Vector3(0, -1, 0),
      Distance) <> nil then
    begin
      // WritelnWarning('Distance ', FloatToStr(Distance));
      PlayerOnGround := Distance < 2;
    end else
      PlayerOnGround := false;
  end;

  if PlayerOnGround then
    WasDoubleJump := false;

  if Container.Pressed.Items[keyW] then
  begin
    if (not WasJumpKeyPressed) and (PlayerOnGround or (PlayerCanDoubleJump and (not WasDoubleJump))) then
    begin
      if not PlayerOnGround then
      begin
        WasDoubleJump := true;
        InSecondJump := true;
        { In second jump just add diffrence betwen current Velocity and JumpVelocity }
        DeltaVelocity.Y := JumpVelocity - Vel.Y;
      end else
        DeltaVelocity.Y := JumpVelocity;
      WasJumpKeyPressed := true;
    end;
  end else
    WasJumpKeyPressed := false;

  if Container.Pressed.Items[keyD] and (PlayerOnGround or InSecondJump) then
  begin
    if InSecondJump then
      DeltaVelocity.x := MaxHorizontalVelocity / 3
    else
      DeltaVelocity.x := MaxHorizontalVelocity / 2;
  end;

  if Container.Pressed.Items[keyA] and (PlayerOnGround or InSecondJump) then
  begin
    if InSecondJump then
      DeltaVelocity.x := MaxHorizontalVelocity / 3
    else
      DeltaVelocity.x := - MaxHorizontalVelocity / 2;
  end;

  if Vel.X + DeltaVelocity.X > 0 then
    Vel.X := Min(Vel.X + DeltaVelocity.X, MaxHorizontalVelocity)
  else
    Vel.X := Max(Vel.X + DeltaVelocity.X, -MaxHorizontalVelocity);

  Vel.Y := Vel.Y + DeltaVelocity.Y;
  Vel.Z := 0;

  { Stop the player without slipping }
  if PlayerOnGround and (Container.Pressed.Items[keyD] = false) and (Container.Pressed.Items[keyA] = false) then
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

  if Vel.X < 0 then
    ScenePlayer.Scale := Vector3(-1, 1, 1)
  else
    ScenePlayer.Scale := Vector3(1, 1, 1);
end;

procedure TStatePlay.Start;
var
  UiOwner: TComponent;

  PlatformsRoot: TCastleTransform;
  CoinsRoot: TCastleTransform;
  GroundsRoot: TCastleTransform;
  GroundsLineRoot: TCastleTransform;
  I, J: Integer;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_play.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;
  MainViewport := UiOwner.FindRequiredComponent('MainViewport') as TCastleViewport;
  CheckboxCameraFollow := UiOwner.FindRequiredComponent('CheckboxCameraFollow') as TCastleCheckbox;
  CheckboxAdvancedPlayer := UiOwner.FindRequiredComponent('AdvancedPlayer') as TCastleCheckbox;

  ScenePlayer := UiOwner.FindRequiredComponent('ScenePlayer') as TCastleScene;

  { Configure physics for player }
  ConfigurePlayerPhysics(ScenePlayer);
  ConfigurePlayerAbilities(ScenePlayer);

  { Configure physics for platforms }
  PlatformsRoot := UiOwner.FindRequiredComponent('Platforms') as TCastleTransform;
  for I := 0 to PlatformsRoot.Count - 1 do
  begin
    WritelnWarning('Configure platform: ' + PlatformsRoot.Items[I].Name);
    ConfigurePlatformPhysics(PlatformsRoot.Items[I] as TCastleScene);
  end;

  { Configure physics for coins }
  CoinsRoot := UiOwner.FindRequiredComponent('Coins') as TCastleTransform;
  for I := 0 to CoinsRoot.Count - 1 do
  begin
    WritelnWarning('Configure coin: ' + CoinsRoot.Items[I].Name);
    ConfigureCoinsPhysics(CoinsRoot.Items[I] as TCastleScene);
  end;

  LevelBounds := TLevelBounds.Create(UiOwner);

  { Configure physics for ground  }

  GroundsRoot := UiOwner.FindRequiredComponent('Grounds') as TCastleTransform;
  for I := 0 to GroundsRoot.Count - 1 do
  begin
    if pos('GroundLine', GroundsRoot.Items[I].Name) = 1 then
    begin
      GroundsLineRoot := GroundsRoot.Items[I];
      for J := 0 to GroundsLineRoot.Count - 1 do
      begin
        ConfigureGroundPhysics(GroundsLineRoot.Items[J] as TCastleScene);
      end;
    end;
  end;


end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  CamPos: TVector3;
begin
  inherited;
  { This virtual method is executed every frame.}

  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if CheckboxCameraFollow.Checked then
  begin
    CamPos := MainViewport.Camera.Position;
    CamPos.X := ScenePlayer.Translation.X;
    CamPos.Y := ScenePlayer.Translation.Y;
    MainViewport.Camera.Position := CamPos;
  end;

  if CheckboxAdvancedPlayer.Checked then
    { uncomment to see less advanced versions }
    //UpdatePlayerByVelocityAndRay(SecondsPassed, HandleInput)
    UpdatePlayerByVelocityAndRayWithDblJump(SecondsPassed, HandleInput)
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

  if Event.IsKey(keyEscape) then
  begin
    TUIState.Current := StateMenu;
    Exit(true);
  end;
end;

end.
