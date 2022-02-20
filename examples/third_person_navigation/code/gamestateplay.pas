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
  CastleTransform, CastleInputs, CastleThirdPersonNavigation, CastleDebugTransform,
  CastleSceneCore,
  GameEnemy;

type
  { Main "playing game" state, where most of the game logic takes place. }
  TStatePlay = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state-main.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    ThirdPersonNavigation: TCastleThirdPersonNavigation;
    SceneAvatar, SceneLevel: TCastleScene;
    CheckboxCameraFollows: TCastleCheckbox;
    CheckboxAimAvatar: TCastleCheckbox;
    CheckboxDebugAvatarColliders: TCastleCheckbox;
    CheckboxImmediatelyFixBlockedCamera: TCastleCheckbox;

    { Enemies behaviors }
    Enemies: TEnemyList;

    DebugAvatar: TDebugTransform;

    procedure ChangeCheckboxCameraFollows(Sender: TObject);
    procedure ChangeCheckboxAimAvatar(Sender: TObject);
    procedure ChangeCheckboxDebugAvatarColliders(Sender: TObject);
    procedure ChangeCheckboxImmediatelyFixBlockedCamera(Sender: TObject);
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

uses SysUtils, Math, StrUtils,
  CastleSoundEngine, CastleLog, CastleStringUtils, CastleFilesUtils, CastleUtils,
  GameStateMenu;

{ TStatePlay ----------------------------------------------------------------- }

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

procedure TStatePlay.Start;

  procedure UsePhysicsForAvatarGravity;
  var
    LevelBody: TRigidBody;
    LevelCollider: TMeshCollider;
    AvatarBody: TRigidBody;
    AvatarCollider: TBoxCollider;
  begin
    LevelBody := TRigidBody.Create(FreeAtStop);
    LevelBody.Dynamic := false;

    LevelCollider := TMeshCollider.Create(LevelBody);
    LevelCollider.Scene := SceneLevel;
    LevelCollider.Restitution := 0.3;

    SceneLevel.RigidBody := LevelBody;

    AvatarBody := TRigidBody.Create(FreeAtStop);
    AvatarBody.LockRotation := [0, 2];

    AvatarCollider := TBoxCollider.Create(AvatarBody);
    AvatarCollider.Size := Vector3(1, 2, 1);
    AvatarCollider.Translation := Vector3(0, 1, 0);
    AvatarCollider.Restitution := 0.3;
    AvatarCollider.Density := 100.0;

    SceneAvatar.RigidBody := AvatarBody;
  end;

var
  SoldierScene: TCastleScene;
  Enemy: TEnemy;
  I: Integer;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  ThirdPersonNavigation := DesignedComponent('ThirdPersonNavigation') as TCastleThirdPersonNavigation;
  SceneAvatar := DesignedComponent('SceneAvatar') as TCastleScene;
  SceneLevel := DesignedComponent('SceneLevel') as TCastleScene;
  CheckboxCameraFollows := DesignedComponent('CheckboxCameraFollows') as TCastleCheckbox;
  CheckboxAimAvatar := DesignedComponent('CheckboxAimAvatar') as TCastleCheckbox;
  CheckboxDebugAvatarColliders := DesignedComponent('CheckboxDebugAvatarColliders') as TCastleCheckbox;
  CheckboxImmediatelyFixBlockedCamera := DesignedComponent('CheckboxImmediatelyFixBlockedCamera') as TCastleCheckbox;

  { Create TEnemy instances, add them to Enemies list }
  Enemies := TEnemyList.Create(true);
  for I := 1 to 4 do
  begin
    SoldierScene := DesignedComponent('SceneSoldier' + IntToStr(I)) as TCastleScene;
    { Below using nil as Owner of TEnemy, as the Enemies list already "owns"
      instances of this class, i.e. it will free them. }
    Enemy := TEnemy.Create(nil);
    SoldierScene.AddBehavior(Enemy);
    Enemies.Add(Enemy);
  end;

  CheckboxCameraFollows.OnChange := {$ifdef FPC}@{$endif}ChangeCheckboxCameraFollows;
  CheckboxAimAvatar.OnChange := {$ifdef FPC}@{$endif}ChangeCheckboxAimAvatar;
  CheckboxDebugAvatarColliders.OnChange := {$ifdef FPC}@{$endif}ChangeCheckboxDebugAvatarColliders;
  CheckboxImmediatelyFixBlockedCamera.OnChange := {$ifdef FPC}@{$endif}ChangeCheckboxImmediatelyFixBlockedCamera;

  { TODO: This code does not define USE_PHYSICS_FOR_AVATAR_GRAVITY,
    and uses CGE old physics using TCastleTransform.Gravity.
    We no longer advise using TCastleTransform.Gravity in general,
    and advise to realize gravity using only "full" physics engine like Kraft,
    using TCastleRigidBody / TCastleCollider.

    We work on making this applicable to 3rd-person avatar too,
    so that you could enable UsePhysicsForAvatarGravity (and actually design
    the physics in CGE editor).
    For now, you can use deprecated TCastleTransform.Gravity for avatar.

    Most likely, this will be made possible by enabling TRigidBody.Dynamic and
    TRigidBody.Animated to be both @true. }
  {.$define USE_PHYSICS_FOR_AVATAR_GRAVITY}

  {$ifdef USE_PHYSICS_FOR_AVATAR_GRAVITY}
  UsePhysicsForAvatarGravity;
  {$else}
  { Make SceneAvatar collide using a sphere.
    Sphere is more useful than default bounding box for avatars and creatures
    that move in the world, look ahead, can climb stairs etc. }
  SceneAvatar.MiddleHeight := 0.9;
  SceneAvatar.CollisionSphereRadius := 0.5;

  { Gravity means that object tries to maintain a constant height
    (SceneAvatar.PreferredHeight) above the ground.
    GrowSpeed means that object raises properly (makes walking up the stairs work).
    FallSpeed means that object falls properly (makes walking down the stairs,
    falling down pit etc. work). }
  SceneAvatar.Gravity := true;
  SceneAvatar.GrowSpeed := 10.0;
  SceneAvatar.FallSpeed := 10.0;
  {$endif}

  { Visualize SceneAvatar bounding box, sphere, middle point, direction etc. }
  DebugAvatar := TDebugTransform.Create(FreeAtStop);
  DebugAvatar.Parent := SceneAvatar;

  { Configure ThirdPersonNavigation, some things that cannot be yet adusted using CGE editor.
    In particular assign some keys that are not assigned by default. }
  ThirdPersonNavigation.Input_LeftStrafe.Assign(keyQ);
  ThirdPersonNavigation.Input_RightStrafe.Assign(keyE);
  ThirdPersonNavigation.Input_CameraCloser.Assign(keyNone, keyNone, '', false, buttonLeft, mwUp);
  ThirdPersonNavigation.Input_CameraFurther.Assign(keyNone, keyNone, '', false, buttonLeft, mwDown);
  ThirdPersonNavigation.MouseLook := true; // by default use mouse look
  ThirdPersonNavigation.Init;
end;

procedure TStatePlay.Stop;
begin
  FreeAndNil(Enemies);
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  // Test: use this to make AimAvatar only when *holding* right mouse button.
  (*
  procedure UpdateAimAvatar;
  begin
    if buttonRight in Container.MousePressed then
      ThirdPersonNavigation.AimAvatar := aaHorizontal
    else
      ThirdPersonNavigation.AimAvatar := aaNone;

    { In this case CheckboxAimAvatar only serves to visualize whether
      the right mouse button is pressed now. }
    CheckboxAimAvatar.Checked := ThirdPersonNavigation.AimAvatar <> aaNone;
  end;
  *)

begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  // UpdateAimAvatar;
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;

  function AvatarRayCast: TCastleTransform;
  begin
    { SceneAvatar.RayCast tests a ray collision,
      ignoring the collisions with SceneAvatar itself (so we don't detect our own
      geometry as colliding). }
    Result := SceneAvatar.RayCast(SceneAvatar.Middle, SceneAvatar.Direction);
  end;

var
  HitByAvatar: TCastleTransform;
  HitEnemy: TEnemy;
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
      - HitByAvatar indicates we hit something
      - It has a behavior of TEnemy. }
    HitByAvatar := AvatarRayCast;
    if (HitByAvatar <> nil) and
       (HitByAvatar.FindBehavior(TEnemy) <> nil) then
    begin
      HitEnemy := HitByAvatar.FindBehavior(TEnemy) as TEnemy;
      HitEnemy.Hurt;
    end;

    Exit(true);
  end;

  if Event.IsKey(keyM) then
  begin
    ThirdPersonNavigation.MouseLook := not ThirdPersonNavigation.MouseLook;
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

  if Event.IsMouseButton(buttonRight) then
  begin
    CheckboxAimAvatar.Checked := not CheckboxAimAvatar.Checked;
    ChangeCheckboxAimAvatar(CheckboxAimAvatar); // update ThirdPersonNavigation.AimAvatar
    Exit(true);
  end;
end;

procedure TStatePlay.ChangeCheckboxCameraFollows(Sender: TObject);
begin
  ThirdPersonNavigation.CameraFollows := CheckboxCameraFollows.Checked;
end;

procedure TStatePlay.ChangeCheckboxAimAvatar(Sender: TObject);
begin
  if CheckboxAimAvatar.Checked then
    ThirdPersonNavigation.AimAvatar := aaHorizontal
  else
    ThirdPersonNavigation.AimAvatar := aaNone;

  { The 3rd option, aaFlying, doesn't make sense for this case,
    when avatar walks on the ground and has Gravity = true. }
end;

procedure TStatePlay.ChangeCheckboxDebugAvatarColliders(Sender: TObject);
begin
  DebugAvatar.Exists := CheckboxDebugAvatarColliders.Checked;
end;

procedure TStatePlay.ChangeCheckboxImmediatelyFixBlockedCamera(Sender: TObject);
begin
  ThirdPersonNavigation.ImmediatelyFixBlockedCamera := CheckboxImmediatelyFixBlockedCamera.Checked;
end;

end.
