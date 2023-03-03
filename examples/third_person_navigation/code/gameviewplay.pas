{
  Copyright 2020-2023 Michalis Kamburelis.

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

{.$define CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleCameras,
  CastleTransform, CastleInputs, CastleThirdPersonNavigation, CastleDebugTransform,
  CastleSceneCore,
  GameEnemy;

type
  { Main "playing game" view, where most of the game logic takes place. }
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    ThirdPersonNavigation: TCastleThirdPersonNavigation;
    SceneAvatar, SceneLevel: TCastleScene;
    AvatarRigidBody: TCastleRigidBody;
    CheckboxCameraFollows: TCastleCheckbox;
    CheckboxAimAvatar: TCastleCheckbox;
    CheckboxDebugAvatarColliders: TCastleCheckbox;
    CheckboxImmediatelyFixBlockedCamera: TCastleCheckbox;
    SliderAirRotationControl: TCastleFloatSlider;
    SliderAirMovementControl: TCastleFloatSlider;
    ButtonChangeTransformationAuto,
      ButtonChangeTransformationDirect,
      ButtonChangeTransformationVelocity,
      ButtonChangeTransformationForce: TCastleButton;
  private
    { Enemies behaviors }
    Enemies: TEnemyList;

    DebugAvatar: TDebugTransform;
    { Change things after ThirdPersonNavigation.ChangeTransformation changed. }
    procedure UpdateAfterChangeTransformation;
    procedure ChangeCheckboxCameraFollows(Sender: TObject);
    procedure ChangeCheckboxAimAvatar(Sender: TObject);
    procedure ChangeCheckboxDebugAvatarColliders(Sender: TObject);
    procedure ChangeCheckboxImmediatelyFixBlockedCamera(Sender: TObject);
    procedure ChangeAirRotationControl(Sender: TObject);
    procedure ChangeAirMovementControl(Sender: TObject);
    procedure ClickChangeTransformationAuto(Sender: TObject);
    procedure ClickChangeTransformationDirect(Sender: TObject);
    procedure ClickChangeTransformationVelocity(Sender: TObject);
    procedure ClickChangeTransformationForce(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils, Math, StrUtils,
  CastleSoundEngine, CastleLog, CastleStringUtils, CastleFilesUtils, CastleUtils,
  GameViewMenu;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
var
  SoldierScene: TCastleScene;
  Enemy: TEnemy;
  I: Integer;
begin
  inherited;

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

  { synchronize state -> UI }
  SliderAirRotationControl.Value := ThirdPersonNavigation.AirRotationControl;
  SliderAirMovementControl.Value := ThirdPersonNavigation.AirMovementControl;
  UpdateAfterChangeTransformation;

  CheckboxCameraFollows.OnChange := {$ifdef FPC}@{$endif} ChangeCheckboxCameraFollows;
  CheckboxAimAvatar.OnChange := {$ifdef FPC}@{$endif} ChangeCheckboxAimAvatar;
  CheckboxDebugAvatarColliders.OnChange := {$ifdef FPC}@{$endif} ChangeCheckboxDebugAvatarColliders;
  CheckboxImmediatelyFixBlockedCamera.OnChange := {$ifdef FPC}@{$endif} ChangeCheckboxImmediatelyFixBlockedCamera;
  SliderAirRotationControl.OnChange := {$ifdef FPC}@{$endif} ChangeAirRotationControl;
  SliderAirMovementControl.OnChange := {$ifdef FPC}@{$endif} ChangeAirMovementControl;
  ButtonChangeTransformationAuto.OnClick := {$ifdef FPC}@{$endif} ClickChangeTransformationAuto;
  ButtonChangeTransformationDirect.OnClick := {$ifdef FPC}@{$endif} ClickChangeTransformationDirect;
  ButtonChangeTransformationVelocity.OnClick := {$ifdef FPC}@{$endif} ClickChangeTransformationVelocity;
  ButtonChangeTransformationForce.OnClick := {$ifdef FPC}@{$endif} ClickChangeTransformationForce;

  {$ifndef CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}
  { Hide UI to test ChangeTransformation = ctForce, it is not finished now,
    not really useful for normal usage. }
  ButtonChangeTransformationForce.Exists := false;
  {$endif}

  { This configures SceneAvatar.Middle point, used for shooting.
    In case of old physics (ChangeTransformation = ctDirect) this is also the center
    of SceneAvatar.CollisionSphereRadius. }
  SceneAvatar.MiddleHeight := 0.9;

  { Configure some parameters of old simple physics,
    these only matter when SceneAvatar.Gravity = true.
    Don't use these deprecated things if you don't plan to use ChangeTransformation = ctDirect! }
  SceneAvatar.GrowSpeed := 10.0;
  SceneAvatar.FallSpeed := 10.0;
  { When avatar collides as sphere it can climb stairs,
    because legs can temporarily collide with objects. }
  SceneAvatar.CollisionSphereRadius := 0.5;

  { Visualize SceneAvatar bounding box, sphere, middle point, direction etc. }
  DebugAvatar := TDebugTransform.Create(FreeAtStop);
  DebugAvatar.Parent := SceneAvatar;

  { Configure ThirdPersonNavigation keys (for now, we don't expose doing this in CGE editor). }
  ThirdPersonNavigation.Input_LeftStrafe.Assign(keyQ);
  ThirdPersonNavigation.Input_RightStrafe.Assign(keyE);
  ThirdPersonNavigation.MouseLook := true; // TODO: assigning it from editor doesn't make mouse hidden in mouse look
  ThirdPersonNavigation.Init;
end;

procedure TViewPlay.Stop;
begin
  FreeAndNil(Enemies);
  inherited;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);

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
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  // UpdateAimAvatar;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;

  function AvatarRayCast: TCastleTransform;
  var
    RayCastResult: TPhysicsRayCastResult;
  begin
    RayCastResult := AvatarRigidBody.PhysicsRayCast(
      SceneAvatar.Middle,
      SceneAvatar.Direction
    );
    Result := RayCastResult.Transform;

    { Alternative version, using Items.PhysicsRayCast
      (everything in world space coordinates).
      This works equally well, showing it here just for reference.

    RayCastResult := MainViewport.Items.PhysicsRayCast(
      SceneAvatar.Parent.LocalToWorld(SceneAvatar.Middle),
      SceneAvatar.Parent.LocalToWorldDirection(SceneAvatar.Direction),
      MaxSingle,
      AvatarRigidBody
    );
    Result := RayCastResult.Transform;
    }

    (* Alternative versions, using old physics,
       see https://castle-engine.io/physics#_old_system_for_collisions_and_gravity .
       They still work (even when you also use new physics).

    if not AvatarRigidBody.Exists then
    begin
      { SceneAvatar.RayCast tests a ray collision,
        ignoring the collisions with SceneAvatar itself (so we don't detect our own
        geometry as colliding). }
      Result := SceneAvatar.RayCast(SceneAvatar.Middle, SceneAvatar.Direction);
    end else
    begin
      { When physics engine is working, we should not toggle Exists multiple
        times in a single frame, which makes the curent TCastleTransform.RayCast not good.
        So use Items.WorldRayCast, and secure from "hitting yourself" by just moving
        the initial ray point by 0.5 units. }
      Result := MainViewport.Items.WorldRayCast(
        SceneAvatar.Middle + SceneAvatar.Direction * 0.5, SceneAvatar.Direction);
    end;
    *)
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
    The TViewPlay.Press method should be used to handle keys
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

  if Event.IsMouseButton(buttonRight) then
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
    Container.View := ViewMenu;
    Exit(true);
  end;
end;

procedure TViewPlay.ChangeCheckboxCameraFollows(Sender: TObject);
begin
  ThirdPersonNavigation.CameraFollows := CheckboxCameraFollows.Checked;
end;

procedure TViewPlay.ChangeCheckboxAimAvatar(Sender: TObject);
begin
  if CheckboxAimAvatar.Checked then
    ThirdPersonNavigation.AimAvatar := aaHorizontal
  else
    ThirdPersonNavigation.AimAvatar := aaNone;

  { The 3rd option, aaFlying, doesn't make sense for this case,
    when avatar walks on the ground and has Gravity = true. }
end;

procedure TViewPlay.ChangeCheckboxDebugAvatarColliders(Sender: TObject);
begin
  DebugAvatar.Exists := CheckboxDebugAvatarColliders.Checked;
end;

procedure TViewPlay.ChangeCheckboxImmediatelyFixBlockedCamera(Sender: TObject);
begin
  ThirdPersonNavigation.ImmediatelyFixBlockedCamera := CheckboxImmediatelyFixBlockedCamera.Checked;
end;

procedure TViewPlay.ChangeAirRotationControl(Sender: TObject);
begin
  ThirdPersonNavigation.AirRotationControl := SliderAirRotationControl.Value;
end;

procedure TViewPlay.ChangeAirMovementControl(Sender: TObject);
begin
  ThirdPersonNavigation.AirMovementControl := SliderAirMovementControl.Value;
end;

procedure TViewPlay.UpdateAfterChangeTransformation;
begin
  ButtonChangeTransformationAuto.Pressed := ThirdPersonNavigation.ChangeTransformation = ctAuto;
  ButtonChangeTransformationDirect.Pressed := ThirdPersonNavigation.ChangeTransformation =  ctDirect;
  ButtonChangeTransformationVelocity.Pressed := ThirdPersonNavigation.ChangeTransformation =  ctVelocity;
  {$ifdef CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}
  ButtonChangeTransformationForce.Pressed := ThirdPersonNavigation.ChangeTransformation = ctForce;
  {$endif}

  { ctDirect requires to set up gravity without physics engine,
    using deprecated TCastleTransform.Gravity.
    See https://castle-engine.io/physics#_old_system_for_collisions_and_gravity }
  AvatarRigidBody.Exists := ThirdPersonNavigation.ChangeTransformation <> ctDirect;

  { Gravity means that object tries to maintain a constant height
    (SceneAvatar.PreferredHeight) above the ground.
    GrowSpeed means that object raises properly (makes walking up the stairs work).
    FallSpeed means that object falls properly (makes walking down the stairs,
    falling down pit etc. work). }
  SceneAvatar.Gravity := not AvatarRigidBody.Exists;
end;

procedure TViewPlay.ClickChangeTransformationAuto(Sender: TObject);
begin
  ThirdPersonNavigation.ChangeTransformation := ctAuto;
  UpdateAfterChangeTransformation;
end;

procedure TViewPlay.ClickChangeTransformationDirect(Sender: TObject);
begin
  ThirdPersonNavigation.ChangeTransformation := ctDirect;
  UpdateAfterChangeTransformation;
end;

procedure TViewPlay.ClickChangeTransformationVelocity(Sender: TObject);
begin
  ThirdPersonNavigation.ChangeTransformation := ctVelocity;
  UpdateAfterChangeTransformation;
end;

procedure TViewPlay.ClickChangeTransformationForce(Sender: TObject);
begin
  {$ifdef CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}
  ThirdPersonNavigation.ChangeTransformation := ctForce;
  {$endif}
  UpdateAfterChangeTransformation;
end;

end.
