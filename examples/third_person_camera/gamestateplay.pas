{
  Copyright 2020-2020 Michalis Kamburelis.

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
  CastleTransform, CastleInputs, CastleDebugTransform,
  GameEnemy;

type
  { 3rd-person camera navigation.
    Create an instance of this and assign to @link(TCastleViewport.Navigation) to use.
    Be sure to also assign @link(Avatar).

    Moving the mouse allows to orbit with the camera around the avatar.
    When AimAvatar, it also allows to point the avatar at the appropriate direction.
    When not AimAvatar, it allows to look at the avatar easily from any side
    (e.g. you can then see avatar's face easily).

    Using keys AWSD and arrows you can move and rotate the avatar,
    and the camera will follow.

    Using the mouse wheel you can get closer / further to the avatar.
  }
  // TODO: add this to CastleCameras to be automatically in editor
  // TODO expose TVector3 to be published
  // TODO setting related properties during design, should update camera
  TCastleThirdPersonNavigation = class(TCastleMouseLookNavigation)
  strict private
    FAvatar: TCastleScene;
    FAvatarHierarchy: TCastleTransform;
    FMaxAvatarRotationSpeed: Single;
    FInitialHeightAboveTarget: Single;
    FDistanceToAvatarTarget: Single;
    FAimAvatar: Boolean;
    FAvatarTarget: TVector3;
    {$ifdef AVATAR_TARGET_FORWARD}
    FAvatarTargetForward: TVector3;
    {$endif}
    FMoveSpeed, FCrouchSpeed, FRunSpeed: Single;
    FRotationSpeed: Single;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
    FInput_RightStrafe: TInputShortcut;
    FInput_LeftStrafe: TInputShortcut;
    FInput_CameraCloser: TInputShortcut;
    FInput_CameraFurther: TInputShortcut;
    FInput_Crouch: TInputShortcut;
    FInput_Run: TInputShortcut;
    function RealAvatarHierarchy: TCastleTransform;
    procedure SetAvatar(const Value: TCastleScene);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
    function CameraPositionInitial(const A: TCastleTransform): TVector3;
    function CameraPositionInitial(const A: TCastleTransform; out TargetWorldPos: TVector3): TVector3;
  protected
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
  public
    const
      DefaultInitialHeightAboveTarget = 1.0;
      DefaultDistanceToAvatarTarget = 4.0;
      DefaultMaxAvatarRotationSpeed = 0.1;
      DefaultAvatarTarget: TVector3 = (Data: (0, 2, 0));
      {$ifdef AVATAR_TARGET_FORWARD}
      DefaultAvatarTargetForward: TVector3 = (Data: (0, 2, 0));
      {$endif}
      DefaultMoveSpeed = 1.0;
      DefaultCrouchSpeed = 0.5;
      DefaultRunSpeed = 2.0;
      DefaultRotationSpeed = Pi * 150 / 180;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;

    { Makes camera be positioned with respect to the current properties and avatar.
      Always call this explicitly once.
      Use this after setting properties like @link(Avatar),
      @link(AvatarHierarchy), @link(DistanceToAvatarTarget),
      @link(InitialHeightAboveTarget).

      TODO: At design-time (in CGE editor), this is automatically called after
      changing relevant properties of this navigation. }
    procedure Init;

    { Translation, from the avatar origin, to the "target" of the avatar where camera
      looks at. This is usually head, and this vector should just describe the height
      of head above the ground.
      By default this is DefaultAvatarTarget = (0, 2, 0). }
    property AvatarTarget: TVector3 read FAvatarTarget write FAvatarTarget;

    {$ifdef AVATAR_TARGET_FORWARD}
    { When the camera looks directly behind the avatar's back,
      it points at AvatarTargetForward, not AvatarTarget.
      This allows to place AvatarTargetForward more in forward (and maybe higher/lower)
      than avatar's head.
      This allows to look more "ahead".

      The effective target is a result of lerp between
      AvatarTargetForward and AvatarTarget, depending on how much is camera now close to
      the initial position "looking from the back of avatar".

      The camera is still always rotating around AvatarTarget
      (so you rotate around avatar's head, even if you look far ahead).
      By default this is DefaultAvatarTargetForward = (0, 2, 0).

      TODO: Not implemented now. I made initial implementation using Lerp,
      and it just works awful -- camera rotates unexpectedly, the user feels
      losing control of the camera at certain angles.
    }
    property AvatarTargetForward: TVector3 read FAvatarTargetForward write FAvatarTargetForward;
    {$endif}

    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_LeftRotate: TInputShortcut read FInput_LeftRotate;
    property Input_RightRotate: TInputShortcut read FInput_RightRotate;
    property Input_LeftStrafe: TInputShortcut read FInput_LeftStrafe;
    property Input_RightStrafe: TInputShortcut read FInput_RightStrafe;
    property Input_CameraCloser: TInputShortcut read FInput_CameraCloser;
    property Input_CameraFurther: TInputShortcut read FInput_CameraFurther;
    property Input_Crouch: TInputShortcut read FInput_Crouch;
    property Input_Run: TInputShortcut read FInput_Run;
  published
    property MouseLookHorizontalSensitivity;
    property MouseLookVerticalSensitivity;
    property InvertVerticalMouseLook;

    { Optional avatar hierarchy that is moved and rotated when this navigation changes.
      When this is @nil, we just move and rotate the @link(Avatar).
      When this is non-nil, then we only move and rotate this AvatarHierarchy.

      If @link(AvatarHierarchy) is non-nil, then it should contain
      @link(Avatar) as a child. @link(AvatarHierarchy) can even be equal to @link(Avatar)
      (it is equivalent to just leaving @link(AvatarHierarchy) as @nil). }
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;

    { Avatar scene, that is animated, moved and rotated when this navigation changes.
      This navigation component will just call @code(Avatar.PlayAnimation('xxx')) when necessary.
      Currently we require the following animations to exist: walk, idle.

      When AvatarHierarchy is @nil, then @name is directly moved and rotated
      to move avatar.
      Otherwise, AvatarHierarchy is moved, and @name should be inside AvatarHierarchy. }
    property Avatar: TCastleScene read FAvatar write SetAvatar;

    { When @link(AimAvatar), this is avatar's rotation speed.
      Should be small enough to make avatar rotation "catch up" with some delay after camera
      rotation. }
    property MaxAvatarRotationSpeed: Single read FMaxAvatarRotationSpeed write FMaxAvatarRotationSpeed
      default DefaultMaxAvatarRotationSpeed;

    { If @true (default) then rotating the camera also rotates (with some delay) the avatar,
      to face the same direction as the camera.
      This is easier to use for players, on the other hand it takes away some flexibility,
      e.g. you cannot look at avatar's face for a long time anymore. }
    property AimAvatar: Boolean read FAimAvatar write FAimAvatar default true;

    // TODO AimAvatarVerticalToo

    { Initial height of camera above the AvatarTarget.
      Together with DistanceToAvatarTarget this determines the initial camera position,
      set by @link(Init).
      It is not used outside of @link(Init). }
    property InitialHeightAboveTarget: Single read FInitialHeightAboveTarget write FInitialHeightAboveTarget
      default DefaultInitialHeightAboveTarget;

    { Preferred distance from camera to the avatar target (head).
      By default user can change it with mouse wheel. }
    property DistanceToAvatarTarget: Single read FDistanceToAvatarTarget write FDistanceToAvatarTarget
      default DefaultDistanceToAvatarTarget;

    { Speed of movement by keys. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed default DefaultMoveSpeed;
    { Speed of movement by keys, when crouching. }
    property CrouchSpeed: Single read FCrouchSpeed write FCrouchSpeed default DefaultCrouchSpeed;
    { Speed of movement by keys, when running. }
    property RunSpeed: Single read FRunSpeed write FRunSpeed default DefaultRunSpeed;
    { Speed of rotating by keys, in radians per second. }
    property RotationSpeed: Single read FRotationSpeed write FRotationSpeed
      default DefaultRotationSpeed;
  end;

  { Main "playing game" state, where most of the game logic takes place. }
  TStatePlay = class(TUIState)
  private
    { Enemies behaviours }
    Enemies: TEnemyList;

    ThirdPersonNavigation: TCastleThirdPersonNavigation;

    { Components designed using CGE editor, loaded from state-main.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    SceneAvatar: TCastleScene;
    CheckboxAimAvatar: TCastleCheckbox;
    { Since we use mouse look always, user cannot really operate CheckboxAimAvatar.
      So it only serves to visualize whether the mouse button is pressed now. }
    //procedure ChangeCheckboxAimAvatar(Sender: TObject);
  public
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

{ TCastleThirdPersonNavigation ----------------------------------------------- }

constructor TCastleThirdPersonNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAvatarTarget := DefaultAvatarTarget;
  {$ifdef AVATAR_TARGET_FORWARD}
  FAvatarTargetForward := DefaultAvatarTargetForward;
  {$endif}
  FMaxAvatarRotationSpeed := DefaultMaxAvatarRotationSpeed;
  FAimAvatar := true;
  FInitialHeightAboveTarget := DefaultInitialHeightAboveTarget;
  FDistanceToAvatarTarget := DefaultDistanceToAvatarTarget;
  FMoveSpeed := DefaultMoveSpeed;
  FCrouchSpeed := DefaultCrouchSpeed;
  FRunSpeed := DefaultRunSpeed;
  FRotationSpeed := DefaultRotationSpeed;

  FInput_Forward                 := TInputShortcut.Create(Self);
  FInput_Backward                := TInputShortcut.Create(Self);
  FInput_LeftRotate              := TInputShortcut.Create(Self);
  FInput_RightRotate             := TInputShortcut.Create(Self);
  FInput_LeftStrafe              := TInputShortcut.Create(Self);
  FInput_RightStrafe             := TInputShortcut.Create(Self);
  FInput_CameraCloser            := TInputShortcut.Create(Self);
  FInput_CameraFurther           := TInputShortcut.Create(Self);
  FInput_Crouch                  := TInputShortcut.Create(Self);
  FInput_Run                     := TInputShortcut.Create(Self);

  Input_Forward                 .Assign(keyW, keyUp);
  Input_Backward                .Assign(keyS, keyDown);
  Input_LeftRotate              .Assign(keyLeft, keyA);
  Input_RightRotate             .Assign(keyRight, keyD);
  Input_LeftStrafe              .Assign(keyNone);
  Input_RightStrafe             .Assign(keyNone);
  Input_CameraCloser            .Assign(keyNone);
  Input_CameraFurther           .Assign(keyNone);
  Input_Crouch                  .Assign(keyCtrl);
  Input_Run                     .Assign(keyShift);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_LeftRotate             .SetSubComponent(true);
  Input_RightRotate            .SetSubComponent(true);
  Input_LeftStrafe             .SetSubComponent(true);
  Input_RightStrafe            .SetSubComponent(true);
  Input_CameraCloser           .SetSubComponent(true);
  Input_CameraFurther          .SetSubComponent(true);
  Input_Crouch                 .SetSubComponent(true);
  Input_Run                    .SetSubComponent(true);

  Input_Forward                .Name := 'Input_Forward';
  Input_Backward               .Name := 'Input_Backward';
  Input_LeftRotate             .Name := 'Input_LeftRotate';
  Input_RightRotate            .Name := 'Input_RightRotate';
  Input_LeftStrafe             .Name := 'Input_LeftStrafe';
  Input_RightStrafe            .Name := 'Input_RightStrafe';
  Input_CameraCloser           .Name := 'Input_CameraCloser';
  Input_CameraFurther          .Name := 'Input_CameraFurther';
  Input_Crouch                 .Name := 'Input_Crouch';
  Input_Run                    .Name := 'Input_Run';
end;

function TCastleThirdPersonNavigation.RealAvatarHierarchy: TCastleTransform;
begin
  if AvatarHierarchy <> nil then
    Result := AvatarHierarchy
  else
    Result := Avatar;
end;

procedure TCastleThirdPersonNavigation.SetAvatar(const Value: TCastleScene);
begin
  if FAvatar <> Value then
  begin
    FAvatar := Value;
    // TODO free notification for Avatar, AvatarHierarchy
  end;
end;

procedure TCastleThirdPersonNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if FAvatarHierarchy <> Value then
  begin
    FAvatarHierarchy := Value;
    // TODO free notification for Avatar, AvatarHierarchy
  end;
end;


function TCastleThirdPersonNavigation.CameraPositionInitial(const A: TCastleTransform): TVector3;
var
  TargetWorldPos: TVector3;
begin
  Result := CameraPositionInitial(A, TargetWorldPos); // ignore resulting TargetWorldPos
end;

function TCastleThirdPersonNavigation.CameraPositionInitial(const A: TCastleTransform; out TargetWorldPos: TVector3): TVector3;
var
  GravUp: TVector3;

  { Return V rotated such that it is
    orthogonal to GravUp. This way it returns V projected
    on the gravity horizontal plane.
    Result is always normalized (length 1).

    Note that when V and GravUp are parallel,
    this just returns current V --- because in such case
    we can't project V on the horizontal plane. }
  function ToGravityPlane(const V: TVector3): TVector3;
  begin
    Result := V;
    if not VectorsParallel(Result, GravUp) then
      MakeVectorsOrthoOnTheirPlane(Result, GravUp);
  end;

var
  TargetWorldDir: TVector3;
  HorizontalShiftFromTarget: Single;
begin
  TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
  TargetWorldDir := A.WorldTransform.MultDirection(TCastleTransform.DefaultDirection[A.Orientation]);

  { InitialHeightAboveTarget, HorizontalShiftFromTarget, DistanceToAvatarTarget
    create a right triangle, so
    InitialHeightAboveTarget^2 + HorizontalShiftFromTarget^2 = DistanceToAvatarTarget^2
  }
  HorizontalShiftFromTarget := Sqrt(Sqr(DistanceToAvatarTarget) - Sqr(InitialHeightAboveTarget));
  GravUp := Camera.GravityUp;

  Result := TargetWorldPos
    + GravUp * InitialHeightAboveTarget
    - ToGravityPlane(TargetWorldDir) * HorizontalShiftFromTarget;
end;

procedure TCastleThirdPersonNavigation.Init;
var
  GravUp: TVector3;
  A: TCastleTransform;
  CameraPos, CameraDir, CameraUp, TargetWorldPos: TVector3;
begin
  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    GravUp := Camera.GravityUp;

    CameraPos := CameraPositionInitial(A, TargetWorldPos);
    CameraDir := TargetWorldPos - CameraPos;
    CameraUp := GravUp; // will be adjusted to be orthogonal to Dir by SetView
    Camera.SetView(CameraPos, CameraDir, CameraUp);

    if Avatar <> nil then
    begin
      Avatar.AutoAnimation := 'idle';
      Avatar.ForceInitialAnimationPose;
    end;
  end;
end;

procedure TCastleThirdPersonNavigation.ProcessMouseLookDelta(const Delta: TVector2);
var
  ToCamera, GravUp: TVector3;
  A: TCastleTransform;

  { Change ToCamera by applying DeltaY from mouse look. }
  procedure ProcessVertical(DeltaY: Single);
  const
    { Do not allow to look exactly up or exactly down,
      as then further vertical moves would be undefined,
      so you would not be able to "get out" of such rotation. }
    // TODO: this does't work reliably enough, we can still jump
    MinimalAngleFromZenith = 0.1;
  var
    Side: TVector3;
    AngleToUp, AngleToDown, MaxChange: Single;
  begin
    Side := -TVector3.CrossProduct(ToCamera, GravUp);
    if DeltaY > 0 then
    begin
      AngleToUp := AngleRadBetweenVectors(ToCamera, GravUp);
      MaxChange := Max(0, AngleToUp - MinimalAngleFromZenith);
      if DeltaY > MaxChange then
        DeltaY := MaxChange;
    end else
    begin
      AngleToDown := AngleRadBetweenVectors(ToCamera, -GravUp);
      MaxChange := Max(0, AngleToDown - MinimalAngleFromZenith);
      if DeltaY < -MaxChange then
        DeltaY := -MaxChange;
    end;
    ToCamera := RotatePointAroundAxisRad(DeltaY, ToCamera, Side);
  end;

  procedure ProcessHorizontal(const DeltaX: Single);
  begin
    ToCamera := RotatePointAroundAxisRad(-DeltaX, ToCamera, GravUp);
  end;

var
  CameraPos, CameraDir, CameraUp, TargetWorldPos: TVector3;
begin
  inherited;
  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
    ToCamera := Camera.Position - TargetWorldPos;
    GravUp := Camera.GravityUp;

    ProcessVertical(Delta[1]);
    ProcessHorizontal(Delta[0]);

    // TODO: check collisions
    CameraPos := TargetWorldPos + ToCamera;
    CameraDir := TargetWorldPos - CameraPos;
    CameraUp := GravUp; // will be adjusted to be orthogonal to Dir by SetView
    Camera.SetView(CameraPos, CameraDir, CameraUp);

    // TODO AimAvatar
  end;
end;

function TCastleThirdPersonNavigation.Press(const Event: TInputPressRelease): Boolean;
var
  A: TCastleTransform;

  procedure CameraDistanceChange(DistanceChange: Single);
  const
    // TODO expose properties
    CameraDistanceChangeSpeed = 0.1;
    MinDistanceToAvatarTarget = 0.5;
    MaxDistanceToAvatarTarget = 10;
  var
    TargetWorldPos, ToCamera: TVector3;
  begin
    DistanceChange := DistanceChange * CameraDistanceChangeSpeed;
    DistanceToAvatarTarget := Clamped(DistanceToAvatarTarget + DistanceChange,
      MinDistanceToAvatarTarget, MaxDistanceToAvatarTarget);

    // TODO: why does zooming-in causes assertion in CastleRays, saying CamDirection is NaN?

    // update Camera.Position
    TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
    ToCamera := Camera.Position - TargetWorldPos;
    Camera.Position := TargetWorldPos + ToCamera.AdjustToLength(DistanceToAvatarTarget);
  end;

begin
  Result := inherited;
  if Result then Exit;

  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    if Input_CameraCloser.IsEvent(Event) then
    begin
      CameraDistanceChange(-1);
      Result := ExclusiveEvents;
    end;
    if Input_CameraFurther.IsEvent(Event) then
    begin
      CameraDistanceChange(1);
      Result := ExclusiveEvents;
    end;
  end;
end;

procedure TCastleThirdPersonNavigation.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
var
  A: TCastleTransform;

  // TODO add test level geoemetry to allow avatar fall

  { Make camera follow the A.Translation.
    Does not follow instantly, which makes a nice effect when character is moving fast.
    Following the character also makes sure that camera stays updated
    (keeps DistanceToAvatarTarget)
    when the avatar is being moved by other routines (e.g. because A.Gravity is working). }
  procedure UpdateCamera;
  var
    TargetWorldPos: TVector3;
  begin
    // TODO: add delay with SmoothTowards

    TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
    Camera.Position := TargetWorldPos - Camera.Direction.AdjustToLength(DistanceToAvatarTarget);
  end;

type
  TSpeedType = (stNormal, stCrouch, stRun);
var
  SpeedType: TSpeedType;
  Speed: Single;
  Moving: Boolean;
  T: TVector3;
begin
  inherited;

  // TODO jumping with space, similar to TCastleWalkNavigation mechanics

  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    if Input_Run.IsPressed(Container) then
    begin
      SpeedType := stRun;
      Speed := RunSpeed;
    end else
    if Input_Crouch.IsPressed(Container) then
    begin
      SpeedType := stCrouch;
      Speed := CrouchSpeed;
    end else
    begin
      SpeedType := stNormal;
      Speed := MoveSpeed;
    end;

    T := TVector3.Zero;
    if Input_Forward.IsPressed(Container) then
    begin
      Moving := true;
      T := T + A.Direction * Speed * SecondsPassed;
    end;
    if Input_Backward.IsPressed(Container) then
    begin
      Moving := true;
      T := T - A.Direction * Speed * SecondsPassed;
    end;
    if Input_RightStrafe.IsPressed(Container) then
    begin
      Moving := true;
      T := T + TVector3.CrossProduct(A.Direction, A.Up) * Speed * SecondsPassed;
    end;
    if Input_LeftStrafe.IsPressed(Container) then
    begin
      Moving := true;
      T := T - TVector3.CrossProduct(A.Direction, A.Up) * Speed * SecondsPassed;
    end;
    if Input_RightRotate.IsPressed(Container) then
    begin
      Moving := true;
      A.Direction := RotatePointAroundAxisRad(-RotationSpeed * SecondsPassed, A.Direction, A.Up)
    end;
    if Input_LeftRotate.IsPressed(Container) then
    begin
      Moving := true;
      A.Direction := RotatePointAroundAxisRad(RotationSpeed * SecondsPassed, A.Direction, A.Up);
    end;

    if not T.IsPerfectlyZero then
      A.Move(T, false);

    if Avatar <> nil then
      case SpeedType of
        stNormal: Avatar.AutoAnimation := Iff(Moving, 'walk', 'idle');
        stRun   : Avatar.AutoAnimation := Iff(Moving, 'run', 'idle');
        stCrouch: Avatar.AutoAnimation := Iff(Moving, 'crouch', 'crouch'); // both when moving and not moving
        // else raise EInternalError.Create('Unhandled SpeedType'); // new FPC will warn in case of unhandled "else"
      end;

    // TODO: drag camera after rotation also, if AimAvatar

    UpdateCamera;
  end;
end;

(*TODO:
  - document / show a way to use this with TPlayer and TLevel
*)

{ TStatePlay ----------------------------------------------------------------- }

procedure TStatePlay.Start;
var
  UiOwner: TComponent;
  SoldierScene: TCastleScene;
  Enemy: TEnemy;
  I: Integer;
  DebugAvatar: TDebugTransform;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_play.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;
  MainViewport := UiOwner.FindRequiredComponent('MainViewport') as TCastleViewport;
  SceneAvatar := UiOwner.FindRequiredComponent('SceneAvatar') as TCastleScene;
  CheckboxAimAvatar := UiOwner.FindRequiredComponent('CheckboxAimAvatar') as TCastleCheckbox;

  { Create TEnemy instances, add them to Enemies list }
  Enemies := TEnemyList.Create(true);
  for I := 1 to 4 do
  begin
    SoldierScene := UiOwner.FindRequiredComponent('SceneSoldier' + IntToStr(I)) as TCastleScene;
    Enemy := TEnemy.Create(SoldierScene);
    Enemies.Add(Enemy);
  end;

  //CheckboxAimAvatar.OnChange := @ChangeCheckboxAimAvatar;

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

  DebugAvatar := TDebugTransform.Create(FreeAtStop);
  DebugAvatar.Attach(SceneAvatar);
  DebugAvatar.Exists := true;

  { Initialize 3rd-person camera initialization }
  ThirdPersonNavigation := TCastleThirdPersonNavigation.Create(FreeAtStop);
  MainViewport.Navigation := ThirdPersonNavigation;

  // Assign some keys that are not assigned by default
  ThirdPersonNavigation.Input_LeftStrafe.Assign(keyQ);
  ThirdPersonNavigation.Input_RightStrafe.Assign(keyE);
  ThirdPersonNavigation.Input_CameraCloser.Assign(keyNone, keyNone, '', false, mbLeft, mwUp);
  ThirdPersonNavigation.Input_CameraFurther.Assign(keyNone, keyNone, '', false, mbLeft, mwDown);

  ThirdPersonNavigation.Avatar := SceneAvatar;
  ThirdPersonNavigation.CrouchSpeed := 2;
  ThirdPersonNavigation.MoveSpeed := 2;
  ThirdPersonNavigation.RunSpeed := 8;
  // TODO: as test: Allow to put camera on right / left shoulder
  ThirdPersonNavigation.Init;

  // TODO:
  // Decide then whether CheckboxAimAvatar reflects the mbRight, or pressing it makes something.
  // (if not, we need to set CheckboxAimAvatar.Enabled = false)

  ThirdPersonNavigation.MouseLook := true; // by default use mouse look
end;

procedure TStatePlay.Stop;
begin
  FreeAndNil(Enemies);
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure UpdateAimAvatar;
  begin
    ThirdPersonNavigation.AimAvatar := mbRight in Container.MousePressed;
    { Since we use mouse look always, user cannot really operate CheckboxAimAvatar.
      So it only serves to visualize whether the mouse button is pressed now. }
    CheckboxAimAvatar.Checked := ThirdPersonNavigation.AimAvatar;
  end;

begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  UpdateAimAvatar;
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;
var
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

  if Event.IsMouseButton(mbLeft) then
  begin
    SoundEngine.Sound(SoundEngine.SoundFromName('shoot_sound'));

    { We clicked on enemy if
      - TransformUnderMouse indicates we hit something
      - This something has, as 1st child, a TEnemy instance.

      We depend here on our TEnemy behaviour:
      - TEnemy instance adds itself as child of TCastleScene
      - TEnemy has no collidable things by itself, so it's not listed among MouseRayHit.
    }
    if (MainViewport.TransformUnderMouse <> nil) and
       (MainViewport.TransformUnderMouse.Count > 0) and
       (MainViewport.TransformUnderMouse.Items[0] is TEnemy) then
    begin
      HitEnemy := MainViewport.TransformUnderMouse.Items[0] as TEnemy;
      HitEnemy.Hurt;
    end;

    Exit(true);
  end;

  if Event.IsKey(CtrlM) then
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
end;

{
procedure TStatePlay.ChangeCheckboxAimAvatar(Sender: TObject);
begin
  ThirdPersonNavigation.AimAvatar := CheckboxAimAvatar.Checked;
end;
}

end.
