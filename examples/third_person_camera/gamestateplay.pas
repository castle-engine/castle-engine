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
  CastleTransform,
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
    FMaxCameraRotationSpeed: Single;
    FInitialHeightAboveTarget: Single;
    FDistanceToAvatarTarget: Single;
    FAimAvatar: Boolean;
    FAvatarTarget: TVector3;
    FAvatarTargetForward: TVector3;
    function RealAvatarHierarchy: TCastleTransform;
    procedure SetAvatar(const Value: TCastleScene);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
  protected
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
  public
    const
      DefaultInitialHeightAboveTarget = 1.0;
      DefaultDistanceToAvatarTarget = 4.0;
      DefaultMaxAvatarRotationSpeed = 0.1;
      DefaultMaxCameraRotationSpeed = 0.2;
      DefaultAvatarTarget: TVector3 = (Data: (0, 2, 0));
      DefaultAvatarTargetForward: TVector3 = (Data: (0, 2, 0));

    constructor Create(AOwner: TComponent); override;

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
      By default this is DefaultAvatarTargetForward = (0, 2, 0). }
    property AvatarTargetForward: TVector3 read FAvatarTargetForward write FAvatarTargetForward;
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
      Should be < MaxCameraRotationSpeed to make avatar rotation "catch up" with some delay. }
    property MaxAvatarRotationSpeed: Single read FMaxAvatarRotationSpeed write FMaxAvatarRotationSpeed
      default DefaultMaxAvatarRotationSpeed;

    { Camera rotation speed. }
    property MaxCameraRotationSpeed: Single read FMaxCameraRotationSpeed write FMaxCameraRotationSpeed
      default DefaultMaxCameraRotationSpeed;

    { If @true (default) then rotating the camera also rotates (with some delay) the avatar,
      to face the same direction as the camera.
      This is easier to use for players, on the other hand it takes away some flexibility,
      e.g. you cannot look at avatar's face for a long time anymore. }
    property AimAvatar: Boolean read FAimAvatar write FAimAvatar default true;

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
  public
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

{ TCastleThirdPersonNavigation ----------------------------------------------- }

constructor TCastleThirdPersonNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAvatarTarget := DefaultAvatarTarget;
  FAvatarTargetForward := DefaultAvatarTargetForward;
  FMaxAvatarRotationSpeed := DefaultMaxAvatarRotationSpeed;
  FMaxCameraRotationSpeed := DefaultMaxCameraRotationSpeed;
  FAimAvatar := true;
  FInitialHeightAboveTarget := DefaultInitialHeightAboveTarget;
  FDistanceToAvatarTarget := DefaultDistanceToAvatarTarget;
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

procedure TCastleThirdPersonNavigation.Init;
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
  A: TCastleTransform;
  CameraPos, CameraDir, CameraUp, TargetWorldPos, TargetWorldDir: TVector3;
  HorizontalShiftFromTarget: Single;
begin
  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
    TargetWorldDir := A.WorldTransform.MultDirection(TCastleTransform.DefaultDirection[A.Orientation]);

    { InitialHeightAboveTarget, HorizontalShiftFromTarget, DistanceToAvatarTarget
      create a right triangle, so
      InitialHeightAboveTarget^2 + HorizontalShiftFromTarget^2 = DistanceToAvatarTarget^2
    }
    HorizontalShiftFromTarget := Sqrt(Sqr(DistanceToAvatarTarget) - Sqr(InitialHeightAboveTarget));
    GravUp := Camera.GravityUp;
    CameraPos := TargetWorldPos
      + GravUp * InitialHeightAboveTarget
      - ToGravityPlane(TargetWorldDir) * HorizontalShiftFromTarget;
    CameraDir := TargetWorldPos - CameraPos;
    CameraUp := GravUp; // will be adjusted to be orthogonal to Dir by SetView
    Camera.SetView(CameraPos, CameraDir, CameraUp);

    if Avatar <> nil then
    begin
      Avatar.PlayAnimation('idle', true);
      Avatar.ForceInitialAnimationPose;
    end;
  end;

  MouseLook := true; // always use mouse look
end;

procedure TCastleThirdPersonNavigation.ProcessMouseLookDelta(const Delta: TVector2);
begin
  inherited;
end;

(*TODO:

  - on mouse wheel, decrease / increase DistanceToAvatarTarget within some bounds
    (MinDistanceToAvatarTarget, MaxDistanceToAvatarTarget, default
     DefaultMinDistanceToAvatarTarget, DefaultMaxDistanceToAvatarTarget
     0.1
     10)

    this immediately moves camera

  - moving mouse left / right / up / down orbits around TargetWorld

  - AWSD moves charaters left / right / forward / back (in avatar space,
    looking at avatar Direction and Up)

    changes animation too

  - in demo:
    expose AimAvatar
    describe keys in label

  - document / show a way to use this with TPlayer and TLevel
*)

{ TStatePlay ----------------------------------------------------------------- }

procedure TStatePlay.Start;
var
  UiOwner: TComponent;
  SoldierScene: TCastleScene;
  Enemy: TEnemy;
  I: Integer;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_play.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;
  MainViewport := UiOwner.FindRequiredComponent('MainViewport') as TCastleViewport;
  SceneAvatar := UiOwner.FindRequiredComponent('SceneAvatar') as TCastleScene;

  { Create TEnemy instances, add them to Enemies list }
  Enemies := TEnemyList.Create(true);
  for I := 1 to 4 do
  begin
    SoldierScene := UiOwner.FindRequiredComponent('SceneSoldier' + IntToStr(I)) as TCastleScene;
    Enemy := TEnemy.Create(SoldierScene);
    Enemies.Add(Enemy);
  end;

  { Initialize 3rd-person camera initialization }
  ThirdPersonNavigation := TCastleThirdPersonNavigation.Create(FreeAtStop);
  MainViewport.Navigation := ThirdPersonNavigation;
  ThirdPersonNavigation.Avatar := SceneAvatar;
  ThirdPersonNavigation.Init;
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
