{ Main "playing game" state, where most of the game logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
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
  TThirdPersonCameraNavigation = class(TCastleNavigation)
  public
    const
      DefaultDistanceToAvatarTarget = 1.0;

    { Makes camera be positioned with respect to the current properties and avatar.
      Always call this explicitly once.
      Use this after setting properties like @link(Avatar),
      @link(AvatarHierarchy), @link(DistanceToAvatarTarget),
      @link(InitialHeightAboveTarget). }
    procedure Init;
  published
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

      When AvatarHierarchy is @nil, this is also directly moved and rotated
      to move avatar.
      Otherwise, AvatarHierarchy is moved, and @name should be inside AvatarHierarchy. }
    property Avatar: TCastleScene read FAvatar write SetAvatar;

    { Translation, from the avatar origin, to the "target" of the avatar where camera
      looks at. This is usually head, and this vector should just describe the height
      of head above the ground.
      By default (0, 2, 0). }
    property AvatarTarget: TVector3

    { When the camera looks directly behind the avatar's back,
      it points at AvatarTargetForward, not AvatarTarget.
      This allows to place AvatarTargetForward more in forward (and maybe higher/lower)
      than avatar's head.
      This allows to look more "ahead".

      The effective target is a result of lerp between
      AvatarTargetForward and AvatarTarget, depending on how much is camera now close to
      the initial position "looking from the back of avatar".

      The camera is still always rotating around AvatarTarget
      (so you rotate around avatar's head, even if you look far ahead). }
    property AvatarTargetForward: TVector3

    { When @link(AimAvatar), this is avatar's rotation speed.
      Should be < MaxCameraRotationSpeed to make avatar rotation "catch up" with some delay. }
    property MaxAvatarRotationSpeed

    { Camera rotation speed. }
    property MaxCameraRotationSpeed

    { If @true (default) then rotating the camera also rotates (with some delay) the avatar,
      to face the same direction as the camera.
      This is easier to use for players, on the other hand it takes away some flexibility,
      e.g. you cannot look at avatar's face for a long time anymore. }
    property AimAvatar: Boolean read FAimAvatar write FAimAvatar default true;

    { Initial height of camera above the AvatarTarget.
      Together with DistanceToAvatarTarget this determines the initial camera position,
      set by @link(Init).
      It is not used outside of @link(Init). }
    property InitialHeightAboveTarget default 1

    { Preferred distance from camera to the avatar target (head).
      By default user can change it with mouse wheel. }
    property DistanceToAvatarTarget read FDistanceToAvatarTarget write FDistanceToAvatarTarget
      default DefaultDistanceToAvatarTarget;
  end;

constructor Create(
  FAvatarTarget := Vector3(0, 2, 0);
  FAimAvatar := true;

function RealAvatarHierarchy: TCastleTransform;
begin
  if AvatarHierarchy <> nil then
    Result := AvatarHierarchy
  else
    Result := Avatar;
end;

SetAvatar
begin
  if FAvatar <> Value then
  begin
    FAvatar := Value;
    // TODO free notification for Avatar, AvatarHierarchy
  end;
end;

SetAvatarHierarchy
begin
  if FAvatarHierarchy <> Value then
  begin
    FAvatarHierarchy := Value;
    // TODO free notification for Avatar, AvatarHierarchy
  end;
end;

procedure Init;
var
  A: TCastleTransform;
begin
  A := RealAvatarHierarchy;
  if A <> nil then
  begin
    TargetWorld := A.WorldTransform(AvatarTarget);
    // TODO: position Viewport.Camera around TargetWorld
  end;
end;

(*TODO:

  - on mouse wheel, decrease / increase DistanceToAvatarTarget within some bounds
    (MinDistanceToAvatarTarget, MaxDistanceToAvatarTarget, default
     DefaultMinDistanceToAvatarTarget, DefaultMaxDistanceToAvatarTarget
     0.1
     10)

    this immediately moves camera

  - activate mouse look (make sure viewport acounts for ot , just like in TCastleWalkNavigation

  - moving mouse left / right / up / down orbits around TargetWorld

  - AWSD moves charaters left / right / forward / back (in avatar space,
    looking at avatar Direction and Up)

    changes animation too

  - in demo:
    remember to use DefaultAnimationTransition
    expose AimAvatar
    describe keys in label
*)

  { Main "playing game" state, where most of the game logic takes place. }
  TStatePlay = class(TUIState)
  private
    { Enemies behaviours }
    Enemies: TEnemyList;

    { Components designed using CGE editor, loaded from state-main.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
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
  WalkNavigation := UiOwner.FindRequiredComponent('WalkNavigation') as TCastleWalkNavigation;

  { Create TEnemy instances, add them to Enemies list }
  Enemies := TEnemyList.Create(true);
  for I := 1 to 4 do
  begin
    SoldierScene := UiOwner.FindRequiredComponent('SceneSoldier' + IntToStr(I)) as TCastleScene;
    Enemy := TEnemy.Create(SoldierScene);
    Enemies.Add(Enemy);
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
