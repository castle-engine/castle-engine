unit FollowingTargetForCamera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform, CastleBehaviors,
  CastleVectors, CastleClassUtils, CastleScene;

type

  { Makes parent follows the target  }
  TFollowingTargetForCamera = class(TCastleBehavior)
  strict private
    FMaxDistanceToTarget: Single;
    FMinDistanceToTarget: Single;
    FTarget: TCastleTransform;
    FDistanceToTarget: Single;
    FCameraSpeed : Single;
    FInitialHeightAboveTarget: Single;
    TargetPoint: TVector3;
    FRadius: Single;
    FImmediatelyFixBlockedCamera: Boolean;
    FInitialised: Boolean;

    function CameraPositionInitial(out TargetWorldPos: TVector3): TVector3;

    { Update camera, to avoid having something collidable between camera position and AvatarTarget.
      Note that CameraDir doesn't have to be normalized. }
    procedure FixCameraForCollisions(var CameraPos: TVector3; const CameraDir: TVector3);

    { Return V rotated such that it is
    orthogonal to GravUp. This way it returns V projected
    on the gravity horizontal plane.
    Result retains the V length (so it is always normalized if V is normalized).

    Note that when V and GravUp are parallel,
    this just returns current V --- because in such case
    we can't project V on the horizontal plane. }
    function ToGravityPlane(const V: TVector3; const GravUp: TVector3): TVector3;

    { Returns MaxSingle if no limit.
      Note that CameraDir doesn't have to be normalized. }
    function CameraMaxDistanceToTarget(const CameraLookPos: TVector3;
      const CameraDir: TVector3): Single;

    procedure SetDistanceToTarget(const AValue: Single);
    procedure SetInitialHeightAboveTarget(const AValue: Single);

    function Camera: TCastleCamera;
  protected

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
    const
      DefaultInitialHeightAboveTarget = 1.0;
      DefaultDistanceToTarget = 4.0;
      DefaultMinDistanceToTarget = 0.5;
      DefaultMaxDistanceToTarget = 10;
      DefaultCameraSpeed = 10;
      { Default value for TCastleNavigation.Radius.
        Matches the default VRML/X3D NavigationInfo.avatarSize[0]. }
      DefaultRadius = 0.25;

    constructor Create(AOwner: TComponent); override;

    { Makes camera be positioned with respect to the current properties and avatar.
      Always call this explicitly once.
      Use this after setting properties like @link(Target),
      @link(DistanceToTarget), @link(InitialHeightAboveTarget).

      TODO:
      At design-time (in CGE editor), this is automatically called after
      changing relevant properties of this navigation. }
    procedure Init;

    function PropertySections(const PropertyName: String): TPropertySections; override;

  published
    property Target: TCastleTransform read FTarget write FTarget;

    { Initial height of camera above the Target.
      Together with DistanceToTarget this determines the initial camera position,
      set by @link(Init).
      It is not used outside of @link(Init). }
    property InitialHeightAboveTarget: Single read FInitialHeightAboveTarget write SetInitialHeightAboveTarget
      {$ifdef FPC}default DefaultInitialHeightAboveTarget{$endif};

    { Preferred distance from camera to the target.
      User can change it with Input_ZoomIn, Input_ZoomOut if you set these inputs
      to some key/mouse button/mouse wheel. }
    property DistanceToTarget: Single read FDistanceToTarget write SetDistanceToTarget
      {$ifdef FPC}default DefaultDistanceToTarget{$endif};

    { Limit of the distance to target, used when changing DistanceToTarget,
      and also when deciding how to adjust camera to avoid collisions.
      @groupBegin }
    property MinDistanceToTarget: Single read FMinDistanceToTarget write FMinDistanceToTarget
      {$ifdef FPC}default DefaultMinDistanceToTarget{$endif};
    property MaxDistanceToTarget: Single read FMaxDistanceToTarget write FMaxDistanceToTarget
      {$ifdef FPC}default DefaultMaxDistanceToTarget{$endif};
    { @groupEnd }

    { The radius of a sphere around the camera
      that makes collisions with the world.

      @unorderedList(
        @item(Collision detection routines use this.)
        @item(It determines the projection near plane (that must be slightly
          smaller than this radius), see also @link(TCastleCamera.ProjectionNear).)
        @item(
          Walk navigation uses this for automatically correcting
          PreferredHeight, otherwise weird things could happen
          if your avatar height is too small compared to the camera radius.
          See @link(CorrectPreferredHeight).

          Especially useful if you let
          user change PreferredHeight at runtime by
          Input_IncreasePreferredHeight, Input_DecreasePreferredHeight.
        )
      ) }
    property Radius: Single read FRadius write FRadius {$ifdef FPC}default DefaultRadius{$endif};

    { Camera position tracks the desired position with given speed (in units per second).
      This makes camera adjust to avatar moving (because of input, or because of gravity
      or other external code) and to not being blocked by the collider. }
    property CameraSpeed: Single read FCameraSpeed write FCameraSpeed
      {$ifdef FPC}default DefaultCameraSpeed{$endif};

    { Immediately (not with delay of CameraSpeed) update camera to never block avatar
      view by a wall, enemy etc. When it is @true, we avoid seeing an invalid geometry
      (e.g. from the wrong side of the wall or inside a creature) @italic(ever),
      but in exchange the camera sometimes has to be adjusted very abrtupty (testcase:
      third_person_navigation demo, stand in the middle of moving enemies, and look around). }
    property ImmediatelyFixBlockedCamera: Boolean read FImmediatelyFixBlockedCamera write FImmediatelyFixBlockedCamera
      default false;

  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TFollowingTargetForCamera -------------------------------------------------- }


constructor TFollowingTargetForCamera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TargetPoint := Vector3(0, 2, 0);

  FMaxDistanceToTarget := DefaultMaxDistanceToTarget;
  FMinDistanceToTarget := DefaultMinDistanceToTarget;
  FDistanceToTarget := DefaultDistanceToTarget;
  FCameraSpeed := DefaultCameraSpeed;
  FInitialHeightAboveTarget := DefaultInitialHeightAboveTarget;
  FRadius :=DefaultRadius;
end;

procedure TFollowingTargetForCamera.Init;
var
  GravUp: TVector3;
  CameraPos, CameraDir, CameraUp, TargetWorldPos: TVector3;
begin
  if (FTarget <> nil) then
  begin
    GravUp := Camera.GravityUp;

    CameraPos := CameraPositionInitial(TargetWorldPos);
    CameraDir := TargetWorldPos - CameraPos;
    if CameraDir.IsZero then
    begin
      { This condition didn't occur in actual tests, this is paranoid check. }
      WritelnWarning('Increase DistanceToAvatarTarget (%f) and/or InitialHeightAboveTarget (%f) to make initial camera further from target', [
        DistanceToTarget,
        InitialHeightAboveTarget
      ]);
      CameraDir := TVector3.One[0];
    end;
    CameraUp := GravUp; // will be adjusted to be orthogonal to Dir by SetView
    FixCameraForCollisions(CameraPos, CameraDir);
    Camera.SetView(CameraPos, CameraDir, CameraUp);
  end;
end;

function TFollowingTargetForCamera.CameraPositionInitial(out TargetWorldPos: TVector3): TVector3;
var
  GravUp: TVector3;
  TargetWorldDir: TVector3;
  HorizontalShiftFromTarget: Single;
begin
  TargetWorldPos := Target.WorldTransform.MultPoint(TargetPoint);
  TargetWorldDir := Target.WorldTransform.MultDirection(TCastleTransform.DefaultDirection[Target.Orientation]);

  if DistanceToTarget < InitialHeightAboveTarget then
  begin
    WritelnWarning('DistanceToTarget (%f) should not be smaller than InitialHeightAboveTarget (%f)', [
      DistanceToTarget,
      InitialHeightAboveTarget
    ]);
    // This effectively assumes that DistanceToAvatarTarget = InitialHeightAboveTarget
    HorizontalShiftFromTarget := 0;
  end else
  begin
    { InitialHeightAboveTarget, HorizontalShiftFromTarget, DistanceToAvatarTarget
      create a right triangle, so
      InitialHeightAboveTarget^2 + HorizontalShiftFromTarget^2 = DistanceToAvatarTarget^2
    }
    HorizontalShiftFromTarget := Sqrt(Sqr(DistanceToTarget) - Sqr(InitialHeightAboveTarget));
  end;

  GravUp := Camera.GravityUp;

  Result := TargetWorldPos
    + GravUp * InitialHeightAboveTarget
    - ToGravityPlane(TargetWorldDir, GravUp) * HorizontalShiftFromTarget;
end;

procedure TFollowingTargetForCamera.FixCameraForCollisions(
  var CameraPos: TVector3; const CameraDir: TVector3);
var
  MaxDistance: Single;
  TargetWorldPos: TVector3;
begin
  if (Target <> nil) then
  begin
    TargetWorldPos := Target.WorldTransform.MultPoint(TargetPoint);
    MaxDistance := CameraMaxDistanceToTarget(TargetWorldPos, CameraDir);
    if PointsDistanceSqr(CameraPos, TargetWorldPos) > Sqr(MaxDistance) then
      // Note that CameraDir is not necessarily normalized now
      CameraPos := TargetWorldPos - CameraDir.AdjustToLength(MaxDistance);
  end;
end;

function TFollowingTargetForCamera.ToGravityPlane(const V: TVector3;
  const GravUp: TVector3): TVector3;
begin
  Result := V;
  if not VectorsParallel(Result, GravUp) then
    MakeVectorsOrthoOnTheirPlane(Result, GravUp);
end;

function TFollowingTargetForCamera.CameraMaxDistanceToTarget(
  const CameraLookPos: TVector3; const CameraDir: TVector3): Single;
var
  CollisionDistance: Single;
  SavedAPickable: Boolean;
begin
  Result := MaxSingle;
  SavedAPickable := Target.Pickable;
  Target.Pickable := false;
  try
    if Target.World.WorldRayCast(CameraLookPos, -CameraDir, CollisionDistance) <> nil then
    begin
      { Use MinDistanceToAvatarTarget to secure in case wall is closer than Radius
        (CollisionDistance - Radius negative)
        or just to close to head.
        Then use MinDistanceToAvatarTarget. }
      Result := Max(MinDistanceToTarget, CollisionDistance - Radius);
    end;
  finally Target.Pickable := SavedAPickable end;
end;

procedure TFollowingTargetForCamera.SetDistanceToTarget(const AValue: Single);
begin
  if FDistanceToTarget <> AValue then
    FDistanceToTarget := AValue;
end;

procedure TFollowingTargetForCamera.SetInitialHeightAboveTarget(
  const AValue: Single);
begin
  if FInitialHeightAboveTarget <> AValue then
    FInitialHeightAboveTarget := AValue;
end;

function TFollowingTargetForCamera.Camera: TCastleCamera;
begin
  if Parent is TCastleCamera then
    Result := TCastleCamera(Parent)
  else
    Result := nil;
end;

procedure TFollowingTargetForCamera.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);

  { Make camera follow the Target.Translation.
    Following the character also makes sure that camera stays updated
    (keeps DistanceToAvatarTarget)
    when the Target is being moved by other routines (e.g. because is working).

    Also avoid camera being blocked by some wall.
    This needs to be redone, in case some wall blocks us e.g. because of collisions.

    Does not follow the perfect location instantly,
    which makes a nice effect when character is moving fast.
    It's inportant to avoid sudden camera moves on sudden avatar moves,
    e.g. changing Y when going up/down stairs. }
  procedure UpdateCamera;
  var
    TargetWorldPos, CameraPos, CameraDir, CameraUp, CameraPosTarget, CameraDirToTarget: TVector3;
    MaxDistance: Single;
  begin
    TargetWorldPos := Target.WorldTransform.MultPoint(TargetPoint);
    WritelnLog('TargetWorldPos ' + TargetWorldPos.ToString);

    Camera.GetView(CameraPos, CameraDir, CameraUp);

    WritelnLog('Old Camera position ' + CameraPos.ToString);

    { We use CameraDirToTarget, not CameraDir, because (since we update with delay)
      camera may look at a slightly shifted point.
      But we still adjust camera position to look (without blockers) at avatar. }
    CameraDirToTarget := TargetWorldPos - CameraPos;

    { We need to check both CameraPosTarget and final CameraPos for collisions.
      But it would be wasteful to call FixCameraForCollisions 2 times,
      to calculate mostly the same.
      So we use one call to CameraMaxDistanceToTarget. }
    MaxDistance := CameraMaxDistanceToTarget(TargetWorldPos, CameraDirToTarget);
    WritelnLog('MAxDistance ' + FloatToStr(MaxDistance));

    { No need to use CameraDir.AdjustToLength(xxx) as we know CameraDir is normalized.
      Note that this is not entirely correct: we use distance we obtained with CameraDirToTarget,
      but our desired camera direction is CameraDir (i.e. unchanged from current camera direction). }
    CameraPosTarget := TargetWorldPos - CameraDir * Min(MaxDistance, DistanceToTarget);

    WritelnLog('CameraPosTarget ' + CameraPosTarget.ToString);

    CameraPos := SmoothTowards(CameraPos, CameraPosTarget, SecondsPassed, CameraSpeed);
    if ImmediatelyFixBlockedCamera then
    begin
      if PointsDistanceSqr(CameraPos, TargetWorldPos) > Sqr(MaxDistance) then
        CameraPos := TargetWorldPos - CameraDir * MaxDistance;
    end;

    Camera.SetView(CameraPos, CameraDir, CameraUp);
    WritelnLog('New Camera position ' + CameraPos.ToString);
  end;

begin
  if (Target = nil) or (CastleApplicationMode <> appRunning) then
    Exit;

  if not FInitialised then
  begin
    Init;
    FInitialised := true;
  end;w

  UpdateCamera;
  inherited Update(SecondsPassed, RemoveMe);
end;

function TFollowingTargetForCamera.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'IdleAnimation', 'WalkAnimation', 'FlyAnimation', 'JumpAnimation',
      'FallAnimation'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TFollowingTargetForCamera, ['Navigation', 'Camera', 'Follow the target']);

end.

