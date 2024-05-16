{
  Copyright 2023-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Behavior that makes parent follow the target (@link(TFollowingTargetForCamera)). }
unit FollowingTargetForCamera;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform, CastleBehaviors,
  CastleVectors, CastleClassUtils, CastleScene, CastleInputAxis;

type
  { Behavior that makes parent follow the target. }
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
    FMouseLookSensitivity: Single;
    FZoomEnabled: Boolean;
    FZoomInputAxis: TCastleInputAxis;
    FCameraDistanceChangeSpeed: Single;

    function CameraPositionInitial(out TargetWorldPos: TVector3): TVector3;

    { Update camera, to avoid having something collidable between camera position and Target.
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

    procedure ProcessMouseLookDelta(const Delta: TVector2);
    function Zoom(const Factor: Single): Boolean;
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
      DefaultMouseLookSensitivity = 0.01;
      DefaultCameraDistanceChangeSpeed = 1;

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

    { Zooming makes camera move closer/further from target. }
    property ZoomEnabled: Boolean read FZoomEnabled write FZoomEnabled default true;

    property MouseLookSensitivity: Single read FMouseLookSensitivity write FMouseLookSensitivity
      {$ifdef FPC}default DefaultMouseLookSensitivity{$endif};

    { Bring the camera closer or further from the target. Works only if
      @link(ZoomEnabled). By deafult (mouse wheel up/down). }
    property ZoomInputAxis: TCastleInputAxis read FZoomInputAxis;

    { Speed with which InputZoomIn, InputZoomOut can change DistanceToTarget. }
    property CameraDistanceChangeSpeed: Single read FCameraDistanceChangeSpeed write FCameraDistanceChangeSpeed
      {$ifdef FPC}default DefaultCameraDistanceChangeSpeed{$endif};
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
  FRadius := DefaultRadius;
  FMouseLookSensitivity := DefaultMouseLookSensitivity;
  FZoomEnabled := true;
  FCameraDistanceChangeSpeed := DefaultCameraDistanceChangeSpeed;

  FZoomInputAxis := TCastleInputAxis.Create(Self);
  FZoomInputAxis.PositiveWheelDirection := mwUp;
  FZoomInputAxis.NegativeWheelDirection := mwDown;
  FZoomInputAxis.SetSubComponent(true);
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

procedure TFollowingTargetForCamera.ProcessMouseLookDelta(const Delta: TVector2);
var
  ToCamera, GravUp: TVector3;

  { Change ToCamera by applying DeltaY from mouse look. }
  procedure ProcessVertical(DeltaY: Single);
  const
    { Do not allow to look exactly up or exactly down,
      as then further vertical moves would be undefined,
      so you would not be able to "get out" of such rotation. }
    MinAngleFromZenith = 0.1;
  var
    Side: TVector3;
    AngleToUp, AngleToDown, MaxChange: Single;
  begin
    Side := -TVector3.CrossProduct(ToCamera, GravUp);
    if DeltaY > 0 then
    begin
      AngleToDown := AngleRadBetweenVectors(ToCamera, -GravUp);
      MaxChange := Max(0, AngleToDown - MinAngleFromZenith);
      if DeltaY > MaxChange then
        DeltaY := MaxChange;
    end else
    begin
      AngleToUp := AngleRadBetweenVectors(ToCamera, GravUp);
      MaxChange := Max(0, AngleToUp - MinAngleFromZenith);
      if DeltaY < -MaxChange then
        DeltaY := -MaxChange;
    end;
    ToCamera := RotatePointAroundAxisRad(DeltaY * MouseLookSensitivity, ToCamera, Side);
  end;

  procedure ProcessHorizontal(const DeltaX: Single);
  begin
    ToCamera := RotatePointAroundAxisRad(-DeltaX * MouseLookSensitivity, ToCamera, GravUp);
  end;

var
  CameraPos, CameraDir, CameraUp, TargetWorldPos, LookPos: TVector3;
begin
  inherited;

  if (Target <> nil) then
  begin
    Camera.GetWorldView(CameraPos, CameraDir, CameraUp);
    GravUp := Camera.GravityUp;

    TargetWorldPos := Target.WorldTransform.MultPoint(TargetPoint);
    // Since camera may update with some delay, we may not look exactly at TargetWorldPos if avatar moved
    LookPos := PointOnLineClosestToPoint(CameraPos, CameraDir, TargetWorldPos);

    ToCamera := CameraPos - LookPos;
    if ToCamera.IsZero then
    begin
      WritelnWarning('TFollowingTargetForCamera camera position at look target, increase DistanceToAvatarTarget');
      Exit;
    end;

    ProcessVertical(Delta[1]);
    ProcessHorizontal(Delta[0]);

    CameraPos := LookPos + ToCamera;
    CameraDir := LookPos - CameraPos;
    CameraUp := GravUp; // will be adjusted to be orthogonal to Dir by SetWorldView
    if ImmediatelyFixBlockedCamera then
      FixCameraForCollisions(CameraPos, CameraDir);
    Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
  end;
end;

function TFollowingTargetForCamera.Zoom(const Factor: Single): Boolean;

  procedure CameraDistanceChange(DistanceChange: Single);
  begin
    DistanceChange := DistanceChange * CameraDistanceChangeSpeed;
    DistanceToTarget := Clamped(DistanceToTarget + DistanceChange,
      MinDistanceToTarget, MaxDistanceToTarget);

    { The actual change in Camera.Position, caused by changing DistanceToAvatarTarget,
      will be done smoothly in UpdateCamera. }
  end;

begin
  Result := false;
  //if not Valid then Exit;
  if Target <> nil then
  begin
    CameraDistanceChange(-Factor);
    Result := true;
  end;
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

    Camera.GetView(CameraPos, CameraDir, CameraUp);

    { We use CameraDirToTarget, not CameraDir, because (since we update with delay)
      camera may look at a slightly shifted point.
      But we still adjust camera position to look (without blockers) at avatar. }
    CameraDirToTarget := TargetWorldPos - CameraPos;

    { We need to check both CameraPosTarget and final CameraPos for collisions.
      But it would be wasteful to call FixCameraForCollisions 2 times,
      to calculate mostly the same.
      So we use one call to CameraMaxDistanceToTarget. }
    MaxDistance := CameraMaxDistanceToTarget(TargetWorldPos, CameraDirToTarget);

    { No need to use CameraDir.AdjustToLength(xxx) as we know CameraDir is normalized.
      Note that this is not entirely correct: we use distance we obtained with CameraDirToTarget,
      but our desired camera direction is CameraDir (i.e. unchanged from current camera direction). }
    CameraPosTarget := TargetWorldPos - CameraDir * Min(MaxDistance, DistanceToTarget);

    CameraPos := SmoothTowards(CameraPos, CameraPosTarget, SecondsPassed, CameraSpeed);
    if ImmediatelyFixBlockedCamera then
    begin
      if PointsDistanceSqr(CameraPos, TargetWorldPos) > Sqr(MaxDistance) then
        CameraPos := TargetWorldPos - CameraDir * MaxDistance;
    end;

    Camera.SetView(CameraPos, CameraDir, CameraUp);
  end;

begin
  if (Target = nil) or (Camera = nil) or (CastleApplicationMode <> appRunning) then
    Exit;

  if not FInitialised then
  begin
    Init;
    FInitialised := true;
  end;

  if not FocusedContainer.MouseLookLastDelta.IsPerfectlyZero then
  begin
    ProcessMouseLookDelta(FocusedContainer.MouseLookLastDelta);
  end;

  if ZoomEnabled then
    Zoom(ZoomInputAxis.Value(FocusedContainer));

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

