{%MainUnit castletransform.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Physics joints. }

{$ifdef read_interface}
  { Base class for physics joints, that connects two physics bodies
    and constraints their relative movement.

    This class provides useful virtual methods to create joints
    (observing the existence of rigid body and parent) and some common
    properties to all joints (allowing all joints to be breakable). }
  TCastleAbstractJoint = class(TCastleBehavior)
  protected
    { When joint is added to world we need to find parent rigid body and set
      callbacks for its initialize/deinitialize events. If this pointer is not nil
      that means that we've done that. }
    FParentRigidBody: TCastleRigidBody;
    FBreakThresholdForce: Single;
    FBreakThresholdTorque: Single;
    FBreakable: Boolean;

    { Create physics engine (Kraft) joint and set its specialized properties. }
    procedure CreateKraftJoint; virtual; abstract;

    { Get physics engine (Kraft) joint instance. }
    function GetKraftJoint: TKraftConstraintJoint; virtual; abstract;

    procedure RecreateKraftJoint;
    procedure InitializeKraftJoint; virtual;
    procedure DeinitializeKraftJoint; virtual; abstract;

    { Can we create joint object. }
    function AllNeededKraftObjectsInitialized: Boolean; virtual;

    { Event callback when TCastleRigidBody is added to the parent transform. }
    procedure OnParentTransformAddBehavior(const Component: TComponent); virtual;

    { Event callback when TCastleRigidBody is removed from the parent transform. }
    procedure OnBeforeParentTransfromRemoveBehavior(const Component: TComponent); virtual;

    { Event callback when TCastleRigidBody behavior is initialized. }
    procedure OnParentRigidBodyInitialized(const Component: TComponent); virtual;

    { Event callback when TCastleRigidBody behavior is deinitialized. }
    procedure OnBeforeParentRigidBodyDeinitialized(const Component: TComponent); virtual;

    { Sets TCastleRigidBody behavior initialize/deinitialize callbacks. }
    procedure SetParentRigidBodyEvents;

    { Removes TCastleRigidBody behavior initialize/deinitialize callbacks. }
    procedure RemoveParentRigidBodyEvents;

    procedure UpdateBreakThresholdForceInKraft;
    procedure UpdateBreakThresholdTorqueInKraft;
    procedure UpdateBreakableInKraft;

    procedure SetBreakThresholdForce(const AValue: Single);
    procedure SetBreakThresholdTorque(const AValue: Single);
    procedure SetBreakable(const AValue: Boolean);
  public
    destructor Destroy; override;
    procedure WorldAfterAttach; override;
    procedure WorldBeforeDetach; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Create design-time transformations that are displayed in the hierarchy
      to visualize physics joints 3D points.
      If you want to visualize the behavior using a visible TCastleTransform,
      then create and add to Parent a descendant of TCastleJointTool,
      and set InternalCastleDesignInvalidate to @true. }
    procedure InternalCreateGizmos; virtual;

    { Destroy design-time stuff.
      You can destroy any component, just be sure
      to set InternalCastleDesignInvalidate to @true. }
    procedure InternalDestroyGizmos; virtual;
  published
    { Makes the joint breakable when force is greater than BreakThresholdForce
      or torque is greater than BreakThresholdTorque. }
    property Breakable: Boolean read FBreakable write SetBreakable default false;

    { Force threshold to break joint. }
    property BreakThresholdForce: Single read FBreakThresholdForce write SetBreakThresholdForce {$ifdef FPC}default 0.0{$endif};

    { Torque threshold to break joint. }
    property BreakThresholdTorque: Single read FBreakThresholdTorque write SetBreakThresholdTorque {$ifdef FPC}default 0.0{$endif};
  end;

  { Abstract class for joints that use only one TCastleTransform. }
  TCastleAbstractOneBodyJoint = class(TCastleAbstractJoint)
  end;

  { Abstract class for joints that connect two TCastleTransform instances. }
  TCastleAbstractTwoBodiesJoint = class(TCastleAbstractJoint)
  private
    { TCastleAbstractTwoBodiesJoint means that joint connects two transforms so this is
      the second one transform pointer }
    FConnected: TCastleTransform;
    { When connected transform is assigned we need found second rigid body and set
      callbacks for its initialize/deinitialize events. If this pointer is not nil
      that means that we done that }
    FConnectedRigidBody: TCastleRigidBody;
    { Stores valu should connected rigid bodies collide or not }
    FConnectedCollide: Boolean;
    { We need observe second connected transform and remove it form
      FConnected when second transform is freed }
    FConnectedTransfromFreeObserver: TFreeNotificationObserver;
  protected
    procedure InitializeKraftJoint; override;
    procedure SetConnected(const AConnected: TCastleTransform); virtual;
    procedure SetConnectedCollide(const AValue: Boolean); virtual;

    function AllNeededKraftObjectsInitialized: Boolean; override;

    procedure OnConnectedFree(const Sender: TFreeNotificationObserver); virtual;

    { Event callback when TCastleRigidBody is added to the connected transform. }
    procedure OnConnectedAddBehavior(const Component: TComponent); virtual;

    { Event callback when TCastleRigidBody is removed from the connected transform. }
    procedure OnConnectedTransfromRemoveBehavior(const Component: TComponent); virtual;

    { Event callback when connected transform TCastleRigidBody behavior is initialized. }
    procedure OnConnectedRigidBodyInitialized(const Component: TComponent); virtual;

    { Event callback when connected transform TCastleRigidBody behavior is deinitialized. }
    procedure OnBeforeConnectedRigidBodyDeinitialized(const Component: TComponent); virtual;

    { Sets connected transform add/remove behavior callbacks and free
      notification observer. }
    procedure SetConnectedEvents;

    { Removes connected transform add/remmove behavior callbacks and free
      notification observer. }
    procedure RemoveConnectedEvents;

    { Sets connected TCastleRigidBody behavior initialize/deinitialize callbacks. }
    procedure SetConnectedRigidBodyEvents;

    { Removes connected TCastleRigidBody behavior initialize/deinitialize
      callbacks. }
    procedure RemoveConnectedRigidBodyEvents;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { We need to check if the connected castle transform is not
      the parent node and we need to change the connected node to nil then. }
    procedure ParentAfterAttach; override;
  published
    { Second transform of the physics joint.
      Note that it can't be the Parent of this joint. }
    property Connected: TCastleTransform read FConnected
      write SetConnected;

    { Should connected transforms (Connected and Parent) collide. }
    property ConnectedCollide: Boolean read FConnectedCollide
      write SetConnectedCollide default false;
  end;

  EConnectedSameAsParent = class (Exception);

  { Hinge joint allows to rotate the transformation around a given axis,
    like a door attached using hinges to the frame.
    @link(Anchor) determines the rotation point and @link(Axis) the axis direction. }
  TCastleHingeJoint = class(TCastleAbstractTwoBodiesJoint)
  private
    FKraftJointHinge: TKraftConstraintJointHinge;
    FAxis: TVector3;
    FAnchor: TVector3;
    FAnchorTransform: TCastleTransform;
    FMinAngle: Single;
    FMaxAngle: Single;
    FAngleLimits: Boolean;
    function GetAnchorForTool: TVector3;
    procedure SetAnchor(const AValue: TVector3);
    procedure SetAxis(const AValue: TVector3);
    procedure SetAngleLimits(const AValue: Boolean);
    procedure SetMinAngle(const AValue: Single);
    procedure SetMaxAngle(const AValue: Single);
    procedure InternalUpdateGizmosName;
  protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;
    procedure InternalCreateGizmos; override;
    procedure InternalDestroyGizmos; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Position of the hinge in the parent transformation coordinates. }
    property Anchor: TVector3 read FAnchor write SetAnchor;

    { Rotation axis around which the @link(Connected) rotates.
      By default +Y. }
    property Axis: TVector3 read FAxis write SetAxis;
  published
    { Turns on minimum and maximum rotation angle limit. }
    property AngleLimits: Boolean read FAngleLimits write SetAngleLimits default false;

    { Minimal angle, in radians, must be a value between -2pi + epsilon to 0.
      Honored only if @link(AngleLimits). }
    property MinAngle: Single read FMinAngle write SetMinAngle {$ifdef FPC}default 0.0{$endif};

    { Maximum angle, in radians, must be a value between 0 to 2pi - epsilon.
      Honored only if @link(AngleLimits). }
    property MaxAngle: Single read FMaxAngle write SetMaxAngle {$ifdef FPC}default 0.0{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlehingejoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Rope joint connects two bodies by an invisible rope that prevents
    the distance between them to grow beyond specfied @link(Distance).
    @link(Anchor) and @link(ConnectedAnchor) define the hook points of the rope. }
  TCastleRopeJoint = class(TCastleAbstractTwoBodiesJoint)
  private
    FKraftJointRope: TKraftConstraintJointRope;
    FAnchor: TVector3;
    FConnectedAnchor: TVector3;
    FDistance: Single;
    FAnchorTransform: TCastleTransform;
    FConnectedAnchorTransform: TCastleTransform;
    function GetAnchorForTool: TVector3;
    procedure SetAnchor(const AValue: TVector3);
    function GetConnectedAnchorForTool: TVector3;
    procedure SetConnectedAnchor(const AValue: TVector3);
    procedure SetDistance(const AValue: Single);
    procedure InternalUpdateGizmosName;
  protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;

    procedure OnConnectedFree(const Sender: TFreeNotificationObserver); override;
  protected
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;
    procedure InternalCreateGizmos; override;
    procedure InternalDestroyGizmos; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { First hook point that is located in parent transform local coordinates. }
    property Anchor: TVector3 read FAnchor write SetAnchor;

    { Second hook point that is located in Connected local coordinates. }
    property ConnectedAnchor: TVector3 read FConnectedAnchor
      write SetConnectedAnchor;
  published
    { Maximum rope length (between Anchor and ConnectedAnchor) }
    property Distance: Single read FDistance write SetDistance {$ifdef FPC}default 0.0{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleropejoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Ball joint allows to freely rotate the transformation of one object relative to another
    (rotate in any axis) but does not allow any movement.
    @link(Anchor) determines the rotation point. }
  TCastleBallJoint = class(TCastleAbstractTwoBodiesJoint)
  private
    FKraftJointBall: TKraftConstraintJointBallSocket;
    FAnchor: TVector3;
    FAnchorTransform: TCastleTransform;
    function GetAnchorForTool: TVector3;
    procedure SetAnchor(const AValue: TVector3);
    procedure InternalUpdateGizmosName;
  protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;
    procedure InternalCreateGizmos; override;
    procedure InternalDestroyGizmos; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { The rotation point of the ball joint in parent transform coordinates. }
    property Anchor: TVector3 read FAnchor write SetAnchor;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleballjoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Distance joint tries to maintain a certain distance between two rigid bodies,
    with certain flexibility (like a spring).
    @link(Anchor) and @link(ConnectedAnchor) actually determine the points between
    which the distance is observed. }
  TCastleDistanceJoint = class(TCastleAbstractTwoBodiesJoint)
  private
    FKraftJointDistance: TKraftConstraintJointDistance;
    FAnchor: TVector3;
    FConnectedAnchor: TVector3;
    FFrequencyHz: Single;
    FDampingRatio: Single;
    FAnchorTransform: TCastleTransform;
    FConnectedAnchorTransform: TCastleTransform;

    function GetAnchorForTool: TVector3;
    procedure SetAnchor(const AValue: TVector3);
    function GetConnectedAnchorForTool: TVector3;
    procedure SetConnectedAnchor(const AValue: TVector3);
    procedure SetFrequencyHz(const AValue: Single);
    procedure SetDampingRatio(const AValue: Single);
    procedure InternalUpdateGizmosName;
  protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;
    procedure InternalCreateGizmos; override;
    procedure InternalDestroyGizmos; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { First point from which the distance is measured. }
    property Anchor: TVector3 read FAnchor write SetAnchor;

    { Second point from which the distance is measured. }
    property ConnectedAnchor: TVector3 read FConnectedAnchor
      write SetConnectedAnchor;
  published
    { Frequency controls how fast the oscillations occur. The smaller the value,
      the longer the oscillation movement. This may not seem intuitive,
      but a higher value causes the oscillation in specified time to be faster
      and therefore the movement is shorter.

      So if you want longer movement make this value smaller.

      See live example: https://gamedev.stackexchange.com/questions/98772/b2distancejoint-with-frequency-equal-to-0-vs-b2weldjoint }
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz {$ifdef FPC}default 0.0{$endif};

    { How fast the oscillations are damped, large values can cause the body
      to not do even one oscillation. }
    property DampingRatio: Single read FDampingRatio write SetDampingRatio {$ifdef FPC}default 0.0{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastledistancejoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Grab joint pulls the rigid body @link(Anchor) towards a @link(TargetWorld) defined
    in the world coordinates.
    The pull is not instant, rather it works like a flexible spring / elastic rope. }
  TCastleGrabJoint = class(TCastleAbstractOneBodyJoint)
  private
    FDampingRatio: Single;
    FFrequencyHz: Single;
    FKraftJointGrab: TKraftConstraintJointGrab;
    FMaxForce: Single;
    FAnchor, FTargetWorld: TVector3;
    FAnchorTransform: TCastleTransform;
    FTargetWorldTransform: TCastleTransform;

    function GetAnchorForTool: TVector3;
    procedure SetAnchor(const AValue: TVector3);
    function GetTargetWorldForTool: TVector3;
    procedure SetTargetWorld(const AValue: TVector3);
    procedure SetFrequencyHz(const AValue: Single);
    procedure SetDampingRatio(const AValue: Single);
    procedure SetMaxForce(const AValue: Single);
    procedure InternalUpdateGizmosName;
    { Returns MaxSingle when MaxForce is zero. }
    function MaxForceNonZero: Single;
  protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    const
      DefaultFrequencyHz = 5.0;
      DefaultDampingRatio = 0.7;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;
    procedure InternalCreateGizmos; override;
    procedure InternalDestroyGizmos; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Point, in local coordinates, that will be pulled to @link(TargetWorld). }
    property Anchor: TVector3 read FAnchor write SetAnchor;

    { Point, in world coordinates, that pulls @link(Anchor).
      With time, @link(Anchor) (in local coords) will match @link(TargetWorld) (in world coords). }
    property TargetWorld: TVector3 read FTargetWorld write SetTargetWorld;
  published
    { Frequency controls how fast the oscillations occur. The smaller the value,
      the longer the oscillation movement. This may not seem intuitive,
      but a higher value causes the oscillation in specified time to be faster
      and therefore the movement is shorter.

      So if you want longer movement make this value smaller.

      See live example: https://gamedev.stackexchange.com/questions/98772/b2distancejoint-with-frequency-equal-to-0-vs-b2weldjoint }
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz {$ifdef FPC}default DefaultFrequencyHz{$endif};

    { How fast the oscillations are damped, large values can cause the body
      will not do even one oscillation. }
    property DampingRatio: Single read FDampingRatio write SetDampingRatio {$ifdef FPC}default DefaultDampingRatio{$endif};

    { Maximal force that will be aplied to the body to move it.
      Zero (default) means "unlimited". }
    property MaxForce: Single read FMaxForce write SetMaxForce {$ifdef FPC}default 0.0{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlegrabjoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleAbstractTwoBodiesJoint ---------------------------------------------------- }

procedure TCastleAbstractTwoBodiesJoint.SetConnectedCollide(const AValue: Boolean);
begin
  if FConnectedCollide = AValue then
    Exit;
  FConnectedCollide := AValue;
  RecreateKraftJoint;
end;

function TCastleAbstractTwoBodiesJoint.AllNeededKraftObjectsInitialized: Boolean;
begin
  Result := (inherited AllNeededKraftObjectsInitialized) and
    (FConnectedRigidBody <> nil) and (FConnectedRigidBody.IsInitialized);
end;

procedure TCastleAbstractTwoBodiesJoint.OnConnectedFree(
  const Sender: TFreeNotificationObserver);
begin
  DeinitializeKraftJoint;
  RemoveConnectedRigidBodyEvents;
  RemoveConnectedEvents;
  FConnectedRigidBody := nil;
  FConnected := nil;
end;

procedure TCastleAbstractTwoBodiesJoint.OnConnectedAddBehavior(
  const Component: TComponent);
begin
  FConnectedRigidBody := Component as TCastleRigidBody;
  SetConnectedRigidBodyEvents;
end;

procedure TCastleAbstractTwoBodiesJoint.OnConnectedTransfromRemoveBehavior(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
  RemoveConnectedRigidBodyEvents;
  FConnectedRigidBody := nil;
end;

procedure TCastleAbstractTwoBodiesJoint.OnConnectedRigidBodyInitialized(
  const Component: TComponent);
begin
  RecreateKraftJoint;
end;

procedure TCastleAbstractTwoBodiesJoint.OnBeforeConnectedRigidBodyDeinitialized(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
end;

procedure TCastleAbstractTwoBodiesJoint.SetConnectedEvents;
begin
  if FConnected = nil then
    Exit;

  FConnected.AddNotificationOnBehaviorAdded(
    {$ifdef FPC}@{$endif} OnConnectedAddBehavior, TCastleRigidBody);

  FConnected.AddNotificationBeforeBehaviorRemove(
    {$ifdef FPC}@{$endif} OnConnectedTransfromRemoveBehavior, TCastleRigidBody);

  if FConnectedTransfromFreeObserver = nil then
  begin
    FConnectedTransfromFreeObserver := TFreeNotificationObserver.Create(Self);
    FConnectedTransfromFreeObserver.OnFreeNotification := {$ifdef FPC}@{$endif}OnConnectedFree;
  end;

  FConnectedTransfromFreeObserver.Observed := FConnected;
end;

procedure TCastleAbstractTwoBodiesJoint.RemoveConnectedEvents;
begin
  if FConnected <> nil then
  begin
    { Remove events from previous connected castle transform }
    FConnected.AddNotificationOnBehaviorAdded(
      {$ifdef FPC}@{$endif} OnConnectedAddBehavior, TCastleRigidBody);

    FConnected.AddNotificationBeforeBehaviorRemove(
      {$ifdef FPC}@{$endif} OnConnectedTransfromRemoveBehavior, TCastleRigidBody);
  end;

  { Stop watching this transform }
  if FConnectedTransfromFreeObserver <> nil then
    FConnectedTransfromFreeObserver.Observed := nil;
end;

procedure TCastleAbstractTwoBodiesJoint.SetConnectedRigidBodyEvents;
begin
  if FConnectedRigidBody <> nil then
  begin
    FConnectedRigidBody.AddNotificationOnInitialized({$ifdef FPC}@{$endif} OnConnectedRigidBodyInitialized);
    FConnectedRigidBody.AddNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeConnectedRigidBodyDeinitialized);
  end;
end;

procedure TCastleAbstractTwoBodiesJoint.RemoveConnectedRigidBodyEvents;
begin
  if FConnectedRigidBody <> nil then
  begin
    FConnectedRigidBody.RemoveNotificationOnInitialized({$ifdef FPC}@{$endif} OnConnectedRigidBodyInitialized);
    FConnectedRigidBody.RemoveNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeConnectedRigidBodyDeinitialized);
  end;
end;

function TCastleAbstractTwoBodiesJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'Connected', 'ConnectedCollide'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleAbstractTwoBodiesJoint.ParentAfterAttach;
begin
  { Remove connected transform if new parent is the same transform }
  if Parent = FConnected then
  begin
    RemoveConnectedRigidBodyEvents;
    FConnectedRigidBody := nil;
    RemoveConnectedEvents;
    FConnected := nil;

    raise Exception.Create('Connected is the same as Parent of TCastleAbstractTwoBodiesJoint, this is not allowed');
    // WritelnWarning...
  end;
end;

procedure TCastleAbstractTwoBodiesJoint.InitializeKraftJoint;
begin
  inherited InitializeKraftJoint;

  // make sure connected rigid body is active after joint creation
  FConnectedRigidBody.WakeUp;
end;

procedure TCastleAbstractTwoBodiesJoint.SetConnected(
  const AConnected: TCastleTransform);
begin
  if (AConnected <> nil) and (AConnected = Parent) then
    raise EConnectedSameAsParent.Create(
      'Can''t set Connected to behavior Parent.');

  if FConnected <> AConnected then
  begin
    if FConnected <> nil then
    begin
      { Remove events from previous connected castle transform }
      RemoveConnectedEvents;

      { Remove events from Rigidbody of old connected transform }
      RemoveConnectedRigidBodyEvents;
    end;

    FConnected := AConnected;
    if FConnected <> nil then
    begin
      { If FConnected is not nil set all events and try
        recreate joint }
      SetConnectedEvents;

      FConnectedRigidBody := FConnected.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
      if FConnectedRigidBody <> nil then
      begin
        SetConnectedRigidBodyEvents;
        RecreateKraftJoint;
      end;
    end;
  end;
end;

{ TCastleAbstractJoint ------------------------------------------------------------- }

destructor TCastleAbstractJoint.Destroy;
begin
  DeinitializeKraftJoint;
  inherited Destroy;
end;

procedure TCastleAbstractJoint.RecreateKraftJoint;
begin
  DeinitializeKraftJoint;
  if AllNeededKraftObjectsInitialized then
    InitializeKraftJoint;
end;

procedure TCastleAbstractJoint.InitializeKraftJoint;
begin
  CreateKraftJoint;
  // make sure rigid body is active after joint creation
  FParentRigidBody.WakeUp;
  UpdateBreakableInKraft;
  UpdateBreakThresholdForceInKraft;
  UpdateBreakThresholdTorqueInKraft;
end;

function TCastleAbstractJoint.AllNeededKraftObjectsInitialized: Boolean;
begin
  Result := (FParentRigidBody <> nil) and FParentRigidBody.IsInitialized;
end;

procedure TCastleAbstractJoint.OnParentTransformAddBehavior(
  const Component: TComponent);
begin
  FParentRigidBody := Component as TCastleRigidBody;
  SetParentRigidBodyEvents;
end;

procedure TCastleAbstractJoint.OnBeforeParentTransfromRemoveBehavior(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
  RemoveParentRigidBodyEvents;
  FParentRigidBody := nil;
end;

procedure TCastleAbstractJoint.OnParentRigidBodyInitialized(
  const Component: TComponent);
begin
  RecreateKraftJoint;
end;

procedure TCastleAbstractJoint.OnBeforeParentRigidBodyDeinitialized(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
end;

procedure TCastleAbstractJoint.SetParentRigidBodyEvents;
begin
  if FParentRigidBody <> nil then
  begin
    FParentRigidBody.AddNotificationOnInitialized({$ifdef FPC}@{$endif} OnParentRigidBodyInitialized);
    FParentRigidBody.AddNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeParentRigidBodyDeinitialized);
  end;
end;

procedure TCastleAbstractJoint.RemoveParentRigidBodyEvents;
begin
  if FParentRigidBody <> nil then
  begin
    FParentRigidBody.RemoveNotificationOnInitialized({$ifdef FPC}@{$endif} OnParentRigidBodyInitialized);
    FParentRigidBody.RemoveNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeParentRigidBodyDeinitialized);
  end;
end;

procedure TCastleAbstractJoint.UpdateBreakThresholdForceInKraft;
begin
  if GetKraftJoint <> nil then
    GetKraftJoint.BreakThresholdForce := FBreakThresholdForce;
end;

procedure TCastleAbstractJoint.UpdateBreakThresholdTorqueInKraft;
begin
  if GetKraftJoint <> nil then
    GetKraftJoint.BreakThresholdTorque := FBreakThresholdTorque;
end;

procedure TCastleAbstractJoint.UpdateBreakableInKraft;
begin
  if GetKraftJoint <> nil then
  begin
    if FBreakable then
      GetKraftJoint.Flags := GetKraftJoint.Flags + [kcfBreakable]
    else
      GetKraftJoint.Flags := GetKraftJoint.Flags - [kcfBreakable];
  end;
end;

procedure TCastleAbstractJoint.SetBreakThresholdForce(const AValue: Single);
begin
  if FBreakThresholdForce <> AValue then
  begin
    FBreakThresholdForce := AValue;
    UpdateBreakThresholdForceInKraft;
  end;
end;

procedure TCastleAbstractJoint.SetBreakThresholdTorque(const AValue: Single);
begin
  if FBreakThresholdTorque <> AValue then
  begin
    FBreakThresholdTorque := AValue;
    UpdateBreakThresholdTorqueInKraft;
  end;
end;

procedure TCastleAbstractJoint.SetBreakable(const AValue: Boolean);
begin
  if FBreakable <> AValue then
  begin
    FBreakable := AValue;
    UpdateBreakableInKraft;
  end;
end;

procedure TCastleAbstractJoint.WorldAfterAttach;
begin
  inherited WorldAfterAttach;

  { This means we are added to world so we need to get parent and check
    Parent has rigid body if has we should try recreate kraft joint. }

  Parent.AddNotificationOnBehaviorAdded(
    {$ifdef FPC}@{$endif} OnParentTransformAddBehavior, TCastleRigidBody);

  Parent.AddNotificationBeforeBehaviorRemove(
    {$ifdef FPC}@{$endif} OnBeforeParentTransfromRemoveBehavior, TCastleRigidBody);

  FParentRigidBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if FParentRigidBody <> nil then
  begin
    SetParentRigidBodyEvents;
    RecreateKraftJoint;
  end;
end;

procedure TCastleAbstractJoint.WorldBeforeDetach;
begin
  { This means we are before detach from world so we need to get parent
  remove all callbacks also rigid body callbacks }

  Parent.RemoveNotificationOnBehaviorAdded(
    {$ifdef FPC}@{$endif} OnParentTransformAddBehavior, TCastleRigidBody);

  Parent.RemoveNotificationBeforeBehaviorRemove(
    {$ifdef FPC}@{$endif} OnBeforeParentTransfromRemoveBehavior, TCastleRigidBody);

  RemoveParentRigidBodyEvents;
  inherited ParentBeforeDetach;
end;

function TCastleAbstractJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'Breakable', 'BreakThresholdForce', 'BreakThresholdTorque'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleAbstractJoint.InternalCreateGizmos;
begin
  // nothing in this class
end;

procedure TCastleAbstractJoint.InternalDestroyGizmos;
begin
  // nothing in this class
end;

{ TCastleHingeJoint ---------------------------------------------------------------- }

constructor TCastleHingeJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenWorldChange := true;
  FAxis:= TVector3.One[1];
  FAnchor := TVector3.Zero;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlehingejoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleHingeJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlehingejoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastleHingeJoint.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleHingeJoint.InternalCreateGizmos;
begin
  inherited;
  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FAnchorTransform).Joint := Self;
    TCastleJointTool(FAnchorTransform).OnGetValue := {$ifdef FPC}@{$endif} GetAnchorForTool;
    TCastleJointTool(FAnchorTransform).OnSetValue := {$ifdef FPC}@{$endif} SetAnchor;
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    TCastleJointTool(FAnchorTransform).Value := Anchor;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleHingeJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleHingeJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'AnchorPersistent', 'AngleLimits', 'MinAngle', 'MaxAngle',
      'AxisPersistent'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleHingeJoint.GetAnchorForTool: TVector3;
begin
  Result := Anchor;
end;

procedure TCastleHingeJoint.SetAnchor(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FAnchor, AValue) then
  begin
    FAnchor := AValue;
    if FAnchorTransform <> nil then
      TCastleJointTool(FAnchorTransform).Value := FAnchor;
    RecreateKraftJoint;
  end;
end;

procedure TCastleHingeJoint.SetAxis(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FAxis, AValue) then
  begin
    FAxis := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.SetWorldRotationAxis(
        VectorToKraft(Parent.LocalToWorldDirection(FAxis)));
  end;
end;

procedure TCastleHingeJoint.SetAngleLimits(const AValue: Boolean);
begin
  if FAngleLimits <> AValue then
  begin
    FAngleLimits := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.EnableLimit(AValue);
  end;
end;

procedure TCastleHingeJoint.SetMinAngle(const AValue: Single);
begin
  if FMinAngle <> AValue then
  begin
    FMinAngle := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.SetMinimumAngleLimit(FMinAngle);
  end;
end;

procedure TCastleHingeJoint.SetMaxAngle(const AValue: Single);
begin
  if FMaxAngle <> AValue then
  begin
    FMaxAngle := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.SetMaximumAngleLimit(FMaxAngle);
  end;
end;

procedure TCastleHingeJoint.InternalUpdateGizmosName;
begin
  if (FAnchorTransform <> nil) and (FAnchorTransform.Name <> Name + '_Anchor') then
  begin
    FAnchorTransform.Name := Name + '_Anchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleHingeJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointHinge;
end;

procedure TCastleHingeJoint.CreateKraftJoint;
begin
  FKraftJointHinge := TKraftConstraintJointHinge.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(Parent.LocalToWorld(FAnchor)),
    VectorToKraft(Parent.LocalToWorldDirection(FAxis)),
    FAngleLimits,
    false,
    FMinAngle,
    FMaxAngle,
    0,
    0,
    ConnectedCollide
  );
end;

procedure TCastleHingeJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointHinge);
end;

procedure TCastleHingeJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlehingejoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleRopeJoint --------------------------------------------------------------- }

constructor TCastleRopeJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenWorldChange := true;
  FAnchor := TVector3.Zero;
  FConnectedAnchor := TVector3.Zero;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleropejoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleRopeJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleropejoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastleRopeJoint.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FConnectedAnchorTransform);
  inherited WorldBeforeDetach;
end;

function TCastleRopeJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'AnchorPersistent', 'ConnectedAnchorPersistent', 'Distance'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleRopeJoint.InternalCreateGizmos;
begin
  inherited;
  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FAnchorTransform).Joint := Self;
    TCastleJointTool(FAnchorTransform).OnGetValue := {$ifdef FPC}@{$endif} GetAnchorForTool;
    TCastleJointTool(FAnchorTransform).OnSetValue := {$ifdef FPC}@{$endif} SetAnchor;
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    FAnchorTransform.Translation := Anchor;
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedAnchorTransform = nil) and (FConnectedRigidBody <> nil) and (FConnectedRigidBody.Parent <> nil) then
  begin
    FConnectedAnchorTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FConnectedAnchorTransform).Joint := Self;
    TCastleJointTool(FConnectedAnchorTransform).OnGetValue := {$ifdef FPC}@{$endif} GetConnectedAnchorForTool;
    TCastleJointTool(FConnectedAnchorTransform).OnSetValue := {$ifdef FPC}@{$endif} SetConnectedAnchor;
    TCastleJointTool(FConnectedAnchorTransform).Color := ConnectedAnchorColor;
    InternalUpdateGizmosName;
    FConnectedAnchorTransform.Translation := ConnectedAnchor;
    FConnectedRigidBody.Parent.Add(FConnectedAnchorTransform);
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleRopeJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FConnectedAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleRopeJoint.GetAnchorForTool: TVector3;
begin
  Result := Anchor;
end;

procedure TCastleRopeJoint.SetAnchor(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FAnchor, AValue) then
  begin
    FAnchor := AValue;
    if FAnchorTransform <> nil then
      TCastleJointTool(FAnchorTransform).Value := FAnchor;
    RecreateKraftJoint;
  end;
end;

function TCastleRopeJoint.GetConnectedAnchorForTool: TVector3;
begin
  Result := ConnectedAnchor;
end;

procedure TCastleRopeJoint.SetConnectedAnchor(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FConnectedAnchor, AValue) then
  begin
    FConnectedAnchor := AValue;
    if FConnectedAnchorTransform <> nil then
      TCastleJointTool(FConnectedAnchorTransform).Value := FConnectedAnchor;
    RecreateKraftJoint;
  end;
end;

procedure TCastleRopeJoint.SetDistance(const AValue: Single);
begin
  if not SameValue(FDistance, AValue) then
  begin
    FDistance := AValue;
    RecreateKraftJoint;
  end;
end;

procedure TCastleRopeJoint.InternalUpdateGizmosName;
begin
  if (FAnchorTransform <> nil) and (FAnchorTransform.Name <> Name + '_Anchor') then
  begin
    FAnchorTransform.Name := Name + '_Anchor';
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedAnchorTransform <> nil) and
     (FConnectedAnchorTransform.Name <> Name + '_ConnectedAnchor') then
  begin
    FConnectedAnchorTransform.Name := Name + '_ConnectedAnchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleRopeJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointRope;
end;

procedure TCastleRopeJoint.CreateKraftJoint;
begin
  FKraftJointRope := TKraftConstraintJointRope.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FAnchor),
    VectorToKraft(FConnectedAnchor),
    FDistance,
    ConnectedCollide
  );
end;

procedure TCastleRopeJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointRope);
end;

procedure TCastleRopeJoint.OnConnectedFree(const Sender: TFreeNotificationObserver);
begin
  { Remove connected transform anchor visualisation because
    its parent is freed }
  FreeAndNil(FConnectedAnchorTransform);

  inherited OnConnectedFree(Sender);
end;

procedure TCastleRopeJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleropejoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleBallJoint --------------------------------------------------------------- }

constructor TCastleBallJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenWorldChange := true;
  FAnchor := TVector3.Zero;
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleballjoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleBallJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleballjoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastleBallJoint.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleBallJoint.InternalCreateGizmos;
begin
  inherited;
  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FAnchorTransform).Joint := Self;
    TCastleJointTool(FAnchorTransform).OnGetValue := {$ifdef FPC}@{$endif} GetAnchorForTool;
    TCastleJointTool(FAnchorTransform).OnSetValue := {$ifdef FPC}@{$endif} SetAnchor;
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    TCastleJointTool(FAnchorTransform).Value := Anchor;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleBallJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleBallJoint.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleBallJoint.GetAnchorForTool: TVector3;
begin
  Result := Anchor;
end;

procedure TCastleBallJoint.SetAnchor(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FAnchor, AValue) then
  begin
    FAnchor := AValue;
    if FAnchorTransform <> nil then
      TCastleJointTool(FAnchorTransform).Value := FAnchor;
    RecreateKraftJoint;
  end;
end;

procedure TCastleBallJoint.InternalUpdateGizmosName;
begin
  if (FAnchorTransform <> nil) and (FAnchorTransform.Name <> Name + '_Anchor') then
  begin
    FAnchorTransform.Name := Name + '_Anchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleBallJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointBall;
end;

procedure TCastleBallJoint.CreateKraftJoint;
begin
  FKraftJointBall := TKraftConstraintJointBallSocket.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(Parent.LocalToWorld(FAnchor)),
    ConnectedCollide
  );
end;

procedure TCastleBallJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointBall);
end;

procedure TCastleBallJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleballjoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleDistanceJoint ------------------------------------------------------------- }

constructor TCastleDistanceJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenWorldChange := true;
  FAnchor := TVector3.Zero;
  FConnectedAnchor := TVector3.Zero;
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastledistancejoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleDistanceJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastledistancejoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited Destroy;
end;

procedure TCastleDistanceJoint.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FConnectedAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleDistanceJoint.InternalCreateGizmos;
begin
  inherited;
  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FAnchorTransform).Joint := Self;
    TCastleJointTool(FAnchorTransform).OnGetValue := {$ifdef FPC}@{$endif} GetAnchorForTool;
    TCastleJointTool(FAnchorTransform).OnSetValue := {$ifdef FPC}@{$endif} SetAnchor;
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    FAnchorTransform.Translation := Anchor;
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedAnchorTransform = nil) and (FConnectedRigidBody <> nil) and (FConnectedRigidBody.Parent <> nil) then
  begin
    FConnectedAnchorTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FConnectedAnchorTransform).Joint := Self;
    TCastleJointTool(FConnectedAnchorTransform).OnGetValue := {$ifdef FPC}@{$endif} GetConnectedAnchorForTool;
    TCastleJointTool(FConnectedAnchorTransform).OnSetValue := {$ifdef FPC}@{$endif} SetConnectedAnchor;
    TCastleJointTool(FConnectedAnchorTransform).Color := ConnectedAnchorColor;
    InternalUpdateGizmosName;
    FConnectedAnchorTransform.Translation := ConnectedAnchor;
    FConnectedRigidBody.Parent.Add(FConnectedAnchorTransform);
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleDistanceJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FConnectedAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleDistanceJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'AnchorPersistent', 'ConnectedAnchorPersistent', 'DampingRatio',
      'FrequencyHz'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleDistanceJoint.GetAnchorForTool: TVector3;
begin
  Result := Anchor;
end;

procedure TCastleDistanceJoint.SetAnchor(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FAnchor, AValue) then
  begin
    FAnchor := AValue;
    if FAnchorTransform <> nil then
      TCastleJointTool(FAnchorTransform).Value := AValue;
    RecreateKraftJoint;
  end;
end;

function TCastleDistanceJoint.GetConnectedAnchorForTool: TVector3;
begin
  Result := ConnectedAnchor;
end;

procedure TCastleDistanceJoint.SetConnectedAnchor(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FConnectedAnchor, AValue) then
  begin
    FConnectedAnchor := AValue;
    if FConnectedAnchorTransform <> nil then
      TCastleJointTool(FConnectedAnchorTransform).Value := AValue;
    RecreateKraftJoint;
  end;
end;

procedure TCastleDistanceJoint.SetFrequencyHz(const AValue: Single);
begin
  if not SameValue(FFrequencyHz, AValue) then
  begin
    FFrequencyHz := AValue;
    RecreateKraftJoint;
  end;
end;

procedure TCastleDistanceJoint.SetDampingRatio(const AValue: Single);
begin
  if not SameValue(FDampingRatio, AValue) then
  begin
    FDampingRatio := AValue;
    RecreateKraftJoint;
  end;
end;

procedure TCastleDistanceJoint.InternalUpdateGizmosName;
begin
  if (FAnchorTransform <> nil) and (FAnchorTransform.Name <> Name + '_Anchor') then
  begin
    FAnchorTransform.Name := Name + '_Anchor';
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedAnchorTransform <> nil) and
     (FConnectedAnchorTransform.Name <> Name + '_ConnectedAnchor')  then
  begin
    FConnectedAnchorTransform.Name := Name + '_ConnectedAnchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleDistanceJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointDistance;
end;

procedure TCastleDistanceJoint.CreateKraftJoint;
begin
  Assert(FKraftJointDistance = nil, 'Second joint initialization');
  FKraftJointDistance := TKraftConstraintJointDistance.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FAnchor),
    VectorToKraft(FConnectedAnchor),
    FFrequencyHz,
    FDampingRatio,
    ConnectedCollide
  );
end;

procedure TCastleDistanceJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointDistance);
end;

procedure TCastleDistanceJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastledistancejoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleGrabJoint ----------------------------------------------------------------- }

constructor TCastleGrabJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenWorldChange := true;
  FTargetWorld := TVector3.Zero;
  FMaxForce := 0.0;
  FFrequencyHz := DefaultFrequencyHz;
  FDampingRatio := DefaultDampingRatio;
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlegrabjoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleGrabJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlegrabjoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastleGrabJoint.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FTargetWorldTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleGrabJoint.InternalCreateGizmos;
begin
  inherited;
  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FAnchorTransform).Joint := Self;
    TCastleJointTool(FAnchorTransform).OnGetValue := {$ifdef FPC}@{$endif} GetAnchorForTool;
    TCastleJointTool(FAnchorTransform).OnSetValue := {$ifdef FPC}@{$endif} SetAnchor;
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    FAnchorTransform.Translation := Anchor;
    InternalCastleDesignInvalidate := true;
  end;

  if FTargetWorldTransform = nil then
  begin
    FTargetWorldTransform := TCastleJointTool.Create(nil);
    TCastleJointTool(FTargetWorldTransform).Joint := Self;
    TCastleJointTool(FTargetWorldTransform).OnGetValue := {$ifdef FPC}@{$endif} GetTargetWorldForTool;
    TCastleJointTool(FTargetWorldTransform).OnSetValue := {$ifdef FPC}@{$endif} SetTargetWorld;
    TCastleJointTool(FTargetWorldTransform).Color := Blue;
    InternalUpdateGizmosName;
    Parent.World.Add(FTargetWorldTransform);
    FTargetWorldTransform.Translation := TargetWorld;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleGrabJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  FreeAndNil(FTargetWorldTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleGrabJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'TargetWorldPersistent', 'AnchorPersistent',
      'DampingRatio', 'FrequencyHz', 'MaxForce'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleGrabJoint.GetTargetWorldForTool: TVector3;
begin
  Result := TargetWorld;
end;

procedure TCastleGrabJoint.SetTargetWorld(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FTargetWorld, AValue) then
  begin
    FTargetWorld := AValue;

    if FTargetWorldTransform <> nil then
      TCastleJointTool(FTargetWorldTransform).Value := FTargetWorld;

    if FKraftJointGrab <> nil then
    begin
      FKraftJointGrab.SetWorldPoint(VectorToKraft(FTargetWorld));
      // Kraft needs us to wake rigid body to make body start moving
      // https://github.com/BeRo1985/kraft/issues/26
      FParentRigidBody.WakeUp;
    end;
  end;
end;

function TCastleGrabJoint.GetAnchorForTool: TVector3;
begin
  Result := Anchor;
end;

procedure TCastleGrabJoint.SetAnchor(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FAnchor, AValue) then
  begin
    FAnchor := AValue;
    if FAnchorTransform <> nil then
      TCastleJointTool(FAnchorTransform).Value := AValue;
    RecreateKraftJoint;
  end;
end;

procedure TCastleGrabJoint.SetFrequencyHz(const AValue: Single);
begin
  if FFrequencyHz <> AValue then
  begin
    FFrequencyHz := AValue;
    RecreateKraftJoint;
  end;
end;

procedure TCastleGrabJoint.SetDampingRatio(const AValue: Single);
begin
  if FDampingRatio <> AValue then
  begin
    FDampingRatio := AValue;
    RecreateKraftJoint;
  end;
end;

function TCastleGrabJoint.MaxForceNonZero: Single;
begin
  if MaxForce = 0 then
    Result := MaxSingle
  else
    Result := MaxForce;
end;

procedure TCastleGrabJoint.SetMaxForce(const AValue: Single);
begin
  if FMaxForce <> AValue then
  begin
    FMaxForce := AValue;
    if FKraftJointGrab <> nil then
      FKraftJointGrab.SetMaximalForce(MaxForceNonZero);
  end;
end;

procedure TCastleGrabJoint.InternalUpdateGizmosName;
begin
  if (FAnchorTransform <> nil) and (FAnchorTransform.Name <> Name + '_Anchor') then
  begin
    FAnchorTransform.Name := Name + '_Anchor';
    InternalCastleDesignInvalidate := true;
  end;

  if (FTargetWorldTransform <> nil) and
     (FTargetWorldTransform.Name <> Name + '_TargetWorld') then
  begin
    FTargetWorldTransform.Name := Name + '_TargetWorld';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleGrabJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointGrab;
end;

procedure TCastleGrabJoint.CreateKraftJoint;
var
  AnchorInWorldCoords: TKraftVector3;
begin
  Assert(FKraftJointGrab = nil, 'Second joint initialization');

  { TKraftConstraintJointGrab.Create takes AWorldPoint as argument,
    and implicitly sets fLocalAnchor  to

      fLocalAnchor:=Vector3TermMatrixMulInverted(AWorldPoint,ARigidBody.fWorldTransform);

    IOW, it converts AWorldPoint into local coordinate space,
    so that initial state means that TKraftConstraintJointGrab doesn't have to do anything
    -- target is at anchor.

    We want a different behavior,
    - Anchor (in local coords) to be explicitly configurable, like other joints
    - TargetWorld configurable, and not necessarily "nothing to do" initially

    So we kind of "trick" Kraft into it:
    - as AWorldPoint argument, we pass AnchorInWorldCoords
    - later we call SetWorldPoint to set real desired TargetWorld
  }

  AnchorInWorldCoords := Vector3TermMatrixMul(
    VectorToKraft(FAnchor),
    FParentRigidBody.FKraftBody.WorldTransform);

  FKraftJointGrab := TKraftConstraintJointGrab.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    AnchorInWorldCoords,
    FFrequencyHz,
    FDampingRatio,
    MaxForceNonZero
  );
  FKraftJointGrab.SetWorldPoint(VectorToKraft(FTargetWorld));
end;

procedure TCastleGrabJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointGrab);
end;

procedure TCastleGrabJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlegrabjoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
