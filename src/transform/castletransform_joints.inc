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
  { The most abstract joint class that observes the existence of rigid body
    and its state, handles proper attachment and detachment from the parent,
    and includes the properties available in all joints. }
  TAbstractJoint = class(TCastleBehavior)
  protected
    { When joint is added to world we need found parent rigid body and set
      callbacks for its initialize/deinitialize events. If this pointer is not nil
      that means that we done that }
    FParentRigidBody: TCastleRigidBody;
    FBreakThresholdForce: Single;
    FBreakThresholdTorque: Single;
    FBreakable: Boolean;

    { Create kraft joint and set it specialized properties }
    procedure CreateKraftJoint; virtual; abstract;

    { Function to get kraft joint }
    function GetKraftJoint: TKraftConstraintJoint; virtual; abstract;

    { Tries to deinitialize and initialize kraft object }
    procedure RecreateKraftJoint;
    procedure InitializeKraftJoint; virtual;
    procedure DeinitializeKraftJoint; virtual; abstract;

    { Means that we can create joint object }
    function AllNeededKraftObjectsInitialized: Boolean; virtual;

    { Event callback when TCastleRigidBody is added to parent transform }
    procedure OnParentTransformAddBehavior(const Component: TComponent); virtual;

    { Event callback when TCastleRigidBody is removed from parent transform }
    procedure OnBeforeParentTransfromRemoveBehavior(const Component: TComponent); virtual;

    { Event callback when TCastleRigidBody behavior is initialized }
    procedure OnParentRigidBodyInitialized(const Component: TComponent); virtual;

    { Event callback when TCastleRigidBody behavior is deinitialized }
    procedure OnBeforeParentRigidBodyDeinitialized(const Component: TComponent); virtual;

    { Sets TCastleRigidBody behavior initialize/deinitialize callbacks }
    procedure SetParentRigidBodyEvents;

    { Removes TCastleRigidBody behavior initialize/deinitialize callbacks }
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
  published
    property Breakable: Boolean read FBreakable write SetBreakable;
    property BreakThresholdForce:Single read FBreakThresholdForce write SetBreakThresholdForce;
    property BreakThresholdTorque:Single read FBreakThresholdTorque write SetBreakThresholdTorque;
  end;

  { Abstract class for joints that uses only one TCastleTransform. }
  TAbstractOneBodyJoint = class(TAbstractJoint)
  end;

  { Abstract class for joints that connects two TCastleTransforms. }
  TAbstractTwoBodiesJoint = class (TAbstractJoint)
  private
    { TAbstractTwoBodiesJoint means that joint connects two transforms so this is
      the second one transform pointer }
    FConnectedTransform: TCastleTransform;
    { When connected transform is assigned we need found second rigid body and set
      callbacks for its initialize/deinitialize events. If this pointer is not nil
      that means that we done that }
    FConnectedRigidBody: TCastleRigidBody;
    { Stores valu should connected rigid bodies collide or not }
    FConnectedCollide: Boolean;
    { We need observe second connected transform and remove it form
      FConnectedTransform when second transform is freed }
    FConnectedTransfromFreeObserver: TFreeNotificationObserver;

  protected
    procedure InitializeKraftJoint; override;

    procedure SetConnectedTransform(const AConnectedTransform: TCastleTransform); virtual;
    procedure SetConnectedCollide(const AValue: Boolean); virtual;

    { Means that we can create joint object }
    function AllNeededKraftObjectsInitialized: Boolean; override;

    procedure OnConnectedTransformFree(const Sender: TFreeNotificationObserver); virtual;
    { Event callback when TCastleRigidBody is added to connected transform }
    procedure OnConnectedTransformAddBehavior(const Component: TComponent); virtual;
    { Event callback when TCastleRigidBody is removed from connected transform }
    procedure OnConnectedTransfromRemoveBehavior(const Component: TComponent); virtual;
    { Event callback when connected transform TCastleRigidBody behavior is initialized }
    procedure OnConnectedRigidBodyInitialized(const Component: TComponent); virtual;
    { Event callback when connected transform TCastleRigidBody behavior is deinitialized }
    procedure OnBeforeConnectedRigidBodyDeinitialized(const Component: TComponent); virtual;

    { Sets connected transform add/remmove behavior callbacks and free
      notification observer }
    procedure SetConnectedTransformEvents;
    { Removes connected transform add/remmove behavior callbacks and free
      notification observer }
    procedure RemoveConnectedTransformEvents;
    { Sets connected TCastleRigidBody behavior initialize/deinitialize callbacks }
    procedure SetConnectedRigidBodyEvents;
    { Removes connected TCastleRigidBody behavior initialize/deinitialize
      callbacks }
    procedure RemoveConnectedRigidBodyEvents;
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { We need to check if sometimes the connected castle transform is not
      the parent node and we need change the connected node to nil then }
    procedure ParentAfterAttach; override;

    { We don't need check WorldAfterAttach and WorldBeforeDetach because all
      things are done in TAbstractJoint }
  published
    { Second transform of physics joint - can't be the Parent of joint. }
    property ConnectedTransform: TCastleTransform read FConnectedTransform
      write SetConnectedTransform;

    { Should connected transforms (ConnectedTransform and Parent)
      collide - default no }
    property ConnectedCollide: Boolean read FConnectedCollide
      write SetConnectedCollide default false;
  end;

  EConnectedTransformTheSameAsParent = class (Exception);

  TCastleHingeJoint = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointHinge: TKraftConstraintJointHinge;
    FMotorSpeed: Single;
    FRotationAxis: TVector3;
    FAnchor: TVector3;
    FAnchorTransform: TCastleTransform; // TDesignJointAnchor
    FMinAngleLimit: Single;
    FMaxAngleLimit: Single;
    FEnableLimits: Boolean;
    FEnableMotor: Boolean;
    FMaxMotorTorque: Single;

    procedure SetAnchor(const AValue: TVector3);
    procedure SetRotationAxis(const AValue: TVector3);
    procedure SetEnableLimits(const AValue: Boolean);
    procedure SetMinAngleLimit(const AValue: Single);
    procedure SetMaxAngleLimit(const AValue: Single);
    procedure SetEnableMotor(const AValue: Boolean);
    procedure SetMotorSpeed(const AValue: Single);
    function GetMotorSpeed: Single;
    procedure SetMaxMotorTorque(const AValue: Single);
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
    function InternalCreateGizmosCreated: Boolean; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is local point should be world?
    property Anchor: TVector3 read FAnchor write SetAnchor;
    // TODO: currently this is local and in kraft is world maybe we should
    // change to world or to castle default?
    property RotationAxis: TVector3 read FRotationAxis write SetRotationAxis;
  published
    property EnableLimits: Boolean read FEnableLimits write SetEnableLimits;
    property MinAngleLimit: Single read FMinAngleLimit write SetMinAngleLimit;
    property MaxAngleLimit: Single read FMaxAngleLimit write SetMaxAngleLimit;
    property EnableMotor: Boolean read FEnableMotor write SetEnableMotor;
    property MotorSpeed: Single read GetMotorSpeed write SetMotorSpeed;
    property MaxMotorTorque: Single read FMaxMotorTorque
      write SetMaxMotorTorque;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlehingejoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleRopeJoint = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointRope: TKraftConstraintJointRope;
    FAnchor: TVector3;
    FConnectedAnchor: TVector3;
    FDistance: Single;
    FAnchorTransform: TCastleTransform; // TDesignJointAnchor
    FConnectedAnchorTransform: TCastleTransform; // TDesignJointConnectedAnchor;
    procedure SetAnchor(const AValue: TVector3);
    procedure SetConnectedAnchor(const AValue: TVector3);
    procedure SetDistance(const AValue: Single);
    procedure InternalUpdateGizmosName;
  protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;

    procedure OnConnectedTransformFree(const Sender: TFreeNotificationObserver); override;
  protected
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WorldBeforeDetach; override;
    procedure InternalCreateGizmos; override;
    procedure InternalDestroyGizmos; override;
    function InternalCreateGizmosCreated: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is local point should be world?
    property Anchor: TVector3 read FAnchor write SetAnchor;
    property ConnectedAnchor: TVector3 read FConnectedAnchor
      write SetConnectedAnchor;
  published
    property Distance: Single read FDistance write SetDistance;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleropejoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleBallJoint = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointBall: TKraftConstraintJointBallSocket;
    FAnchor: TVector3;
    FAnchorTransform: TCastleTransform; // TTemporaryJointAnchor
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
    function InternalCreateGizmosCreated: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is local point should be world?
    property Anchor: TVector3 read FAnchor write SetAnchor;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleballjoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleDistanceJoint = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointDistance: TKraftConstraintJointDistance;
    FAnchor: TVector3;
    FConnectedAnchor: TVector3;
    FFrequencyHz: Single;
    FDampingRatio: Single;
    FAnchorTransform: TCastleTransform; // TTemporaryJointAnchor
    FConnectedAnchorTransform: TCastleTransform; // TTemporaryJointConnectedAnchor

    procedure SetAnchor(const AValue: TVector3);
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
    function InternalCreateGizmosCreated: Boolean; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is local point should be world?
    property Anchor: TVector3 read FAnchor write SetAnchor;
    // TODO: This is local point should be world?
    property ConnectedAnchor: TVector3 read FConnectedAnchor
      write SetConnectedAnchor;

  published
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz;
    property DampingRatio: Single read FDampingRatio write SetDampingRatio;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastledistancejoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleGrabJoint = class (TAbstractOneBodyJoint)
  private
    FDampingRatio: Single;
    FFrequencyHz: Single;
    FKraftJointGrab: TKraftConstraintJointGrab;
    FMaximalForce: Single;
    FWorldPoint: TVector3;
    FWorldPointTransform: TCastleTransform; // TTemporaryJointWorldPoint;

    procedure SetWorldPoint(const AValue: TVector3);
    procedure SetFrequencyHz(const AValue: Single);
    procedure SetDampingRatio(const AValue: Single);
    procedure SetMaximalForce(const AValue: Single);
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
    function InternalCreateGizmosCreated: Boolean; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    // TODO: This is world point should be local? Should be synchronized per update?
    property WorldPoint: TVector3 read FWorldPoint write SetWorldPoint;
  published
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz {$ifdef FPC}default 5{$endif};
    property DampingRatio: Single read FDampingRatio write SetDampingRatio {$ifdef FPC}default 0.7{$endif};
    property MaximalForce: Single read FMaximalForce write SetMaximalForce {$ifdef FPC}default MaxSingle{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlegrabjoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractTwoBodiesJoint ---------------------------------------------------- }

procedure TAbstractTwoBodiesJoint.SetConnectedCollide(const AValue: Boolean);
begin
  if FConnectedCollide = AValue then
    Exit;
  FConnectedCollide := AValue;
  RecreateKraftJoint;
end;

function TAbstractTwoBodiesJoint.AllNeededKraftObjectsInitialized: Boolean;
begin
  Result := (inherited AllNeededKraftObjectsInitialized) and
    (FConnectedRigidBody <> nil) and (FConnectedRigidBody.IsInitialized);
end;

procedure TAbstractTwoBodiesJoint.OnConnectedTransformFree(
  const Sender: TFreeNotificationObserver);
begin
  DeinitializeKraftJoint;
  RemoveConnectedRigidBodyEvents;
  RemoveConnectedTransformEvents;
  FConnectedRigidBody := nil;
  FConnectedTransform := nil;
end;

procedure TAbstractTwoBodiesJoint.OnConnectedTransformAddBehavior(
  const Component: TComponent);
begin
  FConnectedRigidBody := Component as TCastleRigidBody;
  SetConnectedRigidBodyEvents;
end;

procedure TAbstractTwoBodiesJoint.OnConnectedTransfromRemoveBehavior(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
  RemoveConnectedRigidBodyEvents;
  FConnectedRigidBody := nil;
end;

procedure TAbstractTwoBodiesJoint.OnConnectedRigidBodyInitialized(
  const Component: TComponent);
begin
  RecreateKraftJoint;
end;

procedure TAbstractTwoBodiesJoint.OnBeforeConnectedRigidBodyDeinitialized(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
end;

procedure TAbstractTwoBodiesJoint.SetConnectedTransformEvents;
begin
  if FConnectedTransform = nil then
    Exit;

  FConnectedTransform.AddNotificationOnBehaviorAdded(
    {$ifdef FPC}@{$endif} OnConnectedTransformAddBehavior, TCastleRigidBody);

  FConnectedTransform.AddNotificationBeforeBehaviorRemove(
    {$ifdef FPC}@{$endif} OnConnectedTransfromRemoveBehavior, TCastleRigidBody);

  if FConnectedTransfromFreeObserver = nil then
  begin
    FConnectedTransfromFreeObserver := TFreeNotificationObserver.Create(Self);
    FConnectedTransfromFreeObserver.OnFreeNotification := {$ifdef FPC}@{$endif}OnConnectedTransformFree;
  end;

  FConnectedTransfromFreeObserver.Observed := FConnectedTransform;
end;

procedure TAbstractTwoBodiesJoint.RemoveConnectedTransformEvents;
begin
  if FConnectedTransform <> nil then
  begin
    { Remove events from previous connected castle transform }
    FConnectedTransform.AddNotificationOnBehaviorAdded(
      {$ifdef FPC}@{$endif} OnConnectedTransformAddBehavior, TCastleRigidBody);

    FConnectedTransform.AddNotificationBeforeBehaviorRemove(
      {$ifdef FPC}@{$endif} OnConnectedTransfromRemoveBehavior, TCastleRigidBody);
  end;

  { Stop watching this transform }
  if FConnectedTransfromFreeObserver <> nil then
    FConnectedTransfromFreeObserver.Observed := nil;
end;

procedure TAbstractTwoBodiesJoint.SetConnectedRigidBodyEvents;
begin
  if FConnectedRigidBody <> nil then
  begin
    FConnectedRigidBody.AddNotificationOnInitialized({$ifdef FPC}@{$endif} OnConnectedRigidBodyInitialized);
    FConnectedRigidBody.AddNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeConnectedRigidBodyDeinitialized);
  end;
end;

procedure TAbstractTwoBodiesJoint.RemoveConnectedRigidBodyEvents;
begin
  if FConnectedRigidBody <> nil then
  begin
    FConnectedRigidBody.RemoveNotificationOnInitialized({$ifdef FPC}@{$endif} OnConnectedRigidBodyInitialized);
    FConnectedRigidBody.RemoveNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeConnectedRigidBodyDeinitialized);
  end;
end;

function TAbstractTwoBodiesJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'ConnectedTransform') or
     (PropertyName = 'ConnectedCollide') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TAbstractTwoBodiesJoint.ParentAfterAttach;
begin
  { Remove connected transform if new parent is the same transform }
  if Parent = FConnectedTransform then
  begin
    WritelnWarning('I disconnect the connected transform because it is the same transform as parent.');
    RemoveConnectedRigidBodyEvents;
    FConnectedRigidBody := nil;
    RemoveConnectedTransformEvents;
    FConnectedTransform := nil;
  end;
end;

procedure TAbstractTwoBodiesJoint.InitializeKraftJoint;
begin
  inherited InitializeKraftJoint;

  // make sure connected rigid body is active after joint creation
  FConnectedRigidBody.WakeUp;
end;

procedure TAbstractTwoBodiesJoint.SetConnectedTransform(
  const AConnectedTransform: TCastleTransform);
begin
  if (AConnectedTransform <> nil) and (AConnectedTransform = Parent) then
    raise EConnectedTransformTheSameAsParent.Create(
      'Can''t set ConnectedTransform to behavior Parent.');

  if FConnectedTransform <> AConnectedTransform then
  begin
    if FConnectedTransform <> nil then
    begin
      { Remove events from previous connected castle transform }
      RemoveConnectedTransformEvents;

      { Remove events from Rigidbody of old connected transform }
      RemoveConnectedRigidBodyEvents;
    end;

    FConnectedTransform := AConnectedTransform;
    if FConnectedTransform <> nil then
    begin
      { If FConnectedTransform is not nil set all events and try
        recreate joint }
      SetConnectedTransformEvents;

      FConnectedRigidBody := FConnectedTransform.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
      if FConnectedRigidBody <> nil then
      begin
        SetConnectedRigidBodyEvents;
        RecreateKraftJoint;
      end;
    end;
  end;
end;

{ TAbstractJoint ------------------------------------------------------------- }

procedure TAbstractJoint.RecreateKraftJoint;
begin
  DeinitializeKraftJoint;
  if AllNeededKraftObjectsInitialized then
    InitializeKraftJoint;
end;

procedure TAbstractJoint.InitializeKraftJoint;
begin
  CreateKraftJoint;
  // make sure rigid body is active after joint creation
  FParentRigidBody.WakeUp;
  UpdateBreakableInKraft;
  UpdateBreakThresholdForceInKraft;
  UpdateBreakThresholdTorqueInKraft;
end;

function TAbstractJoint.AllNeededKraftObjectsInitialized: Boolean;
begin
  Result := (FParentRigidBody <> nil) and FParentRigidBody.IsInitialized;
end;

procedure TAbstractJoint.OnParentTransformAddBehavior(
  const Component: TComponent);
begin
  FParentRigidBody := Component as TCastleRigidBody;
  SetParentRigidBodyEvents;
end;

procedure TAbstractJoint.OnBeforeParentTransfromRemoveBehavior(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
  RemoveParentRigidBodyEvents;
  FParentRigidBody := nil;
end;

procedure TAbstractJoint.OnParentRigidBodyInitialized(
  const Component: TComponent);
begin
  RecreateKraftJoint;
end;

procedure TAbstractJoint.OnBeforeParentRigidBodyDeinitialized(
  const Component: TComponent);
begin
  DeinitializeKraftJoint;
end;

procedure TAbstractJoint.SetParentRigidBodyEvents;
begin
  if FParentRigidBody <> nil then
  begin
    FParentRigidBody.AddNotificationOnInitialized({$ifdef FPC}@{$endif} OnParentRigidBodyInitialized);
    FParentRigidBody.AddNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeParentRigidBodyDeinitialized);
  end;
end;

procedure TAbstractJoint.RemoveParentRigidBodyEvents;
begin
  if FParentRigidBody <> nil then
  begin
    FParentRigidBody.RemoveNotificationOnInitialized({$ifdef FPC}@{$endif} OnParentRigidBodyInitialized);
    FParentRigidBody.RemoveNotificationBeforeDeinitialized({$ifdef FPC}@{$endif} OnBeforeParentRigidBodyDeinitialized);
  end;
end;

procedure TAbstractJoint.UpdateBreakThresholdForceInKraft;
begin
  if GetKraftJoint <> nil then
    GetKraftJoint.BreakThresholdForce := FBreakThresholdForce;
end;

procedure TAbstractJoint.UpdateBreakThresholdTorqueInKraft;
begin
  if GetKraftJoint <> nil then
    GetKraftJoint.BreakThresholdTorque := FBreakThresholdTorque;
end;

procedure TAbstractJoint.UpdateBreakableInKraft;
begin
  if GetKraftJoint <> nil then
  begin
    if FBreakable then
      GetKraftJoint.Flags := GetKraftJoint.Flags + [kcfBreakable]
    else
      GetKraftJoint.Flags := GetKraftJoint.Flags - [kcfBreakable];
  end;
end;

procedure TAbstractJoint.SetBreakThresholdForce(const AValue: Single);
begin
  if not SameValue(FBreakThresholdForce, AValue) then
  begin
    FBreakThresholdForce := AValue;
    UpdateBreakThresholdForceInKraft;
  end;
end;

procedure TAbstractJoint.SetBreakThresholdTorque(const AValue: Single);
begin
  if not SameValue(FBreakThresholdTorque, AValue) then
  begin
    FBreakThresholdTorque := AValue;
    UpdateBreakThresholdTorqueInKraft;
  end;
end;

procedure TAbstractJoint.SetBreakable(const AValue: Boolean);
begin
  if FBreakable <> AValue then
  begin
    FBreakable := AValue;
    UpdateBreakableInKraft;
  end;
end;

destructor TAbstractJoint.Destroy;
begin
  DeinitializeKraftJoint;
  inherited Destroy;
end;

procedure TAbstractJoint.WorldAfterAttach;
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

procedure TAbstractJoint.WorldBeforeDetach;
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

function TAbstractJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'Breakable') or
     (PropertyName = 'BreakThresholdForce') or
     (PropertyName = 'BreakThresholdTorque') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TCastleHingeJoint ---------------------------------------------------------------- }

procedure TCastleHingeJoint.SetAnchor(const AValue: TVector3);
begin
  if not TVector3.Equals(FAnchor, AValue) then
  begin
    FAnchor := AValue;
    if FAnchorTransform <> nil then
      TDesignJointAnchor(FAnchorTransform).Value := FAnchor;
    RecreateKraftJoint;
  end;
end;

procedure TCastleHingeJoint.SetRotationAxis(const AValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(FRotationAxis, AValue) then
  begin
    FRotationAxis := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.SetWorldRotationAxis(
        VectorToKraft(Parent.LocalToWorldDirection(FRotationAxis)));
  end;
end;

procedure TCastleHingeJoint.SetEnableLimits(const AValue: Boolean);
begin
  if FEnableLimits <> AValue then
  begin
    FEnableLimits := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.EnableLimit(AValue);
  end;
end;

procedure TCastleHingeJoint.SetMinAngleLimit(const AValue: Single);
begin
  if not SameValue(FMinAngleLimit, AValue) then
  begin
    FMinAngleLimit := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.SetMinimumAngleLimit(FMinAngleLimit);
  end;
end;

procedure TCastleHingeJoint.SetMaxAngleLimit(const AValue: Single);
begin
  if not SameValue(FMaxAngleLimit, AValue) then
  begin
    FMaxAngleLimit := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.SetMaximumAngleLimit(FMaxAngleLimit);
  end;
end;

procedure TCastleHingeJoint.SetEnableMotor(const AValue: Boolean);
begin
  if FEnableMotor <> AValue then
  begin
    FEnableMotor := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.EnableMotor(AValue);
  end;
end;

procedure TCastleHingeJoint.SetMotorSpeed(const AValue: Single);
begin
  { Do not check FMotorSpeed <> AValue here it can be changed by craft }
  // TODO: Maybe should be updated in Update? See GetMotorSpeed()?
  FMotorSpeed := AValue;
  if FKraftJointHinge <> nil then
    FKraftJointHinge.SetMotorSpeed(AValue);
end;

function TCastleHingeJoint.GetMotorSpeed: Single;
begin
  if FKraftJointHinge <> nil then
    FMotorSpeed := FKraftJointHinge.GetMotorSpeed;

  Result := FMotorSpeed;
end;

procedure TCastleHingeJoint.SetMaxMotorTorque(const AValue: Single);
begin
  if not SameValue(FMaxMotorTorque, AValue) then
  begin
    FMaxMotorTorque := AValue;
    if FKraftJointHinge <> nil then
      FKraftJointHinge.SetMaximalMotorTorque(FMaxMotorTorque);
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
    VectorToKraft(Parent.LocalToWorldDirection(FRotationAxis)),
    FEnableLimits,
    FEnableMotor,
    FMinAngleLimit,
    FMaxAngleLimit,
    FMotorSpeed,
    FMaxMotorTorque,
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

constructor TCastleHingeJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FRotationAxis:= CastleVectors.Vector3(0, 0, 1);
  FAnchor := CastleVectors.Vector3(0, 0, 0);

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
    FAnchorTransform := TDesignJointAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    TDesignJointAnchor(FAnchorTransform).Value := Anchor;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleHingeJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleHingeJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := FAnchorTransform <> nil;
end;

function TCastleHingeJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'EnableLimits') or
     (PropertyName = 'MinAngleLimit') or
     (PropertyName = 'MaxAngleLimit') or
     (PropertyName = 'EnableMotor') or
     (PropertyName = 'MotorSpeed') or
     (PropertyName = 'MaxMotorTorque') or
     (PropertyName = 'RotationAxisPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlehingejoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleRopeJoint --------------------------------------------------------------- }

procedure TCastleRopeJoint.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    TDesignJointAnchor(FAnchorTransform).Value := FAnchor;
  RecreateKraftJoint;
end;

procedure TCastleRopeJoint.SetConnectedAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FConnectedAnchor, AValue) then
    Exit;

  FConnectedAnchor := AValue;
  if FConnectedAnchorTransform <> nil then
    TDesignJointConnectedAnchor(FConnectedAnchorTransform).Value := FConnectedAnchor;
  RecreateKraftJoint;
end;

procedure TCastleRopeJoint.SetDistance(const AValue: Single);
begin
  if SameValue(FDistance, AValue) then
    Exit;
  FDistance := AValue;
  RecreateKraftJoint;
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

procedure TCastleRopeJoint.OnConnectedTransformFree(const Sender: TFreeNotificationObserver);
begin
  { Remove connected transform anchor visualisation because
    its parent is freed }
  FreeAndNil(FConnectedAnchorTransform);

  inherited OnConnectedTransformFree(Sender);
end;

procedure TCastleRopeJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

constructor TCastleRopeJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := CastleVectors.Vector3(0, 0, 0);
  FConnectedAnchor := CastleVectors.Vector3(0, 0, 0);

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
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'ConnectedAnchorPersistent') or
     (PropertyName = 'Distance') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleRopeJoint.InternalCreateGizmos;
begin
  inherited;
  if FAnchorTransform = nil then
  begin
    FAnchorTransform := TDesignJointAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    FAnchorTransform.Translation := Anchor;
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedAnchorTransform = nil) and (FConnectedRigidBody <> nil) and (FConnectedRigidBody.Parent <> nil) then
  begin
    FConnectedAnchorTransform := TDesignJointConnectedAnchor.Create(nil, Self);
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

function TCastleRopeJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := (FConnectedAnchorTransform <> nil) or (FAnchorTransform <> nil);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleropejoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleBallJoint --------------------------------------------------------------- }

procedure TCastleBallJoint.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    TDesignJointAnchor(FAnchorTransform).Value := FAnchor;
  RecreateKraftJoint;
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

constructor TCastleBallJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := CastleVectors.Vector3(0, 0, 0);
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
    FAnchorTransform := TDesignJointAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    TDesignJointAnchor(FAnchorTransform).Value := Anchor;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleBallJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleBallJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := FAnchorTransform <> nil;
end;

function TCastleBallJoint.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleballjoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleDistanceJoint ------------------------------------------------------------- }

procedure TCastleDistanceJoint.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    TDesignJointAnchor(FAnchorTransform).Value := AValue;
  RecreateKraftJoint;
end;

procedure TCastleDistanceJoint.SetConnectedAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FConnectedAnchor, AValue) then
    Exit;

  FConnectedAnchor := AValue;
  if FConnectedAnchorTransform <> nil then
    TDesignJointConnectedAnchor(FConnectedAnchorTransform).Value := AValue;
  RecreateKraftJoint;
end;

procedure TCastleDistanceJoint.SetFrequencyHz(const AValue: Single);
begin
  if SameValue(FFrequencyHz, AValue) then
    Exit;

  FFrequencyHz := AValue;
  RecreateKraftJoint;
end;

procedure TCastleDistanceJoint.SetDampingRatio(const AValue: Single);
begin
  if SameValue(FDampingRatio, AValue) then
    Exit;

  FDampingRatio := AValue;
  RecreateKraftJoint;
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

constructor TCastleDistanceJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := CastleVectors.Vector3(0, 0, 0);
  FConnectedAnchor := CastleVectors.Vector3(0, 0, 0);
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
    FAnchorTransform := TDesignJointAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.Add(FAnchorTransform);
    FAnchorTransform.Translation := Anchor;
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedAnchorTransform = nil) and (FConnectedRigidBody <> nil) and (FConnectedRigidBody.Parent <> nil) then
  begin
    FConnectedAnchorTransform := TDesignJointConnectedAnchor.Create(nil, Self);
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

function TCastleDistanceJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := (FConnectedAnchorTransform <> nil) or (FAnchorTransform <> nil);
end;

function TCastleDistanceJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'ConnectedAnchorPersistent') or
     (PropertyName = 'DampingRatio') or
     (PropertyName = 'FrequencyHz') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastledistancejoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleGrabJoint ----------------------------------------------------------------- }

procedure TCastleGrabJoint.SetWorldPoint(const AValue: TVector3);
begin
  FWorldPoint := AValue;

  if FWorldPointTransform <> nil then
    TDesignJointWorldPoint(FWorldPointTransform).Value := FWorldPoint;

  if FKraftJointGrab <> nil then
  begin
    FKraftJointGrab.SetWorldPoint(VectorToKraft(FWorldPoint));
    // Kraft needs to set rigis body WakeUp to body start moving
    // https://github.com/BeRo1985/kraft/issues/26
    FParentRigidBody.WakeUp;
  end;
end;

procedure TCastleGrabJoint.SetFrequencyHz(const AValue: Single);
begin
  if SameValue(FFrequencyHz, AValue) then
    Exit;

  FFrequencyHz := AValue;
  RecreateKraftJoint;
end;

procedure TCastleGrabJoint.SetDampingRatio(const AValue: Single);
begin
  if SameValue(FDampingRatio, AValue) then
    Exit;

  FDampingRatio := AValue;
  RecreateKraftJoint;
end;

procedure TCastleGrabJoint.SetMaximalForce(const AValue: Single);
begin
  if SameValue(FMaximalForce, AValue) then
    Exit;

  FMaximalForce := AValue;
  if FKraftJointGrab <> nil then
    FKraftJointGrab.SetMaximalForce(AValue);
end;

procedure TCastleGrabJoint.InternalUpdateGizmosName;
begin
  if (FWorldPointTransform <> nil) and
     (FWorldPointTransform.Name <> Name + '_WorldPoint') then
  begin
    FWorldPointTransform.Name := Name + '_WorldPoint';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleGrabJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointGrab;
end;

procedure TCastleGrabJoint.CreateKraftJoint;
begin
  Assert(FKraftJointGrab = nil, 'Second joint initialization');
  FKraftJointGrab := TKraftConstraintJointGrab.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    VectorToKraft(FWorldPoint),
    FFrequencyHz,
    FDampingRatio,
    FMaximalForce
  );
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

constructor TCastleGrabJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FWorldPoint := CastleVectors.Vector3(0, 0, 0);
  FMaximalForce := MaxSingle;
  FFrequencyHz := 5.0;
  FDampingRatio := 0.7;
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
  FreeAndNil(FWorldPointTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleGrabJoint.InternalCreateGizmos;
begin
  inherited;
  if FWorldPointTransform = nil then
  begin
    FWorldPointTransform := TDesignJointWorldPoint.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.World.Add(FWorldPointTransform);
    FWorldPointTransform.Translation := WorldPoint;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleGrabJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FWorldPointTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleGrabJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := FWorldPointTransform <> nil;
end;

function TCastleGrabJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'WorldPointPersistent') or
     (PropertyName = 'DampingRatio') or
     (PropertyName = 'FrequencyHz') or
     (PropertyName = 'MaximalForce') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlegrabjoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
