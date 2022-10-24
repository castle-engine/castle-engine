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

{ Physics joints - experimental, off by default. }

{$ifdef CASTLE_EXPERIMENTAL_JOINTS}

{$ifdef read_interface}

  TCastleFixedJoint = class(TAbstractTwoBodiesJoint)
  private
    FKraftJointFixed: TKraftConstraintJointFixed;
    FAnchor: TVector3;

    FAnchorTransform: TCastleTransform; //TTemporaryJointAnchor

    procedure SetAnchor(const AValue: TVector3);
    procedure InternalUpdateGizmosName;

  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;

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

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlefixedjoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastlePulleyJoint = class(TAbstractTwoBodiesJoint)
    private
      FKraftJointPulley: TKraftConstraintJointPulley;
      FWorldAnchor: TVector3;
      FWorldGroundAnchor: TVector3;
      FConnectedWorldAnchor: TVector3;
      FConnectedWorldGroundAnchor: TVector3;
      FRatio: Single;

      FWorldAnchorTransform: TCastleTransform; // TTemporaryJointWorldAnchor
      FConnectedWorldAnchorTransform: TCastleTransform; // TTemporaryJointConnectedWorldAnchor;
      FWorldGroundAnchorTransform: TCastleTransform; // TTemporaryJointWorldGroundAnchor;
      FConnectedWorldGroundAnchorTransform: TCastleTransform; // TTemporaryJointConnectedWorldGroundAnchor;

      procedure SetWorldAnchor(const AValue: TVector3);
      procedure SetConnectedWorldAnchor(const AValue: TVector3);
      procedure SetWorldGroundAnchor(const AValue: TVector3);
      procedure SetConnectedWorldGroundAnchor(const AValue: TVector3);
      procedure SetRatio(const AValue: Single);
      procedure InternalUpdateGizmosName;

    strict protected
      function GetKraftJoint: TKraftConstraintJoint; override;
      procedure CreateKraftJoint; override;
      procedure DeinitializeKraftJoint; override;

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

      property WorldAnchor: TVector3 read FWorldAnchor write SetWorldAnchor;
      property ConnectedWorldAnchor: TVector3 read FConnectedWorldAnchor
        write SetConnectedWorldAnchor;
      property WorldGroundAnchor: TVector3 read FWorldGroundAnchor
        write SetWorldGroundAnchor;
      property ConnectedWorldGroundAnchor: TVector3
        read FConnectedWorldGroundAnchor write SetConnectedWorldGroundAnchor;
    published
      property Ratio: Single read FRatio write SetRatio {$ifdef FPC}default 1.0{$endif};

    {$define read_interface_class}
    {$I auto_generated_persistent_vectors/tcastlepulleyjoint_persistent_vectors.inc}
    {$undef read_interface_class}
  end;

  TCastleWorldPlaneDistanceJoint = class (TAbstractOneBodyJoint)
  private
    FKraftJointWorldPlaneDistance: TKraftConstraintJointWorldPlaneDistance;
    FAnchor: TVector3;
    FPlaneNormal: TVector3;
    FPlaneDistance: Single;
    FWorldDistance: Single;
    FDoubleSidedWorldPlane: Boolean;
    FFrequencyHz: Single;
    FDampingRatio: Single;

    FAnchorTransform: TCastleTransform; // TTemporaryJointAnchor;

    procedure SetAnchor(const AValue: TVector3);
    procedure SetPlaneNormal(const AValue: TVector3);
    procedure SetPlaneDistance(const AValue: Single);
    procedure SetWorldDistance(const AValue: Single);
    procedure SetFrequencyHz(const AValue: Single);
    procedure SetDampingRatio(const AValue: Single);
    procedure DoubleSidedWorldPlane(const AValue: Boolean);
    procedure InternalUpdateGizmosName;

  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;

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

    property Anchor: TVector3 read FAnchor write SetAnchor;
    property PlaneNormal: TVector3 read FPlaneNormal write SetPlaneNormal;
  published
    property PlaneDistance: Single read FPlaneDistance write SetPlaneDistance;
    property WorldDistance: Single read FWorldDistance write SetWorldDistance;
    property FrequencyHz: Single read FFrequencyHz write SetFrequencyHz;
    property DampingRatio: Single read FDampingRatio write SetDampingRatio;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleworldplanedistancejoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleSliderJoint = class (TAbstractTwoBodiesJoint)
  private
    FKraftJointSlider: TKraftConstraintJointSlider;

    FWorldSliderAxis: TVector3;

    FWorldAnchor: TVector3;
    FWorldAnchorTransform: TCastleTransform; // TTemporaryJointWorldAnchor

    FEnableLimits: Boolean;
    FMinTranslationLimit: Single;
    FMaxTranslationLimit: Single;

    FEnableMotor: Boolean;
    FMaxMotorForce: Single;
    FMotorSpeed: Single;

    procedure SetWorldAnchor(const AValue: TVector3);
    procedure SetWorldSliderAxis(const AValue: TVector3);
    procedure SetEnableLimits(const AValue: Boolean);
    procedure SetMinTranslationLimit(const AValue: Single);
    procedure SetMaxTranslationLimit(const AValue: Single);
    procedure SetEnableMotor(const AValue: Boolean);
    procedure SetMotorSpeed(const AValue: Single);
    function GetMotorSpeed: Single;
    procedure SetMaxMotorForce(const AValue: Single);
    procedure InternalUpdateGizmosName;

  strict protected
    function GetKraftJoint: TKraftConstraintJoint; override;
    procedure CreateKraftJoint; override;
    procedure DeinitializeKraftJoint; override;

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

    property WorldAnchor: TVector3 read FWorldAnchor write SetWorldAnchor;
    property WorldSliderAxis: TVector3 read FWorldSliderAxis write SetWorldSliderAxis;
  published
    property EnableLimits: Boolean read FEnableLimits write SetEnableLimits;
    property MinTranslationLimit: Single read FMinTranslationLimit write SetMinTranslationLimit;
    property MaxTranslationLimit: Single read FMaxTranslationLimit write SetMaxTranslationLimit;
    property EnableMotor: Boolean read FEnableMotor write SetEnableMotor;
    property MotorSpeed: Single read GetMotorSpeed write SetMotorSpeed;
    property MaxMotorForce: Single read FMaxMotorForce write SetMaxMotorForce;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlesliderjoint_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleFixedJoint --------------------------------------------------------------- }

procedure TCastleFixedJoint.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  if FAnchorTransform <> nil then
    TDesignJointAnchor(FAnchorTransform).Value := FAnchor;
  RecreateKraftJoint;
end;

procedure TCastleFixedJoint.InternalUpdateGizmosName;
begin
  if (FAnchorTransform <> nil) and (FAnchorTransform.Name <> Name + '_Anchor') then
  begin
    FAnchorTransform.Name := Name + '_Anchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleFixedJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointFixed;
end;

procedure TCastleFixedJoint.CreateKraftJoint;
begin
  FKraftJointFixed := TKraftConstraintJointFixed.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(Parent.LocalToWorld(FAnchor)),
    ConnectedCollide
  );
end;

procedure TCastleFixedJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointFixed);
end;

procedure TCastleFixedJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

constructor TCastleFixedJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FAnchor := CastleVectors.Vector3(0, 0, 0);
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlefixedjoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleFixedJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlefixedjoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastleFixedJoint.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleFixedJoint.InternalCreateGizmos;
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

procedure TCastleFixedJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastleFixedJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := FAnchorTransform <> nil;
end;

function TCastleFixedJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlefixedjoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastlePulleyJoint ---------------------------------------------------------------- }

procedure TCastlePulleyJoint.SetWorldAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldAnchor, AValue) then
    Exit;

  FWorldAnchor := AValue;

  if FWorldAnchorTransform <> nil then
    TDesignJointWorldAnchor(FWorldAnchorTransform).Value := AValue;

  RecreateKraftJoint;
end;

procedure TCastlePulleyJoint.SetConnectedWorldAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FConnectedWorldAnchor, AValue) then
    Exit;

  FConnectedWorldAnchor := AValue;

  if FConnectedWorldAnchorTransform <> nil then
    TDesignJointConnectedWorldAnchor(FConnectedWorldAnchorTransform).Value := AValue;

  RecreateKraftJoint;
end;

procedure TCastlePulleyJoint.SetWorldGroundAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldGroundAnchor, AValue) then
    Exit;

  FWorldGroundAnchor := AValue;

  if FWorldGroundAnchorTransform <> nil then
    TDesignJointWorldGroundAnchor(FWorldGroundAnchorTransform).Value := AValue;

  RecreateKraftJoint;
end;

procedure TCastlePulleyJoint.SetConnectedWorldGroundAnchor(const AValue: TVector3
  );
begin
  if TVector3.PerfectlyEquals(FConnectedWorldGroundAnchor, AValue) then
    Exit;

  FConnectedWorldGroundAnchor := AValue;

  if FConnectedWorldGroundAnchorTransform <> nil then
    TDesignJointConnectedWorldGroundAnchor(FConnectedWorldGroundAnchorTransform).Value := AValue;

  RecreateKraftJoint;
end;

procedure TCastlePulleyJoint.SetRatio(const AValue: Single);
begin
  if SameValue(FRatio, AValue) then
    Exit;

  FRatio := AValue;
  RecreateKraftJoint;
end;

procedure TCastlePulleyJoint.InternalUpdateGizmosName;
begin
  if (FWorldAnchorTransform <> nil) and
     (FWorldAnchorTransform.Name <> Name + '_WorldAnchor') then
  begin
    FWorldAnchorTransform.Name := Name + '_WorldAnchor';
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedWorldAnchorTransform <> nil) and
     (FConnectedWorldAnchorTransform.Name <> Name + '_ConnectedWorldAnchor') then
  begin
    FConnectedWorldAnchorTransform.Name := Name + '_ConnectedWorldAnchor';
    InternalCastleDesignInvalidate := true;
  end;

  if (FWorldGroundAnchorTransform <> nil) and
     (FWorldGroundAnchorTransform.Name <> Name + '_WorldGroundAnchor') then
  begin
    FWorldGroundAnchorTransform.Name := Name + '_WorldGroundAnchor';
    InternalCastleDesignInvalidate := true;
  end;

  if (FConnectedWorldGroundAnchorTransform <> nil) and
     (FConnectedWorldGroundAnchorTransform.Name <> Name + '_ConnectedWorldGroundAnchor') then
  begin
    FConnectedWorldGroundAnchorTransform.Name := Name + '_ConnectedWorldGroundAnchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastlePulleyJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointPulley;
end;

procedure TCastlePulleyJoint.CreateKraftJoint;
begin
  Assert(FKraftJointPulley = nil, 'Second joint initialization');
  FKraftJointPulley := TKraftConstraintJointPulley.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FWorldGroundAnchor),
    VectorToKraft(FConnectedWorldGroundAnchor),
    VectorToKraft(FWorldAnchor),
    VectorToKraft(FConnectedWorldAnchor),
    FRatio,
    ConnectedCollide
  );
end;

procedure TCastlePulleyJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointPulley);
end;

procedure TCastlePulleyJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

constructor TCastlePulleyJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;

  {FWorldAnchor := CastleVectors.Vector3(0, 0, 0);
  FWorldGroundAnchor := CastleVectors.Vector3(0, 0, 0);
  FConnectedWorldAnchor := CastleVectors.Vector3(0, 0, 0);
  FConnectedWorldGroundAnchor := CastleVectors.Vector3(0, 0, 0);}

  FWorldAnchor := CastleVectors.Vector3(-1, -1, 0);
  FWorldGroundAnchor := CastleVectors.Vector3(-1, 0, 0);
  FConnectedWorldAnchor := CastleVectors.Vector3(1, -1, 0);
  FConnectedWorldGroundAnchor := CastleVectors.Vector3(1, 0, 0);

  FRatio := 1.0;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlepulleyjoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastlePulleyJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlepulleyjoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastlePulleyJoint.WorldBeforeDetach;
begin
  FreeAndNil(FWorldAnchorTransform);
  FreeAndNil(FConnectedWorldAnchorTransform);
  FreeAndNil(FWorldGroundAnchorTransform);
  FreeAndNil(FConnectedWorldGroundAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastlePulleyJoint.InternalCreateGizmos;
begin
  inherited;
  if FWorldAnchorTransform = nil then
  begin
    FWorldAnchorTransform := TDesignJointWorldAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.World.Add(FWorldAnchorTransform);
    FWorldAnchorTransform.Translation := WorldAnchor;
    InternalCastleDesignInvalidate := true;
  end;

  if FConnectedWorldAnchorTransform = nil then
  begin
    FConnectedWorldAnchorTransform := TDesignJointConnectedWorldAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.World.Add(FConnectedWorldAnchorTransform);
    FConnectedWorldAnchorTransform.Translation := ConnectedWorldAnchor;
    InternalCastleDesignInvalidate := true;
  end;

  if FWorldGroundAnchorTransform = nil then
  begin
    FWorldGroundAnchorTransform := TDesignJointWorldGroundAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.World.Add(FWorldGroundAnchorTransform);
    FWorldGroundAnchorTransform.Translation := WorldGroundAnchor;
    InternalCastleDesignInvalidate := true;
  end;

  if FConnectedWorldGroundAnchorTransform = nil then
  begin
    FConnectedWorldGroundAnchorTransform := TDesignJointConnectedWorldGroundAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.World.Add(FConnectedWorldGroundAnchorTransform);
    FConnectedWorldGroundAnchorTransform.Translation := ConnectedWorldGroundAnchor;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastlePulleyJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FWorldAnchorTransform);
  FreeAndNil(FConnectedWorldAnchorTransform);
  FreeAndNil(FWorldGroundAnchorTransform);
  FreeAndNil(FConnectedWorldGroundAnchorTransform);
  InternalCastleDesignInvalidate := true;
  inherited;
end;

function TCastlePulleyJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := (FWorldAnchorTransform <> nil) or
    (FConnectedWorldAnchorTransform <> nil) or
    (FWorldGroundAnchorTransform <> nil) or
    (FConnectedWorldGroundAnchorTransform <> nil);
end;

function TCastlePulleyJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'WorldAnchorPersistent') or
     (PropertyName = 'ConnectedWorldAnchorPersistent') or
     (PropertyName = 'WorldGroundAnchorPersistent') or
     (PropertyName = 'ConnectedWorldGroundAnchorPersistent') or
     (PropertyName = 'Ratio') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlepulleyjoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleWorldPlaneDistanceJoint --------------------------------------------------- }

procedure TCastleWorldPlaneDistanceJoint.SetAnchor(const AValue: TVector3);
begin
  if TVector3.Equals(FAnchor, AValue) then
    Exit;

  FAnchor := AValue;
  RecreateKraftJoint;
end;

procedure TCastleWorldPlaneDistanceJoint.SetPlaneNormal(const AValue: TVector3);
begin
  if TVector3.Equals(FPlaneNormal, AValue) then
    Exit;

  FPlaneNormal := AValue;
  RecreateKraftJoint;
end;

procedure TCastleWorldPlaneDistanceJoint.SetPlaneDistance(const AValue: Single);
begin
  if SameValue(FPlaneDistance, AValue) then
    Exit;

  FPlaneDistance := AValue;
  RecreateKraftJoint;
end;

procedure TCastleWorldPlaneDistanceJoint.SetWorldDistance(const AValue: Single);
begin
  if SameValue(FWorldDistance, AValue) then
    Exit;

  FWorldDistance := AValue;
  if FKraftJointWorldPlaneDistance <> nil then
    FKraftJointWorldPlaneDistance.SetWorldDistance(FWorldDistance);
end;

procedure TCastleWorldPlaneDistanceJoint.SetFrequencyHz(const AValue: Single);
begin
  if SameValue(FFrequencyHz, AValue) then
    Exit;

  FFrequencyHz := AValue;
  RecreateKraftJoint;
end;

procedure TCastleWorldPlaneDistanceJoint.SetDampingRatio(const AValue: Single);
begin
  if SameValue(FDampingRatio, AValue) then
    Exit;

  FDampingRatio := AValue;
  RecreateKraftJoint;
end;

procedure TCastleWorldPlaneDistanceJoint.DoubleSidedWorldPlane(const AValue: Boolean);
begin
  if FDoubleSidedWorldPlane = AValue then
    Exit;

  FDoubleSidedWorldPlane := AValue;
  RecreateKraftJoint;
end;

procedure TCastleWorldPlaneDistanceJoint.InternalUpdateGizmosName;
begin
  if (FAnchorTransform <> nil) and
     (FAnchorTransform.Name <> Name + '_Anchor') then
  begin
    FAnchorTransform.Name := Name + '_Anchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleWorldPlaneDistanceJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointWorldPlaneDistance;
end;

procedure TCastleWorldPlaneDistanceJoint.CreateKraftJoint;
begin
  Assert(FKraftJointWorldPlaneDistance = nil, 'Second joint initialization');
  FKraftJointWorldPlaneDistance := TKraftConstraintJointWorldPlaneDistance.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    VectorToKraft(FAnchor),
    Plane(Vector3Norm(VectorToKraft(FPlaneNormal)), FPlaneDistance),
    FDoubleSidedWorldPlane,
    FWorldDistance,
    kclbLimitDistance,
    //kclbLimitMinimumDistance,
    //kclbLimitMaximumDistance,
    FFrequencyHz,
    FDampingRatio
  );
end;

procedure TCastleWorldPlaneDistanceJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointWorldPlaneDistance);
end;

procedure TCastleWorldPlaneDistanceJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

constructor TCastleWorldPlaneDistanceJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnchor := CastleVectors.Vector3(0, 0, 0);
  FPlaneNormal := CastleVectors.Vector3(0, 1, 0);
  NeedWorldChangeNotification := true;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleworldplanedistancejoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleWorldPlaneDistanceJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleworldplanedistancejoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastleWorldPlaneDistanceJoint.WorldBeforeDetach;
begin
  FreeAndNil(FAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleWorldPlaneDistanceJoint.InternalCreateGizmos;
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

procedure TCastleWorldPlaneDistanceJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FAnchorTransform);
  inherited;
end;

function TCastleWorldPlaneDistanceJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := FAnchorTransform <> nil;
end;

function TCastleWorldPlaneDistanceJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'AnchorPersistent') or
     (PropertyName = 'PlaneNormalPersistent') or
     (PropertyName = 'PlaneDistance') or
     (PropertyName = 'WorldDistance') or
     (PropertyName = 'FrequencyHz') or
     (PropertyName = 'DampingRatio') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleworldplanedistancejoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleSliderJoint --------------------------------------------------------- }

procedure TCastleSliderJoint.SetWorldAnchor(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldAnchor, AValue) then
    Exit;

  FWorldAnchor := AValue;
  if FWorldAnchorTransform <> nil then
    TDesignJointWorldAnchor(FWorldAnchorTransform).Value := FWorldAnchor;
  RecreateKraftJoint;
end;

procedure TCastleSliderJoint.SetWorldSliderAxis(const AValue: TVector3);
begin
  if TVector3.PerfectlyEquals(FWorldSliderAxis, AValue) then
    Exit;

  FWorldSliderAxis := AValue;
  RecreateKraftJoint;
end;

procedure TCastleSliderJoint.SetEnableLimits(const AValue: Boolean);
begin
  if FEnableLimits = AValue then
    Exit;

  FEnableLimits := AValue;

  if FKraftJointSlider <> nil then
    FKraftJointSlider.EnableLimit(AValue);
end;

procedure TCastleSliderJoint.SetMinTranslationLimit(const AValue: Single);
begin
  FMinTranslationLimit := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMinimumTranslationLimit(FMinTranslationLimit);
end;

procedure TCastleSliderJoint.SetMaxTranslationLimit(const AValue: Single);
begin
  FMaxTranslationLimit := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMaximumTranslationLimit(FMaxTranslationLimit);
end;

procedure TCastleSliderJoint.SetEnableMotor(const AValue: Boolean);
begin
  if FEnableMotor = AValue then
    Exit;

  FEnableMotor := AValue;

  if FKraftJointSlider <> nil then
    FKraftJointSlider.EnableMotor(AValue);
end;

procedure TCastleSliderJoint.SetMotorSpeed(const AValue: Single);
begin
  FMotorSpeed := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMotorSpeed(AValue);
end;

function TCastleSliderJoint.GetMotorSpeed: Single;
begin
  if FKraftJointSlider <> nil then
    FMotorSpeed := FKraftJointSlider.GetMotorSpeed;

  Result := FMotorSpeed;
end;

procedure TCastleSliderJoint.SetMaxMotorForce(const AValue: Single);
begin
  if SameValue(FMaxMotorForce, AValue) then
    Exit;

  FMaxMotorForce := AValue;
  if FKraftJointSlider <> nil then
    FKraftJointSlider.SetMaximalMotorForce(FMaxMotorForce);
end;

procedure TCastleSliderJoint.InternalUpdateGizmosName;
begin
  if (FWorldAnchorTransform <> nil) and
     (FWorldAnchorTransform.Name <> Name + '_WorldAnchor') then
  begin
    FWorldAnchorTransform.Name := Name + '_WorldAnchor';
    InternalCastleDesignInvalidate := true;
  end;
end;

function TCastleSliderJoint.GetKraftJoint: TKraftConstraintJoint;
begin
  Result := FKraftJointSlider;
end;

procedure TCastleSliderJoint.CreateKraftJoint;
begin
  FKraftJointSlider := TKraftConstraintJointSlider.Create(
    World.FKraftEngine,
    FParentRigidBody.FKraftBody,
    FConnectedRigidBody.FKraftBody,
    VectorToKraft(FWorldAnchor),
    VectorToKraft(FWorldSliderAxis),
    FEnableLimits,
    FEnableMotor,
    FMinTranslationLimit,
    FMaxTranslationLimit,
    GetMotorSpeed,
    FMaxMotorForce,
    ConnectedCollide
  );
end;

procedure TCastleSliderJoint.DeinitializeKraftJoint;
begin
  FreeAndNil(FKraftJointSlider);
end;

procedure TCastleSliderJoint.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  InternalUpdateGizmosName;
end;

constructor TCastleSliderJoint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NeedWorldChangeNotification := true;
  FWorldSliderAxis := CastleVectors.Vector3(0, 1, 0);
  FWorldAnchor := CastleVectors.Vector3(0, 0, 0);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlesliderjoint_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleSliderJoint.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlesliderjoint_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited Destroy;
end;

procedure TCastleSliderJoint.WorldBeforeDetach;
begin
  FreeAndNil(FWorldAnchorTransform);
  inherited WorldBeforeDetach;
end;

procedure TCastleSliderJoint.InternalCreateGizmos;
begin
  inherited;
  if FWorldAnchorTransform = nil then
  begin
    FWorldAnchorTransform := TDesignJointWorldAnchor.Create(nil, Self);
    InternalUpdateGizmosName;
    Parent.World.Add(FWorldAnchorTransform);
    TDesignJointWorldAnchor(FWorldAnchorTransform).Value := WorldAnchor;
    InternalCastleDesignInvalidate := true;
  end;
end;

procedure TCastleSliderJoint.InternalDestroyGizmos;
begin
  FreeAndNil(FWorldAnchorTransform);
  inherited;
end;

function TCastleSliderJoint.InternalCreateGizmosCreated: Boolean;
begin
  Result := FWorldAnchorTransform <> nil;
end;

function TCastleSliderJoint.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if (PropertyName = 'WorldAnchorPersistent') or
     (PropertyName = 'EnableLimits') or
     (PropertyName = 'MinTranslationLimit') or
     (PropertyName = 'MaxTranslationLimit') or
     (PropertyName = 'EnableMotor') or
     (PropertyName = 'MotorSpeed') or
     (PropertyName = 'MaxMotorForce') or
     (PropertyName = 'WorldSliderAxisPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlesliderjoint_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}

{$endif CASTLE_EXPERIMENTAL_JOINTS}
