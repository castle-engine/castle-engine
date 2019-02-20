{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { List of pointing device sensors. Only nodes descending from
    X3DPointingDeviceSensorNode, and additionally an Anchor node. }
  TPointingDeviceSensorList = class(TX3DNodeList)
  public
    { Transformation (and inverse) of all the sensors on this list.
      VRML/X3D specification guarantees that all sensors within a single state
      have the same transform. }
    Transform, InvertedTransform: TMatrix4;

    function EnabledCount: Integer;
    function Enabled(Index: Integer): boolean;

    procedure Assign(Source: TPointingDeviceSensorList);
  end;

  { Base type for all pointing device sensors. }
  TAbstractPointingDeviceSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;

    strict private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    { Event out } { }
    strict private FEventIsOver: TSFBoolEvent;
    public property EventIsOver: TSFBoolEvent read FEventIsOver;

    { Activate pointing device sensor. Used by the events engine
      (like TCastleSceneCore) to notify this sensor.

      OverPoint indicates 3D point (in global, that is TCastleSceneCore, coordinates)
      pointed by the mouse.

      In TAbstractPointingDeviceSensorNode class,
      this simply sends isActive := TRUE event. }
    procedure Activate(const Time: TX3DTime;
      const ATransform, AInvertedTransform: TMatrix4;
      const OverPoint: TVector3); virtual;

    { Deactivate pointing device sensor. Used by the events engine
      (like TCastleSceneCore) to notify this sensor.

      In TAbstractPointingDeviceSensorNode class,
      this simply sends isActive := FALSE event. }
    procedure Deactivate(const Time: TX3DTime); virtual;

    {$I auto_generated_node_helpers/x3dnodes_x3dpointingdevicesensornode.inc}
  end;

  { Base type for all drag-style pointing device sensors. }
  TAbstractDragSensorNode = class(TAbstractPointingDeviceSensorNode)
  public
    procedure CreateNode; override;

    strict private FFdAutoOffset: TSFBool;
    public property FdAutoOffset: TSFBool read FFdAutoOffset;

    { Event out } { }
    strict private FEventTrackPoint_changed: TSFVec3fEvent;
    public property EventTrackPoint_changed: TSFVec3fEvent read FEventTrackPoint_changed;

    { Called by events engine (like TCastleSceneCore) when you move your mouse
      over the @italic(active) drag sensor. }
    procedure Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3); virtual;

    {$I auto_generated_node_helpers/x3dnodes_x3ddragsensornode.inc}
  end;

  { Base type for all touch-style pointing device sensors. }
  TAbstractTouchSensorNode = class(TAbstractPointingDeviceSensorNode)
  strict private
    FOnClick: TNotifyEvent;
    procedure EventTouchTimeReceive(Event: TX3DEvent; AValue: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;

    { Event out } { }
    strict private FEventTouchTime: TSFTimeEvent;
    public property EventTouchTime: TSFTimeEvent read FEventTouchTime;

    { Event generated when we register a "click" on the shapes within
      the same group as this TouchSensor node.
      The "click" definition corresponds to the X3D touchTime event generation.

      Note that, in order to register clicks, TCastleScene must
      be configured to process events (@link(TCastleSceneCore.ProcessEvents))
      and be able to detect clicks on particular shapes
      (@link(TCastleSceneCore.Spatial) should contain ssDynamicCollisions).
      Usually you will initialize scene like this:

      @longCode(#
        MyScene := TCastleScene.Create(...);
        MyScene.Load(...); // load X3D graph with TouchSensor node
        MyScene.Spatial := [ssRendering, ssDynamicCollisions];
        MyScene.ProcessEvents := true;
      #)
    }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    {$I auto_generated_node_helpers/x3dnodes_x3dtouchsensornode.inc}
  end;

  { Pointing device sensor to rotate objects around a constrained axis. }
  TCylinderSensorNode = class(TAbstractDragSensorNode)
  strict private
    { Are we in caps mode or cylinder mode (determined at activation) }
    Caps: boolean;
    { If Caps, then this is a plane parallel to Y=0 and coincident with
      original intersection, in global coordinates }
    CapsPlane: TVector4;

    { If not Caps, then these describe (infinitely tall) cylinder
      used for dragging, in global coordinates }
    CylinderAxisOrigin, CylinderAxis: TVector3;
    CylinderRadius: Single;

    { First intersection (at activation), in local coordinates,
      additionally with Y component set to 0. }
    OriginalIntersection: TVector3;

    { Transformation from global coords to the local sensor coords. }
    InvertedTransform: TMatrix4;

    { Was a rotation_changed send during this activation of sensor. }
    WasRotation: boolean;
    { Last value of rotation_changed send, meaningful only if WasRotation. }
    RotationAngle: Single;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdAxisRotation: TSFRotation;
    public property FdAxisRotation: TSFRotation read FFdAxisRotation;

    strict private FFdDiskAngle: TSFFloat;
    public property FdDiskAngle: TSFFloat read FFdDiskAngle;

    strict private FFdMaxAngle: TSFFloat;
    public property FdMaxAngle: TSFFloat read FFdMaxAngle;

    strict private FFdMinAngle: TSFFloat;
    public property FdMinAngle: TSFFloat read FFdMinAngle;

    strict private FFdOffset: TSFFloat;
    public property FdOffset: TSFFloat read FFdOffset;

    { Event out } { }
    strict private FEventRotation_changed: TSFRotationEvent;
    public property EventRotation_changed: TSFRotationEvent read FEventRotation_changed;

    procedure Activate(const Time: TX3DTime;
      const ATransform, AInvertedTransform: TMatrix4;
      const OverPoint: TVector3); override;
    procedure Deactivate(const Time: TX3DTime); override;
    procedure Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3); override;

    {$I auto_generated_node_helpers/x3dnodes_cylindersensor.inc}
  end;

  { Pointing device sensor to move objects on a plane. }
  TPlaneSensorNode = class(TAbstractDragSensorNode)
  strict private
    { Plane* stuff is in global (VRML scene) coordinates. }
    { Plane of the activated sensor. }
    Plane: TVector4;
    { Vectors indicating X and Y axis on the plane.
      Always normalized (do not really have to be strictly orthogonal,
      in case plane was transformed by some shearing; this is Ok). }
    PlaneX, PlaneY: TVector3;
    { Initial intersection point at activation. Always lies on Plane. }
    PlaneOrigin: TVector3;

    { Transformation from global coords to the local sensor coords. }
    InvertedTransform: TMatrix4;

    { Was a translation_changed send during this activation of sensor. }
    WasTranslation: boolean;
    { Last value of translation_changed send, meaningful only if WasTranslation. }
    Translation: TVector3;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdAxisRotation: TSFRotation;
    public property FdAxisRotation: TSFRotation read FFdAxisRotation;

    strict private FFdMaxPosition: TSFVec2f;
    public property FdMaxPosition: TSFVec2f read FFdMaxPosition;

    strict private FFdMinPosition: TSFVec2f;
    public property FdMinPosition: TSFVec2f read FFdMinPosition;

    strict private FFdOffset: TSFVec3f;
    public property FdOffset: TSFVec3f read FFdOffset;

    { Event out } { }
    strict private FEventTranslation_changed: TSFVec3fEvent;
    public property EventTranslation_changed: TSFVec3fEvent read FEventTranslation_changed;

    procedure Activate(const Time: TX3DTime;
      const ATransform, AInvertedTransform: TMatrix4;
      const OverPoint: TVector3); override;
    procedure Deactivate(const Time: TX3DTime); override;
    procedure Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3); override;

    {$I auto_generated_node_helpers/x3dnodes_planesensor.inc}
  end;

  { Pointing device sensor to rotate objects. }
  TSphereSensorNode = class(TAbstractDragSensorNode)
  strict private
    { Sphere center and radius, in world coordinates. }
    SphereCenter: TVector3;
    SphereRadius: Single;
    { First intersection (at activation), in local coordinates. }
    OriginalIntersection: TVector3;

    { Transformation from global coords to the local sensor coords. }
    InvertedTransform: TMatrix4;

    { Was a rotation_changed send during this activation of sensor. }
    WasRotation: boolean;
    { Last value of rotation_changed send, meaningful only if WasRotation. }
    Rotation: TVector4;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdOffset: TSFRotation;
    public property FdOffset: TSFRotation read FFdOffset;

    { Event out } { }
    strict private FEventRotation_changed: TSFRotationEvent;
    public property EventRotation_changed: TSFRotationEvent read FEventRotation_changed;

    procedure Activate(const Time: TX3DTime;
      const ATransform, AInvertedTransform: TMatrix4;
      const OverPoint: TVector3); override;
    procedure Deactivate(const Time: TX3DTime); override;
    procedure Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3); override;

    {$I auto_generated_node_helpers/x3dnodes_spheresensor.inc}
  end;

  { Sensor to catch clicks on 3D objects. }
  TTouchSensorNode = class(TAbstractTouchSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event out } { }
    strict private FEventHitNormal_changed: TSFVec3fEvent;
    public property EventHitNormal_changed: TSFVec3fEvent read FEventHitNormal_changed;

    { Event out } { }
    strict private FEventHitPoint_changed: TSFVec3fEvent;
    public property EventHitPoint_changed: TSFVec3fEvent read FEventHitPoint_changed;

    { Event out } { }
    strict private FEventHitTexCoord_changed: TSFVec2fEvent;
    public property EventHitTexCoord_changed: TSFVec2fEvent read FEventHitTexCoord_changed;

    {$I auto_generated_node_helpers/x3dnodes_touchsensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TPointingDeviceSensorList ------------------------------------------------- }

function TPointingDeviceSensorList.EnabledCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Enabled(I) then
      Inc(Result);
end;

function TPointingDeviceSensorList.Enabled(Index: Integer): boolean;
begin
  Result := (not (Items[Index] is TAbstractPointingDeviceSensorNode)) or
    TAbstractPointingDeviceSensorNode(Items[Index]).FdEnabled.Value;
end;

procedure TPointingDeviceSensorList.Assign(Source: TPointingDeviceSensorList);
begin
  inherited Assign(Source);
  Transform := Source.Transform;
  InvertedTransform := Source.InvertedTransform;
end;

{ Rest of nodes -------------------------------------------------------------- }

procedure TAbstractPointingDeviceSensorNode.CreateNode;
begin
  inherited;

  FFdDescription := TSFString.Create(Self, true, 'description', '');
  AddField(FFdDescription);

  FEventIsOver := TSFBoolEvent.Create(Self, 'isOver', false);
  AddEvent(FEventIsOver);
end;

procedure TAbstractPointingDeviceSensorNode.Activate(const Time: TX3DTime;
  const ATransform, AInvertedTransform: TMatrix4;
  const OverPoint: TVector3);
begin
  EventIsActive.Send(true, Time);
end;

procedure TAbstractPointingDeviceSensorNode.Deactivate(const Time: TX3DTime);
begin
  EventIsActive.Send(false, Time);
end;

procedure TAbstractDragSensorNode.CreateNode;
begin
  inherited;

  FFdAutoOffset := TSFBool.Create(Self, true, 'autoOffset', true);
  AddField(FFdAutoOffset);

  FEventTrackPoint_changed := TSFVec3fEvent.Create(Self, 'trackPoint_changed', false);
  AddEvent(FEventTrackPoint_changed);

  Fdenabled.ChangesAlways := FdEnabled.ChangesAlways + [chDragSensorEnabled];
end;

procedure TAbstractDragSensorNode.Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3);
begin
  { Nothing to do in this class }
end;

procedure TAbstractTouchSensorNode.CreateNode;
begin
  inherited;

  FEventTouchTime := TSFTimeEvent.Create(Self, 'touchTime', false);
   EventTouchTime.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventTouchTimeReceive);
  AddEvent(FEventTouchTime);
end;

procedure TAbstractTouchSensorNode.EventTouchTimeReceive(
  Event: TX3DEvent; AValue: TX3DField; const Time: TX3DTime);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCylinderSensorNode.CreateNode;
begin
  inherited;

  FFdAxisRotation := TSFRotation.Create(Self, true, 'axisRotation', Vector3(0, 1, 0), 0);
  AddField(FFdAxisRotation);

  FFdDiskAngle := TSFFloat.Create(Self, true, 'diskAngle', Pi/12);
  AddField(FFdDiskAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdMaxAngle := TSFFloat.Create(Self, true, 'maxAngle', -1);
  AddField(FFdMaxAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdMinAngle := TSFFloat.Create(Self, true, 'minAngle', 0);
  AddField(FFdMinAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdOffset := TSFFloat.Create(Self, true, 'offset', 0);
  AddField(FFdOffset);
  { X3D specification comment: (-Inf,Inf) }

  FEventRotation_changed := TSFRotationEvent.Create(Self, 'rotation_changed', false);
  AddEvent(FEventRotation_changed);

  DefaultContainerField := 'children';
end;

class function TCylinderSensorNode.ClassX3DType: string;
begin
  Result := 'CylinderSensor';
end;

class function TCylinderSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCylinderSensorNode.Activate(const Time: TX3DTime;
  const ATransform, AInvertedTransform: TMatrix4;
  const OverPoint: TVector3);
var
  Transform, M, IM: TMatrix4;
  CapsPlaneDir: PVector3;
begin
  inherited;

  Transform := ATransform;
  InvertedTransform := AInvertedTransform;

  { Do not apply rotation when it's 0. A small optimization for a common case. }
  if FdAxisRotation.RotationRad <> 0 then
  begin
    RotationMatricesRad(FdAxisRotation.RotationRad, FdAxisRotation.Axis, M, IM);
    Transform := Transform * M;
    InvertedTransform := IM * InvertedTransform;
  end;

  OriginalIntersection := InvertedTransform.MultPoint(OverPoint);
  Caps := AngleRadBetweenVectors(OriginalIntersection,
    Vector3(0, 1, 0)) < FdDiskAngle.Value;

  OriginalIntersection[1] := 0;

  if Caps then
  begin
    { CapsPlane is parallel to Y=0 plane
      (transformed by current sensor transform and by axisRotation),
      and passing though PlaneOrigin. }
    CapsPlaneDir := PVector3(@CapsPlane);
    CapsPlaneDir^ := Transform.MultDirection(TVector3.One[1]);
    CapsPlane[3] := -TVector3.DotProduct(CapsPlaneDir^, OverPoint);
  end else
  begin
    CylinderAxisOrigin := Transform.MultPoint(TVector3.Zero);
    CylinderAxis := Transform.MultDirection(TVector3.One[1]);
    CylinderRadius := Sqrt(PointToLineDistanceSqr(
      CylinderAxisOrigin, CylinderAxis, OverPoint));
  end;

  WasRotation := false;
end;

procedure TCylinderSensorNode.Deactivate(const Time: TX3DTime);
begin
  if FdAutoOffset.Value and WasRotation then
    FdOffset.Send(RotationAngle);

  inherited;
end;

procedure TCylinderSensorNode.Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3);
var
  Intersection, LocalIntersection: TVector3;
  WasIntersection: boolean;
begin
  inherited;

  if Caps then
    WasIntersection := TryPlaneRayIntersection(Intersection,
      CapsPlane, RayOrigin, RayDirection) else
    WasIntersection := TryCylinderRayIntersection(Intersection,
      CylinderAxisOrigin, CylinderAxis, CylinderRadius, RayOrigin, RayDirection);

  if WasIntersection then
  begin
    try
      LocalIntersection := InvertedTransform.MultPoint(Intersection);
    except
      on ETransformedResultInvalid do
      begin
        WritelnLog('Drag sensor', 'Sensor transformation matrix^-1 cannot transform points.');
        Exit;
      end;
    end;
    EventTrackPoint_changed.Send(LocalIntersection, Time);

    { both OriginalIntersection and LocalIntersection have Y component set to 0
      for the sake of calculating RotationAngle. They are both assumed to lie
      nicely on a CapsPlane. }
    LocalIntersection[1] := 0;

    RotationAngle := RotationAngleRadBetweenVectors(
      OriginalIntersection, LocalIntersection, TVector3.One[1]);

    { add offset, and clamp by min/maxAngle }
    RotationAngle := RotationAngle + FdOffset.Value;
    if FdMinAngle.Value <= FdMaxAngle.Value then
      ClampVar(RotationAngle, FdMinAngle.Value, FdMaxAngle.Value);

    EventRotation_changed.Send(Vector4(0, 1, 0, RotationAngle), Time);
    WasRotation := true;
  end;
end;

procedure TPlaneSensorNode.CreateNode;
begin
  inherited;

  FFdAxisRotation := TSFRotation.Create(Self, true, 'axisRotation', Vector3(0, 0, 1), 0);
  AddField(FFdAxisRotation);

  FFdMaxPosition := TSFVec2f.Create(Self, true, 'maxPosition', Vector2(-1, -1));
  AddField(FFdMaxPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdMinPosition := TSFVec2f.Create(Self, true, 'minPosition', Vector2(0, 0));
  AddField(FFdMinPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdOffset := TSFVec3f.Create(Self, true, 'offset', Vector3(0, 0, 0));
  AddField(FFdOffset);
  { X3D specification comment: (-Inf,Inf) }

  FEventTranslation_changed := TSFVec3fEvent.Create(Self, 'translation_changed', false);
  AddEvent(FEventTranslation_changed);

  DefaultContainerField := 'children';
end;

class function TPlaneSensorNode.ClassX3DType: string;
begin
  Result := 'PlaneSensor';
end;

class function TPlaneSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPlaneSensorNode.Activate(const Time: TX3DTime;
  const ATransform, AInvertedTransform: TMatrix4;
  const OverPoint: TVector3);
var
  PlaneDir: PVector3;
  Transform, M, IM: TMatrix4;
begin
  inherited;

  PlaneOrigin := OverPoint;

  Transform := ATransform;
  InvertedTransform := AInvertedTransform;

  { Do not apply rotation when it's 0. A small optimization for a common case. }
  if FdAxisRotation.RotationRad <> 0 then
  begin
    RotationMatricesRad(FdAxisRotation.RotationRad, FdAxisRotation.Axis, M, IM);
    Transform := Transform * M;
    InvertedTransform := IM * InvertedTransform;
  end;

  try
    { Plane is parallel to Z=0 plane
      (transformed by current sensor transform and by axisRotation),
      and passing though PlaneOrigin. }
    PlaneDir := PVector3(@Plane);
    PlaneDir^ := Transform.MultDirection(TVector3.One[2]);
    Plane[3] := -TVector3.DotProduct(PlaneDir^, PlaneOrigin);

    { +X, +Y vectors, transformed by current sensor transform and
      by axisRotation, normalized. }
    PlaneX := Transform.MultDirection(TVector3.One[0]).Normalize;
    PlaneY := Transform.MultDirection(TVector3.One[1]).Normalize;
  except
    on ETransformedResultInvalid do
    begin
      { Transform matrix doesn't manage to transform directions.
        So just assume it's identity, nothing more sensible to do. }
      Plane := Vector4(0, 0, 1, -PlaneOrigin[2]);
      PlaneX := TVector3.One[0];
      PlaneY := TVector3.One[1];
      Transform := TMatrix4.Identity;
      InvertedTransform := TMatrix4.Identity;
      WritelnLog('Drag sensor', 'Sensor transformation matrix cannot transform directions.');
    end;
  end;

  WasTranslation := false;
end;

procedure TPlaneSensorNode.Deactivate(const Time: TX3DTime);
begin
  if FdAutoOffset.Value and WasTranslation then
    { Hm, we don't pass Time below, instead Scene.NextEventTime will be used.
      But that's even better, since we guarantee no loops in Deactivate()
      calls? }
    FdOffset.Send(Translation);

  inherited;
end;

procedure TPlaneSensorNode.Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3);
var
  Intersection, TranslationGlobal: TVector3;
begin
  inherited;

  { If no intersection, it's Ok to do nothing? }

  if TryPlaneRayIntersection(Intersection, Plane, RayOrigin, RayDirection) then
  begin
    try
      { trackPoint_changed should be in the local sensor coordinates
        (with axisRotation), without worrying about offset
        (like for translation_changed).

        This is sensible:
        - track point should visualize the underlying geometry of the drag sensor
          (plane, in this case)
        - this makes track point sensible also for Sphere/CylinderSensor
          (where offset is for rotations, and cannot be applied to
          trackPoint_changed).

        About using local coordinates:
        FreeWRL and OpenVRML seems to also do this in sources,
        and Octaga and InstantReality behavior suggests they do it too.

        About using local coordinates with axisRotation:
        That's more difficult, see
        https://castle-engine.io/x3d_implementation_pointingdevicesensor.php }
      EventTrackPoint_changed.Send(
        InvertedTransform.MultPoint(Intersection), Time);
    except
      on ETransformedResultInvalid do
      begin
        EventTrackPoint_changed.Send(Intersection, Time);
        WritelnLog('Drag sensor', 'Sensor transformation matrix^-1 cannot transform points.');
      end;
    end;

    { An alternative implementation would transform Intersection to
      local coords (with axisRotation, and with origin at PointOrigin).

      Then applying the PointOrigin, PlaneX, PlaneY would be much simpler:
      PointOrigin is just zero, and PlaneX = just (1, 0, 0).
      So instead of TVector3.DotProduct(V, PlaneX) just take V[0]. }

    TranslationGlobal := Intersection - PlaneOrigin;

    { map TranslationGlobal to the plane local coord system
      (with axisRotation) }
    Translation[0] := TVector3.DotProduct(TranslationGlobal, PlaneX);
    Translation[1] := TVector3.DotProduct(TranslationGlobal, PlaneY);
    Translation[2] := 0;

    Translation := Translation + FdOffset.Value;

    if FdMinPosition.Value[0] <= FdMaxPosition.Value[0] then
      ClampVar(Translation.Data[0], FdMinPosition.Value[0], FdMaxPosition.Value[0]);
    if FdMinPosition.Value[1] <= FdMaxPosition.Value[1] then
      ClampVar(Translation.Data[1], FdMinPosition.Value[1], FdMaxPosition.Value[1]);

    EventTranslation_changed.Send(Translation, Time);
    WasTranslation := true;
  end;
end;

procedure TSphereSensorNode.CreateNode;
begin
  inherited;

  FFdOffset := TSFRotation.Create(Self, true, 'offset', Vector3(0, 1, 0), 0);
  AddField(FFdOffset);
  { X3D specification comment: [-1,1],(-Inf,Inf) }

  FEventRotation_changed := TSFRotationEvent.Create(Self, 'rotation_changed', false);
  AddEvent(FEventRotation_changed);

  DefaultContainerField := 'children';
end;

class function TSphereSensorNode.ClassX3DType: string;
begin
  Result := 'SphereSensor';
end;

class function TSphereSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TSphereSensorNode.Activate(const Time: TX3DTime;
  const ATransform, AInvertedTransform: TMatrix4;
  const OverPoint: TVector3);
begin
  inherited;

  InvertedTransform := AInvertedTransform;

  OriginalIntersection := AInvertedTransform.MultPoint(OverPoint);

  SphereCenter := ATransform.MultPoint(TVector3.Zero);
  SphereRadius := PointsDistance(OverPoint, SphereCenter);

  WasRotation := false;
end;

procedure TSphereSensorNode.Deactivate(const Time: TX3DTime);
begin
  if FdAutoOffset.Value and WasRotation then
    FdOffset.Send(Rotation);

  inherited;
end;

procedure TSphereSensorNode.Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3);
var
  Intersection, LocalIntersection, RotationAxis: TVector3;
  RotationAngle: Single;
  RotationQ: TQuaternion;
begin
  inherited;

  { If no intersection, it's Ok to do nothing? }

  if TrySphereRayIntersection(Intersection, SphereCenter, SphereRadius,
    RayOrigin, RayDirection) then
  begin
    try
      LocalIntersection := InvertedTransform.MultPoint(Intersection);
    except
      on ETransformedResultInvalid do
      begin
        WritelnLog('Drag sensor', 'Sensor transformation matrix^-1 cannot transform points.');
        Exit;
      end;
    end;
    EventTrackPoint_changed.Send(LocalIntersection, Time);

    { Rotation always contains offset }
    RotationQ := QuatFromAxisAngle(FdOffset.Axis, FdOffset.RotationRad);

    { Add to RotationQ rotation from OriginalIntersection to current. }
    RotationAxis := TVector3.CrossProduct(OriginalIntersection, LocalIntersection);
    if not RotationAxis.IsZero then
    begin
      RotationAngle := RotationAngleRadBetweenVectors(
        OriginalIntersection, LocalIntersection);
      RotationQ := QuatFromAxisAngle(
        RotationAxis.Normalize, RotationAngle) * RotationQ;
    end;

    Rotation := RotationQ.ToAxisAngle;

    EventRotation_changed.Send(Rotation, Time);
    WasRotation := true;
  end;
end;

procedure TTouchSensorNode.CreateNode;
begin
  inherited;

  FEventHitNormal_changed := TSFVec3fEvent.Create(Self, 'hitNormal_changed', false);
  AddEvent(FEventHitNormal_changed);

  FEventHitPoint_changed := TSFVec3fEvent.Create(Self, 'hitPoint_changed', false);
  AddEvent(FEventHitPoint_changed);

  FEventHitTexCoord_changed := TSFVec2fEvent.Create(Self, 'hitTexCoord_changed', false);
  AddEvent(FEventHitTexCoord_changed);

  DefaultContainerField := 'children';
end;

class function TTouchSensorNode.ClassX3DType: string;
begin
  Result := 'TouchSensor';
end;

class function TTouchSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterPointingDeviceSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TCylinderSensorNode,
    TPlaneSensorNode,
    TSphereSensorNode,
    TTouchSensorNode
  ]);
end;

{$endif read_implementation}
