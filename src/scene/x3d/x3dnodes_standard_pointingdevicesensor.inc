{%MainUnit x3dnodes.pas}
{
  Copyright 2007-2023 Michalis Kamburelis.

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
      X3D specification guarantees that all sensors within a single state
      have the same transform. }
    Transformation: TTransformation;

    function EnabledCount: Integer;
    function Enabled(Index: Integer): boolean;

    procedure Assign(Source: TPointingDeviceSensorList);
  end;

  { Base type for all pointing device sensors. }
  TAbstractPointingDeviceSensorNode = class(TAbstractSensorNode)
  protected
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
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
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

    { Called by events engine (like TCastleSceneCore) when you move your mouse
      over the @italic(active) drag sensor. }
    procedure Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3); virtual;

  {$I auto_generated_node_helpers/x3dnodes_x3ddragsensornode.inc}
  end;

  { Base type for all touch-style pointing device sensors. }
  TAbstractTouchSensorNode = class(TAbstractPointingDeviceSensorNode)
  strict private
    FOnClick: TNotifyEvent;
    procedure EventTouchTimeReceive(const Event: TX3DEvent; const AValue: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

    { Event generated when we register a "click" on the shapes within
      the same group as this TouchSensor node.
      The "click" definition corresponds to the X3D touchTime event generation.

      Note that, in order to register clicks, TCastleScene must
      be configured to process events (@link(TCastleSceneCore.ProcessEvents))
      and be able to detect clicks on particular shapes
      (@link(TCastleSceneCore.PreciseCollisions)).
      Usually you will initialize scene like this:

      @longCode(#
        MyScene := TCastleScene.Create(...);
        MyScene.Load(...); // load X3D graph with TouchSensor node
        MyScene.PreciseCollisions := true;
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
    procedure Activate(const Time: TX3DTime;
      const ATransform, AInvertedTransform: TMatrix4;
      const OverPoint: TVector3); override;
    procedure Deactivate(const Time: TX3DTime); override;
    procedure Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3); override;

  {$I auto_generated_node_helpers/x3dnodes_spheresensor.inc}
  end;

  { Sensor to catch clicks on 3D objects. }
  TTouchSensorNode = class(TAbstractTouchSensorNode)
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
  Transformation := Source.Transformation;
end;

{ TAbstractPointingDeviceSensorNode -------------------------------------------------------------- }

procedure TAbstractPointingDeviceSensorNode.GroupBeforeTraverse(
  const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
begin
  inherited;
  GroupBeforeTraverse_AddPointingDeviceSensor(Self, State, WasPointingDeviceSensor);
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

constructor TAbstractDragSensorNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FdEnabled.ChangeAlways := chDragSensorEnabled;
end;

procedure TAbstractDragSensorNode.Drag(const Time: TX3DTime; const RayOrigin, RayDirection: TVector3);
begin
  { Nothing to do in this class }
end;

constructor TAbstractTouchSensorNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  EventTouchTime.AddNotification({$ifdef FPC}@{$endif} EventTouchTimeReceive);
end;

procedure TAbstractTouchSensorNode.EventTouchTimeReceive(
  const Event: TX3DEvent; const AValue: TX3DField; const Time: TX3DTime);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
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

  OriginalIntersection.Y := 0;

  if Caps then
  begin
    { CapsPlane is parallel to Y=0 plane
      (transformed by current sensor transform and by axisRotation),
      and passing though PlaneOrigin. }
    CapsPlaneDir := PVector3(@CapsPlane);
    CapsPlaneDir^ := Transform.MultDirection(TVector3.One[1]);
    CapsPlane.W := -TVector3.DotProduct(CapsPlaneDir^, OverPoint);
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
    LocalIntersection.Y := 0;

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
    Plane.W := -TVector3.DotProduct(PlaneDir^, PlaneOrigin);

    { +X, +Y vectors, transformed by current sensor transform and
      by axisRotation, normalized. }
    PlaneX := Transform.MultDirection(TVector3.One[0]).Normalize;
    PlaneY := Transform.MultDirection(TVector3.One[1]).Normalize;
  except
    on ETransformedResultInvalid do
    begin
      { Transform matrix doesn't manage to transform directions.
        So just assume it's identity, nothing more sensible to do. }
      Plane := Vector4(0, 0, 1, -PlaneOrigin.Z);
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
      So instead of TVector3.DotProduct(V, PlaneX) just take V.X. }

    TranslationGlobal := Intersection - PlaneOrigin;

    { map TranslationGlobal to the plane local coord system
      (with axisRotation) }
    Translation.X := TVector3.DotProduct(TranslationGlobal, PlaneX);
    Translation.Y := TVector3.DotProduct(TranslationGlobal, PlaneY);
    Translation.Z := 0;

    Translation := Translation + FdOffset.Value;

    if FdMinPosition.Value.X <= FdMaxPosition.Value.X then
      ClampVar(Translation.X, FdMinPosition.Value.X, FdMaxPosition.Value.X);
    if FdMinPosition.Value.Y <= FdMaxPosition.Value.Y then
      ClampVar(Translation.Y, FdMinPosition.Value.Y, FdMaxPosition.Value.Y);

    EventTranslation_changed.Send(Translation, Time);
    WasTranslation := true;
  end;
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
