{
  Copyright 2007-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { List of pointing device sensors. Only nodes descending from
    X3DPointingDeviceSensorNode, and additionally an Anchor node. }
  TPointingDeviceSensorList = class(TVRMLNodeList)
  public
    { Transformation (and inverse) of all the sensors on this list.
      VRML/X3D specification guarantees that all sensors within a single state
      have the same transform. }
    Transform, InvertedTransform: TMatrix4Single;

    function EnabledCount: Integer;
    function Enabled(Index: Integer): boolean;

    procedure Assign(Source: TPointingDeviceSensorList);
  end;

  { }
  TNodeX3DPointingDeviceSensorNode = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    { Event: SFBool, out } { }
    private FEventIsOver: TVRMLEvent;
    public property EventIsOver: TVRMLEvent read FEventIsOver;

    { Activate pointing device sensor. Used by the events engine
      (like TVRMLScene) to notify this sensor.

      OverPoint indicates 3D point (in global, that is TVRMLScene, coordinates)
      pointed by the mouse.

      In TNodeX3DPointingDeviceSensorNode class,
      this simply sends isActive := TRUE event. }
    procedure Activate(const Time: TVRMLTime;
      const ATransform, AInvertedTransform: TMatrix4Single;
      const OverPoint: TVector3Single); virtual;

    { Deactivate pointing device sensor. Used by the events engine
      (like TVRMLScene) to notify this sensor.

      In TNodeX3DPointingDeviceSensorNode class,
      this simply sends isActive := FALSE event. }
    procedure Deactivate(const Time: TVRMLTime); virtual;
  end;

  TNodeX3DDragSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    procedure CreateNode; override;

    private FFdAutoOffset: TSFBool;
    public property FdAutoOffset: TSFBool read FFdAutoOffset;

    { Event: SFVec3f, out } { }
    private FEventTrackPoint_changed: TVRMLEvent;
    public property EventTrackPoint_changed: TVRMLEvent read FEventTrackPoint_changed;

    { Called by events engine (like TVRMLScene) when you move your mouse
      over the @italic(active) drag sensor. }
    procedure Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single); virtual;
  end;

  TNodeX3DTouchSensorNode = class(TNodeX3DPointingDeviceSensorNode)
  public
    procedure CreateNode; override;

    { Event: SFTime, out } { }
    private FEventTouchTime: TVRMLEvent;
    public property EventTouchTime: TVRMLEvent read FEventTouchTime;
  end;

  TNodeCylinderSensor = class(TNodeX3DDragSensorNode)
  private
    { Are we in caps mode or cylinder mode (determined at activation) }
    Caps: boolean;
    { If Caps, then this is a plane parallel to Y=0 and coincident with
      original intersection, in global coordinates }
    CapsPlane: TVector4Single;

    { If not Caps, then these describe (infinitely tall) cylinder
      used for dragging, in global coordinates }
    CylinderAxisOrigin, CylinderAxis: TVector3Single;
    CylinderRadius: Single;

    { First intersection (at activation), in local coordinates,
      additionally with Y component set to 0. }
    OriginalIntersection: TVector3Single;

    { Transformation from global coords to the local sensor coords. }
    InvertedTransform: TMatrix4Single;

    { Was a rotation_changed send during this activation of sensor. }
    WasRotation: boolean;
    { Last value of rotation_changed send, meaningful only if WasRotation. }
    RotationAngle: Single;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAxisRotation: TSFRotation;
    public property FdAxisRotation: TSFRotation read FFdAxisRotation;

    private FFdDiskAngle: TSFFloat;
    public property FdDiskAngle: TSFFloat read FFdDiskAngle;

    private FFdMaxAngle: TSFFloat;
    public property FdMaxAngle: TSFFloat read FFdMaxAngle;

    private FFdMinAngle: TSFFloat;
    public property FdMinAngle: TSFFloat read FFdMinAngle;

    private FFdOffset: TSFFloat;
    public property FdOffset: TSFFloat read FFdOffset;

    { Event: SFRotation, out } { }
    private FEventRotation_changed: TVRMLEvent;
    public property EventRotation_changed: TVRMLEvent read FEventRotation_changed;

    procedure Activate(const Time: TVRMLTime;
      const ATransform, AInvertedTransform: TMatrix4Single;
      const OverPoint: TVector3Single); override;
    procedure Deactivate(const Time: TVRMLTime); override;
    procedure Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single); override;
  end;

  TNodePlaneSensor = class(TNodeX3DDragSensorNode)
  private
    { Plane* stuff is in global (VRML scene) coordinates. }
    { Plane of the activated sensor. }
    Plane: TVector4Single;
    { Vectors indicating X and Y axis on the plane.
      Always normalized (do not really have to be strictly orthogonal,
      in case plane was transformed by some shearing; this is Ok). }
    PlaneX, PlaneY: TVector3Single;
    { Initial intersection point at activation. Always lies on Plane. }
    PlaneOrigin: TVector3Single;

    { Transformation from global coords to the local sensor coords. }
    InvertedTransform: TMatrix4Single;

    { Was a translation_changed send during this activation of sensor. }
    WasTranslation: boolean;
    { Last value of translation_changed send, meaningful only if WasTranslation. }
    Translation: TVector3Single;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAxisRotation: TSFRotation;
    public property FdAxisRotation: TSFRotation read FFdAxisRotation;

    private FFdMaxPosition: TSFVec2f;
    public property FdMaxPosition: TSFVec2f read FFdMaxPosition;

    private FFdMinPosition: TSFVec2f;
    public property FdMinPosition: TSFVec2f read FFdMinPosition;

    private FFdOffset: TSFVec3f;
    public property FdOffset: TSFVec3f read FFdOffset;

    { Event: SFVec3f, out } { }
    private FEventTranslation_changed: TVRMLEvent;
    public property EventTranslation_changed: TVRMLEvent read FEventTranslation_changed;

    procedure Activate(const Time: TVRMLTime;
      const ATransform, AInvertedTransform: TMatrix4Single;
      const OverPoint: TVector3Single); override;
    procedure Deactivate(const Time: TVRMLTime); override;
    procedure Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single); override;
  end;

  TNodeSphereSensor = class(TNodeX3DDragSensorNode)
  private
    { Sphere center and radius, in world coordinates. }
    SphereCenter: TVector3Single;
    SphereRadius: Single;
    { First intersection (at activation), in local coordinates. }
    OriginalIntersection: TVector3Single;

    { Transformation from global coords to the local sensor coords. }
    InvertedTransform: TMatrix4Single;

    { Was a rotation_changed send during this activation of sensor. }
    WasRotation: boolean;
    { Last value of rotation_changed send, meaningful only if WasRotation. }
    Rotation: TVector4Single;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdOffset: TSFRotation;
    public property FdOffset: TSFRotation read FFdOffset;

    { Event: SFRotation, out } { }
    private FEventRotation_changed: TVRMLEvent;
    public property EventRotation_changed: TVRMLEvent read FEventRotation_changed;

    procedure Activate(const Time: TVRMLTime;
      const ATransform, AInvertedTransform: TMatrix4Single;
      const OverPoint: TVector3Single); override;
    procedure Deactivate(const Time: TVRMLTime); override;
    procedure Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single); override;
  end;

  TNodeTouchSensor = class(TNodeX3DTouchSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, out } { }
    private FEventHitNormal_changed: TVRMLEvent;
    public property EventHitNormal_changed: TVRMLEvent read FEventHitNormal_changed;

    { Event: SFVec3f, out } { }
    private FEventHitPoint_changed: TVRMLEvent;
    public property EventHitPoint_changed: TVRMLEvent read FEventHitPoint_changed;

    { Event: SFVec2f, out } { }
    private FEventHitTexCoord_changed: TVRMLEvent;
    public property EventHitTexCoord_changed: TVRMLEvent read FEventHitTexCoord_changed;
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
  Result := (not (Items[Index] is TNodeX3DPointingDeviceSensorNode)) or
    TNodeX3DPointingDeviceSensorNode(Items[Index]).FdEnabled.Value;
end;

procedure TPointingDeviceSensorList.Assign(Source: TPointingDeviceSensorList);
begin
  inherited;
  Transform := Source.Transform;
  InvertedTransform := Source.InvertedTransform;
end;

{ Rest of nodes -------------------------------------------------------------- }

procedure TNodeX3DPointingDeviceSensorNode.CreateNode;
begin
  inherited;

  FFdDescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFdDescription);

  FEventIsOver := TVRMLEvent.Create(Self, 'isOver', TSFBool, false);
  Events.Add(FEventIsOver);
end;

procedure TNodeX3DPointingDeviceSensorNode.Activate(const Time: TVRMLTime;
  const ATransform, AInvertedTransform: TMatrix4Single;
  const OverPoint: TVector3Single);
begin
  EventIsActive.Send(true, Time);
end;

procedure TNodeX3DPointingDeviceSensorNode.Deactivate(const Time: TVRMLTime);
begin
  EventIsActive.Send(false, Time);
end;

procedure TNodeX3DDragSensorNode.CreateNode;
begin
  inherited;

  FFdAutoOffset := TSFBool.Create(Self, 'autoOffset', true);
  Fields.Add(FFdAutoOffset);

  FEventTrackPoint_changed := TVRMLEvent.Create(Self, 'trackPoint_changed', TSFVec3f, false);
  Events.Add(FEventTrackPoint_changed);

  Fdenabled.ChangesAlways := FdEnabled.ChangesAlways + [chDragSensorEnabled];
end;

procedure TNodeX3DDragSensorNode.Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single);
begin
  { Nothing to do in this class }
end;

procedure TNodeX3DTouchSensorNode.CreateNode;
begin
  inherited;

  FEventTouchTime := TVRMLEvent.Create(Self, 'touchTime', TSFTime, false);
  Events.Add(FEventTouchTime);
end;

procedure TNodeCylinderSensor.CreateNode;
begin
  inherited;

  FFdAxisRotation := TSFRotation.Create(Self, 'axisRotation', Vector3Single(0, 1, 0), 0);
  Fields.Add(FFdAxisRotation);

  FFdDiskAngle := TSFFloat.Create(Self, 'diskAngle', Pi/12);
  Fields.Add(FFdDiskAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdMaxAngle := TSFFloat.Create(Self, 'maxAngle', -1);
  Fields.Add(FFdMaxAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdMinAngle := TSFFloat.Create(Self, 'minAngle', 0);
  Fields.Add(FFdMinAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdOffset := TSFFloat.Create(Self, 'offset', 0);
  Fields.Add(FFdOffset);
  { X3D specification comment: (-Inf,Inf) }

  FEventRotation_changed := TVRMLEvent.Create(Self, 'rotation_changed', TSFRotation, false);
  Events.Add(FEventRotation_changed);

  DefaultContainerField := 'children';
end;

class function TNodeCylinderSensor.ClassNodeTypeName: string;
begin
  Result := 'CylinderSensor';
end;

class function TNodeCylinderSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCylinderSensor.Activate(const Time: TVRMLTime;
  const ATransform, AInvertedTransform: TMatrix4Single;
  const OverPoint: TVector3Single);
var
  Transform, M, IM: TMatrix4Single;
  CapsPlaneDir: PVector3Single;
begin
  inherited;

  Transform := ATransform;
  InvertedTransform := AInvertedTransform;

  { Do not apply rotation when it's 0. A small optimization for a common case. }
  if FdAxisRotation.RotationRad <> 0 then
  begin
    RotationMatricesRad(FdAxisRotation.RotationRad, FdAxisRotation.Axis, M, IM);
    Transform := MatrixMult(Transform, M);
    InvertedTransform := MatrixMult(IM, InvertedTransform);
  end;

  OriginalIntersection := MatrixMultPoint(InvertedTransform, OverPoint);
  Caps := AngleRadBetweenVectors(OriginalIntersection,
    Vector3Single(0, 1, 0)) < FdDiskAngle.Value;

  OriginalIntersection[1] := 0;

  if Caps then
  begin
    { CapsPlane is parallel to Y=0 plane
      (transformed by current sensor transform and by axisRotation),
      and passing though PlaneOrigin. }
    CapsPlaneDir := PVector3Single(@CapsPlane);
    CapsPlaneDir^ := MatrixMultDirection(Transform, UnitVector3Single[1]);
    CapsPlane[3] := -VectorDotProduct(CapsPlaneDir^, OverPoint);
  end else
  begin
    CylinderAxisOrigin := MatrixMultPoint(Transform, ZeroVector3Single);
    CylinderAxis := MatrixMultDirection(Transform, UnitVector3Single[1]);
    CylinderRadius := Sqrt(PointToLineDistanceSqr(
      CylinderAxisOrigin, CylinderAxis, OverPoint));
  end;

  WasRotation := false;
end;

procedure TNodeCylinderSensor.Deactivate(const Time: TVRMLTime);
begin
  if FdAutoOffset.Value and WasRotation then
    FdOffset.Send(RotationAngle);

  inherited;
end;

procedure TNodeCylinderSensor.Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single);
var
  Intersection, LocalIntersection: TVector3Single;
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
      LocalIntersection := MatrixMultPoint(InvertedTransform, Intersection);
    except
      on ETransformedResultInvalid do
      begin
        if Log then
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
      OriginalIntersection, LocalIntersection, UnitVector3Single[1]);

    { add offset, and clamp by min/maxAngle }
    RotationAngle += FdOffset.Value;
    if FdMinAngle.Value <= FdMaxAngle.Value then
      Clamp(RotationAngle, FdMinAngle.Value, FdMaxAngle.Value);

    EventRotation_changed.Send(Vector4Single(0, 1, 0, RotationAngle), Time);
    WasRotation := true;
  end;
end;

procedure TNodePlaneSensor.CreateNode;
begin
  inherited;

  FFdAxisRotation := TSFRotation.Create(Self, 'axisRotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdAxisRotation);

  FFdMaxPosition := TSFVec2f.Create(Self, 'maxPosition', Vector2Single(-1, -1));
  Fields.Add(FFdMaxPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdMinPosition := TSFVec2f.Create(Self, 'minPosition', Vector2Single(0, 0));
  Fields.Add(FFdMinPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdOffset := TSFVec3f.Create(Self, 'offset', Vector3Single(0, 0, 0));
  Fields.Add(FFdOffset);
  { X3D specification comment: (-Inf,Inf) }

  FEventTranslation_changed := TVRMLEvent.Create(Self, 'translation_changed', TSFVec3f, false);
  Events.Add(FEventTranslation_changed);

  DefaultContainerField := 'children';
end;

class function TNodePlaneSensor.ClassNodeTypeName: string;
begin
  Result := 'PlaneSensor';
end;

class function TNodePlaneSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePlaneSensor.Activate(const Time: TVRMLTime;
  const ATransform, AInvertedTransform: TMatrix4Single;
  const OverPoint: TVector3Single);
var
  PlaneDir: PVector3Single;
  Transform, M, IM: TMatrix4Single;
begin
  inherited;

  PlaneOrigin := OverPoint;

  Transform := ATransform;
  InvertedTransform := AInvertedTransform;

  { Do not apply rotation when it's 0. A small optimization for a common case. }
  if FdAxisRotation.RotationRad <> 0 then
  begin
    RotationMatricesRad(FdAxisRotation.RotationRad, FdAxisRotation.Axis, M, IM);
    Transform := MatrixMult(Transform, M);
    InvertedTransform := MatrixMult(IM, InvertedTransform);
  end;

  try
    { Plane is parallel to Z=0 plane
      (transformed by current sensor transform and by axisRotation),
      and passing though PlaneOrigin. }
    PlaneDir := PVector3Single(@Plane);
    PlaneDir^ := MatrixMultDirection(Transform, UnitVector3Single[2]);
    Plane[3] := -VectorDotProduct(PlaneDir^, PlaneOrigin);

    { +X, +Y vectors, transformed by current sensor transform and
      by axisRotation, normalized. }
    PlaneX := Normalized(MatrixMultDirection(Transform, UnitVector3Single[0]));
    PlaneY := Normalized(MatrixMultDirection(Transform, UnitVector3Single[1]));
  except
    on ETransformedResultInvalid do
    begin
      { Transform matrix doesn't manage to transform directions.
        So just assume it's identity, nothing more sensible to do. }
      Plane := Vector4Single(0, 0, 1, -PlaneOrigin[2]);
      PlaneX := UnitVector3Single[0];
      PlaneY := UnitVector3Single[1];
      Transform := IdentityMatrix4Single;
      InvertedTransform := IdentityMatrix4Single;
      if Log then
        WritelnLog('Drag sensor', 'Sensor transformation matrix cannot transform directions.');
    end;
  end;

  WasTranslation := false;
end;

procedure TNodePlaneSensor.Deactivate(const Time: TVRMLTime);
begin
  if FdAutoOffset.Value and WasTranslation then
    FdOffset.Send(Translation);

  inherited;
end;

procedure TNodePlaneSensor.Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single);
var
  Intersection, TranslationGlobal: TVector3Single;
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
        http://vrmlengine.sourceforge.net/vrml_implementation_pointingdevicesensor.php }
      EventTrackPoint_changed.Send(
        MatrixMultPoint(InvertedTransform, Intersection), Time);
    except
      on ETransformedResultInvalid do
      begin
        EventTrackPoint_changed.Send(Intersection, Time);
        if Log then
          WritelnLog('Drag sensor', 'Sensor transformation matrix^-1 cannot transform points.');
      end;
    end;

    { An alternative implementation would transform Intersection to
      local coords (with axisRotation, and with origin at PointOrigin).

      Then applying the PointOrigin, PlaneX, PlaneY would be much simpler:
      PointOrigin is just zero, and PlaneX = just (1, 0, 0).
      So instead of VectorDotProduct(V, PlaneX) just take V[0]. }

    TranslationGlobal := Intersection - PlaneOrigin;

    { map TranslationGlobal to the plane local coord system
      (with axisRotation) }
    Translation[0] := VectorDotProduct(TranslationGlobal, PlaneX);
    Translation[1] := VectorDotProduct(TranslationGlobal, PlaneY);
    Translation[2] := 0;

    Translation += FdOffset.Value;

    if FdMinPosition.Value[0] <= FdMaxPosition.Value[0] then
      Clamp(Translation[0], FdMinPosition.Value[0], FdMaxPosition.Value[0]);
    if FdMinPosition.Value[1] <= FdMaxPosition.Value[1] then
      Clamp(Translation[1], FdMinPosition.Value[1], FdMaxPosition.Value[1]);

    EventTranslation_changed.Send(Translation, Time);
    WasTranslation := true;
  end;
end;

procedure TNodeSphereSensor.CreateNode;
begin
  inherited;

  FFdOffset := TSFRotation.Create(Self, 'offset', Vector3Single(0, 1, 0), 0);
  Fields.Add(FFdOffset);
  { X3D specification comment: [-1,1],(-Inf,Inf) }

  FEventRotation_changed := TVRMLEvent.Create(Self, 'rotation_changed', TSFRotation, false);
  Events.Add(FEventRotation_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSphereSensor.ClassNodeTypeName: string;
begin
  Result := 'SphereSensor';
end;

class function TNodeSphereSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeSphereSensor.Activate(const Time: TVRMLTime;
  const ATransform, AInvertedTransform: TMatrix4Single;
  const OverPoint: TVector3Single);
begin
  inherited;

  InvertedTransform := AInvertedTransform;

  OriginalIntersection := MatrixMultPoint(AInvertedTransform, OverPoint);

  SphereCenter := MatrixMultPoint(ATransform, ZeroVector3Single);
  SphereRadius := PointsDistance(OverPoint, SphereCenter);

  WasRotation := false;
end;

procedure TNodeSphereSensor.Deactivate(const Time: TVRMLTime);
begin
  if FdAutoOffset.Value and WasRotation then
    FdOffset.Send(Rotation);

  inherited;
end;

procedure TNodeSphereSensor.Drag(const Time: TVRMLTime; const RayOrigin, RayDirection: TVector3Single);
var
  Intersection, LocalIntersection, RotationAxis: TVector3Single;
  RotationAngle: Single;
  RotationQ: TQuaternion;
begin
  inherited;

  { If no intersection, it's Ok to do nothing? }

  if TrySphereRayIntersection(Intersection, SphereCenter, SphereRadius,
    RayOrigin, RayDirection) then
  begin
    try
      LocalIntersection := MatrixMultPoint(InvertedTransform, Intersection);
    except
      on ETransformedResultInvalid do
      begin
        if Log then
          WritelnLog('Drag sensor', 'Sensor transformation matrix^-1 cannot transform points.');
        Exit;
      end;
    end;
    EventTrackPoint_changed.Send(LocalIntersection, Time);

    { Rotation always contains offset }
    RotationQ := QuatFromAxisAngle(FdOffset.Axis, FdOffset.RotationRad);

    { Add to RotationQ rotation from OriginalIntersection to current. }
    RotationAxis := OriginalIntersection >< LocalIntersection;
    if not ZeroVector(RotationAxis) then
    begin
      RotationAngle := RotationAngleRadBetweenVectors(
        OriginalIntersection, LocalIntersection);
      RotationQ := QuatMultiply(QuatFromAxisAngle(
        Normalized(RotationAxis), RotationAngle), RotationQ);
    end;

    Rotation := QuatToAxisAngle(RotationQ);

    EventRotation_changed.Send(Rotation, Time);
    WasRotation := true;
  end;
end;

procedure TNodeTouchSensor.CreateNode;
begin
  inherited;

  FEventHitNormal_changed := TVRMLEvent.Create(Self, 'hitNormal_changed', TSFVec3f, false);
  Events.Add(FEventHitNormal_changed);

  FEventHitPoint_changed := TVRMLEvent.Create(Self, 'hitPoint_changed', TSFVec3f, false);
  Events.Add(FEventHitPoint_changed);

  FEventHitTexCoord_changed := TVRMLEvent.Create(Self, 'hitTexCoord_changed', TSFVec2f, false);
  Events.Add(FEventHitTexCoord_changed);

  DefaultContainerField := 'children';
end;

class function TNodeTouchSensor.ClassNodeTypeName: string;
begin
  Result := 'TouchSensor';
end;

class function TNodeTouchSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterPointingDeviceSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeCylinderSensor,
    TNodePlaneSensor,
    TNodeSphereSensor,
    TNodeTouchSensor
  ]);
end;

{$endif read_implementation}
