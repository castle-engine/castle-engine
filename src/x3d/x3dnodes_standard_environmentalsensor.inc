{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base type for all the environmental sensor nodes. }
  TAbstractEnvironmentalSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdSize: TSFVec3f;
    public property FdSize: TSFVec3f read FFdSize;

    { Event out } { }
    private FEventEnterTime: TSFTimeEvent;
    public property EventEnterTime: TSFTimeEvent read FEventEnterTime;

    { Event out } { }
    private FEventExitTime: TSFTimeEvent;
    public property EventExitTime: TSFTimeEvent read FEventExitTime;

    {$I auto_generated_node_helpers/x3dnodes_x3denvironmentalsensornode.inc}
  end;

  { Generate events when the viewer enters, exits, and/or moves within
    a region of 3D space. }
  TProximitySensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    { Event out } { }
    private FEventCenterOfRotation_changed: TSFVec3fEvent;
    public property EventCenterOfRotation_changed: TSFVec3fEvent read FEventCenterOfRotation_changed;

    { Event out } { }
    private FEventOrientation_changed: TSFRotationEvent;
    public property EventOrientation_changed: TSFRotationEvent read FEventOrientation_changed;

    { Event out } { }
    private FEventPosition_changed: TSFVec3fEvent;
    public property EventPosition_changed: TSFVec3fEvent read FEventPosition_changed;

    {$I auto_generated_node_helpers/x3dnodes_proximitysensor.inc}
  end;

  { Generates events when the target object
    enters, exits, and moves within a region of 3D space.
    @bold(Not implemented yet.) }
  TTransformSensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTargetObject: TSFNode;
    public property FdTargetObject: TSFNode read FFdTargetObject;

    { Event out } { }
    private FEventOrientation_changed: TSFRotationEvent;
    public property EventOrientation_changed: TSFRotationEvent read FEventOrientation_changed;

    { Event out } { }
    private FEventPosition_changed: TSFVec3fEvent;
    public property EventPosition_changed: TSFVec3fEvent read FEventPosition_changed;

    {$I auto_generated_node_helpers/x3dnodes_transformsensor.inc}
  end;

  { Detects visibility changes of a rectangular box as the user navigates the world. }
  TVisibilitySensorNode = class(TAbstractEnvironmentalSensorNode)
  private
    FIsActive: boolean;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;
    function Box: TBox3D;

    property IsActive: boolean read FIsActive;
    procedure SetIsActive(const Value: boolean; const Time: TX3DTime);

    {$I auto_generated_node_helpers/x3dnodes_visibilitysensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractEnvironmentalSensorNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chEnvironmentalSensorBounds];
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdSize := TSFVec3f.Create(Self, 'size', Vector3Single(0, 0, 0));
   FdSize.ChangesAlways := [chEnvironmentalSensorBounds];
  AddField(FFdSize);
  { X3D specification comment: (-Inf,Inf) }

  FEventEnterTime := TSFTimeEvent.Create(Self, 'enterTime', false);
  AddEvent(FEventEnterTime);

  FEventExitTime := TSFTimeEvent.Create(Self, 'exitTime', false);
  AddEvent(FEventExitTime);

  DefaultContainerField := 'children';
end;

procedure TProximitySensorNode.CreateNode;
begin
  inherited;

  FEventCenterOfRotation_changed := TSFVec3fEvent.Create(Self, 'centerOfRotation_changed', false);
  AddEvent(FEventCenterOfRotation_changed);

  FEventOrientation_changed := TSFRotationEvent.Create(Self, 'orientation_changed', false);
  AddEvent(FEventOrientation_changed);

  FEventPosition_changed := TSFVec3fEvent.Create(Self, 'position_changed', false);
  AddEvent(FEventPosition_changed);

  DefaultContainerField := 'children';
end;

class function TProximitySensorNode.ClassX3DType: string;
begin
  Result := 'ProximitySensor';
end;

class function TProximitySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TProximitySensorNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcProximitySensor;
end;

procedure TTransformSensorNode.CreateNode;
begin
  inherited;

  FFdTargetObject := TSFNode.Create(Self, 'targetObject', [TAbstractX3DGroupingNode, TAbstractShapeNode]);
  AddField(FFdTargetObject);

  FEventOrientation_changed := TSFRotationEvent.Create(Self, 'orientation_changed', false);
  AddEvent(FEventOrientation_changed);

  FEventPosition_changed := TSFVec3fEvent.Create(Self, 'position_changed', false);
  AddEvent(FEventPosition_changed);

  DefaultContainerField := 'children';
end;

class function TTransformSensorNode.ClassX3DType: string;
begin
  Result := 'TransformSensor';
end;

class function TTransformSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TVisibilitySensorNode.CreateNode;
begin
  inherited;
  DefaultContainerField := 'children';
end;

class function TVisibilitySensorNode.ClassX3DType: string;
begin
  Result := 'VisibilitySensor';
end;

class function TVisibilitySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TVisibilitySensorNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcVisibilitySensor;
end;

function TVisibilitySensorNode.Box: TBox3D;
begin
  if (FdSize.Value[0] < 0) or
     (FdSize.Value[1] < 0) or
     (FdSize.Value[2] < 0) then
  begin
    WritelnWarning('VRML/X3D', 'All VisibilitySensor.size components must be >= 0');
    Exit(EmptyBox3D);
  end;

  { In case of VisibilitySensor, specification doesn't say that size = 0
    indicates empty box (like in case of ProximitySensor).
    And this is sensible: flat (0 depth or such) VisibilitySensor
    makes sense. So for a valid VRML/X3D, this box is never EmptyBox3D. }

  Result := Box3DAroundPoint(FdCenter.Value, FdSize.Value);
end;

procedure TVisibilitySensorNode.SetIsActive(const Value: boolean; const Time: TX3DTime);
begin
  if FIsActive <> Value then
  begin
    FIsActive := Value;
    EventIsActive.Send(Value, Time);
    if Value then
      EventEnterTime.Send(Time.Seconds, Time) else
      EventExitTime.Send(Time.Seconds, Time);
  end;
end;

procedure RegisterEnvironmentalSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TProximitySensorNode,
    TTransformSensorNode,
    TVisibilitySensorNode
  ]);
end;

{$endif read_implementation}
