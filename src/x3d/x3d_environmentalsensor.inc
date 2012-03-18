{
  Copyright 2008-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractEnvironmentalSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdSize: TSFVec3f;
    public property FdSize: TSFVec3f read FFdSize;

    { Event: SFTime, out } { }
    private FEventEnterTime: TX3DEvent;
    public property EventEnterTime: TX3DEvent read FEventEnterTime;

    { Event: SFTime, out } { }
    private FEventExitTime: TX3DEvent;
    public property EventExitTime: TX3DEvent read FEventExitTime;
  end;

  TProximitySensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    { Event: SFVec3f, out } { }
    private FEventCenterOfRotation_changed: TX3DEvent;
    public property EventCenterOfRotation_changed: TX3DEvent read FEventCenterOfRotation_changed;

    { Event: SFRotation, out } { }
    private FEventOrientation_changed: TX3DEvent;
    public property EventOrientation_changed: TX3DEvent read FEventOrientation_changed;

    { Event: SFVec3f, out } { }
    private FEventPosition_changed: TX3DEvent;
    public property EventPosition_changed: TX3DEvent read FEventPosition_changed;
  end;

  TTransformSensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTargetObject: TSFNode;
    public property FdTargetObject: TSFNode read FFdTargetObject;

    { Event: SFRotation, out } { }
    private FEventOrientation_changed: TX3DEvent;
    public property EventOrientation_changed: TX3DEvent read FEventOrientation_changed;

    { Event: SFVec3f, out } { }
    private FEventPosition_changed: TX3DEvent;
    public property EventPosition_changed: TX3DEvent read FEventPosition_changed;
  end;

  TVisibilitySensorNode = class(TAbstractEnvironmentalSensorNode)
  private
    FIsActive: boolean;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;
    function Box: TBox3D;

    property IsActive: boolean read FIsActive;
    procedure SetIsActive(const Value: boolean; const Time: TX3DTime);
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractEnvironmentalSensorNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chEnvironmentalSensorBounds];
  Fields.Add(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdSize := TSFVec3f.Create(Self, 'size', Vector3Single(0, 0, 0));
   FdSize.ChangesAlways := [chEnvironmentalSensorBounds];
  Fields.Add(FFdSize);
  { X3D specification comment: (-Inf,Inf) }

  FEventEnterTime := TX3DEvent.Create(Self, 'enterTime', TSFTime, false);
  Events.Add(FEventEnterTime);

  FEventExitTime := TX3DEvent.Create(Self, 'exitTime', TSFTime, false);
  Events.Add(FEventExitTime);

  DefaultContainerField := 'children';
end;

procedure TProximitySensorNode.CreateNode;
begin
  inherited;

  FEventCenterOfRotation_changed := TX3DEvent.Create(Self, 'centerOfRotation_changed', TSFVec3f, false);
  Events.Add(FEventCenterOfRotation_changed);

  FEventOrientation_changed := TX3DEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventOrientation_changed);

  FEventPosition_changed := TX3DEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventPosition_changed);

  DefaultContainerField := 'children';
end;

class function TProximitySensorNode.ClassNodeTypeName: string;
begin
  Result := 'ProximitySensor';
end;

class function TProximitySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TProximitySensorNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcProximitySensor;
end;

procedure TTransformSensorNode.CreateNode;
begin
  inherited;

  FFdTargetObject := TSFNode.Create(Self, 'targetObject', [TAbstractX3DGroupingNode, TAbstractShapeNode]);
  Fields.Add(FFdTargetObject);

  FEventOrientation_changed := TX3DEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventOrientation_changed);

  FEventPosition_changed := TX3DEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventPosition_changed);

  DefaultContainerField := 'children';
end;

class function TTransformSensorNode.ClassNodeTypeName: string;
begin
  Result := 'TransformSensor';
end;

class function TTransformSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TVisibilitySensorNode.CreateNode;
begin
  inherited;
  DefaultContainerField := 'children';
end;

class function TVisibilitySensorNode.ClassNodeTypeName: string;
begin
  Result := 'VisibilitySensor';
end;

class function TVisibilitySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
    OnWarning(wtMajor, 'VRML/X3D', 'All VisibilitySensor.size components must be >= 0');
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
