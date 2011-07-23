{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DEnvironmentalSensorNode = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdSize: TSFVec3f;
    public property FdSize: TSFVec3f read FFdSize;

    { Event: SFTime, out } { }
    private FEventEnterTime: TVRMLEvent;
    public property EventEnterTime: TVRMLEvent read FEventEnterTime;

    { Event: SFTime, out } { }
    private FEventExitTime: TVRMLEvent;
    public property EventExitTime: TVRMLEvent read FEventExitTime;
  end;

  TNodeProximitySensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    { Event: SFVec3f, out } { }
    private FEventCenterOfRotation_changed: TVRMLEvent;
    public property EventCenterOfRotation_changed: TVRMLEvent read FEventCenterOfRotation_changed;

    { Event: SFRotation, out } { }
    private FEventOrientation_changed: TVRMLEvent;
    public property EventOrientation_changed: TVRMLEvent read FEventOrientation_changed;

    { Event: SFVec3f, out } { }
    private FEventPosition_changed: TVRMLEvent;
    public property EventPosition_changed: TVRMLEvent read FEventPosition_changed;
  end;

  TNodeTransformSensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTargetObject: TSFNode;
    public property FdTargetObject: TSFNode read FFdTargetObject;

    { Event: SFRotation, out } { }
    private FEventOrientation_changed: TVRMLEvent;
    public property EventOrientation_changed: TVRMLEvent read FEventOrientation_changed;

    { Event: SFVec3f, out } { }
    private FEventPosition_changed: TVRMLEvent;
    public property EventPosition_changed: TVRMLEvent read FEventPosition_changed;
  end;

  TNodeVisibilitySensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DEnvironmentalSensorNode.CreateNode;
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

  FEventEnterTime := TVRMLEvent.Create(Self, 'enterTime', TSFTime, false);
  Events.Add(FEventEnterTime);

  FEventExitTime := TVRMLEvent.Create(Self, 'exitTime', TSFTime, false);
  Events.Add(FEventExitTime);

  DefaultContainerField := 'children';
end;

procedure TNodeProximitySensor.CreateNode;
begin
  inherited;

  FEventCenterOfRotation_changed := TVRMLEvent.Create(Self, 'centerOfRotation_changed', TSFVec3f, false);
  Events.Add(FEventCenterOfRotation_changed);

  FEventOrientation_changed := TVRMLEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventOrientation_changed);

  FEventPosition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventPosition_changed);

  DefaultContainerField := 'children';
end;

class function TNodeProximitySensor.ClassNodeTypeName: string;
begin
  Result := 'ProximitySensor';
end;

class function TNodeProximitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeProximitySensor.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcProximitySensor;
end;

procedure TNodeTransformSensor.CreateNode;
begin
  inherited;

  FFdTargetObject := TSFNode.Create(Self, 'targetObject', [TNodeX3DGroupingNode, TNodeX3DShapeNode]);
  Fields.Add(FFdTargetObject);

  FEventOrientation_changed := TVRMLEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventOrientation_changed);

  FEventPosition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventPosition_changed);

  DefaultContainerField := 'children';
end;

class function TNodeTransformSensor.ClassNodeTypeName: string;
begin
  Result := 'TransformSensor';
end;

class function TNodeTransformSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeVisibilitySensor.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeVisibilitySensor.ClassNodeTypeName: string;
begin
  Result := 'VisibilitySensor';
end;

class function TNodeVisibilitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterEnvironmentalSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeProximitySensor,
    TNodeTransformSensor,
    TNodeVisibilitySensor
  ]);
end;

{$endif read_implementation}
