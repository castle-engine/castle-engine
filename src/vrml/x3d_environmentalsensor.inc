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

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    { Event: SFTime, out } { }
    private FEvententerTime: TVRMLEvent;
    public property EvententerTime: TVRMLEvent read FEvententerTime;

    { Event: SFTime, out } { }
    private FEventexitTime: TVRMLEvent;
    public property EventexitTime: TVRMLEvent read FEventexitTime;
  end;

  TNodeProximitySensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    { Event: SFVec3f, out } { }
    private FEventcenterOfRotation_changed: TVRMLEvent;
    public property EventcenterOfRotation_changed: TVRMLEvent read FEventcenterOfRotation_changed;

    { Event: SFRotation, out } { }
    private FEventorientation_changed: TVRMLEvent;
    public property Eventorientation_changed: TVRMLEvent read FEventorientation_changed;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;
  end;

  TNodeTransformSensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtargetObject: TSFNode;
    public property FdtargetObject: TSFNode read FFdtargetObject;

    { Event: SFRotation, out } { }
    private FEventorientation_changed: TVRMLEvent;
    public property Eventorientation_changed: TVRMLEvent read FEventorientation_changed;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;
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

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   Fdcenter.ChangesAlways := [chEnvironmentalSensorBounds];
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdsize := TSFVec3f.Create(Self, 'size', Vector3Single(0, 0, 0));
   Fdsize.ChangesAlways := [chEnvironmentalSensorBounds];
  Fields.Add(FFdsize);
  { X3D specification comment: (-Inf,Inf) }

  FEvententerTime := TVRMLEvent.Create(Self, 'enterTime', TSFTime, false);
  Events.Add(FEvententerTime);

  FEventexitTime := TVRMLEvent.Create(Self, 'exitTime', TSFTime, false);
  Events.Add(FEventexitTime);

  DefaultContainerField := 'children';
end;

procedure TNodeProximitySensor.CreateNode;
begin
  inherited;

  FEventcenterOfRotation_changed := TVRMLEvent.Create(Self, 'centerOfRotation_changed', TSFVec3f, false);
  Events.Add(FEventcenterOfRotation_changed);

  FEventorientation_changed := TVRMLEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventorientation_changed);

  FEventposition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

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

  FFdtargetObject := TSFNode.Create(Self, 'targetObject', [TNodeX3DGroupingNode, TNodeX3DShapeNode]);
  Fields.Add(FFdtargetObject);

  FEventorientation_changed := TVRMLEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventorientation_changed);

  FEventposition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

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
