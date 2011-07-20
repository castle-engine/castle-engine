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
  INodeX3DPickableObject = interface(IVRMLNode)
  ['{700E1CE8-97BD-44D2-A60F-D8639F4E72EC}']
    property FdobjectType: TMFString { read GetFdobjectType }; { }
    property Fdpickable: TSFBool { read GetFdpickable }; { }
  end;

  TNodeX3DPickSensorNode = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;

    private FFdobjectType: TMFString;
    public property FdobjectType: TMFString read FFdobjectType;

    private FFdpickingGeometry: TSFNode;
    public property FdpickingGeometry: TSFNode read FFdpickingGeometry;

    private FFdpickTarget: TMFNode;
    public property FdpickTarget: TMFNode read FFdpickTarget;

    { Event: MFNode, out } { }
    private FEventpickedGeometry: TVRMLEvent;
    public property EventpickedGeometry: TVRMLEvent read FEventpickedGeometry;

    private FFdintersectionType: TSFString;
    public property FdintersectionType: TSFString read FFdintersectionType;

    private FFdsortOrder: TSFString;
    public property FdsortOrder: TSFString read FFdsortOrder;
  end;

  TNodeLinePickSensor = class(TNodeX3DPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec3f, out } { }
    private FEventpickedNormal: TVRMLEvent;
    public property EventpickedNormal: TVRMLEvent read FEventpickedNormal;

    { Event: MFVec3f, out } { }
    private FEventpickedPoint: TVRMLEvent;
    public property EventpickedPoint: TVRMLEvent read FEventpickedPoint;

    { Event: MFVec3f, out } { }
    private FEventpickedTextureCoordinate: TVRMLEvent;
    public property EventpickedTextureCoordinate: TVRMLEvent read FEventpickedTextureCoordinate;
  end;

  TNodePickableGroup = class(TNodeX3DGroupingNode, INodeX3DPickableObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdobjectType: TMFString;
    public property FdobjectType: TMFString read FFdobjectType;

    private FFdpickable: TSFBool;
    public property Fdpickable: TSFBool read FFdpickable;
  end;

  TNodePointPickSensor = class(TNodeX3DPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec3f, out } { }
    private FEventpickedPoint: TVRMLEvent;
    public property EventpickedPoint: TVRMLEvent read FEventpickedPoint;
  end;

  TNodePrimitivePickSensor = class(TNodeX3DPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeVolumePickSensor = class(TNodeX3DPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DPickSensorNode.CreateNode;
begin
  inherited;

  FFdobjectType := TMFString.Create(Self, 'objectType', ['ALL']);
  Fields.Add(FFdobjectType);
  { X3D specification comment: ["ALL","NONE","TERRAIN",...] }

  FFdpickingGeometry := TSFNode.Create(Self, 'pickingGeometry', [TNodeX3DGeometryNode]);
  Fields.Add(FFdpickingGeometry);

  { X3D specification (edition 2) specifies X3DInlineNode as 3rd value on
    the list of allowed classes for "pickTarget". But there is no X3DInlineNode
    in the specification... So I just added TNodeInline there (it's the only
    Inline node for X3D anyway. }

  FFdpickTarget := TMFNode.Create(Self, 'pickTarget', [TNodeX3DGroupingNode, TNodeX3DShapeNode, TNodeInline]);
  Fields.Add(FFdpickTarget);

  FEventpickedGeometry := TVRMLEvent.Create(Self, 'pickedGeometry', TMFNode, false);
  Events.Add(FEventpickedGeometry);

  FFdintersectionType := TSFString.Create(Self, 'intersectionType', 'BOUNDS');
  FFdintersectionType.Exposed := false;
  Fields.Add(FFdintersectionType);
  { X3D specification comment: ["GEOMETRY"|"BOUNDS"] }

  FFdsortOrder := TSFString.Create(Self, 'sortOrder', 'CLOSEST');
  FFdsortOrder.Exposed := false;
  Fields.Add(FFdsortOrder);
  { X3D specification comment: ["ANY"|"CLOSEST"|"ALL"|"ALL_SORTED"] }

  DefaultContainerField := 'children';
end;

procedure TNodeLinePickSensor.CreateNode;
begin
  inherited;

  FEventpickedNormal := TVRMLEvent.Create(Self, 'pickedNormal', TMFVec3f, false);
  Events.Add(FEventpickedNormal);

  FEventpickedPoint := TVRMLEvent.Create(Self, 'pickedPoint', TMFVec3f, false);
  Events.Add(FEventpickedPoint);

  FEventpickedTextureCoordinate := TVRMLEvent.Create(Self, 'pickedTextureCoordinate', TMFVec3f, false);
  Events.Add(FEventpickedTextureCoordinate);

  DefaultContainerField := 'children';
end;

class function TNodeLinePickSensor.ClassNodeTypeName: string;
begin
  Result := 'LinePickSensor';
end;

class function TNodeLinePickSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePickableGroup.CreateNode;
begin
  inherited;

  FFdobjectType := TMFString.Create(Self, 'objectType', ['ALL']);
  Fields.Add(FFdobjectType);
  { X3D specification comment: ["ALL","NONE","TERRAIN",...] }

  FFdpickable := TSFBool.Create(Self, 'pickable', true);
  Fields.Add(FFdpickable);

  DefaultContainerField := 'children';
end;

class function TNodePickableGroup.ClassNodeTypeName: string;
begin
  Result := 'PickableGroup';
end;

class function TNodePickableGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePointPickSensor.CreateNode;
begin
  inherited;

  FEventpickedPoint := TVRMLEvent.Create(Self, 'pickedPoint', TMFVec3f, false);
  Events.Add(FEventpickedPoint);

  DefaultContainerField := 'children';
end;

class function TNodePointPickSensor.ClassNodeTypeName: string;
begin
  Result := 'PointPickSensor';
end;

class function TNodePointPickSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePrimitivePickSensor.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodePrimitivePickSensor.ClassNodeTypeName: string;
begin
  Result := 'PrimitivePickSensor';
end;

class function TNodePrimitivePickSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeVolumePickSensor.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeVolumePickSensor.ClassNodeTypeName: string;
begin
  Result := 'VolumePickSensor';
end;

class function TNodeVolumePickSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterPickingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeLinePickSensor,
    TNodePickableGroup,
    TNodePointPickSensor,
    TNodePrimitivePickSensor,
    TNodeVolumePickSensor
  ]);
end;

{$endif read_implementation}
