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
  { }
  IAbstractPickableObject = interface(IX3DNode)
  ['{700E1CE8-97BD-44D2-A60F-D8639F4E72EC}']
    property FdObjectType: TMFString { read GetFdobjectType }; { }
    property FdPickable: TSFBool { read GetFdpickable }; { }

    {$I auto_generated_node_helpers/x3dnodes_x3dpickableobject.inc}
  end;

  TAbstractPickSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;

    private FFdObjectType: TMFString;
    public property FdObjectType: TMFString read FFdObjectType;

    private FFdPickingGeometry: TSFNode;
    public property FdPickingGeometry: TSFNode read FFdPickingGeometry;

    private FFdPickTarget: TMFNode;
    public property FdPickTarget: TMFNode read FFdPickTarget;

    { Event out } { }
    private FEventPickedGeometry: TMFNodeEvent;
    public property EventPickedGeometry: TMFNodeEvent read FEventPickedGeometry;

    private FFdIntersectionType: TSFString;
    public property FdIntersectionType: TSFString read FFdIntersectionType;

    private FFdSortOrder: TSFString;
    public property FdSortOrder: TSFString read FFdSortOrder;

    {$I auto_generated_node_helpers/x3dnodes_x3dpicksensornode.inc}
  end;

  TLinePickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event out } { }
    private FEventPickedNormal: TMFVec3fEvent;
    public property EventPickedNormal: TMFVec3fEvent read FEventPickedNormal;

    { Event out } { }
    private FEventPickedPoint: TMFVec3fEvent;
    public property EventPickedPoint: TMFVec3fEvent read FEventPickedPoint;

    { Event out } { }
    private FEventPickedTextureCoordinate: TMFVec3fEvent;
    public property EventPickedTextureCoordinate: TMFVec3fEvent read FEventPickedTextureCoordinate;

    {$I auto_generated_node_helpers/x3dnodes_linepicksensor.inc}
  end;

  TPickableGroupNode = class(TAbstractX3DGroupingNode, IAbstractPickableObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdObjectType: TMFString;
    public property FdObjectType: TMFString read FFdObjectType;

    private FFdPickable: TSFBool;
    public property FdPickable: TSFBool read FFdPickable;

    {$I auto_generated_node_helpers/x3dnodes_pickablegroup.inc}
  end;

  TPointPickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event out } { }
    private FEventPickedPoint: TMFVec3fEvent;
    public property EventPickedPoint: TMFVec3fEvent read FEventPickedPoint;

    {$I auto_generated_node_helpers/x3dnodes_pointpicksensor.inc}
  end;

  TPrimitivePickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_primitivepicksensor.inc}
  end;

  TVolumePickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_volumepicksensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractPickSensorNode.CreateNode;
begin
  inherited;

  FFdObjectType := TMFString.Create(Self, 'objectType', ['ALL']);
  AddField(FFdObjectType);
  { X3D specification comment: ["ALL","NONE","TERRAIN",...] }

  FFdPickingGeometry := TSFNode.Create(Self, 'pickingGeometry', [TAbstractX3DGeometryNode]);
  AddField(FFdPickingGeometry);

  { X3D specification (edition 2) specifies X3DInlineNode as 3rd value on
    the list of allowed classes for "pickTarget". But there is no X3DInlineNode
    in the specification... So I just added TInlineNode there (it's the only
    Inline node for X3D anyway. }

  FFdPickTarget := TMFNode.Create(Self, 'pickTarget', [TAbstractX3DGroupingNode, TAbstractShapeNode, TInlineNode]);
  AddField(FFdPickTarget);

  FEventPickedGeometry := TMFNodeEvent.Create(Self, 'pickedGeometry', false);
  AddEvent(FEventPickedGeometry);

  FFdIntersectionType := TSFString.Create(Self, 'intersectionType', 'BOUNDS');
  FFdIntersectionType.Exposed := false;
  AddField(FFdIntersectionType);
  { X3D specification comment: ["GEOMETRY"|"BOUNDS"] }

  FFdSortOrder := TSFString.Create(Self, 'sortOrder', 'CLOSEST');
  FFdSortOrder.Exposed := false;
  AddField(FFdSortOrder);
  { X3D specification comment: ["ANY"|"CLOSEST"|"ALL"|"ALL_SORTED"] }

  DefaultContainerField := 'children';
end;

procedure TLinePickSensorNode.CreateNode;
begin
  inherited;

  FEventPickedNormal := TMFVec3fEvent.Create(Self, 'pickedNormal', false);
  AddEvent(FEventPickedNormal);

  FEventPickedPoint := TMFVec3fEvent.Create(Self, 'pickedPoint', false);
  AddEvent(FEventPickedPoint);

  FEventPickedTextureCoordinate := TMFVec3fEvent.Create(Self, 'pickedTextureCoordinate', false);
  AddEvent(FEventPickedTextureCoordinate);

  DefaultContainerField := 'children';
end;

class function TLinePickSensorNode.ClassX3DType: string;
begin
  Result := 'LinePickSensor';
end;

class function TLinePickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPickableGroupNode.CreateNode;
begin
  inherited;

  FFdObjectType := TMFString.Create(Self, 'objectType', ['ALL']);
  AddField(FFdObjectType);
  { X3D specification comment: ["ALL","NONE","TERRAIN",...] }

  FFdPickable := TSFBool.Create(Self, 'pickable', true);
  AddField(FFdPickable);

  DefaultContainerField := 'children';
end;

class function TPickableGroupNode.ClassX3DType: string;
begin
  Result := 'PickableGroup';
end;

class function TPickableGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPointPickSensorNode.CreateNode;
begin
  inherited;

  FEventPickedPoint := TMFVec3fEvent.Create(Self, 'pickedPoint', false);
  AddEvent(FEventPickedPoint);

  DefaultContainerField := 'children';
end;

class function TPointPickSensorNode.ClassX3DType: string;
begin
  Result := 'PointPickSensor';
end;

class function TPointPickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPrimitivePickSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TPrimitivePickSensorNode.ClassX3DType: string;
begin
  Result := 'PrimitivePickSensor';
end;

class function TPrimitivePickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TVolumePickSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TVolumePickSensorNode.ClassX3DType: string;
begin
  Result := 'VolumePickSensor';
end;

class function TVolumePickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterPickingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TLinePickSensorNode,
    TPickableGroupNode,
    TPointPickSensorNode,
    TPrimitivePickSensorNode,
    TVolumePickSensorNode
  ]);
end;

{$endif read_implementation}
