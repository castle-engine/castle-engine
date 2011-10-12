{
  Copyright 2008-2011 Michalis Kamburelis.

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

    { Event: MFNode, out } { }
    private FEventPickedGeometry: TX3DEvent;
    public property EventPickedGeometry: TX3DEvent read FEventPickedGeometry;

    private FFdIntersectionType: TSFString;
    public property FdIntersectionType: TSFString read FFdIntersectionType;

    private FFdSortOrder: TSFString;
    public property FdSortOrder: TSFString read FFdSortOrder;
  end;

  TLinePickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec3f, out } { }
    private FEventPickedNormal: TX3DEvent;
    public property EventPickedNormal: TX3DEvent read FEventPickedNormal;

    { Event: MFVec3f, out } { }
    private FEventPickedPoint: TX3DEvent;
    public property EventPickedPoint: TX3DEvent read FEventPickedPoint;

    { Event: MFVec3f, out } { }
    private FEventPickedTextureCoordinate: TX3DEvent;
    public property EventPickedTextureCoordinate: TX3DEvent read FEventPickedTextureCoordinate;
  end;

  TPickableGroupNode = class(TAbstractX3DGroupingNode, IAbstractPickableObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdObjectType: TMFString;
    public property FdObjectType: TMFString read FFdObjectType;

    private FFdPickable: TSFBool;
    public property FdPickable: TSFBool read FFdPickable;
  end;

  TPointPickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec3f, out } { }
    private FEventPickedPoint: TX3DEvent;
    public property EventPickedPoint: TX3DEvent read FEventPickedPoint;
  end;

  TPrimitivePickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TVolumePickSensorNode = class(TAbstractPickSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractPickSensorNode.CreateNode;
begin
  inherited;

  FFdObjectType := TMFString.Create(Self, 'objectType', ['ALL']);
  Fields.Add(FFdObjectType);
  { X3D specification comment: ["ALL","NONE","TERRAIN",...] }

  FFdPickingGeometry := TSFNode.Create(Self, 'pickingGeometry', [TAbstractX3DGeometryNode]);
  Fields.Add(FFdPickingGeometry);

  { X3D specification (edition 2) specifies X3DInlineNode as 3rd value on
    the list of allowed classes for "pickTarget". But there is no X3DInlineNode
    in the specification... So I just added TInlineNode there (it's the only
    Inline node for X3D anyway. }

  FFdPickTarget := TMFNode.Create(Self, 'pickTarget', [TAbstractX3DGroupingNode, TAbstractShapeNode, TInlineNode]);
  Fields.Add(FFdPickTarget);

  FEventPickedGeometry := TX3DEvent.Create(Self, 'pickedGeometry', TMFNode, false);
  Events.Add(FEventPickedGeometry);

  FFdIntersectionType := TSFString.Create(Self, 'intersectionType', 'BOUNDS');
  FFdIntersectionType.Exposed := false;
  Fields.Add(FFdIntersectionType);
  { X3D specification comment: ["GEOMETRY"|"BOUNDS"] }

  FFdSortOrder := TSFString.Create(Self, 'sortOrder', 'CLOSEST');
  FFdSortOrder.Exposed := false;
  Fields.Add(FFdSortOrder);
  { X3D specification comment: ["ANY"|"CLOSEST"|"ALL"|"ALL_SORTED"] }

  DefaultContainerField := 'children';
end;

procedure TLinePickSensorNode.CreateNode;
begin
  inherited;

  FEventPickedNormal := TX3DEvent.Create(Self, 'pickedNormal', TMFVec3f, false);
  Events.Add(FEventPickedNormal);

  FEventPickedPoint := TX3DEvent.Create(Self, 'pickedPoint', TMFVec3f, false);
  Events.Add(FEventPickedPoint);

  FEventPickedTextureCoordinate := TX3DEvent.Create(Self, 'pickedTextureCoordinate', TMFVec3f, false);
  Events.Add(FEventPickedTextureCoordinate);

  DefaultContainerField := 'children';
end;

class function TLinePickSensorNode.ClassNodeTypeName: string;
begin
  Result := 'LinePickSensor';
end;

class function TLinePickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPickableGroupNode.CreateNode;
begin
  inherited;

  FFdObjectType := TMFString.Create(Self, 'objectType', ['ALL']);
  Fields.Add(FFdObjectType);
  { X3D specification comment: ["ALL","NONE","TERRAIN",...] }

  FFdPickable := TSFBool.Create(Self, 'pickable', true);
  Fields.Add(FFdPickable);

  DefaultContainerField := 'children';
end;

class function TPickableGroupNode.ClassNodeTypeName: string;
begin
  Result := 'PickableGroup';
end;

class function TPickableGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPointPickSensorNode.CreateNode;
begin
  inherited;

  FEventPickedPoint := TX3DEvent.Create(Self, 'pickedPoint', TMFVec3f, false);
  Events.Add(FEventPickedPoint);

  DefaultContainerField := 'children';
end;

class function TPointPickSensorNode.ClassNodeTypeName: string;
begin
  Result := 'PointPickSensor';
end;

class function TPointPickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPrimitivePickSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TPrimitivePickSensorNode.ClassNodeTypeName: string;
begin
  Result := 'PrimitivePickSensor';
end;

class function TPrimitivePickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TVolumePickSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TVolumePickSensorNode.ClassNodeTypeName: string;
begin
  Result := 'VolumePickSensor';
end;

class function TVolumePickSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
