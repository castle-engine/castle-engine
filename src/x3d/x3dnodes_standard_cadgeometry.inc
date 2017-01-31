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
  { Interface for nodes that are valid product structure children. }
  IAbstractX3DProductStructureChildNode = interface(IAbstractChildNode)
  ['{318249E0-BACF-4F16-9E49-3E912F24E0AA}']
    property FdName: TSFString { read FFdName} { };
  end;

  { Abstract class for (most) nodes that are valid product structure children. }
  TAbstractProductStructureChildNode = class(TAbstractChildNode,
    IAbstractX3DProductStructureChildNode)
  public
    procedure CreateNode; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    {$I auto_generated_node_helpers/x3dnodes_x3dproductstructurechildnode.inc}
  end;

  { Holds a set of assemblies or parts grouped together. }
  TCADAssemblyNode = class(TAbstractX3DGroupingNode, IAbstractX3DProductStructureChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    {$I auto_generated_node_helpers/x3dnodes_cadassembly.inc}
  end;

  { Holds the geometry representing a face of a part. }
  TCADFaceNode = class(TAbstractProductStructureChildNode, IAbstractBoundedObject)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdShape: TSFNode;
    public property FdShape: TSFNode read FFdShape;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_cadface.inc}
  end;

  { Hierarchy of nodes used for showing layer structure for the CAD model. }
  TCADLayerNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdVisible: TMFBool;
    public property FdVisible: TMFBool read FFdVisible;

    {$I auto_generated_node_helpers/x3dnodes_cadlayer.inc}
  end;

  { Represents the location and faces that constitute a part in the CAD model. }
  TCADPartNode = class(TAbstractX3DGroupingNode, IAbstractX3DProductStructureChildNode, ITransformNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ApplyTransform(State: TX3DGraphTraverseState); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    {$I auto_generated_node_helpers/x3dnodes_cadpart.inc}
  end;

  { Quads. }
  TIndexedQuadSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_index: TMFInt32Event;
    public property EventSet_index: TMFInt32Event read FEventSet_index;

    private FFdIndex: TMFInt32;
    public property FdIndex: TMFInt32 read FFdIndex;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_indexedquadset.inc}
  end;

  { Quads, not indexed. }
  TQuadSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_quadset.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractProductStructureChildNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  AddField(FFdName);

  DefaultContainerField := 'children';
end;

procedure TCADAssemblyNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  AddField(FFdName);

  DefaultContainerField := 'children';
end;

class function TCADAssemblyNode.ClassX3DType: string;
begin
  Result := 'CADAssembly';
end;

class function TCADAssemblyNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TCADAssemblyNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TCADFaceNode.CreateNode;
begin
  inherited;

  FFdShape := TSFNode.Create(Self, 'shape', [TAbstractShapeNode, TAbstractLODNode]);
  AddField(FFdShape);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf, Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0, Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TCADFaceNode.ClassX3DType: string;
begin
  Result := 'CADFace';
end;

class function TCADFaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TCADFaceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdShape.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TCADLayerNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  AddField(FFdName);

  FFdVisible := TMFBool.Create(Self, 'visible', []);
  AddField(FFdVisible);

  DefaultContainerField := 'children';
end;

class function TCADLayerNode.ClassX3DType: string;
begin
  Result := 'CADLayer';
end;

class function TCADLayerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TCADLayerNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  for I := 0 to FdChildren.Count - 1 do
    if (I >= FdVisible.Count) or FdVisible.Items[I] then
    begin
      Result := Func(Self, FdChildren[I]);
      if Result <> nil then Exit;
    end;
end;

procedure TCADPartNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chTransform];
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdName := TSFString.Create(Self, 'name', '');
  AddField(FFdName);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chTransform];
  AddField(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
   FdScale.ChangesAlways := [chTransform];
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdScaleOrientation.ChangesAlways := [chTransform];
  AddField(FFdScaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chTransform];
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TCADPartNode.ClassX3DType: string;
begin
  Result := 'CADPart';
end;

class function TCADPartNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TCADPartNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TCADPartNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure TCADPartNode.ApplyTransform(State: TX3DGraphTraverseState);
begin
  inherited;
  TransformState(State, Fdcenter.Value, Fdrotation.Value,
    Fdscale.Value, FdscaleOrientation.Value, Fdtranslation.Value);
end;

procedure TIndexedQuadSetNode.CreateNode;
begin
  inherited;

  FEventSet_index := TMFInt32Event.Create(Self, 'set_index', true);
  AddEvent(FEventSet_index);

  FFdIndex := TMFInt32.Create(Self, 'index', []);
   FdIndex.Exposed := false;
   FdIndex.ChangesAlways := [chGeometry];
  AddField(FFdIndex);
  { X3D specification comment: [0,Inf) }
end;

class function TIndexedQuadSetNode.ClassX3DType: string;
begin
  Result := 'IndexedQuadSet';
end;

class function TIndexedQuadSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TIndexedQuadSetNode.CoordIndex: TMFLong;
begin
  Result := FdIndex;
end;

procedure TQuadSetNode.CreateNode;
begin
  inherited;
end;

class function TQuadSetNode.ClassX3DType: string;
begin
  Result := 'QuadSet';
end;

class function TQuadSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterCADGeometryNodes;
begin
  NodesManager.RegisterNodeClasses([
    TCADAssemblyNode,
    TCADFaceNode,
    TCADLayerNode,
    TCADPartNode,
    TIndexedQuadSetNode,
    TQuadSetNode
  ]);
end;

{$endif read_implementation}
