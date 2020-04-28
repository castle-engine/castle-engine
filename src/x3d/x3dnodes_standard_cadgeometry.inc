{
  Copyright 2008-2018 Michalis Kamburelis.

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

    strict private FFdName: TSFString;
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

    strict private FFdName: TSFString;
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

    strict private FFdShape: TSFNode;
    public property FdShape: TSFNode read FFdShape;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
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

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdVisible: TMFBool;
    public property FdVisible: TMFBool read FFdVisible;

    {$I auto_generated_node_helpers/x3dnodes_cadlayer.inc}
  end;

  { Represents the location and faces that constitute a part in the CAD model. }
  TCADPartNode = class(TAbstractX3DGroupingNode, IAbstractX3DProductStructureChildNode, ITransformNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ApplyTransform(var Transformation: TTransformation); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    function TransformationChange: TNodeTransformationChange; override;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    strict private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    {$I auto_generated_node_helpers/x3dnodes_cadpart.inc}
  end;

  { Quads. }
  TIndexedQuadSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    {$ifndef CASTLE_SLIM_NODES}
    { Event in } { }
    strict private FEventSet_index: TMFInt32Event;
    public property EventSet_index: TMFInt32Event read FEventSet_index;
    {$endif}

    strict private FFdIndex: TMFInt32;
    public property FdIndex: TMFInt32 read FFdIndex;

    function CoordIndexField: TMFLong; override;

    procedure InternalCoordPolygons(
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

    procedure InternalCoordPolygons(
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

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  DefaultContainerField := 'children';
end;

procedure TCADAssemblyNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  DefaultContainerField := 'children';
end;

class function TCADAssemblyNode.ClassX3DType: string;
begin
  Result := 'CADAssembly';
end;

function TCADAssemblyNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TCADFaceNode.CreateNode;
begin
  inherited;

  FFdShape := TSFNode.Create(Self, true, 'shape', [TAbstractShapeNode, TAbstractLODNode]);
  AddField(FFdShape);

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', Vector3(0, 0, 0));
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf, Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0, Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TCADFaceNode.ClassX3DType: string;
begin
  Result := 'CADFace';
end;

function TCADFaceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdShape.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TCADLayerNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdVisible := TMFBool.Create(Self, true, 'visible', []);
  AddField(FFdVisible);

  DefaultContainerField := 'children';
end;

class function TCADLayerNode.ClassX3DType: string;
begin
  Result := 'CADLayer';
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

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
   FdCenter.ChangeAlways := chTransform;
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chTransform;
  AddField(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, true, 'scale', Vector3(1, 1, 1));
   FdScale.ChangeAlways := chTransform;
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, true, 'scaleOrientation', Vector3(0, 0, 1), 0);
   FdScaleOrientation.ChangeAlways := chTransform;
  AddField(FFdScaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chTransform;
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TCADPartNode.ClassX3DType: string;
begin
  Result := 'CADPart';
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

procedure TCADPartNode.ApplyTransform(var Transformation: TTransformation);
begin
  inherited;
  Transformation.Multiply(
    Center,
    Rotation,
    Scale,
    ScaleOrientation,
    Translation);
end;

procedure TIndexedQuadSetNode.CreateNode;
begin
  inherited;

  {$ifndef CASTLE_SLIM_NODES}
  FEventSet_index := TMFInt32Event.Create(Self, 'set_index', true);
  AddEvent(FEventSet_index);
  {$endif}

  FFdIndex := TMFInt32.Create(Self, false, 'index', []);
   FdIndex.ChangeAlways := chGeometry;
  AddField(FFdIndex);
  { X3D specification comment: [0,Inf) }
end;

class function TIndexedQuadSetNode.ClassX3DType: string;
begin
  Result := 'IndexedQuadSet';
end;

function TIndexedQuadSetNode.CoordIndexField: TMFLong;
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
