{
  Copyright 2008-2021 Michalis Kamburelis.

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
  TAbstractProductStructureChildNode = class(TAbstractChildNode, IAbstractX3DProductStructureChildNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dproductstructurechildnode.inc}
  end;

  { Holds a set of assemblies or parts grouped together. }
  TCADAssemblyNode = class(TAbstractX3DGroupingNode, IAbstractX3DProductStructureChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    {$I auto_generated_node_helpers/x3dnodes_cadassembly.inc}
  end;

  { Holds the geometry representing a face of a part. }
  TCADFaceNode = class(TAbstractProductStructureChildNode, IAbstractBoundedObject)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    {$I auto_generated_node_helpers/x3dnodes_cadface.inc}
  end;

  { Hierarchy of nodes used for showing layer structure for the CAD model. }
  TCADLayerNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    {$I auto_generated_node_helpers/x3dnodes_cadlayer.inc}
  end;

  { Represents the location and faces that constitute a part in the CAD model. }
  TCADPartNode = class(TAbstractX3DGroupingNode, IAbstractX3DProductStructureChildNode, ITransformNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ApplyTransform(var Transformation: TTransformation); override;
  public
    function TransformationChange: TNodeTransformationChange; override;
    {$I auto_generated_node_helpers/x3dnodes_cadpart.inc}
  end;

  { Quads. }
  TIndexedQuadSetNode = class(TAbstractComposedGeometryNode)
  public
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
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_quadset.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
function TCADAssemblyNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TCADFaceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdShape.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TCADLayerNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  for I := 0 to FdChildren.Count - 1 do
  begin
    Result := Func(Self, FdChildren[I]);
    if Result <> nil then Exit;
  end;
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

function TIndexedQuadSetNode.CoordIndexField: TMFLong;
begin
  Result := FdIndex;
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
