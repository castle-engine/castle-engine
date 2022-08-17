{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Abstract class for (most) nodes that are valid product structure children. }
  TAbstractProductStructureChildNode = class(TAbstractChildNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dproductstructurechildnode.inc}
  end;

  { Holds a set of assemblies or parts grouped together. }
  TCADAssemblyNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    {$I auto_generated_node_helpers/x3dnodes_cadassembly.inc}
  end;

  { Holds the geometry representing a face of a part. }
  TCADFaceNode = class(TAbstractProductStructureChildNode)
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
  TCADPartNode = class(TAbstractX3DGroupingNode)
  strict private
    type
      TCADPartNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: TCADPartNode;
      public
        constructor Create(const AParent: TCADPartNode);
        property Parent: TCADPartNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: TCADPartNodeTransformFunctionality;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
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

    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_indexedquadset.inc}
  end;

  { Quads, not indexed. }
  TQuadSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState;
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

{ TCADPartNode.TCADPartNodeTransformFunctionality ----------------------------------------- }

constructor TCADPartNode.TCADPartNodeTransformFunctionality.Create(const AParent: TCADPartNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure TCADPartNode.TCADPartNodeTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
begin
  inherited;
  Transformation.Multiply(
    Parent.Center,
    Parent.Rotation,
    Parent.Scale,
    Parent.ScaleOrientation,
    Parent.Translation);
end;

{ TCADPartNode --------------------------------------------------------------- }

constructor TCADPartNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformFunctionality := TCADPartNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
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
