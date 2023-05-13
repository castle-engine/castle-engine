{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2023 Michalis Kamburelis.

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
  TCADAssemblyNode = class(TAbstractGroupingNode)
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
  TCADLayerNode = class(TAbstractGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    {$I auto_generated_node_helpers/x3dnodes_cadlayer.inc}
  end;

  { Represents the location and faces that constitute a part in the CAD model. }
  TCADPartNode = class(TAbstractGroupingNode)
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
  strict private
    FInternalTrianglesCoordIndex: TMFLong;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    destructor Destroy; override;

    { Internal TMFLong field that allows to treat the TQuadSetNode essentially like
      TIndexedTriangleSetNode. That is, these indexes have 3 items for each triangle,
      6 items for each quad. You can use them to index all the vertex coordinates
      and per-vertex attributes, and thus render TQuadSetNode just like you would render
      TIndexedTriangleSetNode. }
    function InternalTrianglesCoordIndex: TMFLong;

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

{ TQuadSetNode --------------------------------------------------------------- }

constructor TQuadSetNode.Create(const AX3DName: String = ''; const ABaseUrl: String = '');
begin
  inherited;
  FInternalTrianglesCoordIndex := TMFLong.Create(Self, false, 'internalTrianglesCoordIndex', []);
end;

destructor TQuadSetNode.Destroy;
begin
  FreeAndNil(FInternalTrianglesCoordIndex);
  inherited;
end;

function TQuadSetNode.InternalTrianglesCoordIndex: TMFLong;
var
  QuadsCount, OldQuadsCount, I: TListSize;
begin
  if FdCoord.Value is TCoordinateNode then
    QuadsCount := TCoordinateNode(FdCoord.Value).FdPoint.Items.Count div 4
  else
    QuadsCount := 0;

  if FInternalTrianglesCoordIndex.Count <> QuadsCount * 6 then
  begin
    OldQuadsCount := FInternalTrianglesCoordIndex.Count div 6;
    FInternalTrianglesCoordIndex.Count := QuadsCount * 6;
    for I := OldQuadsCount to QuadsCount - 1 do
    begin
      FInternalTrianglesCoordIndex.Items.L[I * 6    ] := I * 4;
      FInternalTrianglesCoordIndex.Items.L[I * 6 + 1] := I * 4 + 1;
      FInternalTrianglesCoordIndex.Items.L[I * 6 + 2] := I * 4 + 2;

      FInternalTrianglesCoordIndex.Items.L[I * 6 + 3] := I * 4;
      FInternalTrianglesCoordIndex.Items.L[I * 6 + 4] := I * 4 + 2;
      FInternalTrianglesCoordIndex.Items.L[I * 6 + 5] := I * 4 + 3;
    end;
  end;

  Result := FInternalTrianglesCoordIndex;
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
