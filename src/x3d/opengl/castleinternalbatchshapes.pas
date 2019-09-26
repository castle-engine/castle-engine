{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Batch shapes (combine multiple shapes into one) (TBatchShapes). }
unit CastleInternalBatchShapes;

{$I castleconf.inc}

interface

uses CastleSceneInternalShape, CastleShapes, X3DNodes;

type
  TCreateShapeEvent = function(const AGeometry: TAbstractGeometryNode;
    const AState: TX3DGraphTraverseState;
    const ParentInfo: PTraversingInfo): TShape of object;

  TBatchShapes = class
  strict private
    FCollected: TShapeList;
    FWaitingToBeCollected, FMergeTarget: TGLShape;
    FPool: TShapeList;
    FPoolGeometries: TGroupNode;
    { Add Source into Target.
      You can assume that Target is one of our pool shapes,
      with initial state and geometry calculated
      by CreatePoolGeometry, CreatePoolState. }
    procedure Merge(const Target, Source: TGLShape);

    { Clear any possible leftovers from Merge, where a given shape was Target. }
    procedure ClearMerge(const Target: TGLShape);
  public
    constructor Create(const CreateShape: TCreateShapeEvent);
    destructor Destroy; override;

    { Merge given shape into the @link(Collected) shapes.
      During this, the shape may merge with another shape into a single, larger
      shape. Returns @true if the shape was added to @link(Collected),
      otherwise it was not, and should be rendered by the caller immediately
      without the help of batching. }
    function Collect(const Shape: TGLShape): Boolean;

    procedure Commit;

    { Currently collected shapes by @link(Collect).
      Call @link(Commit) before reading this. }
    property Collected: TShapeList read FCollected;

    { Release all shapes and clear the @link(Collected) list. }
    procedure FreeCollected;

    procedure GLContextClose;
  end;

implementation

uses SysUtils,
  X3DFields, CastleUtils, CastleLog;

constructor TBatchShapes.Create(const CreateShape: TCreateShapeEvent);

  procedure InitializePool;
  var
    I: Integer;
    ShapeNode: TShapeNode;
    Geometry: TIndexedFaceSetNode;
    State: TX3DGraphTraverseState;
    ParentInfo: TTraversingInfo;
    Shape: TShape;
  const
    PoolSize = 1;
  begin
    for I := 1 to PoolSize do
    begin
      // initialize Geometry and ShapeNode
      Geometry := TIndexedFaceSetNode.CreateWithShape(ShapeNode);
      Geometry.Coord := TCoordinateNode.Create;
      FPoolGeometries.AddChildren(ShapeNode);

      // initialize State
      State := TX3DGraphTraverseState.Create;
      State.ShapeNode := ShapeNode;

      // initialize ParentInfo
      ParentInfo.Node := ShapeNode;
      ParentInfo.ParentInfo := nil;

      // initialize Shape, add it to FPool
      Shape := CreateShape(Geometry, State, @ParentInfo);
      FPool.Add(Shape);
    end;
  end;

begin
  inherited Create;

  FCollected := TShapeList.Create;
  FCollected.OwnsObjects := false;

  FPoolGeometries := TGroupNode.Create;

  FPool := TShapeList.Create;
  FPool.OwnsObjects := true;

  InitializePool;
end;

destructor TBatchShapes.Destroy;
begin
  FreeAndNil(FCollected);
  FreeAndNil(FPool);
  FreeAndNil(FPoolGeometries);
  inherited;
end;

function TBatchShapes.Collect(const Shape: TGLShape): Boolean;
begin
  { We can only Merge geometries
    - with TIndexedFaceSetNode
    - from VRML 2 (with TShapeNode)

    TODO: Make sure Appearance matches.
    TODO: Make sure fog state matches.
  }
  Result :=
    (Shape.Node <> nil) and
    (Shape.Geometry(true) is TIndexedFaceSetNode);
  if not Result then
    Exit;

  { TODO: merging shapes with blending messes up their order,
    unless you merge them *all* into one shape (which is what we do now). }

  if FMergeTarget <> nil then
  begin
    // 3rd and subsequent shapes (FMergeTarget <> nil, FWaitingToBeCollected = nil)
    { Merge Shape into last FMergeTarget shape.
      TODO: In the future this should check into which (if any)
      FCollected shape we should merge. }
    Merge(FMergeTarget, Shape);
  end else
  if FWaitingToBeCollected <> nil then
  begin
    // 2nd shape (FMergeTarget was nil)
    FMergeTarget := FPool[0] as TGLShape; // for now, assume Pool[0] is available and suitable for merging
    FCollected.Add(FMergeTarget);
    ClearMerge(FMergeTarget);
    Merge(FMergeTarget, FWaitingToBeCollected);
    Merge(FMergeTarget, Shape);
    FWaitingToBeCollected := nil;
  end else
  begin
    // 1st shape (both FMergeTarget and FWaitingToBeCollected were nil)
    FWaitingToBeCollected := Shape;
  end;
end;

procedure TBatchShapes.FreeCollected;
begin
  FCollected.Clear;
  if FMergeTarget <> nil then
  begin
    // don't wait for ClearMerge for this, do this earlier to release reference count
    FMergeTarget.Node.Appearance := nil;
    FMergeTarget := nil;
  end;
end;

procedure TBatchShapes.Commit;
begin
  if FWaitingToBeCollected <> nil then
  begin
    FCollected.Add(FWaitingToBeCollected);
    FWaitingToBeCollected := nil;
  end;
  if FMergeTarget <> nil then
  begin
    // WritelnLog('Merged into %s with %d vertexes, %d indexes, %s bbox', [
    //   FMergeTarget.OriginalGeometry.NiceName,
    //   ((FMergeTarget.OriginalGeometry as TIndexedFaceSetNode).Coord as TCoordinateNode).FdPoint.Count,
    //   (FMergeTarget.OriginalGeometry as TIndexedFaceSetNode).FdCoordIndex.Count,
    //   FMergeTarget.BoundingBox.ToString
    // ]);

    { Mark changes from
      - TIndexedFaceSetNode.FdCoordIndex,
      - TCoordinateNode.FdPoint
    }
    FMergeTarget.Changed(false, [chCoordinate, chGeometry]);
  end;
end;

procedure TBatchShapes.GLContextClose;
var
  Shape: TShape;
begin
  for Shape in FPool do
    TGLShape(Shape).GLContextClose;
end;

procedure TBatchShapes.Merge(const Target, Source: TGLShape);
var
  StateTarget, StateSource: TX3DGraphTraverseState;
  MeshTarget, MeshSource: TIndexedFaceSetNode;
  CoordTarget, CoordSource: TMFVec3f;
  IndexTarget, IndexSource: TLongIntList;
  OldCoordCount, I: Integer;
begin
  StateTarget := Target.OriginalState;
  StateSource := Source.State(true);
  MeshTarget := Target.OriginalGeometry as TIndexedFaceSetNode;
  MeshSource := Source.Geometry(true) as TIndexedFaceSetNode;

  // no vertexes in source mesh, ignore it
  if MeshSource.Coord = nil then
    Exit;

  Assert(Source.Node <> nil); // only such source nodes are passed to Merge
  Target.Node.Appearance := Source.Node.Appearance;

  CoordTarget := MeshTarget.InternalCoordinates(StateTarget);
  CoordSource := MeshSource.InternalCoordinates(StateSource);
  OldCoordCount := CoordTarget.Count;
  CoordTarget.Items.AddRangeTransformed(CoordSource.Items, StateSource.Transform);

  IndexTarget := MeshTarget.FdCoordIndex.Items;
  IndexSource := MeshSource.FdCoordIndex.Items;
  if (IndexTarget.Count <> 0) and
     (IndexTarget.Last >= 0) then
    IndexTarget.Add(-1); // separate from next polygons
  for I := 0 to IndexSource.Count - 1 do
  begin
    if IndexSource[I] >= 0 then
      IndexTarget.Add(IndexSource[I] + OldCoordCount)
    else
      IndexTarget.Add(IndexSource[I]); // don't modify source negative indexes
  end;
end;

procedure TBatchShapes.ClearMerge(const Target: TGLShape);
var
  StateTarget: TX3DGraphTraverseState;
  MeshTarget: TIndexedFaceSetNode;
  CoordTarget: TMFVec3f;
  IndexTarget: TLongIntList;
begin
  StateTarget := Target.OriginalState;
  MeshTarget := Target.OriginalGeometry as TIndexedFaceSetNode;
  CoordTarget := MeshTarget.InternalCoordinates(StateTarget);
  CoordTarget.Items.Clear;
  IndexTarget := MeshTarget.FdCoordIndex.Items;
  IndexTarget.Clear;
end;

end.
