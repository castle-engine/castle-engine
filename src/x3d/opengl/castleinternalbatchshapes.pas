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

uses CastleSceneInternalShape, CastleShapes, X3DNodes, X3DFields;

type
  TCreateShapeEvent = function(const AGeometry: TAbstractGeometryNode;
    const AState: TX3DGraphTraverseState;
    const ParentInfo: PTraversingInfo): TShape of object;

  TMergePipeline = (mpNoTexCoord, mpTexCoord);

  TBatchShapes = class
  strict private
    FCollected: TShapeList;
    FWaitingToBeCollected, FMergeTarget, FPool: array [TMergePipeline] of TGLShape;
    FPoolGeometries: TGroupNode;
    { Add Source into Target.
      You can assume that Target is one of our pool shapes,
      with initial state and geometry calculated
      by CreatePoolGeometry, CreatePoolState. }
    procedure Merge(const Target, Source: TGLShape;
      const P: TMergePipeline);

    { Clear any possible leftovers from Merge, where a given shape was Target. }
    procedure ClearMerge(const Target: TGLShape;
      const P: TMergePipeline);

    { Similar to TAbstractGeometryNode.InternalCoordinates, but for tex coords. }
    class function TexCoordinates(
      const Geometry: TAbstractGeometryNode;
      const State: TX3DGraphTraverseState): TMFVec2f;
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
  CastleUtils, CastleLog;

constructor TBatchShapes.Create(const CreateShape: TCreateShapeEvent);

  procedure InitializePool;
  var
    ShapeNode: TShapeNode;
    Geometry: TIndexedFaceSetNode;
    State: TX3DGraphTraverseState;
    ParentInfo: TTraversingInfo;
    Shape: TGLShape;
    P: TMergePipeline;
  begin
    for P in TMergePipeline do
    begin
      // initialize Geometry and ShapeNode
      Geometry := TIndexedFaceSetNode.CreateWithShape(ShapeNode);
      Geometry.Coord := TCoordinateNode.Create;
      if P = mpTexCoord then
        Geometry.TexCoord := TTextureCoordinateNode.Create;
      FPoolGeometries.AddChildren(ShapeNode);

      // initialize State
      State := TX3DGraphTraverseState.Create;
      State.ShapeNode := ShapeNode;

      // initialize ParentInfo
      ParentInfo.Node := ShapeNode;
      ParentInfo.ParentInfo := nil;

      // initialize Shape, add it to FPool
      Shape := CreateShape(Geometry, State, @ParentInfo) as TGLShape;
      FPool[P] := Shape;
    end;
  end;

begin
  inherited Create;

  FCollected := TShapeList.Create;
  FCollected.OwnsObjects := false;

  FPoolGeometries := TGroupNode.Create;

  InitializePool;
end;

destructor TBatchShapes.Destroy;
var
  P: TMergePipeline;
begin
  FreeAndNil(FCollected);
  for P in TMergePipeline do
    FreeAndNil(FPool[P]);
  FreeAndNil(FPoolGeometries);
  inherited;
end;

function TBatchShapes.Collect(const Shape: TGLShape): Boolean;

  function CanCollectGeometry(const Geometry: TAbstractGeometryNode;
    out HasTexCoord: Boolean): Boolean;
  var
    TexCoord: TAbstractTextureCoordinateNode;
  begin
    Result := false;
    HasTexCoord := false;
    if not (Geometry is TIndexedFaceSetNode) then
      Exit;

    if TIndexedFaceSetNode(Geometry).FdTexCoordIndex.Count <> 0 then
      Exit; // for now we don't handle FdTexCoord

    TexCoord := TIndexedFaceSetNode(Geometry).TexCoord;
    if (TexCoord = nil) or
       (TexCoord is TTextureCoordinateNode) then
    begin
      Result := true;
      HasTexCoord := TexCoord <> nil;
    end;
  end;

var
  HasTexCoord: Boolean;
  P: TMergePipeline;
begin
  { We can only Merge geometries
    - with TIndexedFaceSetNode
    - from VRML 2 (with TShapeNode)

    TODO: Make sure Appearance matches.
    TODO: Make sure fog state matches.
  }
  Result :=
    (Shape.Node <> nil) and
    CanCollectGeometry(Shape.Geometry(true), HasTexCoord);
  if not Result then
    Exit;

  if HasTexCoord then
    P := mpTexCoord
  else
    P := mpNoTexCoord;

  { TODO: merging shapes with blending messes up their order,
    unless you merge them *all* into one shape (which is what we do now). }

  if FMergeTarget[P] <> nil then
  begin
    // 3rd and subsequent shapes (FMergeTarget <> nil, FWaitingToBeCollected = nil)
    { Merge Shape into last FMergeTarget shape.
      TODO: In the future this should check into which (if any)
      FCollected shape we should merge. }
    Merge(FMergeTarget[P], Shape, P);
  end else
  if FWaitingToBeCollected[P] <> nil then
  begin
    // 2nd shape (FMergeTarget was nil)
    FMergeTarget[P] := FPool[P];
    FCollected.Add(FMergeTarget[P]);
    ClearMerge(FMergeTarget[P], P);
    Merge(FMergeTarget[P], FWaitingToBeCollected[P], P);
    Merge(FMergeTarget[P], Shape, P);
    FWaitingToBeCollected[P] := nil;
  end else
  begin
    // 1st shape (both FMergeTarget and FWaitingToBeCollected were nil)
    FWaitingToBeCollected[P] := Shape;
  end;
end;

procedure TBatchShapes.FreeCollected;
var
  P: TMergePipeline;
begin
  FCollected.Clear;
  for P in TMergePipeline do
    if FMergeTarget[P] <> nil then
    begin
      // don't wait for ClearMerge for this, do this earlier to release reference count
      FMergeTarget[P].Node.Appearance := nil;
      FMergeTarget[P] := nil;
    end;
end;

procedure TBatchShapes.Commit;
var
  P: TMergePipeline;
begin
  for P in TMergePipeline do
  begin
    if FWaitingToBeCollected[P] <> nil then
    begin
      FCollected.Add(FWaitingToBeCollected[P]);
      FWaitingToBeCollected[P] := nil;
    end;
    if FMergeTarget[P] <> nil then
    begin
      // WritelnLog('Merged into %s with %d vertexes, %d indexes, %s bbox', [
      //   FMergeTarget[P].OriginalGeometry.NiceName,
      //   ((FMergeTarget[P].OriginalGeometry as TIndexedFaceSetNode).Coord as TCoordinateNode).FdPoint.Count,
      //   (FMergeTarget[P].OriginalGeometry as TIndexedFaceSetNode).FdCoordIndex.Count,
      //   FMergeTarget[P].BoundingBox.ToString
      // ]);

      { Mark changes from
        - TIndexedFaceSetNode.FdCoordIndex,
        - TCoordinateNode.FdPoint
      }
      FMergeTarget[P].Changed(false, [chCoordinate, chGeometry]);
    end;
  end;
end;

procedure TBatchShapes.GLContextClose;
var
  P: TMergePipeline;
begin
  for P in TMergePipeline do
    FPool[P].GLContextClose;
end;

procedure TBatchShapes.Merge(const Target, Source: TGLShape;
  const P: TMergePipeline);
var
  StateTarget, StateSource: TX3DGraphTraverseState;
  MeshTarget, MeshSource: TIndexedFaceSetNode;
  CoordTarget, CoordSource: TMFVec3f;
  TexCoordTarget, TexCoordSource: TMFVec2f;
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

  if P = mpTexCoord then
  begin
    TexCoordTarget := TexCoordinates(MeshTarget, StateTarget);
    TexCoordSource := TexCoordinates(MeshSource, StateSource);
    OldCoordCount := TexCoordTarget.Count;
    TexCoordTarget.Items.AddRange(TexCoordSource.Items);
  end;

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

procedure TBatchShapes.ClearMerge(const Target: TGLShape;
  const P: TMergePipeline);
var
  StateTarget: TX3DGraphTraverseState;
  MeshTarget: TIndexedFaceSetNode;
  CoordTarget: TMFVec3f;
  TexCoordTarget: TMFVec2f;
  IndexTarget: TLongIntList;
begin
  StateTarget := Target.OriginalState;
  MeshTarget := Target.OriginalGeometry as TIndexedFaceSetNode;
  CoordTarget := MeshTarget.InternalCoordinates(StateTarget);
  CoordTarget.Items.Clear;
  IndexTarget := MeshTarget.FdCoordIndex.Items;
  IndexTarget.Clear;
  if P = mpTexCoord then
  begin
    TexCoordTarget := TexCoordinates(MeshTarget, StateTarget);
    TexCoordTarget.Items.Clear;
  end;
end;

class function TBatchShapes.TexCoordinates(
  const Geometry: TAbstractGeometryNode;
  const State: TX3DGraphTraverseState): TMFVec2f;
var
  TexCoordNode: TX3DNode;
begin
  if Geometry.InternalTexCoord(State, TexCoordNode) and
     (TexCoordNode is TTextureCoordinateNode) then
  begin
    Result := TTextureCoordinateNode(TexCoordNode).FdPoint;
  end else
    raise Exception.CreateFmt('Node %s does not have texture coordinates',
      [Geometry.NiceName]);
end;

end.
