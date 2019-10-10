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

  TBatchShapes = class
  strict private
    const
      { In each TMergePipeline we have a few slots,
        to account for different values of things that must match for all
        merged shapes. E.g. Material.EmissiveColor in slot 0 may be white,
        in slot 1 Material.EmissiveColor may be blue.

        We need more than 1 slot, otherwise dynamic batching could be easily
        made worthless if one shape would be e.g. blue, and then 100 others would
        be white.

        But we also cannot have too many slots,
        or the time spent in "finding the right slot to merge" could grow. }
      MergeSlots = 4;
    type
      { Shapes from different pipelines cannot be merged with each other,
        and the pool shapes (in FPool) may be prepared differently for each
        TMergePipeline.
        E.g. pool shapes for mpTexCoord has TexCoord assigned,
        for mpNoTexCoord they have TexCoord=nil. }
      TMergePipeline = (mpNoTexCoord, mpTexCoord);
      TMergeSlot = 0 .. MergeSlots - 1;
      TMergingShapes = array [TMergePipeline, TMergeSlot] of TGLShape;
    var
      FCollected: TShapeList;
      FWaitingToBeCollected, FMergeTarget, FPool: TMergingShapes;
      FPoolGeometries: TGroupNode;
      LogIncreaseSlotsDone: Boolean;

    { Add Source into Target.
      You can assume that Target is one of our pool shapes,
      with initial state and geometry calculated by InitializePool. }
    procedure Merge(const Target, Source: TGLShape;
      const P: TMergePipeline; const FirstMerge: Boolean);

    { Clear any possible leftovers from Merge, where a given shape was Target. }
    procedure ClearMerge(const Target: TGLShape;
      const P: TMergePipeline);

    { Similar to TAbstractGeometryNode.InternalCoordinates, but for tex coords. }
    class function TexCoordinates(
      const Geometry: TAbstractGeometryNode;
      const State: TX3DGraphTraverseState): TMFVec2f;

    procedure DoLogIncreaseSlots;
  public
    var
      { Make sure that shapes on the @link(Collected) list have the same order
        as they are passed on @link(Collect) method.
        This makes batching less aggressive (so less effective,
        less chance of merging many shapes into few),
        but it makes sure that rendering output will be the same,
        if the order was important (e.g. you were rendering
        without Z-buffer test).

        Reset to @false in each @link(FreeCollected). }
      PreserveShapeOrder: Boolean;

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
  CastleUtils, CastleLog, CastleVectors;

{.$define CASTLE_DEBUG_BATCHING}

constructor TBatchShapes.Create(const CreateShape: TCreateShapeEvent);

  function MergePipelineToStr(const P: TBatchShapes.TMergePipeline): String;
  begin
    WriteStr(Result, P);
  end;

  procedure InitializePool;
  var
    ShapeNode: TShapeNode;
    Geometry: TIndexedFaceSetNode;
    State: TX3DGraphTraverseState;
    ParentInfo: TTraversingInfo;
    Shape: TGLShape;
    P: TMergePipeline;
    Slot: TMergeSlot;
  begin
    for P in TMergePipeline do
      for Slot in TMergeSlot do
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
        FPool[P, Slot] := Shape;

        {$ifdef CASTLE_DEBUG_BATCHING}
        Shape.Node.X3DName := 'Batched_' + MergePipelineToStr(P) + '_' + IntToStr(Slot);
        {$endif}
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
  Slot: TMergeSlot;
begin
  FreeAndNil(FCollected);
  for P in TMergePipeline do
    for Slot in TMergeSlot do
      FreeAndNil(FPool[P, Slot]);
  FreeAndNil(FPoolGeometries);
  inherited;
end;

procedure TBatchShapes.DoLogIncreaseSlots;
begin
  if not LogIncreaseSlotsDone then
  begin
    LogIncreaseSlotsDone := true;
    WritelnLog('Consider increasing MergeSlots, to allow more batching');
  end;
end;

function TBatchShapes.Collect(const Shape: TGLShape): Boolean;

  { Is this Shape suitable to consider for merging with @italic(anything).
    If yes, then we also determine the proper TMergePipeline. }
  function MergeableWithAnything(const Shape: TGLShape;
    out P: TMergePipeline): Boolean;
  var
    Geometry: TAbstractGeometryNode;
    TexCoord: TAbstractTextureCoordinateNode;
  begin
    Result := false;

    // We can only Merge geometries from VRML 2 / X3D (with TShapeNode set)
    if Shape.Node = nil then
      Exit;

    // We can only Merge TIndexedFaceSetNode for now
    Geometry := Shape.Geometry(true);
    if not (Geometry is TIndexedFaceSetNode) then
      Exit;

    if TIndexedFaceSetNode(Geometry).FdTexCoordIndex.Count <> 0 then
      Exit; // for now we don't handle texCoordIndex

    TexCoord := TIndexedFaceSetNode(Geometry).TexCoord;
    if TexCoord = nil then
    begin
      P := mpNoTexCoord;
      Result := true;
    end else
    if TexCoord is TTextureCoordinateNode then
    begin
      Result := true;
      P := mpTexCoord;
    end;
  end;

  { Can two given shapes be merged.
    Assumes that both shapes already passed MergeableWithAnything test,
    and have the same TMergePipeline. }
  function Mergeable(const Shape1, Shape2: TGLShape;
    const P: TMergePipeline): Boolean;

    function IndexedFaceSetMatch(const I1, I2: TIndexedFaceSetNode): Boolean;
    begin
      Result :=
        (I1 = I2) or
        (
          (I1.NormalPerVertex = I2.NormalPerVertex) and
          (I1.Solid = I2.Solid) and
          (I1.CreaseAngle = I2.CreaseAngle)
        );
    end;

    function MaterialsMatch(const M1, M2: TMaterialNode): Boolean;
    begin
      Result :=
        (M1 = M2) or
        (
          (M1 <> nil) and
          (M2 <> nil) and
          TVector3.PerfectlyEquals(M1.FdDiffuseColor    .Value, M2.FdDiffuseColor    .Value) and
          TVector3.PerfectlyEquals(M1.FdSpecularColor   .Value, M2.FdSpecularColor   .Value) and
          TVector3.PerfectlyEquals(M1.FdEmissiveColor   .Value, M2.FdEmissiveColor   .Value) and
          (M1.FdAmbientIntensity.Value = M2.FdAmbientIntensity.Value) and
          (M1.FdTransparency    .Value = M2.FdTransparency    .Value)
        );
    end;

    function AppearancesMatch(const A1, A2: TAppearanceNode): Boolean;
    begin
      Result :=
        (A1 = A2) or
        (
          (A1 <> nil) and
          (A2 <> nil) and
          (A1.FdTexture.Value = A2.FdTexture.Value) and
          MaterialsMatch(A1.Material, A2.Material)
        );
    end;

  var
    //State1, State2: TX3DGraphTraverseState;
    Mesh1, Mesh2: TIndexedFaceSetNode;
  begin
    Mesh1 := TIndexedFaceSetNode(Shape1.Geometry(true));
    Mesh2 := TIndexedFaceSetNode(Shape2.Geometry(true));
    Result :=
      // TODO: Make sure also fog state matches.
      IndexedFaceSetMatch(Mesh1, Mesh2) and
      AppearancesMatch(Shape1.Node.Appearance, Shape2.Node.Appearance);
  end;

  { Find a slot in Shapes[P] which is non-nil and can be merged with Shape.
    Assumes that for Shape, we already determined given TMergePipeline. }
  function FindMergeable(const Shapes: TMergingShapes;
    const P: TMergePipeline; const Shape: TGLShape;
    out MergeSlot: TMergeSlot): Boolean;
  begin
    for MergeSlot in TMergeSlot do
      if Shapes[P, MergeSlot] <> nil then
        if Mergeable(Shape, Shapes[P, MergeSlot], P) then
          Exit(true);
    Result := false;
  end;

  { Find an empty slot within this TMergePipeline (P).
    We must have empty FWaitingToBeCollected[P, MergeSlot] (to place there new shape)
    and empty FMergeTarget[P, MergeSlot] (otherwise FPool[P, MergeSlot]
    is already used for other purpose). }
  function FindFreeSlot(const P: TMergePipeline; out MergeSlot: TMergeSlot): Boolean;
  begin
    for MergeSlot in TMergeSlot do
      if (FWaitingToBeCollected[P, MergeSlot] = nil) and
         (FMergeTarget[P, MergeSlot] = nil) then
        Exit(true);
    Result := false;
  end;

var
  P: TMergePipeline;
  Slot: TMergeSlot;
begin
  Result := MergeableWithAnything(Shape, P);
  if not Result then
    Exit;

  if FindMergeable(FMergeTarget, P, Shape, Slot) then
  begin
    { Merge Shape into last FMergeTarget shape.
      This occurs for 3rd and subsequent shapes (has match on FMergeTarget). }
    Merge(FMergeTarget[P, Slot], Shape, P, false);
  end else
  if FindMergeable(FWaitingToBeCollected, P, Shape, Slot) then
  begin
    { Move unmodified shape from FWaitingToBeCollected to FMergeTarget.
      This occurs for 2nd shape (no match on FMergeTarget, but match on FWaitingToBeCollected). }
    FMergeTarget[P, Slot] := FPool[P, Slot];
    FCollected.Add(FMergeTarget[P, Slot]);
    ClearMerge(FMergeTarget[P, Slot], P);
    Merge(FMergeTarget[P, Slot], FWaitingToBeCollected[P, Slot], P, true);
    Merge(FMergeTarget[P, Slot], Shape, P, false);
    FWaitingToBeCollected[P, Slot] := nil;
  end else
  if FindFreeSlot(P, Slot) then
  begin
    { Add shape to FWaitingToBeCollected.
      This occurs on 1st shape (no match on FMergeTarget or FWaitingToBeCollected). }
    FWaitingToBeCollected[P, Slot] := Shape;
  end else
  begin
    DoLogIncreaseSlots;
    Exit(false); // we would like to batch it, but MergeSlots is not enough
  end;
end;

procedure TBatchShapes.FreeCollected;
var
  P: TMergePipeline;
  Slot: TMergeSlot;
begin
  FCollected.Clear;
  for P in TMergePipeline do
    for Slot in TMergeSlot do
      if FMergeTarget[P, Slot] <> nil then
      begin
        // don't wait for ClearMerge for this, do this earlier to release reference count
        FMergeTarget[P, Slot].Node.Appearance := nil;
        FMergeTarget[P, Slot] := nil;
      end;
  PreserveShapeOrder := false;
end;

procedure TBatchShapes.Commit;

  procedure DebugOutput;
  var
    Shape: TShape;
    RootNode: TX3DRootNode;
    TexCoordNode: TX3DNode;
    TexCoordsLog: String;
    Geometry: TIndexedFaceSetNode;
    State: TX3DGraphTraverseState;
  begin
    RootNode := TX3DRootNode.Create;
    for Shape in FCollected do
    begin
      Geometry := Shape.OriginalGeometry as TIndexedFaceSetNode;
      State := Shape.OriginalState;
      if Geometry.InternalTexCoord(State, TexCoordNode) and
         (TexCoordNode is TTextureCoordinateNode) then
        TexCoordsLog := Format('%d tex coords, ', [
          TTextureCoordinateNode(TexCoordNode).FdPoint.Count
        ])
      else
        TexCoordsLog := '';
      WritelnLog('Collected shape: %s %s with %d vertexes, %s%d indexes, %s bbox', [
        Shape.Node.X3DName,
        Geometry.NiceName,
        (Geometry.Coord as TCoordinateNode).FdPoint.Count,
        TexCoordsLog,
        Geometry.FdCoordIndex.Count,
        Shape.BoundingBox.ToString
      ]);
      Shape.Node.KeepExistingBegin;
      RootNode.AddChildren(Shape.Node);
    end;

    if RootNode.FdChildren.Count <> 0 then
      Save3D(RootNode, 'cge_batching_output.x3d', ApplicationName);

    for Shape in FCollected do
      Shape.Node.KeepExistingEnd;

    FreeAndNil(RootNode);
  end;

var
  P: TMergePipeline;
  Slot: TMergeSlot;
begin
  for P in TMergePipeline do
    for Slot in TMergeSlot do
    begin
      if FWaitingToBeCollected[P, Slot] <> nil then
      begin
        FCollected.Add(FWaitingToBeCollected[P, Slot]);
        FWaitingToBeCollected[P, Slot] := nil;
      end;
      if FMergeTarget[P, Slot] <> nil then
      begin
        { Mark changes from
          - TIndexedFaceSetNode.FdCoordIndex,
          - TCoordinateNode.FdPoint
          - TTextureCoordinateNode.FdPoint
        }
        FMergeTarget[P, Slot].Changed(false, [chCoordinate, chTextureCoordinate, chGeometry]);
      end;
    end;

  {$ifdef CASTLE_DEBUG_BATCHING}
  DebugOutput;
  {$endif}
end;

procedure TBatchShapes.GLContextClose;
var
  P: TMergePipeline;
  Slot: TMergeSlot;
begin
  for P in TMergePipeline do
    for Slot in TMergeSlot do
      FPool[P, Slot].GLContextClose;
end;

procedure TBatchShapes.Merge(const Target, Source: TGLShape;
  const P: TMergePipeline; const FirstMerge: Boolean);
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

  if FirstMerge then
  begin
    // assign things that should be equal when merging
    Assert(Source.Node <> nil); // only such source nodes are passed to Merge
    // using here FdAppearance.Value is marginally faster than Appearance, it matters a bit
    Target.Node.FdAppearance.Value := Source.Node.Appearance;
    MeshTarget.NormalPerVertex := MeshSource.NormalPerVertex;
    MeshTarget.Solid := MeshSource.Solid;
    MeshTarget.CreaseAngle := MeshSource.CreaseAngle;
  end;

  CoordTarget := MeshTarget.InternalCoordinates(StateTarget);
  CoordSource := MeshSource.InternalCoordinates(StateSource);
  OldCoordCount := CoordTarget.Count;
  CoordTarget.Items.AddRangeTransformed(CoordSource.Items, StateSource.Transform);

  if P = mpTexCoord then
  begin
    TexCoordTarget := TexCoordinates(MeshTarget, StateTarget);
    TexCoordSource := TexCoordinates(MeshSource, StateSource);
    TexCoordTarget.Items.AddRange(TexCoordSource.Items);
    Check(CoordTarget.Count = TexCoordTarget.Count); // TODO: secure from it
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
