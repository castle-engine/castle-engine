{
  Copyright 2019-2023 Michalis Kamburelis.

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

uses CastleSceneInternalShape, CastleShapes, X3DNodes, X3DFields, X3DLoad;

type
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
        or the time spent in "finding the right slot to merge"
        (FindMergeable in implementation, only for PreserveShapeOrder=false)
        will grow. }
      MergeSlots = 8;
    type
      { Shapes from different pipelines cannot be merged with each other,
        and the pool shapes (in FPool) may be prepared differently for each
        TMergePipeline. }
      TMergePipeline = (
        { TIndexedFaceSetNode with TexCoord=nil }
        mpFacesNoTexCoord,
        { TIndexedFaceSetNode with TexCoord<>nil }
        mpFacesTexCoord,
        { TIndexedTriangleSetNode with TexCoord=nil }
        mpTrianglesNoTexCoord,
        { TIndexedTriangleSetNode with TexCoord<>nil }
        mpTrianglesTexCoord
      );
      TMergeSlot = 0 .. MergeSlots - 1;
      TMergingShapes = array [TMergePipeline, TMergeSlot] of TCollectedShape;
    const
      MergePipelinesWithTexCoord = [mpFacesTexCoord, mpTrianglesTexCoord];
      MergePipelinesWithFaces = [mpFacesNoTexCoord, mpFacesTexCoord];
    var
      FBatched: TCollectedShapeList;
      FPool: TMergingShapes;
      FPoolUsed: array [TMergePipeline] of Integer;

      { FMergeTarget are copies of the respective shapes on FPool list,
        when they first become used by Merge (not only allocated by AllocateSlot). }
      FMergeTarget: TMergingShapes;

      { When PreserveShapeOrder=false, we use this. }
      FUnorderedPreviousShapes: TMergingShapes;

      { When PreserveShapeOrder=true, we use this. }
      FOrderPreviousShape: TCollectedShape;
      FOrderPreviousShapeMerging: Boolean;
      FOrderPreviousShapePipeline: TMergePipeline;

      FPoolGeometries: TGroupNode;
      LogIncreaseSlotsDone: Boolean;

    { Add CollectedSource into CollectedTarget.
      You can assume that CollectedTarget is one of our pool shapes,
      with initial state and geometry calculated by InitializePool. }
    procedure Merge(const CollectedTarget, CollectedSource: TCollectedShape;
      const P: TMergePipeline; const FirstMerge: Boolean);

    { Clear any possible leftovers from Merge, where a given shape was Target. }
    procedure ClearMerge(const CollectedTarget: TCollectedShape;
      const P: TMergePipeline);

    { Similar to TAbstractGeometryNode.InternalCoordinates, but for tex coords. }
    class function TexCoordinates(
      const Geometry: TAbstractGeometryNode;
      const State: TX3DGraphTraverseState): TMFVec2f;

    procedure DoLogIncreaseSlots;
    function GetPoolShapes(const Index: Integer): TGLShape;
  public
    var
      { Make sure that shapes on the @link(Batched) list have the same order
        in which they have been passed to the @link(Batch) method.

        This makes batching less aggressive (so less effective,
        less chance of merging),
        but it makes sure that rendering output will be the same,
        if the order was important (e.g. you were rendering
        without Z-buffer test).

        This matters when using blending, in practice.
        When blending, then TShapesRenderer sorts (using BlendingSort) shapes
        (original, not batched, shapes) and then passes these shapes in correct
        order to this class.
        Batching cannot break this order (as it would make blending
        incorrect).

        Reset to @false in each @link(FreeBatched). }
      PreserveShapeOrder: Boolean;

    constructor Create;
    destructor Destroy; override;

    { Merge given shape into the @link(Batched) shapes.
      During this, the shape may merge with another shape into a single, larger
      shape. Returns @true if the shape was added to @link(Batched),
      otherwise it was not, and should be rendered by the caller immediately
      without the help of batching. }
    function Batch(const CollectedShape: TCollectedShape): Boolean;

    procedure Commit;

    { Currently gathered shapes by @link(Batched).
      Call @link(Commit) before reading this. }
    property Batched: TCollectedShapeList read FBatched;

    { Release all shapes and clear the @link(Collected) list. }
    procedure FreeBatched;

    procedure GLContextClose;

    { Enumerate "pool" shapes.
      This is useful to prepare them (e.g. in TCastleScene.PrepareResources),
      to make sure further usage of them will be fast. }
    property PoolShapes[const Index: Integer]: TGLShape read GetPoolShapes;
    function PoolShapesCount: Integer;
  end;

implementation

uses SysUtils,
  CastleUtils, CastleLog, CastleVectors, CastleRenderOptions;

{.$define CASTLE_DEBUG_BATCHING}

constructor TBatchShapes.Create;

  function MergePipelineToStr(const P: TBatchShapes.TMergePipeline): String;
  const
    Names: array [TBatchShapes.TMergePipeline] of String = (
      'FacesNoTexCoord',
      'FacesTexCoord',
      'TrianglesNoTexCoord',
      'TrianglesTexCoord'
    );
  begin
    Result := Names[P];
  end;

  function CreateShape(const AGeometry: TAbstractGeometryNode;
    const AState: TX3DGraphTraverseState; const ParentInfo: PTraversingInfo): TShape;
  begin
    Result := TGLShape.Create(nil, AGeometry, AState, ParentInfo);
  end;

  procedure InitializePool;
  var
    ShapeNode: TShapeNode;
    Geometry: TAbstractComposedGeometryNode;
    State: TX3DGraphTraverseState;
    ParentInfo: TTraversingInfo;
    Shape: TGLShape;
    P: TMergePipeline;
    Slot: TMergeSlot;
    CollectedShape: TCollectedShape;
  begin
    for P := Low(TMergePipeline) to High(TMergePipeline) do
      for Slot := Low(TMergeSlot) to High(TMergeSlot) do
      begin
        // initialize Geometry and ShapeNode
        if P in MergePipelinesWithFaces then
          Geometry := TIndexedFaceSetNode.CreateWithShape(ShapeNode)
        else
          Geometry := TIndexedTriangleSetNode.CreateWithShape(ShapeNode);
        Geometry.Coord := TCoordinateNode.Create;
        if P in MergePipelinesWithTexCoord then
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
        Shape.DisableSharedCache := true;

        CollectedShape := TCollectedShape.Create;
        CollectedShape.Shape := Shape;
        // We will take RenderOptions from merged Shapes
        CollectedShape.RenderOptions := nil;
        CollectedShape.SceneTransform := TMatrix4.Identity;

        FPool[P, Slot] := CollectedShape;

        {$ifdef CASTLE_DEBUG_BATCHING}
        Shape.Node.X3DName := 'Batched_' + MergePipelineToStr(P) + '_' + IntToStr(Slot);
        {$endif}
      end;
  end;

begin
  inherited Create;
  FBatched := TCollectedShapeList.Create(false);
  FPoolGeometries := TGroupNode.Create;
  InitializePool;
end;

destructor TBatchShapes.Destroy;
var
  P: TMergePipeline;
  Slot: TMergeSlot;
begin
  { In case our GLContextClose was not called yet, but we are destroyed,
    make sure to remove our pool shapes from cache. }
  GLContextClose;

  FreeAndNil(FBatched);
  for P := Low(TMergePipeline) to High(TMergePipeline) do
    for Slot := Low(TMergeSlot) to High(TMergeSlot) do
    begin
      if FPool[P, Slot] <> nil then
      begin
        FreeAndNil(FPool[P, Slot].Shape);
        FreeAndNil(FPool[P, Slot]);
      end;
    end;
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

function TBatchShapes.Batch(const CollectedShape: TCollectedShape): Boolean;

  { Is this Shape suitable to consider for merging with @italic(anything).
    If yes, then we also determine the proper TMergePipeline. }
  function MergeableWithAnything(const Shape: TGLShape;
    out P: TMergePipeline): Boolean;
  var
    Geometry: TAbstractGeometryNode;
    TexCoord: TAbstractTextureCoordinateNode;
    GeometryComposed: TAbstractComposedGeometryNode;
    Faces: TIndexedFaceSetNode;
    // Triangles: TIndexedTriangleSetNode; // not needed for now
  begin
    Result := false;

    // We can only Merge geometries from VRML 2 / X3D (with TShapeNode set)
    if Shape.Node = nil then
      Exit;

    // We can only merge TAbstractComposedGeometryNode for now
    Geometry := Shape.Geometry;
    if not (Geometry is TAbstractComposedGeometryNode) then
      Exit;
    GeometryComposed := TAbstractComposedGeometryNode(Geometry);

    if {$ifndef CASTLE_SLIM_NODES}
       (GeometryComposed.FdAttrib.Count <> 0) or
       (GeometryComposed.FdFogCoord.Value <> nil) or
       {$endif}
       (GeometryComposed.FdColor.Value <> nil) or
       (GeometryComposed.FdNormal.Value <> nil) or
       ( { If the shape needs automatic texture coordinate generation,
           batching is not possible, as the tex coordinate generation looks at shape's bounding box.
           See https://github.com/castle-engine/castle-engine/issues/179 . }
         (GeometryComposed.FdTexCoord.Value = nil) and
         (Shape.Node <> nil) and
         (Shape.Node.Appearance <> nil) and
         (Shape.Node.Appearance.Texture <> nil) ) then
      Exit;

    TexCoord := GeometryComposed.TexCoord;

    if GeometryComposed is TIndexedFaceSetNode then
    begin
      // conditions specific to TIndexedFaceSetNode
      Faces := TIndexedFaceSetNode(GeometryComposed);
      if (Faces.FdTexCoordIndex.Count <> 0) or // for now we don't handle texCoordIndex
         (Faces.FdColorIndex.Count <> 0) or
         (Faces.FdNormalIndex.Count <> 0) then
        Exit;

      if TexCoord = nil then
      begin
        P := mpFacesNoTexCoord;
        Result := true;
      end else
      if TexCoord is TTextureCoordinateNode then
      begin
        Result := true;
        P := mpFacesTexCoord;
      end;
    end else
    if GeometryComposed is TIndexedTriangleSetNode then
    begin
      // conditions specific to TIndexedTriangleSetNode

      if TexCoord = nil then
      begin
        P := mpTrianglesNoTexCoord;
        Result := true;
      end else
      if TexCoord is TTextureCoordinateNode then
      begin
        Result := true;
        P := mpTrianglesTexCoord;
      end;
    end;
  end;

  { Can two given shapes be merged.
    Assumes that both shapes already passed MergeableWithAnything test,
    and have the same TMergePipeline. }
  function Mergeable(const CollectedShape1, CollectedShape2: TCollectedShape;
    const P: TMergePipeline): Boolean;

    function IndexedFaceSetContentsMatch(const I1, I2: TIndexedFaceSetNode): Boolean;
    begin
      Result :=
        (
          (I1.FdNormalPerVertex.Value = I2.FdNormalPerVertex.Value) and
          (I1.FdSolid          .Value = I2.FdSolid          .Value) and
          (I1.FdConvex         .Value = I2.FdConvex         .Value) and
          (I1.FdCcw            .Value = I2.FdCcw            .Value) and
          (I1.FdCreaseAngle    .Value = I2.FdCreaseAngle    .Value)
        );
    end;

    function IndexedTriangleSetContentsMatch(const I1, I2: TIndexedTriangleSetNode): Boolean;
    begin
      Result :=
        (
          (I1.FdNormalPerVertex.Value = I2.FdNormalPerVertex.Value) and
          (I1.FdSolid          .Value = I2.FdSolid          .Value) and
          (I1.FdCcw            .Value = I2.FdCcw            .Value)
        );
    end;

    function AbstractGeometriesMatch(const G1, G2: TAbstractGeometryNode): Boolean;
    begin
      Result :=
        (G1 = G2) or
        (
          (G1 is TIndexedFaceSetNode) and
          (G2 is TIndexedFaceSetNode) and
          IndexedFaceSetContentsMatch(TIndexedFaceSetNode(G1), TIndexedFaceSetNode(G2))
        ) or
        (
          (G1 is TIndexedTriangleSetNode) and
          (G2 is TIndexedTriangleSetNode) and
          IndexedTriangleSetContentsMatch(TIndexedTriangleSetNode(G1), TIndexedTriangleSetNode(G2))
        );
    end;

    function TexturePropertiesContentsMatch(const P1, P2: TTexturePropertiesNode): Boolean;
    begin
      Result :=
        (P1.AnisotropicDegree = P2.AnisotropicDegree) and
        (P1.BoundaryModeR = P2.BoundaryModeR) and
        (P1.BoundaryModeS = P2.BoundaryModeS) and
        (P1.BoundaryModeT = P2.BoundaryModeT) and
        (P1.MagnificationFilter = P2.MagnificationFilter) and
        (P1.MinificationFilter = P2.MinificationFilter) and
        (P1.GenerateMipMaps = P2.GenerateMipMaps) and // unhandled, but soon may be handled
        (P1.GuiTexture = P2.GuiTexture);
    end;

    function TexturePropertiesEqual(const P1, P2: TX3DNode): Boolean;
    begin
      Result :=
        (P1 = P2) or
        (
          (P1 is TTexturePropertiesNode) and
          (P2 is TTexturePropertiesNode) and
          TexturePropertiesContentsMatch(TTexturePropertiesNode(P1), TTexturePropertiesNode(P2))
        );
    end;

    function ImageTextureContentsMatch(const Texture1, Texture2: TImageTextureNode): Boolean;
    begin
      Result :=
        Texture1.FdUrl.Equals(Texture2.FdUrl) and
        (Texture1.FlipVertically = Texture2.FlipVertically) and
        (Texture1.AlphaChannel = Texture2.AlphaChannel) and
        (Texture1.RepeatS = Texture2.RepeatS) and
        (Texture1.RepeatT = Texture2.RepeatT) and
        Texture1.FdEffects.Equals(Texture2.FdEffects) and
        TexturePropertiesEqual(Texture1.FdTextureProperties.Value, Texture2.FdTextureProperties.Value);
    end;

    function TexturesEqual(const Texture1, Texture2: TX3DNode): Boolean;
    begin
      Result :=
        (Texture1 = Texture2) or
        (
          (Texture1 is TImageTextureNode) and
          (Texture2 is TImageTextureNode) and
          ImageTextureContentsMatch(TImageTextureNode(Texture1), TImageTextureNode(Texture2))
        );
    end;

    function MaterialTexturesEqual(const Texture1, Texture2: TX3DNode;
      const Texture1Mapping, Texture2Mapping: String): Boolean;
    begin
      Result :=
        TexturesEqual(Texture1, Texture2) and
        (Texture1Mapping = Texture2Mapping);
    end;

    { Checks contents of M1 and M2,
      assuming that they are both <> nil and different references
      (so there's no point in checking their references). }
    function PhongMaterialsContentsMatch(const M1, M2: TMaterialNode): Boolean;
    begin
      Result :=
        (
          TVector3.PerfectlyEquals(M1.FdDiffuseColor    .Value, M2.FdDiffuseColor    .Value) and
          TVector3.PerfectlyEquals(M1.FdSpecularColor   .Value, M2.FdSpecularColor   .Value) and
          TVector3.PerfectlyEquals(M1.FdEmissiveColor   .Value, M2.FdEmissiveColor   .Value) and
          (M1.FdAmbientIntensity.Value = M2.FdAmbientIntensity.Value) and
          (M1.FdTransparency    .Value = M2.FdTransparency    .Value) and

          MaterialTexturesEqual(M1.FdNormalTexture   .Value, M2.FdNormalTexture   .Value, M1.FdNormalTextureMapping   .Value, M2.FdNormalTextureMapping   .Value) and
          MaterialTexturesEqual(M1.FdDiffuseTexture  .Value, M2.FdDiffuseTexture  .Value, M1.FdDiffuseTextureMapping  .Value, M2.FdDiffuseTextureMapping  .Value) and
          MaterialTexturesEqual(M1.FdSpecularTexture .Value, M2.FdSpecularTexture .Value, M1.FdSpecularTextureMapping .Value, M2.FdSpecularTextureMapping .Value) and
          MaterialTexturesEqual(M1.FdEmissiveTexture .Value, M2.FdEmissiveTexture .Value, M1.FdEmissiveTextureMapping .Value, M2.FdEmissiveTextureMapping .Value) and
          MaterialTexturesEqual(M1.FdAmbientTexture  .Value, M2.FdAmbientTexture  .Value, M1.FdAmbientTextureMapping  .Value, M2.FdAmbientTextureMapping  .Value) and
          MaterialTexturesEqual(M1.FdOcclusionTexture.Value, M2.FdOcclusionTexture.Value, M1.FdOcclusionTextureMapping.Value, M2.FdOcclusionTextureMapping.Value)
        );
    end;

    function UnlitMaterialsContentsMatch(const M1, M2: TUnlitMaterialNode): Boolean;
    begin
      Result :=
        (
          TVector3.PerfectlyEquals(M1.FdEmissiveColor   .Value, M2.FdEmissiveColor   .Value) and
          (M1.FdTransparency    .Value = M2.FdTransparency    .Value) and

          // ignored: MaterialTexturesEqual(M1.FdNormalTexture  .Value, M2.FdNormalTexture  .Value, M1.FdNormalTextureMapping  .Value, M2.FdNormalTextureMapping  .Value) and
          MaterialTexturesEqual(M1.FdEmissiveTexture.Value, M2.FdEmissiveTexture.Value, M1.FdEmissiveTextureMapping.Value, M2.FdEmissiveTextureMapping.Value)
        );
    end;

    function PhysicalMaterialsContentsMatch(const M1, M2: TPhysicalMaterialNode): Boolean;
    begin
      Result :=
        (
          TVector3.PerfectlyEquals(M1.FdEmissiveColor   .Value, M2.FdEmissiveColor   .Value) and
          TVector3.PerfectlyEquals(M1.FdBaseColor       .Value, M2.FdBaseColor       .Value) and
          (M1.FdMetallic    .Value = M2.FdMetallic    .Value) and
          (M1.FdRoughness   .Value = M2.FdRoughness   .Value) and
          (M1.FdTransparency.Value = M2.FdTransparency.Value) and

          MaterialTexturesEqual(M1.FdNormalTexture           .Value, M2.FdNormalTexture           .Value, M1.FdNormalTextureMapping           .Value, M2.FdNormalTextureMapping           .Value) and
          MaterialTexturesEqual(M1.FdEmissiveTexture         .Value, M2.FdEmissiveTexture         .Value, M1.FdEmissiveTextureMapping         .Value, M2.FdEmissiveTextureMapping         .Value) and
          MaterialTexturesEqual(M1.FdBaseTexture             .Value, M2.FdBaseTexture             .Value, M1.FdBaseTextureMapping             .Value, M2.FdBaseTextureMapping             .Value) and
          MaterialTexturesEqual(M1.FdMetallicRoughnessTexture.Value, M2.FdMetallicRoughnessTexture.Value, M1.FdMetallicRoughnessTextureMapping.Value, M2.FdMetallicRoughnessTextureMapping.Value) and
          MaterialTexturesEqual(M1.FdOcclusionTexture        .Value, M2.FdOcclusionTexture        .Value, M1.FdOcclusionTextureMapping        .Value, M2.FdOcclusionTextureMapping        .Value)
        );
    end;

    function AbstractMaterialsMatch(const M1, M2: TAbstractMaterialNode): Boolean;
    begin
      Result :=
        (M1 = M2) or
        (
          (M1 is TMaterialNode) and
          (M2 is TMaterialNode) and
          PhongMaterialsContentsMatch(TMaterialNode(M1), TMaterialNode(M2))
        ) or
        (
          (M1 is TUnlitMaterialNode) and
          (M2 is TUnlitMaterialNode) and
          UnlitMaterialsContentsMatch(TUnlitMaterialNode(M1), TUnlitMaterialNode(M2))
        ) or
        (
          (M1 is TPhysicalMaterialNode) and
          (M2 is TPhysicalMaterialNode) and
          PhysicalMaterialsContentsMatch(TPhysicalMaterialNode(M1), TPhysicalMaterialNode(M2))
        );
    end;

    function AppearancesMatch(const A1, A2: TAppearanceNode): Boolean;
    begin
      Result :=
        (A1 = A2) or
        (
          (A1 <> nil) and
          (A2 <> nil) and
          TexturesEqual(A1.FdTexture.Value, A2.FdTexture.Value) and
          (A1.FdTextureTransform.Value = A2.FdTextureTransform.Value) and
          AbstractMaterialsMatch(A1.Material, A2.Material)
        );
    end;

    function LightsMatch(const Lights1, Lights2: TLightInstancesList): Boolean;
    begin
      Result :=
        (Lights1 = Lights2) or
        (
          (Lights1 <> nil) and
          (Lights2 <> nil) and
          Lights1.Equals(Lights2)
        );
    end;

  var
    Shape1, Shape2: TGLShape;
    Geometry1, Geometry2: TAbstractGeometryNode;
    State1, State2: TX3DGraphTraverseState;
  begin
    Shape1 := CollectedShape1.Shape;
    Shape2 := CollectedShape2.Shape;
    Geometry1 := Shape1.Geometry;
    Geometry2 := Shape2.Geometry;
    State1 := Shape1.State;
    State2 := Shape2.State;
    Result :=
      { Checks begin from the ones most likely to be different (exit early).
        Note that everything compared here must be also assigned in Merge
        (when FirstMerge), to make sure all merged instances keep the same values
        for this stuff. }
      AppearancesMatch(Shape1.Node.Appearance, Shape2.Node.Appearance) and
      AbstractGeometriesMatch(Geometry1, Geometry2) and
      (State1.LocalFog = State2.LocalFog) and
      (Shape1.Node.Shading = Shape2.Node.Shading) and
      LightsMatch(State1.Lights, State2.Lights) and
      (State1.ClipPlanes = State2.ClipPlanes) and
      (State1.Effects = State2.Effects) and
      (CollectedShape1.RenderOptions.Equals(CollectedShape2.RenderOptions)) and
      (CollectedShape1.DepthRange = CollectedShape2.DepthRange);
  end;

  { Find a slot in Shapes[P] which is non-nil and can be merged with Shape.
    Assumes that for Shape, we already determined given TMergePipeline. }
  function FindMergeable(const Shapes: TMergingShapes;
    const P: TMergePipeline; const CollectedShape: TCollectedShape;
    out MergeSlot: TMergeSlot): Boolean;
  var
    MS: TMergeSlot;
  begin
    for MS := Low(TMergeSlot) to High(TMergeSlot) do
    begin
      if Shapes[P, MS] <> nil then
        if Mergeable(CollectedShape, Shapes[P, MS], P) then
        begin
          MergeSlot := MS;
          Exit(true);
        end;
    end;
    Result := false;
  end;

  { First merge of two shapes.
    Sets FMergeTarget[P, Slot] (using FPool[P, Slot]),
    adds it to FBatched,
    places there merge of Shape1 and Shape2.

    Called must first check that FPool[P, Slot] was not used for anything
    (e.g. by checking that FMergeTarget[P, Slot] is nil). }
  procedure InitialMerge(
    const CollectedShape1, CollectedShape2: TCollectedShape;
    const P: TMergePipeline; const Slot: TMergeSlot);
  begin
    {$ifdef CASTLE_DEBUG_BATCHING}
    if FMergeTarget[P, Slot] <> nil then
      WritelnWarning('Batching: merging to already used slot');
    {$endif}
    FMergeTarget[P, Slot] := FPool[P, Slot];
    FBatched.Add(FMergeTarget[P, Slot]);
    ClearMerge(FMergeTarget[P, Slot], P);
    Merge(FMergeTarget[P, Slot], CollectedShape1, P, true);
    Merge(FMergeTarget[P, Slot], CollectedShape2, P, false);
  end;

  function AllocateSlot(const P: TMergePipeline; out Slot: TMergeSlot): Boolean;
  begin
    Result := FPoolUsed[P] < MergeSlots;
    if Result then
    begin
      Slot := FPoolUsed[P];
      Inc(FPoolUsed[P]);
    end;
  end;

  var
    Shape: TGLShape; //< Shortcut for CollectedShape.Shape

  { Algorithm specific for PreserveShapeOrder=true case. }
  procedure DoPreserveShapeOrder;
  var
    P: TMergePipeline;
    Handled: Boolean;
    Slot: TMergeSlot;
  begin
    Handled := false;

    { When PreserveShapeOrder, the algorithm is simpler:
      We try to merge an incoming shape with the previous shape
      (taking into account that a previous shape may be already a result
      of merging, depending on FOrderPreviousShapeMerging).
      If this is not possible, we just push previous shape to FBatched. }

    { In case of DoPreserveShapeOrder, Batch must *always* return true,
      even for non-mergeable shapes. }
    Result := true;

    if not MergeableWithAnything(Shape, P) then
    begin
      { The non-mergeable shapes have to be added
        to FBatched, to make sure they are in correct order between mergeable.
        Testcase: merging scene with indicator (IndexedFaceSet, Text and Rectangle2D) in Unholy. }
      // finish merging previous shape
      if (FOrderPreviousShape <> nil) and
         (not FOrderPreviousShapeMerging) then
        FBatched.Add(FOrderPreviousShape);
      FBatched.Add(CollectedShape);
      FOrderPreviousShape := nil;
      FOrderPreviousShapeMerging := false;
      Handled := true;
    end else
    if (FOrderPreviousShape <> nil) and
       (FOrderPreviousShapePipeline = P) and
       Mergeable(FOrderPreviousShape, CollectedShape, P) then
    begin
      if FOrderPreviousShapeMerging then
      begin
        Merge(FOrderPreviousShape, CollectedShape, P, false);
        Handled := true;
      end else
      if AllocateSlot(P, Slot) then
      begin
        InitialMerge(FOrderPreviousShape, CollectedShape, P, Slot);
        FOrderPreviousShape := FMergeTarget[P, Slot];
        FOrderPreviousShapeMerging := true;
        Handled := true;
      end else
        DoLogIncreaseSlots;
    end;

    if not Handled then
    begin
      // finish merging previous shape, and add new shape as FOrderPreviousShape
      if (FOrderPreviousShape <> nil) and
         (not FOrderPreviousShapeMerging) then
        FBatched.Add(FOrderPreviousShape);
      FOrderPreviousShape := CollectedShape;
      FOrderPreviousShapePipeline := P;
      FOrderPreviousShapeMerging := false;
    end;
  end;

  { Algorithm specific for PreserveShapeOrder=false case. }
  procedure DoIgnoreShapeOrder;
  var
    P: TMergePipeline;
    Slot: TMergeSlot;
  begin
    Result := MergeableWithAnything(Shape, P);
    if not Result then
      Exit;

    { When not PreserveShapeOrder, we try to merge an incoming shape with
      - one of the merges "in progress" (on FUnorderedPreviousShapes and FMergeTarget)
      - or one of the previous shapes, not yet during merging (only on FUnorderedPreviousShapes)
      - or we place it in a new slot, waiting for possible merge in the future. }

    if FindMergeable(FUnorderedPreviousShapes, P, CollectedShape, Slot) then
    begin
      if FMergeTarget[P, Slot] <> nil then
      begin
        { Slot in the middle of merging, so merge more. }
        Merge(FMergeTarget[P, Slot], CollectedShape, P, false);
      end else
      begin
        { Slot not yet merging, so start merging.
          This will set FMergeTarget[P, Slot], so next shapes will know we are in the middle
          of merging. }
        InitialMerge(FUnorderedPreviousShapes[P, Slot], CollectedShape, P, Slot);
        Assert(FMergeTarget[P, Slot] <> nil);
        FUnorderedPreviousShapes[P, Slot] := FMergeTarget[P, Slot];
      end;
    end else
    if AllocateSlot(P, Slot) then
    begin
      { Add shape to FUnorderedPreviousShapes.
        The corresponding FMergeTarget remains nil, so we know it is not yet in the middle of merging.

        Note that this reserves a slot, IOW we treat FPool[P, Slot] as already used.
        It may be wasteful (as we didn't start merging yet, we are not yet sure
        whether FPool[P, Slot] will be needed) but we need to reserve this pool for possible
        merging opportunity.

        TODO: Maybe the logic could be improved, to not reserve pool yet?
        Then our queue "waiting to be possibly merged" would not assign slots
        (corresponding to FPool and FMergeTarget slots).
        The advantage would be that we don't need so many MergeSlots
        in case of PreserveShapeOrder=false to be efficient, in some cases.
      }
      FUnorderedPreviousShapes[P, Slot] := CollectedShape;
    end else
    begin
      DoLogIncreaseSlots;
      Result := false; // we would like to batch it, but MergeSlots is not enough
    end;
  end;

begin
  Shape := CollectedShape.Shape;
  if PreserveShapeOrder then
    DoPreserveShapeOrder
  else
    DoIgnoreShapeOrder;
end;

procedure TBatchShapes.FreeBatched;
var
  P: TMergePipeline;
  Slot: TMergeSlot;
begin
  FBatched.Clear;
  for P := Low(TMergePipeline) to High(TMergePipeline) do
  begin
    for Slot := Low(TMergeSlot) to High(TMergeSlot) do
      if FMergeTarget[P, Slot] <> nil then
      begin
        // all shapes from batching pool should have Shape assigned
        Assert(FMergeTarget[P, Slot].Shape <> nil);

        // don't wait for ClearMerge for this, do this earlier to release reference count
        FMergeTarget[P, Slot].Shape.Node.FdAppearance.Value := nil;
        // make sure this is unassigned, otherwise TX3DGraphTraverseState.Destroy would free it
        FMergeTarget[P, Slot].Shape.State.Lights := nil;
        FMergeTarget[P, Slot] := nil;
      end;
    FPoolUsed[P] := 0;
  end;
  PreserveShapeOrder := false;
end;

procedure TBatchShapes.Commit;

  procedure DebugOutput;
  var
    CollectedShape: TCollectedShape;
    Shape: TGLShape;
    RootNode: TX3DRootNode;
    Geometry: TAbstractGeometryNode;
  begin
    RootNode := TX3DRootNode.Create;
    for CollectedShape in FBatched do
    begin
      Shape := CollectedShape.Shape;
      Geometry := Shape.OriginalGeometry;
      WritelnLog('Collected shape: %s, geometry: %s, bbox: %s', [
        Shape.Node.X3DName,
        Geometry.NiceName,
        Shape.BoundingBox.ToString
      ]);
      Shape.Node.KeepExistingBegin;
      RootNode.AddChildren(Shape.Node);
    end;

    if RootNode.FdChildren.Count <> 0 then
      SaveNode(RootNode, 'cge_batching_output.x3d', ApplicationName);

    for CollectedShape in FBatched do
    begin
      Shape := CollectedShape.Shape;
      Shape.Node.KeepExistingEnd;
    end;

    FreeAndNil(RootNode);
  end;

var
  P: TMergePipeline;
  Slot: TMergeSlot;
begin
  for P := Low(TMergePipeline) to High(TMergePipeline) do
    for Slot := Low(TMergeSlot) to High(TMergeSlot) do
    begin
      if FUnorderedPreviousShapes[P, Slot] <> nil then
      begin
        if FMergeTarget[P, Slot] = nil then
          FBatched.Add(FUnorderedPreviousShapes[P, Slot]);
        FUnorderedPreviousShapes[P, Slot] := nil;
      end;
      if FMergeTarget[P, Slot] <> nil then
      begin
        // all shapes from batching pool should have Shape assigned
        Assert(FMergeTarget[P, Slot].Shape <> nil);

        FMergeTarget[P, Slot].Shape.Changed(false, [
          { Mark changes from
            - TIndexedFaceSetNode.FdCoordIndex, TIndexedTriangleSetNode.FdIndex
            - TCoordinateNode.FdPoint
            - TTextureCoordinateNode.FdPoint
          }
          chCoordinate, chTextureCoordinate, chGeometry
        ]);
      end;
    end;

  if FOrderPreviousShape <> nil then
  begin
    if not FOrderPreviousShapeMerging then
      FBatched.Add(FOrderPreviousShape);
    FOrderPreviousShape := nil;
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
  for P := Low(TMergePipeline) to High(TMergePipeline) do
    for Slot := Low(TMergeSlot) to High(TMergeSlot) do
      FPool[P, Slot].Shape.GLContextClose;
end;

procedure TBatchShapes.Merge(
  const CollectedTarget, CollectedSource: TCollectedShape;
  const P: TMergePipeline; const FirstMerge: Boolean);
var
  Target, Source: TGLShape;
  StateTarget, StateSource: TX3DGraphTraverseState;
  MeshTarget, MeshSource: TAbstractComposedGeometryNode;
  CoordTarget, CoordSource: TMFVec3f;
  TexCoordTarget, TexCoordSource: TMFVec2f;
  IndexTarget, IndexSource: TInt32List;
  OldCoordCount, I: Integer;
begin
  Target := CollectedTarget.Shape;
  Source := CollectedSource.Shape;
  StateTarget := Target.State;
  StateSource := Source.State;
  MeshTarget := Target.Geometry as TAbstractComposedGeometryNode;
  MeshSource := Source.Geometry as TAbstractComposedGeometryNode;

  // no vertexes in source mesh, ignore it
  if MeshSource.Coord = nil then
    Exit;

  if FirstMerge then
  begin
    // assign things that should be equal when merging
    Assert(Source.Node <> nil); // only such source nodes are passed to Merge
    Target.Node.FdShading.Value := Source.Node.FdShading.Value;
    StateTarget.Lights := StateSource.Lights;
    StateTarget.LocalFog := StateSource.LocalFog;
    // using here FdAppearance.Value is marginally faster than Appearance, it matters a bit
    Target.Node.FdAppearance.Value := Source.Node.Appearance;
    MeshTarget.FdNormalPerVertex.Value := MeshSource.FdNormalPerVertex.Value;
    MeshTarget.FdSolid          .Value := MeshSource.FdSolid          .Value;
    if MeshTarget is TIndexedFaceSetNode then
    begin
      TIndexedFaceSetNode(MeshTarget).FdConvex         .Value := TIndexedFaceSetNode(MeshSource).FdConvex         .Value;
      TIndexedFaceSetNode(MeshTarget).FdCreaseAngle    .Value := TIndexedFaceSetNode(MeshSource).FdCreaseAngle    .Value;
    end;

    CollectedTarget.DepthRange := CollectedSource.DepthRange;

    { Assign RenderOptions. }
    CollectedTarget.RenderOptions := CollectedSource.RenderOptions;
    Target.OverrideRenderOptions := CollectedSource.RenderOptions;
  end;

  { Shapes to which we merge should have SceneTransform = identity always. }
  Assert(TMatrix4.PerfectlyEquals(CollectedTarget.SceneTransform, TMatrix4.Identity));

  CoordTarget := MeshTarget.InternalCoordinates(StateTarget);
  CoordSource := MeshSource.InternalCoordinates(StateSource);
  OldCoordCount := CoordTarget.Count;
  CoordTarget.Items.AddRangeTransformed(CoordSource.Items,
    CollectedSource.SceneTransform *
    StateSource.Transformation.Transform);

  if P in MergePipelinesWithTexCoord then
  begin
    TexCoordTarget := TexCoordinates(MeshTarget, StateTarget);
    TexCoordSource := TexCoordinates(MeshSource, StateSource);
    TexCoordTarget.Items.AddRange(TexCoordSource.Items);
    Check(CoordTarget.Count = TexCoordTarget.Count); // TODO: secure from it
  end;

  IndexTarget := MeshTarget.CoordIndexField.Items;
  IndexSource := MeshSource.CoordIndexField.Items;
  if (P in MergePipelinesWithFaces) and
     (IndexTarget.Count <> 0) and
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

procedure TBatchShapes.ClearMerge(const CollectedTarget: TCollectedShape;
  const P: TMergePipeline);
var
  Target: TGLShape;
  StateTarget: TX3DGraphTraverseState;
  MeshTarget: TAbstractComposedGeometryNode;
  CoordTarget: TMFVec3f;
  TexCoordTarget: TMFVec2f;
  IndexTarget: TInt32List;
begin
  { Shapes to which we merge should have SceneTransform = identity always. }
  Assert(TMatrix4.PerfectlyEquals(CollectedTarget.SceneTransform, TMatrix4.Identity));

  Target := CollectedTarget.Shape;

  { Clearing this is not really needed now, Merge always overrides it.
    But better clear it to be safe, instead of having potentially dangling pointer. }
  Target.OverrideRenderOptions := nil;
  CollectedTarget.RenderOptions := nil;

  StateTarget := Target.State;
  MeshTarget := Target.Geometry as TAbstractComposedGeometryNode;
  CoordTarget := MeshTarget.InternalCoordinates(StateTarget);
  CoordTarget.Items.Clear;
  IndexTarget := MeshTarget.CoordIndexField.Items;
  IndexTarget.Clear;
  if P in MergePipelinesWithTexCoord then
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

function TBatchShapes.GetPoolShapes(const Index: Integer): TGLShape;
var
  PipelinesCount: Integer;
begin
  PipelinesCount := Ord(High(TMergePipeline)) + 1;
  Assert(Index div MergeSlots < PipelinesCount);
  Result := FPool[TMergePipeline(Index div MergeSlots), Index mod MergeSlots].Shape;
end;

function TBatchShapes.PoolShapesCount: Integer;
var
  PipelinesCount: Integer;
begin
  PipelinesCount := Ord(High(TMergePipeline)) + 1;
  Result := MergeSlots * PipelinesCount;
end;

end.
