{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Internal data for shadow volumes rendering in shapes. }
unit CastleShapeInternalShadowVolumes;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleVectors, CastleTriangles;

type
  { Edge that is between exactly two triangles.
    It's used by @link(TShapeShadowVolumes.ManifoldEdges),
    and this is crucial for rendering silhouette shadow volumes in OpenGL. }
  TManifoldEdge = record
    { Index to get vertexes of this edge.

      During rendering, the edge is defined by these two vertexes:
      - Triangles[0][VertexIndex]
      - Triangles[0][(VertexIndex + 1) mod 3]

      These are indexes to @link(TShapeShadowVolumes.TrianglesListShadowCasters)
      in turn. This way if a shape changes during animation,
      we need to recalculate @link(TShapeShadowVolumes.TrianglesListShadowCasters),
      but the @link(TShapeShadowVolumes.ManifoldEdges) and
      @link(TShapeShadowVolumes.BorderEdges) may stay unchanged. }
    VertexIndex: Cardinal;

    { Indexes to @link(TShapeShadowVolumes.TrianglesListShadowCasters) array }
    Triangles: array [0..1] of Cardinal;

    { These are vertex values at VertexIndex and (VertexIndex+1)mod 3 positions,
      but @italic(only at generation of manifold edges time).
      Updating this data later would be costly.
      However, using these when generating
      makes a great speed-up when generating manifold edges.

      Memory cost is acceptable: assume we have model with 10 000 faces,
      so 15 000 edges (assuming it's correctly closed manifold), so we waste
      15 000 * 2 * SizeOf(TVector3) = 360 000 bytes... that's really nothing
      to worry (we waste much more on other things).

      Checked with "The Castle": indeed, this costs about 1 MB memory
      (out of 218 MB...), with really lot of creatures... On the other hand,
      this causes small speed-up when loading: loading creatures is about
      5 seconds faster (18 with, 23 without this).

      So memory loss is small, speed gain is noticeable (but still small),
      implementation code is a little simplified, so we're keeping this for now. }
    V0, V1: TVector3;
  end;
  PManifoldEdge = ^TManifoldEdge;

  TManifoldEdgeList = {$ifdef FPC}specialize{$endif} TStructList<TManifoldEdge>;

  { Edge that has one neighbor, i.e. border edge.
    It's used by @link(TShapeShadowVolumes.BorderEdges),
    and this is crucial for rendering silhouette shadow volumes in OpenGL. }
  TBorderEdge = record
    { Index to get vertex of this edge.
      The actual edge's vertexes are not recorded here (this way
      we don't have to update this when the shape changes during animation).
      You should get them as the VertexIndex
      and (VertexIndex+1) mod 3 vertexes of the triangle TriangleIndex. }
    VertexIndex: Cardinal;

    { Index to @link(TShapeShadowVolumes.TrianglesListShadowCasters) array. }
    TriangleIndex: Cardinal;
  end;
  PBorderEdge = ^TBorderEdge;

  TBorderEdgeList = {$ifdef FPC}specialize{$endif} TStructList<TBorderEdge>;

  { Triangles array for shadow casting shape. In local shape coordinates. }
  TTrianglesShadowCastersList = TTriangle3List;

  TShapeShadowVolumes = class
  strict private
    type
      TValidities = set of (
        svTrianglesListShadowCasters,
        svManifoldAndBorderEdges
      );
    var
    Validities: TValidities;
    FTrianglesListShadowCasters: TTrianglesShadowCastersList;
    FManifoldEdges: TManifoldEdgeList;
    FBorderEdges: TBorderEdgeList;
    procedure CalculateIfNeededManifoldAndBorderEdges;
  public
    FShape: TObject;

    constructor Create(const AShape: TObject);
    destructor Destroy; override;

    { Removes svManifoldAndBorderEdges from Validities,
      and clears FManifold/BordEdges variables. }
    procedure InvalidateManifoldAndBorderEdges;

    { Removes svTrianglesListShadowCasters from Validities,
      and clears FTrianglesListShadowCasters variable. }
    procedure InvalidateTrianglesListShadowCasters;

    { Returns an array of triangles that should be shadow casters
      for this scene.

      Results of these functions are cached, and are also owned by this object.
      So don't modify it, don't free it. }
    function TrianglesListShadowCasters: TTrianglesShadowCastersList;

    { ManifoldEdges is a list of edges that have exactly @bold(two) neighbor
      triangles, and BorderEdges is a list of edges that have exactly @bold(one)
      neighbor triangle. These are crucial for rendering shadows using shadow
      volumes.

      Edges with more than two neighbors are allowed. If an edge has an odd
      number of neighbors, it will be placed in BorderEdges. Every other pair
      of neighbors will be "paired" and placed as one manifold edge inside
      ManifoldEdges. So actually edge with exactly 1 neighbor (odd number,
      so makes one BorderEdges item) and edge with exactly 2 neighbors
      (even number, one pair of triangles, makes one item in ManifoldEdges)
      --- they are just a special case of a general rule, that allows any
      neighbors number.

      Note that vertexes must be consistently ordered in triangles.
      For two neighboring triangles, if one triangle's edge has
      order V0, V1, then on the neighbor triangle the order must be reversed
      (V1, V0). This is true in almost all situations, for example
      if you have a closed solid object and all outside faces are ordered
      consistently (all CCW or all CW).
      Failure to order consistently will result in edges not being "paired",
      i.e. we will not recognize that some 2 edges are in fact one edge between
      two neighboring triangles --- and this will result in more edges in
      BorderEdges.

      Both of these lists are calculated at once, i.e. when you call ManifoldEdges
      or BorderEdges for the 1st time, actually both ManifoldEdges and
      BorderEdges are calculated at once. If all edges are in ManifoldEdges,
      then the scene is a correct closed manifold, or rather it's composed
      from any number of closed manifolds.

      Results of these functions are cached, and are also owned by this object.
      So don't modify it, don't free it.

      This uses TrianglesListShadowCasters.

      @groupBegin }
    function ManifoldEdges: TManifoldEdgeList;
    function BorderEdges: TBorderEdgeList;
    { @groupEnd }

    procedure PrepareResources;
    procedure FreeResources;
  end;

implementation

uses SysUtils,
  CastleShapes, X3DNodes, CastleLog, CastleTransform,
  CastleRenderOptions;

constructor TShapeShadowVolumes.Create(const AShape: TObject);
begin
  inherited Create;
  FShape := AShape;
end;

destructor TShapeShadowVolumes.Destroy;
begin
  { free FTrianglesList* variables }
  InvalidateTrianglesListShadowCasters;
  { frees FManifoldEdges, FBorderEdges if needed }
  InvalidateManifoldAndBorderEdges;
  inherited;
end;

{ triangles list ------------------------------------------------------------- }

type
  TTriangleAdder = class
    TriangleList: TTriangle3List;
    procedure AddTriangle(Shape: TObject;
      const Triangle: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  end;

procedure TTriangleAdder.AddTriangle(Shape: TObject;
  const Triangle: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  if Triangle.IsValid then
    TriangleList.Add(Triangle);
end;

function TShapeShadowVolumes.TrianglesListShadowCasters: TTrianglesShadowCastersList;

  function CreateTrianglesListShadowCasters: TTrianglesShadowCastersList;

    function ShadowCaster(AShape: TShape): boolean;
    var
      Shape: TAbstractShapeNode;
    begin
      Shape := AShape.State.ShapeNode;
      Result := not (
        (Shape <> nil) and
        (Shape.FdAppearance.Value <> nil) and
        (Shape.FdAppearance.Value is TAppearanceNode) and
        (not TAppearanceNode(Shape.FdAppearance.Value).FdShadowCaster.Value));
    end;

  var
    TriangleAdder: TTriangleAdder;
    Shape: TShape;
  begin
    Shape := TShape(FShape);

    Result := TTrianglesShadowCastersList.Create;
    try
      Result.Capacity := Shape.TrianglesCount;
      TriangleAdder := TTriangleAdder.Create;
      try
        TriangleAdder.TriangleList := Result;
        if ShadowCaster(Shape) then
          Shape.LocalTriangulate({$ifdef FPC}@{$endif}TriangleAdder.AddTriangle);
        if LogShadowVolumes then
          WritelnLog('Shadow volumes', Format('Shadows casters triangles: %d',
            [Result.Count]));
      finally FreeAndNil(TriangleAdder) end;
    except Result.Free; raise end;
  end;

begin
  if not (svTrianglesListShadowCasters in Validities) then
  begin
    FreeAndNil(FTrianglesListShadowCasters);
    FTrianglesListShadowCasters := CreateTrianglesListShadowCasters;
    Include(Validities, svTrianglesListShadowCasters);
  end;

  Result := FTrianglesListShadowCasters;
end;

procedure TShapeShadowVolumes.InvalidateTrianglesListShadowCasters;
begin
  Exclude(Validities, svTrianglesListShadowCasters);
  FreeAndNil(FTrianglesListShadowCasters);
end;

{ edges lists ------------------------------------------------------------- }

procedure TShapeShadowVolumes.CalculateIfNeededManifoldAndBorderEdges;

  { Sets FManifoldEdges and FBorderEdges. Assumes that FManifoldEdges and
    FBorderEdges are @nil on enter. }
  procedure CalculateManifoldAndBorderEdges;

    { If the counterpart of this edge (edge from neighbor) exists in
      EdgesSingle, then it adds this edge (along with it's counterpart)
      to FManifoldEdges.

      Otherwise, it just adds the edge to EdgesSingle. This can happen
      if it's the 1st time this edge occurs, or maybe the 3d one, 5th...
      all odd occurrences, assuming that ordering of faces is consistent,
      so that counterpart edges are properly detected. }
    procedure AddEdgeCheckManifold(
      EdgesSingle: TManifoldEdgeList;
      const TriangleIndex: Cardinal;
      const V0: TVector3;
      const V1: TVector3;
      const VertexIndex: Cardinal;
      Triangles: TTriangle3List);
    var
      I: Integer;
      EdgePtr: PManifoldEdge;
    begin
      if EdgesSingle.Count <> 0 then
      begin
        EdgePtr := PManifoldEdge(EdgesSingle.L);
        for I := 0 to EdgesSingle.Count - 1 do
        begin
          { It would also be possible to get EdgePtr^.V0/1 by code like

            TrianglePtr := @Triangles.L[EdgePtr^.Triangles[0]];
            EdgeV0 := @TrianglePtr^[EdgePtr^.VertexIndex];
            EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + 1) mod 3];

            But, see TManifoldEdge.V0/1 comments --- current version is
            a little faster.
          }

          { Triangles must be consistently ordered on a manifold,
            so the second time an edge is present, we know it must
            be in different order. So we compare V0 with EdgeV1
            (and V1 with EdgeV0), no need to compare V1 with EdgeV1. }
          if TVector3.PerfectlyEquals(V0, EdgePtr^.V1) and
             TVector3.PerfectlyEquals(V1, EdgePtr^.V0) then
          begin
            EdgePtr^.Triangles[1] := TriangleIndex;

            { Move edge to FManifoldEdges: it has 2 neighboring triangles now. }
            FManifoldEdges.Add^ := EdgePtr^;

            { Remove this from EdgesSingle.
              Note that we delete from EdgesSingle fast, using assignment and
              deleting only from the end (normal Delete would want to shift
              EdgesSingle contents in memory, to preserve order of items;
              but we don't care about order). }
            EdgePtr^ := EdgesSingle.L[EdgesSingle.Count - 1];
            EdgesSingle.Count := EdgesSingle.Count - 1;

            Exit;
          end;
          Inc(EdgePtr);
        end;
      end;

      { New edge: add new item to EdgesSingle }
      EdgePtr := PManifoldEdge(EdgesSingle.Add);
      EdgePtr^.VertexIndex := VertexIndex;
      EdgePtr^.Triangles[0] := TriangleIndex;
      EdgePtr^.V0 := V0;
      EdgePtr^.V1 := V1;
    end;

  var
    I: Integer;
    Triangles: TTriangle3List;
    TrianglePtr: PTriangle3;
    EdgesSingle: TManifoldEdgeList;
  begin
    Assert(FManifoldEdges = nil);
    Assert(FBorderEdges = nil);

    { It's important here that TrianglesListShadowCasters guarantees that only
      valid triangles are included. Otherwise degenerate triangles could make
      shadow volumes rendering result bad. }
    Triangles := TrianglesListShadowCasters;

    FManifoldEdges := TManifoldEdgeList.Create;
    { There is a precise relation between number of edges and number of faces
      on a closed manifold: E = T * 3 / 2. }
    FManifoldEdges.Capacity := Triangles.Count * 3 div 2;

    { EdgesSingle are edges that have no neighbor,
      i.e. have only one adjacent triangle. At the end, what's left here
      will be simply copied to BorderEdges. }
    EdgesSingle := TManifoldEdgeList.Create;
    try
      EdgesSingle.Capacity := Triangles.Count * 3 div 2;

      TrianglePtr := PTriangle3(Triangles.L);
      for I := 0 to Triangles.Count - 1 do
      begin
        { TrianglePtr points to Triangles[I] now }
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^.Data[0], TrianglePtr^.Data[1], 0, Triangles);
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^.Data[1], TrianglePtr^.Data[2], 1, Triangles);
        AddEdgeCheckManifold(EdgesSingle, I, TrianglePtr^.Data[2], TrianglePtr^.Data[0], 2, Triangles);
        Inc(TrianglePtr);
      end;

      FBorderEdges := TBorderEdgeList.Create;

      if EdgesSingle.Count <> 0 then
      begin
        { scene not a perfect manifold: less than 2 faces for some edges
          (the case with more than 2 is already eliminated above).
          So we copy EdgesSingle to BorderEdges. }
        FBorderEdges.Count := EdgesSingle.Count;
        for I := 0 to EdgesSingle.Count - 1 do
        begin
          FBorderEdges.L[I].VertexIndex := EdgesSingle.L[I].VertexIndex;
          FBorderEdges.L[I].TriangleIndex := EdgesSingle.L[I].Triangles[0];
        end;
      end;
    finally FreeAndNil(EdgesSingle); end;

    if LogShadowVolumes then
      WritelnLog('Shadow volumes', Format(
        'Edges: %d manifold, %d border',
        [FManifoldEdges.Count, FBorderEdges.Count] ));
  end;

begin
  if not (svManifoldAndBorderEdges in Validities) then
  begin
    CalculateManifoldAndBorderEdges;
    Include(Validities, svManifoldAndBorderEdges);
  end;
end;

function TShapeShadowVolumes.ManifoldEdges: TManifoldEdgeList;
begin
  CalculateIfNeededManifoldAndBorderEdges;
  Result := FManifoldEdges;
end;

function TShapeShadowVolumes.BorderEdges: TBorderEdgeList;
begin
  CalculateIfNeededManifoldAndBorderEdges;
  Result := FBorderEdges;
end;

procedure TShapeShadowVolumes.InvalidateManifoldAndBorderEdges;
begin
  Exclude(Validities, svManifoldAndBorderEdges);
  FreeAndNil(FManifoldEdges);
  FreeAndNil(FBorderEdges);
end;

procedure TShapeShadowVolumes.PrepareResources;
begin
  TrianglesListShadowCasters;
  ManifoldEdges;
end;

procedure TShapeShadowVolumes.FreeResources;
begin
  InvalidateTrianglesListShadowCasters;
  InvalidateManifoldAndBorderEdges;
end;

end.
