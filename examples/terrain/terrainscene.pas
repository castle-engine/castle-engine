{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Express terrain as TCastleScene. }
unit TerrainScene;

interface

uses Classes,
  CastleVectors, CastleTerrain, CastleScene, X3DNodes, X3DFields,
  CastleRendererBaseTypes;

type
  TTerrainScene = class(TCastleScene)
  private
    Geometry: TIndexedTriangleStripSetNode;
    CoordNode: TCoordinateNode;
    NormalNode: TNormalNode;

    TextureHeights: array [0..3] of Single;
    UVScale: array [1..3] of Single;
    TextureMix, NormalDark, NormalDarkening: Single;

    TextureHeightsFields: array [0..3] of TSFFloat;
    UVScaleFields: array [1..3] of TSFFloat;
  public
    constructor Create(AOwner: TComponent); override;

    { Regenerate geometry (vertexes, normals etc.) to show the current
      Terrain instance. This only calls @link(TTerrain.Height) method.
      BaseSize * 2 is the size of the square layer around the (MiddleX, MiddleY).

      TODO: This does not use TTerrain.Node, to maximize the speed of updating.
      Eventually, we would like to merge these two approaches (TTerrain.Node
      and TTerrainScene.Regenerate) as they really do the same: convert
      TTerrain height information into X3D nodes. }
    procedure Regenerate(Terrain: TTerrain;
      const Subdivision: Cardinal; const BaseSize: Single);

    { Adjust Appearence node to use our terrain shaders. }
    procedure AdjustAppearance(Appearance: TAppearanceNode);
  end;

implementation

uses SysUtils,
  CastleUtils, CastleFilesUtils;

constructor TTerrainScene.Create(AOwner: TComponent);

  procedure SetDefaultShaderUniforms;
  begin
    TextureHeights[0] := 5;
    TextureHeights[1] := 6;
    TextureHeights[2] := 7;
    TextureHeights[3] := 10;
    UVScale[1] := 0.11;
    UVScale[2] := 0.26;
    UVScale[3] := 0.36;
    TextureMix := 1;
    NormalDark := 0.94;
    NormalDarkening := 0.3;
  end;

  procedure LoadInitialScene;
  var
    Shape: TShapeNode;
    Root: TX3DRootNode;
  begin
    Geometry := TIndexedTriangleStripSetNode.Create;

    CoordNode := TCoordinateNode.Create;
    Geometry.Coord := CoordNode;

    NormalNode := TNormalNode.Create;
    Geometry.Normal := NormalNode;

    Shape := TShapeNode.Create;
    Shape.Geometry := Geometry;
    Shape.Appearance := TAppearanceNode.Create;
    AdjustAppearance(Shape.Appearance);

    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);

    Load(Root, true);
  end;

begin
  inherited;
  SetDefaultShaderUniforms;
  LoadInitialScene;
end;

procedure TTerrainScene.AdjustAppearance(Appearance: TAppearanceNode);
var
  Effect: TEffectNode;
  VertexPart, FragmentPart: TEffectPartNode;
  Tex1, Tex2, Tex3: TImageTextureNode;
  I: Integer;
begin
  { initialize Effect node, for a shader effect }
  Effect := TEffectNode.Create;
  Effect.Language := slGLSL;
  Appearance.SetEffects([Effect]);

  { pass textures to shader effect }
  Tex1 := TImageTextureNode.Create;
  Tex1.SetUrl([ApplicationData('textures/island_sand2_d.jpg')]);
  Tex2 := TImageTextureNode.Create;
  Tex2.SetUrl([ApplicationData('textures/ground_mud2_d.jpg')]);
  Tex3 := TImageTextureNode.Create;
  Tex3.SetUrl([ApplicationData('textures/mntn_white_d.jpg')]);

  Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_1', [], Tex1));
  Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_2', [], Tex2));
  Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_3', [], Tex3));

  { pass uniforms to shader effect }
  for I := Low(TextureHeights) to High(TextureHeights) do
  begin
    TextureHeightsFields[I] := TSFFloat.Create(
      Effect, true, 'h' + IntToStr(I), TextureHeights[I]);
    Effect.AddCustomField(TextureHeightsFields[I]);
  end;
  for I := Low(UVScale) to High(UVScale) do
  begin
    UVScaleFields[I] := TSFFloat.Create(
      Effect, true, 'uv_scale_' + IntToStr(I), UVScale[I]);
    Effect.AddCustomField(UVScaleFields[I]);
  end;
  Effect.AddCustomField(TSFFloat.Create(Effect, true, 'texture_mix', TextureMix));
  Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_dark', NormalDark));
  Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_darkening', NormalDarkening));

  { initialize 2 EffectPart nodes (one for vertex shader, one for fragment shader) }
  FragmentPart := TEffectPartNode.Create;
  FragmentPart.ShaderType := stFragment;
  FragmentPart.SetUrl([ApplicationData('shaders/terrain.fs')]);

  VertexPart := TEffectPartNode.Create;
  VertexPart.ShaderType := stVertex;
  VertexPart.SetUrl([ApplicationData('shaders/terrain.vs')]);

  Effect.SetParts([FragmentPart, VertexPart]);

  Appearance.Material := TMaterialNode.Create;
end;

procedure TTerrainScene.Regenerate(Terrain: TTerrain;
  const Subdivision: Cardinal; const BaseSize: Single);
var
  CountSteps, CountSteps1: Cardinal;
  X1, Z1, X2, Z2: Single;

  procedure RegenerateElevationGrid;
  var
    Shape: TShapeNode;
    Root: TX3DRootNode;
  begin
    Shape := Terrain.CreateNode(CountSteps, X2 - X1,
      Vector2(X1, X2),
      Vector2(Z1, Z2));
    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);
    Load(Root, true);
    AdjustAppearance(Shape.Appearance);
  end;

var
  Coord, Normal: TVector3List;
  Index: TLongIntList;

  procedure CalculatePosition(const I, J: Cardinal; out Position: TVector3);
  begin
    { set XZ to cover (X1, Z1) ... (X2, Z2) rectangle with our terrain }
    Position[0] := (X2 - X1) * I / (CountSteps-1) + X1;
    Position[2] := (Z2 - Z1) * J / (CountSteps-1) + Z1;
    Position[1] := Terrain.Height(Position[0], Position[2]);
  end;

  procedure CalculateNormal(const I, J: Cardinal; out Normal: TVector3);
  var
    P, PX, PY: PVector3;
  begin
    P  := Coord.Ptr( I      * CountSteps1 + J);
    PX := Coord.Ptr((I + 1) * CountSteps1 + J);
    PY := Coord.Ptr( I      * CountSteps1 + J + 1);

    { TODO: this is actually normal vector of 1 of the four faces around this
      vertex. Optimally, we should calculate normals on all faces,
      and for vertex normal take average. }
    Normal := TVector3.CrossProduct(
      (PX^ - P^),
      (PY^ - P^)).Normalize;
  end;

var
  I, J, Idx: Cardinal;
  IndexPtr: PLongInt;
begin
  X1 := - BaseSize;
  Z1 := - BaseSize;
  X2 := + BaseSize;
  Z2 := + BaseSize;

  { CountSteps-1 squares (edges) along the way,
    CountSteps points along the way.
    Calculate positions for CountSteps + 1 points
    (+ 1 additional for normal calculation). }
  CountSteps := 1 shl Subdivision + 1;
  CountSteps1 := CountSteps + 1;

  // TODO: for testing
  // RegenerateElevationGrid;
  // Exit;

  Index := Geometry.FdIndex.Items;
  Coord := CoordNode.FdPoint.Items;
  Normal := NormalNode.FdVector.Items;

  { We will render CountSteps^2 points, but we want to calculate
    (CountSteps + 1)^2 points : to be able to calculate normal vectors.
    Normals for the last row and last column will not be calculated,
    and will not be used. }
  Coord.Count := Sqr(CountSteps1);
  Normal.Count := Sqr(CountSteps1);

  { calculate Coord }
  for I := 0 to CountSteps do
    for J := 0 to CountSteps do
    begin
      Idx := I * CountSteps1 + J;
      CalculatePosition(I, J, Coord.List^[Idx]);
    end;
  CoordNode.FdPoint.Changed;

  { calculate Normals }
  for I := 0 to CountSteps - 1 do
    for J := 0 to CountSteps - 1 do
    begin
      Idx := I * CountSteps1 + J;
      CalculateNormal(I, J, Normal.List^[Idx]);
    end;
  NormalNode.FdVector.Changed;

  { calculate Index }
  Index.Count := (CountSteps - 1) * (CountSteps * 2 + 1);
  IndexPtr := PLongInt(Index.List);
  for I := 1 to CountSteps - 1 do
  begin
    for J := 0 to CountSteps - 1 do
    begin
      // order to make it CCW when viewed from above
      IndexPtr^ :=  I      * CountSteps1 + J; Inc(IndexPtr);
      IndexPtr^ := (I - 1) * CountSteps1 + J; Inc(IndexPtr);
    end;
    IndexPtr^ := -1;
    Inc(IndexPtr);
  end;
  Geometry.FdIndex.Changed;
end;

end.
