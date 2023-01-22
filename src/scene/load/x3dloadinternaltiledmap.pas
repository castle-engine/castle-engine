{
  Copyright 2020-2023 Matthias J. Molski, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert Tiled map (tmx file, see https://www.mapeditor.org/) into X3D representation.

  Underneath, the map is loaded using @link(TTiledMap) class,
  thus the map-reading logic is shared with TCastleTiledMapControl.

  @bold Unsupported features

  @orderedList(
    @item 90° rotated tiles (= vertical/horizontal flip + diagonal flip)
    @item Tiled image layers
    @item Tiled object ellipsoids
  )

  @bold X3D Hierarchy of a converted Tiled Map

  @preformatted(
    Root Node --> [0] Switch Node --> [0] Map Node --> [0] Layer Node 1 --> ...
                                                   --> [1] Layer Node 2 --> ...
                                                   ...
                                                   --> [n] Layer Node n+1 --> ...
                                  --> [1] Ghost Node --> [0] Tileset Shape Node 1
                                                     --> [1] Tileset Shape Node 2
                                                     ...
                                                     --> [m] Tileset Shape Node m+1
  )
  The "Map Node" contains the map as a scene graph.

  The "Ghost Node"
  holds all the nodes from which the "Map Node" is composed. It actually holds
  even all tiles (as ready to use shape nodes) provided by the tileset(s).
  From the outside you usually work on the "Map Node" and ignore
  the "Ghost Node".
}
unit X3DLoadInternalTiledMap;

{$I castleconf.inc}

interface

uses
  Classes,
  X3DNodes, CastleLog, CastleTiledMap;

{ Load Tiled map into X3D node.
  This is used by LoadNode, which in turn is used by TCastleSceneCore.Load.

  Underneath, this loads Tiled map using TTiledMap,
  then uses internal conversion class to generate X3D node from it. }
function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

implementation

uses
  SysUtils, Math, Generics.Collections,
  CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, CastleControls, CastleStringUtils,
  CastleImages;

type
  TShapeNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TShapeNode>;
  TShapeNodeListList = {$ifdef FPC}specialize{$endif} TObjectList<TShapeNodeList>;

  { Converter class to convert Tiled map into X3D representations.

    TODO:
    - SmoothScalingSafeBorder, similar to TCastleTiledMapControl, to fix borders on desert example
  }
  TTiledMapConverter = class
  strict private
    FMap: TTiledMap;
    FMapNode: TTransformNode;
    FRootNode: TX3DRootNode;
    FTilesetShapeNodeListList: TShapeNodeListList;

    { Converts tilesets to shape node lists.

      For every tileset in Map.Tilesets there is one TShapeNodeList created
      which holds one shape nodes for every tile of the tileset.

      @bold(IMPORTANT:) This procedure must ensure that the number of shape node lists
      is always the same as the actual tilesets in Map.Tilesets.
      This is crucial for retrieving the correct tileset shape node list from
      TilesetShapeNodeListList later! }
    procedure ConvertTilesets;
    { Tries to construct X3D nodes for each layer. }
    procedure ConvertLayers;

    procedure BuildObjectGroupLayerNode(const LayerNode: TTransformNode;
      const ALayer: TTiledMap.TLayer);

    procedure BuildTileLayerNode(const LayerNode: TTransformNode;
      const ALayer: TTiledMap.TLayer);

    { Convert Tiled coordinates to CGE.
      @groupBegin }
    function ConvY(const TiledCoord: TVector2): TVector2; overload;
    function ConvY(const X, Y: Single): TVector2; overload;
    { @groupEnd }

    { The elements of this list are themselves lists which contain
      the shape node of a tileset each. }
    property TilesetShapeNodeListList: TShapeNodeListList read FTilesetShapeNodeListList write FTilesetShapeNodeListList;
  public
    constructor Create(ATiledMap: TTiledMap);
    destructor Destroy; override;

    { Tries to construct X3D representation from TTiledMap data. }
    procedure ConvertMap;
    { The Tiled map is loaded from stream and NOT free'd automatically. }
    property Map: TTiledMap read FMap write FMap;
    { Holds the X3D representation of the Tiled map. It is NOT free'd
      automatically. Usually the X3D representation is added to a scene
      by Scene.Load(). The scene which will care about freeing. }
    property MapNode: TTransformNode read FMapNode write FMapNode;
    property RootNode: TX3DRootNode read FRootNode write FRootNode;
  end;

procedure TTiledMapConverter.ConvertMap;
begin
  ConvertTilesets;
  ConvertLayers;
end;

procedure TTiledMapConverter.ConvertTilesets;
type
  TTileFlip = (tfNoFlip, tfHFlip, tfVFlip, tfDFlip);
var
  Tile: TTiledMap.TTile;
  Tileset: TTiledMap.TTileset;
  TilesetTextureNode: TImageTextureNode;
  TileGeometryNode: TQuadSetNode;
  TileShapeNode: TShapeNode;
  TilesetTextureTransformNode: TTextureTransformNode;
  TilesetShapeNodeList: TShapeNodeList;

  { The ghost node holds all shape nodes of all tilesets and is added
    to the map node.
    If the map node is free'd, all tiles of the tilesets are free'd via
    the ghost node, even if they were not used as actual map tiles. }
  GhostNode: TX3DRootNode;
  TilesetTexCoordOrigin: TVector2;
  TileFlip: TTileFlip;

  { Calculate the number of rows (of tiles) of a tileset. }
  function RowsInTileset: Cardinal;
  begin
    Result := Tileset.TileCount div Tileset.Columns;
  end;

var
  TexProperties: TTexturePropertiesNode;
  Coord: TCoordinateNode;
  TexCoord: TTextureCoordinateNode;
begin
  TilesetTexCoordOrigin := TVector2.Zero;
  GhostNode := TX3DRootNode.Create;

  for Tileset in Map.Tilesets do
  begin
    { Make sure for each tileset there is a shape node list created
      for consistency and retrieving of correct item-indices later. }
    { Using false argument below, to not own (free) nodes. }
    TilesetShapeNodeList := TShapeNodeList.Create(False); // do not own
    TilesetTextureNode := nil;

    if Assigned(Tileset.Image) then
    begin
      { Prepare texture node of tileset. }
      TilesetTextureNode := TImageTextureNode.Create(Tileset.Name, '');
      TilesetTextureNode.SetUrl([Tileset.Image.URL]);

      TexProperties := TTexturePropertiesNode.Create;
      TexProperties.MagnificationFilter := magDefault;
      TexProperties.MinificationFilter := minDefault;
      TexProperties.BoundaryModeS := bmMirroredRepeat;
      TexProperties.BoundaryModeT := bmMirroredRepeat;
      { Do not force "power of 2" size, which may prevent mipmaps.
        Not resizing is consistent with X3DLoadInternalImage, LoadCastleSpriteSheet,
        users may use it with pixel-art and don't expect images to be resized. }
      TexProperties.GuiTexture := true;
      TilesetTextureNode.TextureProperties := TexProperties;
    end;

    if (Tileset.TileCount = 0) or (Tileset.Columns = 0) then
    begin
      WriteLnWarning('Empty Tileset');
      Continue;
    end;

    for TileFlip := Low(TTileFlip) to High(TTileFlip) do
    begin
      { No flip mode }
      if TileFlip = tfNoFlip then
        TilesetTexCoordOrigin := Vector2(0, 0);

      { Hint: The usual coordnate space covers [0, 1], but because of scaling
              the actual space may cover different dimensions.

        Example for horizontal flipping:
             If the scale factor is 0.5, the texture is expanded by
             factor 2, hence the (unflipped) texture is covered by [0, 2].
             For the flipped texture we do not look at 1 + ...
             but consequently at tileset width / tile width = 2. }

      { Horizontal flip }
      if TileFlip = tfHFlip then
        TilesetTexCoordOrigin := Vector2(Tileset.Image.Width / Tileset.TileWidth, 0);
      { Vertical flip }
      if TileFlip = tfVFlip then
        TilesetTexCoordOrigin := Vector2(0, Tileset.Image.Height / Tileset.Tileheight);
      { Diagonal flip }
      if TileFlip = tfDFlip then
        TilesetTexCoordOrigin := Vector2(Tileset.Image.Width / Tileset.TileWidth,
          Tileset.Image.Height / Tileset.Tileheight);

      for Tile in Tileset.Tiles do
      begin
        { Make tiles of tileset rectangular and with correct dimensions. }
        TileGeometryNode := TQuadSetNode.CreateWithShape(TileShapeNode);

        Coord := TCoordinateNode.Create;
        Coord.SetPoint([
          Vector3(0, 0, 0),
          Vector3(Tileset.TileWidth, 0, 0),
          Vector3(Tileset.TileWidth, Tileset.TileHeight, 0),
          Vector3(0, Tileset.TileHeight, 0)
        ]);
        TileGeometryNode.Coord := Coord;

        TexCoord := TTextureCoordinateNode.Create;
        TexCoord.SetPoint([
          Vector2(0, 0),
          Vector2(1, 0),
          Vector2(1, 1),
          Vector2(0, 1)
        ]);
        TileGeometryNode.TexCoord := TexCoord;

        { Make tiles textured and find correct texture coordinates. }
        TileShapeNode.Appearance := TAppearanceNode.Create;
        if Assigned(TilesetTextureNode) then
        begin
          TileShapeNode.Appearance.Texture := TilesetTextureNode;
          TilesetTextureTransformNode := TTextureTransformNode.Create;

          { Scale tileset texture to fit tile size:
            Scale factor is inverted: E. g. 0.5 means 2x dimension.
            Divide Tileset Tile width/height by full Tileset width/height.
            The latter is extracted from the texture node. }
          TilesetTextureTransformNode.Scale := Vector2(
            Tileset.TileWidth / Tileset.Image.Width,
            Tileset.TileHeight / Tileset.Image.Height
          );

          { Get all tile textures from tileset texture as shape nodes:
          Important: Origin (0/0) of tex. coordinate is bottom-left! }
          TilesetTextureTransformNode.Translation :=
            { Origin: Base coordinate depending on tile flip mode }
            TilesetTexCoordOrigin
            + Vector2(
            { X: Calc. Column (e. g. 0, 1, 2, ...) = No. of full tile widths
                   in tex. coord. space (0,0 to 1,0)
                 + Spacing: Col. * No. of spacings in tex. coord. space
                 + Margin in Tex. coord. space }
            (Tile.Id mod Tileset.Columns)
            + (Tile.Id mod Tileset.Columns) * (Tileset.Spacing / Tileset.TileWidth)
            + (Tileset.Margin / Tileset.TileWidth),
            { Y: Calc. Row (e. g. 3, 2, 1, 0 if 4 rows exist)
                 + Spacing: Row * Spacing in tex. coord. space
                 + Margin in Tex. coord. space }
            ((RowsInTileset - 1) - Floor(Tile.Id / Tileset.Columns))
            + ((RowsInTileset - 1) - Floor(Tile.Id / Tileset.Columns)) * (Tileset.Spacing / Tileset.TileWidth)
            + (Tileset.Margin / Tileset.TileHeight)
          );

          TileShapeNode.Appearance.TextureTransform := TilesetTextureTransformNode;
        end;

        TilesetShapeNodeList.Add(TileShapeNode);
        GhostNode.AddChildren(TileShapeNode);
      end;
    end;

    { Add list of shape nodes of this tileset to
      list of tileset shape node lists. }
    TilesetShapeNodeListList.Add(TilesetShapeNodeList);

    { Add ghost nodes to switch node. }
    (RootNode.FdChildren.Items[0] as TSwitchNode).AddChildren(GhostNode);
  end;
end;

procedure TTiledMapConverter.ConvertLayers;
var
  Layer: TTiledMap.TLayer;
  LayerNode: TTransformNode;
  LayerZ: Single;
const
  LayerZDistanceIncrease: Single = 10; //< Note: 1 is too small for examples/tiled/map_viewer/data/maps/desert_with_objects.tmx
begin
  LayerZ := 0;

  for Layer in Map.Layers do
  begin
    if not Layer.Visible then
      Continue;

    LayerNode := TTransformNode.Create;

    if Layer is TTiledMap.TObjectGroupLayer then
      BuildObjectGroupLayerNode(LayerNode, Layer)
    else
    { TODO:
    if Layer is TTiledMap.TImageLayer then
      BuildImageLayer(Layer, LayerNode)
    else }
      BuildTileLayerNode(LayerNode, Layer);

    MapNode.AddChildren(LayerNode);
    // flip -Layer.OffsetY, as Tiled Y goes down
    LayerNode.Translation := Vector3(Layer.OffsetX, -Layer.OffsetY, LayerZ);
    LayerZ := LayerZ + LayerZDistanceIncrease;
  end;
end;

procedure TTiledMapConverter.BuildObjectGroupLayerNode(const LayerNode: TTransformNode;
  const ALayer: TTiledMap.TLayer);
var
  // Material node of a Tiled obj.
  TiledObjectMaterial: TMaterialNode;
  // A Tiled object instance (as saved in TTiledMap).
  TiledObject: TTiledMap.TTiledObject;
  // Node of a Tiled object.
  TiledObjectNode: TTransformNode;
  // Geometry node of a TiledObject primitive.
  TiledObjectGeometry: TPolyline2DNode;
  // Shape node of a TiledObject.
  TiledObjectShape: TShapeNode;
  // Helper list.
  AVector2List: TVector2List;
  I: Cardinal;
begin
  TiledObjectMaterial := nil;
  TiledObjectNode := nil;
  TiledObjectGeometry := nil;
  TiledObjectShape := nil;
  AVector2List := nil;

  AVector2List := TVector2List.Create;

  for TiledObject in (ALayer as TTiledMap.TObjectGroupLayer).Objects do
  begin
    if not TiledObject.Visible then
      Continue;

    { All Tiled objects of this layer share the same material node. The color
      depends on the layer color in accordance with handling of Tiled editor. }
    if not Assigned(TiledObjectMaterial) then
    begin
      TiledObjectMaterial := TMaterialNode.Create;
      TiledObjectMaterial.EmissiveColor := ALayer.Color;
    end;

    { Every Tiled object is based on a transform node. }
    TiledObjectNode := TTransformNode.Create;
    TiledObjectNode.Translation := Vector3(
      TiledObject.Position.X,
      Map.Height * Map.TileHeight - TiledObject.Position.Y, // Tiled Y goes down, CGE Y goes up
      0
    );

    { Every primitive is implemented as polyline node. Hint: For better
      performance rectangle and point could be implemented as rect. node?}
    TiledObjectGeometry := TPolyline2DNode.CreateWithShape(TiledObjectShape);
    case TiledObject.Primitive of
      topPolyline, topPolygon:
        begin
          AVector2List.Clear;
          AVector2List.Assign(TiledObject.Points);
          for I := 0 to AVector2List.Count-1 do
            AVector2List.Items[I] := ConvY(AVector2List.Items[I]);

          { Polygon: Add point with index 0 to points list to get a closed polygon }
          if TiledObject.Primitive = topPolygon then
            AVector2List.Add(AVector2List.Items[0]);

          TiledObjectGeometry.SetLineSegments(AVector2List);
        end;
      topRectangle:
        begin
          TiledObjectGeometry.SetLineSegments([
            ConvY(0.0, 0.0),
            ConvY(TiledObject.Width, 0.0),
            ConvY(TiledObject.Width, TiledObject.Height),
            ConvY(0.0, TiledObject.Height),
            ConvY(0.0, 0.0)
          ]);
        end;
      topPoint:
        begin
          { TODO: Render points a X3D/OpenGL points, not rectangles. }
          AVector2List.Clear;
          { Construct a rectangle around position of point. }
          AVector2List.Add(ConvY(-1, -1));
          AVector2List.Add(ConvY(-1, 1));
          AVector2List.Add(ConvY(1, 1));
          AVector2List.Add(ConvY(1,-1));
          AVector2List.Add(ConvY(-1, -1));
          TiledObjectGeometry.SetLineSegments(AVector2List);
        end;
      // TODO: handle ellipse
      topEllipse:
        begin
          WritelnWarning('Not supported yet: Ellipse object primitive. Ignored.');
        end;
    end;
    TiledObjectShape.Material := TiledObjectMaterial;
    TiledObjectNode.AddChildren(TiledObjectShape);
    LayerNode.AddChildren(TiledObjectNode);
  end;
    FreeAndNil(AVector2List);
end;

procedure TTiledMapConverter.BuildTileLayerNode(const LayerNode: TTransformNode;
  const ALayer: TTiledMap.TLayer);

  { Get shape node textured correctly considering
    all the flipping bits (horizontal, vertical, diagonal bit).

    Tiled manual about flipping:
    ( https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#layer ) says:
    "When rendering a tile, the order of operation matters.
    The diagonal flip (x/y axis swap) is done first, followed
    by the horizontal and vertical flips." }
  function GetResolvedTileShapeNode(const ATileset: TTiledMap.TTileset;
    const ATile: TTiledMap.TTile;
    const HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean): TShapeNode;
  var
    ATilesetShapeNodeList: TShapeNodeList;
  begin
    Result := nil;

    { Get tileset's shape node list. }
    ATilesetShapeNodeList := TilesetShapeNodeListList.Items[
      Map.Tilesets.IndexOf(ATileset)];
    if not Assigned(ATilesetShapeNodeList) then
      Exit;

    { Get shape node from tileset list if no flip flags are set. }
    if not (HorizontalFlip or VerticalFlip or DiagonalFlip) then
    begin
      Result := ATilesetShapeNodeList.Items[ATileset.Tiles.IndexOf(ATile)];
      Exit;
    end;

    { Get shape node from tileset list if exclusivly the horizontal flip flag is set. }
    if HorizontalFlip and not (VerticalFlip or DiagonalFlip) then
    begin
      { Calc. of correct horizontally flipped tile:

        Tile indices:
        orig.       flipped
        Tex.        Tex.
        0 1 2       2 1 0
        3 4 5  -->  5 4 3
        6 7 8       8 7 6

        In the tileset shape node list, the flipped texture shape nodes follow
        directly after the original texture shape nodes.
        List indices: (0 1 2 3 4 5 6 7 8) (2 1 0 5 4 3 8 7 6) ...

        1. Shift index to start of indices of hor. flipped textures
        2. Shift to row (in which index lies)
        3. Add a row to be on the right side (correct by -1 bc. the last col.
           on the right side is adressed by column count - 1)
        4. Substract index within the correct row to correct index

        Ex. (see diagram above):
        Index of texture 3.
        1. TileCount (here 9) => Index 2 (flipped Tex.)
        2. Floor(3 / 3) = 1; with 3 Cols. => Index 5
        3. Add a row - 1 => Index 3
        4. Subract 3 mod 3 = 0 => Index 3
      }

      Result := ATilesetShapeNodeList.Items[
                  ATileset.TileCount
                  + Floor(ATileset.Tiles.IndexOf(ATile) / ATileset.Columns) * ATileset.Columns
                  + ATileset.Columns - 1
                  - ATileset.Tiles.IndexOf(ATile) mod ATileset.Columns
                ];
      Exit;
    end;

    { Get shape node from tileset list if exclusivly the vertical flip flag is set. }
    if VerticalFlip and not (HorizontalFlip or DiagonalFlip) then
    begin
      { Calc. of correct vertically flipped tile:

        Tile indices:
        orig.       flipped
        Tex.        Tex.
        0 1 2       6 7 8
        3 4 5  -->  3 4 5
        6 7 8       0 1 2

        In the tileset shape node list, the flipped texture shape nodes follow
        directly after the original texture shape nodes.
        List indices: (0 1 2 3 4 5 6 7 8) ... (horizontally flipped) ... (6 7 8...

        1. Shift index to start of indices of vert. flipped textures
        2. Shift into last row
        3. Substract "row index" of original texture
        4. Shift to correct texture in row
      }
      Result := ATilesetShapeNodeList.Items[
                  ATileset.TileCount * 2
                  + ((ATileset.TileCount div ATileset.Columns) - 1) * ATileset.Columns
                  - Floor(ATileset.Tiles.IndexOf(ATile) / ATileset.Columns) * ATileset.Columns
                  + ATileset.Tiles.IndexOf(ATile) mod ATileset.Columns
                ];
      Exit;
    end;

    { Ignore all flip flags if DiagonalFlip and additionally
      HorizontalFlip/VerticalFlip is set (results in rotation).
      TODO: Implement rotation. }
    if DiagonalFlip and (HorizontalFlip or VerticalFlip) then
    begin
      WritelnWarning('Not supported yet: Combination of diagonal- with other flips and rotations. Flags are ignored.');
      Result := ATilesetShapeNodeList.Items[ATileset.Tiles.IndexOf(ATile)];
      Exit;
    end;

    { Get shape node from tileset list if exclusivly the diagonal flip flag is set
      or if horizontal and vertical flip flags (= diagonal flip) are set. }
    if (DiagonalFlip and not (HorizontalFlip or VerticalFlip)) xor
       ((HorizontalFlip and VerticalFlip) and not DiagonalFlip) then
    begin
      { Calc. of correct diagonally flipped tile:

        Tile indices:
        orig.       flipped
        Tex.        Tex.
        0 1 2       8 7 6
        3 4 5  -->  5 4 3
        6 7 8       2 1 0

        In the tileset shape node list, the flipped texture shape nodes follow
        after the horizontally and vertically flipped texture shape nodes.
        List indices:
        (0 1 2 3 4 5 6 7 8) (horizon. flipped) (vert. flipped) (8 7 6 5 4 3 2 1 0)

        1. Shift index to start of indices of diag. flipped textures
        2. Shift into last row
        3. Substract "row index" of original texture
        4. Shift to last col.
        5. Shift back by index
      }
      Result := ATilesetShapeNodeList.Items[
                  ATileset.TileCount * 3
                  + ((ATileset.TileCount div ATileset.Columns) - 1) * ATileset.Columns
                  - Floor(ATileset.Tiles.IndexOf(ATile) / ATileset.Columns) * ATileset.Columns
                  + ATileset.Columns - 1
                  - ATileset.Tiles.IndexOf(ATile) mod ATileset.Columns
                ];
      Exit;
    end;
  end;

  procedure RenderTile(const X, Y: Integer);
  var
    Tileset: TTiledMap.TTileset;
    TileNode: TTransformNode;
    TileShapeNode: TShapeNode;
    HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
    Frame: Integer;
  begin
    if Map.TileRenderData(Vector2Integer(X, Y), ALayer,
      Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) then
    begin
      TileNode := TTransformNode.Create;
      TileNode.Translation := Vector3(Map.TileRenderPosition(Vector2Integer(X, Y)), 0);

      TileShapeNode := GetResolvedTileShapeNode(Tileset, Tileset.Tiles[Frame],
        HorizontalFlip, VerticalFlip, DiagonalFlip);

      TileNode.AddChildren(TileShapeNode);
      LayerNode.AddChildren(TileNode);
    end;
  end;

var
  X, Y: Integer;
begin
  for Y := Map.Height - 1 downto 0 do
    for X := 0 to Map.Width - 1 do
      RenderTile(X, Y);
end;

constructor TTiledMapConverter.Create(ATiledMap: TTiledMap);
var
  SwitchNode: TSwitchNode;
begin
  inherited Create;

  Map := ATiledMap;

  { Create scene's initial node structure: Root --> Switch --> Map }
  RootNode := TX3DRootNode.Create;
  SwitchNode := TSwitchNode.Create;
  RootNode.AddChildren(SwitchNode);
  MapNode := TTransformNode.Create;
  SwitchNode.AddChildren(MapNode);
  SwitchNode.WhichChoice := 0;

  TilesetShapeNodeListList := TShapeNodeListList.Create(True);
end;

destructor TTiledMapConverter.Destroy;
begin
  FreeAndNil(FTilesetShapeNodeListList);

  inherited Destroy;
end;

function TTiledMapConverter.ConvY(const TiledCoord: TVector2): TVector2;
begin
  Result := Vector2(TiledCoord.X, - TiledCoord.Y);
end;

function TTiledMapConverter.ConvY(const X, Y: Single): TVector2;
begin
  Result := ConvY(Vector2(X, Y));
end;

function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  TiledMapFromStream: TTiledMap;
  TiledMapConverter: TTiledMapConverter;
begin
  { The Tiled converter unit expects a TTiledMap object instance,
    hence create one. }
  TiledMapFromStream := TTiledMap.Create(Stream, BaseUrl);
  try
    TiledMapConverter := TTiledMapConverter.Create(TiledMapFromStream);
    try
      TiledMapConverter.ConvertMap;
      Result := TiledMapConverter.RootNode;
    finally FreeAndNil(TiledMapConverter) end;
  finally FreeAndNil(TiledMapFromStream) end;
end;

end.
