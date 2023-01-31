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

  Underneath, the map is loaded using @link(TCastleTiledMapData) class,
  thus the map-reading logic is shared with TCastleTiledMapControl.

  TODO:

  @orderedList(
    @item(Tiled image layers)
    @item(Tiled object ellipsoids)
    @item(At extreme zooms, seams when rendering may appear (regardless of smooth or not filtering,
      regardless of doing Round() on final coords).)
  )
}
unit X3DLoadInternalTiledMap;

{$I castleconf.inc}

interface

uses
  Classes,
  X3DNodes, CastleLog, CastleTiledMap, CastleVectors;

{ Load Tiled map into X3D node.
  This is used by LoadNode, which in turn is used by TCastleSceneCore.Load.

  Underneath, this loads Tiled map using TCastleTiledMapData,
  then uses internal conversion class to generate X3D node from it. }
function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

type
  { Convert Tiled map into X3D node. }
  TCastleTiledMapConverter = class
  strict private
    FMap: TCastleTiledMapData;
    FMapNode: TTransformNode;
    FRootNode: TX3DRootNode;

    { Fills every TTileset.RendererData with TAppearanceNode with texture of this tileset. }
    procedure PrepareTilesets;

    { Constructs X3D nodes for each layer. }
    procedure ConvertLayers;

    procedure FreeUnusedTilesetsRendererData;

    procedure BuildObjectGroupLayerNode(const LayerNode: TTransformNode;
      const ALayer: TCastleTiledMapData.TLayer);

    procedure BuildTileLayerNode(const LayerNode: TTransformNode;
      const ALayer: TCastleTiledMapData.TLayer);

    { Convert Tiled coordinates to CGE.
      @groupBegin }
    function ConvY(const TiledCoord: TVector2): TVector2; overload;
    function ConvY(const X, Y: Single): TVector2; overload;
    { @groupEnd }

    { The Tiled map is loaded from stream and NOT free'd automatically. }
    property Map: TCastleTiledMapData read FMap write FMap;

    { Holds the X3D representation of the Tiled map. It is NOT free'd
      automatically. Usually the X3D representation is added to a scene
      by Scene.Load(). The scene which will care about freeing. }
    property MapNode: TTransformNode read FMapNode write FMapNode;
  public
    type
      TLayerIndex = 0..30;
      TLayers = set of TLayerIndex;
    const
      AllLayers = [Low(TLayerIndex)..High(TLayerIndex)];
    var
      { Workaround rendering artifacts for tilesets without alpha bleeding.
        Set before @link(ConvertMap). }
      SmoothScalingSafeBorder: Boolean;

      { Layers to load.  }
      Layers: TLayers;

    constructor Create(const ATiledMap: TCastleTiledMapData);
    destructor Destroy; override;

    { Constructs RootNode from TCastleTiledMapData data. }
    procedure ConvertMap;

    property RootNode: TX3DRootNode read FRootNode write FRootNode;
  end;

implementation

uses
  SysUtils, Math, Generics.Collections,
  CastleTransform, CastleColors, CastleRectangles, CastleUtils,
  CastleRenderOptions, CastleControls, CastleStringUtils,
  CastleImages, CastleURIUtils;

procedure TCastleTiledMapConverter.ConvertMap;
begin
  PrepareTilesets;
  ConvertLayers;
  FreeUnusedTilesetsRendererData;
end;

procedure TCastleTiledMapConverter.FreeUnusedTilesetsRendererData;
var
  Appearance: TAppearanceNode;
  Tileset: TCastleTiledMapData.TTileset;
begin
  for Tileset in Map.Tilesets do
  begin
    Appearance := Tileset.RendererData as TAppearanceNode;
    FreeIfUnusedAndNil(Appearance);
    Tileset.RendererData := nil;
  end;
end;

procedure TCastleTiledMapConverter.PrepareTilesets;
var
  Texture: TImageTextureNode;
  TexProperties: TTexturePropertiesNode;
  Appearance: TAppearanceNode;
var
  Tileset: TCastleTiledMapData.TTileset;
begin
  for Tileset in Map.Tilesets do
  begin
    if Tileset.Image = nil then
    begin
      WriteLnWarning('Tileset has no image');
      Continue;
    end;

    if (Tileset.TileCount = 0) or (Tileset.Columns = 0) then
    begin
      WriteLnWarning('Empty Tileset');
      Continue;
    end;

    Texture := TImageTextureNode.Create;
    Texture.SetUrl([Tileset.Image.URL]);

    TexProperties := TTexturePropertiesNode.Create;
    TexProperties.MagnificationFilter := magDefault;
    TexProperties.MinificationFilter := minDefault;
    { Do not force "power of 2" size, which may prevent mipmaps.
      Not resizing is consistent with X3DLoadInternalImage, LoadCastleSpriteSheet,
      users may use it with pixel-art and don't expect images to be resized. }
    TexProperties.GuiTexture := true;
    Texture.TextureProperties := TexProperties;

    Appearance := TAppearanceNode.Create(Tileset.Name, '');
    Appearance.Texture := Texture;
    Tileset.RendererData := Appearance;
  end;
end;

procedure TCastleTiledMapConverter.ConvertLayers;
var
  LayerIndex: Integer;
  Layer: TCastleTiledMapData.TLayer;
  LayerNode: TTransformNode;
  LayerZ: Single;
const
  { Distance between Tiled layers in Z. Layers are rendered as 3D objects
    and need some distance to avoid Z-fighting.

    This could be avoided when using RenderContext.DepthFunc := fdAlways,
    but it comes with it's own disadvantages,
    see TCastleTiledMap.AssumePerfectRenderingOrder docs.
    So we always apply this layer Z distance, to work regardless
    of AssumePerfectRenderingOrder.

    Note: 1 is too small for examples/tiled/map_viewer/data/maps/desert_with_objects.tmx }
  LayerZDistanceIncrease: Single = 10;
begin
  LayerZ := 0;

  for LayerIndex := 0 to Map.Layers.Count - 1 do
  begin
    Layer := Map.Layers[LayerIndex];
    if not (
         Layer.Visible and
         (
           (LayerIndex > High(TLayerIndex)) or
           (LayerIndex in Layers)
         )
       ) then
      Continue;

    LayerNode := TTransformNode.Create;

    if Layer is TCastleTiledMapData.TObjectGroupLayer then
      BuildObjectGroupLayerNode(LayerNode, Layer)
    else
    { TODO:
    if Layer is TCastleTiledMapData.TImageLayer then
      BuildImageLayer(Layer, LayerNode)
    else }
      BuildTileLayerNode(LayerNode, Layer);

    MapNode.AddChildren(LayerNode);
    // flip -Layer.OffsetY, as Tiled Y goes down
    LayerNode.Translation := Vector3(Layer.OffsetX, -Layer.OffsetY, LayerZ);
    LayerZ := LayerZ + LayerZDistanceIncrease;
  end;
end;

procedure TCastleTiledMapConverter.BuildObjectGroupLayerNode(const LayerNode: TTransformNode;
  const ALayer: TCastleTiledMapData.TLayer);
var
  // Material node of a Tiled obj.
  TiledObjectMaterial: TMaterialNode;
  // A Tiled object instance (as saved in TCastleTiledMapData).
  TiledObject: TCastleTiledMapData.TTiledObject;
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

  for TiledObject in (ALayer as TCastleTiledMapData.TObjectGroupLayer).Objects do
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

procedure TCastleTiledMapConverter.BuildTileLayerNode(const LayerNode: TTransformNode;
  const ALayer: TCastleTiledMapData.TLayer);

  function GetTileCoordRect(const TilePosition: TVector2Integer;
    const Tileset: TCastleTiledMapData.TTileset): TFloatRectangle;
  begin
    Result := FloatRectangle(
      Map.TileRenderPosition(TilePosition),
      Tileset.TileWidth,
      Tileset.TileHeight
    );
  end;

  function GetTileTexCoordRect(const Tile: TCastleTiledMapData.TTile; const Tileset: TCastleTiledMapData.TTileset): TFloatRectangle;
  begin
    Result := FloatRectangle(
      (Tile.Id mod Tileset.Columns) * (Tileset.TileWidth + Tileset.Spacing)
      + Tileset.Margin,
      (Tile.Id div Tileset.Columns) * (Tileset.TileHeight + Tileset.Spacing)
      + Tileset.Margin,
      Tileset.TileWidth,
      Tileset.TileHeight
    );

    if SmoothScalingSafeBorder then
      Result := Result.Grow(-0.51);

    { fix Result to be in 0..1 range }
    Result.Left := Result.Left / Tileset.Image.Width;
    Result.Width := Result.Width / Tileset.Image.Width;
    Result.Bottom := Result.Bottom / Tileset.Image.Height;
    Result.Height := Result.Height / Tileset.Image.Height;

    // account that Tiled logic for texture coords assumes Y goes down
    Result.Bottom := 1 - Result.Bottom - Result.Height;
  end;

type
  { 4 texture coordinates, in order:
    - left-bottom
    - right-bottom
    - right-top
    - left-top }
  TQuadTexCoords = array [0..3] of TVector2;

  { Flip rendering of the tile by changing texture coordinates. }
  procedure ApplyFlips(var TexCoord: TQuadTexCoords;
    const HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean);

    procedure SwapValues(var V1, V2: TVector2);
    var
      Tmp: TVector2;
    begin
      Tmp := V1;
      V1 := V2;
      V2 := Tmp;
    end;

  begin
    { Following https://doc.mapeditor.org/en/latest/reference/global-tile-ids/#gid-tile-flipping :
      When rendering an orthographic or isometric tile, the order of operations matters.
      The diagonal flip is done first, followed by the horizontal and vertical flips.
      The diagonal flip should flip the bottom left and top right corners of the tile,
      and can be thought of as an x/y axis swap. For hexagonal tiles, the order does not matter. }
    if DiagonalFlip then
      SwapValues(TexCoord[0], TexCoord[2]);
    if HorizontalFlip then
    begin
      SwapValues(TexCoord[0], TexCoord[1]);
      SwapValues(TexCoord[2], TexCoord[3]);
    end;
    if VerticalFlip then
    begin
      SwapValues(TexCoord[0], TexCoord[3]);
      SwapValues(TexCoord[1], TexCoord[2]);
    end;
  end;

var
  LastTileTileset: TCastleTiledMapData.TTileset;
  LastTileCoord: TCoordinateNode;
  LastTileTexCoord: TTextureCoordinateNode;

  procedure RenderTile(const TilePosition: TVector2Integer);
  var
    Tileset: TCastleTiledMapData.TTileset;
    Frame: Integer;
    HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
    CoordRect, TexCoordRect: TFloatRectangle;
    Geometry: TQuadSetNode;
    Shape: TShapeNode;
    Coord: TCoordinateNode;
    TexCoord: TTextureCoordinateNode;
    TexCoordArray: TQuadTexCoords;
  begin
    if Map.TileRenderData(TilePosition, ALayer,
      Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) then
    begin
      if LastTileTileset = Tileset then
      begin
        { Append tile to last geometry node }
        Coord := LastTileCoord;
        TexCoord := LastTileTexCoord;
      end else
      begin
        { Create new geometry node for this tile }
        Geometry := TQuadSetNode.CreateWithShape(Shape);
        Coord := TCoordinateNode.Create;
        Geometry.Coord := Coord;
        TexCoord := TTextureCoordinateNode.Create;
        Geometry.TexCoord := TexCoord;
        Shape.Appearance := Tileset.RendererData as TAppearanceNode;
        LayerNode.AddChildren(Shape);

        LastTileTileset := Tileset;
        LastTileCoord := Coord;
        LastTileTexCoord := TexCoord;
      end;

      CoordRect := GetTileCoordRect(TilePosition, Tileset);
      if Between(Frame, 0, Tileset.Tiles.Count - 1) then
        TexCoordRect := GetTileTexCoordRect(Tileset.Tiles[Frame], Tileset)
      else
      begin
        WritelnWarning('Tiled', 'Invalid frame id %d', [Frame]);
        // some fallback, to have something defined
        TexCoordRect := FloatRectangle(0, 0, Tileset.TileWidth, Tileset.TileHeight);
      end;

      Coord.FdPoint.Items.AddRange([
        Vector3(CoordRect.Left , CoordRect.Bottom, 0),
        Vector3(CoordRect.Right, CoordRect.Bottom, 0),
        Vector3(CoordRect.Right, CoordRect.Top   , 0),
        Vector3(CoordRect.Left , CoordRect.Top   , 0)
      ]);

      TexCoordArray[0] := Vector2(TexCoordRect.Left , TexCoordRect.Bottom);
      TexCoordArray[1] := Vector2(TexCoordRect.Right, TexCoordRect.Bottom);
      TexCoordArray[2] := Vector2(TexCoordRect.Right, TexCoordRect.Top);
      TexCoordArray[3] := Vector2(TexCoordRect.Left , TexCoordRect.Top);
      ApplyFlips(TexCoordArray, HorizontalFlip, VerticalFlip, DiagonalFlip);
      TexCoord.FdPoint.Items.AddRange(TexCoordArray);
    end;
  end;

var
  X, Y: Integer;
begin
  LastTileTileset := nil;

  for Y := Map.Height - 1 downto 0 do
    for X := 0 to Map.Width - 1 do
      RenderTile(Vector2Integer(X, Y));
end;

constructor TCastleTiledMapConverter.Create(const ATiledMap: TCastleTiledMapData);
begin
  inherited Create;

  Layers := AllLayers;

  Map := ATiledMap;

  RootNode := TX3DRootNode.Create;
  MapNode := TTransformNode.Create;
  RootNode.AddChildren(MapNode);
end;

destructor TCastleTiledMapConverter.Destroy;
begin
  inherited Destroy;
end;

function TCastleTiledMapConverter.ConvY(const TiledCoord: TVector2): TVector2;
begin
  Result := Vector2(TiledCoord.X, - TiledCoord.Y);
end;

function TCastleTiledMapConverter.ConvY(const X, Y: Single): TVector2;
begin
  Result := ConvY(Vector2(X, Y));
end;

function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  TiledMapFromStream: TCastleTiledMapData;
  TiledMapConverter: TCastleTiledMapConverter;
begin
  { The Tiled converter unit expects a TCastleTiledMapData object instance,
    hence create one. }
  TiledMapFromStream := TCastleTiledMapData.Create(Stream, BaseUrl);
  try
    TiledMapConverter := TCastleTiledMapConverter.Create(TiledMapFromStream);
    try
      TiledMapConverter.ConvertMap;
      Result := TiledMapConverter.RootNode;
    finally FreeAndNil(TiledMapConverter) end;
  finally FreeAndNil(TiledMapFromStream) end;
end;

end.
