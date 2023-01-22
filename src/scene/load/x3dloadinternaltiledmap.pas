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
  CastleVectors, CastleTransform, CastleColors, CastleRectangles,
  CastleRenderOptions, CastleControls, CastleStringUtils,
  CastleImages;

type
  { Converter class to convert Tiled map into X3D representations.

    TODO:
    - SmoothScalingSafeBorder, similar to TCastleTiledMapControl, to fix borders on desert example
  }
  TTiledMapConverter = class
  strict private
    FMap: TTiledMap;
    FMapNode: TTransformNode;
    FRootNode: TX3DRootNode;

    { Load tiles to nodes.
      Fills TTile.RendererData with TShapeNode to render this. }
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
var
  Tile: TTiledMap.TTile;
  Tileset: TTiledMap.TTileset;
  TilesetTextureNode: TImageTextureNode;
  TileGeometryNode: TQuadSetNode;
  TileShapeNode: TShapeNode;

  { The ghost node holds all shape nodes of all tilesets and is added
    to the map node.
    If the map node is free'd, all tiles of the tilesets are free'd via
    the ghost node, even if they were not used as actual map tiles. }
  GhostNode: TGroupNode;

  TexProperties: TTexturePropertiesNode;
  Coord: TCoordinateNode;
  TexCoord: TTextureCoordinateNode;
  TexRect: TFloatRectangle;
begin
  GhostNode := TGroupNode.Create;

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

    { Prepare texture node of tileset. }
    TilesetTextureNode := TImageTextureNode.Create(Tileset.Name, '');
    TilesetTextureNode.SetUrl([Tileset.Image.URL]);

    TexProperties := TTexturePropertiesNode.Create;
    TexProperties.MagnificationFilter := magDefault;
    TexProperties.MinificationFilter := minDefault;
    { Do not force "power of 2" size, which may prevent mipmaps.
      Not resizing is consistent with X3DLoadInternalImage, LoadCastleSpriteSheet,
      users may use it with pixel-art and don't expect images to be resized. }
    TexProperties.GuiTexture := true;
    TilesetTextureNode.TextureProperties := TexProperties;

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

      TexRect := FloatRectangle(
        (Tile.Id mod Tileset.Columns) * (Tileset.TileWidth + Tileset.Spacing)
        + Tileset.Margin,
        (Tile.Id div Tileset.Columns) * (Tileset.TileHeight + Tileset.Spacing)
        + Tileset.Margin,
        Tileset.TileWidth,
        Tileset.TileHeight
      );

      { fix TexRect to be in 0..1 range }
      TexRect.Left := TexRect.Left / Tileset.Image.Width;
      TexRect.Width := TexRect.Width / Tileset.Image.Width;
      TexRect.Bottom := TexRect.Bottom / Tileset.Image.Height;
      TexRect.Height := TexRect.Height / Tileset.Image.Height;

      // account that Tiled logic for texture coords assumes Y goes down
      TexRect.Bottom := 1 - TexRect.Bottom - TexRect.Height;

      TexCoord := TTextureCoordinateNode.Create;
      TexCoord.SetPoint([
        Vector2(TexRect.Left , TexRect.Bottom),
        Vector2(TexRect.Right, TexRect.Bottom),
        Vector2(TexRect.Right, TexRect.Top),
        Vector2(TexRect.Left , TexRect.Top)
      ]);
      TileGeometryNode.TexCoord := TexCoord;

      TileShapeNode.Appearance := TAppearanceNode.Create;
      TileShapeNode.Appearance.Texture := TilesetTextureNode;

      Tile.RendererData := TileShapeNode;
      GhostNode.AddChildren(TileShapeNode);
    end;

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

      TileShapeNode := Tileset.Tiles[Frame].RendererData as TShapeNode;

      // TODO: apply HorizontalFlip, VerticalFlip, DiagonalFlip for this instance

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
end;

destructor TTiledMapConverter.Destroy;
begin
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
