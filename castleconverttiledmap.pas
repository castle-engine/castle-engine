{
  Copyright 2020-2021 Matthias J. Molski.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert Tiled map (see https://www.mapeditor.org/) loaded by
  CastleTiledMap unit into X3D representation.

  This unit is on purpose not fully integrated with the engine yet. This makes
  debugging easier. If unit is fully functional, it should be integrated with
  the Scene.Load mechanism. Until then it works as follows:

  1. Create Tiled map (TTiledMap).
  2. Prepare X3D scene.
  3. Convert Tiled map by this unit (CastleConvertTiledMap).
  4. Load X3D scene directly by X3D representation (use Scene.Load).

  TODO:
  1. Integrate unit with Castle Game Engine (e.g. add to pack., use castle conf.
     inc., ...)
  2. (obsolete)
  3. (obsolete)
  4. Update topPoint (see there) + handle ellipsoids (see there)
  5. Shift TShapeNodeList(+ListList) (generic) to x3dnodes_standard_texturing.inc?
  6. How to handle overlapping tiles of the same layer (Z-buffer fighting)?
  7. Refine spacing/margin calculations, still borders in desert example.

  EXPECTED X3D HIERARCHY:

  Root Node --> [0] Switch Node --> [0] Map Node --> [0] Layer Node 1 --> ...
                                                 --> [1] Layer Node 2 --> ...
                                                 ...
                                                 --> [n] Layer Node n+1 --> ...
                                --> [1] Ghost Node --> [0] Tileset Shape Node 1
                                                   --> [1] Tileset Shape Node 2
                                                   ...
                                                   --> [m] Tileset Shape Node m+1

  REMARKS:
  1. Coordinate systems: The Tiled editor uses a classical coordinate system
     with origin (0,0) at top-left position. The CGE uses the OpenGL coordinate
     system with origin (0,0) at bottom-left. The conversion of coordinates
     works as follows: The top-left position of the Tiled map is placed at the
     origin of the CGE coordinate system. In short: the origins are placed onto
     each other.
     A simple translation of the Map node by the map height allows it to follow
     CGE/OpenGL convention.
  2. Naming convention: Objects that derive from TTiledMap (TTile, TLayer, ...)
     are called accordingly. Nodes which are derived/converted from these
     objects should explicitly have the name-suffix "Node" in it. To make these
     destinctions easier, new node types (usually derived from TTransformNode)
     are introduced.

       Ex. for layers:
         var
           ALayer, Layer, TiledLayer, ... : TTiledMap.TLayer;
       but
           ALayerNode, LayerNode, TiledLayerNode, ... : TTiledLayerNode;

}
unit CastleConvertTiledMap;

{$I castleconf.inc}

interface

uses
  Classes,
  X3DNodes, CastleTiledMap;

{ Converts a Tiled map into a X3D representation for the Castle Game Engine.
  The result can be returned to Scene.Load method.

  The debug mode needs considerably more ressources.

  @param(ATiledMap must be a Tiled map as loaded by the CastleTiledMap unit.)
  @param(ADebugMode turns the debug mode on or off.) }
function ConvertTiledMap(ATiledMap: TTiledMap; ADebugMode: Boolean = False): TX3DRootNode;

implementation

uses
  SysUtils, Math, Generics.Collections,
  CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, CastleControls, CastleStringUtils,
  CastleImages;

const
  LayerZDistanceDefault: Single = 0.1;

  OrangeRedRGB   : TCastleColorRGB = (Data: ( 1.0 , 0.27 , 0.0));

type
  TTiledLayerNode = TTransformNode;
  TTiledObjectNode = TTransformNode;
  TTiledTileNode = TTransformNode;
  TShapeNodeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TShapeNode>;
  TShapeNodeListList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TShapeNodeList>;

  { Converter class to convert Tiled map into X3D representations. }

  { TTiledMapConverter }

  TTiledMapConverter = class
  strict private
    FDebugMode: Boolean;
    FDebugNode: TX3DRootNode;
    FDebugMaterialNode: TUnlitMaterialNode;
    FDebugLinePropertiesNode: TLinePropertiesNode;
    FDebugAppearanceNode: TAppearanceNode;
    FDebugFontStyleNode: TFontStyleNode;

    FMap: TTiledMap;
    FMapNode: TTransformNode;
    FLayerZDistance: Single;
    FRootNode: TX3DRootNode;
    FTilesetShapeNodeListList: TShapeNodeListList;

    FConvYMatrix: TMatrix2;

    { Converts tilesets to shape node lists.

      For every tileset in Map.Tilesets there is one TShapeNodeList created
      which holds one shape nodes for every tile of the tileset.

      IMPORTANT: This procedure must ensure that the number of shape node lists
      is always the same as the actual tilesets in Map.Tilesets.
      This crucial for retrieving the correct tileset shape node list from
      TilesetShapeNodeListList later! }
    procedure ConvertTilesets;
    { Tries to construct X3D nodes for each layer. }
    procedure ConvertLayers;
    { Builds Object Group layer node from TTiledMap data. }
    function BuildObjectGroupLayerNode(const ALayer: TTiledMap.TLayer): TTiledLayerNode;
    { Builds Tile layer node from TTiledMap data. }
    function BuildTileLayerNode(const ALayer: TTiledMap.TLayer): TTiledLayerNode;

    {   HELPER FUNCTIONS   }

    { Map width in pixels. }
    function MapWidth: Cardinal;
    { Map height in pixels. }
    function MapHeight: Cardinal;
    { Tile width of map tile (not necessarily tileset tile!) in pixels. }
    function TileWidth: Cardinal;
    { Tile height of map tile (not necessarily tileset tile!) in pixels. }
    function TileHeight: Cardinal;
    { Convert Tiled Y-values to Y-values according to definition, see remarks
      above.
      @groupBegin }
    function ConvY(const TiledY: Single): Single; overload;
    function ConvY(const TiledYVector2: TVector2): TVector2; overload;
    { @groupEnd }

    { Converts two float values into TVector2 and Y-value (CY: Convert Y)
      according to def., see remarks above. }
    function Vector2CY(const X, Y: Single): TVector2;

    {   DEBUG FUNCTIONS    }

    { Build a label which displays a lot of useful information about the map
      data for debugging. }
    procedure BuildDebugInformationLabel;
    { Build a reference 3d coordinate system with description of axis and
      origin. It is slightly moved along Z-axis to be infront of everything. }
    procedure BuildDebugCoordinateSystem;
    { Build a rectangluar debug object at pos. X,Y with dim. W,H. }
    procedure BuildDebugObject(const X, Y, W, H: Longint; const AName: String);

    {   DEBUG PROPERTIES   }

    { The DebugNode holds all debug nodes and is added to MapNode if debug mode is
      on. This is important for automatic free'ing of all debug objects.

      The other "global" nodes are shared by most debug objects.

      @groupBegin }
    property DebugNode: TX3DRootNode read FDebugNode write FDebugNode;
    property DebugMaterialNode: TUnlitMaterialNode read FDebugMaterialNode write FDebugMaterialNode;
    property DebugLinePropertiesNode: TLinePropertiesNode read FDebugLinePropertiesNode write FDebugLinePropertiesNode;
    property DebugAppearanceNode: TAppearanceNode read FDebugAppearanceNode write FDebugAppearanceNode;
    property DebugFontStyleNode: TFontStyleNode read FDebugFontStyleNode write FDebugFontStyleNode;
    { @groupEnd }

    {   MAP PROPERTIES   }

    { Mirrors 2d-vector at X-axis in XY-plane. Necessary for conversion of
      Tiled Y-values according to definition, see remarks above. }
    property ConvYMatrix: TMatrix2 read FConvYMatrix;
    { The elements of this list are themselves lists which contain
      the shape node of a tileset each. }
    property TilesetShapeNodeListList: TShapeNodeListList read FTilesetShapeNodeListList write FTilesetShapeNodeListList;
  public
    constructor Create(ATiledMap: TTiledMap; ADebugMode: Boolean = False);
    destructor Destroy; override;

    { Tries to construct X3D representation from TTiledMap data. }
    procedure ConvertMap;

    property Map: TTiledMap read FMap write FMap;
    { Holds the X3D representation of the Tiled map. Is not free'd
      automatically.

      TODO : What if MapNode is never returned and manually free'd?
      Improve by getter func.! }
    property MapNode: TTransformNode read FMapNode write FMapNode;
    property RootNode: TX3DRootNode read FRootNode write FRootNode;
    { The different layers are rendered in a certain order (last to first).
      This effect is achieved in x3d model by shifting these layers by
      this distance along the Z-axis. }
    property LayerZDistance: Single read FLayerZDistance write FLayerZDistance;

    { If true, all objects are represented in debug mode. }
    property DebugMode: Boolean read FDebugMode;
  end;

procedure TTiledMapConverter.ConvertMap;
begin
  ConvertTilesets;
  ConvertLayers;
  if DebugMode then
  begin
    BuildDebugInformationLabel;
    BuildDebugCoordinateSystem;
  end;
end;

procedure TTiledMapConverter.ConvertTilesets;
var
  Tile: TTiledMap.TTile;
  Tileset: TTiledMap.TTileset;            // A tileset
  TilesetWidth: Cardinal = 0;             // Width of tileset in pixels.
  TilesetHeight: Cardinal = 0;            // Height of tileset in pixels.
  TilesetTextureNode: TImageTextureNode;  // A tileset node
  TileGeometryNode: TRectangle2DNode;     // A tile geometry node (of tileset)
  TileShapeNode: TShapeNode;              // A shape node (of tileset)
  TilesetTextureTransformNode: TTextureTransformNode; // A transform node for
                                                      // the tileset texture
  TilesetShapeNodeList: TShapeNodeList;

  { The ghost node holds all shape nodes of all tilesets and is added
    to the map node.
    If the map node is free'd, all tiles of the tilesets are free'd via
    the ghost node, even if they were not used as actual map tiles. }
  GhostNode: TX3DRootNode;

  { Calculate the number of rows (of tiles) of a tileset. }
  function RowsInTileset: Cardinal;
  begin
    Result := Tileset.TileCount div Tileset.Columns;
  end;

begin
  GhostNode := TX3DRootNode.Create;

  for Tileset in Map.Tilesets do
  begin
    { Make sure for each tileset there is a shape node list created
      for consistency and retrieving of correct item-indices later. }
    { False arg. at Create: Very important!
      Competes otherweise with access of X3D node list. }
    TilesetShapeNodeList := TShapeNodeList.Create(False);
    TilesetTextureNode := nil;

    if Assigned(Tileset.Image) then
    begin
      { Prepare texture node of tileset. }
      TilesetTextureNode := TImageTextureNode.Create(Tileset.Name, '');
      TilesetTextureNode.SetUrl([Tileset.Image.URL]);
      TilesetTextureNode.TextureProperties := TTexturePropertiesNode.Create;
      TilesetTextureNode.TextureProperties.MagnificationFilter := magDefault;
      TilesetTextureNode.TextureProperties.MinificationFilter := minDefault;
    end;

    for Tile in Tileset.Tiles do
    begin
      { Make tiles of tileset rectangular and with correct dimensions. }
      TileGeometryNode := TRectangle2DNode.CreateWithShape(TileShapeNode);
      TileGeometryNode.Size := Vector2(Tileset.TileWidth, Tileset.TileHeight);

      { Make tiles textured and find correct texture coordinates. }
      TileShapeNode.Appearance := TAppearanceNode.Create;
      if Assigned(TilesetTextureNode) then
      begin
        TileShapeNode.Appearance.Texture := TilesetTextureNode;
        TilesetTextureTransformNode := TTextureTransformNode.Create;

        { Scale tileset texture to fit tile size:
          Scale factor is inverted: E. g. 0.5 means 2x dimension.
          Divide Tileset Tile width/height by full Tileset width/height.
          The latter is extracted from the texture node.

          TODO: Easier just to use rows/columns? Vec2(1/Rows, 1/Cols) }
        TilesetWidth :=  TilesetTextureNode.TextureImage.Width;
        TilesetHeight := TilesetTextureNode.TextureImage.Height;
        TilesetTextureTransformNode.Scale := Vector2(
          Tileset.TileWidth / TilesetWidth,
          Tileset.TileHeight / TilesetHeight
        );

        TilesetWidth :=  TilesetTextureNode.TextureImage.Width;
        TilesetHeight := TilesetTextureNode.TextureImage.Height;
        { Translate tileset texture:
        Important: Origin (0/0) of tex. coordinate is bottom-left! }
        TilesetTextureTransformNode.Translation := Vector2(
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

    { Add list of shape nodes of this tileset to
      list of tileset shape node lists. }
    TilesetShapeNodeListList.Add(TilesetShapeNodeList);

    { Add ghost nodes to switch node. }
    (RootNode.FdChildren.Items[0] as TSwitchNode).AddChildren(GhostNode);
  end;
end;

procedure TTiledMapConverter.ConvertLayers;
var
  Layer: TTiledMap.TLayer;             // A (tile, object, image) layer
  LayerNode: TTiledLayerNode;          // Node of a (tile, object, image) layer.
begin

  for Layer in Map.Layers do
  begin
    if DebugMode then
      BuildDebugObject(Round(Layer.OffsetX), Round(Layer.OffsetY), MapWidth,
        MapHeight, Layer.Name);

    if not Layer.Visible then
      Continue;

    { Every Layer has an individual layer node. }
    LayerNode := nil;

    if (Layer is TTiledMap.TObjectGroupLayer) then
    begin
      LayerNode := BuildObjectGroupLayerNode(Layer);
    end else
    if (Layer is TTiledMap.TImageLayer) then
    begin
      { TODO : Implement!
        LayerTransformNode := BuildImageLayer(Layer); }
    end else
    begin
      LayerNode := BuildTileLayerNode(Layer);
    end;

    if Assigned(LayerNode) then
    begin
      MapNode.AddChildren(LayerNode);
      LayerZDistance := LayerZDistance + LayerZDistanceDefault;
    end;
  end;
end;

function TTiledMapConverter.BuildObjectGroupLayerNode(
  const ALayer: TTiledMap.TLayer): TTiledLayerNode;
var
  TiledObjectMaterial: TMaterialNode = nil;    // Material node of a Tiled obj.
  TiledObject: TTiledMap.TTiledObject;         // A Tiled object instance (as
                                               // saved in TTiledMap).
  TiledObjectNode: TTiledObjectNode = nil;     // Node of a Tiled object.
  TiledObjectGeometry: TPolyline2DNode = nil;  // Geometry node of a TiledObject primitive.
  TiledObjectShape: TShapeNode = nil;          // Shape node of a TiledObject.
  AVector2List: TVector2List = nil;            // Helper list.
  I: Cardinal;

begin
  Result := nil;

  AVector2List := TVector2List.Create;

  for TiledObject in (ALayer as TTiledMap.TObjectGroupLayer).Objects do
  begin

    if not TiledObject.Visible then
      Continue;

    { At this point it is clear that at least one visible Tiled object is
      present on the Object group layer. Hence the layer node and the material
      node is created. }
    if not Assigned(Result) then
      Result := TTiledLayerNode.Create;   // Tiled object group layer node.

    { All Tiled objects of this layer share the same material node. The color
      depends on the layer color in accordance with handling of Tiled editor. }
    if not Assigned(TiledObjectMaterial) then
    begin
      TiledObjectMaterial := TMaterialNode.Create;
      TiledObjectMaterial.EmissiveColor := ALayer.Color;
    end;

    { Every Tiled object is based on a transform node. }
    TiledObjectNode := TTiledObjectNode.Create;
    TiledObjectNode.Translation := Vector3(ConvY(ALayer.Offset +
      TiledObject.Position), LayerZDistance);

    { Every primitive is implemented as polyline node. Hint: For better
      performance rectangle and point could be implemented as rect. node?}
    TiledObjectGeometry := TPolyline2DNode.CreateWithShape(TiledObjectShape);
    case TiledObject.Primitive of
      topPolyline, topPolygon:
        begin
          AVector2List.Clear;
          AVector2List.Assign(TiledObject.Points);
          for I := 0 to AVector2List.Count-1 do
            AVector2List.Items[I] :=  ConvY(AVector2List.Items[I]);

          { Polygon: Add point with index 0 to points list to get a closed polygon }
          if TiledObject.Primitive = topPolygon then
            AVector2List.Add(AVector2List.Items[0]);

          TiledObjectGeometry.SetLineSegments(AVector2List);
        end;
      topRectangle:
        begin
          TiledObjectGeometry.SetLineSegments([Vector2CY(0.0, 0.0), Vector2CY(
            TiledObject.Width, 0.0), Vector2CY(
            TiledObject.Width, TiledObject.Height), Vector2CY(
            0.0, TiledObject.Height), Vector2CY(0.0, 0.0)]);
        end;
      topPoint:
        begin
          { TODO : Use rectangle as representation of point. }
          AVector2List.Clear;
          { Construct a rectangle around position of point. }
          AVector2List.Add(Vector2CY(-1, -1));
          AVector2List.Add(Vector2CY(-1, 1));
          AVector2List.Add(Vector2CY(1, 1));
          AVector2List.Add(Vector2CY(1,-1));
          AVector2List.Add(Vector2CY(-1, -1));
          TiledObjectGeometry.SetLineSegments(AVector2List);
        end;
      // TODO: handle ellipse
    end;
    TiledObjectShape.Material := TiledObjectMaterial;
    TiledObjectNode.AddChildren(TiledObjectShape);
    Result.AddChildren(TiledObjectNode);
  end;
    FreeAndNil(AVector2List);
end;

function TTiledMapConverter.BuildTileLayerNode(const ALayer: TTiledMap.TLayer
  ): TTiledLayerNode;
var
  DebugTile: TTiledMap.TTile;              // A tile for debugging.
  DebugTileset: TTiledMap.TTileset;        // A tileset for debuggin.
  I: Cardinal;

  { Get the associated tileset of a specific tile by the tileset's FirstGID.

    Hint: A map tile isn't always associated with a tileset tile, e. g.
          if the tileset tile is larger than the tiles of the map an therefore
          covers several map tiles. For these tiles the is GID = 0. This
          function evaluates to nil for these tiles.

    Note: The lowest real GID starts with GID = 1.

    TODO : Handle flipped tiles. The tileset alloction may be wrong otherwise! }
  function GetTilesetOfTile(const ATileGID: Cardinal): TTiledMap.TTileset;
  var
    Tileset: TTiledMap.TTileset;
  begin
    Result := nil;
    { GID = 0 means there is no tileset associated with the tile. }
    if ATileGID = 0 then
      Exit;

    { "In order to find out from which tileset the tile is you need to find
      the tileset with the highest firstgid that is still lower or equal
      than the gid.The tilesets are always stored with increasing
      firstgids."
      (https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#tmx-data) }
    for Tileset in Map.Tilesets do
    begin
      if ATileGID >= Tileset.FirstGID then
        Result := Tileset;
    end;

    { "The highest three bits of the gid store the flipped states. Bit 32 is
      used for storing whether the tile is horizontally flipped, bit 31 is used
      for the vertically flipped tiles and bit 30 indicates whether the tile is
      flipped (anti) diagonally, enabling tile rotation. These bits have to be
      read and cleared before you can find out which tileset a tile belongs to."
      https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#data }

    { TODO : Handle flipped tiles! }

    //Writeln('  GetTilesetOfTile(GID: ' + IntToStr(ATileGID) + ') --> ', Result.Name, ' (FirstGID = ', Result.FirstGID, ')');
  end;

  { Get a specific tile obj. by its global ID from a specific tileset. }
  function GetTileFromTileset(const ATileGID: Cardinal;
    const ATileset: TTiledMap.TTileset): TTiledMap.TTile;
  var
    Tile: TTiledMap.TTile;
  begin
    Result := nil;
    if not Assigned(ATileset) then
      Exit;

    for Tile in ATileset.Tiles do
    begin
      if (ATileset.FirstGID + Tile.Id) = ATileGID then
      begin
        Result := Tile;
        Exit;
      end;
    end;
  end;

  { Returns the tile and the associated tileset of a certain tile GID.
    Returns nil respectively, if not found. }
  procedure GetTilesetAndTileByGID(const ATileGID: Cardinal;
    out ATileset: TTiledMap.TTileset; out ATile: TTiledMap.TTile);
  begin
    ATileset := GetTilesetOfTile(ATileGID);
    ATile := GetTileFromTileset(ATileGID, ATileset);
  end;

  { Zero-based. }
  function RowOfTileInMap: Cardinal;
  begin
    Result := Floor(I / Map.Width);
    //Writeln('RowOfTileInMap: ', Result);
  end;

  { Zero-based. }
  function ColumnOfTileInMap: Cardinal;
  begin
    Result := I mod Map.Width;
    //Writeln('ColumnOfTileInMap: ', Result);
  end;

  { Determines the position of a tile in the map
    by index (the index is used in Column-/Row-function). }
  function PositionOfTileByIndex(const ATileset: TTiledMap.TTileset): TVector2;
  begin
    { "The Rectangle2D node specifies a rectangle centred at (0, 0)
      in the current local 2D coordinate system and aligned with
      the local coordinate axes. By default, the box measures 2 units
      in each dimension, from -1 to +1.0"
      (https://www.web3d.org/specifications/X3Dv4Draft/
       ISO-IEC19775-1v4-CD/Part01/components/geometry2D.html#Rectangle2D) }
    Result := Vector2(0, 0);
    if not Assigned(ATileset) then
      Exit;

    Result := Vector2CY(
      ColumnOfTileInMap * TileWidth       // X
      + ATileset.TileWidth div 2,         // Compensate centring of rect. 2d node (see quote above)
      (RowOfTileInMap + 1) * TileHeight   // Y
      - ATileset.TileHeight * 0.5         // Tileset tiles are "anchored" bottom-left and compensate centring
       );
  end;

  function GetTileShapeNode(const ATileset: TTiledMap.TTileset;
    const ATile: TTiledMap.TTile): TShapeNode;
  var
    ATilesetShapeNodeList: TShapeNodeList;
  begin
    Result := nil;
    { Get tileset's shape node list. }
    ATilesetShapeNodeList := TilesetShapeNodeListList.Items[
      Map.Tilesets.IndexOf(ATileset)];
    if not Assigned(ATilesetShapeNodeList) then
      Exit;

    { Get correct shape node from tileset list. }
    Result := ATilesetShapeNodeList.Items[ATileset.Tiles.IndexOf(ATile)];
  end;

  { The actual conversion of a tile. }
  procedure ConvertTile;
  var
    Tile: TTiledMap.TTile;                  // A Tile.
    Tileset: TTiledMap.TTileset;            // A Tileset.

    { Tile nodes. }
    TileNode: TTiledTileNode;
    TileShapeNode: TShapeNode;
  begin
    { Try to get tileset. Only if it exists for this tile,
      an actual tile node is created. }
    GetTilesetAndTileByGID(ALayer.Data.Data[I], Tileset, Tile);
    if Assigned(Tileset) and Assigned(Tile) then
    begin
      TileNode := TTiledTileNode.Create;
      TileNode.Translation := Vector3(PositionOfTileByIndex(Tileset),
        LayerZDistance);
      TileShapeNode := GetTileShapeNode(Tileset, Tile);
      TileNode.AddChildren(TileShapeNode);
      Result.AddChildren(TileNode);
    end;
  end;

begin
  Result := TTiledLayerNode.Create;        // The resulting layer node.

  if DebugMode then
  begin
    for I := 0 to High(ALayer.Data.Data) do
    begin
      GetTilesetAndTileByGID(ALayer.Data.Data[I], DebugTileset, DebugTile);
      if Assigned(DebugTileset) and Assigned(DebugTile) then
      begin
        BuildDebugObject(
          ColumnOfTileInMap * TileWidth,
          (RowOfTileInMap + 1) * TileHeight - DebugTileset.TileHeight, // Y: The tiles of tilesets are "anchored" bottom-left
          DebugTileset.TileWidth,
          DebugTileset.TileHeight, 'GID: ' + IntToStr(DebugTile.Id + 1));
      end;
    end;
  end;

  { Run through tile GIDs of this tile layer. }
  for I := 0 to High(ALayer.Data.Data) do
  begin
    ConvertTile;
  end;
end;

constructor TTiledMapConverter.Create(ATiledMap: TTiledMap; ADebugMode: Boolean
  );
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

  LayerZDistance := 0.0; // The first layer is at Z = 0.0.
  TilesetShapeNodeListList := TShapeNodeListList.Create(True);

  FDebugMode := ADebugMode;
  DebugNode := nil;
  DebugMaterialNode := nil;
  DebugLinePropertiesNode := nil;
  DebugAppearanceNode := nil;
  DebugFontStyleNode := nil;
  if DebugMode then
  begin
    DebugNode := TX3DRootNode.Create;
    MapNode.AddChildren(DebugNode);

    DebugMaterialNode := TUnlitMaterialNode.Create;
    DebugMaterialNode.EmissiveColor := OrangeRedRGB;
    DebugLinePropertiesNode := TLinePropertiesNode.Create;
    DebugLinePropertiesNode.LinewidthScaleFactor := 1.0;
    DebugAppearanceNode := TAppearanceNode.Create;
    DebugAppearanceNode.Material := DebugMaterialNode;
    DebugAppearanceNode.LineProperties := DebugLinePropertiesNode;

    DebugFontStyleNode := TFontStyleNode.Create;
    DebugFontStyleNode.Size := 0.5 * (MapWidth + MapHeight) / 25;
  end;

  ConvYMatrix.Items[0,0] := 1;
  ConvYMatrix.Items[1,0] := 0;
  ConvYMatrix.Items[0,1] := 0;
  ConvYMatrix.Items[1,1] := -1;
end;

destructor TTiledMapConverter.Destroy;
begin
  FreeAndNil(FTilesetShapeNodeListList);

  inherited Destroy;
end;

function TTiledMapConverter.MapWidth: Cardinal;
begin
  Result := TileWidth * Map.Width;
end;

function TTiledMapConverter.MapHeight: Cardinal;
begin
  Result := TileHeight * Map.Height;
end;

function TTiledMapConverter.TileWidth: Cardinal;
begin
  Result := Map.TileWidth;
end;

function TTiledMapConverter.TileHeight: Cardinal;
begin
  Result := Map.TileHeight;
end;

function TTiledMapConverter.ConvY(const TiledY: Single): Single;
begin
  Result := -TiledY;
end;

function TTiledMapConverter.ConvY(const TiledYVector2: TVector2): TVector2;
begin
  Result :=  ConvYMatrix * TiledYVector2;
end;

function TTiledMapConverter.Vector2CY(const X, Y: Single): TVector2;
begin
  Result := ConvY(Vector2(X, Y));
end;

procedure TTiledMapConverter.BuildDebugInformationLabel;
var
  { Label objects. }
  DebugInfoLabel: TTransformNode;
  DebugInfoLabelGeom: TTextNode;
  DebugInfoLabelShape: TShapeNode;
  InfoLabelStringList: TCastleStringList;
  I: Cardinal;
begin
  DebugInfoLabelGeom := TTextNode.CreateWithShape(DebugInfoLabelShape);
  DebugInfoLabelGeom.FontStyle := DebugFontStyleNode;
  DebugInfoLabelShape.Appearance := DebugAppearanceNode;
  DebugInfoLabel := TTransformNode.Create;
  DebugInfoLabel.AddChildren(DebugInfoLabelShape);
  DebugInfoLabel.Translation := Vector3(MapWidth + 20.0, 0.0, 0.1);
  InfoLabelStringList := TCastleStringList.Create;
  try
    InfoLabelStringList.Add('Map width/height (in tiles | in px): ' +
      IntToStr(Map.Width) + '/' + IntToStr(Map.Height) + ' | ' +
      IntToStr(MapWidth) + '/' + IntToStr(MapHeight));
    InfoLabelStringList.Add(' ');

    InfoLabelStringList.Add('Tilesets (GIDs):');
    for I := 0 to Map.Tilesets.Count - 1 do
      InfoLabelStringList.Add('  ' + IntToStr(I) + ': ' +
        (Map.Tilesets.Items[I] as TTiledMap.TTileset).Name + ' (' +
        IntToStr((Map.Tilesets.Items[I] as TTiledMap.TTileset).FirstGID) +
        ' - ' + IntToStr((Map.Tilesets.Items[I] as TTiledMap.TTileset).FirstGID
        + (Map.Tilesets.Items[I] as TTiledMap.TTileset).TileCount - 1) +
        ')');
    InfoLabelStringList.Add(' ');

    InfoLabelStringList.Add('Layers:');
    for I := 0 to Map.Layers.Count - 1 do
    begin
      InfoLabelStringList.Add('  ' + IntToStr(I) + ': ' +
        (Map.Layers.Items[I] as TTiledMap.TLayer).Name);
    end;

    DebugInfoLabelGeom.SetString(InfoLabelStringList);
  finally
    FreeAndNil(InfoLabelStringList);
  end;
  DebugNode.AddChildren(DebugInfoLabel);

end;

procedure TTiledMapConverter.BuildDebugCoordinateSystem;
var
  { Axis objects. }
  DebugAxisGeom: array[0..2] of TLineSetNode;
  DebugAxisCoord: array[0..2] of TCoordinateNode;
  DebugAxisShape: array[0..2] of TShapeNode;

  { Naming objects. }
  DebugAxisName: array[0..3] of TTransformNode;
  DebugAxisNameGeom: array[0..3] of TTextNode;
  DebugAxisNameShape: array[0..3] of TShapeNode;

  { General objects (and vars.) }
  DebugAxisMaterial: TUnlitMaterialNode;
  DebugAxisLineProperties: TLinePropertiesNode;
  I: Byte;
  OriginVector: TVector3;
  AxisLength, AxisNameGap: Single;
begin
  OriginVector := Vector3(0.0, 0.0, 0.1); // Z = 0.1 to be visible against layer
  AxisLength := 0.5 * (MapWidth + MapHeight) / 3;
  AxisNameGap := 0.5 * (MapWidth + MapHeight) / 10;

  DebugAxisMaterial := TUnlitMaterialNode.Create;
  DebugAxisMaterial.EmissiveColor := WhiteRGB;

  DebugAxisLineProperties := TLinePropertiesNode.Create;
  DebugAxisLineProperties.LinewidthScaleFactor := 2.0;

  for I := 0 to 2 do
  begin
    { Construct three axis at origin along X, Y and Z. }
    DebugAxisGeom[I] := TLineSetNode.CreateWithShape(DebugAxisShape[I]);
    DebugAxisShape[I].Appearance := TAppearanceNode.Create;
    DebugAxisShape[I].Appearance.Material := DebugAxisMaterial;
    DebugAxisShape[I].Appearance.LineProperties := DebugAxisLineProperties;
    DebugAxisCoord[I] := TCoordinateNode.Create;
    case I of
      0: DebugAxisCoord[I].SetPoint([OriginVector, Vector3(AxisLength, 0.0, 0.1)
           ]); // X-Axis
      1: DebugAxisCoord[I].SetPoint([OriginVector, Vector3(0.0, AxisLength, 0.1)
           ]); // Y-Axis
      2: DebugAxisCoord[I].SetPoint([OriginVector, Vector3(0.0, 0.0,
           0.1 + AxisLength)]); // Z-Axis
    end;
    DebugAxisGeom[I].SetVertexCount([DebugAxisCoord[I].CoordCount]);
    DebugAxisGeom[I].Coord := DebugAxisCoord[I];
    DebugNode.AddChildren(DebugAxisShape[I]);
  end;

  for I := 0 to 3 do
  begin
    { Construct axis description for X-, Y- and Z-axis and origin. }
    DebugAxisNameGeom[I] := TTextNode.CreateWithShape(DebugAxisNameShape[I]);
    DebugAxisNameShape[I].Appearance := TAppearanceNode.Create;
    DebugAxisNameShape[I].Appearance.Material := DebugAxisMaterial;
    case I of
      0: DebugAxisNameGeom[I].SetString(['X']);
      1: DebugAxisNameGeom[I].SetString(['Y']);
      2: DebugAxisNameGeom[I].SetString(['Z']);
      3: DebugAxisNameGeom[I].SetString(['O']);
    end;
    DebugAxisNameGeom[I].FontStyle := DebugFontStyleNode;
    DebugAxisName[I] := TTransformNode.Create;
    case I of
      0: DebugAxisName[I].Translation := Vector3(AxisLength + AxisNameGap, 0.0,
           0.1);
      1: DebugAxisName[I].Translation := Vector3(0.0, AxisLength + AxisNameGap,
           0.1);
      2: DebugAxisName[I].Translation := Vector3(0.0, 0.0, AxisLength +
           AxisNameGap);
      3: DebugAxisName[I].Translation := Vector3(-AxisNameGap, -AxisNameGap,
           0.1);
    end;
    DebugAxisName[I].AddChildren(DebugAxisNameShape[I]);
    DebugNode.AddChildren(DebugAxisName[I]);
  end;
end;

procedure TTiledMapConverter.BuildDebugObject(const X, Y, W, H: Longint;
  const AName: String);
var
  { All Debug objects are based on a Transform node. }
  DebugObject: TTransformNode = nil;
  { Outline-Debug object. }
  { Hint: TRectangle2DNode is always filled, even if TFillPropertiesNode has
    property filled set to false. }
  DebugGeometryOutline: TPolyline2DNode = nil;
  DebugShapeOutline: TShapeNode = nil;
  { Name-Debug object. }
  DebugGeometryName: TTextNode = nil;
  DebugShapeName: TShapeNode = nil;
  DebugNameGap: Single;
begin
  { Build Outline-Debug object. }
  DebugGeometryOutline := TPolyline2DNode.CreateWithShape(DebugShapeOutline);
  DebugShapeOutline.Appearance := DebugAppearanceNode;
  { Create anti-clockwise rectangle. }
  DebugGeometryOutline.SetLineSegments([Vector2(0.0, ConvY(0.0)),
  Vector2(Single(W), ConvY(0.0)), Vector2(Single(W), ConvY(Single(H))),
  Vector2(0.0, ConvY(Single(H))), Vector2(0.0, ConvY(0.0))]);

  { Build Name-Debug object. }
  DebugGeometryName := TTextNode.CreateWithShape(DebugShapeName);
  DebugGeometryName.SetString(AName);
  DebugGeometryName.FontStyle := DebugFontStyleNode;
  DebugShapeName.Appearance := DebugAppearanceNode;
  DebugNameGap := 0.5 * (W + H) / 10;

  { Create Debug transform node for Outline- and NameDebug nodes. Add them to
    the Debug node. }
  DebugObject := TTransformNode.Create;
  DebugObject.Translation := Vector3(
    Single(X),
    ConvY(Single(Y)),
    LayerZDistance);
  DebugObject.AddChildren(DebugShapeOutline);
  DebugNode.AddChildren(DebugObject);

  DebugObject := TTransformNode.Create;
  DebugObject.Translation := Vector3(
    Single(X) + DebugNameGap,
    ConvY(Single(Y)) + DebugNameGap,
    LayerZDistance);
  DebugObject.AddChildren(DebugShapeName);
  DebugNode.AddChildren(DebugObject);
end;

function ConvertTiledMap(ATiledMap: TTiledMap; ADebugMode: Boolean
  ): TX3DRootNode;
var
  ATiledMapConverter: TTiledMapConverter;
begin
  Result := nil;

  if not Assigned(ATiledMap) then
    Exit;

  try
    ATiledMapConverter := TTiledMapConverter.Create(ATiledMap, ADebugMode);
    ATiledMapConverter.ConvertMap;
    Result := ATiledMapConverter.RootNode;
  finally
    FreeAndNil(ATiledMapConverter);
  end;

end;

end.

