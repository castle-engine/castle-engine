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
  2. Turn off debug mode.
  3. Check SetDebugMode: RemoveChildren free's instance of node?
  4. Update topPoint (see there) + handle ellipsoids (see there)
  5. Shift TImageTextureNodeList (generic) to x3dnodes_standard_texturing.inc?

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
  The result can be returned to Scene.Load method. }
function ConvertTiledMap(ATiledMap: TTiledMap): TX3DRootNode;

implementation

uses
  SysUtils, Math, Generics.Collections,
  CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, CastleControls, CastleStringUtils, X3DLoadInternalImage,
  CastleImages;

type
  TTiledLayerNode = TTransformNode;
  TTiledObjectNode = TTransformNode;
  TTiledTileNode = TTransformNode;
  TImageTextureNodeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TImageTextureNode>;
  TShapeNodeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TShapeNode>;
  TShapeNodeListList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TShapeNodeList>;
  { Converter class to convert Tiled map into X3D representations. }

  { TTiledMapConverter }

  TTiledMapConverter = class
  strict private
    FDebugMode: Boolean;
    FDebugNode: TX3DRootNode;

    FMap: TTiledMap;
    FMapNode: TX3DRootNode;
    FTilesetTextureNodeList: TImageTextureNodeList;
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
      above. }
    function ConvY(TiledY: Single): Single; overload;
    function ConvY(TiledYVector2: TVector2): TVector2; overload;
    { Converts two float values into TVector2 and Y-value (CY: Convert Y)
      according to def., see remarks above. }
    function Vector2CY(const X, Y: Single): TVector2;
    { Get the index of a tileset from texture node list.
      This function is important to resolve the relation between
      Tileset <--> Tileset Texture node. }
    function GetTilesetTextureNode(ATileset: TTiledMap.TTileset): TImageTextureNode;

    {   DEBUG FUNCTIONS    }

    { Build a label which displays a lot of useful information about the map
      data for debugging. }
    procedure BuildDebugInformationLabel;
    { Build a reference 3d coordinate system with description of axis and
      origin. It is slightly moved along Z-axis to be infront of everything. }
    procedure BuildDebugCoordinateSystem;
    { Build a rectangluar debug object at pos. X,Y with dim. W,H. }
    procedure BuildDebugObject(const X, Y, W, H: Longint; const AName: String);
    { Makes sure that a Debug node is added/removed from Map node list and
      is constructed/destroyed accordingly. }
    procedure SetDebugMode(AValue: Boolean);

    {   PROPERTIES   }

    { This node holds all debug nodes and is added to MapNode if debug mode is
      on. This is important for automatic free'ing of all debug objects. }
    property DebugNode: TX3DRootNode read FDebugNode write FDebugNode;
    { Mirrors 2d-vector at X-axis in XY-plane. Necessary for conversion of
      Tiled Y-values according to definition, see remarks above. }
    property ConvYMatrix: TMatrix2 read FConvYMatrix;
    property TilesetTextureNodeList: TImageTextureNodeList read FTilesetTextureNodeList write FTilesetTextureNodeList;
    property TilesetShapeNodeListList: TShapeNodeListList read FTilesetShapeNodeListList write FTilesetShapeNodeListList;
  public
    constructor Create;
    destructor Destroy; override;

    { Tries to construct X3D representation from TTiledMap data. }
    procedure ConvertMap;

    property Map: TTiledMap read FMap write FMap;
    { Holds the X3D representation of the Tiled map. Is not free'd
      automatically.

      TODO : What if MapNode is never returned and manually free'd?
      Improve by getter func.! }
    property MapNode: TX3DRootNode read FMapNode;

    { If true, all objects are represented in debug mode. }
    property DebugMode: Boolean read FDebugMode write SetDebugMode;
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

  { Calculate the number of rows (of tiles) of a tileset. }
  function RowsInTileset: Cardinal;
  begin
    Result := Tileset.TileCount div Tileset.Columns;
  end;

begin
  for Tileset in Map.Tilesets do
  begin
    //Writeln('Convert tileset: ',Tileset.Name);

    { Make sure for each tileset there is a shape node list created
      for consistency and retrieving of correct item-indices later. }
    TilesetShapeNodeList := TShapeNodeList.Create(False);
    TilesetTextureNode := nil;

    if Assigned(Tileset.Image) then
    begin
      { Prepare texture node of tileset. }
      //Writeln('  Image source: ', Tileset.Image.URL);
      TilesetTextureNode := TImageTextureNode.Create(Tileset.Name, '');
      TilesetTextureNode.SetUrl([Tileset.Image.URL]);
      TilesetTextureNode.TextureProperties := TTexturePropertiesNode.Create;
      TilesetTextureNode.TextureProperties.MagnificationFilter := magDefault;
      TilesetTextureNode.TextureProperties.MinificationFilter := minDefault;
      //Writeln('  Texture image loaded: ',TilesetTextureNode.IsTextureImage);
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

        { Translate tileset texture:
        Important: Origin of tex. coordinate is bottom-left! }
        //Tile := GetTileFromTileset(ALayer.Data.Data[I], Tileset);
        TilesetTextureTransformNode.Translation := Vector2(
          (Tile.Id mod Tileset.Columns),
          (RowsInTileset - 1) - Floor(Tile.Id / Tileset.Columns)
        );

        { Scale tileset texture:
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

        TileShapeNode.Appearance.TextureTransform := TilesetTextureTransformNode;
      end;

      TilesetShapeNodeList.Add(TileShapeNode);
    end;

    { Add list of shape nodes of this tileset to
      list of tileset shape node lists. }
    TilesetShapeNodeListList.Add(TilesetShapeNodeList);
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
      MapNode.AddChildren(LayerNode);
  end;

  //RootTransformNode.Rotation := Vector4(1, 0, 0, Pi);  // rotate scene by 180 deg around x-axis

  //Result := TX3DRootNode.Create;
  //Result.AddChildren(RootTransformNode);
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
      TiledObject.Position), 0);

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
    J: Cardinal;
  begin
    Result := nil;
      { "In order to find out from which tileset the tile is you need to find
        the tileset with the highest firstgid that is still lower or equal
        than the gid.The tilesets are always stored with increasing
        firstgids."
        (https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#tmx-data) }
      for J := 0 to Map.Tilesets.Count - 1 do
      begin
        if ATileGID >= (Map.Tilesets.Items[J] as TTiledMap.TTileset).FirstGID then
          Result := Map.Tilesets.Items[J];
      end;
      { "The highest three bits of the gid store the flipped states. Bit 32 is
        used for storing whether the tile is horizontally flipped, bit 31 is used
        for the vertically flipped tiles and bit 30 indicates whether the tile is
        flipped (anti) diagonally, enabling tile rotation. These bits have to be
        read and cleared before you can find out which tileset a tile belongs to."
        https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#data }

      { TODO : Handle flipped tiles! }

    //Writeln('GetTilesetOfTile: ', IntToStr(J));
  end;

  { Get a specific tile by its GID from a specific tileset. }
  //function GetTileFromTileset(ATileGID: Cardinal; ATileset: TTiledMap.TTileset): TTiledMap.TTile;
  //var
  //  J: Cardinal;
  //  TilesetTileID: Cardinal; // This is the (G)ID of a tile from the tileset
  //                           // Note: TTile.Id is local with resp. to tileset
  //begin
  //  Result := nil;
  //  for J := 0 to ATileset.TileCount - 1 do
  //  begin
  //    { Convert local tile id to GID. }
  //    TilesetTileID := ATileset.FirstGID + J;
  //
  //    if TilesetTileID = ATileGID then
  //    begin
  //      //Writeln('GetTileFromTileset: Result = ', IntToStr(J));
  //      Result := ATileset.Tiles.Items[J];
  //      Exit;
  //    end;
  //  end;
  //end;

  { Get a specific tile obj. by its global ID from a specific tileset. }
  function GetTileFromTileset(ATileGID: Cardinal; ATileset: TTiledMap.TTileset): TTiledMap.TTile;
  var
    Tile: TTiledMap.TTile;
    TilesetTileID: Cardinal; // This is the (G)ID of a tile from the tileset
                             // Note: TTile.Id is local with resp. to tileset
  begin
    Result := nil;
    if (not Assigned(ATileset)) then
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
  function PositionOfTileByIndex(ATileset: TTiledMap.TTileset): TVector2;
  begin
    Result := Vector2(0, 0);
    if not Assigned(ATileset) then
      Exit;

    Result := Vector2CY(
      ColumnOfTileInMap * TileWidth,                           // X
      (RowOfTileInMap + 1) * TileHeight - ATileset.TileHeight  // Y: The tiles of tilesets are "anchored" bottom-left
       );
  end;

  //function GetIndexOfTileset(ATileset: TTiledMap.TTileset): Longint;
  //var
  //  I: Cardinal;
  //begin
  //  Result := -1;
  //  for I := 0 to High(Map.Tilesets) do
  //  begin
  //    if ATileset = Map.Tilesets.I
  //  end;
  //end;

  function GetTileShapeNode(ATileset: TTiledMap.TTileset; ATile: TTiledMap.TTile): TShapeNode;
  var
    ATilesetShapeNodeList: TShapeNodeList;
  begin
    Result := nil;
    { Get tileset's shape node list. }
    ATilesetShapeNodeList := TilesetShapeNodeListList.Items[Map.Tilesets.IndexOf(ATileset)];
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
    TileGeometryNode: TRectangle2DNode;
    TileShapeNode: TShapeNode;
    TileMaterialNode: TUnlitMaterialNode;
    TilesetTextureNode: TImageTextureNode;
    TilesetTextureTransformNode: TTextureTransformNode;
    TileCoordinateNode: TCoordinateNode;
  begin
    { Try to get tileset. Only if it exists for this tile,
      an actual tile node is created. }
    Tileset := GetTilesetOfTile(ALayer.Data.Data[I]);
    Tile := GetTileFromTileset(ALayer.Data.Data[I], Tileset);
    //TilesetTextureNode := GetTilesetTextureNode(Tileset);
    if Assigned(Tileset) then
    begin
      TileNode := TTiledTileNode.Create;
      TileNode.Translation := Vector3(PositionOfTileByIndex(Tileset), 0);
      //TileGeometryNode := TRectangle2DNode.CreateWithShape(TileShapeNode);
      //TileGeometryNode.Size := Vector2(Tileset.TileWidth, Tileset.TileHeight);
      //
      //TileShapeNode.Appearance := TAppearanceNode.Create;
      //TileShapeNode.Appearance.Texture := TilesetTextureNode;
      //TilesetTextureTransformNode := TTextureTransformNode.Create;
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
      //Writeln(I, ' --> GID: ', ALayer.Data.Data[GID]);
      DebugTileset := GetTilesetOfTile(ALayer.Data.Data[I]);
      if Assigned(DebugTileset) then
      begin
        DebugTile := GetTileFromTileset(ALayer.Data.Data[I], DebugTileset);
        BuildDebugObject(
          ColumnOfTileInMap * TileWidth,
          (RowOfTileInMap + 1) * TileHeight - DebugTileset.TileHeight, // Y: The tiles of tilesets are "anchored" bottom-left
          DebugTileset.TileWidth,
          DebugTileset.TileHeight, 'GID: ' + IntToStr(DebugTile.Id));
      end;
    end;
  end;

  { Run through tile GIDs of this tile layer. }
  for I := 0 to High(ALayer.Data.Data) do
  begin
    ConvertTile;
  end;
end;

constructor TTiledMapConverter.Create;
begin
  inherited Create;

  FMapNode := TX3DRootNode.Create;
  TilesetTextureNodeList := TImageTextureNodeList.Create;
  TilesetTextureNodeList.OwnsObjects := False; // Very important!
                                               // Competes with access of X3D node list!
  TilesetShapeNodeListList := TShapeNodeListList.Create;
   TilesetShapeNodeListList.OwnsObjects := False;  // Very important!
                                                   // Competes with access of X3D node list!

  DebugMode := False; //True;

  ConvYMatrix.Items[0,0] := 1;
  ConvYMatrix.Items[1,0] := 0;
  ConvYMatrix.Items[0,1] := 0;
  ConvYMatrix.Items[1,1] := -1;
end;

destructor TTiledMapConverter.Destroy;
begin
  FreeAndNil(FTilesetTextureNodeList);
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

function TTiledMapConverter.ConvY(TiledY: Single): Single;
begin
  Result := -TiledY;
end;

function TTiledMapConverter.ConvY(TiledYVector2: TVector2): TVector2;
begin
  Result :=  ConvYMatrix * TiledYVector2;
end;

function TTiledMapConverter.Vector2CY(const X, Y: Single): TVector2;
begin
  Result := ConvY(Vector2(X, Y));
end;

function TTiledMapConverter.GetTilesetTextureNode(ATileset: TTiledMap.TTileset
  ): TImageTextureNode;
var
  TilesetTextureNode: TImageTextureNode;
begin
  Result := nil;
  for ATileset in Map.Tilesets do
  begin
    for TilesetTextureNode in TilesetTextureNodeList do
    begin
      if ATileset.Name = TilesetTextureNode.X3DName then
      begin
        Result := TilesetTextureNode;
        Exit;
      end;
    end;
  end;
end;

procedure TTiledMapConverter.BuildDebugInformationLabel;
var
  { Label objects. }
  DebugInfoLabel: TTransformNode;
  DebugInfoLabelGeom: TTextNode;
  DebugInfoLabelShape: TShapeNode;
  DebugInfoLabelShapeMaterial: TMaterialNode;
  InfoLabelStringList: TCastleStringList;
  I: Cardinal;
begin
  DebugInfoLabelGeom := TTextNode.CreateWithShape(DebugInfoLabelShape);
  DebugInfoLabelGeom.FontStyle := TFontStyleNode.Create;
  DebugInfoLabelGeom.FontStyle.Size := 10.0;
  DebugInfoLabelShapeMaterial := TMaterialNode.Create;
  DebugInfoLabelShapeMaterial.EmissiveColor := WhiteRGB;
  DebugInfoLabelShape.Appearance := TAppearanceNode.Create;
  DebugInfoLabelShape.Appearance.Material := DebugInfoLabelShapeMaterial;
  DebugInfoLabel := TTransformNode.Create;
  DebugInfoLabel.AddChildren(DebugInfoLabelShape);
  DebugInfoLabel.Translation := Vector3(MapWidth + 20.0, 0.0, 0.1);
  InfoLabelStringList := TCastleStringList.Create;
  try
    InfoLabelStringList.Add('Map width/height (in tiles | in px): ' +
      IntToStr(Map.Width) + '/' + IntToStr(Map.Height) + ' | ' +
      IntToStr(MapWidth) + '/' + IntToStr(MapHeight));
    InfoLabelStringList.Add('Tilesets (First GID):');
    for I := 0 to Map.Tilesets.Count - 1 do
      InfoLabelStringList.Add('  ' + IntToStr(I) + ': ' +
        (Map.Tilesets.Items[I] as TTiledMap.TTileset).Name + ' (' +
        IntToStr((Map.Tilesets.Items[I] as TTiledMap.TTileset).FirstGID) +
        ' - ' + IntToStr((Map.Tilesets.Items[I] as TTiledMap.TTileset).FirstGID
        + (Map.Tilesets.Items[I] as TTiledMap.TTileset).TileCount - 1) +
        ')');
    { TODO : Why doesn't the tsx file store the tile information... (see Test512.tsx) }
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
  DebugAxisMaterial: TMaterialNode;
  DebugAxisLineProperties: TLinePropertiesNode;
  I: Byte;
  OriginVector: TVector3;
const
  AxisLength = 50.0;
  AxisNameGap = 10.0; // Gap between end of axis and name
begin
  OriginVector := Vector3(0.0, 0.0, 0.1);

  DebugAxisMaterial := TMaterialNode.Create;
  DebugAxisMaterial.EmissiveColor := RedRGB;

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
    DebugAxisNameGeom[I].FontStyle := TFontStyleNode.Create;
    DebugAxisNameGeom[I].FontStyle.Size := 10.0;
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

  DebugMaterial: TMaterialNode = nil;
  DebugLineProperties: TLinePropertiesNode = nil;
const
  NameGap = 20;
begin
  { Build Outline-Debug object. }
  DebugGeometryOutline := TPolyline2DNode.CreateWithShape(DebugShapeOutline);
  { Create anti-clockwise rectangle. }
  DebugGeometryOutline.SetLineSegments([Vector2(0.0, ConvY(0.0)),
  Vector2(Single(W), ConvY(0.0)), Vector2(Single(W), ConvY(Single(H))),
  Vector2(0.0, ConvY(Single(H))), Vector2(0.0, ConvY(0.0))]);

  { Build Name-Debug object. }
  DebugGeometryName := TTextNode.CreateWithShape(DebugShapeName);
  DebugGeometryName.SetString(AName);
  DebugGeometryName.FontStyle := TFontStyleNode.Create;
  DebugGeometryName.FontStyle.Size := 20.0;

  { Use the same material and line property node for Outline- and
    Name-Debug object. }
  DebugMaterial := TMaterialNode.Create;
  DebugMaterial.EmissiveColor := YellowRGB;

  DebugLineProperties := TLinePropertiesNode.Create;
  DebugLineProperties.LinewidthScaleFactor := 1.0;

  DebugShapeOutline.Appearance := TAppearanceNode.Create;
  DebugShapeOutline.Appearance.Material := DebugMaterial;
  DebugShapeOutline.Appearance.LineProperties := DebugLineProperties;

  DebugShapeName.Appearance := TAppearanceNode.Create;
  DebugShapeName.Appearance.Material := DebugMaterial;
  DebugShapeName.Appearance.LineProperties := DebugLineProperties;

  { Create Debug transform node for Outline- and NameDebug nodes. Add them to
    the Debug node. }
  DebugObject := TTransformNode.Create;
  DebugObject.Translation := Vector3(Single(X), ConvY(Single(Y)), 0.0);
  DebugObject.AddChildren(DebugShapeOutline);
  DebugNode.AddChildren(DebugObject);

  DebugObject := TTransformNode.Create;
  DebugObject.Translation := Vector3(Single(X) + NameGap, ConvY(Single(Y)) +
    NameGap, 0.0);
  DebugObject.AddChildren(DebugShapeName);
  DebugNode.AddChildren(DebugObject);
end;

procedure TTiledMapConverter.SetDebugMode(AValue: Boolean);
begin
  if FDebugMode = AValue then
    Exit;
  FDebugMode:=AValue;
  if DebugMode = True then
  begin
    if Assigned(DebugNode) then
      FreeAndNil(FDebugNode);
    DebugNode := TX3DRootNode.Create;
    MapNode.AddChildren(DebugNode);
  end else
  begin
    MapNode.RemoveChildren(DebugNode);
    { TODO: Check if RemoveChildren also free's instance of the node.
      Would make manual free'ing here obsolete. }
    if Assigned(DebugNode) then
      FreeAndNil(FDebugNode);
  end;
end;

function ConvertTiledMap(ATiledMap: TTiledMap): TX3DRootNode;
var
  ATiledMapConverter: TTiledMapConverter;
begin
  Result := nil;

  if not Assigned(ATiledMap) then
    Exit;

  try
    ATiledMapConverter := TTiledMapConverter.Create;
    ATiledMapConverter.Map := ATiledMap;
    ATiledMapConverter.ConvertMap;
    Result := ATiledMapConverter.MapNode;
  finally
    FreeAndNil(ATiledMapConverter);
  end;

end;

end.

