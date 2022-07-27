{
  Copyright 2020-2022 Matthias J. Molski.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert Tiled map (see https://www.mapeditor.org/) loaded by
  @link(CastleTiledMap) unit into X3D representation.

  This unit allows to use the Scene.Load mechanism to load Tiled map files.
  Tiled map files usually carry the extension "tmx".

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

var
  { When @true, loading Tiled map will add extra nodes with debug information. }
  TiledDebugMode: Boolean = false;

{ This function carries out three major steps and is usually triggered by
  the Scene.Load mechanism.

  The major steps carried out by this function are as follows:

  @orderedList(
    @item(Create Tiled map instance (@link(TTiledMap)) from tmx file
      via @link(TStream).)
    @item(Create X3D scene from that instance by the
      @link(TTiledMapConverter) class.)
    @item(Return the generated @link(TX3DRootNode) of the X3D scene.)
  )

  The debug mode needs considerably more ressources. By default it is turned
  off and can only be turned on in code in this function.
}
function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

implementation

uses
  SysUtils, Math, Generics.Collections,
  CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, CastleControls, CastleStringUtils,
  CastleImages;

const
  LayerZDistanceDefault: Single = 0.1;
  OrangeRedRGB   : TCastleColorRGB = (X: 1.0; Y: 0.27; Z: 0.0);

type
  { These @link(TTransformNode) types can be useful to find the desired
    node more reliably in event routines. }
  TTiledLayerNode = TTransformNode;
  TTiledObjectNode = TTransformNode;
  TTiledTileNode = TTransformNode;

  TShapeNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TShapeNode>;
  TShapeNodeListList = {$ifdef FPC}specialize{$endif} TObjectList<TShapeNodeList>;

  { Converter class to convert Tiled map into X3D representations.

  @bold(Developer Remarks)
  @orderedList(
    @item(
       @bold(Coordinate systems:)
       The Tiled editor uses a classical coordinate system
       with origin (0,0) at top-left position. The CGE uses the OpenGL coordinate
       system with origin (0,0) at bottom-left. The conversion of coordinates
       works as follows: The top-left position of the Tiled map is placed at the
       origin of the CGE coordinate system. In short: the origins are placed onto
       each other.

       A simple translation of the Map node by the map height allows it to follow
       CGE/OpenGL convention.
    )
    @item(
      @bold(Naming convention:)
       Objects that derive from @link(TTiledMap) (@link(TTile), @link(TLayer), ...)
       are called accordingly. Nodes which are derived/converted from these
       objects should explicitly have the name-suffix "Node" in it. To make these
       destinctions easier, new node types (usually derived from @link(TTransformNode))
       are introduced.

       Ex. for layers:
       @preformatted(
           var
             ALayer, Layer, TiledLayer, ... : TTiledMap.TLayer;
       )
       but
       @preformatted(
             ALayerNode, LayerNode, TiledLayerNode, ... : TTiledLayerNode;
       )
    @item(
      @orderedList(
        @item Update topPoint (see there)
        @item Shift TShapeNodeList(+ListList) (generic) to x3dnodes_standard_texturing.inc?
        @item How to handle overlapping tiles of the same layer (Z-buffer fighting)?
        @item Refine spacing/margin calculations, still borders in desert example.
      )
    )
  }
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
    { Builds Object Group layer node from @link(TTiledMap) data. }
    function BuildObjectGroupLayerNode(const ALayer: TTiledMap.TLayer): TTiledLayerNode;
    { Builds Tile layer node from @link(TTiledMap) data. }
    function BuildTileLayerNode(const ALayer: TTiledMap.TLayer): TTiledLayerNode;

    {   HELPER FUNCTIONS   }

    { Map width in pixels. }
    function MapWidthPx: Cardinal;
    { Map height in pixels. }
    function MapHeightPx: Cardinal;
    { Get the dimensions of the tileset image in pixels.
      TODO: Easier just to use rows/columns? Vec2(1/Rows, 1/Cols)
      @groupBegin }
    function TilesetWidthPx(const AImageTextureNode: TImageTextureNode): Cardinal;
    function TilesetHeightPx(const AImageTextureNode: TImageTextureNode): Cardinal;
    { @groupEnd }
    { Tile width of map tile (not necessarily tileset tile!) in pixels. }
    function TileWidthPx: Cardinal;
    { Tile height of map tile (not necessarily tileset tile!) in pixels. }
    function TileHeightPx: Cardinal;
    { Convert Tiled Y-values (CY: Convert Y) to Y-values according to
      definition. Mirrors 2d-vector at X-axis in XY-plane.
      See remarks at head of unit file.
      @groupBegin }
    function ConvY(const TiledYVector2: TVector2): TVector2; overload;
    function ConvY(const X, Y: Single): TVector2; overload;
    { @groupEnd }

    {   DEBUG FUNCTIONS    }

    { Build a label which displays a lot of useful information about the map
      data for debugging. }
    procedure BuildDebugInformationLabel;
    { Build a reference 3d coordinate system with description of axis and
      origin. It is slightly moved along Z-axis to be infront of everything. }
    procedure BuildDebugCoordinateSystem;
    { Build a rectangluar debug object at pos. X,Y with dim. W,H. }
    procedure BuildDebugObject(const X, Y, W, H: Integer; const AName: String);

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

    { The elements of this list are themselves lists which contain
      the shape node of a tileset each. }
    property TilesetShapeNodeListList: TShapeNodeListList read FTilesetShapeNodeListList write FTilesetShapeNodeListList;
  public
    constructor Create(ATiledMap: TTiledMap; ADebugMode: Boolean = False);
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
type
  TTileFlip = (tfNoFlip, tfHFlip, tfVFlip, tfDFlip);
var
  Tile: TTiledMap.TTile;
  Tileset: TTiledMap.TTileset;            // A tileset
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
  TilesetTexCoordOrigin: TVector2;
  TileFlip: TTileFlip;

  { Calculate the number of rows (of tiles) of a tileset. }
  function RowsInTileset: Cardinal;
  begin
    Result := Tileset.TileCount div Tileset.Columns;
  end;

begin
  TilesetTexCoordOrigin := TVector2.Zero;
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
      { Fix Tileset.Columns if necessary, testcase: examples/tiled/map_viewer/data/maps/desert.tmx }
      { TODO: This code is also done by TCastleTiledMapControl.LoadTilesetsImages.
        Move the logic to TTiledMap? }
      if Tileset.Columns = 0 then
        Tileset.Columns := Tileset.Image.Width div Tileset.TileWidth;
      if Tileset.TileCount = 0 then
        Tileset.TileCount := (Tileset.Image.Height div Tileset.TileHeight) * Tileset.Columns;

      { Prepare texture node of tileset. }
      TilesetTextureNode := TImageTextureNode.Create(Tileset.Name, '');
      TilesetTextureNode.SetUrl([Tileset.Image.URL]);
      TilesetTextureNode.TextureProperties := TTexturePropertiesNode.Create;
      TilesetTextureNode.TextureProperties.MagnificationFilter := magDefault;
      TilesetTextureNode.TextureProperties.MinificationFilter := minDefault;

      { Set S,T boundary modes. }
      TilesetTextureNode.TextureProperties.BoundaryModeS := bmMirroredRepeat;
      TilesetTextureNode.TextureProperties.BoundaryModeT := bmMirroredRepeat;
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
        TilesetTexCoordOrigin := Vector2(TilesetWidthPx(TilesetTextureNode) / Tileset.TileWidth, 0);
      { Vertical flip }
      if TileFlip = tfVFlip then
        TilesetTexCoordOrigin := Vector2(0, TilesetHeightPx(TilesetTextureNode) / Tileset.Tileheight);
      { Diagonal flip }
      if TileFlip = tfDFlip then
        TilesetTexCoordOrigin := Vector2(TilesetWidthPx(TilesetTextureNode) / Tileset.TileWidth,
          TilesetHeightPx(TilesetTextureNode) / Tileset.Tileheight);

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
            The latter is extracted from the texture node. }
          TilesetTextureTransformNode.Scale := Vector2(
            Tileset.TileWidth / TilesetWidthPx(TilesetTextureNode),
            Tileset.TileHeight / TilesetHeightPx(TilesetTextureNode)
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
  Layer: TTiledMap.TLayer;             // A (tile, object, image) layer
  LayerNode: TTiledLayerNode;          // Node of a (tile, object, image) layer.
begin

  for Layer in Map.Layers do
  begin
    if DebugMode then
      BuildDebugObject(Round(Layer.OffsetX), Round(Layer.OffsetY), MapWidthPx,
        MapHeightPx, Layer.Name);

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
  // Material node of a Tiled obj.
  TiledObjectMaterial: TMaterialNode;
  // A Tiled object instance (as saved in TTiledMap).
  TiledObject: TTiledMap.TTiledObject;
  // Node of a Tiled object.
  TiledObjectNode: TTiledObjectNode;
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

  Result := TTiledLayerNode.Create;   // Tiled object group layer node.

  { Move layer node according to layer offset }
  Result.Translation := Vector3(ConvY(ALayer.OffsetX, ALayer.OffsetY), 0);

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
    TiledObjectNode := TTiledObjectNode.Create;
    TiledObjectNode.Translation := Vector3(ConvY(TiledObject.Position), 0);

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
          TiledObjectGeometry.SetLineSegments([ConvY(0.0, 0.0), ConvY(
            TiledObject.Width, 0.0), ConvY(
            TiledObject.Width, TiledObject.Height), ConvY(
            0.0, TiledObject.Height), ConvY(0.0, 0.0)]);
        end;
      topPoint:
        begin
          { TODO : Use rectangle as representation of point. }
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

  { Resolves the GID consdering potential horizontal, vertical and/or
    diagonal bits. Returns the resolved GID.

    Only the resolved GID can be used to detect the correct tileset/tiles
    if these bits are set.

    See also:
    "The highest three bits of the gid store the flipped states. Bit 32 is
    used for storing whether the tile is horizontally flipped, bit 31 is used
    for the vertically flipped tiles and bit 30 indicates whether the tile is
    flipped (anti) diagonally, enabling tile rotation. These bits have to be
    read and cleared before you can find out which tileset a tile belongs to."
    https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#data }
  function GetResolvedGID(const ATileGID: Cardinal;
    out HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean): Cardinal; overload;
  const
    HorizontalFlag = $80000000;
    VerticalFlag   = $40000000;
    DiagonalFlag   = $20000000;
    ClearFlag      = $1FFFFFFF;
  begin
    { Check which flags are set. }
    HorizontalFlip := ATileGID and HorizontalFlag > 0;
    VerticalFlip := ATileGID and VerticalFlag > 0;
    DiagonalFlip := ATileGID and DiagonalFlag > 0;

    { Clear GID }
    Result := ATileGID and ClearFlag;
  end;

  function GetResolvedGID(const ATileGID: Cardinal): Cardinal; overload;
  var
    Trash: Boolean;  // Just there to call overloaded function.
  begin
    Result := GetResolvedGID(ATileGID, Trash, Trash, Trash);
  end;

  { The resolved tile shape node is textured correctly considering
    all the flipping bits (horizontal, vertical, diagonal bit). }
  function GetResolvedTileShapeNode(const ATileset: TTiledMap.TTileset;
    const ATile: TTiledMap.TTile; const ATileGID: Cardinal): TShapeNode;
  var
    HFlip: Boolean;
    VFlip: Boolean;
    DFlip: Boolean;
    ATilesetShapeNodeList: TShapeNodeList;
  begin
    HFlip := false;
    VFlip := false;
    DFlip := false;
    Result := nil;

    { Get tileset's shape node list. }
    ATilesetShapeNodeList := TilesetShapeNodeListList.Items[
      Map.Tilesets.IndexOf(ATileset)];
    if not Assigned(ATilesetShapeNodeList) then
      Exit;

    { Get flip mode of tile }
    { Important:
      "When rendering an orthographic or isometric tile, the order of
       operations matters. The diagonal flip is done first, followed
       by the horizontal and vertical flips. The diagonal flip should
       flip the bottom left and top right corners of the tile, and can
       be thought of as an x/y axis swap. For hexagonal tiles,
       the order does not matter."
       https://doc.mapeditor.org/en/stable/reference/global-tile-ids/#tile-flipping }
    GetResolvedGID(ATileGID, HFlip, VFlip, DFlip);

    { Get shape node from tileset list if no flip flags are set. }
    if not (HFlip or VFlip or DFlip) then
    begin
      Result := ATilesetShapeNodeList.Items[ATileset.Tiles.IndexOf(ATile)];
      Exit;
    end;

    { Get shape node from tileset list if exclusivly the horizontal flip flag is set. }
    if HFlip and not (VFlip or DFlip) then
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
    if VFlip and not (HFlip or DFlip) then
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

    { Ignore all flip flags if DFlip and additionally
      HFlip/VFlip is set (results in rotation).
      TODO: Implement rotation. }
    if DFlip and (HFlip or VFlip) then
    begin
      WritelnWarning('Not supported yet: Combination of diagonal- with other flips and rotations. Flags are ignored.');
      Result := ATilesetShapeNodeList.Items[ATileset.Tiles.IndexOf(ATile)];
      Exit;
    end;

    { Get shape node from tileset list if exclusivly the diagonal flip flag is set
      or if horizontal and vertical flip flags (= diagonal flip) are set. }
    if (DFlip and not (HFlip or VFlip)) xor ((HFlip and VFlip) and not DFlip) then
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

  { Get the associated tileset of a specific tile by the tileset's FirstGID.

    Hint: A map tile isn't always associated with a tileset tile, e. g.
          if the tileset tile is larger than the tiles of the map an therefore
          covers several map tiles. For these tiles the is GID = 0. This
          function evaluates to nil for these tiles.

    Note: The lowest real GID starts with GID = 1. }
  function GetTilesetOfTile(ATileGID: Cardinal): TTiledMap.TTileset;
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
      if GetResolvedGID(ATileGID) >= Tileset.FirstGID then
        Result := Tileset;
    end;

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
      if (ATileset.FirstGID + Tile.Id) = GetResolvedGID(ATileGID) then
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

    Result := ConvY(
      ColumnOfTileInMap * TileWidthPx       // X
      + ATileset.TileWidth div 2,         // Compensate centring of rect. 2d node (see quote above)
      (RowOfTileInMap + 1) * TileHeightPx   // Y
      - ATileset.TileHeight * 0.5         // Tileset tiles are "anchored" bottom-left and compensate centring
       );
  end;

  { The actual conversion of a tile. }
  procedure ConvertTile;
  var
    Tile: TTiledMap.TTile;                  // A Tile.
    Tileset: TTiledMap.TTileset;            // A Tileset.
    GID: Cardinal;

    { Tile nodes. }
    TileNode: TTiledTileNode;
    TileShapeNode: TShapeNode;
  begin
    { Try to get tileset. Only if it exists for this tile,
      an actual tile node is created. }
    GID := ALayer.Data.Data[I];
    GetTilesetAndTileByGID(GID, Tileset, Tile);
    if Assigned(Tileset) and Assigned(Tile) then
    begin
      TileNode := TTiledTileNode.Create('Tile node');
      TileNode.Translation := Vector3(PositionOfTileByIndex(Tileset),
        LayerZDistance);

      { Consider horizontal-, vertical-, diagonal flipping.

      "When rendering a tile, the order of operation matters.
       The diagonal flip (x/y axis swap) is done first, followed
       by the horizontal and vertical flips."
      (https://doc.mapeditor.org/en/stable/reference/tmx-map-format/#layer)
      }
      TileShapeNode := GetResolvedTileShapeNode(Tileset, Tile, GID);

      TileNode.AddChildren(TileShapeNode);
      Result.AddChildren(TileNode);
    end;
  end;

begin
  Result := TTiledLayerNode.Create;        // The resulting layer node.

  { Move layer node according to layer offset }
  Result.Translation := Vector3(ConvY(ALayer.OffsetX, ALayer.OffsetY), 0);

  if DebugMode then
  begin
    for I := 0 to High(ALayer.Data.Data) do
    begin
      GetTilesetAndTileByGID(ALayer.Data.Data[I], DebugTileset, DebugTile);
      if Assigned(DebugTileset) and Assigned(DebugTile) then
      begin
        BuildDebugObject(
          ColumnOfTileInMap * TileWidthPx,
          (RowOfTileInMap + 1) * TileHeightPx - DebugTileset.TileHeight, // Y: The tiles of tilesets are "anchored" bottom-left
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
    DebugFontStyleNode.Size := 0.5 * (MapWidthPx + MapHeightPx) / 25;
  end;
end;

destructor TTiledMapConverter.Destroy;
begin
  FreeAndNil(FTilesetShapeNodeListList);

  inherited Destroy;
end;

function TTiledMapConverter.MapWidthPx: Cardinal;
begin
  Result := TileWidthPx * Map.Width;
end;

function TTiledMapConverter.MapHeightPx: Cardinal;
begin
  Result := TileHeightPx * Map.Height;
end;

function TTiledMapConverter.TilesetWidthPx(
  const AImageTextureNode: TImageTextureNode): Cardinal;
begin
  Result := 0;
  if (AImageTextureNode <> nil) and
     // image is nil when it could not be loaded
     (AImageTextureNode.TextureImage <> nil) then
    Result :=  AImageTextureNode.TextureImage.Width;
end;

function TTiledMapConverter.TilesetHeightPx(
  const AImageTextureNode: TImageTextureNode): Cardinal;
begin
  Result := 0;
  if (AImageTextureNode <> nil) and
     // image is nil when it could not be loaded
     (AImageTextureNode.TextureImage <> nil) then
    Result :=  AImageTextureNode.TextureImage.Height;
end;

function TTiledMapConverter.TileWidthPx: Cardinal;
begin
  Result := Map.TileWidth;
end;

function TTiledMapConverter.TileHeightPx: Cardinal;
begin
  Result := Map.TileHeight;
end;

function TTiledMapConverter.ConvY(const TiledYVector2: TVector2): TVector2;
const
  ConvYMatrix: TMatrix2 = (Data: ((1, 0), (0, -1)));
begin
  Result :=  ConvYMatrix * TiledYVector2;
end;

function TTiledMapConverter.ConvY(const X, Y: Single): TVector2;
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
  DebugInfoLabel.Translation := Vector3(MapWidthPx + 20.0, 0.0, 0.1);
  InfoLabelStringList := TCastleStringList.Create;
  try
    InfoLabelStringList.Add('Map width/height (in tiles | in px): ' +
      IntToStr(Map.Width) + '/' + IntToStr(Map.Height) + ' | ' +
      IntToStr(MapWidthPx) + '/' + IntToStr(MapHeightPx));
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
  AxisLength := 0.5 * (MapWidthPx + MapHeightPx) / 3;
  AxisNameGap := 0.5 * (MapWidthPx + MapHeightPx) / 10;

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

procedure TTiledMapConverter.BuildDebugObject(const X, Y, W, H: Integer;
  const AName: String);
var
  { All Debug objects are based on a Transform node. }
  DebugObject: TTransformNode;
  { Outline-Debug object. }
  { Hint: TRectangle2DNode is always filled, even if TFillPropertiesNode has
    property filled set to false. }
  DebugGeometryOutline: TPolyline2DNode;
  DebugShapeOutline: TShapeNode;
  { Name-Debug object. }
  DebugGeometryName: TTextNode;
  DebugShapeName: TShapeNode;
  DebugNameGap: Single;
begin
  DebugObject := nil;
  DebugGeometryOutline := nil;
  DebugShapeOutline := nil;
  DebugGeometryName := nil;
  DebugShapeName := nil;

  { Build Outline-Debug object. }
  DebugGeometryOutline := TPolyline2DNode.CreateWithShape(DebugShapeOutline);
  DebugShapeOutline.Appearance := DebugAppearanceNode;
  { Create anti-clockwise rectangle. }
  DebugGeometryOutline.SetLineSegments([ConvY(0.0, 0.0),
  ConvY(Single(W), 0.0), ConvY(Single(W), Single(H)),
  ConvY(0.0, Single(H)), ConvY(0.0, 0.0)]);

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
    ConvY(Single(X), Single(Y)),
    LayerZDistance + LayerZDistanceDefault / 2);   // Z: Shift debug object slightly infront of layer
                                                   //    (esp. important for tile layers).
  DebugObject.AddChildren(DebugShapeOutline);
  DebugNode.AddChildren(DebugObject);

  DebugObject := TTransformNode.Create;
  DebugObject.Translation := Vector3(
    ConvY(Single(X), Single(Y)) + Vector2(DebugNameGap, DebugNameGap),
    LayerZDistance + LayerZDistanceDefault / 2);   // Z: Shift debug object slightly infront of layer
                                                   //    (esp. important for tile layers).
  DebugObject.AddChildren(DebugShapeName);
  DebugNode.AddChildren(DebugObject);
end;

function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String
  ): TX3DRootNode;
var
  TiledMapFromStream: TTiledMap;
  TiledMapConverter: TTiledMapConverter;
begin
  { The Tiled converter unit expects a TTiledMap object instance,
    hence create one. }
  TiledMapFromStream := TTiledMap.Create(Stream, BaseUrl);
  try
    TiledMapConverter := TTiledMapConverter.Create(TiledMapFromStream, TiledDebugMode);
    try
      TiledMapConverter.ConvertMap;
      Result := TiledMapConverter.RootNode;
    finally FreeAndNil(TiledMapConverter) end;
  finally FreeAndNil(TiledMapFromStream) end;
end;

end.
