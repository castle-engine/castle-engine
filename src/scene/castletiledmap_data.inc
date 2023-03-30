{%MainUnit castletiledmap.pas}
{
  Copyright 2015-2021 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading and manipulating "Tiled" map files (TCastleTiledMapData class). }

{$ifdef read_interface}

type
  TAssignableObject = class(TPersistent)
  end;
  TAssignableObjectClass = class of TAssignableObject;

  TTilesData = array of Cardinal;

  TPropertyType = (ptString ,ptInteger, ptFloat, ptBoolean, ptColor, ptFile, ptObject);

  { Loading and manipulating "Tiled" map files (http://mapeditor.org). }
  TCastleTiledMapData = class(TAssignableObject)
  public
    const
      PropertyTypeNames: array[TPropertyType] of string =('', 'int', 'float', 'bool',
        'color', 'file', 'object');
    type
      TProperty = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
        function PropertyTypeFromName(const AName: string): TPropertyType;
      public
        { The name of the property. }
        Name: string;
        { The value of the property. }
        Value: string;
        { The type of the property. Can be string (default), int, float, bool, color
          or file (since 0.16, with color and file added in 0.17). }
        AType: TPropertyType;
        procedure Assign(Source: TPersistent);override;

        function AsFloat(const ADefaultValue: Single): Single;
        function AsInteger(const ADefaultValue: Integer): Integer;
        function AsString(): String;
        function AsBoolean(const ADefaultValue: Boolean): Boolean;
        function AsColor(const ADefaultValue: TCastleColor): TCastleColor;

        procedure SetFloat(const AValue: Single);
        procedure SetInteger(const AValue: Integer);
        procedure SetString(const AValue: String);
        procedure SetBoolean(const AValue: Boolean);
        procedure SetColor(const AValue: TCastleColor);
      end;

      { List of properties. }
      TPropertyList = class({$ifdef FPC}specialize{$endif} TObjectList<TProperty>)
      private
        function GetItems(const AName: String): TProperty;
        function GetOrCreateItems(const AName: String): TProperty;
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        procedure Assign(const APropertyList: TPropertyList);

        function FloatValue(const AName: string; var AValue: Single): Boolean;
        function IntegerValue(const AName: string; var AValue: Integer): Boolean;
        function StringValue(const AName: string; var AValue: String): Boolean;
        function BooleanValue(const AName: string; var AValue: Boolean): Boolean;
        function ColorValue(const AName: string; var AValue: TCastleColor): Boolean;

        procedure SetFloatValue(const AName: string; const AValue: Single);
        procedure SetIntegerValue(const AName: string; const AValue: Integer);
        procedure SetStringValue(const AName: string; const AValue: String);
        procedure SetBooleanValue(const AName: string; const AValue: Boolean);
        procedure SetColorValue(const AName: string; const AValue: TCastleColor);

        procedure AddOrSetProperty(const AName: String; const AValue: String;
          const APropType: TPropertyType = ptString);

        property Items[const AName: String]: TProperty read GetItems;
      end;

      TEncodingType = (etNone, etBase64, etCSV);
      TCompressionType = (ctNone, ctGZip, ctZLib);

      { Binary data definition. }
      TData = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { The encoding used to encode the tile layer data. When used, it can be
          "base64" and "csv" at the moment. }
        Encoding: TEncodingType;
        { The compression used to compress the tile layer data. Tiled Qt supports
          "gzip" and "zlib". }
        Compression: TCompressionType;
        { Binary data. Uncompressed and decoded. }
        Data: TTilesData;

        IgnoreDataAssign: Boolean;
        procedure Assign(Source: TPersistent);override;
      end;

      { Image definition. }
      TImage = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { Used for embedded images, in combination with a data child element.
          Valid values are file extensions like png, gif, jpg, bmp, etc. (since 0.9) }
        Format: string;
        { The reference to the tileset image file (Tiled supports most common
          image formats). }
        URL: string;
        { Defines a specific color that is treated as transparent. }
        Trans: TCastleColorRGB;
        { The image width in pixels (optional, used for tile index correction when
          the image changes). }
        Width: Cardinal;
        { The image height in pixels (optional). }
        Height: Cardinal;
        { Embedded image data (since 0.9). }
        Data: TData;
        destructor Destroy; override;
        procedure DetermineSize;
        procedure Assign(Source: TPersistent);override;
      end;

      TObjectsDrawOrder = (odoIndex, odoTopDown);

      TTiledObjectPrimitive = (topRectangle, topPoint, topEllipse, topPolygon,
        topPolyLine);

      { Object definition. }
      TTiledObject = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { Unique ID of the object. Each object that is placed on a map gets
          a unique id. Even if an object was deleted, no object gets the same ID.
          Can not be changed in Tiled Qt. (since Tiled 0.11) }
        Id: Integer;
        { The name of the object. An arbitrary string. }
        Name: string;
        { The type of the object. An arbitrary string. }
        Type_: string;
        { The x coordinate of the object in pixels. }
        X: Single;
        { The y coordinate of the object in pixels. }
        Y: Single;
        { The width of the object in pixels (defaults to 0). }
        Width: Single;
        { The height of the object in pixels (defaults to 0). }
        Height: Single;
        { The rotation of the object in degrees clockwise (defaults to 0). (since 0.10) }
        Rotation: Single;
        { An reference to a tile (optional). }
        GId: Cardinal;
        { Whether the object is shown (1) or hidden (0). Defaults to 1. (since 0.9) }
        Visible: Boolean;
        Properties: TPropertyList;
        { List of points for poligon and poliline. }
        Points: TVector2List;
        Primitive: TTiledObjectPrimitive;
        Image: TImage;
        constructor Create;
        destructor Destroy; override;
        { X and Y packed in a vector. }
        function Position: TVector2;
        procedure Assign(Source: TPersistent);override;
      end;

      TTiledObjectList = class({$ifdef FPC}specialize{$endif} TObjectList<TTiledObject>)
      public
        procedure Assign(const ATiledObjectList: TTiledObjectList);
      end;

      TLayer = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String); virtual;
        procedure SetYSortEnabled(const AValue: Boolean);
      public
        { The name of the layer. }
        Name: string;
        { The opacity of the layer as a value from 0 to 1. Defaults to 1. }
        Opacity: Single;
        { Whether the layer is shown or hidden. Defaults to true. }
        Visible: Boolean;
        { Rendering offset for this layer in pixels. Defaults to 0. (since 0.14). }
        OffsetX: Single;
        { Rendering offset for this layer in pixels. Defaults to 0. (since 0.14). }
        OffsetY: Single;
        Properties: TPropertyList;
        Data: TData;
        { The color used to display the objects in this group. }
        Color: TCastleColorRGB;
        { The width of the object group in tiles. Meaningless. }
        Width: Integer;
        { The height of the object group in tiles. Meaningless. }
        Height: Integer;
        constructor Create;
        destructor Destroy; override;
        { OffsetX and OffsetY packed in a vector. }
        function Offset: TVector2;
        procedure Assign(Source: TPersistent);override;
        function GetYSortEnabled: Boolean;

        property YSortEnabled: Boolean read GetYSortEnabled write SetYSortEnabled ;
      end;

      TObjectGroupLayer = class(TLayer)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String); override;
      public
        { Whether the objects are drawn according to the order of appearance
          ("index") or sorted by their y-coordinate ("topdown"). Defaults to "topdown". }
        DrawOrder: TObjectsDrawOrder;
        Objects: TTiledObjectList;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent);override;
      end;

      TImageLayer = class(TLayer)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String); override;
      public
        { Used by ImageLayer. }
        Image: TImage;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent);override;
      end;

      TLayerClass = class of TLayer;

      { List of layers. }
      TLayerList = class({$ifdef FPC}specialize{$endif} TObjectList<TLayer>)
      public
      procedure Assign(const ALayerList: TLayerList);
      end;

      { Single frame of animation. }
      TFrame = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { The local ID of a tile within the parent tileset. }
        TileId: Cardinal;
        { How long (in milliseconds) this frame should be displayed before advancing
          to the next frame. }
        Duration: Cardinal;
        procedure Assign(Source: TPersistent);override;
      end;

      { Contains a list of animation frames.
        As of Tiled 0.10, each tile can have exactly one animation associated with it.
        In the future, there could be support for multiple named animations on a tile. }
      TAnimation = class({$ifdef FPC}specialize{$endif} TObjectList<TFrame>)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        procedure Assign(const AAnimation: TAnimation);
      end;

      TTile = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { The local tile ID within its tileset. }
        Id: Cardinal;
        { Defines the terrain type of each corner of the tile, given as
          comma-separated indexes in the terrain types array in the order top-left,
          top-right, bottom-left, bottom-right. Leaving out a value means that corner
          has no terrain. (optional) (since 0.9) }
        Terrain: TVector4Integer;
        { A percentage indicating the probability that this tile is chosen when it
          competes with others while editing with the terrain tool. (optional) (since 0.9) }
        Probability: Single;
        Properties: TPropertyList;
        Image: TImage;
        { ObjectGroup since 0.10. }
        ObjectGroup: TObjectGroupLayer;
        Animation: TAnimation;
        { Use to render the tile. Not a part of the file format.
          In case of X3DLoadInternalTiledMap, this is always TShapeNode. }
        RendererData: TObject;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent);override;
      end;

      { Tiles list. }
      TTileList = class({$ifdef FPC}specialize{$endif} TObjectList<TTile>)
      public
        procedure Assign(const ATileList: TTileList);
      end;

      TTerrain = class(TAssignableObject)
      public
        { The name of the terrain type. }
        Name: string;
        { The local tile-id of the tile that represents the terrain visually. }
        Tile: Cardinal;
        Properties: TPropertyList;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent);override;
      end;

      { This element defines an array of terrain types, which can be referenced from
        the terrain attribute of the tile element. }
      TTerrainTypes = class({$ifdef FPC}specialize{$endif} TObjectList<TTerrain>)
      public
        procedure Assign(const ATerrainTypes: TTerrainTypes);
      end;

      { Tileset definition. }
      TTileset = class(TAssignableObject)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { The first global tile ID of this tileset (this global ID maps to the first
        tile in this tileset). }
        FirstGID: Cardinal;
        { If this tileset is stored in an external TSX (Tile Set XML) file, this
          attribute refers to that file. That TSX file has the same structure as the
          <tileset> element described here. (There is the firstgid attribute missing
          and this source attribute is also not there. These two attributes
          are kept in the TMX map, since they are map specific.) }
        URL: string;
        { The name of this tileset. }
        Name: string;
        { The (maximum) width of the tiles in this tileset. }
        TileWidth: Cardinal;
        { The (maximum) height of the tiles in this tileset. }
        TileHeight: Cardinal;
        { The spacing in pixels between the tiles in this tileset (applies to the
          tileset image). }
        Spacing: Cardinal;
        { The margin around the tiles in this tileset (applies to the tileset image). }
        Margin: Cardinal;
        { The number of tiles in this tileset (since 0.13) }
        TileCount: Cardinal;
        { The number of tile columns in the tileset. For image collection tilesets
        it is editable and is used when displaying the tileset. (since 0.15) }
        Columns: Cardinal;
        { This element is used to specify an offset in pixels, to be applied when
          drawing a tile from the related tileset. When not present, no offset is applied. }
        TileOffset: TVector2Integer;
        Properties: TPropertyList;
        Image: TImage;
        Tiles: TTileList;
        TerrainTypes: TTerrainTypes; //todo: loading TerrainTypes
        { Use to render the tileset. Not a part of the file format.
          In case of TCastleTiledMapControl, this is always TSprite. }
        RendererData: TObject;

        { Already handled by ForceTilesetSpacing. }
        Modified: Boolean;
        CacheImage: TCastleImage;
        { Original data, before handled by ForceTilesetSpacing. }
        OriginalMargin, OriginalSpacing: Cardinal;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent);override;
        function ValidTileId(const TileId : Integer): Boolean;
      end;

      { List of tilesets. }
      TTilesetList = class({$ifdef FPC}specialize{$endif} TObjectList<TTileset>)
      public
        procedure Assign(const ATilesetList: TTilesetList);
      end;

      TMapOrientation = (
        moOrthogonal,
        moIsometric,
        moIsometricStaggered,
        moHexagonal
      );
      TMapRenderOrder = (mroRightDown, mroRightUp, mroLeftDown, mroLeftUp);
      TStaggerAxis = (saX, saY);
      TStaggerIndex = (siOdd, siEven);

  strict private
    { Map stuff. }
    { The TMX format version, generally 1.0. }
    FVersion: string;
    FOrientation: TMapOrientation;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FTileWidth: Cardinal;
    FTileHeight: Cardinal;
    FHexSideLength: Cardinal;
    FStaggerAxis: TStaggerAxis;
    FStaggerIndex: TStaggerIndex;
    FBackgroundColor: TCastleColor;
    FRenderOrder: TMapRenderOrder;
    FBaseUrl: string;
    FTilesets: TTilesetList;
    FProperties: TPropertyList;
    FLayers: TLayerList;
    { Load TMX file from stream. }
    procedure LoadTMXFile(const Stream: TStream; const ABaseUrl: String); overload;
    { Load TMX file by URL. }
    procedure LoadTMXFile(const AURL: String); overload;
    { Load TMX file internally from XMLDocument instance. }
    procedure LoadTMXFileInternal(const Doc: TXMLDocument);

    procedure CreateLists;
    procedure AssignVars(const AMapData: TCastleTiledMapData);
  public
    property Layers: TLayerList read FLayers write FLayers;
    { Map orientation. }
    property Orientation: TMapOrientation read FOrientation;
    property Properties: TPropertyList read FProperties;
    property Tilesets: TTilesetList read FTilesets;
    { The map width in tiles. }
    property Width: Cardinal read FWidth write FWidth;
    { The map height in tiles. }
    property Height: Cardinal read FHeight write FHeight;
    { The width of a tile. }
    property TileWidth: Cardinal read FTileWidth;
    { The height of a tile. }
    property TileHeight: Cardinal read FTileHeight;
    { The height of a hexagon side.
      Only relevant when @link(Orientation) = moHexagonal. }
    property HexSideLength: Cardinal read FHexSideLength;
    { Only relevant when @link(Orientation) = moIsometricStaggered or moHexagonal. }
    property StaggerAxis: TStaggerAxis read FStaggerAxis;
    { Which rows are shifted by 1.
      Only relevant when @link(Orientation) = moIsometricStaggered or moHexagonal. }
    property StaggerIndex: TStaggerIndex read FStaggerIndex;
    { Background color of the map.
      It may be unset in Tiled, which results in transparent color here. }
    property BackgroundColor: TCastleColor read FBackgroundColor;
    { The order in which tiles on tile layers are rendered. Valid values are
      right-down (the default), right-up, left-down and left-up. In all cases,
      the map is drawn row-by-row. (since 0.10, but only supported for orthogonal
      maps at the moment) }
    property RenderOrder: TMapRenderOrder read FRenderOrder;
    { Constructor.
      @param(AURL URL to Tiled (TMX) file.) }
    constructor Create(const Stream: TStream; const ABaseUrl: String); overload;
    constructor Create(const AURL: string); overload;
    constructor Create; overload;
    destructor Destroy; override;

    { Is the given tile number valid.
      Valid map tiles are from (0, 0) (lower-left) to
      (@link(Width) - 1, @link(Height) - 1) (upper-right). }
    function TilePositionValid(const TilePosition: TVector2Integer): Boolean;

    { Detect tile under given position.

      Input Position is in local map coordinates.

      Output TilePosition must is a tile coordinate,
      where map bottom-left corner is (0, 0).

      This method returns @false if the position is outside of the map.
      Valid map tiles are defined as by @link(TilePositionValid). }
    function PositionToTile(const Position: TVector2;
      out TilePosition: TVector2Integer): Boolean;

    function TilePositionToIndex(const ATilePosition: TVector2Integer):integer;

    { Left-bottom corner where the given tile should be rendered. }
    function TileRenderPosition(const TilePosition: TVector2Integer): TVector2;

    { Information about which image (and how) should be displayed at given map position.
      Similar to the following @link(TileRenderData).}
    function TileRenderDataSimple(const TilePosition: TVector2Integer;
      const Layer: TLayer;
      out Tileset: TTileset;
      out Frame: Integer;
      out Data: Cardinal): Boolean;

    { Information about which image (and how) should be displayed at given map position. }
    function TileRenderData(const TilePosition: TVector2Integer;
      const Layer: TLayer;
      out Tileset: TTileset;
      out Frame: Integer;
      out HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean): Boolean;

    { Are the two given tiles neighbors.
      Takes into account map @link(Orientation), so it works for hexagonal,
      orthogonal etc. maps. }
    function TileNeighbor(const Tile1, Tile2: TVector2Integer;
      const CornersAreNeighbors: Boolean): Boolean;

    procedure Assign(Source: TPersistent);override;
  end;

  TTiledMap = TCastleTiledMapData deprecated 'use TCastleTiledMapData';
{$endif read_interface}

{$ifdef read_implementation}

{ global helpers ------------------------------------------------------------- }

function TiledToColorRGB(S: String): TCastleColorRGB;
begin
  if SCharIs(S, 1, '#') then
    Delete(S, 1, 1);
  Result := HexToColorRGB(S);
end;

function TiledToColor(const S: String): TCastleColor;
begin
  Result := Vector4(TiledToColorRGB(S), 1);
end;

procedure TCastleTiledMapData.TLayerList.Assign(const ALayerList: TLayerList);
var
  Layer, NewLayer: TLayer;
begin
  Clear;
  for Layer in ALayerList do
  begin
    NewLayer := TLayerClass(Layer.ClassType).Create;
    NewLayer.Assign(Layer);
    Add(NewLayer);
  end;
end;

procedure TCastleTiledMapData.TTerrainTypes.Assign(
  const ATerrainTypes: TTerrainTypes);
var
  Terrain, NewTerrain: TTerrain;
begin
  Clear;
  for Terrain in ATerrainTypes do
  begin
    NewTerrain := TTerrain.Create;
    NewTerrain.Assign(Terrain);
    Add(NewTerrain);
  end;
end;

procedure TCastleTiledMapData.TTileList.Assign(const ATileList: TTileList);
var
  Tile, NewTile: TTile;
begin
  Clear;
  for Tile in ATileList do
  begin
    NewTile := TTile.Create;
    NewTile.Assign(Tile);
    Add(NewTile);
  end;
end;

constructor TCastleTiledMapData.TTerrain.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
end;

destructor TCastleTiledMapData.TTerrain.Destroy;
begin
  FreeAndNil(Properties);
  inherited Destroy;
end;

procedure TCastleTiledMapData.TTerrain.Assign(Source: TPersistent);
begin
  if (Source is TTerrain) then
  begin
    Name := TTerrain(Source).Name;
    Tile := TTerrain(Source).Tile;
    Properties.Assign(TTerrain(Source).Properties);
  end else inherited;
end;

procedure TCastleTiledMapData.TTiledObjectList.Assign(
  const ATiledObjectList: TTiledObjectList);
var
  TiledObject, NewTiledObject: TTiledObject;
begin
  Clear;
  for TiledObject in ATiledObjectList do
  begin
    NewTiledObject := TTiledObject.Create;
    NewTiledObject.Assign(TiledObject);
    Add(NewTiledObject);
  end;
end;

procedure TCastleTiledMapData.TTilesetList.Assign(
  const ATilesetList: TTilesetList);
var
  Tileset, NewTileset: TTileset;
begin
  Clear;
  for Tileset in ATilesetList do
  begin
    NewTileset := TTileset.Create;
    NewTileset.Assign(Tileset);
    Add(NewTileset);
  end;
end;

{ TProperty ------------------------------------------------------------------ }

procedure TCastleTiledMapData.TProperty.Load(const Element: TDOMElement; const BaseUrl: String);
begin
  Name := Element.AttributeStringDef('name', '');
  Value := Element.AttributeStringDef('value', '');
  AType := PropertyTypeFromName(Element.AttributeStringDef('type', ''));
end;

function TCastleTiledMapData.TProperty.PropertyTypeFromName(const AName: string
  ): TPropertyType;
var
  APropType: TPropertyType;
begin
  Result := TPropertyType.ptString;
  if AName = '' then Exit;
  for APropType in TPropertyType do
  begin
    if PropertyTypeNames[APropType] = AName then Exit(APropType);
  end;
end;

procedure TCastleTiledMapData.TProperty.Assign(Source: TPersistent
  );
begin
  if (Source is TProperty) then
  begin
    Name := TProperty(Source).Name;
    AType := TProperty(Source).AType;
    Value := TProperty(Source).Value;
  end else inherited;
end;

function TCastleTiledMapData.TProperty.AsFloat(const ADefaultValue: Single): Single;
begin
  Result := StrToFloatDefDot(Value, ADefaultValue);
end;

function TCastleTiledMapData.TProperty.AsInteger(const ADefaultValue: Integer): Integer;
begin
  Result := StrToIntDef(Value, ADefaultValue);
end;

function TCastleTiledMapData.TProperty.AsString(): String;
begin
  Result := Value;
end;

function TCastleTiledMapData.TProperty.AsBoolean(
  const ADefaultValue: Boolean): Boolean;
begin
  if Value = 'true' then Result := True
  else
  if Value = 'false' then Result := False
  else
    Result := ADefaultValue;
end;

function TCastleTiledMapData.TProperty.AsColor(
  const ADefaultValue: TCastleColor): TCastleColor;
begin
  { hex string: '#AARRGGBB' }
  if Length(Value) = 9 then
  begin
    try
      Result := Vector4(
        StrHexToInt(Copy(Value, 4, 2)) / 255,
        StrHexToInt(Copy(Value, 6, 2)) / 255,
        StrHexToInt(Copy(Value, 8, 2)) / 255,
        StrHexToInt(Copy(Value, 2, 2)) / 255);
    except
      Result := ADefaultValue;
    end
  end else Result := ADefaultValue;
end;

procedure TCastleTiledMapData.TProperty.SetFloat(const AValue: Single);
begin
  AType := ptFloat;
  Value := AValue.ToString;
end;

procedure TCastleTiledMapData.TProperty.SetInteger(const AValue: Integer);
begin
  AType := ptInteger;
  Value := AValue.ToString;
end;

procedure TCastleTiledMapData.TProperty.SetString(const AValue: String);
begin
  AType := ptString;
  Value := AValue;
end;

procedure TCastleTiledMapData.TProperty.SetBoolean(const AValue: Boolean);
begin
  AType := ptBoolean;
  if AValue then Value := 'true' else Value := 'false';
end;

procedure TCastleTiledMapData.TProperty.SetColor(const AValue: TCastleColor);
begin
  AType := ptColor;
  { hex string: '#AARRGGBB' }
  Value := '#'+ IntToHex(RoundClamp255(AValue.W * 255), 2) +
                IntToHex(RoundClamp255(AValue.X * 255), 2) +
                IntToHex(RoundClamp255(AValue.Y * 255), 2) +
                IntToHex(RoundClamp255(AValue.Z * 255), 2);
end;

{ TPropertyList -------------------------------------------------------------- }

function TCastleTiledMapData.TPropertyList.GetItems(const AName: String
  ): TProperty;
var
  P: TProperty;
begin
  for P in Self do
  begin
    if P.Name = AName then Exit(P);
  end;
  Result := nil;
end;

function TCastleTiledMapData.TPropertyList.GetOrCreateItems(const AName: String
  ): TProperty;
begin
  Result := Items[AName];
  if not Assigned(Result) Then
  begin
    Result := TProperty.Create;
    Result.Name := AName;
    Self.Add(Result);
  end;
end;

procedure TCastleTiledMapData.TPropertyList.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  NewProperty: TProperty;
begin
  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      if LowerCase(I.Current.TagName) = 'property' then
      begin
        NewProperty := TProperty.Create;
        NewProperty.Load(I.Current, BaseUrl);
        Add(NewProperty);
      end;
    end;
  finally FreeAndNil(I) end;
end;

procedure TCastleTiledMapData.TPropertyList.Assign(
  const APropertyList: TPropertyList);
var
  Prop, NewProp: TProperty;
begin
  Clear;
  for Prop in APropertyList do
  begin
    NewProp := TProperty.Create;
    NewProp.Assign(Prop);
    Add(NewProp);
  end;
end;

function TCastleTiledMapData.TPropertyList.FloatValue(const AName: string;
  var AValue: Single): Boolean;
var
  P: TProperty;
begin
  P := self.Items[AName];
  Result := Assigned(P);
  if Assigned(P) then AValue := P.AsFloat(AValue);
end;

function TCastleTiledMapData.TPropertyList.IntegerValue(const AName: string;
  var AValue: Integer): Boolean;
var
  P: TProperty;
begin
  P := self.Items[AName];
  Result := Assigned(P);
  if Assigned(P) then AValue := P.AsInteger(AValue);
end;

function TCastleTiledMapData.TPropertyList.StringValue(const AName: string;
  var AValue: String): Boolean;
var
  P: TProperty;
begin
  P := self.Items[AName];
  Result := Assigned(P);
  if Assigned(P) then AValue := P.AsString;
end;

function TCastleTiledMapData.TPropertyList.BooleanValue(const AName: string;
  var AValue: Boolean): Boolean;
var
  P: TProperty;
begin
  P := self.Items[AName];
  Result := Assigned(P);
  if Assigned(P) then AValue := P.AsBoolean(AValue);
end;

function TCastleTiledMapData.TPropertyList.ColorValue(const AName: string;
  var AValue: TCastleColor): Boolean;
var
  P: TProperty;
begin
  P := self.Items[AName];
  Result := Assigned(P);
  if Assigned(P) then AValue := P.AsColor(AValue);
end;

procedure TCastleTiledMapData.TPropertyList.SetFloatValue(const AName: string;
  const AValue: Single);
begin
  GetOrCreateItems(AName).SetFloat(AValue);
end;

procedure TCastleTiledMapData.TPropertyList.SetIntegerValue(
  const AName: string; const AValue: Integer);
begin
  GetOrCreateItems(AName).SetInteger(AValue);
end;

procedure TCastleTiledMapData.TPropertyList.SetStringValue(const AName: string;
  const AValue: String);
begin
  GetOrCreateItems(AName).SetString(AValue);
end;

procedure TCastleTiledMapData.TPropertyList.SetBooleanValue(
  const AName: string; const AValue: Boolean);
begin
  GetOrCreateItems(AName).SetBoolean(AValue);
end;

procedure TCastleTiledMapData.TPropertyList.SetColorValue(const AName: string;
  const AValue: TCastleColor);
begin
  GetOrCreateItems(AName).SetColor(AValue);
end;

procedure TCastleTiledMapData.TPropertyList.AddOrSetProperty(
  const AName: String; const AValue: String; const APropType: TPropertyType);
var
  P: TProperty;
begin
  P := GetOrCreateItems(AName);
  P.AType := APropType;
  P.Value := AValue;
end;

{ TData ---------------------------------------------------------------------- }

procedure TCastleTiledMapData.TData.Load(const Element: TDOMElement; const BaseUrl: String);

  procedure AddData(const Item: Cardinal);
  var
    L: Integer;
  begin
    L := Length(Data);
    SetLength(Data, L + 1);
    Data[L] := Item;
  end;

  procedure ReadEncodingNone;
  var
    I: TXMLElementIterator;
  begin
    I := TXMLElementFilteringIterator.Create(Element, 'tile');
    try
      while I.GetNext do
        AddData(I.Current.AttributeCardinalDef('gid', 0));
    finally FreeAndNil(I) end;
  end;

  procedure ReadEncodingBase64;
  const
    BufferSize = 16;
  var
    Decoder: TBase64DecodingStream;
    Decompressor: TStream;
    Buffer: array[0..BufferSize-1] of Cardinal;
    DataCount, DataLength: Longint;
  begin
    Decoder := TBase64DecodingStream.Create(TStringStream.Create(Element.TextData));
    try
      Decoder.SourceOwner := true;

      case Compression of
        ctGzip: WritelnWarning('TData.Load', 'TODO: Gzip format not implemented');
        ctZLib:
          begin
            Decompressor := TDecompressionStream.Create(Decoder);
            try
              repeat
                DataCount := Decompressor.Read(Buffer, BufferSize * SizeOf(Cardinal));
                DataLength := Length(Data);
                SetLength(Data, DataLength+(DataCount div SizeOf(Cardinal)));
                if DataCount > 0 then // because if DataCount=0 then ERangeCheck error
                  Move(Buffer, Data[DataLength], DataCount);
              until DataCount < SizeOf(Buffer);
            finally
              Decompressor.Free;
            end;
          end;
        ctNone:
          begin
            repeat
              DataCount := Decoder.Read(Buffer, BufferSize * SizeOf(Cardinal));
              DataLength := Length(Data);
              SetLength(Data, DataLength+(DataCount div SizeOf(Cardinal)));
              if DataCount > 0 then // because if DataCount=0 then ERangeCheck error
                Move(Buffer, Data[DataLength], DataCount);
            until DataCount < SizeOf(Buffer);
          end;
      end;
    finally FreeAndNil(Decoder) end;
  end;

  procedure ReadEncodingCSV;
  var
    RawData, Token: String;
    SeekPos: Integer;
  begin
    RawData := Element.TextData;
    SeekPos := 1;
    repeat
      Token := NextToken(RawData, SeekPos, [','] + WhiteSpaces);
      if Token = '' then break;
      { We use StrToDWord, as any value in 32-bit unsigned range is OK,
        even values > 32-bit signed range.
        E.g. test_hexagonal_tile_60x60x30.tmx on
        https://github.com/bjorn/tiled/tree/master/examples
        has values like 3221225473 . }
      AddData(StrToDWord(Token));
    until false;
  end;

var
  TmpStr: string;
begin
  Encoding := etNone;
  Compression := ctNone;

  if Element.AttributeString('encoding', TmpStr) then
  begin
    if TmpStr = 'base64' then
      Encoding := etBase64
    else
    if TmpStr = 'csv' then
      Encoding := etCSV
    else
      WritelnWarning('Invalid Tiled encoding "%s"', [TmpStr]);
  end;

  if Element.AttributeString('compression', TmpStr) then
  begin
    if TmpStr = 'gzip' then
      Compression := ctGzip
    else
    if TmpStr = 'zlib' then
      Compression := ctZLib
    else
      WritelnWarning('Invalid Tiled compression "%s"', [TmpStr]);
  end;

  case Encoding of
    etNone: ReadEncodingNone;
    etBase64: ReadEncodingBase64;
    etCSV: ReadEncodingCSV;
  end;
end;

procedure TCastleTiledMapData.TData.Assign(Source: TPersistent);
begin
  if (Source is TData) then
  begin
    Encoding := TData(Source).Encoding;
    Compression := TData(Source).Compression;
    if not IgnoreDataAssign then
      Data := TData(Source).Data;
  end else inherited;
end;

{ TImage --------------------------------------------------------------------- }

destructor TCastleTiledMapData.TImage.Destroy;
begin
  FreeAndNil(Data);
  inherited;
end;

procedure TCastleTiledMapData.TImage.Load(const Element: TDOMElement; const BaseUrl: String);
const
  DefaultTrans: TCastleColorRGB = (X: 1.0; Y: 0.0; Z: 1.0); {Fuchsia}
var
  I: TXMLElementIterator;
  TmpStr, URLPrefix: string;
begin
  if Element.AttributeString('format', TmpStr) then
    Format := TmpStr;
  if Element.AttributeString('source', TmpStr) then
  begin
    URLPrefix := CombineURI(BaseUrl, TmpStr);
    { Tiled allows loading images ignoring the '.png' suffix }
    if (ExtractFileExt(TmpStr) = '') and not URIFileExists(URLPrefix) then
      URL := URLPrefix + '.png'
    else
      URL := URLPrefix;
  end;
  if Element.AttributeString('trans', TmpStr) then
    Trans := TiledToColorRGB(TmpStr)
  else
    Trans := DefaultTrans;
  if Element.AttributeString('width', TmpStr) then
    Width := StrToInt(TmpStr);
  if Element.AttributeString('height', TmpStr) then
    Height := StrToInt(TmpStr);

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      if LowerCase(I.Current.TagName) = 'data' then
      begin
        if Data = nil then
          Data := TData.Create;
        Data.Load(I.Current, BaseUrl);
      end;
    end;
  finally FreeAndNil(I) end;
end;

procedure TCastleTiledMapData.TImage.DetermineSize;
var
 ImageData: TEncodedImage;
begin
  if (Width = 0) and (Height = 0) then
  begin
    { Note: While we could store Image to use it loader, e.g.

        TilesetTextureNode.LoadFromImage(Tileset.Image, false, Tileset.Image.URL);

      ... but this would be an optimization that complicates code for a seldom use-case
      (because new Tiled files should include size).
      So we don't do it, and just load + free image only to read size. }

    WritelnLog('Determining image size by loading it. It is more efficient to store image size in TMX file.');
    ImageData := LoadEncodedImage(URL);
    try
      Width := ImageData.Width;
      Height := ImageData.Height;
    finally FreeAndNil(ImageData) end;
  end;
end;

procedure TCastleTiledMapData.TImage.Assign(Source: TPersistent);
begin
  if (Source is TImage) then
  begin
    Format := TImage(Source).Format;
    URL := TImage(Source).URL;
    Trans := TImage(Source).Trans;
    Width := TImage(Source).Width;
    Height := TImage(Source).Height;
    { Data is created on demand }
    if not Assigned(TImage(Source).Data) then
      FreeAndNil(Data) else
    begin
      if not Assigned(Data) then Data := TData.Create;
      Data.Assign(TImage(Source).Data);
    end;
  end else inherited;

end;

{ TFrame --------------------------------------------------------------------- }

procedure TCastleTiledMapData.TFrame.Load(const Element: TDOMElement; const BaseUrl: String);
var
  TmpStr: string;
begin
  if Element.AttributeString('tileid', TmpStr) then
    TileId := StrToInt(TmpStr);
  if Element.AttributeString('duration', TmpStr) then
    Duration := StrToInt(TmpStr);
end;

procedure TCastleTiledMapData.TFrame.Assign(Source: TPersistent);
begin
  if (Source is TFrame) then
  begin
    TileId := TFrame(Source).TileId;
    Duration := TFrame(Source).Duration;
  end else inherited;
end;

{ TAnimation ----------------------------------------------------------------- }

procedure TCastleTiledMapData.TAnimation.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  NewFrame: TFrame;
begin
  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      if LowerCase(I.Current.TagName) = 'frame' then
      begin
        NewFrame := TFrame.Create;
        NewFrame.Load(I.Current, BaseUrl);
        Add(NewFrame);
      end;
    end;
  finally FreeAndNil(I) end;
end;

procedure TCastleTiledMapData.TAnimation.Assign(const AAnimation: TAnimation);
var
  Frame, NewFrame: TFrame;
begin
  Clear;
  for Frame in AAnimation do
  begin
    NewFrame := TFrame.Create;
    NewFrame.Assign(Frame);
    Add(NewFrame);
  end;
end;

{ TTile ------------------------------------------------------------------- }

constructor TCastleTiledMapData.TTile.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
  Animation := TAnimation.Create;
  Image := TImage.Create;

  { Default values }
  Probability := 1.0;
end;

destructor TCastleTiledMapData.TTile.Destroy;
begin
  FreeAndNil(Properties);
  FreeAndNil(Animation);
  FreeAndNil(Image);
  FreeAndNil(ObjectGroup);
  inherited;
end;

procedure TCastleTiledMapData.TTile.Assign(Source: TPersistent);
begin
  if (Source is TTile) then
  begin
    Id := TTile(Source).Id;
    Terrain := TTile(Source).Terrain;
    Probability := TTile(Source).Probability;
    Properties.Assign(TTile(Source).Properties);
    Image.Assign(TTile(Source).Image);
    { ObjectGroup is created on demand }
    if not Assigned(TTile(Source).ObjectGroup) then
      FreeAndNil(ObjectGroup) else
    begin
      if not Assigned(ObjectGroup) then ObjectGroup := TObjectGroupLayer.Create;
      ObjectGroup.Assign(TTile(Source).ObjectGroup);
    end;
    Animation.Assign(TTile(Source).Animation);
    RendererData := TTile(Source).RendererData;
  end else inherited;
end;

procedure TCastleTiledMapData.TTile.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  TmpStr, LowerTagName: string;
  SPosition: Integer;
begin
  if Element.AttributeString('id', TmpStr) then
    Id := StrToInt(TmpStr);
  if Element.AttributeString('terrain', TmpStr) then
  begin
    SPosition := 1;
    Terrain.X := StrToInt(NextToken(TmpStr, SPosition, [',']));
    Terrain.Y := StrToInt(NextToken(TmpStr, SPosition, [',']));
    Terrain.Z := StrToInt(NextToken(TmpStr, SPosition, [',']));
    Terrain.W := StrToInt(NextToken(TmpStr, SPosition, [',']));
  end;
  if Element.AttributeString('probability', TmpStr) then
    Probability := StrToFloatDot(TmpStr);

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      LowerTagName := LowerCase(I.Current.TagName8);
      if LowerTagName = 'properties' then
        Properties.Load(I.Current, BaseUrl)
      else
      if LowerTagName = 'image' then
        Image.Load(I.Current, BaseUrl)
      else
      if LowerTagName = 'animation' then
        Animation.Load(I.Current, BaseUrl)
      else
      if LowerTagName = 'objectgroup' then
      begin
        if ObjectGroup = nil then
          ObjectGroup := TObjectGroupLayer.Create;
        ObjectGroup.Load(I.Current, BaseUrl);
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TTiledObject ------------------------------------------------------------------- }

constructor TCastleTiledMapData.TTiledObject.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
  Image := TImage.Create;
end;

destructor TCastleTiledMapData.TTiledObject.Destroy;
begin
  FreeAndNil(Properties);
  FreeAndNil(Points);
  FreeAndNil(Image);
  inherited;
end;

procedure TCastleTiledMapData.TTiledObject.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  TmpStr: string;

  function ReadVector(const S: String): TVector2;
  var
    SeekPos: Integer;
    Token: String;
  begin
    SeekPos := 1;
    Token := NextToken(S, SeekPos, [',']);
    Result.X := StrToFloatDot(Token);
    Token := NextToken(S, SeekPos, [',']);
    Result.Y := StrToFloatDot(Token);
    Token := NextToken(S, SeekPos, [',']);
    if Token <> '' then
      raise Exception.CreateFmt('Unexpected vector format in Tiled map: %s', [S]);
  end;

  procedure ReadPoints(const PointsString: string; var PointsList: TVector2List);
  var
    SeekPos: Integer;
    Token: String;
  begin
    if not Assigned(PointsList) then PointsList := TVector2List.Create;
    SeekPos := 1;
    repeat
      Token := NextToken(PointsString, SeekPos, [' ']);
      if Token = '' then Break;
      PointsList.Add(ReadVector(Token));
    until false;
  end;

var
  LowerTagName: String;
begin
  Width := 0;
  Height := 0;
  Rotation := 0;
  Visible := True;
  if Element.AttributeString('id', TmpStr) then
    Id := StrToInt(TmpStr);
  if Element.AttributeString('name', TmpStr) then
    Name := TmpStr;
  if Element.AttributeString('type', TmpStr) then
    Type_ := TmpStr;
  if Element.AttributeString('x', TmpStr) then
    X := StrToFloatDot(TmpStr);
  if Element.AttributeString('y', TmpStr) then
    Y := StrToFloatDot(TmpStr);
  if Element.AttributeString('width', TmpStr) then
    Width := StrToFloatDot(TmpStr);
  if Element.AttributeString('height', TmpStr) then
    Height := StrToFloatDot(TmpStr);
  if Element.AttributeString('rotation', TmpStr) then
    Rotation := StrToFloatDot(TmpStr);
  if Element.AttributeString('gid', TmpStr) then
    GId := StrToDWord(TmpStr);
  if Element.AttributeString('visible', TmpStr) then
    if TmpStr = '0' then
      Visible := False;

  { Assume rectangle TiledObject first as it is the only element which has no
    sub-element in TMX file which indicates its primitive type. Will be over-
    ridden by follwing iteration, if a sub-element exists. }
  Primitive := topRectangle;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      LowerTagName := LowerCase(I.Current.TagName8);
      if LowerTagName = 'properties' then
        Properties.Load(I.Current, BaseUrl)
      else
      if LowerTagName = 'point' then
        Primitive := topPoint
      else
      if LowerTagName = 'ellipse' then
        Primitive := topEllipse
      else
      if LowerTagName = 'polygon' then
      begin
        Primitive := topPolygon;
        ReadPoints(I.Current.AttributeStringDef('points', ''), Points);
      end else
      if LowerTagName = 'polyline' then
      begin
        Primitive := topPolyLine;
        ReadPoints(I.Current.AttributeStringDef('points', ''), Points);
      end else
      if LowerTagName = 'image' then
        Image.Load(I.Current, BaseUrl);
    end;
  finally FreeAndNil(I) end;
end;

function TCastleTiledMapData.TTiledObject.Position: TVector2;
begin
  Result := Vector2(X, Y);
end;

procedure TCastleTiledMapData.TTiledObject.Assign(
  Source: TPersistent);
begin
  if (Source is TTiledObject) then
  begin
    Id := TTiledObject(Source).Id;
    Name := TTiledObject(Source).Name;
    Type_ := TTiledObject(Source).Type_;
    X := TTiledObject(Source).X;
    Y := TTiledObject(Source).Y;
    Width := TTiledObject(Source).Width;
    Height := TTiledObject(Source).Height;
    Rotation := TTiledObject(Source).Rotation;
    GId := TTiledObject(Source).GId;
    Visible := TTiledObject(Source).Visible;
    Properties.Assign(TTiledObject(Source).Properties);
    Points.Assign(TTiledObject(Source).Points);
    Primitive := TTiledObject(Source).Primitive;
    Image.Assign(TTiledObject(Source).Image);
  end else inherited;

end;

{ TLayer ------------------------------------------------------------------- }

constructor TCastleTiledMapData.TLayer.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
  Data := TData.Create;
end;

destructor TCastleTiledMapData.TLayer.Destroy;
begin
  FreeAndNil(Properties);
  FreeAndNil(Data);
  inherited;
end;

procedure TCastleTiledMapData.TLayer.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  TmpStr, LowerTagName: string;
begin
  Opacity := 1;
  Visible := True;
  OffsetX := 0;
  OffsetY := 0;
  if Element.AttributeString('color', TmpStr) then
    Color := TiledToColorRGB(TmpStr)
  else
    Color := BlackRGB; // Default layer color in Tiled editor if no color is set.
  Name := Element.AttributeStringDef('name', '');
  if Element.AttributeString('opacity', TmpStr) then
    Opacity := StrToFloatDot(TmpStr);
  if Element.AttributeStringDef('visible', '1') = '0' then
    Visible := false;
  OffsetX := Element.AttributeSingleDef('offsetx', 0);
  OffsetY := Element.AttributeSingleDef('offsety', 0);

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      LowerTagName := LowerCase(I.Current.TagName8);
      if LowerTagName = 'properties' then
        Properties.Load(I.Current, BaseUrl)
      else
      if LowerTagName = 'data' then
      begin
        if Data = nil then
          Data := TData.Create;
        Data.Load(I.Current, BaseUrl);
      end;
    end;
  finally FreeAndNil(I) end;
end;

procedure TCastleTiledMapData.TLayer.SetYSortEnabled(const AValue: Boolean);
begin
  Properties.SetBooleanValue('ysort', AValue);
end;

function TCastleTiledMapData.TLayer.Offset: TVector2;
begin
  Result := Vector2(OffsetX, OffsetY);
end;

procedure TCastleTiledMapData.TLayer.Assign(Source: TPersistent);
begin
  if (Source is TLayer) then
  begin
    Name := TLayer(Source).Name;
    Opacity := TLayer(Source).Opacity;
    Visible := TLayer(Source).Visible;
    OffsetX := TLayer(Source).OffsetX;
    OffsetY := TLayer(Source).OffsetY;
    Properties.Assign(TLayer(Source).Properties);
    Data.Assign(TLayer(Source).Data);
    Color := TLayer(Source).Color;
    Width := TLayer(Source).Width;
    Height := TLayer(Source).Height;
  end else inherited;
end;

function TCastleTiledMapData.TLayer.GetYSortEnabled: Boolean;
begin
  Result := False;
  Properties.BooleanValue('ysort', Result);
end;

{ TObjectGroupLayer ---------------------------------------------------------- }

constructor TCastleTiledMapData.TObjectGroupLayer.Create;
begin
  inherited;
  Objects := TTiledObjectList.Create;
end;

destructor TCastleTiledMapData.TObjectGroupLayer.Destroy;
begin
  FreeAndNil(Objects);
  inherited;
end;

procedure TCastleTiledMapData.TObjectGroupLayer.Assign(
  Source: TPersistent);
begin
  if (Source is TObjectGroupLayer) then
  begin
    DrawOrder := TObjectGroupLayer(Source).DrawOrder;
    Objects.Assign(TObjectGroupLayer(Source).Objects);
  end;
  inherited;
end;

procedure TCastleTiledMapData.TObjectGroupLayer.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  NewObject: TTiledObject;
  TmpStr: string;
begin
  inherited;

  DrawOrder := odoTopDown;

  if Element.AttributeString('draworder', TmpStr) then
  begin
    if TmpStr = 'index' then
      DrawOrder := odoIndex
    else
    if TmpStr = 'topdown' then
      DrawOrder := odoTopDown;
  end;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      if  LowerCase(I.Current.TagName) = 'object' then
      begin
        NewObject := TTiledObject.Create;
        NewObject.Load(I.Current, BaseUrl);
        Objects.Add(NewObject);
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TImageLayer ---------------------------------------------------------------- }

destructor TCastleTiledMapData.TImageLayer.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure TCastleTiledMapData.TImageLayer.Assign(
  Source: TPersistent);
begin
  if (Source is TImageLayer) then
  begin
    Image.Assign(TImageLayer(Source).Image);
  end;
  inherited;
end;

procedure TCastleTiledMapData.TImageLayer.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
begin
  inherited;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      if LowerCase(I.Current.TagName) = 'image' then
      begin
        if Image = nil then
          Image := TImage.Create;
        Image.Load(I.Current, BaseUrl);
      end;
    end;
  finally FreeAndNil(I) end;
end;

constructor TCastleTiledMapData.TImageLayer.Create;
begin
  inherited;
  Image := TImage.Create;
end;

{ TTileset ------------------------------------------------------------------- }

constructor TCastleTiledMapData.TTileset.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
  Tiles := TTileList.Create;
  TerrainTypes := TTerrainTypes.Create;
  Image := TImage.Create;
  FirstGID := 1;
end;

destructor TCastleTiledMapData.TTileset.Destroy;
begin
  FreeAndNil(CacheImage);
  FreeAndNil(Image);
  FreeAndNil(Tiles);
  FreeAndNil(TerrainTypes);
  FreeAndNil(Properties);
  inherited;
end;

procedure TCastleTiledMapData.TTileset.Assign(Source: TPersistent);
begin
  if (Source is TTileset) then
  begin
    FirstGID := TTileset(Source).FirstGID;
    URL := TTileset(Source).URL;
    Name := TTileset(Source).Name;
    TileWidth := TTileset(Source).TileWidth;
    TileHeight := TTileset(Source).TileHeight;
    Spacing := TTileset(Source).Spacing;
    Margin := TTileset(Source).Margin;
    TileCount := TTileset(Source).TileCount;
    Columns := TTileset(Source).Columns;
    TileOffset := TTileset(Source).TileOffset;
    Properties.Assign(TTileset(Source).Properties);
    Image.Assign(TTileset(Source).Image);
    Tiles.Assign(TTileset(Source).Tiles);
    TerrainTypes.Assign(TTileset(Source).TerrainTypes);
    RendererData := TTileset(Source).RendererData;
    Modified := TTileset(Source).Modified;
    OriginalMargin := TTileset(Source).OriginalMargin;
    OriginalSpacing := TTileset(Source).OriginalSpacing;
  end else inherited;
end;

function TCastleTiledMapData.TTileset.ValidTileId(const TileId: Integer
  ): Boolean;
begin
  Result := Between(TileId, 0, Tiles.Count - 1);
end;

procedure TCastleTiledMapData.TTileset.Load(const Element: TDOMElement; const BaseUrl: String);

  { TSX file loading. }
  procedure LoadTilesetFromURL;
  var
    Doc: TXMLDocument;
  begin
    Doc := URLReadXML(URL);
    try
      Check(LowerCase(Doc.DocumentElement.TagName) = 'tileset',
        'Root element of TSX file must be <tileset>');
      Load(Doc.DocumentElement, URL);
    finally
      FreeAndNil(Doc);
    end;
  end;

  { Reorder and complete Tiles list, to make sure

    - Tiles.Count = TileCount
    - Tiles list has no nil elements.

        Tiled doesn't explicitly write all tiles in TSX file.
        When the tile has everything default (probability equals 1.0,
        no terrains set, no type set...) it is not present in TSX.
        But it is more comfortable to have all instances of TTile available in API.

    - All TTile instances are at index corresponding to their Id. }
  procedure CompleteTiles;
  var
    NewTiles: TTileList;
    Tile: TTile;
    I: Integer;
  begin
    NewTiles := TTileList.Create(true);
    NewTiles.Count := TileCount;

    { place existing tiles in NewTiles, making sure they are at proper index }
    for Tile in Tiles do
    begin
      if not Between(Tile.Id, 0, TileCount - 1) then
      begin
        WritelnWarning('Tile id %d is outside of allowed range 0..%d', [
          Tile.Id,
          TileCount - 1
        ]);
        Continue;
      end;
      Check(NewTiles[Tile.Id] = nil, 'Multiple tiles have the same id');
      NewTiles[Tile.Id] := Tile;
    end;

    { add new tiles to fill the gaps }
    for I := 0 to TileCount - 1 do
      if NewTiles[I] = nil then
      begin
        NewTiles[I] := TTile.Create;
        NewTiles[I].Id := I;
      end;

    { swap Tiles into NewTiles }
    Tiles.OwnsObjects := false;
    FreeAndNil(Tiles);
    Tiles := NewTiles;
  end;

var
  I: TXMLElementIterator;
  NewTile: TTile;
  TmpStr, LowerTagName: string;
begin
  TileOffset := TVector2Integer.Zero;
  Spacing := 0;
  Margin := 0;

  if Element.AttributeString('firstgid', TmpStr) then
    FirstGID := StrToDWord(TmpStr);
  { Otherwise (if firstgid not specified in XML element), leave FirstGID unchanged.
    This is important to correctly read "firstgid" in case <tileset> in TMX points
    to external file like

      <tileset firstgid="10" source="image3x3-regular.tsx"/>

     This means that TCastleTiledMapData.TTileset.Load calls itself,
     and outer call set FirstGID to 10 and it should not be overridden by inner call.
     Testcases in examples/tiled/map_viewer/data/maps/multiple_tilesets/ . }

  if Element.AttributeString('source', TmpStr) then
  begin
    URL := CombineURI(BaseUrl, TmpStr);
    LoadTilesetFromURL;
    Exit;
  end;
  if Element.AttributeString('name', TmpStr) then
    Name := TmpStr;
  if Element.AttributeString('tilewidth', TmpStr) then
    TileWidth := StrToInt(TmpStr);
  if Element.AttributeString('tileheight', TmpStr) then
    TileHeight := StrToInt(TmpStr);
  if Element.AttributeString('spacing', TmpStr) then
    Spacing := StrToInt(TmpStr);
  if Element.AttributeString('margin', TmpStr) then
    Margin := StrToInt(TmpStr);
  if Element.AttributeString('tilecount', TmpStr) then
    TileCount := StrToInt(TmpStr)
  else
    TileCount := 0;
  if Element.AttributeString('columns', TmpStr) then
    Columns := StrToInt(TmpStr)
  else
    Columns := 0;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      LowerTagName := LowerCase(I.Current.TagName8);
      if LowerTagName = 'tileoffset' then
      begin
        TileOffset.X := I.Current.AttributeIntegerDef('x', 0);
        TileOffset.Y := I.Current.AttributeIntegerDef('y', 0);
      end else
      if LowerTagName = 'properties' then
        Properties.Load(I.Current, BaseUrl)
      else
      if LowerTagName = 'image' then
        Image.Load(I.Current, BaseUrl)
      else
      if LowerTagName = 'tile' then
      begin
        NewTile := TTile.Create;
        NewTile.Load(I.Current, BaseUrl);
        Tiles.Add(NewTile);
      end;
    end;
  finally FreeAndNil(I) end;

  { In case tileset doesn't specify image size,
    testcase: examples/tiled/map_viewer/data/maps/perspective_walls.tmx }
  Image.DetermineSize;

  { Fix Columns if necessary, testcase: examples/tiled/map_viewer/data/maps/desert.tmx }
  if Columns = 0 then
    Columns := Image.Width div TileWidth;

  { Fix TileCount, if it is not found in (old) tsx-files prior to version 0.13 (Aug 2015). }
  if TileCount = 0 then
    TileCount := (Image.Height div TileHeight) * Columns;

  CompleteTiles;
end;

{ TCastleTiledMapData ------------------------------------------------------------------ }

procedure TCastleTiledMapData.LoadTMXFile(const Stream: TStream; const ABaseUrl: String);
var
  Doc: TXMLDocument;
begin
  ReadXMLFile(Doc, Stream, ABaseUrl);
  try
    LoadTMXFileInternal(Doc);
  finally FreeAndNil(Doc) end;
end;

procedure TCastleTiledMapData.LoadTMXFile(const AURL: String);
var
  Doc: TXMLDocument;
begin
  Doc := URLReadXML(AURL);
  try
    LoadTMXFileInternal(Doc);
  finally FreeAndNil(Doc) end;
end;

procedure TCastleTiledMapData.LoadTMXFileInternal(const Doc: TXMLDocument);
var
  TmpStr, LowerTagName: string;
  I: TXMLElementIterator;
  NewLayer: TLayer;
  NewTileset: TTileset;
begin
  // Parse map attributes
  Check(LowerCase(Doc.DocumentElement.TagName) = 'map',
    'Root element of TMX file must be <map>');
  if Doc.DocumentElement.AttributeString('version', TmpStr) then
    FVersion := TmpStr;
  if Doc.DocumentElement.AttributeString('orientation', TmpStr) then
  begin
    if TmpStr = 'orthogonal' then
      FOrientation := moOrthogonal
    else
    if TmpStr = 'isometric' then
      FOrientation := moIsometric
    else
    if TmpStr = 'staggered' then
      FOrientation := moIsometricStaggered
    else
    if TmpStr = 'hexagonal' then
      FOrientation := moHexagonal
    else
      WritelnWarning('Invalid orientation "%s" in Tiled map file (TMX)', [TmpStr]);
  end;
  if Doc.DocumentElement.AttributeString('width', TmpStr) then
    FWidth := StrToInt(TmpStr);
  if Doc.DocumentElement.AttributeString('height', TmpStr) then
    FHeight := StrToInt(TmpStr);
  if Doc.DocumentElement.AttributeString('tilewidth', TmpStr) then
    FTileWidth := StrToInt(TmpStr);
  if Doc.DocumentElement.AttributeString('tileheight', TmpStr) then
    FTileHeight := StrToInt(TmpStr);
  if Doc.DocumentElement.AttributeString('hexsidelength', TmpStr) then
    FHexSideLength := StrToInt(TmpStr);
  if Doc.DocumentElement.AttributeString('staggeraxis', TmpStr) then
  begin
    if TmpStr = 'x' then
      FStaggerAxis := saX
    else
    if TmpStr = 'y' then
      FStaggerAxis := saY
    else
      WritelnWarning('Invalid staggeraxis "%s" in Tiled map file (TMX)', [TmpStr]);
  end;
  if Doc.DocumentElement.AttributeString('staggerindex', TmpStr) then
  begin
    if TmpStr = 'odd' then
      FStaggerIndex := siOdd
    else
    if TmpStr = 'even' then
      FStaggerIndex := siEven
    else
      WritelnWarning('Invalid staggerindex "%s" in Tiled map file (TMX)', [TmpStr]);
  end;
  if Doc.DocumentElement.AttributeString('backgroundcolor', TmpStr) then
    FBackgroundColor := TiledToColor(TmpStr);
  if Doc.DocumentElement.AttributeString('renderorder', TmpStr) then
  begin
    if TmpStr = 'right-down' then
      FRenderOrder := mroRightDown
    else
    if TmpStr = 'right-up' then
      FRenderOrder := mroRightUp
    else
    if TmpStr = 'left-down' then
      FRenderOrder := mroLeftDown
    else
    if TmpStr = 'left-up' then
      FRenderOrder := mroLeftUp
    else
      WritelnWarning('Invalid renderOrder "%s" in Tiled map file (TMX)', [TmpStr]);
  end;
  // Parse map children
  I := TXMLElementIterator.Create(Doc.DocumentElement);
  try
    while I.GetNext do
    begin
      LowerTagName := LowerCase(I.Current.TagName8);
      if LowerTagName = 'tileset' then
      begin
        NewTileset := TTileset.Create;
        NewTileset.Load(I.Current, FBaseUrl);
        FTilesets.Add(NewTileset);
      end else
      if LowerTagName = 'layer' then
      begin
        NewLayer := TLayer.Create;
        NewLayer.Load(I.Current, FBaseUrl);
        FLayers.Add(NewLayer);
      end else
      if LowerTagName = 'objectgroup' then
      begin
        NewLayer := TObjectGroupLayer.Create;
        NewLayer.Load(I.Current, FBaseUrl);
        FLayers.Add(NewLayer);
      end else
      if LowerTagName = 'imagelayer' then
      begin
        NewLayer := TImageLayer.Create;
        NewLayer.Load(I.Current, FBaseUrl);
        FLayers.Add(NewLayer);
      end else
      if LowerTagName = 'properties' then
        FProperties.Load(I.Current, FBaseUrl);
    end;
  finally FreeAndNil(I) end;
end;

procedure TCastleTiledMapData.CreateLists;
begin
  FTilesets := TTilesetList.Create(true);
  FProperties := TPropertyList.Create(true);
  FLayers := TLayerList.Create(true);
end;

procedure TCastleTiledMapData.AssignVars(const AMapData: TCastleTiledMapData);
begin
  FVersion := AMapData.FVersion;
  FOrientation := AMapData.FOrientation;
  FWidth := AMapData.FWidth;
  FHeight := AMapData.FHeight;
  FTileWidth := AMapData.FTileWidth;
  FTileHeight := AMapData.FTileHeight;
  FHexSideLength := AMapData.FHexSideLength;
  FStaggerAxis := AMapData.FStaggerAxis;
  FStaggerIndex := AMapData.FStaggerIndex;
  FBackgroundColor := AMapData.FBackgroundColor;
  FRenderOrder := AMapData.FRenderOrder;
  FBaseUrl := AMapData.FBaseUrl;
end;

constructor TCastleTiledMapData.Create(const Stream: TStream; const ABaseUrl: String);
begin
  inherited Create;

  CreateLists;
  FBaseUrl := ABaseUrl;

  //Load TMX
  LoadTMXFile(Stream, ABaseUrl);
end;

constructor TCastleTiledMapData.Create(const AURL: string);
var
  StreamOptions: TStreamOptions;
  Gzipped: boolean;
  Stream: TStream;
begin
  // calculate Stream from AURL, automatically gunzip if extension says it
  StreamOptions := [];
  URIMimeType(AURL, Gzipped);
  if Gzipped then
    Include(StreamOptions, soGzip);
  Stream := Download(AURL, StreamOptions);
  try
    Create(Stream, AURL);
  finally FreeAndNil(Stream) end;
end;

constructor TCastleTiledMapData.Create;
begin
  inherited Create;
  CreateLists;
end;

destructor TCastleTiledMapData.Destroy;
begin
  FreeAndNil(FTilesets);
  FreeAndNil(FProperties);
  FreeAndNil(FLayers);
  inherited Destroy;
end;

function TCastleTiledMapData.TilePositionValid(const TilePosition: TVector2Integer): Boolean;
begin
  Result :=
    (TilePosition.X >= 0) and
    (TilePosition.X < Width) and
    (TilePosition.Y >= 0) and
    (TilePosition.Y < Height);
end;

function TCastleTiledMapData.PositionToTile(const Position: TVector2;
  out TilePosition: TVector2Integer): Boolean;
var
  X, Y, ResultYPlusX, ResultYMinusX: Single;
  RowIncreaseY: Single;
begin
  { unpack vector, for simpler code and for speed }
  X := Position.X;
  Y := Position.Y;

  case Orientation of
    moIsometric:
      begin
        Y := Y - (Width - 1) * TileHeight / 2;
        ResultYPlusX  := X / (TileWidth  / 2);
        ResultYMinusX := Y / (TileHeight / 2) - 1; // not sure why this -1 at end is needed...
        TilePosition.X := Floor((ResultYPlusX - ResultYMinusX) / 2);
        TilePosition.Y := Floor((ResultYPlusX + ResultYMinusX) / 2);
      end;
    moIsometricStaggered:
      begin
        // TODO: right now this assumes Stagger Axis = Y
        { TODO: This doesn't have smart logic to account for diagonals.
          To hide this fact, we do "- 0.25" below, to at least match correct tile
          when we're over it's center. }
        TilePosition.Y := Floor(Y / (TileHeight / 2) - 0.25);
        if (not Odd(TilePosition.Y)) xor (StaggerIndex <> siOdd) then
          TilePosition.X := Floor(X / TileWidth - 0.5)
        else
          TilePosition.X := Floor(X / TileWidth);
      end;
    moHexagonal:
      begin
        // TODO: right now this assumes Stagger Axis = Y
        { TODO: This doesn't have smart logic to detect exact tile under mouse
          in case position is between diagonals.
          As such, this works sensibly only for large HexSideLength,
          when TileHeight - HexSideLength is small. }
        RowIncreaseY := TileHeight - (TileHeight - HexSideLength) / 2;
        TilePosition.Y := Floor(Y / RowIncreaseY);
        if (not Odd(TilePosition.Y)) xor (StaggerIndex <> siOdd) then
          TilePosition.X := Floor(X / TileWidth - 0.5)
        else
          TilePosition.X := Floor(X / TileWidth);
      end;
    // As a fallback, unsupported modes are as orthogonal
    else
      begin
        TilePosition.X := Floor(X / TileWidth);
        TilePosition.Y := Floor(Y / TileHeight);
      end;
  end;
  Result := TilePositionValid(TilePosition);
end;

function TCastleTiledMapData.TilePositionToIndex(
  const ATilePosition: TVector2Integer): integer;
begin
  Result := ATilePosition.X + (Height - 1 - ATilePosition.Y) * Width;
end;

function TCastleTiledMapData.TileRenderPosition(const TilePosition: TVector2Integer): TVector2;
var
  X, Y: Integer;
  RowIncreaseY: Single;
begin
  { unpack vector, for simpler code and for speed }
  X := TilePosition.X;
  Y := TilePosition.Y;

  case Orientation of
    moIsometric:
      begin
        { At the beginning imagine a simpler equation:

            Result.X := (X + Y) * TileWidth  / 2;
            Result.Y := (Y - X) * TileHeight / 2;

          The Y position of the bottom-most tile, at (Width - 1, 0),
          would be -(Width - 1) * TileHeight / 2.
          So adjust Result.Y to place the map always at positive Y. }

        Result.X := (            X + Y) * TileWidth  / 2;
        Result.Y := (Width - 1 + Y - X) * TileHeight / 2;
      end;
    moIsometricStaggered:
      begin
        // TODO: right now this assumes Stagger Axis = Y
        Result.X := X * TileWidth;
        if (not Odd(Y)) xor (StaggerIndex <> siOdd) then
          Result.X := Result.X + TileWidth / 2;
        Result.Y := Y * TileHeight / 2;
      end;
    moHexagonal:
      begin
        // TODO: right now this assumes Stagger Axis = Y
        RowIncreaseY := TileHeight - (TileHeight - HexSideLength) / 2;
        Result.X := X * TileWidth;
        if (not Odd(Y)) xor (StaggerIndex <> siOdd) then
          Result.X := Result.X + TileWidth / 2;
        Result.Y := Y * RowIncreaseY;
      end;
    // As a fallback, unsupported modes are as orthogonal
    else
      begin
        Result.X := X * TileWidth;
        Result.Y := Y * TileHeight;
      end;
  end;
end;

function TCastleTiledMapData.TileRenderDataSimple(
  const TilePosition: TVector2Integer; const Layer: TLayer; out
  Tileset: TTileset; out Frame: Integer; out Data: Cardinal): Boolean;

  { Returns the tileset that contains the global ID. }
  function GIDToTileset(const AGID: Cardinal): TTileSet;
  var
    i: Integer;
  begin
    for i := 0 to FTilesets.Count - 1 do
      if FTilesets.Items[i].FirstGID > AGID then
      begin
        Result := FTilesets[i-1];
        Exit;
      end;
    Result := FTilesets[FTilesets.Count - 1];
  end;

const
  ClearFlag      = $1FFFFFFF;
var
  Index: Integer;
  GID: Cardinal;
begin
  Result := false;

  Index := TilePositionToIndex(TilePosition);
  Data := Layer.Data.Data[Index];
  GID := Data and ClearFlag;
  if GID = 0 then Exit;

  Tileset := GIDToTileset(GID);
  Frame := GID - Tileset.FirstGID;

  Result := true;
end;

function TCastleTiledMapData.TileRenderData(const TilePosition: TVector2Integer;
  const Layer: TLayer;
  out Tileset: TTileset;
  out Frame: Integer;
  out HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean): Boolean;

const
  HorizontalFlag = $80000000;
  VerticalFlag   = $40000000;
  DiagonalFlag   = $20000000;
var
  Dat: Cardinal;
begin
  Result := TileRenderDataSimple(TilePosition, Layer, Tileset, Frame, Dat);
  if not Result then Exit;

  HorizontalFlip := Dat and HorizontalFlag > 0;
  VerticalFlip := Dat and VerticalFlag > 0;
  DiagonalFlip := Dat and DiagonalFlag > 0;
end;

function TCastleTiledMapData.TileNeighbor(const Tile1, Tile2: TVector2Integer;
  const CornersAreNeighbors: Boolean): Boolean;
var
  XDiff, YDiff: Integer;
begin
  // eliminate easy cases first
  if (not TilePositionValid(Tile1)) or
     (not TilePositionValid(Tile2)) or
     TVector2Integer.Equals(Tile1, Tile2) then
    Exit(false);

  XDiff := Tile1.X - Tile2.X;
  YDiff := Tile1.Y - Tile2.Y;

  case Orientation of
    moIsometricStaggered:
      begin
        // first check for neighbors touching edges (not only corners)
        Result := Between(XDiff, -1, 1) and ((YDiff = -1) or (YDiff = 1));

        if Result then
        begin
          // TODO: right now this assumes Stagger Axis = Y
          { The above condition allowed 6 tiles to be neighbors to Tile2.
            We need to eliminate 2 cases now.
            Which 2 cases to eliminate depends on whether we're on odd or even row. }
          if (not Odd(Tile2.Y)) xor (StaggerIndex <> siOdd) then
          begin
            if (XDiff = -1) and ((YDiff = -1) or (YDiff = 1)) then
              Result := false;
          end else
          begin
            if (XDiff = 1) and ((YDiff = -1) or (YDiff = 1)) then
              Result := false;
          end;
        end;

        // check for neighbors touching corners
        if CornersAreNeighbors then
          Result := Result or
            ((YDiff = 0) and ((XDiff = -1) or (XDiff = 1))) or
            ((XDiff = 0) and ((YDiff = -2) or (YDiff = 2)));
      end;
    moHexagonal:
      begin
        Result := Between(XDiff, -1, 1) and Between(YDiff, -1, 1);
        if Result then
        begin
          // TODO: right now this assumes Stagger Axis = Y
          { The above condition allowed 8 tiles to be neighbors to Tile2.
            We need to eliminate 2 cases now, since there are only 6 neighbors
            on hexagonal map. Which 2 cases to eliminate depends on whether
            we're on odd or even row. }
          if (not Odd(Tile2.Y)) xor (StaggerIndex <> siOdd) then
          begin
            if (XDiff = -1) and ((YDiff = -1) or (YDiff = 1)) then
              Result := false;
          end else
          begin
            if (XDiff = 1) and ((YDiff = -1) or (YDiff = 1)) then
              Result := false;
          end;
        end;
      end;
    { As a fallback, unsupported modes are as moOrthogonal.
      This logic matches also moIsometric. }
    else
      begin
        if CornersAreNeighbors then
          Result := Between(XDiff, -1, 1) and Between(YDiff, -1, 1)
        else
          Result :=
            ( (XDiff = 0) and ((YDiff = -1) or (YDiff = 1)) ) or
            ( (YDiff = 0) and ((XDiff = -1) or (XDiff = 1)) );
      end;
  end;
end;

procedure TCastleTiledMapData.Assign(Source: TPersistent);
var
  AMapData: TCastleTiledMapData;
begin
  if (Source is TCastleTiledMapData) then
  begin
    AMapData := TCastleTiledMapData(Source);

    AssignVars(AMapData);
    FTilesets.Assign(AMapData.Tilesets);
    FProperties.Assign(AMapData.Properties);
    FLayers.Assign(AMapData.Layers);
  end else inherited;
end;

{$endif read_implementation}
