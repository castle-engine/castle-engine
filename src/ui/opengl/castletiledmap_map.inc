{
  Copyright 2015-2018 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading and manipulating "Tiled" map files (TTiledMap class). }

{$ifdef read_interface}

type
  { Loading and manipulating "Tiled" map files (http://mapeditor.org). }
  TTiledMap = class
  public
    type
      TProperty = class
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { The name of the property. }
        Name: string;
        { The value of the property. }
        Value: string;
        { The type of the property. Can be string (default), int, float, bool, color
          or file (since 0.16, with color and file added in 0.17). }
        AType: string;
      end;

      { List of properties. }
      TPropertyList = class(specialize TObjectList<TProperty>)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      end;

      TEncodingType = (etNone, etBase64, etCSV);
      TCompressionType = (ctNone, ctGZip, ctZLib);

      { Binary data definition. }
      TData = class
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
        Data: array of Cardinal;
      end;

      { Image definition. }
      TImage = class
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
      end;

      TObjectsDrawOrder = (odoIndex, odoTopDown);

      TTileObjectPrimitive = (topRectangle, topPoint, topEllipse, topPolygon,
        topPolyLine);

      { Object definition. }
      TTiledObject = class
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
        GId: Integer;
        { Whether the object is shown (1) or hidden (0). Defaults to 1. (since 0.9) }
        Visible: Boolean;
        Properties: TPropertyList;
        { List of points for poligon and poliline. }
        Points: TVector2List;
        Primitive: TTileObjectPrimitive;
        Image: TImage;
        constructor Create;
        destructor Destroy; override;
        { X and Y packed in a vector. }
        function Position: TVector2;
      end;

      TTiledObjectList = specialize TObjectList<TTiledObject>;

      TLayer = class
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String); virtual;
      public
        { The name of the layer. }
        Name: string;
        { The opacity of the layer as a value from 0 to 1. Defaults to 1. }
        Opacity: Single;
        { Whether the layer is shown (1) or hidden (0). Defaults to 1. }
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
      end;

      TObjectGroupLayer = class(TLayer)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String); override;
      public
        { Whether the objects are drawn according to the order of appearance
          ("index") or sorted by their y-coordinate ("topdown"). Defaults to "topdown". }
        DrawOrder: TObjectsDrawOrder;
        Objects: TTiledObjectList;
        destructor Destroy; override;
      end;

      TImageLayer = class(TLayer)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String); override;
      public
        { Used by ImageLayer. }
        Image: TImage;
        destructor Destroy; override;
      end;

      { List of layers. }
      TLayerList = specialize TObjectList<TLayer>;

      { Single frame of animation. }
      TFrame = class
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      public
        { The local ID of a tile within the parent tileset. }
        TileId: Cardinal;
        { How long (in milliseconds) this frame should be displayed before advancing
          to the next frame. }
        Duration: Cardinal;
      end;

      { Contains a list of animation frames.
        As of Tiled 0.10, each tile can have exactly one animation associated with it.
        In the future, there could be support for multiple named animations on a tile. }
      TAnimation = class(specialize TObjectList<TFrame>)
      private
        procedure Load(const Element: TDOMElement; const BaseUrl: String);
      end;

      TTile = class
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
        constructor Create;
        destructor Destroy; override;
      end;

      { Tiles list. }
      TTileList = specialize TObjectList<TTile>;

      TTerrain = class
        { The name of the terrain type. }
        Name: string;
        { The local tile-id of the tile that represents the terrain visually. }
        Tile: Cardinal;
        Properties: TPropertyList;
      end;

      { This element defines an array of terrain types, which can be referenced from
        the terrain attribute of the tile element. }
      TTerrainTypes = specialize TObjectList<TTerrain>;

      { Tileset definition. }
      TTileset = class
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
        constructor Create;
        destructor Destroy; override;
      end;

      { List of tilesets. }
      TTilesetList = specialize TObjectList<TTileset>;

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
    BaseUrl: string;
    FTilesets: TTilesetList;
    FProperties: TPropertyList;
    FLayers: TLayerList;
    procedure LoadTMXFile(const AURL: string);
  public
    property Layers: TLayerList read FLayers;
    { Map orientation. }
    property Orientation: TMapOrientation read FOrientation;
    property Properties: TPropertyList read FProperties;
    property Tilesets: TTilesetList read FTilesets;
    { The map width in tiles. }
    property Width: Cardinal read FWidth;
    { The map height in tiles. }
    property Height: Cardinal read FHeight;
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
    constructor Create(const AURL: string);
    destructor Destroy; override;

    { Is the given tile number valid.
      Valid map tiles are from (0, 0) (lower-left) to
      (@link(Width) - 1, @link(Height) - 1) (upper-right). }
    function TilePositionValid(const TilePosition: TVector2Integer): Boolean;

    { Detect tile under given position.

      Similar to @link(TCastleTiledMapControl.PositionToTile).
      Given TilePosition must be in local map coordinates,
      where each map piece has size (TileWidth, TileHeight),
      and map bottom-left corner is in (0, 0).

      This method returns @false if the position is outside of the map.
      Valid map tiles are defined as by @link(TilePositionValid). }
    function PositionToTile(const Position: TVector2;
      out TilePosition: TVector2Integer): Boolean;

    { Left-bottom corner where the given tile should be rendered. }
    function TileRenderPosition(const TilePosition: TVector2Integer): TVector2;

    { Information about which image (and how) should be displayed at given map position. }
    function TileRenderData(const TilePosition: TVector2Integer;
      const Layer: TTiledMap.TLayer;
      out Tileset: TTiledMap.TTileset;
      out Frame: Integer;
      out HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean): Boolean;

    { Are the two given tiles neighbors.
      Takes into account map @link(Orientation), so it works for hexagonal,
      orthogonal etc. maps. }
    function TileNeighbor(const Tile1, Tile2: TVector2Integer;
      const CornersAreNeighbors: Boolean): Boolean;
  end;

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

{ TProperty ------------------------------------------------------------------ }

procedure TTiledMap.TProperty.Load(const Element: TDOMElement; const BaseUrl: String);
begin
  Name := Element.AttributeStringDef('name', '');
  Value := Element.AttributeStringDef('value', '');
  AType := Element.AttributeStringDef('type', '');
end;

{ TPropertyList -------------------------------------------------------------- }

procedure TTiledMap.TPropertyList.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  NewProperty: TProperty;
begin
  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'property':
          begin
            NewProperty := TProperty.Create;
            NewProperty.Load(I.Current, BaseUrl);
            Add(NewProperty);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TData ---------------------------------------------------------------------- }

procedure TTiledMap.TData.Load(const Element: TDOMElement; const BaseUrl: String);
const
  BufferSize = 16;
  CSVDataSeparator = Char(',');
var
  I: TXMLElementIterator;
  TmpStr, RawData: string;
  Decompressor: TStream;
  Decoder: TBase64DecodingStream;
  Buffer: array[0..BufferSize-1] of Cardinal;
  DataCount, DataLength: Longint;
  CSVItem: string;
  tmpChar, p: PChar;
  CSVDataCount: Cardinal;
  UsePlainXML: Boolean;
begin
  UsePlainXML := False;
  Decoder := nil;
  try
    Encoding := etNone;
    Compression := ctNone;
    if Element.AttributeString('encoding', TmpStr) then
      case TmpStr of
        'base64': Encoding := etBase64;
        'csv': Encoding := etCSV;
      end;
    if Element.AttributeString('compression', TmpStr) then
      case TmpStr of
        'gzip': Compression := ctGzip;
        'zlib': Compression := ctZLib;
      end;

    if (Encoding = etNone) and (Compression = ctNone) then
    begin
      UsePlainXML := True;
    end else begin
      RawData := Element.TextData;
      case Encoding of
        etBase64: begin
          Decoder := TBase64DecodingStream.Create(TStringStream.Create(RawData));
          Decoder.SourceOwner := true;
        end;
        etCSV: begin
          // remove EOLs
          RawData := StringReplace(RawData, #10, '', [rfReplaceAll]);
          RawData := StringReplace(RawData, #13, '', [rfReplaceAll]);
          // count data
          CSVDataCount := 0;
          tmpChar := StrScan(PChar(RawData), CSVDataSeparator);
          while tmpChar <> nil do
          begin
            Inc(CSVDataCount);
            tmpChar := StrScan(StrPos(tmpChar, CSVDataSeparator) + 1, CSVDataSeparator);
          end;
          // read data
          SetLength(Data, CSVDataCount + 1);
          p := PChar(RawData);
          DataCount := 0;
          repeat
            tmpChar := StrPos(p, CSVDataSeparator);
            if tmpChar = nil then tmpChar := StrScan(p, #0);
            SetString(CSVItem, p, tmpChar - p);
            { Note that any value in 32-bit unsigned range is OK,
              even values > 32-bit signed range.
              E.g. test_hexagonal_tile_60x60x30.tmx on
              https://github.com/bjorn/tiled/tree/master/examples
              has values like 3221225473 . }
            Data[DataCount] := StrToDWord(CSVItem);
            Inc(DataCount);
            p := tmpChar + 1;
          until tmpChar^ = #0;
        end;
      end;
      case Compression of
        ctGzip: WritelnWarning('TData.Load', 'TODO: Gzip format not implemented');
        ctZLib: begin
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
        ctNone: begin
          // Base64 only
          if Encoding = etBase64 then
            repeat
              DataCount := Decoder.Read(Buffer, BufferSize * SizeOf(Cardinal));
              DataLength := Length(Data);
              SetLength(Data, DataLength+(DataCount div SizeOf(Cardinal)));
              if DataCount > 0 then // because if DataCount=0 then ERangeCheck error
                Move(Buffer, Data[DataLength], DataCount);
            until DataCount < SizeOf(Buffer);
        end;
      end;
    end;

    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        case LowerCase(I.Current.TagName) of
          'tile':
            if UsePlainXML then
            begin
              SetLength(Data, Length(Data)+1);
              Data[High(Data)] := I.Current.AttributeCardinalDef('gid', 0);
            end;
        end;
      end;
    finally FreeAndNil(I) end;
  finally FreeAndNil(Decoder) end;
end;

{ TImage --------------------------------------------------------------------- }

destructor TTiledMap.TImage.Destroy;
begin
  FreeAndNil(Data);
  inherited;
end;

procedure TTiledMap.TImage.Load(const Element: TDOMElement; const BaseUrl: String);
const
  DefaultTrans: TCastleColorRGB = (Data: (1.0, 0.0, 1.0)); {Fuchsia}
var
  I: TXMLElementIterator;
  TmpStr: string;
begin
  if Element.AttributeString('format', TmpStr) then
    Format := TmpStr;
  if Element.AttributeString('source', TmpStr) then
    URL := CombineURI(BaseUrl, TmpStr);
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
      case LowerCase(I.Current.TagName) of
        'data':
          begin
            if Data = nil then
              Data := TData.Create;
            Data.Load(I.Current, BaseUrl);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

procedure TTiledMap.TImage.DetermineSize;
var
 ImageData: TEncodedImage;
begin
  if (Width = 0) and (Height = 0) then
  begin
    WritelnLog('Determining image size by loading it. It is more efficient to store image size in TMX file.');
    ImageData := LoadEncodedImage(URL);
    try
      Width := ImageData.Width;
      Height := ImageData.Height;
    finally FreeAndNil(ImageData) end;
  end;
end;

{ TFrame --------------------------------------------------------------------- }

procedure TTiledMap.TFrame.Load(const Element: TDOMElement; const BaseUrl: String);
var
  TmpStr: string;
begin
  if Element.AttributeString('tileid', TmpStr) then
    TileId := StrToInt(TmpStr);
  if Element.AttributeString('duration', TmpStr) then
    Duration := StrToInt(TmpStr);
end;

{ TAnimation ----------------------------------------------------------------- }

procedure TTiledMap.TAnimation.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  NewFrame: TFrame;
begin
  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'frame':
          begin
            NewFrame := TFrame.Create;
            NewFrame.Load(I.Current, BaseUrl);
            Add(NewFrame);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TTile ------------------------------------------------------------------- }

constructor TTiledMap.TTile.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
  Animation := TAnimation.Create;
  Image := TImage.Create;
end;

destructor TTiledMap.TTile.Destroy;
begin
  FreeAndNil(Properties);
  FreeAndNil(Animation);
  FreeAndNil(Image);
  FreeAndNil(ObjectGroup);
  inherited;
end;

procedure TTiledMap.TTile.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  TmpStr: string;
  SPosition: Integer;
begin
  if Element.AttributeString('id', TmpStr) then
    Id := StrToInt(TmpStr);
  if Element.AttributeString('terrain', TmpStr) then
  begin
    SPosition := 1;
    Terrain[0] := StrToInt(NextToken(TmpStr, SPosition, [',']));
    Terrain[1] := StrToInt(NextToken(TmpStr, SPosition, [',']));
    Terrain[2] := StrToInt(NextToken(TmpStr, SPosition, [',']));
    Terrain[3] := StrToInt(NextToken(TmpStr, SPosition, [',']));
  end;
  if Element.AttributeString('probability', TmpStr) then
    Probability := StrToFloatDot(TmpStr);

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'properties': Properties.Load(I.Current, BaseUrl);
        'image': Image.Load(I.Current, BaseUrl);
        'animation': Animation.Load(I.Current, BaseUrl);
        'objectgroup':
          begin
            if ObjectGroup = nil then
              ObjectGroup := TObjectGroupLayer.Create;
            ObjectGroup.Load(I.Current, BaseUrl);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TTiledObject ------------------------------------------------------------------- }

constructor TTiledMap.TTiledObject.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
end;

destructor TTiledMap.TTiledObject.Destroy;
begin
  FreeAndNil(Properties);
  FreeAndNil(Points);
  inherited;
end;

procedure TTiledMap.TTiledObject.Load(const Element: TDOMElement; const BaseUrl: String);
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
    Result[0] := StrToFloatDot(Token);
    Token := NextToken(S, SeekPos, [',']);
    Result[1] := StrToFloatDot(Token);
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
    GId := StrToInt(TmpStr);
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
      case LowerCase(I.Current.TagName) of
        'properties': Properties.Load(I.Current, BaseUrl);
        'point': Primitive := topPoint;
        'ellipse': Primitive := topEllipse;
        'polygon':
          begin
            Primitive := topPolygon;
            ReadPoints(I.Current.AttributeStringDef('points', ''), Points);
          end;
        'polyline':
          begin
            Primitive := topPolyLine;
            ReadPoints(I.Current.AttributeStringDef('points', ''), Points);
          end;
        'image': Image.Load(I.Current, BaseUrl);
      end;
    end;
  finally FreeAndNil(I) end;
end;

function TTiledMap.TTiledObject.Position: TVector2;
begin
  Result := Vector2(X, Y);
end;

{ TLayer ------------------------------------------------------------------- }

constructor TTiledMap.TLayer.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
end;

destructor TTiledMap.TLayer.Destroy;
begin
  FreeAndNil(Properties);
  FreeAndNil(Data);
  inherited;
end;

procedure TTiledMap.TLayer.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  TmpStr: string;
begin
  Opacity := 1;
  Visible := True;
  OffsetX := 0;
  OffsetY := 0;
  Name := Element.AttributeStringDef('name', '');
  if Element.AttributeString('opacity', TmpStr) then
    Opacity := StrToFloatDot(TmpStr);
  if Element.GetAttribute('visible') = '0' then
    Visible := False;
  OffsetX := Element.AttributeSingleDef('offsetx', 0);
  OffsetY := Element.AttributeSingleDef('offsety', 0);

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'properties': Properties.Load(I.Current, BaseUrl);
        'data':
          begin
            if Data = nil then
              Data := TData.Create;
            Data.Load(I.Current, BaseUrl);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

function TTiledMap.TLayer.Offset: TVector2;
begin
  Result := Vector2(OffsetX, OffsetY);
end;

{ TObjectGroupLayer ---------------------------------------------------------- }

destructor TTiledMap.TObjectGroupLayer.Destroy;
begin
  FreeAndNil(Objects);
  inherited;
end;

procedure TTiledMap.TObjectGroupLayer.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
  NewObject: TTiledObject;
  TmpStr: string;
begin
  inherited;

  DrawOrder := odoTopDown;

  if Element.AttributeString('draworder', TmpStr) then
    case TmpStr of
      'index': DrawOrder := odoIndex;
      'topdown': DrawOrder := odoTopDown;
    end;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'object':
          begin
            NewObject := TTiledObject.Create;
            NewObject.Load(I.Current, BaseUrl);
            if not Assigned(Objects) then
              Objects := TTiledObjectList.Create;
            Objects.Add(NewObject);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TImageLayer ---------------------------------------------------------------- }

destructor TTiledMap.TImageLayer.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure TTiledMap.TImageLayer.Load(const Element: TDOMElement; const BaseUrl: String);
var
  I: TXMLElementIterator;
begin
  inherited;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'image':
          begin
            if Image = nil then
              Image := TImage.Create;
            Image.Load(I.Current, BaseUrl);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TTileset ------------------------------------------------------------------- }

constructor TTiledMap.TTileset.Create;
begin
  inherited;
  Properties := TPropertyList.Create;
  Tiles := TTileList.Create;
  Image := TImage.Create;
end;

destructor TTiledMap.TTileset.Destroy;
begin
  FreeAndNil(Image);
  FreeAndNil(Tiles);
  FreeAndNil(Properties);
  inherited;
end;

procedure TTiledMap.TTileset.Load(const Element: TDOMElement; const BaseUrl: String);

  { TSX file loading. }
  procedure LoadTilesetFromURL;
  var
    Doc: TXMLDocument;
  begin
    Doc := URLReadXML(URL);
    try
      Check(LowerCase(Doc.DocumentElement.TagName) = 'tileset',
        'Root element of TSX file must be <tileset>');
      Load(Doc.DocumentElement, BaseUrl);
    finally
      FreeAndNil(Doc);
    end;
  end;

var
  I: TXMLElementIterator;
  NewTile: TTile;
  TmpStr: string;
begin
  TileOffset := TVector2Integer.Zero;
  Spacing := 0;
  Margin := 0;

  if Element.AttributeString('firstgid', TmpStr) then
    FirstGID := StrToInt(TmpStr)
  else
    FirstGID := 1;
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
      case LowerCase(I.Current.TagName) of
        'tileoffset':
          begin
            TileOffset[0] := I.Current.AttributeIntegerDef('x', 0);
            TileOffset[1] := I.Current.AttributeIntegerDef('y', 0);
          end;
        'properties': Properties.Load(I.Current, BaseUrl);
        'image': Image.Load(I.Current, BaseUrl);
        'tile':
          begin
            NewTile := TTile.Create;
            NewTile.Load(I.Current, BaseUrl);
            Tiles.Add(NewTile);
          end;
      end;
    end;
  finally FreeAndNil(I) end;

  if TileCount = 0 then
  begin
    Image.DetermineSize;
    TileCount := (Image.Width div TileWidth) * (Image.Height div TileHeight);
  end;
end;

{ TTiledMap ------------------------------------------------------------------ }

procedure TTiledMap.LoadTMXFile(const AURL: string);
var
  Doc: TXMLDocument;
  TmpStr: string;
  I: TXMLElementIterator;
  NewLayer: TLayer;
  NewTileset: TTileset;
begin
  Doc := URLReadXML(AURL);
  try
    // Parse map attributes
    Check(LowerCase(Doc.DocumentElement.TagName) = 'map',
      'Root element of TMX file must be <map>');
    if Doc.DocumentElement.AttributeString('version', TmpStr) then
      FVersion := TmpStr;
    if Doc.DocumentElement.AttributeString('orientation', TmpStr) then
      case TmpStr of
        'orthogonal': FOrientation := moOrthogonal;
        'isometric': FOrientation := moIsometric;
        'staggered': FOrientation := moIsometricStaggered;
        'hexagonal': FOrientation := moHexagonal;
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
      case TmpStr of
        'x': FStaggerAxis := saX;
        'y': FStaggerAxis := saY;
        else WritelnWarning('Invalid staggeraxis "%s" in Tiled map file (TMX)', [TmpStr]);
      end;
    if Doc.DocumentElement.AttributeString('staggerindex', TmpStr) then
      case TmpStr of
        'odd': FStaggerIndex := siOdd;
        'even': FStaggerIndex := siEven;
        else WritelnWarning('Invalid staggerindex "%s" in Tiled map file (TMX)', [TmpStr]);
      end;
    if Doc.DocumentElement.AttributeString('backgroundcolor', TmpStr) then
      FBackgroundColor := TiledToColor(TmpStr);
    if Doc.DocumentElement.AttributeString('renderorder', TmpStr) then
      case TmpStr of
        'right-down': FRenderOrder := mroRightDown;
        'right-up': FRenderOrder := mroRightUp;
        'left-down': FRenderOrder := mroLeftDown;
        'left-up': FRenderOrder := mroLeftUp;
      end;
    // Parse map children
    I := TXMLElementIterator.Create(Doc.DocumentElement);
    try
      while I.GetNext do
      begin
        case LowerCase(I.Current.TagName) of
          'tileset':
            begin
              NewTileset := TTileset.Create;
              NewTileset.Load(I.Current, BaseUrl);
              FTilesets.Add(NewTileset);
            end;
          'layer':
            begin
              NewLayer := TLayer.Create;
              NewLayer.Load(I.Current, BaseUrl);
              FLayers.Add(NewLayer);
            end;
          'objectgroup':
            begin
              NewLayer := TObjectGroupLayer.Create;
              NewLayer.Load(I.Current, BaseUrl);
              FLayers.Add(NewLayer);
            end;
          'imagelayer':
            begin
              NewLayer := TImageLayer.Create;
              NewLayer.Load(I.Current, BaseUrl);
              FLayers.Add(NewLayer);
            end;
          'properties': FProperties.Load(I.Current, BaseUrl);
        end;
      end;
    finally FreeAndNil(I) end;
  finally
    FreeAndNil(Doc);
  end;
end;

constructor TTiledMap.Create(const AURL: string);
begin
  FTilesets := TTilesetList.Create;
  FProperties := TPropertyList.Create;
  FLayers := TLayerList.Create(true);
  BaseUrl := AURL;

  //Load TMX
  LoadTMXFile(AURL);
end;

destructor TTiledMap.Destroy;
begin
  FreeAndNil(FTilesets);
  FreeAndNil(FProperties);
  FreeAndNil(FLayers);
  inherited Destroy;
end;

function TTiledMap.TilePositionValid(const TilePosition: TVector2Integer): Boolean;
begin
  Result :=
    (TilePosition.Data[0] >= 0) and
    (TilePosition.Data[0] < Width) and
    (TilePosition.Data[1] >= 0) and
    (TilePosition.Data[1] < Height);
end;

function TTiledMap.PositionToTile(const Position: TVector2;
  out TilePosition: TVector2Integer): Boolean;
var
  X, Y, ResultYPlusX, ResultYMinusX: Single;
  RowIncreaseY: Single;
begin
  { unpack vector, for simpler code and for speed }
  X := Position.Data[0];
  Y := Position.Data[1];

  case Orientation of
    moIsometric:
      begin
        Y -= (Width - 1) * TileHeight / 2;
        ResultYPlusX  := X / (TileWidth  / 2);
        ResultYMinusX := Y / (TileHeight / 2) - 1; // not sure why this -1 at end is needed...
        TilePosition.Data[0] := Floor((ResultYPlusX - ResultYMinusX) / 2);
        TilePosition.Data[1] := Floor((ResultYPlusX + ResultYMinusX) / 2);
      end;
    moIsometricStaggered:
      begin
        // TODO: right now this assumes Stagger Axis = Y
        { TODO: This doesn't have smart logic to account for diagonals.
          To hide this fact, we do "- 0.25" below, to at least match correct tile
          when we're over it's center. }
        TilePosition.Data[1] := Floor(Y / (TileHeight / 2) - 0.25);
        if (not Odd(TilePosition.Data[1])) xor (StaggerIndex <> siOdd) then
          TilePosition.Data[0] := Floor(X / TileWidth - 0.5)
        else
          TilePosition.Data[0] := Floor(X / TileWidth);
      end;
    moHexagonal:
      begin
        // TODO: right now this assumes Stagger Axis = Y
        { TODO: This doesn't have smart logic to detect exact tile under mouse
          in case position is between diagonals.
          As such, this works sensibly only for large HexSideLength,
          when TileHeight - HexSideLength is small. }
        RowIncreaseY := TileHeight - (TileHeight - HexSideLength) / 2;
        TilePosition.Data[1] := Floor(Y / RowIncreaseY);
        if (not Odd(TilePosition.Data[1])) xor (StaggerIndex <> siOdd) then
          TilePosition.Data[0] := Floor(X / TileWidth - 0.5)
        else
          TilePosition.Data[0] := Floor(X / TileWidth);
      end;
    // As a fallback, unsupported modes are as orthogonal
    else
      begin
        TilePosition.Data[0] := Floor(X / TileWidth);
        TilePosition.Data[1] := Floor(Y / TileHeight);
      end;
  end;
  Result := TilePositionValid(TilePosition);
end;

function TTiledMap.TileRenderPosition(const TilePosition: TVector2Integer): TVector2;
var
  X, Y: Integer;
  RowIncreaseY: Single;
begin
  { unpack vector, for simpler code and for speed }
  X := TilePosition.Data[0];
  Y := TilePosition.Data[1];

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

function TTiledMap.TileRenderData(const TilePosition: TVector2Integer;
  const Layer: TTiledMap.TLayer;
  out Tileset: TTiledMap.TTileset;
  out Frame: Integer;
  out HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean): Boolean;

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
  HorizontalFlag = $80000000;
  VerticalFlag   = $40000000;
  DiagonalFlag   = $20000000;
  ClearFlag      = $1FFFFFFF;
var
  Index: Integer;
  GID, Dat: Cardinal;
begin
  Result := false;

  Index := TilePosition.X + (Height - 1 - TilePosition.Y) * Width;
  Dat := Layer.Data.Data[Index];
  GID := Dat and ClearFlag;
  if GID = 0 then Exit;

  Tileset := GIDToTileset(GID);
  Frame := GID - Tileset.FirstGID;
  HorizontalFlip := Dat and HorizontalFlag > 0;
  VerticalFlip := Dat and VerticalFlag > 0;
  DiagonalFlip := Dat and DiagonalFlag > 0;

  Result := true;
end;

function TTiledMap.TileNeighbor(const Tile1, Tile2: TVector2Integer;
  const CornersAreNeighbors: Boolean): Boolean;
var
  XDiff, YDiff: Integer;
begin
  // eliminate easy cases first
  if (not TilePositionValid(Tile1)) or
     (not TilePositionValid(Tile2)) or
     TVector2Integer.Equals(Tile1, Tile2) then
    Exit(false);

  XDiff := Tile1.Data[0] - Tile2.Data[0];
  YDiff := Tile1.Data[1] - Tile2.Data[1];

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

{$endif read_implementation}
