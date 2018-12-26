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

{ TMX files processing unit. Based on Tiled v0.17. }
unit CastleTiledMap;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, DOM, XMLRead, base64, zstream, Generics.Collections,
  CastleVectors, CastleColors, CastleUtils, CastleURIUtils, CastleXMLUtils,
  CastleLog, CastleStringUtils, CastleUIControls, CastleGLImages;

type
  { Loading and manipulating "Tiled" map files (http://mapeditor.org).
    Based on Tiled version 0.14. }
  TTiledMap = class
  public
    type
      TProperty = class
      private
        procedure Load(const Element: TDOMElement);
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
        procedure Load(const Element: TDOMElement);
      end;

      TEncodingType = (etNone, etBase64, etCSV);
      TCompressionType = (ctNone, ctGZip, ctZLib);

      { Binary data definition. }
      TData = class
      private
        procedure Load(const Element: TDOMElement);
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
        procedure Load(const Element: TDOMElement);
      public
        { Used for embedded images, in combination with a data child element.
          Valid values are file extensions like png, gif, jpg, bmp, etc. (since 0.9) }
        Format: string;
        { The reference to the tileset image file (Tiled supports most common
          image formats). }
        Source: string;
        { Defines a specific color that is treated as transparent (example value:
          "#FF00FF" for magenta). Up until Tiled 0.12, this value is written out
          without a # but this is planned to change. }
        Trans: TCastleColorRGB;
        { The image width in pixels (optional, used for tile index correction when
          the image changes). }
        Width: Cardinal;
        { The image height in pixels (optional). }
        Height: Cardinal;
        { Embedded image data (since 0.9). }
        Data: TData;
        destructor Destroy; override;
      end;

      TObjectsDrawOrder = (odoIndex, odoTopDown);

      TTileObjectPrimitive = (topEllipse, topPoligon, topPolyLine);

      { Object definition. }
      TTiledObject = class
      private
        procedure Load(const Element: TDOMElement);
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
      end;

      TTiledObjectList = specialize TObjectList<TTiledObject>;

      TLayer = class
      private
        procedure Load(const Element: TDOMElement); virtual;
      public
        { The name of the layer. }
        Name: string;
        { The opacity of the layer as a value from 0 to 1. Defaults to 1. }
        Opacity: Single;
        { Whether the layer is shown (1) or hidden (0). Defaults to 1. }
        Visible: Boolean;
        { Rendering offset for this layer in pixels. Defaults to 0. (since 0.14). }
        OffsetX: Integer;
        { Rendering offset for this layer in pixels. Defaults to 0. (since 0.14). }
        OffsetY: Integer;
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
      end;

      TObjectGroupLayer = class(TLayer)
      private
        procedure Load(const Element: TDOMElement); override;
      public
        { Whether the objects are drawn according to the order of appearance
          ("index") or sorted by their y-coordinate ("topdown"). Defaults to "topdown". }
        DrawOrder: TObjectsDrawOrder;
        Objects: TTiledObjectList;
        destructor Destroy; override;
      end;

      TImageLayer = class(TLayer)
      private
        procedure Load(const Element: TDOMElement); override;
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
        procedure Load(const Element: TDOMElement);
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
        procedure Load(const Element: TDOMElement);
      end;

      TTile = class
      private
        procedure Load(const Element: TDOMElement);
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
        Source: string;
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
        { Use to render the tileset. Not a part of the file format. }
        ImageData: TSprite;
        constructor Create;
        destructor Destroy; override;
      end;

      { List of tilesets. }
      TTilesetList = specialize TObjectList<TTileset>;

      TMapOrientation = (moOrthogonal, moIsometric, moStaggered);
      TMapRenderOrder = (mroRightDown, mroRightUp, mroLeftDown, mroLeftUp);

  strict private
    { Map stuff. }
    { The TMX format version, generally 1.0. }
    FVersion: string;
    FOrientation: TMapOrientation;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FTileWidth: Cardinal;
    FTileHeight: Cardinal;
    { The background color of the map. (since 0.9, optional) }
    FBackgroundColor: TCastleColorRGB;
    FRenderOrder: TMapRenderOrder;
    BaseUrl: string;
    FTilesets: TTilesetList;
    FProperties: TPropertyList;
    FLayers: TLayerList;
    procedure LoadTMXFile(const AURL: string);
  public
    property Layers: TLayerList read FLayers;
    { Map orientation. Tiled supports "orthogonal", "isometric" and "staggered"
      (since 0.9) at the moment. }
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
    { The order in which tiles on tile layers are rendered. Valid values are
      right-down (the default), right-up, left-down and left-up. In all cases,
      the map is drawn row-by-row. (since 0.10, but only supported for orthogonal
      maps at the moment) }
    property RenderOrder: TMapRenderOrder read FRenderOrder;
    { Constructor.
      @param(AURL URL to Tiled (TMX) file.) }
    constructor Create(const AURL: string);
    destructor Destroy; override;
    { Returns the tileset that contains the global ID. }
    function GIDToTileset(const AGID: Cardinal): TTileSet;
  end;

{$define read_interface}
{$I castletiledmap_control.inc}
{$undef read_interface}

implementation

{ TProperty ------------------------------------------------------------------ }

procedure TTiledMap.TProperty.Load(const Element: TDOMElement);
begin
  Name := Element.AttributeStringDef('name', '');
  Value := Element.AttributeStringDef('value', '');
  AType := Element.AttributeStringDef('type', '');
end;

{ TPropertyList -------------------------------------------------------------- }

procedure TTiledMap.TPropertyList.Load(const Element: TDOMElement);
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
            NewProperty.Load(I.Current);
            Add(NewProperty);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TData ---------------------------------------------------------------------- }

procedure TTiledMap.TData.Load(const Element: TDOMElement);
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
            Data[DataCount] := StrToInt(CSVItem);
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

procedure TTiledMap.TImage.Load(const Element: TDOMElement);
const
  DefaultTrans: TCastleColorRGB = (Data: (1.0, 0.0, 1.0)); {Fuchsia}
var
  I: TXMLElementIterator;
  TmpStr: string;
begin
  if Element.AttributeString('format', TmpStr) then
    Format := TmpStr;
  if Element.AttributeString('source', TmpStr) then
    Source := TmpStr;
  if Element.AttributeString('trans', TmpStr) then
  begin
    if TmpStr[1]='#' then Delete(TmpStr, 1, 1);
    Trans := HexToColorRGB(TmpStr);
  end else
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
            Data.Load(I.Current);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TFrame --------------------------------------------------------------------- }

procedure TTiledMap.TFrame.Load(const Element: TDOMElement);
var
  TmpStr: string;
begin
  if Element.AttributeString('tileid', TmpStr) then
    TileId := StrToInt(TmpStr);
  if Element.AttributeString('duration', TmpStr) then
    Duration := StrToInt(TmpStr);
end;

{ TAnimation ----------------------------------------------------------------- }

procedure TTiledMap.TAnimation.Load(const Element: TDOMElement);
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
            NewFrame.Load(I.Current);
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

procedure TTiledMap.TTile.Load(const Element: TDOMElement);
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
    Probability := StrToFloat(TmpStr);

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'properties': Properties.Load(I.Current);
        'image': Image.Load(I.Current);
        'animation': Animation.Load(I.Current);
        'objectgroup':
          begin
            if ObjectGroup = nil then
              ObjectGroup := TObjectGroupLayer.Create;
            ObjectGroup.Load(I.Current);
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

procedure TTiledMap.TTiledObject.Load(const Element: TDOMElement);
var
  I: TXMLElementIterator;
  TmpStr: string;

  procedure ReadPoints(const PointsString: string; var PointsList: TVector2List);
  const
    PointsSeparator = Char(' ');
    SinglePointSeparator = Char(',');
  var
    tmpChar, p: PChar;
    tmpChar2, p2: PChar;
    tmpPoint, tmpPoint2: string;
    VectorPoint: TVector2;
  begin
    if not Assigned(PointsList) then PointsList := TVector2List.Create;
    p := PChar(PointsString);
    repeat
      tmpChar := StrPos(p, PointsSeparator);
      if tmpChar = nil then tmpChar := StrScan(p, #0);
      SetString(tmpPoint, p, tmpChar - p);
      p2 := PChar(tmpPoint);

      tmpChar2 := StrPos(p2, SinglePointSeparator);
      SetString(tmpPoint2, p2, tmpChar2 - p2);
      VectorPoint[0] := StrToFloat(tmpPoint2);
      p2 := tmpChar2 + 1;
      tmpChar2 := StrScan(p2, #0);
      SetString(tmpPoint2, p2, tmpChar2 - p2);
      VectorPoint[1] := StrToFloat(tmpPoint2);
      PointsList.Add(VectorPoint);

      p := tmpChar + 1;
    until tmpChar^ = #0;
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
    X := StrToFloat(TmpStr);
  if Element.AttributeString('y', TmpStr) then
    Y := StrToFloat(TmpStr);
  if Element.AttributeString('width', TmpStr) then
    Width := StrToFloat(TmpStr);
  if Element.AttributeString('height', TmpStr) then
    Height := StrToFloat(TmpStr);
  if Element.AttributeString('rotation', TmpStr) then
    Rotation := StrToFloat(TmpStr);
  if Element.AttributeString('gid', TmpStr) then
    GId := StrToInt(TmpStr);
  if Element.AttributeString('visible', TmpStr) then
    if TmpStr = '0' then
      Visible := False;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'properties': Properties.Load(I.Current);
        'ellipse': Primitive := topEllipse;
        'polygon':
          begin
            Primitive := topPoligon;
            ReadPoints(I.Current.AttributeStringDef('points', ''), Points);
          end;
        'polyline':
          begin
            Primitive := topPolyLine;
            ReadPoints(I.Current.AttributeStringDef('points', ''), Points);
          end;
        'image': Image.Load(I.Current);
      end;
    end;
  finally FreeAndNil(I) end;
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

procedure TTiledMap.TLayer.Load(const Element: TDOMElement);
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
    Opacity := StrToFloat(TmpStr);
  if Element.GetAttribute('visible') = '0' then
    Visible := False;
  if Element.AttributeString('offsetx', TmpStr) then
    OffsetX := StrToInt(TmpStr);
  if Element.AttributeString('offsety', TmpStr) then
    OffsetY := StrToInt(TmpStr);

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      case LowerCase(I.Current.TagName) of
        'properties': Properties.Load(I.Current);
        'data':
          begin
            if Data = nil then
              Data := TData.Create;
            Data.Load(I.Current);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
end;

{ TObjectGroupLayer ---------------------------------------------------------- }

destructor TTiledMap.TObjectGroupLayer.Destroy;
begin
  FreeAndNil(Objects);
  inherited;
end;

procedure TTiledMap.TObjectGroupLayer.Load(const Element: TDOMElement);
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
            NewObject.Load(I.Current);
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

procedure TTiledMap.TImageLayer.Load(const Element: TDOMElement);
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
            Image.Load(I.Current);
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

procedure TTiledMap.TTileset.Load(const Element: TDOMElement;
  const BaseUrl: String);

  { TSX file loading. }
  procedure LoadTilesetFromFile(const AFileName: string);
  var
    Doc: TXMLDocument;
  begin
    Doc := URLReadXML(CombineURI(BaseUrl, AFileName));
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
    Source := TmpStr;
    LoadTilesetFromFile(Source);
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
        'properties': Properties.Load(I.Current);
        'image': Image.Load(I.Current);
        'tile':
          begin
            NewTile := TTile.Create;
            NewTile.Load(I.Current);
            Tiles.Add(NewTile);
          end;
      end;
    end;
  finally FreeAndNil(I) end;
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
        'staggered': FOrientation := moStaggered;
      end;
    if Doc.DocumentElement.AttributeString('width', TmpStr) then
      FWidth := StrToInt(TmpStr);
    if Doc.DocumentElement.AttributeString('height', TmpStr) then
      FHeight := StrToInt(TmpStr);
    if Doc.DocumentElement.AttributeString('tilewidth', TmpStr) then
      FTileWidth := StrToInt(TmpStr);
    if Doc.DocumentElement.AttributeString('tileheight', TmpStr) then
      FTileHeight := StrToInt(TmpStr);
    if Doc.DocumentElement.AttributeString('backgroundcolor', TmpStr) then
      FBackgroundColor := HexToColorRGB(TmpStr);
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
              NewLayer.Load(I.Current);
              FLayers.Add(NewLayer);
            end;
          'objectgroup':
            begin
              NewLayer := TObjectGroupLayer.Create;
              NewLayer.Load(I.Current);
              FLayers.Add(NewLayer);
            end;
          'imagelayer':
            begin
              NewLayer := TImageLayer.Create;
              NewLayer.Load(I.Current);
              FLayers.Add(NewLayer);
            end;
          'properties': FProperties.Load(I.Current);
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

function TTiledMap.GIDToTileset(const AGID: Cardinal): TTileSet;
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

{$define read_implementation}
{$I castletiledmap_control.inc}
{$undef read_implementation}

end.
