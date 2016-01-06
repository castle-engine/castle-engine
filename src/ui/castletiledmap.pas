{
  Copyright 2015, 2016 Tomasz Wojtyś

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TMX files processing unit. }
unit CastleTiledMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, base64, zstream, {zlib,} CastleGenericLists, CastleVectors,
  CastleColors, CastleUtils, CastleURIUtils, CastleXMLUtils, CastleLog;

type
  TProperty = record
    { The name of the property. }
    Name: string;
    { The value of the property. }
    Value: string;
  end;

  { List of properties. }
  TProperties = specialize TGenericStructList<TProperty>;

  TEncodingType = (ET_None, ET_Base64, ET_CSV);
  TCompressionType = (CT_None, CT_GZip, CT_ZLib);

  { Binary data definition. }
  TData = record //todo: is encoded and compressed really necessary to keep?
    { The encoding used to encode the tile layer data. When used, it can be
      "base64" and "csv" at the moment. }
    Encoding: TEncodingType;
    { The compression used to compress the tile layer data. Tiled Qt supports
      "gzip" and "zlib". }
    Compression: TCompressionType;
    { Binary data. Uncompressed and decoded. }
    Data: array of Cardinal;
    Tiles: array of Integer;
  end;

  { Image definition. }
  TImage = record
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
    //todo: data
  end;

  //PTileset = ^TTileset;
  { Tileset definition. }
  TTileset = record
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
    { This element is used to specify an offset in pixels, to be applied when
      drawing a tile from the related tileset. When not present, no offset is applied. }
    TileOffset: TVector2Integer;
    Properties: TProperties;
    Image: TImage;
    // todo: Tile
    // todo: TerrainTypes
  end;

  { List of tilesets. }
  TTilesets = specialize TGenericStructList<TTileset>;

  TObjectsDrawOrder = (ODO_Index, ODO_TopDown);

  { Object group definition. Moved to TLayer. }
  {TObjectGroup = record
    { The name of the object group. }
    Name: string;
    { The color used to display the objects in this group. }
    Color: TCastleColorRGB;
    { The x coordinate of the object group in tiles. Defaults to 0 and can no longer be changed in Tiled Qt. }
    X: Integer;
    { The y coordinate of the object group in tiles. Defaults to 0 and can no longer be changed in Tiled Qt. }
    Y: Integer;
    { The width of the object group in tiles. Meaningless. }
    Width: Integer;
    { The height of the object group in tiles. Meaningless. }
    Height: Integer;
    { The opacity of the layer as a value from 0 to 1. Defaults to 1. }
    Opacity: Single;
    { Whether the layer is shown (1) or hidden (0). Defaults to 1. }
    Visible: Boolean;
    { Rendering offset for this object group in pixels. Defaults to 0. (since 0.14) }
    OffsetX: Integer;
    { Rendering offset for this object group in pixels. Defaults to 0. (since 0.14) }
    OffsetY: Integer;
    { Whether the objects are drawn according to the order of appearance
      ("index") or sorted by their y-coordinate ("topdown"). Defaults to "topdown". }
    DrawOrder: TObjectsDrawOrder;
    Objects: TTiledObjects;
    Properties: TProperties;
  end;}

  TTileObjectPrimitive = (TOP_Ellipse, TOP_Poligon, TOP_PolyLine);

  { Object definition. }
  TTiledObject = record
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
    Properties: TProperties;
    { List of points for poligon and poliline. }
    Points: TVector2SingleList;
    Primitive: TTileObjectPrimitive;
    Image: TImage;
  end;

  TTiledObjects = specialize TGenericStructList<TTiledObject>;
  TLayerType = (LT_Layer, LT_ObjectGroup, LT_ImageLayer);

  { Layer definition. Internally we treat "object group" as normal layer. }
  TLayer = record
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
    Properties: TProperties;
    Data: TData;
    { If True then the layer will be treated as object group instead of normal layer. }
    IsLayerAnObjectGroup: Boolean;
    { The color used to display the objects in this group. }
    Color: TCastleColorRGB;
    { The x coordinate of the object group in tiles. Defaults to 0 and can no longer be changed in Tiled Qt. }
    X: Integer;
    { The y coordinate of the object group in tiles. Defaults to 0 and can no longer be changed in Tiled Qt. }
    Y: Integer;
    { The width of the object group in tiles. Meaningless. }
    Width: Integer;
    { The height of the object group in tiles. Meaningless. }
    Height: Integer;
    { Whether the objects are drawn according to the order of appearance
      ("index") or sorted by their y-coordinate ("topdown"). Defaults to "topdown". }
    DrawOrder: TObjectsDrawOrder;
    Objects: TTiledObjects;
    LayerType: TLayerType;
    { Used by ImageLayer. }
    Image: TImage;
  end;

  { List of layers. }
  TLayers = specialize TGenericStructList<TLayer>;

  TMapOrientation = (MO_Orthogonal, MO_Isometric, MO_Staggered);
  TMapRenderOrder = (MRO_RightDown, MRO_RightUp, MRO_LeftDown, MRO_LeftUp);

  { Loading and manipulating "Tiled" map files (http://mapeditor.org).
    Based on Tiled version 0.14. }
  TCastleTiledMap = class
  private
    { Map stuff. }
    { The TMX format version, generally 1.0. }
    FVersion: string; //todo: change to set?
    { Map orientation. Tiled supports "orthogonal", "isometric" and "staggered"
      (since 0.9) at the moment. }
    FOrientation: TMapOrientation;
    { The map width in tiles. }
    FWidth: Cardinal;
    { The map height in tiles. }
    FHeight: Cardinal;
    { The width of a tile. }
    FTileWidth: Cardinal;
    { The height of a tile. }
    FTileHeight: Cardinal;
    { The background color of the map. (since 0.9, optional) }
    FBackgroundColor: TCastleColorRGB;
    { The order in which tiles on tile layers are rendered. Valid values are
      right-down (the default), right-up, left-down and left-up. In all cases,
      the map is drawn row-by-row. (since 0.10, but only supported for orthogonal
      maps at the moment) }
    FRenderOrder: TMapRenderOrder;

    procedure LoadTileset(Element: TDOMElement);
    procedure LoadProperty(Element: TDOMElement; var AProperty: TProperty);
    procedure LoadProperties(Element: TDOMElement; var AProperties: TProperties);
    procedure LoadImage(Element: TDOMElement; var AImage: TImage);
    procedure LoadLayer(Element: TDOMElement);
    procedure LoadObjectGroup(Element: TDOMElement);
    procedure LoadTiledObject(Element: TDOMElement; var ATiledObject: TTiledObject);
    procedure LoadImageLayer(Element: TDOMElement);
    procedure LoadData(Element: TDOMElement; var AData: TData);
  private
    FTilesets: TTilesets;
    FProperties: TProperties;
    FLayers: TLayers;
    procedure LoadTMXFile(AURL: string);
  public
    { @param(AURL) - URL to TMX file. }
    constructor Create(AURL: string);
    destructor Destroy; override;
  end;

implementation

procedure TCastleTiledMap.LoadTileset(Element: TDOMElement);
var
  I: TXMLElementIterator;
  NewTileset: TTileset;
  TmpStr: string;
begin
  with NewTileset do
  begin
    TileOffset := ZeroVector2Integer;
    Properties := nil;
    if Element.AttributeString('firstgid', TmpStr) then
      FirstGID := StrToInt(TmpStr);
    if Element.AttributeString('source', TmpStr) then
      Source := TmpStr;
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
      TileCount := StrToInt(TmpStr);
    WritelnLog('LoadTileset firstgid', IntToStr(FirstGID));
    WritelnLog('LoadTileset source', Source);
    WritelnLog('LoadTileset Name', Name);
    WritelnLog('LoadTileset TileWidth', IntToStr(TileWidth));
    WritelnLog('LoadTileset TileHeight', IntToStr(TileHeight));
    WritelnLog('LoadTileset Spacing', IntToStr(Spacing));
    WritelnLog('LoadTileset Margin', IntToStr(Margin));
    WritelnLog('LoadTileset TileCount', IntToStr(TileCount));

    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        WritelnLog('LoadTileset element', I.Current.TagName);
        case LowerCase(I.Current.TagName) of
          'tileoffset': begin
            TileOffset[0] := StrToInt(I.Current.GetAttribute('x'));
            TileOffset[1] := StrToInt(I.Current.GetAttribute('y'));
          end;
          'properties': LoadProperties(I.Current, Properties);
          'image': LoadImage(I.Current, Image);
        end;
      end;
    finally FreeAndNil(I) end;
  end;

  FTilesets.Add(NewTileset);
end;

procedure TCastleTiledMap.LoadProperty(Element: TDOMElement;
  var AProperty: TProperty);
begin
  AProperty.Name := Element.GetAttribute('name');
  AProperty.Value := Element.GetAttribute('value');
  WritelnLog('LoadProperty name', AProperty.Name);
  WritelnLog('LoadProperty value', AProperty.Value);
end;

procedure TCastleTiledMap.LoadProperties(Element: TDOMElement;
  var AProperties: TProperties);
var
  I: TXMLElementIterator;
  NewProperty: TProperty;
begin
  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      WritelnLog('LoadProperties element', I.Current.TagName);
      case LowerCase(I.Current.TagName) of
        'property': LoadProperty(I.Current, NewProperty);
      end;
    end;
    if not Assigned (AProperties) then
      AProperties := TProperties.Create;
    AProperties.Add(NewProperty);
  finally FreeAndNil(I) end;
end;

procedure TCastleTiledMap.LoadImage(Element: TDOMElement; var AImage: TImage);
begin
  with AImage do
  begin
    Format := Element.GetAttribute('format');
    Source := Element.GetAttribute('source');
    if Element.hasAttribute('trans') then //todo: if no trans then use some default trans
      Trans := HexToColorRGB(Element.GetAttribute('trans')); //todo: test convertion
    Width := StrToInt(Element.GetAttribute('width'));
    Height := StrToInt(Element.GetAttribute('height'));
    WritelnLog('LoadImage Format', Format);
    WritelnLog('LoadImage Source', Source);
    //WritelnLog('LoadImage Trans', ColorRGBToHex(Trans));//todo: ERangeError sometimes
    WritelnLog('LoadImage Width', IntToStr(Width));
    WritelnLog('LoadImage Height', IntToStr(Height));
  end;
  //todo: loading data element
end;

procedure TCastleTiledMap.LoadLayer(Element: TDOMElement);
var
  I: TXMLElementIterator;
  NewLayer: TLayer;
  TmpStr: string;
begin
  with NewLayer do
  begin
    Properties := nil;
    Opacity := 1;
    Visible := True;
    OffsetX := 0;
    OffsetY := 0;
    LayerType := LT_Layer;
    Name := Element.GetAttribute('name');
    if Element.AttributeString('opacity', TmpStr) then
      Opacity := StrToFloat(TmpStr);
    if Element.GetAttribute('visible') = '0' then
      Visible := False;
    if Element.AttributeString('offsetx', TmpStr) then
      OffsetX := StrToInt(TmpStr);
    if Element.AttributeString('offsety', TmpStr) then
      OffsetY := StrToInt(TmpStr);
    WritelnLog('LoadTileset Name', Name);
    WritelnLog('LoadTileset Visible', BoolToStr(Visible, 'True', 'False'));
    WritelnLog('LoadTileset Opacity', FloatToStr(Opacity));
    WritelnLog('LoadTileset OffsetX', IntToStr(OffsetX));
    WritelnLog('LoadTileset OffsetY', IntToStr(OffsetY));

    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        WritelnLog('LoadLayer element', I.Current.TagName);
        case LowerCase(I.Current.TagName) of
          'properties': LoadProperties(I.Current, Properties);
          'data': LoadData(I.Current, Data);
        end;
      end;
    finally FreeAndNil(I) end;
  end;

  FLayers.Add(NewLayer);
end;

procedure TCastleTiledMap.LoadObjectGroup(Element: TDOMElement);
var
  I: TXMLElementIterator;
  NewLayer: TLayer;
  NewObject: TTiledObject;
  TmpStr: string;
begin
  with NewLayer do
  begin
    Properties := nil;
    Objects := nil;
    Opacity := 1;
    Visible := True;
    OffsetX := 0;
    OffsetY := 0;
    DrawOrder := ODO_TopDown;
    LayerType := LT_ObjectGroup;
    Name := Element.GetAttribute('name');
    if Element.AttributeString('opacity', TmpStr) then
      Opacity := StrToFloat(TmpStr);
    if Element.GetAttribute('visible') = '0' then
      Visible := False;
    if Element.AttributeString('offsetx', TmpStr) then
      OffsetX := StrToInt(TmpStr);
    if Element.AttributeString('offsety', TmpStr) then
      OffsetY := StrToInt(TmpStr);
    if Element.AttributeString('draworder', TmpStr) then
      case TmpStr of
        'index': DrawOrder := ODO_Index;
        'topdown': DrawOrder := ODO_TopDown;
      end;
    WritelnLog('LoadTileset Name', Name);
    WritelnLog('LoadTileset Visible', BoolToStr(Visible, 'True', 'False'));
    WritelnLog('LoadTileset Opacity', FloatToStr(Opacity));
    WritelnLog('LoadTileset OffsetX', IntToStr(OffsetX));
    WritelnLog('LoadTileset OffsetY', IntToStr(OffsetY));
  end;

  I := TXMLElementIterator.Create(Element);
  try
    while I.GetNext do
    begin
      WritelnLog('LoadObjectGroup element', I.Current.TagName);
      case LowerCase(I.Current.TagName) of
        'properties': LoadProperties(I.Current, NewLayer.Properties);
        'object': begin
          LoadTiledObject(I.Current, NewObject);
          if not Assigned(NewLayer.Objects) then
            NewLayer.Objects := TTiledObjects.Create;
          NewLayer.Objects.Add(NewObject);
        end;
      end;
    end;
  finally FreeAndNil(I) end;

  FLayers.Add(NewLayer);
end;

procedure TCastleTiledMap.LoadTiledObject(Element: TDOMElement;
  var ATiledObject: TTiledObject);
var
  I: TXMLElementIterator;
  TmpStr: string;
begin
  with ATiledObject do
  begin
    Width := 0;
    Height := 0;
    Rotation := 0;
    Visible := True;
    Properties := nil;
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
        WritelnLog('LoadTiledObject element', I.Current.TagName);
        case LowerCase(I.Current.TagName) of
          'properties': LoadProperties(I.Current, Properties);
          'ellipse': Primitive := TOP_Ellipse;
          'polygon': Primitive := TOP_Poligon;//todo: points
          'polyline': Primitive := TOP_PolyLine;//todo: points
          'image': LoadImage(I.Current, Image);
        end;
      end;
    finally FreeAndNil(I) end;
  end;
end;

procedure TCastleTiledMap.LoadImageLayer(Element: TDOMElement);
var
  I: TXMLElementIterator;
  NewLayer: TLayer;
  TmpStr: string;
begin
  with NewLayer do
  begin
    Properties := nil;
    Opacity := 1;
    Visible := True;
    LayerType := LT_ImageLayer;
    Name := Element.GetAttribute('name');
    if Element.AttributeString('opacity', TmpStr) then
      Opacity := StrToFloat(TmpStr);
    if Element.GetAttribute('visible') = '0' then
      Visible := False;
    if Element.AttributeString('x', TmpStr) then
      X := StrToInt(TmpStr);
    if Element.AttributeString('y', TmpStr) then
      Y := StrToInt(TmpStr);
    WritelnLog('LoadTileset Name', Name);
    WritelnLog('LoadTileset Visible', BoolToStr(Visible, 'True', 'False'));
    WritelnLog('LoadTileset Opacity', FloatToStr(Opacity));
    WritelnLog('LoadTileset X', IntToStr(X));
    WritelnLog('LoadTileset Y', IntToStr(Y));

    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        WritelnLog('LoadImageLayer element', I.Current.TagName);
        case LowerCase(I.Current.TagName) of
          'properties': LoadProperties(I.Current, NewLayer.Properties);
          'image': LoadImage(I.Current, Image);
        end;
      end;
    finally FreeAndNil(I) end;
  end;

  FLayers.Add(NewLayer);
end;

procedure TCastleTiledMap.LoadData(Element: TDOMElement; var AData: TData);
const
  BufferSize = 16;
var
  I: TXMLElementIterator;
  TmpStr, RawData{, DecodedData}: string;
  Decompressor, Decoder: TStream;
  Buffer: array[0..BufferSize-1] of Cardinal;
  DataCount, DataLength: Longint;
begin
  with AData do
  begin
    Encoding := ET_None;
    Compression := CT_None;
    if Element.AttributeString('encoding', TmpStr) then
      case TmpStr of
        'base64': Encoding := ET_Base64;
        'csv': Encoding := ET_CSV;
      end;
    if Element.AttributeString('compression', TmpStr) then
      case TmpStr of
        'gzip': Compression := CT_Gzip;
        'zlib': Compression := CT_ZLib;
      end;

    if (Encoding = ET_None) or (Compression = CT_None) then
    begin
      // todo: use XML tiles
    end else begin
      RawData := Element.TextContent;
      WritelnLog('LoadData RawData', RawData);
      case Encoding of
        ET_Base64: begin
          Decoder := TBase64DecodingStream.Create(TStringStream.Create(RawData));
        end;
        ET_CSV: ; //todo: csv reading
      end;
      case Compression of
        CT_Gzip: WritelnLog('LoadData', 'Gzip format not implemented'); //todo: gzip reading
        CT_ZLib: begin
          try
            Decompressor := TDecompressionStream.Create(Decoder);
            repeat
              DataCount := Decompressor.Read(Buffer, BufferSize * SizeOf(Cardinal));
              DataLength := Length(Data);
              SetLength(Data, DataLength+(DataCount div SizeOf(Cardinal)));
              if DataCount > 0 then // becouse if DataCount=0 then ERangeCheck error
                Move(Buffer, Data[DataLength], DataCount);
            until DataCount < SizeOf(Buffer);
          finally
            Decompressor.Free;
          end;
        end;
      end;

      //todo: tile flipping
    end;

    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        WritelnLog('LoadData element', I.Current.TagName);
        case LowerCase(I.Current.TagName) of
          'tile': begin
            SetLength(Tiles, Length(Tiles)+1);
            Tiles[High(Tiles)] := StrToInt(I.Current.GetAttribute('gid'));
          end;
        end;
      end;
    finally FreeAndNil(I) end;
  end;
end;

procedure TCastleTiledMap.LoadTMXFile(AURL: string);
var
  Doc: TXMLDocument;
  TmpStr: string;
  I: TXMLElementIterator;
begin
  Doc := nil;
  try
    ReadXMLFile(Doc, URIDeleteProtocol(AURL));  //todo: check AbsoluteURI

    //Parse map attributes
    Check(LowerCase(Doc.DocumentElement.TagName) = 'map',
      'Root element of TMX file must be <map>');
    if Doc.DocumentElement.AttributeString('version', TmpStr) then
      FVersion := TmpStr;
    if Doc.DocumentElement.AttributeString('orientation', TmpStr) then
      case TmpStr of
        'orthogonal': FOrientation := MO_Orthogonal;
        'isometric': FOrientation := MO_Isometric;
        'staggered': FOrientation := MO_Staggered;
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
        'right-down': FRenderOrder := MRO_RightDown;
        'right-up': FRenderOrder := MRO_RightUp;
        'left-down': FRenderOrder := MRO_LeftDown;
        'left-up': FRenderOrder := MRO_LeftUp;
      end;
    //Parse map childrens
    I := TXMLElementIterator.Create(Doc.DocumentElement);
    try
      while I.GetNext do
      begin
        WritelnLog('LoadTMXFile element', I.Current.TagName);
        case LowerCase(I.Current.TagName) of
          'tileset': LoadTileset(I.Current);
          'layer': LoadLayer(I.Current);
          'objectgroup': LoadObjectGroup(I.Current);
          'imagelayer': LoadImageLayer(I.Current);
          'properties': LoadProperties(I.Current, FProperties);
        end;
      end;
    finally FreeAndNil(I) end;
  finally
    FreeAndNil(Doc);
  end;
end;

constructor TCastleTiledMap.Create(AURL: string);
begin
  FTilesets := TTilesets.Create;
  FProperties := TProperties.Create;
  FLayers := TLayers.Create;

  //Load TMX
  LoadTMXFile(AURL);
end;

destructor TCastleTiledMap.Destroy;
begin
  FreeAndNil(FTilesets);
  FreeAndNil(FProperties);
  FreeAndNil(FLayers);
  inherited Destroy;
end;


end.

