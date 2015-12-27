{
  Copyright 2015 Tomasz Wojtyś

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
  Classes, SysUtils, CastleGenericLists, CastleVectors, CastleColors;

type
  TProperty = record
    { The name of the property. }
    Name: string;
    { The value of the property. }
    Value: string;
  end;

  { List of properties. }
  TProperties = specialize TGenericStructList<TProperty>;

  TEncodingType = (ET_Base64, ET_CSV);
  TCompressionType = (CT_GZip, ZLib);

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
    // todo: Tile
  end;

  PImage = ^TImage;
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
    Image: PImage;
    // todo: Tile
    // todo: TerrainTypes
  end;

  { List of tilesets. }
  TTilesets = specialize TGenericStructList<TTileset>;

  //PLayer = ^TLayer;
  { Layer definition. }
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
  end;

  { List of layers. }
  TLayers = specialize TGenericStructList<TLayer>;

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
    X: Integer;
    { The y coordinate of the object in pixels. }
    Y: Integer;
    { The width of the object in pixels (defaults to 0). }
    Width: Integer;
    { The height of the object in pixels (defaults to 0). }
    Height: Integer;
    { The rotation of the object in degrees clockwise (defaults to 0). (since 0.10) }
    Rotation: Single;
    { An reference to a tile (optional). }
    GId: Integer;
    { Whether the object is shown (1) or hidden (0). Defaults to 1. (since 0.9) }
    Visible: Boolean;
    Properties: TProperties;
    { List of points for poligon and poliline. }
    Points: TVector2IntegerList;
    Primitive: TTileObjectPrimitive;
    // todo: image
  end;

  TTiledObjects = specialize TGenericStructList<TTiledObject>;

  TObjectsDrawOrder = (ODO_Index, ODO_TopDown);

  { Object group definition. }
  TObjectGroup = record
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
  end;

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

procedure TCastleTiledMap.LoadTMXFile(AURL: string);
begin

end;

constructor TCastleTiledMap.Create(AURL: string);
begin
  FTilesets := TTilesets.Create;
  FProperties := TProperties.Create;
  FLayers := TLayers.Create;

  //Load TMX
  LoadTMXFile(AURL); //try?

  //Parse parameters

  //Create atlas
  //FAtlas := TSprite.Create('URL',frames, cols, rows);
end;

destructor TCastleTiledMap.Destroy;
begin
  FreeAndNil(FTilesets);
  FreeAndNil(FProperties);
  FreeAndNil(FLayers);
  inherited Destroy;
end;


end.

