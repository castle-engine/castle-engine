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

{ Part of CastleControls unit: drawing 2D tiled map (TMX) on screen
  (TCastleTiledMap class). }

{$ifdef read_interface}

type

  { Loading and manipulating "Tiled" map files (http://mapeditor.org).
    Based on Tiled version 0.14. }
  TCastleTiledMap = class(TUIControl)
  type
    TProperty = record
      { The name of the property. }
      Name: string;
      { The value of the property. }
      Value: string;
    end;

    { Binary data definition. }
    TData = record
      { The encoding used to encode the tile layer data. When used, it can be
        "base64" and "csv" at the moment. }
      Encoding: string; //todo: make set type?
      { The compression used to compress the tile layer data. Tiled Qt supports
        "gzip" and "zlib". }
      Compression: string; //todo: make set type?
      { Binary data. }
      Data: array of Cardinal;
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
      Trans: string; //todo: convert to some color format?
      { The image width in pixels (optional, used for tile index correction when
        the image changes). }
      Width: Cardinal;
      { The image height in pixels (optional). }
      Height: Cardinal;
    end;

    PTileset = ^TTileset;
    { Tileset definition. }
    TTileset = record
      { Sprite loaded from parsed XML. }
      Sprite: TSprite;
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
      Image: PImage;
    end;

    { List of tilesets. }
    TTilesets = specialize TFPGList<PTileset>;

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
    end;

  private
    { Map stuff. }
    { The TMX format version, generally 1.0. }
    FVersion: string; //todo: change to set?
    { Map orientation. Tiled supports "orthogonal", "isometric" and "staggered"
      (since 0.9) at the moment. }
    FOrientation: string; //todo: change to set?
    { The map width in tiles. }
    FWidth: Cardinal;
    { The map height in tiles. }
    FHeight: Cardinal;
    { The width of a tile. }
    FTileWidth: Cardinal;
    { The height of a tile. }
    FTileHeight: Cardinal;
    { The background color of the map. (since 0.9, optional) }
    FBackgroundColor: string; //todo: convert to some color format?
    { The order in which tiles on tile layers are rendered. Valid values are
      right-down (the default), right-up, left-down and left-up. In all cases,
      the map is drawn row-by-row. (since 0.10, but only supported for orthogonal
      maps at the moment) }
    FRenderOrder: string; //todo: convert to some color format?
  private
    FTilesets: TTilesets;
    procedure LoadTMXFile(AURL: string);
  public
    { @param(AURL) - URL to TMX file. }
    constructor Create(AOwner: TComponent; AURL: string); reintroduce;
    destructor Destroy; override;
    procedure Render; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TCastleTiledMap.LoadTMXFile(AURL: string);
begin

end;

constructor TCastleTiledMap.Create(AOwner: TComponent; AURL: string);
begin
  inherited Create(AOwner);
  FTilesets := TTilesets.Create;

  //Load TMX
  LoadTMXFile(AURL); //try?

  //Parse parameters

  //Create atlas
  //FAtlas := TSprite.Create('URL',frames, cols, rows);
end;

destructor TCastleTiledMap.Destroy;
begin
  FreeAndNil(FTilesets);
  inherited Destroy;
end;

procedure TCastleTiledMap.Render;
begin
  inherited Render;
end;

{$endif read_implementation}
