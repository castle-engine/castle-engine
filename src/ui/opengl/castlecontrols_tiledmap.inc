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

  { Loading and manipulating "Tiled" map files (http://mapeditor.org). }
  TCastleTiledMap = class(TUIControl)
  type
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
    end;

    { List of tilesets. }
    TTilesets = specialize TFPGList<TTileset>;
  private
    FTilesets: TTilesets;
    procedure LoadTMXFile(AURL: string);
  public
    constructor Create(AURL: string);
    destructor Destroy; override;
    procedure Render; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TCastleTiledMap.LoadTMXFile(AURL: string);
begin

end;

constructor TCastleTiledMap.Create(AURL: string);
begin
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
