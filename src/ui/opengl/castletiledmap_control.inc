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

{ Display a 2D tiled map (TMX) (TCastleTiledMapControl class). }

{$ifdef read_interface}

type
  { Display a 2D tiled map (TMX). }
  TCastleTiledMapControl = class(TCastleUserInterface)
  strict private
    FTiledMap: TTiledMap;
    FURL: String;
    FObjectsVisible: Boolean;
    procedure RenderOrthogonal;
    procedure RenderHexagonal;
    procedure RenderIsometric;
    procedure RenderObjects;
    procedure LoadTilesetsImages;
    { Free images created by LoadTilesetsImages. }
    procedure FreeTilesetsImages;
    { Free everything created by SetURL. }
    procedure Close;
    procedure SetURL(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;

    { Load given TMX file to memory and creates structures. }
    procedure LoadFromFile(const AURL: string); deprecated 'set URL instead of calling LoadFromFile';
    property ObjectsVisible: Boolean read FObjectsVisible write FObjectsVisible;
  published
    { URL (or just filename) of the Tiled map file (TMX). }
    property URL: String read FURL write SetURL;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TCastleTiledMapControl.RenderOrthogonal;
var
  j, k, l: Integer;
  Lay: TTiledMap.TLayer;
  Tileset: TTiledMap.TTileset;
  Spr: TSprite;
  GID, Dat: Cardinal;
const
  HorizontalFlag = $80000000;
  VerticalFlag   = $40000000;
  DiagonalFlag   = $20000000;
  ClearFlag      = $1FFFFFFF;
begin
  { Layers. }
  for Lay in FTiledMap.Layers do
  begin
    if not Lay.Visible then Continue;
    if (Lay is TTiledMap.TObjectGroupLayer) or
       (Lay is TTiledMap.TImageLayer) then
      Continue;
    //todo: use opacity
    //todo: use offset
    //todo: use map render order
    l := 0;
    for j := FTiledMap.Height - 1 downto 0 do
      for k := 0 to FTiledMap.Width - 1 do
      begin     // writelnlog('tiled control','data high:%d',[High(Lay.Data.Data)]);
        Dat := Lay.Data.Data[l];  //writelnlog('tiled control', 'render ortho 0; gid:%d',[GID]);
        GID := Dat and ClearFlag;
        Inc(l);
        if GID = 0 then Continue;
        Tileset := FTiledMap.GIDToTileset(GID);
        Spr := Tileset.ImageData;
        Spr.X := k * FTiledMap.TileWidth;
        Spr.Y := j * FTiledMap.TileHeight;
        Spr.Frame := GID - Tileset.FirstGID;
        Spr.HorizontalFlip := Dat and HorizontalFlag > 0;
        Spr.VerticalFlip := Dat and VerticalFlag > 0;
        //todo: test flipping diagonally
        Spr.DiagonalFlip := Dat and DiagonalFlag > 0;
        Spr.Draw;
      end;
  end;
end;

procedure TCastleTiledMapControl.RenderHexagonal;
begin
  //todo: implement me
end;

procedure TCastleTiledMapControl.RenderIsometric;
begin
  //todo: implement me
end;

procedure TCastleTiledMapControl.RenderObjects;
var
  Lay: TTiledMap.TLayer;
begin
  { Object groups. }
  for Lay in FTiledMap.Layers do
  begin
    // if Lay is TTiledMap.TObjectGroupLayer then
    // TODO: draw TObjectGroupLayer here
    // TODO: when to draw TImageLayer?
  end;
end;

procedure TCastleTiledMapControl.LoadTilesetsImages;
var
  Tileset: TTiledMap.TTileset;
  Spr: TSprite;
begin
  if FTiledMap <> nil then
    for Tileset in FTiledMap.Tilesets do
    begin
      //todo: use transparent color (implement in TSprite?)
      if Tileset.Columns = 0 then
        Tileset.Columns := Tileset.Image.Width div Tileset.TileWidth;
      if Tileset.TileCount = 0 then
        Tileset.TileCount := (Tileset.Image.Height div Tileset.TileHeight) * Tileset.Columns;
      Spr := TSprite.Create(
        CombineURI(URL, Tileset.Image.Source), Tileset.TileCount,
        Tileset.Columns, Tileset.TileCount div Tileset.Columns,
        True, False);
      Spr.DrawingWidth := FTiledMap.TileWidth;
      Spr.DrawingHeight := FTiledMap.TileHeight;
      Spr.FrameWidth := FTiledMap.TileWidth;
      Spr.FrameHeight := FTiledMap.TileHeight;
      Spr.LeftMargin := Tileset.Margin;
      Spr.TopMargin := Tileset.Margin;
      Spr.HorizontalSpacing := Tileset.Spacing;
      Spr.VerticalSpacing := Tileset.Spacing;
      Spr.VerticalSpacingBottom := true;
      Tileset.ImageData := Spr;
    end;
end;

procedure TCastleTiledMapControl.FreeTilesetsImages;
var
  Tileset: TTiledMap.TTileset;
begin
  if FTiledMap <> nil then
    for Tileset in FTiledMap.Tilesets do
      FreeAndNil(Tileset.ImageData);
end;

constructor TCastleTiledMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FObjectsVisible := False;
end;

destructor TCastleTiledMapControl.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TCastleTiledMapControl.Close;
begin
  FreeTilesetsImages;
  FreeAndNil(FTiledMap);
end;

procedure TCastleTiledMapControl.LoadFromFile(const AURL: string);
begin
  URL := AURL;
end;

procedure TCastleTiledMapControl.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    Close;
    if Value <> '' then
      FTiledMap := TTiledMap.Create(Value);
    LoadTilesetsImages;
  end;
end;

procedure TCastleTiledMapControl.Render;
begin
  inherited Render;
  if FTiledMap = nil then Exit;

  case FTiledMap.Orientation of
    moOrthogonal: RenderOrthogonal;
    moIsometric: RenderIsometric;
    moStaggered: RenderHexagonal;
  end;

  if FObjectsVisible then RenderObjects;
end;

{$endif read_implementation}
