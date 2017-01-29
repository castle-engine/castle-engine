{
  Copyright 2015-2017, 2016 Tomasz Wojtyś

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display a 2D tiled map (TMX) (TCastleTiledMap class). }

{$ifdef read_interface}

type
  { Display a 2D tiled map (TMX). }
  TCastleTiledMapControl = class(TUIControl)
  private
    FTiledMap: TTiledMap;
    { Tilesets images. }
    FImages: TSpriteList;
    FObjectsVisible: Boolean;
    FDataPath: string;
    procedure RenderOrthogonal;
    procedure RenderHexagonal;
    procedure RenderIsometric;
    procedure RenderObjects;
    procedure LoadTilesetsImages;
    { Free images created by LoadTilesetsImages. }
    procedure FreeTilesetsImages;
    { Free everything created by LoadFromFile. }
    procedure Close;
  public
    property ObjectsVisible: Boolean read FObjectsVisible write FObjectsVisible;
    { If using this constructor then you have to use LoadFromFile procedure after. }
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; AURL: string); overload;
    destructor Destroy; override;
    { Load given TMX file to memory and creates structures. }
    procedure LoadFromFile(AURL: string);
    procedure Render; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TCastleTiledMapControl.RenderOrthogonal;
var
  i, j, k, l: Integer;
  Lay: PLayer;
  Ts: PTileset;
  Spr: TSprite;
  GID, Dat: Cardinal;
const
  HorizontalFlag = $80000000;
  VerticalFlag   = $40000000;
  DiagonalFlag   = $20000000;
  ClearFlag      = $1FFFFFFF;
begin
  { Layers. }
  for i := 0 to FTiledMap.Layers.Count - 1 do
  begin
    Lay := FTiledMap.Layers. Ptr(i);
    if not Lay^.Visible then Continue;
    if (Lay^.LayerType = ltObjectGroup) or (Lay^.LayerType = ltImageLayer) then
      Continue;
    //todo: use opacity
    //todo: use offset
    //todo: use map render order
    l := 0;
    for j := FTiledMap.Height - 1 downto 0 do
      for k := 0 to FTiledMap.Width - 1 do
      begin     // writelnlog('tiled control','data high:%d',[High(Lay^.Data.Data)]);
        Dat := Lay^.Data.Data[l];  //writelnlog('tiled control', 'render ortho 0; gid:%d',[GID]);
        GID := Dat and ClearFlag;
        Inc(l);
        if GID = 0 then Continue;
        Ts := FTiledMap.GIDToTileset(GID);
        Spr := TSprite(Ts^.ImageData);
        Spr.X := k * FTiledMap.TileWidth;
        Spr.Y := j * FTiledMap.TileHeight;
        Spr.Frame := GID - Ts^.FirstGID;
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
  i: Integer;
  // Lay: PLayer;
begin
  { Object groups. }
  for i := 0 to FTiledMap.Layers.Count - 1 do
  begin
    // if (Lay^.LayerType = LTLayer) or (Lay^.LayerType = ltImageLayer) then
    //   Continue;
    //todo: draw objects
    //todo: draw image layer here?
    {case Lay^.DrawOrder of
        odoIndex:;
        odoTopDown:;
                  end;  }

  end;
end;

procedure TCastleTiledMapControl.LoadTilesetsImages;
var
  I: Integer;
  Ts: PTileset;
  Spr: TSprite;
begin
  for I := 0 to FTiledMap.Tilesets.Count - 1 do
  begin
    //todo: use transparent color (implement in TSprite?)
    Ts := FTiledMap.Tilesets.Ptr(i);
    if Ts^.Columns = 0 then
      Ts^.Columns := Ts^.Image.Width div Ts^.TileWidth;
    if Ts^.TileCount = 0 then
      Ts^.TileCount := (Ts^.Image.Height div Ts^.TileHeight) * Ts^.Columns;
    Spr := TSprite.Create(FDataPath + Ts^.Image.Source, Ts^.TileCount,
                           Ts^.Columns, Ts^.TileCount div Ts^.Columns,
                           True, False);
    Spr.DrawingWidth := FTiledMap.TileWidth;
    Spr.DrawingHeight := FTiledMap.TileHeight;
    Spr.FrameWidth := FTiledMap.TileWidth;
    Spr.FrameHeight := FTiledMap.TileHeight;
    Spr.LeftMargin := Ts^.Margin;
    Spr.TopMargin := Ts^.Margin;
    Spr.HorizontalSpacing := Ts^.Spacing;
    Spr.VerticalSpacing := Ts^.Spacing;
    Spr.VerticalSpacingBottom := true;
    Writelnlog('LoadTlesetsImages', 'columns:%d', [Ts^.Columns]);
    Ts^.ImageData := Spr;
  end;
end;

procedure TCastleTiledMapControl.FreeTilesetsImages;
var
  I: Integer;
  Ts: PTileset;
begin
  if FTiledMap <> nil then
    for I := 0 to FTiledMap.Tilesets.Count - 1 do
    begin
      Ts := FTiledMap.Tilesets.Ptr(i);
      FreeAndNil(Ts^.ImageData);
    end;
end;

constructor TCastleTiledMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FObjectsVisible := False;
end;

constructor TCastleTiledMapControl.Create(AOwner: TComponent; AURL: string);
begin
  Create(AOwner);
  LoadFromFile(AURL);
end;

destructor TCastleTiledMapControl.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TCastleTiledMapControl.Close;
begin
  FreeAndNil(FImages);
  FreeTilesetsImages;
  FreeAndNil(FTiledMap);
end;

procedure TCastleTiledMapControl.LoadFromFile(AURL: string);
begin
  Close;
  FTiledMap := TTiledMap.Create(AURL);
  FDataPath := ExtractURIPath(AURL);
  FImages := TSpriteList.Create(True);
  LoadTilesetsImages;
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
