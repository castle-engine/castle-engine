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

{ Part of CastleControls unit: drawing 2D tiled map (TMX) on screen
  (TCastleTiledMap class). }

{$ifdef read_interface}

type

	{ TCastleTiledMapControl }

  TCastleTiledMapControl = class(TUIControl)
  private
    FTiledMap: TTiledMap;
    { Tilesets images. }
    FImages: TSpriteList;
    FDoRenderObjects: Boolean;
    FDataPath: string;
    procedure RenderOrthogonal;
    procedure RenderHexagonal;
    procedure RenderIsometric;
    procedure RenderObjects;
    procedure LoadTilesetsImages;
  public
    property DoRenderObjects: Boolean read FDoRenderObjects write FDoRenderObjects;
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
  _lay: PLayer;
  _ts: PTileset;
  _spr: TSprite;
  _GID, _dat: Cardinal;
const
  HorizontalFlag = $80000000;
  VerticalFlag   = $40000000;
  DiagonalFlag   = $20000000;
  ClearFlag      = $1FFFFFFF;
begin
  { Layers. }
  for i := 0 to FTiledMap.Layers.Count - 1 do
  begin
    _lay := FTiledMap.Layers. Ptr(i);
    if not _lay^.Visible then Continue;
    if (_lay^.LayerType = LT_ObjectGroup) or (_lay^.LayerType = LT_ImageLayer) then
      Continue;
    //todo: use opacity
    //todo: use offset
    //todo: use map render order
    l := 0;
    for j := FTiledMap.Height - 1 downto 0 do
      for k := 0 to FTiledMap.Width - 1 do
      begin     // writelnlog('tiled control','data high:%d',[High(_lay^.Data.Data)]);
        _dat := _lay^.Data.Data[l];  //writelnlog('tiled control', 'render ortho 0; gid:%d',[_GID]);
        _GID := _dat and ClearFlag;
        Inc(l);
        if _GID = 0 then Continue;
        _ts := FTiledMap.GIDToTileset(_GID);
        _spr := TSprite(_ts^.ImageData);
        _spr.X := k * FTiledMap.TileWidth;
        _spr.Y := j * FTiledMap.TileHeight;
        _spr.Frame := _GID - _ts^.FirstGID;
        _spr.HorizontalFlip := _dat and HorizontalFlag > 0;
        _spr.VerticalFlip := _dat and VerticalFlag > 0;
        //todo: test flipping diagonally
        _spr.DiagonalFlip := _dat and DiagonalFlag > 0;
        _spr.Draw;
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
  _lay: PLayer;
begin
  { Object groups. }
  for i := 0 to FTiledMap.Layers.Count - 1 do
  begin
    if (_lay^.LayerType = LT_Layer) or (_lay^.LayerType = LT_ImageLayer) then
      Continue;
    //todo: draw objects
    //todo: draw image layer here?
    {case _lay^.DrawOrder of
        ODO_Index:;
        ODO_TopDown:;
		  end;  }

  end;
end;

procedure TCastleTiledMapControl.LoadTilesetsImages;
var
  i: Integer;
  _ts: PTileset;
  _spr: TSprite;
begin
  //todo: use tileset spacing and margin parameters; add spacing to TSprite
  for i := 0 to FTiledMap.Tilesets.Count - 1 do
  begin
    _ts := FTiledMap.Tilesets.Ptr(i);
    if _ts^.Columns = 0 then
      _ts^.Columns := _ts^.Image.Width div _ts^.TileWidth;
    if _ts^.TileCount = 0 then
      _ts^.TileCount := (_ts^.Image.Height div _ts^.TileHeight) * _ts^.Columns;
    _spr := TSprite.Create(FDataPath + _ts^.Image.Source, _ts^.TileCount,
                           _ts^.Columns, _ts^.TileCount div _ts^.Columns,
                           True, False);
    _spr.DrawingWidth := FTiledMap.TileWidth;
    _spr.DrawingHeight := FTiledMap.TileHeight;
    _spr.FrameWidth := FTiledMap.TileWidth;
    _spr.FrameHeight := FTiledMap.TileHeight;
    _spr.LeftMargin := _ts^.Margin;
    _spr.TopMargin := _ts^.Margin;
    _spr.HorizontalSpacing := _ts^.Spacing;
    _spr.VerticalSpacing := _ts^.Spacing;
    writelnlog('LoadTlesetsImages', 'columns:%d',[_ts^.Columns]);
    _ts^.ImageData := _spr;
	end;
end;

constructor TCastleTiledMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDoRenderObjects := False;
end;

constructor TCastleTiledMapControl.Create(AOwner: TComponent; AURL: string);
begin
  Create(AOwner);
  LoadFromFile(AURL);
end;

destructor TCastleTiledMapControl.Destroy;
begin
  FImages.Free;
  FTiledMap.Free;
  inherited Destroy;
end;

procedure TCastleTiledMapControl.LoadFromFile(AURL: string);
var
  i, j: Integer;
  _lay: PLayer;
begin
  if not (FImages = nil) then FImages.Free;
  if not (FTiledMap = nil) then FTiledMap.Free;
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
    MO_Orthogonal: RenderOrthogonal;
    MO_Isometric: RenderIsometric;
    MO_Staggered: RenderHexagonal;
  end;

  if FDoRenderObjects then RenderObjects;
end;

{$endif read_implementation}
