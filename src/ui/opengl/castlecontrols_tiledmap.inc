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
  _spr: PSprite;
  _GID: Cardinal;
begin
  { Layers. }
  for i := 0 to FTiledMap.Layers.Count - 1 do
  begin
    _lay := FTiledMap.Layers. Ptr(i);
    if not _lay^.Visible then Continue;
    //todo: use opacity
    //todo: use offset
    //todo: use map render order
    l := 0;
    for j := 0 to FTiledMap.Height - 1 do
      for k := 0 downto FTiledMap.Width - 1 do
      begin
        _GID := _lay^.Data.Data[l];
        _ts := FTiledMap.GIDToTileset(_GID);
        _spr := _ts^.ImageData;
        //todo: _spr^.X:=;
        _spr^.Frame := _GID - _ts^.FirstGID;
        Inc(l);
			end;
	end;

  { Object groups. }
  {case _lay^.DrawOrder of
      ODO_Index:;
      ODO_TopDown:;
		end;  }
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
begin
  //todo: implement me
end;

procedure TCastleTiledMapControl.LoadTilesetsImages;
var
  i: Integer;
  _ts: PTileset;
  _spr: TSprite;
begin
  // todo: IF TSX:

  // IF NOT TSX:
  for i := 0 to FTiledMap.Tilesets.Count - 1 do
  begin
    _ts := FTiledMap.Tilesets.Ptr(i);
    _spr := TSprite.Create(FDataPath + _ts^.Image.Source, _ts^.TileCount,
                           _ts^.Columns, _ts^.TileCount div _ts^.Columns,
                           True, False);
    WritelnLog('LoadTlesetsImages path', FDataPath + _ts^.Image.Source);
    _ts^.ImageData := @_spr;
    FImages.Add(_spr); //todo: do usunięcia bo pointer w tileset?
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
  { ereasing flipping data }
  for i := 0 to FTiledMap.Layers.Count - 1 do
  begin
    _lay := FTiledMap.Layers. Ptr(i);
    for j := 0 to High(_lay^.Data.Data) do
      _lay^.Data.Data[j] := _lay^.Data.Data[j] and $3FFFFFFF;
    //todo: tile flipping feature in rendering
	end;

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
