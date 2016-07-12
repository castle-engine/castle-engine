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
begin
  //todo: implement me
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
  _ts: TTileset;
  _spr: TSprite;
begin

  for i := 0 to FTiledMap.Tilesets.Count - 1 do
  begin
    _ts := FTiledMap.Tilesets.Items[i];
    //todo: columns rows
    _spr := TSprite.Create(_ts.Source, _ts.TileCount, 1{}, 1{}, True, False);
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
begin
  if not (FImages = nil) then FImages.Free;
  if not (FTiledMap = nil) then FTiledMap.Free;
  FTiledMap := TTiledMap.Create(AURL);
  FImages := TSpriteList.Create(True);
  //todo: foreach tileset load the proper sprite and add to list
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
