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
  TCastleTiledMapControl = class(TUIControl)
  private
    FTiledMap: TTiledMap;
    procedure RenderOrthogonal;
    procedure RenderHexagonal;
    procedure RenderIsometric;
  public
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

constructor TCastleTiledMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TCastleTiledMapControl.Create(AOwner: TComponent; AURL: string);
begin
  Create(AOwner);
  FTiledMap := TTiledMap.Create(AURL);
end;

destructor TCastleTiledMapControl.Destroy;
begin
  FTiledMap.Free;
  inherited Destroy;
end;

procedure TCastleTiledMapControl.LoadFromFile(AURL: string);
begin
  if not (FTiledMap = nil) then FTiledMap.Free;
  FTiledMap := TTiledMap.Create(AURL);
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
end;

{$endif read_implementation}
