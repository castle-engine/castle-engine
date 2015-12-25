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
  TCastleTiledMapControl = class(TUIControl)
  private
    FTiledMap: TCastleTiledMap;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleTiledMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TCastleTiledMapControl.Render;
begin
  inherited Render;
end;

{$endif read_implementation}
