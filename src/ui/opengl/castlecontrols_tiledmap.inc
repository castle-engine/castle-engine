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

{ Part of CastleControls unit: drawing 2D tiled map (TMX) on screen (TCastleTiledMap class). }

{$ifdef read_interface}

type

  { Loading and manipulating "Tiled" map files (http://mapeditor.org). }
  TCastleTiledMap = class(TUIControl)
  private
    FAtlas: TSprite;
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
  //Load TMX
  LoadTMXFile(AURL); //try?

  //Parse parameters

  //Create atlas
  //FAtlas := TSprite.Create('URL',frames, cols, rows);
end;

destructor TCastleTiledMap.Destroy;
begin
  FreeAndNil(FAtlas);
  inherited Destroy;
end;

procedure TCastleTiledMap.Render;
begin
  inherited Render;
end;

{$endif read_implementation}
