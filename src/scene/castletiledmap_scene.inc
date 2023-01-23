{%MainUnit castletiledmap.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display a map created in @url(https://www.mapeditor.org/ Tiled) in a viewport (TCastleTiledMap class). }

{$ifdef read_interface}

type
  { Display a map created in (https://www.mapeditor.org/ Tiled) in a viewport. }
  TCastleTiledMap = class(TCastleTransform)
  strict private
    FMap: TCastleTiledMapData;
    FURL: String;
    FSmoothScaling, FSmoothScalingSafeBorder: Boolean;
    FScene: TCastleScene;
    procedure SetURL(const Value: string);
    procedure SetSmoothScaling(const Value: Boolean);
    procedure SetSmoothScalingSafeBorder(const Value: Boolean);
    procedure ReloadMap;
    procedure UpdateSmoothScaling;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Currently loaded map (from @link(URL)), if any.
      @nil if no map is loaded (when @link(URL) is empty). Read-only. }
    property Map: TCastleTiledMapData read FMap;
  published
    { URL (or just filename) of the Tiled map file (TMX). }
    property URL: String read FURL write SetURL;

    { How are images scaled.
      @true means we use nice "bilinear" filter.
      @false means we use pixelated "nearest" filter.

      Beware that using @true, and scaling the map, requires preparing
      the tileset (atlas of tile images) in a proper way. The space
      around each tile must have at least 1-pixel margin
      filled with the same color and alpha,
      otherwise bad colors will creep in because of bilinear filtering.
      See https://castle-engine.io/manual_alpha_bleeding.php .
      Alternatively, you can use @link(SmoothScalingSafeBorder).
    }
    property SmoothScaling: Boolean read FSmoothScaling write SetSmoothScaling default false;

    { In case of @link(SmoothScaling),
      prevent the color outside of rendered frame from creeping in,
      by stretching the rendered frame a bit.
      See @link(TSprite.SmoothScalingSafeBorder).

      It is better to prepare the sprite (tileset) images
      with some spacing between tiles, and alpha bleeding
      (see https://castle-engine.io/manual_alpha_bleeding.php )
      instead of using this property.
      This property workarounds the fact that some sprite (tileset) images
      lack proper spacing and color bleeding, but it distorts the image
      a bit, so it's no longer pixel-perfect. }
    property SmoothScalingSafeBorder: Boolean read FSmoothScalingSafeBorder write SetSmoothScalingSafeBorder
      default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleTiledMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScene := TCastleScene.Create(nil);
  FScene.SetTransient;
  { This is the best BlendingSort.
    IOW we assume the tiles are already provided in the correct rendering order,
    blending sorting should not mess with it.
    Testcase: data/maps/isometric_grass_and_water.tmx . }
  FScene.RenderOptions.BlendingSort := bsNone;
  Add(FScene);

  FSmoothScaling := false;
  FSmoothScalingSafeBorder := false;

  UpdateSmoothScaling;
end;

destructor TCastleTiledMap.Destroy;
begin
  FreeAndNil(FMap);
  inherited;
end;

function TCastleTiledMap.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'URL', 'SmoothScaling', 'SmoothScalingSafeBorder'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleTiledMap.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    ReloadMap;
  end;
end;

procedure TCastleTiledMap.ReloadMap;
var
  Converter: TCastleTiledMapConverter;
begin
  FreeAndNil(FMap);
  FScene.Load(nil, true);

  if FURL <> '' then
  begin
    FMap := TCastleTiledMapData.Create(URL);
    Converter := TCastleTiledMapConverter.Create(FMap);
    try
      Converter.SmoothScalingSafeBorder := SmoothScalingSafeBorder;
      Converter.ConvertMap;
      FScene.Load(Converter.RootNode, true);
    finally FreeAndNil(Converter) end;
  end;
end;

procedure TCastleTiledMap.SetSmoothScaling(const Value: Boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    UpdateSmoothScaling;
  end;
end;

procedure TCastleTiledMap.UpdateSmoothScaling;
begin
  if SmoothScaling then
  begin
    FScene.RenderOptions.MinificationFilter := minLinear;
    FScene.RenderOptions.MagnificationFilter := magLinear;
  end else
  begin
    FScene.RenderOptions.MinificationFilter := minNearest;
    FScene.RenderOptions.MagnificationFilter := magNearest;
  end;
end;

procedure TCastleTiledMap.SetSmoothScalingSafeBorder(const Value: Boolean);
begin
  if FSmoothScalingSafeBorder <> Value then
  begin
    FSmoothScalingSafeBorder := Value;
    ReloadMap;
  end;
end;

{$endif read_implementation}
