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
  { Display a map created in @url(https://www.mapeditor.org/ Tiled) in a viewport.
    See https://castle-engine.io/tiled_maps for a documentation. }
  TCastleTiledMap = class(TCastleTransform)
  public
    { These types and consts are synchronized with X3DLoadInternalTiledMap }
    type
      { Layer index that can be used with @link(Layers). }
      TLayerIndex = 0..30;
      { Type for @link(Layers), set of visible layers. }
      TLayers = set of TLayerIndex;
    const
      { Indicates that all @link(Layers) should be visible. }
      AllLayers = [Low(TLayerIndex)..High(TLayerIndex)];
  strict private
    FForceTilesetSpacing: Boolean;
    FMap: TCastleTiledMapData;
    FURL: String;
    FSmoothScaling, FSmoothScalingSafeBorder: Boolean;
    FScene: TCastleScene;
    FLayers: TLayers;
    procedure SetForceTilesetSpacing(const AValue: Boolean);
    procedure SetURL(const Value: string);
    procedure SetSmoothScaling(const Value: Boolean);
    procedure SetSmoothScalingSafeBorder(const Value: Boolean);
    procedure ReloadMap;
    procedure UpdateSmoothScaling;
    procedure SetLayers(const Value: TLayers);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure PlayAnimations;
    procedure StopAnimations(const ResetAnimations: Boolean);
    function HasAnimations: Boolean;

    { Currently loaded map (from @link(URL)), if any.
      @nil if no map is loaded (when @link(URL) is empty). Read-only. }
    property Map: TCastleTiledMapData read FMap;
  published
    { URL (or just filename) of the Tiled map file (TMX).

      Note that loading new map from new URL automatically starts animations.
      You can stop them with @link(StopAnimations). }
    property URL: String read FURL write SetURL;

    { How are images scaled.
      @true means we use nice "bilinear" filter.
      @false means we use pixelated "nearest" filter.

      Beware that using @true, and scaling the map, requires preparing
      the tileset (atlas of tile images) in a proper way. The space
      around each tile must have at least 1-pixel margin
      filled with the same color and alpha,
      otherwise bad colors will creep in because of bilinear filtering.
      See @url(https://castle-engine.io/manual_alpha_bleeding.php alpha bleeding).
      Alternatively, you can use @link(SmoothScalingSafeBorder). }
    property SmoothScaling: Boolean read FSmoothScaling write SetSmoothScaling default false;

    { In case of @link(SmoothScaling),
      prevent the color outside of rendered frame from creeping in,
      by stretching the rendered frame a bit.
      See @link(TSprite.SmoothScalingSafeBorder).

      It is better to prepare the sprite (tileset) images
      with some spacing between tiles, and @url(https://castle-engine.io/manual_alpha_bleeding.php
      alpha bleeding) instead of using this property.
      This property workarounds the fact that some sprite (tileset) images
      lack proper spacing and color bleeding, but it distorts the image
      a bit, so it's no longer pixel-perfect.

      Note that changing this when map is loaded reloads it,
      and automatically starts animations.
      You can stop them with @link(StopAnimations).

      See also @link(ForceTilesetSpacing) that workarounds artifacts with
      @link(SmoothScaling) in a different way, by adding spacing in the tileset texture,
      which makes this trick (@name) not necessary.
      Do not use both @link(ForceTilesetSpacing) and @name --
      these 2 properties are different workarounds for the same problem,
      it doesn't make sense to use them both.

      @seealso ForceTilesetSpacing }
    property SmoothScalingSafeBorder: Boolean read FSmoothScalingSafeBorder write SetSmoothScalingSafeBorder
      default false;

    { Use a modified version of the tileset image,
      with a proper space between each tile in the texture (1-pixel size border around each tile,
      effectively 2-pixel wide spacing between each tile).

      Makes sense to be used only with @link(SmoothScaling) = @true,
      it is pointless (but harmless) when @link(SmoothScaling) = @false.

      See also @link(SmoothScalingSafeBorder) that workarounds artifacts with
      @link(SmoothScaling) in a different way, by changing texture coordinates
      such that this spacing is not necessary.
      Do not use both @link(SmoothScalingSafeBorder) and @name --
      these 2 properties are different workarounds for the same problem,
      it doesn't make sense to use them both.

      Note that changing this when map is loaded reloads it,
      and automatically starts animations.
      You can stop them with @link(StopAnimations).

      @seealso SmoothScalingSafeBorder }
    property ForceTilesetSpacing: Boolean read FForceTilesetSpacing write SetForceTilesetSpacing default false;

    { Layers to include when loading this map.

      Note that changing this when map is loaded reloads it,
      and automatically starts animations.
      You can stop them with @link(StopAnimations). }
    property Layers: TLayers read FLayers write SetLayers default AllLayers;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTiledMap ------------------------------------------------------------ }

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
  FForceTilesetSpacing := false;

  FLayers := AllLayers;

  UpdateSmoothScaling;
end;

destructor TCastleTiledMap.Destroy;
begin
  FreeAndNil(FScene);
  FreeAndNil(FMap);
  inherited;
end;

function TCastleTiledMap.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'URL', 'SmoothScaling', 'SmoothScalingSafeBorder', 'Layers', 'ForceTilesetSpacing'
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
    try
      ReloadMap;
    except
      { If loading file failed, and we're inside CGE editor,
        merely report a warning. This allows deserializing in CGE editor
        designs with broken URLs. }
      on E: Exception do
      begin
        if CastleDesignMode then
        begin
          WritelnWarning('TCastleTiledMap', 'Failed to load tiledmap "%s": %s',
            [URIDisplay(Value), ExceptMessage(E)]);
          Exit;
        end else
          raise;
      end;
    end;
  end;
end;

procedure TCastleTiledMap.SetForceTilesetSpacing(const AValue: Boolean);
begin
  if FForceTilesetSpacing <> AValue then
  begin
    FForceTilesetSpacing := AValue;
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
      Converter.ForceTilesetSpacing := ForceTilesetSpacing;
      Converter.Layers := Layers;
      Converter.ConvertMap;
      FScene.Load(Converter.RootNode, true);
      PlayAnimations;

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

procedure TCastleTiledMap.SetLayers(const Value: TLayers);
begin
  if FLayers <> Value then
  begin
    FLayers := Value;
    ReloadMap;
  end;
end;

procedure TCastleTiledMap.PlayAnimations;
var
  I: Integer;
  TimeSensor: TTimeSensorNode;
begin
  FScene.ProcessEvents := true;
  for I := 0 to FScene.AnimationsList.Count - 1 do
  begin
    TimeSensor := FScene.AnimationsList.Objects[I] as TTimeSensorNode;
    TimeSensor.Start(true);
  end;
end;

procedure TCastleTiledMap.StopAnimations(const ResetAnimations: Boolean);
var
  I: Integer;
  TimeSensor: TTimeSensorNode;
begin
  for I := 0 to FScene.AnimationsList.Count - 1 do
  begin
    TimeSensor := FScene.AnimationsList.Objects[I] as TTimeSensorNode;
    TimeSensor.Stop;
    if ResetAnimations then
      TimeSensor.FakeTime(0, false, true);
  end;
end;

function TCastleTiledMap.HasAnimations: Boolean;
begin
  Result := FScene.AnimationsList.Count > 0;
end;

{$endif read_implementation}
