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
    type
      TMapList = {$Ifdef fpc}specialize{$endif}TObjectList<TCastleScene>;
    var
      FForceTilesetSpacing: Boolean;
      FItems: TCastleTransform;
      FMap: TCastleTiledMapData;
      FSubMaps: TMapList;
      FURL: String;
      FSmoothScaling, FSmoothScalingSafeBorder: Boolean;
      FScene: TCastleScene;
      FLayers: TLayers;
    procedure SetForceTilesetSpacing(const AValue: Boolean);
    procedure SetURL(const Value: string);
    procedure SetSmoothScaling(const Value: Boolean);
    procedure SetSmoothScalingSafeBorder(const Value: Boolean);
    procedure ReloadMapData;
    procedure ConvertMap;
    procedure ConvertToScene(const AMapData: TCastleTiledMapData;const AScene: TCastleScene;const ALayers: TLayers);
    procedure CreateYSortScenes;
    procedure ReloadMap(const KeepMapData: Boolean = False);
    procedure UpdateSmoothScaling;
    procedure UpdateSmoothScalingOneScene(const AScene: TCastleScene);
    procedure SetLayers(const Value: TLayers);
    procedure PlayAnimationsOneScene(const AScene: TCastleScene);
    procedure StopAnimationsOneScene(const AScene: TCastleScene; const ResetAnimations: Boolean);
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

    property Items:TCastleTransform read FItems ;
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

  FSubMaps := TMapList.Create(True);
  FItems := TCastleTransform.Create(Self);
  FItems.SetSubComponent(true);
  FItems.Name := 'Items';
  Add(FItems);
  FItems.BlendingSort := bsYSort;

  FMap := TCastleTiledMapData.Create;

  FSmoothScaling := false;
  FSmoothScalingSafeBorder := false;
  FForceTilesetSpacing := false;

  FLayers := AllLayers;

  UpdateSmoothScaling;
end;

destructor TCastleTiledMap.Destroy;
begin
  FreeAndNil(FSubMaps);
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
    ReloadMap(False);
  end;
end;

procedure TCastleTiledMap.ReloadMap(const KeepMapData: Boolean);
begin
  if not KeepMapData then ReloadMapData;
  ConvertMap;
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
var
  AScene: TCastleScene;
begin
  UpdateSmoothScalingOneScene(FScene);
  for AScene in FSubMaps do
  begin
    UpdateSmoothScalingOneScene(AScene);
  end;
end;

procedure TCastleTiledMap.UpdateSmoothScalingOneScene(const AScene: TCastleScene
  );
begin
  if SmoothScaling then
  begin
    AScene.RenderOptions.MinificationFilter := minLinear;
    AScene.RenderOptions.MagnificationFilter := magLinear;
  end else
  begin
    AScene.RenderOptions.MinificationFilter := minNearest;
    AScene.RenderOptions.MagnificationFilter := magNearest;
  end;
end;

procedure TCastleTiledMap.SetSmoothScalingSafeBorder(const Value: Boolean);
begin
  if FSmoothScalingSafeBorder <> Value then
  begin
    FSmoothScalingSafeBorder := Value;
    ReloadMap(True);
  end;
end;

procedure TCastleTiledMap.ReloadMapData;
begin
  FreeAndNil(FMap);
  if FURL <> '' then
    FMap := TCastleTiledMapData.Create(URL);
end;

procedure TCastleTiledMap.ConvertMap;
begin
  ConvertToScene(FMap, FScene, Layers);
  CreateYSortScenes;
end;

procedure TCastleTiledMap.ConvertToScene(const AMapData: TCastleTiledMapData;
  const AScene: TCastleScene; const ALayers: TLayers);
var
  Converter: TCastleTiledMapConverter;
begin
  AScene.Load(nil, true);
  if AMapData = nil then Exit;

  Converter := TCastleTiledMapConverter.Create(AMapData);
  try
    Converter.SmoothScalingSafeBorder := SmoothScalingSafeBorder;
    Converter.ForceTilesetSpacing := ForceTilesetSpacing;
    Converter.Layers := ALayers;
    Converter.ConvertMap;
    AScene.Load(Converter.RootNode, true);
    PlayAnimationsOneScene(AScene);

  finally FreeAndNil(Converter) end;
end;

procedure TCastleTiledMap.CreateYSortScenes;
var
  Layer: TCastleTiledMapData.TLayer;
  LayerIndex: integer;

  procedure ProcessLayer;
  type
    TTileData = record
      Position: TVector2Integer;
      Data: Cardinal;
    end;

    TTileDataList = {$Ifdef fpc}specialize {$endif}TList<TTileData>;
    { Abs YSortOffset, Data }
    TTilesCompositing = {$Ifdef fpc}specialize {$endif}TObjectDictionary<Single, TTileDataList>;
  var
    TilesCompositing: TTilesCompositing;
    X, Y: Integer;
    Tileset: TCastleTiledMapData.TTileset;
    Frame: Integer;
    TileYSortOffset: Single;
    Data: Cardinal;
    TileDataList: TTileDataList;
    TileData: TTileData;

    function GetYSortOffset: Single;
    var
      ZOffset: Single;
    begin
      ZOffset := 0;
      Tileset.Tiles[Frame].Properties.FloatValue(TCastleTiledMapData.TileZOffsetPropName, ZOffset);
      Result := - ZOffset + (Y + 0.5) * Map.TileHeight;
    end;

    procedure AddToTilesCompositing;
    begin
      TileData.Position := Vector2Integer(X, Y);
      TileData.Data := Data;

      if TilesCompositing.TryGetValue(TileYSortOffset, TileDataList) then
        TileDataList.Add(TileData)
      else
      begin
        TileDataList := TTileDataList.Create;
        TileDataList.Add(TileData);
        TilesCompositing.Add(TileYSortOffset, TileDataList);
      end;
    end;

    procedure CreateMaps;
    var
      ATileYSortOffset: Single;//for delphi

      procedure CreateMap;
      var
        NewMap: TCastleScene;
        ARect: TRectangle;
        AData: TTilesData;
        NewPos: TVector2Integer;

          function ComposeMapData: Boolean;
          var
            ATileData: TTileData; //for delphi
          begin
            ARect := TRectangle.Empty;
            for ATileData in TileDataList do
            begin
              ARect := ARect + Rectangle(ATileData.Position, 1, 1);
            end;
            Result := not ARect.IsEmpty;
            if not Result then Exit;

            SetLength(AData, ARect.Width * ARect.Height);
            if Length(AData) > 0 then
              FillChar(AData[0], Length(AData) * SizeOf(AData[0]), 0);
            for ATileData in TileDataList do
            begin
              NewPos := ATileData.Position - ARect.LeftBottom;
              NewPos.Y := ARect.Height -1 - NewPos.Y;
              AData[NewPos.X + NewPos.Y * ARect.Width] := ATileData.Data;
            end;

          end;
      var
        OldLayerList, NewLayerList: TCastleTiledMapData.TLayerList;
        OldWidth, OldHeight: Cardinal;
        NewLayer: TCastleTiledMapData.TLayer;
      begin
        if not ComposeMapData then Exit;

        NewMap := TCastleScene.Create(FItems);
        NewMap.SetTransient;
        NewMap.RenderOptions.BlendingSort := bsNone;
        UpdateSmoothScalingOneScene(NewMap);
        FItems.Add(NewMap);

        OldLayerList := FMap.Layers;
        OldWidth := FMap.Width;
        OldHeight := FMap.Height;

        NewLayer := TCastleTiledMapData.TLayer.Create;
        NewLayer.Data.IgnoreDataAssign := True;
        NewLayer.Assign(Layer);
        NewLayer.YSortEnabled := False;
        NewLayer.Data.Data := AData;
        NewLayerList := TCastleTiledMapData.TLayerList.Create(True);
        NewLayerList.Add(NewLayer);

        FMap.Layers := NewLayerList;
        FMap.Width := ARect.Width;
        FMap.Height := ARect.Height;

        ConvertToScene(FMap, NewMap, AllLayers);

        FMap.Layers := OldLayerList;
        FMap.Width := OldWidth;
        FMap.Height := OldHeight;
        FreeAndNil(NewLayerList);

        NewMap.Translation := Vector3(ARect.Left * FMap.TileWidth, ARect.Bottom * FMap.TileHeight, 0);
        NewMap.YSortOffset := ATileYSortOffset - ARect.Bottom * FMap.TileHeight;
        FSubMaps.Add(NewMap);

        {
        WritelnLog('CreateMap', 'Compose Tile Count: ' + TileDataList.Count.ToString);
        WritelnLog('CreateMap', Format('Pos: %f, %f Size: %d, %d',[NewMap.Translation.X,
          NewMap.Translation.Y, NewMap.Map.Width, NewMap.Map.Height]));
          }
      end;
    begin
      for ATileYSortOffset in TilesCompositing.Keys do
      begin
        if TilesCompositing.TryGetValue(ATileYSortOffset, TileDataList) then
          CreateMap;
      end;
    end;

  begin
    TilesCompositing := TTilesCompositing.Create([doOwnsValues]);
    try
      for Y := Map.Height - 1 downto 0 do
        for X := 0 to Map.Width - 1 do
        begin
          if not FMap.TileRenderDataSimple(Vector2Integer(X, Y), Layer, Tileset, Frame,
            Data) then Continue;
          if not Tileset.ValidTileId(Frame) then
          begin
            WritelnWarning('CreateYSortScenes', 'ProcessLayer => ValidTileId: ' + Frame.ToString);
            Continue;
          end;
          TileYSortOffset := GetYSortOffset;
          AddToTilesCompositing;
        end;

      CreateMaps;
    finally
      FreeAndNil(TilesCompositing);
    end;
  end;

begin
  FSubMaps.Clear;

  if FMap = nil then Exit;

  for LayerIndex := 0 to FMap.Layers.Count - 1 do
  begin
    Layer := FMap.Layers[LayerIndex];
    if not (
       Layer.Visible and Layer.YSortEnabled and
       (
         (LayerIndex > High(TLayerIndex)) or
         (LayerIndex in Layers)
       )
     ) then Continue;
    ProcessLayer;
  end;

  WritelnLog('SubMap Count',FSubMaps.Count.ToString);
end;

procedure TCastleTiledMap.SetLayers(const Value: TLayers);
begin
  if FLayers <> Value then
  begin
    FLayers := Value;
    ReloadMap(True);
  end;
end;

procedure TCastleTiledMap.PlayAnimationsOneScene(const AScene: TCastleScene);
var
  I: Integer;
  TimeSensor: TTimeSensorNode;
begin
  AScene.ProcessEvents := true;
  for I := 0 to AScene.AnimationsList.Count - 1 do
  begin
    TimeSensor := AScene.AnimationsList.Objects[I] as TTimeSensorNode;
    TimeSensor.Start(true);
  end;
end;

procedure TCastleTiledMap.StopAnimationsOneScene(const AScene: TCastleScene;
  const ResetAnimations: Boolean);
var
  I: Integer;
  TimeSensor: TTimeSensorNode;
begin
  for I := 0 to AScene.AnimationsList.Count - 1 do
  begin
    TimeSensor := AScene.AnimationsList.Objects[I] as TTimeSensorNode;
    TimeSensor.Stop;
    if ResetAnimations then
      TimeSensor.FakeTime(0, false, true);
  end;
end;

procedure TCastleTiledMap.PlayAnimations;
var
  AScene: TCastleScene;
begin
  PlayAnimationsOneScene(FScene);
  for AScene in FSubMaps do PlayAnimationsOneScene(AScene);
end;

procedure TCastleTiledMap.StopAnimations(const ResetAnimations: Boolean);
var
  AScene: TCastleScene;
begin
  StopAnimationsOneScene(FScene, ResetAnimations);
  for AScene in FSubMaps do StopAnimationsOneScene(AScene, ResetAnimations);
end;

function TCastleTiledMap.HasAnimations: Boolean;
begin
  Result := FScene.AnimationsList.Count > 0;
end;

{$endif read_implementation}
