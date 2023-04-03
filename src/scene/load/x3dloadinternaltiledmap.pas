{
  Copyright 2020-2023 Matthias J. Molski, Michalis Kamburelis, Freedomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert Tiled map (tmx file, see https://www.mapeditor.org/) into X3D representation.

  Underneath, the map is loaded using @link(TCastleTiledMapData) class,
  thus the map-reading logic is shared with TCastleTiledMapControl.

  TODO:

  @orderedList(
    @item(Tiled image layers)
    @item(Tiled object ellipsoids)
  )
}
unit X3DLoadInternalTiledMap;

{$I castleconf.inc}

interface

uses
  Classes,
  X3DNodes, CastleLog, CastleTiledMap, CastleVectors, Generics.Collections;

{ Load Tiled map into X3D node.
  This is used by LoadNode, which in turn is used by TCastleSceneCore.Load.

  Underneath, this loads Tiled map using TCastleTiledMapData,
  then uses internal conversion class to generate X3D node from it. }
function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

type
  { Convert Tiled map into X3D node. }
  TCastleTiledMapConverter = class
  strict private
    type
      TTilesetNodes = record
        Coord: TCoordinateNode;
        TexCoord: TTextureCoordinateNode;
      end;

      TAnimationNodes = record
        CoordNodes: TTilesetNodes;
        TexCoordInterp: TCoordinateInterpolator2DNode;
        CycleIntervalMs: Cardinal;
      end;

      { Map from CycleInterval in milliseconds -> TTimeSensorNode. }
      TLayerTimeSensors = {$ifdef FPC}specialize{$endif} TDictionary<Cardinal, TTimeSensorNode>;

      TLayerAnimations = {$ifdef FPC}specialize{$endif} TDictionary<TCastleTiledMapData.TAnimation, TAnimationNodes>;

      TLayerConversion = class({$ifdef FPC}specialize{$endif} TDictionary<TCastleTiledMapData.TTileset, TTilesetNodes>)
      strict private
        FLayerTimeSensors: TLayerTimeSensors;
        FLayerAnimations: TLayerAnimations;
      public
        constructor Create;{$ifdef FPC} override;{$endif}
        destructor Destroy;override;
        property LayerTimeSensors: TLayerTimeSensors read FLayerTimeSensors;
        property LayerAnimations: TLayerAnimations read FLayerAnimations;
      end;

    var
      FMap: TCastleTiledMapData;
      FMapNode: TTransformNode;
      FRootNode: TX3DRootNode;

    { Fills every TTileset.RendererData with TAppearanceNode with texture of this tileset. }
    procedure PrepareTilesets;

    { Constructs X3D nodes for each layer. }
    procedure ConvertLayers;

    procedure FreeUnusedTilesetsRendererData;

    procedure BuildObjectGroupLayerNode(const LayerNode: TTransformNode;
      const ALayer: TCastleTiledMapData.TLayer);

    procedure BuildTileLayerNode(const LayerNode: TTransformNode;
      const ALayer: TCastleTiledMapData.TLayer);

    { Convert Tiled coordinates to CGE.
      @groupBegin }
    function ConvY(const TiledCoord: TVector2): TVector2; overload;
    function ConvY(const X, Y: Single): TVector2; overload;
    { @groupEnd }

    { The Tiled map is loaded from stream and NOT free'd automatically. }
    property Map: TCastleTiledMapData read FMap write FMap;

    { Holds the X3D representation of the Tiled map. It is NOT free'd
      automatically. Usually the X3D representation is added to a scene
      by Scene.Load(). The scene which will care about freeing. }
    property MapNode: TTransformNode read FMapNode write FMapNode;
  public
    type
      TLayerIndex = 0..30;
      TLayers = set of TLayerIndex;
    const
      AllLayers = [Low(TLayerIndex)..High(TLayerIndex)];
    var
      { Workaround rendering artifacts for tilesets without alpha bleeding.
        Set before @link(ConvertMap). }
      SmoothScalingSafeBorder: Boolean;

      { See @link(TCastleTiledMap.ForceTilesetSpacing). }
      ForceTilesetSpacing : Boolean;

      { Layers to load.  }
      Layers: TLayers;

    constructor Create(const ATiledMap: TCastleTiledMapData);
    destructor Destroy; override;

    { Constructs RootNode from TCastleTiledMapData data. }
    procedure ConvertMap;

    property RootNode: TX3DRootNode read FRootNode write FRootNode;
  end;

implementation

uses
  SysUtils, Math,
  CastleTransform, CastleColors, CastleRectangles, CastleUtils,
  CastleRenderOptions, CastleControls, CastleStringUtils,
  CastleImages, CastleURIUtils;

constructor TCastleTiledMapConverter.TLayerConversion.Create;
begin
  inherited;
  FLayerTimeSensors := TLayerTimeSensors.Create;
  FLayerAnimations := TLayerAnimations.Create;
end;

destructor TCastleTiledMapConverter.TLayerConversion.Destroy;
begin
  FLayerTimeSensors.Free;
  FLayerAnimations.Free;
  inherited;
end;

procedure TCastleTiledMapConverter.ConvertMap;
begin
  PrepareTilesets;
  ConvertLayers;
  FreeUnusedTilesetsRendererData;
end;

procedure TCastleTiledMapConverter.FreeUnusedTilesetsRendererData;
var
  Appearance: TAppearanceNode;
  Tileset: TCastleTiledMapData.TTileset;
begin
  for Tileset in Map.Tilesets do
  begin
    Appearance := Tileset.RendererData as TAppearanceNode;
    FreeIfUnusedAndNil(Appearance);
    Tileset.RendererData := nil;
  end;
end;

procedure TCastleTiledMapConverter.PrepareTilesets;
var
  Tileset: TCastleTiledMapData.TTileset;

  { Make a tileset image with added paddings,
    also modify the Tileset to point to the new tileset image (with larger sizes). }
  function ForceTilesetImageSpacing(const AURL: String): TCastleImage;
  var
    OriginalImage: TCastleImage;
    Col, Row: Integer;
    SrcPos, Pos: TVector2Integer;
    ColumnCount, RowCount: Cardinal;

    function TilePosition(const AImage: TEncodedImage; const AMargin, ASpacing: Cardinal): TVector2Integer;
    begin
      Result.X := AMargin + Col * (Tileset.TileWidth + ASpacing);
      Result.Y := AImage.Height - (AMargin + Row * (Tileset.TileHeight + ASpacing)
        + Tileset.TileHeight);
    end;

    procedure Draw(const APos, ASrcPos: TVector2Integer; const AWidth, AHeight: Integer);
    begin
      Result.DrawFrom(OriginalImage, APos.X, APos.Y, ASrcPos.X, ASrcPos.Y, AWidth, AHeight, dmOverwrite);
    end;

  const
    NewMargin = 1;
    NewSpacing = 2;
  var
    OldMargin, OldSpacing:Cardinal;
  begin
    if Tileset.Modified and Assigned(Tileset.CacheImage) and (NewMargin = Tileset.Margin)
      and (NewSpacing = Tileset.Spacing) then
    begin
      //WritelnLog('ForceTilesetImageSpacing', Format('Using CacheImage for %s', [AURL]));
      Exit(Tileset.CacheImage.CreateCopy as TCastleImage);
    end;

    OriginalImage := LoadImage(AURL);
    Result := TCastleImageClass(OriginalImage.ClassType).Create;

    try
      { Must read Margin,Spacing before processed by ForceTilesetImageSpacing. }
      if Tileset.Modified then
      begin
        Assert(NewMargin = Tileset.Margin, 'ForceTilesetImageSpacing: Wrong margin');
        Assert(NewSpacing = Tileset.Spacing, 'ForceTilesetImageSpacing: Wrong spacing');
        OldMargin := Tileset.OriginalMargin;
        OldSpacing := Tileset.OriginalSpacing;
      end else
      begin
        OldMargin := Tileset.Margin;
        OldSpacing := Tileset.Spacing;
      end;
      ColumnCount := (OriginalImage.Width - 2 * OldMargin + OldSpacing)
        div (Tileset.TileWidth + OldSpacing);
      RowCount := (OriginalImage.Height - 2 * OldMargin + OldSpacing)
        div (Tileset.TileHeight + OldSpacing);

      Result.SetSize(ColumnCount * (Tileset.TileWidth + NewSpacing) - NewSpacing + 2 * NewMargin,
        RowCount * (Tileset.TileHeight + NewSpacing) - NewSpacing + 2 * NewMargin);

      for Row := 0 to RowCount - 1 do
      begin
        for Col := 0 to ColumnCount - 1 do
        begin
          Pos := TilePosition(Result, NewMargin, NewSpacing);
          SrcPos := TilePosition(OriginalImage, OldMargin, OldSpacing);

          { Draw original tiles -----------------------------------------------}
          Draw(Pos, SrcPos, Tileset.TileWidth, Tileset.TileHeight);

          { Draw frame -----------------------------------------------}

          { Left }
          Draw(Pos + Vector2Integer(-1, 0),                  SrcPos
            , 1, Tileset.TileHeight);
          { Right }
          Draw(Pos + Vector2Integer(Tileset.TileWidth, 0),   SrcPos + Vector2Integer(Tileset.TileWidth - 1, 0)
            , 1, Tileset.TileHeight);
          { Bottom }
          Draw(Pos + Vector2Integer(0, -1),                  SrcPos
            , Tileset.TileWidth, 1);
          { Top }
          Draw(Pos + Vector2Integer(0, Tileset.TileHeight),  SrcPos + Vector2Integer(0, Tileset.TileHeight - 1)
            , Tileset.TileWidth, 1);

          { LeftBottom }
          Draw(Pos + Vector2Integer(-1, -1),                 SrcPos
            , 1, 1);
          { RightBottom }
          Draw(Pos + Vector2Integer(Tileset.TileWidth, -1),  SrcPos + Vector2Integer(Tileset.TileWidth - 1, 0)
            , 1, 1);
          { LeftTop }
          Draw(Pos + Vector2Integer(-1, Tileset.TileHeight), SrcPos + Vector2Integer(0, Tileset.TileHeight - 1)
            , 1, 1);
          { RightTop }
          Draw(Pos + Vector2Integer(Tileset.TileWidth, Tileset.TileHeight)
            ,SrcPos + Vector2Integer(Tileset.TileWidth - 1, Tileset.TileHeight - 1), 1, 1);

        end;
      end;

      WritelnLog('Tiled', 'Added spacing to tileset image "%s", new margin %d, new spacing %d, old size %d x %d -> new size %d x %d', [
        URIDisplay(AURL),
        NewMargin,
        NewSpacing,
        Tileset.Image.Width,
        Tileset.Image.Height,
        Result.Width,
        Result.Height
      ]);

      if not Tileset.Modified then
      begin
        Tileset.OriginalMargin := OldMargin;
        Tileset.OriginalSpacing := OldSpacing;
        Tileset.Modified := True;
        FreeAndNil(Tileset.CacheImage);
        Tileset.CacheImage := Result.CreateCopy as TCastleImage;

        Tileset.Margin := NewMargin;
        Tileset.Spacing := NewSpacing;
        Tileset.Image.Width := Result.Width;
        Tileset.Image.Height := Result.Height;
      end;
    finally
      FreeAndNil(OriginalImage);
    end;
  end;

var
  Texture: TImageTextureNode;
  TexProperties: TTexturePropertiesNode;
  Appearance: TAppearanceNode;
begin
  for Tileset in Map.Tilesets do
  begin
    if Tileset.Image = nil then
    begin
      WriteLnWarning('Tileset has no image');
      Continue;
    end;

    if (Tileset.TileCount = 0) or (Tileset.Columns = 0) then
    begin
      WriteLnWarning('Empty Tileset');
      Continue;
    end;

    Texture := TImageTextureNode.Create;
    if ForceTilesetSpacing then
      Texture.LoadFromImage(ForceTilesetImageSpacing(Tileset.Image.URL), true, '')
    else
      Texture.SetUrl([Tileset.Image.URL]);

    TexProperties := TTexturePropertiesNode.Create;
    TexProperties.MagnificationFilter := magDefault;
    TexProperties.MinificationFilter := minDefault;
    { Do not force "power of 2" size, which may prevent mipmaps.
      Not resizing is consistent with X3DLoadInternalImage, LoadCastleSpriteSheet,
      users may use it with pixel-art and don't expect images to be resized. }
    TexProperties.GuiTexture := true;
    Texture.TextureProperties := TexProperties;

    Appearance := TAppearanceNode.Create(Tileset.Name, '');
    Appearance.Texture := Texture;
    Tileset.RendererData := Appearance;
  end;
end;

procedure TCastleTiledMapConverter.ConvertLayers;
var
  LayerIndex: Integer;
  Layer: TCastleTiledMapData.TLayer;
  LayerNode: TTransformNode;
  LayerZ: Single;
const
  { Distance between Tiled layers in Z. Layers are rendered as 3D objects
    and need some distance to avoid Z-fighting.

    This could be avoided when using RenderContext.DepthFunc := fdAlways,
    we even tried it at one point (TCastleTiledMap.AssumePerfectRenderingOrder),
    but it had with it's own disadvantages:
    Rendering with RenderContext.DepthFunc = fdAlways
    assumes that really *everything*, including other things
    that could be behind / in front of this Tiled map, are arranged in the TCastleViewport.Items
    tree in the correct order. That is, things behind the Tiled map must be earlier than
    the TCastleTiledMap component in the transformation tree. And things in front of Tiled map must
    be after the TCastleTiledMap component in the transformation tree.
    And this assumption must be preserved by blending sorting done
    by @link(TCastleAbstractRootTransform.BlendingSort), if any.

    So we don't use RenderContext.DepthFunc = fdAlways anymore.
    Instead we apply layer Z distance.

    Note: 1 is too small for examples/tiled/map_viewer/data/maps/desert_with_objects.tmx }
  LayerZDistanceIncrease: Single = 10;
begin
  LayerZ := 0;

  for LayerIndex := 0 to Map.Layers.Count - 1 do
  begin
    Layer := Map.Layers[LayerIndex];
    if Layer.YSortEnabled then Continue;
    if not (
         Layer.Visible and
         (
           (LayerIndex > High(TLayerIndex)) or
           (LayerIndex in Layers)
         )
       ) then
      Continue;

    LayerNode := TTransformNode.Create;

    if Layer is TCastleTiledMapData.TObjectGroupLayer then
      BuildObjectGroupLayerNode(LayerNode, Layer)
    else
    { TODO:
    if Layer is TCastleTiledMapData.TImageLayer then
      BuildImageLayer(Layer, LayerNode)
    else }
      BuildTileLayerNode(LayerNode, Layer);

    MapNode.AddChildren(LayerNode);
    // flip -Layer.OffsetY, as Tiled Y goes down
    LayerNode.Translation := Vector3(Layer.OffsetX, -Layer.OffsetY, LayerZ);
    LayerZ := LayerZ + LayerZDistanceIncrease;
  end;
end;

procedure TCastleTiledMapConverter.BuildObjectGroupLayerNode(const LayerNode: TTransformNode;
  const ALayer: TCastleTiledMapData.TLayer);
var
  // Material node of a Tiled obj.
  TiledObjectMaterial: TMaterialNode;
  // A Tiled object instance (as saved in TCastleTiledMapData).
  TiledObject: TCastleTiledMapData.TTiledObject;
  // Node of a Tiled object.
  TiledObjectNode: TTransformNode;
  // Geometry node of a TiledObject primitive.
  TiledObjectGeometry: TPolyline2DNode;
  // Shape node of a TiledObject.
  TiledObjectShape: TShapeNode;
  // Helper list.
  AVector2List: TVector2List;
  I: Cardinal;
begin
  TiledObjectMaterial := nil;
  TiledObjectNode := nil;
  TiledObjectGeometry := nil;
  TiledObjectShape := nil;
  AVector2List := nil;

  AVector2List := TVector2List.Create;

  for TiledObject in (ALayer as TCastleTiledMapData.TObjectGroupLayer).Objects do
  begin
    if not TiledObject.Visible then
      Continue;

    { All Tiled objects of this layer share the same material node. The color
      depends on the layer color in accordance with handling of Tiled editor. }
    if not Assigned(TiledObjectMaterial) then
    begin
      TiledObjectMaterial := TMaterialNode.Create;
      TiledObjectMaterial.EmissiveColor := ALayer.Color;
    end;

    { Every Tiled object is based on a transform node. }
    TiledObjectNode := TTransformNode.Create;
    TiledObjectNode.Translation := Vector3(
      TiledObject.Position.X,
      Map.Height * Map.TileHeight - TiledObject.Position.Y, // Tiled Y goes down, CGE Y goes up
      0
    );

    { Every primitive is implemented as polyline node. Hint: For better
      performance rectangle and point could be implemented as rect. node?}
    TiledObjectGeometry := TPolyline2DNode.CreateWithShape(TiledObjectShape);
    case TiledObject.Primitive of
      topPolyline, topPolygon:
        begin
          AVector2List.Clear;
          AVector2List.Assign(TiledObject.Points);
          for I := 0 to AVector2List.Count-1 do
            AVector2List.Items[I] := ConvY(AVector2List.Items[I]);

          { Polygon: Add point with index 0 to points list to get a closed polygon }
          if TiledObject.Primitive = topPolygon then
            AVector2List.Add(AVector2List.Items[0]);

          TiledObjectGeometry.SetLineSegments(AVector2List);
        end;
      topRectangle:
        begin
          TiledObjectGeometry.SetLineSegments([
            ConvY(0.0, 0.0),
            ConvY(TiledObject.Width, 0.0),
            ConvY(TiledObject.Width, TiledObject.Height),
            ConvY(0.0, TiledObject.Height),
            ConvY(0.0, 0.0)
          ]);
        end;
      topPoint:
        begin
          { TODO: Render points a X3D/OpenGL points, not rectangles. }
          AVector2List.Clear;
          { Construct a rectangle around position of point. }
          AVector2List.Add(ConvY(-1, -1));
          AVector2List.Add(ConvY(-1, 1));
          AVector2List.Add(ConvY(1, 1));
          AVector2List.Add(ConvY(1,-1));
          AVector2List.Add(ConvY(-1, -1));
          TiledObjectGeometry.SetLineSegments(AVector2List);
        end;
      // TODO: handle ellipse
      topEllipse:
        begin
          WritelnWarning('Not supported yet: Ellipse object primitive. Ignored.');
        end;
    end;
    TiledObjectShape.Material := TiledObjectMaterial;
    TiledObjectNode.AddChildren(TiledObjectShape);
    LayerNode.AddChildren(TiledObjectNode);
  end;
    FreeAndNil(AVector2List);
end;

procedure TCastleTiledMapConverter.BuildTileLayerNode(const LayerNode: TTransformNode;
  const ALayer: TCastleTiledMapData.TLayer);

  function GetTileCoordRect(const TilePosition: TVector2Integer;
    const Tileset: TCastleTiledMapData.TTileset): TFloatRectangle;
  begin
    Result := FloatRectangle(
      Map.TileRenderPosition(TilePosition) + Vector2(Tileset.TileOffset.X, - Tileset.TileOffset.Y),
      Tileset.TileWidth,
      Tileset.TileHeight
    );
  end;

  function GetTileTexCoordRect(const Tile: TCastleTiledMapData.TTile; const Tileset: TCastleTiledMapData.TTileset): TFloatRectangle;
  begin
    Result := FloatRectangle(
      (Tile.Id mod Tileset.Columns) * (Tileset.TileWidth + Tileset.Spacing)
      + Tileset.Margin,
      (Tile.Id div Tileset.Columns) * (Tileset.TileHeight + Tileset.Spacing)
      + Tileset.Margin,
      Tileset.TileWidth,
      Tileset.TileHeight
    );

    if SmoothScalingSafeBorder then
      Result := Result.Grow(-0.51)
    else
      { Fixes appearance of seams at certain zoom levels. }
      Result := Result.Grow(-0.01);

    { fix Result to be in 0..1 range }
    Result.Left := Result.Left / Tileset.Image.Width;
    Result.Width := Result.Width / Tileset.Image.Width;
    Result.Bottom := Result.Bottom / Tileset.Image.Height;
    Result.Height := Result.Height / Tileset.Image.Height;

    // account that Tiled logic for texture coords assumes Y goes down
    Result.Bottom := 1 - Result.Bottom - Result.Height;
  end;

type
  { 4 texture coordinates, in order:
    - left-bottom
    - right-bottom
    - right-top
    - left-top }
  TQuadTexCoords = array [0..3] of TVector2;

  { Flip rendering of the tile by changing texture coordinates. }
  procedure ApplyFlips(var TexCoord: TQuadTexCoords;
    const HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean);

    procedure SwapValues(var V1, V2: TVector2);
    var
      Tmp: TVector2;
    begin
      Tmp := V1;
      V1 := V2;
      V2 := Tmp;
    end;

  begin
    { Following https://doc.mapeditor.org/en/latest/reference/global-tile-ids/#gid-tile-flipping :
      When rendering an orthographic or isometric tile, the order of operations matters.
      The diagonal flip is done first, followed by the horizontal and vertical flips.
      The diagonal flip should flip the bottom left and top right corners of the tile,
      and can be thought of as an x/y axis swap. For hexagonal tiles, the order does not matter. }
    if DiagonalFlip then
      SwapValues(TexCoord[0], TexCoord[2]);
    if HorizontalFlip then
    begin
      SwapValues(TexCoord[0], TexCoord[1]);
      SwapValues(TexCoord[2], TexCoord[3]);
    end;
    if VerticalFlip then
    begin
      SwapValues(TexCoord[0], TexCoord[3]);
      SwapValues(TexCoord[1], TexCoord[2]);
    end;
  end;

var
  LayerConversion: TLayerConversion;
  Tileset: TCastleTiledMapData.TTileset;
  Geometry: TQuadSetNode;
  Shape: TShapeNode;
  Frame: Integer;
  CoordRect, TexCoordRect: TFloatRectangle;
  TexCoordArray: TQuadTexCoords;
  HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
  LayerIndex : Integer;
  { Render order. }
  CurrentZ: Single;
  { animations var.}
  TimeSensor: TTimeSensorNode;

  function ValidTileId(const TileId : Integer):Boolean;
  begin
    Result := Tileset.ValidTileId(TileId);
  end;

  procedure CalcTexCoordArray(const TileId:Integer);
  begin
    if ValidTileId(TileId) then
      TexCoordRect := GetTileTexCoordRect(Tileset.Tiles[TileId], Tileset)
    else
    begin
      WritelnWarning('Tiled', 'Invalid frame id %d', [TileId]);
      // some fallback, to have something defined
      TexCoordRect := FloatRectangle(0, 0, Tileset.TileWidth, Tileset.TileHeight);
    end;

    TexCoordArray[0] := Vector2(TexCoordRect.Left , TexCoordRect.Bottom);
    TexCoordArray[1] := Vector2(TexCoordRect.Right, TexCoordRect.Bottom);
    TexCoordArray[2] := Vector2(TexCoordRect.Right, TexCoordRect.Top);
    TexCoordArray[3] := Vector2(TexCoordRect.Left , TexCoordRect.Top);
    ApplyFlips(TexCoordArray, HorizontalFlip, VerticalFlip, DiagonalFlip);
  end;

  function CreateTimeSensor(const CycleIntervalMs :Cardinal): TTimeSensorNode;
  begin
    Result := TTimeSensorNode.Create(Format('TimeSensor_%d_%d', [LayerIndex,CycleIntervalMs]));
    Result.CycleInterval := CycleIntervalMs / 1000;
    { Add TimeSensor to Root node }
    LayerNode.AddChildren(Result);
    Result.Loop := true;
  end;

  function CreateNodes: TTilesetNodes;
  begin
    Geometry := TQuadSetNode.CreateWithShape(Shape);
    Result.Coord := TCoordinateNode.Create;
    Geometry.Coord := Result.Coord;
    Result.TexCoord := TTextureCoordinateNode.Create;
    Geometry.TexCoord := Result.TexCoord;
    Shape.Appearance := Tileset.RendererData as TAppearanceNode;
    if ALayer.Opacity <> 1 then
    begin
      Shape.Material := TUnlitMaterialNode.Create;
      (Shape.Material as TUnlitMaterialNode).Transparency := 1 - ALayer.Opacity;
    end;
    LayerNode.AddChildren(Shape);
  end;

  function GetOrCreateTimeSensor(const CycleIntervalMs:Cardinal) :TTimeSensorNode;
  begin
    if LayerConversion.LayerTimeSensors.TryGetValue(CycleIntervalMs, Result) then Exit;

    Result := CreateTimeSensor(CycleIntervalMs);
    LayerConversion.LayerTimeSensors.Add(CycleIntervalMs, Result);
  end;

  procedure AddToTexCoordInterp(const AnimationNodes: TAnimationNodes; const bCreate: Boolean);
  var
    I, StartIndex, Step, FrameCount: SizeInt;
    Durations: Single;
    AniFrame: TCastleTiledMapData.TFrame;
  begin
    FrameCount := Tileset.Tiles[Frame].Animation.Count;

    { Silence Delphi warnings about uninitialized variables,
      in this case the warning is really a false alarm -- our bCreate stays constant,
      so in each case the variables that will be really used are initialized. }
    {$ifndef FPC}
    Durations := 0;
    StartIndex := 0;
    Step := 0;
    {$endif}

    if bCreate then
      Durations := 0
    else
    begin
      Step := AnimationNodes.TexCoordInterp.FdKeyValue.Items.Count div FrameCount;
      StartIndex := Step;
      Step := Step + 4;
    end;

    for I := 0 to FrameCount - 1 do
    begin
      AniFrame := Tileset.Tiles[Frame].Animation.Items[I];

      CalcTexCoordArray(AniFrame.TileId);

      if bCreate then
      begin
        AnimationNodes.TexCoordInterp.FdKeyValue.Items.AddRange(TexCoordArray);
        AnimationNodes.TexCoordInterp.FdKey.Items.Add(Durations / AnimationNodes.CycleIntervalMs);
        Durations := Durations + AniFrame.Duration;
      end else
      begin
        AnimationNodes.TexCoordInterp.FdKeyValue.Items.InsertRange(StartIndex, TexCoordArray);
        StartIndex := StartIndex + Step;
      end;
    end;
  end;

  function CreateAnimationNodes: TAnimationNodes;
  var
    I: integer;
  begin
    Result.CoordNodes := CreateNodes;
    Result.TexCoordInterp := TCoordinateInterpolator2DNode.Create;
    Result.TexCoordInterp.Interpolation := inStep;

    { Calc CycleInterval. }
    Result.CycleIntervalMs := 0;
    for I := 0 to Tileset.Tiles[Frame].Animation.Count - 1 do
      Result.CycleIntervalMs := Result.CycleIntervalMs + Tileset.Tiles[Frame].Animation.Items[I].Duration;

    { Get TimeSensor. }
    TimeSensor := GetOrCreateTimeSensor(Result.CycleIntervalMs);

    AddToTexCoordInterp(Result, true);
    { Add to rootnode. }
    LayerNode.AddChildren(Result.TexCoordInterp);
    LayerNode.AddRoute(TimeSensor.EventFraction_changed, Result.TexCoordInterp.EventSet_fraction);
    LayerNode.AddRoute(Result.TexCoordInterp.EventValue_changed, Result.CoordNodes.TexCoord.FdPoint);
  end;

  function GetOrCreateNodesForTileset: TTilesetNodes;
  begin
    if LayerConversion.TryGetValue(Tileset, Result) then Exit;

    Result := CreateNodes;
    LayerConversion.Add(Tileset, Result);
  end;

  function GetOrCreateNodesForAnimation: TAnimationNodes;
  begin
    if LayerConversion.LayerAnimations.TryGetValue(Tileset.Tiles[Frame].Animation, Result) then
    begin
      AddToTexCoordInterp(Result, false);
      Exit;
    end;

    Result := CreateAnimationNodes;
    LayerConversion.LayerAnimations.Add(Tileset.Tiles[Frame].Animation, Result);
  end;

  procedure RenderTile(const TilePosition: TVector2Integer);
  var
    Nodes: TTilesetNodes;

    procedure AddCoordPoints;
    begin
      CoordRect := GetTileCoordRect(TilePosition, Tileset);
      Nodes.Coord.FdPoint.Items.AddRange([
        Vector3(CoordRect.Left , CoordRect.Bottom, CurrentZ),
        Vector3(CoordRect.Right, CoordRect.Bottom, CurrentZ),
        Vector3(CoordRect.Right, CoordRect.Top   , CurrentZ),
        Vector3(CoordRect.Left , CoordRect.Top   , CurrentZ)
      ]);
    end;

    procedure AddTexCoordPoints;
    begin
      CalcTexCoordArray(Frame);
      Nodes.TexCoord.FdPoint.Items.AddRange(TexCoordArray);
    end;

    function HasAnimation:Boolean;
    begin
      Result := Tileset.Tiles[Frame].Animation.Count > 0;
    end;

  begin
    if Map.TileRenderData(TilePosition, ALayer,
      Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) then
    begin
      if not ValidTileId(Frame) then
      begin
        WritelnWarning('Invalid TileId:%d TilePosition:' + TilePosition.ToString, [Frame]);
        Exit;
      end;

      { If not Created then Create and Add to Dictionary. }
      if HasAnimation then
        Nodes := GetOrCreateNodesForAnimation.CoordNodes
      else
        Nodes := GetOrCreateNodesForTileset;

      AddCoordPoints;
      AddTexCoordPoints;

      CurrentZ := CurrentZ + 1 / (Map.Width * Map.Height);
    end;
  end;

  procedure PrepareData;
  begin
    CurrentZ := 0;
    LayerIndex := FMap.Layers.IndexOf(ALayer);
  end;

var
  X, Y: Integer;
begin
  PrepareData;
  LayerConversion := TLayerConversion.Create;
  try
    for Y := Map.Height - 1 downto 0 do
      for X := 0 to Map.Width - 1 do
        RenderTile(Vector2Integer(X, Y));
  finally
    FreeAndNil(LayerConversion);
  end;
end;

constructor TCastleTiledMapConverter.Create(const ATiledMap: TCastleTiledMapData);
begin
  inherited Create;

  Layers := AllLayers;

  Map := ATiledMap;

  RootNode := TX3DRootNode.Create;
  MapNode := TTransformNode.Create;
  RootNode.AddChildren(MapNode);
end;

destructor TCastleTiledMapConverter.Destroy;
begin
  inherited Destroy;
end;

function TCastleTiledMapConverter.ConvY(const TiledCoord: TVector2): TVector2;
begin
  Result := Vector2(TiledCoord.X, - TiledCoord.Y);
end;

function TCastleTiledMapConverter.ConvY(const X, Y: Single): TVector2;
begin
  Result := ConvY(Vector2(X, Y));
end;

function LoadTiledMap2d(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  TiledMapFromStream: TCastleTiledMapData;
  TiledMapConverter: TCastleTiledMapConverter;
begin
  { The Tiled converter unit expects a TCastleTiledMapData object instance,
    hence create one. }
  TiledMapFromStream := TCastleTiledMapData.Create(Stream, BaseUrl);
  try
    TiledMapConverter := TCastleTiledMapConverter.Create(TiledMapFromStream);
    try
      TiledMapConverter.ConvertMap;
      Result := TiledMapConverter.RootNode;
    finally FreeAndNil(TiledMapConverter) end;
  finally FreeAndNil(TiledMapFromStream) end;
end;

end.
