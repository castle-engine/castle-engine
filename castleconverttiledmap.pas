{
  Copyright 2020-2021 Matthias J. Molski.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert Tiled map (see https://www.mapeditor.org/) loaded by
  CastleTiledMap unit into X3D representation.

  This unit is on purpose not fully integrated with the engine yet. This makes
  debugging easier. If unit is fully functional, it should be integrated with
  the Scene.Load mechanism. Until then it works as follows:

  1. Create Tiled map (TTiledMap).
  2. Prepare X3D scene.
  3. Convert Tiled map by this unit (CastleConvertTiledMap).
  4. Load X3D scene directly by X3D representation (use Scene.Load).

  TODO:
  1. Integrate unit with Castle Game Engine (e.g. add to pack., use castle conf.
     inc., ...)
  2. Turn off debug mode.

}
unit CastleConvertTiledMap;

//{$I castleconf.inc}

interface

uses
  Classes, Math,
  X3DNodes, CastleTiledMap, CastleVectors, CastleTransform, CastleColors,
  CastleRenderOptions, X3DLoadInternalImage;

{ Converts a Tiled map into a X3D representation for the Castle Game Engine.
  The result can be returned to Scene.Load method. }
function ConvertTiledMap(ATiledMap: TTiledMap): TX3DRootNode;

implementation

uses
  SysUtils;

type
  { Converter class to convert Tiled map into X3D representations. }

  { TTiledMapConverter }

  TTiledMapConverter = class
  strict private
    FDebugMode: Boolean;

    FMap: TTiledMap;
    FMapNode: TX3DRootNode;

    { Tries to construct X3D nodes for each layer. }
    procedure ConvertLayers;
    { Builds Object Group layer node from TTiledMap data. }
    function BuildObjectGroupLayerNode(const ALayer: TTiledMap.TLayer): TTransformNode;
    { Builds Tile layer node from TTiledMap data. }
    function BuildTileLayerNode(const ALayer: TTiledMap.TLayer): TTransformNode;

    { Helper functions }
    { Map width in pixels. }
    function MapWidth: Cardinal;
    { Map height in pixels. }
    function MapHeight: Cardinal;

    { Build a rectangluar debug object at pos. X,Y with dim. W,H. }
    procedure BuildDebugObject(const X, Y, W, H: Cardinal; const AName: String);

  public
    constructor Create;
    destructor Destroy; override;

    { Tries to construct X3D representation from TTiledMap data. }
    procedure ConvertMap;

    property Map: TTiledMap read FMap write FMap;
    { Holds the X3D representation of the Tiled map. Is not free'd
      automatically.

      TODO : What if MapNode is never returned and manually free'd?
      Improve by getter func.! }
    property MapNode: TX3DRootNode read FMapNode;

    { If true, all objects are represented in debug mode. }
    property DebugMode: Boolean read FDebugMode write FDebugMode;
  end;

procedure TTiledMapConverter.ConvertMap;
begin
  ConvertLayers;
end;

procedure TTiledMapConverter.ConvertLayers;
var
  Layer: TTiledMap.TLayer;            // A (tile, object, image) layer
  LayerTransformNode: TTransformNode; // Node of a (tile, object, image) layer.
begin

  for Layer in Map.Layers do
  begin
    if DebugMode then
      BuildDebugObject(Round(Layer.OffsetX), Round(Layer.OffsetY), MapWidth, MapHeight, Layer.Name);

    if not Layer.Visible then
      Continue;

    { Every Layer has an individual layer node. }
    LayerTransformNode := nil;

    if (Layer is TTiledMap.TObjectGroupLayer) then
    begin
      LayerTransformNode := BuildObjectGroupLayerNode(Layer);
    end else
    if (Layer is TTiledMap.TImageLayer) then
    begin
      { TODO : Implement!
        LayerTransformNode := BuildImageLayer(Layer); }
    end else
    begin
      LayerTransformNode := BuildTileLayerNode(Layer);
    end;

    if Assigned(LayerTransformNode) then
      MapNode.AddChildren(LayerTransformNode);
  end;

  //RootTransformNode.Rotation := Vector4(1, 0, 0, Pi);  // rotate scene by 180 deg around x-axis

  //Result := TX3DRootNode.Create;
  //Result.AddChildren(RootTransformNode);
end;

function TTiledMapConverter.BuildObjectGroupLayerNode(
  const ALayer: TTiledMap.TLayer): TTransformNode;
begin
  Result := TTransformNode.Create;
end;

function TTiledMapConverter.BuildTileLayerNode(const ALayer: TTiledMap.TLayer
  ): TTransformNode;
begin
  Result := TTransformNode.Create;
end;

constructor TTiledMapConverter.Create;
begin
  inherited Create;

  FMapNode := TX3DRootNode.Create;

  DebugMode := True;  // DebugMode := False; // Default
end;

destructor TTiledMapConverter.Destroy;
begin

  inherited Destroy;
end;

function TTiledMapConverter.MapWidth: Cardinal;
begin
  Result := Map.TileWidth * Map.Width;
end;

function TTiledMapConverter.MapHeight: Cardinal;
begin
  Result := Map.TileHeight * Map.Height;
end;

procedure TTiledMapConverter.BuildDebugObject(const X, Y, W, H: Cardinal;
  const AName: String);
var
  DebugGeometry: TRectangle2DNode;
  DebugShape: TShapeNode;
  DebugMaterial: TMaterialNode;
  DebugFillProperties: TFillPropertiesNode;
  DebugLineProperties: TLinePropertiesNode;
begin
  DebugGeometry := TRectangle2DNode.CreateWithShape(DebugShape);
  DebugGeometry.Size := Vector2(Single(W), Single(H));

  DebugMaterial := TMaterialNode.Create;
  DebugMaterial.EmissiveColor := YellowRGB;

  DebugFillProperties := TFillPropertiesNode.Create;
  DebugFillProperties.Filled := False;
  DebugFillProperties.Hatched := False;

  DebugLineProperties := TLinePropertiesNode.Create;
  DebugLineProperties.LinewidthScaleFactor := 3.0;

  DebugShape.Appearance := TAppearanceNode.Create;
  DebugShape.Appearance.Material := DebugMaterial;
  DebugShape.Appearance.FillProperties := DebugFillProperties;
  DebugShape.Appearance.LineProperties := DebugLineProperties;

  MapNode.AddChildren(DebugShape);
end;

function ConvertTiledMap(ATiledMap: TTiledMap): TX3DRootNode;
var
  ATiledMapConverter: TTiledMapConverter;
begin
  Result := nil;

  if not Assigned(ATiledMap) then
    Exit;

  try
    ATiledMapConverter := TTiledMapConverter.Create;
    ATiledMapConverter.Map := ATiledMap;
    ATiledMapConverter.ConvertMap;
    Result := ATiledMapConverter.MapNode;
  finally
    FreeAndNil(ATiledMapConverter);
  end;

end;

end.

