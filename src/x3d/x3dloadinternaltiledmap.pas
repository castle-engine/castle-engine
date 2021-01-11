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

{ Load tiled map created by Tiled. (See https://www.mapeditor.org/) }
unit X3DLoadInternalTiledMap;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils,
  X3DNodes, CastleTiledMap, CastleVectors, CastleTransform, CastleColors;

function LoadTiledMap2d(const URL: String): TX3DRootNode;

implementation

function BuildSceneFromTiledMap(Map: TTiledMap): TX3DRootNode;
var
  { All general variables to built the scene. }
  RootTransformNode: TTransformNode;  // The root node of the scene.
  Layer: TTiledMap.TLayer;            // A (tile, object, image) layer
  LayerTransformNode: TTransformNode; // Node of a (tile, object, image) layer.

  function CalcObjLayerHeight: Cardinal;
  begin
    Result := Map.Height * Map.TileHeight;
  end;

  function BuildObjectGroupLayer(const ALayer: TTiledMap.TLayer): TTransformNode;
  var
    ObjMaterial: TMaterialNode = nil;       // Material node of a TiledObject.
    TiledObj: TTiledMap.TTiledObject;       // A TiledObject.
    ObjTransformNode: TTransformNode = nil; // Transform node of a TiledObject.
    ObjPolyNode: TPolyline2DNode = nil;     // Geometry node of a TiledObject primitive.
    ObjShapeNode: TShapeNode = nil;         // Shape node of a TiledObject.
    ObjVector2List: TVector2List = nil;     // Helper list.

    procedure CalcVectorListFromRect(const AVector2List: TVector2List;
    const w, h: Single);
    begin
      AVector2List.Add(Vector2(0, 0));
      AVector2List.Add(Vector2(w, 0));
      AVector2List.Add(Vector2(w, h));
      AVector2List.Add(Vector2(0, h));
      AVector2List.Add(AVector2List.Items[0]);
    end;

  begin
    { All TiledObjects of this layer share the same material node. }
    ObjMaterial := TMaterialNode.Create;
    ObjMaterial.EmissiveColor := ALayer.Color;

    ObjVector2List := TVector2List.Create;  // Helper list.
    Result := TTransformNode.Create;        // The resulting layer node.

    for TiledObj in (ALayer as TTiledMap.TObjectGroupLayer).Objects do
    begin
      if not TiledObj.Visible then
        Continue;

      { Every TiledObject has its own root transform node. }
      ObjTransformNode := TTransformNode.Create;
      ObjTransformNode.Translation := Vector3(ALayer.Offset.X + TiledObj.Position.X,
      ALayer.Offset.Y + TiledObj.Position.Y, 0);
      { Every primitive is implemented as polyline node. Hint: For better
        performance rectangle and point could be implemented as rect. node?}
      ObjPolyNode := TPolyline2DNode.CreateWithShape(ObjShapeNode);
      case TiledObj.Primitive of
        topPolyline:
          begin
            ObjVector2List.Clear;
            ObjVector2List.Assign(TiledObj.Points);
            ObjPolyNode.SetLineSegments(ObjVector2List);
          end;
        topPolygon:
          begin
            ObjVector2List.Clear;
            ObjVector2List.Assign(TiledObj.Points);
            { add point with index 0 to points list to get a closed polygon }
            ObjVector2List.Add(ObjVector2List.Items[0]);
            ObjPolyNode.SetLineSegments(ObjVector2List);
          end;
        topRectangle:
          begin
            ObjVector2List.Clear;
            CalcVectorListFromRect(ObjVector2List, TiledObj.Width,
              TiledObj.Height);
            ObjPolyNode.SetLineSegments(ObjVector2List);
          end;
        topPoint:
          begin
            ObjVector2List.Clear;
            CalcVectorListFromRect(ObjVector2List, 1, 1);
            { A point is a rectangle with width and height of 1 unit. }
            ObjPolyNode.SetLineSegments(ObjVector2List);
          end;
        // TODO: handle ellipse
      end;
      ObjShapeNode.Material := ObjMaterial;
      ObjTransformNode.AddChildren(ObjShapeNode);
      Result.AddChildren(ObjTransformNode);
    end;
    FreeAndNil(ObjVector2List);
  end;

begin
  { Root node for scene. }
  RootTransformNode := TTransformNode.Create;

  for Layer in Map.Layers do
  begin
    if not Layer.Visible then
      Continue;

    { Every Layer has an individual layer node. }
    LayerTransformNode := nil;

    { Object Group Layer. }
    if (Layer is TTiledMap.TObjectGroupLayer) then
    begin
      LayerTransformNode := BuildObjectGroupLayer(Layer);
    end else
    { Image Layers. }
    if (Layer is TTiledMap.TImageLayer) then
    begin
      { TODO : Implement! }
    end else
    { Tile Layers. }
    begin

    end;

    if Assigned(LayerTransformNode) then
      RootTransformNode.AddChildren(LayerTransformNode);
  end;

  RootTransformNode.Rotation := Vector4(1, 0, 0, Pi);  // rotate scene by 180 deg around x-axis

  Result := TX3DRootNode.Create;
  Result.AddChildren(RootTransformNode);
end;

function LoadTiledMap2d(const URL: String): TX3DRootNode;
var
  ATiledMap: TTiledMap;
begin
  Result := nil;
  ATiledMap := TTiledMap.Create(URL);
  try
    Result := BuildSceneFromTiledMap(ATiledMap);
  finally
    FreeAndNil(ATiledMap);
  end;
end;

end.

