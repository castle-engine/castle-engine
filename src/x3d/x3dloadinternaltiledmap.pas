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

  { All variables related to TiledObjects. }
  ObjLayer: TTiledMap.TLayer;         // Object group layer of TiledMap.
  TiledObj: TTiledMap.TTiledObject;   // A TiledObject.
  ObjTransformNode: TTransformNode;   // Transform node of a TiledObject primitive.
  ObjPolyNode: TPolyline2DNode = nil; // Geometry node of a TiledObject primitive.
  ObjMaterial: TMaterialNode = nil;   // Material node of a TiledObject primitive.
  ObjShapeNode: TShapeNode = nil;     // Shape node of a TiledObject primitive.
  ObjVector2List: TVector2List = nil; // Helper list.

  function CalcObjLayerHeight: Cardinal;
  begin
    Result := Map.Height * Map.TileHeight;
  end;

  function CreateAndPrepareTransformNode: TTransformNode;
  begin
    Result := TTransformNode.Create;
    Result.Translation := Vector3(ObjLayer.Offset.X + TiledObj.Position.X,
      ObjLayer.Offset.Y + TiledObj.Position.Y, 0);
  end;

  procedure CalcVectorListFromRect(var AVector2List: TVector2List;
    const w, h: Single);
  begin
    AVector2List.Add(Vector2(0, 0));
    AVector2List.Add(Vector2(w, 0));
    AVector2List.Add(Vector2(w, h));
    AVector2List.Add(Vector2(0, h));
    AVector2List.Add(AVector2List.Items[0]);
  end;

begin
  { Root node for scene. }
  RootTransformNode := TTransformNode.Create;

  { All TiledObjects share the same material node. }
  ObjMaterial := TMaterialNode.Create;
  ObjMaterial.EmissiveColor := GreenRGB;  // all TiledOnbjects are green

  { Object groups. }
  ObjVector2List := TVector2List.Create;  // Helper list.

  for ObjLayer in Map.Layers do
  begin
    if (ObjLayer is TTiledMap.TObjectGroupLayer) and ObjLayer.Visible then
    begin
      { TODO : Every object layer should have an own transform node! Implement! }
      for TiledObj in (ObjLayer as TTiledMap.TObjectGroupLayer).Objects do
      begin
        if TiledObj.Visible then
        begin
          { Every TiledObject has its own root transform node. }
          ObjTransformNode := CreateAndPrepareTransformNode;
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
          { TODO : When object layer nodes are implemented, objects should be
            added to them. The layer node should be added to root finally. }
          RootTransformNode.AddChildren(ObjTransformNode);
        end;
      end;
    end;
  end;
  FreeAndNil(ObjVector2List);
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

