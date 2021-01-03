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

type
  TRectList = record
    Points: TVector2List;

  end;

function LoadTiledMap2d(const URL: string): TX3DRootNode;

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
  HandleVector2List: TVector2List;    // Handle to free list.

  function CreateAndPrepareTransformNode: TTransformNode;
  begin
    Result := TTransformNode.Create;
    Result.Translation := Vector3(ObjLayer.Offset.X + TiledObj.Position.X,
      ObjLayer.Offset.Y + TiledObj.Position.Y, 0);
  end;

  function CreateVectorListFromRect(const w, h: Single): TVector2List;
  begin
    Result := TVector2List.Create;
    Result.Add(Vector2(0, 0));
    Result.Add(Vector2(w, 0));
    Result.Add(Vector2(w, h));
    Result.Add(Vector2(0, h));
    Result.Add(Result.Items[0]);
  end;

begin
  { Root node for scene. }
  RootTransformNode := TTransformNode.Create;

  { All TiledObjects share the same material node. }
  ObjMaterial := TMaterialNode.Create;
  ObjMaterial.EmissiveColor := GreenRGB;  // all TiledOnbjects are green

  { Object groups. }
  for ObjLayer in Map.Layers do
  begin
    if (ObjLayer is TTiledMap.TObjectGroupLayer) and ObjLayer.Visible then
    begin
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
                ObjPolyNode.SetLineSegments(TiledObj.Points);
            topPolygon:
              begin
                { add point with index 0 to points list to get a closed polygon
                 (simple solution, but dirty --> original Map data is modified)
                 TODO : Implement cleaner solution. }
                TiledObj.Points.Add(TiledObj.Points.Items[0]);
                ObjPolyNode.SetLineSegments(TiledObj.Points);
              end;
            topRectangle:
              begin
                HandleVector2List := CreateVectorListFromRect(TiledObj.Width,
                  TiledObj.Height);
                ObjPolyNode.SetLineSegments(HandleVector2List);
                FreeAndNil(HandleVector2List);
              end;
            topPoint:
              begin
                // A point is a rectangle with width and height of 1 unit.
                HandleVector2List := CreateVectorListFromRect(1, 1);
                ObjPolyNode.SetLineSegments(HandleVector2List);
                FreeAndNil(HandleVector2List);
              end;
            // TODO: handle ellipse
          end;
          ObjShapeNode.Material := ObjMaterial;
          ObjTransformNode.AddChildren(ObjShapeNode);
          RootTransformNode.AddChildren(ObjTransformNode);
        end;
      end;
    end;
  end;
  Result := TX3DRootNode.Create;
  Result.AddChildren(RootTransformNode);
end;

function LoadTiledMap2d(const URL: string): TX3DRootNode;
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

