{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering utilities for TCastleScene. }
unit SceneUtilities;

interface

uses CastleVectors, CastleColors, CastleShapes, CastleScene;

type
  { Callback used by SetSceneColors.

    Given here VertexPosition is in local coordinates of the given Shape.
    Multiply by Shape.State.Transform to convert to scene coordinate space.

    VertexIndex is the direct index to Shape.Geometry.Coordinates. }
  TVertexColorEvent = function (
    const Shape: TShape;
    const VertexPosition: TVector3;
    const VertexIndex: Integer): TCastleColorRGB of object;

{ Set per-vertex color for all vertexes by a provided callback.
  This modifies scene shapes to add/modify TColorNode instances.

  This makes some assumptions about per-vertex colors setup of every Shape:
  - Assumes colorIndex is empty (default).
  - Assumes ColorPerVertex is TRUE (default).
  IOW, this routine assumes that we have one color corresponding to one coordinate (position).
  This is usually the case.
}
procedure SetSceneColors(const Scene: TCastleScene; const OnVertexColor: TVertexColorEvent);

procedure RemoveSceneColors(const Scene: TCastleScene);

implementation

uses X3DFields, X3DNodes, CastleRenderOptions;

procedure SetSceneColors(const Scene: TCastleScene; const OnVertexColor: TVertexColorEvent);
var
  Geometry: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  ShapeList: TShapeList;
  Shape: TShape;
  Coord: TMFVec3f;
  CoordVectors, ColorVectors: TVector3List;
  ColorField: TSFNode;
  I: Integer;
  ColorNode: TColorNode;
begin
  ShapeList := Scene.Shapes.TraverseList(
    { OnlyActive } false, { OnlyVisible } true, { OnlyCollidable } false);
  for Shape in ShapeList do
  begin
    { Do not try to modify the "proxy" objects
      (like IndexedFaceSet auto-generated to render the Sphere).
      It makes no effect, as the "changed color" event causes the proxy to be regenerated.
      So we look only at OriginalGeometry/State. }
    Geometry := Shape.OriginalGeometry;
    State := Shape.OriginalState;

    if not Geometry.InternalCoord(State, Coord) then Continue;
    CoordVectors := Coord.Items;

    ColorField := Geometry.ColorField;
    if ColorField = nil then Continue;

    // calculate ColorNode, ColorVectors
    if ColorField.Value is TColorNode then
      ColorNode := TColorNode(ColorField.Value)
    else
    begin
      ColorNode := TColorNode.Create;
      ColorNode.Mode := cmModulate;
      ColorField.Value := ColorNode;
      // ColorField.Changed; // TODO: not enough, so we do Scene.ChangedAll
    end;
    ColorVectors := ColorNode.FdColor.Items;

    ColorVectors.Count := CoordVectors.Count;

    for I := 0 to CoordVectors.Count - 1 do
      ColorVectors.L[I] := OnVertexColor(Shape, CoordVectors.L[I], I);

    // ColorNode.FdColor.Changed; // TODO: not enough, so we do Scene.ChangedAll
  end;

  { TODO: For some reason, the above Xxx.Changed are not enough,
    even with Scene.ProcessEvents.
    For now, just do (costly) ChangedAll. }
  Scene.ChangedAll;
end;

procedure RemoveSceneColors(const Scene: TCastleScene);
var
  Geometry: TAbstractGeometryNode;
  //State: TX3DGraphTraverseState;
  ShapeList: TShapeList;
  Shape: TShape;
  ColorField: TSFNode;
begin
  ShapeList := Scene.Shapes.TraverseList(
    { OnlyActive } false, { OnlyVisible } true, { OnlyCollidable } false);
  for Shape in ShapeList do
  begin
    Geometry := Shape.OriginalGeometry;
    //State := Shape.OriginalState;

    ColorField := Geometry.ColorField;
    if ColorField = nil then Continue;

    ColorField.Value := nil;
  end;

  Scene.ChangedAll;
end;

end.
