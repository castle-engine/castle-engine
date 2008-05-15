{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ VRMLNodes_BoundingBoxes, czyli implementacja metod BoundingBox
  dla node'ow wywodzacych sie od TNodeGeneralShape. }

{ TNodeGeneralShape --------------------------------------------------------- }

function TNodeGeneralShape.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
 result := BoundingBoxTransform(LocalBoundingBox(State), State.Transform);
end;

function TNodeGeneralShape.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var NewState: TVRMLGraphTraverseState;
begin
 NewState := TVRMLGraphTraverseState.CreateCopy(State);
 try
  NewState.Transform := IdentityMatrix4Single;
  result := BoundingBox(NewState);
 finally NewState.Free end;
end;

{ ------------------------------------------------------------------------ }

function Text_BoundingBox(State: TVRMLGraphTraverseState;
  const Size, Spacing: Single;
  const Justify: TVRMLFontJustify;
  const TTFont: PTrueTypeFont;
  const Strings: TDynStringArray;
  const Depth: Single): TBox3d;
var
  points: array [0..7] of TVector3Single;
  PointsCount: Cardinal;
  DescendScaled, yscale, XScale, maxRowWidth, minx, miny, maxx, maxy: Single;
  I: integer;
begin
  YScale := Size / TTFontSimpleRowHeight(TTFont);
  { TODO: Use maxEntent, length for VRML 2.0.
    Use width for VRML 1.0. }
  XScale := YScale;
  DescendScaled := TTFontSimpleDescend(TTFont) * YScale;

  MaxRowWidth := 0;
  for I := 0 to Strings.Count - 1 do
    MaxTo1st(MaxRowWidth,
      TTFontTextWidth(TTFont, Strings[i]) * XScale);

  maxy := Size - DescendScaled;
  miny := -(Strings.Count - 1) * Size * Spacing - DescendScaled;
  case Justify of
    fjBegin : begin minx := 0;              maxx := maxRowWidth end;
    fjMiddle: begin minx := -maxRowWidth/2; maxx := maxRowWidth/2 end;
    fjEnd   : begin minx := -maxRowWidth;   maxx := 0 end;
    else raise EInternalError.Create('Invalid FontStyle Justify value');
  end;

  points[0] := Vector3Single(minx, maxy, 0);
  points[1] := Vector3Single(maxx, maxy, 0);
  points[2] := Vector3Single(minx, miny, 0);
  points[3] := Vector3Single(maxx, miny, 0);
  PointsCount := 4;

  if Depth <> 0 then
  begin
    { add another set of 4 points, on given -Depth }
    PointsCount := 8;
    for I := 4 to 7 do
    begin
      Points[I] := Points[I - 4];
      Points[I][2] := -Depth;
    end;
  end;

  result := CalculateBoundingBox(@Points, PointsCount, 0, State.Transform);
end;

function TNodeAsciiText_1.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Text_BoundingBox(State,
    State.LastNodes.FontStyle.FdSize.Value,
    FdSpacing.Value,
    Justify,
    State.LastNodes.FontStyle.TTF_Font,
    FdString.Items,
    0);
end;

function TNodeText.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var
  Size, Spacing: Single;
  Justify: TVRMLFontJustify;
  TTFont: PTrueTypeFont;
begin
  if FontStyle = nil then
  begin
    Size := TNodeFontStyle_2.DefaultSize;
    Spacing := TNodeFontStyle_2.DefaultSpacing;
    Justify := TNodeFontStyle_2.DefaultJustify;
    TTFont := TNodeFontStyle_2.DefaultTTF_Font;
  end else
  begin
    Size := FontStyle.FdSize.Value;
    Spacing := FontStyle.FdSpacing.Value;
    Justify := FontStyle.Justify;
    TTFont := FontStyle.TTF_Font;
  end;

  Result := Text_BoundingBox(State,
    Size, Spacing, Justify, TTFont, FdString.Items, 0);
end;

function TNodeText3D.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var
  Size, Spacing: Single;
  Justify: TVRMLFontJustify;
  TTFont: PTrueTypeFont;
begin
  if FontStyle = nil then
  begin
    Size := TNodeFontStyle_2.DefaultSize;
    Spacing := TNodeFontStyle_2.DefaultSpacing;
    Justify := TNodeFontStyle_2.DefaultJustify;
    TTFont := TNodeFontStyle_2.DefaultTTF_Font;
  end else
  begin
    Size := FontStyle.FdSize.Value;
    Spacing := FontStyle.FdSpacing.Value;
    Justify := FontStyle.Justify;
    TTFont := FontStyle.TTF_Font;
  end;

  Result := Text_BoundingBox(State,
    Size, Spacing, Justify, TTFont, FdString.Items, FdDepth.Value);
end;

function Cone_BoundingBox(State: TVRMLGraphTraverseState;
  const BottomRadius, Height: Single;
  const Sides, Bottom: boolean): TBox3d;
var points: array[0..4]of TVector3Single;
    firstPoint: integer;
begin
 {liczymy bounding box takiego ostroslupa o podstawie z kwadratu.
  Tak jest prosto (po prostu 5 punktow); jasne ze w latwo sobie wyobrazic
  taka transformacje ze wyliczony w ten sposob bounding box jest nieco
  za duzy - ale wydaje mi sie ze nie jest tak zle.}
 points[0] := Vector3Single(0, Height/2, 0);
 points[1] := Vector3Single(-BottomRadius, -Height/2, -BottomRadius);
 points[2] := Vector3Single(-BottomRadius, -Height/2, +BottomRadius);
 points[3] := Vector3Single(+BottomRadius, -Height/2, -BottomRadius);
 points[4] := Vector3Single(+BottomRadius, -Height/2, +BottomRadius);
 if Sides then
  firstPoint := 0 else
 if Bottom then
  firstPoint := 1 { nie uwzgledniaj pierwszego punktu = szczytu stozka } else
  begin result := EmptyBox3d; exit end;

 result := CalculateBoundingBox(
   @points[firstPoint],
   High(points)+1-firstPoint, 0, State.Transform);
end;

function TNodeCone_1.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

function TNodeCone_2.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdSide.Value, FdBottom.Value);
end;

function Box_LocalBoundingBox(
  const SizeX, SizeY, SizeZ: Single;
  State: TVRMLGraphTraverseState): TBox3d;
begin
  result[0] := Vector3Single(-SizeX/2, -SizeY/2, -SizeZ/2);
  result[1] := Vector3Single( SizeX/2,  SizeY/2,  SizeZ/2);
end;

function TNodeCube_1.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Box_LocalBoundingBox(
    FdWidth.Value, FdHeight.Value, FdDepth.Value, State);
end;

function TNodeBox.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Box_LocalBoundingBox(
    FdSize.Value[0], FdSize.Value[1], FdSize.Value[2], State);
end;

function Cylinder_LocalBoundingBox(State: TVRMLGraphTraverseState;
  const Height, Radius: Single;
  const Bottom, Side, Top: boolean): TBox3d;
var points: array[0..3]of TVector3Single;
begin
 {points 0 = bottom start, 1 = bottom end}
 points[0] := Vector3Single(-Radius, -Height/2, -Radius);
 points[1] := Vector3Single(+Radius, -Height/2, +Radius);
 {points 2 = top start, 3 = top end}
 points[2] := Vector3Single(-Radius,  Height/2, -Radius);
 points[3] := Vector3Single(+Radius,  Height/2, +Radius);

 if Side or (Top and Bottom) then
  result := Box3d(points[0], points[3]) else
 if Top then
  result := Box3d(points[2], points[3]) else
 if Bottom then
  result := Box3d(points[0], points[1]) else
  result := EmptyBox3d;
end;

function TNodeCylinder_1.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Cylinder_LocalBoundingBox(State,
    FdHeight.Value, FdRadius.Value,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

function TNodeCylinder_2.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Cylinder_LocalBoundingBox(State,
    FdHeight.Value, FdRadius.Value,
    FdBottom.Value, FdSide.Value, FdTop.Value);
end;

function Sphere_LocalBoundingBox(const Radius: Single): TBox3d;
begin
  Result := Box3d(
    Vector3Single(-Radius, -Radius, -Radius),
    Vector3Single(+Radius, +Radius, +Radius) );
end;

function TNodeSphere_1.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Sphere_LocalBoundingBox(FdRadius.Value);
end;

function TNodeSphere_2.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Sphere_LocalBoundingBox(FdRadius.Value);
end;

{ TODO: TNodeSphere, Cylinder, Cone.BoundingBox moze dac wiekszy box niz trzeba. }

{ Indexed nodes -------------------------------------------------------- }

type
  TGI_BBox_Calculator = class
  private
    Coord: TMFVec3f;
    CoordIndex: TMFLong;
    function GetVertexFromIndex(index: integer): TVector3Single;
    function GetIndex(indexNum: integer): integer;
  end;

function TGI_BBox_Calculator.GetVertexFromIndex(index: integer): TVector3Single;
begin
  { na pewno index >= 0, CalculateBoundingBoxFromIndices nie bedzie
    pytalo o indeksy ujemne. }
  if Index < Coord.Count then
    Result := Coord.Items.Items[Index] else
  begin
    VRMLNonFatalError(Format('Wrong vertex index in indexed node ' +
      '(not enouch points in Coordinate node defined: ' +
      'index is %d, we have only %d vertices)',
      [Index, Coord.Count]));
    Result := ZeroVector3Single;
  end;
end;

function TGI_BBox_Calculator.GetIndex(IndexNum: integer): integer;
begin
  { nie musimy sprawdzac czy indexNum jest dobry -
    CalculateBoundingBoxFromIndices bedzie pytalo tylko o indeksy spomiedzy
    0..VertIndices.Count-1 }
  Result := CoordIndex.Items.Items[IndexNum];
end;

function Indexed_BoundingBox(State: TVRMLGraphTraverseState;
  Coord: TMFVec3f; CoordIndex: TMFLong): TBox3d;
var
  Calculator: TGI_BBox_Calculator;
begin
  Calculator := TGI_BBox_Calculator.Create;
  try
    Calculator.Coord := Coord;
    Calculator.CoordIndex := CoordIndex;
    Result := CalculateBoundingBoxFromIndices(
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetIndex,
      CoordIndex.Count,
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexFromIndex,
      State.Transform);
  finally Calculator.Free end;
end;

function TNodeGeneralIndexed_1.BoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Indexed_BoundingBox(State,
    State.LastNodes.Coordinate3.FdPoint, FdCoordIndex);
end;

function TNodeIndexedFaceSet_2.BoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
begin
  if (FdCoord.Value <> nil) and (FdCoord.Value is TNodeCoordinate) then
    Result := Indexed_BoundingBox(State,
      TNodeCoordinate(FdCoord.Value).FdPoint, FdCoordIndex) else
    Result := EmptyBox3d;
end;

function TNodeIndexedLineSet_2.BoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
begin
  if (FdCoord.Value <> nil) and (FdCoord.Value is TNodeCoordinate) then
    Result := Indexed_BoundingBox(State,
      TNodeCoordinate(FdCoord.Value).FdPoint, FdCoordIndex) else
    Result := EmptyBox3d;
end;

function Indexed_LocalBoundingBox(State: TVRMLGraphTraverseState;
  Coord: TMFVec3f; CoordIndex: TMFLong): TBox3d;
var
  Calculator: TGI_BBox_Calculator;
begin
  Calculator := TGI_BBox_Calculator.Create;
  try
    Calculator.Coord := Coord;
    Calculator.CoordIndex := CoordIndex;
    Result := CalculateBoundingBoxFromIndices(
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetIndex,
      CoordIndex.Count,
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexFromIndex);
  finally Calculator.Free end;
end;

function TNodeGeneralIndexed_1.LocalBoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
begin
  Result := Indexed_LocalBoundingBox(State,
    State.LastNodes.Coordinate3.FdPoint, FdCoordIndex);
end;

function TNodeIndexedFaceSet_2.LocalBoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
begin
  if (FdCoord.Value <> nil) and (FdCoord.Value is TNodeCoordinate) then
    Result := Indexed_LocalBoundingBox(State,
      TNodeCoordinate(FdCoord.Value).FdPoint, FdCoordIndex) else
    Result := EmptyBox3d;
end;

function TNodeIndexedLineSet_2.LocalBoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
begin
  if (FdCoord.Value <> nil) and (FdCoord.Value is TNodeCoordinate) then
    Result := Indexed_BoundingBox(State,
      TNodeCoordinate(FdCoord.Value).FdPoint, FdCoordIndex) else
    Result := EmptyBox3d;
end;

{ TNodePointSet ------------------------------------------------------------- }

type
  TPS_BBox_Calculator = class
    Coord: TMFVec3f;
    indexOffset: integer;
    PMatrix: PMatrix4Single;
    function GetVertex(i: integer): TVector3Single;
  end;

  function TPS_BBox_Calculator.GetVertex(i: integer): TVector3Single;
  begin
    Result := MultMatrixPoint(PMatrix^, Coord.Items.Items[i+indexOffset])
  end;

function PointSet_BoundingBox(State: TVRMLGraphTraverseState;
  Coord: TMFVec3f; StartIndex, NumPoints: Integer): TBox3d;
var
  Calculator: TPS_BBox_Calculator;
begin
  Calculator := TPS_BBox_Calculator.Create;
  try
    Calculator.Coord := Coord;
    Calculator.IndexOffset := StartIndex;
    Calculator.PMatrix := @State.Transform;
    Result := CalculateBoundingBox(
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertex, NumPoints);
  finally Calculator.Free end;
end;

function TNodePointSet_1.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var
  StartIndex, NumPoints: integer;
begin
  CalculateRange(State.LastNodes.Coordinate3, StartIndex, NumPoints);
  Result := PointSet_BoundingBox(State,
    State.LastNodes.Coordinate3.FdPoint, StartIndex, NumPoints);
end;

function TNodePointSet_2.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var
  Coord: TMFVec3f;
begin
  if (FdCoord.Value <> nil) and (FdCoord.Value is TNodeCoordinate) then
  begin
    Coord := TNodeCoordinate(FdCoord.Value).FdPoint;
    Result := PointSet_BoundingBox(State, Coord, 0, Coord.Count);
  end else
    Result := EmptyBox3d;
end;

{ TNodeElevationGrid --------------------------------------------------------- }

function TNodeElevationGrid.LocalBoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
var
  Y: Single;
  I: Integer;
begin
  Result := EmptyBox3d;
  if IsNotEmpty then
  begin
    Result[0, 0] := 0;
    Result[0, 2] := 0;
    Result[1, 0] := FdXSpacing.Value * (FdXDimension.Value - 1);
    Result[1, 2] := FdZSpacing.Value * (FdZDimension.Value - 1);

    { now calculate Result[0, 1] and Result[1, 1] }
    Y := FdHeight.Items.Items[0];
    Result[0, 1] := Y;
    Result[1, 1] := Y;
    for I := 1 to FdXDimension.Value * FdZDimension.Value - 1 do
    begin
      Y := FdHeight.Items.Items[I];
      MinTo1st(Result[0, 1], Y);
      MaxTo1st(Result[1, 1], Y);
    end;
  end;
end;

{ TNodeExtrusion --------------------------------------------------------- }

function TNodeExtrusion.LocalBoundingBox(
  State: TVRMLGraphTraverseState): TBox3d;
var
  I, J: Integer;
  E: TVRMLExtrusion;
  LastY, LastZ: TVector3Single;
  Transform: TMatrix4Single;
  Point2d: PVector2Single;
begin
  Result := EmptyBox3d;

  E := TVRMLExtrusion.Create;
  try
    E.Node := Self;
    for I := 0 to E.High do
    begin
      E.SpineTransformTo1st(I, LastY, LastZ, Transform);
      for J := 0 to FdCrossSection.Count - 1 do
      begin
        Point2d := @FdCrossSection.Items.Items[J];
        Box3dSumTo1st(Result, MultMatrixPoint(Transform,
          Vector3Single(Point2d^[0], 0, Point2d^[1])));
      end;
    end;
  finally FreeAndNil(E) end;
end;

{ eof ------------------------------------------------------------------------- }
