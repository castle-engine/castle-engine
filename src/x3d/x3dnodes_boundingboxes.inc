{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implement BoundingBox / LocalBoundingBox for geometry nodes. }

{ ----------------------------------------------------------------------------
  Bounding box utilities for nodes with explicit Coordinates (indexed and not).

  Note that we have two separate implementations for local and non-local
  bounding box, that is we do not depend on TAbstractGeometryNode methods
  to automatically calculate one using the other:

  - Calculating LocalBoundingBox explicitly is faster than
    calculating non-local bbox with dummy identity matrix
    (like TAbstractGeometryNode.LocalBoundingBox does).

  - Calculating BoundingBox explicitly results in better (more tighter)
    bbox than using LocalBoundingBox and transforming it.
}

type
  TGI_BBox_Calculator = class
  private
    Coord: TMFVec3f;
    CoordIndex: TMFLong;
    Geometry: TAbstractGeometryNode;
    WasSomeValidIndex: boolean;
    function GetVertexFromIndex(index: integer): TVector3Single;
    function GetIndex(indexNum: integer): integer;
  end;

function TGI_BBox_Calculator.GetVertexFromIndex(index: integer): TVector3Single;
begin
  { na pewno index >= 0, CalculateBoundingBoxFromIndices nie bedzie
    pytalo o indeksy ujemne. }
  if Index < Coord.Count then
  begin
    WasSomeValidIndex := true;
    Result := Coord.Items.L[Index];
  end else
  begin
    CoordIndex.OnWarning_WrongVertexIndex(
      Geometry.NodeTypeName, Index, Coord.Count);
    Result := ZeroVector3Single;
  end;
end;

function TGI_BBox_Calculator.GetIndex(IndexNum: integer): integer;
begin
  { nie musimy sprawdzac czy indexNum jest dobry -
    CalculateBoundingBoxFromIndices bedzie pytalo tylko o indeksy between
    0..VertIndices.Count-1 }
  Result := CoordIndex.Items.L[IndexNum];
end;

function IndexedCoords_BoundingBox(Geometry: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  Coord: TMFVec3f; CoordIndex: TMFLong): TBox3D;
var
  Calculator: TGI_BBox_Calculator;
begin
  Calculator := TGI_BBox_Calculator.Create;
  try
    Calculator.Coord := Coord;
    Calculator.CoordIndex := CoordIndex;
    Calculator.Geometry := Geometry;
    Calculator.WasSomeValidIndex := false;
    Result := CalculateBoundingBoxFromIndices(
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetIndex,
      CoordIndex.Count,
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexFromIndex,
      State.Transform);
    if not Calculator.WasSomeValidIndex then
      Result := EmptyBox3D;
  finally Calculator.Free end;
end;

function IndexedCoords_LocalBoundingBox(Geometry: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  Coord: TMFVec3f; CoordIndex: TMFLong): TBox3D;
var
  Calculator: TGI_BBox_Calculator;
begin
  Calculator := TGI_BBox_Calculator.Create;
  try
    Calculator.Coord := Coord;
    Calculator.CoordIndex := CoordIndex;
    Calculator.Geometry := Geometry;
    Calculator.WasSomeValidIndex := false;
    Result := CalculateBoundingBoxFromIndices(
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetIndex,
      CoordIndex.Count,
      {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexFromIndex);
    if not Calculator.WasSomeValidIndex then
      Result := EmptyBox3D;
  finally Calculator.Free end;
end;

function Coords_BoundingBox(State: TX3DGraphTraverseState;
  Coord: TMFVec3f): TBox3D;
begin
  Result := CalculateBoundingBox(PVector3Single(Coord.Items.List),
    Coord.Items.Count, 0, State.Transform);
end;

function Coords_LocalBoundingBox(State: TX3DGraphTraverseState;
  Coord: TMFVec3f): TBox3D;
begin
  Result := CalculateBoundingBox(PVector3Single(Coord.Items.List),
    Coord.Items.Count, 0);
end;

{ TAbstractGeometryNode --------------------------------------------------------- }

function TAbstractGeometryNode.CallProxyBoundingBox(
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := ProxyGeometry.BoundingBox(ProxyState, nil, nil);
end;

function TAbstractGeometryNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  C: TMFVec3f;
begin
  if Coord(State, C) then
  begin
    if C <> nil then
    begin
      if CoordIndex <> nil then
        Result := IndexedCoords_BoundingBox(Self, State, C, CoordIndex) else
        Result := Coords_BoundingBox(State, C);
    end else
      Result := EmptyBox3D;
  end else
  begin
    if ProxyGeometry <> nil then
      Result := CallProxyBoundingBox(ProxyGeometry, ProxyState) else
      Result := LocalBoundingBox(State, ProxyGeometry, ProxyState).Transform(State.Transform);
  end;
end;

function TAbstractGeometryNode.CallProxyLocalBoundingBox(
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := ProxyGeometry.LocalBoundingBox(ProxyState, nil, nil);
end;

function TAbstractGeometryNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  NewState: TX3DGraphTraverseState;
  C: TMFVec3f;
begin
  if Coord(State, C) then
  begin
    if C <> nil then
    begin
      if CoordIndex <> nil then
        Result := IndexedCoords_LocalBoundingBox(Self, State, C, CoordIndex) else
        Result := Coords_LocalBoundingBox(State, C);
    end else
      Result := EmptyBox3D;
  end else
  begin
    if ProxyGeometry <> nil then
      Result := CallProxyLocalBoundingBox(ProxyGeometry, ProxyState) else
    begin
      NewState := TX3DGraphTraverseState.CreateCopy(State);
      try
        NewState.Transform := IdentityMatrix4Single;
        result := BoundingBox(NewState, ProxyGeometry, ProxyState);
      finally NewState.Free end;
    end;
  end;
end;

{ ------------------------------------------------------------------------ }

function Text_LocalBoundingBox(State: TX3DGraphTraverseState;
  const Size, Spacing: Single;
  const Justify: TX3DFontJustify;
  const TTFont: PTrueTypeFont;
  const Strings: TCastleStringList;
  const Depth: Single): TBox3D;
var
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

  Result.Data[0][0] := MinX;
  Result.Data[0][1] := MinY;
  Result.Data[0][2] := 0;
  Result.Data[1][0] := MaxX;
  Result.Data[1][1] := MaxY;
  Result.Data[1][2] := -Depth;
end;

function TAsciiTextNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Text_LocalBoundingBox(State,
    State.LastNodes.FontStyle.FdSize.Value,
    FdSpacing.Value,
    Justify,
    State.LastNodes.FontStyle.TTF_Font,
    FdString.Items,
    0);
end;

function TTextNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  Size, Spacing: Single;
  Justify: TX3DFontJustify;
  TTFont: PTrueTypeFont;
begin
  if FontStyle = nil then
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
    TTFont := TFontStyleNode.DefaultTTF_Font;
  end else
  begin
    Size := FontStyle.FdSize.Value;
    Spacing := FontStyle.FdSpacing.Value;
    Justify := FontStyle.Justify;
    TTFont := FontStyle.TTF_Font;
  end;

  Result := Text_LocalBoundingBox(State,
    Size, Spacing, Justify, TTFont, FdString.Items, 0);
end;

function TText3DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  Size, Spacing: Single;
  Justify: TX3DFontJustify;
  TTFont: PTrueTypeFont;
begin
  if FontStyle = nil then
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
    TTFont := TFontStyleNode.DefaultTTF_Font;
  end else
  begin
    Size := FontStyle.FdSize.Value;
    Spacing := FontStyle.FdSpacing.Value;
    Justify := FontStyle.Justify;
    TTFont := FontStyle.TTF_Font;
  end;

  Result := Text_LocalBoundingBox(State,
    Size, Spacing, Justify, TTFont, FdString.Items, FdDepth.Value);
end;

function Cone_BoundingBox(State: TX3DGraphTraverseState;
  const BottomRadius, Height: Single;
  const Sides, Bottom, Local: boolean): TBox3D;
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
  begin result := EmptyBox3D; exit end;

 if Local then
   Result := CalculateBoundingBox(@points[firstPoint], High(points)+1-firstPoint, 0) else
   Result := CalculateBoundingBox(@points[firstPoint], High(points)+1-firstPoint, 0, State.Transform);
end;

function TConeNode_1.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM], false);
end;

function TConeNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdSide.Value, FdBottom.Value, false);
end;

function TConeNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM], true);
end;

function TConeNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdSide.Value, FdBottom.Value, true);
end;

function Box_LocalBoundingBox(
  const SizeX, SizeY, SizeZ: Single;
  State: TX3DGraphTraverseState): TBox3D;
begin
  result.Data[0] := Vector3Single(-SizeX/2, -SizeY/2, -SizeZ/2);
  result.Data[1] := Vector3Single( SizeX/2,  SizeY/2,  SizeZ/2);
end;

function TCubeNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Box_LocalBoundingBox(
    FdWidth.Value, FdHeight.Value, FdDepth.Value, State);
end;

function TBoxNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Box_LocalBoundingBox(
    FdSize.Value[0], FdSize.Value[1], FdSize.Value[2], State);
end;

function TCubeNode_1.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transform);
end;

function TBoxNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transform);
end;

function Cylinder_LocalBoundingBox(State: TX3DGraphTraverseState;
  const Height, Radius: Single;
  const Bottom, Side, Top: boolean): TBox3D;
var points: array[0..3]of TVector3Single;
begin
 {points 0 = bottom start, 1 = bottom end}
 points[0] := Vector3Single(-Radius, -Height/2, -Radius);
 points[1] := Vector3Single(+Radius, -Height/2, +Radius);
 {points 2 = top start, 3 = top end}
 points[2] := Vector3Single(-Radius,  Height/2, -Radius);
 points[3] := Vector3Single(+Radius,  Height/2, +Radius);

 if Side or (Top and Bottom) then
  result := Box3D(points[0], points[3]) else
 if Top then
  result := Box3D(points[2], points[3]) else
 if Bottom then
  result := Box3D(points[0], points[1]) else
  result := EmptyBox3D;
end;

function TCylinderNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cylinder_LocalBoundingBox(State,
    FdHeight.Value, FdRadius.Value,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

function TCylinderNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cylinder_LocalBoundingBox(State,
    FdHeight.Value, FdRadius.Value,
    FdBottom.Value, FdSide.Value, FdTop.Value);
end;

function TCylinderNode_1.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transform);
end;

function TCylinderNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transform);
end;

function Sphere_LocalBoundingBox(const Radius: Single): TBox3D;
begin
  Result := Box3D(
    Vector3Single(-Radius, -Radius, -Radius),
    Vector3Single(+Radius, +Radius, +Radius) );
end;

function TSphereNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Sphere_LocalBoundingBox(FdRadius.Value);
end;

function TSphereNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Sphere_LocalBoundingBox(FdRadius.Value);
end;

function TSphereNode_1.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transform);
end;

function TSphereNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transform);
end;

{ TElevationGridNode --------------------------------------------------------- }

function TElevationGridNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  Y: Single;
  I: Integer;
begin
  Result := EmptyBox3D;
  if IsNotEmpty then
  begin
    Result.Data[0, 0] := 0;
    Result.Data[0, 2] := 0;
    Result.Data[1, 0] := FdXSpacing.Value * (FdXDimension.Value - 1);
    Result.Data[1, 2] := FdZSpacing.Value * (FdZDimension.Value - 1);

    { now calculate Result[0, 1] and Result[1, 1] }
    Y := FdHeight.Items.L[0];
    Result.Data[0, 1] := Y;
    Result.Data[1, 1] := Y;
    for I := 1 to FdXDimension.Value * FdZDimension.Value - 1 do
    begin
      Y := FdHeight.Items.L[I];
      MinTo1st(Result.Data[0, 1], Y);
      MaxTo1st(Result.Data[1, 1], Y);
    end;
  end;
end;

{ TExtrusionNode --------------------------------------------------------- }

function TExtrusionNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  I, J: Integer;
  E: TVRMLExtrusion;
  LastY, LastZ: TVector3Single;
  Transform: TMatrix4Single;
  Point2d: PVector2Single;
begin
  Result := EmptyBox3D;

  E := TVRMLExtrusion.Create;
  try
    E.Node := Self;
    for I := 0 to E.High do
    begin
      E.SpineTransformTo1st(I, LastY, LastZ, Transform);
      for J := 0 to FdCrossSection.Count - 1 do
      begin
        Point2d := Addr(FdCrossSection.Items.L[J]);
        Result.Add(MatrixMultPoint(Transform,
          Vector3Single(Point2d^[0], 0, Point2d^[1])));
      end;
    end;
  finally FreeAndNil(E) end;
end;

function TExtrusionNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  I, J: Integer;
  E: TVRMLExtrusion;
  LastY, LastZ: TVector3Single;
  Transform: TMatrix4Single;
  Point2d: PVector2Single;
begin
  Result := EmptyBox3D;

  E := TVRMLExtrusion.Create;
  try
    E.Node := Self;
    for I := 0 to E.High do
    begin
      E.SpineTransformTo1st(I, LastY, LastZ, Transform);
      for J := 0 to FdCrossSection.Count - 1 do
      begin
        Point2d := Addr(FdCrossSection.Items.L[J]);
        Result.Add(MatrixMultPoint(State.Transform,
          MatrixMultPoint(Transform,
            Vector3Single(Point2d^[0], 0, Point2d^[1]))));
      end;
    end;
  finally FreeAndNil(E) end;
end;

{ eof ------------------------------------------------------------------------ }
