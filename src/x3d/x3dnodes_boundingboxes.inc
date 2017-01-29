{
  Copyright 2003-2017 Michalis Kamburelis.

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
    CoordIndex.WritelnWarning_WrongVertexIndex(
      Geometry.X3DType, Index, Coord.Count);
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
  const Justify, JustifyMinor: TX3DFontJustify;
  const Font: TTextureFontData;
  const Strings: TCastleStringList): TBox3D;
var
  YScale, XScale, MaxRowWidth, MinX, MinY, MaxX, MaxY: Single;
  I: integer;
begin
  YScale := Size / Font.RowHeight;
  { TODO: Use maxEntent, length for VRML 2.0. Use width for VRML 1.0. }
  XScale := YScale;

  MaxRowWidth := 0;
  for I := 0 to Strings.Count - 1 do
    MaxVar(MaxRowWidth, Font.TextWidth(Strings[I]) * XScale);

  case Justify of
    fjBegin, fjFirst :
              begin MinX := 0;               MaxX := MaxRowWidth end;
    fjMiddle: begin MinX := -MaxRowWidth/ 2; MaxX := MaxRowWidth / 2 end;
    fjEnd   : begin MinX := -MaxRowWidth;    MaxX := 0 end;
    else raise EInternalError.Create('Text_LocalBoundingBox: Invalid FontStyle Justify value');
  end;
  case JustifyMinor of
    fjFirst : begin MaxY := 1;                   MinY := - (Strings.Count - 1); end;
    fjBegin : begin MaxY := 0;                   MinY := - Strings.Count; end;
    fjMiddle: begin MaxY := - Strings.Count / 2; MinY := -MaxY; end;
    fjEnd   : begin MaxY :=   Strings.Count / 2; MinY := 0; end;
    else raise EInternalError.Create('Text_LocalBoundingBox: Invalid FontStyle JustifyMinor value');
  end;
  MinY *= Size * Spacing;
  MaxY *= Size * Spacing;

  Result.Data[0][0] := MinX;
  Result.Data[0][1] := MinY;
  Result.Data[0][2] := 0;
  Result.Data[1][0] := MaxX;
  Result.Data[1][1] := MaxY;
  Result.Data[1][2] := 0;
end;

function TAsciiTextNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Text_LocalBoundingBox(State,
    State.LastNodes.FontStyle.FdSize.Value,
    FdSpacing.Value,
    Justify, fjFirst,
    State.LastNodes.FontStyle.Font,
    FdString.Items);
end;

function TTextNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  Size, Spacing: Single;
  Justify, JustifyMinor: TX3DFontJustify;
begin
  if FontStyle = nil then
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
    JustifyMinor := TFontStyleNode.DefaultJustifyMinor;
  end else
  begin
    Size := FontStyle.FdSize.Value;
    Spacing := FontStyle.FdSpacing.Value;
    Justify := FontStyle.Justify;
    JustifyMinor := FontStyle.JustifyMinor;
  end;

  Result := Text_LocalBoundingBox(State,
    Size, Spacing, Justify, JustifyMinor, Font, FdString.Items);
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
      MinVar(Result.Data[0, 1], Y);
      MaxVar(Result.Data[1, 1], Y);
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
      E.SpineTransformVar(I, LastY, LastZ, Transform);
      for J := 0 to FdCrossSection.Count - 1 do
      begin
        Point2d := FdCrossSection.Items.Ptr(J);
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
      E.SpineTransformVar(I, LastY, LastZ, Transform);
      for J := 0 to FdCrossSection.Count - 1 do
      begin
        Point2d := FdCrossSection.Items.Ptr(J);
        Result.Add(MatrixMultPoint(State.Transform,
          MatrixMultPoint(Transform,
            Vector3Single(Point2d^[0], 0, Point2d^[1]))));
      end;
    end;
  finally FreeAndNil(E) end;
end;

{ eof ------------------------------------------------------------------------ }
