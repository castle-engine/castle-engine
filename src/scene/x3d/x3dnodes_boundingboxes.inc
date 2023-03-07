{
  Copyright 2003-2023 Michalis Kamburelis.

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

{ By default bounding boxes we calculate for meshes are larger than necessary
  in some situations (e.g. when you have a shape that is rotated).
  That it because we do 2 optimizations:

  1. Calculate bounding boxes of transformed shapes as
    a "transformed(bounding box, for all vertexes)".
    This results in worse box than using "bounding boxm for all trasform(vertex)",
    but it avoids multiplying each vertex by a matrix.

  2. Calculate bounding boxes of indexed and non-indexed shapes the same way,
    by just iterating over coordinates, without paying attention to indexes.
    This results in worse box than looking at indexes
    (in case coordinates contain distant and unused vertexes) but is faster.

  You can define CASTLE_PRECISE_MESH_BOUNDING_BOXES to restore
  slower but more precise bounding boxes.
}
{.$define CASTLE_PRECISE_MESH_BOUNDING_BOXES}

{$ifdef CASTLE_PRECISE_MESH_BOUNDING_BOXES}
type
  TGI_BBox_Calculator = class
  public
    Coord: TMFVec3f;
    CoordIndex: TMFLong;
    Geometry: TAbstractGeometryNode;
    WasSomeValidIndex: boolean;
    function GetVertexFromIndex(index: integer): TVector3;
    function GetIndex(indexNum: integer): integer;
  end;

function TGI_BBox_Calculator.GetVertexFromIndex(index: integer): TVector3;
begin
  { na pewno index >= 0, CalculateBoundingBoxFromIndices nie bedzie
    pytalo o indeksy ujemne. }
  if Index < Coord.Count then
  begin
    WasSomeValidIndex := true;
    Result := Coord.Items.List^[Index];
  end else
  begin
    CoordIndex.WritelnWarning_WrongVertexIndex(
      Geometry.X3DType, Index, Coord.Count);
    Result := TVector3.Zero;
  end;
end;

function TGI_BBox_Calculator.GetIndex(IndexNum: integer): integer;
begin
  { nie musimy sprawdzac czy indexNum jest dobry -
    CalculateBoundingBoxFromIndices bedzie pytalo tylko o indeksy between
    0..VertIndices.Count-1 }
  Result := CoordIndex.Items.List^[IndexNum];
end;
{$endif CASTLE_PRECISE_MESH_BOUNDING_BOXES}

function Coords_BoundingBox(State: TX3DGraphTraverseState;
  Coord: TMFVec3f): TBox3D; forward;
function Coords_LocalBoundingBox(State: TX3DGraphTraverseState;
  Coord: TMFVec3f): TBox3D; forward;

function IndexedCoords_BoundingBox(Geometry: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  Coord: TMFVec3f; CoordIndex: TMFLong): TBox3D;
{$ifndef CASTLE_PRECISE_MESH_BOUNDING_BOXES}
begin
  Result := Coords_BoundingBox(State, Coord);
{$else}
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
      {$ifdef FPC} @ {$endif} Calculator.GetIndex,
      CoordIndex.Count,
      {$ifdef FPC} @ {$endif} Calculator.GetVertexFromIndex,
      State.Transformation.Transform);
    if not Calculator.WasSomeValidIndex then
      Result := TBox3D.Empty;
  finally Calculator.Free end;
{$endif}
end;

function IndexedCoords_LocalBoundingBox(Geometry: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  Coord: TMFVec3f; CoordIndex: TMFLong): TBox3D;
{$ifndef CASTLE_PRECISE_MESH_BOUNDING_BOXES}
begin
  Result := Coords_LocalBoundingBox(State, Coord);
{$else}
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
      {$ifdef FPC} @ {$endif} Calculator.GetIndex,
      CoordIndex.Count,
      {$ifdef FPC} @ {$endif} Calculator.GetVertexFromIndex);
    if not Calculator.WasSomeValidIndex then
      Result := TBox3D.Empty;
  finally Calculator.Free end;
{$endif}
end;

function Coords_BoundingBox(State: TX3DGraphTraverseState;
  Coord: TMFVec3f): TBox3D;
{$ifndef CASTLE_PRECISE_MESH_BOUNDING_BOXES}
begin
  Result := Coords_LocalBoundingBox(State, Coord).Transform(
    State.Transformation.Transform);
{$else}
begin
  Result := CalculateBoundingBox(Coord.Items.L,
    Coord.Items.Count, 0, State.Transformation.Transform);
{$endif}
end;

function Coords_LocalBoundingBox(State: TX3DGraphTraverseState;
  Coord: TMFVec3f): TBox3D;
begin
  Result := CalculateBoundingBox(PVector3(Coord.Items.L),
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
  {$ifndef CASTLE_PRECISE_MESH_BOUNDING_BOXES}
  if (State.ShapeNode <> nil) and
     (not State.ShapeNode.BBox.IsEmpty) then
  begin
    Result := State.ShapeNode.BBox.Transform(State.Transformation.Transform); // use precalculated bounding box, fastest
  end else
  {$endif}
  if InternalCoord(State, C) then
  begin
    if C <> nil then
    begin
      if CoordIndexField <> nil then
        Result := IndexedCoords_BoundingBox(Self, State, C, CoordIndexField)
      else
        Result := Coords_BoundingBox(State, C);
    end else
      Result := TBox3D.Empty;
  end else
  begin
    if ProxyGeometry <> nil then
      Result := CallProxyBoundingBox(ProxyGeometry, ProxyState)
    else
      Result := LocalBoundingBox(State, ProxyGeometry, ProxyState).Transform(State.Transformation.Transform);
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
  if (State.ShapeNode <> nil) and
     (not State.ShapeNode.BBox.IsEmpty) then
  begin
    Result := State.ShapeNode.BBox; // use precalculated bounding box, fastest
  end else
  if InternalCoord(State, C) then
  begin
    if C <> nil then
    begin
      if CoordIndexField <> nil then
        Result := IndexedCoords_LocalBoundingBox(Self, State, C, CoordIndexField) else
        Result := Coords_LocalBoundingBox(State, C);
    end else
      Result := TBox3D.Empty;
  end else
  begin
    if ProxyGeometry <> nil then
      Result := CallProxyLocalBoundingBox(ProxyGeometry, ProxyState) else
    begin
      NewState := TX3DGraphTraverseState.CreateCopy(State);
      try
        NewState.Transformation.Init;
        result := BoundingBox(NewState, ProxyGeometry, ProxyState);
      finally NewState.Free end;
    end;
  end;
end;

{ ------------------------------------------------------------------------ }

function Text_LocalBoundingBox(State: TX3DGraphTraverseState;
  const Size, Spacing: Single;
  const Justify, JustifyMinor: TX3DFontJustify;
  const Font: TCastleFont;
  const Strings: TCastleStringList): TBox3D;
var
  YScale, XScale, MaxRowWidth, MinX, MinY, MaxX, MaxY, Descend: Single;
  I: integer;
begin
  YScale := Size / Font.Height;
  { TODO: Use maxEntent, length for X3D and VRML 2.0. Use width for VRML 1.0. }
  XScale := YScale;

  MaxRowWidth := 0;
  for I := 0 to Strings.Count - 1 do
    MaxVar(MaxRowWidth, Font.TextWidth(Strings[I]) * XScale);

  case Justify of
    fjBegin, fjFirst :
              begin MinX := 0;                 MaxX := MaxRowWidth end;
    fjMiddle: begin MinX := - MaxRowWidth / 2; MaxX := MaxRowWidth / 2 end;
    fjEnd   : begin MinX := - MaxRowWidth;     MaxX := 0 end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('Text_LocalBoundingBox: Invalid FontStyle Justify value');
    {$endif}
  end;
  case JustifyMinor of
    fjFirst : begin MaxY := 1;                   MinY := - (Strings.Count - 1); end;
    fjBegin : begin MaxY := 0;                   MinY := - Strings.Count; end;
    fjMiddle: begin MaxY := - Strings.Count / 2; MinY := -MaxY; end;
    fjEnd   : begin MaxY :=   Strings.Count;     MinY := 0; end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('Text_LocalBoundingBox: Invalid FontStyle JustifyMinor value');
    {$endif}
  end;
  MinY := MinY * (Size * Spacing);
  MaxY := MaxY * (Size * Spacing);

  // for JustifyMinor = fjFirst, the level 0 is at baseline, so lower bbox
  if JustifyMinor = fjFirst then
  begin
    Descend := Size * Font.DescenderHeight / Font.Height;
    MinY := MinY - (Descend * Spacing);
    MaxY := MaxY - (Descend * Spacing);
  end;

  Result := Box3D(
    Vector3(MinX, MinY, 0),
    Vector3(MaxX, MaxY, 0));
end;

function TAsciiTextNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Text_LocalBoundingBox(State,
    State.VRML1State.FontStyle.FdSize.Value,
    FdSpacing.Value,
    Justify, fjFirst,
    State.VRML1State.FontStyle.Font,
    FdString.Items);
end;

function TTextNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  Size, Spacing: Single;
  Justify, JustifyMinor: TX3DFontJustify;
begin
  if FontStyle is TFontStyleNode then // also checks FontStyle <> nil
  begin
    Size := TFontStyleNode(FontStyle).FdSize.Value;
    Spacing := TFontStyleNode(FontStyle).FdSpacing.Value;
    Justify := TFontStyleNode(FontStyle).Justify;
    JustifyMinor := TFontStyleNode(FontStyle).JustifyMinor;
  end else
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
    JustifyMinor := TFontStyleNode.DefaultJustifyMinor;
  end;

  Result := Text_LocalBoundingBox(State,
    Size, Spacing, Justify, JustifyMinor, Font, FdString.Items);
end;

function TTextNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { The default implementation in TAbstractGeometryNode.BoundingBox would
    use ProxyGeometry (generated by TextProxy) to calculate bbox.

    We override it, because we really prefer to use Text_LocalBoundingBox,
    because it will work OK even when maxDisplayChars is used.
    maxDisplayChars should change the look,
    but we don't want it to change the bounding box of the scene.
    This way animating the text (like in demo-models
    https://github.com/castle-engine/demo-models/blob/master/animation/simple_tests_interpolation_and_time/animate_text_max_display_chars.x3dv
    or in "The Unholy Society") doesn't change the text bounding box,
    making camera and all derived sizes "constant" regardless of maxDisplayChars. }

  Result := LocalBoundingBox(State, ProxyGeometry, ProxyState).Transform(State.Transformation.Transform);
end;

function Cone_BoundingBox(State: TX3DGraphTraverseState;
  const BottomRadius, Height: Single;
  const Sides, Bottom, Local: boolean): TBox3D;
var points: array [0..4] of TVector3;
    firstPoint: integer;
begin
 {liczymy bounding box takiego ostroslupa o podstawie z kwadratu.
  Tak jest prosto (po prostu 5 punktow); jasne ze w latwo sobie wyobrazic
  taka transformacje ze wyliczony w ten sposob bounding box jest nieco
  za duzy - ale wydaje mi sie ze nie jest tak zle.}
 points[0] := Vector3(0, Height/2, 0);
 points[1] := Vector3(-BottomRadius, -Height/2, -BottomRadius);
 points[2] := Vector3(-BottomRadius, -Height/2, +BottomRadius);
 points[3] := Vector3(+BottomRadius, -Height/2, -BottomRadius);
 points[4] := Vector3(+BottomRadius, -Height/2, +BottomRadius);
 if Sides then
  firstPoint := 0 else
 if Bottom then
  firstPoint := 1 { nie uwzgledniaj pierwszego punktu = szczytu stozka } else
  begin result := TBox3D.Empty; exit end;

 if Local then
   Result := CalculateBoundingBox(@points[firstPoint], High(points)+1-firstPoint, 0)
 else
   Result := CalculateBoundingBox(@points[firstPoint], High(points)+1-firstPoint, 0, State.Transformation.Transform);
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TConeNode_1.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM], false);
end;
{$warnings on}

function TConeNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdSide.Value, FdBottom.Value, false);
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TConeNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cone_BoundingBox(State,
    FdBottomRadius.Value, FdHeight.Value,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM], true);
end;
{$warnings on}

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
  result.Data[0] := Vector3(-SizeX/2, -SizeY/2, -SizeZ/2);
  result.Data[1] := Vector3( SizeX/2,  SizeY/2,  SizeZ/2);
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
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transformation.Transform);
end;

function TBoxNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transformation.Transform);
end;

function Cylinder_LocalBoundingBox(State: TX3DGraphTraverseState;
  const Height, Radius: Single;
  const Bottom, Side, Top: boolean): TBox3D;
var points: array[0..3]of TVector3;
begin
 {points 0 = bottom start, 1 = bottom end}
 points[0] := Vector3(-Radius, -Height/2, -Radius);
 points[1] := Vector3(+Radius, -Height/2, +Radius);
 {points 2 = top start, 3 = top end}
 points[2] := Vector3(-Radius,  Height/2, -Radius);
 points[3] := Vector3(+Radius,  Height/2, +Radius);

 if Side or (Top and Bottom) then
  result := Box3D(points[0], points[3]) else
 if Top then
  result := Box3D(points[2], points[3]) else
 if Bottom then
  result := Box3D(points[0], points[1]) else
  result := TBox3D.Empty;
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TCylinderNode_1.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := Cylinder_LocalBoundingBox(State,
    FdHeight.Value, FdRadius.Value,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP]);
end;
{$warnings on}

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
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transformation.Transform);
end;

function TCylinderNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transformation.Transform);
end;

function Sphere_LocalBoundingBox(const Radius: Single): TBox3D;
begin
  Result := Box3D(
    Vector3(-Radius, -Radius, -Radius),
    Vector3(+Radius, +Radius, +Radius) );
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
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transformation.Transform);
end;

function TSphereNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Override this, to avoid inherited using slower Proxy method. }
  Result := LocalBoundingBox(State, nil, nil).Transform(State.Transformation.Transform);
end;

{ TElevationGridNode --------------------------------------------------------- }

function TElevationGridNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  Y: Single;
  I: Integer;
begin
  Result := TBox3D.Empty;
  if IsNotEmpty then
  begin
    Result.Data[0].X := 0;
    Result.Data[0].Z := 0;
    Result.Data[1].X := FdXSpacing.Value * (FdXDimension.Value - 1);
    Result.Data[1].Z := FdZSpacing.Value * (FdZDimension.Value - 1);

    { now calculate Result.Data[0].Y and Result.Data[1].Y }
    Y := FdHeight.Items.List^[0];
    Result.Data[0].Y := Y;
    Result.Data[1].Y := Y;
    for I := 1 to FdXDimension.Value * FdZDimension.Value - 1 do
    begin
      Y := FdHeight.Items.List^[I];
      MinVar(Result.Data[0].Y, Y);
      MaxVar(Result.Data[1].Y, Y);
    end;
  end;
end;

{ TExtrusionNode --------------------------------------------------------- }

function TExtrusionNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  I, J: Integer;
  E: TVRMLExtrusion;
  LastY, LastZ: TVector3;
  Transform: TMatrix4;
  Point2d: PVector2;
begin
  Result := TBox3D.Empty;

  E := TVRMLExtrusion.Create;
  try
    E.Node := Self;
    for I := 0 to E.High do
    begin
      E.SpineTransformVar(I, LastY, LastZ, Transform);
      for J := 0 to FdCrossSection.Count - 1 do
      begin
        Point2d := PVector2(FdCrossSection.Items.Ptr(J));
        Result.Include(Transform.MultPoint(
          Vector3(Point2d^[0], 0, Point2d^[1])));
      end;
    end;
  finally FreeAndNil(E) end;
end;

function TExtrusionNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
var
  I, J: Integer;
  E: TVRMLExtrusion;
  LastY, LastZ: TVector3;
  Transform: TMatrix4;
  Point2d: PVector2;
begin
  Result := TBox3D.Empty;

  E := TVRMLExtrusion.Create;
  try
    E.Node := Self;
    for I := 0 to E.High do
    begin
      E.SpineTransformVar(I, LastY, LastZ, Transform);
      for J := 0 to FdCrossSection.Count - 1 do
      begin
        Point2d := PVector2(FdCrossSection.Items.Ptr(J));
        Result.Include(State.Transformation.Transform.MultPoint(
          Transform.MultPoint(
            Vector3(Point2d^[0], 0, Point2d^[1]))));
      end;
    end;
  finally FreeAndNil(E) end;
end;

{ eof ------------------------------------------------------------------------ }
