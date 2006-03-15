{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ VRMLNodes_BoundingBoxes, czyli implementacja metod BoundingBox
  dla node'ow wywodzacych sie od TNodeGeneralShape. }

{ TNodeGeneralShape --------------------------------------------------------- }

function TNodeGeneralShape.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
 result := BoundingBoxTransform(LocalBoundingBox(State), State.CurrMatrix);
end;

function TNodeGeneralShape.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var NewState: TVRMLGraphTraverseState;
begin
 NewState := TVRMLGraphTraverseState.CreateCopy(State);
 try
  NewState.CurrMatrix := IdentityMatrix4Single;
  result := BoundingBox(NewState);
 finally NewState.Free end;
end;

{ ------------------------------------------------------------------------ }

function TNodeAsciiText.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var points: array[0..3]of TVector3Single;
    descend, rowh, yscale, maxRowWidth, minx, miny, maxx, maxy: Single;
    ttfont: PTrueTypeFont;
    i: integer;
begin
 ttfont := State.LastNodes.FontStyle.TTF_Font;

 rowh := State.LastNodes.FontStyle.FdSize.Value;
 yscale := rowh / TTFontSimpleRowHeight(ttfont);
 descend := TTFontSimpleDescend(ttfont)*yscale;

 maxRowWidth := 0;
 for i := 0 to FdString.Count-1 do
  maxRowWidth := KambiUtils.max(maxRowWidth,
    TTFontTextWidth(ttfont, FdString.Items.Items[i])*yscale{sorry-use here FdWidth});

 maxy := rowh-descend;
 miny := -(FdString.Count-1)*FdSpacing.Value*rowh - descend;
 case FdJustification.Value of
  JUSTIFICATION_LEFT   :begin minx := 0;              maxx := maxRowWidth end;
  JUSTIFICATION_CENTER :begin minx := -maxRowWidth/2; maxx := maxRowWidth/2 end;
  JUSTIFICATION_RIGHT  :begin minx := -maxRowWidth;   maxx := 0 end;
  else raise EInternalError.Create('Invalid FdJustification.Value');
 end;

 points[0] := Vector3Single(minx, maxy, 0);
 points[1] := Vector3Single(maxx, maxy, 0);
 points[2] := Vector3Single(minx, miny, 0);
 points[3] := Vector3Single(maxx, miny, 0);

 result := CalculateBoundingBox(@points, 4, 0, State.CurrMatrix);
end;

function TNodeCone.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var points: array[0..4]of TVector3Single;
    firstPoint: integer;
begin
 {liczymy bounding box takiego ostroslupa o podstawie z kwadratu.
  Tak jest prosto (po prostu 5 punktow); jasne ze w latwo sobie wyobrazic
  taka transformacje ze wyliczony w ten sposob bounding box jest nieco
  za duzy - ale wydaje mi sie ze nie jest tak zle.}
 points[0] := Vector3Single(0, FdHeight.Value/2, 0);
 points[1] := Vector3Single(-FdBottomRadius.Value, -FdHeight.Value/2, -FdBottomRadius.Value);
 points[2] := Vector3Single(-FdBottomRadius.Value, -FdHeight.Value/2, +FdBottomRadius.Value);
 points[3] := Vector3Single(+FdBottomRadius.Value, -FdHeight.Value/2, -FdBottomRadius.Value);
 points[4] := Vector3Single(+FdBottomRadius.Value, -FdHeight.Value/2, +FdBottomRadius.Value);
 if FdParts.Flags[CONE_PARTS_SIDES] then
  firstPoint := 0 else
 if FdParts.Flags[CONE_PARTS_BOTTOM] then
  firstPoint := 1 { nie uwzgledniaj pierwszego punktu = szczytu stozka } else
  begin result := EmptyBox3d; exit end;

 result := CalculateBoundingBox(
   @points[firstPoint],
   High(points)+1-firstPoint, 0, State.CurrMatrix);
end;

function TNodeCube.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
 result[0] := Vector3Single(-FdWidth.Value/2, -FdHeight.Value/2, -FdDepth.Value/2);
 result[1] := Vector3Single( FdWidth.Value/2,  FdHeight.Value/2,  FdDepth.Value/2);
end;

function TNodeCylinder.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var points: array[0..3]of TVector3Single;
begin
 {points 0 = bottom start, 1 = bottom end}
 points[0] := Vector3Single(-FdRadius.Value, -FdHeight.Value/2, -FdRadius.Value);
 points[1] := Vector3Single(+FdRadius.Value, -FdHeight.Value/2, +FdRadius.Value);
 {points 2 = top start, 3 = top end}
 points[2] := Vector3Single(-FdRadius.Value,  FdHeight.Value/2, -FdRadius.Value);
 points[3] := Vector3Single(+FdRadius.Value,  FdHeight.Value/2, +FdRadius.Value);

 if FdParts.Flags[CYLINDER_PARTS_SIDES] or
   (FdParts.Flags[CYLINDER_PARTS_TOP] and FdParts.Flags[CYLINDER_PARTS_BOTTOM]) then
  result := Box3d(points[0], points[3]) else
 if FdParts.Flags[CYLINDER_PARTS_TOP] then
  result := Box3d(points[2], points[3]) else
 if FdParts.Flags[CYLINDER_PARTS_BOTTOM] then
  result := Box3d(points[0], points[1]) else
  result := EmptyBox3d;
end;

function TNodeSphere.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
begin
 result := Box3d(
   Vector3Single(-FdRadius.Value, -FdRadius.Value, -FdRadius.Value),
   Vector3Single(+FdRadius.Value, +FdRadius.Value, +FdRadius.Value) );
end;

{sorry- TNodeSphere, Cylinder, Cone.BoundingBox moze dac wiekszy box niz trzeba. }

{ TNodeGeneralIndexed -------------------------------------------------------- }

type
  TGI_BBox_Calculator = class
  private
    LastCoordinate3: TNodeCoordinate3;
    CoordIndex: PArray_Longint;
    function GetVertexFromIndex(index: integer): TVector3Single;
    function GetIndex(indexNum: integer): integer;
  end;

function TGI_BBox_Calculator.GetVertexFromIndex(index: integer): TVector3Single;
begin
 {na pewno index >= 0, CalculateBoundingBoxFromIndices nie bedzie
  pytalo o indeksy ujemne.}
 if index >= LastCoordinate3.FdPoint.Count then
  raise EVRMLError.CreateFmt('Wrong vertex index in Indexed Node '+
    '(not enouch points in Coordinate3 defined : index is %d, we have only '+
    '%d)', [index, LastCoordinate3.FdPoint.Count]);
 result := LastCoordinate3.FdPoint.Items.Items[index];
end;

function TGI_BBox_Calculator.GetIndex(indexNum: integer): integer;
begin
 {nie musimy sprawdzac czy indexNum jest dobry -
   CalculateBoundingBoxFromIndices bedzie pytalo tylko o indeksy spomiedzy
   0..VertIndices.Count-1 }
 result := CoordIndex^[indexNum];
end;

function TNodeGeneralIndexed.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var Calculator: TGI_BBox_Calculator;
begin
 Calculator := TGI_BBox_Calculator.Create;
 try
  Calculator.LastCoordinate3 := State.LastNodes.Coordinate3;
  Calculator.CoordIndex := FdCoordIndex.Items.ItemsArray;
  result := CalculateBoundingBoxFromIndices(
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetIndex,
    FdCoordIndex.Count,
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexFromIndex,
    State.CurrMatrix);
 finally Calculator.Free end;
end;

function TNodeGeneralIndexed.LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var Calculator: TGI_BBox_Calculator;
begin
 Calculator := TGI_BBox_Calculator.Create;
 try
  Calculator.LastCoordinate3 := State.LastNodes.Coordinate3;
  Calculator.CoordIndex := FdCoordIndex.Items.ItemsArray;
  result := CalculateBoundingBoxFromIndices(
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetIndex,
    FdCoordIndex.Count,
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexFromIndex);
 finally Calculator.Free end;
end;

{ TNodePointSet ------------------------------------------------------------- }

type
  TPS_BBox_Calculator = class
    Verts: TMFVec3f;
    indexOffset: integer;
    PMatrix: PMatrix4Single;
    function GetVertex(i: integer): TVector3Single;
  end;
  function TPS_BBox_Calculator.GetVertex(i: integer): TVector3Single;
  begin result := MultMatrixPoint(PMatrix^, Verts.Items.Items[i+indexOffset]) end;

function TNodePointSet.BoundingBox(State: TVRMLGraphTraverseState): TBox3d;
var startIndex, numPoints: integer;
    Calculator: TPS_BBox_Calculator;
begin
 CalculateRange(State.LastNodes.Coordinate3, startIndex, numPoints);
 Calculator := TPS_BBox_Calculator.Create;
 try
  Calculator.Verts := State.LastNodes.Coordinate3.FdPoint;
  Calculator.indexOffset := startIndex;
  Calculator.PMatrix := @State.CurrMatrix;
  result := CalculateBoundingBox(
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertex, numPoints);
 finally Calculator.Free end;
end;

{ eof ------------------------------------------------------------------------- }
