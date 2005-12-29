{
  Copyright 2003-2004 Michalis Kamburelis.

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

{ implementacja metod VerticesCount i TrianglesCount dla specyficznych
  potomkow TNodeGeneralShape }
  
{ just a shortcuts for names often used in this file }  
{$define QUADRIC_SLICES:=(State.LastNodes.KambiTriangulation.QuadricSlices)}
{$define QUADRIC_STACKS:=(State.LastNodes.KambiTriangulation.QuadricStacks)}
{$define RECT_DIVISIONS:=(State.LastNodes.KambiTriangulation.RectDivisions)}  

function TNodeAsciiText.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin result:=0;{sorry} end;

function TNodeAsciiText.VerticesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin result:=0;{sorry} end;

function TNodeCone.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 result:=0;
 if FdParts.Flags[CONE_PARTS_SIDES] then
 begin
  if OverTriangulate then
   result += QUADRIC_SLICES * ((QUADRIC_STACKS-1)*2 + 1) else
   result += QUADRIC_SLICES;
 end;
 if FdParts.Flags[CONE_PARTS_BOTTOM] then
  result += QUADRIC_SLICES;
end;

function TNodeCone.VerticesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 if OverTriangulate then
 begin
  if FdParts.Flags[CONE_PARTS_BOTTOM] and FdParts.Flags[CONE_PARTS_SIDES] then
   result:=QUADRIC_SLICES + 1 + (QUADRIC_STACKS-1)*QUADRIC_SLICES + 1 else
  if FdParts.Flags[CONE_PARTS_BOTTOM] then
   result:=QUADRIC_SLICES + 1 else
  if FdParts.Flags[CONE_PARTS_SIDES] then
   result:=QUADRIC_SLICES + (QUADRIC_STACKS-1)*QUADRIC_SLICES + 1 else
   result:=0;
 end else
 begin
  if FdParts.Flags[CONE_PARTS_BOTTOM] or FdParts.Flags[CONE_PARTS_SIDES] then
  begin
   result:=QUADRIC_SLICES;
   if FdParts.Flags[CONE_PARTS_BOTTOM] then Inc(result);
   if FdParts.Flags[CONE_PARTS_SIDES] then Inc(result);
  end else
   result:=0;
 end;
end;

function TNodeCube.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 if OverTriangulate then
  {6 scian, na kazdej Sqr(RECT_DIVISIONS+1) kwadracikow}
  result:=Sqr(RECT_DIVISIONS+1)*2 *6 else
  result:=6 * 2;
end;

function TNodeCube.VerticesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 if OverTriangulate then
  {6 scian, w srodku kazdej Sqr(RECT_DIVISIONS) vertexow,
   12 krawedzi, na srodku kazdej RECT_DIVISIONS vertexow,
   8 rogow }
  result:=6*Sqr(RECT_DIVISIONS) + 12*RECT_DIVISIONS + 8 else
  result:=8;
end;

function TNodeCylinder.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 result:=0;
 if FdParts.Flags[CYLINDER_PARTS_SIDES] then
 begin
  if OverTriangulate then
   result += 2 * QUADRIC_SLICES*QUADRIC_STACKS else
   result += 2 * QUADRIC_SLICES;
 end;
 if FdParts.Flags[CYLINDER_PARTS_TOP] then result += QUADRIC_SLICES;
 if FdParts.Flags[CYLINDER_PARTS_BOTTOM] then result += QUADRIC_SLICES;
end;

function TNodeCylinder.VerticesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 result:=0;
 if FdParts.Flags[CYLINDER_PARTS_SIDES] then
 begin
  if OverTriangulate then
   result += (QUADRIC_STACKS+1) * QUADRIC_SLICES else
   result += 2*QUADRIC_SLICES;
  if FdParts.Flags[CYLINDER_PARTS_TOP] then Inc(result);
  if FdParts.Flags[CYLINDER_PARTS_BOTTOM] then Inc(result);
 end else
 begin
  if FdParts.Flags[CYLINDER_PARTS_TOP] then result += QUADRIC_SLICES + 1;
  if FdParts.Flags[CYLINDER_PARTS_BOTTOM] then result += QUADRIC_SLICES + 1;
 end;
end;

function TNodeGeneralIndexed.VerticesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 result:=State.LastNodes.Coordinate3.FdPoint.Count;
end;

function TNodeIndexed_Faces_Or_Triangles.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
var startIndex,endIndex:integer;
begin
 startIndex:=0;
 result:=0;
 while startIndex<FdCoordIndex.Count do
 begin
  endIndex:=startIndex;
  while (endIndex<FdCoordIndex.Count) and
        (FdCoordIndex.Items.Items[endIndex]<>-1) do
   Inc(endIndex);

  result+= Cardinal(max(endIndex-startIndex-2,0));
  startIndex:=endIndex+1;
 end;
end;

function TNodeIndexedLineSet.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin result:=0; end;

function TNodePointSet.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin result:=0; end;

function TNodePointSet.VerticesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
var dummy, numPoints:integer;
begin
 CalculateRange(State.LastNodes.Coordinate3, dummy, numPoints);
 result:=numPoints;
end;

function TNodeSphere.TrianglesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 result:=QUADRIC_SLICES * ((QUADRIC_STACKS-2)*2 + 2);
end;

function TNodeSphere.VerticesCount(State:TVRMLGraphTraverseState; OverTriangulate:boolean):Cardinal;
begin
 result:=QUADRIC_SLICES * (QUADRIC_STACKS-1) + 2;
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}
