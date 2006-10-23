{
  Copyright 2004 Michalis Kamburelis.

  This file is part of "Kambi's 3dgraph Pascal units".

  "Kambi's 3dgraph Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dgraph Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dgraph Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Calculating convex hull.) }

unit ConvexHullUnit;

interface

uses VectorMath, KambiUtils, Math;

{ Calculates ConvexHull ignoring Z coordinates of pixels
  (i.e., Points[i][2] are ignored for every i).
  Returns newly created array with the indices to Points,
  i.e. if you want to draw an edge of convex hull,
  you want to iterate over points (for each i) Points[Result[i]]).

  Points.Count must be >= 1. }
function ConvexHull(Points: TDynVector3SingleArray): TDynIntegerArray;

implementation

function ConvexHull(Points: TDynVector3SingleArray): TDynIntegerArray;

{ this is the Jarvis algorithm, based on description in Cormen's
  "Introduction to alg." }

var InResult:TDynBooleanArray;

  function FindNext(Start:Integer; var NextI:Integer; RightSide:boolean):boolean;
  { Starting from Points[Start], knowing that InResult[Start],
    find next vertex on convex hull. If RightSide then we're moving from
    lowest vertex to highest, walking over the right edge of the convex hull.
    Else we're moving from highest to lowest, walking over the left edge
    of hull.

    Return false if RightSide and Start is the highest vertex,
    or (not RightSide) and Start is the lowest vertex.
    Else sets Next as appropriate and returns true. 
    
    Returned Next for SURE has InResult[Next] = false. }
  var MaxCotanAngle, ThisCotan:Single;
      MaxCotanAngleI, i:Integer;
  begin
   MaxCotanAngle:=-MaxSingle;
   MaxCotanAngleI:=-1;
   for i:=0 to Points.Count-1 do
    if not InResult[i] then
    begin
     if FloatsEqual(Points.Items[i][1], Points.Items[Start][1]) then
     begin
      if RightSide = (Points.Items[i][0] > Points.Items[Start][0]) then
      begin
       MaxCotanAngle:=MaxSingle;
       MaxCotanAngleI:=i;
      end;
     end else
     if RightSide = (Points.Items[i][1] > Points.Items[Start][1]) then
     begin
      ThisCotan:=(Points.Items[i][0] - Points.Items[Start][0]) /
                 (Points.Items[i][1] - Points.Items[Start][1]);
      if ThisCotan > MaxCotanAngle then
      begin
       MaxCotanAngle:=ThisCotan;
       MaxCotanAngleI:=i;
      end;
     end;
    end;

   Result:=MaxCotanAngleI <> -1;
   if Result then NextI:=MaxCotanAngleI;
  end;

  procedure MarkNext(i:Integer);
  begin
   InResult[i]:=true;
   Result.AppendItem(i);
  end;

var MinY:Single;
    i0, i, NextI:Integer;
begin
 Assert(Points.Count >= 1);

 { find i0, index of lowest point in Points }
 MinY:=Points.Items[0][1];
 i0:=0;
 for i:=1 to Points.Count-1 do
  if Points.Items[i][1] < MinY then
  begin
   MinY:=Points.Items[i][1];
   i0:=i;
  end;

 InResult:=TDynBooleanArray.Create(Points.Count);
 try
  InResult.SetAll(false);
  Result:=TDynIntegerArray.Create;
  try 
   MarkNext(i0);  
  
   i:=i0;
   while FindNext(i, NextI, true ) do begin i:=NextI; MarkNext(i); end;
   while FindNext(i, NextI, false) do begin i:=NextI; MarkNext(i); end;
   
  except Result.Free; raise end;
 finally InResult.Free end;
end;

end.
