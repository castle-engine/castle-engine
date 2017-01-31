{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Calculating convex hull.)

  TODO: This is an old and abandoned unit.
  The ConvexHull utility should just be moved to TVector3SingleList method?
}

unit CastleConvexHull;

{$I castleconf.inc}

interface

uses CastleVectors, CastleUtils, Math;

{ Calculates ConvexHull ignoring Z coordinates of pixels.
  That is, all Points[*][2] are ignored.
  Returns newly created array with the indices to Points.
  If you want to draw an edge of convex hull,
  you want to iterate over these points like (for each i) Points[Result[i]]).

  Points.Count must be >= 1. }
function ConvexHull(Points: TVector3SingleList): TIntegerList;

implementation

function ConvexHull(Points: TVector3SingleList): TIntegerList;

{ this is the Jarvis algorithm, based on description in Cormen's
  "Introduction to alg." }

var InResult: TBooleanList;

  function FindNext(Start: Integer; var NextI: Integer; RightSide: boolean): boolean;
  { Starting from Points[Start], knowing that InResult[Start],
    find next vertex on convex hull. If RightSide then we're moving from
    lowest vertex to highest, walking over the right edge of the convex hull.
    Else we're moving from highest to lowest, walking over the left edge
    of hull.

    Return false if RightSide and Start is the highest vertex,
    or (not RightSide) and Start is the lowest vertex.
    Else sets Next as appropriate and returns true.

    Returned Next for SURE has InResult[Next] = false. }
  var MaxCotanAngle, ThisCotan: Single;
      MaxCotanAngleI, i: Integer;
  begin
   MaxCotanAngle := -MaxSingle;
   MaxCotanAngleI := -1;
   for i := 0 to Points.Count-1 do
    if not InResult[i] then
    begin
     if FloatsEqual(Points.L[i][1], Points.L[Start][1]) then
     begin
      if RightSide = (Points.L[i][0] > Points.L[Start][0]) then
      begin
       MaxCotanAngle := MaxSingle;
       MaxCotanAngleI := i;
      end;
     end else
     if RightSide = (Points.L[i][1] > Points.L[Start][1]) then
     begin
      ThisCotan:=(Points.L[i][0] - Points.L[Start][0]) /
                 (Points.L[i][1] - Points.L[Start][1]);
      if ThisCotan > MaxCotanAngle then
      begin
       MaxCotanAngle := ThisCotan;
       MaxCotanAngleI := i;
      end;
     end;
    end;

   Result := MaxCotanAngleI <> -1;
   if Result then NextI := MaxCotanAngleI;
  end;

  procedure MarkNext(i: Integer);
  begin
   InResult[i] := true;
   Result.Add(i);
  end;

var MinY: Single;
    i0, i, NextI: Integer;
begin
 Assert(Points.Count >= 1);

 { find i0, index of lowest point in Points }
 MinY := Points.L[0][1];
 i0 := 0;
 for i := 1 to Points.Count-1 do
  if Points.L[i][1] < MinY then
  begin
   MinY := Points.L[i][1];
   i0 := i;
  end;

 InResult := TBooleanList.Create;
 try
  InResult.Count := Points.Count; { TFPGList already initializes all to false }
  Result := TIntegerList.Create;
  try
   MarkNext(i0);

   i := i0;
   while FindNext(i, NextI, true ) do begin i := NextI; MarkNext(i); end;
   while FindNext(i, NextI, false) do begin i := NextI; MarkNext(i); end;

  except Result.Free; raise end;
 finally InResult.Free end;
end;

end.
