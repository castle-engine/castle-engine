{%MainUnit castlevectors.pas}
{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleVectors routines (global functions, procedures)
  for Double precision of vectors and matrices. }

{$ifdef read_interface}

{ Intersection of three 3D planes, results in a single 3D point.
  If the intersection is not a single 3D point, result is undefined,
  so don't try to use this.
  @groupBegin }
function ThreePlanesIntersectionPointDouble(
  const Plane0, Plane1, Plane2: TVector4Double): TVector3Double; overload;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

{ Unfortunately, this is an ugly copy-paste of a subset
  of castlevectors_records_single.inc now.
  As Delphi does not support,
  nor does it support generics using mathematical expressions
  parameterized with floats (see castlevectors_records_float.inc
  comments, you cannot "narrow" generics to something like "numer"),
  ... there seems to be no better way now.

  Also, we need to rename functions to XxxDouble, to avoid FPC bugs
  with "duplicate ASM label", see
  https://bugs.freepascal.org/view.php?id=32188
}

function MaxAbsVectorCoordDouble(const v: TVector3Double): integer;
begin
  Result := 0;
  if Abs(v.Y) > Abs(v[Result]) then Result := 1;
  if Abs(v.Z) > Abs(v[Result]) then Result := 2;
end;

procedure TwoPlanesIntersectionLineDouble(const Plane0, Plane1: TVector4Double;
  out Line0, LineVector: TVector3Double);
var
  Plane0Dir: TVector3Double absolute Plane0;
  Plane1Dir: TVector3Double absolute Plane1;
  NonZeroIndex, Index1, Index2: T3DAxis;
  PlaneWithNonZeroIndex1: PVector4Double;
  PlaneMultiply, Sum_Index2, Sum_3: Double;
begin
  LineVector := TVector3Double.CrossProduct(Plane0Dir, Plane1Dir);

  NonZeroIndex := MaxAbsVectorCoordDouble(LineVector);
  if IsZero(LineVector[NonZeroIndex]) then
    raise EPlanesParallel.Create(
      'Unable to calculate intersection line of two planes ' +
      Plane0.ToRawString + ' and ' + Plane1.ToRawString + ' because ' +
      'planes are parallel');

  { Since LineVector[NonZeroIndex] <> 0, we know that we can find exactly
    one point on this line by assuming that Point[NonZeroIndex] = 0. }
  Line0.Data[NonZeroIndex] := 0;
  RestOf3DCoords(NonZeroIndex, Index1, Index2);

  { Now we must solve
      Plane0[Index1] * Line0[Index1] + Plane0[Index2] * Line0[Index2] + Plane0.W = 0
      Plane1[Index1] * Line0[Index1] + Plane1[Index2] * Line0[Index2] + Plane1.W = 0
    We want to sum these two equations to eliminate Line0[Index1]:
      0                              + Sum_Index2     * Line0[Index2] + Sum_3 = 0
  }
  if not IsZero(Plane0[Index1]) then
  begin
    PlaneWithNonZeroIndex1 := @Plane0;
    PlaneMultiply := - Plane1[Index1] / Plane0[Index1];
    Sum_Index2 := Plane0[Index2] * PlaneMultiply + Plane1[Index2];
    Sum_3      := Plane0.W      * PlaneMultiply + Plane1.W;
  end else
  if not IsZero(Plane1[Index1]) then
  begin
    PlaneWithNonZeroIndex1 := @Plane1;
    PlaneMultiply := - Plane0[Index1] / Plane1[Index1];
    Sum_Index2 := Plane0[Index2] + Plane1[Index2] * PlaneMultiply;
    Sum_3      := Plane0.W      + Plane1.W      * PlaneMultiply;
  end else
  begin
    { If Plane0[Index1] = Plane1[Index1] = 0, this is simple.
        Sum_Index2 := Plane0[Index2] + Plane1[Index2];
        Sum_3      := Plane0.W      + Plane1.W     ;
        PlaneWithNonZeroIndex1 := ???;
      But it's useless, because then I will not be able to calculate
      Line0[Index1] (after obtaining Line0[Index2]).
      TODO -- some proof that this cannot happen for correct input ? }
    raise Exception.Create('Cannot calculate intersection line of two planes');
  end;

  { Now we know that
      Sum_Index2 * Line0[Index2] + Sum_3 = 0
    Sum_Index2 must be <> 0, since we know that Line0[Index2] must be uniquely
    determined ? Right ? TODO -- I'm not sure, how to prove this simply ?
  }
  Line0.Data[Index2] := - Sum_3 / Sum_Index2;

  { Note we have
      PlaneWithNonZeroIndex1^[Index1] * Line0[Index1] +
      PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
      PlaneWithNonZeroIndex1^.W = 0
    All is known except Line0[Index1],
    PlaneWithNonZeroIndex1^[Index1] is for sure <> 0. }
  Line0.Data[Index1] := -
    (PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
     PlaneWithNonZeroIndex1^.W) /
    PlaneWithNonZeroIndex1^[Index1];
end;

function TryPlaneLineIntersectionDouble(out t: Double;
  const plane: TVector4Double; const line0, lineVector: TVector3Double): boolean; overload;
var
  PlaneDir: TVector3Double absolute plane;
  Dot: Double;
begin
  Dot := TVector3Double.DotProduct(LineVector, PlaneDir);
  if not IsZero(Dot) then
  begin
    Result := true;
    t := -(plane.X*line0.X + plane.Y*line0.Y + plane.Z*line0.Z + plane.W)/Dot;
  end else
    Result := false;
end;

function TryPlaneLineIntersectionDouble(out intersection: TVector3Double;
  const plane: TVector4Double; const line0, lineVector: TVector3Double): boolean; overload;
var
  T: Double;
begin
  Result := TryPlaneLineIntersectionDouble(t, Plane, Line0, LineVector);
  if Result then Intersection := Line0 + LineVector * t;
end;

function ThreePlanesIntersectionPointDouble(
  const Plane0, Plane1, Plane2: TVector4Double): TVector3Double;
var
  Line0, LineVector: TVector3Double;
begin
  TwoPlanesIntersectionLineDouble(Plane0, Plane1, Line0, LineVector);
  if not TryPlaneLineIntersectionDouble(Result, Plane2, Line0, LineVector) then
    raise Exception.Create('Cannot calculate intersection point of three planes :' +
      'intersection line of first two planes is parallel to the 3rd plane');
end;

{$endif read_implementation}
