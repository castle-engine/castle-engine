{
  Copyright 2003-2008 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ dual implementation, czyli kod ktory piszemy jednoczesnie w dwoch
  wersjach : jedna robi wszystko na typach Single, druga na typach Double.
  Uzywamy tego pliku _DoubleImplementation.inc jak gigantycznego makra.

  NIE robimy tego jako makro zeby 1) uniknac zamieszania z pisaniem
  komentarzy w srodku tego makra, FPC 1.0.6 mialo tu pewne problemy,
  wymuszalo uzywanie zagniezdzonych komentarzy jesli chciales zrobic
  makra w srodku makr, a w rezultacie Delphi nie umialo takiego czegos
  nawet sparsowac, 2) FPC 1.0.6 nie umialo sobie chwilowo radzic z tak duzym
  makrem i musialem je dzielic na mniejsze kawalki. Gdy mam to wszystko
  w pliku to nie ma problemu. 3) FPC 1.0.6 nie wypisywalo ladnie komunikatow
  bledow kompilacji w makrze (nie bylo lineinfo).
  Ogolnie, FPC 1.0.6 mialo pare niedorobek ktore razem sugeruja zeby
  uzywac plikow $Include zamiast gigantycznych makr.

  Uzywaj w srodku tego wielkiego makra makr TYPE_SCALAR, TYPE_VECTOR3,
  TYPE_VECTOR4, TYPE_TRIANGLE3, TYPE_MATRIX4, SCALAR_EQUALITY_EPSILON
  (i ew. innych, patrz implementacja VectorMath przy wywolaniu $I Ten_Plik)
  aby odwolywac sie do odpowiednich typow.
}

procedure SwapValues(var V1, V2: TYPE_VECTOR2);
var
  Tmp: TYPE_VECTOR2;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

procedure SwapValues(var V1, V2: TYPE_VECTOR3);
var
  Tmp: TYPE_VECTOR3;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

procedure SwapValues(var V1, V2: TYPE_VECTOR4);
var
  Tmp: TYPE_VECTOR4;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

function VectorAverage(const V: TYPE_VECTOR3): TYPE_SCALAR;
begin
  Result := (V[0] + V[1] + V[2]) / 3;
end;

operator := (const V: TYPE_VECTOR2_OBJECT): TYPE_VECTOR2;
begin
  Result := V.Data;
end;

operator := (const V: TYPE_VECTOR3_OBJECT): TYPE_VECTOR3;
begin
  Result := V.Data;
end;

operator := (const V: TYPE_VECTOR4_OBJECT): TYPE_VECTOR4;
begin
  Result := V.Data;
end;

operator := (const V: TYPE_VECTOR2): TYPE_VECTOR2_OBJECT;
begin
  Result.Data := V;
end;

operator := (const V: TYPE_VECTOR3): TYPE_VECTOR3_OBJECT;
begin
  Result.Data := V;
end;

operator := (const V: TYPE_VECTOR4): TYPE_VECTOR4_OBJECT;
begin
  Result.Data := V;
end;

procedure NormalizeTo1st(var V: TYPE_VECTOR3);
var len: TYPE_SCALAR;
begin
 len := Sqrt( Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]) );
 if len = 0.0 then exit;
 V[0] /= len;
 V[1] /= len;
 V[2] /= len;
end;

procedure NormalizePlaneTo1st(var v: TYPE_VECTOR4);
var len: TYPE_SCALAR;
begin
 len := Sqrt( Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]) );
 V[0] /= len;
 V[1] /= len;
 V[2] /= len;
 V[3] /= len;
end;

function IsZeroVector(const V: TYPE_VECTOR3): boolean;
begin
 result := (Abs(V[0]) < SCALAR_EQUALITY_EPSILON) and
           (Abs(V[1]) < SCALAR_EQUALITY_EPSILON) and
           (Abs(V[2]) < SCALAR_EQUALITY_EPSILON);
end;

function IsZeroVector(const V: TYPE_VECTOR4): boolean;
begin
 result := (Abs(V[0]) < SCALAR_EQUALITY_EPSILON) and
           (Abs(V[1]) < SCALAR_EQUALITY_EPSILON) and
           (Abs(V[2]) < SCALAR_EQUALITY_EPSILON) and
           (Abs(V[3]) < SCALAR_EQUALITY_EPSILON);
end;

function VectorAdjustToLength(const v: TYPE_VECTOR3; VecLen: TYPE_SCALAR): TYPE_VECTOR3;
begin
 result := VectorScale(v, VecLen/VectorLen(v));
end;

procedure VectorAdjustToLengthTo1st(var v: TYPE_VECTOR3; VecLen: TYPE_SCALAR);
begin
 VectorScaleTo1st(v, VecLen/VectorLen(v));
end;

function VectorLen(const v: TYPE_VECTOR3): TYPE_SCALAR;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TYPE_VECTOR3): TYPE_SCALAR;
begin
  Result := Sqr(v[0]) + Sqr(v[1]) + Sqr(v[2]);
end;

function VectorLen(const v: TYPE_VECTOR2): TYPE_SCALAR;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TYPE_VECTOR2): TYPE_SCALAR;
begin
  Result := Sqr(v[0]) + Sqr(v[1]);
end;

function VectorExpComponents(const v: TYPE_VECTOR3; const Exp: TYPE_SCALAR): TYPE_VECTOR3;
begin
 result[0] := Power(v[0], Exp);
 result[1] := Power(v[1], Exp);
 result[2] := Power(v[2], Exp);
end;

procedure VectorExpComponentsTo1st(var v: TYPE_VECTOR3; const Exp: TYPE_SCALAR);
begin
 v[0] := Power(v[0], Exp);
 v[1] := Power(v[1], Exp);
 v[2] := Power(v[2], Exp);
end;

function CosAngleBetweenVectors(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
var
  LensSquared: Float;
begin
  (* jak widac, wykrecam sie jednym pierwiastkowaniem pierwiastkujac
     VectorLenSqr(v1) i VectorLenSqr(v2) jednoczesnie. *)

  LensSquared := VectorLenSqr(v1) * VectorLenSqr(v2);
  if LensSquared < SCALAR_EQUALITY_EPSILON then
    raise EVectorMathInvalidOp.Create(
      'Cannot calculate angle between vectors, at least one of the vectors is zero');

  (* musimy robic tu Clamp do (-1, 1) bo praktyka pokazala ze czasami na skutek
     bledow obliczen zmiennoprzec. wynik tej funkcji jest maciupinke poza
     zakresem. A Cosinum musi byc w zakresie -1..1, w szczegolnosci
     ArcCos() dla czegos choc troche poza zakresem wywala paskudny EInvalidArgument *)
  result := Clamped(
    VectorDotProduct(v1, v2) / Sqrt(LensSquared), -1.0, 1.0);
end;

function AngleRadBetweenVectors(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
begin
 result := ArcCos(CosAngleBetweenVectors(v1, v2));
end;

function CosAngleBetweenNormals(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
begin
 result := Clamped(VectorDotProduct(v1, v2), -1.0, 1.0);
end;

function AngleRadBetweenNormals(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
begin
 result := ArcCos(CosAngleBetweenNormals(v1, v2));
end;

function Normalized(const v: TYPE_VECTOR3): TYPE_VECTOR3;
begin
  result := v;
  NormalizeTo1st(result);
end;

function Vector_Get_Normalized(const V: TYPE_VECTOR3_OBJECT): TYPE_VECTOR3_OBJECT;
begin
  Result.Data := V.Data;
  NormalizeTo1st(Result.Data);
end;

procedure Vector_Normalize(var V: TYPE_VECTOR3_OBJECT);
begin
  NormalizeTo1st(V.Data);
end;

function RotatePointAroundAxisDeg(angleDeg: TYPE_SCALAR; const pt: TYPE_VECTOR3;
  const axisVec: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 result := RotatePointAroundAxisRad(DegToRad(angleDeg), pt, axisVec);
end;

function RotatePointAroundAxisRad(angleRad: TYPE_SCALAR; const pt: TYPE_VECTOR3;
  const axisVec: TYPE_VECTOR3): TYPE_VECTOR3;
(* ponizsza procedurka to tylko rozpisana i choc odrobinke zoptymalizowana
   wersja pomnozenia pt przez RotationMatrixDeg(angleDeg, axisVec). *)
var x, y,z, l: TYPE_SCALAR;
    sinAngle, cosAngle: Float;
begin
 SinCos(angleRad, sinAngle, cosAngle);
 l := VectorLen(axisVec);

 (* normalizuj vektor axisLen i jednoczesnie rozloz mi go na zmienne
    o przejrzystych nazwach x, y,z *)
 x := axisVec[0]/l;
 y := axisVec[1]/l;
 z := axisVec[2]/l;

 result[0] := (cosAngle + (1 - cosAngle) * x * x)     * pt[0]
           + ((1 - cosAngle) * x * y - z * sinAngle)  * pt[1]
           + ((1 - cosAngle) * x * z + y * sinAngle)  * pt[2];

 result[1] := ((1 - cosAngle) * x * y + z * sinAngle)  * pt[0]
           + (cosAngle + (1 - cosAngle) * y * y)       * pt[1]
           + ((1 - cosAngle) * y * z - x * sinAngle)   * pt[2];

 result[2] := ((1 - cosAngle) * x * z - y * sinAngle)  * pt[0]
           + ((1 - cosAngle) * y * z + x * sinAngle)   * pt[1]
           + (cosAngle + (1 - cosAngle) * z * z)       * pt[2];

(*
  Proba ujecia powyzszego jednym wzorem - niepelna bo nie ujalem
  - lub + przed upVec[ThirdOne()], ale to i tak bez znaczenia, wersja
  powyzsza jest bardziej optymalna bo juz rozwinieta.

   for i := 0 to 2 do
   begin
    newMoveVec[i] := (cosAngle + (1 - cosAngle) * upVec[i] * upVec[i]) * moveVec[i];
    for j := 0 to 2 do
     if j <> i do
      newMoveVec[i] := newMoveVec[i] +
       ((1 - cosAngle) * upVec[i] * upVec[j] - upVec[ThirdOne(i, j)] * sinAngle) * moveVec[j];
   end;
*)
end;

function MaxVectorCoord(const v: TYPE_VECTOR3): integer;
begin
 result := 0;
 (* dzieki temu ze testy ponizej wykonujemy w kolejnosci (
    zaczynamy z result = 0, potem v[1], potem v[2]) i ze wymagamy
    ostrej ">" przewagi zeby zwyciezyc mamy gwarantowane ze robimy
    zgodnie z interfejsem jesli chodzi o pierwszenstwo wspolrzednych. *)
 if v[1] > v[result] then result := 1;
 if v[2] > v[result] then result := 2;
end;

function MaxVectorCoord(const v: TYPE_VECTOR4): integer;
begin
 result := 0;
 if v[1] > v[result] then result := 1;
 if v[2] > v[result] then result := 2;
 if v[3] > v[result] then result := 3;
end;

function MaxAbsVectorCoord(const v: TYPE_VECTOR3): integer;
begin
 result := 0;
 (* dzieki temu ze testy ponizej wykonujemy w kolejnosci (
    zaczynamy z result = 0, potem v[1], potem v[2]) i ze wymagamy
    ostrej ">" przewagi zeby zwyciezyc mamy gwarantowane ze robimy
    zgodnie z interfejsem jesli chodzi o pierwszenstwo wspolrzednych. *)
 if Abs(v[1]) > Abs(v[result]) then result := 1;
 if Abs(v[2]) > Abs(v[result]) then result := 2;
end;

function PlaneDirInDirection(const Plane: TYPE_VECTOR4; const Direction: TYPE_VECTOR3): TYPE_VECTOR3;
var PlaneDir: TYPE_VECTOR3 absolute Plane;
begin
 result := PlaneDirInDirection(PlaneDir, Direction);
end;

function PlaneDirInDirection(const PlaneDir, Direction: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 (* "Normalny" sposob aby sprawdzic czy dwa wektory wskazuja z tej samej
   plaszczyzny to porownac
         VectorDotProduct(V1, PlaneDir) > 0
         VectorDotProduct(V2, PlaneDir) > 0
   czyli tak jakby obciac czwarta wspolrzedna plaszczyzny (zeby plaszczyzna
   przechodzila przez (0, 0,0)) i sprawdzic czy dwa punkty leza po tej samej
   stronie plaszczyzny

   (jezeli jeden z wektorow V1 lub V2 jest rownolegly do plaszczyzny,
    tzn. VectorDotProduct(V*, PlaneDir) = 0 to przyjmujemy ze drugi
    moze byc w dowolna strone, wiec nawet sie
    nie przejmujemy co bedzie gdy zajdzie rownosc w ktorejs z powyzszych
    nierownosci).

   Ale mozna to uproscic gdy V1 = PlaneDir. Wiemy ze
     VectorDotProduct(PlaneDir, PlaneDir) > 0
   bo to przeciez suma trzech kwadratow. Wiec wystarczy sprawdzic czy
     VectorDotProduct(Direction, PlaneDir) > 0
   - jesli nie to trzeba odwrocic Normal. *)
 if VectorDotProduct(Direction, PlaneDir) < 0 then
  result := VectorNegate(PlaneDir) else
  result := PlaneDir;
end;

function PlaneDirNotInDirection(const Plane: TYPE_VECTOR4; const Direction: TYPE_VECTOR3): TYPE_VECTOR3;
var PlaneDir: TYPE_VECTOR3 absolute Plane;
begin
 result := PlaneDirNotInDirection(PlaneDir, Direction);
end;

procedure TwoPlanesIntersectionLine(const Plane0, Plane1: TYPE_VECTOR4;
  out Line0, LineVector: TYPE_VECTOR3);
var
  Plane0Dir: TYPE_VECTOR3 absolute Plane0;
  Plane1Dir: TYPE_VECTOR3 absolute Plane1;
  NonZeroIndex, Index1, Index2: Integer;
  PlaneWithNonZeroIndex1: PTR_TYPE_VECTOR4;
  PlaneMultiply, Sum_Index2, Sum_3: TYPE_SCALAR;
begin
 LineVector := VectorProduct(Plane0Dir, Plane1Dir);

 NonZeroIndex := MaxAbsVectorCoord(LineVector);
 if IsZero(LineVector[NonZeroIndex]) then
  raise EPlanesParallel.Create(
    'Unable to calculate intersection line of two planes ' +
    VectorToRawStr(Plane0) + ' and ' + VectorToRawStr(Plane1) + ' because ' +
    'planes are parallel');

 { Since LineVector[NonZeroIndex] <> 0, we know that we can find exactly
   one point on this line by assuming that Point[NonZeroIndex] = 0. }
 Line0[NonZeroIndex] := 0;
 RestOf3dCoords(NonZeroIndex, Index1, Index2);

 { Now we must solve
     Plane0[Index1] * Line0[Index1] + Plane0[Index2] * Line0[Index2] + Plane0[3] = 0
     Plane1[Index1] * Line0[Index1] + Plane1[Index2] * Line0[Index2] + Plane1[3] = 0
   We want to sum these two equations to eliminate Line0[Index1]:
     0                                 + Sum_Index2        * Line0[Index2] + Sum_3        = 0
 }
 if not IsZero(Plane0[Index1]) then
 begin
  PlaneWithNonZeroIndex1 := @Plane0;
  PlaneMultiply := - Plane1[Index1] / Plane0[Index1];
  Sum_Index2 := Plane0[Index2] * PlaneMultiply + Plane1[Index2];
  Sum_3      := Plane0[3]      * PlaneMultiply + Plane1[3];
 end else
 if not IsZero(Plane1[Index1]) then
 begin
  PlaneWithNonZeroIndex1 := @Plane1;
  PlaneMultiply := - Plane0[Index1] / Plane1[Index1];
  Sum_Index2 := Plane0[Index2] + Plane1[Index2] * PlaneMultiply;
  Sum_3      := Plane0[3]      + Plane1[3]      * PlaneMultiply;
 end else
 begin
  { If Plane0[Index1] = Plane1[Index1] = 0, this is simple.
      Sum_Index2 := Plane0[Index2] + Plane1[Index2];
      Sum_3      := Plane0[3]      + Plane1[3]     ;
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
 Line0[Index2] := - Sum_3 / Sum_Index2;

 { Note we have
     PlaneWithNonZeroIndex1^[Index1] * Line0[Index1] +
     PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
     PlaneWithNonZeroIndex1^[3] = 0
   All is known except Line0[Index1],
   PlaneWithNonZeroIndex1^[Index1] is for sure <> 0. }
 Line0[Index1] := -
   (PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
    PlaneWithNonZeroIndex1^[3]) /
   PlaneWithNonZeroIndex1^[Index1];
end;

function Lines2DIntersection(const Line0, Line1: TYPE_VECTOR3): TYPE_VECTOR2;
var
  Ratio, Divide: TYPE_SCALAR;
begin
  { Only one from Line0[0], Line0[1] may be zero.
    Take larger one for numerical stability. }
  if Abs(Line0[0]) > Abs(Line0[1]) then
  begin
    Ratio := Line1[0] / Line0[0];

    { we have equations
        Line0[0] * x + Line0[1] * y + Line0[2] = 0
        Line1[0] * x + Line1[1] * y + Line1[2] = 0
      Multiply first equation by Ratio and subtract to 2nd one:
        y * (Line0[1] * Ratio - Line1[1]) + Line0[2] * Ratio - Line1[2] = 0 }
    Divide := Line0[1] * Ratio - Line1[1];
    if Divide = 0 then
      raise ELinesParallel.Create('Lines are parallel, Lines2DIntersection not possible');
    Result[1] := - (Line0[2] * Ratio - Line1[2]) / Divide;
    Result[0] := - (Line0[1] * Result[1] + Line0[2]) / Line0[0];
  end else
  begin
    Ratio := Line1[1] / Line0[1];

    { we have equations
        Line0[0] * x + Line0[1] * y + Line0[2] = 0
        Line1[0] * x + Line1[1] * y + Line1[2] = 0
      Multiply first equation by Ratio and subtract to 2nd one:
        x * (Line0[0] * Ratio - Line1[0]) + Line0[2] * Ratio - Line1[2] = 0 }
    Divide := Line0[0] * Ratio - Line1[0];
    if Divide = 0 then
      raise ELinesParallel.Create('Lines are parallel, Lines2DIntersection not possible');
    Result[0] := - (Line0[2] * Ratio - Line1[2]) / Divide;
    Result[1] := - (Line0[0] * Result[0] + Line0[2]) / Line0[1];
  end;

  { tests: (checking should write zeros)
  Writeln('intersection 2d: ', VectorToNiceStr(Line0), ' ',
    VectorToNiceStr(Line1), ' gives ', VectorToNiceStr(Result), ' checking: ',
    FloatToNiceStr(Line0[0] * Result[0] + Line0[1] * Result[1] + Line0[2]), ' ',
    FloatToNiceStr(Line1[0] * Result[0] + Line1[1] * Result[1] + Line1[2])); }
end;

function ThreePlanesIntersectionPoint(
  const Plane0, Plane1, Plane2: TYPE_VECTOR4): TYPE_VECTOR3;
var Line0, LineVector: TYPE_VECTOR3;
begin
 TwoPlanesIntersectionLine(Plane0, Plane1, Line0, LineVector);
 if not TryPlaneLineIntersection(Result, Plane2, Line0, LineVector) then
  raise Exception.Create('Cannot calculate intersection point of three planes :' +
    'intersection line of first two planes is parallel to the 3rd plane');
end;

function PlaneMove(const Plane: TYPE_VECTOR4;
  const Move: TYPE_VECTOR3): TYPE_VECTOR4;
begin
  { Given a plane Ax + By + Cz + D = 0.
    We want to find a new plane, moved by Move.
    Actually, we want to find only new D, since we know that (A, B, C)
    is a normal vector of the plane, so it doesn't change.

    Math says: old plane equation is OK for point (x, y, z) if and only if
    new plane equation is OK for (x, y, z) + Move. Therefore
      Ax + By + Cz + D = 0 iff
      A * (x + Move[0]) + B * (y + Move[1]) + C * (z + Move[2]) + NewD = 0
    The 2nd equation can be rewritten as
      Ax + By + Cz + NewD + A * Move[0] + B * Move[1] + C * Move[2] = 0
    so
      NewD = D - (A * Move[0] + B * Move[1] + C * Move[2]);
  }

  Result := Plane;
  Result[3] -= Plane[0] * Move[0] +
               Plane[1] * Move[1] +
               Plane[2] * Move[2];
end;

procedure PlaneMoveTo1st(var Plane: TYPE_VECTOR4; const Move: TYPE_VECTOR3);
begin
  Plane[3] -= Plane[0] * Move[0] +
              Plane[1] * Move[1] +
              Plane[2] * Move[2];
end;

function PlaneAntiMove(const Plane: TYPE_VECTOR4;
  const Move: TYPE_VECTOR3): TYPE_VECTOR4;
begin
  { Like PlaneMove, but Move vector is negated.
    So we just do "Result[3] +=" instead of "Result[3] -=". }

  Result := Plane;
  Result[3] += Plane[0] * Move[0] +
               Plane[1] * Move[1] +
               Plane[2] * Move[2];
end;

{$define VectorsSamePlaneDirections_Implement:=
var v1dot, v2dot: TYPE_SCALAR;
begin
 v1dot := VectorDotProduct(v1, PlaneDir);
 v2dot := VectorDotProduct(v2, PlaneDir);
 result := IsZero(v1dot) or IsZero(v2dot) or ((v1dot > 0) = (v2dot > 0));
end;}

  function VectorsSamePlaneDirections(const v1, v2: TYPE_VECTOR3;
    const Plane: TYPE_VECTOR4): boolean;
  var PlaneDir: TYPE_VECTOR3 absolute Plane;
  VectorsSamePlaneDirections_Implement

  function VectorsSamePlaneDirections(const v1, v2: TYPE_VECTOR3;
    const PlaneDir: TYPE_VECTOR3): boolean;
  VectorsSamePlaneDirections_Implement

{$undef VectorsSamePlaneDirections_Implement}

function PointsSamePlaneSides(const p1, p2: TYPE_VECTOR3; const Plane: TYPE_VECTOR4): boolean;
var p1Side, p2Side: TYPE_SCALAR;
begin
 p1Side := p1[0]*Plane[0] + p1[1]*Plane[1] + p1[2]*Plane[2] + Plane[3];
 p2Side := p2[0]*Plane[0] + p2[1]*Plane[1] + p2[2]*Plane[2] + Plane[3];
 result := IsZero(p1Side) or IsZero(p2Side) or ((p1Side > 0) = (p2Side > 0));
end;

function PlaneDirNotInDirection(const PlaneDir, Direction: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 if VectorDotProduct(Direction, PlaneDir) > 0 then
  result := VectorNegate(PlaneDir) else
  result := PlaneDir;
end;

function PointsDistance(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
begin
 (* result := Sqrt(PointsDistanceSqr(v1, v2));, rozwiniete w nadziei na (drobny)
    zysk szybkosci *)
 result := Sqrt( Sqr(v2[0]-v1[0]) + Sqr(v2[1]-v1[1]) + Sqr(v2[2]-v1[2]) );
end;

function PointsDistanceSqr(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
begin
 (* result := VectorLenSqr(VectorSubtract(v2, v1));, rozwiniete w nadziei na (drobny)
    zysk szybkosci *)
 result := Sqr(v2[0]-v1[0]) + Sqr(v2[1]-v1[1]) + Sqr(v2[2]-v1[2]);
end;

function PointsDistanceSqr(const v1, v2: TYPE_VECTOR2): TYPE_SCALAR;
begin
 (* result := VectorLenSqr(VectorSubtract(v2, v1));, rozwiniete w nadziei na (drobny)
    zysk szybkosci *)
 result := Sqr(v2[0]-v1[0]) + Sqr(v2[1]-v1[1]);
end;

function PointsDistanceXYSqr(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
begin
  Result := Sqr(v2[0]-v1[0]) + Sqr(v2[1]-v1[1]);
end;

function VectorsEqual(const v1, v2: TYPE_VECTOR2;
  const EqualityEpsilon: TYPE_SCALAR): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := CompareMem(@V1, @V2, SizeOf(V1)) else
    Result := (Abs(v1[0]-v2[0]) < EqualityEpsilon) and
              (Abs(v1[1]-v2[1]) < EqualityEpsilon);
end;

function VectorsEqual(const v1, v2: TYPE_VECTOR3): boolean;
begin
  if SCALAR_EQUALITY_EPSILON = 0 then
    Result := CompareMem(@V1, @V2, SizeOf(V1)) else
    Result := FloatsEqual(v1[0], v2[0]) and
              FloatsEqual(v1[1], v2[1]) and
              FloatsEqual(v1[2], v2[2]);
end;

function VectorsEqual(const v1, v2: TYPE_VECTOR3;
  const EqualityEpsilon: TYPE_SCALAR): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := CompareMem(@V1, @V2, SizeOf(V1)) else
    Result := (Abs(v1[0]-v2[0]) < EqualityEpsilon) and
              (Abs(v1[1]-v2[1]) < EqualityEpsilon) and
              (Abs(v1[2]-v2[2]) < EqualityEpsilon);
end;

function VectorsEqual(const v1, v2: TYPE_VECTOR4): boolean;
begin
  if SCALAR_EQUALITY_EPSILON = 0 then
    Result := CompareMem(@V1, @V2, SizeOf(V1)) else
    Result := FloatsEqual(v1[0], v2[0]) and
              FloatsEqual(v1[1], v2[1]) and
              FloatsEqual(v1[2], v2[2]) and
              FloatsEqual(v1[3], v2[3]);
end;

function VectorsEqual(const v1, v2: TYPE_VECTOR4;
  const EqualityEpsilon: TYPE_SCALAR): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := CompareMem(@V1, @V2, SizeOf(V1)) else
    Result := (Abs(v1[0]-v2[0]) < EqualityEpsilon) and
              (Abs(v1[1]-v2[1]) < EqualityEpsilon) and
              (Abs(v1[2]-v2[2]) < EqualityEpsilon) and
              (Abs(v1[3]-v2[3]) < EqualityEpsilon);
end;

function VectorsPerfectlyEqual(const v1, v2: TYPE_VECTOR2): boolean; inline;
begin
  Result := CompareMem(@V1, @V2, SizeOf(V1));
end;

function VectorsPerfectlyEqual(const v1, v2: TYPE_VECTOR3): boolean; inline;
begin
  Result := CompareMem(@V1, @V2, SizeOf(V1));
end;

function VectorsPerfectlyEqual(const v1, v2: TYPE_VECTOR4): boolean; inline;
begin
  Result := CompareMem(@V1, @V2, SizeOf(V1));
end;

function MatricesEqual(const M1, M2: TYPE_MATRIX3;
  const EqualityEpsilon: TYPE_SCALAR): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := CompareMem(@M1, @M2, SizeOf(M1)) else
    Result :=
      (Abs(M1[0, 0] - M2[0, 0]) < EqualityEpsilon) and
      (Abs(M1[0, 1] - M2[0, 1]) < EqualityEpsilon) and
      (Abs(M1[0, 2] - M2[0, 2]) < EqualityEpsilon) and

      (Abs(M1[1, 0] - M2[1, 0]) < EqualityEpsilon) and
      (Abs(M1[1, 1] - M2[1, 1]) < EqualityEpsilon) and
      (Abs(M1[1, 2] - M2[1, 2]) < EqualityEpsilon) and

      (Abs(M1[2, 0] - M2[2, 0]) < EqualityEpsilon) and
      (Abs(M1[2, 1] - M2[2, 1]) < EqualityEpsilon) and
      (Abs(M1[2, 2] - M2[2, 2]) < EqualityEpsilon);
end;

function MatricesEqual(const M1, M2: TYPE_MATRIX4;
  const EqualityEpsilon: TYPE_SCALAR): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := CompareMem(@M1, @M2, SizeOf(M1)) else
    Result :=
      (Abs(M1[0, 0] - M2[0, 0]) < EqualityEpsilon) and
      (Abs(M1[0, 1] - M2[0, 1]) < EqualityEpsilon) and
      (Abs(M1[0, 2] - M2[0, 2]) < EqualityEpsilon) and
      (Abs(M1[0, 3] - M2[0, 3]) < EqualityEpsilon) and

      (Abs(M1[1, 0] - M2[1, 0]) < EqualityEpsilon) and
      (Abs(M1[1, 1] - M2[1, 1]) < EqualityEpsilon) and
      (Abs(M1[1, 2] - M2[1, 2]) < EqualityEpsilon) and
      (Abs(M1[1, 3] - M2[1, 3]) < EqualityEpsilon) and

      (Abs(M1[2, 0] - M2[2, 0]) < EqualityEpsilon) and
      (Abs(M1[2, 1] - M2[2, 1]) < EqualityEpsilon) and
      (Abs(M1[2, 2] - M2[2, 2]) < EqualityEpsilon) and
      (Abs(M1[2, 3] - M2[2, 3]) < EqualityEpsilon) and

      (Abs(M1[3, 0] - M2[3, 0]) < EqualityEpsilon) and
      (Abs(M1[3, 1] - M2[3, 1]) < EqualityEpsilon) and
      (Abs(M1[3, 2] - M2[3, 2]) < EqualityEpsilon) and
      (Abs(M1[3, 3] - M2[3, 3]) < EqualityEpsilon);
end;

function Lerp(const A: TYPE_SCALAR; const M1, M2: TYPE_MATRIX3): TYPE_MATRIX3;
begin
  Result[0, 0] := M1[0, 0] + A * (M2[0, 0] - M1[0, 0]);
  Result[0, 1] := M1[0, 1] + A * (M2[0, 1] - M1[0, 1]);
  Result[0, 2] := M1[0, 2] + A * (M2[0, 2] - M1[0, 2]);

  Result[1, 0] := M1[1, 0] + A * (M2[1, 0] - M1[1, 0]);
  Result[1, 1] := M1[1, 1] + A * (M2[1, 1] - M1[1, 1]);
  Result[1, 2] := M1[1, 2] + A * (M2[1, 2] - M1[1, 2]);

  Result[2, 0] := M1[2, 0] + A * (M2[2, 0] - M1[2, 0]);
  Result[2, 1] := M1[2, 1] + A * (M2[2, 1] - M1[2, 1]);
  Result[2, 2] := M1[2, 2] + A * (M2[2, 2] - M1[2, 2]);
end;

function Lerp(const A: TYPE_SCALAR; const M1, M2: TYPE_MATRIX4): TYPE_MATRIX4;
begin
  Result[0, 0] := M1[0, 0] + A * (M2[0, 0] - M1[0, 0]);
  Result[0, 1] := M1[0, 1] + A * (M2[0, 1] - M1[0, 1]);
  Result[0, 2] := M1[0, 2] + A * (M2[0, 2] - M1[0, 2]);
  Result[0, 3] := M1[0, 3] + A * (M2[0, 3] - M1[0, 3]);

  Result[1, 0] := M1[1, 0] + A * (M2[1, 0] - M1[1, 0]);
  Result[1, 1] := M1[1, 1] + A * (M2[1, 1] - M1[1, 1]);
  Result[1, 2] := M1[1, 2] + A * (M2[1, 2] - M1[1, 2]);
  Result[1, 3] := M1[1, 3] + A * (M2[1, 3] - M1[1, 3]);

  Result[2, 0] := M1[2, 0] + A * (M2[2, 0] - M1[2, 0]);
  Result[2, 1] := M1[2, 1] + A * (M2[2, 1] - M1[2, 1]);
  Result[2, 2] := M1[2, 2] + A * (M2[2, 2] - M1[2, 2]);
  Result[2, 3] := M1[2, 3] + A * (M2[2, 3] - M1[2, 3]);

  Result[3, 0] := M1[3, 0] + A * (M2[3, 0] - M1[3, 0]);
  Result[3, 1] := M1[3, 1] + A * (M2[3, 1] - M1[3, 1]);
  Result[3, 2] := M1[3, 2] + A * (M2[3, 2] - M1[3, 2]);
  Result[3, 3] := M1[3, 3] + A * (M2[3, 3] - M1[3, 3]);
end;

function MatricesPerfectlyEqual(const M1, M2: TYPE_MATRIX3): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

function MatricesPerfectlyEqual(const M1, M2: TYPE_MATRIX4): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

function VectorsPerp(const v1, v2: TYPE_VECTOR3): boolean;
begin
 (* prosto : result := CosAngleBetweenVectors(v1, v2) = 0.
    Ale mozna zobaczyc jak liczymy CosAngleBetweenVectors - to jest
    VectorDorProduct / cos-tam. Wynik jest = 0 <=> VectorDotProduct = 0. *)
 result := IsZero(VectorDotProduct(v1, v2), SCALAR_EQUALITY_EPSILON*2);
end;

function VectorsParallel(const v1, v2: TYPE_VECTOR3): boolean;
var mc, c1, c2: Integer;
    Scale: TYPE_SCALAR;
begin
 mc := MaxAbsVectorCoord(v1);
 if IsZero(v1[mc]) then Exit(true);

 Scale := v2[mc] / v1[mc];
 RestOf3dCoords(mc, c1, c2);
 result := FloatsEqual(v1[c1] * Scale, v2[c1]) and
           FloatsEqual(v1[c2] * Scale, v2[c2]);
end;

procedure MakeVectorsAngleDegOnTheirPlane(var v1: TYPE_VECTOR3;
  const v2: TYPE_VECTOR3; AngleDeg: TYPE_SCALAR);
var rotAxis: TYPE_VECTOR3;
    v1len: TYPE_SCALAR;
begin
  (* dostosuj v1 tak zeby pomiedzy nim o wektorem v2 =
     byl kat AngleDeg. Robimy to tak : v1 i v2 wyznaczaja plaszczyzne,
     czyli jej wektor normalny rotAxis. Potem obroc wektor v2 o -Angle wokol
     rotAxis - otrzymasz szukany wektor lezacy na tej samej plaszczyznie
     co v2. Muisz obrocic o -Angle bo w interfejsie piszemy ze chcemy
     miec sytuacje gdzie z obrot v1 wokol VectorProduct(v1, v2) da v2
     a my tutaj obracamy przeciez v2. *)
  v1len := VectorLen(v1);
  rotAxis := VectorProduct(v1, v2);
  v1 := VectorAdjustToLength(
    RotatePointAroundAxisDeg(-AngleDeg, v2, rotAxis), v1len);
end;

procedure MakeVectorsAngleRadOnTheirPlane(var v1: TYPE_VECTOR3;
  const v2: TYPE_VECTOR3; AngleRad: TYPE_SCALAR);
var rotAxis: TYPE_VECTOR3;
    v1len: TYPE_SCALAR;
begin
  v1len := VectorLen(v1);
  rotAxis := VectorProduct(v1, v2);
  v1 := VectorAdjustToLength(
    RotatePointAroundAxisRad(-AngleRad, v2, rotAxis), v1len);
end;

procedure MakeVectorsOrthoOnTheirPlane(var v1: TYPE_VECTOR3;
  const v2: TYPE_VECTOR3);
begin
  { TODO: can we speed this up ?
    For Pi/2, we can do just v1 := VectorProduct of sthg and adjust v1 length. }
  MakeVectorsAngleRadOnTheirPlane(v1, v2, Pi/2);
end;

function AnyPerpVector(const v: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 if IsZero(v[0]) and IsZero(v[1]) then
 begin
  result[0] := 0;
  result[1] := v[2];
  result[2] := -v[1];
 end else
 begin
  result[0] := v[1];
  result[1] := -v[0];
  result[2] := 0;
 end;
end;

function IsLineParallelToPlane(const lineVector: TYPE_VECTOR3; const plane: TYPE_VECTOR4): boolean;
var PlaneDir: TYPE_VECTOR3 absolute plane;
begin
 result := VectorsPerp(lineVector, PlaneDir);
end;

function IsLineParallelToSimplePlane(const lineVector: TYPE_VECTOR3;
  const PlaneConstCoord: integer): boolean;
begin
 result := IsZero(lineVector[PlaneConstCoord]);
end;

function AreParallelVectorsSameDirection(
  const Vector1, Vector2: TYPE_VECTOR3): boolean;
var
  Coord: Integer;
begin
  { Assuming that Vector1 is non-zero, MaxAbsVectorCoord(Vector1)
    must be non-zero. }
  Coord := MaxAbsVectorCoord(Vector1);

  Result := (Vector1[Coord] > 0) = (Vector2[Coord] > 0);
end;

function PointOnPlaneClosestToPoint(const plane: TYPE_VECTOR4; const point: TYPE_VECTOR3): TYPE_VECTOR3;
var d: TYPE_SCALAR;
    PlaneDir: TYPE_VECTOR3 absolute plane;
begin
 (*licz punkt Pr - punkt na plaszczyznie plane bedacy rzutem prostopadlym
   punktu pos na ta plaszczyzne. Pr = pos + d * PlaneDir.
   plane[0]*Pr[0] + plane[1]*Pr[1] + plane[2]*Pr[2] + plane[3] = 0,
   mamy wiec
   plane[0]*(pos[0] + d*plane[0])+
   plane[1]*(pos[1] + d*plane[1])+
   plane[2]*(pos[2] + d*plane[2])+ plane[3] = 0
   Przeksztalcajac otrzymujemy rownanie na d.*)
 d := -(plane[0]*point[0] + plane[1]*point[1] + plane[2]*point[2] + plane[3])/
     VectorLenSqr(PlaneDir);
 result := VectorAdd(point, VectorScale(PlaneDir, d));
end;

function PointToPlaneDistanceSqr(const Point: TYPE_VECTOR3;
  const Plane: TYPE_VECTOR4): TYPE_SCALAR;
begin
 Result :=
   Sqr(Plane[0] * Point[0] +
       Plane[1] * Point[1] +
       Plane[2] * Point[2] +
       Plane[3]) /
   (Sqr(Plane[0]) + Sqr(Plane[1]) + Sqr(Plane[2]));
end;

function PointToNormalizedPlaneDistance(const Point: TYPE_VECTOR3;
  const Plane: TYPE_VECTOR4): TYPE_SCALAR;
begin
 Result :=
   Abs(Plane[0] * Point[0] +
       Plane[1] * Point[1] +
       Plane[2] * Point[2] +
       Plane[3]);
end;

function PointToPlaneDistance(const Point: TYPE_VECTOR3;
  const Plane: TYPE_VECTOR4): TYPE_SCALAR;
begin
 Result :=
   Abs(Plane[0] * Point[0] +
       Plane[1] * Point[1] +
       Plane[2] * Point[2] +
       Plane[3]) /
   Sqrt(Sqr(Plane[0]) + Sqr(Plane[1]) + Sqr(Plane[2]));
end;

function PointToSimplePlaneDistance(const point: TYPE_VECTOR3;
  const PlaneConstCoord: integer; const PlaneConstValue: TYPE_SCALAR): TYPE_SCALAR;
begin
 result := Abs(point[PlaneConstCoord]-PlaneConstValue);
end;

function PointOnLineClosestToPoint(
  const line0, lineVector, point: TYPE_VECTOR3): TYPE_VECTOR3;
var d: TYPE_SCALAR;
begin
 (*
  wiemy ze wektory result-point i lineVector (albo result-line0) sa prostopadle.
  (result[0]-point[0])*(lineVector[0]) +
  (result[1]-point[1])*(lineVector[1]) +
  (result[2]-point[2])*(lineVector[2]) = 0 czyli
  result[0]*lineVector[0] +
  result[1]*lineVector[1] +
  result[2]*lineVector[2] = point[0]*lineVector[0] +
                            point[1]*lineVector[1] +
                            point[2]*lineVector[2]
  Wiemy ze result wyraza sie jako line0 + lineVector*d
  result = line0+lineVector*d
  czyli
  result[0] = line0[0] + lineVector[0]*d
  result[1] = line0[1] + lineVector[1]*d
  result[2] = line0[2] + lineVector[2]*d
  a wiec 4 rownania, 4 niewiadome i juz wiemy ze jestesmy w domu.
  Podstawiamy :
  (line0[0] + lineVector[0]*d)*lineVector[0]+
  (line0[1] + lineVector[1]*d)*lineVector[1]+
  (line0[2] + lineVector[2]*d)*lineVector[2] = point[0]*lineVector[0] +
                            point[1]*lineVector[1] +
                            point[2]*lineVector[2]
  d*(Sqr(lineVector[0])+ Sqr(lineVector[1])+ Sqr(lineVector[2]) ) =
  d*VectorLenSqr(lineVector) =
  lineVector[0]*(point[0]-line0[0]) +
  lineVector[1]*(point[1]-line0[1]) +
  lineVector[2]*(point[2]-line0[2]);
  i stad mamy d. *)
 d := (lineVector[0] * (point[0]-line0[0]) +
       lineVector[1] * (point[1]-line0[1]) +
       lineVector[2] * (point[2]-line0[2]) ) / VectorLenSqr(lineVector);
 result := VectorAdd(line0, VectorScale(lineVector, d));
end;

function PointToLineDistanceSqr(const point, line0, lineVector: TYPE_VECTOR3): TYPE_SCALAR;
begin
 result := PointsDistanceSqr(point, PointOnLineClosestToPoint(line0, lineVector, point));
end;

function TryPlaneLineIntersection(out t: TYPE_SCALAR;
  const plane: TYPE_VECTOR4; const line0, lineVector: TYPE_VECTOR3): boolean;
var PlaneDir: TYPE_VECTOR3 absolute plane;
    Dot: TYPE_SCALAR;
begin
 Dot := VectorDotProduct(LineVector, PlaneDir);
 if not IsZero(Dot) then
 begin
  result := true;
  t := -(plane[0]*line0[0] + plane[1]*line0[1] + plane[2]*line0[2] + plane[3])/Dot;
 end else
  result := false;
end;

function TryPlaneLineIntersection(out intersection: TYPE_VECTOR3;
  const plane: TYPE_VECTOR4; const line0, lineVector: TYPE_VECTOR3): boolean;
var t: TYPE_SCALAR;
begin
 result := TryPlaneLineIntersection(t, Plane, Line0, LineVector);
 if result then Intersection := VectorAdd(Line0, VectorScale(LineVector, t));
end;

function TryPlaneRayIntersection(out Intersection: TYPE_VECTOR3;
  const Plane: TYPE_VECTOR4; const Ray0, RayVector: TYPE_VECTOR3): boolean;
var MaybeT: TYPE_SCALAR;
begin
 result := TryPlaneLineIntersection(MaybeT, Plane, Ray0, RayVector) and (MaybeT >= 0);
 if result then Intersection := VectorAdd(Ray0, VectorScale(RayVector, MaybeT));
end;

function TryPlaneRayIntersection(
  out Intersection: TYPE_VECTOR3; out T: TYPE_SCALAR;
  const Plane: TYPE_VECTOR4; const Ray0, RayVector: TYPE_VECTOR3): boolean;
var MaybeT: TYPE_SCALAR;
begin
 result := TryPlaneLineIntersection(MaybeT, Plane, Ray0, RayVector) and (MaybeT >= 0);
 if result then
 begin
  // Intersection := VectorAdd(Ray0, VectorScale(RayVector, MaybeT));
  // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
  Intersection := RayVector;
  VectorScaleTo1st(Intersection, MaybeT);
  VectorAddTo1st(Intersection, Ray0);

  t := MaybeT;
 end;
end;

function TryPlaneSegmentDirIntersection(out Intersection: TYPE_VECTOR3;
  const Plane: TYPE_VECTOR4; const Segment0, SegmentVector: TYPE_VECTOR3): boolean;
var MaybeT: TYPE_SCALAR;
begin
 result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
   (MaybeT >= 0) and (MaybeT <= 1);
 if result then Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));
end;

function TryPlaneSegmentDirIntersection(
  out Intersection: TYPE_VECTOR3; out T: TYPE_SCALAR;
  const Plane: TYPE_VECTOR4; const Segment0, SegmentVector: TYPE_VECTOR3): boolean;
var MaybeT: TYPE_SCALAR;
begin
 result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
   (MaybeT >= 0) and (MaybeT <= 1);
 if result then
 begin
  // Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));
  // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
  Intersection := SegmentVector;
  VectorScaleTo1st(Intersection, MaybeT);
  VectorAddTo1st(Intersection, Segment0);

  t := MaybeT;
 end;
end;

(*$I VectorMath_TrySimplePlaneXxxIntersection.inc*)

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TYPE_VECTOR3): boolean;
var vecSizes: TYPE_VECTOR3;
    c, i: integer;
begin
 (*rzutujemy 3 zadane punkty na ta wspolrzedna na ktorej mamy najwieksza swobode*)
 for i := 0 to 2 do vecSizes[i] := Abs(pos1[i]-pos2[i]);
 c := MaxVectorCoord(vecSizes);
 result := ((pos1[c] <= intersection[c]) and (intersection[c] <= pos2[c])) or
           ((pos1[c] >= intersection[c]) and (intersection[c] >= pos2[c]));
end;

function LineOfTwoDifferentPoints2d(const p1, p2: TYPE_VECTOR2): TYPE_VECTOR3;
var lineVector: TYPE_VECTOR2;
    cGood, cOther: integer;
begin
 (* chcemy zeby Vector2f(result) i p2-p1(=lineVector) byly prostopadle czyli ich
    iloczyn skalarny = 0 czyli result[0]*lineVector[0] +
    result[1]*lineVector[1] = 0. Niech cGood to wspolrzedna
    lineVector rozna od 0, cOther to ta druga.
    Niech result[cOther] = -1 i zobaczmy ze wtedy mozemy skonstruowac
    result[cGood] = lineVector[cOther] / lineVector[cGood]. *)
 lineVector := VectorSubtract(p2, p1);
 if Abs(lineVector[0]) > Abs(lineVector[1]) then
  begin cGood := 0; cOther := 1 end else
  begin cOther := 0; cGood := 1 end;
 result[cOther] := -1;
 result[cGood] := lineVector[cOther] / lineVector[cGood];

 (* result[0]*p1[0] + result[1]*p1[1] + result[2] = 0 wiec widac jak obliczyc
    teraz result[2] *)
 result[2] := -result[0]*p1[0] -result[1]*p1[1];
end;

function IsSpheresCollision(const Sphere1Center: TYPE_VECTOR3; const Sphere1Radius: TYPE_SCALAR;
  const Sphere2Center: TYPE_VECTOR3; const Sphere2Radius: TYPE_SCALAR): boolean;
begin
 result := PointsDistanceSqr(Sphere1Center, Sphere2Center)<=
   Sqr(Sphere1Radius+Sphere2Radius);
end;

function PointToSegmentDistanceSqr(const point, pos1, pos2: TYPE_VECTOR3): TYPE_SCALAR;
var Closest: TYPE_VECTOR3;
begin
 Closest := PointOnLineClosestToPoint(pos1, VectorSubtract(pos2, pos1), point);
 if IsPointOnSegmentLineWithinSegment(Closest, pos1, pos2) then
  result := PointsDistanceSqr(Closest, point) else
  result := KambiUtils.min(PointsDistanceSqr(pos1, point),
                      PointsDistanceSqr(pos2, point));
end;

function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TYPE_VECTOR3;
  const TunnelRadius: TYPE_SCALAR; const SphereCenter: TYPE_VECTOR3;
  const SphereRadius: TYPE_SCALAR): boolean;
begin
 result := PointToSegmentDistanceSqr(SphereCenter, Tunnel1, Tunnel2)<=
   Sqr(SphereRadius+TunnelRadius);
end;

function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TYPE_VECTOR3;
  const SphereRadius: TYPE_SCALAR): boolean;
var SphereRadiusSqr: TYPE_SCALAR;
    Intersect: TYPE_VECTOR3;
begin
 SphereRadiusSqr := Sqr(SphereRadius);
 result:= (PointsDistanceSqr(pos1, SphereCenter) <= SphereRadiusSqr) or
          (PointsDistanceSqr(pos2, SphereCenter) <= SphereRadiusSqr);
 if not result then
 begin
  Intersect := PointOnLineClosestToPoint(pos1, VectorSubtract(pos2, pos1), SphereCenter);
  result := IsPointOnSegmentLineWithinSegment(Intersect, pos1, pos2) and
    (PointsDistanceSqr(Intersect, SphereCenter) <= SphereRadiusSqr);
 end;
end;

(* cos do trojkatow TTriangle** ------------------------------------------------- *)

function IsValidTriangle(const Tri: TYPE_TRIANGLE3): boolean;
begin
 (* chcemy sprawdzic czy Tri nie jest zdegenerowanym trojkatem, czyli czy
    wyznacza dobra plaszczyzne; wiec nie tylko wszstkie punkty musza byc
    rozne ale tez nie moga byc wspolliniowe. Jak to najlatwiej sprawdzic ?

    Sprawdzajac czy VectorProduct(
      VectorSubtract(Tri[2], Tri[1]),
      VectorSubtract(Tri[0], Tri[1]))
    ma dlugosc > 0.
    Mozna to uzasadnic na dwa sposoby : prosto i nie do konca scisle -
    liczymy TrianglePlane uzywajac takiego wlasnie VectorProduct, a przeciez
    chcemy wlasnie sprawdzic czy TrianglePlane zwroci prawidlowy plane.
    Albo inaczej : te trzy punkty wyznaczaja plaszczyzne wtw. gdy rownoleglobok
    ktorego dwa boki tworza wektory Tri[2]-Tri[1] i Tri[0]-Tri[1] ma niezerowe
    pole. A przeciez dlugosc VectorProduct to pole tego rownolegloboku.
    Wiec testujac dlugosc VectorProduct testujemy dlugosc tego rownolegloboku.

    Taki VectorProduct() mozemy obliczyc uzywajac TriangleDir().
 *)
 result := VectorLenSqr(TriangleDir(Tri)) > Sqr(SCALAR_EQUALITY_EPSILON);
end;

function TriangleDir(const Tri: TYPE_TRIANGLE3): TYPE_VECTOR3;
begin
 result := VectorProduct(
   VectorSubtract(Tri[2], Tri[1]),
   VectorSubtract(Tri[0], Tri[1]));
end;

function TriangleDir(const p0, p1, p2: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 result := VectorProduct(
   VectorSubtract(p2, p1),
   VectorSubtract(p0, p1));
end;

function TriangleNormal(const Tri: TYPE_TRIANGLE3): TYPE_VECTOR3;
begin
 result := Normalized( TriangleDir(Tri) );
end;

function TriangleNormal(const p0, p1, p2: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 result := Normalized( TriangleDir(p0, p1, p2) );
end;

function TrianglePlane(const Tri: TYPE_TRIANGLE3): TYPE_VECTOR4;
var ResultDir: TYPE_VECTOR3 absolute result;
begin
 ResultDir := TriangleDir(Tri);
 (* Punkt Tri[0] musi lezec na plane result. Wiec musi zachodzic
    ResulrDir[0]*Tri[0, 0] + ResulrDir[1]*Tri[0, 1] + ResulrDir[2]*Tri[0, 2]
      + result[3] = 0.
    Stad widac jak wyznaczyc result[3]. *)
 result[3] := -ResultDir[0]*Tri[0, 0]
              -ResultDir[1]*Tri[0, 1]
              -ResultDir[2]*Tri[0, 2];
end;

function TrianglePlane(const p0, p1, p2: TYPE_VECTOR3): TYPE_VECTOR4;
var ResultDir: TYPE_VECTOR3 absolute result;
begin
 REsultDir := TriangleDir(p0, p1, p2);
 result[3] := -ResultDir[0]*p0[0]
              -ResultDir[1]*p0[1]
              -ResultDir[2]*p0[2];
end;

function TriangleNormPlane(const Tri: TYPE_TRIANGLE3): TYPE_VECTOR4;
var resultNormal: TYPE_VECTOR3 absolute result;
begin
 (* dzialamy tak samo jak TrianglePlane tyle ze teraz uzywamy TriangleNormal
    zamiast TriangleNormalNotNorm *)
 resultNormal := TriangleNormal(Tri);
 result[3] := -resultNormal[0]*Tri[0, 0] -resultNormal[1]*Tri[0, 1]
   -resultNormal[2]*Tri[0, 2];
end;

function TriangleArea(const Tri: TYPE_TRIANGLE3): TYPE_SCALAR;
begin
 result := VectorLen(
   VectorProduct(VectorSubtract(Tri[1], Tri[0]),
                 VectorSubtract(Tri[2], Tri[0]))) / 2;
end;

function TriangleAreaSqr(const Tri: TYPE_TRIANGLE3): TYPE_SCALAR;
begin
 result := VectorLenSqr(
   VectorProduct(VectorSubtract(Tri[1], Tri[0]),
                 VectorSubtract(Tri[2], Tri[0]))) / 4;
end;

function IsPointWithinTriangle2d(const P: TYPE_VECTOR2; const Tri: TYPE_TRIANGLE2): boolean;
var TriMiddle: TYPE_VECTOR2;
    line: TYPE_VECTOR3;
    i: integer;
begin
 TriMiddle := VectorAdd( VectorAdd(VectorScale(Tri[0], 1/3), VectorScale(Tri[1], 1/3)), VectorScale(Tri[2], 1/3));
 (* teraz dla kazdej z 3 prostych zawierajacych boki trojkata
    P musi byc po tej samej stronie prostej co TriMiddle. *)
 for i := 0 to 2 do
 begin
  line := LineOfTwoDifferentPoints2d(Tri[i], Tri[(i+1)mod 3]);
  if TriMiddle[0]*line[0] + TriMiddle[1]*line[1] + line[2] >= 0 then
  begin
   if P[0]*line[0] + P[1]*line[1] + line[2] < -SCALAR_EQUALITY_EPSILON then exit(false);
  end else
  begin
   if P[0]*line[0] + P[1]*line[1] + line[2] > SCALAR_EQUALITY_EPSILON then exit(false);
  end;
 end;

 result := true;
end;

function IsPointOnTrianglePlaneWithinTriangle(const P: TYPE_VECTOR3;
  const Tri: TYPE_TRIANGLE3; const TriDir: TYPE_VECTOR3): boolean;
(*kiedys uzywalem tu prostego testu ktory sprawdzal czy trzy katy :
  pomiedzy wektorami (t[0]-p) a (t[1]-p), (t[1]-p) a (t[2]-p) i
  (t[2]-p) a (t[0]-p) sumuja sie do 360 stopni. Tamten algorytm
  jest obecnie w VectorMath_old_algorithms.pas. Podobnie jak jeszcze
  inna wersja : testowanie poprzez rzutowanie trojkata na 2D.
  To jest wiec wersja trzecia.

  Probowalem roznych wersji algorytmu bo myslalem ze na skutek bledow obliczen
  czasami ta procedura odpowiada zle. Po jakims czasie okazalo sie jednak
  ze byl to wynik zupelnie zwyczajnego bledu w TVRMLNodes.Triangulate
  ktory na skutek rozrzutnego zapisywania VRML'i w Blenderze ujawnial sie
  dosc rzadko. Znalazlem blad i wszystko dziala pieknie i zostalem szczesliwym
  czlowiekiem z trzema implementacjami tej samej procedury. Dlaczego
  wybralem akurat ponizsza ? Bo okazala sie najszybsza (chociaz dosc nieznacznie),
  ma tez elegancki zapis.

  Niniejszym zaimplementowalem ta procedure wedlug prostego algorytmu z
  http://geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm .
  Jest tak prosty ze powtorze go tutaj :
  o. kazdy punkt na plaszczyznie Plane trojkata T mozna wyrazic 1-znacznie
     jako P = T[0] + s*u + t*v gdzie s i t to dowolne skalary a u = (T[1]-T[0]),
     v = (T[2]-T[0]). W ten sposob czynimy z ramion trojkata jakby osie
     wspolrzednych i s, t okreslaja nam punkt P w tym ukladzie wspolrzednych
     (osie nie sa prostopadle -> geometria afiniczna sie klania)
  o. jezeli P jest w srodku trojkata to s >= 0 i t >= 0 i s+t <= 1
     (bo rozwinmy inaczej P = T[0]*(1-s-t) + T[1]*s + T[2]*t i widac
     ze s i t to zwyczajne zmienne postaci barycentrycznej punktu w trojkacie)
  o. pozostaje znalezc s i t.
     niech w = P - T[0]. Wiemy wiec ze w = s*u + t*v. Niech x^ oznacza
     VectorProduct(x, PlaneDir), tj. wektor prostopadly do x i lezacy
     ciagle na plaszczynie Plane. Wezmy DotProduct obu stron rownania
     z v^, skorzystajmy z milych wlasnosci iloczynu skalarnego
       ((a+b).v = a.v + b.v  i (s*a).v = s * (a.v) gdzie a, b,v to wektory,
        s to skalar, * jest na skalarach a + raz na wektorach a raz na skalarach)
     i mamy
     w.v^ = s*u.v^ + t*v.v^
     Wektory v i v^ sa prostopadle wiec ich dotProcuct = 0.
     Mamy w ten sposob s : s = w.v^ / (u.v^)
     Analogicznie liczmy v.
  o. Na stronie proponuja dalsze optymalizacje ktore jednak nie wydaja
     mi sie rzeczywiscie zmniejszac ilosci wykonywanych operacji,
     wiec ich tu nie implementowalem.
  Niniejszy algorytm obejmuje juz przypadek gdyby P = T[0] lub T[1] lub T[2].
  (chociaz ten algorytm nie dziala dla zdegenerowanych trojkatow, gdy u lub v = 0
  to wystapi dzielenia przez zero; ale to nic, bo my w tym module w ogole
  nie dopuszczamy zdegenerowanych trojkatow).
*)
var s, t: TYPE_SCALAR;
    w, u,v, ortho: TYPE_VECTOR3;
    One: TYPE_SCALAR;
begin
 u := VectorSubtract(Tri[1], Tri[0]);
 v := VectorSubtract(Tri[2], Tri[0]);
 w := VectorSubtract(P, Tri[0]);

 One := 1+SCALAR_EQUALITY_EPSILON;

 ortho := VectorProduct(v, TriDir);
 s := VectorDotProduct(w, ortho) / VectorDotProduct(u, ortho);
 if (s < -SCALAR_EQUALITY_EPSILON) or
    { As far as only correctness is concerned, check for s > One isn't needed
      here since we will check s+t <= One later anyway.
      But for the speed, it's better to make here a quick check
      "s > One" and in many cases avoid the long calculation of T.

      See /win/3dmodels/rayhunter-demos/raporty/2006-11-12/README:
      speed of this procedure has a significant impact
      on the ray-tracer speed, so it's really good optimization. }
    (s > One) then
   Exit(false);

 ortho := VectorProduct(u, TriDir);
 t := VectorDotProduct(w, ortho) / VectorDotProduct(v, ortho);
 if t < -SCALAR_EQUALITY_EPSILON then
   Exit(false);

 result := s+t <= One;
end;

//function IsPointOnTrianglePlaneWithinTriangle(const P: TYPE_VECTOR3;
//  const Tri: TYPE_TRIANGLE3): boolean;
//begin
// result := IsPointOnTrianglePlaneWithinTriangle(P, Tri, TriangleDir(Tri));
//end;

{$define TryTriangleRayCollision_IMPLEMENT:=
var MaybeIntersection: TYPE_VECTOR3;
    MaybeT: TYPE_SCALAR;
    TriDir: TYPE_VECTOR3 absolute TriPlane;
begin
 result := TryPlaneRayIntersection(MaybeIntersection, MaybeT, TriPlane, Ray0, RayVector) and
         IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
 if result then
 begin
  Intersection := MaybeIntersection;
  {$ifdef HAS_T} T := MaybeT; {$endif}
 end;
end;}

{$define TryTriangleSegmentDirCollision_IMPLEMENT:=
var MaybeIntersection: TYPE_VECTOR3;
    MaybeT: TYPE_SCALAR;
    TriDir: TYPE_VECTOR3 absolute TriPlane;
begin
 result := TryPlaneSegmentDirIntersection(MaybeIntersection, MaybeT, TriPlane, Segment0, SegmentVector) and
         IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
 if result then
 begin
  Intersection := MaybeIntersection;
  {$ifdef HAS_T} T := MaybeT; {$endif}
 end;
end;}

function IsTriangleSegmentCollision(const Tri: TYPE_TRIANGLE3;
  const TriPlane: TYPE_VECTOR4; const pos1, pos2: TYPE_VECTOR3): boolean;
var lineVector, MaybeIntersection: TYPE_VECTOR3;
    TriDir: TYPE_VECTOR3 absolute TriPlane;
begin
 lineVector := VectorSubtract(pos2, pos1);
 result := TryPlaneLineIntersection(MaybeIntersection, TriPlane, pos1, lineVector) and
           IsPointOnSegmentLineWithinSegment(MaybeIntersection, pos1, pos2) and
           IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
end;

function IsTriangleSegmentCollision(const Tri: TYPE_TRIANGLE3; const pos1, pos2: TYPE_VECTOR3): boolean;
begin
 result := IsTriangleSegmentCollision(Tri, TrianglePlane(Tri), pos1, pos2);
end;

function TryTriangleSegmentCollision(var Intersection: TYPE_VECTOR3;
  const Tri: TYPE_TRIANGLE3; const TriPlane: TYPE_VECTOR4;
  const pos1, pos2: TYPE_VECTOR3): boolean;
begin
 result := TryTriangleSegmentDirCollision(Intersection, Tri, TriPlane,
   Pos1, VectorSubtract(Pos2, Pos1));
end;

function TryTriangleSegmentDirCollision(var Intersection: TYPE_VECTOR3; var T: TYPE_SCALAR;
  const Tri: TYPE_TRIANGLE3; const TriPlane: TYPE_VECTOR4;
  const Segment0, SegmentVector: TYPE_VECTOR3): boolean;
{$define HAS_T}
TryTriangleSegmentDirCollision_IMPLEMENT
{$undef HAS_T}

function TryTriangleSegmentDirCollision(var Intersection: TYPE_VECTOR3;
  const Tri: TYPE_TRIANGLE3; const TriPlane: TYPE_VECTOR4;
  const Segment0, SegmentVector: TYPE_VECTOR3): boolean;
TryTriangleSegmentDirCollision_IMPLEMENT

function IsTriangleSphereCollision(const Tri: TYPE_TRIANGLE3;
  const TriPlane: TYPE_VECTOR4;
  const SphereCenter: TYPE_VECTOR3; SphereRadius: TYPE_SCALAR): boolean;
(*$define HAS_PRECALC_PLANE*)
(*$I vectormath_istrianglespherecollision.inc*)
(*$undef HAS_PRECALC_PLANE*)

function IsTriangleSphereCollision(const Tri: TYPE_TRIANGLE3;
  const SphereCenter: TYPE_VECTOR3; SphereRadius: TYPE_SCALAR): boolean;
(*$I vectormath_istrianglespherecollision.inc*)

function TryTriangleRayCollision(var Intersection: TYPE_VECTOR3; var T: TYPE_SCALAR;
  const Tri: TYPE_TRIANGLE3; const TriPlane: TYPE_VECTOR4;
  const Ray0, RayVector: TYPE_VECTOR3): boolean;
{$define HAS_T}
TryTriangleRayCollision_IMPLEMENT
{$undef HAS_T}

function TryTriangleRayCollision(var Intersection: TYPE_VECTOR3;
  const Tri: TYPE_TRIANGLE3; const TriPlane: TYPE_VECTOR4;
  const Ray0, RayVector: TYPE_VECTOR3): boolean;
TryTriangleRayCollision_IMPLEMENT

(* pare funkcji *ToStr -------------------------------------------------------- *)

function FloatToNiceStr(f: TYPE_SCALAR): string;
begin
 result := Format('%'+FloatNiceFormat, [f]);
end;

function VectorToNiceStr(const v: array of TYPE_SCALAR): string;
var i: integer;
begin
 result := '(';
 for i := 0 to High(v)-1 do result := result +FloatToNiceStr(v[i]) +', ';
 if High(v) >= 0 then result := result +FloatToNiceStr(v[High(v)]) +')';
end;

function MatrixToNiceStr(const v: TYPE_MATRIX4; const LineIndent: string): string;
begin
 result := Format('%s[ %4s %4s %4s %4s ]'+nl+
                  '%s| %4s %4s %4s %4s |'+nl+
                  '%s| %4s %4s %4s %4s |'+nl+
                  '%s[ %4s %4s %4s %4s ]',
  [LineIndent, FloatToNiceStr(v[0, 0]), FloatToNiceStr(v[1, 0]), FloatToNiceStr(v[2, 0]), FloatToNiceStr(v[3, 0]),
   LineIndent, FloatToNiceStr(v[0, 1]), FloatToNiceStr(v[1, 1]), FloatToNiceStr(v[2, 1]), FloatToNiceStr(v[3, 1]),
   LineIndent, FloatToNiceStr(v[0, 2]), FloatToNiceStr(v[1, 2]), FloatToNiceStr(v[2, 2]), FloatToNiceStr(v[3, 2]),
   LineIndent, FloatToNiceStr(v[0, 3]), FloatToNiceStr(v[1, 3]), FloatToNiceStr(v[2, 3]), FloatToNiceStr(v[3, 3]) ]);
end;

function TriangleToNiceStr(const t: TYPE_TRIANGLE2): string;
begin
 result := '['+VectorToNiceStr(t[0])+', '
              +VectorToNiceStr(t[1])+', '
              +VectorToNiceStr(t[2])+']';
end;

function TriangleToNiceStr(const t: TYPE_TRIANGLE3): string;
begin
 result := '['+VectorToNiceStr(t[0])+', '
              +VectorToNiceStr(t[1])+', '
              +VectorToNiceStr(t[2])+']';
end;

function FloatToRawStr(f: TYPE_SCALAR): string;
begin
 result := Format('%g', [f]);
end;

function VectorToRawStr(const v: array of TYPE_SCALAR): string;
var i: integer;
begin
 result := '';
 for i := 0 to High(v)-1 do result += FloatToRawStr(v[i]) +' ';
 if High(v) >= 0 then result += FloatToRawStr(v[High(v)]);
end;

function TriangleToRawStr(const T: TYPE_TRIANGLE3): string;
begin
  Result := '[' + VectorToRawStr(T[0]) + ', '
                + VectorToRawStr(T[1]) + ', '
                + VectorToRawStr(T[2]) + ']';
end;

(* operacje na macierzach -------------------------------------------------- *)

function TranslationMatrix(const Transl: TYPE_VECTOR3): TMatrix4Single;
begin
  result := IdentityMatrix4Single;
  result[3, 0] := Transl[0];
  result[3, 1] := Transl[1];
  result[3, 2] := Transl[2];
end;

function TranslationMatrix(const X, Y, Z: TYPE_SCALAR): TMatrix4Single;
begin
  result := IdentityMatrix4Single;
  result[3, 0] := X;
  result[3, 1] := Y;
  result[3, 2] := Z;
end;

procedure TranslationMatrices(const X, Y, Z: TYPE_SCALAR;
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  Matrix := IdentityMatrix4Single;
  Matrix[3, 0] := X;
  Matrix[3, 1] := Y;
  Matrix[3, 2] := Z;

  InvertedMatrix := IdentityMatrix4Single;
  InvertedMatrix[3, 0] := -X;
  InvertedMatrix[3, 1] := -Y;
  InvertedMatrix[3, 2] := -Z;
end;

procedure TranslationMatrices(const Transl: TYPE_VECTOR3;
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  Matrix := IdentityMatrix4Single;
  Matrix[3, 0] := Transl[0];
  Matrix[3, 1] := Transl[1];
  Matrix[3, 2] := Transl[2];

  InvertedMatrix := IdentityMatrix4Single;
  InvertedMatrix[3, 0] := -Transl[0];
  InvertedMatrix[3, 1] := -Transl[1];
  InvertedMatrix[3, 2] := -Transl[2];
end;

function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TYPE_VECTOR3): TMatrix4Single;
var i: integer;
begin
 for i := 0 to 2 do
 begin
  result[0, i] := NewX[i];
  result[1, i] := NewY[i];
  result[2, i] := NewZ[i];
  result[3, i] := NewOrigin[i];
 end;
 (* dolny wiersz: *)
 result[0, 3] := 0; result[1, 3] := 0; result[2, 3] := 0; result[3, 3] := 1;
end;

function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TYPE_VECTOR3): TMatrix4Single;
begin
 result := TransformToCoordsMatrix(NewOrigin,
   Normalized(NewX), Normalized(NewY), Normalized(NewZ));
end;

function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TYPE_VECTOR3): TMatrix4Single;
var i: integer;
begin
 for i := 0 to 2 do
 begin
  (* w linijkach ponizej zawarta jest cala subtelna roznica pomiedzy
     TrasformToCoords a TransformFromCoords.
     X/Y/Z aplikujemy inaczej (tak jakby transponujemy gorna lewa macierz
     3x3 w wyniku w porownianiu z wersja To) *)
  result[i, 0] := OldX[i];
  result[i, 1] := OldY[i];
  result[i, 2] := OldZ[i];
 end;

 (* druga roznica w porownaniu z wersja "To" : musimy inaczej aplikowac
    OldOrigin. Nie dosc ze OldOrigin musi byc tutaj zanegowane to jeszcze
    cala macierz musi byc tak naprawde przesuniecia o OldOrigin juz
    PO zrobieniu obrotow - patrz Compendium (15) z moimi dopiskami. *)
 result[3, 0] := -VectorDotProduct(OldOrigin, OldX);
 result[3, 1] := -VectorDotProduct(OldOrigin, OldY);
 result[3, 2] := -VectorDotProduct(OldOrigin, OldZ);

 (* dolny wiersz: *)
 result[0, 3] := 0; result[1, 3] := 0; result[2, 3] := 0; result[3, 3] := 1;
end;

function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TYPE_VECTOR3): TMatrix4Single;
begin
 result := TransformFromCoordsMatrix(OldOrigin,
   Normalized(OldX), Normalized(OldY), Normalized(OldZ));
end;

function LookAtMatrix(const Eye, Center, Up: TYPE_VECTOR3): TMatrix4Single;
begin
 result := LookDirMatrix(Eye, VectorSubtract(Center, Eye), Up);
end;

function LookDirMatrix(const Eye, Dir, Up: TYPE_VECTOR3): TMatrix4Single;
var GoodDir, GoodUp, Side: TYPE_VECTOR3;
begin
 Side := Normalized(VectorProduct(Dir, Up));
 GoodDir := Normalized(Dir);
 (* przelicz GoodUp z Side i GoodDir. W ten sposob robimy dwie rzeczy :
    - zapewniamy sobie ze GoodUp jest dobry, tzn. prostopadly do Dir
      (i naturalnie jest prostop. do Side, ale to juz mielismy zagwarantowane
      z tego ze liczymy Side := Normalized(VectorProduct(Dir, Up)))
    - zapewniamy sobie ze GoodUp juz jest znormalizowany (bo Side i GoodDir
      juz sa znormalizowane, wiec ich product tez ma dlugosc 1);
      w rezultacie udalo nam sie wykonac tylko dwie normalizacje
      (zamiast trzech) przy podawaniu trzech wektorow kierunku dla
      TransformFromCoordsMatrix *)
 GoodUp := VectorProduct(Side, GoodDir);

 (* piszac powyzej te drobne optymalizacje wzorowalem sie na kodzie
    procedury gluLookAt w implementacji GLU w SGI Sample OpenGL Implementation;
    matematycznie, obliczamy to samo. *)

 result := TransformFromCoordsMatrix(Eye, Side, GoodUp, VectorNegate(GoodDir));
end;

{
nie moze byc w dual implementation, potrzebujemy tego pod Delphi.

function GrayscaleValue(const v: TYPE_VECTOR3): TYPE_SCALAR;
begin
 result := GrayscaleValuesFloat[0]*v[0]+
           GrayscaleValuesFloat[1]*v[1]+
           GrayscaleValuesFloat[2]*v[2];
end;
}
