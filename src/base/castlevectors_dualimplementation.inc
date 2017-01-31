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

{ Implementation of CastleVectors for both floating-point types
  (Single and Double). This is included two times within castlevectors.pas
  implementation, with macros like TVector3 and TScalar
  defined for appropriate floating-point types. }

{$define VECTOR_OP_FUNCS:=
function VectorOpFuncName(const V1, V2: TVector2): TVector2;
begin
  Result[0] := V1[0] VectorOp V2[0];
  Result[1] := V1[1] VectorOp V2[1];
end;

function VectorOpFuncName(const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[0] VectorOp V2[0];
  Result[1] := V1[1] VectorOp V2[1];
  Result[2] := V1[2] VectorOp V2[2];
end;

function VectorOpFuncName(const V1, V2: TVector4): TVector4;
begin
  Result[0] := V1[0] VectorOp V2[0];
  Result[1] := V1[1] VectorOp V2[1];
  Result[2] := V1[2] VectorOp V2[2];
  Result[3] := V1[3] VectorOp V2[3];
end;

procedure VectorOpVarFuncName(var v1: TVector2; const v2: TVector2);
begin
  V1[0] VectorOpVar V2[0];
  V1[1] VectorOpVar V2[1];
end;

procedure VectorOpVarFuncName(var V1: TVector3; const V2: TVector3);
begin
  V1[0] VectorOpVar V2[0];
  V1[1] VectorOpVar V2[1];
  V1[2] VectorOpVar V2[2];
end;

procedure VectorOpVarFuncName(var v1: TVector4; const v2: TVector4);
begin
  V1[0] VectorOpVar V2[0];
  V1[1] VectorOpVar V2[1];
  V1[2] VectorOpVar V2[2];
  V1[3] VectorOpVar V2[3];
end;
}

  {$define VectorOpFuncName := VectorSubtract}
  {$define VectorOpVarFuncName := VectorSubtractVar}
  {$define VectorOp := -}
  {$define VectorOpVar := -=}
  VECTOR_OP_FUNCS

  {$define VectorOpFuncName := VectorAdd}
  {$define VectorOpVarFuncName := VectorAddVar}
  {$define VectorOp:= +}
  {$define VectorOpVar:= +=}
  VECTOR_OP_FUNCS

{$undef VectorOpFuncName}
{$undef VectorOpVarFuncName}
{$undef VectorOp}
{$undef VectorOpVar}
{$undef VECTOR_OP_FUNCS}

function VectorScale(const v1: TVector2; const Scalar: TScalar): TVector2;
begin
  Result[0] := V1[0] * Scalar;
  Result[1] := V1[1] * Scalar;
end;

function VectorScale(const v1: TVector3; const Scalar: TScalar): TVector3;
begin
  Result[0] := V1[0] * Scalar;
  Result[1] := V1[1] * Scalar;
  Result[2] := V1[2] * Scalar;
end;

function VectorScale(const v1: TVector4; const Scalar: TScalar): TVector4;
begin
  Result[0] := V1[0] * Scalar;
  Result[1] := V1[1] * Scalar;
  Result[2] := V1[2] * Scalar;
  Result[3] := V1[3] * Scalar;
end;

procedure VectorScaleVar(var v1: TVector2; const Scalar: TScalar);
begin
  V1[0] *= Scalar;
  V1[1] *= Scalar;
end;

procedure VectorScaleVar(var v1: TVector3; const Scalar: TScalar);
begin
  V1[0] *= Scalar;
  V1[1] *= Scalar;
  V1[2] *= Scalar;
end;

procedure VectorScaleVar(var v1: TVector4; const Scalar: TScalar);
begin
  V1[0] *= Scalar;
  V1[1] *= Scalar;
  V1[2] *= Scalar;
  V1[3] *= Scalar;
end;

function VectorNegate(const v: TVector2): TVector2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

function VectorNegate(const v: TVector3): TVector3;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
end;

function VectorNegate(const v: TVector4): TVector4;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
  Result[3] := -v[3];
end;

procedure VectorNegateVar(var v: TVector2);
begin
  v[0] := -v[0];
  v[1] := -v[1];
end;

procedure VectorNegateVar(var v: TVector3);
begin
  v[0] := -v[0];
  v[1] := -v[1];
  v[2] := -v[2];
end;

procedure VectorNegateVar(var v: TVector4);
begin
  v[0] := -v[0];
  v[1] := -v[1];
  v[2] := -v[2];
  v[3] := -v[3];
end;

function VectorProduct(const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[1]*V2[2] - V1[2]*V2[1];
  Result[1] := V1[2]*V2[0] - V1[0]*V2[2];
  Result[2] := V1[0]*V2[1] - V1[1]*V2[0];
end;

function VectorAbs(const V: TVector2): TVector2;
begin
  Result[0] := Abs(V[0]);
  Result[1] := Abs(V[1]);
end;

function VectorAbs(const V: TVector3): TVector3;
begin
  Result[0] := Abs(V[0]);
  Result[1] := Abs(V[1]);
  Result[2] := Abs(V[2]);
end;

function VectorAbs(const V: TVector4): TVector4;
begin
  Result[0] := Abs(V[0]);
  Result[1] := Abs(V[1]);
  Result[2] := Abs(V[2]);
  Result[3] := Abs(V[3]);
end;

function VectorDotProduct(const V1, V2: TVector2): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1];
end;

function VectorDotProduct(const V1, V2: TVector3): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1]+ V1[2]*V2[2];
end;

function VectorDotProduct(const V1, V2: TVector4): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1]+ V1[2]*V2[2]+ V1[3]*V2[3];
end;

function VectorDotProduct(const V1: TVector3; const V2: TVector4): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1]+ V1[2]*V2[2]+       V2[3];
end;

function VectorMultiplyComponents(const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[0] * V2[0];
  Result[1] := V1[1] * V2[1];
  Result[2] := V1[2] * V2[2];
end;

procedure VectorMultiplyComponentsVar(var V1: TVector3; const V2: TVector3);
begin
  V1[0] *= V2[0];
  V1[1] *= V2[1];
  V1[2] *= V2[2];
end;

function SwapEndian(const V: TVector2): TVector2;
begin
  Result[0] := SwapEndian(V[0]);
  Result[1] := SwapEndian(V[1]);
end;

function SwapEndian(const V: TVector3): TVector3;
begin
  Result[0] := SwapEndian(V[0]);
  Result[1] := SwapEndian(V[1]);
  Result[2] := SwapEndian(V[2]);
end;

function SwapEndian(const V: TVector4): TVector4;
begin
  Result[0] := SwapEndian(V[0]);
  Result[1] := SwapEndian(V[1]);
  Result[2] := SwapEndian(V[2]);
  Result[3] := SwapEndian(V[3]);
end;

function NtoLE(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function NtoLE(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function NtoLE(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function NtoBE(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function NtoBE(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function NtoBE(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function LEtoN(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function LEtoN(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function LEtoN(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function BEtoN(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function BEtoN(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function BEtoN(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

procedure SwapValues(var V1, V2: TVector2);
var
  Tmp: TVector2;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

procedure SwapValues(var V1, V2: TVector3);
var
  Tmp: TVector3;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

procedure SwapValues(var V1, V2: TVector4);
var
  Tmp: TVector4;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

function VectorAverage(const V: TVector3): TScalar;
begin
  Result := (V[0] + V[1] + V[2]) / 3;
end;

operator := (const V: TVector2_): TVector2;
begin
  Result := V.Data;
end;

operator := (const V: TVector3_): TVector3;
begin
  Result := V.Data;
end;

operator := (const V: TVector4_): TVector4;
begin
  Result := V.Data;
end;

operator := (const V: TVector2): TVector2_;
begin
  Result.Data := V;
end;

operator := (const V: TVector3): TVector3_;
begin
  Result.Data := V;
end;

operator := (const V: TVector4): TVector4_;
begin
  Result.Data := V;
end;

procedure NormalizeVar(var V: TVector2);
var
  len: TScalar;
begin
  len := Sqrt( Sqr(V[0]) + Sqr(V[1]) );
  if len = 0.0 then exit;
  V[0] /= len;
  V[1] /= len;
end;

procedure NormalizeVar(var V: TVector3);
var
  len: TScalar;
begin
  len := Sqrt( Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]) );
  if len = 0.0 then exit;
  V[0] /= len;
  V[1] /= len;
  V[2] /= len;
end;

procedure NormalizePlaneVar(var v: TVector4);
var
  len: TScalar;
begin
  len := Sqrt( Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]) );
  V[0] /= len;
  V[1] /= len;
  V[2] /= len;
  V[3] /= len;
end;

function ZeroVector(const V: TVector2): boolean;
begin
  result := (Abs(V[0]) < ScalarEqualityEpsilon) and
            (Abs(V[1]) < ScalarEqualityEpsilon);
end;

function ZeroVector(const V: TVector3): boolean;
begin
  result := (Abs(V[0]) < ScalarEqualityEpsilon) and
            (Abs(V[1]) < ScalarEqualityEpsilon) and
            (Abs(V[2]) < ScalarEqualityEpsilon);
end;

function ZeroVector(const V: TVector4): boolean;
begin
  result := (Abs(V[0]) < ScalarEqualityEpsilon) and
            (Abs(V[1]) < ScalarEqualityEpsilon) and
            (Abs(V[2]) < ScalarEqualityEpsilon) and
            (Abs(V[3]) < ScalarEqualityEpsilon);
end;

function ZeroVector(const V: TVector2; const EqualityEpsilon: TScalar): boolean;
begin
  result := (Abs(V[0]) < EqualityEpsilon) and
            (Abs(V[1]) < EqualityEpsilon);
end;

function ZeroVector(const V: TVector3; const EqualityEpsilon: TScalar): boolean;
begin
  result := (Abs(V[0]) < EqualityEpsilon) and
            (Abs(V[1]) < EqualityEpsilon) and
            (Abs(V[2]) < EqualityEpsilon);
end;

function ZeroVector(const V: TVector4; const EqualityEpsilon: TScalar): boolean;
begin
  result := (Abs(V[0]) < EqualityEpsilon) and
            (Abs(V[1]) < EqualityEpsilon) and
            (Abs(V[2]) < EqualityEpsilon) and
            (Abs(V[3]) < EqualityEpsilon);
end;

function PerfectlyZeroVector(const V: TVector2): boolean;
begin
  Result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function PerfectlyZeroVector(const V: TVector3): boolean;
begin
  Result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function PerfectlyZeroVector(const V: TVector4): boolean;
begin
  Result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function VectorAdjustToLength(const v: TVector2; VecLen: TScalar): TVector2;
begin
  result := VectorScale(v, VecLen/VectorLen(v));
end;

function VectorAdjustToLength(const v: TVector3; VecLen: TScalar): TVector3;
begin
  result := VectorScale(v, VecLen/VectorLen(v));
end;

procedure VectorAdjustToLengthVar(var v: TVector2; VecLen: TScalar);
begin
  VectorScaleVar(v, VecLen/VectorLen(v));
end;

procedure VectorAdjustToLengthVar(var v: TVector3; VecLen: TScalar);
begin
  VectorScaleVar(v, VecLen/VectorLen(v));
end;

function VectorLen(const v: TVector2): TScalar;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TVector2): TScalar;
begin
  Result := Sqr(v[0]) + Sqr(v[1]);
end;

function VectorLen(const v: TVector3): TScalar;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TVector3): TScalar;
begin
  Result := Sqr(v[0]) + Sqr(v[1]) + Sqr(v[2]);
end;

function VectorLen(const v: TVector4): TScalar;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TVector4): TScalar;
begin
  Result := Sqr(v[0]) + Sqr(v[1]) + Sqr(v[2]) + Sqr(v[3]);
end;

function VectorPowerComponents(const v: TVector3; const Exp: TScalar): TVector3;
begin
  Result[0] := Power(v[0], Exp);
  Result[1] := Power(v[1], Exp);
  Result[2] := Power(v[2], Exp);
end;

procedure VectorPowerComponentsVar(var v: TVector3; const Exp: TScalar);
begin
  v[0] := Power(v[0], Exp);
  v[1] := Power(v[1], Exp);
  v[2] := Power(v[2], Exp);
end;

function CosAngleBetweenVectors(const V1, V2: TVector3): TScalar;
var
  LensSquared: Float;
begin
  (* jak widac, wykrecam sie jednym pierwiastkowaniem pierwiastkujac
     VectorLenSqr(v1) i VectorLenSqr(v2) jednoczesnie. *)

  LensSquared := VectorLenSqr(v1) * VectorLenSqr(v2);
  if LensSquared < ScalarEqualityEpsilon then
    raise EVectorInvalidOp.Create(
      'Cannot calculate angle between vectors, at least one of the vectors is zero');

  (* musimy robic tu Clamp do (-1, 1) bo praktyka pokazala ze czasami na skutek
     bledow obliczen zmiennoprzec. wynik tej funkcji jest maciupinke poza
     zakresem. A Cosinum musi byc w zakresie -1..1, w szczegolnosci
     ArcCos() dla czegos choc troche poza zakresem wywala paskudny EInvalidArgument *)
  result := Clamped(
    VectorDotProduct(V1, V2) / Sqrt(LensSquared), -1.0, 1.0);
end;

function AngleRadBetweenVectors(const V1, V2: TVector3): TScalar;
begin
  result := ArcCos(CosAngleBetweenVectors(V1, V2));
end;

function CosAngleBetweenNormals(const V1, V2: TVector3): TScalar;
begin
  result := Clamped(VectorDotProduct(V1, V2), -1.0, 1.0);
end;

function AngleRadBetweenNormals(const V1, V2: TVector3): TScalar;
begin
  result := ArcCos(CosAngleBetweenNormals(V1, V2));
end;

function RotationAngleRadBetweenVectors(const V1, V2, Cross: TVector3): TScalar;
begin
  Result := AngleRadBetweenVectors(V1, V2);
  if PointsDistanceSqr(RotatePointAroundAxisRad( Result, V1, Cross), V2) >
     PointsDistanceSqr(RotatePointAroundAxisRad(-Result, V1, Cross), V2) then
    Result := -Result;

  { Note that an assertion here that

      PointsDistance(RotatePointAroundAxisRad(Result, V1, Cross), V2)

    is zero would *not* be correct: V1 and V2 may have different
    lengths, and then really neither Result nor -Result will get
    V1 to rotate exactly to V2. However, the algorithm is still correct:
    The valid Result (AngleRadBetweenVectors or -AngleRadBetweenVectors)
    will result in shorter distance (difference between V1 and V2 lengths),
    the invalid result would for sure make longer distance. }

{ Commented out by default because this assertion is costly.
  But it should be valid, you can uncomment it for test!

  Assert(FloatsEqual(
    PointsDistance(RotatePointAroundAxisRad(Result, V1, Cross), V2),
    Abs(VectorLen(V1) - VectorLen(V2)), 0.01));
}
end;

function RotationAngleRadBetweenVectors(const V1, V2: TVector3): TScalar;
begin
  Result := RotationAngleRadBetweenVectors(V1, V2, VectorProduct(V1, V2));
end;

function Normalized(const v: TVector3): TVector3;
begin
  Result := v;
  NormalizeVar(result);
end;

function Normalized(const v: TVector2): TVector2;
begin
  Result := v;
  NormalizeVar(result);
end;

function Vector_Get_Normalized(const V: TVector3_): TVector3_;
begin
  Result.Data := V.Data;
  NormalizeVar(Result.Data);
end;

procedure Vector_Normalize(var V: TVector3_);
begin
  NormalizeVar(V.Data);
end;

function RotatePointAroundAxisDeg(Angle: TScalar; const Point: TVector3;
  const Axis: TVector3): TVector3;
begin
  result := RotatePointAroundAxisRad(DegToRad(Angle), Point, Axis);
end;

function RotatePointAroundAxisRad(Angle: TScalar; const Point: TVector3;
  const Axis: TVector3): TVector3;
var
  x, y,z, l: TScalar;
  sinAngle, cosAngle: Float;
begin
  SinCos(Angle, sinAngle, cosAngle);
  l := VectorLen(Axis);

  { normalize and decompose Axis vector }
  x := Axis[0]/l;
  y := Axis[1]/l;
  z := Axis[2]/l;

  Result[0] := (cosAngle + (1 - cosAngle) * x * x)     * Point[0]
            + ((1 - cosAngle) * x * y - z * sinAngle)  * Point[1]
            + ((1 - cosAngle) * x * z + y * sinAngle)  * Point[2];

  Result[1] := ((1 - cosAngle) * x * y + z * sinAngle)  * Point[0]
            + (cosAngle + (1 - cosAngle) * y * y)       * Point[1]
            + ((1 - cosAngle) * y * z - x * sinAngle)   * Point[2];

  Result[2] := ((1 - cosAngle) * x * z - y * sinAngle)  * Point[0]
            + ((1 - cosAngle) * y * z + x * sinAngle)   * Point[1]
            + (cosAngle + (1 - cosAngle) * z * z)       * Point[2];
end;

function MaxVectorCoord(const v: TVector2): integer;
begin
  result := 0;
  if v[1] > v[result] then result := 1;
end;

function MaxVectorCoord(const v: TVector3): integer;
begin
  result := 0;
  { order of comparisons is important. We start from 0, then 1 and 2,
    and change only when differ (>, not just >=). This way we
    guarantee that when values are equal, lower coordinate wins. }
  if v[1] > v[result] then result := 1;
  if v[2] > v[result] then result := 2;
end;

function MinVectorCoord(const v: TVector3): integer;
begin
  result := 0;
  if v[1] < v[result] then result := 1;
  if v[2] < v[result] then result := 2;
end;

function MaxVectorCoord(const v: TVector4): integer;
begin
  result := 0;
  if v[1] > v[result] then result := 1;
  if v[2] > v[result] then result := 2;
  if v[3] > v[result] then result := 3;
end;

function MaxAbsVectorCoord(const v: TVector2): integer;
begin
  result := 0;
  if Abs(v[1]) > Abs(v[result]) then result := 1;
end;

function MaxAbsVectorCoord(const v: TVector3): integer;
begin
  result := 0;
  if Abs(v[1]) > Abs(v[result]) then result := 1;
  if Abs(v[2]) > Abs(v[result]) then result := 2;
end;

procedure SortAbsVectorCoord(const v: TVector3;
  out Max, Middle, Min: Integer);
begin
  Max := 0;
  if Abs(V[1]) > Abs(V[Max]) then Max := 1;
  if Abs(V[2]) > Abs(V[Max]) then Max := 2;
  case Max of
    0: if Abs(V[1]) >= Abs(V[2]) then begin Middle := 1; Min := 2; end else begin Middle := 2; Min := 1; end;
    1: if Abs(V[0]) >= Abs(V[2]) then begin Middle := 0; Min := 2; end else begin Middle := 2; Min := 0; end;
    else {2: }
       if Abs(V[0]) >= Abs(V[1]) then begin Middle := 0; Min := 1; end else begin Middle := 1; Min := 0; end;
  end;
end;

function PlaneDirInDirection(const Plane: TVector4; const Direction: TVector3): TVector3;
var
  PlaneDir: TVector3 absolute Plane;
begin
  result := PlaneDirInDirection(PlaneDir, Direction);
end;

function PlaneDirInDirection(const PlaneDir, Direction: TVector3): TVector3;
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

function PlaneDirNotInDirection(const Plane: TVector4; const Direction: TVector3): TVector3;
var
  PlaneDir: TVector3 absolute Plane;
begin
  result := PlaneDirNotInDirection(PlaneDir, Direction);
end;

procedure TwoPlanesIntersectionLine(const Plane0, Plane1: TVector4;
  out Line0, LineVector: TVector3);
var
  Plane0Dir: TVector3 absolute Plane0;
  Plane1Dir: TVector3 absolute Plane1;
  NonZeroIndex, Index1, Index2: Integer;
  PlaneWithNonZeroIndex1: PVector4;
  PlaneMultiply, Sum_Index2, Sum_3: TScalar;
begin
  LineVector := VectorProduct(Plane0Dir, Plane1Dir);

  NonZeroIndex := MaxAbsVectorCoord(LineVector);
  if Zero(LineVector[NonZeroIndex]) then
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
  if not Zero(Plane0[Index1]) then
  begin
    PlaneWithNonZeroIndex1 := @Plane0;
    PlaneMultiply := - Plane1[Index1] / Plane0[Index1];
    Sum_Index2 := Plane0[Index2] * PlaneMultiply + Plane1[Index2];
    Sum_3      := Plane0[3]      * PlaneMultiply + Plane1[3];
  end else
  if not Zero(Plane1[Index1]) then
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

function Lines2DIntersection(const Line0, Line1: TVector3): TVector2;
var
  Ratio, Divide: TScalar;
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
  const Plane0, Plane1, Plane2: TVector4): TVector3;
var
  Line0, LineVector: TVector3;
begin
  TwoPlanesIntersectionLine(Plane0, Plane1, Line0, LineVector);
  if not TryPlaneLineIntersection(Result, Plane2, Line0, LineVector) then
    raise Exception.Create('Cannot calculate intersection point of three planes :' +
      'intersection line of first two planes is parallel to the 3rd plane');
end;

function PlaneMove(const Plane: TVector4;
  const Move: TVector3): TVector4;
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

procedure PlaneMoveVar(var Plane: TVector4; const Move: TVector3);
begin
  Plane[3] -= Plane[0] * Move[0] +
              Plane[1] * Move[1] +
              Plane[2] * Move[2];
end;

function PlaneAntiMove(const Plane: TVector4;
  const Move: TVector3): TVector4;
begin
  { Like PlaneMove, but Move vector is negated.
    So we just do "Result[3] +=" instead of "Result[3] -=". }

  Result := Plane;
  Result[3] += Plane[0] * Move[0] +
               Plane[1] * Move[1] +
               Plane[2] * Move[2];
end;

{$define VectorsSamePlaneDirections_Implement:=
var
  v1dot, v2dot: TScalar;
begin
  v1dot := VectorDotProduct(v1, PlaneDir);
  v2dot := VectorDotProduct(v2, PlaneDir);
  result := Zero(v1dot) or Zero(v2dot) or ((v1dot > 0) = (v2dot > 0));
end;}

  function VectorsSamePlaneDirections(const V1, V2: TVector3;
    const Plane: TVector4): boolean;
  var PlaneDir: TVector3 absolute Plane;
  VectorsSamePlaneDirections_Implement

  function VectorsSamePlaneDirections(const V1, V2: TVector3;
    const PlaneDir: TVector3): boolean;
  VectorsSamePlaneDirections_Implement

{$undef VectorsSamePlaneDirections_Implement}

function PointsSamePlaneSides(const p1, p2: TVector3; const Plane: TVector4): boolean;
var
  p1Side, p2Side: TScalar;
begin
  p1Side := p1[0]*Plane[0] + p1[1]*Plane[1] + p1[2]*Plane[2] + Plane[3];
  p2Side := p2[0]*Plane[0] + p2[1]*Plane[1] + p2[2]*Plane[2] + Plane[3];
  result := Zero(p1Side) or Zero(p2Side) or ((p1Side > 0) = (p2Side > 0));
end;

function PlaneDirNotInDirection(const PlaneDir, Direction: TVector3): TVector3;
begin
  if VectorDotProduct(Direction, PlaneDir) > 0 then
    result := VectorNegate(PlaneDir) else
    result := PlaneDir;
end;

function PointsDistance(const V1, V2: TVector2): TScalar;
begin
  { Result := Sqrt(PointsDistanceSqr(V1, V2));, expanded for speed }
  result := Sqrt( Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]) );
end;

function PointsDistance(const V1, V2: TVector3): TScalar;
begin
  { Result := Sqrt(PointsDistanceSqr(V1, V2));, expanded for speed }
  result := Sqrt( Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]) + Sqr(V2[2]-V1[2]) );
end;

function PointsDistanceSqr(const V1, V2: TVector3): TScalar;
begin
  { Result := VectorLenSqr(VectorSubtract(v2, v1));, expanded for speed }
  result := Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]) + Sqr(V2[2]-V1[2]);
end;

function PointsDistanceSqr(const V1, V2: TVector2): TScalar;
begin
  { Result := VectorLenSqr(VectorSubtract(v2, v1));, expanded for speed }
  result := Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]);
end;

function PointsDistance2DSqr(const V1, V2: TVector3; const IgnoreIndex: Integer): TScalar;
begin
  case IgnoreIndex of
    0: Result := Sqr(V2[1] - V1[1]) + Sqr(V2[2] - V1[2]);
    1: Result := Sqr(V2[2] - V1[2]) + Sqr(V2[0] - V1[0]);
    2: Result := Sqr(V2[0] - V1[0]) + Sqr(V2[1] - V1[1]);
    else PointsDistance2DSqr_InvalidIgnoreIndex;
  end;
end;

function VectorsEqual(const V1, V2: TVector2): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) else
    Result := (Abs(V1[0]-V2[0]) < ScalarEqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < ScalarEqualityEpsilon);
end;

function VectorsEqual(const V1, V2: TVector2;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) else
    Result := (Abs(V1[0]-V2[0]) < EqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < EqualityEpsilon);
end;

function VectorsEqual(const V1, V2: TVector3): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) else
    Result := FloatsEqual(V1[0], V2[0]) and
              FloatsEqual(V1[1], V2[1]) and
              FloatsEqual(V1[2], V2[2]);
end;

function VectorsEqual(const V1, V2: TVector3;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) else
    Result := (Abs(V1[0]-V2[0]) < EqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < EqualityEpsilon) and
              (Abs(V1[2]-V2[2]) < EqualityEpsilon);
end;

function VectorsEqual(const V1, V2: TVector4): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) and
              (V1[3] = V2[3]) else
    Result := FloatsEqual(V1[0], V2[0]) and
              FloatsEqual(V1[1], V2[1]) and
              FloatsEqual(V1[2], V2[2]) and
              FloatsEqual(V1[3], V2[3]);
end;

function VectorsEqual(const V1, V2: TVector4;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) and
              (V1[3] = V2[3]) else
    Result := (Abs(V1[0]-V2[0]) < EqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < EqualityEpsilon) and
              (Abs(V1[2]-V2[2]) < EqualityEpsilon) and
              (Abs(V1[3]-V2[3]) < EqualityEpsilon);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2): boolean;
{$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3): boolean;
{$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4): boolean;
{$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]) and
            (V1[3] = V2[3]);
end;

function MatricesEqual(const M1, M2: TMatrix3;
  const EqualityEpsilon: TScalar): boolean;
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

function MatricesEqual(const M1, M2: TMatrix4;
  const EqualityEpsilon: TScalar): boolean;
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

function Lerp(const A: TScalar; const M1, M2: TMatrix3): TMatrix3;
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

function Lerp(const A: TScalar; const M1, M2: TMatrix4): TMatrix4;
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

function Lerp(const A: TScalar; const V1, V2: TVector2): TVector2;
begin
  Result[0] := V1[0] + a*(V2[0]-V1[0]);
  Result[1] := V1[1] + a*(V2[1]-V1[1]);
end;

function Lerp(const A: TScalar; const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[0] + a*(V2[0]-V1[0]);
  Result[1] := V1[1] + a*(V2[1]-V1[1]);
  Result[2] := V1[2] + a*(V2[2]-V1[2]);
end;

function Lerp(const A: TScalar; const V1, V2: TVector4): TVector4;
begin
  Result[0] := V1[0] + a*(V2[0]-V1[0]);
  Result[1] := V1[1] + a*(V2[1]-V1[1]);
  Result[2] := V1[2] + a*(V2[2]-V1[2]);
  Result[3] := V1[3] + a*(V2[3]-V1[3]);
end;

function MatricesPerfectlyEqual(const M1, M2: TMatrix3): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

function MatricesPerfectlyEqual(const M1, M2: TMatrix4): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

function VectorsPerp(const V1, V2: TVector3): boolean;
begin
  (* prosto : result := CosAngleBetweenVectors(V1, V2) = 0.
     Ale mozna zobaczyc jak liczymy CosAngleBetweenVectors - to jest
     VectorDotProduct / cos-tam. Wynik jest = 0 <=> VectorDotProduct = 0. *)
  result := Zero(VectorDotProduct(V1, V2), ScalarEqualityEpsilon*2);
end;

function VectorsParallel(const V1, V2: TVector3): boolean;
var
  mc, c1, c2: Integer;
  Scale: TScalar;
begin
  mc := MaxAbsVectorCoord(v1);
  if Zero(V1[mc]) then Exit(true);

  Scale := V2[mc] / V1[mc];
  RestOf3dCoords(mc, c1, c2);
  result := FloatsEqual(V1[c1] * Scale, V2[c1]) and
            FloatsEqual(V1[c2] * Scale, V2[c2]);
end;

procedure MakeVectorsAngleRadOnTheirPlane(var v1: TVector3;
  const v2: TVector3; const AngleRad: TScalar; const ResultWhenParallel: TVector3);
var
  rotAxis: TVector3;
  v1len: TScalar;
begin
  v1len := VectorLen(v1);
  rotAxis := VectorProduct(V1, V2);
  if ZeroVector(rotAxis) then
    V1 := ResultWhenParallel else
    V1 := VectorAdjustToLength(
      RotatePointAroundAxisRad(-AngleRad, v2, rotAxis), v1len);
end;

procedure MakeVectorsOrthoOnTheirPlane(var v1: TVector3; const v2: TVector3);
begin
  { TODO: can we speed this up ?
    For Pi/2, the RotatePointAroundAxisRad can probably be speed up. }
  MakeVectorsAngleRadOnTheirPlane(V1, V2, Pi / 2, AnyOrthogonalVector(V2));
end;

function AnyOrthogonalVector(const v: TVector3): TVector3;
begin
  { This uses a simple trick to make an orthogonal vector:
    if you take @code(Result := (V[1], -V[0], 0)) then the dot product
    between the Result and V is zero, so they are orthogonal.
    There's also a small check needed to use a similar but different version
    when the only non-zero component of V is V[2]. }

  if Zero(v[0]) and Zero(v[1]) then
  begin
    Result[0] := 0;
    Result[1] := v[2];
    Result[2] := -v[1];
  end else
  begin
    Result[0] := v[1];
    Result[1] := -v[0];
    Result[2] := 0;
  end;
end;

function AnyOrthogonalVector(const v: TVector2): TVector2;
begin
  Result[0] :=  V[1];
  Result[1] := -V[0];
end;

function IsLineParallelToPlane(const lineVector: TVector3; const plane: TVector4): boolean;
var
  PlaneDir: TVector3 absolute plane;
begin
  result := VectorsPerp(lineVector, PlaneDir);
end;

function IsLineParallelToSimplePlane(const lineVector: TVector3;
  const PlaneConstCoord: integer): boolean;
begin
  result := Zero(lineVector[PlaneConstCoord]);
end;

function AreParallelVectorsSameDirection(
  const Vector1, Vector2: TVector3): boolean;
var
  Coord: Integer;
begin
  { Assuming that Vector1 is non-zero, MaxAbsVectorCoord(Vector1)
    must be non-zero. }
  Coord := MaxAbsVectorCoord(Vector1);

  Result := (Vector1[Coord] > 0) = (Vector2[Coord] > 0);
end;

function PointOnPlaneClosestToPoint(const plane: TVector4; const point: TVector3): TVector3;
var
  d: TScalar;
  PlaneDir: TVector3 absolute plane;
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

function PointToPlaneDistanceSqr(const Point: TVector3;
  const Plane: TVector4): TScalar;
begin
  Result :=
    Sqr(Plane[0] * Point[0] +
        Plane[1] * Point[1] +
        Plane[2] * Point[2] +
        Plane[3]) /
    (Sqr(Plane[0]) + Sqr(Plane[1]) + Sqr(Plane[2]));
end;

function PointToNormalizedPlaneDistance(const Point: TVector3;
  const Plane: TVector4): TScalar;
begin
  Result :=
    Abs(Plane[0] * Point[0] +
        Plane[1] * Point[1] +
        Plane[2] * Point[2] +
        Plane[3]);
end;

function PointToPlaneDistance(const Point: TVector3;
  const Plane: TVector4): TScalar;
begin
  Result :=
    Abs(Plane[0] * Point[0] +
        Plane[1] * Point[1] +
        Plane[2] * Point[2] +
        Plane[3]) /
    Sqrt(Sqr(Plane[0]) + Sqr(Plane[1]) + Sqr(Plane[2]));
end;

function PointToSimplePlaneDistance(const point: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar): TScalar;
begin
  result := Abs(point[PlaneConstCoord]-PlaneConstValue);
end;

function PointOnLineClosestToPoint(
  const line0, lineVector, point: TVector3): TVector3;
var
  d: TScalar;
begin
  (*
   wiemy ze wektory result-point i lineVector (albo result-line0) sa prostopadle.
   (Result[0]-point[0])*(lineVector[0]) +
   (Result[1]-point[1])*(lineVector[1]) +
   (Result[2]-point[2])*(lineVector[2]) = 0 czyli
   Result[0]*lineVector[0] +
   Result[1]*lineVector[1] +
   Result[2]*lineVector[2] = point[0]*lineVector[0] +
                             point[1]*lineVector[1] +
                             point[2]*lineVector[2]
   Wiemy ze result wyraza sie jako line0 + lineVector*d
   result = line0+lineVector*d
   czyli
   Result[0] = line0[0] + lineVector[0]*d
   Result[1] = line0[1] + lineVector[1]*d
   Result[2] = line0[2] + lineVector[2]*d
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

function PointOnLineClosestToPoint(
  const line0, lineVector, point: TVector2): TVector2;
var
  d: TScalar;
begin
  d := (lineVector[0] * (point[0]-line0[0]) +
        lineVector[1] * (point[1]-line0[1]) ) / VectorLenSqr(lineVector);
  result := VectorAdd(line0, VectorScale(lineVector, d));
end;

function PointToLineDistanceSqr(const point, line0, lineVector: TVector3): TScalar;
begin
  result := PointsDistanceSqr(point, PointOnLineClosestToPoint(line0, lineVector, point));
end;

function TryPlaneLineIntersection(out t: TScalar;
  const plane: TVector4; const line0, lineVector: TVector3): boolean;
var
  PlaneDir: TVector3 absolute plane;
  Dot: TScalar;
begin
  Dot := VectorDotProduct(LineVector, PlaneDir);
  if not Zero(Dot) then
  begin
    result := true;
    t := -(plane[0]*line0[0] + plane[1]*line0[1] + plane[2]*line0[2] + plane[3])/Dot;
  end else
    result := false;
end;

function TryPlaneLineIntersection(out intersection: TVector3;
  const plane: TVector4; const line0, lineVector: TVector3): boolean;
var
  t: TScalar;
begin
  result := TryPlaneLineIntersection(t, Plane, Line0, LineVector);
  if result then Intersection := VectorAdd(Line0, VectorScale(LineVector, t));
end;

function TryPlaneRayIntersection(out Intersection: TVector3;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, RayOrigin, RayDirection) and (MaybeT >= 0);
  if result then Intersection := VectorAdd(RayOrigin, VectorScale(RayDirection, MaybeT));
end;

function TryPlaneRayIntersection(
  out Intersection: TVector3; out T: TScalar;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, RayOrigin, RayDirection) and (MaybeT >= 0);
  if result then
  begin
    // Intersection := VectorAdd(RayOrigin, VectorScale(RayDirection, MaybeT));
    // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
    Intersection := RayDirection;
    VectorScaleVar(Intersection, MaybeT);
    VectorAddVar(Intersection, RayOrigin);

    t := MaybeT;
  end;
end;

function TryPlaneSegmentDirIntersection(out Intersection: TVector3;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
    (MaybeT >= 0) and (MaybeT <= 1);
  if result then Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));
end;

function TryPlaneSegmentDirIntersection(
  out Intersection: TVector3; out T: TScalar;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
    (MaybeT >= 0) and (MaybeT <= 1);
  if result then
  begin
    // Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));
    // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
    Intersection := SegmentVector;
    VectorScaleVar(Intersection, MaybeT);
    VectorAddVar(Intersection, Segment0);

    t := MaybeT;
  end;
end;

(*$I castlevectors_trysimpleplanexxxintersection.inc*)

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector3): boolean;
var
  vecSizes: TVector3;
  c, i: integer;
begin
  (*rzutujemy 3 zadane punkty na ta wspolrzedna na ktorej mamy najwieksza swobode*)
  for i := 0 to High(vecSizes) do
    vecSizes[i] := Abs(pos1[i]-pos2[i]);
  c := MaxVectorCoord(vecSizes);
  result := ((pos1[c] <= intersection[c]) and (intersection[c] <= pos2[c])) or
            ((pos1[c] >= intersection[c]) and (intersection[c] >= pos2[c]));
end;

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector2): boolean;
var
  vecSizes: TVector2;
  c, i: integer;
begin
  (*rzutujemy 3 zadane punkty na ta wspolrzedna na ktorej mamy najwieksza swobode*)
  for i := 0 to High(vecSizes) do
    vecSizes[i] := Abs(pos1[i]-pos2[i]);
  c := MaxVectorCoord(vecSizes);
  result := ((pos1[c] <= intersection[c]) and (intersection[c] <= pos2[c])) or
            ((pos1[c] >= intersection[c]) and (intersection[c] >= pos2[c]));
end;

function LineOfTwoDifferentPoints2d(const p1, p2: TVector2): TVector3;
var
  lineVector: TVector2;
  cGood, cOther: integer;
begin
  (* chcemy zeby Vector2f(result) i p2-p1(=lineVector) byly prostopadle czyli ich
     iloczyn skalarny = 0 czyli Result[0]*lineVector[0] +
     Result[1]*lineVector[1] = 0. Niech cGood to wspolrzedna
     lineVector rozna od 0, cOther to ta druga.
     Niech Result[cOther] = -1 i zobaczmy ze wtedy mozemy skonstruowac
     Result[cGood] = lineVector[cOther] / lineVector[cGood]. *)
  lineVector := VectorSubtract(p2, p1);
  if Abs(lineVector[0]) > Abs(lineVector[1]) then
    begin cGood := 0; cOther := 1 end else
    begin cOther := 0; cGood := 1 end;
  Result[cOther] := -1;
  Result[cGood] := lineVector[cOther] / lineVector[cGood];

  (* Result[0]*p1[0] + Result[1]*p1[1] + Result[2] = 0 wiec widac jak obliczyc
     teraz Result[2] *)
  Result[2] := -Result[0]*p1[0] -Result[1]*p1[1];
end;

function IsSpheresCollision(const Sphere1Center: TVector3; const Sphere1Radius: TScalar;
  const Sphere2Center: TVector3; const Sphere2Radius: TScalar): boolean;
begin
  result := PointsDistanceSqr(Sphere1Center, Sphere2Center)<=
    Sqr(Sphere1Radius+Sphere2Radius);
end;

function PointToSegmentDistanceSqr(const point, pos1, pos2: TVector3): TScalar;
var
  Closest: TVector3;
begin
  Closest := PointOnLineClosestToPoint(pos1, VectorSubtract(pos2, pos1), point);
  if IsPointOnSegmentLineWithinSegment(Closest, pos1, pos2) then
    result := PointsDistanceSqr(Closest, point) else
    result := CastleUtils.min(PointsDistanceSqr(pos1, point),
                              PointsDistanceSqr(pos2, point));
end;

function PlaneTransform(const Plane: TVector4; const Matrix: TMatrix4): TVector4;
var
  MaxCoord: Integer;
  PlaneDir: TVector3 absolute Plane;
  NewPlaneDir: TVector3 absolute Result;
  PlanePoint, NewPlanePoint: TVector3;
begin
  { calculate point that for sure lies on a plane.
    For this, we need a plane direction coordinate that isn't zero
    --- we know that such coordinate exists, since plane direction cannot be zero.
    For maximum numeric stability, choose largest coordinate. }
  MaxCoord := MaxAbsVectorCoord(PlaneDir);
  PlanePoint := ZeroVector3;
  PlanePoint[MaxCoord] := -Plane[3] / Plane[MaxCoord];

  NewPlanePoint := MatrixMultPoint(Matrix, PlanePoint);
  NewPlaneDir := MatrixMultDirection(Matrix, PlaneDir);
  Result[3] := -VectorDotProduct(NewPlanePoint, NewPlaneDir);
end;

function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TVector3;
  const TunnelRadius: TScalar; const SphereCenter: TVector3;
  const SphereRadius: TScalar): boolean;
begin
  result := PointToSegmentDistanceSqr(SphereCenter, Tunnel1, Tunnel2)<=
    Sqr(SphereRadius+TunnelRadius);
end;

function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TVector3;
  const SphereRadius: TScalar): boolean;
var
  SphereRadiusSqr: TScalar;
  Intersect: TVector3;
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

{ Solve intersection routine with a ray that resolved into a quadratic
  equation. The solution is such T >= 0 that
    A * T^2 + B * T + C = 0 }
function TryRayIntersectionQuadraticEquation(out T: TScalar;
  const A, B, C: TScalar): boolean;
var
  Delta, T1, T2: TScalar;
begin
  Delta := Sqr(B) - 4 * A * C;

  if Delta < 0 then
    Result := false else
  if Delta = 0 then
  begin
    T := -B / (2 * A);
    Result := T >= 0;
  end else
  begin
    Delta := Sqrt(Delta);

    { There are two solutions, choose closest to RayOrigin (smallest)
      but >= 0 (the one < 0 does not fall on ray). }
    T1 := (-B - Delta) / (2 * A);
    T2 := (-B + Delta) / (2 * A);
    OrderUp(T1, T2);
    if T1 >= 0 then
    begin
      T := T1;
      Result := true;
    end else
    if T2 >= 0 then
    begin
      T := T2;
      Result := true;
    end else
      Result := false;
  end;
end;

function TrySphereRayIntersection(out Intersection: TVector3;
  const SphereCenter: TVector3; const SphereRadius: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  T, A, B, C: TScalar;
  RayOriginMinusCenter: TVector3;
begin
  { Intersection = RayOrigin + RayDirection * T,
    Also Distance(Intersection, SphereCenter) = SphereRadius,
    so Distance(RayOrigin + RayDirection * T, SphereCenter) = SphereRadius.

    Expand this, and use to calculate T: we get a quadratic equation for T.
    A * T^2 + B * T + C = 0. }
  RayOriginMinusCenter := RayOrigin - SphereCenter;
  A := Sqr(RayDirection[0]) +
       Sqr(RayDirection[1]) +
       Sqr(RayDirection[2]);
  B := 2 * RayDirection[0] * RayOriginMinusCenter[0] +
       2 * RayDirection[1] * RayOriginMinusCenter[1] +
       2 * RayDirection[2] * RayOriginMinusCenter[2];
  C := Sqr(RayOriginMinusCenter[0]) +
       Sqr(RayOriginMinusCenter[1]) +
       Sqr(RayOriginMinusCenter[2]) - Sqr(SphereRadius);

  Result := TryRayIntersectionQuadraticEquation(T, A, B, C);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

function TryCylinderRayIntersection(out Intersection: TVector3;
  const CylinderAxisOrigin, CylinderAxis: TVector3;
  const CylinderRadius: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  T, AA, BB, CC: TScalar;
  X, Y, B: TVector3;
begin
  { We know Intersection = RayOrigin + RayDirection * T.
    For cylinder, normalize CylinderAxis and then

      VectorLen( VectorProduct(Intersection - CylinderAxisOrigin, CylinderAxis))
      = CylinderRadius

    (For why, see http://en.wikipedia.org/wiki/Cross_product:
    length of VectorProduct is the area of parallelogram between it's vectors.
    This is equal to area of CylinderRadius * length CylinderAxis in this case.)
    Got the idea from oliii post on
    http://www.gamedev.net/community/forums/topic.asp?topic_id=467789

    Insert ray equation into cylinder equation, and solve for T. }

  X := RayOrigin - CylinderAxisOrigin;
  Y := RayDirection;
  B := Normalized(CylinderAxis);

  { Now let A = X + Y * T, then VectorLen(A x B)^2 = CylinderRadius^2.
    Solve for T. Expanding this by hand would be *real* pain,
    so I used open-source maxima (http://maxima.sourceforge.net/):

    display2d:false$
    a0: x0 + y0 * t;
    a1: x1 + y1 * t;
    a2: x2 + y2 * t;
    rsqr: (a0 * b1 - b0 * a1)^2 + (a1 * b2 - a2 * b1)^2 + (a2 * b0 + a0 * b2)^2;
    expand (rsqr);

    At this point I just took the maxima output, and grouped by
    hand 30 sum items to get AA, BB, CC such that
    AA * T^2 + BB * T + CC = 0. (I could probably let maxima do this also,
    but was too lazy to read the docs :)

    And CC gets additional "- CylinderRadius^2".
  }

  AA := Sqr(B[1])*Sqr(Y[2]) + Sqr(B[0])*Sqr(Y[2]) - 2*B[1]*B[2]*Y[1]*Y[2]
    + 2*B[0]*B[2]*Y[0]*Y[2] + Sqr(B[2])*Sqr(Y[1]) + Sqr(B[0])*Sqr(Y[1])
    - 2*B[0]*B[1]*Y[0]*Y[1] + Sqr(B[2])*Sqr(Y[0]) + Sqr(B[1])*Sqr(Y[0]);

  BB := 2*Sqr(B[1])*X[2]*Y[2] + 2*Sqr(B[0])*X[2]*Y[2] - 2*B[1]*B[2]*X[1]*Y[2]
      + 2*B[0]*B[2]*X[0]*Y[2] - 2*B[1]*B[2]*X[2]*Y[1] + 2*Sqr(B[2])*X[1]*Y[1]
      + 2*Sqr(B[0])*X[1]*Y[1] - 2*B[0]*B[1]*X[0]*Y[1] + 2*B[0]*B[2]*X[2]*Y[0]
      - 2*B[0]*B[1]*X[1]*Y[0] + 2*Sqr(B[2])*X[0]*Y[0] + 2*Sqr(B[1])*X[0]*Y[0];

  CC := Sqr(B[1])*Sqr(X[2]) + Sqr(B[0])*Sqr(X[2]) - 2*B[1]*B[2]*X[1]*X[2]
    + 2*B[0]*B[2]*X[0]*X[2] + Sqr(B[2])*Sqr(X[1]) + Sqr(B[0])*Sqr(X[1])
    - 2*B[0]*B[1]*X[0]*X[1] + Sqr(B[2])*Sqr(X[0]) + Sqr(B[1])*Sqr(X[0])
    - Sqr(CylinderRadius);

  Result := TryRayIntersectionQuadraticEquation(T, AA, BB, CC);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

(* pare funkcji *ToStr -------------------------------------------------------- *)

function FloatToNiceStr(f: TScalar): string;
begin
  result := Format('%'+FloatNiceFormat, [f]);
end;

function VectorToNiceStr(const v: array of TScalar): string;
var
  i: integer;
begin
  result := '(';
  for i := 0 to High(v)-1 do result := result +FloatToNiceStr(v[i]) +', ';
  if High(v) >= 0 then result := result +FloatToNiceStr(v[High(v)]) +')';
end;

function MatrixToNiceStr(const v: TMatrix4; const LineIndent: string): string;
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

function MatrixToRawStr(const v: TMatrix4; const LineIndent: string): string;
begin
  result := Format('%s[ %4s %4s %4s %4s ]'+nl+
                   '%s| %4s %4s %4s %4s |'+nl+
                   '%s| %4s %4s %4s %4s |'+nl+
                   '%s[ %4s %4s %4s %4s ]',
   [LineIndent, FloatToRawStr(v[0, 0]), FloatToRawStr(v[1, 0]), FloatToRawStr(v[2, 0]), FloatToRawStr(v[3, 0]),
    LineIndent, FloatToRawStr(v[0, 1]), FloatToRawStr(v[1, 1]), FloatToRawStr(v[2, 1]), FloatToRawStr(v[3, 1]),
    LineIndent, FloatToRawStr(v[0, 2]), FloatToRawStr(v[1, 2]), FloatToRawStr(v[2, 2]), FloatToRawStr(v[3, 2]),
    LineIndent, FloatToRawStr(v[0, 3]), FloatToRawStr(v[1, 3]), FloatToRawStr(v[2, 3]), FloatToRawStr(v[3, 3]) ]);
end;

function FloatToRawStr(f: TScalar): string;
begin
  result := Format('%g', [f]);
end;

function VectorToRawStr(const v: array of TScalar): string;
var i: integer;
begin
  result := '';
  for i := 0 to High(v)-1 do result += FloatToRawStr(v[i]) +' ';
  if High(v) >= 0 then result += FloatToRawStr(v[High(v)]);
end;

{ simple matrix math --------------------------------------------------------- }

function MatrixAdd(const m1, m2: TMatrix3): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := m1[i, j] + m2[i, j];
end;

procedure MatrixAddVar(var m1: TMatrix3; const m2: TMatrix3);
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      m1[i, j] += m2[i, j];
end;

function MatrixAdd(const m1, m2: TMatrix4): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := m1[i, j] + m2[i, j];
end;

procedure MatrixAddVar(var m1: TMatrix4; const m2: TMatrix4);
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      m1[i, j] += m2[i, j];
end;

function MatrixSubtract(const m1, m2: TMatrix3): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := m1[i, j] - m2[i, j];
end;

procedure MatrixSubtractVar(var m1: TMatrix3; const m2: TMatrix3);
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      m1[i, j] -= m2[i, j];
end;

function MatrixSubtract(const m1, m2: TMatrix4): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := m1[i, j] - m2[i, j];
end;

procedure MatrixSubtractVar(var m1: TMatrix4; const m2: TMatrix4);
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      m1[i, j] -= m2[i, j];
end;

function MatrixNegate(const m1: TMatrix3): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := - m1[i, j];
end;

function MatrixNegate(const m1: TMatrix4): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := - m1[i, j];
end;

function MatrixMultScalar(const m: TMatrix3; const s: TScalar): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := m[i, j]*s;
end;

function MatrixMultScalar(const m: TMatrix4; const s: TScalar): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := m[i, j]*s;
end;

function MatrixMultPoint(const m: TMatrix4;
  const pt: TVector3): TVector3;
var
  Divisor: TScalar;
begin
  { Simple implementation:
  Result := Vector3SinglePoint(MatrixMultVector(m, Vector4Single(pt))); }

  Result[0] := M[0, 0] * Pt[0] + M[1, 0] * Pt[1] + M[2, 0] * Pt[2] + M[3, 0];
  Result[1] := M[0, 1] * Pt[0] + M[1, 1] * Pt[1] + M[2, 1] * Pt[2] + M[3, 1];
  Result[2] := M[0, 2] * Pt[0] + M[1, 2] * Pt[1] + M[2, 2] * Pt[2] + M[3, 2];

  { It looks strange, but the check below usually pays off.

    Tests: 17563680 calls of this proc within Creatures.PrepareRender
    inside "The Castle", gprof says that time without this check
    is 12.01 secs and with this checks it's 8.25.

    Why ? Because in 99% of situations, the conditions "(M[0, 3] = 0) and ..."
    is true. Because that's how all usual matrices in 3D graphics
    (translation, rotation, scaling) look like.
    So usually I pay 4 comparisons (exact comparisons, not things like
    FloatsEqual) and I avoid 3 multiplications, 4 additions and
    3 divisions. }

  if not (
    (M[0, 3] = 0) and
    (M[1, 3] = 0) and
    (M[2, 3] = 0) and
    (M[3, 3] = 1)) then
  begin
    Divisor := M[0, 3] * Pt[0] + M[1, 3] * Pt[1] + M[2, 3] * Pt[2] + M[3, 3];
    if Zero(Divisor) then
      raise ETransformedResultInvalid.Create('3D point transformed by 4x4 matrix to a direction');

    Result[0] /= Divisor;
    Result[1] /= Divisor;
    Result[2] /= Divisor;
  end;
end;

function MatrixMultPoint(const m: TMatrix4; const pt: TVector2): TVector2;
var
  Res3D: TVector3;
begin
  Res3D := MatrixMultPoint(M, Vector3(Pt, 0));
  Result[0] := Res3D[0];
  Result[1] := Res3D[1];
end;

function MatrixMultDirection(const m: TMatrix4;
  const Dir: TVector3): TVector3;
var
  Divisor: TScalar;
begin
  Result[0] := M[0, 0] * Dir[0] + M[1, 0] * Dir[1] + M[2, 0] * Dir[2];
  Result[1] := M[0, 1] * Dir[0] + M[1, 1] * Dir[1] + M[2, 1] * Dir[2];
  Result[2] := M[0, 2] * Dir[0] + M[1, 2] * Dir[1] + M[2, 2] * Dir[2];

  if not (
    (M[0, 3] = 0) and
    (M[1, 3] = 0) and
    (M[2, 3] = 0) ) then
  begin
    Divisor := M[0, 3] * Dir[0] + M[1, 3] * Dir[1] + M[2, 3] * Dir[2];
    if not Zero(Divisor) then
      raise ETransformedResultInvalid.Create(Format(
        '3D direction transformed by 4x4 matrix to a point, with divisor = %f (%g), with matrix:',
        [Divisor, Divisor]) + NL + MatrixToRawStr(M, '  '));
  end;
end;

function MatrixMultDirection(const m: TMatrix4; const Dir: TVector2): TVector2;
var
  Res3D: TVector3;
begin
  Res3D := MatrixMultDirection(M, Vector3(Dir, 0));
  Result[0] := Res3D[0];
  Result[1] := Res3D[1];
end;

function MatrixMultVector(const m: TMatrix4; const v: TVector4): TVector4;
{var i, j: integer;}
begin
  {
  for i := 0 to 3 do
  begin
   Result[i] := 0;
   for j := 0 to 3 do Result[i] := Result[i] + m[j, i]*v[j];
  end;

  Code expanded for the sake of speed:}

  Result[0] := M[0, 0] * V[0] + M[1, 0] * V[1] + M[2, 0] * V[2] + M[3, 0] * V[3];
  Result[1] := M[0, 1] * V[0] + M[1, 1] * V[1] + M[2, 1] * V[2] + M[3, 1] * V[3];
  Result[2] := M[0, 2] * V[0] + M[1, 2] * V[1] + M[2, 2] * V[2] + M[3, 2] * V[3];
  Result[3] := M[0, 3] * V[0] + M[1, 3] * V[1] + M[2, 3] * V[2] + M[3, 3] * V[3];
end;

function MatrixMultVector(const m: TMatrix3; const v: TVector3): TVector3;
begin
  Result[0] := M[0, 0] * V[0] + M[1, 0] * V[1] + M[2, 0] * V[2];
  Result[1] := M[0, 1] * V[0] + M[1, 1] * V[1] + M[2, 1] * V[2];
  Result[2] := M[0, 2] * V[0] + M[1, 2] * V[1] + M[2, 2] * V[2];
end;

function MatrixMultVector(const m: TMatrix2; const v: TVector2): TVector2;
begin
  Result[0] := M[0, 0] * V[0] + M[1, 0] * V[1];
  Result[1] := M[0, 1] * V[0] + M[1, 1] * V[1];
end;

function MatrixMult(const m1, m2: TMatrix4): TMatrix4;
{var i, j, k: integer;}
begin
(*
  FillChar(result, SizeOf(result), 0);
  for i := 0 to 3 do { i = wiersze, j = kolumny }
    for j := 0 to 3 do
      for k := 0 to 3 do
        Result[j, i] += m1[k, i]*m2[j, k];
*)

  { This is code above expanded for speed sake
    (code generated by console.testy/genMultMatrix) }
  Result[0, 0] := m1[0, 0] * m2[0, 0] + m1[1, 0] * m2[0, 1] + m1[2, 0] * m2[0, 2] + m1[3, 0] * m2[0, 3];
  Result[1, 0] := m1[0, 0] * m2[1, 0] + m1[1, 0] * m2[1, 1] + m1[2, 0] * m2[1, 2] + m1[3, 0] * m2[1, 3];
  Result[2, 0] := m1[0, 0] * m2[2, 0] + m1[1, 0] * m2[2, 1] + m1[2, 0] * m2[2, 2] + m1[3, 0] * m2[2, 3];
  Result[3, 0] := m1[0, 0] * m2[3, 0] + m1[1, 0] * m2[3, 1] + m1[2, 0] * m2[3, 2] + m1[3, 0] * m2[3, 3];
  Result[0, 1] := m1[0, 1] * m2[0, 0] + m1[1, 1] * m2[0, 1] + m1[2, 1] * m2[0, 2] + m1[3, 1] * m2[0, 3];
  Result[1, 1] := m1[0, 1] * m2[1, 0] + m1[1, 1] * m2[1, 1] + m1[2, 1] * m2[1, 2] + m1[3, 1] * m2[1, 3];
  Result[2, 1] := m1[0, 1] * m2[2, 0] + m1[1, 1] * m2[2, 1] + m1[2, 1] * m2[2, 2] + m1[3, 1] * m2[2, 3];
  Result[3, 1] := m1[0, 1] * m2[3, 0] + m1[1, 1] * m2[3, 1] + m1[2, 1] * m2[3, 2] + m1[3, 1] * m2[3, 3];
  Result[0, 2] := m1[0, 2] * m2[0, 0] + m1[1, 2] * m2[0, 1] + m1[2, 2] * m2[0, 2] + m1[3, 2] * m2[0, 3];
  Result[1, 2] := m1[0, 2] * m2[1, 0] + m1[1, 2] * m2[1, 1] + m1[2, 2] * m2[1, 2] + m1[3, 2] * m2[1, 3];
  Result[2, 2] := m1[0, 2] * m2[2, 0] + m1[1, 2] * m2[2, 1] + m1[2, 2] * m2[2, 2] + m1[3, 2] * m2[2, 3];
  Result[3, 2] := m1[0, 2] * m2[3, 0] + m1[1, 2] * m2[3, 1] + m1[2, 2] * m2[3, 2] + m1[3, 2] * m2[3, 3];
  Result[0, 3] := m1[0, 3] * m2[0, 0] + m1[1, 3] * m2[0, 1] + m1[2, 3] * m2[0, 2] + m1[3, 3] * m2[0, 3];
  Result[1, 3] := m1[0, 3] * m2[1, 0] + m1[1, 3] * m2[1, 1] + m1[2, 3] * m2[1, 2] + m1[3, 3] * m2[1, 3];
  Result[2, 3] := m1[0, 3] * m2[2, 0] + m1[1, 3] * m2[2, 1] + m1[2, 3] * m2[2, 2] + m1[3, 3] * m2[2, 3];
  Result[3, 3] := m1[0, 3] * m2[3, 0] + m1[1, 3] * m2[3, 1] + m1[2, 3] * m2[3, 2] + m1[3, 3] * m2[3, 3];
end;

function MatrixMult(const m1, m2: TMatrix3): TMatrix3;
begin
  Result[0, 0] := m1[0, 0] * m2[0, 0] + m1[1, 0] * m2[0, 1] + m1[2, 0] * m2[0, 2];
  Result[1, 0] := m1[0, 0] * m2[1, 0] + m1[1, 0] * m2[1, 1] + m1[2, 0] * m2[1, 2];
  Result[2, 0] := m1[0, 0] * m2[2, 0] + m1[1, 0] * m2[2, 1] + m1[2, 0] * m2[2, 2];
  Result[0, 1] := m1[0, 1] * m2[0, 0] + m1[1, 1] * m2[0, 1] + m1[2, 1] * m2[0, 2];
  Result[1, 1] := m1[0, 1] * m2[1, 0] + m1[1, 1] * m2[1, 1] + m1[2, 1] * m2[1, 2];
  Result[2, 1] := m1[0, 1] * m2[2, 0] + m1[1, 1] * m2[2, 1] + m1[2, 1] * m2[2, 2];
  Result[0, 2] := m1[0, 2] * m2[0, 0] + m1[1, 2] * m2[0, 1] + m1[2, 2] * m2[0, 2];
  Result[1, 2] := m1[0, 2] * m2[1, 0] + m1[1, 2] * m2[1, 1] + m1[2, 2] * m2[1, 2];
  Result[2, 2] := m1[0, 2] * m2[2, 0] + m1[1, 2] * m2[2, 1] + m1[2, 2] * m2[2, 2];
end;

function MatrixMult(const m1, m2: TMatrix2): TMatrix2;
begin
  Result[0, 0] := m1[0, 0] * m2[0, 0] + m1[1, 0] * m2[0, 1];
  Result[1, 0] := m1[0, 0] * m2[1, 0] + m1[1, 0] * m2[1, 1];
  Result[0, 1] := m1[0, 1] * m2[0, 0] + m1[1, 1] * m2[0, 1];
  Result[1, 1] := m1[0, 1] * m2[1, 0] + m1[1, 1] * m2[1, 1];
end;

{ matrix transforms for 3D graphics  ----------------------------------------- }

function TranslationMatrix(const Transl: TVector3): TMatrix4Single;
begin
  result := IdentityMatrix4Single;
  Result[3, 0] := Transl[0];
  Result[3, 1] := Transl[1];
  Result[3, 2] := Transl[2];
end;

function TranslationMatrix(const X, Y, Z: TScalar): TMatrix4Single;
begin
  result := IdentityMatrix4Single;
  Result[3, 0] := X;
  Result[3, 1] := Y;
  Result[3, 2] := Z;
end;

procedure TranslationMatrices(const X, Y, Z: TScalar;
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

procedure TranslationMatrices(const Transl: TVector3;
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

procedure MultMatrixTranslation(var M: TMatrix4;
  const Transl: TVector3);
var
  NewColumn: TVector4;
begin
  NewColumn := M[3];
  NewColumn[0] += M[0, 0] * Transl[0] + M[1, 0] * Transl[1] + M[2, 0] * Transl[2];
  NewColumn[1] += M[0, 1] * Transl[0] + M[1, 1] * Transl[1] + M[2, 1] * Transl[2];
  NewColumn[2] += M[0, 2] * Transl[0] + M[1, 2] * Transl[1] + M[2, 2] * Transl[2];
  NewColumn[3] += M[0, 3] * Transl[0] + M[1, 3] * Transl[1] + M[2, 3] * Transl[2];
  M[3] := NewColumn;
end;

procedure MultMatricesTranslation(var M, MInvert: TMatrix4;
  const Transl: TVector3);
var
  NewColumn: TVector4;
  { OldLastRow may use the same space as NewColumn }
  OldLastRow: TVector4 absolute NewColumn;
begin
  NewColumn := M[3];
  NewColumn[0] += M[0, 0] * Transl[0] + M[1, 0] * Transl[1] + M[2, 0] * Transl[2];
  NewColumn[1] += M[0, 1] * Transl[0] + M[1, 1] * Transl[1] + M[2, 1] * Transl[2];
  NewColumn[2] += M[0, 2] * Transl[0] + M[1, 2] * Transl[1] + M[2, 2] * Transl[2];
  NewColumn[3] += M[0, 3] * Transl[0] + M[1, 3] * Transl[1] + M[2, 3] * Transl[2];
  M[3] := NewColumn;

  OldLastRow[0] := MInvert[0, 3];
  OldLastRow[1] := MInvert[1, 3];
  OldLastRow[2] := MInvert[2, 3];
  OldLastRow[3] := MInvert[3, 3];

  MInvert[0, 0] -= Transl[0] * OldLastRow[0];
  MInvert[1, 0] -= Transl[0] * OldLastRow[1];
  MInvert[2, 0] -= Transl[0] * OldLastRow[2];
  MInvert[3, 0] -= Transl[0] * OldLastRow[3];
  MInvert[0, 1] -= Transl[1] * OldLastRow[0];
  MInvert[1, 1] -= Transl[1] * OldLastRow[1];
  MInvert[2, 1] -= Transl[1] * OldLastRow[2];
  MInvert[3, 1] -= Transl[1] * OldLastRow[3];
  MInvert[0, 2] -= Transl[2] * OldLastRow[0];
  MInvert[1, 2] -= Transl[2] * OldLastRow[1];
  MInvert[2, 2] -= Transl[2] * OldLastRow[2];
  MInvert[3, 2] -= Transl[2] * OldLastRow[3];
end;

function TransformToCoords(const V, NewX, NewY, NewZ: TVector3): TVector3;
begin
  Result[0] := V[0] * NewX[0] + V[1] * NewY[0] + V[2] * NewZ[0];
  Result[1] := V[0] * NewX[1] + V[1] * NewY[1] + V[2] * NewZ[1];
  Result[2] := V[0] * NewX[2] + V[1] * NewY[2] + V[2] * NewZ[2];
end;

function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Result[0, i] := NewX[i];
    Result[1, i] := NewY[i];
    Result[2, i] := NewZ[i];
    Result[3, i] := NewOrigin[i];
  end;
  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformToCoordsMatrix(const NewX, NewY, NewZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Result[0, i] := NewX[i];
    Result[1, i] := NewY[i];
    Result[2, i] := NewZ[i];
  end;
  { right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4Single;
begin
  result := TransformToCoordsMatrix(NewOrigin,
    Normalized(NewX), Normalized(NewY), Normalized(NewZ));
end;

function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    { Difference between TrasformToCoords and TransformFromCoords:
      up-left 3x3 matrix is applied in a transposed manner,
      compared with TrasformToCoords. }
    Result[i, 0] := OldX[i];
    Result[i, 1] := OldY[i];
    Result[i, 2] := OldZ[i];
  end;

  { Another difference between TrasformToCoords and TransformFromCoords:
    - OldOrigin must be negated here
    - OldOrigin must have directions applied
    See e.g. Global Illumination Compendium by Philip Dutre, section (15). }
  Result[3, 0] := -VectorDotProduct(OldOrigin, OldX);
  Result[3, 1] := -VectorDotProduct(OldOrigin, OldY);
  Result[3, 2] := -VectorDotProduct(OldOrigin, OldZ);

  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformFromCoordsMatrix(const OldX, OldY, OldZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    { Difference between TrasformToCoords and TransformFromCoords:
      up-left 3x3 matrix is applied in a transposed manner,
      compared with TrasformToCoords. }
    Result[i, 0] := OldX[i];
    Result[i, 1] := OldY[i];
    Result[i, 2] := OldZ[i];
  end;

  { right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4Single;
begin
  result := TransformFromCoordsMatrix(OldOrigin,
    Normalized(OldX), Normalized(OldY), Normalized(OldZ));
end;

procedure TransformCoordsMatrices(const NewX, NewY, NewZ: TVector3;
  out ToCoords, FromCoords: TMatrix4Single);
begin
  ToCoords := TransformToCoordsMatrix(NewX, NewY, NewZ);
  FromCoords := TransformFromCoordsMatrix(NewX, NewY, NewZ);
end;

function LookAtMatrix(const Eye, Center, Up: TVector3): TMatrix4Single;
begin
  result := LookDirMatrix(Eye, VectorSubtract(Center, Eye), Up);
end;

function LookDirMatrix(const Eye, Dir, Up: TVector3): TMatrix4Single;
var
  GoodDir, GoodUp, Side: TVector3;
begin
  Side := Normalized(VectorProduct(Dir, Up));
  GoodDir := Normalized(Dir);

  { Recalculate GoodUp from Side and GoodDir. This way:
    1. We make sure GoodUp is orthogonal to Side and GoodDir.
       The Up was already orthogonal to Side,
       but it was not necessarily orthogonal to GoodDir.
    2. We make sure GoodUp is normalized (vector product of normalized
       vectors is also normalized).
    This is done looking at gluLookAt implementation in
    SGI Sample OpenGL Implementation. }
  GoodUp := VectorProduct(Side, GoodDir);

  Result := LookDirMatrix(Eye, GoodDir, Side, GoodUp);
end;

function LookDirMatrix(const Eye, Dir, Side, Up: TVector3): TMatrix4Single;
begin
  { VectorNegate on Dir, since in right-handed coordinate system
    you look in the -Z direction. }
  Result := TransformFromCoordsMatrix(Eye, Side, Up, VectorNegate(Dir));
end;

function FastLookDirMatrix(const Direction, Up: TVector3): TMatrix4Single;
var
  Side: TVector3;
  i: integer;
begin
  Side := VectorProduct(Direction, Up);

  { Make TransformToCoordsMatrix with origin zero now. }

  for i := 0 to 2 do
  begin
    Result[i, 0] := Side[i];
    Result[i, 1] := Up[i];
    Result[i, 2] := -Direction[i]; { negate Direction, since it goes to -Z }
  end;

  { bottom row and right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;

  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;

  Result[3, 3] := 1;
end;

function InverseFastLookDirMatrix(const Direction, Up: TVector3): TMatrix4Single;
var
  Side: TVector3;
  i: integer;
begin
  Side := VectorProduct(Direction, Up);

  { Inverse of LookDirMatrix is now to make
    TransformToCoordsMatrix with origin zero. }

  for i := 0 to 2 do
  begin
    Result[0, i] := Side[i];
    Result[1, i] := Up[i];
    Result[2, i] := -Direction[i]; { negate Direction, since it goes to -Z }
  end;

  { bottom row and right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;

  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;

  Result[3, 3] := 1;
end;

{ We're happily using FPC's Matrix unit to trivially implement
  matrix determinant and inverse. }

function MatrixDeterminant(const M: TMatrix2): TScalar;
var
  MM: TMatrix2_;
begin
  { Note that generally data should be transposed between
    TMatrix2_ and TMatrix2. But in this case, it's not needed,
    as the determinant of the transposition is exactly the same. }

  MM.Data := M;
  Result := MM.Determinant;
end;

function MatrixDeterminant(const M: TMatrix3): TScalar;
var
  MM: TMatrix3_;
begin
  MM.Data := M;
  Result := MM.Determinant;
end;

function MatrixDeterminant(const M: TMatrix4): TScalar;
var
  MM: TMatrix4_;
begin
  MM.Data := M;
  Result := MM.Determinant;
end;

function MatrixInverse(const M: TMatrix2; const Determinant: TScalar): TMatrix2;
var
  MM: TMatrix2_;
begin
  { Note that generally data should be transposed between
    TMatrix2_ and TMatrix2. But in this case, it's not needed,
    as the transpose of the inverse is the inverse of the transpose.
    Which means that
      Result = transpose(inverse(transpose(m))
             = transpose(transpose(inverse(m))) = just inverse(m)) }

  MM.Data := M;
  Result := MM.Inverse(Determinant).Data;
end;

function MatrixInverse(const M: TMatrix3; const Determinant: TScalar): TMatrix3;
var
  MM: TMatrix3_;
begin
  MM.Data := M;
  Result := MM.Inverse(Determinant).Data;
end;

function MatrixInverse(const M: TMatrix4; const Determinant: TScalar): TMatrix4;
var
  MM: TMatrix4_;
begin
  MM.Data := M;
  Result := MM.Inverse(Determinant).Data;
end;

function MatrixRow(const m: TMatrix2; const Row: Integer): TVector2;
begin
  Result[0] := M[0][Row];
  Result[1] := M[1][Row];
end;

function MatrixRow(const m: TMatrix3; const Row: Integer): TVector3;
begin
  Result[0] := M[0][Row];
  Result[1] := M[1][Row];
  Result[2] := M[2][Row];
end;

function MatrixRow(const m: TMatrix4; const Row: Integer): TVector4;
begin
  Result[0] := M[0][Row];
  Result[1] := M[1][Row];
  Result[2] := M[2][Row];
  Result[3] := M[3][Row];
end;

procedure MatrixTransposeVar(var M: TMatrix3);
var
  Tmp: TScalar;
begin
  Tmp := M[0, 1]; M[0, 1] := M[1, 0]; M[1, 0] := Tmp;
  Tmp := M[0, 2]; M[0, 2] := M[2, 0]; M[2, 0] := Tmp;
  Tmp := M[1, 2]; M[1, 2] := M[2, 1]; M[2, 1] := Tmp;
end;

function MatrixTranspose(const M: TMatrix3): TMatrix3;
begin
  Result[0, 0] := M[0, 0];
  Result[0, 1] := M[1, 0];
  Result[0, 2] := M[2, 0];

  Result[1, 0] := M[0, 1];
  Result[1, 1] := M[1, 1];
  Result[1, 2] := M[2, 1];

  Result[2, 0] := M[0, 2];
  Result[2, 1] := M[1, 2];
  Result[2, 2] := M[2, 2];
end;

function MatrixTranspose(const M: TMatrix4): TMatrix4;
begin
  Result[0, 0] := M[0, 0];
  Result[0, 1] := M[1, 0];
  Result[0, 2] := M[2, 0];
  Result[0, 3] := M[3, 0];

  Result[1, 0] := M[0, 1];
  Result[1, 1] := M[1, 1];
  Result[1, 2] := M[2, 1];
  Result[1, 3] := M[3, 1];

  Result[2, 0] := M[0, 2];
  Result[2, 1] := M[1, 2];
  Result[2, 2] := M[2, 2];
  Result[2, 3] := M[3, 2];

  Result[3, 0] := M[0, 3];
  Result[3, 1] := M[1, 3];
  Result[3, 2] := M[2, 3];
  Result[3, 3] := M[3, 3];
end;
