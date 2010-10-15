{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ implementacja w style VectorMath_DualImplementation, ale tutaj
  implementujemy rzeczy ktore sa zaimplementowane takze w VectorMathInlines.
  W ponizszych procedurach wersje inline NIE beda uzywane - wlasnie po to
  aby umozliwic zaimplementwanie tych wersji nie-inline.
}

{$define VECTOR_OP_FUNCS:=
function VectorOpFuncName(const V1, V2: TVector2): TVector2;
begin
 result[0] := v1[0] VectorOp v2[0];
 result[1] := v1[1] VectorOp v2[1];
end;

function VectorOpFuncName(const V1, V2: TVector3): TVector3;
begin
 result[0] := v1[0] VectorOp v2[0];
 result[1] := v1[1] VectorOp v2[1];
 result[2] := v1[2] VectorOp v2[2];
end;

function VectorOpFuncName(const V1, V2: TVector4): TVector4;
begin
 result[0] := v1[0] VectorOp v2[0];
 result[1] := v1[1] VectorOp v2[1];
 result[2] := v1[2] VectorOp v2[2];
 result[3] := v1[3] VectorOp v2[3];
end;

procedure VectorOpTo1stFuncName(var v1: TVector2; const v2: TVector2);
begin
 v1[0] VectorOpTo1st v2[0];
 v1[1] VectorOpTo1st v2[1];
end;

procedure VectorOpTo1stFuncName(var V1: TVector3; const V2: TVector3);
begin
 v1[0] VectorOpTo1st v2[0];
 v1[1] VectorOpTo1st v2[1];
 v1[2] VectorOpTo1st v2[2];
end;

procedure VectorOpTo1stFuncName(var v1: TVector4; const v2: TVector4);
begin
 v1[0] VectorOpTo1st v2[0];
 v1[1] VectorOpTo1st v2[1];
 v1[2] VectorOpTo1st v2[2];
 v1[3] VectorOpTo1st v2[3];
end;
}

  {$define VectorOpFuncName := VectorSubtract}
  {$define VectorOpTo1stFuncName := VectorSubtractTo1st}
  {$define VectorOp := -}
  {$define VectorOpTo1st := -=}
  VECTOR_OP_FUNCS

  {$define VectorOpFuncName := VectorAdd}
  {$define VectorOpTo1stFuncName := VectorAddTo1st}
  {$define VectorOp:= +}
  {$define VectorOpTo1st:= +=}
  VECTOR_OP_FUNCS

{$undef VectorOpFuncName}
{$undef VectorOpTo1stFuncName}
{$undef VectorOp}
{$undef VectorOpTo1st}
{$undef VECTOR_OP_FUNCS}

function VectorScale(const v1: TVector2; const Scalar: TScalar): TVector2;
begin
 result[0] := v1[0] * Scalar;
 result[1] := v1[1] * Scalar;
end;

function VectorScale(const v1: TVector3; const Scalar: TScalar): TVector3;
begin
 result[0] := v1[0] * Scalar;
 result[1] := v1[1] * Scalar;
 result[2] := v1[2] * Scalar;
end;

function VectorScale(const v1: TVector4; const Scalar: TScalar): TVector4;
begin
 result[0] := v1[0] * Scalar;
 result[1] := v1[1] * Scalar;
 result[2] := v1[2] * Scalar;
 result[3] := v1[3] * Scalar;
end;

procedure VectorScaleTo1st(var v1: TVector2; const Scalar: TScalar);
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
end;

procedure VectorScaleTo1st(var v1: TVector3; const Scalar: TScalar);
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
 v1[2] *= Scalar;
end;

procedure VectorScaleTo1st(var v1: TVector4; const Scalar: TScalar);
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
 v1[2] *= Scalar;
 v1[3] *= Scalar;
end;

function VectorNegate(const v: TVector2): TVector2;
begin
 result[0] := -v[0];
 result[1] := -v[1];
end;

function VectorNegate(const v: TVector3): TVector3;
begin
 result[0] := -v[0];
 result[1] := -v[1];
 result[2] := -v[2];
end;

function VectorNegate(const v: TVector4): TVector4;
begin
 result[0] := -v[0];
 result[1] := -v[1];
 result[2] := -v[2];
 result[3] := -v[3];
end;

procedure VectorNegateTo1st(var v: TVector2);
begin
 v[0] := -v[0];
 v[1] := -v[1];
end;

procedure VectorNegateTo1st(var v: TVector3);
begin
 v[0] := -v[0];
 v[1] := -v[1];
 v[2] := -v[2];
end;

procedure VectorNegateTo1st(var v: TVector4);
begin
 v[0] := -v[0];
 v[1] := -v[1];
 v[2] := -v[2];
 v[3] := -v[3];
end;

function VectorProduct(const V1, V2: TVector3): TVector3;
begin
 result[0] := v1[1]*v2[2] - v1[2]*v2[1];
 result[1] := v1[2]*v2[0] - v1[0]*v2[2];
 result[2] := v1[0]*v2[1] - v1[1]*v2[0];
end;

function VectorDotProduct(const V1, V2: TVector2): TScalar;
begin
 result := v1[0]*v2[0]+ v1[1]*v2[1];
end;

function VectorDotProduct(const V1, V2: TVector3): TScalar;
begin
 result := v1[0]*v2[0]+ v1[1]*v2[1]+ v1[2]*v2[2];
end;

function VectorDotProduct(const V1, V2: TVector4): TScalar;
begin
 result := v1[0]*v2[0]+ v1[1]*v2[1]+ v1[2]*v2[2]+ v1[3]*v2[3];
end;

function VectorDotProduct(const V1: TVector3; const V2: TVector4): TScalar;
begin
 result := v1[0]*v2[0]+ v1[1]*v2[1]+ v1[2]*v2[2]+       v2[3];
end;

function VectorMultiplyComponents(const V1, V2: TVector3): TVector3;
begin
 result[0] := v1[0] * v2[0];
 result[1] := v1[1] * v2[1];
 result[2] := v1[2] * v2[2];
end;

procedure VectorMultiplyComponentsTo1st(var V1: TVector3; const V2: TVector3);
begin
 v1[0] *= v2[0];
 v1[1] *= v2[1];
 v1[2] *= v2[2];
end;
