{
  Copyright 2003-2006 Michalis Kamburelis.

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
}

{ implementacja w style VectorMath_DualImplementation, ale tutaj
  implementujemy rzeczy ktore sa zaimplementowane takze w VectorMathInlines.
  W ponizszych procedurach wersje inline NIE beda uzywane - wlasnie po to
  aby umozliwic zaimplementwanie tych wersji nie-inline.
}

{$define VECTOR_OP_FUNCS:=
function VectorOpFuncName(const v1, v2: TYPE_VECTOR2): TYPE_VECTOR2;
begin
 result[0] := v1[0] VectorOp v2[0];
 result[1] := v1[1] VectorOp v2[1];
end;

function VectorOpFuncName(const v1, v2: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 result[0] := v1[0] VectorOp v2[0];
 result[1] := v1[1] VectorOp v2[1];
 result[2] := v1[2] VectorOp v2[2];
end;

procedure VectorOpTo1stFuncName(var v1: TYPE_VECTOR2; const v2: TYPE_VECTOR2);
begin
 v1[0] VectorOpTo1st v2[0];
 v1[1] VectorOpTo1st v2[1];
end;

procedure VectorOpTo1stFuncName(var v1: TYPE_VECTOR3; const v2: TYPE_VECTOR3);
begin
 v1[0] VectorOpTo1st v2[0];
 v1[1] VectorOpTo1st v2[1];
 v1[2] VectorOpTo1st v2[2];
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

function VectorScale(const v1: TYPE_VECTOR2; const Scalar: TYPE_SCALAR): TYPE_VECTOR2;
begin
 result[0] := v1[0] * Scalar;
 result[1] := v1[1] * Scalar;
end;

function VectorScale(const v1: TYPE_VECTOR3; const Scalar: TYPE_SCALAR): TYPE_VECTOR3;
begin
 result[0] := v1[0] * Scalar;
 result[1] := v1[1] * Scalar;
 result[2] := v1[2] * Scalar;
end;

function VectorScale(const v1: TYPE_VECTOR4; const Scalar: TYPE_SCALAR): TYPE_VECTOR4;
begin
 result[0] := v1[0] * Scalar;
 result[1] := v1[1] * Scalar;
 result[2] := v1[2] * Scalar;
 result[3] := v1[3] * Scalar;
end;

procedure VectorScaleTo1st(var v1: TYPE_VECTOR2; const Scalar: TYPE_SCALAR);
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
end;

procedure VectorScaleTo1st(var v1: TYPE_VECTOR3; const Scalar: TYPE_SCALAR);
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
 v1[2] *= Scalar;
end;

procedure VectorScaleTo1st(var v1: TYPE_VECTOR4; const Scalar: TYPE_SCALAR);
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
 v1[2] *= Scalar;
 v1[3] *= Scalar;
end;

function VectorNegate(const v: TYPE_VECTOR2): TYPE_VECTOR2;
begin
 result[0] := -v[0];
 result[1] := -v[1];
end;

function VectorNegate(const v: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 result[0] := -v[0];
 result[1] := -v[1];
 result[2] := -v[2];
end;

procedure VectorNegateTo1st(var v: TYPE_VECTOR2);
begin
 v[0] := -v[0];
 v[1] := -v[1];
end;

procedure VectorNegateTo1st(var v: TYPE_VECTOR3);
begin
 v[0] := -v[0];
 v[1] := -v[1];
 v[2] := -v[2];
end;

procedure VectorNegateTo1st(var v: TYPE_VECTOR4);
begin
 v[0] := -v[0];
 v[1] := -v[1];
 v[2] := -v[2];
 v[3] := -v[3];
end;

function VectorProduct(const v1, v2: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 result[0] := v1[1]*v2[2] - v1[2]*v2[1];
 result[1] := v1[2]*v2[0] - v1[0]*v2[2];
 result[2] := v1[0]*v2[1] - v1[1]*v2[0];
end;

function VectorDotProduct(const v1, v2: TYPE_VECTOR3): TYPE_SCALAR;
begin
 result := v1[0]*v2[0]+ v1[1]*v2[1]+ v1[2]*v2[2];
end;

function VectorDotProduct(const v1, v2: TYPE_VECTOR4): TYPE_SCALAR;
begin
 result := v1[0]*v2[0]+ v1[1]*v2[1]+ v1[2]*v2[2]+ v1[3]*v2[3];
end;

function VectorDotProduct(const v1: TYPE_VECTOR3; const v2: TYPE_VECTOR4): TYPE_SCALAR;
begin
 result := v1[0]*v2[0]+ v1[1]*v2[1]+ v1[2]*v2[2]+       v2[3];
end;

function VectorMultiplyComponents(const v1, v2: TYPE_VECTOR3): TYPE_VECTOR3;
begin
 result[0] := v1[0] * v2[0];
 result[1] := v1[1] * v2[1];
 result[2] := v1[2] * v2[2];
end;

procedure VectorMultiplyComponentsTo1st(var v1: TYPE_VECTOR3; const v2: TYPE_VECTOR3);
begin
 v1[0] *= v2[0];
 v1[1] *= v2[1];
 v1[2] *= v2[2];
end;
