{
  Copyright 2003,2004 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ don't use VectorMathInlines now - due to some bugs in fpc 1.0.10
  (not tested under 1.1 now so I hadn't even tried to separate these
  bugs and submit them) }
{ $define USE_VECTOR_MATH_INLINES}
{$ifdef USE_VECTOR_MATH_INLINES}

{ --------------------------------------------------------------------------------
  zmien nazwy funkcji ktore tu sa na nazwy z "_Inlined". W ten sposob
  zarowno w deklaracjach funkcji w tym pliku sa de facto uzywane nazwy "_Inlined"
  i w kodzie ktory bedzie napisany po wlaczeniu tego pliku tez wszystkie
  uzycia tych funkcji beda widziane przez kompilator jako nazwy z "_Inlined".
  W ten sposob kompilator nie bedzie chcial uzywac wersji tych funkcji
  z VectorMath (co mialoby miejsce bez tych deklaracji, bo tu i w VectorMath
  wiele rzeczy jest overloaded i kompilator chcialby robic cross-unit
  overloading) }

{$define VectorSubtract:=VectorSubtract_Inlined}
{$define VectorSubtractTo1st:=VectorSubtractTo1st_Inlined}
{$define VectorAdd:=VectorAdd_Inlined}
{$define VectorAddTo1st:=VectorAddTo1st_Inlined}
{$define VectorScale:=VectorScale_Inlined}
{$define VectorScaleTo1st:=VectorScaleTo1st_Inlined}
{$define VectorNegate:=VectorNegate_Inlined}
{$define VectorNegateTo1st:=VectorNegateTo1st_Inlined}
{$define VectorProduct:=VectorProduct_Inlined}
{$define VectorDotProduct:=VectorDotProduct_Inlined}
{$define VectorMultEachPos:=VectorMultEachPos_Inlined}
{$define VectorMultEachPosTo1st:=VectorMultEachPosTo1st_Inlined}

{ --------------------------------------------------------------------------------
  zasadniczy kod w makrze dual implementation }

{$define VECTOR_MATH_INLINES_DUAL_IMPLEMENTATION:=
function VectorSubtract(const v1,v2:TYPE_VECTOR2):TYPE_VECTOR2; inline; overload;
begin
 result[0]:=v1[0] - v2[0];
 result[1]:=v1[1] - v2[1];
end;

function VectorSubtract(const v1,v2:TYPE_VECTOR3):TYPE_VECTOR3; inline; overload;
begin
 result[0]:=v1[0] - v2[0];
 result[1]:=v1[1] - v2[1];
 result[2]:=v1[2] - v2[2];
end;

procedure VectorSubtractTo1st(var v1:TYPE_VECTOR2; const v2:TYPE_VECTOR2); inline; overload;
begin
 v1[0] -= v2[0];
 v1[1] -= v2[1];
end;

procedure VectorSubtractTo1st(var v1:TYPE_VECTOR3; const v2:TYPE_VECTOR3); inline; overload;
begin
 v1[0] -= v2[0];
 v1[1] -= v2[1];
 v1[2] -= v2[2];
end;

function VectorAdd(const v1,v2:TYPE_VECTOR2):TYPE_VECTOR2; inline; overload;
begin
 result[0]:=v1[0] + v2[0];
 result[1]:=v1[1] + v2[1];
end;

function VectorAdd(const v1,v2:TYPE_VECTOR3):TYPE_VECTOR3; inline; overload;
begin
 result[0]:=v1[0] + v2[0];
 result[1]:=v1[1] + v2[1];
 result[2]:=v1[2] + v2[2];
end;

procedure VectorAddTo1st(var v1:TYPE_VECTOR2; const v2:TYPE_VECTOR2); inline; overload;
begin
 v1[0] += v2[0];
 v1[1] += v2[1];
end;

procedure VectorAddTo1st(var v1:TYPE_VECTOR3; const v2:TYPE_VECTOR3); inline; overload;
begin
 v1[0] += v2[0];
 v1[1] += v2[1];
 v1[2] += v2[2];
end;

function VectorScale(const v1:TYPE_VECTOR2; Scalar:TYPE_SCALAR):TYPE_VECTOR2; inline; overload;
begin
 result[0]:=v1[0] * Scalar;
 result[1]:=v1[1] * Scalar;
end;

function VectorScale(const v1:TYPE_VECTOR3; Scalar:TYPE_SCALAR):TYPE_VECTOR3; inline; overload;
begin
 result[0]:=v1[0] * Scalar;
 result[1]:=v1[1] * Scalar;
 result[2]:=v1[2] * Scalar;
end;

procedure VectorScaleTo1st(var v1:TYPE_VECTOR2; const Scalar:TYPE_SCALAR); inline; overload;
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
end;

procedure VectorScaleTo1st(var v1:TYPE_VECTOR3; const Scalar:TYPE_SCALAR); inline; overload;
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
 v1[2] *= Scalar;
end;

procedure VectorScaleTo1st(var v1: TYPE_VECTOR4; const Scalar: TYPE_SCALAR); inline; overload;
begin
 v1[0] *= Scalar;
 v1[1] *= Scalar;
 v1[2] *= Scalar;
 v1[3] *= Scalar;
end;

function VectorNegate(const v:TYPE_VECTOR2):TYPE_VECTOR2; inline; overload;
begin
 result[0]:=-v[0];
 result[1]:=-v[1];
end;

function VectorNegate(const v:TYPE_VECTOR3):TYPE_VECTOR3; inline; overload;
begin
 result[0]:=-v[0];
 result[1]:=-v[1];
 result[2]:=-v[2];
end;

procedure VectorNegateTo1st(var v:TYPE_VECTOR2); inline; overload;
begin
 v[0]:=-v[0];
 v[1]:=-v[1];
end;

procedure VectorNegateTo1st(var v:TYPE_VECTOR3); inline; overload;
begin
 v[0]:=-v[0];
 v[1]:=-v[1];
 v[2]:=-v[2];
end;

function VectorProduct(const v1,v2:TYPE_VECTOR3):TYPE_VECTOR3; inline; overload;
begin
 result[0]:=v1[1]*v2[2] - v1[2]*v2[1];
 result[1]:=v1[2]*v2[0] - v1[0]*v2[2];
 result[2]:=v1[0]*v2[1] - v1[1]*v2[0];
end;

function VectorDotProduct(const v1,v2:TYPE_VECTOR3):TYPE_SCALAR; inline; overload;
begin
 result:=v1[0]*v2[0]+ v1[1]*v2[1]+ v1[2]*v2[2];
end;

function VectorMultEachPos(const v1,v2:TYPE_VECTOR3):TYPE_VECTOR3; inline; overload;
begin
 result[0]:=v1[0] * v2[0];
 result[1]:=v1[1] * v2[1];
 result[2]:=v1[2] * v2[2];
end;

procedure VectorMultEachPosTo1st(var v1:TYPE_VECTOR3; const v2:TYPE_VECTOR3); inline; overload;
begin
 v1[0] *= v2[0];
 v1[1] *= v2[1];
 v1[2] *= v2[2];
end;
}

{ --------------------------------------------------------------------------------
  uzyj makra dual implementation }

{$define TYPE_SCALAR:=Single}
{$define TYPE_VECTOR2:=TVector2Single}
{$define TYPE_VECTOR3:=TVector3Single}
{$define TYPE_VECTOR4:=TVector4Single}
{$define TYPE_TRIANGLE2:=TTriangle2Single}
{$define TYPE_TRIANGLE3:=TTriangle3Single}
{$define TYPE_MATRIX4:=TMatrix4Single}
{$define SCALAR_EQUALITY_EPSILON:=SingleEqualityEpsilon}
{$define ONE_VECTOR:=OneVectorSingle}
{$define IDENTITY_MATRIX:=IdentityMatrix4Single}
VECTOR_MATH_INLINES_DUAL_IMPLEMENTATION

{$define TYPE_SCALAR:=Double}
{$define TYPE_VECTOR2:=TVector2Double}
{$define TYPE_VECTOR3:=TVector3Double}
{$define TYPE_VECTOR4:=TVector4Double}
{$define TYPE_TRIANGLE2:=TTriangle2Double}
{$define TYPE_TRIANGLE3:=TTriangle3Double}
{$define TYPE_MATRIX4:=TMatrix4Double}
{$define SCALAR_EQUALITY_EPSILON:=DoubleEqualityEpsilon}
{$define ONE_VECTOR:=OneVectorDouble}
{$define IDENTITY_MATRIX:=IdentityMatrix4Double}
VECTOR_MATH_INLINES_DUAL_IMPLEMENTATION

{$undef VECTOR_MATH_INLINES_DUAL_IMPLEMENTATION}

{ eof ------------------------------------------------------------ }

{$endif}