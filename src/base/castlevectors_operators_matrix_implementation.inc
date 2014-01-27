{ Implement matrix operators.
  Copied and adjusted from FPC's rtl/inc/mmatimp.inc }

{*****************************************************************************
                           Matrix to matrix operations
*****************************************************************************}

operator + (const m1,m2:objectname) : objectname;

{Add the elements of a matrix to each other.}

begin
    result[0,0]:=m1[0,0]+m2[0,0];
    result[0,1]:=m1[0,1]+m2[0,1];
{$if matsize>=3}
    result[0,2]:=m1[0,2]+m2[0,2];
{$endif}
{$if matsize>=4}
    result[0,3]:=m1[0,3]+m2[0,3];
{$endif}
    result[1,0]:=m1[1,0]+m2[1,0];
    result[1,1]:=m1[1,1]+m2[1,1];
{$if matsize>=3}
    result[1,2]:=m1[1,2]+m2[1,2];
{$endif}
{$if matsize>=4}
    result[1,3]:=m1[1,3]+m2[1,3];
{$endif}
{$if matsize>=3}
    result[2,0]:=m1[2,0]+m2[2,0];
    result[2,1]:=m1[2,1]+m2[2,1];
    result[2,2]:=m1[2,2]+m2[2,2];
{$endif}
{$if matsize>=4}
    result[2,3]:=m1[2,3]+m2[2,3];
    result[3,0]:=m1[3,0]+m2[3,0];
    result[3,1]:=m1[3,1]+m2[3,1];
    result[3,2]:=m1[3,2]+m2[3,2];
    result[3,3]:=m1[3,3]+m2[3,3];
{$endif}
end;

operator - (const m1,m2:objectname) : objectname;

{Subtract the elements of two matrixes from each other.}

begin
    result[0,0]:=m1[0,0]-m2[0,0];
    result[0,1]:=m1[0,1]-m2[0,1];
{$if matsize>=3}
    result[0,2]:=m1[0,2]-m2[0,2];
{$endif}
{$if matsize>=4}
    result[0,3]:=m1[0,3]-m2[0,3];
{$endif}
    result[1,0]:=m1[1,0]-m2[1,0];
    result[1,1]:=m1[1,1]-m2[1,1];
{$if matsize>=3}
    result[1,2]:=m1[1,2]-m2[1,2];
{$endif}
{$if matsize>=4}
    result[1,3]:=m1[1,3]-m2[1,3];
{$endif}
{$if matsize>=3}
    result[2,0]:=m1[2,0]-m2[2,0];
    result[2,1]:=m1[2,1]-m2[2,1];
    result[2,2]:=m1[2,2]-m2[2,2];
{$endif}
{$if matsize>=4}
    result[2,3]:=m1[2,3]-m2[2,3];
    result[3,0]:=m1[3,0]-m2[3,0];
    result[3,1]:=m1[3,1]-m2[3,1];
    result[3,2]:=m1[3,2]-m2[3,2];
    result[3,3]:=m1[3,3]-m2[3,3];
{$endif}
end;

operator - (const m1:objectname) : objectname;

{Negate the elements of a matrix.}

begin
    result[0,0]:=-m1[0,0];
    result[0,1]:=-m1[0,1];
{$if matsize>=3}
    result[0,2]:=-m1[0,2];
{$endif}
{$if matsize>=4}
    result[0,3]:=-m1[0,3];
{$endif}
    result[1,0]:=-m1[1,0];
    result[1,1]:=-m1[1,1];
{$if matsize>=3}
    result[1,2]:=-m1[1,2];
{$endif}
{$if matsize>=4}
    result[1,3]:=-m1[1,3];
{$endif}
{$if matsize>=3}
    result[2,0]:=-m1[2,0];
    result[2,1]:=-m1[2,1];
    result[2,2]:=-m1[2,2];
{$endif}
{$if matsize>=4}
    result[2,3]:=-m1[2,3];
    result[3,0]:=-m1[3,0];
    result[3,1]:=-m1[3,1];
    result[3,2]:=-m1[3,2];
    result[3,3]:=-m1[3,3];
{$endif}
end;

{ We do not use matrix multiplication code from FPC Matrix unit here,
  as their matrices are transposed compared to ours.
  Instead we use our own code, adjusted from MatrixMult implementation. }
operator * (const m1,m2:objectname) : objectname;
begin
  { Note that we don't use the implementation of this from FPC Matrix unit,
    as our matrices are transposed. Also, our MatrixMultVector is already
    implemented. }
  Result := MatrixMult(M1, M2);
end;

{*****************************************************************************
                           Vector/matrix operations
*****************************************************************************}

operator * (const m:objectname;const v:vectorcompanion) : vectorcompanion;
begin
  { Note that we don't use the implementation of this from FPC Matrix unit,
    as our matrices are transposed. Also, our MatrixMultVector is already
    implemented. }
  Result := MatrixMultVector(M, V);
end;

{*****************************************************************************
                           Matrix/scalar operations
*****************************************************************************}

operator * (const m:objectname;const x:datatype) : objectname;

{Multiplies the elements of a matrix.}

begin
  result[0,0]:=m[0,0]*x;
  result[0,1]:=m[0,1]*x;
{$if matsize>=3}
  result[0,2]:=m[0,2]*x;
{$endif}
{$if matsize>=4}
  result[0,3]:=m[0,3]*x;
{$endif}
  result[1,0]:=m[1,0]*x;
  result[1,1]:=m[1,1]*x;
{$if matsize>=3}
  result[1,2]:=m[1,2]*x;
{$endif}
{$if matsize>=4}
  result[1,3]:=m[1,3]*x;
{$endif}
{$if matsize>=3}
  result[2,0]:=m[2,0]*x;
  result[2,1]:=m[2,1]*x;
  result[2,2]:=m[2,2]*x;
{$endif}
{$if matsize>=4}
  result[2,3]:=m[2,3]*x;
  result[3,0]:=m[3,0]*x;
  result[3,1]:=m[3,1]*x;
  result[3,2]:=m[3,2]*x;
  result[3,3]:=m[3,3]*x;
{$endif}
end;

operator / (const m:objectname;const x:datatype) : objectname;

{Divides the elements of a matrix.

 In most cases, you will want to avoid this and multiply by the inverse.
 In case you need to preserve accuracy, dividing might be better though.}

begin
  result[0,0]:=m[0,0]/x;
  result[0,1]:=m[0,1]/x;
{$if matsize>=3}
  result[0,2]:=m[0,2]/x;
{$endif}
{$if matsize>=4}
  result[0,3]:=m[0,3]/x;
{$endif}
  result[1,0]:=m[1,0]/x;
  result[1,1]:=m[1,1]/x;
{$if matsize>=3}
  result[1,2]:=m[1,2]/x;
{$endif}
{$if matsize>=4}
  result[1,3]:=m[1,3]/x;
{$endif}
{$if matsize>=3}
  result[2,0]:=m[2,0]/x;
  result[2,1]:=m[2,1]/x;
  result[2,2]:=m[2,2]/x;
{$endif}
{$if matsize>=4}
  result[2,3]:=m[2,3]/x;
  result[3,0]:=m[3,0]/x;
  result[3,1]:=m[3,1]/x;
  result[3,2]:=m[3,2]/x;
  result[3,3]:=m[3,3]/x;
{$endif}
end;

