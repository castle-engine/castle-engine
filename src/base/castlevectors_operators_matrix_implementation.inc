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

(* TODO: Fix, FPC matrix was ordered by rows, we should make transpose inside.

Doing transpose here would be a waste of time...
  function MatrixTranspose(const M: TMatrix4Single): TMatrix4Single;
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


operator * (const m1,m2:objectname) : objectname;

{Multiply two matrixes.}

var r:array[0..matsize-1] of datatype;
    i:byte;

begin
    for i:=0 to matsize-1 do
        begin
            r:=m1[i];
            result[i,0]:=r[0]*m2[0,0]
                             +r[1]*m2[1,0]
             {$if matsize>=3}+r[2]*m2[2,0]{$endif}
             {$if matsize>=4}+r[3]*m2[3,0]{$endif};
            result[i,1]:=r[0]*m2[0,1]
                             +r[1]*m2[1,1]
             {$if matsize>=3}+r[2]*m2[2,1]{$endif}
             {$if matsize>=4}+r[3]*m2[3,1]{$endif};
        {$if matsize>=3}
            result[i,2]:=r[0]*m2[0,2]
                             +r[1]*m2[1,2]
                             +r[2]*m2[2,2]
             {$if matsize>=4}+r[3]*m2[3,2]{$endif};
        {$endif}
        {$if matsize>=4}
            result[i,3]:=r[0]*m2[0,3]
                             +r[1]*m2[1,3]
                             +r[2]*m2[2,3]
                             +r[3]*m2[3,3];
        {$endif}
        end;
end;
*)

{*****************************************************************************
                           Vector/matrix operations
*****************************************************************************}

(* TODO: Fix, FPC matrix was ordered by rows, we should make transpose inside.

operator * (const m:objectname;const v:vectorcompanion) : vectorcompanion;

{Multiplies a matrix with a vector.}

begin
    result[0]:=m[0,0]*v[0]
                   +m[0,1]*v[1]
   {$if matsize>=3}+m[0,2]*v[2]{$endif}
   {$if matsize>=4}+m[0,3]*v[3]{$endif};
    result[1]:=m[1,0]*v[0]
                   +m[1,1]*v[1]
   {$if matsize>=3}+m[1,2]*v[2]{$endif}
   {$if matsize>=4}+m[1,3]*v[3]{$endif};
{$if matsize>=3}
    result[2]:=m[2,0]*v[0]
                   +m[2,1]*v[1]
                   +m[2,2]*v[2]
   {$if matsize>=4}+m[2,3]*v[3]{$endif};
{$endif}
{$if matsize>=4}
    result[3]:=m[3,0]*v[0]
                   +m[3,1]*v[1]
                   +m[3,2]*v[2]
                   +m[3,3]*v[3];
{$endif}
end;

*)

{$if matsize>=4}
{ For now, we just define 4x4 multiplications. Copied from MatrixMult. }
operator * (const m1,m2:objectname) : objectname;
begin
  result[0, 0] := m1[0, 0] * m2[0, 0] + m1[1, 0] * m2[0, 1] + m1[2, 0] * m2[0, 2] + m1[3, 0] * m2[0, 3];
  result[1, 0] := m1[0, 0] * m2[1, 0] + m1[1, 0] * m2[1, 1] + m1[2, 0] * m2[1, 2] + m1[3, 0] * m2[1, 3];
  result[2, 0] := m1[0, 0] * m2[2, 0] + m1[1, 0] * m2[2, 1] + m1[2, 0] * m2[2, 2] + m1[3, 0] * m2[2, 3];
  result[3, 0] := m1[0, 0] * m2[3, 0] + m1[1, 0] * m2[3, 1] + m1[2, 0] * m2[3, 2] + m1[3, 0] * m2[3, 3];
  result[0, 1] := m1[0, 1] * m2[0, 0] + m1[1, 1] * m2[0, 1] + m1[2, 1] * m2[0, 2] + m1[3, 1] * m2[0, 3];
  result[1, 1] := m1[0, 1] * m2[1, 0] + m1[1, 1] * m2[1, 1] + m1[2, 1] * m2[1, 2] + m1[3, 1] * m2[1, 3];
  result[2, 1] := m1[0, 1] * m2[2, 0] + m1[1, 1] * m2[2, 1] + m1[2, 1] * m2[2, 2] + m1[3, 1] * m2[2, 3];
  result[3, 1] := m1[0, 1] * m2[3, 0] + m1[1, 1] * m2[3, 1] + m1[2, 1] * m2[3, 2] + m1[3, 1] * m2[3, 3];
  result[0, 2] := m1[0, 2] * m2[0, 0] + m1[1, 2] * m2[0, 1] + m1[2, 2] * m2[0, 2] + m1[3, 2] * m2[0, 3];
  result[1, 2] := m1[0, 2] * m2[1, 0] + m1[1, 2] * m2[1, 1] + m1[2, 2] * m2[1, 2] + m1[3, 2] * m2[1, 3];
  result[2, 2] := m1[0, 2] * m2[2, 0] + m1[1, 2] * m2[2, 1] + m1[2, 2] * m2[2, 2] + m1[3, 2] * m2[2, 3];
  result[3, 2] := m1[0, 2] * m2[3, 0] + m1[1, 2] * m2[3, 1] + m1[2, 2] * m2[3, 2] + m1[3, 2] * m2[3, 3];
  result[0, 3] := m1[0, 3] * m2[0, 0] + m1[1, 3] * m2[0, 1] + m1[2, 3] * m2[0, 2] + m1[3, 3] * m2[0, 3];
  result[1, 3] := m1[0, 3] * m2[1, 0] + m1[1, 3] * m2[1, 1] + m1[2, 3] * m2[1, 2] + m1[3, 3] * m2[1, 3];
  result[2, 3] := m1[0, 3] * m2[2, 0] + m1[1, 3] * m2[2, 1] + m1[2, 3] * m2[2, 2] + m1[3, 3] * m2[2, 3];
  result[3, 3] := m1[0, 3] * m2[3, 0] + m1[1, 3] * m2[3, 1] + m1[2, 3] * m2[3, 2] + m1[3, 3] * m2[3, 3];
end;
{$endif}

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

