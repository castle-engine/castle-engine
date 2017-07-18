{ Implement vector operators.
  Copied and adjusted from FPC's rtl/inc/mvecimp.inc }

{*****************************************************************************
                            Vector to vector operations
*****************************************************************************}

operator + (const x,y:objectname) : objectname;

{Adds the elements of both vectors together.}

begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
{$if vecsize>=3}
    result[2]:=x[2]+y[2];
{$endif}
{$if vecsize>=4}
    result[3]:=x[3]+y[3];
{$endif}
end;

operator - (const x:objectname) : objectname;

{Negates the elements of a vector.}

begin
    result[0]:=-x[0];
    result[1]:=-x[1];
{$if vecsize>=3}
    result[2]:=-x[2];
{$endif}
{$if vecsize>=4}
    result[3]:=-x[3];
{$endif}
end;

operator - (const x,y:objectname) : objectname;

{Subtracts the elements of both vectors together.}

begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
{$if vecsize>=3}
    result[2]:=x[2]-y[2];
{$endif}
{$if vecsize>=4}
    result[3]:=x[3]-y[3];
{$endif}
end;

operator * (const x,y:objectname) : objectname;

{Multiplies the elements of two vectors.}

begin
    result[0]:=x[0]*y[0];
    result[1]:=x[1]*y[1];
{$if vecsize>=3}
    result[2]:=x[2]*y[2];
{$endif}
{$if vecsize>=4}
    result[3]:=x[3]*y[3];
{$endif}
end;

{$ifndef CASTLEVECTOR_OPERATOR_ON_INTEGERS}
operator / (const x,y:objectname) : objectname;

{Divides the elements of two vectors.

 In most cases, you will want to avoid this and multiply by the inverse.
 In case you need to preserve accuracy, dividing might be better though.}

begin
    result[0]:=x[0]/y[0];
    result[1]:=x[1]/y[1];
{$if vecsize>=3}
    result[2]:=x[2]/y[2];
{$endif}
{$if vecsize>=4}
    result[3]:=x[3]/y[3];
{$endif}
end;
{$endif CASTLEVECTOR_OPERATOR_ON_INTEGERS}

operator ** (const x,y:objectname) : datatype;

{Calculates the inproduct of two vectors.}

begin
  result:=x[0]*y[0]
         +x[1]*y[1]
         {$if vecsize>=3}+x[2]*y[2]{$endif}
         {$if vecsize>=4}+x[3]*y[3]{$endif};
end;

{$if vecsize=3}
operator >< (const x,y:objectname) : objectname;

{Calculates the exproduct of two vectors. The exproduct exists only for
 3-dimensional vectors}

begin
  result[0]:=x[1]*y[2]-x[2]*y[1];
  result[1]:=x[2]*y[0]-x[0]*y[2];
  result[2]:=x[0]*y[1]-x[1]*y[0];
end;
{$endif}

{*****************************************************************************
                            Vector/scalar operations
*****************************************************************************}

operator * (const x:objectname;y:datatype) : objectname;

{Multiplies all vector elements by a scalar.}

begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
{$if vecsize>=3}
    result[2]:=x[2]*y;
{$endif}
{$if vecsize>=4}
    result[3]:=x[3]*y;
{$endif}
end;

{$ifndef CASTLEVECTOR_OPERATOR_ON_INTEGERS}
operator / (const x:objectname;y:datatype) : objectname;

{Divides all vector elements by a scalar.}

begin
    result[0]:=x[0]/y;
    result[1]:=x[1]/y;
{$if vecsize>=3}
    result[2]:=x[2]/y;
{$endif}
{$if vecsize>=4}
    result[3]:=x[3]/y;
{$endif}
end;
{$endif CASTLEVECTOR_OPERATOR_ON_INTEGERS}
