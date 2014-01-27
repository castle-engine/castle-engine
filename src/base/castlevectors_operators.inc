{ Overload operators for our vector/matrix types.
  This can only be used in FPC with ObjFpc mode, Delphi doesn't allow this.

  Most of the code copied and adjusted from FPC's Matrix unit (rtl/inc/matrix.pp),
  that defines the same operators for object types (like Tvector2_single),
  but we want this for our types TVector2Single (equivalent to
  FPC's Matrix Tvector2_single_data).
  We also overload sensible operators for integer-vectors.

  FreePascal's Matrix unit copyright:
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Daniel Mantione

  FPC's RTL license is exactly the same as our engine's license
  (LGPL >= 2 with static linking exception), no problems here.
  See the file COPYING.txt for license description.
}

{$ifdef read_interface}

{Vector to vector operations.}
operator + (const x,y:Tvector2_single_data) : Tvector2_single_data;
operator + (const x,y:Tvector2_double_data) : Tvector2_double_data;
operator + (const x,y:Tvector2_extended_data) : Tvector2_extended_data;
operator + (const x,y:Tvector2Integer) : Tvector2Integer;
operator + (const x,y:Tvector3_single_data) : Tvector3_single_data;
operator + (const x,y:Tvector3_double_data) : Tvector3_double_data;
operator + (const x,y:Tvector3_extended_data) : Tvector3_extended_data;
operator + (const x,y:Tvector3Integer) : Tvector3Integer;
operator + (const x,y:Tvector4_single_data) : Tvector4_single_data;
operator + (const x,y:Tvector4_double_data) : Tvector4_double_data;
operator + (const x,y:Tvector4_extended_data) : Tvector4_extended_data;
operator + (const x,y:Tvector4Integer) : Tvector4Integer;

operator - (const x,y:Tvector2_single_data) : Tvector2_single_data;
operator - (const x,y:Tvector2_double_data) : Tvector2_double_data;
operator - (const x,y:Tvector2_extended_data) : Tvector2_extended_data;
operator - (const x,y:Tvector2Integer) : Tvector2Integer;
operator - (const x,y:Tvector3_single_data) : Tvector3_single_data;
operator - (const x,y:Tvector3_double_data) : Tvector3_double_data;
operator - (const x,y:Tvector3_extended_data) : Tvector3_extended_data;
operator - (const x,y:Tvector3Integer) : Tvector3Integer;
operator - (const x,y:Tvector4_single_data) : Tvector4_single_data;
operator - (const x,y:Tvector4_double_data) : Tvector4_double_data;
operator - (const x,y:Tvector4_extended_data) : Tvector4_extended_data;
operator - (const x,y:Tvector4Integer) : Tvector4Integer;

operator - (const x:Tvector2_single_data) : Tvector2_single_data;
operator - (const x:Tvector2_double_data) : Tvector2_double_data;
operator - (const x:Tvector2_extended_data) : Tvector2_extended_data;
operator - (const x:Tvector2Integer) : Tvector2Integer;
operator - (const x:Tvector3_single_data) : Tvector3_single_data;
operator - (const x:Tvector3_double_data) : Tvector3_double_data;
operator - (const x:Tvector3_extended_data) : Tvector3_extended_data;
operator - (const x:Tvector3Integer) : Tvector3Integer;
operator - (const x:Tvector4_single_data) : Tvector4_single_data;
operator - (const x:Tvector4_double_data) : Tvector4_double_data;
operator - (const x:Tvector4_extended_data) : Tvector4_extended_data;
operator - (const x:Tvector4Integer) : Tvector4Integer;

operator * (const x,y:Tvector2_single_data) : Tvector2_single_data;
operator * (const x,y:Tvector2_double_data) : Tvector2_double_data;
operator * (const x,y:Tvector2_extended_data) : Tvector2_extended_data;
operator * (const x,y:Tvector2Integer) : Tvector2Integer;
operator * (const x,y:Tvector3_single_data) : Tvector3_single_data;
operator * (const x,y:Tvector3_double_data) : Tvector3_double_data;
operator * (const x,y:Tvector3_extended_data) : Tvector3_extended_data;
operator * (const x,y:Tvector3Integer) : Tvector3Integer;
operator * (const x,y:Tvector4_single_data) : Tvector4_single_data;
operator * (const x,y:Tvector4_double_data) : Tvector4_double_data;
operator * (const x,y:Tvector4_extended_data) : Tvector4_extended_data;
operator * (const x,y:Tvector4Integer) : Tvector4Integer;

operator ** (const x,y:Tvector2_single_data) : single;
operator ** (const x,y:Tvector2_double_data) : double;
operator ** (const x,y:Tvector2_extended_data) : extended;
operator ** (const x,y:Tvector2Integer) : Integer;
operator ** (const x,y:Tvector3_single_data) : single;
operator ** (const x,y:Tvector3_double_data) : double;
operator ** (const x,y:Tvector3_extended_data) : extended;
operator ** (const x,y:Tvector3Integer) : Integer;
operator ** (const x,y:Tvector4_single_data) : single;
operator ** (const x,y:Tvector4_double_data) : double;
operator ** (const x,y:Tvector4_extended_data) : extended;
operator ** (const x,y:Tvector4Integer) : Integer;

operator >< (const x,y:Tvector3_single_data) : Tvector3_single_data;
operator >< (const x,y:Tvector3_double_data) : Tvector3_double_data;
operator >< (const x,y:Tvector3_extended_data) : Tvector3_extended_data;
operator >< (const x,y:Tvector3Integer) : Tvector3Integer;

{Vector/scalar operations.

  Note: we don't define vector+scalar, vector-scalar overloads
  (FPC's Matrix unit does define them) as
  - They make type-checking less
    strict and generally lead to trivial errors in my experience
    (it's easy to accidentaly write code like "vector1 + scalar"
    while you instead wanted to write "vector1 + scalar * vector2").
  - They are also not really useful (they add/subtract the same scalar
    from all components, we don't really see this as an often operation
    in engine code).
  So, they make more harm than gain.
}
{ }
operator * (const x:Tvector2_single_data;y:single) : Tvector2_single_data;
operator * (const x:Tvector2_double_data;y:double) : Tvector2_double_data;
operator * (const x:Tvector2_extended_data;y:extended) : Tvector2_extended_data;
operator * (const x:Tvector3_single_data;y:single) : Tvector3_single_data;
operator * (const x:Tvector3_double_data;y:double) : Tvector3_double_data;
operator * (const x:Tvector3_extended_data;y:extended) : Tvector3_extended_data;
operator * (const x:Tvector4_single_data;y:single) : Tvector4_single_data;
operator * (const x:Tvector4_double_data;y:double) : Tvector4_double_data;
operator * (const x:Tvector4_extended_data;y:extended) : Tvector4_extended_data;

operator / (const x:Tvector2_single_data;y:single) : Tvector2_single_data;
operator / (const x:Tvector2_double_data;y:double) : Tvector2_double_data;
operator / (const x:Tvector2_extended_data;y:extended) : Tvector2_extended_data;
operator / (const x:Tvector3_single_data;y:single) : Tvector3_single_data;
operator / (const x:Tvector3_double_data;y:double) : Tvector3_double_data;
operator / (const x:Tvector3_extended_data;y:extended) : Tvector3_extended_data;
operator / (const x:Tvector4_single_data;y:single) : Tvector4_single_data;
operator / (const x:Tvector4_double_data;y:double) : Tvector4_double_data;
operator / (const x:Tvector4_extended_data;y:extended) : Tvector4_extended_data;

operator + (const m1,m2:Tmatrix2_single_data) : Tmatrix2_single_data;
operator + (const m1,m2:Tmatrix2_double_data) : Tmatrix2_double_data;
operator + (const m1,m2:Tmatrix3_single_data) : Tmatrix3_single_data;
operator + (const m1,m2:Tmatrix3_double_data) : Tmatrix3_double_data;
operator + (const m1,m2:Tmatrix4_single_data) : Tmatrix4_single_data;
operator + (const m1,m2:Tmatrix4_double_data) : Tmatrix4_double_data;

operator - (const m1,m2:Tmatrix2_single_data) : Tmatrix2_single_data;
operator - (const m1,m2:Tmatrix2_double_data) : Tmatrix2_double_data;
operator - (const m1,m2:Tmatrix3_single_data) : Tmatrix3_single_data;
operator - (const m1,m2:Tmatrix3_double_data) : Tmatrix3_double_data;
operator - (const m1,m2:Tmatrix4_single_data) : Tmatrix4_single_data;
operator - (const m1,m2:Tmatrix4_double_data) : Tmatrix4_double_data;

operator - (const m1:Tmatrix2_single_data) : Tmatrix2_single_data;
operator - (const m1:Tmatrix2_double_data) : Tmatrix2_double_data;
operator - (const m1:Tmatrix3_single_data) : Tmatrix3_single_data;
operator - (const m1:Tmatrix3_double_data) : Tmatrix3_double_data;
operator - (const m1:Tmatrix4_single_data) : Tmatrix4_single_data;
operator - (const m1:Tmatrix4_double_data) : Tmatrix4_double_data;

operator * (const m1,m2:Tmatrix2_single_data) : Tmatrix2_single_data;
operator * (const m1,m2:Tmatrix2_double_data) : Tmatrix2_double_data;
operator * (const m1,m2:Tmatrix3_single_data) : Tmatrix3_single_data;
operator * (const m1,m2:Tmatrix3_double_data) : Tmatrix3_double_data;
operator * (const m1,m2:Tmatrix4_single_data) : Tmatrix4_single_data;
operator * (const m1,m2:Tmatrix4_double_data) : Tmatrix4_double_data;

{Matrix/vector operations}
operator * (const m:Tmatrix2_single_data;const v:Tvector2_single_data) : Tvector2_single_data;
operator * (const m:Tmatrix2_double_data;const v:Tvector2_double_data) : Tvector2_double_data;
operator * (const m:Tmatrix3_single_data;const v:Tvector3_single_data) : Tvector3_single_data;
operator * (const m:Tmatrix3_double_data;const v:Tvector3_double_data) : Tvector3_double_data;
operator * (const m:Tmatrix4_single_data;const v:Tvector4_single_data) : Tvector4_single_data;
operator * (const m:Tmatrix4_double_data;const v:Tvector4_double_data) : Tvector4_double_data;

{Matrix/scalar operations}
operator * (const m:Tmatrix2_single_data;const x:single) : Tmatrix2_single_data;
operator * (const m:Tmatrix2_double_data;const x:double) : Tmatrix2_double_data;
operator * (const m:Tmatrix3_single_data;const x:single) : Tmatrix3_single_data;
operator * (const m:Tmatrix3_double_data;const x:double) : Tmatrix3_double_data;
operator * (const m:Tmatrix4_single_data;const x:single) : Tmatrix4_single_data;
operator * (const m:Tmatrix4_double_data;const x:double) : Tmatrix4_double_data;

operator / (const m:Tmatrix2_single_data;const x:single) : Tmatrix2_single_data;
operator / (const m:Tmatrix2_double_data;const x:double) : Tmatrix2_double_data;
operator / (const m:Tmatrix3_single_data;const x:single) : Tmatrix3_single_data;
operator / (const m:Tmatrix3_double_data;const x:double) : Tmatrix3_double_data;
operator / (const m:Tmatrix4_single_data;const x:single) : Tmatrix4_single_data;
operator / (const m:Tmatrix4_double_data;const x:double) : Tmatrix4_double_data;

{$endif read_interface}

{$ifdef read_implementation}

{ To implement, use macros and
  castlevectors_operators_vector_implementation.inc,
  castlevectors_operators_matrix_implementation.inc
  includes. Copied and adjusted from FPC's Matrix implementation.
  I added CASTLEVECTOR_OPERATOR_ON_INTEGERS option, to overload also
  for integer vectors. }

{******************************************************************************
                                Tvector2_single_data
******************************************************************************}

{Need to use capitals due to bug in FPC. Bug was fixed in FPC 1.9.3 on
 10 Feb. 2004}
{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=Tvector2_single_data}
{$DEFINE vecsize:=2}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector2_single_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                Tvector2_double_data
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=Tvector2_double_data}
{$DEFINE vecsize:=2}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector2_double_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                               Tvector2_extended_data
******************************************************************************}

{$DEFINE datatype:=EXTENDED}
{$DEFINE objectname:=Tvector2_extended_data}
{$DEFINE vecsize:=2}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector2_extended_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                Tvector2Integer
******************************************************************************}

{$DEFINE datatype:=Integer}
{$DEFINE objectname:=Tvector2Integer}
{$DEFINE vecsize:=2}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector2Integer}
{$define CASTLEVECTOR_OPERATOR_ON_INTEGERS}
{$i castlevectors_operators_vector_implementation.inc}
{$undef CASTLEVECTOR_OPERATOR_ON_INTEGERS}


{******************************************************************************
                                Tvector3_single_data
******************************************************************************}

{Need to use capitals due to bug in FPC. Bug was fixed in FPC 1.9.3 on
 10 Feb. 2004}
{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=Tvector3_single_data}
{$DEFINE vecsize:=3}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector3_single_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                Tvector3_double_data
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=Tvector3_double_data}
{$DEFINE vecsize:=3}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector3_double_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                               Tvector3_extended_data
******************************************************************************}

{$DEFINE datatype:=EXTENDED}
{$DEFINE objectname:=Tvector3_extended_data}
{$DEFINE vecsize:=3}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector3_extended_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                Tvector3Integer
******************************************************************************}

{$DEFINE datatype:=Integer}
{$DEFINE objectname:=Tvector3Integer}
{$DEFINE vecsize:=3}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector3Integer}
{$define CASTLEVECTOR_OPERATOR_ON_INTEGERS}
{$i castlevectors_operators_vector_implementation.inc}
{$undef CASTLEVECTOR_OPERATOR_ON_INTEGERS}


{******************************************************************************
                                Tvector4_single_data
******************************************************************************}

{Need to use capitals due to bug in FPC. Bug was fixed in FPC 1.9.3 on
 10 Feb. 2004}
{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=Tvector4_single_data}
{$DEFINE vecsize:=4}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector4_single_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                Tvector4_double_data
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=Tvector4_double_data}
{$DEFINE vecsize:=4}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector4_double_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                               Tvector4_extended_data
******************************************************************************}

{$DEFINE datatype:=EXTENDED}
{$DEFINE objectname:=Tvector4_extended_data}
{$DEFINE vecsize:=4}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector4_extended_data}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                Tvector4Integer
******************************************************************************}

{$DEFINE datatype:=Integer}
{$DEFINE objectname:=Tvector4Integer}
{$DEFINE vecsize:=4}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for Tvector4Integer}
{$define CASTLEVECTOR_OPERATOR_ON_INTEGERS}
{$i castlevectors_operators_vector_implementation.inc}
{$undef CASTLEVECTOR_OPERATOR_ON_INTEGERS}


{******************************************************************************
                                Tmatrix2_single_data
******************************************************************************}

{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=Tmatrix2_single_data}
{$DEFINE vectorcompanion:=Tvector2_single_data}
{$DEFINE matsize:=2}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for Tmatrix2_single_data}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                Tmatrix2_double_data
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=Tmatrix2_double_data}
{$DEFINE vectorcompanion:=Tvector2_double_data}
{$DEFINE matsize:=2}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for Tmatrix2_double_data}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                Tmatrix3_single_data
******************************************************************************}

{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=Tmatrix3_single_data}
{$DEFINE vectorcompanion:=Tvector3_single_data}
{$DEFINE matsize:=3}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for Tmatrix3_single_data}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                Tmatrix3_double_data
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=Tmatrix3_double_data}
{$DEFINE vectorcompanion:=Tvector3_double_data}
{$DEFINE matsize:=3}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for Tmatrix3_double_data}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                Tmatrix4_single_data
******************************************************************************}

{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=Tmatrix4_single_data}
{$DEFINE vectorcompanion:=Tvector4_single_data}
{$DEFINE matsize:=4}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for Tmatrix4_single_data}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                Tmatrix4_double_data
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=Tmatrix4_double_data}
{$DEFINE vectorcompanion:=Tvector4_double_data}
{$DEFINE matsize:=4}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for Tmatrix4_double_data}
{$i castlevectors_operators_matrix_implementation.inc}

{$endif read_implementation}
