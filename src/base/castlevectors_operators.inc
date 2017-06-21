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
operator + (const x,y:TVector2Single) : TVector2Single;
operator + (const x,y:TVector2Double) : TVector2Double;
operator + (const x,y:TVector2Extended) : TVector2Extended;
operator + (const x,y:Tvector2Integer) : Tvector2Integer;
operator + (const x,y:TVector3Single) : TVector3Single;
operator + (const x,y:TVector3Double) : TVector3Double;
operator + (const x,y:TVector3Extended) : TVector3Extended;
operator + (const x,y:Tvector3Integer) : Tvector3Integer;
operator + (const x,y:TVector4Single) : TVector4Single;
operator + (const x,y:TVector4Double) : TVector4Double;
operator + (const x,y:TVector4Extended) : TVector4Extended;
operator + (const x,y:Tvector4Integer) : Tvector4Integer;

operator - (const x,y:TVector2Single) : TVector2Single;
operator - (const x,y:TVector2Double) : TVector2Double;
operator - (const x,y:TVector2Extended) : TVector2Extended;
operator - (const x,y:Tvector2Integer) : Tvector2Integer;
operator - (const x,y:TVector3Single) : TVector3Single;
operator - (const x,y:TVector3Double) : TVector3Double;
operator - (const x,y:TVector3Extended) : TVector3Extended;
operator - (const x,y:Tvector3Integer) : Tvector3Integer;
operator - (const x,y:TVector4Single) : TVector4Single;
operator - (const x,y:TVector4Double) : TVector4Double;
operator - (const x,y:TVector4Extended) : TVector4Extended;
operator - (const x,y:Tvector4Integer) : Tvector4Integer;

operator - (const x:TVector2Single) : TVector2Single;
operator - (const x:TVector2Double) : TVector2Double;
operator - (const x:TVector2Extended) : TVector2Extended;
operator - (const x:Tvector2Integer) : Tvector2Integer;
operator - (const x:TVector3Single) : TVector3Single;
operator - (const x:TVector3Double) : TVector3Double;
operator - (const x:TVector3Extended) : TVector3Extended;
operator - (const x:Tvector3Integer) : Tvector3Integer;
operator - (const x:TVector4Single) : TVector4Single;
operator - (const x:TVector4Double) : TVector4Double;
operator - (const x:TVector4Extended) : TVector4Extended;
operator - (const x:Tvector4Integer) : Tvector4Integer;

operator * (const x,y:TVector2Single) : TVector2Single;
operator * (const x,y:TVector2Double) : TVector2Double;
operator * (const x,y:TVector2Extended) : TVector2Extended;
operator * (const x,y:Tvector2Integer) : Tvector2Integer;
operator * (const x,y:TVector3Single) : TVector3Single;
operator * (const x,y:TVector3Double) : TVector3Double;
operator * (const x,y:TVector3Extended) : TVector3Extended;
operator * (const x,y:Tvector3Integer) : Tvector3Integer;
operator * (const x,y:TVector4Single) : TVector4Single;
operator * (const x,y:TVector4Double) : TVector4Double;
operator * (const x,y:TVector4Extended) : TVector4Extended;
operator * (const x,y:Tvector4Integer) : Tvector4Integer;

operator ** (const x,y:TVector2Single) : single;
operator ** (const x,y:TVector2Double) : double;
operator ** (const x,y:TVector2Extended) : extended;
operator ** (const x,y:Tvector2Integer) : Integer;
operator ** (const x,y:TVector3Single) : single;
operator ** (const x,y:TVector3Double) : double;
operator ** (const x,y:TVector3Extended) : extended;
operator ** (const x,y:Tvector3Integer) : Integer;
operator ** (const x,y:TVector4Single) : single;
operator ** (const x,y:TVector4Double) : double;
operator ** (const x,y:TVector4Extended) : extended;
operator ** (const x,y:Tvector4Integer) : Integer;

operator >< (const x,y:TVector3Single) : TVector3Single;
operator >< (const x,y:TVector3Double) : TVector3Double;
operator >< (const x,y:TVector3Extended) : TVector3Extended;
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
operator * (const x:TVector2Single;y:single) : TVector2Single;
operator * (const x:TVector2Double;y:double) : TVector2Double;
operator * (const x:TVector2Extended;y:extended) : TVector2Extended;
operator * (const x:TVector3Single;y:single) : TVector3Single;
operator * (const x:TVector3Double;y:double) : TVector3Double;
operator * (const x:TVector3Extended;y:extended) : TVector3Extended;
operator * (const x:TVector4Single;y:single) : TVector4Single;
operator * (const x:TVector4Double;y:double) : TVector4Double;
operator * (const x:TVector4Extended;y:extended) : TVector4Extended;

operator / (const x:TVector2Single;y:single) : TVector2Single;
operator / (const x:TVector2Double;y:double) : TVector2Double;
operator / (const x:TVector2Extended;y:extended) : TVector2Extended;
operator / (const x:TVector3Single;y:single) : TVector3Single;
operator / (const x:TVector3Double;y:double) : TVector3Double;
operator / (const x:TVector3Extended;y:extended) : TVector3Extended;
operator / (const x:TVector4Single;y:single) : TVector4Single;
operator / (const x:TVector4Double;y:double) : TVector4Double;
operator / (const x:TVector4Extended;y:extended) : TVector4Extended;

operator + (const m1,m2:TMatrix2Single) : TMatrix2Single;
operator + (const m1,m2:TMatrix2Double) : TMatrix2Double;
operator + (const m1,m2:TMatrix3Single) : TMatrix3Single;
operator + (const m1,m2:TMatrix3Double) : TMatrix3Double;
operator + (const m1,m2:TMatrix4Single) : TMatrix4Single;
operator + (const m1,m2:TMatrix4Double) : TMatrix4Double;

operator - (const m1,m2:TMatrix2Single) : TMatrix2Single;
operator - (const m1,m2:TMatrix2Double) : TMatrix2Double;
operator - (const m1,m2:TMatrix3Single) : TMatrix3Single;
operator - (const m1,m2:TMatrix3Double) : TMatrix3Double;
operator - (const m1,m2:TMatrix4Single) : TMatrix4Single;
operator - (const m1,m2:TMatrix4Double) : TMatrix4Double;

operator - (const m1:TMatrix2Single) : TMatrix2Single;
operator - (const m1:TMatrix2Double) : TMatrix2Double;
operator - (const m1:TMatrix3Single) : TMatrix3Single;
operator - (const m1:TMatrix3Double) : TMatrix3Double;
operator - (const m1:TMatrix4Single) : TMatrix4Single;
operator - (const m1:TMatrix4Double) : TMatrix4Double;

{ Note: matrix * matrix and matrix * vector multiplication
  is inside castlevectors_generic_float.inc.

  We do not use matrix multiplication code from FPC Matrix unit here,
  as their matrices are transposed compared to ours.
  Instead we use our own code, adjusted from MatrixMult implementation. }

{Matrix/scalar operations}
operator * (const m:TMatrix2Single;const x:single) : TMatrix2Single;
operator * (const m:TMatrix2Double;const x:double) : TMatrix2Double;
operator * (const m:TMatrix3Single;const x:single) : TMatrix3Single;
operator * (const m:TMatrix3Double;const x:double) : TMatrix3Double;
operator * (const m:TMatrix4Single;const x:single) : TMatrix4Single;
operator * (const m:TMatrix4Double;const x:double) : TMatrix4Double;

operator / (const m:TMatrix2Single;const x:single) : TMatrix2Single;
operator / (const m:TMatrix2Double;const x:double) : TMatrix2Double;
operator / (const m:TMatrix3Single;const x:single) : TMatrix3Single;
operator / (const m:TMatrix3Double;const x:double) : TMatrix3Double;
operator / (const m:TMatrix4Single;const x:single) : TMatrix4Single;
operator / (const m:TMatrix4Double;const x:double) : TMatrix4Double;

{$endif read_interface}

{$ifdef read_implementation}

{ To implement, use macros and
  castlevectors_operators_vector_implementation.inc,
  castlevectors_operators_matrix_implementation.inc
  includes. Copied and adjusted from FPC's Matrix implementation.
  I added CASTLEVECTOR_OPERATOR_ON_INTEGERS option, to overload also
  for integer vectors. }

{******************************************************************************
                                TVector2Single
******************************************************************************}

{Need to use capitals due to bug in FPC. Bug was fixed in FPC 1.9.3 on
 10 Feb. 2004}
{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=TVector2Single}
{$DEFINE vecsize:=2}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector2Single}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                TVector2Double
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=TVector2Double}
{$DEFINE vecsize:=2}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector2Double}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                               TVector2Extended
******************************************************************************}

{$DEFINE datatype:=EXTENDED}
{$DEFINE objectname:=TVector2Extended}
{$DEFINE vecsize:=2}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector2Extended}
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
                                TVector3Single
******************************************************************************}

{Need to use capitals due to bug in FPC. Bug was fixed in FPC 1.9.3 on
 10 Feb. 2004}
{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=TVector3Single}
{$DEFINE vecsize:=3}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector3Single}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                TVector3Double
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=TVector3Double}
{$DEFINE vecsize:=3}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector3Double}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                               TVector3Extended
******************************************************************************}

{$DEFINE datatype:=EXTENDED}
{$DEFINE objectname:=TVector3Extended}
{$DEFINE vecsize:=3}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector3Extended}
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
                                TVector4Single
******************************************************************************}

{Need to use capitals due to bug in FPC. Bug was fixed in FPC 1.9.3 on
 10 Feb. 2004}
{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=TVector4Single}
{$DEFINE vecsize:=4}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector4Single}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                                TVector4Double
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=TVector4Double}
{$DEFINE vecsize:=4}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector4Double}
{$i castlevectors_operators_vector_implementation.inc}

{******************************************************************************
                               TVector4Extended
******************************************************************************}

{$DEFINE datatype:=EXTENDED}
{$DEFINE objectname:=TVector4Extended}
{$DEFINE vecsize:=4}
{.$INFO Compile castlevectors_operators_vector_implementation.inc for TVector4Extended}
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
                                TMatrix2Single
******************************************************************************}

{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=TMatrix2Single}
{$DEFINE vectorcompanion:=TVector2Single}
{$DEFINE matsize:=2}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for TMatrix2Single}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                TMatrix2Double
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=TMatrix2Double}
{$DEFINE vectorcompanion:=TVector2Double}
{$DEFINE matsize:=2}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for TMatrix2Double}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                TMatrix3Single
******************************************************************************}

{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=TMatrix3Single}
{$DEFINE vectorcompanion:=TVector3Single}
{$DEFINE matsize:=3}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for TMatrix3Single}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                TMatrix3Double
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=TMatrix3Double}
{$DEFINE vectorcompanion:=TVector3Double}
{$DEFINE matsize:=3}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for TMatrix3Double}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                TMatrix4Single
******************************************************************************}

{$DEFINE datatype:=SINGLE}
{$DEFINE objectname:=TMatrix4Single}
{$DEFINE vectorcompanion:=TVector4Single}
{$DEFINE matsize:=4}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for TMatrix4Single}
{$i castlevectors_operators_matrix_implementation.inc}

{******************************************************************************
                                TMatrix4Double
******************************************************************************}

{$DEFINE datatype:=DOUBLE}
{$DEFINE objectname:=TMatrix4Double}
{$DEFINE vectorcompanion:=TVector4Double}
{$DEFINE matsize:=4}
{.$INFO Compile castlevectors_operators_matrix_implementation.inc for TMatrix4Double}
{$i castlevectors_operators_matrix_implementation.inc}

{$endif read_implementation}
