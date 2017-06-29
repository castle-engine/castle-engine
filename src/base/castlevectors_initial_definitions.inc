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

{ CastleVectors basic types and consts. }

{$ifdef read_interface}

{ Most types below are packed anyway, so the "packed" keyword below
  is often not needed (but it doesn't hurt).

  The fact that types
  below are packed is useful to easily map some of them to
  OpenGL, OpenAL types etc. It's also useful to be able to safely
  compare the types for exact equality by routines like CompareMem. }

type
  { }
  TVector2Single = packed array [0..1] of Single;     PVector2Single = ^TVector2Single;
  TVector2Double = packed array [0..1] of Double;     PVector2Double = ^TVector2Double;
  TVector2Extended = packed array [0..1] of Extended; PVector2Extended = ^TVector2Extended;
  TVector2Byte = packed array [0..1] of Byte;         PVector2Byte = ^TVector2Byte;
  TVector2Word = packed array [0..1] of Word;         PVector2Word = ^TVector2Word;
  TVector2SmallInt = packed array [0..1] of SmallInt; PVector2SmallInt = ^TVector2SmallInt;
  TVector2Longint = packed array [0..1] of Longint;   PVector2Longint = ^TVector2Longint;
  TVector2Pointer = packed array [0..1] of Pointer;   PVector2Pointer = ^TVector2Pointer;
  TVector2Cardinal = packed array [0..1] of Cardinal; PVector2Cardinal = ^TVector2Cardinal;
  TVector2Integer = packed array [0..1] of Integer;   PVector2Integer = ^TVector2Integer;

  TVector3Single = packed array [0..2] of Single;     PVector3Single = ^TVector3Single;
  TVector3Double = packed array [0..2] of Double;     PVector3Double = ^TVector3Double;
  TVector3Extended = packed array [0..2] of Extended; PVector3Extended = ^TVector3Extended;
  TVector3Byte = packed array [0..2] of Byte;         PVector3Byte = ^TVector3Byte;
  TVector3Word = packed array [0..2] of Word;         PVector3Word = ^TVector3Word;
  TVector3Longint = packed array [0..2] of Longint;   PVector3Longint = ^TVector3Longint;
  TVector3Pointer = packed array [0..2] of Pointer;   PVector3Pointer = ^TVector3Pointer;
  TVector3Integer = packed array [0..2] of Integer;   PVector3Integer = ^TVector3Integer;
  TVector3Cardinal = packed array [0..2] of Cardinal; PVector3Cardinal = ^TVector3Cardinal;

  TVector4Single = packed array [0..3] of Single;     PVector4Single = ^TVector4Single;
  TVector4Double = packed array [0..3] of Double;     PVector4Double = ^TVector4Double;
  TVector4Extended = packed array [0..3] of Extended; PVector4Extended = ^TVector4Extended;
  TVector4Byte = packed array [0..3] of Byte;         PVector4Byte = ^TVector4Byte;
  TVector4Word = packed array [0..3] of Word;         PVector4Word = ^TVector4Word;
  TVector4Longint = packed array [0..3] of Longint;   PVector4Longint = ^TVector4Longint;
  TVector4Pointer = packed array [0..3] of Pointer;   PVector4Pointer = ^TVector4Pointer;
  TVector4Cardinal = packed array [0..3] of Cardinal; PVector4Cardinal = ^TVector4Cardinal;
  TVector4Integer = packed array [0..3] of Integer;   PVector4Integer = ^TVector4Integer;

  { Matrices types.

    The indexing rules of these types are the same as indexing rules
    for matrix types of OpenGL. I.e. the 1st index specifies the column
    (where the leftmost column is numbered zero), 2nd index specifies the row
    (where the uppermost row is numbered zero).

    @bold(Note that this is different than how FPC Matrix unit
    treats matrices ! If you want to pass matrices between Matrix unit
    and this unit, you must transpose them !)

    As you can see, matrices below are not declared explicitly
    as 2-dimensional arrays (like @code(array [0..3, 0..3] of Single)),
    but they are 1-dimensional arrays of vectors.
    This is sometimes useful and comfortable.

    @groupBegin }
  TMatrix2Single = packed array [0..1] of TVector2Single;    PMatrix2Single = ^TMatrix2Single;
  TMatrix2Double = packed array [0..1] of TVector2Double;    PMatrix2Double = ^TMatrix2Double;
  TMatrix2Longint = packed array [0..1] of TVector2Longint;  PMatrix2Longint = ^TMatrix2Longint;

  TMatrix3Single = packed array [0..2] of TVector3Single;    PMatrix3Single = ^TMatrix3Single;
  TMatrix3Double = packed array [0..2] of TVector3Double;    PMatrix3Double = ^TMatrix3Double;
  TMatrix3Longint = packed array [0..2] of TVector3Longint;  PMatrix3Longint = ^TMatrix3Longint;

  TMatrix4Single = packed array [0..3] of TVector4Single;    PMatrix4Single = ^TMatrix4Single;
  TMatrix4Double = packed array [0..3] of TVector4Double;    PMatrix4Double = ^TMatrix4Double;
  TMatrix4Longint = packed array [0..3] of TVector4Longint;  PMatrix4Longint = ^TMatrix4Longint;
  { @groupEnd }

  { The "infinite" arrays, useful for some type-casting hacks }

  { }
  TArray_Vector2Byte = packed array [0..MaxInt div SizeOf(TVector2Byte)-1] of TVector2Byte;
  PArray_Vector2Byte = ^TArray_Vector2Byte;
  TArray_Vector3Byte = packed array [0..MaxInt div SizeOf(TVector3Byte)-1] of TVector3Byte;
  PArray_Vector3Byte = ^TArray_Vector3Byte;
  TArray_Vector4Byte = packed array [0..MaxInt div SizeOf(TVector4Byte)-1] of TVector4Byte;
  PArray_Vector4Byte = ^TArray_Vector4Byte;

  TArray_Vector2Cardinal = packed array [0..MaxInt div SizeOf(TVector2Cardinal) - 1] of TVector2Cardinal;
  PArray_Vector2Cardinal = ^TArray_Vector2Cardinal;

  TArray_Vector2Extended = packed array [0..MaxInt div SizeOf(TVector2Extended) - 1] of TVector2Extended;
  PArray_Vector2Extended = ^TArray_Vector2Extended;

  TArray_Vector2Single = packed array [0..MaxInt div SizeOf(TVector2Single) - 1] of TVector2Single;
  PArray_Vector2Single = ^TArray_Vector2Single;
  TArray_Vector3Single = packed array [0..MaxInt div SizeOf(TVector3Single) - 1] of TVector3Single;
  PArray_Vector3Single = ^TArray_Vector3Single;
  TArray_Vector4Single = packed array [0..MaxInt div SizeOf(TVector4Single) - 1] of TVector4Single;
  PArray_Vector4Single = ^TArray_Vector4Single;

  EVectorInvalidOp = class(Exception);

  TGetVertexFromIndexFunc = function (Index: integer): TVector3Single of object;

const
  ZeroVector2Integer: TVector2Integer = (0, 0);
  ZeroVector2Single: TVector2Single = (0, 0);
  ZeroVector2Double: TVector2Double = (0, 0);

  ZeroVector3Single: TVector3Single = (0, 0, 0);
  ZeroVector3Double: TVector3Double = (0, 0, 0);

  ZeroVector4Single: TVector4Single = (0, 0, 0, 0);
  ZeroVector4Double: TVector4Double = (0, 0, 0, 0);

  ZeroMatrix2Single: TMatrix2Single =   ((0, 0), (0, 0));
  ZeroMatrix2Double: TMatrix2Double =   ((0, 0), (0, 0));
  ZeroMatrix2Longint: TMatrix2Longint = ((0, 0), (0, 0));

  ZeroMatrix3Single: TMatrix3Single =   ((0, 0, 0), (0, 0, 0), (0, 0, 0));
  ZeroMatrix3Double: TMatrix3Double =   ((0, 0, 0), (0, 0, 0), (0, 0, 0));
  ZeroMatrix3Longint: TMatrix3Longint = ((0, 0, 0), (0, 0, 0), (0, 0, 0));

  ZeroMatrix4Single: TMatrix4Single =   ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));
  ZeroMatrix4Double: TMatrix4Double =   ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));
  ZeroMatrix4Longint: TMatrix4Longint = ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));

  IdentityMatrix2Single: TMatrix2Single =   ((1, 0), (0, 1));
  IdentityMatrix2Double: TMatrix2Double =   ((1, 0), (0, 1));
  IdentityMatrix2Longint: TMatrix2Longint = ((1, 0), (0, 1));

  IdentityMatrix3Single: TMatrix3Single =   ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  IdentityMatrix3Double: TMatrix3Double =   ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  IdentityMatrix3Longint: TMatrix3Longint = ((1, 0, 0), (0, 1, 0), (0, 0, 1));

  IdentityMatrix4Single: TMatrix4Single =   ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));
  IdentityMatrix4Double: TMatrix4Double =   ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));
  IdentityMatrix4Longint: TMatrix4Longint = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

  UnitVector3Single: array[0..2]of TVector3Single = ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  UnitVector3Double: array[0..2]of TVector3Double = ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  UnitVector4Single: array[0..3]of TVector4Single = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));
  UnitVector4Double: array[0..3]of TVector4Double = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

{ ---------------------------------------------------------------------------- }
{ @section(FloatsEqual and related things) }

var
  { Values that differ less than given *EqualityEpsilon are assumed
    as equal by FloatsEqual (and so by all other routines in this unit).

    Note that initial *EqualityEpsilon values are quite large,
    if you compare them with the epsilons used by CastleUtils.SameValue
    or Math.SameValue. Well, unfortunately they have to be so large,
    to always detect collisions.

    You can change the variables below (but always keep them >= 0).

    Exact 0 always means that exact comparison will be used.

    @groupBegin }
  SingleEqualityEpsilon: Single   = 1e-7;
  DoubleEqualityEpsilon: Double   = 1e-12;
  ExtendedEqualityEpsilon: Extended = {$ifdef EXTENDED_EQUALS_DOUBLE} 1e-12 {$else} 1e-16 {$endif};
  { @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
