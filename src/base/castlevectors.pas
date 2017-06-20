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

{ @abstract(Vector and matrix types and operations.
  Includes operations on basic geometric objects (2D and 3D),
  like collision-checking routines.)

  Representation of geometric objects in this unit :

  @unorderedList(
    @item(
      @italic(Plane in 3D space) is a vector TVector4*. Such vector [A, B, C, D]
      defines a surface that consists of all points satisfying equation
      @code(A * x + B * y + C * z + D = 0). At least one of A, B, C must be
      different than zero.

      Vector [A, B, C] is called PlaneDir in many places.
      Or PlaneNormal when it's guaranteed (or required to be) normalized,
      i.e. scaled to have length 1.)

    @item(
      @italic(Line in 3D space) is represented by two 3D vectors:
      Line0 and LineVector. They determine a line consisting of all
      points that can be calculated as @code(Line0 + R * LineVector)
      where R is any real value.

      LineVector must not be a zero vector.)

    @item(
      @italic(Line in 2D space) is sometimes represented as 2D vectors
      Line0 and LineVector (analogously like line in 3D).

      And sometimes it's represented as a 3-items vector,
      like TVector3Single (for [A, B, C] line consists of all
      points satisfying @code(A * x + B * y + C = 0)).
      At least one of A, B must be different than zero.)

    @item(
      A @italic(tunnel) is an object that you get by moving a sphere
      along the line segment. In other words, this is like a cylinder,
      but ended with a hemispheres. The tunnel is represented in this
      unit as two points Tunnel1, Tunnel2 (this defines a line segment)
      and a TunnelRadius.)

    @item(
      A @italic(ray) is defined just like a line: two vectors RayOrigin and RayDirection,
      RayDirection must be nonzero.
      Ray consists of all points @code(RayOrigin + R * RayDirection)
      for R being any real value >= 0.)

    @item(
      A @italic(simple plane in 3D) is a plane parallel to one of
      the three basic planes. This is a plane defined by the equation
      @code(X = Const) or @code(Y = Count) or @code(Z = Const).
      Such plane is represented as PlaneConstCoord integer value equal
      to 0, 1 or 2 and PlaneConstValue.

      Note that you can always represent the same plane using a more
      general plane 3D equation, just take

      @preformatted(
        Plane[0..2 / PlaneConstCoord] = 0,
        Plane[PlaneConstCoord] = -1,
        Plane[3] = PlaneConstValue.
      )

      On such "simple plane" we can perform many calculations
      much faster.)

    @item(
      A @italic(line segment) (often referred to as just @italic(segment))
      is represented by two points Pos1 and Pos2.
      For some routines the order of points Pos1 and Pos2 is significant
      (but this is always explicitly stated in the interface, so don't worry).

      Sometimes line segment is also represented as
      Segment0 and SegmentVector, this consists of all points
      @code(Segment0 + SegmentVector * t) for t in [0..1].
      SegmentVector must not be a zero vector.

      Conversion between the two representations above is trivial,
      just take Pos1 = Segment0 and Pos2 = Segment0 + SegmentVector.)
  )

  In descriptions of geometric objects above, I often
  stated some requirements, e.g. the triangle must not be "degenerated"
  to a line segment, RayDirection must not be a zero vector, etc.
  You should note that these requirements are generally @italic(not checked)
  by routines in this unit (for the sake of speed) and passing
  wrong values to many of the routines may lead to serious bugs
  --- maybe the function will raise some arithmetic exception,
  maybe it will return some nonsensible result. In other words: when
  calling these functions, always make sure that values you pass
  satisfy the requirements.

  (However, wrong input values should never lead to some serious
  bugs like access violations or range check errors ---
  in cases when it would be possible, we safeguard against this.
  That's because sometimes you simply cannot guarantee for 100%
  that input values are correct, because of floating-point precision
  problems -- see below.)

  As for floating-point precision:
  @unorderedList(
    @item(Well, floating-point inaccuracy is, as always, a problem.
      This unit always uses FloatsEqual
      and variables SingleEqualityEpsilon, DoubleEqualityEpsilon
      and ExtendedEpsilonEquality when comparison of floating-point
      values is needed. In some cases you may be able to adjust these
      variables to somewhat fine-tune the comparisons.)

    @item(For collision-detecting routines, the general strategy
      in case of uncertainty (when we're not sure whether there
      is a collision or the objects are just very very close to each other)
      is to say that there @italic(is a collision).

      This means that we may detect a collision when in fact the precise
      mathematical calculation says that there is no collision.

      This approach should be suitable for most use cases.)
  )

  A design question about this unit: Right now I must declare two variables
  to define a sphere (like @code(SphereCenter: vector; SphereRadius: scalar;))
  Why not wrap all the geometric objects (spheres, lines, rays, tunnels etc.)
  inside some records ? For example, define a sphere as

  @longcode(#
    TSphere = record Center: vector; Radius: scalar; end;
  #)

  The answer: this is not so good idea, because it would create
  a lot of such types into unit, and I would have to implement
  simple functions that construct and convert between these
  types. Consider e.g. when I have a tunnel (given
  as Tunnel1, Tunnel2 points and TunnelRadius vector)
  and I want to "extract" the properties of the sphere at the 1st end
  of this tunnel. Right now, it's simple: just consider
  Tunnel1 as a sphere center, and TunnelRadius is obviously a sphere radius.
  Computer doesn't have to actually do anything, you just have to think
  in a different way about Tunnel1 and TunnelRadius.
  But if I would have tunnel wrapped in a type like @code(TTunnel)
  and a sphere wrapped in a type like @code(TSphere), then I would
  have to actually implement this trivial conversion. And then doing
  such trivial conversion at run-time would take some time,
  because you have to copy 6 floating-point values.
  This would be a very serious waste of time at run-time.
  Well, on the other hand, routines could take less parameters
  (e.g. only 1 parameter @code(TTunnel), instead of three vector parameters),
  but (overall) we would still loose a lot of time.

  In many places where I have to return collision with
  a line segment, a line or a ray there are alternative versions
  that return just a scalar "T" instead of a calculated point.
  The actual collision point can be calculated then like
  @code(RayOrigin + T * RayDirection). Of course for rays you can be sure
  that T is >= 0, for line segments you can be sure that
  0 <= T <= 1. The "T" is often useful, because it allows
  you to easily calculate collision point, and also the distance
  to the collision (you can e.g. compare T1 and T2 to compare which
  collision is closer, and when your RayDirection is normalized then
  the T gives you the exact distance). Thanks to this you can often
  entirely avoid calculating the actual collision point
  (@code(RayOrigin + T * RayDirection)).

  Contains some stuff useful for integration with FPC's Matrix unit.
  For now, there are some "glueing" functions here like
  Vector_Get_Normalized that allow you to comfortably
  perform operations on Matrix unit object types.
  Most important is also the overload of ":=" operator that allows
  you to switch between CastleVectors arrays and Matrix objects without
  any syntax obfuscation. Although note that this overload is a little
  dangerous, since now code like
  @preformatted(  V3 := VectorProduct(V1, V2);)
  compiles and works both when all three V1, V2 and V3 are TVector3Single arrays
  or TVector3_Single objects. However, for the case when they are all
  TVector3_Single objects, this is highly un-optimal, and
  @preformatted(  V3 := V1 >< V2;)
  is much faster, since it avoids the implicit conversions (unnecessary
  memory copying around).
}

unit CastleVectors;

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, Matrix, CastleGenericLists;

{$define read_interface}

{ Define pointer types for all Matrix unit types. }
type
  { }
  Pvector2_single   = ^Tvector2_single  ;
  Pvector2_double   = ^Tvector2_double  ;
  Pvector2_extended = ^Tvector2_extended;

  Pvector3_single   = ^Tvector3_single  ;
  Pvector3_double   = ^Tvector3_double  ;
  Pvector3_extended = ^Tvector3_extended;

  Pvector4_single   = ^Tvector4_single  ;
  Pvector4_double   = ^Tvector4_double  ;
  Pvector4_extended = ^Tvector4_extended;

  Pmatrix2_single   = ^Tmatrix2_single  ;
  Pmatrix2_double   = ^Tmatrix2_double  ;
  Pmatrix2_extended = ^Tmatrix2_extended;

  Pmatrix3_single   = ^Tmatrix3_single  ;
  Pmatrix3_double   = ^Tmatrix3_double  ;
  Pmatrix3_extended = ^Tmatrix3_extended;

  Pmatrix4_single   = ^Tmatrix4_single  ;
  Pmatrix4_double   = ^Tmatrix4_double  ;
  Pmatrix4_extended = ^Tmatrix4_extended;

{ Most types below are packed anyway, so the "packed" keyword below
  is often not needed (but it doesn't hurt).

  The fact that types
  below are packed is useful to easily map some of them to
  OpenGL, OpenAL types etc. It's also useful to be able to safely
  compare the types for exact equality by routines like CompareMem. }

type
  { }
  TVector2Single = Tvector2_single_data;              PVector2Single = ^TVector2Single;
  TVector2Double = Tvector2_double_data;              PVector2Double = ^TVector2Double;
  TVector2Extended = Tvector2_extended_data;          PVector2Extended = ^TVector2Extended;
  TVector2Byte = packed array [0..1] of Byte;         PVector2Byte = ^TVector2Byte;
  TVector2Word = packed array [0..1] of Word;         PVector2Word = ^TVector2Word;
  TVector2SmallInt = packed array [0..1] of SmallInt; PVector2SmallInt = ^TVector2SmallInt;
  TVector2Longint = packed array [0..1] of Longint;   PVector2Longint = ^TVector2Longint;
  TVector2Pointer = packed array [0..1] of Pointer;   PVector2Pointer = ^TVector2Pointer;
  TVector2Cardinal = packed array [0..1] of Cardinal; PVector2Cardinal = ^TVector2Cardinal;
  TVector2Integer = packed array [0..1] of Integer;   PVector2Integer = ^TVector2Integer;

  TVector3Single = Tvector3_single_data;              PVector3Single = ^TVector3Single;
  TVector3Double = Tvector3_double_data;              PVector3Double = ^TVector3Double;
  TVector3Extended = Tvector3_extended_data;          PVector3Extended = ^TVector3Extended;
  TVector3Byte = packed array [0..2] of Byte;         PVector3Byte = ^TVector3Byte;
  TVector3Word = packed array [0..2] of Word;         PVector3Word = ^TVector3Word;
  TVector3Longint = packed array [0..2] of Longint;   PVector3Longint = ^TVector3Longint;
  TVector3Pointer = packed array [0..2] of Pointer;   PVector3Pointer = ^TVector3Pointer;
  TVector3Integer = packed array [0..2] of Integer;   PVector3Integer = ^TVector3Integer;
  TVector3Cardinal = packed array [0..2] of Cardinal; PVector3Cardinal = ^TVector3Cardinal;

  TVector4Single = Tvector4_single_data;              PVector4Single = ^TVector4Single;
  TVector4Double = Tvector4_double_data;              PVector4Double = ^TVector4Double;
  TVector4Extended = Tvector4_extended_data;          PVector4Extended = ^TVector4Extended;
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
  TMatrix2Single = Tmatrix2_single_data;                   PMatrix2Single = ^TMatrix2Single;
  TMatrix2Double = Tmatrix2_double_data;                   PMatrix2Double = ^TMatrix2Double;
  TMatrix2Longint = packed array[0..1]of TVector2Longint;  PMatrix2Longint = ^TMatrix2Longint;

  TMatrix3Single = Tmatrix3_single_data;                   PMatrix3Single = ^TMatrix3Single;
  TMatrix3Double = Tmatrix3_double_data;                   PMatrix3Double = ^TMatrix3Double;
  TMatrix3Longint = packed array[0..2]of TVector3Longint;  PMatrix3Longint = ^TMatrix3Longint;

  TMatrix4Single = Tmatrix4_single_data;                   PMatrix4Single = ^TMatrix4Single;
  TMatrix4Double = Tmatrix4_double_data;                   PMatrix4Double = ^TMatrix4Double;
  TMatrix4Longint = packed array[0..3]of TVector4Longint;  PMatrix4Longint = ^TMatrix4Longint;
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

{ Construct and convert vectors and other types ------------------------------ }

{ }
function Vector2Cardinal(const x, y: Cardinal): TVector2Cardinal;
function Vector2Integer(const x, y: Integer): TVector2Integer;
function Vector2LongInt(const x, y: Longint): TVector2LongInt;
function Vector2SmallInt(const x, y: SmallInt): TVector2SmallInt;

function Vector2Single(const x, y: Single): TVector2Single; overload;
function Vector2Single(const V: TVector2Double): TVector2Single; overload;

function Vector2Double(const x, y: Double): TVector2Double;

function Vector3Single(const x, y: Single; const z: Single = 0.0): TVector3Single; overload;
function Vector3Single(const v3: TVector3Double): TVector3Single; overload;
function Vector3Single(const v3: TVector3Byte): TVector3Single; overload;
function Vector3Single(const v2: TVector2Single; const z: Single = 0.0): TVector3Single; overload;

function Vector3Longint(const p0, p1, p2: Longint): TVector3Longint;

function Vector3Double(const x, y: Double; const z: Double = 0.0): TVector3Double; overload;
function Vector3Double(const v: TVector3Single): TVector3Double; overload;
function Vector3Double(const v2: TVector2Double; const z: Double = 0.0): TVector3Double; overload;

function Vector4Single(const x, y: Single;
  const z: Single = 0; const w: Single = 1): TVector4Single; overload;
function Vector4Single(const v3: TVector3Single;
  const w: Single = 1): TVector4Single; overload;
function Vector4Single(const v2: TVector2Single;
  const z: Single = 0; const w: Single = 1): TVector4Single; overload;
function Vector4Single(const ub: TVector4Byte): TVector4Single; overload;
function Vector4Single(const V3: TVector3Byte; const W: Byte): TVector4Single; overload;
function Vector4Single(const v: TVector4Double): TVector4Single; overload;

function Vector4Double(const x, y, z ,w: Double): TVector4Double; overload;
function Vector4Double(const v: TVector4Single): TVector4Double; overload;

function Vector2Byte(x, y: Byte): TVector2Byte; overload;

function Vector3Byte(x, y, z: Byte): TVector3Byte; overload;

{ Convert float vectors into byte vectors.
  Each float component is converted such that float 0.0 (or less) results in
  0 byte, 1.0 (or more) results in byte 255 (note: not 256).
  Values between 0.0 and 1.0 are appropriately (linearly) converted
  into the byte range.
  @groupBegin }
function Vector3Byte(const v: TVector3Single): TVector3Byte; overload;
function Vector3Byte(const v: TVector3Double): TVector3Byte; overload;
function Vector4Byte(const f4: TVector4Single): TVector4Byte; overload;
{ @groupEnd }

function Vector4Byte(const x, y, z: Byte; const w: Byte = 255): TVector4Byte; overload;
function Vector4Byte(const f3: TVector3Byte; w: Byte): TVector4Byte; overload;

function Vector4Integer(const X, Y, Z, W: Integer): TVector4Integer;

{ Convert a point in homogeneous coordinates into normal 3D point.
  In other words, convert 4D @code((x, y, z, w)) into
  @code((x/w, y/w, z/w)). Make sure the 4th vector component <> 0. }
function Vector3SinglePoint(const v: TVector4Single): TVector3Single;

{ Convert 3D vector into 2D by simply discarding (ignoring) the last component. }
function Vector2SingleCut(const v: TVector3Single): TVector2Single;

{ Convert 4D vector into 3D by simply discarding (ignoring) the 4th vector
  component. }
function Vector3SingleCut(const v: TVector4Single): TVector3Single;
function Vector3ByteCut(const v: TVector4Byte): TVector3Byte;

{ Construct and normalize 3D vector value. }
function Normal3Single(const x, y: Single; const z: Single = 0.0): TVector3Single; overload;

{ Convert string to vector. Each component is simply parsed by StrToFloat,
  and components must be separated by whitespace (see @link(WhiteSpaces) constant).
  @raises(EConvertError In case of problems during conversion (invalid float
    or unexpected string end or expected but missed string end).)
  @groupBegin }
function Vector2SingleFromStr(const S: string): TVector2Single;
function Vector3SingleFromStr(const S: string): TVector3Single;
function Vector3DoubleFromStr(const S: string): TVector3Double;
function Vector3ExtendedFromStr(const S: string): TVector3Extended;
function Vector4SingleFromStr(const S: string): TVector4Single;
{ @groupEnd }

{ Convert between single and double precision matrices.
  @groupBegin }
function Matrix2Double(const M: TMatrix2Single): TMatrix2Double;
function Matrix2Single(const M: TMatrix2Double): TMatrix2Single;
function Matrix3Double(const M: TMatrix3Single): TMatrix3Double;
function Matrix3Single(const M: TMatrix3Double): TMatrix3Single;
function Matrix4Double(const M: TMatrix4Single): TMatrix4Double;
function Matrix4Single(const M: TMatrix4Double): TMatrix4Single;
{ @groupEnd }

type
  EPlanesParallel = class(Exception);
  ELinesParallel = class(Exception);

var
  { Format used by functions named "ToNiceStr", like FloatToNiceStr and VectorToNiceStr. }
  FloatNiceFormat: string = 'f';

type
  ETransformedResultInvalid = class(EVectorInvalidOp);

{ Inverse the matrix, trying harder (but possibly slower).

  Basically, they internally calculate determinant and then calculate
  inverse using this determinant. Return @false if the determinant is zero.

  The main feature is that Single precision versions actually internally
  calculate everything (determinant and inverse) in Double precision.
  This gives better accuracy, and safety from matrices with very very small
  (but not zero) determinants.

  This is quite important for many matrices. For example, a 4x4 matrix
  with scaling = 1/200 (which can be easily found in practice,
  see e.g. castle/data/levels/gate/gate_processed.wrl) already
  has determinant = 1/8 000 000, which will not pass Zero test
  (with SingleEqualityEpsilon). But it's possible to calculate it
  (even on Single precision, although safer in Double precision).

  @groupBegin }
function TryMatrixInverse(const M: TMatrix2Single; out MInverse: TMatrix2Single): boolean; overload;
function TryMatrixInverse(const M: TMatrix3Single; out MInverse: TMatrix3Single): boolean; overload;
function TryMatrixInverse(const M: TMatrix4Single; out MInverse: TMatrix4Single): boolean; overload;
{$ifdef CASTLE_HAS_DOUBLE_PRECISION}
function TryMatrixInverse(const M: TMatrix2Double; out MInverse: TMatrix2Double): boolean; overload;
function TryMatrixInverse(const M: TMatrix3Double; out MInverse: TMatrix3Double): boolean; overload;
function TryMatrixInverse(const M: TMatrix4Double; out MInverse: TMatrix4Double): boolean; overload;
{$endif CASTLE_HAS_DOUBLE_PRECISION}
{ @groupEnd }

{ includes ------------------------------------------------------------------- }

{$I castlevectors_lists.inc}
{$I castlevectors_operators.inc}

{$I castlevectors_byte.inc}
{$I castlevectors_integer.inc}
{$I castlevectors_cardinal.inc}
{$I castlevectors_single.inc}
{$I castlevectors_double.inc}
{$I castlevectors_extended.inc}

{$define TScalar := Single}
{$define TVector2 := TVector2Single}
{$define TVector3 := TVector3Single}
{$define TVector4 := TVector4Single}
{$define PVector2 := PVector2Single}
{$define PVector3 := PVector3Single}
{$define PVector4 := PVector4Single}
{$define TMatrix2 := TMatrix2Single}
{$define TMatrix3 := TMatrix3Single}
{$define TMatrix4 := TMatrix4Single}
{$define ScalarEqualityEpsilon := SingleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Single}
{$define ZeroVector3 := ZeroVector3Single}
{$define IdentityMatrix4 := IdentityMatrix4Single}
{$define TMatrix2_ := TMatrix2_Single}
{$define TMatrix3_ := TMatrix3_Single}
{$define TMatrix4_ := TMatrix4_Single}
{$define TVector2_ := TVector2_Single}
{$define TVector3_ := TVector3_Single}
{$define TVector4_ := TVector4_Single}
{$define Vector3 := Vector3Single}
{$I castlevectors_generic_float.inc}

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}
{$define TScalar := Double}
{$define TVector2 := TVector2Double}
{$define TVector3 := TVector3Double}
{$define TVector4 := TVector4Double}
{$define PVector2 := PVector2Double}
{$define PVector3 := PVector3Double}
{$define PVector4 := PVector4Double}
{$define TMatrix2 := TMatrix2Double}
{$define TMatrix3 := TMatrix3Double}
{$define TMatrix4 := TMatrix4Double}
{$define ScalarEqualityEpsilon := DoubleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Double}
{$define ZeroVector3 := ZeroVector3Double}
{$define IdentityMatrix4 := IdentityMatrix4Double}
{$define TMatrix2_ := TMatrix2_Double}
{$define TMatrix3_ := TMatrix3_Double}
{$define TMatrix4_ := TMatrix4_Double}
{$define TVector2_ := TVector2_Double}
{$define TVector3_ := TVector3_Double}
{$define TVector4_ := TVector4_Double}
{$define Vector3 := Vector3Double}
{$I castlevectors_generic_float.inc}
{$endif CASTLE_HAS_DOUBLE_PRECISION}

{$undef read_interface}

implementation

uses Math, CastleStringUtils, CastleColors;

{$define read_implementation}

{ Separated from PointsDistance2DSqr, to not slowdown it by implicit
  try/finally section because we use string. }
procedure PointsDistance2DSqr_InvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for PointsDistance2DSqr');
end;

{ includes ------------------------------------------------------------------- }

{$I castlevectors_lists.inc}
{$I castlevectors_operators.inc}

{$I castlevectors_byte.inc}
{$I castlevectors_integer.inc}
{$I castlevectors_cardinal.inc}
{$I castlevectors_single.inc}
{$I castlevectors_double.inc}
{$I castlevectors_extended.inc}

{$define TScalar := Single}
{$define TVector2 := TVector2Single}
{$define TVector3 := TVector3Single}
{$define TVector4 := TVector4Single}
{$define PVector2 := PVector2Single}
{$define PVector3 := PVector3Single}
{$define PVector4 := PVector4Single}
{$define TMatrix2 := TMatrix2Single}
{$define TMatrix3 := TMatrix3Single}
{$define TMatrix4 := TMatrix4Single}
{$define ScalarEqualityEpsilon := SingleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Single}
{$define ZeroVector3 := ZeroVector3Single}
{$define IdentityMatrix4 := IdentityMatrix4Single}
{$define TMatrix2_ := TMatrix2_Single}
{$define TMatrix3_ := TMatrix3_Single}
{$define TMatrix4_ := TMatrix4_Single}
{$define TVector2_ := TVector2_Single}
{$define TVector3_ := TVector3_Single}
{$define TVector4_ := TVector4_Single}
{$define Vector3 := Vector3Single}
{$I castlevectors_generic_float.inc}

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}
{$define TScalar := Double}
{$define TVector2 := TVector2Double}
{$define TVector3 := TVector3Double}
{$define TVector4 := TVector4Double}
{$define PVector2 := PVector2Double}
{$define PVector3 := PVector3Double}
{$define PVector4 := PVector4Double}
{$define TMatrix2 := TMatrix2Double}
{$define TMatrix3 := TMatrix3Double}
{$define TMatrix4 := TMatrix4Double}
{$define ScalarEqualityEpsilon := DoubleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Double}
{$define ZeroVector3 := ZeroVector3Double}
{$define IdentityMatrix4 := IdentityMatrix4Double}
{$define TMatrix2_ := TMatrix2_Double}
{$define TMatrix3_ := TMatrix3_Double}
{$define TMatrix4_ := TMatrix4_Double}
{$define TVector2_ := TVector2_Double}
{$define TVector3_ := TVector3_Double}
{$define TVector4_ := TVector4_Double}
{$define Vector3 := Vector3Double}
{$I castlevectors_generic_float.inc}
{$endif CASTLE_HAS_DOUBLE_PRECISION}


{ type constructors ---------------------------------------------------------- }

function Vector2Integer(const x, y: Integer): TVector2Integer;
begin
  result[0] := x; result[1] := y;
end;

function Vector2LongInt(const x, y: Longint): TVector2LongInt;
begin
  result[0] := x; result[1] := y;
end;

function Vector2SmallInt(const x, y: SmallInt): TVector2SmallInt;
begin
  result[0] := x; result[1] := y;
end;

function Vector2Cardinal(const x, y: Cardinal): TVector2Cardinal;
begin
  result[0] := x; result[1] := y;
end;

function Vector2Single(const x, y: Single): TVector2Single;
begin
  result[0] := x; result[1] := y;
end;

function Vector2Single(const V: TVector2Double): TVector2Single;
begin
  Result[0] := V[0];
  Result[1] := V[1];
end;

function Vector2Double(const x, y: Double): TVector2Double;
begin
  result[0] := x; result[1] := y;
end;

function Vector4Single(const x, y: Single; const z: Single{=0}; const w: Single{=1}): TVector4Single;
begin
  result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4Single(const v3: TVector3Single; const w: Single{=1}): TVector4Single;
begin
  move(v3, result, SizeOf(TVector3Single));
  result[3] := w;
end;

function Vector4Single(const v2: TVector2Single;
  const z: Single = 0; const w: Single = 1): TVector4Single;
begin
  Move(V2, Result, SizeOf(TVector2Single));
  Result[2] := Z;
  Result[3] := W;
end;

function Vector4Single(const ub: TVector4Byte): TVector4Single;
begin
  result[0] := ub[0]/255;
  result[1] := ub[1]/255;
  result[2] := ub[2]/255;
  result[3] := ub[3]/255;
end;

function Vector4Single(const V3: TVector3Byte; const W: Byte): TVector4Single;
begin
  result[0] := V3[0] / 255;
  result[1] := V3[1] / 255;
  result[2] := V3[2] / 255;
  result[3] := W;
end;

function Vector4Single(const v: TVector4Double): TVector4Single;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := v[3];
end;

function Vector4Double(const x, y, z, w: Double): TVector4Double;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

function Vector4Double(const v: TVector4Single): TVector4Double;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := v[3];
end;

function Vector3Single(const x, y: Single; const z: Single{=0.0}): TVector3Single;
begin
  result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3Double(const x, y: Double; const z: Double{=0.0}): TVector3Double;
begin
  result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3Single(const v3: TVector3Double): TVector3Single;
begin
  result[0] := v3[0]; result[1] := v3[1]; result[2] := v3[2];
end;

function Vector3Single(const v3: TVector3Byte): TVector3Single;
begin
  result[0] := v3[0]/255;
  result[1] := v3[1]/255;
  result[2] := v3[2]/255;
end;

function Vector3Single(const v2: TVector2Single; const z: Single): TVector3Single;
begin
  move(v2, result, SizeOf(v2));
  result[2] := z;
end;

function Vector3Double(const v: TVector3Single): TVector3Double;
begin
  result[0] := v[0]; result[1] := v[1]; result[2] := v[2];
end;

function Vector3Double(const v2: TVector2Double; const z: Double): TVector3Double;
begin
  move(v2, result, SizeOf(v2));
  result[2] := z;
end;

function Vector2Byte(x, y: Byte): TVector2Byte;
begin
  result[0] := x; result[1] := y;
end;

function Vector3Byte(x, y, z: Byte): TVector3Byte;
begin
  result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3Byte(const v: TVector3Single): TVector3Byte;
begin
  result[0] := Clamped(Round(v[0] * 255), Low(Byte), High(Byte));
  result[1] := Clamped(Round(v[1] * 255), Low(Byte), High(Byte));
  result[2] := Clamped(Round(v[2] * 255), Low(Byte), High(Byte));
end;

function Vector3Byte(const v: TVector3Double): TVector3Byte;
begin
  result[0] := Clamped(Round(v[0] * 255), Low(Byte), High(Byte));
  result[1] := Clamped(Round(v[1] * 255), Low(Byte), High(Byte));
  result[2] := Clamped(Round(v[2] * 255), Low(Byte), High(Byte));
end;

function Vector3Longint(const p0, p1, p2: Longint): TVector3Longint;
begin
  result[0] := p0;
  result[1] := p1;
  result[2] := p2;
end;

function Vector4Byte(const x, y, z, w: Byte): TVector4Byte;
begin
  result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4Byte(const f4: TVector4Single): TVector4Byte;
begin
  result[0] := Round(f4[0] * 255);
  result[1] := Round(f4[1] * 255);
  result[2] := Round(f4[2] * 255);
  result[3] := Round(f4[3] * 255);
end;

function Vector4Byte(const f3: TVector3Byte; w: Byte): TVector4Byte;
begin
  result[0] := f3[0];
  result[1] := f3[1];
  result[2] := f3[2];
  result[3] := w;
end;

function Vector4Integer(const X, Y, Z, W: Integer): TVector4Integer;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
  Result[3] := W;
end;

function Vector3SinglePoint(const v: TVector4Single): TVector3Single;
begin
  result[0] := v[0]/v[3];
  result[1] := v[1]/v[3];
  result[2] := v[2]/v[3];
end;

function Vector2SingleCut(const v: TVector3Single): TVector2Single;
begin
  Move(v, Result, SizeOf(Result));
end;

function Vector3SingleCut(const v: TVector4Single): TVector3Single;
begin
  Move(v, Result, SizeOf(Result));
end;

function Vector3ByteCut(const v: TVector4Byte): TVector3Byte;
begin
  Move(v, Result, SizeOf(Result));
end;

function Normal3Single(const x, y: Single; const z: Single{=0}): TVector3Single;
begin
  result[0] := x; result[1] := y; result[2] := z;
  NormalizeVar3Singlev(@result);
end;

function Vector2SingleFromStr(const S: string): TVector2Single;
var
  SPosition: Integer;
begin
  SPosition := 1;
  Result[0] := StrToFloat(NextToken(S, SPosition));
  Result[1] := StrToFloat(NextToken(S, SPosition));
  if NextToken(S, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');
end;

function Vector3SingleFromStr(const s: string): TVector3Single; {$I castlevectors_vector3fromstr.inc}
function Vector3DoubleFromStr(const s: string): TVector3Double; {$I castlevectors_vector3fromstr.inc}
function Vector3ExtendedFromStr(const s: string): TVector3Extended; {$I castlevectors_vector3fromstr.inc}

function Vector4SingleFromStr(const S: string): TVector4Single;
var
  SPosition: Integer;
begin
  SPosition := 1;
  Result[0] := StrToFloat(NextToken(S, SPosition));
  Result[1] := StrToFloat(NextToken(S, SPosition));
  Result[2] := StrToFloat(NextToken(S, SPosition));
  Result[3] := StrToFloat(NextToken(S, SPosition));
  if NextToken(s, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');
end;

function Matrix2Double(const M: TMatrix2Single): TMatrix2Double;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
end;

function Matrix2Single(const M: TMatrix2Double): TMatrix2Single;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
end;

function Matrix3Double(const M: TMatrix3Single): TMatrix3Double;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
end;

function Matrix3Single(const M: TMatrix3Double): TMatrix3Single;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
end;

function Matrix4Double(const M: TMatrix4Single): TMatrix4Double;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];
  Result[0][3] := M[0][3];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];
  Result[1][3] := M[1][3];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
  Result[2][3] := M[2][3];

  Result[3][0] := M[3][0];
  Result[3][1] := M[3][1];
  Result[3][2] := M[3][2];
  Result[3][3] := M[3][3];
end;

function Matrix4Single(const M: TMatrix4Double): TMatrix4Single;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];
  Result[0][3] := M[0][3];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];
  Result[1][3] := M[1][3];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
  Result[2][3] := M[2][3];

  Result[3][0] := M[3][0];
  Result[3][1] := M[3][1];
  Result[3][2] := M[3][2];
  Result[3][3] := M[3][3];
end;

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}

{ When CASTLE_HAS_DOUBLE_PRECISION is not defined, use slower implementations
  of TryMatrixInverse, that convert to Double precision. }

function TryMatrixInverse(const M: TMatrix2Single; out MInverse: TMatrix2Single): boolean;
var
  D: Double;
  MD, MDInverse: TMatrix2Double;
begin
  MD := Matrix2Double(M);
  D := MatrixDeterminant(MD);
  Result := not Zero(D);
  if Result then
  begin
    MDInverse := MatrixInverse(MD, D);
    MInverse := Matrix2Single(MDInverse);
  end;
end;

function TryMatrixInverse(const M: TMatrix2Double; out MInverse: TMatrix2Double): boolean;
var
  D: Double;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix3Single; out MInverse: TMatrix3Single): boolean;
var
  D: Double;
  MD, MDInverse: TMatrix3Double;
begin
  MD := Matrix3Double(M);
  D := MatrixDeterminant(MD);
  Result := not Zero(D);
  if Result then
  begin
    MDInverse := MatrixInverse(MD, D);
    MInverse := Matrix3Single(MDInverse);
  end;
end;

function TryMatrixInverse(const M: TMatrix3Double; out MInverse: TMatrix3Double): boolean;
var
  D: Double;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix4Single; out MInverse: TMatrix4Single): boolean;
var
  D: Double;
  MD, MDInverse: TMatrix4Double;
begin
  MD := Matrix4Double(M);
  D := MatrixDeterminant(MD);
  Result := not Zero(D);
  if Result then
  begin
    MDInverse := MatrixInverse(MD, D);
    MInverse := Matrix4Single(MDInverse);
  end;
end;

function TryMatrixInverse(const M: TMatrix4Double; out MInverse: TMatrix4Double): boolean;
var
  D: Double;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;
{$else}

{ When CASTLE_HAS_DOUBLE_PRECISION is not defined, use simpler implementations
  of TryMatrixInverse, that stay within Single precision. }

function TryMatrixInverse(const M: TMatrix2Single; out MInverse: TMatrix2Single): boolean;
var
  D: Single;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix3Single; out MInverse: TMatrix3Single): boolean;
var
  D: Single;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix4Single; out MInverse: TMatrix4Single): boolean;
var
  D: Single;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;
{$endif CASTLE_HAS_DOUBLE_PRECISION}


end.
