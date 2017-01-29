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

  TVector4SingleList = class;

  TVector3SingleList = class(specialize TGenericStructList<TVector3Single>)
  public
    procedure AssignNegated(Source: TVector3SingleList);
    { Negate all items. }
    procedure Negate;
    { Normalize all items. Zero vectors are left as zero. }
    procedure Normalize;
    { Multiply each item, component-wise, with V. }
    procedure MultiplyComponents(const V: TVector3Single);

    { Assign linear interpolation between two other vector arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);

    { Assign linear interpolation between two other vector arrays,
      and normalize resulting vectors.
      @seealso AssignLerp }
    procedure AssignLerpNormalize(const Fraction: Single;
      V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);

    { Assign linear interpolation between two other vector arrays,
      treating vectors as RGB colors and interpolating in HSV space.
      @seealso AssignLerp }
    procedure AssignLerpRgbInHsv(const Fraction: Single;
      V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);

    procedure AddList(Source: TVector3SingleList);
    procedure AddListRange(Source: TVector3SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector3Single);
    procedure AssignArray(const A: array of TVector3Single);

    { Convert to TVector4SingleList, with 4th vector component in
      new array set to constant W. }
    function ToVector4Single(const W: Single): TVector4SingleList;

    { When two vertexes on the list are closer than MergeDistance,
      set them truly (exactly) equal.
      Returns how many vertex positions were changed. }
    function MergeCloseVertexes(MergeDistance: Single): Cardinal;
  end;

  TVector2SingleList = class(specialize TGenericStructList<TVector2Single>)
  public
    { Calculate minimum and maximum values for both dimensions of
      this set of points. Returns @false when Count = 0. }
    function MinMax(out Min, Max: TVector2Single): boolean;

    { Assign linear interpolation between two other vector arrays.
      @seealso TVector3SingleList.AssignLerp }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TVector2SingleList; Index1, Index2, ACount: Integer);

    procedure AddList(Source: TVector2SingleList);
    procedure AddListRange(Source: TVector2SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector2Single);
    procedure AssignArray(const A: array of TVector2Single);
  end;

  TVector4SingleList = class(specialize TGenericStructList<TVector4Single>)
  public
    procedure AddList(Source: TVector4SingleList);
    procedure AddListRange(Source: TVector4SingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of TVector4Single);
    procedure AssignArray(const A: array of TVector4Single);
  end;

  TVector3CardinalList = specialize TGenericStructList<TVector3Cardinal>;

  TVector2DoubleList = class(specialize TGenericStructList<TVector2Double>)
  public
    function ToVector2Single: TVector2SingleList;
    procedure AddList(Source: TVector2DoubleList);
    procedure AddArray(const A: array of TVector2Double);
  end;

  TVector3DoubleList = class(specialize TGenericStructList<TVector3Double>)
  public
    function ToVector3Single: TVector3SingleList;
    procedure AddList(Source: TVector3DoubleList);
    procedure AddArray(const A: array of TVector3Double);
  end;

  TVector4DoubleList = class(specialize TGenericStructList<TVector4Double>)
  public
    function ToVector4Single: TVector4SingleList;
    procedure AddList(Source: TVector4DoubleList);
    procedure AddArray(const A: array of TVector4Double);
  end;

  TMatrix3SingleList = class(specialize TGenericStructList<TMatrix3Single>)
  public
    procedure AddList(Source: TMatrix3SingleList);
    procedure AddArray(const A: array of TMatrix3Single);
  end;

  TMatrix3DoubleList = class(specialize TGenericStructList<TMatrix3Double>)
  public
    function ToMatrix3Single: TMatrix3SingleList;
    procedure AddList(Source: TMatrix3DoubleList);
    procedure AddArray(const A: array of TMatrix3Double);
  end;

  TMatrix4SingleList = class(specialize TGenericStructList<TMatrix4Single>)
  public
    procedure AddList(Source: TMatrix4SingleList);
    procedure AddArray(const A: array of TMatrix4Single);
  end;

  TMatrix4DoubleList = class(specialize TGenericStructList<TMatrix4Double>)
  public
    function ToMatrix4Single: TMatrix4SingleList;
    procedure AddList(Source: TMatrix4DoubleList);
    procedure AddArray(const A: array of TMatrix4Double);
  end;

  TVector2IntegerList = class(specialize TGenericStructList<TVector2Integer>)
    procedure Reverse;
    procedure AddList(Source: TVector2IntegerList);
  end;

  TVector2SmallIntList = specialize TGenericStructList<TVector2SmallInt>;

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
  ExtendedEqualityEpsilon: Extended = 1e-16;
  { @groupEnd }

{ Compare two float values, with some epsilon.
  When two float values differ by less than given epsilon, they are
  considered equal.

  @groupBegin }
function FloatsEqual(const f1, f2: Single): boolean; overload;
function FloatsEqual(const f1, f2: Double): boolean; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function FloatsEqual(const f1, f2: Extended): boolean; overload;
{$endif}
function FloatsEqual(const f1, f2, EqEpsilon: Single): boolean; overload;
function FloatsEqual(const f1, f2, EqEpsilon: Double): boolean; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function FloatsEqual(const f1, f2, EqEpsilon: Extended): boolean; overload;
{$endif}
{ @groupEnd }

{ Compare float value with zero, with some epsilon.
  This is somewhat optimized version of doing FloatsEqual(F1, 0).

  This is named Zero, not IsZero --- to not collide with IsZero function
  in Math unit (that has the same purpose, but uses different epsilons
  by default).

  @groupBegin }
function Zero(const f1: Single): boolean; overload;
function Zero(const f1: Double): boolean; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function Zero(const f1: Extended): boolean; overload;
{$endif}
function Zero(const f1, EqEpsilon: Single  ): boolean; overload;
function Zero(const f1, EqEpsilon: Double  ): boolean; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function Zero(const f1, EqEpsilon: Extended): boolean; overload;
{$endif}

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

{ Overload := operator to allow conversion between
  Matrix unit objects and this unit's arrays easy. }
operator := (const V: TVector2_Single): TVector2Single;
operator := (const V: TVector3_Single): TVector3Single;
operator := (const V: TVector4_Single): TVector4Single;
operator := (const V: TVector2Single): TVector2_Single;
operator := (const V: TVector3Single): TVector3_Single;
operator := (const V: TVector4Single): TVector4_Single;

{ Endianess utility functions for vectors  ----------------------------------- }

function SwapEndian(const V: TVector2Single): TVector2Single; overload;
function SwapEndian(const V: TVector2Double): TVector2Double; overload;
function SwapEndian(const V: TVector3Single): TVector3Single; overload;
function SwapEndian(const V: TVector3Double): TVector3Double; overload;
function SwapEndian(const V: TVector4Single): TVector4Single; overload;
function SwapEndian(const V: TVector4Double): TVector4Double; overload;

function LEtoN(const V: TVector2Single): TVector2Single; overload;
function LEtoN(const V: TVector2Double): TVector2Double; overload;
function LEtoN(const V: TVector3Single): TVector3Single; overload;
function LEtoN(const V: TVector3Double): TVector3Double; overload;
function LEtoN(const V: TVector4Single): TVector4Single; overload;
function LEtoN(const V: TVector4Double): TVector4Double; overload;

function BEtoN(const V: TVector2Single): TVector2Single; overload;
function BEtoN(const V: TVector2Double): TVector2Double; overload;
function BEtoN(const V: TVector3Single): TVector3Single; overload;
function BEtoN(const V: TVector3Double): TVector3Double; overload;
function BEtoN(const V: TVector4Single): TVector4Single; overload;
function BEtoN(const V: TVector4Double): TVector4Double; overload;

function NtoLE(const V: TVector2Single): TVector2Single; overload;
function NtoLE(const V: TVector2Double): TVector2Double; overload;
function NtoLE(const V: TVector3Single): TVector3Single; overload;
function NtoLE(const V: TVector3Double): TVector3Double; overload;
function NtoLE(const V: TVector4Single): TVector4Single; overload;
function NtoLE(const V: TVector4Double): TVector4Double; overload;

function NtoBE(const V: TVector2Single): TVector2Single; overload;
function NtoBE(const V: TVector2Double): TVector2Double; overload;
function NtoBE(const V: TVector3Single): TVector3Single; overload;
function NtoBE(const V: TVector3Double): TVector3Double; overload;
function NtoBE(const V: TVector4Single): TVector4Single; overload;
function NtoBE(const V: TVector4Double): TVector4Double; overload;

{ Simple vectors operations  ------------------------------------------------- }

{ }
procedure SwapValues(var V1, V2: TVector2Single); overload;
procedure SwapValues(var V1, V2: TVector2Double); overload;
procedure SwapValues(var V1, V2: TVector3Single); overload;
procedure SwapValues(var V1, V2: TVector3Double); overload;
procedure SwapValues(var V1, V2: TVector4Single); overload;
procedure SwapValues(var V1, V2: TVector4Double); overload;

function VectorAverage(const V: TVector3Single): Single; overload;
function VectorAverage(const V: TVector3Double): Double; overload;

{ When you really, really must approximate a 3D scale by a single float.
  If your algorithm cannot handle non-uniform 3D scale,
  you have to approximate 3D scale by a single float.

  This is similar to an average of X, Y, Z, but it additionally
  secures in case some of X, Y, Z are negative and some are positive
  (like scale (-1, 1), common in 2D to flip horizontally).

  @groupBegin }
function Approximate3DScale(const X, Y, Z: Single): Single;
function Approximate3DScale(const V: TVector3Single): Single;
{ @groupEnd }

{ Linear interpolation between two vector values.
  Returns (1-A) * V1 + A * V2 (well, calculated a little differently for speed).
  So A = 0 gives V1, A = 1 gives V2, and values between and around are
  interpolated.

  @groupBegin }
function Lerp(const A: Single; const V1, V2: TVector2Byte): TVector2Byte; overload;
function Lerp(const A: Single; const V1, V2: TVector3Byte): TVector3Byte; overload;
function Lerp(const A: Single; const V1, V2: TVector4Byte): TVector4Byte; overload;
function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2Single; overload;
function Lerp(const A: Single; const V1, V2: TVector2Single): TVector2Single; overload;
function Lerp(const A: Single; const V1, V2: TVector3Single): TVector3Single; overload;
function Lerp(const A: Single; const V1, V2: TVector4Single): TVector4Single; overload;
function Lerp(const A: Double; const V1, V2: TVector2Double): TVector2Double; overload;
function Lerp(const A: Double; const V1, V2: TVector3Double): TVector3Double; overload;
function Lerp(const A: Double; const V1, V2: TVector4Double): TVector4Double; overload;
function Lerp(const A: Single; const M1, M2: TMatrix3Single): TMatrix3Single; overload;
function Lerp(const A: Single; const M1, M2: TMatrix4Single): TMatrix4Single; overload;
function Lerp(const A: Double; const M1, M2: TMatrix3Double): TMatrix3Double; overload;
function Lerp(const A: Double; const M1, M2: TMatrix4Double): TMatrix4Double; overload;
{ @groupEnd }

function Vector_Init_Lerp(const A: Single; const V1, V2: TVector3_Single): TVector3_Single; overload;
function Vector_Init_Lerp(const A: Single; const V1, V2: TVector4_Single): TVector4_Single; overload;
function Vector_Init_Lerp(const A: Double; const V1, V2: TVector3_Double): TVector3_Double; overload;
function Vector_Init_Lerp(const A: Double; const V1, V2: TVector4_Double): TVector4_Double; overload;

{ Normalize the first 3 vector components. For zero vectors, does nothing.
  @groupBegin }
procedure NormalizeVar3Singlev(vv: PVector3Single);
procedure NormalizeVar3Bytev(vv: PVector3Byte);
{ @groupEnd }

procedure NormalizeVar(var v: TVector2Single); overload;
procedure NormalizeVar(var v: TVector2Double); overload;
procedure NormalizeVar(var v: TVector3Single); overload;
procedure NormalizeVar(var v: TVector3Double); overload;

function Normalized(const v: TVector2Single): TVector2Single; overload;
function Normalized(const v: TVector2Double): TVector2Double; overload;
function Normalized(const v: TVector3Single): TVector3Single; overload;
function Normalized(const v: TVector3Double): TVector3Double; overload;

function Vector_Get_Normalized(const V: TVector3_Single): TVector3_Single; overload;
function Vector_Get_Normalized(const V: TVector3_Double): TVector3_Double; overload;

procedure Vector_Normalize(var V: TVector3_Single); overload;
procedure Vector_Normalize(var V: TVector3_Double); overload;

{ This normalizes Plane by scaling all @italic(four) coordinates of Plane
  so that length of plane vector (taken from 1st @italic(three) coordinates)
  is one.

  Also, contrary to normal NormalizeVar on 3-component vectors,
  this will fail with some awful error (like floating point overflow)
  in case length of plane vector is zero. That's because we know
  that plane vector @italic(must) be always non-zero. }
procedure NormalizePlaneVar(var v: TVector4Single); overload;
procedure NormalizePlaneVar(var v: TVector4Double); overload;

function ZeroVector(const v: TVector2Single): boolean; overload;
function ZeroVector(const v: TVector2Double): boolean; overload;
function ZeroVector(const v: TVector3Single): boolean; overload;
function ZeroVector(const v: TVector3Double): boolean; overload;
function ZeroVector(const v: TVector4Single): boolean; overload;
function ZeroVector(const v: TVector4Double): boolean; overload;

function ZeroVector(const v: TVector2Single; const EqualityEpsilon: Single): boolean; overload;
function ZeroVector(const v: TVector2Double; const EqualityEpsilon: Double): boolean; overload;
function ZeroVector(const v: TVector3Single; const EqualityEpsilon: Single): boolean; overload;
function ZeroVector(const v: TVector3Double; const EqualityEpsilon: Double): boolean; overload;
function ZeroVector(const v: TVector4Single; const EqualityEpsilon: Single): boolean; overload;
function ZeroVector(const v: TVector4Double; const EqualityEpsilon: Double): boolean; overload;

function ZeroVector(const v: TVector4Integer): boolean; overload;
function ZeroVector(const v: TVector4Cardinal): boolean; overload;

function PerfectlyZeroVector(const v: TVector2Single): boolean; overload;
function PerfectlyZeroVector(const v: TVector2Double): boolean; overload;
function PerfectlyZeroVector(const v: TVector3Single): boolean; overload;
function PerfectlyZeroVector(const v: TVector3Double): boolean; overload;
function PerfectlyZeroVector(const v: TVector4Single): boolean; overload;
function PerfectlyZeroVector(const v: TVector4Double): boolean; overload;

{ Subtract two vectors.

  Versions *Var place result back into the 1st vector,
  like "-=" operator. Are @italic(very very slightly) faster.

  @groupBegin }
function VectorSubtract(const V1, V2: TVector2Single): TVector2Single; overload;
function VectorSubtract(const V1, V2: TVector2Double): TVector2Double; overload;
function VectorSubtract(const V1, V2: TVector3Single): TVector3Single; overload;
function VectorSubtract(const V1, V2: TVector3Double): TVector3Double; overload;
function VectorSubtract(const V1, V2: TVector4Single): TVector4Single; overload;
function VectorSubtract(const V1, V2: TVector4Double): TVector4Double; overload;
procedure VectorSubtractVar(var v1: TVector2Single; const v2: TVector2Single); overload;
procedure VectorSubtractVar(var v1: TVector2Double; const v2: TVector2Double); overload;
procedure VectorSubtractVar(var v1: TVector3Single; const v2: TVector3Single); overload;
procedure VectorSubtractVar(var v1: TVector3Double; const v2: TVector3Double); overload;
procedure VectorSubtractVar(var v1: TVector4Single; const v2: TVector4Single); overload;
procedure VectorSubtractVar(var v1: TVector4Double; const v2: TVector4Double); overload;
{ @groupEnd }

{ Add two vectors.

  Versions *Var place result back into the 1st vector,
  like "+=" operator. Are @italic(very very slightly) faster.

  @groupBegin }
function VectorAdd(const V1, V2: TVector2Single): TVector2Single; overload;
function VectorAdd(const V1, V2: TVector2Double): TVector2Double; overload;
function VectorAdd(const V1, V2: TVector3Single): TVector3Single; overload;
function VectorAdd(const V1, V2: TVector3Double): TVector3Double; overload;
function VectorAdd(const V1, V2: TVector4Single): TVector4Single; overload;
function VectorAdd(const V1, V2: TVector4Double): TVector4Double; overload;
procedure VectorAddVar(var v1: TVector2Single; const v2: TVector2Single); overload;
procedure VectorAddVar(var v1: TVector2Double; const v2: TVector2Double); overload;
procedure VectorAddVar(var v1: TVector3Single; const v2: TVector3Single); overload;
procedure VectorAddVar(var v1: TVector3Double; const v2: TVector3Double); overload;
procedure VectorAddVar(var v1: TVector4Single; const v2: TVector4Single); overload;
procedure VectorAddVar(var v1: TVector4Double; const v2: TVector4Double); overload;
{ @groupEnd }

{ Scale vector (aka multiply by scalar).

  Versions *Var scale place result back into the 1st vector,
  like "*=" operator. Are @italic(very very slightly) faster.

  @groupBegin }
function VectorScale(const v1: TVector2Single; const Scalar: Single): TVector2Single; overload;
function VectorScale(const v1: TVector2Double; const Scalar: Double): TVector2Double; overload;
function VectorScale(const v1: TVector3Single; const Scalar: Single): TVector3Single; overload;
function VectorScale(const v1: TVector3Double; const Scalar: Double): TVector3Double; overload;
function VectorScale(const v1: TVector4Single; const Scalar: Single): TVector4Single; overload;
function VectorScale(const v1: TVector4Double; const Scalar: Double): TVector4Double; overload;
procedure VectorScaleVar(var v1: TVector2Single; const Scalar: Single); overload;
procedure VectorScaleVar(var v1: TVector2Double; const Scalar: Double); overload;
procedure VectorScaleVar(var v1: TVector3Single; const Scalar: Single); overload;
procedure VectorScaleVar(var v1: TVector3Double; const Scalar: Double); overload;
procedure VectorScaleVar(var v1: TVector4Single; const Scalar: Single); overload;
procedure VectorScaleVar(var v1: TVector4Double; const Scalar: Double); overload;
{ @groupEnd }

{ Negate vector (return -V).

  Versions *Var scale place result back into the 1st vector.
  Are @italic(very very slightly) faster.

  @groupBegin }
function VectorNegate(const v: TVector2Single): TVector2Single; overload;
function VectorNegate(const v: TVector2Double): TVector2Double; overload;
function VectorNegate(const v: TVector3Single): TVector3Single; overload;
function VectorNegate(const v: TVector3Double): TVector3Double; overload;
function VectorNegate(const v: TVector4Single): TVector4Single; overload;
function VectorNegate(const v: TVector4Double): TVector4Double; overload;
procedure VectorNegateVar(var v: TVector2Single); overload;
procedure VectorNegateVar(var v: TVector2Double); overload;
procedure VectorNegateVar(var v: TVector3Single); overload;
procedure VectorNegateVar(var v: TVector3Double); overload;
procedure VectorNegateVar(var v: TVector4Single); overload;
procedure VectorNegateVar(var v: TVector4Double); overload;
{ @groupEnd }

{ Scale vector such that it has given length (VecLen).
  Given VecLen may be negative, then we'll additionally negate the vector.
  @groupBegin }
function VectorAdjustToLength(const v: TVector2Single; VecLen: Single): TVector2Single; overload;
function VectorAdjustToLength(const v: TVector3Single; VecLen: Single): TVector3Single; overload;
function VectorAdjustToLength(const v: TVector3Double; VecLen: Double): TVector3Double; overload;
procedure VectorAdjustToLengthVar(var v: TVector2Single; VecLen: Single); overload;
procedure VectorAdjustToLengthVar(var v: TVector3Single; VecLen: Single); overload;
procedure VectorAdjustToLengthVar(var v: TVector3Double; VecLen: Double); overload;
{ @groupEnd }

{ Vector length.
  @groupBegin }
function VectorLen(const v: TVector2Single): Single; overload;
function VectorLen(const v: TVector2Double): Double; overload;
function VectorLen(const v: TVector3Single): Single; overload;
function VectorLen(const v: TVector3Double): Double; overload;
function VectorLen(const v: TVector3Byte): Single; overload;
function VectorLen(const v: TVector4Single): Single; overload;
function VectorLen(const v: TVector4Double): Double; overload;
{ @groupEnd }

{ Vector length squared.

  This is slightly faster than calculating actual vector length,
  as it avoids doing expensive Sqrt. In many cases, you can
  operate on such squared vector length, and thus you gain some speed.
  For example, to check if vector is longer than 10,
  check @code(VectorLenSqr(V) > 100) instead of @code(VectorLen(V) > 10).

  Also note that when you have a vector with discrete values
  (like TVector3Byte), VectorLenSqr returns a precide integer
  value, while VectorLen must return floating-point value. }
function VectorLenSqr(const v: TVector2Single): Single; overload;
function VectorLenSqr(const v: TVector2Double): Double; overload;
function VectorLenSqr(const v: TVector3Single): Single; overload;
function VectorLenSqr(const v: TVector3Double): Double; overload;
function VectorLenSqr(const v: TVector3Byte): Integer; overload;
function VectorLenSqr(const v: TVector4Single): Single; overload;
function VectorLenSqr(const v: TVector4Double): Double; overload;

{ Vector cross product.

  This is a vector orthogonal to both given vectors.
  Generally there are two such vectors, this function returns
  the one following right-hand rule. More precisely, V1, V2 and
  VectorProduct(V1, V2) are in the same relation as basic X, Y, Z
  axes. Reverse the order of arguments to get negated result.

  If you use this to calculate a normal vector of a triangle
  (P0, P1, P2): note that @code(VectorProduct(P1 - P0, P1 - P2))
  points out from CCW triangle side in right-handed coordinate system.

  When V1 and V2 are parallel (that is, when V1 = V2 multiplied by some scalar),
  and this includes the case when one of them is zero,
  then result is a zero vector.

  See http://en.wikipedia.org/wiki/Cross_product
  @groupBegin }
function VectorProduct(const V1, V2: TVector3Double): TVector3Double; overload;
function VectorProduct(const V1, V2: TVector3Single): TVector3Single; overload;
{ @groupEnd }

{ Dot product (aka scalar product) of two vectors.

  Overloaded versions that take as one argument 3-component vector and
  as the second argument 4-component vector: they simply behave like
  the missing 4th component would be equal 1.0. This is useful when
  V1 is a 3D point and V2 is something like plane equation.

  @groupBegin }
function VectorDotProduct(const V1, V2: TVector2Single): Single; overload;
function VectorDotProduct(const V1, V2: TVector2Double): Double; overload;

function VectorDotProduct(const V1, V2: TVector3Single): Single; overload;
function VectorDotProduct(const V1, V2: TVector3Double): Double; overload;

function VectorDotProduct(const V1, V2: TVector4Single): Single; overload;
function VectorDotProduct(const V1, V2: TVector4Double): Double; overload;

function VectorDotProduct(const v1: TVector3Single; const v2: TVector4Single): Single; overload;
function VectorDotProduct(const v1: TVector3Double; const v2: TVector4Double): Double; overload;
{ @groupEnd }

{ Multiply two vectors component-wise.
  That is, Result[I] := V1[I] * V2[I] for each I.

  @groupBegin }
function VectorMultiplyComponents(const V1, V2: TVector3Single): TVector3Single; overload;
function VectorMultiplyComponents(const V1, V2: TVector3Double): TVector3Double; overload;
procedure VectorMultiplyComponentsVar(var v1: TVector3Single; const v2: TVector3Single); overload;
procedure VectorMultiplyComponentsVar(var v1: TVector3Double; const v2: TVector3Double); overload;
{ @groupEnd }

{ Change each vector component into Power(component, Exp).
  @raises(EInvalidArgument When some component is < 0 and Exp <> 0.
    Version VectorPowerComponentsVar leaves the V in undefined state
    in case of such exception.) }
function VectorPowerComponents(const v: TVector3Single; const Exp: Single): TVector3Single; overload;
function VectorPowerComponents(const v: TVector3Double; const Exp: Double): TVector3Double; overload;
procedure VectorPowerComponentsVar(var v: TVector3Single; const Exp: Single); overload;
procedure VectorPowerComponentsVar(var v: TVector3Double; const Exp: Double); overload;

{ Cosinus of angle between two vectors.

  CosAngleBetweenNormals is a little faster, but must receive
  normalized (length 1) vectors. This avoids expensive Sqrt
  inside CosAngleBetweenVectors.

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function CosAngleBetweenVectors(const V1, V2: TVector3Single): Single; overload;
function CosAngleBetweenVectors(const V1, V2: TVector3Double): Double; overload;
function CosAngleBetweenNormals(const V1, V2: TVector3Single): Single; overload;
function CosAngleBetweenNormals(const V1, V2: TVector3Double): Double; overload;
{ @groupEnd }

{ Angle between two vectors, in radians.
  Returns always positive angle, between 0 and Pi.

  AngleRadBetweenNormals is a little faster, but must receive
  normalized (length 1) vectors. This avoids expensive Sqrt.
  See also CosAngleBetweenVectors and CosAngleBetweenNormals
  to avoid expensive ArcCos.

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function AngleRadBetweenVectors(const V1, V2: TVector3Single): Single; overload;
function AngleRadBetweenVectors(const V1, V2: TVector3Double): Double; overload;
function AngleRadBetweenNormals(const V1, V2: TVector3Single): Single; overload;
function AngleRadBetweenNormals(const V1, V2: TVector3Double): Double; overload;
{ @groupEnd }

{ Signed angle between two vectors, in radians.
  As opposed to AngleRadBetweenNormals, this returns a signed angle,
  between -Pi and Pi. This is guaranteed to be such angle that rotating
  V1 around vector cross product (V1 x V2) will produce V2.
  As you see, the order or arguments is important (just like it's important
  for vector cross).

  Overloaded versions with Cross argument assume the rotation is done around
  given Cross vector, which @italic(must) be a cross product or it's negation
  (in other words, it must be orthogonal to both vectors).

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function RotationAngleRadBetweenVectors(const V1, V2: TVector3Single): Single; overload;
function RotationAngleRadBetweenVectors(const V1, V2: TVector3Double): Double; overload;
function RotationAngleRadBetweenVectors(const V1, V2, Cross: TVector3Single): Single; overload;
function RotationAngleRadBetweenVectors(const V1, V2, Cross: TVector3Double): Double; overload;
{ @groupEnd }

{ Rotate point Point around the Axis by given Angle.
  Axis cannot be zero.

  Note that this is equivalent to constructing a rotation matrix
  and then using it, like

  @longCode(#
    M := RotationMatrixDeg(Angle, Axis);
    Result := MatrixMultPoint(M, Point);
  #)

  Except this will be a little faster. So rotations are done in the
  same direction as RotationMatrixDeg, and as OpenGL.
  @groupBegin }
function RotatePointAroundAxisDeg(Angle: Single; const Point: TVector3Single; const Axis: TVector3Single): TVector3Single; overload;
function RotatePointAroundAxisDeg(Angle: Double; const Point: TVector3Double; const Axis: TVector3Double): TVector3Double; overload;
function RotatePointAroundAxisRad(Angle: Single; const Point: TVector3Single; const Axis: TVector3Single): TVector3Single; overload;
function RotatePointAroundAxisRad(Angle: Double; const Point: TVector3Double; const Axis: TVector3Double): TVector3Double; overload;
{ @groupEnd }

{ Rotate point in 2D, in a counter-clockwise fashion.
  AngleRad is in radians. }
function RotatePoint2D(const Point: TVector2Single; const AngleRad: Single): TVector2Single;

{ Which coordinate (0, 1, 2, and eventually 3 for 4D versions) is the largest.
  When the vector components are equal, the first one "wins", for example
  if V[0] = V[1] (and are larger than other vector component) we return 0.
  MaxAbsVectorCoord compares the absolute value of components.
  @groupBegin }
function MaxVectorCoord(const v: TVector2Single): integer; overload;
function MaxVectorCoord(const v: TVector2Double): integer; overload;
function MaxVectorCoord(const v: TVector3Single): integer; overload;
function MaxVectorCoord(const v: TVector3Double): integer; overload;
function MaxVectorCoord(const v: TVector4Single): integer; overload;
function MaxVectorCoord(const v: TVector4Double): integer; overload;
function MaxAbsVectorCoord(const v: TVector2Single): integer; overload;
function MaxAbsVectorCoord(const v: TVector2Double): integer; overload;
function MaxAbsVectorCoord(const v: TVector3Single): integer; overload;
function MaxAbsVectorCoord(const v: TVector3Double): integer; overload;
{ @groupEnd }

function VectorAbs(const V: TVector2Single): TVector2Single;
function VectorAbs(const V: TVector2Double): TVector2Double;
function VectorAbs(const V: TVector3Single): TVector3Single;
function VectorAbs(const V: TVector3Double): TVector3Double;
function VectorAbs(const V: TVector4Single): TVector4Single;
function VectorAbs(const V: TVector4Double): TVector4Double;

function MinVectorCoord(const v: TVector3Single): integer; overload;
function MinVectorCoord(const v: TVector3Double): integer; overload;

procedure SortAbsVectorCoord(const v: TVector3Single; out Max, Middle, Min: Integer); overload;
procedure SortAbsVectorCoord(const v: TVector3Double; out Max, Middle, Min: Integer); overload;

{ Vector orthogonal to plane and pointing in the given direction.

  Given a plane equation (or just the first 3 components of this equation),
  we have vector orthogonal to the plane (just the first 3 components of plane
  equation). This returns either this vector, or it's negation.
  It chooses the one that points in the same 3D half-space as given Direction.

  When given Direction is paralell to Plane, returns original
  plane direction, not it's negation.

  This really simply returns the first 3 components of plane equation.
  possibly negated. So e.g. if the plane direction was normalized, result
  is normalized too.

  PlaneDirNotInDirection chooses the direction opposite to given Direction
  parameter. So it's like @code(PlaneDirInDirection(Plane, -Direction)).

  @groupBegin }
function PlaneDirInDirection(const Plane: TVector4Single; const Direction: TVector3Single): TVector3Single; overload;
function PlaneDirInDirection(const PlaneDir, Direction: TVector3Single): TVector3Single; overload;
function PlaneDirInDirection(const Plane: TVector4Double; const Direction: TVector3Double): TVector3Double; overload;
function PlaneDirInDirection(const PlaneDir, Direction: TVector3Double): TVector3Double; overload;
function PlaneDirNotInDirection(const Plane: TVector4Single; const Direction: TVector3Single): TVector3Single; overload;
function PlaneDirNotInDirection(const PlaneDir, Direction: TVector3Single): TVector3Single; overload;
function PlaneDirNotInDirection(const Plane: TVector4Double; const Direction: TVector3Double): TVector3Double; overload;
function PlaneDirNotInDirection(const PlaneDir, Direction: TVector3Double): TVector3Double; overload;
{ @groupEnd }

type
  EPlanesParallel = class(Exception);

{ Intersection of two 3D planes.
  @raises EPlanesParallel If planes are parallel.
  @groupBegin }
procedure TwoPlanesIntersectionLine(const Plane0, Plane1: TVector4Single;
  out Line0, LineVector: TVector3Single); overload;
procedure TwoPlanesIntersectionLine(const Plane0, Plane1: TVector4Double;
  out Line0, LineVector: TVector3Double); overload;
{ @groupEnd }

type
  ELinesParallel = class(Exception);

{ Intersection of two 2D lines.
  2D lines are expressed here as a vector of three values (A,B,C),
  such that Ax+By+C=0 is true for points on the line.
  @raises ELinesParallel if lines parallel
  @groupBegin }
function Lines2DIntersection(const Line0, Line1: TVector3Single): TVector2Single; overload;
function Lines2DIntersection(const Line0, Line1: TVector3Double): TVector2Double; overload;
{ @groupEnd }

{ Intersection of three 3D planes, results in a single 3D point.
  If the intersection is not a single 3D point, result is undefined,
  so don't try to use this.
  @groupBegin }
function ThreePlanesIntersectionPoint(
  const Plane0, Plane1, Plane2: TVector4Single): TVector3Single; overload;
function ThreePlanesIntersectionPoint(
  const Plane0, Plane1, Plane2: TVector4Double): TVector3Double; overload;
{ @groupEnd }

{ Move a plane by a specifed vector.
  The first three plane numbers (plane normal vector) don't change
  (so, in particular, if you used the plane to define the half-space,
  the half-space gets moved as it should).

  PlaneAntiMove work like PlaneMove, but they translate by negated Move
  So it's like PlaneAntiMove(Plane, V) := PlaneMove(Plane, -V),
  but (very slightly) faster.

  This works Ok with invalid planes (1st three components = 0),
  that is after the move the plane remains invalid (1st three components
  remain = 0).

  @groupBegin }
function PlaneMove(const Plane: TVector4Single;
  const Move: TVector3Single): TVector4Single; overload;
function PlaneMove(const Plane: TVector4Double;
  const Move: TVector3Double): TVector4Double; overload;

procedure PlaneMoveVar(var Plane: TVector4Single; const Move: TVector3Single); overload;
procedure PlaneMoveVar(var Plane: TVector4Double; const Move: TVector3Double); overload;

function PlaneAntiMove(const Plane: TVector4Single;
  const Move: TVector3Single): TVector4Single; overload;
function PlaneAntiMove(const Plane: TVector4Double;
  const Move: TVector3Double): TVector4Double; overload;
{ @groupEnd }

{ Check if both directions indicate the same side of given 3D plane.
  If one direction is parallel to the plane, also returns @true.
  You can specify only the first 3 components of plane equation (PlaneDir),
  since the 4th component would be ignored anyway.
  @groupBegin }
function VectorsSamePlaneDirections(const V1, V2: TVector3Single; const Plane: TVector4Single): boolean; overload;
function VectorsSamePlaneDirections(const V1, V2: TVector3Double; const Plane: TVector4Double): boolean; overload;
function VectorsSamePlaneDirections(const V1, V2: TVector3Single; const PlaneDir: TVector3Single): boolean; overload;
function VectorsSamePlaneDirections(const V1, V2: TVector3Double; const PlaneDir: TVector3Double): boolean; overload;
{ @groupEnd }

{ Check if both points are on the same side of given 3D plane.
  If one of the points is exactly on the plane, also returns @true.
  @groupBegin }
function PointsSamePlaneSides(const p1, p2: TVector3Single; const Plane: TVector4Single): boolean; overload;
function PointsSamePlaneSides(const p1, p2: TVector3Double; const Plane: TVector4Double): boolean; overload;
{ @groupEnd }

function PointsDistance(const V1, V2: TVector2Single): Single; overload;
function PointsDistance(const V1, V2: TVector2Double): Double; overload;
function PointsDistance(const V1, V2: TVector3Single): Single; overload;
function PointsDistance(const V1, V2: TVector3Double): Double; overload;
function PointsDistanceSqr(const V1, V2: TVector3Single): Single; overload;
function PointsDistanceSqr(const V1, V2: TVector3Double): Double; overload;
function PointsDistanceSqr(const V1, V2: TVector2Single): Single; overload;
function PointsDistanceSqr(const V1, V2: TVector2Double): Double; overload;

{ Distance between points projected on the 2D plane.
  Projection is done by rejecting IgnoreIndex coordinate (must be 0, 1 or 2).
  @groupBegin }
function PointsDistance2DSqr(const V1, V2: TVector3Single; const IgnoreIndex: Integer): Single; overload;
function PointsDistance2DSqr(const V1, V2: TVector3Double; const IgnoreIndex: Integer): Double; overload;
{ @groupEnd }

{ Compare two vectors, with epsilon to tolerate slightly different floats.
  Uses singleEqualityEpsilon, DoubleEqualityEpsilon just like FloatsEqual.

  Note that the case when EqualityEpsilon (or SingleEqualityEpsilon
  or DoubleEqualityEpsilon) is exactly 0 is optimized here,
  just like VectorsPerfectlyEqual.

  @seealso VectorsPerfectlyEqual

  @groupBegin }
function VectorsEqual(const V1, V2: TVector2Single): boolean; overload;
function VectorsEqual(const V1, V2: TVector2Double): boolean; overload;
function VectorsEqual(const V1, V2: TVector2Single; const EqualityEpsilon: Single): boolean; overload;
function VectorsEqual(const V1, V2: TVector2Double; const EqualityEpsilon: Double): boolean; overload;
function VectorsEqual(const V1, V2: TVector3Single): boolean; overload;
function VectorsEqual(const V1, V2: TVector3Double): boolean; overload;
function VectorsEqual(const V1, V2: TVector3Single; const EqualityEpsilon: Single): boolean; overload;
function VectorsEqual(const V1, V2: TVector3Double; const EqualityEpsilon: Double): boolean; overload;
function VectorsEqual(const V1, V2: TVector4Single): boolean; overload;
function VectorsEqual(const V1, V2: TVector4Double): boolean; overload;
function VectorsEqual(const V1, V2: TVector4Single; const EqualityEpsilon: Single): boolean; overload;
function VectorsEqual(const V1, V2: TVector4Double; const EqualityEpsilon: Double): boolean; overload;
{ @groupEnd }

{ Compare two vectors using perfect comparison, that is using the "=" operator
  to compare floats.
  @seealso VectorsEqual
  @groupBegin }
function VectorsPerfectlyEqual(const V1, V2: TVector2Single): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector2Double): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector3Single): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector3Double): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector4Single): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector4Double): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector2Byte  ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector3Byte  ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector4Byte  ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{ @groupEnd }

function MatricesEqual(const M1, M2: TMatrix3Single; const EqualityEpsilon: Single): boolean; overload;
function MatricesEqual(const M1, M2: TMatrix3Double; const EqualityEpsilon: Double): boolean; overload;
function MatricesEqual(const M1, M2: TMatrix4Single; const EqualityEpsilon: Single): boolean; overload;
function MatricesEqual(const M1, M2: TMatrix4Double; const EqualityEpsilon: Double): boolean; overload;

function MatricesPerfectlyEqual(const M1, M2: TMatrix3Single): boolean; overload;
function MatricesPerfectlyEqual(const M1, M2: TMatrix3Double): boolean; overload;
function MatricesPerfectlyEqual(const M1, M2: TMatrix4Single): boolean; overload;
function MatricesPerfectlyEqual(const M1, M2: TMatrix4Double): boolean; overload;

function VectorsPerp(const V1, V2: TVector3Single): boolean; overload;
function VectorsPerp(const V1, V2: TVector3Double): boolean; overload;

{ Are the two vectors parallel (one is a scaled version of another).
  In particular, if one of the vectors is zero, then this is @true.
  @groupBegin }
function VectorsParallel(const V1, V2: TVector3Single): boolean; overload;
function VectorsParallel(const V1, V2: TVector3Double): boolean; overload;
{ @groupEnd }

{ Adjust the V1 vector to force given angle between V1 and V2.
  Vector V1 will be adjusted, such that it has the same length
  and the 3D plane defined by V1, V2 and (0, 0, 0) is the same.

  When vectors are parallel (this includes the case when one of them is zero),
  we set V1 to ResultWhenParallel.

  We make it such that V1 rotated around axis VectorProduct(V1, V2) by given
  angle will result in V2. Note that this means that
  @code(MakeVectorsAngleRadOnTheirPlane(V1, V2, Angle, ...))
  results in the same (not reversed) relation between vectors as
  @code(MakeVectorsAngleRadOnTheirPlane(V2, V1, Angle, ...)).
  That's because you change the arguments order, but also VectorProduct
  sign changes.
  @groupBegin }
procedure MakeVectorsAngleRadOnTheirPlane(var v1: TVector3Single;
  const v2: TVector3Single; const AngleRad: Single;
  const ResultWhenParallel: TVector3Single); overload;
procedure MakeVectorsAngleRadOnTheirPlane(var v1: TVector3Double;
  const v2: TVector3Double; const AngleRad: Double;
  const ResultWhenParallel: TVector3Double); overload;
{ @groupEnd }

{ Adjust the V1 vector to force V1 and V2 to be orthogonal.
  When vectors are parallel, we set V1 to be AnyOrthogonalVector(V2). }
procedure MakeVectorsOrthoOnTheirPlane(var v1: TVector3Single;
  const v2: TVector3Single); overload;
procedure MakeVectorsOrthoOnTheirPlane(var v1: TVector3Double;
  const v2: TVector3Double); overload;

{ Return, deterministically, some vector orthogonal to V.
  When V is non-zero, then the result is non-zero.
  @groupBegin }
function AnyOrthogonalVector(const v: TVector2Single): TVector2Single; overload;
function AnyOrthogonalVector(const v: TVector2Double): TVector2Double; overload;
function AnyOrthogonalVector(const v: TVector3Single): TVector3Single; overload;
function AnyOrthogonalVector(const v: TVector3Double): TVector3Double; overload;
{ @groupEnd }

function IsLineParallelToPlane(const lineVector: TVector3Single; const plane: TVector4Single): boolean; overload;
function IsLineParallelToPlane(const lineVector: TVector3Double; const plane: TVector4Double): boolean; overload;

function IsLineParallelToSimplePlane(const lineVector: TVector3Single;
  const PlaneConstCoord: integer): boolean; overload;
function IsLineParallelToSimplePlane(const lineVector: TVector3Double;
  const PlaneConstCoord: integer): boolean; overload;

{ Assuming that Vector1 and Vector2 are parallel,
  check do they point in the same direction.

  This assumes that both vectors are non-zero.
  If one of the vectors is zero, the result is undefined --- false or true.
  (but the function will surely not raise some floating point error etc.) }
function AreParallelVectorsSameDirection(
  const Vector1, Vector2: TVector3Single): boolean; overload;
function AreParallelVectorsSameDirection(
  const Vector1, Vector2: TVector3Double): boolean; overload;

{ Orthogonally project a point on a plane, that is find a closest
  point to Point lying on a Plane.
  @groupBegin }
function PointOnPlaneClosestToPoint(const plane: TVector4Single; const point: TVector3Single): TVector3Single; overload;
function PointOnPlaneClosestToPoint(const plane: TVector4Double; const point: TVector3Double): TVector3Double; overload;
{ @groupEnd }

function PointToPlaneDistanceSqr(const Point: TVector3Single;
  const Plane: TVector4Single): Single; overload;
function PointToPlaneDistanceSqr(const Point: TVector3Double;
  const Plane: TVector4Double): Double; overload;

{ Distance from a point to a plane (with already normalized direction).

  Note: distance of the plane from origin point (0,0,0) may be simply
  obtained by Abs(Plane[3]) when Plane is Normalized.
  @groupBegin }
function PointToNormalizedPlaneDistance(const Point: TVector3Single;
  const Plane: TVector4Single): Single; overload;
function PointToNormalizedPlaneDistance(const Point: TVector3Double;
  const Plane: TVector4Double): Double; overload;
{ @groupEnd }

{ Distance from a point to a plane.

  Note that calculating this costs you one Sqrt
  (contrary to PointToPlaneDistanceSqr or
  PointToNormalizedPlaneDistance).

  @groupBegin }
function PointToPlaneDistance(const Point: TVector3Single;
  const Plane: TVector4Single): Single; overload;
function PointToPlaneDistance(const Point: TVector3Double;
  const Plane: TVector4Double): Double; overload;
{ @groupEnd }

function PointToSimplePlaneDistance(const point: TVector3Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single): Single; overload;
function PointToSimplePlaneDistance(const point: TVector3Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double): Double; overload;

function PointOnLineClosestToPoint(const line0, lineVector, point: TVector3Single): TVector3Single; overload;
function PointOnLineClosestToPoint(const line0, lineVector, point: TVector3Double): TVector3Double; overload;
function PointOnLineClosestToPoint(const line0, lineVector, point: TVector2Single): TVector2Single; overload;
function PointOnLineClosestToPoint(const line0, lineVector, point: TVector2Double): TVector2Double; overload;

function PointToLineDistanceSqr(const point, line0, lineVector: TVector3Single): Single; overload;
function PointToLineDistanceSqr(const point, line0, lineVector: TVector3Double): Double; overload;

{ Plane and line intersection.

  Returns @false and doesn't modify Intersection or T when
  the line is parallel to the plane (this includes the case when
  the line @italic(lies on a plane), so theoretically the whole
  line is an intersection).

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = Line0 + LineVector * T).
  @groupBegin }
function TryPlaneLineIntersection(out intersection: TVector3Single;
  const plane: TVector4Single; const line0, lineVector: TVector3Single): boolean; overload;
function TryPlaneLineIntersection(out intersection: TVector3Double;
  const plane: TVector4Double; const line0, lineVector: TVector3Double): boolean; overload;
function TryPlaneLineIntersection(out t: Single;
  const plane: TVector4Single; const line0, lineVector: TVector3Single): boolean; overload;
function TryPlaneLineIntersection(out t: Double;
  const plane: TVector4Double; const line0, lineVector: TVector3Double): boolean; overload;
{ @groupEnd }

{ Plane and ray intersection.

  Returns @false and doesn't modify Intersection or T when
  the ray is parallel to the plane (this includes the case when
  the ray @italic(lies on a plane). Also returns @false when the ray would
  have to point in the opposite direction to hit the plane.

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = RayOrigin + RayDirection * T).
  @groupBegin }
function TrySimplePlaneRayIntersection(out Intersection: TVector3Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean; overload;
function TrySimplePlaneRayIntersection(out Intersection: TVector3Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const RayOrigin, RayDirection: TVector3Double): boolean; overload;
function TrySimplePlaneRayIntersection(out Intersection: TVector3Single; out T: Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean; overload;
function TrySimplePlaneRayIntersection(out Intersection: TVector3Double; out T: Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const RayOrigin, RayDirection: TVector3Double): boolean; overload;
function TrySimplePlaneRayIntersection(out T: Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean; overload;
function TrySimplePlaneRayIntersection(out T: Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const RayOrigin, RayDirection: TVector3Double): boolean; overload;

function TryPlaneRayIntersection(out Intersection: TVector3Single;
  const Plane: TVector4Single; const RayOrigin, RayDirection: TVector3Single): boolean; overload;
function TryPlaneRayIntersection(out Intersection: TVector3Double;
  const Plane: TVector4Double; const RayOrigin, RayDirection: TVector3Double): boolean; overload;
function TryPlaneRayIntersection(out Intersection: TVector3Single; out T: Single;
  const Plane: TVector4Single; const RayOrigin, RayDirection: TVector3Single): boolean; overload;
function TryPlaneRayIntersection(out Intersection: TVector3Double; out T: Double;
  const Plane: TVector4Double; const RayOrigin, RayDirection: TVector3Double): boolean; overload;
{ @groupEnd }

{ Plane and line segment intersection.

  Returns @false and doesn't modify Intersection or T when
  the segment is parallel to the plane (this includes the case when
  the segment @italic(lies on a plane). Also returns @false when the segment
  would have to be longer to hit the plane.

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = RayOrigin + RayDirection * T).
  @groupBegin }
function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const Segment0, SegmentVector: TVector3Single): boolean; overload;
function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const Segment0, SegmentVector: TVector3Double): boolean; overload;
function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3Single; var T: Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const Segment0, SegmentVector: TVector3Single): boolean; overload;
function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3Double; var T: Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const Segment0, SegmentVector: TVector3Double): boolean; overload;
function TrySimplePlaneSegmentDirIntersection(var T: Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const Segment0, SegmentVector: TVector3Single): boolean; overload;
function TrySimplePlaneSegmentDirIntersection(var T: Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const Segment0, SegmentVector: TVector3Double): boolean; overload;

function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3Single): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const Pos1, Pos2: TVector3Double): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3Single; out T: Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3Single): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3Double; out T: Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const Pos1, Pos2: TVector3Double): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out T: Single;
  const PlaneConstCoord: integer; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3Single): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out T: Double;
  const PlaneConstCoord: integer; const PlaneConstValue: Double;
  const Pos1, Pos2: TVector3Double): boolean; overload;

function TryPlaneSegmentDirIntersection(out Intersection: TVector3Single;
  const Plane: TVector4Single; const Segment0, SegmentVector: TVector3Single): boolean; overload;
function TryPlaneSegmentDirIntersection(out Intersection: TVector3Double;
  const Plane: TVector4Double; const Segment0, SegmentVector: TVector3Double): boolean; overload;
function TryPlaneSegmentDirIntersection(out Intersection: TVector3Single; out T: Single;
  const Plane: TVector4Single; const Segment0, SegmentVector: TVector3Single): boolean; overload;
function TryPlaneSegmentDirIntersection(out Intersection: TVector3Double; out T: Double;
  const Plane: TVector4Double; const Segment0, SegmentVector: TVector3Double): boolean; overload;
{ @groupEnd }

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector2Single): boolean; overload;
function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector2Double): boolean; overload;
function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector3Single): boolean; overload;
function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector3Double): boolean; overload;

{ Line passing through two @italic(different) points.
  When the points are equal, undefined.
  @groupBegin }
function LineOfTwoDifferentPoints2d(const p1, p2: TVector2Single): TVector3Single; overload;
function LineOfTwoDifferentPoints2d(const p1, p2: TVector2Double): TVector3Double; overload;
{ @groupEnd }

function PointToSegmentDistanceSqr(const point, pos1, pos2: TVector3Single): Single; overload;
function PointToSegmentDistanceSqr(const point, pos1, pos2: TVector3Double): Double; overload;

{ Transform plane by a matrix.

  @raises(ETransformedResultInvalid Raised when matrix
  will transform some point to a direction, or direction to point,
  in homogeneous coordinates.)

  @groupBegin }
function PlaneTransform(const Plane: TVector4Single; const Matrix: TMatrix4Single): TVector4Single;
function PlaneTransform(const Plane: TVector4Double; const Matrix: TMatrix4Double): TVector4Double;
{ @groupEnd }

function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TVector3Single;
  const TunnelRadius: Single; const SphereCenter: TVector3Single;
  const SphereRadius: Single): boolean; overload;
function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TVector3Double;
  const TunnelRadius: Double; const SphereCenter: TVector3Double;
  const SphereRadius: Double): boolean; overload;

function IsSpheresCollision(const Sphere1Center: TVector3Single; const Sphere1Radius: Single;
  const Sphere2Center: TVector3Single; const Sphere2Radius: Single): boolean; overload;
function IsSpheresCollision(const Sphere1Center: TVector3Double; const Sphere1Radius: Double;
  const Sphere2Center: TVector3Double; const Sphere2Radius: Double): boolean; overload;

function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TVector3Single;
  const SphereRadius: Single): boolean; overload;
function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TVector3Double;
  const SphereRadius: Double): boolean; overload;

function TrySphereRayIntersection(out Intersection: TVector3Single;
  const SphereCenter: TVector3Single; const SphereRadius: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean; overload;
function TrySphereRayIntersection(out Intersection: TVector3Double;
  const SphereCenter: TVector3Double; const SphereRadius: Double;
  const RayOrigin, RayDirection: TVector3Double): boolean; overload;

{ Intersection between an (infinitely tall) cylinder and a ray.
  @groupBegin }
function TryCylinderRayIntersection(out Intersection: TVector3Single;
  const CylinderAxisOrigin, CylinderAxis: TVector3Single;
  const CylinderRadius: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean; overload;
function TryCylinderRayIntersection(out Intersection: TVector3Double;
  const CylinderAxisOrigin, CylinderAxis: TVector3Double;
  const CylinderRadius: Double;
  const RayOrigin, RayDirection: TVector3Double): boolean; overload;
{ @groupEnd }

{ Converting stuff to string ---------------------------------------------------

  Functions named ToNiceStr use FloatToNiceStr that in turn uses
  Format('%' + FloatNiceFormat, [f]). In effect, the floating-point value
  is by default displayed nicely for human, and moreover you can control
  the output by global FloatNiceFormat value.

  Also, functions named ToNiceStr sometimes add some decoration (like
  "[ ]" characters around matrix rows) to make the result look nice
  and readable.

  Functions that take a LineIndent parameter (may) output a multiline-string.
  In such case, the last line is @italic(never) terminated with newline
  character(s).

  Functions named ToRawStr output the precise floating-point value,
  using the ugly exponential (scientific) notation if needed.
  They are suitable for storing the floating-point value in a file,
  with a best precision possible.

  Also, functions named ToRawStr do not add any decoration when outputting
  vectors / matrices. They simply spit a sequence of floating-point values
  separated by spaces.
}

{ }
var
  FloatNiceFormat: string = 'f';

function FloatToNiceStr(f: Single): string; overload;
function FloatToNiceStr(f: Double): string; overload;
function VectorToNiceStr(const v: array of Byte): string; overload;
function VectorToNiceStr(const v: array of Single): string; overload;
function VectorToNiceStr(const v: array of Double): string; overload;
function VectorToNiceStr(const v: array of Integer): string; overload;
function MatrixToNiceStr(const v: TMatrix4Single; const LineIndent: string): string; overload;
function MatrixToNiceStr(const v: TMatrix4Double; const LineIndent: string): string; overload;

function FloatToRawStr(f: Single): string; overload;
function FloatToRawStr(f: Double): string; overload;
function VectorToRawStr(const v: array of Byte): string; overload;
function VectorToRawStr(const v: array of Integer): string; overload;
function VectorToRawStr(const v: array of Single): string; overload;
function VectorToRawStr(const v: array of Double): string; overload;
function MatrixToRawStr(const v: TMatrix4Single; const LineIndent: string): string; overload;
function MatrixToRawStr(const v: TMatrix4Double; const LineIndent: string): string; overload;

{ Matrix operations ---------------------------------------------------------- }

{ }
function MatrixAdd(const m1, m2: TMatrix3Single): TMatrix3Single; overload;
function MatrixAdd(const m1, m2: TMatrix4Single): TMatrix4Single; overload;
function MatrixAdd(const m1, m2: TMatrix3Double): TMatrix3Double; overload;
function MatrixAdd(const m1, m2: TMatrix4Double): TMatrix4Double; overload;

procedure MatrixAddVar(var m1: TMatrix3Single; const m2: TMatrix3Single); overload;
procedure MatrixAddVar(var m1: TMatrix4Single; const m2: TMatrix4Single); overload;
procedure MatrixAddVar(var m1: TMatrix3Double; const m2: TMatrix3Double); overload;
procedure MatrixAddVar(var m1: TMatrix4Double; const m2: TMatrix4Double); overload;

function MatrixSubtract(const m1, m2: TMatrix3Single): TMatrix3Single; overload;
function MatrixSubtract(const m1, m2: TMatrix4Single): TMatrix4Single; overload;
function MatrixSubtract(const m1, m2: TMatrix3Double): TMatrix3Double; overload;
function MatrixSubtract(const m1, m2: TMatrix4Double): TMatrix4Double; overload;

procedure MatrixSubtractVar(var m1: TMatrix3Single; const m2: TMatrix3Single); overload;
procedure MatrixSubtractVar(var m1: TMatrix4Single; const m2: TMatrix4Single); overload;
procedure MatrixSubtractVar(var m1: TMatrix3Double; const m2: TMatrix3Double); overload;
procedure MatrixSubtractVar(var m1: TMatrix4Double; const m2: TMatrix4Double); overload;

function MatrixNegate(const m1: TMatrix3Single): TMatrix3Single; overload;
function MatrixNegate(const m1: TMatrix4Single): TMatrix4Single; overload;
function MatrixNegate(const m1: TMatrix3Double): TMatrix3Double; overload;
function MatrixNegate(const m1: TMatrix4Double): TMatrix4Double; overload;

function MatrixMultScalar(const m: TMatrix3Single; const s: Single): TMatrix3Single; overload;
function MatrixMultScalar(const m: TMatrix4Single; const s: Single): TMatrix4Single; overload;
function MatrixMultScalar(const m: TMatrix3Double; const s: Double): TMatrix3Double; overload;
function MatrixMultScalar(const m: TMatrix4Double; const s: Double): TMatrix4Double; overload;

type
  ETransformedResultInvalid = class(EVectorInvalidOp);

{ Transform a 3D or 2D point with 4x4 matrix.

  This works by temporarily converting point to 4-component vector
  (4th component is 1). After multiplying matrix * vector we divide
  by 4th component. So this works Ok for all matrices,
  even with last row different than identity (0, 0, 0, 1).
  E.g. this works for projection matrices too.

  @raises(ETransformedResultInvalid This is raised when matrix
  will transform point to a direction (vector with 4th component
  equal zero). In this case we just cannot interpret the result as a point.)

  @groupBegin }
function MatrixMultPoint(const m: TMatrix4Single; const pt: TVector3Single): TVector3Single; overload;
function MatrixMultPoint(const m: TMatrix4Double; const pt: TVector3Double): TVector3Double; overload;
function MatrixMultPoint(const m: TMatrix4Single; const pt: TVector2Single): TVector2Single; overload;
function MatrixMultPoint(const m: TMatrix4Double; const pt: TVector2Double): TVector2Double; overload;
{ @groupEnd }

{ Transform a 3D or 2D direction with 4x4 matrix.

  This works by temporarily converting direction to 4-component vector
  (4th component is 0). After multiplying matrix * vector we check
  is the 4th component still 0 (eventually raising ETransformedResultInvalid).

  @raises(ETransformedResultInvalid This is raised when matrix
  will transform direction to a point (vector with 4th component
  nonzero). In this case we just cannot interpret the result as a direction.)

  @groupBegin }
function MatrixMultDirection(const m: TMatrix4Single; const Dir: TVector3Single): TVector3Single; overload;
function MatrixMultDirection(const m: TMatrix4Double; const Dir: TVector3Double): TVector3Double; overload;
function MatrixMultDirection(const m: TMatrix4Single; const Dir: TVector2Single): TVector2Single; overload;
function MatrixMultDirection(const m: TMatrix4Double; const Dir: TVector2Double): TVector2Double; overload;
{ @groupEnd }

function MatrixMultVector(const m: TMatrix2Single; const v: TVector2Single): TVector2Single; overload;
function MatrixMultVector(const m: TMatrix3Single; const v: TVector3Single): TVector3Single; overload;
function MatrixMultVector(const m: TMatrix4Single; const v: TVector4Single): TVector4Single; overload;
function MatrixMultVector(const m: TMatrix2Double; const v: TVector2Double): TVector2Double; overload;
function MatrixMultVector(const m: TMatrix3Double; const v: TVector3Double): TVector3Double; overload;
function MatrixMultVector(const m: TMatrix4Double; const v: TVector4Double): TVector4Double; overload;

function MatrixMult(const m1, m2: TMatrix2Single): TMatrix2Single; overload;
function MatrixMult(const m1, m2: TMatrix3Single): TMatrix3Single; overload;
function MatrixMult(const m1, m2: TMatrix4Single): TMatrix4Single; overload;
function MatrixMult(const m1, m2: TMatrix2Double): TMatrix2Double; overload;
function MatrixMult(const m1, m2: TMatrix3Double): TMatrix3Double; overload;
function MatrixMult(const m1, m2: TMatrix4Double): TMatrix4Double; overload;

function MatrixRow(const m: TMatrix2Single; const Row: Integer): TVector2Single; overload;
function MatrixRow(const m: TMatrix3Single; const Row: Integer): TVector3Single; overload;
function MatrixRow(const m: TMatrix4Single; const Row: Integer): TVector4Single; overload;
function MatrixRow(const m: TMatrix2Double; const Row: Integer): TVector2Double; overload;
function MatrixRow(const m: TMatrix3Double; const Row: Integer): TVector3Double; overload;
function MatrixRow(const m: TMatrix4Double; const Row: Integer): TVector4Double; overload;

function MatrixDeterminant(const M: TMatrix2Single): Single; overload;
function MatrixDeterminant(const M: TMatrix2Double): Double; overload;
function MatrixDeterminant(const M: TMatrix3Single): Single; overload;
function MatrixDeterminant(const M: TMatrix3Double): Double; overload;
function MatrixDeterminant(const M: TMatrix4Single): Single; overload;
function MatrixDeterminant(const M: TMatrix4Double): Double; overload;

{ Inverse the matrix.

  They do division by Determinant internally, so will raise exception
  from this float division if the matrix is not reversible.

  @groupBegin }
function MatrixInverse(const M: TMatrix2Single; const Determinant: Single): TMatrix2Single; overload;
function MatrixInverse(const M: TMatrix2Double; const Determinant: Double): TMatrix2Double; overload;
function MatrixInverse(const M: TMatrix3Single; const Determinant: Single): TMatrix3Single; overload;
function MatrixInverse(const M: TMatrix3Double; const Determinant: Double): TMatrix3Double; overload;
function MatrixInverse(const M: TMatrix4Single; const Determinant: Single): TMatrix4Single; overload;
function MatrixInverse(const M: TMatrix4Double; const Determinant: Double): TMatrix4Double; overload;
{ @groupEnd }

{ Transpose the matrix.
  @groupBegin }
procedure MatrixTransposeVar(var M: TMatrix3Single); overload;
procedure MatrixTransposeVar(var M: TMatrix3Double); overload;

function MatrixTranspose(const M: TMatrix3Single): TMatrix3Single; overload;
function MatrixTranspose(const M: TMatrix3Double): TMatrix3Double; overload;
function MatrixTranspose(const M: TMatrix4Single): TMatrix4Single; overload;
function MatrixTranspose(const M: TMatrix4Double): TMatrix4Double; overload;
{ @groupEnd }

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
function TryMatrixInverse(const M: TMatrix2Double; out MInverse: TMatrix2Double): boolean; overload;
function TryMatrixInverse(const M: TMatrix3Single; out MInverse: TMatrix3Single): boolean; overload;
function TryMatrixInverse(const M: TMatrix3Double; out MInverse: TMatrix3Double): boolean; overload;
function TryMatrixInverse(const M: TMatrix4Single; out MInverse: TMatrix4Single): boolean; overload;
function TryMatrixInverse(const M: TMatrix4Double; out MInverse: TMatrix4Double): boolean; overload;
{ @groupEnd }

{ Convert ModelView matrix to a Normal matrix, just like 3D graphic libraries do.
  See e.g. http://www.lighthouse3d.com/tutorials/glsl-tutorial/the-normal-matrix/
  for explanation why this is necessary, and how it's done. }
function ModelViewToNormalMatrix(const M: TMatrix4Single): TMatrix3Single;

{ Multiply vector by a transposition of the same vector.
  For 3d vectors, this results in a 3x3 matrix.
  To put this inside a 4x4 matrix,
  we fill the last row and column just like for an identity matrix.

  This is useful for calculating rotation matrix. }
function VectorMultTransposedSameVector(const v: TVector3Single): TMatrix4Single;

const
  { Special value that you can pass to FrustumProjMatrix and
    PerspectiveProjMatrix as ZFar, with intention to set far plane at infinity.

    If would be "cooler" to define ZFarInfinity as Math.Infinity,
    but operating on Math.Infinity requires unnecessary turning
    off of compiler checks. The point was only to have some special ZFar
    value, so 0 is as good as Infinity. }
  ZFarInfinity = 0.0;

{ Functions to create common 4x4 matrices used in 3D graphics.

  These functions generate the same matrices that are made by corresponding
  OpenGL (gl or glu) functions. So rotations will be generated in the same
  fashion, etc. For exact specification of what matrices they create see
  OpenGL specification for routines glTranslate, glScale, glRotate,
  glOrtho, glFrustum, gluPerspective.

  For frustum and pespective projection matrices, we have a special bonus
  here: you can pass as ZFar the special value ZFarInfinity.
  Then you get perspective projection matrix withour far clipping plane,
  which is very useful for z-fail shadow volumes technique.

  Functions named Matrices below generate both normal and inverted matrices.
  For example, function RotationMatrices returns two matrices that you
  could calculate separately by

  @longCode(#
          Matrix: = RotationMatrix( Angle, Axis);
  InvertedMatrix: = RotationMatrix(-Angle, Axis);
  #)

  This is useful sometimes, and generating them both at the same time
  allows for some speedup (for example, calling RotationMatrix twice will
  calculate sincos of Angle twice).

  Note that inverse of scaling matrix will not exist if the
  ScaleFactor has one of the components zero !
  Depending on InvertedMatrixIdentityIfNotExists, this will
  (if @false) raise division by zero exception or (if @true) cause
  the matrix to be set to identity.

  Note that rotation matrix (both normal and inverse) is always defined,
  for Axis = zero both normal and inverse matrices are set to identity.

  @groupBegin }
function TranslationMatrix(const X, Y, Z: Single): TMatrix4Single; overload;
function TranslationMatrix(const X, Y, Z: Double): TMatrix4Single; overload;
function TranslationMatrix(const Transl: TVector3Single): TMatrix4Single; overload;
function TranslationMatrix(const Transl: TVector3Double): TMatrix4Single; overload;

procedure TranslationMatrices(const X, Y, Z: Single; out Matrix, InvertedMatrix: TMatrix4Single); overload;
procedure TranslationMatrices(const X, Y, Z: Double; out Matrix, InvertedMatrix: TMatrix4Single); overload;
procedure TranslationMatrices(const Transl: TVector3Single; out Matrix, InvertedMatrix: TMatrix4Single); overload;
procedure TranslationMatrices(const Transl: TVector3Double; out Matrix, InvertedMatrix: TMatrix4Single); overload;

function ScalingMatrix(const ScaleFactor: TVector3Single): TMatrix4Single;

procedure ScalingMatrices(const ScaleFactor: TVector3Single;
  InvertedMatrixIdentityIfNotExists: boolean;
  out Matrix, InvertedMatrix: TMatrix4Single);

function RotationMatrixRad(const AngleRad: Single; const Axis: TVector3Single): TMatrix4Single; overload;
function RotationMatrixDeg(const AngleDeg: Single; const Axis: TVector3Single): TMatrix4Single; overload;
function RotationMatrixRad(const AngleRad: Single; const AxisX, AxisY, AxisZ: Single): TMatrix4Single; overload;
function RotationMatrixDeg(const AngleDeg: Single; const AxisX, AxisY, AxisZ: Single): TMatrix4Single; overload;

procedure RotationMatricesRad(const AngleRad: Single; const Axis: TVector3Single;
  out Matrix, InvertedMatrix: TMatrix4Single);
procedure RotationMatricesRad(const AxisAngle: TVector4Single;
  out Matrix, InvertedMatrix: TMatrix4Single);

function OrthoProjMatrix(const left, right, bottom, top, zNear, zFar: Single): TMatrix4Single;
function Ortho2dProjMatrix(const left, right, bottom, top: Single): TMatrix4Single;
function FrustumProjMatrix(const left, right, bottom, top, zNear, zFar: Single): TMatrix4Single;
function PerspectiveProjMatrixDeg(const fovyDeg, aspect, zNear, zFar: Single): TMatrix4Single;
function PerspectiveProjMatrixRad(const fovyRad, aspect, zNear, zFar: Single): TMatrix4Single;
{ @groupEnd }

{ Multiply matrix M by translation matrix.

  This is equivalent to M := MatrixMult(M, TranslationMatrix(Transl)),
  but it works much faster since TranslationMatrix is a very simple matrix
  and multiplication by it may be much optimized.

  An additional speedup comes from the fact that the result is placed
  back in M (so on places where M doesn't change (and there's a lot
  of them for multiplication with translation matrix) there's no useless
  copying).

  MultMatricesTranslation is analogous to calculating both
  TranslationMatrix(Transl) and it's inverse, and then

  @longCode(#
    M := MatrixMult(M, translation);
    MInvert := MatrixMult(inverted translation, MInvert);
  #)

  The idea is that if M represented some translation, and MInvert it's
  inverse, then after MultMatricesTranslation this will still hold.

  @groupBegin }
procedure MultMatrixTranslation(var M: TMatrix4Single; const Transl: TVector3Single); overload;
procedure MultMatrixTranslation(var M: TMatrix4Double; const Transl: TVector3Double); overload;
procedure MultMatricesTranslation(var M, MInvert: TMatrix4Single; const Transl: TVector3Single); overload;
procedure MultMatricesTranslation(var M, MInvert: TMatrix4Double; const Transl: TVector3Double); overload;
{ @groupEnd }

function MatrixDet4x4(const mat: TMatrix4Single): Single;
function MatrixDet3x3(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
function MatrixDet2x2(const a, b, c, d: Single): Single;

{ Transform coordinates to / from a coordinate system.
  Stuff multiplied by this matrix is supplied in other coordinate system.

  The "new" coordinate system (you specify it explicitly for
  TransformToCoordsMatrix) is the coordinate system in which your 3D stuff
  is defined. That is, when you supply the points (that will later be
  multiplied by TransformToCoordsMatrix) you think in the "new" coordinate
  system. The "old" coordinate system
  (you specify it explicitly for TransformFromCoordsMatrix)
  is the coordinate system of stuff @italic(after)
  it's multiplied by this matrix.

  This may get confusing, so to be more precise:

  @unorderedList(

    @item(
      TransformToCoordsMatrix says how the new coords system looks
      from the point of view of the old coords system.
      A stuff lying at (0, 0, 0) in new coord system will be seen
      at NewOrigin after the transformation (in the old coordinate system).
      Similarly, direction (0, 1, 0) will be seen as NewY after
      the transformation.)

    @item(
      TransformFromCoordsMatrix is the inverse: how the old system
      is seen from the new one. If before the transformation you are
      at OldOrigin, then after the transformation you are at (0, 0, 0).
      This is natural way to implement LookAtMatrix, LookDirMatrix.)
  )

  The lengths of directions (New or Old X, Y, Z vectors) are meaningful.
  These vectors correspond to unit vectors (1, 0, 0), (0, 1, 0) and (0, 0, 1)
  in the other coordinate system. Supplying here non-normalized vectors
  will result in scaling.

  You can use the "NoScale" versions to have the vectors automatically
  normalized, thus you waste a little time (on normalizing) but you
  avoid the scaling.

  Overloaded versions without OldOrigin / NewOrigin parameters
  work like the old/new origin is zero. IOW, the origin of the coordinate
  system doesn't change in this case.

  @groupBegin }
function TransformToCoordsMatrix(const
  NewX, NewY, NewZ: TVector3Single): TMatrix4Single; overload;
function TransformToCoordsMatrix(const
  NewX, NewY, NewZ: TVector3Double): TMatrix4Single; overload;
function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3Single): TMatrix4Single; overload;
function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3Double): TMatrix4Single; overload;
function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3Single): TMatrix4Single; overload;
function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3Double): TMatrix4Single; overload;

function TransformFromCoordsMatrix(const
  OldX, OldY, OldZ: TVector3Single): TMatrix4Single; overload;
function TransformFromCoordsMatrix(const
  OldX, OldY, OldZ: TVector3Double): TMatrix4Single; overload;
function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3Single): TMatrix4Single; overload;
function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3Double): TMatrix4Single; overload;
function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3Single): TMatrix4Single; overload;
function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3Double): TMatrix4Single; overload;
{ @groupEnd }

{ Calculate matrix to convert to given coordinate system
  (like TransformToCoordsMatrix) and it's inverse
  (like TransformFromCoordsMatrix).

  @groupBegin }
procedure TransformCoordsMatrices(const NewX, NewY, NewZ: TVector3Single;
  out ToCoords, FromCoords: TMatrix4Single); overload;
procedure TransformCoordsMatrices(const NewX, NewY, NewZ: TVector3Double;
  out ToCoords, FromCoords: TMatrix4Single); overload;
{ @groupEnd }

{ Transform vector into new coordinate space.

  Equivalent to @code(MatrixMultPoint(TransformToCoordsMatrix(ZeroVector3Single,
  NewX, NewY, NewZ))). So the origin of new coordinate system is at the same
  place. You should pass NewX, NewY, NewZ vectors normalized if you want to
  preserve vector length.
  @groupBegin }
function TransformToCoords(const V, NewX, NewY, NewZ: TVector3Single): TVector3Single;
function TransformToCoords(const V, NewX, NewY, NewZ: TVector3Double): TVector3Double;
{ @groupEnd }

{ Camera matrix to look at the specified point (or along the specified direction).
  Work according to right-handed coordinate system.

  When applied to the scene, they transform it, such that a camera standing
  at (0, 0, 0) (with dir (0, 0, -1) and up vector (0, 1, 0)),
  was seeing the same view as if it was standing at Eye
  (with given Dir and Up vectors).

  For LookAtMatrix, looking direction is implicitly given as @code(Center - Eye).
  Just like gluLookAt.

  @unorderedList(
    @item(For the overloaded LookDirMatrix version with Side parameter,
      we assume that Dir, Side and Up are already normalized
      and orthogonal to each other.)

    @item(For the overloaded version without the Side parameter,
      Dir and Up do not have to normalized.
      We'll normalize them if needed, so their lengths do not affect the result
      (just as the distance between Center and Eye points for LookAtMatrix).

      Also, Dir and Up do not have to be perfectly orthogonal
      (we will eventually adjust Up internally to make it orthogonal to Up).

      You still must make sure that Dir and Up are not parallel.)
  )

  @groupBegin }
function LookAtMatrix(const Eye, Center, Up: TVector3Single): TMatrix4Single; overload;
function LookAtMatrix(const Eye, Center, Up: TVector3Double): TMatrix4Single; overload;
function LookDirMatrix(const Eye, Dir, Up: TVector3Single): TMatrix4Single; overload;
function LookDirMatrix(const Eye, Dir, Up: TVector3Double): TMatrix4Single; overload;
function LookDirMatrix(const Eye, Dir, Side, Up: TVector3Single): TMatrix4Single; overload;
function LookDirMatrix(const Eye, Dir, Side, Up: TVector3Double): TMatrix4Single; overload;
{ @groupEnd }

{ Calculate LookDirMatrix (or it's inverse), fast.

  Has some assumptions that make it run fast:
  @unorderedList(
    @item(It assumes camera position is zero.)
    @item(It assumes that Dir and Up are already normalized and orthogonal.)
  )

  @groupBegin
}
function FastLookDirMatrix(const Direction, Up: TVector3Single): TMatrix4Single;
function FastLookDirMatrix(const Direction, Up: TVector3Double): TMatrix4Single;
function InverseFastLookDirMatrix(const Direction, Up: TVector3Single): TMatrix4Single;
function InverseFastLookDirMatrix(const Direction, Up: TVector3Double): TMatrix4Single;
{ @groupEnd }

{$I castlevectors_operators.inc}

{$undef read_interface}

implementation

uses Math, CastleStringUtils, CastleColors;

{$define read_implementation}

{$I castlevectors_operators.inc}

{ Separated from PointsDistance2DSqr, to not slowdown it by implicit
  try/finally section because we use string. }
procedure PointsDistance2DSqr_InvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for PointsDistance2DSqr');
end;

{ include castlevectors_dualimplementation.inc ---------------------------------- }

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
{$I castlevectors_dualimplementation.inc}

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
{$I castlevectors_dualimplementation.inc}

{ TVector3SingleList ----------------------------------------------------- }

procedure TVector3SingleList.AssignNegated(Source: TVector3SingleList);
begin
  Assign(Source);
  Negate;
end;

procedure TVector3SingleList.Negate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    VectorNegateVar(L[I]);
end;

procedure TVector3SingleList.Normalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    NormalizeVar(L[I]);
end;

procedure TVector3SingleList.MultiplyComponents(const V: TVector3Single);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    VectorMultiplyComponentsVar(L[I], V);
end;

procedure TVector3SingleList.AssignLerp(const Fraction: Single;
  V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

procedure TVector3SingleList.AssignLerpNormalize(const Fraction: Single;
  V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
  begin
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
    NormalizeVar(L[I]);
  end;
end;

procedure TVector3SingleList.AssignLerpRgbInHsv(const Fraction: Single;
  V1, V2: TVector3SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := LerpRgbInHsv(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

function TVector3SingleList.ToVector4Single(const W: Single): TVector4SingleList;
var
  I: Integer;
begin
  Result := TVector4SingleList.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.L[I] := Vector4Single(L[I], W);
end;

function TVector3SingleList.MergeCloseVertexes(MergeDistance: Single): Cardinal;
var
  V1, V2: PVector3Single;
  I, J: Integer;
begin
  MergeDistance := Sqr(MergeDistance);
  Result := 0;

  V1 := PVector3Single(List);
  for I := 0 to Count - 1 do
  begin
    { Find vertexes closer to L[I], and merge them.

      Note that this is not optimal: we could avoid processing
      here L[I] that were detected previously (and possibly merged)
      as being equal to some previous items. But in practice this seems
      not needed, as there are not many merged vertices in typical situation,
      so time saving would be minimal (and small temporary memory cost
      introduced). }

    V2 := Ptr(I + 1);
    for J := I + 1 to Count - 1 do
    begin
      if PointsDistanceSqr(V1^, V2^) < MergeDistance then
        { We do the VectorsPerfectlyEqual comparison only to get nice Result.
          But this *is* an important value for the user, so it's worth it. }
        if not VectorsPerfectlyEqual(V1^, V2^) then
        begin
          V2^ := V1^;
          Inc(Result);
        end;
      Inc(V2);
    end;

    Inc(V1);
  end;
end;

procedure TVector3SingleList.AddList(Source: TVector3SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector3Single) * Source.Count);
end;

procedure TVector3SingleList.AddListRange(Source: TVector3SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector3Single) * AddCount);
end;

procedure TVector3SingleList.AddArray(const A: array of TVector3Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector3Single) * (High(A) + 1));
end;

procedure TVector3SingleList.AssignArray(const A: array of TVector3Single);
begin
  Clear;
  AddArray(A);
end;

{ TVector2SingleList ----------------------------------------------------- }

function TVector2SingleList.MinMax(out Min, Max: TVector2Single): boolean;
var
  I: Integer;
begin
  Result := Count > 0;
  if Result then
  begin
    Min := L[0];
    Max := L[0];
    for I := 1 to Count - 1 do
    begin
      if L[I][0] < Min[0] then Min[0] := L[I][0] else
      if L[I][0] > Max[0] then Max[0] := L[I][0];

      if L[I][1] < Min[1] then Min[1] := L[I][1] else
      if L[I][1] > Max[1] then Max[1] := L[I][1];
    end;
  end;
end;

procedure TVector2SingleList.AssignLerp(const Fraction: Single;
  V1, V2: TVector2SingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    L[I] := Lerp(Fraction, V1.L[Index1 + I], V2.L[Index2 + I]);
end;

procedure TVector2SingleList.AddList(Source: TVector2SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector2Single) * Source.Count);
end;

procedure TVector2SingleList.AddListRange(Source: TVector2SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector2Single) * AddCount);
end;

procedure TVector2SingleList.AddArray(const A: array of TVector2Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector2Single) * (High(A) + 1));
end;

procedure TVector2SingleList.AssignArray(const A: array of TVector2Single);
begin
  Clear;
  AddArray(A);
end;

{ TVector4SingleList ----------------------------------------------------- }

procedure TVector4SingleList.AddList(Source: TVector4SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector4Single) * Source.Count);
end;

procedure TVector4SingleList.AddListRange(Source: TVector4SingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(TVector4Single) * AddCount);
end;

procedure TVector4SingleList.AddArray(const A: array of TVector4Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector4Single) * (High(A) + 1));
end;

procedure TVector4SingleList.AssignArray(const A: array of TVector4Single);
begin
  Clear;
  AddArray(A);
end;

{ TVector2DoubleList ----------------------------------------------------- }

function TVector2DoubleList.ToVector2Single: TVector2SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector2SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 2 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector2DoubleList.AddList(Source: TVector2DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector2Double) * Source.Count);
end;

procedure TVector2DoubleList.AddArray(const A: array of TVector2Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector2Double) * (High(A) + 1));
end;

{ TVector3DoubleList ----------------------------------------------------- }

function TVector3DoubleList.ToVector3Single: TVector3SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector3SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector3DoubleList.AddList(Source: TVector3DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector3Double) * Source.Count);
end;

procedure TVector3DoubleList.AddArray(const A: array of TVector3Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector3Double) * (High(A) + 1));
end;

{ TVector4DoubleList ----------------------------------------------------- }

function TVector4DoubleList.ToVector4Single: TVector4SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TVector4SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TVector4DoubleList.AddList(Source: TVector4DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector4Double) * Source.Count);
end;

procedure TVector4DoubleList.AddArray(const A: array of TVector4Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TVector4Double) * (High(A) + 1));
end;

{ TVector2IntegerList ---------------------------------------------------- }

procedure TVector2IntegerList.Reverse;
var
  I: Integer;
begin
  { Need to specially check for Count = 0 case, since (0-1) div 2 = -1 div 2 = 0
    which means that loop would try invalid Exchange(0, -1). }
  if Count = 0 then Exit;
  for I := 0 to (Count - 1) div 2 do
    Exchange(I, Count - 1 - I);
end;

procedure TVector2IntegerList.AddList(Source: TVector2IntegerList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TVector2Integer) * Source.Count);
end;

{ TMatrix3SingleList ----------------------------------------------------- }

procedure TMatrix3SingleList.AddList(Source: TMatrix3SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix3Single) * Source.Count);
end;

procedure TMatrix3SingleList.AddArray(const A: array of TMatrix3Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix3Single) * (High(A) + 1));
end;

{ TMatrix4SingleList ----------------------------------------------------- }

procedure TMatrix4SingleList.AddList(Source: TMatrix4SingleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix4Single) * Source.Count);
end;

procedure TMatrix4SingleList.AddArray(const A: array of TMatrix4Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix4Single) * (High(A) + 1));
end;

{ TMatrix3DoubleList ----------------------------------------------------- }

function TMatrix3DoubleList.ToMatrix3Single: TMatrix3SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TMatrix3SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 3 * 3 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix3DoubleList.AddList(Source: TMatrix3DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix3Double) * Source.Count);
end;

procedure TMatrix3DoubleList.AddArray(const A: array of TMatrix3Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix3Double) * (High(A) + 1));
end;

{ TMatrix4DoubleList ----------------------------------------------------- }

function TMatrix4DoubleList.ToMatrix4Single: TMatrix4SingleList;
var
  I: Integer;
  Source: PDouble;
  Dest: PSingle;
begin
  Result := TMatrix4SingleList.Create;
  Result.Count := Count;
  Source := PDouble(List);
  Dest := PSingle(Result.List);
  for I := 0 to Count * 4 * 4 - 1 do
  begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure TMatrix4DoubleList.AddList(Source: TMatrix4DoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(TMatrix4Double) * Source.Count);
end;

procedure TMatrix4DoubleList.AddArray(const A: array of TMatrix4Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(TMatrix4Double) * (High(A) + 1));
end;

{ FloatsEqual ------------------------------------------------------------- }

function FloatsEqual(const f1, f2: Single): boolean;
begin
  if SingleEqualityEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < SingleEqualityEpsilon;
end;

function FloatsEqual(const f1, f2: Double): boolean;
begin
  if DoubleEqualityEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < DoubleEqualityEpsilon;
end;

{$ifndef EXTENDED_EQUALS_DOUBLE}
function FloatsEqual(const f1, f2: Extended): boolean;
begin
  if ExtendedEqualityEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < ExtendedEqualityEpsilon
end;
{$endif}

function FloatsEqual(const f1, f2, EqEpsilon: Single): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < EqEpsilon
end;

function FloatsEqual(const f1, f2, EqEpsilon: Double): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < EqEpsilon
end;

{$ifndef EXTENDED_EQUALS_DOUBLE}
function FloatsEqual(const f1, f2, EqEpsilon: Extended): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < EqEpsilon
end;
{$endif}

function Zero(const f1: Single  ): boolean;
begin
  if SingleEqualityEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1)<  SingleEqualityEpsilon
end;

function Zero(const f1: Double  ): boolean;
begin
  if DoubleEqualityEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1)<  DoubleEqualityEpsilon
end;

{$ifndef EXTENDED_EQUALS_DOUBLE}
function Zero(const f1: Extended): boolean;
begin
  if ExtendedEqualityEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1) < ExtendedEqualityEpsilon
end;
{$endif}

function Zero(const f1, EqEpsilon: Single  ): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = 0 else
    result := Abs(f1) < EqEpsilon
end;

function Zero(const f1, EqEpsilon: Double  ): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1) < EqEpsilon
end;

{$ifndef EXTENDED_EQUALS_DOUBLE}
function Zero(const f1, EqEpsilon: Extended): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1) < EqEpsilon
end;
{$endif}

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

{ some math on vectors ------------------------------------------------------- }

function Approximate3DScale(const X, Y, Z: Single): Single;
begin
  if (X * Y < 0) or
     (X * Z < 0) or
     (Y * X < 0) then
    { If some values have opposite signs, it's better to make
      an average of absolute values. This way a scale like (-1, 1, 1),
      that flips X but preserves size, results in 1 (not 1/3).
      Bug reproduce: escape with mirrored map parts. }
    Result := (Abs(X) + Abs(Y) + Abs(Z)) / 3 else
    Result := (    X  +     Y  +     Z ) / 3;
end;

function Approximate3DScale(const V: TVector3Single): Single;
begin
  Result := Approximate3DScale(V[0], V[1], V[2]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2Byte): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3Byte): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4Byte): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]) and
            (V1[3] = V2[3]);
end;

function Lerp(const A: Single; const V1, V2: TVector2Byte): TVector2Byte;
begin
  Result[0] := Clamped(Round(V1[0] + A * (V2[0] - V1[0])), 0, High(Byte));
  Result[1] := Clamped(Round(V1[1] + A * (V2[1] - V1[1])), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector3Byte): TVector3Byte;
begin
  Result[0] := Clamped(Round(V1[0] + A * (V2[0] - V1[0])), 0, High(Byte));
  Result[1] := Clamped(Round(V1[1] + A * (V2[1] - V1[1])), 0, High(Byte));
  Result[2] := Clamped(Round(V1[2] + A * (V2[2] - V1[2])), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector4Byte): TVector4Byte;
begin
  Result[0] := Clamped(Round(V1[0] + A * (V2[0] - V1[0])), 0, High(Byte));
  Result[1] := Clamped(Round(V1[1] + A * (V2[1] - V1[1])), 0, High(Byte));
  Result[2] := Clamped(Round(V1[2] + A * (V2[2] - V1[2])), 0, High(Byte));
  Result[3] := Clamped(Round(V1[3] + A * (V2[3] - V1[3])), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2Single;
begin
 result[0] := V1[0] + a*(V2[0]-V1[0]);
 result[1] := V1[1] + a*(V2[1]-V1[1]);
end;

function Vector_Init_Lerp(const A: Single; const V1, V2: TVector3_Single): TVector3_Single;
begin
  Result.Data[0] := V1.Data[0] + A * (V2.Data[0] - V1.Data[0]);
  Result.Data[1] := V1.Data[1] + A * (V2.Data[1] - V1.Data[1]);
  Result.Data[2] := V1.Data[2] + A * (V2.Data[2] - V1.Data[2]);
end;

function Vector_Init_Lerp(const A: Single; const V1, V2: TVector4_Single): TVector4_Single;
begin
  Result.Data[0] := V1.Data[0] + A * (V2.Data[0] - V1.Data[0]);
  Result.Data[1] := V1.Data[1] + A * (V2.Data[1] - V1.Data[1]);
  Result.Data[2] := V1.Data[2] + A * (V2.Data[2] - V1.Data[2]);
  Result.Data[3] := V1.Data[3] + A * (V2.Data[3] - V1.Data[3]);
end;

function Vector_Init_Lerp(const A: Double; const V1, V2: TVector3_Double): TVector3_Double;
begin
  Result.Data[0] := V1.Data[0] + A * (V2.Data[0] - V1.Data[0]);
  Result.Data[1] := V1.Data[1] + A * (V2.Data[1] - V1.Data[1]);
  Result.Data[2] := V1.Data[2] + A * (V2.Data[2] - V1.Data[2]);
end;

function Vector_Init_Lerp(const A: Double; const V1, V2: TVector4_Double): TVector4_Double;
begin
  Result.Data[0] := V1.Data[0] + A * (V2.Data[0] - V1.Data[0]);
  Result.Data[1] := V1.Data[1] + A * (V2.Data[1] - V1.Data[1]);
  Result.Data[2] := V1.Data[2] + A * (V2.Data[2] - V1.Data[2]);
  Result.Data[3] := V1.Data[3] + A * (V2.Data[3] - V1.Data[3]);
end;

procedure NormalizeVar3Singlev(vv: PVector3Single);
var
  Len: Single;
begin
  Len := Sqrt(
    Sqr(vv^[0]) +
    Sqr(vv^[1]) +
    Sqr(vv^[2]));
  if Len = 0 then exit;
  vv^[0] := vv^[0] / Len;
  vv^[1] := vv^[1] / Len;
  vv^[2] := vv^[2] / Len;
end;

procedure NormalizeVar3Bytev(vv: PVector3Byte);
var
  Len: integer;
begin
  Len := Round( Sqrt(
    Sqr(Integer(vv^[0])) +
    Sqr(Integer(vv^[1])) +
    Sqr(Integer(vv^[2]))) );
  if Len = 0 then exit;
  vv^[0] := vv^[0] div Len;
  vv^[1] := vv^[1] div Len;
  vv^[2] := vv^[2] div Len;
end;

function ZeroVector(const v: TVector4Cardinal): boolean;
begin
  result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function ZeroVector(const v: TVector4Integer): boolean;
begin
  result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function VectorLen(const v: TVector3Byte): Single;
begin
  result := Sqrt(VectorLenSqr(v))
end;

function VectorLenSqr(const v: TVector3Byte): Integer;
begin
  result := Sqr(Integer(v[0])) + Sqr(Integer(v[1])) + Sqr(Integer(v[2]));
end;

function VectorToNiceStr(const v: array of Byte): string; overload;
var
  i: Integer;
begin
  result := '(';
  for i := 0 to High(v)-1 do result := result +IntToStr(v[i]) +', ';
  if High(v) >= 0 then result := result +IntToStr(v[High(v)]) +')';
end;

function VectorToNiceStr(const v: array of Integer): string; overload;
var
  i: Integer;
begin
  result := '(';
  for i := 0 to High(v)-1 do result := result +IntToStr(v[i]) +', ';
  if High(v) >= 0 then result := result +IntToStr(v[High(v)]) +')';
end;

function VectorToRawStr(const v: array of Byte): string;
var i: integer;
begin
  result := '';
  for i := 0 to High(v)-1 do result += IntToStr(v[i]) +' ';
  if High(v) >= 0 then result += IntToStr(v[High(v)]);
end;

function VectorToRawStr(const v: array of Integer): string;
var i: integer;
begin
  result := '';
  for i := 0 to High(v)-1 do result += IntToStr(v[i]) +' ';
  if High(v) >= 0 then result += IntToStr(v[High(v)]);
end;

{ math with matrices ---------------------------------------------------------- }

function VectorMultTransposedSameVector(const v: TVector3Single): TMatrix4Single;
begin
  (* Naive version:

  for i := 0 to 2 do { i = column, j = row }
    for j := 0 to 2 do
      result[i, j] := v[i]*v[j];

  Expanded and optimized version below. *)

  result[0, 0] := sqr(v[0]);
  result[1, 1] := sqr(v[1]);
  result[2, 2] := sqr(v[2]);

  result[0, 1] := v[0]*v[1]; result[1, 0] := result[0, 1];
  result[0, 2] := v[0]*v[2]; result[2, 0] := result[0, 2];
  result[1, 2] := v[1]*v[2]; result[2, 1] := result[1, 2];

  { Fill the last row and column like an identity matrix }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;

  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;

  Result[3, 3] := 1;
end;

function ScalingMatrix(const ScaleFactor: TVector3Single): TMatrix4Single;
begin
  result := IdentityMatrix4Single;
  result[0, 0] := ScaleFactor[0];
  result[1, 1] := ScaleFactor[1];
  result[2, 2] := ScaleFactor[2];
end;

procedure ScalingMatrices(const ScaleFactor: TVector3Single;
  InvertedMatrixIdentityIfNotExists: boolean;
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  Matrix := IdentityMatrix4Single;
  Matrix[0, 0] := ScaleFactor[0];
  Matrix[1, 1] := ScaleFactor[1];
  Matrix[2, 2] := ScaleFactor[2];

  InvertedMatrix := IdentityMatrix4Single;
  if not
    (InvertedMatrixIdentityIfNotExists and
      ( Zero(ScaleFactor[0]) or
        Zero(ScaleFactor[1]) or
        Zero(ScaleFactor[2]) )) then
  begin
    InvertedMatrix[0, 0] := 1 / ScaleFactor[0];
    InvertedMatrix[1, 1] := 1 / ScaleFactor[1];
    InvertedMatrix[2, 2] := 1 / ScaleFactor[2];
  end;
end;

function RotatePoint2D(const Point: TVector2Single; const AngleRad: Single): TVector2Single;
var
  AngleSin, AngleCos: Float;
  S, C: Single;
begin
  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to Single once }
  S := AngleSin;
  C := AngleCos;
  Result[0] := Point[0] * C - Point[1] * S;
  Result[1] := Point[0] * S + Point[1] * C;
end;

function RotationMatrixRad(const AngleRad: Single;
  const Axis: TVector3Single): TMatrix4Single;
var
  NormAxis: TVector3Single;
  AngleSin, AngleCos: Float;
  S, C: Single;
begin
  NormAxis := Normalized(Axis);

  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to Single once }
  S := AngleSin;
  C := AngleCos;

  Result := VectorMultTransposedSameVector(NormAxis);

  { We do not touch the last column and row of Result in the following code,
    treating Result like a 3x3 matrix. The last column and row are already Ok. }

  { Expanded Result := Result + (IdentityMatrix3Single - Result) * AngleCos; }
  Result[0, 0] += (1 - Result[0, 0]) * C;
  Result[1, 0] +=    - Result[1, 0]  * C;
  Result[2, 0] +=    - Result[2, 0]  * C;

  Result[0, 1] +=    - Result[0, 1]  * C;
  Result[1, 1] += (1 - Result[1, 1]) * C;
  Result[2, 1] +=    - Result[2, 1]  * C;

  Result[0, 2] +=    - Result[0, 2]  * C;
  Result[1, 2] +=    - Result[1, 2]  * C;
  Result[2, 2] += (1 - Result[2, 2]) * C;

  NormAxis[0] *= S;
  NormAxis[1] *= S;
  NormAxis[2] *= S;

  { Add M3 (from OpenGL matrix equations) }
  Result[1, 0] += -NormAxis[2];
  Result[2, 0] +=  NormAxis[1];

  Result[0, 1] +=  NormAxis[2];
  Result[2, 1] += -NormAxis[0];

  Result[0, 2] += -NormAxis[1];
  Result[1, 2] +=  NormAxis[0];
end;

procedure RotationMatricesRad(const AxisAngle: TVector4Single;
  out Matrix, InvertedMatrix: TMatrix4Single);
var
  Axis: TVector3Single absolute AxisAngle;
begin
  RotationMatricesRad(AxisAngle[3], Axis, Matrix, InvertedMatrix);
end;

procedure RotationMatricesRad(const AngleRad: Single;
  const Axis: TVector3Single;
  out Matrix, InvertedMatrix: TMatrix4Single);
var
  NormAxis: TVector3Single;
  V: Single;
  AngleSin, AngleCos: Float;
  S, C: Single;
begin
  NormAxis := Normalized(Axis);

  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to Single once }
  S := AngleSin;
  C := AngleCos;

  Matrix := VectorMultTransposedSameVector(NormAxis);

  { We do not touch the last column and row of Matrix in the following code,
    treating Matrix like a 3x3 matrix. The last column and row are already Ok. }

  { Expanded Matrix := Matrix + (IdentityMatrix3Single - Matrix) * AngleCos; }
  Matrix[0, 0] += (1 - Matrix[0, 0]) * C;
  Matrix[1, 0] +=    - Matrix[1, 0]  * C;
  Matrix[2, 0] +=    - Matrix[2, 0]  * C;

  Matrix[0, 1] +=    - Matrix[0, 1]  * C;
  Matrix[1, 1] += (1 - Matrix[1, 1]) * C;
  Matrix[2, 1] +=    - Matrix[2, 1]  * C;

  Matrix[0, 2] +=    - Matrix[0, 2]  * C;
  Matrix[1, 2] +=    - Matrix[1, 2]  * C;
  Matrix[2, 2] += (1 - Matrix[2, 2]) * C;

  { Up to this point, calculated Matrix is also good for InvertedMatrix }
  InvertedMatrix := Matrix;

  NormAxis[0] *= S;
  NormAxis[1] *= S;
  NormAxis[2] *= S;

  { Now add M3 to Matrix, and subtract M3 from InvertedMatrix.
    That's because for the inverted rotation, AngleSin is negated,
    so the M3 should be subtracted. }
  V := -NormAxis[2]; Matrix[1, 0] += V; InvertedMatrix[1, 0] -= V;
  V :=  NormAxis[1]; Matrix[2, 0] += V; InvertedMatrix[2, 0] -= V;

  V :=  NormAxis[2]; Matrix[0, 1] += V; InvertedMatrix[0, 1] -= V;
  V := -NormAxis[0]; Matrix[2, 1] += V; InvertedMatrix[2, 1] -= V;

  V := -NormAxis[1]; Matrix[0, 2] += V; InvertedMatrix[0, 2] -= V;
  V :=  NormAxis[0]; Matrix[1, 2] += V; InvertedMatrix[1, 2] -= V;
end;

function RotationMatrixDeg(const AngleDeg: Single; const Axis: TVector3Single): TMatrix4Single;
begin
  result := RotationMatrixRad(DegToRad(AngleDeg), Axis);
end;

function RotationMatrixDeg(const AngleDeg: Single;
  const AxisX, AxisY, AxisZ: Single): TMatrix4Single;
begin
  result := RotationMatrixRad(DegToRad(AngleDeg), Vector3Single(AxisX, AxisY, AxisZ));
end;

function RotationMatrixRad(const AngleRad: Single;
  const AxisX, AxisY, AxisZ: Single): TMatrix4Single;
begin
  result := RotationMatrixRad(AngleRad, Vector3Single(AxisX, AxisY, AxisZ));
end;

function OrthoProjMatrix(const Left, Right, Bottom, Top, ZNear, ZFar: Single): TMatrix4Single;
var
  Width, Height, Depth: Single;
begin
  Width := Right - Left;
  Height := Top - Bottom;
  Depth := ZFar - ZNear;

  result := ZeroMatrix4Single;
  result[0, 0] := 2 / Width;
  result[1, 1] := 2 / Height;
  result[2, 2] := - 2 / Depth; { tutaj - bo nasze Z-y sa ujemne w glab ekranu }
  result[3, 0] := - (Right + Left) / Width;
  result[3, 1] := - (Top + Bottom) / Height;
  result[3, 2] := - (ZFar + ZNear) / Depth;
  result[3, 3] := 1;
end;

function Ortho2dProjMatrix(const Left, Right, Bottom, Top: Single): TMatrix4Single;
var
  Width, Height: Single;
begin
  {wersja prosta : result := OrthoProjMatrix(Left, Right, Bottom, Top, -1, 1);}
  {wersja zoptymalizowana :}
  Width := Right - Left;
  Height := Top - Bottom;
  {Depth := ZFar - ZNear = (1 - (-1)) = 2}

  Result := ZeroMatrix4Single;
  Result[0, 0] := 2 / Width;
  Result[1, 1] := 2 / Height;
  Result[2, 2] := {-2 / Depth = -2 / 2} -1;
  Result[3, 0] := - (Right + Left) / Width;
  Result[3, 1] := - (Top + Bottom) / Height;
  Result[3, 2] := {- (ZFar + ZNear) / Depth = 0 / 2} 0;
  Result[3, 3] := 1;
end;

function FrustumProjMatrix(const Left, Right, Bottom, Top, ZNear, ZFar: Single): TMatrix4Single;

{ This is of course based on "OpenGL Programming Guide",
  Appendix G "... and Transformation Matrices".
  ZFarInfinity version based on various sources, pretty much every
  article about shadow volumes mentions z-fail and this trick. }

var
  Width, Height, Depth, ZNear2: Single;
begin
  Width := Right - Left;
  Height := Top - Bottom;
  ZNear2 := ZNear * 2;

  Result := ZeroMatrix4Single;
  Result[0, 0] := ZNear2         / Width;
  Result[2, 0] := (Right + Left) / Width;
  Result[1, 1] := ZNear2         / Height;
  Result[2, 1] := (Top + Bottom) / Height;
  if ZFar <> ZFarInfinity then
  begin
    Depth := ZFar - ZNear;
    Result[2, 2] := - (ZFar + ZNear) / Depth;
    Result[3, 2] := - ZNear2 * ZFar  / Depth;
  end else
  begin
    Result[2, 2] := -1;
    Result[3, 2] := -ZNear2;
  end;
  Result[2, 3] := -1;
end;

function PerspectiveProjMatrixDeg(const FovyDeg, Aspect, ZNear, ZFar: Single): TMatrix4Single;
begin
  Result := PerspectiveProjMatrixRad(DegToRad(FovyDeg), Aspect, ZNear, ZFar);
end;

function PerspectiveProjMatrixRad(const FovyRad, Aspect, ZNear, ZFar: Single): TMatrix4Single;
{ Based on various sources, e.g. sample implementation of
  glu by SGI in Mesa3d sources. }
var
  Depth, ZNear2, Cotangent: Single;
begin
  ZNear2 := ZNear * 2;
  Cotangent := CastleCoTan(FovyRad / 2);

  Result := ZeroMatrix4Single;
  Result[0, 0] := Cotangent / Aspect;
  Result[1, 1] := Cotangent;
  if ZFar <> ZFarInfinity then
  begin
    Depth := ZFar - ZNear;
    Result[2, 2] := - (ZFar + ZNear) / Depth;
    Result[3, 2] := - ZNear2 * ZFar  / Depth;
  end else
  begin
    Result[2, 2] := -1;
    Result[3, 2] := -ZNear2;
  end;

  Result[2, 3] := -1;
end;

{ kod dla MatrixDet* przerobiony z vect.c z mgflib }

function MatrixDet4x4(const mat: TMatrix4Single): Single;
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: Single;
begin
  a1 := mat[0][0]; b1 := mat[0][1];
  c1 := mat[0][2]; d1 := mat[0][3];

  a2 := mat[1][0]; b2 := mat[1][1];
  c2 := mat[1][2]; d2 := mat[1][3];

  a3 := mat[2][0]; b3 := mat[2][1];
  c3 := mat[2][2]; d3 := mat[2][3];

  a4 := mat[3][0]; b4 := mat[3][1];
  c4 := mat[3][2]; d4 := mat[3][3];

  result := a1 * MatrixDet3x3 (b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * MatrixDet3x3 (a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * MatrixDet3x3 (a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * MatrixDet3x3 (a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;


function MatrixDet3x3(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
begin
  result := a1 * MatrixDet2x2 (b2, b3, c2, c3)
          - b1 * MatrixDet2x2 (a2, a3, c2, c3)
          + c1 * MatrixDet2x2 (a2, a3, b2, b3);
end;

function MatrixDet2x2(const a, b, c, d: Single): Single;
begin
  result := a * d - b * c;
end;

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

function ModelViewToNormalMatrix(const M: TMatrix4Single): TMatrix3Single;
var
  D: Single;
  M3: TMatrix3Single;
begin
  Move(M[0], M3[0], SizeOf(TVector3Single));
  Move(M[1], M3[1], SizeOf(TVector3Single));
  Move(M[2], M3[2], SizeOf(TVector3Single));
  D := MatrixDeterminant(M3);
  if Zero(D) then
    { modelview not invertible, nothing good to do but we have to continue
      without errors, since this can happen with arbitrary 3D scenes. }
    Result := M3 else
  begin
    Result := MatrixInverse(M3, D);
    MatrixTransposeVar(Result);
  end;
end;

end.
