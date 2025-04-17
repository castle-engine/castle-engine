{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Vector and matrix types and basic geometric operations.)

  Various routines in this unit perform operations
  on geometric objects, like spheres and line segments.
  Here's an outline @bold(how do we represent various geometric objects):

  @unorderedList(
    @item(@italic(Point in 3D) is a TVector3.)

    @item(
      @italic(Plane in 3D) is a TVector4. Such vector [A, B, C, D]
      defines a surface that consists of all points satisfying equation
      @code(A * x + B * y + C * z + D = 0). At least one of A, B, C must be
      different than zero.

      Vector [A, B, C] is called PlaneDir in many places.
      It is called PlaneNormal when it's guaranteed (or required to be) normalized,
      i.e. scaled to have length 1.)

    @item(
      @italic(Line in 3D space) is represented by two TVector3 values:
      Line0 and LineVector. They determine a line consisting of all
      points that can be calculated as @code(Line0 + R * LineVector)
      where R is any real value.

      LineVector must not be a zero vector.)

    @item(
      @italic(Line in 2D space) is sometimes represented as two TVector2 values called
      Line0 and LineVector (analogously like line in 3D).

      And sometimes it's represented as one TVector3 value:
      [A, B, C], where line consists of all points satisfying @code(A * x + B * y + C = 0).
      At least one of A, B must be different than zero.)

    @item(
      A @italic(tunnel) is an object that you get by moving a sphere
      along the line segment. In other words, this is like a cylinder,
      but ends with two hemispheres. The tunnel is represented in this
      unit as two points Tunnel1, Tunnel2 (this defines a line segment)
      and a TunnelRadius.)

    @item(
      A @italic(ray) is defined just like a line: two vectors RayOrigin and RayDirection,
      RayDirection must be nonzero.
      Ray consists of all points @code(RayOrigin + R * RayDirection)
      for R being any real value >= 0.)

    @item(
      An @italic(axis-aligned plane in 3D) is a plane parallel to one of
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
    )

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

  @bold(Requirements of the geometric objects:)

  In descriptions of the geometric objects above, you can see
  some @italic(requirements), e.g.
  @italic("the triangle must not be degenerated (equivalent to a line segment)"),
  @italic("RayDirection must not be a zero vector"), etc.
  These requirements are generally @italic(not checked)
  by routines in this unit (for the sake of speed) and @italic(passing
  wrong values to many of the routines may lead to exceptions)
  (like an arithmetic exception) or nonsensible results. So try to make sure
  that the values you pass satisfy the requirements.

  However, @italic(wrong input values should never lead to exceptions
  that would be a security risk), like access violations or range check errors.
  As these exceptions could be uncatched in some situations,
  and then allow accessing a memory that otherwise should not be accessed,
  so we protect from this.
  Sometimes you simply cannot guarantee for 100%
  that input values are correct, because the floating-point operations
  are inherently not precise.

  @bold(About floating-point precision:)

  @unorderedList(
    @item(As floating-point operations are never precise,
      when comparing floating-point values we always use some epsilon
      to "tolerate" some small differences.

      This epsilon is either done through the standard Math.SameValue and Math.IsZero
      or by using our own SingleEpsilon and DoubleEpsilon constants
      (they are equal to standard Math unit epsilon values).
      The floating-point vector and matrix comparison methods, like
      @link(CastleVectorsInternalSingle.TGenericVector3.Equals TVector3.Equals) and
      @link(CastleVectorsInternalSingle.TGenericVector3.IsZero TVector3.IsZero), compare with such epsilon.

      If you really want to compare things precisely,
      use methods like
      @link(CastleVectorsInternalSingle.TGenericVector3.PerfectlyEquals TVector3.PerfectlyEquals) or
      @link(CastleVectorsInternalSingle.TGenericVector3.IsPerfectlyZero TVector3.IsPerfectlyZero).
    )

    @item(For collision-detecting routines, the general strategy
      in case of uncertainty (when we're not sure whether there
      is a collision or the objects are just very very close to each other)
      is to say that there @italic(is a collision).

      This means that we may detect a collision when in fact the precise
      mathematical calculation says that there is no collision.

      This approach should be suitable for most use cases.)
  )
}
unit CastleVectors;

{$I castleconf.inc}

interface

uses SysUtils, Generics.Collections, Classes, Math,
  CastleUtils, CastleVectorsInternalSingle, CastleVectorsInternalDouble, CastleClassUtils;

{$define read_interface}

{$I castlevectors_float.inc}
{$I castlevectors_single.inc}
{$I castlevectors_double.inc}
{$I castlevectors_byte.inc}
{$I castlevectors_integer.inc}
{$I castlevectors_cardinal.inc}
{$I castlevectors_smallint.inc}
{$I castlevectors_lists.inc}
{$I castlevectors_lists_double.inc}
{$I castlevectors_compatibility_deprecated.inc}
{$I castlevectors_miscellaneous.inc}
{$I castlevectors_persistent.inc}
{$I castlevectors_transformation.inc}
{$I castlevectors_border.inc}
{$I castlevectors_matrix_decompose.inc}

{$undef read_interface}

implementation

uses
  CastleStringUtils, CastleColors, CastleLog, CastleQuaternions;

{$define read_implementation}

{$I castlevectors_miscellaneous.inc}
{$I castlevectors_float.inc}
{$I castlevectors_single.inc}
{$I castlevectors_double.inc}
{$I castlevectors_byte.inc}
{$I castlevectors_integer.inc}
{$I castlevectors_cardinal.inc}
{$I castlevectors_smallint.inc}
{$I castlevectors_lists.inc}
{$I castlevectors_lists_double.inc}
{$I castlevectors_compatibility_deprecated.inc}
{$I castlevectors_persistent.inc}
{$I castlevectors_transformation.inc}
{$I castlevectors_border.inc}
{$I castlevectors_matrix_decompose.inc}

end.
