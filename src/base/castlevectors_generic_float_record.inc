{%MainUnit castlevectors.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*Interface and implementation of a TGenericVector* types,
  that can use any floating-point precision (Single, Double, Extended).

  ----------------------------------------------------------------------------
  Note that we cannot use proper generics to define a TGenericVector3<T>,
  because this cannot work under Delphi:

    function TGenericVector3 {$ifndef FPC}<T>{$endif}.Length: T;
    begin
      Result := Sqrt(
        (X * X) +
        (Y * Y) +
        (Z * Z)
      );
    end;

  Delphi wants to check the correctness when reading the generic,
  and it will not allow artihmetic operations on X,
  and it cannot narrow the type to something where artihmetic operations are OK.
  See
  https://stackoverflow.com/questions/40059579/arithmetic-operations-with-generic-types-in-delphi

  Under FPC it works, it seems that FPC checks Sqrt() correctness only later.
  But FPC doesn't allow "Sqr(X)" too, so it's possible that the lack
  of an early Sqrt check is just an FPC bug.

  ----------------------------------------------------------------------------
  Note: We avoid introducing in these records *any* method that changes
  the current record value. E.g.

  - X, Y, Z cannot be properties, esp. they cannot be properties with setters.
    They have to be simple fields.

  - Likewise we don't want a method like "Init" that modifies current record.

  The only way to change the record should be through direct field access.

  Why? Because it was leading to traps.

    S.Translation.X := S.Translation.X + 10; // when X is a setter
    S.Translation.Init;

  .. would compile, but actually be incorrect, as you (potentially -- depending
  on optimization) modify only the temporary record value.
*)

type
  { Vector of 2 floating-point values.
    @seealso TGenericVector3 }
  TGenericVector2 = packed record
  public
    const
      Count = 2;
    type
      TIndex = 0..Count - 1;
  strict private
    function GetItems(const Index: TIndex): TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetItems(const Index: TIndex; const Value: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class function GetOne(const Index: TIndex): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123.456) if you want to set by index,
      or @code(MyVector.X := 123.456). }
    property AsArray [const Index: TIndex]: TGenericScalar read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: TGenericScalar read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X,Y';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: TGenericScalar read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X,Y';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector2): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericVector2): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TGenericVector2): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector2; const Scalar: TGenericScalar): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericVector2): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    { Vector * vector makes a component-wise multiplication.
      This is consistent with GLSL and other vector APIs. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V1, V2: TGenericVector2): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}/{$else}Divide{$endif} (const V: TGenericVector2; const Scalar: TGenericScalar): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    procedure Init(const AX, AY: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
      deprecated 'initialize instead like "V := Vector2(X, Y)"; modifying a temporary record value is a trap, e.g. this is not reliable: "Scene.TranslationXY.Init(X, Y)"';

    function ToString: string;

    { Convert to string using the most precise (not always easily readable
      by humans) float format. This may use the exponential
      (scientific) notation to represent the floating-point value,
      if needed.

      This is suitable for storing the value in a file,
      with a best precision possible. }
    function ToRawString: string;

    function Normalize: TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure NormalizeMe; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
      deprecated 'normalize instead like "V := V.Normalize"; modifying a temporary record value is a trap, e.g. this is not reliable: "Scene.TranslationXY.NormalizeMe"';

    function Length: TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Vector length squared.
      This is slightly faster than @link(Length) as it avoids calculating
      a square root along the way. (But, please remember to not optimize
      your code without a need. Optimize only parts that are proven bottlenecks,
      otherwise don't make the code less readable for the sake of speed.) }
    function LengthSqr: TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Calculate a new vector scaled so that it has length equal to NewLength.
      NewLength may be negative, in which case we'll negate the vector
      and then adjust it's length to Abs(NewLength). }
    function AdjustToLength(const NewLength: TGenericScalar): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Dot product of two vectors.
      See https://en.wikipedia.org/wiki/Dot_product . }
    class function DotProduct(const V1, V2: TGenericVector2): TGenericScalar; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Absolute value on all components. }
    function Abs: TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Are all components equal to zero (within some epsilon margin). }
    function IsZero: boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Are all components equal to zero (within Epsilon margin). }
    function IsZero(const Epsilon: TGenericScalar): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    function IsPerfectlyZero: boolean;

    { Compare two vectors, with epsilon to tolerate slightly different floats. }
    class function Equals(const V1, V2: TGenericVector2): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;
    class function Equals(const V1, V2: TGenericVector2; const Epsilon: TGenericScalar): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;

    { Compare two vectors using exact comparison (like the "=" operator to compare floats). }
    class function PerfectlyEquals(const V1, V2: TGenericVector2): boolean; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Linear interpolation between two vector values.
      @seealso TGenericVector3.Lerp }
    class function Lerp(const A: TGenericScalar; const V1, V2: TGenericVector2): TGenericVector2; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    class function Zero: TGenericVector2; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class property One [const Index: TIndex]: TGenericVector2 read GetOne;

    case Integer of
      0: (X, Y: TGenericScalar);
      1: ({ Access (get, set) vector components by index. } Data: array [TIndex] of TGenericScalar);
  end;

  { Vector of 3 floating-point values.

    This is generic type (although not using "proper" Pascal generics
    for implementation reasons). In has two actual uses:

    @orderedList(
      @itemSpacing Compact
      @item @link(TVector3), a vector of 3 Single values (floats with single precision),
      @item @link(TVector3Double), a vector of 3 Double values (floats with double precision).
    )

    The actual type of TGenericScalar is
    Single or Double for (respectively) @link(TVector3) or @link(TVector3Double).
  }
  TGenericVector3 = packed record
  public
    const
      Count = 3;
    type
      TIndex = 0..Count - 1;
  strict private
    function GetItems(const Index: TIndex): TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetItems(const Index: TIndex; const Value: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class function GetOne(const Index: TIndex): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123.456) if you want to set by index,
      or @code(MyVector.X := 123.456). }
    property AsArray [const Index: TIndex]: TGenericScalar read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: TGenericScalar read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: TGenericScalar read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector3): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericVector3): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TGenericVector3): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector3; const Scalar: TGenericScalar): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericVector3): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    { Vector * vector makes a component-wise multiplication.
      This is consistent with GLSL and other vector APIs. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V1, V2: TGenericVector3): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}/{$else}Divide{$endif} (const V: TGenericVector3; const Scalar: TGenericScalar): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    procedure Init(const AX, AY, AZ: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
      deprecated 'initialize instead like "V := Vector3(X, Y, Z)"; modifying a temporary record value is a trap, e.g. this is not reliable: "Scene.Translation.Init(X, Y, Z)"';

    function ToString: string;

    { Convert to string using the most precise (not always easily readable
      by humans) float format. This may use the exponential
      (scientific) notation to represent the floating-point value,
      if needed.

      This is suitable for storing the value in a file,
      with a best precision possible. }
    function ToRawString: string;

    function Normalize: TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure NormalizeMe; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
      deprecated 'normalize instead like "V := V.Normalize"; modifying a temporary record value is a trap, e.g. this is not reliable: "Scene.Translation.NormalizeMe"';

    function Length: TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Vector length squared.
      This is slightly faster than @link(Length) as it avoids calculating
      a square root along the way. (But, please remember to not optimize
      your code without a need. Optimize only parts that are proven bottlenecks,
      otherwise don't make the code less readable for the sake of speed.) }
    function LengthSqr: TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Calculate a new vector scaled so that it has length equal to NewLength.
      NewLength may be negative, in which case we'll negate the vector
      and then adjust it's length to Abs(NewLength). }
    function AdjustToLength(const NewLength: TGenericScalar): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Vector cross product.
      See http://en.wikipedia.org/wiki/Cross_product .

      Result is a vector orthogonal to both given vectors.
      Generally there are two such vectors, this method returns
      the one following right-hand rule. More precisely, V1, V2 and
      Product(V1, V2) are in the same relation as basic X, Y, Z
      axes. Reverse the order of arguments to get negated result.

      If you use this to calculate a normal vector of a triangle
      (P0, P1, P2): note that @code(TVector3.CrossProduct(P1 - P0, P1 - P2))
      points out from CCW triangle side in right-handed coordinate system.

      When V1 and V2 are parallel (that is, when V1 = V2 multiplied by some scalar),
      and this includes the case when one of them is zero,
      then result is a zero vector. }
    class function CrossProduct(const V1, V2: TGenericVector3): TGenericVector3; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Dot product of two vectors.
      See https://en.wikipedia.org/wiki/Dot_product . }
    class function DotProduct(const V1, V2: TGenericVector3): TGenericScalar; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Absolute value on all components. }
    function Abs: TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Smallest component. }
    function Min: TGenericScalar;

    { Largest component. }
    function Max: TGenericScalar;

    { Average from all components. }
    function Average: TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Are all components equal to zero (within some epsilon margin). }
    function IsZero: boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Are all components equal to zero (within Epsilon margin). }
    function IsZero(const Epsilon: TGenericScalar): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    function IsPerfectlyZero: boolean; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Compare two vectors, with epsilon to tolerate slightly different floats. }
    class function Equals(const V1, V2: TGenericVector3): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;
    class function Equals(const V1, V2: TGenericVector3; const Epsilon: TGenericScalar): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;

    { Compare two vectors using exact comparison (like the "=" operator to compare floats). }
    class function PerfectlyEquals(const V1, V2: TGenericVector3): boolean; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Linear interpolation between two vector values.
      Returns @code((1-A) * V1 + A * V2).

      So:

      @unorderedList(
        @itemSpacing Compact
        @item A = 0 gives V1,
        @item A = 1 gives V2,
        @item values between are interpolated,
        @item values outside are extrapolated.
      ) }
    class function Lerp(const A: TGenericScalar; const V1, V2: TGenericVector3): TGenericVector3; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    class function Zero: TGenericVector3; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class property One [const Index: TIndex]: TGenericVector3 read GetOne;

    case Integer of
      0: (X, Y, Z: TGenericScalar);
      1: ({ Access (get, set) vector components by index. } Data: array [TIndex] of TGenericScalar);
      2: (XY: TGenericVector2);
  end;

  { Vector of 4 floating-point values.
    @seealso TGenericVector3 }
  TGenericVector4 = packed record
  public
    const
      Count = 4;
    type
      TIndex = 0..Count - 1;
  strict private
    function GetItems(const Index: TIndex): TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetItems(const Index: TIndex; const Value: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class function GetOne(const Index: TIndex): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;
  public
    { Get vector components by index.
      This is a default property, so you can write @code(MyVector[0])
      instead of @code(MyVector.Data[0]) or @code(MyVector.AsArray[0]).

      But note that this is not writeable (because exposing writeable properties on vectors
      would cause some subtle traps, see https://castle-engine.io/coding_traps ).
      Use @code(MyVector.Data[0] := 123.456) if you want to set by index,
      or @code(MyVector.X := 123.456). }
    property AsArray [const Index: TIndex]: TGenericScalar read GetItems; default;

    { @exclude
      Access (get, set) vector components by index. }
    property InternalData [const Index: TIndex]: TGenericScalar read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z, W';{$endif}
    { @exclude
      Access (get, set) vector components by index. }
    property Items [const Index: TIndex]: TGenericScalar read GetItems write SetItems;
      {$ifdef FPC}deprecated 'use Data or X, Y, Z, W';{$endif}

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector4): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericVector4): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const V: TGenericVector4): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector4; const Scalar: TGenericScalar): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericVector4): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    { Vector * vector makes a component-wise multiplication.
      This is consistent with GLSL and other vector APIs. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V1, V2: TGenericVector4): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}/{$else}Divide{$endif} (const V: TGenericVector4; const Scalar: TGenericScalar): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    procedure Init(const AX, AY, AZ, AW: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
      deprecated 'initialize instead like "V := Vector4(X, Y, Z, W)"; modifying a temporary record value is a trap, e.g. this is not reliable: "Scene.Rotation.Init(X, Y, Z, W)"';

    function ToString: string;

    { Convert to string using the most precise (not always easily readable
      by humans) float format. This may use the exponential
      (scientific) notation to represent the floating-point value,
      if needed.

      This is suitable for storing the value in a file,
      with a best precision possible. }
    function ToRawString: string;

    function Length: TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Vector length squared.
      This is slightly faster than @link(Length) as it avoids calculating
      a square root along the way. (But, please remember to not optimize
      your code without a need. Optimize only parts that are proven bottlenecks,
      otherwise don't make the code less readable for the sake of speed.) }
    function LengthSqr: TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Calculate a new vector scaled so that it has length equal to NewLength.
      NewLength may be negative, in which case we'll negate the vector
      and then adjust it's length to Abs(NewLength). }
    function AdjustToLength(const NewLength: TGenericScalar): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Dot product of two vectors.
      See https://en.wikipedia.org/wiki/Dot_product . }
    class function DotProduct(const V1, V2: TGenericVector4): TGenericScalar; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Absolute value on all components. }
    function Abs: TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Are all components equal to zero (within some epsilon margin). }
    function IsZero: boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Are all components equal to zero (within Epsilon margin). }
    function IsZero(const Epsilon: TGenericScalar): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    function IsPerfectlyZero: boolean; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Compare two vectors, with epsilon to tolerate slightly different floats. }
    class function Equals(const V1, V2: TGenericVector4): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;
    class function Equals(const V1, V2: TGenericVector4; const Epsilon: TGenericScalar): boolean; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif} static;

    { Compare two vectors using exact comparison (like the "=" operator to compare floats). }
    class function PerfectlyEquals(const V1, V2: TGenericVector4): boolean; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Convert a 4D homogeneous coordinate to 3D position. }
    function ToPosition: TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Linear interpolation between two vector values.
      Works analogous to @link(TGenericVector3.Lerp TVector3.Lerp) }
    class function Lerp(const A: TGenericScalar; const V1, V2: TGenericVector4): TGenericVector4; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    class function Zero: TGenericVector4; static; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class property One [const Index: TIndex]: TGenericVector4 read GetOne;

    case Integer of
      0: (X, Y, Z, W: TGenericScalar);
      1: ({ Access (get, set) vector components by index. } Data: array [TIndex] of TGenericScalar);
      2: ({ Get first 2 components as a 2D vector.
            This simply rejects the remaining vector components. }
          XY: TGenericVector2;
          ZW: TGenericVector2);
      3: ({ Get first 3 components as a 3D vector.
            This simply rejects the 4th component. }
          XYZ: TGenericVector3);
  end;

  { 2x2 matrix of floating-point values.
    @seealso TGenericMatrix3 }
  TGenericMatrix2 = record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const AColumn, ARow: TIndex): TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetItems(const AColumn, ARow: TIndex; const Value: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    function GetRows(const ARow: TIndex): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetRows(const ARow: TIndex; const Value: TGenericVector2); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    function GetColumns(const AColumn: TIndex): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetColumns(const AColumn: TIndex; const Value: TGenericVector2); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
  public
    var
      Data: array [TIndex, TIndex] of TGenericScalar;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix2): TGenericMatrix2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericMatrix2): TGenericMatrix2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const M: TGenericMatrix2): TGenericMatrix2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix2; const Scalar: TGenericScalar): TGenericMatrix2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericMatrix2): TGenericMatrix2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const M: TGenericMatrix2; const V: TGenericVector2): TGenericVector2; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    { Matrix * matrix makes a normal matrix algebraic multiplication
      (not component-wise multiplication).
      Note that this is different from vectors, where vector * vector
      makes a component-wise multiplication. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const M1, M2: TGenericMatrix2): TGenericMatrix2;

    function ToString(const LineIndent: string = ''): string;

    { Convert to string using the most precise (not always easily readable
      by humans) float format. This may use the exponential
      (scientific) notation to represent the floating-point value,
      if needed.

      This is suitable for storing the value in a file,
      with a best precision possible. }
    function ToRawString(const LineIndent: string = ''): string;

    property Items [const AColumn, ARow: TIndex]: TGenericScalar read GetItems write SetItems; default;
    property Rows [const ARow: TIndex]: TGenericVector2 read GetRows write SetRows;
    property Columns [const AColumn: TIndex]: TGenericVector2 read GetColumns write SetColumns;
    function Determinant: TGenericScalar;

    { Inverse the matrix.

      This does division by ADeterminant internally, so will raise exception
      from this float division if the matrix is not reversible.
      Check Math.IsZero(ADeterminant) first to avoid this, or use @link(TryInverse). }
    function Inverse(ADeterminant: TGenericScalar): TGenericMatrix2;

    { Inverse the matrix, or return @false if the matrix is not invertible. }
    function TryInverse(out MInverse: TGenericMatrix2): boolean;

    function Transpose: TGenericMatrix2;

    { Compare two vectors, with epsilon to tolerate slightly different floats. }
    class function Equals(const M1, M2: TGenericMatrix2): boolean; overload; static;
    class function Equals(const M1, M2: TGenericMatrix2; const Epsilon: TGenericScalar): boolean; overload; static;

    { Compare two vectors using exact comparison (like the "=" operator to compare floats). }
    class function PerfectlyEquals(const M1, M2: TGenericMatrix2): boolean; static;

    { Linear interpolation between two matrix values.
      @seealso TGenericVector3.Lerp }
    class function Lerp(const A: TGenericScalar; const M1, M2: TGenericMatrix2): TGenericMatrix2; static;

    class function Zero: TGenericMatrix2; static;
    class function Identity: TGenericMatrix2; static;
  end;

  { 3x3 matrix of floating-point values.
    Column-major, just like OpenGL, which means that the first index
    of @link(Data) array should be treated as a column number,
    the 2nd index is the row number.

    This is generic type (although not using "proper" Pascal generics
    for implementation reasons). In has two actual uses:

    @orderedList(
      @itemSpacing Compact
      @item @link(TMatrix3), a matrix of 3 Single values (floats with single precision),
      @item @link(TMatrix3Double), a matrix of 3 Double values (floats with double precision).
    )

    The type TGenericScalar is, accordingly,
    Single or Double for @link(TMatrix3) or @link(TMatrix3Double).
  }
  TGenericMatrix3 = record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const AColumn, ARow: TIndex): TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetItems(const AColumn, ARow: TIndex; const Value: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    function GetRows(const ARow: TIndex): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetRows(const ARow: TIndex; const Value: TGenericVector3); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    function GetColumns(const AColumn: TIndex): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetColumns(const AColumn: TIndex; const Value: TGenericVector3); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
  public
    type
      TIndexArray = array[TIndex] of TGenericScalar;
    var
      //Data: array [TIndex, TIndex] of TGenericScalar;
      Data: array [TIndex] of TIndexArray;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix3): TGenericMatrix3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericMatrix3): TGenericMatrix3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const M: TGenericMatrix3): TGenericMatrix3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix3; const Scalar: TGenericScalar): TGenericMatrix3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericMatrix3): TGenericMatrix3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const M: TGenericMatrix3; const V: TGenericVector3): TGenericVector3; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    { Matrix * matrix makes a normal matrix algebraic multiplication
      (not component-wise multiplication).
      Note that this is different from vectors, where vector * vector
      makes a component-wise multiplication. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const M1, M2: TGenericMatrix3): TGenericMatrix3;

    function ToString(const LineIndent: string = ''): string;

    { Convert to string using the most precise (not always easily readable
      by humans) float format. This may use the exponential
      (scientific) notation to represent the floating-point value,
      if needed.

      This is suitable for storing the value in a file,
      with a best precision possible. }
    function ToRawString(const LineIndent: string = ''): string;

    property Items [const AColumn, ARow: TIndex]: TGenericScalar read GetItems write SetItems; default;
    property Rows [const ARow: TIndex]: TGenericVector3 read GetRows write SetRows;
    property Columns [const AColumn: TIndex]: TGenericVector3 read GetColumns write SetColumns;
    function Determinant: TGenericScalar;

    { Inverse the matrix.

      This does division by ADeterminant internally, so will raise exception
      from this float division if the matrix is not reversible.
      Check Math.IsZero(ADeterminant) first to avoid this, or use @link(TryInverse). }
    function Inverse(ADeterminant: TGenericScalar): TGenericMatrix3;

    { Inverse the matrix, or return @false if the matrix is not invertible. }
    function TryInverse(out MInverse: TGenericMatrix3): boolean;

    function Transpose: TGenericMatrix3;

    { Compare two vectors, with epsilon to tolerate slightly different floats. }
    class function Equals(const M1, M2: TGenericMatrix3): boolean; overload; static;
    class function Equals(const M1, M2: TGenericMatrix3; const Epsilon: TGenericScalar): boolean; overload; static;

    { Compare two vectors using exact comparison (like the "=" operator to compare floats). }
    class function PerfectlyEquals(const M1, M2: TGenericMatrix3): boolean; static;

    { Linear interpolation between two matrix values.
      @seealso TGenericVector3.Lerp }
    class function Lerp(const A: TGenericScalar; const M1, M2: TGenericMatrix3): TGenericMatrix3; static;

    class function Zero: TGenericMatrix3; static;
    class function Identity: TGenericMatrix3; static;
  end;

  { 4x4 matrix of floating-point values.
    @seealso TGenericMatrix3 }
  TGenericMatrix4 = record
  public
    type
      TIndex = 0..3;
  strict private
    function GetItems(const AColumn, ARow: TIndex): TGenericScalar; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetItems(const AColumn, ARow: TIndex; const Value: TGenericScalar); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    function GetRows(const ARow: TIndex): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetRows(const ARow: TIndex; const Value: TGenericVector4); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    function GetColumns(const AColumn: TIndex): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure SetColumns(const AColumn: TIndex; const Value: TGenericVector4); {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    procedure RaisePositionTransformResultInvalid;
    { Raise ETransformedResultInvalid.
      Since TGenericMatrix4.MultDirection is time-critical (it is used in TFrustum.Transform
      which can be a bottleneck, testcase: The Unholy Society),
      keep it fast. }
    procedure RaiseDirectionTransformedResultInvalid(const Divisor: TGenericScalar);
  public
    type
      TIndexArray = array[TIndex] of TGenericScalar;
    var
      //Data: array [TIndex, TIndex] of TGenericScalar;
      Data: array [TIndex] of TIndexArray;

    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix4): TGenericMatrix4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericMatrix4): TGenericMatrix4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}-{$else}Negative{$endif} (const M: TGenericMatrix4): TGenericMatrix4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix4; const Scalar: TGenericScalar): TGenericMatrix4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericMatrix4): TGenericMatrix4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const M: TGenericMatrix4; const V: TGenericVector4): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
    { Matrix * matrix makes a normal matrix algebraic multiplication
      (not component-wise multiplication).
      Note that this is different from vectors, where vector * vector
      makes a component-wise multiplication. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const M1, M2: TGenericMatrix4): TGenericMatrix4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    { Calculates "M.Transpose * V" in a faster way
      (without spending *any* time on calculating transposition). }
    function TransposeMultiply(const V: TGenericVector4): TGenericVector4; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

    function ToString(const LineIndent: string = ''): string;

    { Convert to string using the most precise (not always easily readable
      by humans) float format. This may use the exponential
      (scientific) notation to represent the floating-point value,
      if needed.

      This is suitable for storing the value in a file,
      with a best precision possible. }
    function ToRawString(const LineIndent: string = ''): string;

    property Items [const AColumn, ARow: TIndex]: TGenericScalar read GetItems write SetItems; default;
    property Rows [const ARow: TIndex]: TGenericVector4 read GetRows write SetRows;
    property Columns [const AColumn: TIndex]: TGenericVector4 read GetColumns write SetColumns;
    function Determinant: TGenericScalar;

    { Inverse the matrix.

      This does division by ADeterminant internally, so will raise exception
      from this float division if the matrix is not reversible.
      Check Math.IsZero(ADeterminant) first to avoid this, or use @link(TryInverse). }
    function Inverse(ADeterminant: TGenericScalar): TGenericMatrix4;

    { Inverse the matrix, or return @false if the matrix is not invertible. }
    function TryInverse(out MInverse: TGenericMatrix4): boolean;

    function Transpose: TGenericMatrix4;

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
    function MultPoint(const Pt: TGenericVector3): TGenericVector3; overload;
    function MultPoint(const Pt: TGenericVector2): TGenericVector2; overload;
    { @groupEnd }

    { Transform a 3D or 2D direction with 4x4 matrix.

      This works by temporarily converting direction to 4-component vector
      (4th component is 0). After multiplying matrix * vector we check
      is the 4th component still 0 (eventually raising ETransformedResultInvalid).

      @raises(ETransformedResultInvalid This is raised when matrix
      will transform direction to a point (vector with 4th component
      nonzero). In this case we just cannot interpret the result as a direction.)

      @groupBegin }
    function MultDirection(const Dir: TGenericVector3): TGenericVector3; overload;
    function MultDirection(const Dir: TGenericVector2): TGenericVector2; overload;
    { @groupEnd }

    { Compare two vectors, with epsilon to tolerate slightly different floats. }
    class function Equals(const M1, M2: TGenericMatrix4): boolean; overload; static;
    class function Equals(const M1, M2: TGenericMatrix4; const Epsilon: TGenericScalar): boolean; overload; static;

    { Compare two vectors using exact comparison (like the "=" operator to compare floats). }
    class function PerfectlyEquals(const M1, M2: TGenericMatrix4): boolean; static;

    { Linear interpolation between two matrix values.
      @seealso TGenericVector3.Lerp }
    class function Lerp(const A: TGenericScalar; const M1, M2: TGenericMatrix4): TGenericMatrix4; static;

    class function Zero: TGenericMatrix4; static;
    class function Identity: TGenericMatrix4; static;
  end;

implementation

uses Math,
  CastleUtils;

{ TGenericVector2 ------------------------------------------------------------ }

class operator TGenericVector2. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector2): TGenericVector2;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TGenericVector2. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericVector2): TGenericVector2;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TGenericVector2. {$ifdef FPC}-{$else}Negative{$endif} (const V: TGenericVector2): TGenericVector2;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
end;

class operator TGenericVector2. {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector2; const Scalar: TGenericScalar): TGenericVector2;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
end;

class operator TGenericVector2. {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericVector2): TGenericVector2;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
end;

class operator TGenericVector2. {$ifdef FPC}*{$else}Multiply{$endif} (const V1, V2: TGenericVector2): TGenericVector2;
begin
  Result.X := V1.X * V2.X;
  Result.Y := V1.Y * V2.Y;
end;

class operator TGenericVector2. {$ifdef FPC}/{$else}Divide{$endif} (const V: TGenericVector2; const Scalar: TGenericScalar): TGenericVector2;
var
  ScalarInv: TGenericScalar;
begin
  ScalarInv := 1 / Scalar; // multiplication is faster than division, so invert once
  Result.X := V.X * ScalarInv;
  Result.Y := V.Y * ScalarInv;
end;

procedure TGenericVector2.Init(const AX, AY: TGenericScalar);
begin
  X := AX;
  Y := AY;
end;

function TGenericVector2.ToString: string;
begin
  Result := FormatDot('%f %f', [X, Y]);
end;

function TGenericVector2.ToRawString: string;
begin
  Result := FormatDot('%g %g', [X, Y]);
end;

function TGenericVector2.GetItems(const Index: TIndex): TGenericScalar;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    { We use here numbers, not strings, for internal errors.
      Reason: avoid implicit try..finally clause,
      this method must be fast. }
    {$ifndef FPC} else raise EInternalError.Create(202202111); {$endif}
  end;
end;

procedure TGenericVector2.SetItems(const Index: TIndex; const Value: TGenericScalar);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    {$ifndef FPC} else raise EInternalError.Create(202202112); {$endif}
  end;
end;

function TGenericVector2.Normalize: TGenericVector2;
var
  L: TGenericScalar;
begin
  L := Length;
  if L = 0 then
    Result := Self
  else
  begin
    L := 1 / L;
    Result.X := X * L;
    Result.Y := Y * L;
  end;
end;

procedure TGenericVector2.NormalizeMe;
var
  L: TGenericScalar;
begin
  L := Length;
  if L <> 0 then
  begin
    L := 1 / L;
    X := X * L;
    Y := Y * L;
  end;
end;

function TGenericVector2.Length: TGenericScalar;
begin
  Result := Sqrt(
    (X * X) +
    (Y * Y)
  );
end;

function TGenericVector2.LengthSqr: TGenericScalar;
begin
  Result :=
    (X * X) +
    (Y * Y);
end;

function TGenericVector2.AdjustToLength(const NewLength: TGenericScalar): TGenericVector2;
begin
  Result := Self * (NewLength / Length);
end;

class function TGenericVector2.DotProduct(const V1, V2: TGenericVector2): TGenericScalar;
begin
  Result :=
    V1.X * V2.X +
    V1.Y * V2.Y;
end;

function TGenericVector2.Abs: TGenericVector2;
begin
  Result.X := System.Abs(X);
  Result.Y := System.Abs(Y);
end;

function TGenericVector2.IsZero: boolean;
begin
  Result :=
    Math.IsZero(X) and
    Math.IsZero(Y);
end;

function TGenericVector2.IsZero(const Epsilon: TGenericScalar): boolean;
begin
  Result := (System.Abs(X) < Epsilon) and
            (System.Abs(Y) < Epsilon);
end;

function TGenericVector2.IsPerfectlyZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0);
end;

class function TGenericVector2.Equals(const V1, V2: TGenericVector2): boolean;
begin
  Result :=
    SameValue(V1.X, V2.X) and
    SameValue(V1.Y, V2.Y);
end;

class function TGenericVector2.Equals(const V1, V2: TGenericVector2; const Epsilon: TGenericScalar): boolean;
begin
  if Epsilon = 0 then
    Result := (V1.X = V2.X) and
              (V1.Y = V2.Y) else
    Result := (System.Abs(V1.X-V2.X) < Epsilon) and
              (System.Abs(V1.Y-V2.Y) < Epsilon);
end;

class function TGenericVector2.PerfectlyEquals(const V1, V2: TGenericVector2): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y);
end;

class function TGenericVector2.Lerp(const A: TGenericScalar; const V1, V2: TGenericVector2): TGenericVector2;
begin
  Result.X := V1.X + A * (V2.X - V1.X);
  Result.Y := V1.Y + A * (V2.Y - V1.Y);
end;

class function TGenericVector2.Zero: TGenericVector2;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TGenericVector2.GetOne(const Index: TIndex): TGenericVector2;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Data[Index] := 1.0;
end;

{ TGenericVector3 ------------------------------------------------------------ }

class operator TGenericVector3. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector3): TGenericVector3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TGenericVector3. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericVector3): TGenericVector3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TGenericVector3. {$ifdef FPC}-{$else}Negative{$endif} (const V: TGenericVector3): TGenericVector3;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
end;

class operator TGenericVector3. {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector3; const Scalar: TGenericScalar): TGenericVector3;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
  Result.Z := V.Z * Scalar;
end;

class operator TGenericVector3. {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericVector3): TGenericVector3;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
  Result.Z := V.Z * Scalar;
end;

class operator TGenericVector3. {$ifdef FPC}*{$else}Multiply{$endif} (const V1, V2: TGenericVector3): TGenericVector3;
begin
  Result.X := V1.X * V2.X;
  Result.Y := V1.Y * V2.Y;
  Result.Z := V1.Z * V2.Z;
end;

class operator TGenericVector3. {$ifdef FPC}/{$else}Divide{$endif} (const V: TGenericVector3; const Scalar: TGenericScalar): TGenericVector3;
var
  ScalarInv: TGenericScalar;
begin
  ScalarInv := 1 / Scalar; // multiplication is faster than division, so invert once
  Result.X := V.X * ScalarInv;
  Result.Y := V.Y * ScalarInv;
  Result.Z := V.Z * ScalarInv;
end;

procedure TGenericVector3.Init(const AX, AY, AZ: TGenericScalar);
begin
  X := AX;
  Y := AY;
  Z := AZ;
end;

function TGenericVector3.ToString: string;
begin
  Result := FormatDot('%f %f %f', [X, Y, Z]);
end;

function TGenericVector3.ToRawString: string;
begin
  Result := FormatDot('%g %g %g', [X, Y, Z]);
end;

function TGenericVector3.GetItems(const Index: TIndex): TGenericScalar;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    {$ifndef FPC} else raise EInternalError.Create(202202113); {$endif}
  end;
end;

procedure TGenericVector3.SetItems(const Index: TIndex; const Value: TGenericScalar);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    {$ifndef FPC} else raise EInternalError.Create(202202114); {$endif}
  end;
end;

function TGenericVector3.Normalize: TGenericVector3;
var
  L: TGenericScalar;
begin
  L := Length;
  if L = 0 then
    Result := Self
  else
  begin
    L := 1 / L;
    Result.X := X * L;
    Result.Y := Y * L;
    Result.Z := Z * L;
  end;
end;

procedure TGenericVector3.NormalizeMe;
var
  L: TGenericScalar;
begin
  L := Length;
  if L <> 0 then
  begin
    L := 1 / L;
    X := X * L;
    Y := Y * L;
    Z := Z * L;
  end;
end;

function TGenericVector3.Length: TGenericScalar;
begin
  Result := Sqrt(
    (X * X) +
    (Y * Y) +
    (Z * Z)
  );
end;

function TGenericVector3.LengthSqr: TGenericScalar;
begin
  Result :=
    (X * X) +
    (Y * Y) +
    (Z * Z);
end;

function TGenericVector3.AdjustToLength(const NewLength: TGenericScalar): TGenericVector3;
begin
  Result := Self * (NewLength / Length);
end;

class function TGenericVector3.CrossProduct(const V1, V2: TGenericVector3): TGenericVector3;
begin
  Result.X := V1.Y * V2.Z - V1.Z * V2.Y;
  Result.Y := V1.Z * V2.X - V1.X * V2.Z;
  Result.Z := V1.X * V2.Y - V1.Y * V2.X;
end;

class function TGenericVector3.DotProduct(const V1, V2: TGenericVector3): TGenericScalar;
begin
  Result :=
    V1.X * V2.X +
    V1.Y * V2.Y +
    V1.Z * V2.Z;
end;

function TGenericVector3.Abs: TGenericVector3;
begin
  Result.X := System.Abs(X);
  Result.Y := System.Abs(Y);
  Result.Z := System.Abs(Z);
end;

function TGenericVector3.Min: TGenericScalar;
begin
  Result := MinValue([X, Y, Z]);
end;

function TGenericVector3.Max: TGenericScalar;
begin
  Result := MaxValue([X, Y, Z]);
end;

function TGenericVector3.Average: TGenericScalar;
begin
  Result := (X + Y + Z) / 3;
end;

function TGenericVector3.IsZero: boolean;
begin
  Result :=
    Math.IsZero(X) and
    Math.IsZero(Y) and
    Math.IsZero(Z);
end;

function TGenericVector3.IsZero(const Epsilon: TGenericScalar): boolean;
begin
  Result := (System.Abs(X) < Epsilon) and
            (System.Abs(Y) < Epsilon) and
            (System.Abs(Z) < Epsilon);
end;

function TGenericVector3.IsPerfectlyZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0);
end;

class function TGenericVector3.Equals(const V1, V2: TGenericVector3): boolean;
begin
  Result :=
    SameValue(V1.X, V2.X) and
    SameValue(V1.Y, V2.Y) and
    SameValue(V1.Z, V2.Z);
end;

class function TGenericVector3.Equals(const V1, V2: TGenericVector3; const Epsilon: TGenericScalar): boolean;
begin
  if Epsilon = 0 then
    Result := (V1.X = V2.X) and
              (V1.Y = V2.Y) and
              (V1.Z = V2.Z) else
    Result := (System.Abs(V1.X-V2.X) < Epsilon) and
              (System.Abs(V1.Y-V2.Y) < Epsilon) and
              (System.Abs(V1.Z-V2.Z) < Epsilon);
end;

class function TGenericVector3.PerfectlyEquals(const V1, V2: TGenericVector3): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z);
end;

class function TGenericVector3.Lerp(const A: TGenericScalar; const V1, V2: TGenericVector3): TGenericVector3;
begin
  Result.X := V1.X + A * (V2.X - V1.X);
  Result.Y := V1.Y + A * (V2.Y - V1.Y);
  Result.Z := V1.Z + A * (V2.Z - V1.Z);
end;

class function TGenericVector3.Zero: TGenericVector3;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TGenericVector3.GetOne(const Index: TIndex): TGenericVector3;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Data[Index] := 1.0;
end;

{ TGenericVector4 ------------------------------------------------------------ }

class operator TGenericVector4. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector4): TGenericVector4;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

class operator TGenericVector4. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericVector4): TGenericVector4;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

class operator TGenericVector4. {$ifdef FPC}-{$else}Negative{$endif} (const V: TGenericVector4): TGenericVector4;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
  Result.Z := - V.Z;
  Result.W := - V.W;
end;

class operator TGenericVector4. {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector4; const Scalar: TGenericScalar): TGenericVector4;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
  Result.Z := V.Z * Scalar;
  Result.W := V.W * Scalar;
end;

class operator TGenericVector4. {$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericVector4): TGenericVector4;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
  Result.Z := V.Z * Scalar;
  Result.W := V.W * Scalar;
end;

class operator TGenericVector4. {$ifdef FPC}*{$else}Multiply{$endif} (const V1, V2: TGenericVector4): TGenericVector4;
begin
  Result.X := V1.X * V2.X;
  Result.Y := V1.Y * V2.Y;
  Result.Z := V1.Z * V2.Z;
  Result.W := V1.W * V2.W;
end;

class operator TGenericVector4. {$ifdef FPC}/{$else}Divide{$endif} (const V: TGenericVector4; const Scalar: TGenericScalar): TGenericVector4;
var
  ScalarInv: TGenericScalar;
begin
  ScalarInv := 1 / Scalar; // multiplication is faster than division, so invert once
  Result.X := V.X * ScalarInv;
  Result.Y := V.Y * ScalarInv;
  Result.Z := V.Z * ScalarInv;
  Result.W := V.W * ScalarInv;
end;

procedure TGenericVector4.Init(const AX, AY, AZ, AW: TGenericScalar);
begin
  X := AX;
  Y := AY;
  Z := AZ;
  W := AW;
end;

function TGenericVector4.ToString: string;
begin
  Result := FormatDot('%f %f %f %f', [X, Y, Z, W]);
end;

function TGenericVector4.ToRawString: string;
begin
  Result := FormatDot('%g %g %g %g', [X, Y, Z, W]);
end;

function TGenericVector4.GetItems(const Index: TIndex): TGenericScalar;
begin
  case Index of
    0: Result := X;
    1: Result := Y;
    2: Result := Z;
    3: Result := W;
    {$ifndef FPC} else raise EInternalError.Create(202202115); {$endif}
  end;
end;

procedure TGenericVector4.SetItems(const Index: TIndex; const Value: TGenericScalar);
begin
  case Index of
    0: X := Value;
    1: Y := Value;
    2: Z := Value;
    3: W := Value;
    {$ifndef FPC} else raise EInternalError.Create(202202116); {$endif}
  end;
end;

function TGenericVector4.Length: TGenericScalar;
begin
  Result := Sqrt(
    (X * X) +
    (Y * Y) +
    (Z * Z) +
    (W * W)
  );
end;

function TGenericVector4.LengthSqr: TGenericScalar;
begin
  Result :=
    (X * X) +
    (Y * Y) +
    (Z * Z) +
    (W * W);
end;

function TGenericVector4.AdjustToLength(const NewLength: TGenericScalar): TGenericVector4;
begin
  Result := Self * (NewLength / Length);
end;

class function TGenericVector4.DotProduct(const V1, V2: TGenericVector4): TGenericScalar;
begin
  Result :=
    V1.X * V2.X +
    V1.Y * V2.Y +
    V1.Z * V2.Z +
    V1.W * V2.W;
end;

function TGenericVector4.Abs: TGenericVector4;
begin
  Result.X := System.Abs(X);
  Result.Y := System.Abs(Y);
  Result.Z := System.Abs(Z);
  Result.W := System.Abs(W);
end;

function TGenericVector4.IsZero: boolean;
begin
  Result :=
    Math.IsZero(X) and
    Math.IsZero(Y) and
    Math.IsZero(Z) and
    Math.IsZero(W);
end;

function TGenericVector4.IsZero(const Epsilon: TGenericScalar): boolean;
begin
  Result := (System.Abs(X) < Epsilon) and
            (System.Abs(Y) < Epsilon) and
            (System.Abs(Z) < Epsilon) and
            (System.Abs(W) < Epsilon);
end;

function TGenericVector4.IsPerfectlyZero: boolean;
begin
  Result :=
    (X = 0) and
    (Y = 0) and
    (Z = 0) and
    (W = 0);
end;

class function TGenericVector4.Equals(const V1, V2: TGenericVector4): boolean;
begin
  Result :=
    SameValue(V1.X, V2.X) and
    SameValue(V1.Y, V2.Y) and
    SameValue(V1.Z, V2.Z) and
    SameValue(V1.W, V2.W);
end;

class function TGenericVector4.Equals(const V1, V2: TGenericVector4; const Epsilon: TGenericScalar): boolean;
begin
  if Epsilon = 0 then
    Result := (V1.X = V2.X) and
              (V1.Y = V2.Y) and
              (V1.Z = V2.Z) and
              (V1.W = V2.W) else
    Result := (System.Abs(V1.X-V2.X) < Epsilon) and
              (System.Abs(V1.Y-V2.Y) < Epsilon) and
              (System.Abs(V1.Z-V2.Z) < Epsilon) and
              (System.Abs(V1.W-V2.W) < Epsilon);
end;

class function TGenericVector4.PerfectlyEquals(const V1, V2: TGenericVector4): boolean;
begin
  Result := (V1.X = V2.X) and
            (V1.Y = V2.Y) and
            (V1.Z = V2.Z) and
            (V1.W = V2.W);
end;

function TGenericVector4.ToPosition: TGenericVector3;
var
  Inv: TGenericScalar;
begin
  Inv := 1 / W;
  Result.X := X * Inv;
  Result.Y := Y * Inv;
  Result.Z := Z * Inv;
end;

class function TGenericVector4.Lerp(const A: TGenericScalar; const V1, V2: TGenericVector4): TGenericVector4;
begin
  Result.X := V1.X + A * (V2.X - V1.X);
  Result.Y := V1.Y + A * (V2.Y - V1.Y);
  Result.Z := V1.Z + A * (V2.Z - V1.Z);
  Result.W := V1.W + A * (V2.W - V1.W);
end;

class function TGenericVector4.Zero: TGenericVector4;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TGenericVector4.GetOne(const Index: TIndex): TGenericVector4;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Data[Index] := 1.0;
end;

{ TGenericMatrix2 ------------------------------------------------------------ }

class operator TGenericMatrix2. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix2): TGenericMatrix2;
begin
  Result.Data[0, 0] := A.Data[0, 0] + B.Data[0, 0];
  Result.Data[0, 1] := A.Data[0, 1] + B.Data[0, 1];

  Result.Data[1, 0] := A.Data[1, 0] + B.Data[1, 0];
  Result.Data[1, 1] := A.Data[1, 1] + B.Data[1, 1];
end;

class operator TGenericMatrix2. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericMatrix2): TGenericMatrix2;
begin
  Result.Data[0, 0] := A.Data[0, 0] - B.Data[0, 0];
  Result.Data[0, 1] := A.Data[0, 1] - B.Data[0, 1];

  Result.Data[1, 0] := A.Data[1, 0] - B.Data[1, 0];
  Result.Data[1, 1] := A.Data[1, 1] - B.Data[1, 1];
end;

class operator TGenericMatrix2. {$ifdef FPC}-{$else}Negative{$endif} (const M: TGenericMatrix2): TGenericMatrix2;
begin
  Result.Data[0, 0] := - M.Data[0, 0];
  Result.Data[0, 1] := - M.Data[0, 1];

  Result.Data[1, 0] := - M.Data[1, 0];
  Result.Data[1, 1] := - M.Data[1, 1];
end;

class operator TGenericMatrix2.{$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix2; const Scalar: TGenericScalar): TGenericMatrix2;
begin
  Result.Data[0, 0] := V.Data[0, 0] * Scalar;
  Result.Data[0, 1] := V.Data[0, 1] * Scalar;

  Result.Data[1, 0] := V.Data[1, 0] * Scalar;
  Result.Data[1, 1] := V.Data[1, 1] * Scalar;
end;

class operator TGenericMatrix2.{$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericMatrix2): TGenericMatrix2;
begin
  Result.Data[0, 0] := V.Data[0, 0] * Scalar;
  Result.Data[0, 1] := V.Data[0, 1] * Scalar;

  Result.Data[1, 0] := V.Data[1, 0] * Scalar;
  Result.Data[1, 1] := V.Data[1, 1] * Scalar;
end;

class operator TGenericMatrix2.{$ifdef FPC}*{$else}Multiply{$endif} (const M: TGenericMatrix2; const V: TGenericVector2): TGenericVector2;
begin
  Result.X := M.Data[0, 0] * V.X + M.Data[1, 0] * V.Y;
  Result.Y := M.Data[0, 1] * V.X + M.Data[1, 1] * V.Y;
end;

class operator TGenericMatrix2.{$ifdef FPC}*{$else}Multiply{$endif} (const M1, M2: TGenericMatrix2): TGenericMatrix2;
begin
  Result.Data[0, 0] := M1.Data[0, 0] * M2.Data[0, 0] + M1.Data[1, 0] * M2.Data[0, 1];
  Result.Data[1, 0] := M1.Data[0, 0] * M2.Data[1, 0] + M1.Data[1, 0] * M2.Data[1, 1];
  Result.Data[0, 1] := M1.Data[0, 1] * M2.Data[0, 0] + M1.Data[1, 1] * M2.Data[0, 1];
  Result.Data[1, 1] := M1.Data[0, 1] * M2.Data[1, 0] + M1.Data[1, 1] * M2.Data[1, 1];
end;

function TGenericMatrix2.ToString(const LineIndent: string): string;
begin
  Result := FormatDot('%s%f %f' + NL +
                      '%s%f %f',
   [LineIndent, Data[0, 0], Data[1, 0],
    LineIndent, Data[0, 1], Data[1, 1] ]);
end;

function TGenericMatrix2.ToRawString(const LineIndent: string): string;
begin
  Result := FormatDot('%s%g %g' + NL +
                      '%s%g %g',
   [LineIndent, Data[0, 0], Data[1, 0],
    LineIndent, Data[0, 1], Data[1, 1] ]);
end;

function TGenericMatrix2.GetItems(const AColumn, ARow: TIndex): TGenericScalar;
begin
  Result := Data[AColumn, ARow];
end;

procedure TGenericMatrix2.SetItems(const AColumn, ARow: TIndex; const Value: TGenericScalar);
begin
  Data[AColumn, ARow] := Value;
end;

function TGenericMatrix2.GetRows(const ARow: TIndex): TGenericVector2;
begin
  Result.X := Data[0, ARow];
  Result.Y := Data[1, ARow];
end;

procedure TGenericMatrix2.SetRows(const ARow: TIndex; const Value: TGenericVector2);
begin
  Data[0, ARow] := Value.X;
  Data[1, ARow] := Value.Y;
end;

function TGenericMatrix2.GetColumns(const AColumn: TIndex): TGenericVector2;
begin
  Result := TGenericVector2(Data[AColumn]); // TODO: should not require a typecast
end;

procedure TGenericMatrix2.SetColumns(const AColumn: TIndex; const Value: TGenericVector2);
begin
  TGenericVector2(Data[AColumn]) := Value; // TODO: should not require a typecast
end;

function MatrixDet2x2(const a1, a2, b1, b2: TGenericScalar): TGenericScalar;
{ Matrix is
  [ a1 a2 ]
  [ b1 b2 ]
}
begin
  Result := a1 * b2 - a2 * b1;
end;

function TGenericMatrix2.Determinant: TGenericScalar;
begin
  Result := MatrixDet2x2(
    Data[0, 0], Data[1, 0],
    Data[0, 1], Data[1, 1]
  );
end;

function TGenericMatrix2.Inverse(ADeterminant: TGenericScalar): TGenericMatrix2;
begin
  { Code adapted from FPC Matrix unit (same license as Castle Game Engine).

    Note that the code below is taken from FPC Matrix unit which has transposed
    matrices. So we should transpose the input and output, in general.
    But in this case, it's not needed, as the transpose of the inverse
    is the inverse of the transpose.
    Which means that

      Result = transpose(inverse(transpose(m))
             = transpose(transpose(inverse(m)))
             = just inverse(m))
  }

  ADeterminant := 1 / ADeterminant;
  Result.Data[0, 0] :=  Data[1, 1] * ADeterminant;
  Result.Data[0, 1] := -Data[0, 1] * ADeterminant;
  Result.Data[1, 0] := -Data[1, 0] * ADeterminant;
  Result.Data[1, 1] :=  Data[0, 0] * ADeterminant;
end;

function TGenericMatrix2.TryInverse(out MInverse: TGenericMatrix2): boolean;
var
  D: TGenericScalar;
begin
  D := Determinant;
  Result := not Math.IsZero(D);
  if Result then
    MInverse := Inverse(D);
end;

function TGenericMatrix2.Transpose: TGenericMatrix2;
begin
  Result.Data[0, 0] := Data[0, 0];
  Result.Data[0, 1] := Data[1, 0];

  Result.Data[1, 0] := Data[0, 1];
  Result.Data[1, 1] := Data[1, 1];
end;

class function TGenericMatrix2.Equals(const M1, M2: TGenericMatrix2): boolean;
begin
  Result :=
    SameValue(M1.Data[0, 0], M2.Data[0, 0]) and
    SameValue(M1.Data[0, 1], M2.Data[0, 1]) and

    SameValue(M1.Data[1, 0], M2.Data[1, 0]) and
    SameValue(M1.Data[1, 1], M2.Data[1, 1]);
end;

class function TGenericMatrix2.Equals(const M1, M2: TGenericMatrix2; const Epsilon: TGenericScalar): boolean;
begin
  if Epsilon = 0 then
    Result := CompareMem(@M1, @M2, SizeOf(M1))
  else
    Result :=
      (System.Abs(M1.Data[0, 0] - M2.Data[0, 0]) < Epsilon) and
      (System.Abs(M1.Data[0, 1] - M2.Data[0, 1]) < Epsilon) and

      (System.Abs(M1.Data[1, 0] - M2.Data[1, 0]) < Epsilon) and
      (System.Abs(M1.Data[1, 1] - M2.Data[1, 1]) < Epsilon);
end;

class function TGenericMatrix2.PerfectlyEquals(const M1, M2: TGenericMatrix2): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

class function TGenericMatrix2.Lerp(const A: TGenericScalar; const M1, M2: TGenericMatrix2): TGenericMatrix2;
begin
  Result.Data[0, 0] := M1.Data[0, 0] + A * (M2.Data[0, 0] - M1.Data[0, 0]);
  Result.Data[0, 1] := M1.Data[0, 1] + A * (M2.Data[0, 1] - M1.Data[0, 1]);

  Result.Data[1, 0] := M1.Data[1, 0] + A * (M2.Data[1, 0] - M1.Data[1, 0]);
  Result.Data[1, 1] := M1.Data[1, 1] + A * (M2.Data[1, 1] - M1.Data[1, 1]);
end;

class function TGenericMatrix2.Zero: TGenericMatrix2;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TGenericMatrix2.Identity: TGenericMatrix2;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result[0, 0] := 1;
  Result[1, 1] := 1;
end;

{ TGenericMatrix3 ------------------------------------------------------------ }

class operator TGenericMatrix3. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix3): TGenericMatrix3;
begin
  Result.Data[0, 0] := A.Data[0, 0] + B.Data[0, 0];
  Result.Data[0, 1] := A.Data[0, 1] + B.Data[0, 1];
  Result.Data[0, 2] := A.Data[0, 2] + B.Data[0, 2];

  Result.Data[1, 0] := A.Data[1, 0] + B.Data[1, 0];
  Result.Data[1, 1] := A.Data[1, 1] + B.Data[1, 1];
  Result.Data[1, 2] := A.Data[1, 2] + B.Data[1, 2];

  Result.Data[2, 0] := A.Data[2, 0] + B.Data[2, 0];
  Result.Data[2, 1] := A.Data[2, 1] + B.Data[2, 1];
  Result.Data[2, 2] := A.Data[2, 2] + B.Data[2, 2];
end;

class operator TGenericMatrix3. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericMatrix3): TGenericMatrix3;
begin
  Result.Data[0, 0] := A.Data[0, 0] - B.Data[0, 0];
  Result.Data[0, 1] := A.Data[0, 1] - B.Data[0, 1];
  Result.Data[0, 2] := A.Data[0, 2] - B.Data[0, 2];

  Result.Data[1, 0] := A.Data[1, 0] - B.Data[1, 0];
  Result.Data[1, 1] := A.Data[1, 1] - B.Data[1, 1];
  Result.Data[1, 2] := A.Data[1, 2] - B.Data[1, 2];

  Result.Data[2, 0] := A.Data[2, 0] - B.Data[2, 0];
  Result.Data[2, 1] := A.Data[2, 1] - B.Data[2, 1];
  Result.Data[2, 2] := A.Data[2, 2] - B.Data[2, 2];
end;

class operator TGenericMatrix3. {$ifdef FPC}-{$else}Negative{$endif} (const M: TGenericMatrix3): TGenericMatrix3;
begin
  Result.Data[0, 0] := - M.Data[0, 0];
  Result.Data[0, 1] := - M.Data[0, 1];
  Result.Data[0, 2] := - M.Data[0, 2];

  Result.Data[1, 0] := - M.Data[1, 0];
  Result.Data[1, 1] := - M.Data[1, 1];
  Result.Data[1, 2] := - M.Data[1, 2];

  Result.Data[2, 0] := - M.Data[2, 0];
  Result.Data[2, 1] := - M.Data[2, 1];
  Result.Data[2, 2] := - M.Data[2, 2];
end;

class operator TGenericMatrix3.{$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix3; const Scalar: TGenericScalar): TGenericMatrix3;
begin
  Result.Data[0, 0] := V.Data[0, 0] * Scalar;
  Result.Data[0, 1] := V.Data[0, 1] * Scalar;
  Result.Data[0, 2] := V.Data[0, 2] * Scalar;

  Result.Data[1, 0] := V.Data[1, 0] * Scalar;
  Result.Data[1, 1] := V.Data[1, 1] * Scalar;
  Result.Data[1, 2] := V.Data[1, 2] * Scalar;

  Result.Data[2, 0] := V.Data[2, 0] * Scalar;
  Result.Data[2, 1] := V.Data[2, 1] * Scalar;
  Result.Data[2, 2] := V.Data[2, 2] * Scalar;
end;

class operator TGenericMatrix3.{$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericMatrix3): TGenericMatrix3;
begin
  Result.Data[0, 0] := V.Data[0, 0] * Scalar;
  Result.Data[0, 1] := V.Data[0, 1] * Scalar;
  Result.Data[0, 2] := V.Data[0, 2] * Scalar;

  Result.Data[1, 0] := V.Data[1, 0] * Scalar;
  Result.Data[1, 1] := V.Data[1, 1] * Scalar;
  Result.Data[1, 2] := V.Data[1, 2] * Scalar;

  Result.Data[2, 0] := V.Data[2, 0] * Scalar;
  Result.Data[2, 1] := V.Data[2, 1] * Scalar;
  Result.Data[2, 2] := V.Data[2, 2] * Scalar;
end;

class operator TGenericMatrix3.{$ifdef FPC}*{$else}Multiply{$endif} (const M: TGenericMatrix3; const V: TGenericVector3): TGenericVector3;
begin
  Result.X := M.Data[0, 0] * V.X + M.Data[1, 0] * V.Y + M.Data[2, 0] * V.Z;
  Result.Y := M.Data[0, 1] * V.X + M.Data[1, 1] * V.Y + M.Data[2, 1] * V.Z;
  Result.Z := M.Data[0, 2] * V.X + M.Data[1, 2] * V.Y + M.Data[2, 2] * V.Z;
end;

class operator TGenericMatrix3.{$ifdef FPC}*{$else}Multiply{$endif} (const M1, M2: TGenericMatrix3): TGenericMatrix3;
begin
  Result.Data[0, 0] := M1.Data[0, 0] * M2.Data[0, 0] + M1.Data[1, 0] * M2.Data[0, 1] + M1.Data[2, 0] * M2.Data[0, 2];
  Result.Data[1, 0] := M1.Data[0, 0] * M2.Data[1, 0] + M1.Data[1, 0] * M2.Data[1, 1] + M1.Data[2, 0] * M2.Data[1, 2];
  Result.Data[2, 0] := M1.Data[0, 0] * M2.Data[2, 0] + M1.Data[1, 0] * M2.Data[2, 1] + M1.Data[2, 0] * M2.Data[2, 2];
  Result.Data[0, 1] := M1.Data[0, 1] * M2.Data[0, 0] + M1.Data[1, 1] * M2.Data[0, 1] + M1.Data[2, 1] * M2.Data[0, 2];
  Result.Data[1, 1] := M1.Data[0, 1] * M2.Data[1, 0] + M1.Data[1, 1] * M2.Data[1, 1] + M1.Data[2, 1] * M2.Data[1, 2];
  Result.Data[2, 1] := M1.Data[0, 1] * M2.Data[2, 0] + M1.Data[1, 1] * M2.Data[2, 1] + M1.Data[2, 1] * M2.Data[2, 2];
  Result.Data[0, 2] := M1.Data[0, 2] * M2.Data[0, 0] + M1.Data[1, 2] * M2.Data[0, 1] + M1.Data[2, 2] * M2.Data[0, 2];
  Result.Data[1, 2] := M1.Data[0, 2] * M2.Data[1, 0] + M1.Data[1, 2] * M2.Data[1, 1] + M1.Data[2, 2] * M2.Data[1, 2];
  Result.Data[2, 2] := M1.Data[0, 2] * M2.Data[2, 0] + M1.Data[1, 2] * M2.Data[2, 1] + M1.Data[2, 2] * M2.Data[2, 2];
end;

function TGenericMatrix3.ToString(const LineIndent: string): string;
begin
  Result := FormatDot('%s%f %f %f' + NL +
                      '%s%f %f %f' + NL +
                      '%s%f %f %f',
   [LineIndent, Data[0, 0], Data[1, 0], Data[2, 0],
    LineIndent, Data[0, 1], Data[1, 1], Data[2, 1],
    LineIndent, Data[0, 2], Data[1, 2], Data[2, 2] ]);
end;

function TGenericMatrix3.ToRawString(const LineIndent: string): string;
begin
  Result := FormatDot('%s%g %g %g' + NL +
                      '%s%g %g %g' + NL +
                      '%s%g %g %g',
   [LineIndent, Data[0, 0], Data[1, 0], Data[2, 0],
    LineIndent, Data[0, 1], Data[1, 1], Data[2, 1],
    LineIndent, Data[0, 2], Data[1, 2], Data[2, 2] ]);
end;

function TGenericMatrix3.GetItems(const AColumn, ARow: TIndex): TGenericScalar;
begin
  Result := Data[AColumn, ARow];
end;

procedure TGenericMatrix3.SetItems(const AColumn, ARow: TIndex; const Value: TGenericScalar);
begin
  Data[AColumn, ARow] := Value;
end;

function TGenericMatrix3.GetRows(const ARow: TIndex): TGenericVector3;
begin
  Result.X := Data[0, ARow];
  Result.Y := Data[1, ARow];
  Result.Z := Data[2, ARow];
end;

procedure TGenericMatrix3.SetRows(const ARow: TIndex; const Value: TGenericVector3);
begin
  Data[0, ARow] := Value.X;
  Data[1, ARow] := Value.Y;
  Data[2, ARow] := Value.Z;
end;

function TGenericMatrix3.GetColumns(const AColumn: TIndex): TGenericVector3;
begin
  Result := TGenericVector3(Data[AColumn]); // TODO: should not require a typecast
end;

procedure TGenericMatrix3.SetColumns(const AColumn: TIndex; const Value: TGenericVector3);
begin
  TGenericVector3(Data[AColumn]) := Value; // TODO: should not require a typecast
end;

function MatrixDet3x3(const a1, a2, a3, b1, b2, b3, c1, c2, c3: TGenericScalar): TGenericScalar;
begin
  Result := a1 * MatrixDet2x2 (b2, b3, c2, c3)
          - b1 * MatrixDet2x2 (a2, a3, c2, c3)
          + c1 * MatrixDet2x2 (a2, a3, b2, b3);
end;

function TGenericMatrix3.Determinant: TGenericScalar;
begin
  Result := MatrixDet3x3(
    Data[0, 0], Data[1, 0], Data[2, 0],
    Data[0, 1], Data[1, 1], Data[2, 1],
    Data[0, 2], Data[1, 2], Data[2, 2]
  );
end;

function TGenericMatrix3.Inverse(ADeterminant: TGenericScalar): TGenericMatrix3;
begin
  { Code adapted from FPC Matrix unit (same license as Castle Game Engine).
    This calculates the inverse of a transpose, but it doesn't matter,
    see MatrixInverse on TMatrix2 comments. }

  ADeterminant := 1 / ADeterminant;
  Result.Data[0,0] := (Data[1,1] * Data[2,2]-Data[2,1] * Data[1,2]) * ADeterminant;
  Result.Data[0,1] := -(Data[0,1] * Data[2,2]-Data[2,1] * Data[0,2]) * ADeterminant;
  Result.Data[0,2] := (Data[0,1] * Data[1,2]-Data[1,1] * Data[0,2]) * ADeterminant;
  Result.Data[1,0] := -(Data[1,0] * Data[2,2]-Data[2,0] * Data[1,2]) * ADeterminant;
  Result.Data[1,1] := (Data[0,0] * Data[2,2]-Data[2,0] * Data[0,2]) * ADeterminant;
  Result.Data[1,2] := -(Data[0,0] * Data[1,2]-Data[1,0] * Data[0,2]) * ADeterminant;
  Result.Data[2,0] := (Data[1,0] * Data[2,1]-Data[2,0] * Data[1,1]) * ADeterminant;
  Result.Data[2,1] := -(Data[0,0] * Data[2,1]-Data[2,0] * Data[0,1]) * ADeterminant;
  Result.Data[2,2] := (Data[0,0] * Data[1,1]-Data[1,0] * Data[0,1]) * ADeterminant;
end;

function TGenericMatrix3.TryInverse(out MInverse: TGenericMatrix3): boolean;
var
  D: TGenericScalar;
begin
  D := Determinant;
  Result := not Math.IsZero(D);
  if Result then
    MInverse := Inverse(D);
end;

function TGenericMatrix3.Transpose: TGenericMatrix3;
begin
  Result.Data[0, 0] := Data[0, 0];
  Result.Data[0, 1] := Data[1, 0];
  Result.Data[0, 2] := Data[2, 0];

  Result.Data[1, 0] := Data[0, 1];
  Result.Data[1, 1] := Data[1, 1];
  Result.Data[1, 2] := Data[2, 1];

  Result.Data[2, 0] := Data[0, 2];
  Result.Data[2, 1] := Data[1, 2];
  Result.Data[2, 2] := Data[2, 2];
end;

class function TGenericMatrix3.Equals(const M1, M2: TGenericMatrix3): boolean;
begin
  Result :=
    SameValue(M1.Data[0, 0], M2.Data[0, 0]) and
    SameValue(M1.Data[0, 1], M2.Data[0, 1]) and
    SameValue(M1.Data[0, 2], M2.Data[0, 2]) and

    SameValue(M1.Data[1, 0], M2.Data[1, 0]) and
    SameValue(M1.Data[1, 1], M2.Data[1, 1]) and
    SameValue(M1.Data[1, 2], M2.Data[1, 2]) and

    SameValue(M1.Data[2, 0], M2.Data[2, 0]) and
    SameValue(M1.Data[2, 1], M2.Data[2, 1]) and
    SameValue(M1.Data[2, 2], M2.Data[2, 2]);
end;

class function TGenericMatrix3.Equals(const M1, M2: TGenericMatrix3; const Epsilon: TGenericScalar): boolean;
begin
  if Epsilon = 0 then
    Result := CompareMem(@M1, @M2, SizeOf(M1))
  else
    Result :=
      (System.Abs(M1.Data[0, 0] - M2.Data[0, 0]) < Epsilon) and
      (System.Abs(M1.Data[0, 1] - M2.Data[0, 1]) < Epsilon) and
      (System.Abs(M1.Data[0, 2] - M2.Data[0, 2]) < Epsilon) and

      (System.Abs(M1.Data[1, 0] - M2.Data[1, 0]) < Epsilon) and
      (System.Abs(M1.Data[1, 1] - M2.Data[1, 1]) < Epsilon) and
      (System.Abs(M1.Data[1, 2] - M2.Data[1, 2]) < Epsilon) and

      (System.Abs(M1.Data[2, 0] - M2.Data[2, 0]) < Epsilon) and
      (System.Abs(M1.Data[2, 1] - M2.Data[2, 1]) < Epsilon) and
      (System.Abs(M1.Data[2, 2] - M2.Data[2, 2]) < Epsilon);
end;

class function TGenericMatrix3.PerfectlyEquals(const M1, M2: TGenericMatrix3): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

class function TGenericMatrix3.Lerp(const A: TGenericScalar; const M1, M2: TGenericMatrix3): TGenericMatrix3;
begin
  Result.Data[0, 0] := M1.Data[0, 0] + A * (M2.Data[0, 0] - M1.Data[0, 0]);
  Result.Data[0, 1] := M1.Data[0, 1] + A * (M2.Data[0, 1] - M1.Data[0, 1]);
  Result.Data[0, 2] := M1.Data[0, 2] + A * (M2.Data[0, 2] - M1.Data[0, 2]);

  Result.Data[1, 0] := M1.Data[1, 0] + A * (M2.Data[1, 0] - M1.Data[1, 0]);
  Result.Data[1, 1] := M1.Data[1, 1] + A * (M2.Data[1, 1] - M1.Data[1, 1]);
  Result.Data[1, 2] := M1.Data[1, 2] + A * (M2.Data[1, 2] - M1.Data[1, 2]);

  Result.Data[2, 0] := M1.Data[2, 0] + A * (M2.Data[2, 0] - M1.Data[2, 0]);
  Result.Data[2, 1] := M1.Data[2, 1] + A * (M2.Data[2, 1] - M1.Data[2, 1]);
  Result.Data[2, 2] := M1.Data[2, 2] + A * (M2.Data[2, 2] - M1.Data[2, 2]);
end;

class function TGenericMatrix3.Zero: TGenericMatrix3;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TGenericMatrix3.Identity: TGenericMatrix3;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result[0, 0] := 1;
  Result[1, 1] := 1;
  Result[2, 2] := 1;
end;

{ TGenericMatrix4 ------------------------------------------------------------ }

class operator TGenericMatrix4. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix4): TGenericMatrix4;
begin
  Result.Data[0, 0] := A.Data[0, 0] + B.Data[0, 0];
  Result.Data[0, 1] := A.Data[0, 1] + B.Data[0, 1];
  Result.Data[0, 2] := A.Data[0, 2] + B.Data[0, 2];
  Result.Data[0, 3] := A.Data[0, 3] + B.Data[0, 3];

  Result.Data[1, 0] := A.Data[1, 0] + B.Data[1, 0];
  Result.Data[1, 1] := A.Data[1, 1] + B.Data[1, 1];
  Result.Data[1, 2] := A.Data[1, 2] + B.Data[1, 2];
  Result.Data[1, 3] := A.Data[1, 3] + B.Data[1, 3];

  Result.Data[2, 0] := A.Data[2, 0] + B.Data[2, 0];
  Result.Data[2, 1] := A.Data[2, 1] + B.Data[2, 1];
  Result.Data[2, 2] := A.Data[2, 2] + B.Data[2, 2];
  Result.Data[2, 3] := A.Data[2, 3] + B.Data[2, 3];

  Result.Data[3, 0] := A.Data[3, 0] + B.Data[3, 0];
  Result.Data[3, 1] := A.Data[3, 1] + B.Data[3, 1];
  Result.Data[3, 2] := A.Data[3, 2] + B.Data[3, 2];
  Result.Data[3, 3] := A.Data[3, 3] + B.Data[3, 3];
end;

class operator TGenericMatrix4. {$ifdef FPC}-{$else}Subtract{$endif} (const A, B: TGenericMatrix4): TGenericMatrix4;
begin
  Result.Data[0, 0] := A.Data[0, 0] - B.Data[0, 0];
  Result.Data[0, 1] := A.Data[0, 1] - B.Data[0, 1];
  Result.Data[0, 2] := A.Data[0, 2] - B.Data[0, 2];
  Result.Data[0, 3] := A.Data[0, 3] - B.Data[0, 3];

  Result.Data[1, 0] := A.Data[1, 0] - B.Data[1, 0];
  Result.Data[1, 1] := A.Data[1, 1] - B.Data[1, 1];
  Result.Data[1, 2] := A.Data[1, 2] - B.Data[1, 2];
  Result.Data[1, 3] := A.Data[1, 3] - B.Data[1, 3];

  Result.Data[2, 0] := A.Data[2, 0] - B.Data[2, 0];
  Result.Data[2, 1] := A.Data[2, 1] - B.Data[2, 1];
  Result.Data[2, 2] := A.Data[2, 2] - B.Data[2, 2];
  Result.Data[2, 3] := A.Data[2, 3] - B.Data[2, 3];

  Result.Data[3, 0] := A.Data[3, 0] - B.Data[3, 0];
  Result.Data[3, 1] := A.Data[3, 1] - B.Data[3, 1];
  Result.Data[3, 2] := A.Data[3, 2] - B.Data[3, 2];
  Result.Data[3, 3] := A.Data[3, 3] - B.Data[3, 3];
end;

class operator TGenericMatrix4. {$ifdef FPC}-{$else}Negative{$endif} (const M: TGenericMatrix4): TGenericMatrix4;
begin
  Result.Data[0, 0] := - M.Data[0, 0];
  Result.Data[0, 1] := - M.Data[0, 1];
  Result.Data[0, 2] := - M.Data[0, 2];
  Result.Data[0, 3] := - M.Data[0, 3];

  Result.Data[1, 0] := - M.Data[1, 0];
  Result.Data[1, 1] := - M.Data[1, 1];
  Result.Data[1, 2] := - M.Data[1, 2];
  Result.Data[1, 3] := - M.Data[1, 3];

  Result.Data[2, 0] := - M.Data[2, 0];
  Result.Data[2, 1] := - M.Data[2, 1];
  Result.Data[2, 2] := - M.Data[2, 2];
  Result.Data[2, 3] := - M.Data[2, 3];

  Result.Data[3, 0] := - M.Data[3, 0];
  Result.Data[3, 1] := - M.Data[3, 1];
  Result.Data[3, 2] := - M.Data[3, 2];
  Result.Data[3, 3] := - M.Data[3, 3];
end;

class operator TGenericMatrix4.{$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix4; const Scalar: TGenericScalar): TGenericMatrix4;
begin
  Result.Data[0, 0] := V.Data[0, 0] * Scalar;
  Result.Data[0, 1] := V.Data[0, 1] * Scalar;
  Result.Data[0, 2] := V.Data[0, 2] * Scalar;
  Result.Data[0, 3] := V.Data[0, 3] * Scalar;

  Result.Data[1, 0] := V.Data[1, 0] * Scalar;
  Result.Data[1, 1] := V.Data[1, 1] * Scalar;
  Result.Data[1, 2] := V.Data[1, 2] * Scalar;
  Result.Data[1, 3] := V.Data[1, 3] * Scalar;

  Result.Data[2, 0] := V.Data[2, 0] * Scalar;
  Result.Data[2, 1] := V.Data[2, 1] * Scalar;
  Result.Data[2, 2] := V.Data[2, 2] * Scalar;
  Result.Data[2, 3] := V.Data[2, 3] * Scalar;

  Result.Data[3, 0] := V.Data[3, 0] * Scalar;
  Result.Data[3, 1] := V.Data[3, 1] * Scalar;
  Result.Data[3, 2] := V.Data[3, 2] * Scalar;
  Result.Data[3, 3] := V.Data[3, 3] * Scalar;
end;

class operator TGenericMatrix4.{$ifdef FPC}*{$else}Multiply{$endif} (const Scalar: TGenericScalar; const V: TGenericMatrix4): TGenericMatrix4;
begin
  Result.Data[0, 0] := V.Data[0, 0] * Scalar;
  Result.Data[0, 1] := V.Data[0, 1] * Scalar;
  Result.Data[0, 2] := V.Data[0, 2] * Scalar;
  Result.Data[0, 3] := V.Data[0, 3] * Scalar;

  Result.Data[1, 0] := V.Data[1, 0] * Scalar;
  Result.Data[1, 1] := V.Data[1, 1] * Scalar;
  Result.Data[1, 2] := V.Data[1, 2] * Scalar;
  Result.Data[1, 3] := V.Data[1, 3] * Scalar;

  Result.Data[2, 0] := V.Data[2, 0] * Scalar;
  Result.Data[2, 1] := V.Data[2, 1] * Scalar;
  Result.Data[2, 2] := V.Data[2, 2] * Scalar;
  Result.Data[2, 3] := V.Data[2, 3] * Scalar;

  Result.Data[3, 0] := V.Data[3, 0] * Scalar;
  Result.Data[3, 1] := V.Data[3, 1] * Scalar;
  Result.Data[3, 2] := V.Data[3, 2] * Scalar;
  Result.Data[3, 3] := V.Data[3, 3] * Scalar;
end;

function TGenericMatrix4.TransposeMultiply(const V: TGenericVector4): TGenericVector4;
begin
  Result.X := Data[0, 0] * V.X + Data[0, 1] * V.Y + Data[0, 2] * V.Z + Data[0, 3] * V.W;
  Result.Y := Data[1, 0] * V.X + Data[1, 1] * V.Y + Data[1, 2] * V.Z + Data[1, 3] * V.W;
  Result.Z := Data[2, 0] * V.X + Data[2, 1] * V.Y + Data[2, 2] * V.Z + Data[2, 3] * V.W;
  Result.W := Data[3, 0] * V.X + Data[3, 1] * V.Y + Data[3, 2] * V.Z + Data[3, 3] * V.W;
end;

class operator TGenericMatrix4.{$ifdef FPC}*{$else}Multiply{$endif} (const M: TGenericMatrix4; const V: TGenericVector4): TGenericVector4;
{var
  I, J: Integer;}
begin
  { We cannot have this uncommented (because we cannot use blindly Writeln,
    and depending on CastleLog for WritelnWarning makes FPC internal errors).
  if @V = @Result then
    Writeln('TGenericMatrix4.Multiply(vector) may be invalid: Argument and Result have the same address'); }

  {
  for I := 0 to 3 do
  begin
    Result.Data[I] := 0;
    for J := 0 to 3 do
      Result.Data[I] := Result.Data[I] + M.Data[J, I] * V.Data[J];
  end;

  Code expanded for the sake of speed:}

  Result.X := M.Data[0, 0] * V.X + M.Data[1, 0] * V.Y + M.Data[2, 0] * V.Z + M.Data[3, 0] * V.W;
  Result.Y := M.Data[0, 1] * V.X + M.Data[1, 1] * V.Y + M.Data[2, 1] * V.Z + M.Data[3, 1] * V.W;
  Result.Z := M.Data[0, 2] * V.X + M.Data[1, 2] * V.Y + M.Data[2, 2] * V.Z + M.Data[3, 2] * V.W;
  Result.W := M.Data[0, 3] * V.X + M.Data[1, 3] * V.Y + M.Data[2, 3] * V.Z + M.Data[3, 3] * V.W;
end;

class operator TGenericMatrix4.{$ifdef FPC}*{$else}Multiply{$endif} (const M1, M2: TGenericMatrix4): TGenericMatrix4;
{var
  I, J, K: Integer;}
begin
(*
  FillChar(Result, SizeOf(Result), 0);
  for I := 0 to 3 do { i = rows, j = columns }
    for J := 0 to 3 do
      for K := 0 to 3 do
        Result.Data[J, I] := Result.Data[J, I] + M1.Data[K, I] * M2.Data[J, K];
*)

  { This is code above expanded for speed sake
    (code generated by genMultMatrix) }
  Result.Data[0, 0] := M1.Data[0, 0] * M2.Data[0, 0] + M1.Data[1, 0] * M2.Data[0, 1] + M1.Data[2, 0] * M2.Data[0, 2] + M1.Data[3, 0] * M2.Data[0, 3];
  Result.Data[1, 0] := M1.Data[0, 0] * M2.Data[1, 0] + M1.Data[1, 0] * M2.Data[1, 1] + M1.Data[2, 0] * M2.Data[1, 2] + M1.Data[3, 0] * M2.Data[1, 3];
  Result.Data[2, 0] := M1.Data[0, 0] * M2.Data[2, 0] + M1.Data[1, 0] * M2.Data[2, 1] + M1.Data[2, 0] * M2.Data[2, 2] + M1.Data[3, 0] * M2.Data[2, 3];
  Result.Data[3, 0] := M1.Data[0, 0] * M2.Data[3, 0] + M1.Data[1, 0] * M2.Data[3, 1] + M1.Data[2, 0] * M2.Data[3, 2] + M1.Data[3, 0] * M2.Data[3, 3];
  Result.Data[0, 1] := M1.Data[0, 1] * M2.Data[0, 0] + M1.Data[1, 1] * M2.Data[0, 1] + M1.Data[2, 1] * M2.Data[0, 2] + M1.Data[3, 1] * M2.Data[0, 3];
  Result.Data[1, 1] := M1.Data[0, 1] * M2.Data[1, 0] + M1.Data[1, 1] * M2.Data[1, 1] + M1.Data[2, 1] * M2.Data[1, 2] + M1.Data[3, 1] * M2.Data[1, 3];
  Result.Data[2, 1] := M1.Data[0, 1] * M2.Data[2, 0] + M1.Data[1, 1] * M2.Data[2, 1] + M1.Data[2, 1] * M2.Data[2, 2] + M1.Data[3, 1] * M2.Data[2, 3];
  Result.Data[3, 1] := M1.Data[0, 1] * M2.Data[3, 0] + M1.Data[1, 1] * M2.Data[3, 1] + M1.Data[2, 1] * M2.Data[3, 2] + M1.Data[3, 1] * M2.Data[3, 3];
  Result.Data[0, 2] := M1.Data[0, 2] * M2.Data[0, 0] + M1.Data[1, 2] * M2.Data[0, 1] + M1.Data[2, 2] * M2.Data[0, 2] + M1.Data[3, 2] * M2.Data[0, 3];
  Result.Data[1, 2] := M1.Data[0, 2] * M2.Data[1, 0] + M1.Data[1, 2] * M2.Data[1, 1] + M1.Data[2, 2] * M2.Data[1, 2] + M1.Data[3, 2] * M2.Data[1, 3];
  Result.Data[2, 2] := M1.Data[0, 2] * M2.Data[2, 0] + M1.Data[1, 2] * M2.Data[2, 1] + M1.Data[2, 2] * M2.Data[2, 2] + M1.Data[3, 2] * M2.Data[2, 3];
  Result.Data[3, 2] := M1.Data[0, 2] * M2.Data[3, 0] + M1.Data[1, 2] * M2.Data[3, 1] + M1.Data[2, 2] * M2.Data[3, 2] + M1.Data[3, 2] * M2.Data[3, 3];
  Result.Data[0, 3] := M1.Data[0, 3] * M2.Data[0, 0] + M1.Data[1, 3] * M2.Data[0, 1] + M1.Data[2, 3] * M2.Data[0, 2] + M1.Data[3, 3] * M2.Data[0, 3];
  Result.Data[1, 3] := M1.Data[0, 3] * M2.Data[1, 0] + M1.Data[1, 3] * M2.Data[1, 1] + M1.Data[2, 3] * M2.Data[1, 2] + M1.Data[3, 3] * M2.Data[1, 3];
  Result.Data[2, 3] := M1.Data[0, 3] * M2.Data[2, 0] + M1.Data[1, 3] * M2.Data[2, 1] + M1.Data[2, 3] * M2.Data[2, 2] + M1.Data[3, 3] * M2.Data[2, 3];
  Result.Data[3, 3] := M1.Data[0, 3] * M2.Data[3, 0] + M1.Data[1, 3] * M2.Data[3, 1] + M1.Data[2, 3] * M2.Data[3, 2] + M1.Data[3, 3] * M2.Data[3, 3];
end;

function TGenericMatrix4.ToString(const LineIndent: string): string;
begin
  Result := FormatDot('%s%f %f %f %f' + NL +
                      '%s%f %f %f %f' + NL +
                      '%s%f %f %f %f' + NL +
                      '%s%f %f %f %f',
   [LineIndent, Data[0, 0], Data[1, 0], Data[2, 0], Data[3, 0],
    LineIndent, Data[0, 1], Data[1, 1], Data[2, 1], Data[3, 1],
    LineIndent, Data[0, 2], Data[1, 2], Data[2, 2], Data[3, 2],
    LineIndent, Data[0, 3], Data[1, 3], Data[2, 3], Data[3, 3] ]);
end;

function TGenericMatrix4.ToRawString(const LineIndent: string): string;
begin
  Result := FormatDot('%s%g %g %g %g' + NL +
                      '%s%g %g %g %g' + NL +
                      '%s%g %g %g %g' + NL +
                      '%s%g %g %g %g',
   [LineIndent, Data[0, 0], Data[1, 0], Data[2, 0], Data[3, 0],
    LineIndent, Data[0, 1], Data[1, 1], Data[2, 1], Data[3, 1],
    LineIndent, Data[0, 2], Data[1, 2], Data[2, 2], Data[3, 2],
    LineIndent, Data[0, 3], Data[1, 3], Data[2, 3], Data[3, 3] ]);
end;

function TGenericMatrix4.GetItems(const AColumn, ARow: TIndex): TGenericScalar;
begin
  Result := Data[AColumn, ARow];
end;

procedure TGenericMatrix4.SetItems(const AColumn, ARow: TIndex; const Value: TGenericScalar);
begin
  Data[AColumn, ARow] := Value;
end;

function TGenericMatrix4.GetRows(const ARow: TIndex): TGenericVector4;
begin
  Result.X := Data[0, ARow];
  Result.Y := Data[1, ARow];
  Result.Z := Data[2, ARow];
  Result.W := Data[3, ARow];
end;

procedure TGenericMatrix4.SetRows(const ARow: TIndex; const Value: TGenericVector4);
begin
  Data[0, ARow] := Value.X;
  Data[1, ARow] := Value.Y;
  Data[2, ARow] := Value.Z;
  Data[3, ARow] := Value.W;
end;

function TGenericMatrix4.GetColumns(const AColumn: TIndex): TGenericVector4;
begin
  Result := TGenericVector4(Data[AColumn]); // TODO: should not require a typecast
end;

procedure TGenericMatrix4.SetColumns(const AColumn: TIndex; const Value: TGenericVector4);
begin
  TGenericVector4(Data[AColumn]) := Value; // TODO: should not require a typecast
end;

function TGenericMatrix4.Determinant: TGenericScalar;
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: TGenericScalar;
begin
  a1 := Data[0, 0]; b1 := Data[0, 1];
  c1 := Data[0, 2]; d1 := Data[0, 3];

  a2 := Data[1, 0]; b2 := Data[1, 1];
  c2 := Data[1, 2]; d2 := Data[1, 3];

  a3 := Data[2, 0]; b3 := Data[2, 1];
  c3 := Data[2, 2]; d3 := Data[2, 3];

  a4 := Data[3, 0]; b4 := Data[3, 1];
  c4 := Data[3, 2]; d4 := Data[3, 3];

  Result := a1 * MatrixDet3x3 (b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * MatrixDet3x3 (a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * MatrixDet3x3 (a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * MatrixDet3x3 (a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;

function TGenericMatrix4.Inverse(ADeterminant: TGenericScalar): TGenericMatrix4;
begin
  { Code adapted from FPC Matrix unit (same license as Castle Game Engine).
    This calculates the inverse of a transpose, but it doesn't matter,
    see TGenericMatrix3.Inverse comments. }

  ADeterminant := 1 / ADeterminant;
  Result.Data[0,0] := ADeterminant * (Data[1,1] * (Data[2,2] * Data[3,3] - Data[2,3] * Data[3,2])+
                                      Data[1,2] * (Data[2,3] * Data[3,1] - Data[2,1] * Data[3,3])+
                                      Data[1,3] * (Data[2,1] * Data[3,2] - Data[2,2] * Data[3,1]));
  Result.Data[0,1] := ADeterminant * (Data[2,1] * (Data[0,2] * Data[3,3] - Data[0,3] * Data[3,2])+
                                      Data[2,2] * (Data[0,3] * Data[3,1] - Data[0,1] * Data[3,3])+
                                      Data[2,3] * (Data[0,1] * Data[3,2] - Data[0,2] * Data[3,1]));
  Result.Data[0,2] := ADeterminant * (Data[3,1] * (Data[0,2] * Data[1,3] - Data[0,3] * Data[1,2])+
                                      Data[3,2] * (Data[0,3] * Data[1,1] - Data[0,1] * Data[1,3])+
                                      Data[3,3] * (Data[0,1] * Data[1,2] - Data[0,2] * Data[1,1]));
  Result.Data[0,3] := ADeterminant * (Data[0,1] * (Data[1,3] * Data[2,2] - Data[1,2] * Data[2,3])+
                                      Data[0,2] * (Data[1,1] * Data[2,3] - Data[1,3] * Data[2,1])+
                                      Data[0,3] * (Data[1,2] * Data[2,1] - Data[1,1] * Data[2,2]));
  Result.Data[1,0] := ADeterminant * (Data[1,2] * (Data[2,0] * Data[3,3] - Data[2,3] * Data[3,0])+
                                      Data[1,3] * (Data[2,2] * Data[3,0] - Data[2,0] * Data[3,2])+
                                      Data[1,0] * (Data[2,3] * Data[3,2] - Data[2,2] * Data[3,3]));
  Result.Data[1,1] := ADeterminant * (Data[2,2] * (Data[0,0] * Data[3,3] - Data[0,3] * Data[3,0])+
                                      Data[2,3] * (Data[0,2] * Data[3,0] - Data[0,0] * Data[3,2])+
                                      Data[2,0] * (Data[0,3] * Data[3,2] - Data[0,2] * Data[3,3]));
  Result.Data[1,2] := ADeterminant * (Data[3,2] * (Data[0,0] * Data[1,3] - Data[0,3] * Data[1,0])+
                                      Data[3,3] * (Data[0,2] * Data[1,0] - Data[0,0] * Data[1,2])+
                                      Data[3,0] * (Data[0,3] * Data[1,2] - Data[0,2] * Data[1,3]));
  Result.Data[1,3] := ADeterminant * (Data[0,2] * (Data[1,3] * Data[2,0] - Data[1,0] * Data[2,3])+
                                      Data[0,3] * (Data[1,0] * Data[2,2] - Data[1,2] * Data[2,0])+
                                      Data[0,0] * (Data[1,2] * Data[2,3] - Data[1,3] * Data[2,2]));
  Result.Data[2,0] := ADeterminant * (Data[1,3] * (Data[2,0] * Data[3,1] - Data[2,1] * Data[3,0])+
                                      Data[1,0] * (Data[2,1] * Data[3,3] - Data[2,3] * Data[3,1])+
                                      Data[1,1] * (Data[2,3] * Data[3,0] - Data[2,0] * Data[3,3]));
  Result.Data[2,1] := ADeterminant * (Data[2,3] * (Data[0,0] * Data[3,1] - Data[0,1] * Data[3,0])+
                                      Data[2,0] * (Data[0,1] * Data[3,3] - Data[0,3] * Data[3,1])+
                                      Data[2,1] * (Data[0,3] * Data[3,0] - Data[0,0] * Data[3,3]));
  Result.Data[2,2] := ADeterminant * (Data[3,3] * (Data[0,0] * Data[1,1] - Data[0,1] * Data[1,0])+
                                      Data[3,0] * (Data[0,1] * Data[1,3] - Data[0,3] * Data[1,1])+
                                      Data[3,1] * (Data[0,3] * Data[1,0] - Data[0,0] * Data[1,3]));
  Result.Data[2,3] := ADeterminant * (Data[0,3] * (Data[1,1] * Data[2,0] - Data[1,0] * Data[2,1])+
                                      Data[0,0] * (Data[1,3] * Data[2,1] - Data[1,1] * Data[2,3])+
                                      Data[0,1] * (Data[1,0] * Data[2,3] - Data[1,3] * Data[2,0]));
  Result.Data[3,0] := ADeterminant * (Data[1,0] * (Data[2,2] * Data[3,1] - Data[2,1] * Data[3,2])+
                                      Data[1,1] * (Data[2,0] * Data[3,2] - Data[2,2] * Data[3,0])+
                                      Data[1,2] * (Data[2,1] * Data[3,0] - Data[2,0] * Data[3,1]));
  Result.Data[3,1] := ADeterminant * (Data[2,0] * (Data[0,2] * Data[3,1] - Data[0,1] * Data[3,2])+
                                      Data[2,1] * (Data[0,0] * Data[3,2] - Data[0,2] * Data[3,0])+
                                      Data[2,2] * (Data[0,1] * Data[3,0] - Data[0,0] * Data[3,1]));
  Result.Data[3,2] := ADeterminant * (Data[3,0] * (Data[0,2] * Data[1,1] - Data[0,1] * Data[1,2])+
                                      Data[3,1] * (Data[0,0] * Data[1,2] - Data[0,2] * Data[1,0])+
                                      Data[3,2] * (Data[0,1] * Data[1,0] - Data[0,0] * Data[1,1]));
  Result.Data[3,3] := ADeterminant * (Data[0,0] * (Data[1,1] * Data[2,2] - Data[1,2] * Data[2,1])+
                                      Data[0,1] * (Data[1,2] * Data[2,0] - Data[1,0] * Data[2,2])+
                                      Data[0,2] * (Data[1,0] * Data[2,1] - Data[1,1] * Data[2,0]));
end;

function TGenericMatrix4.TryInverse(out MInverse: TGenericMatrix4): boolean;
var
  D: TGenericScalar;
begin
  D := Determinant;
  Result := not Math.IsZero(D);
  if Result then
    MInverse := Inverse(D);
end;

function TGenericMatrix4.Transpose: TGenericMatrix4;
begin
  Result.Data[0, 0] := Data[0, 0];
  Result.Data[0, 1] := Data[1, 0];
  Result.Data[0, 2] := Data[2, 0];
  Result.Data[0, 3] := Data[3, 0];

  Result.Data[1, 0] := Data[0, 1];
  Result.Data[1, 1] := Data[1, 1];
  Result.Data[1, 2] := Data[2, 1];
  Result.Data[1, 3] := Data[3, 1];

  Result.Data[2, 0] := Data[0, 2];
  Result.Data[2, 1] := Data[1, 2];
  Result.Data[2, 2] := Data[2, 2];
  Result.Data[2, 3] := Data[3, 2];

  Result.Data[3, 0] := Data[0, 3];
  Result.Data[3, 1] := Data[1, 3];
  Result.Data[3, 2] := Data[2, 3];
  Result.Data[3, 3] := Data[3, 3];
end;

procedure TGenericMatrix4.RaisePositionTransformResultInvalid;
begin
  raise ETransformedResultInvalid.Create('3D point transformed by 4x4 matrix to a direction');
end;

function TGenericMatrix4.MultPoint(const Pt: TGenericVector3): TGenericVector3;
var
  Divisor: TGenericScalar;
begin
  { Simple implementation:
  Result := (M * Vector4(Pt, 1)).ToPosition; }

  { We cannot have this uncommented (because we cannot use blindly Writeln,
    and depending on CastleLog for WritelnWarning makes FPC internal errors).
  if @Pt = @Result then
    Writeln('TGenericMatrix4.MultPoint may be invalid: Argument and Result have the same address'); }

  Result.X := Data[0, 0] * Pt.X + Data[1, 0] * Pt.Y + Data[2, 0] * Pt.Z + Data[3, 0];
  Result.Y := Data[0, 1] * Pt.X + Data[1, 1] * Pt.Y + Data[2, 1] * Pt.Z + Data[3, 1];
  Result.Z := Data[0, 2] * Pt.X + Data[1, 2] * Pt.Y + Data[2, 2] * Pt.Z + Data[3, 2];

  { It looks strange, but the check below usually pays off.

    Tests: 17563680 calls of this proc within Creatures.PrepareRender
    inside "The Castle", gprof says that time without this check
    is 12.01 secs and with this checks it's 8.25.

    Why ? Because in 99% of situations, the conditions "(Data[0, 3] = 0) and ..."
    is true. Because that's how all usual matrices in 3D graphics
    (translation, rotation, scaling) look like.
    So usually I pay 4 comparisons (exact comparisons, not things like
    FloatsEqual) and I avoid 3 multiplications, 4 additions and
    3 divisions. }

  if not (
    (Data[0, 3] = 0) and
    (Data[1, 3] = 0) and
    (Data[2, 3] = 0) and
    (Data[3, 3] = 1)) then
  begin
    Divisor :=
      Data[0, 3] * Pt.X +
      Data[1, 3] * Pt.Y +
      Data[2, 3] * Pt.Z +
      Data[3, 3];
    if Math.IsZero(Divisor) then
      RaisePositionTransformResultInvalid;

    Divisor := 1 / Divisor;
    Result.X := Result.X * Divisor;
    Result.Y := Result.Y * Divisor;
    Result.Z := Result.Z * Divisor;
  end;
end;

function TGenericMatrix4.MultPoint(const Pt: TGenericVector2): TGenericVector2;
var
  V3: TGenericVector3;
  Res3D: TGenericVector3;
begin
  V3.X := Pt.X;
  V3.Y := Pt.Y;
  V3.Z := 0;

  Res3D := MultPoint(V3);

  Result.X := Res3D.X;
  Result.Y := Res3D.Y;
end;

procedure TGenericMatrix4.RaiseDirectionTransformedResultInvalid(const Divisor: TGenericScalar);
begin
  raise ETransformedResultInvalid.Create(Format(
    '3D direction transformed by 4x4 matrix to a point, with divisor = %f (%g), with matrix:',
    [Divisor, Divisor]) + NL + ToString);
end;

function TGenericMatrix4.MultDirection(const Dir: TGenericVector3): TGenericVector3;
var
  Divisor: TGenericScalar;
begin
  { We cannot have this uncommented (because we cannot use blindly Writeln,
    and depending on CastleLog for WritelnWarning makes FPC internal errors).
  if @Dir = @Result then
    Writeln('TGenericMatrix4.MultDirection may be invalid: Argument and Result have the same address'); }

  Result.X := Data[0, 0] * Dir.X + Data[1, 0] * Dir.Y + Data[2, 0] * Dir.Z;
  Result.Y := Data[0, 1] * Dir.X + Data[1, 1] * Dir.Y + Data[2, 1] * Dir.Z;
  Result.Z := Data[0, 2] * Dir.X + Data[1, 2] * Dir.Y + Data[2, 2] * Dir.Z;

  if not (
    (Data[0, 3] = 0) and
    (Data[1, 3] = 0) and
    (Data[2, 3] = 0) ) then
  begin
    Divisor := Data[0, 3] * Dir.X + Data[1, 3] * Dir.Y + Data[2, 3] * Dir.Z;
    if not Math.IsZero(Divisor) then
      RaiseDirectionTransformedResultInvalid(Divisor);
  end;
end;

function TGenericMatrix4.MultDirection(const Dir: TGenericVector2): TGenericVector2;
var
  V3: TGenericVector3;
  Res3D: TGenericVector3;
begin
  V3.X := Dir.X;
  V3.Y := Dir.Y;
  V3.Z := 0;

  Res3D := MultDirection(V3);

  Result.X := Res3D.X;
  Result.Y := Res3D.Y;
end;

class function TGenericMatrix4.Equals(const M1, M2: TGenericMatrix4): boolean;
begin
  Result :=
    SameValue(M1.Data[0, 0], M2.Data[0, 0]) and
    SameValue(M1.Data[0, 1], M2.Data[0, 1]) and
    SameValue(M1.Data[0, 2], M2.Data[0, 2]) and
    SameValue(M1.Data[0, 3], M2.Data[0, 3]) and

    SameValue(M1.Data[1, 0], M2.Data[1, 0]) and
    SameValue(M1.Data[1, 1], M2.Data[1, 1]) and
    SameValue(M1.Data[1, 2], M2.Data[1, 2]) and
    SameValue(M1.Data[1, 3], M2.Data[1, 3]) and

    SameValue(M1.Data[2, 0], M2.Data[2, 0]) and
    SameValue(M1.Data[2, 1], M2.Data[2, 1]) and
    SameValue(M1.Data[2, 2], M2.Data[2, 2]) and
    SameValue(M1.Data[2, 3], M2.Data[2, 3]) and

    SameValue(M1.Data[3, 0], M2.Data[3, 0]) and
    SameValue(M1.Data[3, 1], M2.Data[3, 1]) and
    SameValue(M1.Data[3, 2], M2.Data[3, 2]) and
    SameValue(M1.Data[3, 3], M2.Data[3, 3]);
end;

class function TGenericMatrix4.Equals(const M1, M2: TGenericMatrix4; const Epsilon: TGenericScalar): boolean;
begin
  if Epsilon = 0 then
    Result := CompareMem(@M1, @M2, SizeOf(M1))
  else
    Result :=
      (System.Abs(M1.Data[0, 0] - M2.Data[0, 0]) < Epsilon) and
      (System.Abs(M1.Data[0, 1] - M2.Data[0, 1]) < Epsilon) and
      (System.Abs(M1.Data[0, 2] - M2.Data[0, 2]) < Epsilon) and
      (System.Abs(M1.Data[0, 3] - M2.Data[0, 3]) < Epsilon) and

      (System.Abs(M1.Data[1, 0] - M2.Data[1, 0]) < Epsilon) and
      (System.Abs(M1.Data[1, 1] - M2.Data[1, 1]) < Epsilon) and
      (System.Abs(M1.Data[1, 2] - M2.Data[1, 2]) < Epsilon) and
      (System.Abs(M1.Data[1, 3] - M2.Data[1, 3]) < Epsilon) and

      (System.Abs(M1.Data[2, 0] - M2.Data[2, 0]) < Epsilon) and
      (System.Abs(M1.Data[2, 1] - M2.Data[2, 1]) < Epsilon) and
      (System.Abs(M1.Data[2, 2] - M2.Data[2, 2]) < Epsilon) and
      (System.Abs(M1.Data[2, 3] - M2.Data[2, 3]) < Epsilon) and

      (System.Abs(M1.Data[3, 0] - M2.Data[3, 0]) < Epsilon) and
      (System.Abs(M1.Data[3, 1] - M2.Data[3, 1]) < Epsilon) and
      (System.Abs(M1.Data[3, 2] - M2.Data[3, 2]) < Epsilon) and
      (System.Abs(M1.Data[3, 3] - M2.Data[3, 3]) < Epsilon);
end;

class function TGenericMatrix4.PerfectlyEquals(const M1, M2: TGenericMatrix4): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

class function TGenericMatrix4.Lerp(const A: TGenericScalar; const M1, M2: TGenericMatrix4): TGenericMatrix4;
begin
  Result.Data[0, 0] := M1.Data[0, 0] + A * (M2.Data[0, 0] - M1.Data[0, 0]);
  Result.Data[0, 1] := M1.Data[0, 1] + A * (M2.Data[0, 1] - M1.Data[0, 1]);
  Result.Data[0, 2] := M1.Data[0, 2] + A * (M2.Data[0, 2] - M1.Data[0, 2]);
  Result.Data[0, 3] := M1.Data[0, 3] + A * (M2.Data[0, 3] - M1.Data[0, 3]);

  Result.Data[1, 0] := M1.Data[1, 0] + A * (M2.Data[1, 0] - M1.Data[1, 0]);
  Result.Data[1, 1] := M1.Data[1, 1] + A * (M2.Data[1, 1] - M1.Data[1, 1]);
  Result.Data[1, 2] := M1.Data[1, 2] + A * (M2.Data[1, 2] - M1.Data[1, 2]);
  Result.Data[1, 3] := M1.Data[1, 3] + A * (M2.Data[1, 3] - M1.Data[1, 3]);

  Result.Data[2, 0] := M1.Data[2, 0] + A * (M2.Data[2, 0] - M1.Data[2, 0]);
  Result.Data[2, 1] := M1.Data[2, 1] + A * (M2.Data[2, 1] - M1.Data[2, 1]);
  Result.Data[2, 2] := M1.Data[2, 2] + A * (M2.Data[2, 2] - M1.Data[2, 2]);
  Result.Data[2, 3] := M1.Data[2, 3] + A * (M2.Data[2, 3] - M1.Data[2, 3]);

  Result.Data[3, 0] := M1.Data[3, 0] + A * (M2.Data[3, 0] - M1.Data[3, 0]);
  Result.Data[3, 1] := M1.Data[3, 1] + A * (M2.Data[3, 1] - M1.Data[3, 1]);
  Result.Data[3, 2] := M1.Data[3, 2] + A * (M2.Data[3, 2] - M1.Data[3, 2]);
  Result.Data[3, 3] := M1.Data[3, 3] + A * (M2.Data[3, 3] - M1.Data[3, 3]);
end;

class function TGenericMatrix4.Zero: TGenericMatrix4;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TGenericMatrix4.Identity: TGenericMatrix4;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result[0, 0] := 1;
  Result[1, 1] := 1;
  Result[2, 2] := 1;
  Result[3, 3] := 1;
end;

end.
