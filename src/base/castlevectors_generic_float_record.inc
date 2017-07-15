(*Interface and implementation of a TGenericVector* types,
  that can use any floating-point precision (Single, Double, Extended).

  ----------------------------------------------------------------------------
  Note that we cannot use proper generics to define a TGenericVector3<T>,
  because this cannot work under Delphi:

    function TGenericVector3 {$ifndef FPC_OBJFPC}<T>{$endif}.Length: T;
    begin
      Result := Sqrt(
        (Data[0] * Data[0]) +
        (Data[1] * Data[1]) +
        (Data[2] * Data[2])
      );
    end;

  Delphi wants to check the correctness when reading the generic,
  and it will not allow artihmetic operations on Data[0],
  and it cannot narrow the type to something where artihmetic operations are OK.
  See
  https://stackoverflow.com/questions/40059579/arithmetic-operations-with-generic-types-in-delphi

  Under FPC it works, it seems that FPC checks Sqrt() correctness only later.
  But FPC doesn't allow "Sqr(Data[0])" too, so it's possible that the lack
  of an early Sqrt check is just an FPC bug.
*)

type
  { Vector of 2 floating-point values.
    @seealso TGenericVector3 }
  TGenericVector2 = record
  public
    type
      TIndex = 0..1;
  strict private
    function GetItems(const Index: TIndex): TGenericScalar;
  public
    var
      Data: array [TIndex] of TGenericScalar;
    const
      Zero: TGenericVector2 = (Data: (0, 0));
    procedure Init(const X, Y: TGenericScalar);
    function Length: TGenericScalar;
    function Normalize: TGenericVector2;
    function ToString: string;
    property Items [const Index: TIndex]: TGenericScalar read GetItems; default;
    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector2): TGenericVector2;
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector2; const Scalar: TGenericScalar): TGenericVector2;
  end;

  { Vector of 3 floating-point values.

    This is generic type (although not using "proper" Pascal generics
    for implementation reasons). In has two actual uses:

    @orderedList(
      @itemSpacing Compact
      @item @link(TVector3), a vector of 3 Single values (floats with single precision),
      @item @link(TVector3d), a vector of 3 Double values (floats with double precision).
    )

    The type TGenericScalar is, accordingly,
    Single or Double for @link(TVector3) or @link(TVector3d).
  }
  TGenericVector3 = record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const Index: TIndex): TGenericScalar;
  public
    var
      Data: array [TIndex] of TGenericScalar;
    const
      Zero: TGenericVector3 = (Data: (0, 0, 0));
    procedure Init(const X, Y, Z: TGenericScalar);
    function Length: TGenericScalar;
    function Normalize: TGenericVector3;
    function ToString: string;
    property Items [const Index: TIndex]: TGenericScalar read GetItems; default;
    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector3): TGenericVector3;
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector3; const Scalar: TGenericScalar): TGenericVector3;
  end;

  { Vector of 4 floating-point values.
    @seealso TGenericVector3 }
  TGenericVector4 = record
  public
    type
      TIndex = 0..3;
  strict private
    function GetItems(const Index: TIndex): TGenericScalar;
  public
    var
      Data: array [TIndex] of TGenericScalar;
    const
      Zero: TGenericVector3 = (Data: (0, 0, 0));
    procedure Init(const X, Y, Z, W: TGenericScalar);
    function Length: TGenericScalar;
    function ToString: string;
    property Items [const Index: TIndex]: TGenericScalar read GetItems; default;
    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector4): TGenericVector4;
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector4; const Scalar: TGenericScalar): TGenericVector4;
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
      @item @link(TMatrix3d), a matrix of 3 Double values (floats with double precision).
    )

    The type TGenericScalar is, accordingly,
    Single or Double for @link(TMatrix3) or @link(TMatrix3d).
  }
  TGenericMatrix3 = record
  public
    type
      TIndex = 0..2;
  public
    var
      Data: array [TIndex, TIndex] of TGenericScalar;
    const
      Zero    : TGenericMatrix3 = (Data: ((0, 0, 0), (0, 0, 0), (0, 0, 0)));
      Identity: TGenericMatrix3 = (Data: ((1, 0, 0), (0, 1, 0), (0, 0, 1)));
    function ToString: string;
    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix3): TGenericMatrix3;
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix3; const Scalar: TGenericScalar): TGenericMatrix3;
  end;

  { 4x4 matrix of floating-point values.
    @seealso TGenericMatrix3 }
  TGenericMatrix4 = record
  public
    type
      TIndex = 0..3;
  public
    var
      Data: array [TIndex, TIndex] of TGenericScalar;
    const
      Zero    : TGenericMatrix4 = (Data: ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)));
      Identity: TGenericMatrix4 = (Data: ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1)));
    function ToString: string;
    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericMatrix4): TGenericMatrix4;
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericMatrix4; const Scalar: TGenericScalar): TGenericMatrix4;

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
  end;

implementation

uses Math,
  CastleUtils;

{ TGenericVector2 ------------------------------------------------------------ }

procedure TGenericVector2.Init(const X, Y: TGenericScalar);
begin
  Data[0] := X;
  Data[1] := Y;
end;

function TGenericVector2.Length: TGenericScalar;
begin
  Result := Sqrt(
    (Data[0] * Data[0]) +
    (Data[1] * Data[1])
  );
end;

function TGenericVector2.Normalize: TGenericVector2;
var
  L: TGenericScalar;
begin
  L := Length;
  if L = 0 then
    Result.Data := Data
  else
  begin
    Result.Data[0] := Data[0] / Length;
    Result.Data[1] := Data[1] / Length;
  end;
end;

function TGenericVector2.ToString: string;
begin
  Result := Format('%f %f', [Data[0], Data[1]]);
end;

function TGenericVector2.GetItems(const Index: TIndex): TGenericScalar;
begin
  Result := Data[Index];
end;

class operator TGenericVector2. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector2): TGenericVector2;
begin
  Result.Data[0] := A[0] + B[0];
  Result.Data[1] := A[1] + B[1];
end;

class operator TGenericVector2. {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector2; const Scalar: TGenericScalar): TGenericVector2;
begin
  Result.Data[0] := V[0] * Scalar;
  Result.Data[1] := V[1] * Scalar;
end;

{ TGenericVector3 ------------------------------------------------------------ }

procedure TGenericVector3.Init(const X, Y, Z: TGenericScalar);
begin
  Data[0] := X;
  Data[1] := Y;
  Data[2] := Z;
end;

function TGenericVector3.Length: TGenericScalar;
begin
  Result := Sqrt(
    (Data[0] * Data[0]) +
    (Data[1] * Data[1]) +
    (Data[2] * Data[2])
  );
end;

function TGenericVector3.Normalize: TGenericVector3;
var
  L: TGenericScalar;
begin
  L := Length;
  if L = 0 then
    Result.Data := Data
  else
  begin
    Result.Data[0] := Data[0] / Length;
    Result.Data[1] := Data[1] / Length;
    Result.Data[2] := Data[2] / Length;
  end;
end;

function TGenericVector3.ToString: string;
begin
  Result := Format('%f %f %f', [Data[0], Data[1], Data[2]]);
end;

function TGenericVector3.GetItems(const Index: TIndex): TGenericScalar;
begin
  Result := Data[Index];
end;

class operator TGenericVector3. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector3): TGenericVector3;
begin
  Result.Data[0] := A[0] + B[0];
  Result.Data[1] := A[1] + B[1];
  Result.Data[2] := A[2] + B[2];
end;

class operator TGenericVector3. {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector3; const Scalar: TGenericScalar): TGenericVector3;
begin
  Result.Data[0] := V[0] * Scalar;
  Result.Data[1] := V[1] * Scalar;
  Result.Data[2] := V[2] * Scalar;
end;

{ TGenericVector4 ------------------------------------------------------------ }

procedure TGenericVector4.Init(const X, Y, Z, W: TGenericScalar);
begin
  Data[0] := X;
  Data[1] := Y;
  Data[2] := Z;
  Data[3] := Z;
end;

function TGenericVector4.Length: TGenericScalar;
begin
  Result := Sqrt(
    (Data[0] * Data[0]) +
    (Data[1] * Data[1]) +
    (Data[2] * Data[2])
  );
end;

function TGenericVector4.ToString: string;
begin
  Result := Format('%f %f %f', [Data[0], Data[1], Data[2]]);
end;

function TGenericVector4.GetItems(const Index: TIndex): TGenericScalar;
begin
  Result := Data[Index];
end;

class operator TGenericVector4. {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector4): TGenericVector4;
begin
  Result.Data[0] := A[0] + B[0];
  Result.Data[1] := A[1] + B[1];
  Result.Data[2] := A[2] + B[2];
end;

class operator TGenericVector4. {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector4; const Scalar: TGenericScalar): TGenericVector4;
begin
  Result.Data[0] := V[0] * Scalar;
  Result.Data[1] := V[1] * Scalar;
  Result.Data[2] := V[2] * Scalar;
end;

{ TGenericMatrix3 ------------------------------------------------------------ }

function TGenericMatrix3.ToString: string;
begin
  Result := Format('%f %f %f' + NL +
                   '%f %f %f' + NL +
                   '%f %f %f',
   [Data[0, 0], Data[1, 0], Data[2, 0],
    Data[0, 1], Data[1, 1], Data[2, 1],
    Data[0, 2], Data[1, 2], Data[2, 2] ]);
end;

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

{ TGenericMatrix4 ------------------------------------------------------------ }

function TGenericMatrix4.ToString: string;
begin
  Result := Format('%f %f %f %f' + NL +
                   '%f %f %f %f' + NL +
                   '%f %f %f %f' + NL +
                   '%f %f %f %f',
   [Data[0, 0], Data[1, 0], Data[2, 0], Data[3, 0],
    Data[0, 1], Data[1, 1], Data[2, 1], Data[3, 1],
    Data[0, 2], Data[1, 2], Data[2, 2], Data[3, 2],
    Data[0, 3], Data[1, 3], Data[2, 3], Data[3, 3] ]);
end;

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

function TGenericMatrix4.MultPoint(const Pt: TGenericVector3): TGenericVector3;
var
  Divisor: TGenericScalar;
begin
  { Simple implementation:
  Result := Vector3SinglePoint(MatrixMultVector(m, Vector4Single(pt))); }

  Result.Data[0] := Data[0, 0] * Pt.Data[0] + Data[1, 0] * Pt.Data[1] + Data[2, 0] * Pt.Data[2] + Data[3, 0];
  Result.Data[1] := Data[0, 1] * Pt.Data[0] + Data[1, 1] * Pt.Data[1] + Data[2, 1] * Pt.Data[2] + Data[3, 1];
  Result.Data[2] := Data[0, 2] * Pt.Data[0] + Data[1, 2] * Pt.Data[1] + Data[2, 2] * Pt.Data[2] + Data[3, 2];

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
      Data[0, 3] * Pt.Data[0] +
      Data[1, 3] * Pt.Data[1] +
      Data[2, 3] * Pt.Data[2] +
      Data[3, 3];
    if IsZero(Divisor) then
      raise ETransformedResultInvalid.Create('3D point transformed by 4x4 matrix to a direction');

    Result.Data[0] /= Divisor;
    Result.Data[1] /= Divisor;
    Result.Data[2] /= Divisor;
  end;
end;

function TGenericMatrix4.MultPoint(const Pt: TGenericVector2): TGenericVector2;
var
  V3: TGenericVector3;
  Res3D: TGenericVector3;
begin
  V3.Data[0] := Pt.Data[0];
  V3.Data[1] := Pt.Data[1];
  V3.Data[2] := 0;

  Res3D := MultPoint(V3);

  Result.Data[0] := Res3D[0];
  Result.Data[1] := Res3D[1];
end;

function TGenericMatrix4.MultDirection(const Dir: TGenericVector3): TGenericVector3;
var
  Divisor: TGenericScalar;
begin
  Result.Data[0] := Data[0, 0] * Dir[0] + Data[1, 0] * Dir[1] + Data[2, 0] * Dir[2];
  Result.Data[1] := Data[0, 1] * Dir[0] + Data[1, 1] * Dir[1] + Data[2, 1] * Dir[2];
  Result.Data[2] := Data[0, 2] * Dir[0] + Data[1, 2] * Dir[1] + Data[2, 2] * Dir[2];

  if not (
    (Data[0, 3] = 0) and
    (Data[1, 3] = 0) and
    (Data[2, 3] = 0) ) then
  begin
    Divisor := Data[0, 3] * Dir[0] + Data[1, 3] * Dir[1] + Data[2, 3] * Dir[2];
    if not IsZero(Divisor) then
      raise ETransformedResultInvalid.Create(Format(
        '3D direction transformed by 4x4 matrix to a point, with divisor = %f (%g), with matrix:',
        [Divisor, Divisor]) + NL + ToString);
  end;
end;

function TGenericMatrix4.MultDirection(const Dir: TGenericVector2): TGenericVector2;
var
  V3: TGenericVector3;
  Res3D: TGenericVector3;
begin
  V3.Data[0] := Dir.Data[0];
  V3.Data[1] := Dir.Data[1];
  V3.Data[2] := 0;

  Res3D := MultDirection(V3);

  Result.Data[0] := Res3D[0];
  Result.Data[1] := Res3D[1];
end;

end.
