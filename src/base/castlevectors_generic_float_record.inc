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

{$ifdef read_interface}

type
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
    Data: array [TIndex] of TGenericScalar;
    procedure Init(const X: TGenericScalar = 0; const Y: TGenericScalar = 0; const Z: TGenericScalar = 0);
    function Length: TGenericScalar;
    function Normalize: TGenericVector3;
    function ToString: string;
    property Items [const Index: TIndex]: TGenericScalar read GetItems; default;
    class operator {$ifdef FPC}+{$else}Add{$endif} (const A, B: TGenericVector3): TGenericVector3;
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const V: TGenericVector3; const Scalar: TGenericScalar): TGenericVector3;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TGenericVector3 ------------------------------------------------------------ }

procedure TGenericVector3.Init(const X: TGenericScalar = 0; const Y: TGenericScalar = 0; const Z: TGenericScalar = 0);
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

{$endif read_implementation}
