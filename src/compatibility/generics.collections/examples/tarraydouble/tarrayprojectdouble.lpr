// Generic types for FreeSparta.com and FreePascal!
// Original version by keeper89.blogspot.com, 2011
// FPC version by Maciej Izak (hnb), 2014

program TArrayProjectDouble;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Math, Types, Generics.Collections, Generics.Defaults;

type
  TDoubleIntegerArray = array of TIntegerDynArray;

procedure PrintMatrix(A: TDoubleIntegerArray);
var
  i, j: Integer;
begin
  for i := Low(A) to High(A) do
  begin
    for j := Low(A[0]) to High(A[0]) do
      Write(A[i, j]: 3, ' ');
    Writeln;
  end;
  Writeln; Writeln;
end;

function CustomCompare_1(constref Left, Right: TIntegerDynArray): Integer;
begin
  Result := TCompare.Integer(Right[0], Left[0]);
end;

function CustomCompare_2(constref Left, Right: TIntegerDynArray): Integer;
var
  i: Integer;
begin
  i := 0;
  repeat
    Result := TCompare.Integer(Right[i], Left[i]);
    Inc(i);
  until ((Result <> 0) or (i = Length(Left)));
end;

var
  A: TDoubleIntegerArray;
  FoundIndex: Integer;
  i, j: Integer;

begin
  WriteLn('Working with TArray - a two-dimensional integer array');
  WriteLn;

  // Fill integer array with random numbers [1 .. 50]
  SetLength(A, 4, 7);
  Randomize;
  for i := Low(A) to High(A) do
    for j := Low(A[0]) to High(A[0]) do
      A[i, j] := Math.RandomRange(1, 50);

  // Equate some of the elements for further "cascade" sorting
  A[1, 0] := A[0, 0];
  A[2, 0] := A[0, 0];
  A[1, 1] := A[0, 1];

  // Print out what happened
  Writeln('The original array:');
  PrintMatrix(A);

  // ! FPC don't support anonymous methods yet
  //TArray.Sort<TIntegerDynArray>(A, TComparer<TIntegerDynArray>.Construct(
  //  function (const Left, Right: TIntegerDynArray): Integer
  //  begin
  //    Result := Right[0] - Left[0];
  //  end));
  // Sort descending 1st column, with cutom comparer_1
  TArrayHelper<TIntegerDynArray>.Sort(A, TComparer<TIntegerDynArray>.Construct(
    CustomCompare_1));
  Writeln('Descending in column 1:');
  PrintMatrix(A);

  // Sort descending 1st column "cascade" -
  // If the line items are equal, compare neighboring
  TArrayHelper<TIntegerDynArray>.Sort(A, TComparer<TIntegerDynArray>.Construct(
    CustomCompare_2));
  Writeln('Cascade sorting, starting from the 1st column:');
  PrintMatrix(A);

  Readln;
end.

