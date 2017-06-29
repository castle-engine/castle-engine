// Generic types for FreeSparta.com and FreePascal!
// Original version by keeper89.blogspot.com, 2011
// FPC version by Maciej Izak (hnb), 2014

program TArrayProjectSingle;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Math, Types, Generics.Collections, Generics.Defaults;

function CompareIntReverse(constref Left, Right: Integer): Integer;
begin
  Result := TCompare.Integer(Right, Left);
end;

type
  TForCompare = class
  public
    function CompareIntReverseMethod(constref Left, Right: Integer): Integer;
  end;

function TForCompare.CompareIntReverseMethod(constref Left, Right: Integer): Integer;
begin
  Result := TCompare.Integer(Right, Left);
end;

procedure PrintMatrix(A: TIntegerDynArray);
var
  item: Integer;
begin
  for item in A do
    Write(item, ' ');
  Writeln; Writeln;
end;

var
  A: TIntegerDynArray;
  FoundIndex: PtrInt;
  ForCompareObj: TForCompare;
begin
  WriteLn('Working with TArray - one-dimensional integer array');
  WriteLn;

  // Fill a one-dimensional array of integers by random numbers [1 .. 10]
  A := TIntegerDynArray.Create(1, 6, 3, 2, 9);

  // Print out what happened
  Writeln('The original array:');
  PrintMatrix(A);

  // Sort ascending without comparator
  TArrayHelper<Integer>.Sort(A);
  Writeln('Ascending Sort without parameters:');
  PrintMatrix(A);

  // ! FPC don't support anonymous methods yet
  // Sort descending, the comparator is constructed
  // using an anonymous method
  //TArray.Sort<Integer>(A, TComparer<Integer>.Construct(
  //  function (const Left, Right: Integer): Integer
  //  begin
  //    Result := Math.CompareValue(Right, Left)
  //  end));

  // Sort descending, the comparator is constructed
  // using an method
  TArrayHelper<Integer>.Sort(A, TComparer<Integer>.Construct(
    ForCompareObj.CompareIntReverseMethod));
  Writeln('Descending by TComparer<Integer>.Construct(ForCompareObj.Method):');
  PrintMatrix(A);

  // Again sort ascending by using defaul
  TArrayHelper<Integer>.Sort(A, TComparer<Integer>.Default);
  Writeln('Ascending by TComparer<Integer>.Default:');
  PrintMatrix(A);

  // Again descending using own comparator function
  TArrayHelper<Integer>.Sort(A, TComparer<Integer>.Construct(CompareIntReverse));
  Writeln('Descending by TComparer<Integer>.Construct(CompareIntReverse):');
  PrintMatrix(A);

  // Searches for a nonexistent element
  Writeln('BinarySearch nonexistent element');
  if TArrayHelper<Integer>.BinarySearch(A, 5, FoundIndex) then
    Writeln('5 is found, its index ', FoundIndex)
  else
    Writeln('5 not found!');
  Writeln;

  // Search for an existing item with default comparer
  Writeln('BinarySearch for an existing item ');
  if TArrayHelper<Integer>.BinarySearch(A, 6, FoundIndex) then
    Writeln('6 is found, its index ', FoundIndex)
  else
    Writeln('6 not found!');
  Writeln;

  // Search for an existing item with custom comparer
  Writeln('BinarySearch for an existing item with custom comparer');
  if TArrayHelper<Integer>.BinarySearch(A, 6, FoundIndex,
    TComparer<Integer>.Construct(CompareIntReverse)) then
    Writeln('6 is found, its index ', FoundIndex)
  else
    Writeln('6 not found!');
  Writeln;

  Readln;
end.

