{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @section(Basic algorithms.) }

{$ifdef read_interface}

const
  { CountToUseSimpleSort internal comments:
    The real best value depends on the processor, compiler (FPC/Delphi,
    and their version) and what data is sorted
    (comparison time (how fast IsSmaller function is ?),
    copy time (how large is ArrRecordSize ?)).

    Tests: all for testSort compiled with -dRELEASE:

    @preformatted(
      CountToUseSimpleSort     `testSort 2 100000 100'  `testSort 2 1000000 100'
      0:                       0.29   1.33              3.60   19.37
      5:                       0.27   1.44              3.23   20.16
      10:                      0.27   1.26              3.05   18.57
      15:                      0.32   1.63              3.21   19.05
      20:                      0.24   1.31              3.04   19.05
      30:                      0.36   2.17              4.26   18.14
      40:                      0.24   1.25              2.87   17.90
      50:                      0.28   1.58              3.40   22.13
      100:                     0.35   1.95              4.08   26.31
      1000:                    1.80   12.44             18.97  140.50
    )

    Hmm. Tests above seem poor --- actually it's not clear what value
    of DefaultCountToUseSimpleSort is best, it's only clear that such
    value should be between 0..50. However they show that
    CountToUseSimpleSort doesn't improve time of @link(Sort) much.

    For now I chose 10. In the future maybe I'll do more testing
    with my program testSort and choose a better value here. }

  { When should the complicated sorting algorithm fallback to a simpler one.
    If number of items is <= CountToUseSimpleSort,
    then @link(Sort) will fallback to SimpleSort (= sort by choosing for now)
    instead of recursive QuickSort.
    Set CountToUseSimpleSort = 0 to make it never fall back to SimpleSort.

    By default this is DefaultCountToUseSimpleSort. }
  DefaultCountToUseSimpleSort = 10;

{ Sort given table of items.

  Sorts items ascending, that is Arr[FirstIndex] <= ... <= Arr[LastIndex].

  @param(Arr Pointer to items array in memory.)

  @param(ArrRecordSize Size (in bytes) of every item. This is the size
    of item that will be moved around in the memory.)

  @param(IsSmallerFunc Comparison function, should return is "A < B" true.

    I'm assuming here that IsSmallerFunc works like mathematical "<":
    it's not reflexive (IsSmallerFunc(A, A) = @false),
    for A <> B exactly one of IsSmallerFunc(A, B) or IsSmallerFunc(B, A) is true,
    and it's transitive.

    Note that IsSmallerFunc gets only the pointers to the items.
    These may be pointers to the Arr, or a pointer to internal temporary copy
    of some array item. So IsSmallerFunc cannot modify the item underneath
    (it would not work for internal copy, and also would create problems
    with types that need initialization/finalization since our internal
    copy is done using low-level memory copying.))

  @param(FirstIndex FirstIndex and LastIndex allow you to sort only
    given part of the array. We don't touch items outside of this range.
    Note that you could achieve the effect of FirstIndex > 0 also
    by increasing the Arr pointer, but FirstIndex is usually more comfortable.

    If FirstIndex > LastIndex, we do nothing.)

  @param(ArrStride Distance (in bytes) between array items in memory.
    If you don't provide it, we'll assume ArrRecordSize.
    ArrStride is useful if your array is interleaved with another array,
    and you want to keep the other data untouched by sorting.

    ArrStride must be > 0, actually it must be > ArrRecordSize for sensible
    behavior. (Unless ArrRecordSize is 0, then ArrStride may be 0 too.).)

  @groupBegin }
procedure Sort(Arr: pointer; ArrRecordSize: Cardinal;
  IsSmallerFunc: TIsSmallerFunc; IsSmallerFuncData: Pointer;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
procedure Sort(Arr: pointer; ArrRecordSize: Cardinal; ArrStride: integer;
  IsSmallerFunc: TIsSmallerFunc; IsSmallerFuncData: Pointer;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;

procedure SortByObject(Arr: pointer; ArrRecordSize: Cardinal;
  IsSmallerFunc: TIsSmallerFuncByObject; FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
procedure SortByObject(Arr: pointer; ArrRecordSize: Cardinal; ArrStride: integer;
  IsSmallerFunc: TIsSmallerFuncByObject; FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

procedure Sort(Arr: pointer; ArrRecordSize: Cardinal;
  IsSmallerFunc: TIsSmallerFunc; IsSmallerFuncData: Pointer;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort(Arr, ArrRecordSize, ArrRecordSize, IsSmallerFunc, IsSmallerFuncData,
   FirstIndex, LastIndex, CountToUseSimpleSort);
end;

procedure Sort(Arr: pointer; ArrRecordSize: Cardinal; ArrStride: integer;
  IsSmallerFunc: TIsSmallerFunc; IsSmallerFuncData: Pointer;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
{$I castleutils_implement_sort.inc}

procedure SortByObject(Arr: pointer; ArrRecordSize: Cardinal;
  IsSmallerFunc: TIsSmallerFuncByObject;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 SortByObject(Arr, ArrRecordSize, ArrRecordSize, IsSmallerFunc,
   FirstIndex, LastIndex, CountToUseSimpleSort);
end;

procedure SortByObject(Arr: pointer; ArrRecordSize: Cardinal; ArrStride: integer;
  IsSmallerFunc: TIsSmallerFuncByObject; FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
{ For the sake of maximum speed, I'm not calling here Sort()
  with some prepared non-object function that would pass values to a method.
  This could slow things down a little in extreme cases, since IsSmaller
  is called very often by Sort implementation. }
{$define SORT_BY_OBJECT}
{$I castleutils_implement_sort.inc}
{$undef SORT_BY_OBJECT}

{$endif read_implementation}
