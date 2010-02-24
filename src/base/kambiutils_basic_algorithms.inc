{
  Copyright 2002,2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @section(Basic algorithms)

  Not much for now. }

{$ifdef read_interface}

const
  { If number of items is <= CountToUseSimpleSort,
    then @link(Sort) will fallback to SimpleSort (= sort by choosing for now)
    instead of recursive QuickSort.
    So CountToUseSimpleSort = 0 means that it will never fall back
    to SimpleSort.

    DefaultCountToUseSimpleSort is default value of parameter
    CountToUseSimpleSort. Note that it's impossible to just
    experimentally set DefaultCountToUseSimpleSort to a "best" value,
    because what value is best depends on processor, compiler (FPC/Delphi,
    and what version) and what data is sorted
    (comparison time (how fast IsSmaller function is ?),
    copy time (how big is ArrRecordSize ?)).

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
  DefaultCountToUseSimpleSort = 10;

{ This Sorts table of any items.

  You must give Arr --- pointer to items array in memory,
  and ArrRecordSize --- size (in bytes) of every item,
  IsSmaller -- function that for two arguments should return
  is "a < b" true (i.e. I'm assuming here that IsSmaller works
  like mathematical "<" --- it's not reflexive (not IsSmaller(a, a)),
  for a <> b either IsSmaller(a, b) or IsSmaller(b, a) (but not both),
  it's transitive.

  Using parameters FirstIndex and LastIndex you can sort
  only part of the array, items outside given range will be be even read.
  (actually you could achieve effect of FirstIndex by simply shifting
  the pointer Arr, but using FirstIndex is more comfortable).
  If FirstIndex>LastIndex, this is NOP.

  It sorts items ascending, i.e. Arr[FirstIndex] <= ... <= Arr[LastIndex].
  (where meaning of "<=" is implicated by meaning of "<" which is given
  by IsSmaller).

  TODO: Polish below.

  Jeszcze slowo o wskaznikach jakie bedzie dostawac IsSmallerFunc : mianowicie
  te wskazniki niekoniecznie zawsze beda rowne PointerAdd(Arr, ArrStride*n) dla
  jakiegos n. Byc moze wybierajac np. element dzielacy dla quicksorta skopiujemy
  sobie jego wartosc na chwile i wtedy wskaznik bedzie wskazywal na lokalna
  kopie danej wartosci ? Kopiowanie bedziemy musieli robic metodami nisko-poziomowymi
  wiec i tak nie ma problemu z rzeczami ktore wymagaja Initialize/Finalize.
  Chociaz moze byc w zwiazku z tym problem jesli IsSmallerFunc bedzie moglo
  przy okazji zmodyfikowac jakos zawartosc danego rekordu - no coz, nie moze
  tego robic ! IsSmaller powinno byc ladna funkcja.

  Ze wzgledu na to ze w srodku @name samo wykonuje przestawianie elementow
  na liscie wiec argument ArrRecordSize nie moze byc oszukany - musi byc wiekszy
  od zera i musi rzeczywiscie obejmowac dokladnie zawartosc rekordu.
  Z drugiej strony mozesz zawsze uzyc wersji z ArrStride jezeli rekordy
  ktore chcesz przestawiac sa przeplecione z innymi danymi ktorych nie chcesz
  przestawiac razem z rekordami. ArrStride mowi co ile bajtow jest nastepny rekord.
  Domyslnie ArrStride to ArrRecordSize. Oczywiscie mozna sobie zrobic krzywde jezeli podasz
  np. ArrRecordSize>0 i ArrStride w zakresie [0..ArrRecordSize).

  W klasach TDyn*Array zaimplementowalem wersje tej procedury bezpieczna
  ze wzgledu na typy - tam podajesz tylko funkcje typu
  function (const a,b:<typ>): boolean [of object]; gdzie <typ> to typ elementow
  tablicy.

  TODO: chwilowo ArrStride nie moze byc ujemne (to moze czasem pozwolic na pewne sztuczki).
  TODO: zrobic tez wersje ktora zamiast ArrRecordSize pobiera SwapItemsFunc.
}
procedure Sort(Arr: pointer; ArrRecordSize: Cardinal;
  IsSmallerFunc: TIsSmallerFunc; IsSmallerFuncData: Pointer;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer {$ifdef DEFPARS} = DefaultCountToUseSimpleSort {$endif}); overload;

procedure Sort(Arr: pointer; ArrRecordSize: Cardinal; ArrStride: integer;
  IsSmallerFunc: TIsSmallerFunc; IsSmallerFuncData: Pointer;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer {$ifdef DEFPARS} = DefaultCountToUseSimpleSort {$endif}); overload;

procedure SortByObject(Arr: pointer; ArrRecordSize: Cardinal;
  IsSmallerFunc: TIsSmallerFuncByObject; FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer {$ifdef DEFPARS} = DefaultCountToUseSimpleSort {$endif}); overload;
procedure SortByObject(Arr: pointer; ArrRecordSize: Cardinal; ArrStride: integer;
  IsSmallerFunc: TIsSmallerFuncByObject; FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer {$ifdef DEFPARS} = DefaultCountToUseSimpleSort {$endif}); overload;

{ TODO: special versions of Sort for standard predefined types
  (integers, floats, strings) would be nice. }

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
{$I kambiutils_implement_sort.inc}

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
{$I kambiutils_implement_sort.inc}
{$undef SORT_BY_OBJECT}

{$endif read_implementation}
