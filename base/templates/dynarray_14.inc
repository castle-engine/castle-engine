{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Before including this file, define types :
    TDynArrayItem_14,
    PDynArrayItem_14 -- this must be ^TDynArrayItem_14.
  (in FPC you can also use macros to define these identifiers)

  Including this file will define types :
    TDynArray_14 -- a class that represents an array of TDynArrayItem_14,
    TInfiniteArray_14 -- array[0..<infinite>]of TDynArrayItem_14,
    PInfiniteArray_14 -- ^TInfiniteArray_14,
    TDynArrayItemIsSmallerFunc_14,
    TDynArrayItemIsSmallerFuncByObject_14,
      ( function (const a, b: TDynArrayItem_14): boolean [of object] );

  Jezeli symbol DYNARRAY_14_IS_STRUCT jest zdefiniowany nie ma wlasciwosci Values - powinienes
  zawsze uzywac wlasciwosci .Items[] (ktora nie jest wlasciwoscia tablicowa,
  ona jest wskaznikiem na tablice, wiec nie moze byc defautowa...)
  Items to wskaznik na tablice elementow typu TDynArrayItem_14. Mozesz na niej dowolnie
  operowac (pamietaj ze RangeChecking nie dziala - nie ma jak); takie zachowanie jest
  dobre gdy TArrayItem to rekord lub inna tablica. Wlasciwosc Values nie bedzie istniala
  (moglaby istniec; ale jej uzycie czesciej prowadziloby do bledow niz do
  wygodnych zastosowan : gdyby TDynArrayItem_14 byl np. rekordem record x, y: integer end
  to A[10].x zostaloby odczytane jako A.GetValues(10).x := 10 a wiec zmieniloby wartosc
  tymczasowej kopii wartosci tablicy 10, a wiec de facto nie zmieniloby niczego...
  podobnie wyrazenie @@A[10].x byloby juz arcy-niebezpieczne bo zwracaloby
  wskaznik poprawny tylko tymczasowo; dzialaloby tylko A[10] := R gdzie R jest innym
  rekordem typu TDynArrayItem_14; podobnie zachowywalby sie kod gdyby TDynArray_14 bylo
  np. zwykla tablica; pamietaj ze obiekt (klasa) to tak naprawde tylko wskaznik
  wiec jesli chcesz miec tablice TDynMyClassArray to nie potrzebujesz (i nie chcesz)
  definiowac symbolu DYNARRAY_14_IS_STRUCT, ten problem nie istnieje dla klas !)
  (still better, you can use ObjectsList instead of DynArray if you want
  to have objects on your list).

  Jezeli DYNARRAY_14_IS_STRUCT jest zdefiniowane to domyslna jest wlasciwosc Pointers[i]
  ktora zwraca po prostu @@Items[i]. Uzywajac tej wlasciwosci mozesz operowac
  na TDynArray_14 jakby rzeczywiscie byla tablica - tylko pamietaj ze Pointers
  zwracaja wlasnie wskazniki (czyli PDynArrayItem_14, zamiast TDynArrayItem_14).

  W przeciwnym przypadku wlasciwosc Values bedzie zwracala elementy TDynArrayItem_14
  i pozwalala na ustawianie takich elementow (co jest ok jesli te elementy sa
  male i elementarne (elementarne = sa uzywane po lewej stronie instrukcji przypisania
  same albo tylko jako jakis indeks tablicy itp.). Wlasciwosc Items ciagle bedzie
  istniala - i moze byc uzyteczna, np. nie rob @@A[10], rob @@A.Items[10] z tych samych
  powodow co wyzej. Zreszta A.Items[] jest zawsze bezpieczne, po prostu jesli
  TDynArrayItem_14 nie jest struktura to ZAZWYCZAJ Values tez daje rezultaty jakich
  moznaby na pierwszy rzut oka oczekiwac.

  Jezeli symbol DYNARRAY_14_IS_INIT_FINI_TYPE jest zdefiniowany to na wszystkich nowo
  tworzonych elementach  tablicy (przez SetLength lub przez parametr InitialLength
  w Create) zostanie wywolane Finalize a na wszystkich zwalnianych (w SetLength
  lub w Destroy) elementach zostanie wywolane Finalize. To bedzie dzialac ok
  bez wzgledu na to czy DYNARRAY_14_IS_STRUCT czy nie.

  Jesli symbol DYNARRAY_14_USE_EQUALITY_OP (albo DYNARRAY_?_... jezeli uzywasz
  spawnerow) to zdefiniuje pare metod ktore uzywaja standardowego porownania
  operatorem "=" do wykonania czegos na elementac tablicy. W tym momencie
  jest to tylko metoda IndexOf. Jesli zdefiniujesz
  DYNARRAY_14_USE_EQUALITY_COMPAREMEM (DYNARRAY_?_... dla spawnerow)
  to bedzie uzywane CompareMem() zamiast operatora "="
  (nie wolno ci zdefiniowac jednoczesnie DYNARRAY_14_USE_EQUALITY_COMPAREMEM i
  DYNARRAY_14_USE_EQUALITY_OP, bedzie blad kompilacji).

  Define symbol DYNARRAY_14_IS_FUNCTION if TDynArrayItem_14 is a
  function/procedure pointer (of object or not of object).
  This is required, because (in Delphi syntax mode at least) we have
  to write some things differently.
  Also, this automatically defines DYNARRAY_14_USE_EQUALITY_OP.
}

{$ifdef read_interface}

{$ifdef DYNARRAY_14_IS_FUNCTION}
  {$define DYNARRAY_14_USE_EQUALITY_OP}
{$endif}

{$ifdef DYNARRAY_14_USE_EQUALITY_OP}
  {$ifdef DYNARRAY_14_USE_EQUALITY_COMPAREMEM}
    Error: you cannot define both *_USE_EQUALITY_OP and *_USE_EQUALITY_COMPAREMEM.
  {$endif}
{$endif}

{$ifdef DYNARRAY_14_USE_EQUALITY_OP}
  {$define DYNARRAY_14_USE_EQUALITY}
{$endif}

{$ifdef DYNARRAY_14_USE_EQUALITY_COMPAREMEM}
  {$define DYNARRAY_14_USE_EQUALITY}
{$endif}

  { }
  TInfiniteArray_14 = array[0..MaxInt div SizeOf(TDynArrayItem_14)-1]of TDynArrayItem_14;
  PInfiniteArray_14 = ^TInfiniteArray_14;

  TDynArrayItemIsSmallerFunc_14 = function (const a, b: TDynArrayItem_14): boolean;
  TDynArrayItemIsSmallerFuncByObject_14 = function (const a, b: TDynArrayItem_14): boolean of object;

  TDynArray_14 = class(TDynArrayBase)
  private
    {$ifndef DYNARRAY_14_IS_STRUCT}
    function GetValues(ItemNum: integer): TDynArrayItem_14;
    procedure SetValues(ItemNum: integer; const Item: TDynArrayItem_14);
    {$endif}
    function GetPointers(ItemNum: integer): PDynArrayItem_14;
    function GetItems:
      {$ifdef FPC_OBJFPC} PDynArrayItem_14 {$else} PInfiniteArray_14 {$endif};
    function GetItemsArray: PInfiniteArray_14;
  private
    { Helpers to implement Sort() }
    Sort_IsSmallerFunc: TDynArrayItemIsSmallerFunc_14;
    Sort_IsSmallerFuncByObject: TDynArrayItemIsSmallerFuncByObject_14;
    function Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
    function Propagate_IsSmallerFuncByObject(const Val1, Val2: pointer): boolean;
  public

    constructor Create; overload;
    constructor Create(InitialLength: Integer); overload;

    { This is a comfortable Items property, that allows you to both read and
      write items in this array.

      Note that it has different types:
      for Delphi (and FPC Delphi mode) it has PInfiniteArray_14 type,
      for ObjFpc mode is has PDynArrayItem_14.
      This way you can always access items like @code(Items[Index])
      (no need to write @code(Items^[Index])).

      @noAutoLinkHere }
    property Items: {$ifdef FPC_OBJFPC} PDynArrayItem_14 {$else}
      PInfiniteArray_14 {$endif} read GetItems;

    { This is like @link(Items), but it has always (both for FPC ObjFpc
      mode and Delphi) type PInfiniteArray_14. This means that you
      can always read/write items like @code(ItemsArray^[Index]).
      Under ObjFpc you will not be able to write
      @code(ItemsArray[Index]) (i.e. omit this ^ character). }
    property ItemsArray: PInfiniteArray_14 read GetItemsArray;

    { @noAutoLinkHere }
    property Pointers[ItemNum: integer]: PDynArrayItem_14 read GetPointers;
      {$ifdef DYNARRAY_14_IS_STRUCT} default; {$endif}

    {$ifndef DYNARRAY_14_IS_STRUCT}
    { @noAutoLinkHere }
    property Values[ItemNum: integer]: TDynArrayItem_14
      read GetValues write SetValues; default;
    {$endif}

    {$ifdef DYNARRAY_14_IS_INIT_FINI_TYPE}
    procedure SetLength(NewLength: Integer); override;
    {$endif}

    { AppendItem = IncLength and set last element to Item }
    procedure AppendItem(const Item: TDynArrayItem_14);
    procedure AppendArray(const NewItems: array of TDynArrayItem_14); overload;
    procedure AppendArray(const NewItems: array of TDynArrayItem_14; NewItemsAddCount: integer); overload;
    procedure AppendDynArray(Arr: TDynArray_14);

    { Copies contents of Source to this object.

      This is equivalent to doing @code(Length := 0) and
      @code(AppendDynarray(Source)).
      Maybe some day I will make TDynArray_14 descendant of TPersistent,
      then I will make Assign compatible with the TPersistent.Assign interface.

      Remember that this is *not* a so-called deep copy,
      because if Source contains some pointers we're simply
      copying these pointers (not duplicating their content
      constructing new objects or something like that).
      This is just a simple copy of memory contents
      (well, with proper Initialize/Finalize in case
      DYNARRAY_14_IS_INIT_FINI_TYPE). }
    procedure Assign(Source: TDynArray_14);

    { This is equivalent to @code(Count := 0) and then
      @code(AppendArray(NewItems)). }
    procedure AssignArray(const NewItems: array of TDynArrayItem_14);

    {$ifdef DYNARRAY_14_USE_EQUALITY}
    function EqualItems(const Item1, Item2: TDynArrayItem_14): boolean;
    function Equal(DynArr: TDynArray_14): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem_14): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem_14; ArrCount: Integer): boolean; overload;

    { Returns -1 if not found }
    function IndexOf(const Item: TDynArrayItem_14): Integer;

    { Returns -1 if not found, searches within
      Items[BeginIndex] .. Items[EndIndex].
      BeginIndex may be < 0 and EndIndex may be > High, it doesn't matter,
      the range will be "clamped" as it should. }
    function IndexInRange(const Item: TDynArrayItem_14;
      BeginIndex, EndIndex: Integer): Integer;

    { Searches for and deletes first item equal to Value. }
    procedure DeleteFirstEqual(const Value: TDynArrayItem_14);

    { Searches for and deletes all items equal to Value. }
    procedure DeleteAllEqual(const Value: TDynArrayItem_14);
    {$endif}

(*   TODO, as soon as this will be needed somewhere:
 {$ifdef DYNARRAY_USE_PLUSMINUS_OP}
    function Sum: TDynArrayItem_14 *)

    { Just like Delete from Pascal's RTL on strings:
      deletes Count items starting from position Index.
      Length of array will be decreased by Count items,
      and the last items of array will be shifted to the left to take
      positions of deleted items.

      Note that Index and DelCount parameters will be checked for
      correctness and eventually corrected. So if Index is >= Count
      then nothing will be removed. And if DelCount items from
      Index would be larger than Count then DelCount will be corrected
      to be smaller. }
    procedure Delete(Index, DelCount: integer);

    { Inserts Item at position Index into array.
      Index must be in range 0 .. Count (yes, we can @noAutoLink(insert)
      element at the end of the array).
      After insertion array has Count increased by 1, Items[Index] = Item and all
      items that were on positions [Index..old High] are now at [Index+1..new High]. }
    procedure Insert(Index: integer; const Item: TDynArrayItem_14);

    procedure SetAll(const Item: TDynArrayItem_14);

    { Sorts items. All you have to do is to pass some IsSmallerFunc
      -- see @link(KambiUtils.Sort) for specification what it should do,
      but note that @link(TDynArrayItemIsSmallerFunc_14) here gets parameter
      types already casted to TDynArrayItem_14, instead of dirty pointers
      (like IsSmallerFunc that must be passed to @link(KambiUtils.Sort)).

      If you don't give First/LastIndex it assumes FirstIndex = 0 and
      LastIndex = High, so the whole array will be sorted. }
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_14;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_14;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_14;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_14;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
  end;

{$endif}

{$ifdef read_implementation}

constructor TDynArray_14.Create;
begin
 inherited Create(SizeOf(TDynArrayItem_14), 0)
end;

constructor TDynArray_14.Create(InitialLength: Integer);
begin
 inherited Create(SizeOf(TDynArrayItem_14), InitialLength);
end;

function TDynArray_14.GetItems:
  {$ifdef FPC_OBJFPC} PDynArrayItem_14 {$else} PInfiniteArray_14 {$endif};
begin
  Result :=
    {$ifdef FPC_OBJFPC} PDynArrayItem_14 {$else} PInfiniteArray_14 {$endif} (A);
end;

function TDynArray_14.GetItemsArray: PInfiniteArray_14;
begin
  Result := PInfiniteArray_14(A);
end;

function TDynArray_14.GetPointers(ItemNum: integer): PDynArrayItem_14;
begin
 result := @(Items[ItemNum]);
end;

{$ifndef DYNARRAY_14_IS_STRUCT}
function TDynArray_14.GetValues(ItemNum: integer): TDynArrayItem_14;
begin result := Items[ItemNum] end;

procedure TDynArray_14.SetValues(ItemNum: integer; const Item: TDynArrayItem_14);
begin
 Items[ItemNum] := Item;
end;
{$endif}

{$ifdef DYNARRAY_14_IS_INIT_FINI_TYPE}
procedure TDynArray_14.SetLength(NewLength: Integer);
var oldlen, i: integer;
begin
 if NewLength < Length then
 begin
  for i := NewLength to Length-1 do Finalize(Items[i]);
  inherited SetLength(NewLength);
 end else
 begin
  oldlen := Length;
  inherited SetLength(NewLength);
  for i := oldlen to NewLength-1 do Initialize(Items[i]);
 end;
end;
{$endif}

procedure TDynArray_14.AppendItem(const Item: TDynArrayItem_14);
begin
 IncLength;
 Items[Length-1] := Item;
end;

procedure TDynArray_14.AppendArray(const NewItems: array of TDynArrayItem_14;
  NewItemsAddCount: integer);
var i, OldLen: integer;
begin
 OldLen := Length;
 SetLength(Length + NewItemsAddCount);
 for i := 0 to NewItemsAddCount-1 do
  Items[OldLen+i] := NewItems[i];
end;

procedure TDynArray_14.AppendArray(const NewItems: array of TDynArrayItem_14);
begin
 AppendArray(NewItems, System.High(NewItems)+1);
end;

procedure TDynArray_14.AppendDynArray(Arr: TDynArray_14);
begin
 AppendArray(Arr.ItemsArray^, Arr.Count);
end;

procedure TDynArray_14.Assign(Source: TDynArray_14);
{$ifdef DYNARRAY_14_IS_INIT_FINI_TYPE}
var i: Integer;
{$endif}
begin
 Length := Source.Length;
 {$ifdef DYNARRAY_14_IS_INIT_FINI_TYPE}
 { We're copying one-by-one, so that Initialize/Finalize
   of items is properly called. }
 for i := 0 to Source.Length-1 do Items[i] := Source.Items[i];
 {$else}
 { In this case we can copy memory fast, using Move }
 System.Move(
   {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_14_IS_FUNCTION} @ {$endif} {$endif} Source.Items[0],
   {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_14_IS_FUNCTION} @ {$endif} {$endif} Items[0],
   Length * SizeOf(TDynArrayItem_14));
 {$endif}
end;

procedure TDynArray_14.AssignArray(const NewItems: array of TDynArrayItem_14);
begin
  Count := 0;
  AppendArray(NewItems);
end;

{$ifdef DYNARRAY_14_USE_EQUALITY}

function TDynArray_14.EqualItems(const Item1, Item2: TDynArrayItem_14): boolean;
begin
 {$ifdef DYNARRAY_14_USE_EQUALITY_OP}
 Result := (
   {$ifdef DYNARRAY_14_IS_FUNCTION} @ {$endif} Item1 =
   {$ifdef DYNARRAY_14_IS_FUNCTION} @ {$endif} Item2);
 {$else}
   {$ifdef DYNARRAY_14_USE_EQUALITY_COMPAREMEM}
   result := CompareMem(@Item1, @Item2, SizeOf(TDynArrayItem_14));
   {$else}
   Error: one of the DYNARRAY_USE_EQUALITY_* must be defined.
   {$endif}
 {$endif}
end;

function TDynArray_14.Equal(DynArr: TDynArray_14): boolean;
begin
 result := Equal(DynArr.ItemsArray^, DynArr.Count);
end;

function TDynArray_14.Equal(const Arr: array of TDynArrayItem_14): boolean;
begin
 result := Equal(Arr, System.High(Arr)+1);
end;

function TDynArray_14.Equal(const Arr: array of TDynArrayItem_14;
  ArrCount: Integer): boolean;
var i: Integer;
begin
 if ArrCount <> Length then begin result := false; Exit end;

 for i := 0 to ArrCount-1 do
  if not EqualItems(Arr[i], Items[i]) then
   begin result := false; Exit end;

 result := true;
end;

function TDynArray_14.IndexOf(const Item: TDynArrayItem_14): integer;
begin
 for Result := 0 to High do
  if EqualItems(Items[Result], Item) then Exit;
 Result := -1;
end;

function TDynArray_14.IndexInRange(const Item: TDynArrayItem_14;
  BeginIndex, EndIndex: Integer): Integer;
begin
 if BeginIndex < 0 then BeginIndex := 0;
 if EndIndex > High then EndIndex := High;
 for Result := BeginIndex to EndIndex do
  if EqualItems(Items[Result], Item) then Exit;
 Result := -1;
end;

procedure TDynArray_14.DeleteFirstEqual(const Value: TDynArrayItem_14);
var
  I: Integer;
begin
  I := IndexOf(Value);
  if I <> -1 then
    Delete(I, 1);
end;

procedure TDynArray_14.DeleteAllEqual(const Value: TDynArrayItem_14);
var
  I: Integer;
begin
  I := 0;
  while I < Count do
  begin
    if EqualItems(Items[I], Value) then
      Delete(I, 1) else
      Inc(I);
  end;
end;
{$endif}

procedure TDynArray_14.Delete(Index, DelCount: integer);
var i: integer;
begin
 { Make sure Index and DelCount are sensible first }
 if Index >= Count then
   Exit;
 MinTo1st(DelCount, Count - Index);

 {$ifdef DYNARRAY_14_IS_INIT_FINI_TYPE}
 for i := Index to Index + DelCount - 1 do
  Finalize(Items[i]);
 {$endif}

 { This copies elements from Index+Count .. Length-1
   to positions Index .. Length -Count -1 using direct memory
   copying (Move), so it doesn't care about Initialize/Finalize.
   This way it's fast. }
 for i := Index to Length - 1 - DelCount do
  { Instead of Items[i] := Items[i + DelCount]; }
  System.Move(
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_14_IS_FUNCTION} @ {$endif} {$endif} Items[i+DelCount],
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_14_IS_FUNCTION} @ {$endif} {$endif} Items[i],
    SizeOf(TDynArrayItem_14));

 { Decrease Length without doing Finalize on the tail of the array. }
 SetLengthNoInitFini(Length - DelCount);
end;

procedure TDynArray_14.Insert(Index: integer; const Item: TDynArrayItem_14);
var i: integer;
begin
 IncLength;
 for i := High downto Index+1 do Items[i] := Items[i-1];
 Items[Index] := Item;
end;

procedure TDynArray_14.SetAll(const Item: TDynArrayItem_14);
var
  i: integer;
  ItemAsByte: byte absolute Item;
  {$ifdef FPC}
  ItemAsWord: Word absolute Item;
  ItemAsDWord: DWord absolute Item;
  {$endif}
begin
 {$ifdef DYNARRAY_14_IS_INIT_FINI_TYPE}
 for i := 0 to Length-1 do Items[i] := Item;
 {$else}
 case SizeOf(TDynArrayItem_14) of
  1: System.FillChar(Items^, Count, ItemAsByte);
  {$ifdef FPC}
  2: FillWord(Items^, Count, ItemAsWord);
  4: FillDWord(Items^, Count, ItemAsDWord);
  {$endif}
  else
    for i := 0 to Length-1 do Items[i] := Item;
 end;
 {$endif}
end;

{ sortowania; najpierw Sort bez parametrow *Index --------------------------- }

procedure TDynArray_14.Sort(
  IsSmallerFunc: TDynArrayItemIsSmallerFunc_14;
  CountToUseSimpleSort: Integer);
begin
 Sort(IsSmallerFunc, 0, High, CountToUseSimpleSort);
end;

procedure TDynArray_14.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_14;
  CountToUseSimpleSort: Integer);
begin
 SortByObject(IsSmallerFunc, 0, High, CountToUseSimpleSort);
end;

{ -----------------------------------------------------------------------------
  sortowania; propagacja z metod na Pointerach do metod na TDynArrayItem_14 }

function TDynArray_14.Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
begin
 result := Sort_IsSmallerFunc(PDynArrayItem_14(Val1)^,PDynArrayItem_14(Val2)^)
end;

function TDynArray_14.Propagate_IsSmallerFuncByObject(
  const Val1, Val2: pointer): boolean;
begin
 result := Sort_IsSmallerFuncByObject(PDynArrayItem_14(Val1)^,PDynArrayItem_14(Val2)^)
end;

{ sortowania; wlasciwe metody Sort ------------------------------------------ }
{ Self.Propagate_* below required because of fpc 1.0.10 func_ofobject_bug }

procedure TDynArray_14.Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_14;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFunc := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem_14),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFunc,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

procedure TDynArray_14.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_14;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFuncByObject := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem_14),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFuncByObject,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

{$endif}
