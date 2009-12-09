{
  Copyright 2002-2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Before including this file, define types :
    TDynArrayItem,
    PDynArrayItem -- this must be ^TDynArrayItem.
  (in FPC you can also use macros to define these identifiers)

  Including this file will define types :
    TDynArray -- a class that represents an array of TDynArrayItem,
    TInfiniteArray -- array[0..<infinite>]of TDynArrayItem,
    PInfiniteArray -- ^TInfiniteArray,
    TDynArrayItemIsSmallerFunc,
    TDynArrayItemIsSmallerFuncByObject,
      ( function (const a, b: TDynArrayItem): boolean [of object] );

  Jezeli symbol DYNARRAY_IS_STRUCT jest zdefiniowany nie ma wlasciwosci Values - powinienes
  zawsze uzywac wlasciwosci .Items[] (ktora nie jest wlasciwoscia tablicowa,
  ona jest wskaznikiem na tablice, wiec nie moze byc defautowa...)
  Items to wskaznik na tablice elementow typu TDynArrayItem. Mozesz na niej dowolnie
  operowac (pamietaj ze RangeChecking nie dziala - nie ma jak); takie zachowanie jest
  dobre gdy TArrayItem to rekord lub inna tablica. Wlasciwosc Values nie bedzie istniala
  (moglaby istniec; ale jej uzycie czesciej prowadziloby do bledow niz do
  wygodnych zastosowan : gdyby TDynarrayItem byl np. rekordem record x, y: integer end
  to A[10].x zostaloby odczytane jako A.GetValues(10).x := 10 a wiec zmieniloby wartosc
  tymczasowej kopii wartosci tablicy 10, a wiec de facto nie zmieniloby niczego...
  podobnie wyrazenie @@A[10].x byloby juz arcy-niebezpieczne bo zwracaloby
  wskaznik poprawny tylko tymczasowo; dzialaloby tylko A[10] := R gdzie R jest innym
  rekordem typu TDynArrayItem; podobnie zachowywalby sie kod gdyby TDynArray bylo
  np. zwykla tablica; pamietaj ze obiekt (klasa) to tak naprawde tylko wskaznik
  wiec jesli chcesz miec tablice TDynMyClassArray to nie potrzebujesz (i nie chcesz)
  definiowac symbolu DYNARRAY_IS_STRUCT, ten problem nie istnieje dla klas !)
  (still better, you can use ObjectsList instead of DynArray if you want
  to have objects on your list).

  Jezeli DYNARRAY_IS_STRUCT jest zdefiniowane to domyslna jest wlasciwosc Pointers[i]
  ktora zwraca po prostu @@Items[i]. Uzywajac tej wlasciwosci mozesz operowac
  na TDynArray jakby rzeczywiscie byla tablica - tylko pamietaj ze Pointers
  zwracaja wlasnie wskazniki (czyli PDynArrayItem, zamiast TDynArrayItem).

  W przeciwnym przypadku wlasciwosc Values bedzie zwracala elementy TDynArrayItem
  i pozwalala na ustawianie takich elementow (co jest ok jesli te elementy sa
  male i elementarne (elementarne = sa uzywane po lewej stronie instrukcji przypisania
  same albo tylko jako jakis indeks tablicy itp.). Wlasciwosc Items ciagle bedzie
  istniala - i moze byc uzyteczna, np. nie rob @@A[10], rob @@A.Items[10] z tych samych
  powodow co wyzej. Zreszta A.Items[] jest zawsze bezpieczne, po prostu jesli
  TDynArrayItem nie jest struktura to ZAZWYCZAJ Values tez daje rezultaty jakich
  moznaby na pierwszy rzut oka oczekiwac.

  Jezeli symbol DYNARRAY_IS_INIT_FINI_TYPE jest zdefiniowany to na wszystkich nowo
  tworzonych elementach  tablicy (przez SetLength lub przez parametr InitialLength
  w Create) zostanie wywolane Finalize a na wszystkich zwalnianych (w SetLength
  lub w Destroy) elementach zostanie wywolane Finalize. To bedzie dzialac ok
  bez wzgledu na to czy DYNARRAY_IS_STRUCT czy nie.

  Jesli symbol DYNARRAY_USE_EQUALITY_OP (albo DYNARRAY_?_... jezeli uzywasz
  spawnerow) to zdefiniuje pare metod ktore uzywaja standardowego porownania
  operatorem "=" do wykonania czegos na elementac tablicy. W tym momencie
  jest to tylko metoda IndexOf. Jesli zdefiniujesz
  DYNARRAY_USE_EQUALITY_COMPAREMEM (DYNARRAY_?_... dla spawnerow)
  to bedzie uzywane CompareMem() zamiast operatora "="
  (nie wolno ci zdefiniowac jednoczesnie DYNARRAY_USE_EQUALITY_COMPAREMEM i
  DYNARRAY_USE_EQUALITY_OP, bedzie blad kompilacji).

  Define symbol DYNARRAY_IS_FUNCTION if TDynArrayItem is a
  function/procedure pointer (of object or not of object).
  This is required, because (in Delphi syntax mode at least) we have
  to write some things differently.
  Also, this automatically defines DYNARRAY_USE_EQUALITY_OP.

  Additionally, define DYNARRAY_IS_FUNCTION_METHOD for method ("of object")
  pointers. Turns out that this is needed, since comparing method
  pointers with normal "=" operator compares only code pointer
  (done by FPC for Delphi compatibility, according to
  [http://bugs.freepascal.org/view.php?id=11868]).
}

{$ifdef read_interface}

{$ifdef DYNARRAY_IS_FUNCTION}
  {$define DYNARRAY_USE_EQUALITY_OP}
{$endif}

{$ifdef DYNARRAY_USE_EQUALITY_OP}
  {$ifdef DYNARRAY_USE_EQUALITY_COMPAREMEM}
    Error: you cannot define both *_USE_EQUALITY_OP and *_USE_EQUALITY_COMPAREMEM.
  {$endif}
{$endif}

{$ifdef DYNARRAY_USE_EQUALITY_OP}
  {$define DYNARRAY_USE_EQUALITY}
{$endif}

{$ifdef DYNARRAY_USE_EQUALITY_COMPAREMEM}
  {$define DYNARRAY_USE_EQUALITY}
{$endif}

  { }
  TInfiniteArray = array[0..MaxInt div SizeOf(TDynArrayItem)-1]of TDynArrayItem;
  PInfiniteArray = ^TInfiniteArray;

  TDynArrayItemIsSmallerFunc = function (const a, b: TDynArrayItem): boolean;
  TDynArrayItemIsSmallerFuncByObject = function (const a, b: TDynArrayItem): boolean of object;

  TDynArray = class(TDynArrayBase)
  private
    {$ifndef DYNARRAY_IS_STRUCT}
    function GetValues(ItemNum: integer): TDynArrayItem;
    procedure SetValues(ItemNum: integer; const Item: TDynArrayItem);
    {$endif}
    function GetPointers(ItemNum: integer): PDynArrayItem;
    function GetItems:
      {$ifdef FPC_OBJFPC} PDynArrayItem {$else} PInfiniteArray {$endif};
      {$ifdef SUPPORTS_INLINE} inline; {$endif}
    function GetItemsArray: PInfiniteArray;
    function GetFirst: TDynArrayItem;
    function GetLast: TDynArrayItem;
    procedure SetFirst(const Value: TDynArrayItem);
    procedure SetLast(const Value: TDynArrayItem);
  private
    { Helpers to implement Sort() }
    Sort_IsSmallerFunc: TDynArrayItemIsSmallerFunc;
    Sort_IsSmallerFuncByObject: TDynArrayItemIsSmallerFuncByObject;
    function Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
    function Propagate_IsSmallerFuncByObject(const Val1, Val2: pointer): boolean;
  public
    { Note that we do not want to use below "overload" keyword,
      as we do *not* want here to allow cross-class (Delphi-style) overloading.
      This would allow caller to call Create(ElementSize, InitialLength),
      with disastrous consequences if ElementSize is wrong.

      The constructor Create(ElementSize, InitialLength) should only
      be available to TDynArray implementation, and hidden from the
      outside world.

      Unfortunately for Delphi, we cannot do this AFAIK (overloading is always
      cross-class). Under FPC thankfully we can. }
    { }

    constructor Create; {$ifdef DELPHI} overload; {$endif}
    constructor Create(InitialLength: Integer); {$ifdef DELPHI} overload; {$endif}
    constructor CreateCopy(Source: TDynArray);

    { This is a comfortable Items property, that allows you to both read and
      write items in this array.

      Note that it has different types:
      for Delphi (and FPC Delphi mode) it has PInfiniteArray type,
      for ObjFpc mode is has PDynArrayItem.
      This way you can always access items like @code(Items[Index])
      (no need to write @code(Items^[Index])). }
    property Items: {$ifdef FPC_OBJFPC} PDynArrayItem {$else}
      PInfiniteArray {$endif} read GetItems;

    { This is like @link(Items), but it has always (both for FPC ObjFpc
      mode and Delphi) type PInfiniteArray. This means that you
      can always read/write items like @code(ItemsArray^[Index]).
      Under ObjFpc you will not be able to write
      @code(ItemsArray[Index]) (i.e. omit this ^ character). }
    property ItemsArray: PInfiniteArray read GetItemsArray;

    property Pointers[ItemNum: integer]: PDynArrayItem read GetPointers;
      {$ifdef DYNARRAY_IS_STRUCT} default; {$endif}

    {$ifndef DYNARRAY_IS_STRUCT}
    { }
    property Values[ItemNum: integer]: TDynArrayItem
      read GetValues write SetValues; default;
    {$endif}

    {$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
    procedure SetLength(NewLength: Integer); override;
    {$endif}

    { Increase length, adding new Item at the end. }
    procedure AppendItem(const Item: TDynArrayItem); overload;

    { Increase length and return the pointer to new item at the end. }
    function AppendItem: PDynArrayItem; overload;

    procedure AppendArray(const NewItems: array of TDynArrayItem); overload;
    procedure AppendArray(const NewItems: array of TDynArrayItem; NewItemsAddCount: integer); overload;

    procedure AppendDynArray(Arr: TDynArray); overload;
    procedure AppendDynArray(Arr: TDynArray; Index, ACount: Integer); overload;

    { Copies contents of Source to this object.

      This is equivalent to doing @code(Length := 0) and
      @code(AppendDynarray(Source)).
      Maybe some day I will make TDynArray descendant of TPersistent,
      then I will make Assign compatible with the TPersistent.Assign interface.

      Remember that this is *not* a so-called deep copy,
      because if Source contains some pointers we're simply
      copying these pointers (not duplicating their content
      constructing new objects or something like that).
      This is just a simple copy of memory contents
      (well, with proper Initialize/Finalize in case
      DYNARRAY_IS_INIT_FINI_TYPE). }
    procedure Assign(Source: TDynArray);

    { This is equivalent to @code(Count := 0) and then
      @code(AppendArray(NewItems)). }
    procedure AssignArray(const NewItems: array of TDynArrayItem);

    {$ifdef DYNARRAY_USE_EQUALITY}
    function EqualItems(const Item1, Item2: TDynArrayItem): boolean;
    function Equal(DynArr: TDynArray): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem; ArrCount: Integer): boolean; overload;

    { Search for given item, starting from StartIndex (default is to
      search whole list). Returns -1 if not found. }
    function IndexOf(const Item: TDynArrayItem; StartIndex: Integer = 0): Integer;

    { Search for given item, returns -1 if not found.
      Searches within array range Items[BeginIndex] .. Items[EndIndex].
      BeginIndex may be < 0 and EndIndex may be > High, it doesn't matter,
      the range will be "clamped" as it should. }
    function IndexInRange(const Item: TDynArrayItem;
      BeginIndex, EndIndex: Integer): Integer;

    { Searches for and deletes the first item equal to Value.
      Returns the index of removed item (-1 if not found). }
    function Remove(const Value: TDynArrayItem): Integer;

    { Searches for and deletes all items equal to Value. }
    procedure RemoveAll(const Value: TDynArrayItem);

    { If the Item is not already present in the array (checked like IndexOf)
      then add this item (like by AppendItem). }
    procedure AddIfNotExists(const Item: TDynArrayItem);

    procedure DeleteDuplicates;
    {$endif}

    { Delete items from the array, starting from given the Index.
      By default DelCount is 1, so it simply removes one item.

      This can be used like Delete from Pascal's RTL on strings:
      deletes Count items starting from position Index.
      Length of array will be decreased by Count items,
      and the last items of array will be shifted to the left to take
      positions of deleted items.

      Note that Index and DelCount parameters will be checked for
      correctness and eventually corrected. So if Index is >= Count
      then nothing will be removed. And if DelCount items from
      Index would be larger than Count then DelCount will be corrected
      to be smaller. }
    procedure Delete(const Index: Integer; DelCount: Integer = 1);

    { Inserts Item at position Index into array.
      Index must be in range 0 .. Count (yes, we can @noAutoLink(insert)
      element at the end of the array).
      After insertion array has Count increased by 1, Items[Index] = Item and all
      items that were on positions [Index..old High] are now at [Index+1..new High]. }
    procedure Insert(Index: integer; const Item: TDynArrayItem);

    procedure SetAll(const Item: TDynArrayItem);

    { Sorts items. All you have to do is to pass some IsSmallerFunc
      -- see @link(KambiUtils.Sort) for specification what it should do,
      but note that @link(TDynArrayItemIsSmallerFunc) here gets parameter
      types already casted to TDynArrayItem, instead of dirty pointers
      (like IsSmallerFunc that must be passed to @link(KambiUtils.Sort)).

      If you don't give First/LastIndex it assumes FirstIndex = 0 and
      LastIndex = High, so the whole array will be sorted. }
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;

    {$ifdef DYNARRAY_USE_PLUSMINUS_OP}
    function Sum: TDynArrayItem;
    {$endif DYNARRAY_USE_PLUSMINUS_OP}

    property First: TDynArrayItem read GetFirst write SetFirst;
    property Last : TDynArrayItem read GetLast  write SetLast ;
  end;

{$endif}

{$ifdef read_implementation}

constructor TDynArray.Create;
begin
 inherited Create(SizeOf(TDynArrayItem), 0)
end;

constructor TDynArray.Create(InitialLength: Integer);
begin
 inherited Create(SizeOf(TDynArrayItem), InitialLength);
end;

constructor TDynArray.CreateCopy(Source: TDynArray);
begin
  Create;
  Assign(Source);
end;

function TDynArray.GetItems:
  {$ifdef FPC_OBJFPC} PDynArrayItem {$else} PInfiniteArray {$endif};
begin
  Result :=
    {$ifdef FPC_OBJFPC} PDynArrayItem {$else} PInfiniteArray {$endif} (A);
end;

function TDynArray.GetItemsArray: PInfiniteArray;
begin
  Result := PInfiniteArray(A);
end;

function TDynArray.GetPointers(ItemNum: integer): PDynArrayItem;
begin
 result := @(Items[ItemNum]);
end;

{$ifndef DYNARRAY_IS_STRUCT}
function TDynArray.GetValues(ItemNum: integer): TDynArrayItem;
begin result := Items[ItemNum] end;

procedure TDynArray.SetValues(ItemNum: integer; const Item: TDynArrayItem);
begin
 Items[ItemNum] := Item;
end;
{$endif}

{$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
procedure TDynArray.SetLength(NewLength: Integer);
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

procedure TDynArray.AppendItem(const Item: TDynArrayItem);
begin
  IncLength;
  Items[Length-1] := Item;
end;

function TDynArray.AppendItem: PDynArrayItem;
begin
  IncLength;
  Result := @(Items[Length-1]);
end;

procedure TDynArray.AppendArray(const NewItems: array of TDynArrayItem;
  NewItemsAddCount: integer);
var
  OldLength: Integer;
{$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
  I: Integer;
{$endif}
begin
  OldLength := Length;
  SetLength(Length + NewItemsAddCount);
{$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
  { We're copying one-by-one, so that Initialize/Finalize
   of items is properly called. }
  for i := 0 to NewItemsAddCount - 1 do
    Items[OldLength + i] := NewItems[i];
{$else}
  { In this case we can copy memory fast, using Move }
  System.Move(
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} NewItems[0],
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} Items[OldLength],
    NewItemsAddCount * SizeOf(TDynArrayItem));
{$endif}
end;

procedure TDynArray.AppendArray(const NewItems: array of TDynArrayItem);
begin
  AppendArray(NewItems, System.High(NewItems)+1);
end;

procedure TDynArray.AppendDynArray(Arr: TDynArray);
begin
  AppendArray(Arr.ItemsArray^, Arr.Count);
end;

procedure TDynArray.AppendDynArray(Arr: TDynArray; Index, ACount: Integer);
begin
  AppendArray(PInfiniteArray(@Arr.Items[Index])^, ACount);
end;

procedure TDynArray.Assign(Source: TDynArray);
{$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
var
  i: Integer;
{$endif}
begin
 Length := Source.Length;
 {$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
 { We're copying one-by-one, so that Initialize/Finalize
   of items is properly called. }
 for i := 0 to Source.Length-1 do Items[i] := Source.Items[i];
 {$else}
 { In this case we can copy memory fast, using Move }
 System.Move(
   {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} Source.Items[0],
   {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} Items[0],
   Length * SizeOf(TDynArrayItem));
 {$endif}
end;

procedure TDynArray.AssignArray(const NewItems: array of TDynArrayItem);
begin
  Count := 0;
  AppendArray(NewItems);
end;

{$ifdef DYNARRAY_USE_EQUALITY}

function TDynArray.EqualItems(const Item1, Item2: TDynArrayItem): boolean;
begin
  Result :=
    {$ifdef DYNARRAY_IS_FUNCTION_METHOD}
      (TMethod(Item1).Code = TMethod(Item2).Code) and
      (TMethod(Item1).Data = TMethod(Item2).Data);
    {$else DYNARRAY_IS_FUNCTION_METHOD}
      {$ifdef DYNARRAY_USE_EQUALITY_OP}
        ({$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} Item1 =
         {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} Item2);
      {$else}
        {$ifdef DYNARRAY_USE_EQUALITY_COMPAREMEM}
        CompareMem(@Item1, @Item2, SizeOf(TDynArrayItem));
        {$else}
        Error: one of the DYNARRAY_USE_EQUALITY_* must be defined.
        {$endif}
      {$endif}
    {$endif DYNARRAY_IS_FUNCTION_METHOD}
end;

function TDynArray.Equal(DynArr: TDynArray): boolean;
begin
 result := Equal(DynArr.ItemsArray^, DynArr.Count);
end;

function TDynArray.Equal(const Arr: array of TDynArrayItem): boolean;
begin
 result := Equal(Arr, System.High(Arr)+1);
end;

function TDynArray.Equal(const Arr: array of TDynArrayItem;
  ArrCount: Integer): boolean;
var i: Integer;
begin
 if ArrCount <> Length then begin result := false; Exit end;

 for i := 0 to ArrCount-1 do
  if not EqualItems(Arr[i], Items[i]) then
   begin result := false; Exit end;

 result := true;
end;

function TDynArray.IndexOf(const Item: TDynArrayItem; StartIndex: Integer): Integer;
begin
  for Result := StartIndex to High do
    if EqualItems(Items[Result], Item) then Exit;
  Result := -1;
end;

function TDynArray.IndexInRange(const Item: TDynArrayItem;
  BeginIndex, EndIndex: Integer): Integer;
begin
 if BeginIndex < 0 then BeginIndex := 0;
 if EndIndex > High then EndIndex := High;
 for Result := BeginIndex to EndIndex do
  if EqualItems(Items[Result], Item) then Exit;
 Result := -1;
end;

function TDynArray.Remove(const Value: TDynArrayItem): Integer;
begin
  Result := IndexOf(Value);
  if Result <> -1 then Delete(Result);
end;

procedure TDynArray.RemoveAll(const Value: TDynArrayItem);
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

procedure TDynArray.AddIfNotExists(const Item: TDynArrayItem);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I = -1 then
    AppendItem(Item);
end;

procedure TDynArray.DeleteDuplicates;
var
  I, Index: integer;
begin
  I := 0;
  while I < Count do
  begin
    Index := I + 1;
    repeat
      Index := IndexOf(Items[I], Index);
      if Index = -1 then Break;
      Delete(Index);
    until false;

    Inc(I);
  end;
end;
{$endif}

procedure TDynArray.Delete(const Index: Integer; DelCount: Integer);
var i: integer;
begin
 { Make sure Index and DelCount are sensible first }
 if Index >= Count then
   Exit;
 MinTo1st(DelCount, Count - Index);

 {$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
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
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} Items[i+DelCount],
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_IS_FUNCTION} @ {$endif} {$endif} Items[i],
    SizeOf(TDynArrayItem));

 { Decrease Length without doing Finalize on the tail of the array. }
 SetLengthNoInitFini(Length - DelCount);
end;

procedure TDynArray.Insert(Index: integer; const Item: TDynArrayItem);
var i: integer;
begin
 IncLength;
 for i := High downto Index+1 do Items[i] := Items[i-1];
 Items[Index] := Item;
end;

procedure TDynArray.SetAll(const Item: TDynArrayItem);
var
  i: integer;
  ItemAsByte: byte absolute Item;
  {$ifdef FPC}
  ItemAsWord: Word absolute Item;
  ItemAsDWord: DWord absolute Item;
  {$endif}
begin
 {$ifdef DYNARRAY_IS_INIT_FINI_TYPE}
 for i := 0 to Length-1 do Items[i] := Item;
 {$else}
 case SizeOf(TDynArrayItem) of
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

procedure TDynArray.Sort(
  IsSmallerFunc: TDynArrayItemIsSmallerFunc;
  CountToUseSimpleSort: Integer);
begin
 Sort(IsSmallerFunc, 0, High, CountToUseSimpleSort);
end;

procedure TDynArray.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject;
  CountToUseSimpleSort: Integer);
begin
 SortByObject(IsSmallerFunc, 0, High, CountToUseSimpleSort);
end;

{ -----------------------------------------------------------------------------
  sortowania; propagacja z metod na Pointerach do metod na TDynArrayItem }

function TDynArray.Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
begin
 result := Sort_IsSmallerFunc(PDynArrayItem(Val1)^,PDynArrayItem(Val2)^)
end;

function TDynArray.Propagate_IsSmallerFuncByObject(
  const Val1, Val2: pointer): boolean;
begin
 result := Sort_IsSmallerFuncByObject(PDynArrayItem(Val1)^,PDynArrayItem(Val2)^)
end;

{ sortowania; wlasciwe metody Sort ------------------------------------------ }
{ Self.Propagate_* below required because of fpc 1.0.10 func_ofobject_bug }

procedure TDynArray.Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFunc := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFunc,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

procedure TDynArray.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFuncByObject := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFuncByObject,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

{$ifdef DYNARRAY_USE_PLUSMINUS_OP}
function TDynArray.Sum: TDynArrayItem;
var
  I: Integer;
begin
  Result := 0; { this will be improved to some macro like DYNARRAY_ZERO_ITEM }
  for I := 0 to High do
    Result := Result + Items[I];
end;
{$endif DYNARRAY_USE_PLUSMINUS_OP}

function TDynArray.GetFirst: TDynArrayItem;
begin
  Result := Items[0];
end;

function TDynArray.GetLast: TDynArrayItem;
begin
  Result := Items[Count - 1];
end;

procedure TDynArray.SetFirst(const Value: TDynArrayItem);
begin
  Items[0] := Value;
end;

procedure TDynArray.SetLast(const Value: TDynArrayItem);
begin
  Items[Count - 1] := Value;
end;

{$endif}
