{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Template of a class defining a dynamic (resizeable) array of items.

  Before including this file, define types:
    TDynArrayItem_2,
    PDynArrayItem_2 -- this must be ^TDynArrayItem_2.
  (in FPC you can also use macros to define these identifiers)

  Including this file will define types :
    TDynArray_2 -- a class that represents an array of TDynArrayItem_2,
    TInfiniteArray_2 -- array[0..<infinite>]of TDynArrayItem_2,
    PInfiniteArray_2 -- ^TInfiniteArray_2,
    TDynArrayItemIsSmallerFunc_2,
    TDynArrayItemIsSmallerFuncByObject_2,
      ( function (const a, b: TDynArrayItem_2): boolean [of object] );

  You can also define some symbols:

  @unorderedList(
    @item(DYNARRAY_2_IS_STRUCT: Then our array is a structure, like a record
      or old-(TP-style)-object.

      In this case we don't define the @code(Values) property, as it would be too
      error-prone. When you define a property that returns a record,
      you can use @code(Values[I].SomeProperty := 123) but it will
      actually not work, as @code(Values[I]) is only a temporary returned
      record.

      The default class property is then the @code(Pointers) property,
      that returns pointer to the appropriate item. This allows you to modify
      the record contents safely.

      Note that, regardless of whether you define DYNARRAY_2_IS_STRUCT or not,
      we always provide the @code(Items) property. @code(Items) is a direct
      pointer to your items array, and so will work perfectly with records too,
      can be modified as usual etc.

      When DYNARRAY_2_IS_STRUCT is not defined, then @code(Values) property
      is defined, and it's even the default class property. So you can then treat
      the instances of this TDynArray_2 as real arrays.)

    @item(DYNARRAY_2_IS_INIT_FINI_TYPE: You have to define this if your type requires
      Initialize/Finalize call. Define this for AnsiStrings, interfaces,
      records containing AnsiStrings or interfaces and such.)

    @item(DYNARRAY_2_USE_EQUALITY_OP: Use the "=" operator on your items
      to define some additional methods like IndexOf.)

    @item(DYNARRAY_2_USE_EQUALITY_COMPAREMEM: Use the CompareMem function
      to compare your items, this will also provide methods like IndexOf,
      although implemented differently.)

    @item(DYNARRAY_2_USE_PLUSMINUS_OP: Use the "+" operator to define functions
      like @code(Sum).)

    @item(DYNARRAY_2_IS_FUNCTION: Define this if your item is a function / procedure
      pointer (not of object). This is needed in Delphi mode.
      Also, this automatically defines DYNARRAY_2_USE_EQUALITY_OP.)

    @item(DYNARRAY_2_IS_FUNCTION_METHOD: Define this if your item is a method
      (function / procedure of object). Similar to DYNARRAY_2_IS_FUNCTION,
      this is needed to correctly handle methods. We have to differentiate
      between normal functions and methods, because methods cannot be
      (reasonably) compared by the "=" operator, see
      [http://bugs.freepascal.org/view.php?id=11868].)
  )
}

{$ifdef read_interface}

{$ifdef DYNARRAY_2_IS_FUNCTION}
  {$define DYNARRAY_2_USE_EQUALITY_OP}
{$endif}

{$ifdef DYNARRAY_2_USE_EQUALITY_OP}
  {$ifdef DYNARRAY_2_USE_EQUALITY_COMPAREMEM}
    Error: you cannot define both *_USE_EQUALITY_OP and *_USE_EQUALITY_COMPAREMEM.
  {$endif}
{$endif}

{$ifdef DYNARRAY_2_USE_EQUALITY_OP}
  {$define DYNARRAY_2_USE_EQUALITY}
{$endif}

{$ifdef DYNARRAY_2_USE_EQUALITY_COMPAREMEM}
  {$define DYNARRAY_2_USE_EQUALITY}
{$endif}

  { }
  TInfiniteArray_2 = array[0..MaxInt div SizeOf(TDynArrayItem_2)-1]of TDynArrayItem_2;
  PInfiniteArray_2 = ^TInfiniteArray_2;

  TDynArrayItemIsSmallerFunc_2 = function (const a, b: TDynArrayItem_2): boolean;
  TDynArrayItemIsSmallerFuncByObject_2 = function (const a, b: TDynArrayItem_2): boolean of object;

  TDynArray_2 = class(TDynArrayBase)
  private
    {$ifndef DYNARRAY_2_IS_STRUCT}
    function GetValues(ItemNum: integer): TDynArrayItem_2;
    procedure SetValues(ItemNum: integer; const Item: TDynArrayItem_2);
    {$endif}
    function GetPointers(ItemNum: integer): PDynArrayItem_2;
    function GetItems:
      {$ifdef FPC_OBJFPC} PDynArrayItem_2 {$else} PInfiniteArray_2 {$endif};
      {$ifdef SUPPORTS_INLINE} inline; {$endif}
    function GetItemsArray: PInfiniteArray_2;
    function GetFirst: TDynArrayItem_2;
    function GetLast: TDynArrayItem_2;
    procedure SetFirst(const Value: TDynArrayItem_2);
    procedure SetLast(const Value: TDynArrayItem_2);
  private
    { Helpers to implement Sort() }
    Sort_IsSmallerFunc: TDynArrayItemIsSmallerFunc_2;
    Sort_IsSmallerFuncByObject: TDynArrayItemIsSmallerFuncByObject_2;
    function Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
    function Propagate_IsSmallerFuncByObject(const Val1, Val2: pointer): boolean;
  public
    { Note that we do not want to use below "overload" keyword,
      as we do *not* want here to allow cross-class (Delphi-style) overloading.
      This would allow caller to call Create(ElementSize, InitialLength),
      with disastrous consequences if ElementSize is wrong.

      The constructor Create(ElementSize, InitialLength) should only
      be available to TDynArray_2 implementation, and hidden from the
      outside world.

      Unfortunately for Delphi, we cannot do this AFAIK (overloading is always
      cross-class). Under FPC ObjFpc thankfully we can. }
    { }

    constructor Create; {$ifndef FPC_OBJFPC} overload; {$endif}
    constructor Create(InitialLength: Integer); {$ifndef FPC_OBJFPC} overload; {$endif}
    constructor CreateCopy(Source: TDynArray_2);

    { Direct access to your items, you can both read and write using this.

      Note that it has different types:
      for Delphi (and FPC Delphi mode) it has PInfiniteArray_2 type,
      for ObjFpc mode is has PDynArrayItem_2.
      This way you can always access items like @code(Items[Index])
      (no need to write @code(Items^[Index])). }
    property Items: {$ifdef FPC_OBJFPC} PDynArrayItem_2 {$else}
      PInfiniteArray_2 {$endif} read GetItems;

    { Pointer to your items. Exactly like @link(Items), but always of
      PInfiniteArray_2 type.

      This means that you can always read/write items like @code(ItemsArray^[Index]).
      Under ObjFpc you will not be able to write
      @code(ItemsArray[Index]) (i.e. omit this ^ character). }
    property ItemsArray: PInfiniteArray_2 read GetItemsArray;

    property Pointers[ItemNum: integer]: PDynArrayItem_2 read GetPointers;
      {$ifdef DYNARRAY_2_IS_STRUCT} default; {$endif}

    {$ifndef DYNARRAY_2_IS_STRUCT}
    { }
    property Values[ItemNum: integer]: TDynArrayItem_2
      read GetValues write SetValues; default;
    {$endif}

    {$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
    procedure SetLength(NewLength: Integer); override;
    {$endif}

    { Add new item at the end of the array. Increases length by 1. }
    procedure Add(const Item: TDynArrayItem_2); overload;
    procedure Add(const Item: TDynArrayItem_2; const DuplicateCount: Cardinal); overload;

    { Increase the length by 1, and return the pointer to the new item at the end. }
    function Add: PDynArrayItem_2; overload;

    procedure AppendArray(const NewItems: array of TDynArrayItem_2); overload;
    procedure AppendArray(const NewItems: array of TDynArrayItem_2; NewItemsAddCount: integer); overload;

    procedure AppendDynArray(Arr: TDynArray_2); overload;
    procedure AppendDynArray(Arr: TDynArray_2; Index, ACount: Integer); overload;

    { Copy the contents of Source to this object.

      This is equivalent to doing @code(Length := 0) and
      @code(AppendDynarray(Source)).
      Maybe some day I will make TDynArray_2 descendant of TPersistent,
      then I will make Assign compatible with the TPersistent.Assign interface.

      Remember that this is *not* a so-called deep copy,
      because if Source contains some pointers we're simply
      copying these pointers (not duplicating their content
      constructing new objects or something like that).
      This is just a simple copy of memory contents
      (well, with proper Initialize/Finalize in case
      DYNARRAY_2_IS_INIT_FINI_TYPE). }
    procedure Assign(Source: TDynArray_2);

    { Clear (like @code(Count := 0)) and then @code(AppendArray(NewItems)). }
    procedure AssignArray(const NewItems: array of TDynArrayItem_2);

    {$ifdef DYNARRAY_2_USE_EQUALITY}
    function EqualItems(const Item1, Item2: TDynArrayItem_2): boolean;
    function Equal(DynArr: TDynArray_2): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem_2): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem_2; ArrCount: Integer): boolean; overload;

    { Search for given item, starting from StartIndex (default is to
      search the whole list). Returns -1 if not found. }
    function IndexOf(const Item: TDynArrayItem_2; StartIndex: Integer = 0): Integer;

    { Searches for and deletes the first item equal to Value.
      Returns the index of removed item (-1 if not found). }
    function Remove(const Value: TDynArrayItem_2): Integer;

    { If the Item is not already present in the array (checked like IndexOf)
      then add this item (like by Add). }
    procedure AddIfNotExists(const Item: TDynArrayItem_2);
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

    { Insert Item at position Index into array.
      Index must be in range 0 .. Count (yes, we can @noAutoLink(insert)
      element at the end of the array).
      After insertion array has Count increased by 1, Items[Index] = Item and all
      items that were on positions [Index..old High] are now at [Index+1..new High]. }
    procedure Insert(Index: integer; const Item: TDynArrayItem_2);

    procedure SetAll(const Item: TDynArrayItem_2);

    { Sort items. All you have to do is to pass some IsSmallerFunc
      -- see @link(KambiUtils.Sort) for specification what it should do,
      but note that @link(TDynArrayItemIsSmallerFunc_2) here gets parameter
      types already casted to TDynArrayItem_2, instead of dirty pointers
      (like IsSmallerFunc that must be passed to @link(KambiUtils.Sort)).

      If you don't give First/LastIndex it assumes FirstIndex = 0 and
      LastIndex = High, so the whole array will be sorted. }
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_2;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_2;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_2;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_2;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;

    {$ifdef DYNARRAY_2_USE_PLUSMINUS_OP}
    function Sum: TDynArrayItem_2;
    {$endif DYNARRAY_2_USE_PLUSMINUS_OP}

    property First: TDynArrayItem_2 read GetFirst write SetFirst;
    property Last : TDynArrayItem_2 read GetLast  write SetLast ;
  end;

{$endif}

{$ifdef read_implementation}

constructor TDynArray_2.Create;
begin
  inherited Create(SizeOf(TDynArrayItem_2), 0)
end;

constructor TDynArray_2.Create(InitialLength: Integer);
begin
  inherited Create(SizeOf(TDynArrayItem_2), InitialLength);
end;

constructor TDynArray_2.CreateCopy(Source: TDynArray_2);
begin
  Create;
  Assign(Source);
end;

function TDynArray_2.GetItems:
  {$ifdef FPC_OBJFPC} PDynArrayItem_2 {$else} PInfiniteArray_2 {$endif};
begin
  Result :=
    {$ifdef FPC_OBJFPC} PDynArrayItem_2 {$else} PInfiniteArray_2 {$endif} (A);
end;

function TDynArray_2.GetItemsArray: PInfiniteArray_2;
begin
  Result := PInfiniteArray_2(A);
end;

function TDynArray_2.GetPointers(ItemNum: integer): PDynArrayItem_2;
begin
  Result := @(Items[ItemNum]);
end;

{$ifndef DYNARRAY_2_IS_STRUCT}
function TDynArray_2.GetValues(ItemNum: integer): TDynArrayItem_2;
begin
  Result := Items[ItemNum];
end;

procedure TDynArray_2.SetValues(ItemNum: integer; const Item: TDynArrayItem_2);
begin
  Items[ItemNum] := Item;
end;
{$endif}

{$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
procedure TDynArray_2.SetLength(NewLength: Integer);
var
  oldlen, i: integer;
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

procedure TDynArray_2.Add(const Item: TDynArrayItem_2);
begin
  IncLength;
  Items[Length-1] := Item;
end;

procedure TDynArray_2.Add(const Item: TDynArrayItem_2; const DuplicateCount: Cardinal);
var
  I, C: Integer;
begin
  C := Count;
  Count := Count + DuplicateCount;
  for I := C to Count - 1 do
    Items[I] := Item;
end;

function TDynArray_2.Add: PDynArrayItem_2;
begin
  IncLength;
  Result := @(Items[Length-1]);
end;

procedure TDynArray_2.AppendArray(const NewItems: array of TDynArrayItem_2;
  NewItemsAddCount: integer);
var
  OldLength: Integer;
{$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
  I: Integer;
{$endif}
begin
  OldLength := Length;
  SetLength(Length + NewItemsAddCount);
{$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
  { We're copying one-by-one, so that Initialize/Finalize
   of items is properly called. }
  for i := 0 to NewItemsAddCount - 1 do
    Items[OldLength + i] := NewItems[i];
{$else}
  { In this case we can copy memory fast, using Move }
  System.Move(
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} NewItems[0],
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} Items[OldLength],
    NewItemsAddCount * SizeOf(TDynArrayItem_2));
{$endif}
end;

procedure TDynArray_2.AppendArray(const NewItems: array of TDynArrayItem_2);
begin
  AppendArray(NewItems, System.High(NewItems)+1);
end;

procedure TDynArray_2.AppendDynArray(Arr: TDynArray_2);
begin
  AppendArray(Arr.ItemsArray^, Arr.Count);
end;

procedure TDynArray_2.AppendDynArray(Arr: TDynArray_2; Index, ACount: Integer);
begin
  AppendArray(PInfiniteArray_2(@Arr.Items[Index])^, ACount);
end;

procedure TDynArray_2.Assign(Source: TDynArray_2);
{$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
var
  i: Integer;
{$endif}
begin
  Length := Source.Length;
  {$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
  { We're copying one-by-one, so that Initialize/Finalize
    of items is properly called. }
  for i := 0 to Source.Length-1 do Items[i] := Source.Items[i];
  {$else}
  { In this case we can copy memory fast, using Move }
  System.Move(
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} Source.Items[0],
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} Items[0],
    Length * SizeOf(TDynArrayItem_2));
  {$endif}
end;

procedure TDynArray_2.AssignArray(const NewItems: array of TDynArrayItem_2);
begin
  Count := 0;
  AppendArray(NewItems);
end;

{$ifdef DYNARRAY_2_USE_EQUALITY}

function TDynArray_2.EqualItems(const Item1, Item2: TDynArrayItem_2): boolean;
begin
  Result :=
    {$ifdef DYNARRAY_2_IS_FUNCTION_METHOD}
      (TMethod(Item1).Code = TMethod(Item2).Code) and
      (TMethod(Item1).Data = TMethod(Item2).Data);
    {$else DYNARRAY_2_IS_FUNCTION_METHOD}
      {$ifdef DYNARRAY_2_USE_EQUALITY_OP}
        ({$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} Item1 =
         {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} Item2);
      {$else}
        {$ifdef DYNARRAY_2_USE_EQUALITY_COMPAREMEM}
        CompareMem(@Item1, @Item2, SizeOf(TDynArrayItem_2));
        {$else}
        Error: one of the DYNARRAY_USE_EQUALITY_* must be defined.
        {$endif}
      {$endif}
    {$endif DYNARRAY_2_IS_FUNCTION_METHOD}
end;

function TDynArray_2.Equal(DynArr: TDynArray_2): boolean;
begin
  Result := Equal(DynArr.ItemsArray^, DynArr.Count);
end;

function TDynArray_2.Equal(const Arr: array of TDynArrayItem_2): boolean;
begin
  Result := Equal(Arr, System.High(Arr)+1);
end;

function TDynArray_2.Equal(const Arr: array of TDynArrayItem_2;
  ArrCount: Integer): boolean;
var
  i: Integer;
begin
  if ArrCount <> Length then begin result := false; Exit end;

  for i := 0 to ArrCount-1 do
    if not EqualItems(Arr[i], Items[i]) then
      begin result := false; Exit end;

  result := true;
end;

function TDynArray_2.IndexOf(const Item: TDynArrayItem_2; StartIndex: Integer): Integer;
begin
  for Result := StartIndex to High do
    if EqualItems(Items[Result], Item) then Exit;
  Result := -1;
end;

function TDynArray_2.Remove(const Value: TDynArrayItem_2): Integer;
begin
  Result := IndexOf(Value);
  if Result <> -1 then Delete(Result);
end;

procedure TDynArray_2.AddIfNotExists(const Item: TDynArrayItem_2);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I = -1 then
    Add(Item);
end;
{$endif}

procedure TDynArray_2.Delete(const Index: Integer; DelCount: Integer);
var i: integer;
begin
  { Make sure Index and DelCount are sensible first }
  if Index >= Count then
    Exit;
  MinTo1st(DelCount, Count - Index);

  {$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
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
      {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} Items[i+DelCount],
      {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_2_IS_FUNCTION} @ {$endif} {$endif} Items[i],
      SizeOf(TDynArrayItem_2));

  { Decrease Length without doing Finalize on the tail of the array. }
  SetLengthNoInitFini(Length - DelCount);
end;

procedure TDynArray_2.Insert(Index: integer; const Item: TDynArrayItem_2);
var
  i: integer;
begin
  IncLength;
  for i := High downto Index+1 do Items[i] := Items[i-1];
  Items[Index] := Item;
end;

procedure TDynArray_2.SetAll(const Item: TDynArrayItem_2);
var
  i: integer;
  ItemAsByte: byte absolute Item;
  {$ifdef FPC}
  ItemAsWord: Word absolute Item;
  ItemAsDWord: DWord absolute Item;
  {$endif}
begin
  {$ifdef DYNARRAY_2_IS_INIT_FINI_TYPE}
  for i := 0 to Length-1 do Items[i] := Item;
  {$else}
  case SizeOf(TDynArrayItem_2) of
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

procedure TDynArray_2.Sort(
  IsSmallerFunc: TDynArrayItemIsSmallerFunc_2;
  CountToUseSimpleSort: Integer);
begin
  Sort(IsSmallerFunc, 0, High, CountToUseSimpleSort);
end;

procedure TDynArray_2.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_2;
  CountToUseSimpleSort: Integer);
begin
  SortByObject(IsSmallerFunc, 0, High, CountToUseSimpleSort);
end;

{ -----------------------------------------------------------------------------
  sortowania; propagacja z metod na Pointerach do metod na TDynArrayItem_2 }

function TDynArray_2.Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
begin
  Result := Sort_IsSmallerFunc(PDynArrayItem_2(Val1)^,PDynArrayItem_2(Val2)^)
end;

function TDynArray_2.Propagate_IsSmallerFuncByObject(
  const Val1, Val2: pointer): boolean;
begin
  Result := Sort_IsSmallerFuncByObject(PDynArrayItem_2(Val1)^,PDynArrayItem_2(Val2)^)
end;

{ sortowania; wlasciwe metody Sort ------------------------------------------ }
{ Self.Propagate_* below required because of fpc 1.0.10 func_ofobject_bug }

procedure TDynArray_2.Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_2;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFunc := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem_2),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFunc,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

procedure TDynArray_2.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_2;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFuncByObject := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem_2),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFuncByObject,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

{$ifdef DYNARRAY_2_USE_PLUSMINUS_OP}
function TDynArray_2.Sum: TDynArrayItem_2;
var
  I: Integer;
begin
  Result := 0; { this will be improved to some macro like DYNARRAY_ZERO_ITEM }
  for I := 0 to High do
    Result := Result + Items[I];
end;
{$endif DYNARRAY_2_USE_PLUSMINUS_OP}

function TDynArray_2.GetFirst: TDynArrayItem_2;
begin
  Result := Items[0];
end;

function TDynArray_2.GetLast: TDynArrayItem_2;
begin
  Result := Items[Count - 1];
end;

procedure TDynArray_2.SetFirst(const Value: TDynArrayItem_2);
begin
  Items[0] := Value;
end;

procedure TDynArray_2.SetLast(const Value: TDynArrayItem_2);
begin
  Items[Count - 1] := Value;
end;

{$endif}
