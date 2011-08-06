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
    TDynArrayItem_12,
    PDynArrayItem_12 -- this must be ^TDynArrayItem_12.
  (in FPC you can also use macros to define these identifiers)

  Including this file will define types :
    TDynArray_12 -- a class that represents an array of TDynArrayItem_12,
    TInfiniteArray_12 -- array[0..<infinite>]of TDynArrayItem_12,
    PInfiniteArray_12 -- ^TInfiniteArray_12,
    TDynArrayItemIsSmallerFunc_12,
    TDynArrayItemIsSmallerFuncByObject_12,
      ( function (const a, b: TDynArrayItem_12): boolean [of object] );

  You can also define some symbols:

  @unorderedList(
    @item(DYNARRAY_12_IS_STRUCT: Then our array is a structure, like a record
      or old-(TP-style)-object.

      In this case we don't define the @code(Values) property, as it would be too
      error-prone. When you define a property that returns a record,
      you can use @code(Values[I].SomeProperty := 123) but it will
      actually not work, as @code(Values[I]) is only a temporary returned
      record.

      The default class property is then the @code(Pointers) property,
      that returns pointer to the appropriate item. This allows you to modify
      the record contents safely.

      Note that, regardless of whether you define DYNARRAY_12_IS_STRUCT or not,
      we always provide the @code(Items) property. @code(Items) is a direct
      pointer to your items array, and so will work perfectly with records too,
      can be modified as usual etc.

      When DYNARRAY_12_IS_STRUCT is not defined, then @code(Values) property
      is defined, and it's even the default class property. So you can then treat
      the instances of this TDynArray_12 as real arrays.)

    @item(DYNARRAY_12_IS_INIT_FINI_TYPE: You have to define this if your type requires
      Initialize/Finalize call. Define this for AnsiStrings, interfaces,
      records containing AnsiStrings or interfaces and such.)

    @item(DYNARRAY_12_USE_EQUALITY_OP: Use the "=" operator on your items
      to define some additional methods like IndexOf.)

    @item(DYNARRAY_12_USE_EQUALITY_COMPAREMEM: Use the CompareMem function
      to compare your items, this will also provide methods like IndexOf,
      although implemented differently.)

    @item(DYNARRAY_12_IS_FUNCTION: Define this if your item is a function / procedure
      pointer (not of object). This is needed in Delphi mode.
      Also, this automatically defines DYNARRAY_12_USE_EQUALITY_OP.)

    @item(DYNARRAY_12_IS_FUNCTION_METHOD: Define this if your item is a method
      (function / procedure of object). Similar to DYNARRAY_12_IS_FUNCTION,
      this is needed to correctly handle methods. We have to differentiate
      between normal functions and methods, because methods cannot be
      (reasonably) compared by the "=" operator, see
      [http://bugs.freepascal.org/view.php?id=11868].)
  )
}

{$ifdef read_interface}

{$ifdef DYNARRAY_12_IS_FUNCTION}
  {$define DYNARRAY_12_USE_EQUALITY_OP}
{$endif}

{$ifdef DYNARRAY_12_USE_EQUALITY_OP}
  {$ifdef DYNARRAY_12_USE_EQUALITY_COMPAREMEM}
    Error: you cannot define both *_USE_EQUALITY_OP and *_USE_EQUALITY_COMPAREMEM.
  {$endif}
{$endif}

{$ifdef DYNARRAY_12_USE_EQUALITY_OP}
  {$define DYNARRAY_12_USE_EQUALITY}
{$endif}

{$ifdef DYNARRAY_12_USE_EQUALITY_COMPAREMEM}
  {$define DYNARRAY_12_USE_EQUALITY}
{$endif}

  { }
  TInfiniteArray_12 = array[0..MaxInt div SizeOf(TDynArrayItem_12)-1]of TDynArrayItem_12;
  PInfiniteArray_12 = ^TInfiniteArray_12;

  TDynArrayItemIsSmallerFunc_12 = function (const a, b: TDynArrayItem_12): boolean;
  TDynArrayItemIsSmallerFuncByObject_12 = function (const a, b: TDynArrayItem_12): boolean of object;

  TDynArray_12 = class(TDynArrayBase)
  private
    {$ifndef DYNARRAY_12_IS_STRUCT}
    function GetValues(ItemNum: integer): TDynArrayItem_12;
    procedure SetValues(ItemNum: integer; const Item: TDynArrayItem_12);
    {$endif}
    function GetPointers(ItemNum: integer): PDynArrayItem_12;
    function GetItems:
      {$ifdef FPC_OBJFPC} PDynArrayItem_12 {$else} PInfiniteArray_12 {$endif};
      {$ifdef SUPPORTS_INLINE} inline; {$endif}
    function GetList: PInfiniteArray_12;
    function GetFirst: TDynArrayItem_12;
    function GetLast: TDynArrayItem_12;
    procedure SetFirst(const Value: TDynArrayItem_12);
    procedure SetLast(const Value: TDynArrayItem_12);
  private
    { Helpers to implement Sort() }
    Sort_IsSmallerFunc: TDynArrayItemIsSmallerFunc_12;
    Sort_IsSmallerFuncByObject: TDynArrayItemIsSmallerFuncByObject_12;
    function Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
    function Propagate_IsSmallerFuncByObject(const Val1, Val2: pointer): boolean;
  public
    constructor Create;

    { Direct access to your items, you can both read and write using this.

      Note that it has different types:
      for Delphi (and FPC Delphi mode) it has PInfiniteArray_12 type,
      for ObjFpc mode is has PDynArrayItem_12.
      This way you can always access items like @code(Items[Index])
      (no need to write @code(Items^[Index])). }
    property Items: {$ifdef FPC_OBJFPC} PDynArrayItem_12 {$else}
      PInfiniteArray_12 {$endif} read GetItems;

    { Pointer to your items. Exactly like @link(Items), but always of
      PInfiniteArray_12 type.

      This means that you can always read/write items like @code(List^[Index]).
      Under ObjFpc you will not be able to write
      @code(List[Index]) (i.e. omit this ^ character). }
    property List: PInfiniteArray_12 read GetList;

    {$ifndef DYNARRAY_12_IS_STRUCT}
    { }
    property Values[ItemNum: integer]: TDynArrayItem_12
      read GetValues write SetValues; default;
    {$endif}

    {$ifdef DYNARRAY_12_IS_INIT_FINI_TYPE}
    procedure SetCount(NewCount: Integer); override;
    {$endif}

    { Add new item at the end of the array. Increases Count by 1. }
    procedure Add(const Item: TDynArrayItem_12); overload;
    procedure Add(const Item: TDynArrayItem_12; const DuplicateCount: Cardinal); overload;

    { Increase the Count by 1, and return the pointer to the new item at the end. }
    function Add: PDynArrayItem_12; overload;

    { Copy the contents of Source to this object.

      This is equivalent to doing @code(Count := 0) and
      @code(AddList(Source)).
      Maybe some day I will make TDynArray_12 descendant of TPersistent,
      then I will make Assign compatible with the TPersistent.Assign interface.

      Remember that this is *not* a so-called deep copy,
      because if Source contains some pointers we're simply
      copying these pointers (not duplicating their content
      constructing new objects or something like that).
      This is just a simple copy of memory contents
      (well, with proper Initialize/Finalize in case
      DYNARRAY_12_IS_INIT_FINI_TYPE). }
    procedure Assign(Source: TDynArray_12);

    {$ifdef DYNARRAY_12_USE_EQUALITY}
    function EqualItems(const Item1, Item2: TDynArrayItem_12): boolean;
    function Equal(DynArr: TDynArray_12): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem_12): boolean; overload;
    function Equal(const Arr: array of TDynArrayItem_12; ArrCount: Integer): boolean; overload;

    { Search for given item, starting from StartIndex (default is to
      search the whole list). Returns -1 if not found. }
    function IndexOf(const Item: TDynArrayItem_12; StartIndex: Integer = 0): Integer;

    { Searches for and deletes the first item equal to Value.
      Returns the index of removed item (-1 if not found). }
    function Remove(const Value: TDynArrayItem_12): Integer;

    { If the Item is not already present in the array (checked like IndexOf)
      then add this item (like by Add). }
    procedure AddIfNotExists(const Item: TDynArrayItem_12);
    {$endif}

    { Delete item of given Index from the array.
      Count of array will be decreased by 1,
      and the last items of array will be shifted to the left. }
    procedure Delete(const Index: Integer);

    { Insert Item at position Index.
      Index must be in range 0 .. Count (yes, we can @noAutoLink(insert)
      element at the end of the array). The following items in the array
      are shifted to the right and arrat Count is increased by 1. }
    procedure Insert(Index: integer; const Item: TDynArrayItem_12);

    procedure SetAll(const Item: TDynArrayItem_12);

    { Sort items. All you have to do is to pass some IsSmallerFunc
      -- see @link(KambiUtils.Sort) for specification what it should do,
      but note that @link(TDynArrayItemIsSmallerFunc_12) here gets parameter
      types already casted to TDynArrayItem_12, instead of dirty pointers
      (like IsSmallerFunc that must be passed to @link(KambiUtils.Sort)).

      If you don't give First/LastIndex it assumes FirstIndex = 0 and
      LastIndex = Count - 1, so the whole array will be sorted. }
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_12;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_12;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_12;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;
    procedure SortByObject(IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_12;
      FirstIndex, LastIndex: integer;
      CountToUseSimpleSort: Integer = DefaultCountToUseSimpleSort); overload;

    property First: TDynArrayItem_12 read GetFirst write SetFirst;
    property Last : TDynArrayItem_12 read GetLast  write SetLast ;
  end;

{$endif}

{$ifdef read_implementation}

constructor TDynArray_12.Create;
begin
  inherited Create(SizeOf(TDynArrayItem_12), 0)
end;

function TDynArray_12.GetItems:
  {$ifdef FPC_OBJFPC} PDynArrayItem_12 {$else} PInfiniteArray_12 {$endif};
begin
  Result :=
    {$ifdef FPC_OBJFPC} PDynArrayItem_12 {$else} PInfiniteArray_12 {$endif} (FList);
end;

function TDynArray_12.GetList: PInfiniteArray_12;
begin
  Result := PInfiniteArray_12(FList);
end;

function TDynArray_12.GetPointers(ItemNum: integer): PDynArrayItem_12;
begin
  Result := @(Items[ItemNum]);
end;

{$ifndef DYNARRAY_12_IS_STRUCT}
function TDynArray_12.GetValues(ItemNum: integer): TDynArrayItem_12;
begin
  Result := Items[ItemNum];
end;

procedure TDynArray_12.SetValues(ItemNum: integer; const Item: TDynArrayItem_12);
begin
  Items[ItemNum] := Item;
end;
{$endif}

{$ifdef DYNARRAY_12_IS_INIT_FINI_TYPE}
procedure TDynArray_12.SetCount(NewCount: Integer);
var
  oldlen, i: integer;
begin
  if NewCount < Count then
  begin
    for i := NewCount to Count-1 do Finalize(Items[i]);
    inherited SetCount(NewCount);
  end else
  begin
    oldlen := Count;
    inherited SetCount(NewCount);
    for i := oldlen to NewCount-1 do Initialize(Items[i]);
  end;
end;
{$endif}

procedure TDynArray_12.Add(const Item: TDynArrayItem_12);
begin
  IncCount;
  Items[Count-1] := Item;
end;

procedure TDynArray_12.Add(const Item: TDynArrayItem_12; const DuplicateCount: Cardinal);
var
  I, C: Integer;
begin
  C := Count;
  Count := Count + DuplicateCount;
  for I := C to Count - 1 do
    Items[I] := Item;
end;

function TDynArray_12.Add: PDynArrayItem_12;
begin
  IncCount;
  Result := @(Items[Count-1]);
end;

procedure TDynArray_12.Assign(Source: TDynArray_12);
{$ifdef DYNARRAY_12_IS_INIT_FINI_TYPE}
var
  i: Integer;
{$endif}
begin
  Count := Source.Count;
  {$ifdef DYNARRAY_12_IS_INIT_FINI_TYPE}
  { We're copying one-by-one, so that Initialize/Finalize
    of items is properly called. }
  for i := 0 to Source.Count-1 do Items[i] := Source.Items[i];
  {$else}
  { In this case we can copy memory fast, using Move }
  System.Move(
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_12_IS_FUNCTION} @ {$endif} {$endif} Source.Items[0],
    {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_12_IS_FUNCTION} @ {$endif} {$endif} Items[0],
    Count * SizeOf(TDynArrayItem_12));
  {$endif}
end;

{$ifdef DYNARRAY_12_USE_EQUALITY}

function TDynArray_12.EqualItems(const Item1, Item2: TDynArrayItem_12): boolean;
begin
  Result :=
    {$ifdef DYNARRAY_12_IS_FUNCTION_METHOD}
      (TMethod(Item1).Code = TMethod(Item2).Code) and
      (TMethod(Item1).Data = TMethod(Item2).Data);
    {$else DYNARRAY_12_IS_FUNCTION_METHOD}
      {$ifdef DYNARRAY_12_USE_EQUALITY_OP}
        ({$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_12_IS_FUNCTION} @ {$endif} {$endif} Item1 =
         {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_12_IS_FUNCTION} @ {$endif} {$endif} Item2);
      {$else}
        {$ifdef DYNARRAY_12_USE_EQUALITY_COMPAREMEM}
        CompareMem(@Item1, @Item2, SizeOf(TDynArrayItem_12));
        {$else}
        Error: one of the DYNARRAY_USE_EQUALITY_* must be defined.
        {$endif}
      {$endif}
    {$endif DYNARRAY_12_IS_FUNCTION_METHOD}
end;

function TDynArray_12.Equal(DynArr: TDynArray_12): boolean;
begin
  Result := Equal(DynArr.List^, DynArr.Count);
end;

function TDynArray_12.Equal(const Arr: array of TDynArrayItem_12): boolean;
begin
  Result := Equal(Arr, High(Arr)+1);
end;

function TDynArray_12.Equal(const Arr: array of TDynArrayItem_12;
  ArrCount: Integer): boolean;
var
  i: Integer;
begin
  if ArrCount <> Count then begin result := false; Exit end;

  for i := 0 to ArrCount-1 do
    if not EqualItems(Arr[i], Items[i]) then
      begin result := false; Exit end;

  result := true;
end;

function TDynArray_12.IndexOf(const Item: TDynArrayItem_12; StartIndex: Integer): Integer;
begin
  for Result := StartIndex to Count - 1 do
    if EqualItems(Items[Result], Item) then Exit;
  Result := -1;
end;

function TDynArray_12.Remove(const Value: TDynArrayItem_12): Integer;
begin
  Result := IndexOf(Value);
  if Result <> -1 then Delete(Result);
end;

procedure TDynArray_12.AddIfNotExists(const Item: TDynArrayItem_12);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I = -1 then
    Add(Item);
end;
{$endif}

procedure TDynArray_12.Delete(const Index: Integer);
var i: integer;
begin
  { Make sure Index is sensible }
  if (Index < 0) or (Index >= Count) then
    Exit;

  {$ifdef DYNARRAY_12_IS_INIT_FINI_TYPE}
  Finalize(Items[Index]);
  {$endif}

  { This copies elements from Index+Count .. Count-1
    to positions Index .. Count -Count -1 using direct memory
    copying (Move), so it doesn't care about Initialize/Finalize.
    This way it's fast. }
  for i := Index to Count - 2 do
    { Instead of Items[i] := Items[i + 1]; }
    System.Move(
      {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_12_IS_FUNCTION} @ {$endif} {$endif} Items[i+1],
      {$ifndef FPC_OBJFPC} {$ifdef DYNARRAY_12_IS_FUNCTION} @ {$endif} {$endif} Items[i],
      SizeOf(TDynArrayItem_12));

  { Decrease Count without doing Finalize on the tail of the array. }
  SetCountNoInitFini(Count - 1);
end;

procedure TDynArray_12.Insert(Index: integer; const Item: TDynArrayItem_12);
var
  i: integer;
begin
  IncCount;
  for i := Count - 1 downto Index+1 do Items[i] := Items[i-1];
  Items[Index] := Item;
end;

procedure TDynArray_12.SetAll(const Item: TDynArrayItem_12);
var
  i: integer;
  ItemAsByte: byte absolute Item;
  {$ifdef FPC}
  ItemAsWord: Word absolute Item;
  ItemAsDWord: DWord absolute Item;
  {$endif}
begin
  {$ifdef DYNARRAY_12_IS_INIT_FINI_TYPE}
  for i := 0 to Count-1 do Items[i] := Item;
  {$else}
  case SizeOf(TDynArrayItem_12) of
    1: System.FillChar(Items^, Count, ItemAsByte);
    {$ifdef FPC}
    2: FillWord(Items^, Count, ItemAsWord);
    4: FillDWord(Items^, Count, ItemAsDWord);
    {$endif}
    else
      for i := 0 to Count-1 do Items[i] := Item;
  end;
  {$endif}
end;

{ sortowania; najpierw Sort bez parametrow *Index --------------------------- }

procedure TDynArray_12.Sort(
  IsSmallerFunc: TDynArrayItemIsSmallerFunc_12;
  CountToUseSimpleSort: Integer);
begin
  Sort(IsSmallerFunc, 0, Count - 1, CountToUseSimpleSort);
end;

procedure TDynArray_12.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_12;
  CountToUseSimpleSort: Integer);
begin
  SortByObject(IsSmallerFunc, 0, Count - 1, CountToUseSimpleSort);
end;

{ -----------------------------------------------------------------------------
  sortowania; propagacja z metod na Pointerach do metod na TDynArrayItem_12 }

function TDynArray_12.Propagate_IsSmallerFunc(const Val1, Val2: pointer): boolean;
begin
  Result := Sort_IsSmallerFunc(PDynArrayItem_12(Val1)^,PDynArrayItem_12(Val2)^)
end;

function TDynArray_12.Propagate_IsSmallerFuncByObject(
  const Val1, Val2: pointer): boolean;
begin
  Result := Sort_IsSmallerFuncByObject(PDynArrayItem_12(Val1)^,PDynArrayItem_12(Val2)^)
end;

{ sortowania; wlasciwe metody Sort ------------------------------------------ }
{ Self.Propagate_* below required because of fpc 1.0.10 func_ofobject_bug }

procedure TDynArray_12.Sort(IsSmallerFunc: TDynArrayItemIsSmallerFunc_12;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFunc := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem_12),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFunc,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

procedure TDynArray_12.SortByObject(
  IsSmallerFunc: TDynArrayItemIsSmallerFuncByObject_12;
  FirstIndex, LastIndex: integer;
  CountToUseSimpleSort: Integer);
begin
 Sort_IsSmallerFuncByObject := IsSmallerFunc;
 KambiUtils.SortByObject(Items, SizeOf(TDynArrayItem_12),
   {$ifdef FPC_OBJFPC} @ {$endif} Self.Propagate_IsSmallerFuncByObject,
   FirstIndex, LastIndex,
   CountToUseSimpleSort);
end;

function TDynArray_12.GetFirst: TDynArrayItem_12;
begin
  Result := Items[0];
end;

function TDynArray_12.GetLast: TDynArrayItem_12;
begin
  Result := Items[Count - 1];
end;

procedure TDynArray_12.SetFirst(const Value: TDynArrayItem_12);
begin
  Items[0] := Value;
end;

procedure TDynArray_12.SetLast(const Value: TDynArrayItem_12);
begin
  Items[Count - 1] := Value;
end;

{$endif}
