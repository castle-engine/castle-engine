{%MainUnit castleutils.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Our TCastleFastList. }

{$ifdef read_interface}

type
  { Simple fast list. Exposes most common operations, like addition and reading.

    Can be specialized with any type (simple types
    like integers, or records, class instances...) that does not need
    automatic memory management. So don't use with
    e.g. AnsiStrings or COM interfaces or records with them
    (we would not initialize/release such memory properly).

    Does not have any "ownership" (like in TObjectList that can "own" children)
    or notification mechanism (that would call a method / callback every
    time something is added / removed).
    This avoids operations that would force things like "clear" to perform
    one-by-one iteration over items (like notification of removals
    or dropping reference counts).

    Items are compared (by @link(IndexOf), @link(Remove), @link(PerfectlyEquals))
    by their memory contents.

    Manages memory to optimize speed, not conserve memory: operations
    only allocate memory, until you do @link(ReleaseMemory) (or free this
    instance, of course). Aside from this, we only grow, even when you
    call @link(Clear) or decrease @link(Count). }
  {$ifdef FPC}generic{$endif}
  TCastleFastList<T> = class
  public
    type
      { Pointer to the list contents. }
      PtrT = ^T;
  strict private
    type
      TEnumerator = record
      strict private
        FList: TCastleFastList {$ifndef FPC}<T>{$endif};
        FPosition: TListSize;
        function GetCurrent: T; inline;
      public
        constructor Create(const AList: TCastleFastList {$ifndef FPC}<T>{$endif});
        function MoveNext: Boolean; inline;
        property Current: T read GetCurrent;
      end;

    var
      FItems: PtrT;
      { FItems is allocated for SizeOf(T) * FCapacity.
        When FCapacity = 0 <=> FItems = nil. }
      FCapacity: TListSize;
      { Exposed to user count of items.
        Always FCount <= FCapacity. }
      FCount: TListSize;
    function GetItems(const Index: TListSize): T;
    procedure SetItems(const Index: TListSize; const Value: T);
    procedure SetCount(const Value: TListSize);
    procedure SetCapacity(const Value: TListSize);
    { Make sure Capacity is at least MinimumCapacity,
      growing it by a factor (not only to MinimumCapacity)
      to make a series of such calls fast. }
    procedure Grow(const MinimumCapacity: TListSize);
  public
    destructor Destroy; override;

    { How many items are in the list.

      Increasing it adds items filled with zeros.
      Decreasing it only forgets the items at the end, keeping the allocated memory. }
    property Count: TListSize read FCount write SetCount;

    { How many items can be in the list without allocating more memory.
      Setting it to less than @link(Count) also decreases @link(Count). }
    property Capacity: TListSize read FCapacity write SetCapacity;

    { Get or set items in the list. }
    property Items[const Index: TListSize]: T read GetItems write SetItems; default;

    { Access the list contents through a pointer, to get or set them efficiently.

      Use this to directly access a list value, like this:

      @longCode(#
        MyIntList.L[I] := 123;
        // Equivalent but less efficient to syntax without L:
        // MyIntList[I] := 123;

        MyRecordList.L[I].MyField := 123;
        // Never use syntax without L (could modify temporary value):
        // MyRecordList[I].MyField := 123; //< never do this!
      #)

      The above examples of L work only in FPC ObjFpc mode or in Delphi with pointermath "on"
      (see https://docwiki.embarcadero.com/RADStudio/Sydney/en/Pointer_Math_(Delphi) ).
      These are CGE default settings when being compiled with FPC or Delphi,
      so you can depend on them at least in CGE implementation.
      Note that Delphi code using this doesn't need to be compiled with
      pointermath "on", it is enough that this unit is compiled with
      pointermath "on" and then one can use math on PtrT.

      This is in particular useful if you have a list of records and you would like
      to set their fields.  }
    property L: PtrT read FItems;

    { Pointer to the I-th item. }
    function Ptr(const I: TListSize): PtrT;

    {$ifndef FPC}
    { Get and set items, for generic code in Delphi
      (where the class is a generic constraint, like TX3DSimpleMultField).
      @exclude }
    function InternalGetItem(const Index: TListSize): T;
    { @exclude }
    procedure InternalSetItem(const Index: TListSize; const Value: T);
    {$endif}

    { Size of one item, in bytes. }
    function ItemSize: TListSize;

    { Add new item. Returns its index. }
    function Add(const Item: T): TListSize; overload;

    { Increase Count and return pointer to new item.
      Comfortable and efficient way to add a new item that you want to immediately
      initialize. The new item is filled with zeros. }
    function Add: PtrT; overload;

    { Insert an item at the given index, moving the following items. }
    procedure Insert(const Index: TListSize; const Item: T);

    { Insert the items of the array at the given index, moving the following items. }
    procedure InsertRange(const Index: TListSize; const A: array of T);

    { Remove the item at the given index, moving the following items. }
    procedure Delete(const Index: TListSize);

    { Remove DelCount items starting at the given index, moving the following items. }
    procedure DeleteRange(const Index, DelCount: TListSize);

    { Index of the first item equal to Item (comparing memory), -1 if none. }
    function IndexOf(const Item: T): TListSize;

    { Is there an item equal to Item (comparing memory). }
    function Contains(const Item: T): Boolean;

    { Remove the first item equal to Item (comparing memory).
      Returns its index, -1 if none. }
    function Remove(const Item: T): TListSize;

    { Swap the items at indexes Index1 and Index2. }
    procedure Exchange(const Index1, Index2: TListSize);

    { Reverse the order of items. }
    procedure Reverse;

    { The first item. The list must not be empty. }
    function First: T;

    { The last item. The list must not be empty. }
    function Last: T;

    { Clearing is equivalent to setting Count := 0.
      Does not deallocate memory (to keep it for future reuse),
      call @link(ReleaseMemory) if you want to do that. }
    procedure Clear;

    { Set Count and Capacity to 0, deallocating the memory. }
    procedure ReleaseMemory;

    { Make sure Capacity is at least MinimumCapacity. }
    procedure AllocateAtLeast(const MinimumCapacity: TListSize);

    { Make this list a copy of the Source. }
    procedure Assign(const Source: TCastleFastList {$ifndef FPC}<T>{$endif}); overload;
    { Make this list a copy of the array. }
    procedure Assign(const A: array of T); overload;

    { Add all the items of Source. }
    procedure AddRange(const Source: TCastleFastList {$ifndef FPC}<T>{$endif}); overload;
    { Add all the items of the array. }
    procedure AddRange(const A: array of T); overload;

    { Add a subrange of another list here. }
    procedure AddSubRange(const Source: TCastleFastList {$ifndef FPC}<T>{$endif};
      const Index, AddCount: TListSize);

    { Assign here a linear interpolation of two other arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations.

      @raises(ELinearInterpolationImpossible On classes where linear interpolation is not possible,
        e.g. we cannot linearly interpolate lists of booleans.) }
    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: TCastleFastList {$ifndef FPC}<T>{$endif};
      const Index1, Index2, ACount: TListSize); virtual;

    { Assign linear interpolation between two other arrays.
      @raises EListsDifferentCount If V1 and V2 have different count.
      @raises(ELinearInterpolationImpossible On classes where linear interpolation is not possible,
        e.g. we cannot linearly interpolate lists of booleans.) }
    procedure AssignLerp(const Fraction: Single;
      const V1, V2: TCastleFastList {$ifndef FPC}<T>{$endif});

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(
      const SecondValue: TCastleFastList {$ifndef FPC}<T>{$endif}): Boolean;

    { Copy of the items, as a new dynamic array. }
    function ToArray: {$ifdef FPC}specialize{$endif} TArray<T>;

    { Makes "for .. in" work on this list.
      Don't access directly.
      @exclude }
    function GetEnumerator: TEnumerator;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleFastList.TEnumerator ------------------------------------------------- }

function TCastleFastList{$ifndef FPC}<T>{$endif}.TEnumerator.GetCurrent: T;
begin
  Result := FList.L[FPosition];
end;

constructor TCastleFastList{$ifndef FPC}<T>{$endif}.TEnumerator.Create(
  const AList: TCastleFastList {$ifndef FPC}<T>{$endif});
begin
  FList := AList;
  FPosition := -1;
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TCastleFastList<T> ----------------------------------------------------------- }

function TCastleFastList{$ifndef FPC}<T>{$endif}.Ptr(const I: TListSize): PtrT;
begin
  { Cast to PtrUInt is important on Delphi with 64-bit CPU,
    otherwise Delphi could implement the + using signed Int64
    and make range check errors when pointer is > High(Int64)
    (possible on Android/Aarch64). }
  Result := PtrT(PtrUInt(FItems) + PtrUInt(SizeOf(T)) * PtrUInt(I));
end;

{$ifndef FPC}
function TCastleFastList<T>.InternalGetItem(const Index: TListSize): T;
begin
  Result := Ptr(Index)^;
end;

procedure TCastleFastList<T>.InternalSetItem(const Index: TListSize; const Value: T);
begin
  Ptr(Index)^ := Value;
end;
{$endif}

function TCastleFastList{$ifndef FPC}<T>{$endif}.ItemSize: TListSize;
begin
  Result := SizeOf(T);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Grow(const MinimumCapacity: TListSize);
const
  GrowCount = 4;
  GrowFactor = 1.5;
begin
  if MinimumCapacity > FCapacity then
    AllocateAtLeast(Max(MinimumCapacity,
      Max(FCapacity + GrowCount, Trunc(FCapacity * GrowFactor))));
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.Add(const Item: T): TListSize;
begin
  // make sure FCapacity is large enough
  if FCount = FCapacity then
    Grow(FCount + 1);
  Assert(FCount < FCapacity);

  Result := FCount;
  Ptr(FCount)^ := Item;
  Inc(FCount);
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.Add: PtrT;
begin
  Count := Count + 1;
  Result := Ptr(Count - 1);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.SetCount(const Value: TListSize);
begin
  if Value < 0 then
    raise EListError.CreateFmt('Invalid list count %d', [Value]);
  if Value > FCount then
  begin
    Grow(Value);
    FillChar(Ptr(FCount)^, SizeOf(T) * (Value - FCount), 0);
  end;
  FCount := Value;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.SetCapacity(const Value: TListSize);
begin
  if Value < 0 then
    raise EListError.CreateFmt('Invalid list capacity %d', [Value]);
  if Value = 0 then
    ReleaseMemory
  else
  if Value <> FCapacity then
  begin
    ReallocMem(FItems, Value * SizeOf(T));
    FCapacity := Value;
    if FCount > FCapacity then
      FCount := FCapacity;
  end;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Insert(const Index: TListSize; const Item: T);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt('List index %d out of bounds (count %d)', [Index, FCount]);
  if FCount = FCapacity then
    Grow(FCount + 1);
  if Index < FCount then
    System.Move(Ptr(Index)^, Ptr(Index + 1)^, SizeOf(T) * (FCount - Index));
  Ptr(Index)^ := Item;
  Inc(FCount);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.InsertRange(const Index: TListSize; const A: array of T);
begin
  if (Index < 0) or (Index > FCount) then
    raise EListError.CreateFmt('List index %d out of bounds (count %d)', [Index, FCount]);
  if Length(A) = 0 then
    Exit;
  Grow(FCount + Length(A));
  if Index < FCount then
    System.Move(Ptr(Index)^, Ptr(Index + Length(A))^, SizeOf(T) * (FCount - Index));
  System.Move(A[0], Ptr(Index)^, SizeOf(T) * Length(A));
  FCount := FCount + Length(A);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Delete(const Index: TListSize);
begin
  DeleteRange(Index, 1);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.DeleteRange(const Index, DelCount: TListSize);
begin
  if DelCount = 0 then
    Exit;
  if (Index < 0) or (DelCount < 0) or (Index + DelCount > FCount) then
    raise EListError.CreateFmt('List range %d, %d out of bounds (count %d)', [Index, DelCount, FCount]);
  if Index + DelCount < FCount then
    System.Move(Ptr(Index + DelCount)^, Ptr(Index)^, SizeOf(T) * (FCount - Index - DelCount));
  FCount := FCount - DelCount;
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.IndexOf(const Item: T): TListSize;
var
  I: TListSize;
begin
  for I := 0 to FCount - 1 do
    if CompareMem(Ptr(I), @Item, SizeOf(T)) then
      Exit(I);
  Result := -1;
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.Contains(const Item: T): Boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.Remove(const Item: T): TListSize;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Exchange(const Index1, Index2: TListSize);
var
  Tmp: T;
begin
  Tmp := Ptr(Index1)^;
  Ptr(Index1)^ := Ptr(Index2)^;
  Ptr(Index2)^ := Tmp;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Reverse;
var
  I: TListSize;
begin
  for I := 0 to FCount div 2 - 1 do
    Exchange(I, FCount - 1 - I);
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.First: T;
begin
  Result := Items[0];
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.Last: T;
begin
  Result := Items[FCount - 1];
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Clear;
begin
  FCount := 0;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.ReleaseMemory;
begin
  FreeMemNiling(Pointer(FItems));
  FCapacity := 0;
  FCount := 0;
end;

destructor TCastleFastList{$ifndef FPC}<T>{$endif}.Destroy;
begin
  FreeMem(FItems); // simplified version of ReleaseMemory, for max speed
  inherited;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AllocateAtLeast(const MinimumCapacity: TListSize);
begin
  if MinimumCapacity > FCapacity then
  begin
    ReallocMem(FItems, MinimumCapacity * SizeOf(T));
    FCapacity := MinimumCapacity;
  end;
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.GetItems(const Index: TListSize): T;
begin
  Result := Ptr(Index)^;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.SetItems(const Index: TListSize; const Value: T);
begin
  Ptr(Index)^ := Value;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Assign(const Source: TCastleFastList {$ifndef FPC}<T>{$endif});
begin
  AllocateAtLeast(Source.Count);
  if Source.Count <> 0 then
    System.Move(Source.L^, L^, Source.Count * SizeOf(T));
  FCount := Source.Count;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Assign(const A: array of T);
begin
  Clear;
  AddRange(A);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AddRange(const Source: TCastleFastList {$ifndef FPC}<T>{$endif});
begin
  AddSubRange(Source, 0, Source.Count);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AddRange(const A: array of T);
var
  OldCount: TListSize;
begin
  if Length(A) = 0 then
    Exit;
  OldCount := FCount;
  Grow(OldCount + Length(A));
  System.Move(A[0], Ptr(OldCount)^, SizeOf(T) * Length(A));
  FCount := OldCount + Length(A);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AddSubRange(
  const Source: TCastleFastList {$ifndef FPC}<T>{$endif};
  const Index, AddCount: TListSize);
var
  OldCount: TListSize;
begin
  if AddCount = 0 then
    Exit;
  OldCount := FCount;
  Grow(OldCount + AddCount);
  { Source may be Self, so take its pointer after Grow }
  System.Move(Source.Ptr(Index)^, Ptr(OldCount)^, SizeOf(T) * AddCount);
  FCount := OldCount + AddCount;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AssignLerpRange(const Fraction: Single;
  const V1, V2: TCastleFastList {$ifndef FPC}<T>{$endif};
  const Index1, Index2, ACount: TListSize);
begin
  raise ELinearInterpolationImpossible.Create('AssignLerpRange not implemented for ' + ClassName);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AssignLerp(const Fraction: Single;
  const V1, V2: TCastleFastList {$ifndef FPC}<T>{$endif});
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);
  AssignLerpRange(Fraction, V1, V2, 0, 0, V1.Count);
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.PerfectlyEquals(
  const SecondValue: TCastleFastList {$ifndef FPC}<T>{$endif}): Boolean;
begin
  Result :=
    (Count = SecondValue.Count) and
    ((Count = 0) or CompareMem(L, SecondValue.L, SizeOf(T) * Count));
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.ToArray: {$ifdef FPC}specialize{$endif} TArray<T>;
begin
  SetLength(Result, FCount);
  if FCount <> 0 then
    System.Move(L^, Result[0], SizeOf(T) * FCount);
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$endif read_implementation}
