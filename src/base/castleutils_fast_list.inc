{%MainUnit castleutils.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
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

    Manages memory to optimize speed, not conserve memory: operations
    only allocate memory, until you do @link(ReleaseMemory) (or free this
    instance, of course). Aside from this, we only grow, even when you
    call @link(Clear). }
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
        FPosition: Integer;
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
      FCapacity: Integer;
      { Exposed to user count of items.
        Always FCount <= FCapacity. }
      FCount: Integer;
    function GetItems(const Index: Integer): T;
    procedure SetItems(const Index: Integer; const Value: T);
  public
    destructor Destroy; override;

    { How many items are in the list. }
    property Count: Integer read FCount;

    { Get or set items in the list. }
    property Items[const Index: Integer]: T read GetItems write SetItems; default;

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

    { Add new item. }
    procedure Add(const Item: T);

    { Clearing is equivalent to setting Count := 0.
      Does not deallocate memory (to keep it for future reuse),
      call @link(ReleaseMemory) if you want to do that. }
    procedure Clear;

    procedure ReleaseMemory;
    procedure AllocateAtLeast(const MinimumCapacity: Integer);
    procedure Assign(const Source: TCastleFastList {$ifndef FPC}<T>{$endif});
    procedure AddRange(const Source: TCastleFastList {$ifndef FPC}<T>{$endif});

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

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Add(const Item: T);
const
  GrowCount = 4;
  GrowFactor = 1.5;
var
  NewCapacity: Integer;
begin
  // make sure FCapacity is large enough
  if FCount = FCapacity then
  begin
    NewCapacity := Max(FCapacity + GrowCount, Trunc(FCapacity * GrowFactor));
    ReallocMem(FItems, NewCapacity * SizeOf(T));
    FCapacity := NewCapacity;
  end;
  Assert(FCount < FCapacity);

  FItems[FCount] := Item;
  Inc(FCount);
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Clear;
begin
  FCount := 0;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.ReleaseMemory;
begin
  FreeMemNiling(FItems);
  FCapacity := 0;
  FCount := 0;
end;

destructor TCastleFastList{$ifndef FPC}<T>{$endif}.Destroy;
begin
  FreeMem(FItems); // simplified version of ReleaseMemory, for max speed
  inherited;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AllocateAtLeast(const MinimumCapacity: Integer);
begin
  if MinimumCapacity > FCapacity then
  begin
    ReallocMem(FItems, MinimumCapacity * SizeOf(T));
    FCapacity := MinimumCapacity;
  end;
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.GetItems(const Index: Integer): T;
begin
  Result := L[Index];
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.SetItems(const Index: Integer; const Value: T);
begin
  L[Index] := Value;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.Assign(const Source: TCastleFastList {$ifndef FPC}<T>{$endif});
begin
  AllocateAtLeast(Source.Count);
  Move(Source.L^, L^, Source.Count * SizeOf(T));
  FCount := Source.Count;
end;

procedure TCastleFastList{$ifndef FPC}<T>{$endif}.AddRange(const Source: TCastleFastList {$ifndef FPC}<T>{$endif});
var
  NewCount: Integer;
begin
  NewCount := Count + Source.Count;
  AllocateAtLeast(NewCount);
  Move(Source.L^, L[Count], Source.Count * SizeOf(T));
  FCount := NewCount;
end;

function TCastleFastList{$ifndef FPC}<T>{$endif}.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$endif read_implementation}
