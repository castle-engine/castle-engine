{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Types in  : TObjectsListItem_10.

  Types out : TObjectsList_10.

  Requires KambiUtils (for PArray_TObject) and
  KambiClassUtils in uses clause (because inherits from
  KambiClassUtils.TObjectsList_Abstract).

  Note: all operations (Items[], Delete etc.) will be range changed
  if you will compile this will range checking ($R+) on.

  No "type" keyword is present at the beginnig of interface part to allow
  these classes to be a chain in mutually dependent class declarations
  (they must occur in one "type" section).
}

{ Needed for TOBJECT_HAS_EQUALS }
{$I kambiconf.inc}

{$ifdef read_interface}

  { }
  TObjectsListIsSmallerFunction_10 =
    function (const A, B: TObjectsListItem_10): boolean of object;

  TObjectsList_10 = class(TObjectsList_Abstract)
  private
    List: PArray_TObject;
    fCount,
    fCapacity: integer;
    function GetItem(i: integer): TObjectsListItem_10;
    procedure SetItem(i: integer; Item: TObjectsListItem_10);
    procedure SetCount(value: integer);
    procedure SetCapacity(value: integer);

    {$ifopt R+}
    procedure CheckIndex(Index: integer; const s: string);
    procedure CheckInsertIndex(Index: integer; const s: string);
    {$endif R+}

    procedure NeededCapacityForOneMore;

    procedure InsertArrayPtr(Index: integer; A: PArray_TObject; ACount: Integer);

    function GetFirst: TObjectsListItem_10;
    procedure SetFirst(const Value: TObjectsListItem_10);
    function GetLast: TObjectsListItem_10;
    procedure SetLast(const Value: TObjectsListItem_10);
  private
    GoodIsSmallerFunction: TObjectsListIsSmallerFunction_10;
    function ProxyIsSmallerFunction(const A, B: Pointer): boolean;

  public
    property Items[i: integer]: TObjectsListItem_10
      read GetItem write SetItem; default;

    { How many items are on the list. You can also set this value:
      shrinking count means that we remove ending items,
      growing count means that we add @nil items. }
    property Count: Integer read fCount write SetCount;

    { For how many items we have allocated memory. }
    property Capacity: integer read fCapacity write SetCapacity;

    { Shortcut for Items[0]. }
    property First: TObjectsListItem_10 read GetFirst write SetFirst;

    { Shortcut for Items[Count-1]. }
    property Last: TObjectsListItem_10 read GetLast write SetLast;

    function IsFirst(Value: TObjectsListItem_10): boolean;
    function IsLast(Value: TObjectsListItem_10): boolean;

    { Sort Items between FirstIndex and LastIndex.
      FirstIndex and LastIndex must be valid indexes,
      i.e. between 0 and Count - 1 and FirstIndex <= LastIndex. }
    procedure Sort(FirstIndex, LastIndex: Cardinal;
      IsSmallerFunction: TObjectsListIsSmallerFunction_10); overload;

    { Sorts all items. }
    procedure Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_10); overload;

    { This assigns Source items references and count to our list.
      Note that Source.Capacity is not copied (Capacity is only
      a performance-tuning setting anyway). }
    procedure Assign(Source: TPersistent); override;

    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}

    { Does list contain this Item. Shortcut for @code(IndexOf(Item) <> -1). }
    function Exists(Item: TObjectsListItem_10): boolean;

    { Adding ----------------------------------------------------------------- }

    { }
    procedure Insert(Index: integer; Item: TObjectsListItem_10);
    procedure InsertArray(Index: integer; const A: array of TObjectsListItem_10);
    procedure InsertList(Index: integer; SecondList: TObjectsList_10);

    { Add an item at the end of the list.
      Just a shortcut for Insert(Count, Item). }
    procedure Add(Item: TObjectsListItem_10);

    procedure AddIfNotExists(Item: TObjectsListItem_10);

    procedure AddArray(const A: array of TObjectsListItem_10);
    procedure AddList(SecondList: TObjectsList_10);

    { Search the list for given Item, starting from StartIndex (default is to
      search whole list). Returns -1 if not found. }
    function IndexOf(Item: TObjectsListItem_10; StartIndex: integer = 0): integer;

    { Deleting --------------------------------------------------------------- }

    { }
    procedure Delete(Index: integer); overload;

    { Search the list for given Item, and if found --- remove it.
      Removes only the first instance.
      Returns the index of removed item (-1 if not found). }
    function Remove(Item: TObjectsListItem_10): Integer; overload;

    { Remove all found instances of given Item.
      Returns how many instances were removed (that is, how much Count
      was decreased). }
    function RemoveAll(Item: TObjectsListItem_10): Cardinal;

    { For each item of the list, delete all it's duplicates. }
    procedure DeleteDuplicates;

    { Equivalent to doing Count := 0 }
    procedure Clear;

    { Exchange (swap places) two list items. }
    procedure Exchange(index1, index2: integer);

    { Move one list item to another index. Items in between will be
      appropriately shifted by one (to the left or right). }
    procedure Move(oldIndex, newIndex: integer);

    { Replace all OldItem instances with NewItem. }
    procedure ReplaceAll(OldItem, NewItem: TObjectsListItem_10);

    procedure SetAll(Item: TObjectsListItem_10);

    { Freeing list (items) --------------------------------------------------- }

    { }
    procedure FreeAndNil(Index: integer);

    { Clears list, freeing all list items first.
      Beware: this assumes that there are no duplicates (and, obviously,
      only valid existing object instances). If unsure, see DeleteDuplicates. }
    procedure FreeContents; override;

    constructor Create;

    { Inits list contents with InitialItems.
      This is just a shortcut for Create + AddArray(InitialItems). }
    constructor CreateFromArray(const InitialItems: array of TObjectsListItem_10);

    { Inits list contents with InitialItems.
      Similiar to CreateFromArray: shortcut for Create + AddList(InitialItems). }
    constructor CreateFromList(InitialItems: TObjectsList_10);

    destructor Destroy; override;
  end;

{$endif}

{$ifdef read_implementation}

{$ifopt R+}
procedure TObjectsList_10.CheckIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index >= Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_10 index : %d,'
    +' but should be between 0 and %d (Count-1)', [S, Index, Count-1]);
end;

procedure TObjectsList_10.CheckInsertIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index > Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_10 insert index : %d,'
    +' but should be between 0 and %d (Count)', [S, Index, Count]);
end;
{$endif R+}

function TObjectsList_10.GetItem(i: integer): TObjectsListItem_10;
begin
 {$ifopt R+} CheckIndex(i, 'GetItem'); {$endif}
 result := TObjectsListItem_10(List^[i])
end;

procedure TObjectsList_10.SetItem(i: integer; Item: TObjectsListItem_10);
begin
 {$ifopt R+} CheckIndex(i, 'SetItem'); {$endif}
 List^[i] := Item
end;

procedure TObjectsList_10.SetCapacity(value: integer);
begin
 fCapacity := value;
 ReallocMem(List, value*SizeOf(TObject));
end;

procedure TObjectsList_10.SetCount(value: integer);
var i: integer;
begin
 {jezeli to tylko male zmniejszenie/zwiekszenie rozmiaru to nie realokuj pamieci}
 if not Between(value, fCapacity div 2, fCapacity) then
  Capacity := value;

 if value > Count then
  for i := Count to value-1 do List^[i] := nil;
 fCount := value;
end;

function TObjectsList_10.GetFirst: TObjectsListItem_10;
begin
 Result := Items[0];
end;

procedure TObjectsList_10.SetFirst(const Value: TObjectsListItem_10);
begin
 Items[0] := Value;
end;

function TObjectsList_10.GetLast: TObjectsListItem_10;
begin
 Result := Items[Count-1];
end;

procedure TObjectsList_10.SetLast(const Value: TObjectsListItem_10);
begin
 Items[Count-1] := Value;
end;

function TObjectsList_10.ProxyIsSmallerFunction(const A, B: Pointer): boolean;
begin
  { A and B are now pointers, just like List was a pointer.
    I must dereference A and B now to get their object references. }
  Result := GoodIsSmallerFunction(
    TObjectsListItem_10(PtrObject(A)^),
    TObjectsListItem_10(PtrObject(B)^));
end;

function TObjectsList_10.IsFirst(Value: TObjectsListItem_10): boolean;
begin
  Result := (Count > 0) and (Items[0] = Value);
end;

function TObjectsList_10.IsLast(Value: TObjectsListItem_10): boolean;
begin
  Result := (Count > 0) and (Items[Count - 1] = Value);
end;

procedure TObjectsList_10.Sort(FirstIndex, LastIndex: Cardinal;
  IsSmallerFunction: TObjectsListIsSmallerFunction_10);
begin
  GoodIsSmallerFunction := IsSmallerFunction;
  SortByObject(List, SizeOf(TObject),
    {$ifdef FPC_OBJFPC}@{$endif} ProxyIsSmallerFunction,
    FirstIndex, LastIndex);
end;

procedure TObjectsList_10.Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_10);
begin
  if Count <> 0 then
    Sort(0, Count - 1, IsSmallerFunction);
end;

procedure TObjectsList_10.Assign(Source: TPersistent);
begin
  if Source is TObjectsList_10 then
  begin
    Count := TObjectsList_10(Source).Count;

    {$ifdef KAMBI_CHECK_POINTER}
    { It's actually legal to call System.Move with nil pointers
      and Count = 0. But pointer validity (-gc) checks will fail
      (they detect that you try to dereference nil pointers). }
    if Count = 0 then Exit;
    {$endif}

    System.Move(TObjectsList_10(Source).List^, List^, Count * SizeOf(Pointer));
  end else
    inherited;
end;

function TObjectsList_10.Equals(SecondValue: TObject): boolean;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TObjectsList_10) and
    (Count = TObjectsList_10(SecondValue).Count) and
    CompareMem(TObjectsList_10(SecondValue).List, List, Count * SizeOf(Pointer));
end;

procedure TObjectsList_10.NeededCapacityForOneMore;
begin
 Capacity := Capacity + 4;
end;

procedure TObjectsList_10.Insert(Index: integer; Item: TObjectsListItem_10);
var i: integer;
begin
 { Simple (not optimized) implementation would be:
     InsertArrayPtr(Index, @Item, 1); }

 {$ifopt R+} CheckInsertIndex(Index, 'Insert'); {$endif}

 if Capacity = Count then NeededCapacityForOneMore;
 for i := Count downto Index+1 do List^[i] := List^[i-1];
 List^[Index] := Item;
 Inc(fCount);
end;

procedure TObjectsList_10.InsertArrayPtr(Index: integer;
  A: PArray_TObject; ACount: Integer);
var i: Integer;
    RequiredCapacity: Integer;
begin
 {$ifopt R+} CheckInsertIndex(Index, 'InsertArrayPtr'); {$endif}

 { prepare Capacity, realloc List }
 RequiredCapacity := Count + ACount;
 if RequiredCapacity > Capacity then
  Capacity := RequiredCapacity;

 { prepare FCount }
 FCount := FCount + ACount;

 { move List contents }
 for i := Count-1 downto Index + ACount do
  List^[i] := List^[i - ACount];

 { copy A contents to List }
 { Not optimized version:
     for i := Index + ACount - 1 downto Index do List^[i] := A^[i - Index]; }
 { SizeOf(Pointer) should be SizeOf(TObject) actually, but "SizeOf(TObject)"
   does not compile with FPC 1.0.10 (bug, not present in FPC 1.0.6).
   It's the same thing, anyway. }
 System.Move(A^[0], List^[Index], ACount*SizeOf(Pointer));
end;

procedure TObjectsList_10.InsertArray(Index: integer;
  const A: array of TObjectsListItem_10);
begin
 InsertArrayPtr(Index, @A, System.High(A)+1);
end;

procedure TObjectsList_10.InsertList(Index: integer; SecondList: TObjectsList_10);
begin
 InsertArrayPtr(Index, SecondList.List, SecondList.Count);
end;

procedure TObjectsList_10.Add(Item: TObjectsListItem_10);
begin
 Insert(Count, Item);
end;

procedure TObjectsList_10.AddIfNotExists(Item: TObjectsListItem_10);
begin
  if IndexOf(Item) = -1 then
    Add(Item);
end;

procedure TObjectsList_10.AddArray(const A: array of TObjectsListItem_10);
begin
 InsertArray(Count, A);
end;

procedure TObjectsList_10.AddList(SecondList: TObjectsList_10);
begin
 InsertList(Count, SecondList);
end;

function TObjectsList_10.IndexOf(Item: TObjectsListItem_10; StartIndex: integer): integer;
begin
  for result := StartIndex to Count-1 do
    if Item = List^[result] then exit;
  result := -1;
end;

function TObjectsList_10.Exists(Item: TObjectsListItem_10): boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

procedure TObjectsList_10.Delete(Index: integer);
var i: integer;
begin
 {$ifopt R+} CheckIndex(Index, 'Delete'); {$endif}
 for i := Index to Count-2 do List^[i] := List^[i+1];
 Dec(fCount);
 if Count <= Capacity div 2 then Capacity := Count;
end;

function TObjectsList_10.Remove(Item: TObjectsListItem_10): Integer;
begin
 Result := IndexOf(Item);
 if Result <> -1 then Delete(Result);
end;

function TObjectsList_10.RemoveAll(Item: TObjectsListItem_10): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  I := 0;
  while I < Count do
  begin
    if Items[I] = Item then
      begin Delete(I); Inc(Result) end else
      Inc(I);
  end;
end;

procedure TObjectsList_10.DeleteDuplicates;
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

procedure TObjectsList_10.Clear;
begin Count := 0 end;

procedure TObjectsList_10.Exchange(index1, index2: integer);
var tmp: TObject;
begin
 {$ifopt R+} CheckIndex(index1, 'Exchange');
             CheckIndex(index2, 'Exchange'); {$endif}
 tmp := List^[index1];
 List^[index1] := List^[index2];
 List^[index2] := tmp;
end;

procedure TObjectsList_10.Move(oldIndex, newIndex: integer);
var Item: TObject;
    i: integer;
begin
 {$ifopt R+} CheckIndex(oldIndex, 'Move');
             CheckIndex(newIndex, 'Move'); {$endif}
 Item := List^[oldIndex];
 if oldIndex < newIndex then
 begin
  for i := oldIndex to newIndex-1 do List^[i] := List^[i+1];
 end else
 begin
  for i := oldIndex downto newIndex+1 do List^[i] := List^[i-1];
 end;
 List^[newIndex] := Item;
end;

procedure TObjectsList_10.ReplaceAll(OldItem, NewItem: TObjectsListItem_10);
var i: integer;
begin
 for i := 0 to Count-1 do if Items[i] = OldItem then Items[i] := NewItem;
end;

procedure TObjectsList_10.SetAll(Item: TObjectsListItem_10);
{$ifdef CPU32}
begin
  {$ifdef KAMBI_CHECK_POINTER}
  { It's actually legal to call FillDWord with nil pointer
    when Count = 0. But pointer validity (-gc) checks will fail
    (they detect that you try to dereference nil pointers). }
  if Count = 0 then Exit;
  {$endif}

  FillDWord(List^, Count, PtrUInt(Item));
{$else}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Items[I] := Item;
{$endif}
end;

procedure TObjectsList_10.FreeAndNil(Index: integer);
begin SysUtils.FreeAndNil(List^[Index]) end;

procedure TObjectsList_10.FreeContents;
var i: integer;
begin
 for i := 0 to Count-1 do List^[i].Free;
 Clear;
end;

constructor TObjectsList_10.Create;
begin
 inherited;
end;

constructor TObjectsList_10.CreateFromArray
  (const InitialItems: array of TObjectsListItem_10);
begin
 Create();
 AddArray(InitialItems);
end;

constructor TObjectsList_10.CreateFromList(InitialItems: TObjectsList_10);
begin
 Create();
 AddList(InitialItems);
end;

destructor TObjectsList_10.Destroy;
begin
 { note : calling Clear (or setting Count := 0) does not guarantee
   really freeing memory. That's why we set here Capacity. }
 Capacity := 0;

 inherited;
end;

{$endif}
