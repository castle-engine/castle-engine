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

{ Types in  : TObjectsListItem_5.

  Types out : TObjectsList_5.

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
  TObjectsListIsSmallerFunction_5 =
    function (const A, B: TObjectsListItem_5): boolean of object;

  TObjectsList_5 = class(TObjectsList_Abstract)
  private
    List: PArray_TObject;
    fCount,
    fCapacity: integer;
    function GetItem(i: integer): TObjectsListItem_5;
    procedure SetItem(i: integer; Item: TObjectsListItem_5);
    procedure SetCount(value: integer);
    procedure SetCapacity(value: integer);

    {$ifopt R+}
    procedure CheckIndex(Index: integer; const s: string);
    procedure CheckInsertIndex(Index: integer; const s: string);
    {$endif R+}

    procedure NeededCapacityForOneMore;

    procedure InsertArrayPtr(Index: integer; A: PArray_TObject; ACount: Integer);

    function GetFirst: TObjectsListItem_5;
    procedure SetFirst(const Value: TObjectsListItem_5);
    function GetLast: TObjectsListItem_5;
    procedure SetLast(const Value: TObjectsListItem_5);
  private
    GoodIsSmallerFunction: TObjectsListIsSmallerFunction_5;
    function ProxyIsSmallerFunction(const A, B: Pointer): boolean;

  public
    property Items[i: integer]: TObjectsListItem_5
      read GetItem write SetItem; default;

    { How many items are on the list. You can also set this value:
      shrinking count means that we remove ending items,
      growing count means that we add @nil items. }
    property Count: Integer read fCount write SetCount;

    { For how many items we have allocated memory. }
    property Capacity: integer read fCapacity write SetCapacity;

    { Shortcut for Items[0]. }
    property First: TObjectsListItem_5 read GetFirst write SetFirst;

    { Shortcut for Items[Count-1]. }
    property Last: TObjectsListItem_5 read GetLast write SetLast;

    function IsFirst(Value: TObjectsListItem_5): boolean;
    function IsLast(Value: TObjectsListItem_5): boolean;

    { Sort Items between FirstIndex and LastIndex.
      FirstIndex and LastIndex must be valid indexes,
      i.e. between 0 and Count - 1 and FirstIndex <= LastIndex. }
    procedure Sort(FirstIndex, LastIndex: Cardinal;
      IsSmallerFunction: TObjectsListIsSmallerFunction_5); overload;

    { Sorts all items. }
    procedure Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_5); overload;

    { This assigns Source items references and count to our list.
      Note that Source.Capacity is not copied (Capacity is only
      a performance-tuning setting anyway). }
    procedure Assign(Source: TPersistent); override;

    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}

    { Does list contain this Item. Shortcut for @code(IndexOf(Item) <> -1). }
    function Exists(Item: TObjectsListItem_5): boolean;

    { Adding ----------------------------------------------------------------- }

    { }
    procedure Insert(Index: integer; Item: TObjectsListItem_5);
    procedure InsertArray(Index: integer; const A: array of TObjectsListItem_5);
    procedure InsertList(Index: integer; SecondList: TObjectsList_5);

    { Add an item at the end of the list.
      Just a shortcut for Insert(Count, Item). }
    procedure Add(Item: TObjectsListItem_5);

    procedure AddIfNotExists(Item: TObjectsListItem_5);

    procedure AddArray(const A: array of TObjectsListItem_5);
    procedure AddList(SecondList: TObjectsList_5);

    { Search the list for given Item, starting from StartIndex (default is to
      search whole list). Returns -1 if not found. }
    function IndexOf(Item: TObjectsListItem_5; StartIndex: integer = 0): integer;

    { Deleting --------------------------------------------------------------- }

    { }
    procedure Delete(Index: integer); overload;

    { Search the list for given Item, and if found --- remove it.
      Removes only the first instance.
      Returns the index of removed item (-1 if not found). }
    function Remove(Item: TObjectsListItem_5): Integer; overload;

    { Remove all found instances of given Item.
      Returns how many instances were removed (that is, how much Count
      was decreased). }
    function RemoveAll(Item: TObjectsListItem_5): Cardinal;

    { Equivalent to doing Count := 0 }
    procedure Clear;

    { Exchange (swap places) two list items. }
    procedure Exchange(index1, index2: integer);

    { Move one list item to another index. Items in between will be
      appropriately shifted by one (to the left or right). }
    procedure Move(oldIndex, newIndex: integer);

    { Replace all OldItem instances with NewItem. }
    procedure ReplaceAll(OldItem, NewItem: TObjectsListItem_5);

    procedure SetAll(Item: TObjectsListItem_5);

    { Freeing list (items) --------------------------------------------------- }

    { }
    procedure FreeAndNil(Index: integer);

    { Clears list, freeing all list items first.
      Beware: this assumes that there are no duplicates (and, obviously,
      only valid existing object instances). }
    procedure FreeContents; override;

    constructor Create;

    { Inits list contents with InitialItems.
      This is just a shortcut for Create + AddArray(InitialItems). }
    constructor CreateFromArray(const InitialItems: array of TObjectsListItem_5);

    { Inits list contents with InitialItems.
      Similiar to CreateFromArray: shortcut for Create + AddList(InitialItems). }
    constructor CreateFromList(InitialItems: TObjectsList_5);

    destructor Destroy; override;
  end;

{$endif}

{$ifdef read_implementation}

{$ifopt R+}
procedure TObjectsList_5.CheckIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index >= Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_5 index : %d,'
    +' but should be between 0 and %d (Count-1)', [S, Index, Count-1]);
end;

procedure TObjectsList_5.CheckInsertIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index > Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_5 insert index : %d,'
    +' but should be between 0 and %d (Count)', [S, Index, Count]);
end;
{$endif R+}

function TObjectsList_5.GetItem(i: integer): TObjectsListItem_5;
begin
 {$ifopt R+} CheckIndex(i, 'GetItem'); {$endif}
 result := TObjectsListItem_5(List^[i])
end;

procedure TObjectsList_5.SetItem(i: integer; Item: TObjectsListItem_5);
begin
 {$ifopt R+} CheckIndex(i, 'SetItem'); {$endif}
 List^[i] := Item
end;

procedure TObjectsList_5.SetCapacity(value: integer);
begin
 fCapacity := value;
 ReallocMem(List, value*SizeOf(TObject));
end;

procedure TObjectsList_5.SetCount(value: integer);
var i: integer;
begin
 {jezeli to tylko male zmniejszenie/zwiekszenie rozmiaru to nie realokuj pamieci}
 if not Between(value, fCapacity div 2, fCapacity) then
  Capacity := value;

 if value > Count then
  for i := Count to value-1 do List^[i] := nil;
 fCount := value;
end;

function TObjectsList_5.GetFirst: TObjectsListItem_5;
begin
 Result := Items[0];
end;

procedure TObjectsList_5.SetFirst(const Value: TObjectsListItem_5);
begin
 Items[0] := Value;
end;

function TObjectsList_5.GetLast: TObjectsListItem_5;
begin
 Result := Items[Count-1];
end;

procedure TObjectsList_5.SetLast(const Value: TObjectsListItem_5);
begin
 Items[Count-1] := Value;
end;

function TObjectsList_5.ProxyIsSmallerFunction(const A, B: Pointer): boolean;
begin
  { A and B are now pointers, just like List was a pointer.
    I must dereference A and B now to get their object references. }
  Result := GoodIsSmallerFunction(
    TObjectsListItem_5(PtrObject(A)^),
    TObjectsListItem_5(PtrObject(B)^));
end;

function TObjectsList_5.IsFirst(Value: TObjectsListItem_5): boolean;
begin
  Result := (Count > 0) and (Items[0] = Value);
end;

function TObjectsList_5.IsLast(Value: TObjectsListItem_5): boolean;
begin
  Result := (Count > 0) and (Items[Count - 1] = Value);
end;

procedure TObjectsList_5.Sort(FirstIndex, LastIndex: Cardinal;
  IsSmallerFunction: TObjectsListIsSmallerFunction_5);
begin
  GoodIsSmallerFunction := IsSmallerFunction;
  SortByObject(List, SizeOf(TObject),
    {$ifdef FPC_OBJFPC}@{$endif} ProxyIsSmallerFunction,
    FirstIndex, LastIndex);
end;

procedure TObjectsList_5.Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_5);
begin
  if Count <> 0 then
    Sort(0, Count - 1, IsSmallerFunction);
end;

procedure TObjectsList_5.Assign(Source: TPersistent);
begin
  if Source is TObjectsList_5 then
  begin
    Count := TObjectsList_5(Source).Count;

    {$ifdef KAMBI_CHECK_POINTER}
    { It's actually legal to call System.Move with nil pointers
      and Count = 0. But pointer validity (-gc) checks will fail
      (they detect that you try to dereference nil pointers). }
    if Count = 0 then Exit;
    {$endif}

    System.Move(TObjectsList_5(Source).List^, List^, Count * SizeOf(Pointer));
  end else
    inherited;
end;

function TObjectsList_5.Equals(SecondValue: TObject): boolean;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TObjectsList_5) and
    (Count = TObjectsList_5(SecondValue).Count) and
    CompareMem(TObjectsList_5(SecondValue).List, List, Count * SizeOf(Pointer));
end;

procedure TObjectsList_5.NeededCapacityForOneMore;
begin
 Capacity := Capacity + 4;
end;

procedure TObjectsList_5.Insert(Index: integer; Item: TObjectsListItem_5);
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

procedure TObjectsList_5.InsertArrayPtr(Index: integer;
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

procedure TObjectsList_5.InsertArray(Index: integer;
  const A: array of TObjectsListItem_5);
begin
 InsertArrayPtr(Index, @A, System.High(A)+1);
end;

procedure TObjectsList_5.InsertList(Index: integer; SecondList: TObjectsList_5);
begin
 InsertArrayPtr(Index, SecondList.List, SecondList.Count);
end;

procedure TObjectsList_5.Add(Item: TObjectsListItem_5);
begin
 Insert(Count, Item);
end;

procedure TObjectsList_5.AddIfNotExists(Item: TObjectsListItem_5);
begin
  if IndexOf(Item) = -1 then
    Add(Item);
end;

procedure TObjectsList_5.AddArray(const A: array of TObjectsListItem_5);
begin
 InsertArray(Count, A);
end;

procedure TObjectsList_5.AddList(SecondList: TObjectsList_5);
begin
 InsertList(Count, SecondList);
end;

function TObjectsList_5.IndexOf(Item: TObjectsListItem_5; StartIndex: integer): integer;
begin
  for result := StartIndex to Count-1 do
    if Item = List^[result] then exit;
  result := -1;
end;

function TObjectsList_5.Exists(Item: TObjectsListItem_5): boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

procedure TObjectsList_5.Delete(Index: integer);
var i: integer;
begin
 {$ifopt R+} CheckIndex(Index, 'Delete'); {$endif}
 for i := Index to Count-2 do List^[i] := List^[i+1];
 Dec(fCount);
 if Count <= Capacity div 2 then Capacity := Count;
end;

function TObjectsList_5.Remove(Item: TObjectsListItem_5): Integer;
begin
 Result := IndexOf(Item);
 if Result <> -1 then Delete(Result);
end;

function TObjectsList_5.RemoveAll(Item: TObjectsListItem_5): Cardinal;
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

procedure TObjectsList_5.Clear;
begin Count := 0 end;

procedure TObjectsList_5.Exchange(index1, index2: integer);
var tmp: TObject;
begin
 {$ifopt R+} CheckIndex(index1, 'Exchange');
             CheckIndex(index2, 'Exchange'); {$endif}
 tmp := List^[index1];
 List^[index1] := List^[index2];
 List^[index2] := tmp;
end;

procedure TObjectsList_5.Move(oldIndex, newIndex: integer);
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

procedure TObjectsList_5.ReplaceAll(OldItem, NewItem: TObjectsListItem_5);
var i: integer;
begin
 for i := 0 to Count-1 do if Items[i] = OldItem then Items[i] := NewItem;
end;

procedure TObjectsList_5.SetAll(Item: TObjectsListItem_5);
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

procedure TObjectsList_5.FreeAndNil(Index: integer);
begin SysUtils.FreeAndNil(List^[Index]) end;

procedure TObjectsList_5.FreeContents;
var i: integer;
begin
 for i := 0 to Count-1 do List^[i].Free;
 Clear;
end;

constructor TObjectsList_5.Create;
begin
 inherited;
end;

constructor TObjectsList_5.CreateFromArray
  (const InitialItems: array of TObjectsListItem_5);
begin
 Create();
 AddArray(InitialItems);
end;

constructor TObjectsList_5.CreateFromList(InitialItems: TObjectsList_5);
begin
 Create();
 AddList(InitialItems);
end;

destructor TObjectsList_5.Destroy;
begin
 { note : calling Clear (or setting Count := 0) does not guarantee
   really freeing memory. That's why we set here Capacity. }
 Capacity := 0;

 inherited;
end;

{$endif}
