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

{ Types in  : TObjectsListItem_9.

  Types out : TObjectsList_9.

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
  TObjectsListIsSmallerFunction_9 =
    function (const A, B: TObjectsListItem_9): boolean of object;

  TObjectsList_9 = class(TObjectsList_Abstract)
  private
    List: PArray_TObject;
    fCount,
    fCapacity: integer;
    function GetItem(i: integer): TObjectsListItem_9;
    procedure SetItem(i: integer; Item: TObjectsListItem_9);
    procedure SetCount(value: integer);
    procedure SetCapacity(value: integer);

    {$ifopt R+}
    procedure CheckIndex(Index: integer; const s: string);
    procedure CheckInsertIndex(Index: integer; const s: string);
    {$endif R+}

    procedure NeededCapacityForOneMore;

    procedure InsertArrayPtr(Index: integer; A: PArray_TObject; ACount: Integer);

    function GetFirst: TObjectsListItem_9;
    procedure SetFirst(const Value: TObjectsListItem_9);
    function GetLast: TObjectsListItem_9;
    procedure SetLast(const Value: TObjectsListItem_9);
  private
    GoodIsSmallerFunction: TObjectsListIsSmallerFunction_9;
    function ProxyIsSmallerFunction(const A, B: Pointer): boolean;

  public
    property Items[i: integer]: TObjectsListItem_9
      read GetItem write SetItem; default;

    { How many items are on the list. You can also set this value:
      shrinking count means that we remove ending items,
      growing count means that we add @nil items. }
    property Count: Integer read fCount write SetCount;

    { For how many items we have allocated memory. }
    property Capacity: integer read fCapacity write SetCapacity;

    { Shortcut for Items[0]. }
    property First: TObjectsListItem_9 read GetFirst write SetFirst;

    { Shortcut for Items[Count-1]. }
    property Last: TObjectsListItem_9 read GetLast write SetLast;

    function IsFirst(Value: TObjectsListItem_9): boolean;
    function IsLast(Value: TObjectsListItem_9): boolean;

    { Sort Items between FirstIndex and LastIndex.
      FirstIndex and LastIndex must be valid indexes,
      i.e. between 0 and Count - 1 and FirstIndex <= LastIndex. }
    procedure Sort(FirstIndex, LastIndex: Cardinal;
      IsSmallerFunction: TObjectsListIsSmallerFunction_9); overload;

    { Sorts all items. }
    procedure Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_9); overload;

    { This assigns Source items references and count to our list.
      Note that Source.Capacity is not copied (Capacity is only
      a performance-tuning setting anyway). }
    procedure Assign(Source: TPersistent); override;

    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}

    { Does list contain this Item. Shortcut for @code(IndexOf(Item) <> -1). }
    function Exists(Item: TObjectsListItem_9): boolean;

    { Adding ----------------------------------------------------------------- }

    { }
    procedure Insert(Index: integer; Item: TObjectsListItem_9);
    procedure InsertArray(Index: integer; const A: array of TObjectsListItem_9);
    procedure InsertList(Index: integer; SecondList: TObjectsList_9);

    { Add an item at the end of the list.
      Just a shortcut for Insert(Count, Item). }
    procedure Add(Item: TObjectsListItem_9);

    procedure AddIfNotExists(Item: TObjectsListItem_9);

    procedure AddArray(const A: array of TObjectsListItem_9);
    procedure AddList(SecondList: TObjectsList_9);

    { Search the list for given Item, starting from StartIndex (default is to
      search whole list). Returns -1 if not found. }
    function IndexOf(Item: TObjectsListItem_9; StartIndex: integer = 0): integer;

    { Deleting --------------------------------------------------------------- }

    { }
    procedure Delete(Index: integer); overload;

    { Search the list for given Item, and if found --- remove it.
      Removes only the first instance.
      Returns the index of removed item (-1 if not found). }
    function Remove(Item: TObjectsListItem_9): Integer; overload;

    { Equivalent to doing Count := 0 }
    procedure Clear;

    { Exchange (swap places) two list items. }
    procedure Exchange(index1, index2: integer);

    { Move one list item to another index. Items in between will be
      appropriately shifted by one (to the left or right). }
    procedure Move(oldIndex, newIndex: integer);

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
    constructor CreateFromArray(const InitialItems: array of TObjectsListItem_9);

    { Inits list contents with InitialItems.
      Similiar to CreateFromArray: shortcut for Create + AddList(InitialItems). }
    constructor CreateFromList(InitialItems: TObjectsList_9);

    destructor Destroy; override;
  end;

{$endif}

{$ifdef read_implementation}

{$ifopt R+}
procedure TObjectsList_9.CheckIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index >= Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_9 index : %d,'
    +' but should be between 0 and %d (Count-1)', [S, Index, Count-1]);
end;

procedure TObjectsList_9.CheckInsertIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index > Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_9 insert index : %d,'
    +' but should be between 0 and %d (Count)', [S, Index, Count]);
end;
{$endif R+}

function TObjectsList_9.GetItem(i: integer): TObjectsListItem_9;
begin
 {$ifopt R+} CheckIndex(i, 'GetItem'); {$endif}
 result := TObjectsListItem_9(List^[i])
end;

procedure TObjectsList_9.SetItem(i: integer; Item: TObjectsListItem_9);
begin
 {$ifopt R+} CheckIndex(i, 'SetItem'); {$endif}
 List^[i] := Item
end;

procedure TObjectsList_9.SetCapacity(value: integer);
begin
 fCapacity := value;
 ReallocMem(List, value*SizeOf(TObject));
end;

procedure TObjectsList_9.SetCount(value: integer);
var i: integer;
begin
 {jezeli to tylko male zmniejszenie/zwiekszenie rozmiaru to nie realokuj pamieci}
 if not Between(value, fCapacity div 2, fCapacity) then
  Capacity := value;

 if value > Count then
  for i := Count to value-1 do List^[i] := nil;
 fCount := value;
end;

function TObjectsList_9.GetFirst: TObjectsListItem_9;
begin
 Result := Items[0];
end;

procedure TObjectsList_9.SetFirst(const Value: TObjectsListItem_9);
begin
 Items[0] := Value;
end;

function TObjectsList_9.GetLast: TObjectsListItem_9;
begin
 Result := Items[Count-1];
end;

procedure TObjectsList_9.SetLast(const Value: TObjectsListItem_9);
begin
 Items[Count-1] := Value;
end;

function TObjectsList_9.ProxyIsSmallerFunction(const A, B: Pointer): boolean;
begin
  { A and B are now pointers, just like List was a pointer.
    I must dereference A and B now to get their object references. }
  Result := GoodIsSmallerFunction(
    TObjectsListItem_9(PtrObject(A)^),
    TObjectsListItem_9(PtrObject(B)^));
end;

function TObjectsList_9.IsFirst(Value: TObjectsListItem_9): boolean;
begin
  Result := (Count > 0) and (Items[0] = Value);
end;

function TObjectsList_9.IsLast(Value: TObjectsListItem_9): boolean;
begin
  Result := (Count > 0) and (Items[Count - 1] = Value);
end;

procedure TObjectsList_9.Sort(FirstIndex, LastIndex: Cardinal;
  IsSmallerFunction: TObjectsListIsSmallerFunction_9);
begin
  GoodIsSmallerFunction := IsSmallerFunction;
  SortByObject(List, SizeOf(TObject),
    {$ifdef FPC_OBJFPC}@{$endif} ProxyIsSmallerFunction,
    FirstIndex, LastIndex);
end;

procedure TObjectsList_9.Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_9);
begin
  if Count <> 0 then
    Sort(0, Count - 1, IsSmallerFunction);
end;

procedure TObjectsList_9.Assign(Source: TPersistent);
begin
  if Source is TObjectsList_9 then
  begin
    Count := TObjectsList_9(Source).Count;

    {$ifdef KAMBI_CHECK_POINTER}
    { It's actually legal to call System.Move with nil pointers
      and Count = 0. But pointer validity (-gc) checks will fail
      (they detect that you try to dereference nil pointers). }
    if Count = 0 then Exit;
    {$endif}

    System.Move(TObjectsList_9(Source).List^, List^, Count * SizeOf(Pointer));
  end else
    inherited;
end;

function TObjectsList_9.Equals(SecondValue: TObject): boolean;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TObjectsList_9) and
    (Count = TObjectsList_9(SecondValue).Count) and
    CompareMem(TObjectsList_9(SecondValue).List, List, Count * SizeOf(Pointer));
end;

procedure TObjectsList_9.NeededCapacityForOneMore;
begin
 Capacity := Capacity + 4;
end;

procedure TObjectsList_9.Insert(Index: integer; Item: TObjectsListItem_9);
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

procedure TObjectsList_9.InsertArrayPtr(Index: integer;
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

procedure TObjectsList_9.InsertArray(Index: integer;
  const A: array of TObjectsListItem_9);
begin
 InsertArrayPtr(Index, @A, System.High(A)+1);
end;

procedure TObjectsList_9.InsertList(Index: integer; SecondList: TObjectsList_9);
begin
 InsertArrayPtr(Index, SecondList.List, SecondList.Count);
end;

procedure TObjectsList_9.Add(Item: TObjectsListItem_9);
begin
 Insert(Count, Item);
end;

procedure TObjectsList_9.AddIfNotExists(Item: TObjectsListItem_9);
begin
  if IndexOf(Item) = -1 then
    Add(Item);
end;

procedure TObjectsList_9.AddArray(const A: array of TObjectsListItem_9);
begin
 InsertArray(Count, A);
end;

procedure TObjectsList_9.AddList(SecondList: TObjectsList_9);
begin
 InsertList(Count, SecondList);
end;

function TObjectsList_9.IndexOf(Item: TObjectsListItem_9; StartIndex: integer): integer;
begin
  for result := StartIndex to Count-1 do
    if Item = List^[result] then exit;
  result := -1;
end;

function TObjectsList_9.Exists(Item: TObjectsListItem_9): boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

procedure TObjectsList_9.Delete(Index: integer);
var i: integer;
begin
 {$ifopt R+} CheckIndex(Index, 'Delete'); {$endif}
 for i := Index to Count-2 do List^[i] := List^[i+1];
 Dec(fCount);
 if Count <= Capacity div 2 then Capacity := Count;
end;

function TObjectsList_9.Remove(Item: TObjectsListItem_9): Integer;
begin
 Result := IndexOf(Item);
 if Result <> -1 then Delete(Result);
end;

procedure TObjectsList_9.Clear;
begin Count := 0 end;

procedure TObjectsList_9.Exchange(index1, index2: integer);
var tmp: TObject;
begin
 {$ifopt R+} CheckIndex(index1, 'Exchange');
             CheckIndex(index2, 'Exchange'); {$endif}
 tmp := List^[index1];
 List^[index1] := List^[index2];
 List^[index2] := tmp;
end;

procedure TObjectsList_9.Move(oldIndex, newIndex: integer);
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

procedure TObjectsList_9.FreeAndNil(Index: integer);
begin SysUtils.FreeAndNil(List^[Index]) end;

procedure TObjectsList_9.FreeContents;
var i: integer;
begin
 for i := 0 to Count-1 do List^[i].Free;
 Clear;
end;

constructor TObjectsList_9.Create;
begin
 inherited;
end;

constructor TObjectsList_9.CreateFromArray
  (const InitialItems: array of TObjectsListItem_9);
begin
 Create();
 AddArray(InitialItems);
end;

constructor TObjectsList_9.CreateFromList(InitialItems: TObjectsList_9);
begin
 Create();
 AddList(InitialItems);
end;

destructor TObjectsList_9.Destroy;
begin
 { note : calling Clear (or setting Count := 0) does not guarantee
   really freeing memory. That's why we set here Capacity. }
 Capacity := 0;

 inherited;
end;

{$endif}
