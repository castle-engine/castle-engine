{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Types in  : TObjectsListItem_6.

  Types out : TObjectsList_6.

  Requires KambiUtils (for PArray_TObject) and
  KambiClassUtils in uses clause (because inherits from
  KambiClassUtils.TObjectsList_Abstract).

  Note: all operations (Items[], Delete etc.) will be range changed
  if you will compile this will range checking ($R+) on.

  No "type" keyword is present at the beginnig of interface part to allow
  these classes to be a chain in mutually dependent class declarations
  (they must occur in one "type" section).
}

{$ifdef read_interface}

  { }
  TObjectsListIsSmallerFunction_6 =
    function (const A, B: TObjectsListItem_6): boolean of object;

  TObjectsList_6 = class(TObjectsList_Abstract)
  private
    Arr: PArray_TObject;
    fCount,
    { capacity = na ile obiektow naprawde jest zaalokowana pamiec }
    fCapacity: integer;
    function GetItem(i: integer): TObjectsListItem_6;
    procedure SetItem(i: integer; Item: TObjectsListItem_6);
    procedure SetCount(value: integer);
    procedure SetCapacity(value: integer);

    {$ifopt R+}
    procedure CheckIndex(Index: integer; const s: string);
    procedure CheckInsertIndex(Index: integer; const s: string);
    {$endif R+}

    procedure NeededCapacityForOneMore;

    procedure InsertArrayPtr(Index: integer; A: PArray_TObject; ACount: Integer);

    function GetFirst: TObjectsListItem_6;
    procedure SetFirst(const Value: TObjectsListItem_6);
    function GetLast: TObjectsListItem_6;
    procedure SetLast(const Value: TObjectsListItem_6);
  private
    GoodIsSmallerFunction: TObjectsListIsSmallerFunction_6;
    function ProxyIsSmallerFunction(const A, B: Pointer): boolean;

  public
    property Items[i: integer]: TObjectsListItem_6
      read GetItem write SetItem; default;

    {ustawianie Counta : zmniejszanie oznacza kasowanie koncowych elementow,
     zwiekszanie - dodawanie nil'i}
    property Count: Integer read fCount write SetCount;

    procedure IncCount;
    procedure DecCount;

    { Shortcut for Count - 1. Yes, when Count = 0, High is -1. }
    function High: Integer;

    property Capacity: integer read fCapacity write SetCapacity;

    { Shortcut for Items[0]. }
    property First: TObjectsListItem_6 read GetFirst write SetFirst;

    { Shortcut for Items[Count-1]. }
    property Last: TObjectsListItem_6 read GetLast write SetLast;

    function IsFirst(Value: TObjectsListItem_6): boolean;
    function IsLast(Value: TObjectsListItem_6): boolean;

    { Sort Items between FirstIndex and LastIndex.
      FirstIndex and LastIndex must be valid indexes,
      i.e. between 0 and High and FirstIndex <= LastIndex. }
    procedure Sort(FirstIndex, LastIndex: Cardinal;
      IsSmallerFunction: TObjectsListIsSmallerFunction_6); overload;

    { Sorts all items. }
    procedure Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_6); overload;

    { This assigns Source items references and count to our list.
      Note that Source.Capacity is not copied (Capacity is only
      a performance-tuning setting anyway).

      TODO: untested yet! }
    procedure Assign(Source: TObjectsList_6);

    function Equals(SecondValue: TObjectsList_6): boolean;

    { dodawanie ---------------------------------------- }

    { }
    procedure Insert(Index: integer; Item: TObjectsListItem_6);
    procedure InsertArray(Index: integer; const A: array of TObjectsListItem_6);
    procedure InsertList(Index: integer; List: TObjectsList_6);

    { Add always adds at the end of the list.
      In other words, AddXxx(...) is always just a shortcut for
      InsertXxx(Count, ...). }
    procedure Add(Item: TObjectsListItem_6);

    procedure AddIfNotExists(Item: TObjectsListItem_6);

    procedure AddArray(const A: array of TObjectsListItem_6);
    procedure AddList(List: TObjectsList_6);

    { szuka zaczynajac od StartIndex obiektu Item , zwraca -1 jesli
      nie znalazl. }
    function IndexOf(Item: TObjectsListItem_6; StartIndex: integer = 0): integer;

    { usuwanie ------------------------------------------------------------ }

    { }
    procedure Delete(Index: integer); overload;

    { Robi IndexOf i Delete(integer), jezeli znalazl. Zwraca czy znalazl
      (czyli czy zrobil Delete(integer)). }
    function Delete(Item: TObjectsListItem_6): boolean; overload;

    { DeleteAll robi Delete(integer) dopoki IndexOf znajduje.
      Zwraca ile elementow usunal (czyli o ile zmalalo Count). }
    function DeleteAll(Item: TObjectsListItem_6): integer;

    { dla kazdego obiektu na liscie kasuje wszystkie jego nastepujace duplikaty }
    procedure DeleteDuplicates;

    { Equivalent to doing Count := 0 }
    procedure Clear;

    { przesuwanie, zmienianie }
    procedure Exchange(index1, index2: integer);
    procedure Move(oldIndex, newIndex: integer);

    { zamieni wszystkie wystapienia OldItem na NewItem }
    procedure ReplaceAll(OldItem, NewItem: TObjectsListItem_6);

    procedure SetAll(Item: TObjectsListItem_6);

    { ------------------------------------------------------------
      rzeczy ktore robia gdzies Free obiektom na liscie }

    { }
    procedure FreeAndNil(Index: integer);

    { robi jakby Clear tyle ze wszystkim wyrzucanym obiektom robi najpierw .Free.
      Jesli nie masz pewnosci - upewnij sie najpierw ze na liscie nie ma
      duplikatow przez DeleteDuplicates }
    procedure FreeContents; override;

    {procedure FreeWithContents; in _Abstract}

    constructor Create;

    { Inits list contents with InitialItems.
      This is just a shortcut for Create + AddArray(InitialItems). }
    constructor CreateFromArray(const InitialItems: array of TObjectsListItem_6);

    { Similiar to CreateFromArray: shortcut for
      Create + AddList(InitialItems) }
    constructor CreateFromList(InitialItems: TObjectsList_6);

    destructor Destroy; override;
  end;

{$endif}

{$ifdef read_implementation}

{$ifopt R+}
procedure TObjectsList_6.CheckIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index >= Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_6 index : %d,'
    +' but should be between 0 and %d (Count-1)', [S, Index, Count-1]);
end;

procedure TObjectsList_6.CheckInsertIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index > Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_6 insert index : %d,'
    +' but should be between 0 and %d (Count)', [S, Index, Count]);
end;
{$endif R+}

function TObjectsList_6.GetItem(i: integer): TObjectsListItem_6;
begin
 {$ifopt R+} CheckIndex(i, 'GetItem'); {$endif}
 result := TObjectsListItem_6(Arr^[i])
end;

procedure TObjectsList_6.SetItem(i: integer; Item: TObjectsListItem_6);
begin
 {$ifopt R+} CheckIndex(i, 'SetItem'); {$endif}
 Arr^[i] := Item
end;

procedure TObjectsList_6.SetCapacity(value: integer);
begin
 fCapacity := value;
 ReallocMem(Arr, value*SizeOf(TObject));
end;

procedure TObjectsList_6.SetCount(value: integer);
var i: integer;
begin
 {jezeli to tylko male zmniejszenie/zwiekszenie rozmiaru to nie realokuj pamieci}
 if not Between(value, fCapacity div 2, fCapacity) then
  Capacity := value;

 if value > Count then
  for i := Count to value-1 do Arr^[i] := nil;
 fCount := value;
end;

procedure TObjectsList_6.IncCount;
begin
  Count := FCount + 1;
end;

procedure TObjectsList_6.DecCount;
begin
  Count := FCount - 1;
end;

function TObjectsList_6.High: Integer;
begin
 Result := FCount - 1;
end;

function TObjectsList_6.GetFirst: TObjectsListItem_6;
begin
 Result := Items[0];
end;

procedure TObjectsList_6.SetFirst(const Value: TObjectsListItem_6);
begin
 Items[0] := Value;
end;

function TObjectsList_6.GetLast: TObjectsListItem_6;
begin
 Result := Items[Count-1];
end;

procedure TObjectsList_6.SetLast(const Value: TObjectsListItem_6);
begin
 Items[Count-1] := Value;
end;

function TObjectsList_6.ProxyIsSmallerFunction(const A, B: Pointer): boolean;
begin
  { A and B are now pointers, just like Arr was a pointer.
    I must dereference A and B now to get their object references. }
  Result := GoodIsSmallerFunction(
    TObjectsListItem_6(PtrObject(A)^),
    TObjectsListItem_6(PtrObject(B)^));
end;

function TObjectsList_6.IsFirst(Value: TObjectsListItem_6): boolean;
begin
  Result := (Count > 0) and (Items[0] = Value);
end;

function TObjectsList_6.IsLast(Value: TObjectsListItem_6): boolean;
begin
  Result := (Count > 0) and (Items[Count - 1] = Value);
end;

procedure TObjectsList_6.Sort(FirstIndex, LastIndex: Cardinal;
  IsSmallerFunction: TObjectsListIsSmallerFunction_6);
begin
  GoodIsSmallerFunction := IsSmallerFunction;
  SortByObject(Arr, SizeOf(TObject),
    {$ifdef FPC_OBJFPC}@{$endif} ProxyIsSmallerFunction,
    FirstIndex, LastIndex);
end;

procedure TObjectsList_6.Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_6);
begin
  if Count <> 0 then
    Sort(0, High, IsSmallerFunction);
end;

procedure TObjectsList_6.Assign(Source: TObjectsList_6);
begin
  Count := Source.Count;

  {$ifdef KAMBI_CHECK_POINTER}
  { It's actually legal to call System.Move with nil pointers
    and Count = 0. But pointer validity (-gc) checks will fail
    (they detect that you try to dereference nil pointers). }
  if Count = 0 then Exit;
  {$endif}

  System.Move(Source.Arr^, Arr^, Count * SizeOf(Pointer));
end;

function TObjectsList_6.Equals(SecondValue: TObjectsList_6): boolean;
begin
  Result :=
    (Count = SecondValue.Count) and
    CompareMem(SecondValue.Arr, Arr, Count * SizeOf(Pointer));
end;

procedure TObjectsList_6.NeededCapacityForOneMore;
begin
 Capacity := Capacity + 4;
end;

procedure TObjectsList_6.Insert(Index: integer; Item: TObjectsListItem_6);
var i: integer;
begin
 { Simple (not optimized) implementation would be:
     InsertArrayPtr(Index, @Item, 1); }

 {$ifopt R+} CheckInsertIndex(Index, 'Insert'); {$endif}

 if Capacity = Count then NeededCapacityForOneMore;
 for i := Count downto Index+1 do Arr^[i] := Arr^[i-1];
 Arr^[Index] := Item;
 Inc(fCount);
end;

procedure TObjectsList_6.InsertArrayPtr(Index: integer;
  A: PArray_TObject; ACount: Integer);
var i: Integer;
    RequiredCapacity: Integer;
begin
 {$ifopt R+} CheckInsertIndex(Index, 'InsertArrayPtr'); {$endif}

 { prepare Capacity, realloc Arr }
 RequiredCapacity := Count + ACount;
 if RequiredCapacity > Capacity then
  Capacity := RequiredCapacity;

 { prepare FCount }
 FCount := FCount + ACount;

 { move Arr contents }
 for i := Count-1 downto Index + ACount do
  Arr^[i] := Arr^[i - ACount];

 { copy A contents to Arr }
 { Not optimized version:
     for i := Index + ACount - 1 downto Index do Arr^[i] := A^[i - Index]; }
 { SizeOf(Pointer) should be SizeOf(TObject) actually, but "SizeOf(TObject)"
   does not compile with FPC 1.0.10 (bug, not present in FPC 1.0.6).
   It's the same thing, anyway. }
 System.Move(A^[0], Arr^[Index], ACount*SizeOf(Pointer));
end;

procedure TObjectsList_6.InsertArray(Index: integer;
  const A: array of TObjectsListItem_6);
begin
 InsertArrayPtr(Index, @A, System.High(A)+1);
end;

procedure TObjectsList_6.InsertList(Index: integer; List: TObjectsList_6);
begin
 InsertArrayPtr(Index, List.Arr, List.Count);
end;

procedure TObjectsList_6.Add(Item: TObjectsListItem_6);
begin
 Insert(Count, Item);
end;

procedure TObjectsList_6.AddIfNotExists(Item: TObjectsListItem_6);
begin
  if IndexOf(Item) = -1 then
    Add(Item);
end;

procedure TObjectsList_6.AddArray(const A: array of TObjectsListItem_6);
begin
 InsertArray(Count, A);
end;

procedure TObjectsList_6.AddList(List: TObjectsList_6);
begin
 InsertList(Count, List);
end;

function TObjectsList_6.IndexOf(Item: TObjectsListItem_6; StartIndex: integer): integer;
begin
 for result := StartIndex to Count-1 do
  if Item = Arr^[result] then exit;
 result := -1;
end;

procedure TObjectsList_6.Delete(Index: integer);
var i: integer;
begin
 {$ifopt R+} CheckIndex(Index, 'Delete'); {$endif}
 for i := Index to Count-2 do Arr^[i] := Arr^[i+1];
 Dec(fCount);
 if Count <= Capacity div 2 then Capacity := Count;
end;

function TObjectsList_6.Delete(Item: TObjectsListItem_6): boolean;
var Index: Integer;
begin
 Index := IndexOf(Item);
 Result := Index >= 0;
 if Result then Delete(Index);
end;

function TObjectsList_6.DeleteAll(Item: TObjectsListItem_6): integer;
var i: integer;
begin
 result := 0;
 i := 0;
 while i < Count do
 begin
  if Items[i] = Item then
   begin Delete(i); Inc(result) end else
   Inc(i);
 end;
end;

procedure TObjectsList_6.DeleteDuplicates;
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

procedure TObjectsList_6.Clear;
begin Count := 0 end;

procedure TObjectsList_6.Exchange(index1, index2: integer);
var tmp: TObject;
begin
 {$ifopt R+} CheckIndex(index1, 'Exchange');
             CheckIndex(index2, 'Exchange'); {$endif}
 tmp := Arr^[index1];
 Arr^[index1] := Arr^[index2];
 Arr^[index2] := tmp;
end;

procedure TObjectsList_6.Move(oldIndex, newIndex: integer);
var Item: TObject;
    i: integer;
begin
 {$ifopt R+} CheckIndex(oldIndex, 'Move');
             CheckIndex(newIndex, 'Move'); {$endif}
 Item := Arr^[oldIndex];
 if oldIndex < newIndex then
 begin
  for i := oldIndex to newIndex-1 do Arr^[i] := Arr^[i+1];
 end else
 begin
  for i := oldIndex downto newIndex+1 do Arr^[i] := Arr^[i-1];
 end;
 Arr^[newIndex] := Item;
end;

procedure TObjectsList_6.ReplaceAll(OldItem, NewItem: TObjectsListItem_6);
var i: integer;
begin
 for i := 0 to Count-1 do if Items[i] = OldItem then Items[i] := NewItem;
end;

procedure TObjectsList_6.SetAll(Item: TObjectsListItem_6);
{$ifdef CPU32}
begin
  {$ifdef KAMBI_CHECK_POINTER}
  { It's actually legal to call FillDWord with nil pointer
    when Count = 0. But pointer validity (-gc) checks will fail
    (they detect that you try to dereference nil pointers). }
  if Count = 0 then Exit;
  {$endif}

  FillDWord(Arr^, Count, PtrUInt(Item));
{$else}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Items[I] := Item;
{$endif}
end;

procedure TObjectsList_6.FreeAndNil(Index: integer);
begin SysUtils.FreeAndNil(Arr^[Index]) end;

procedure TObjectsList_6.FreeContents;
var i: integer;
begin
 for i := 0 to Count-1 do Arr^[i].Free;
 Clear;
end;

constructor TObjectsList_6.Create;
begin
 inherited;
end;

constructor TObjectsList_6.CreateFromArray
  (const InitialItems: array of TObjectsListItem_6);
begin
 Create();
 AddArray(InitialItems);
end;

constructor TObjectsList_6.CreateFromList(InitialItems: TObjectsList_6);
begin
 Create();
 AddList(InitialItems);
end;

destructor TObjectsList_6.Destroy;
begin
 { note : calling Clear (or setting Count := 0) does not guarantee
   really freeing memory. That's why we set here Capacity. }
 Capacity := 0;

 inherited;
end;

{$endif}
