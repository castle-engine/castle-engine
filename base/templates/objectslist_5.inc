{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
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

{$ifdef read_interface}

  { }
  TObjectsListIsSmallerFunction_5 =
    function (const A, B: TObjectsListItem_5): boolean of object;

  TObjectsList_5 = class(TObjectsList_Abstract)
  private
    Arr: PArray_TObject;
    fCount,
    { capacity = na ile obiektow naprawde jest zaalokowana pamiec }
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

    {ustawianie Counta : zmniejszanie oznacza kasowanie koncowych elementow,
     zwiekszanie - dodawanie nil'i}
    property Count: Integer read fCount write SetCount;

    procedure IncCount;
    procedure DecCount;

    { Shortcut for Count - 1. Yes, when Count = 0, High is -1. }
    function High: Integer;

    property Capacity: integer read fCapacity write SetCapacity;

    { Shortcut for Items[0]. }
    property First: TObjectsListItem_5 read GetFirst write SetFirst;

    { Shortcut for Items[Count-1]. }
    property Last: TObjectsListItem_5 read GetLast write SetLast;

    function IsFirst(Value: TObjectsListItem_5): boolean;
    function IsLast(Value: TObjectsListItem_5): boolean;

    { Sort Items between FirstIndex and LastIndex.
      FirstIndex and LastIndex must be valid indexes,
      i.e. between 0 and High and FirstIndex <= LastIndex. }
    procedure Sort(FirstIndex, LastIndex: Cardinal;
      IsSmallerFunction: TObjectsListIsSmallerFunction_5); overload;

    { Sorts all items. }
    procedure Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_5); overload;

    { This assigns Source items references and count to our list.
      Note that Source.Capacity is not copied (Capacity is only
      a performance-tuning setting anyway).

      TODO: untested yet! }
    procedure Assign(Source: TObjectsList_5);

    function Equals(SecondValue: TObjectsList_5): boolean;

    { dodawanie ---------------------------------------- }

    { }
    procedure Insert(Index: integer; Item: TObjectsListItem_5);
    procedure InsertArray(Index: integer; const A: array of TObjectsListItem_5);
    procedure InsertList(Index: integer; List: TObjectsList_5);

    { Add always adds at the end of the list.
      In other words, AddXxx(...) is always just a shortcut for
      InsertXxx(Count, ...). }
    procedure Add(Item: TObjectsListItem_5);

    procedure AddIfNotExists(Item: TObjectsListItem_5);

    procedure AddArray(const A: array of TObjectsListItem_5);
    procedure AddList(List: TObjectsList_5);

    { szuka zaczynajac od StartIndex obiektu Item , zwraca -1 jesli
      nie znalazl. }
    function IndexOf(Item: TObjectsListItem_5; StartIndex: integer = 0): integer;

    { usuwanie ------------------------------------------------------------ }

    { }
    procedure Delete(Index: integer); overload;

    { Robi IndexOf i Delete(integer), jezeli znalazl. Zwraca czy znalazl
      (czyli czy zrobil Delete(integer)). }
    function Delete(Item: TObjectsListItem_5): boolean; overload;

    { DeleteAll robi Delete(integer) dopoki IndexOf znajduje.
      Zwraca ile elementow usunal (czyli o ile zmalalo Count). }
    function DeleteAll(Item: TObjectsListItem_5): integer;

    { dla kazdego obiektu na liscie kasuje wszystkie jego nastepujace duplikaty }
    procedure DeleteDuplicates;

    { Equivalent to doing Count := 0 }
    procedure Clear;

    { przesuwanie, zmienianie }
    procedure Exchange(index1, index2: integer);
    procedure Move(oldIndex, newIndex: integer);

    { zamieni wszystkie wystapienia OldItem na NewItem }
    procedure ReplaceAll(OldItem, NewItem: TObjectsListItem_5);

    procedure SetAll(Item: TObjectsListItem_5);

    { Replace first found descendant of ReplaceClass with NewItem.
      In case no descendant of ReplaceClass was found,
      we'll we add NewItem to the list (depending on AddBeginning value:
      at the beginning or at the end of the list).

      If NewItem is @nil, this simply removes the first found
      descendant of ReplaceClass.

      Returns the replaced (or removed) old item. Or @nil, if none was found
      (or there was @nil inside the list).

      The typical use scenario for this method is when NewItem is also
      a descendant from ReplaceClass, and you always keep at most one
      ReplaceClass descendant on the list.
      For example, you have UI controls list (like
      TGLWindow.Controls), and you want your NewItem to be the only instance
      of TGLMenu class inside.
      Moreover, in case order on the list is important (for example on
      TGLWindow.Controls order corresponds to screen depth --- what control
      is under / above each other), you want to place NewItem at the same
      position as previous TGLMenu instance, if any. }
    function MakeSingle(ReplaceClass: TClass; NewItem: TObjectsListItem_5;
      AddBeginning: boolean = false): TObjectsListItem_5;

    { Delete first found descendant of RemoveClass.
      Returns the removed item. Or @nil, if none was found
      (or there was @nil inside the list and it got removed). }
    function Delete(RemoveClass: TClass): TObjectsListItem_5; overload;

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
    constructor CreateFromArray(const InitialItems: array of TObjectsListItem_5);

    { Similiar to CreateFromArray: shortcut for
      Create + AddList(InitialItems) }
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
 result := TObjectsListItem_5(Arr^[i])
end;

procedure TObjectsList_5.SetItem(i: integer; Item: TObjectsListItem_5);
begin
 {$ifopt R+} CheckIndex(i, 'SetItem'); {$endif}
 Arr^[i] := Item
end;

procedure TObjectsList_5.SetCapacity(value: integer);
begin
 fCapacity := value;
 ReallocMem(Arr, value*SizeOf(TObject));
end;

procedure TObjectsList_5.SetCount(value: integer);
var i: integer;
begin
 {jezeli to tylko male zmniejszenie/zwiekszenie rozmiaru to nie realokuj pamieci}
 if not Between(value, fCapacity div 2, fCapacity) then
  Capacity := value;

 if value > Count then
  for i := Count to value-1 do Arr^[i] := nil;
 fCount := value;
end;

procedure TObjectsList_5.IncCount;
begin
  Count := FCount + 1;
end;

procedure TObjectsList_5.DecCount;
begin
  Count := FCount - 1;
end;

function TObjectsList_5.High: Integer;
begin
 Result := FCount - 1;
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
  { A and B are now pointers, just like Arr was a pointer.
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
  SortByObject(Arr, SizeOf(TObject),
    {$ifdef FPC_OBJFPC}@{$endif} ProxyIsSmallerFunction,
    FirstIndex, LastIndex);
end;

procedure TObjectsList_5.Sort(IsSmallerFunction: TObjectsListIsSmallerFunction_5);
begin
  if Count <> 0 then
    Sort(0, High, IsSmallerFunction);
end;

procedure TObjectsList_5.Assign(Source: TObjectsList_5);
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

function TObjectsList_5.Equals(SecondValue: TObjectsList_5): boolean;
begin
  Result :=
    (Count = SecondValue.Count) and
    CompareMem(SecondValue.Arr, Arr, Count * SizeOf(Pointer));
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
 for i := Count downto Index+1 do Arr^[i] := Arr^[i-1];
 Arr^[Index] := Item;
 Inc(fCount);
end;

procedure TObjectsList_5.InsertArrayPtr(Index: integer;
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

procedure TObjectsList_5.InsertArray(Index: integer;
  const A: array of TObjectsListItem_5);
begin
 InsertArrayPtr(Index, @A, System.High(A)+1);
end;

procedure TObjectsList_5.InsertList(Index: integer; List: TObjectsList_5);
begin
 InsertArrayPtr(Index, List.Arr, List.Count);
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

procedure TObjectsList_5.AddList(List: TObjectsList_5);
begin
 InsertList(Count, List);
end;

function TObjectsList_5.IndexOf(Item: TObjectsListItem_5; StartIndex: integer): integer;
begin
 for result := StartIndex to Count-1 do
  if Item = Arr^[result] then exit;
 result := -1;
end;

procedure TObjectsList_5.Delete(Index: integer);
var i: integer;
begin
 {$ifopt R+} CheckIndex(Index, 'Delete'); {$endif}
 for i := Index to Count-2 do Arr^[i] := Arr^[i+1];
 Dec(fCount);
 if Count <= Capacity div 2 then Capacity := Count;
end;

function TObjectsList_5.Delete(Item: TObjectsListItem_5): boolean;
var Index: Integer;
begin
 Index := IndexOf(Item);
 Result := Index >= 0;
 if Result then Delete(Index);
end;

function TObjectsList_5.DeleteAll(Item: TObjectsListItem_5): integer;
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

procedure TObjectsList_5.DeleteDuplicates;
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

procedure TObjectsList_5.Clear;
begin Count := 0 end;

procedure TObjectsList_5.Exchange(index1, index2: integer);
var tmp: TObject;
begin
 {$ifopt R+} CheckIndex(index1, 'Exchange');
             CheckIndex(index2, 'Exchange'); {$endif}
 tmp := Arr^[index1];
 Arr^[index1] := Arr^[index2];
 Arr^[index2] := tmp;
end;

procedure TObjectsList_5.Move(oldIndex, newIndex: integer);
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

  FillDWord(Arr^, Count, PtrUInt(Item));
{$else}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Items[I] := Item;
{$endif}
end;

function TObjectsList_5.MakeSingle(ReplaceClass: TClass; NewItem: TObjectsListItem_5;
  AddBeginning: boolean): TObjectsListItem_5;
var
  I: Integer;
begin
  if NewItem = nil then
  begin
    Result := Delete(ReplaceClass);
    Exit;
  end;

  for I := 0 to Count - 1 do
    if Arr^[I] is ReplaceClass then
    begin
      Result := TObjectsListItem_5(Arr^[I]);
      Arr^[I] := NewItem;
      Exit;
    end;

  Result := nil;
  if AddBeginning then
    Insert(0, NewItem) else
    Insert(Count, NewItem);
end;

function TObjectsList_5.Delete(RemoveClass: TClass): TObjectsListItem_5;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Arr^[I] is RemoveClass then
    begin
      Result := TObjectsListItem_5(Arr^[I]);
      Delete(I);
      Exit;
    end;

  Result := nil;
end;

procedure TObjectsList_5.FreeAndNil(Index: integer);
begin SysUtils.FreeAndNil(Arr^[Index]) end;

procedure TObjectsList_5.FreeContents;
var i: integer;
begin
 for i := 0 to Count-1 do Arr^[i].Free;
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
