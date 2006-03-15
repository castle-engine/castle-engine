{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ typy in  : TObjectsListItem_2
  typy out : TObjectsList_2

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

  TObjectsList_2 = class(TObjectsList_Abstract)
  private
    Arr: PArray_TObject;
    fCount,
    { capacity = na ile obiektow naprawde jest zaalokowana pamiec }
    fCapacity: integer;
    function GetItem(i: integer): TObjectsListItem_2;
    procedure SetItem(i: integer; Item: TObjectsListItem_2);
    procedure SetCount(value: integer);
    procedure SetCapacity(value: integer);

    {$ifopt R+}
    procedure CheckIndex(Index: integer; const s: string);
    procedure CheckInsertIndex(Index: integer; const s: string);
    {$endif R+}

    procedure NeededCapacityForOneMore;

    procedure InsertArrayPtr(Index: integer; A: PArray_TObject; ACount: Integer);

    function GetFirst: TObjectsListItem_2;
    procedure SetFirst(const Value: TObjectsListItem_2);
    function GetLast: TObjectsListItem_2;
    procedure SetLast(const Value: TObjectsListItem_2);
  public
    property Items[i: integer]: TObjectsListItem_2
      read GetItem write SetItem; default;

    {ustawianie Counta : zmniejszanie oznacza kasowanie koncowych elementow,
     zwiekszanie - dodawanie nil'i}
    property Count: Integer read fCount write SetCount;

    { Shortcut for Count - 1. Yes, when Count = 0, High is -1. }
    function High: Integer;

    property Capacity: integer read fCapacity write SetCapacity;

    { Shortcut for Items[0]. }
    property First: TObjectsListItem_2 read GetFirst write SetFirst;

    { Shortcut for Items[Count-1]. }
    property Last: TObjectsListItem_2 read GetLast write SetLast;

    { dodawanie ---------------------------------------- }

    { }
    procedure Insert(Index: integer; Item: TObjectsListItem_2);
    procedure InsertArray(Index: integer; const A: array of TObjectsListItem_2);
    procedure InsertList(Index: integer; List: TObjectsList_2);

    { Add always adds at the end of the list.
      In other words, AddXxx(...) is always just a shortcut for
      InsertXxx(Count, ...). }
    procedure Add(Item: TObjectsListItem_2);
    procedure AddArray(const A: array of TObjectsListItem_2);
    procedure AddList(List: TObjectsList_2);

    { szuka zaczynajac od StartIndex obiektu Item , zwraca -1 jesli
      nie znalazl. Jesli nie podasz StartIndex to tak jakbys podal
      StartIndex = 0. }
    function IndexOf(Item: TObjectsListItem_2; StartIndex: integer): integer; overload;
    function IndexOf(Item: TObjectsListItem_2): integer; overload;

    { usuwanie ------------------------------------------------------------ }

    { }
    procedure Delete(Index: integer); overload;

    { Robi IndexOf i Delete(integer), jezeli znalazl. Zwraca czy znalazl
      (czyli czy zrobil Delete(integer)). }
    function Delete(Item: TObjectsListItem_2): boolean; overload;

    { DeleteAll robi Delete(integer) dopoki IndexOf znajduje.
      Zwraca ile elementow usunal (czyli o ile zmalalo Count). }
    function DeleteAll(Item: TObjectsListItem_2): integer;

    { dla kazdego obiektu na liscie kasuje wszystkie jego nastepujace duplikaty }
    procedure DeleteDuplicates;

    { Equivalent to doing Count := 0 }
    procedure Clear;

    { przesuwanie, zmienianie }
    procedure Exchange(index1, index2: integer);
    procedure Move(oldIndex, newIndex: integer);

    { zamieni wszystkie wystapienia OldItem na NewItem }
    procedure ReplaceAll(OldItem, NewItem: TObjectsListItem_2);

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
    constructor CreateFromArray(const InitialItems: array of TObjectsListItem_2);

    { Similiar to CreateFromArray: shortcut for
      Create + AddList(InitialItems) }
    constructor CreateFromList(InitialItems: TObjectsList_2);

    destructor Destroy; override;
  end;

{$endif}

{$ifdef read_implementation}

{$ifopt R+}
procedure TObjectsList_2.CheckIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index >= Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_2 index : %d,'
    +' but should be between 0 and %d (Count-1)', [S, Index, Count-1]);
end;

procedure TObjectsList_2.CheckInsertIndex(Index: integer; const s: string);
begin
 if (Index < 0) or (Index > Count) then
  raise ERangeError.CreateFmt('(%s) Wrong TObjectsList_2 insert index : %d,'
    +' but should be between 0 and %d (Count)', [S, Index, Count]);
end;
{$endif R+}

function TObjectsList_2.GetItem(i: integer): TObjectsListItem_2;
begin
 {$ifopt R+} CheckIndex(i, 'GetItem'); {$endif}
 result := TObjectsListItem_2(Arr^[i])
end;

procedure TObjectsList_2.SetItem(i: integer; Item: TObjectsListItem_2);
begin
 {$ifopt R+} CheckIndex(i, 'SetItem'); {$endif}
 Arr^[i] := Item
end;

procedure TObjectsList_2.SetCapacity(value: integer);
begin
 fCapacity := value;
 ReallocMem(Arr, value*SizeOf(TObject));
end;

procedure TObjectsList_2.SetCount(value: integer);
var i: integer;
begin
 {jezeli to tylko male zmniejszenie/zwiekszenie rozmiaru to nie realokuj pamieci}
 if not Between(value, fCapacity div 2, fCapacity) then
  Capacity := value;

 if value > Count then
  for i := Count to value-1 do Arr^[i] := nil;
 fCount := value;
end;

function TObjectsList_2.High: Integer;
begin
 Result := Count - 1;
end;

function TObjectsList_2.GetFirst: TObjectsListItem_2;
begin
 Result := Items[0];
end;

procedure TObjectsList_2.SetFirst(const Value: TObjectsListItem_2);
begin
 Items[0] := Value;
end;

function TObjectsList_2.GetLast: TObjectsListItem_2;
begin
 Result := Items[Count-1];
end;

procedure TObjectsList_2.SetLast(const Value: TObjectsListItem_2);
begin
 Items[Count-1] := Value;
end;

procedure TObjectsList_2.NeededCapacityForOneMore;
begin
 Capacity := Capacity + 4;
end;

procedure TObjectsList_2.Insert(Index: integer; Item: TObjectsListItem_2);
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

procedure TObjectsList_2.InsertArrayPtr(Index: integer;
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

procedure TObjectsList_2.InsertArray(Index: integer;
  const A: array of TObjectsListItem_2);
begin
 InsertArrayPtr(Index, @A, System.High(A)+1);
end;

procedure TObjectsList_2.InsertList(Index: integer; List: TObjectsList_2);
begin
 InsertArrayPtr(Index, List.Arr, List.Count);
end;

procedure TObjectsList_2.Add(Item: TObjectsListItem_2);
begin
 Insert(Count, Item);
end;

procedure TObjectsList_2.AddArray(const A: array of TObjectsListItem_2);
begin
 InsertArray(Count, A);
end;

procedure TObjectsList_2.AddList(List: TObjectsList_2);
begin
 InsertList(Count, List);
end;

function TObjectsList_2.IndexOf(Item: TObjectsListItem_2; StartIndex: integer): integer;
begin
 for result := StartIndex to Count-1 do
  if Item = Arr^[result] then exit;
 result := -1;
end;

function TObjectsList_2.IndexOf(Item: TObjectsListItem_2): integer;
begin result := IndexOf(Item, 0) end;

procedure TObjectsList_2.Delete(Index: integer);
var i: integer;
begin
 {$ifopt R+} CheckIndex(Index, 'Delete'); {$endif}
 for i := Index to Count-2 do Arr^[i] := Arr^[i+1];
 Dec(fCount);
 if Count <= Capacity div 2 then Capacity := Count;
end;

function TObjectsList_2.Delete(Item: TObjectsListItem_2): boolean;
var Index: Integer;
begin
 Index := IndexOf(Item);
 Result := Index >= 0;
 if Result then Delete(Index);
end;

function TObjectsList_2.DeleteAll(Item: TObjectsListItem_2): integer;
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

procedure TObjectsList_2.DeleteDuplicates;
var i, index: integer;
begin
 i := 0;
 while i < count do
 begin
  index := i+1;
  repeat
   index := IndexOf(Items[i], index);
   if index < 0 then break;
   Delete(index);
  until false;

  Inc(i);
 end;
end;

procedure TObjectsList_2.Clear;
begin Count := 0 end;

procedure TObjectsList_2.Exchange(index1, index2: integer);
var tmp: TObject;
begin
 {$ifopt R+} CheckIndex(index1, 'Exchange');
             CheckIndex(index2, 'Exchange'); {$endif}
 tmp := Arr^[index1];
 Arr^[index1] := Arr^[index2];
 Arr^[index2] := tmp;
end;

procedure TObjectsList_2.Move(oldIndex, newIndex: integer);
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

procedure TObjectsList_2.ReplaceAll(OldItem, NewItem: TObjectsListItem_2);
var i: integer;
begin
 for i := 0 to Count-1 do if Items[i] = OldItem then Items[i] := NewItem;
end;

procedure TObjectsList_2.FreeAndNil(Index: integer);
begin SysUtils.FreeAndNil(Arr^[Index]) end;

procedure TObjectsList_2.FreeContents;
var i: integer;
begin
 for i := 0 to Count-1 do Arr^[i].Free;
 Clear;
end;

constructor TObjectsList_2.Create;
begin
 inherited;
end;

constructor TObjectsList_2.CreateFromArray
  (const InitialItems: array of TObjectsListItem_2);
begin
 Create();
 AddArray(InitialItems);
end;

constructor TObjectsList_2.CreateFromList(InitialItems: TObjectsList_2);
begin
 Create();
 AddList(InitialItems);
end;

destructor TObjectsList_2.Destroy;
begin
 { note : calling Clear (or setting Count := 0) does not guarantee
   really freeing memory. That's why we set here Capacity. }
 Capacity := 0;

 inherited;
end;

{$endif}
