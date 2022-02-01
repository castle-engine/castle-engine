{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type

  { List of TCastleUserInterface instances. }
  TCastleUserInterfaceList = class({$ifdef FPC}specialize{$endif} TObjectList<TCastleUserInterface>)
  public
    { Add child control, at the front of other children. }
    procedure InsertFront(const NewItem: TCastleUserInterface); overload;
    procedure InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertFront(const NewItems: TCastleUserInterfaceList); overload;

    { Add child control, at the back of other children. }
    procedure InsertBack(const NewItem: TCastleUserInterface); overload;
    procedure InsertBackIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertBack(const NewItems: TCastleUserInterfaceList); overload;

    { Insert, honoring @link(TCastleUserInterface.KeepInFront). }
    procedure InsertWithZOrder(Index: Integer; const Item: TCastleUserInterface);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleUserInterfaceList ------------------------------------------------------------- }

procedure TCastleUserInterfaceList.InsertFront(const NewItem: TCastleUserInterface);
begin
  InsertWithZOrder(Count, NewItem);
end;

procedure TCastleUserInterfaceList.InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
begin
  if IndexOf(NewItem) = -1 then
    InsertFront(NewItem);
end;

procedure TCastleUserInterfaceList.InsertFront(const NewItems: TCastleUserInterfaceList);
var
  I: Integer;
begin
  for I := 0 to NewItems.Count - 1 do
    InsertFront(NewItems[I]);
end;

procedure TCastleUserInterfaceList.InsertBack(const NewItem: TCastleUserInterface);
begin
  InsertWithZOrder(0, NewItem);
end;

procedure TCastleUserInterfaceList.InsertBackIfNotExists(const NewItem: TCastleUserInterface);
begin
  if IndexOf(NewItem) = -1 then
    InsertBack(NewItem);
end;

procedure TCastleUserInterfaceList.InsertBack(const NewItems: TCastleUserInterfaceList);
var
  I: Integer;
begin
  for I := NewItems.Count - 1 downto 0 do
    InsertBack(NewItems[I]);
end;

procedure TCastleUserInterfaceList.InsertWithZOrder(Index: Integer; const Item: TCastleUserInterface);
var
  I: Integer;
begin
  Index := Clamped(Index, 0, Count);
  if Item.KeepInFront or
     (Count = 0) or
     (Index = 0) or
     (not Items[Index - 1].KeepInFront) then
    Insert(Index, Item) else
  begin
    for I := Index - 1 downto 0 do
      if not Items[I].KeepInFront then
      begin
        Insert(I + 1, Item);
        Exit;
      end;
    { everything has KeepInFront = true }
    Insert(0, Item);
  end;
end;

{$endif read_implementation}
