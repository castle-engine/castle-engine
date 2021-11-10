{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TStructList }

{$ifdef read_interface}

type
  { List of structures.
    This is just TList class from Generics.Collections, with some useful helpers. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TStructList<T> = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<T>
    {$ifndef FPC}, ICastleItemList<T, TStructList<T>>{$endif})
  {$ifndef FPC}
  strict private
    procedure SetCount(const Value: TListSize);
    function GetCount: TListSize;
    procedure SetCapacity(Value: TListSize);
    function GetCapacity: TListSize;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): Hresult; virtual; stdcall;
  {$endif}
  public
    type
      PtrT = ^T;
      TTypeList = array [0 .. {$ifdef FPC} MaxInt div SizeOf(T) - 1 {$else} 1000 * 1000 {$endif}] of T;
      PTypeList = ^TTypeList;

    { Access the list contents directly through a pointer.

      This is useful if you have a list of records and you would like to set their fields.
      This will not work correctly:

      @longCode(#
      type
        TMyRecord = record MyField: Integer; end;
        TMyRecordList = specialize TGenericStructList<TMyRecord>;
      var
        MyList: TMyRecordList;
      begin
        // ...
        MyList[I].MyField := 123;
      #)

      (It will not work OK because you would modify only a temporary record
      returned by the @code(MyList[I]) getter.)
      Instead, setting by

      @longCode(#
        MyList.List^[I].MyField := 123;
      #)

      will work OK. Or you can use (only in FPC ObjFpc mode) even shorter this:

      @longCode(#
        MyList.L[I].MyField := 123;
      #)

      @seealso L }
    function List: PTypeList;

    { Access the list contents directly through a pointer to T structure.

      This is exactly the same pointer as @link(List), but the type is different:
      this points to a single item.
      This is useful if you have a list of records and you would like to set their fields.
      This allows to use @code(L[I]) instead of @code(List^[I]) (only in FPC ObjFpc mode).

      See the @link(List) description for a more detailed explanation and example.
      @seealso List }
    function L: PtrT;

    {$ifndef FPC}
    function InternalGetItem(const Index: TListSize): T;
    procedure InternalSetItem(const Index: TListSize; const Value: T);
    function GetInternalItems: TStructList<T>;
    //procedure InternalAddSubRange(Source: TList<T>; Index, AddCount: TListSize);

    {$endif}

    { Increase Count and return pointer to new item.
      Comfortable and efficient way to add a new item that you want to immediately
      initialize. }
    function Add: PtrT; overload;

    { Pointer to ith item. }
    function Ptr(const I: TListSize): PtrT;

    procedure Assign(const Source: TStructList {$ifndef CASTLE_OBJFPC}<T>{$endif}); overload;
    procedure Assign(const A: array of T); overload;

    function ItemSize: TListSize;
    {$ifndef FPC}
    procedure AddSubRange(const Source: TStructList<T>; const Index, AddCount: TListSize);
    procedure AssignLerp(const Fraction: Single;
      const V1, V2: TStructList<T>; const Index1, Index2, ACount: TListSize); virtual;
    function PerfectlyEquals(SecondValue: TObject): boolean; virtual; abstract;
    {$endif}

    procedure AddArray(const A: array of T); deprecated 'use AddRange';
    procedure AddList(const Source: TStructList {$ifndef CASTLE_OBJFPC}<T>{$endif}); deprecated 'use AddRange';
    procedure AssignArray(const A: array of T); deprecated 'use Assign';
    {$ifndef FPC}
    property Count: TListSize read GetCount write SetCount;
    {$endif}
  end;

{$endif}

{$ifdef read_implementation}

{ TStructList ---------------------------------------------------------------- }

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.List: PTypeList;
begin
  Result := PTypeList({$ifdef FPC} FItems {$else} inherited List {$endif});
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.L: PtrT;
begin
  Result := PtrT({$ifdef FPC} FItems {$else} inherited List {$endif});
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.Add: PtrT;
begin
  Count := Count + 1;
  Result := Addr(List^[Count - 1]);
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.Ptr(const I: TListSize): PtrT;
begin
  Result := Addr(List^[I]);
end;

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.Assign(const Source: TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif});
begin
  Clear;
  AddRange(Source);
end;

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.Assign(const A: array of T);
begin
  Clear;
  AddRange(A);
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.ItemSize: TListSize;
begin
  Result := SizeOf(T);
end;

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.AddArray(const A: array of T);
begin
  AddRange(A);
end;

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.AddList(const Source: TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif});
begin
  AddRange(Source);
end;

{$ifndef FPC}
procedure TStructList<T>.AddSubRange(const Source: TStructList<T>; const Index, AddCount: TListSize);
var
  OldCount: TListSize;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.List^[Index], List^[OldCount], SizeOf(T) * AddCount);
end;

procedure TStructList<T>.AssignLerp(const Fraction: Single;
  const V1, V2: TStructList<T>; const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
begin
  raise Exception.Create('Not implemented');
end;

{$endif}

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.AssignArray(const A: array of T);
begin
  Assign(A);
end;

{$ifndef FPC}
function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.GetCount: TListSize;
begin
  Result := TList(Self).Count;
end;

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.SetCount(const Value: TListSize);
begin
  inherited Count := Value;
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.GetCapacity: TListSize;
begin
  Result := TList(Self).Capacity;
end;

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.SetCapacity(Value: TListSize);
begin
  inherited Capacity := Value;
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}._AddRef: Integer;
begin
  Result := -1;
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}._Release: Integer;
begin
  Result := -1;
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.InternalGetItem(
  const Index: TListSize): T;
begin
  Result := List^[Index];
end;

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.InternalSetItem(
  const Index: TListSize; const Value: T);
begin
  Items[Index] := Value;
end;

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.GetInternalItems:
  TStructList<T>;
begin
  Result := Self;
end;

{$endif}

{$endif read_implementation}
