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
  TStructList<T> = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<T>)
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

    { Increase Count and return pointer to new item.
      Comfortable and efficient way to add a new item that you want to immediately
      initialize. }
    function Add: PtrT; overload;

    { Pointer to ith item. }
    function Ptr(I: Integer): PtrT;

    procedure Assign(const Source: TStructList {$ifndef CASTLE_OBJFPC}<T>{$endif}); overload;
    procedure Assign(const A: array of T); overload;

    function ItemSize: SizeInt;

    procedure AddArray(const A: array of T); deprecated 'use AddRange';
    procedure AddList(const Source: TStructList {$ifndef CASTLE_OBJFPC}<T>{$endif}); deprecated 'use AddRange';
    procedure AssignArray(const A: array of T); deprecated 'use Assign';
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

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.Ptr(I: Integer): PtrT;
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

function TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.ItemSize: SizeInt;
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

procedure TStructList{$ifndef CASTLE_OBJFPC}<T>{$endif}.AssignArray(const A: array of T);
begin
  Assign(A);
end;

{$endif read_implementation}
