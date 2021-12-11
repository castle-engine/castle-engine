{
  Copyright 2002-2021 Michalis Kamburelis.

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
  ELinearInterpolationImpossible = class(Exception);

  { List of structures.
    This is just TList class from Generics.Collections, with some useful helpers. }
  {$ifdef FPC}generic{$endif}
  TStructList<T> = class({$ifdef FPC}specialize{$endif} TList<T>)
  public
    type
      PtrT = ^T;
      { TODO: Can we calculate better size on Delphi?
        Now we just assume that T has max size 16 * 16.
        This declaration will fail if you try to use TStructList with larger T.
        Note: we cannot declare list size too small, or using it may fail with range check error
        (e.g. Delphi, in IndexedPolygons_TrianglesCount, testcase: build_3d_object_by_code_2_tunnel. }
      TTypeList = array [0 .. MaxInt div {$ifdef FPC}SizeOf(T){$else}(16 * 16){$endif} - 1] of T;
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
    {$endif}

    { Increase Count and return pointer to new item.
      Comfortable and efficient way to add a new item that you want to immediately
      initialize. }
    function Add: PtrT; overload;

    { Pointer to ith item. }
    function Ptr(const I: TListSize): PtrT;

    procedure Assign(const Source: TStructList {$ifndef FPC}<T>{$endif}); overload;
    procedure Assign(const A: array of T); overload;

    function ItemSize: TListSize;

    { Add a subrange of another list here.

      This method may ignore the OnNotify mechanism, for the sake of fast execution. }
    procedure AddSubRange(const Source: TStructList {$ifndef FPC}<T>{$endif};
      const Index, AddCount: TListSize);

    { Assign here a linear interpolation of two other arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations.

      @raises(ELinearInterpolationImpossible On classes where linear interpolation is not possible,
        e.g. we cannot linearly interpolate lists of strings.) }
    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: TStructList {$ifndef FPC}<T>{$endif};
      const Index1, Index2, ACount: TListSize); virtual;

    { Assign linear interpolation between two other arrays.
      @raises EListsDifferentCount If V1 and V2 have different count.
      @raises(ELinearInterpolationImpossible On classes where linear interpolation is not possible,
        e.g. we cannot linearly interpolate lists of strings.) }
    procedure AssignLerp(const Fraction: Single;
      const V1, V2: TStructList {$ifndef FPC}<T>{$endif});

    { Does the SecondValue have equal type, length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(
      const SecondValue: TStructList{$ifndef FPC}<T>{$endif}): Boolean;

    procedure AddArray(const A: array of T); deprecated 'use AddRange';
    procedure AddList(const Source: TStructList {$ifndef FPC}<T>{$endif}); deprecated 'use AddRange';
    procedure AssignArray(const A: array of T); deprecated 'use Assign';
  end;

{$endif}

{$ifdef read_implementation}

{ TStructList ---------------------------------------------------------------- }

function TStructList{$ifndef FPC}<T>{$endif}.List: PTypeList;
begin
  Result := PTypeList({$ifdef FPC} FItems {$else} inherited List {$endif});
end;

function TStructList{$ifndef FPC}<T>{$endif}.L: PtrT;
begin
  Result := PtrT({$ifdef FPC} FItems {$else} inherited List {$endif});
end;

function TStructList{$ifndef FPC}<T>{$endif}.Add: PtrT;
begin
  Count := Count + 1;
  Result := Addr(List^[Count - 1]);
end;

function TStructList{$ifndef FPC}<T>{$endif}.Ptr(const I: TListSize): PtrT;
begin
  Result := Addr(List^[I]);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.Assign(const Source: TStructList{$ifndef FPC}<T>{$endif});
begin
  Clear;
  AddRange(Source);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.Assign(const A: array of T);
begin
  Clear;
  AddRange(A);
end;

function TStructList{$ifndef FPC}<T>{$endif}.ItemSize: TListSize;
begin
  Result := SizeOf(T);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.AddArray(const A: array of T);
begin
  AddRange(A);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.AddList(const Source: TStructList{$ifndef FPC}<T>{$endif});
begin
  AddRange(Source);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.AddSubRange(
  const Source: TStructList{$ifndef FPC}<T>{$endif};
  const Index, AddCount: TListSize);
var
  OldCount: TListSize;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.List^[Index], List^[OldCount], SizeOf(T) * AddCount);
end;

function TStructList{$ifndef FPC}<T>{$endif}.PerfectlyEquals(
  const SecondValue: TStructList{$ifndef FPC}<T>{$endif}): boolean;
begin
  Result :=
    (Count = SecondValue.Count) and
    CompareMem(List, SecondValue.List, SizeOf(T) * Count);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.AssignLerpRange(const Fraction: Single;
  const V1, V2: TStructList{$ifndef FPC}<T>{$endif};
  const Index1, Index2, ACount: TListSize);
begin
  raise Exception.Create('AssignLerpRange not implemented for ' + ClassName);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.AssignLerp(const Fraction: Single;
  const V1, V2: TStructList {$ifndef FPC}<T>{$endif});
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);
  AssignLerpRange(Fraction, V1, V2, 0, 0, V1.Count);
end;

procedure TStructList{$ifndef FPC}<T>{$endif}.AssignArray(const A: array of T);
begin
  Assign(A);
end;

{$ifndef FPC}
function TStructList<T>.InternalGetItem(
  const Index: TListSize): T;
begin
  Result := List^[Index];
end;

procedure TStructList<T>.InternalSetItem(
  const Index: TListSize; const Value: T);
begin
  Items[Index] := Value;
end;
{$endif}

{$endif read_implementation}
