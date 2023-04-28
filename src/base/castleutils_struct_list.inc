{%MainUnit castleutils.pas}
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

    { Access the list contents through a pointer to an array of items.

      This is exactly the same pointer as @link(L), but the type is different:
      List points to an array of items, while L points to a single item.

      Using @link(L) is preferred over this @link(List), because @link(L)
      avoids "fake infinite list" type TTypeList, that in particular on Delphi
      may have too small size (and cause range check errors).

      See the @link(L) description for a more detailed explanation and example.

      @seealso L }
    function List: PTypeList; deprecated 'use L, like MyList.L[123] instead of MyList.List^[123]';

    { Access the list contents through a pointer, to get or set them efficiently.

      Use this to directly access a list value, like this:

      @longCode(#
        MyIntList.L[I] := 123;
        // Equivalent but less efficient to syntax without L:
        // MyIntList[I] := 123;

        MyIntList.L[I] := MyIntList.L[I] + 123;
        // Equivalent but less efficient to syntax without L:
        // MyIntList[I] := MyIntList[I] + 123;

        MyRecordList.L[I].MyField := 123;
        // Never use syntax without L (see below for reasons):
        // MyRecordList[I].MyField := 123; //< never do this!
      #)

      The above examples of L work only in FPC ObjFpc mode or in Delphi with pointermath "on"
      (see https://docwiki.embarcadero.com/RADStudio/Sydney/en/Pointer_Math_(Delphi) ).
      These are CGE default settings when being compiled with FPC or Delphi,
      so you can depend on them at least in CGE implementation.
      Note that Delphi applications just using L don't actually have pointermath "on",
      it is enough that this unit is compiled with pointermath "on" and then one can use
      math on PtrT.

      This is in particular useful if you have a list of records and you would like
      to set their fields. E.g. this is incorrect (and should not even compile if
      MyField is really a simple field):

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

      It will not work OK because you would modify only a temporary record
      returned by the @code(MyList[I]) getter.

      In contrast, this will work OK:

      @longCode(#
        MyList.L[I].MyField := 123;
      #)

      Using @link(L) is preferred over this @link(List), because it avoids "fake infinite list"
      type TTypeList, that in particular on Delphi may have too small size.

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
  Result := Ptr(Count - 1);
end;

function TStructList{$ifndef FPC}<T>{$endif}.Ptr(const I: TListSize): PtrT;
begin
  { List^ upper bound in Delphi may be too small, so ignore range check errors.
    We cannot use Addr(L[I]) or @L[I] as then FPC compilation fails. }
  Result := PtrT(PtrUInt(L) + PtrUInt(I) * PtrUInt(SizeOf(T)));
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
    System.Move(Source.Ptr(Index)^, Ptr(OldCount)^, SizeOf(T) * AddCount);
end;

function TStructList{$ifndef FPC}<T>{$endif}.PerfectlyEquals(
  const SecondValue: TStructList{$ifndef FPC}<T>{$endif}): boolean;
begin
  Result :=
    (Count = SecondValue.Count) and
    CompareMem(L, SecondValue.L, SizeOf(T) * Count);
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
  { List^ upper bound in Delphi may be too small, so ignore range check errors.
    Unfortunately we cannot use L here, as then Delphi cannot compile cubic Bezier nodes... }
  {$warnings off} // using deprecated
  {$I norqcheckbegin.inc}
  Result := List^[Index];
  {$I norqcheckend.inc}
  {$warnings on}
end;

procedure TStructList<T>.InternalSetItem(
  const Index: TListSize; const Value: T);
begin
  Items[Index] := Value;
end;
{$endif}

{$endif read_implementation}
