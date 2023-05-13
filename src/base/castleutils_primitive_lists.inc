{%MainUnit castleutils.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Lists of primitive types.
  Based on TStructList, which is based on TList from Generics.Collections. }

{$ifdef read_interface}

type
  TSingleList = class;
  TDoubleList = class;
  TInt32List = class;
  TUInt32List = class;

  { Raised by various AssignLerp implementations on lists,
    like @link(TStructList.AssignLerp),
    when the arguments have different length. }
  EListsDifferentCount = class(Exception);

  { List of Word (16-bit unsigned) values. }
  TWordList = class({$ifdef FPC}specialize{$endif} TStructList<Word>)
  public
    function Max: Word;
    procedure Assign(const Source: TInt32List); overload;
    procedure Assign(const Source: TUInt32List); overload;
  end;

  TUInt16List = TWordList;

  { List of 32-bit unsigned values. }
  TUInt32List = class({$ifdef FPC}specialize{$endif} TStructList<UInt32>)
  public
    function Max: UInt32;
    procedure Assign(const Source: TInt32List);
  end;

  { Old name for TUInt32List.

    Since Embarcadero made LongWord type so weirdly defined that it's usage should be avoided
    ( https://castle-engine.io/coding_conventions#_do_not_use_longint_longword_use_int32_uint32_int64_uint64_integer_cardinal ),
    you should rather use TUInt32List name. }
  TLongWordList = TUInt32List deprecated 'use TUInt32List';

  { List of Float values (defined in Math unit as float with maximum precision).
    This is deprecated as precision of floats is not defined cross-platform,
    see https://castle-engine.io/coding_conventions#_do_not_use_extended_use_single_double . }
  TFloatList = class({$ifdef FPC}specialize{$endif} TStructList<Float>)
    procedure AppendFloats(Floats: TSingleList);
  end {$ifdef FPC}deprecated 'use TSingleList for most cases, eventually TDoubleList'{$endif};

  TSingleArray = packed array [0..MaxInt div SizeOf(Single) - 1] of Single;
  PSingleArray = ^TSingleArray;

  { List of Single (single-precision floating point) values. }
  TSingleList = class({$ifdef FPC}specialize{$endif} TStructList<Single>)
    procedure AppendFloats(Floats: TFloatList);
    function ToDouble: TDoubleList;

    { Assign value from TDoubleList, converting to single-precision. }
    procedure Assign(const Source: TDoubleList); overload;

    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<Single>;
      const Index1, Index2, ACount: TListSize); override;

    procedure AddListRange(const Source: TSingleList; const Index, AddCount: TListSize); deprecated 'use AddSubRange';

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    function Sum: Single;

    { Multiply all items by Factor. }
    procedure MultiplyAll(const Factor: Single);

    { Sort and remove (exact) duplicates.
      Removal of duplicates is very fast, utilizing the fact that the list is sorted. }
    procedure SortAndRemoveDuplicates;
  end;

  { List of Double (double-precision floating point) values. }
  TDoubleList = class({$ifdef FPC}specialize{$endif} TStructList<Double>)
    function ToSingle: TSingleList;

    { Assign value from TSingleList, converting to single-precision. }
    procedure Assign(const Source: TSingleList); overload;

    procedure AssignLerpRange(const Fraction: Single;
      const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<Double>;
      const Index1, Index2, ACount: TListSize); override;

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  { List of Cardinal (unsigned) values.

    In practice, Cardinal is always 32-bit signed now,
    but use this when you don't want to explicitly care about size,
    see https://castle-engine.io/coding_conventions#_do_not_use_longint_longword_use_int32_uint32_int64_uint64_integer_cardinal . }
  TCardinalList = class({$ifdef FPC}specialize{$endif} TStructList<Cardinal>)
    function BigSum: Int64;
    function Sum: Cardinal;
  end;

  { List of booleans. }
  TBooleanList = class({$ifdef FPC}specialize{$endif} TStructList<boolean>)
  public
    { Set appropriate item to given value.
      These are useful methods to pass as callback in some situations
      (e.g. TCastleScene.RenderFrustumOctree passes
      RenderFrustumOctree_Visible.SetTrue method as callback
      that should mark visible items.)
      @groupBegin }
    procedure SetFalse(const Index: TListSize);
    procedure SetTrue(const Index: TListSize);
    { @groupEnd }
    function ToLongInt: TInt32List; deprecated 'use ToInt32';
    function ToInt32: TInt32List;

    { Does the SecondValue have equal length and content.
      This method does the same thing as @link(PerfectlyEquals)
      in case of Booleans list, that are always compared perfectly. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  TInt32Array = packed array [0..MaxInt div SizeOf(Int32) - 1] of Int32;
  PInt32Array = ^TInt32Array;

  { List of 32-bit signed integer values. }
  TInt32List = class({$ifdef FPC}specialize{$endif} TStructList<Int32>)
  public
    function Max: Int32;
    function Sum: Int32;
    procedure DeleteRange(const Index: TListSize; DelCount: TListSize = 1);
    { Add given Item a number of times to the list. }
    procedure AddDuplicate(const Item: Int32; const DuplicateCount: TListSize);

    { Does the SecondValue have equal length and content.
      This method does the same thing as @link(PerfectlyEquals)
      in case of Int32 list, that are always compared perfectly. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  TLongIntArray = TInt32Array;
  PLongIntArray = PInt32Array;

  { Old name for TUInt32List.

    Since Embarcadero made LongWord type so weirdly defined that it's usage should be avoided
    ( https://castle-engine.io/coding_conventions#_do_not_use_longint_longword_use_int32_uint32_int64_uint64_integer_cardinal ),
    you should rather use TUInt32List name. }
  TLongIntList = TInt32List deprecated 'TInt32List';

  { List of Integer (signed) values.

    In practice, Integer is always 32-bit signed now,
    but use this when you don't want to explicitly care about size,
    see https://castle-engine.io/coding_conventions#_do_not_use_longint_longword_use_int32_uint32_int64_uint64_integer_cardinal . }
  TIntegerList = class({$ifdef FPC}specialize{$endif} TStructList<Integer>)
  public
    function Sum: Integer;
  end;
{$endif}

{$ifdef read_implementation}

{ TWordList ------------------------------------------------------------ }

function TWordList.Max: Word;
var
  I: TListSize;
begin
  Result := Low(Word);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

procedure TWordList.Assign(const Source: TInt32List);
var
  I: TListSize;
  Src: PInt32;
  Dest: PWord;
begin
  Count := Source.Count;
  Src := PInt32(Source.L);
  Dest := PWord(L);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure TWordList.Assign(const Source: TUInt32List);
var
  I: TListSize;
  Src: PUInt32;
  Dest: PWord;
begin
  Count := Source.Count;
  Src := PUInt32(Source.L);
  Dest := PWord(L);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TUInt32List ------------------------------------------------------------ }

function TUInt32List.Max: UInt32;
var
  I: TListSize;
begin
  Result := Low(UInt32);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

procedure TUInt32List.Assign(const Source: TInt32List);
begin
  { Copy memory directly.
    This implicitly converts Int32 to UInt32 values, without range checking.
    But, in exchange, it's fast. }
  Count := Source.Count;
  if Count <> 0 then
    System.Move(Source.L[0], L[0], SizeOf(UInt32) * Count);
end;

{ TFloatList ------------------------------------------------------------ }

procedure TFloatList.AppendFloats(Floats: TSingleList);
var
  OldCount, I: TListSize;
begin
  OldCount := Count;
  Count := Count + Floats.Count;
  for i := 0 to Floats.Count - 1 do
    Items[OldCount + i] := Floats.Items[i];
end;

{ TSingleList ------------------------------------------------------------ }

procedure TSingleList.AppendFloats(Floats: TFloatList);
var
  OldCount, I: TListSize;
begin
  OldCount := Count;
  Count := Count + Floats.Count;
  for i := 0 to Floats.Count - 1 do
    Items[OldCount+i] := Floats.Items[i];
end;

function TSingleList.ToDouble: TDoubleList;
begin
  Result := TDoubleList.Create;
  Result.Assign(Self);
end;

procedure TSingleList.Assign(const Source: TDoubleList);
var
  I: TListSize;
  Src: PDouble;
  Dest: PSingle;
begin
  Count := Source.Count;
  Src := PDouble(Source.L);
  Dest := PSingle(L);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure TSingleList.AssignLerpRange(const Fraction: Single;
  const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<Single>;
  const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
  PtrTarget, Ptr1, Ptr2: PtrT;
begin
  Count := ACount;
  PtrTarget := L;
  Ptr1 := V1.Ptr(Index1);
  Ptr2 := V2.Ptr(Index2);
  for I := 0 to Count - 1 do
  begin
    PtrTarget^ := Lerp(Fraction, Ptr1^, Ptr2^);
    Inc(PtrTarget);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
end;

procedure TSingleList.AddListRange(const Source: TSingleList;
  const Index, AddCount: TListSize);
begin
  AddSubRange(Source, Index, AddCount);
end;

function TSingleList.Equals(SecondValue: TObject): boolean;
var
  I: TListSize;
begin
  Result := SecondValue is TSingleList;
  if Result then
  begin
    Result := Count = TSingleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not SameValue(TSingleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TSingleList.Sum: Single;
var
  I: TListSize;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

procedure TSingleList.MultiplyAll(const Factor: Single);
var
  I: TListSize;
begin
  for I := 0 to Count - 1 do
    Items[I] := Items[I] * Factor;
end;

procedure TSingleList.SortAndRemoveDuplicates;
var
  NewPosition, SourcePosition: TListSize;
begin
  Sort;

  { Both numbers NewPosition and SourcePosition move from 0 to Count.
    While SourcePosition always increases (and will definitely reach Count),
    NewPosition may grow slower, it indicates where should the Items[SourcePosition]
    be placed in the output list.
    Always NewPosition <= SourcePosition. }
  NewPosition := 0;
  SourcePosition := 0;
  while SourcePosition + 1 < Count do
  begin
    if Items[SourcePosition] <> Items[SourcePosition + 1] then
    begin
      Inc(NewPosition);
      Items[NewPosition] := Items[SourcePosition + 1];
    end;
    Inc(SourcePosition);
  end;
  Count := NewPosition + 1;
end;

{ TDoubleList ------------------------------------------------------------ }

function TDoubleList.ToSingle: TSingleList;
begin
  Result := TSingleList.Create;
  Result.Assign(Self);
end;

procedure TDoubleList.Assign(const Source: TSingleList);
var
  I: TListSize;
  Src: PSingle;
  Dest: PDouble;
begin
  Count := Source.Count;
  Src := PSingle(Source.L);
  Dest := PDouble(L);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure TDoubleList.AssignLerpRange(const Fraction: Single;
  const V1, V2: {$ifdef FPC}specialize{$endif} TStructList<Double>;
  const Index1, Index2, ACount: TListSize);
var
  I: TListSize;
  PtrTarget, Ptr1, Ptr2: PtrT;
begin
  Count := ACount;
  PtrTarget := L;
  Ptr1 := V1.Ptr(Index1);
  Ptr2 := V2.Ptr(Index2);
  for I := 0 to Count - 1 do
  begin
    PtrTarget^ := Lerp(Fraction, Ptr1^, Ptr2^);
    Inc(PtrTarget);
    Inc(Ptr1);
    Inc(Ptr2);
  end;
end;

function TDoubleList.Equals(SecondValue: TObject): boolean;
var
  I: TListSize;
begin
  Result := SecondValue is TDoubleList;
  if Result then
  begin
    Result := Count = TDoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if not SameValue(TDoubleList(SecondValue).L[I], L[I]) then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TCardinalList ------------------------------------------------------------ }

function TCardinalList.BigSum: Int64;
var
  i: TListSize;
begin
  result := 0;
  for i := 0 to Count-1 do
    result := result + Items[i];
end;

function TCardinalList.Sum: Cardinal;
var
  I: TListSize;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

{ TBooleanList ------------------------------------------------------------ }

procedure TBooleanList.SetFalse(const Index: TListSize);
begin
 Items[Index] := false;
end;

procedure TBooleanList.SetTrue(const Index: TListSize);
begin
 Items[Index] := true;
end;

function TBooleanList.ToLongInt: TInt32List;
begin
  Result := ToInt32;
end;

function TBooleanList.ToInt32: TInt32List;
var
  I: TListSize;
begin
  Result := TInt32List.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.Items[I] := Ord(Items[I]);
end;

function TBooleanList.Equals(SecondValue: TObject): boolean;
begin
  Result := (SecondValue is TBooleanList) and PerfectlyEquals(TBooleanList(SecondValue));
end;

{ TInt32List ----------------------------------------------------------- }

function TInt32List.Max: Int32;
var
  I: TListSize;
begin
  Result := Low(Int32);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

function TInt32List.Sum: Int32;
var
  I: TListSize;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

procedure TInt32List.DeleteRange(const Index: TListSize; DelCount: TListSize);
var
  I: TListSize;
begin
  { Make sure Index and DelCount are sensible first }
  if Index >= Count then
    Exit;
  MinVar(DelCount, Count - Index);

  for I := Index to Count - 1 - DelCount do
    Items[I] := Items[I + DelCount];

  Count := Count - DelCount;
end;

procedure TInt32List.AddDuplicate(const Item: Int32; const DuplicateCount: TListSize);
var
  I, C: TListSize;
begin
  C := Count;
  Count := Count + DuplicateCount;
  for I := C to Count - 1 do
    Items[I] := Item;
end;

function TInt32List.Equals(SecondValue: TObject): boolean;
begin
  Result := (SecondValue is TInt32List) and PerfectlyEquals(TInt32List(SecondValue));
end;

{ TIntegerList ----------------------------------------------------------- }

function TIntegerList.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

{$endif}
