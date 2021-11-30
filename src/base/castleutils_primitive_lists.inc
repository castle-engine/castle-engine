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

{ Lists of primitive types. Using FPC generic TFPGList. }

{$ifdef read_interface}

type
  TSingleList = class;
  TDoubleList = class;
  TLongIntList = class;

  EListsDifferentCount = class(Exception);

  TWordList = class({$ifdef FPC}specialize{$endif} TStructList<Word>)
  public
    function Max: Word;
    procedure Assign(const Source: TLongIntList);
  end;

  TLongWordList = class({$ifdef FPC}specialize{$endif} TStructList<LongWord>)
  public
    function Max: LongWord;
    procedure Assign(const Source: TLongIntList);
  end;

  TFloatList = class({$ifdef FPC}specialize{$endif} TStructList<Float>)
    procedure AppendFloats(Floats: TSingleList);
  end;

  TSingleArray = packed array [0..MaxInt div SizeOf(Single) - 1] of Single;
  PSingleArray = ^TSingleArray;

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

  TCardinalList = class({$ifdef FPC}specialize{$endif} TStructList<Cardinal>)
    function BigSum: Int64;
    function Sum: Cardinal;
  end;

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
    function ToLongInt: TLongIntList;

    { Does the SecondValue have equal length and content.
      This method does the same thing as @link(PerfectlyEquals)
      in case of Booleans list, that are always compared perfectly. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

  TLongIntArray = packed array [0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  PLongIntArray = ^TLongIntArray;

  TLongIntList = class({$ifdef FPC}specialize{$endif} TStructList<LongInt>)
  public
    function Max: LongInt;
    function Sum: LongInt;
    procedure DeleteRange(const Index: TListSize; DelCount: TListSize = 1);
    { Add given Item a number of times to the list. }
    procedure AddDuplicate(const Item: LongInt; const DuplicateCount: TListSize);

    { Does the SecondValue have equal length and content.
      This method does the same thing as @link(PerfectlyEquals)
      in case of LongInt list, that are always compared perfectly. }
    function Equals(SecondValue: TObject): boolean; override;
  end;

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

procedure TWordList.Assign(const Source: TLongIntList);
var
  I: TListSize;
  Src: PLongInt;
  Dest: PWord;
begin
  Count := Source.Count;
  Src := PLongInt(Source.L);
  Dest := PWord(L);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TLongWordList ------------------------------------------------------------ }

function TLongWordList.Max: LongWord;
var
  I: TListSize;
begin
  Result := Low(LongWord);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

procedure TLongWordList.Assign(const Source: TLongIntList);
begin
  { Copy memory directly.
    This implicitly converts LongInt to LongWord values, without range checking.
    But, in exchange, it's fast. }
  Count := Source.Count;
  if Count <> 0 then
    System.Move(Source.List^[0], List^[0], SizeOf(LongWord) * Count);
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
  PtrTarget, Ptr1, Ptr2: PSingle;
begin
  Count := ACount;
  PtrTarget := PSingle(List);
  Ptr1 := @V1.List^[Index1];
  Ptr2 := @V2.List^[Index2];
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
        if not SameValue(TSingleList(SecondValue).List^[I], List^[I]) then
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
  PtrTarget, Ptr1, Ptr2: PDouble;
begin
  Count := ACount;
  PtrTarget := PDouble(List);
  Ptr1 := @V1.List^[Index1];
  Ptr2 := @V2.List^[Index2];
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
        if not SameValue(TDoubleList(SecondValue).List^[I], List^[I]) then
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

function TBooleanList.ToLongInt: TLongIntList;
var
  I: TListSize;
begin
  Result := TLongIntList.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.Items[I] := Ord(Items[I]);
end;

function TBooleanList.Equals(SecondValue: TObject): boolean;
begin
  Result := (SecondValue is TBooleanList) and PerfectlyEquals(TBooleanList(SecondValue));
end;

{ TLongIntList ----------------------------------------------------------- }

function TLongIntList.Max: LongInt;
var
  I: TListSize;
begin
  Result := Low(LongInt);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

function TLongIntList.Sum: LongInt;
var
  I: TListSize;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

procedure TLongIntList.DeleteRange(const Index: TListSize; DelCount: TListSize);
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

procedure TLongIntList.AddDuplicate(const Item: LongInt; const DuplicateCount: TListSize);
var
  I, C: TListSize;
begin
  C := Count;
  Count := Count + DuplicateCount;
  for I := C to Count - 1 do
    Items[I] := Item;
end;

function TLongIntList.Equals(SecondValue: TObject): boolean;
begin
  Result := (SecondValue is TLongIntList) and PerfectlyEquals(TLongIntList(SecondValue));
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
