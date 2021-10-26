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

{ Lists of primitive types. Using FPC generic TFPGList. }

{$ifdef read_interface}

type
  TSingleList = class;
  TDoubleList = class;
  TLongIntList = class;

  EListsDifferentCount = class(Exception);

  TWordList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<Word>)
  public
    function Max: Word;
    procedure Assign(const Source: TLongIntList);
  end;

  TLongWordList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<LongWord>)
  public
    function Max: LongWord;
    procedure Assign(const Source: TLongIntList);
  end;

  TFloatList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<Float>)
    procedure AppendFloats(Floats: TSingleList);
  end;

  TSingleArray = packed array [0..MaxInt div SizeOf(Single) - 1] of Single;
  PSingleArray = ^TSingleArray;

  TSingleList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<Single>)
    procedure AppendFloats(Floats: TFloatList);
    function ToDouble: TDoubleList;

    { Assign value from TDoubleList, converting to double-precision. }
    procedure Assign(const Source: TDoubleList); overload;

    { Assign linear interpolation between two other float arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TSingleList; Index1, Index2, ACount: Integer); overload;

    {$ifndef FPC}
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TStructList<Single>; Index1, Index2, ACount: Integer); overload; override;
    {$endif}

    procedure AddSubRange(Source: TSingleList; Index, AddCount: Integer);
    procedure AddListRange(Source: TSingleList; Index, AddCount: Integer); deprecated 'use AddSubRange';

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Single; const V1, V2: TSingleList); overload;

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;

    function Sum: Single;

    { Multiply all items by Factor. }
    procedure MultiplyAll(const Factor: Single);

    { Sort and remove (exact) duplicates.
      Removal of duplicates is very fast, utilizing the fact that the list is sorted. }
    procedure SortAndRemoveDuplicates;
  end;

  TDoubleList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<Double>)
    function ToSingle: TSingleList;

    { Assign value from TSingleList, converting to single-precision. }
    procedure Assign(const Source: TSingleList); overload;

    { Assign linear interpolation between two other vector arrays.
      @raises EListsDifferentCount If V1 and V2 have different count. }
    procedure AssignLerp(const Fraction: Double; const V1, V2: TDoubleList);

    { Does the SecondValue have equal length and content.
      The values are compared with an Epsilon tolerance, as usual for floating-point
      values. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.
      The values are compared perfectly, without any tolerance for difference. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  TCardinalList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<Cardinal>)
    function BigSum: Int64;
    function Sum: Cardinal;
  end;

  TBooleanList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<boolean> {$ifndef FPC}, ICastleItemList<boolean, TStructList<boolean>> {$endif})
  public
    { Set appropriate item to given value.
      These are useful methods to pass as callback in some situations
      (e.g. TCastleScene.RenderFrustumOctree passes
      RenderFrustumOctree_Visible.SetTrue method as callback
      that should mark visible items.)
      @groupBegin }
    procedure SetFalse(Index: Integer);
    procedure SetTrue(Index: Integer);
    { @groupEnd }
    function ToLongInt: TLongIntList;

    { Does the SecondValue have equal length and content. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.

      This method does the same thing as @link(Equals).
      It is defined for consistency -- on some lists, like @link(TSingleList),
      there is an important difference between Equals (compares with some
      epsilon tolerance) and PerfectlyEquals. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  TLongIntArray = packed array [0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  PLongIntArray = ^TLongIntArray;

  TLongIntList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<LongInt>)
  public
    function Max: LongInt;
    function Sum: LongInt;
    procedure DeleteRange(const Index: Integer; DelCount: Integer = 1);
    { Add given Item a number of times to the list. }
    procedure AddDuplicate(const Item: LongInt; const DuplicateCount: SizeInt);

    { Does the SecondValue have equal length and content. }
    function Equals(SecondValue: TObject): boolean; override;

    { Does the SecondValue have equal length and content.

      This method does the same thing as @link(Equals).
      It is defined for consistency -- on some lists, like @link(TSingleList),
      there is an important difference between Equals (compares with some
      epsilon tolerance) and PerfectlyEquals. }
    function PerfectlyEquals(SecondValue: TObject): boolean;
  end;

  TIntegerList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<Integer>)
  public
    function Sum: Integer;
  end;
{$endif}

{$ifdef read_implementation}

{ TWordList ------------------------------------------------------------ }

function TWordList.Max: Word;
var
  I: Integer;
begin
  Result := Low(Word);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

procedure TWordList.Assign(const Source: TLongIntList);
var
  I: Integer;
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
  I: Integer;
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
var OldCount, i: Integer;
begin
 OldCount := Count;
 Count := Count + Floats.Count;
 for i := 0 to Floats.Count - 1 do Items[OldCount+i] := Floats.Items[i];
end;

{ TSingleList ------------------------------------------------------------ }

procedure TSingleList.AppendFloats(Floats: TFloatList);
var OldCount, i: Integer;
begin
 OldCount := Count;
 Count := Count + Floats.Count;
 for i := 0 to Floats.Count - 1 do Items[OldCount+i] := Floats.Items[i];
end;

function TSingleList.ToDouble: TDoubleList;
begin
  Result := TDoubleList.Create;
  Result.Assign(Self);
end;

procedure TSingleList.Assign(const Source: TDoubleList);
var
  I: Integer;
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

procedure TSingleList.AssignLerp(const Fraction: Single;
  V1, V2: TSingleList; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    Items[I] := Lerp(Fraction, V1.Items[Index1 + I], V2.Items[Index2 + I]);
end;

{$ifndef FPC}
procedure TSingleList.AssignLerp(const Fraction: Single;
      V1, V2: TStructList<Single>; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    Items[I] := Lerp(Fraction, V1.Items[Index1 + I], V2.Items[Index2 + I]);
end;
{$endif}

procedure TSingleList.AddListRange(Source: TSingleList; Index, AddCount: Integer);
begin
  AddSubRange(Source, Index, AddCount);
end;

procedure TSingleList.AddSubRange(Source: TSingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.List^[Index], List^[OldCount], SizeOf(Single) * AddCount);
end;

procedure TSingleList.AssignLerp(const Fraction: Single; const V1, V2: TSingleList);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[I], V2.List^[I]);
end;

function TSingleList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
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

function TSingleList.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TSingleList;
  if Result then
  begin
    Result := Count = TSingleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if TSingleList(SecondValue).List^[I] <> List^[I] then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TSingleList.Sum: Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

procedure TSingleList.MultiplyAll(const Factor: Single);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I] := Items[I] * Factor;
end;

procedure TSingleList.SortAndRemoveDuplicates;
var
  NewPosition, SourcePosition: Integer;
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
  I: Integer;
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

procedure TDoubleList.AssignLerp(const Fraction: Double; const V1, V2: TDoubleList);
var
  I: Integer;
begin
  if V1.Count <> V2.Count then
    raise EListsDifferentCount.CreateFmt('Different lists length: %d and %d, cannot do AssignLerp on the lists',
      [V1.Count, V2.Count]);

  Count := V1.Count;
  for I := 0 to Count - 1 do
    List^[I] := Lerp(Fraction, V1.List^[I], V2.List^[I]);
end;

function TDoubleList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
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

function TDoubleList.PerfectlyEquals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TDoubleList;
  if Result then
  begin
    Result := Count = TDoubleList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if TDoubleList(SecondValue).List^[I] <> List^[I] then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

{ TCardinalList ------------------------------------------------------------ }

function TCardinalList.BigSum: Int64;
var i: integer;
begin
 result := 0;
 for i := 0 to Count-1 do result := result + Items[i];
end;

function TCardinalList.Sum: Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

{ TBooleanList ------------------------------------------------------------ }

procedure TBooleanList.SetFalse(Index: Integer);
begin
 Items[Index] := false;
end;

procedure TBooleanList.SetTrue(Index: Integer);
begin
 Items[Index] := true;
end;

function TBooleanList.ToLongInt: TLongIntList;
var
  I: Integer;
begin
  Result := TLongIntList.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.Items[I] := Ord(Items[I]);
end;

function TBooleanList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TBooleanList;
  if Result then
  begin
    Result := Count = TBooleanList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if TBooleanList(SecondValue).List^[I] <> List^[I] then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TBooleanList.PerfectlyEquals(SecondValue: TObject): boolean;
begin
  Result := Equals(SecondValue);
end;

{ TLongIntList ----------------------------------------------------------- }

function TLongIntList.Max: LongInt;
var
  I: Integer;
begin
  Result := Low(LongInt);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

function TLongIntList.Sum: LongInt;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I];
end;

procedure TLongIntList.DeleteRange(const Index: Integer; DelCount: Integer);
var
  I: Integer;
begin
  { Make sure Index and DelCount are sensible first }
  if Index >= Count then
    Exit;
  MinVar(DelCount, Count - Index);

  for I := Index to Count - 1 - DelCount do
    Items[I] := Items[I + DelCount];

  Count := Count - DelCount;
end;

procedure TLongIntList.AddDuplicate(const Item: LongInt; const DuplicateCount: SizeInt);
var
  I, C: Integer;
begin
  C := Count;
  Count := Count + DuplicateCount;
  for I := C to Count - 1 do
    Items[I] := Item;
end;

function TLongIntList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result := SecondValue is TLongIntList;
  if Result then
  begin
    Result := Count = TLongIntList(SecondValue).Count;
    if Result then
      for I := 0 to Count - 1 do
        if TLongIntList(SecondValue).List^[I] <> List^[I] then
        begin
          Result := false;
          Exit;
        end;
  end;
end;

function TLongIntList.PerfectlyEquals(SecondValue: TObject): boolean;
begin
  Result := Equals(SecondValue);
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
