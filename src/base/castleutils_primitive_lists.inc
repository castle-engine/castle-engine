{
  Copyright 2002-2017 Michalis Kamburelis.

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

  TWordList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<Word>)
  public
    procedure AddList(Source: TWordList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of Word); deprecated 'use AddRange';
    function Max: Word;
    procedure Assign(const Source: TLongIntList);
  end;

  TLongWordList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<LongWord>)
  public
    procedure AddList(Source: TLongWordList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of LongWord); deprecated 'use AddRange';
    function Max: LongWord;
    procedure Assign(const Source: TLongIntList);
  end;

  TFloatList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<Float>)
    procedure AppendFloats(Floats: TSingleList);
    procedure AddList(Source: TFloatList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of Float); deprecated 'use AddRange';
  end;

  TArray_Single = packed array [0..MaxInt div SizeOf(Single) - 1] of Single;
  PArray_Single = ^TArray_Single;

  TSingleList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<Single>)
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
      V1, V2: TSingleList; Index1, Index2, ACount: Integer);
    procedure AddList(Source: TSingleList); deprecated 'use AddRange';
    procedure AddListRange(Source: TSingleList; Index, AddCount: Integer);
    procedure AddArray(constref A: array of Single); deprecated 'use AddRange';
  end;

  TDoubleList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<Double>)
    function ToSingle: TSingleList;

    { Assign value from TSingleList, converting to single-precision. }
    procedure Assign(const Source: TSingleList); overload;

    procedure AddList(Source: TDoubleList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of Double); deprecated 'use AddRange';
  end;

  TCardinalList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<Cardinal>)
    function BigSum: Int64;
    function Sum: Cardinal;
    procedure AddList(Source: TCardinalList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of Cardinal); deprecated 'use AddRange';
  end;

  TBooleanList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<boolean>)
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
    procedure AddList(Source: TBooleanList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of boolean); deprecated 'use AddRange';
  end;

  TArray_LongInt = packed array [0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  PArray_LongInt = ^TArray_LongInt;

  TLongIntList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<LongInt>)
  public
    function Max: LongInt;
    function Sum: LongInt;
    procedure DeleteRange(const Index: Integer; DelCount: Integer = 1);
    procedure AddList(Source: TLongIntList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of LongInt); deprecated 'use AddRange';
    { Add given Item a number of times to the list. }
    procedure AddDuplicate(const Item: LongInt; const DuplicateCount: Cardinal);
  end;

  TIntegerList = class({$ifdef FPC_OBJFPC}specialize{$endif} TStructList<Integer>)
  public
    function Sum: Integer;
    procedure AddList(Source: TIntegerList); deprecated 'use AddRange';
    procedure AddArray(constref A: array of Integer); deprecated 'use AddRange';
  end;
{$endif}

{$ifdef read_implementation}

{ TWordList ------------------------------------------------------------ }

procedure TWordList.AddList(Source: TWordList);
begin
  AddRange(Source);
end;

procedure TWordList.AddArray(constref A: array of Word);
begin
  AddRange(A);
end;

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
  Src := Source.L;
  Dest := L;
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TLongWordList ------------------------------------------------------------ }

procedure TLongWordList.AddList(Source: TLongWordList);
begin
  AddRange(Source);
end;

procedure TLongWordList.AddArray(constref A: array of LongWord);
begin
  AddRange(A);
end;

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

procedure TFloatList.AddList(Source: TFloatList);
begin
  AddRange(Source);
end;

procedure TFloatList.AddArray(constref A: array of Float);
begin
  AddRange(A);
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
  Src := Source.L;
  Dest := L;
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

procedure TSingleList.AddList(Source: TSingleList);
begin
  AddRange(Source);
end;

procedure TSingleList.AddListRange(Source: TSingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.List^[Index], List^[OldCount], SizeOf(Single) * AddCount);
end;

procedure TSingleList.AddArray(constref A: array of Single);
begin
  AddRange(A);
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
  Src := Source.L;
  Dest := L;
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure TDoubleList.AddList(Source: TDoubleList);
begin
  AddRange(Source);
end;

procedure TDoubleList.AddArray(constref A: array of Double);
begin
  AddRange(A);
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
    Result += Items[I];
end;

procedure TCardinalList.AddList(Source: TCardinalList);
begin
  AddRange(Source);
end;

procedure TCardinalList.AddArray(constref A: array of Cardinal);
begin
  AddRange(A);
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

procedure TBooleanList.AddList(Source: TBooleanList);
begin
  AddRange(Source);
end;

procedure TBooleanList.AddArray(constref A: array of boolean);
begin
  AddRange(A);
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
    Result += Items[I];
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

procedure TLongIntList.AddList(Source: TLongIntList);
begin
  AddRange(Source);
end;

procedure TLongIntList.AddArray(constref A: array of LongInt);
begin
  AddRange(A);
end;

procedure TLongIntList.AddDuplicate(const Item: LongInt; const DuplicateCount: Cardinal);
var
  I, C: Integer;
begin
  C := Count;
  Count := Count + DuplicateCount;
  for I := C to Count - 1 do
    Items[I] := Item;
end;

{ TIntegerList ----------------------------------------------------------- }

function TIntegerList.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result += Items[I];
end;

procedure TIntegerList.AddList(Source: TIntegerList);
begin
  AddRange(Source);
end;

procedure TIntegerList.AddArray(constref A: array of Integer);
begin
  AddRange(A);
end;

{$endif}
