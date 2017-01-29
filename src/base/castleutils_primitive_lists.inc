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

  TWordList = class(specialize TFPGList<Word>)
  public
    procedure AddList(Source: TWordList);
    procedure AddArray(const A: array of Word);
    function L: PWord;
    function Max: Word;
    procedure Assign(Source: TLongIntList);
  end;

  TLongWordList = class(specialize TFPGList<LongWord>)
  public
    procedure AddList(Source: TLongWordList);
    procedure AddArray(const A: array of LongWord);
    function L: PLongWord;
    function Max: LongWord;
    procedure Assign(Source: TLongIntList);
  end;

  TFloatList = class(specialize TFPGList<Float>)
    procedure AppendFloats(Floats: TSingleList);
    procedure AddList(Source: TFloatList);
    procedure AddArray(const A: array of Float);
    function L: PFloat;
  end;

  TArray_Single = packed array [0..MaxInt div SizeOf(Single) - 1] of Single;
  PArray_Single = ^TArray_Single;

  TSingleList = class(specialize TFPGList<Single>)
    procedure AppendFloats(Floats: TFloatList);
    function ToDouble: TDoubleList;
    { Assign value from TDoubleList, converting to double-precision. }
    procedure Assign(Source: TDoubleList); overload;

    { Assign linear interpolation between two other float arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TSingleList; Index1, Index2, ACount: Integer);
    procedure AddList(Source: TSingleList);
    procedure AddListRange(Source: TSingleList; Index, AddCount: Integer);
    procedure AddArray(const A: array of Single);
    function L: PSingle;
  end;

  TDoubleList = class(specialize TFPGList<Double>)
    function ToSingle: TSingleList;
    { Assign value from TSingleList, converting to double-precision. }
    procedure Assign(Source: TSingleList); overload;
    procedure AddList(Source: TDoubleList);
    procedure AddArray(const A: array of Double);
    function L: PDouble;
  end;

  TCardinalList = class(specialize TFPGList<Cardinal>)
    function BigSum: Int64;
    function Sum: Cardinal;
    procedure AddList(Source: TCardinalList);
    procedure AddArray(const A: array of Cardinal);
    function L: PCardinal;
  end;

  TBooleanList = class(specialize TFPGList<boolean>)
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
    procedure AddList(Source: TBooleanList);
    procedure AddArray(const A: array of boolean);
    function L: PBoolean;
  end;

  TArray_LongInt = packed array [0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  PArray_LongInt = ^TArray_LongInt;

  TLongIntList = class(specialize TFPGList<LongInt>)
  public
    function Max: LongInt;
    function Sum: LongInt;
    procedure DeleteRange(const Index: Integer; DelCount: Integer = 1);
    procedure AddList(Source: TLongIntList);
    procedure AddArray(const A: array of LongInt);
    { Add given Item a number of times to the list. }
    procedure AddDuplicate(const Item: LongInt; const DuplicateCount: Cardinal);
    function L: PLongInt;
  end;

  TIntegerList = class(specialize TFPGList<Integer>)
  public
    function Sum: Integer;
    procedure AddList(Source: TIntegerList);
    procedure AddArray(const A: array of Integer);
    function L: PInteger;
  end;
{$endif}

{$ifdef read_implementation}

{ TWordList ------------------------------------------------------------ }

procedure TWordList.AddList(Source: TWordList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(Word) * Source.Count);
end;

procedure TWordList.AddArray(const A: array of Word);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(Word) * (High(A) + 1));
end;

function TWordList.L: PWord;
begin
  Result := PWord(FList);
end;

function TWordList.Max: Word;
var
  I: Integer;
begin
  Result := Low(Word);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

procedure TWordList.Assign(Source: TLongIntList);
var
  I: Integer;
  Src: PLongInt;
  Dest: PWord;
begin
  Count := Source.Count;
  Src := PLongInt(Source.List);
  Dest := PWord(List);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TLongWordList ------------------------------------------------------------ }

procedure TLongWordList.AddList(Source: TLongWordList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(LongWord) * Source.Count);
end;

procedure TLongWordList.AddArray(const A: array of LongWord);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(LongWord) * (High(A) + 1));
end;

function TLongWordList.L: PLongWord;
begin
  Result := PLongWord(FList);
end;

function TLongWordList.Max: LongWord;
var
  I: Integer;
begin
  Result := Low(LongWord);
  for I := 0 to Count - 1 do
    MaxVar(Result, Items[I]);
end;

procedure TLongWordList.Assign(Source: TLongIntList);
begin
  { Copy memory directly.
    This implicitly converts LongInt to LongWord values, without range checking.
    But, in exchange, it's fast. }
  Count := Source.Count;
  if Count <> 0 then
    System.Move(Source.L[0], L[0], SizeOf(LongWord) * Count);
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
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(Float) * Source.Count);
end;

procedure TFloatList.AddArray(const A: array of Float);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(Float) * (High(A) + 1));
end;

function TFloatList.L: PFloat;
begin
  Result := PFloat(FList);
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

procedure TSingleList.Assign(Source: TDoubleList);
var
  I: Integer;
  Src: PDouble;
  Dest: PSingle;
begin
  Count := Source.Count;
  Src := PDouble(Source.List);
  Dest := PSingle(List);
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
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(Single) * Source.Count);
end;

procedure TSingleList.AddListRange(Source: TSingleList; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if Source.Count <> 0 then
    System.Move(Source.L[Index], L[OldCount], SizeOf(Single) * AddCount);
end;

procedure TSingleList.AddArray(const A: array of Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(Single) * (High(A) + 1));
end;

function TSingleList.L: PSingle;
begin
  Result := PSingle(FList);
end;

{ TDoubleList ------------------------------------------------------------ }

function TDoubleList.ToSingle: TSingleList;
begin
  Result := TSingleList.Create;
  Result.Assign(Self);
end;

procedure TDoubleList.Assign(Source: TSingleList);
var
  I: Integer;
  Src: PSingle;
  Dest: PDouble;
begin
  Count := Source.Count;
  Src := PSingle(Source.List);
  Dest := PDouble(List);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

procedure TDoubleList.AddList(Source: TDoubleList);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(Double) * Source.Count);
end;

procedure TDoubleList.AddArray(const A: array of Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(Double) * (High(A) + 1));
end;

function TDoubleList.L: PDouble;
begin
  Result := PDouble(FList);
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
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(Cardinal) * Source.Count);
end;

procedure TCardinalList.AddArray(const A: array of Cardinal);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(Cardinal) * (High(A) + 1));
end;

function TCardinalList.L: PCardinal;
begin
  Result := PCardinal(FList);
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
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(boolean) * Source.Count);
end;

procedure TBooleanList.AddArray(const A: array of boolean);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(boolean) * (High(A) + 1));
end;

function TBooleanList.L: PBoolean;
begin
  Result := PBoolean(FList);
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
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(LongInt) * Source.Count);
end;

procedure TLongIntList.AddArray(const A: array of LongInt);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(LongInt) * (High(A) + 1));
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

function TLongIntList.L: PLongInt;
begin
  Result := PLongInt(FList);
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
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + Source.Count;
  if Source.Count <> 0 then
    System.Move(Source.L[0], L[OldCount], SizeOf(Integer) * Source.Count);
end;

procedure TIntegerList.AddArray(const A: array of Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], L[OldCount], SizeOf(Integer) * (High(A) + 1));
end;

function TIntegerList.L: PInteger;
begin
  Result := PInteger(FList);
end;

{$endif}
