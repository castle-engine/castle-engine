{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Lists of primitive types. Using FPC generic TFPGList. }

{$ifdef read_interface}

type
  TDynSingleArray = class;
  TDynDoubleArray = class;
  TDynLongIntArray = class;

  TDynLongWordArray = class(specialize TFPGList<LongWord>)
  public
    procedure AddList(L: TDynLongWordArray);
    procedure AddArray(const A: array of LongWord);
  end;

  TDynFloatArray = class(specialize TFPGList<Float>)
    procedure AppendFloats(Floats: TDynSingleArray);
    procedure AddList(L: TDynFloatArray);
    procedure AddArray(const A: array of Float);
  end;

  TArray_Single = packed array [0..MaxInt div SizeOf(Single) - 1] of Single;
  PArray_Single = ^TArray_Single;

  TDynSingleArray = class(specialize TFPGList<Single>)
    procedure AppendFloats(Floats: TDynFloatArray);
    function ToDouble: TDynDoubleArray;
    { Assign value from TDynDoubleArray, converting to double-precision. }
    procedure Assign(Source: TDynDoubleArray); overload;

    { Assign linear interpolation between two other float arrays.
      We take ACount items, from V1[Index1 ... Index1 + ACount - 1] and
      V2[Index2 ... Index2 + ACount - 1], and interpolate between them
      like normal Lerp functions.

      It's Ok for both V1 and V2 to be the same objects.
      But their ranges should not overlap, for future optimizations
      (although it's Ok for current implementation). }
    procedure AssignLerp(const Fraction: Single;
      V1, V2: TDynSingleArray; Index1, Index2, ACount: Integer);
    procedure AddList(L: TDynSingleArray);
    procedure AddListRange(L: TDynSingleArray; Index, AddCount: Integer);
    procedure AddArray(const A: array of Single);
  end;

  TDynDoubleArray = class(specialize TFPGList<Double>)
    function ToSingle: TDynSingleArray;
    { Assign value from TDynSingleArray, converting to double-precision. }
    procedure Assign(Source: TDynSingleArray); overload;
    procedure AddList(L: TDynDoubleArray);
    procedure AddArray(const A: array of Double);
  end;

  TDynCardinalArray = class(specialize TFPGList<Cardinal>)
    function BigSum: Int64;
    function Sum: Cardinal;
    procedure AddList(L: TDynCardinalArray);
    procedure AddArray(const A: array of Cardinal);
  end;

  TDynBooleanArray = class(specialize TFPGList<boolean>)
  public
    { Set appropriate item to given value.
      These are useful methods to pass as callback in some situations
      (e.g. TVRMLGLScene.RenderFrustumOctree passes
      RenderFrustumOctree_Visible.SetTrue method as callback
      that should mark visible items.)
      @groupBegin }
    procedure SetFalse(Index: Integer);
    procedure SetTrue(Index: Integer);
    { @groupEnd }
    function ToLongInt: TDynLongIntArray;
    procedure AddList(L: TDynBooleanArray);
    procedure AddArray(const A: array of boolean);
  end;

  TArray_LongInt = packed array [0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  PArray_LongInt = ^TArray_LongInt;

  TDynLongIntArray = class(specialize TFPGList<LongInt>)
  public
    function Max: LongInt;
    function Sum: LongInt;
    procedure DeleteRange(const Index: Integer; DelCount: Integer = 1);
    procedure AddList(L: TDynLongIntArray);
    procedure AddArray(const A: array of LongInt);
    { Add given Item a number of times to the list. }
    procedure AddDuplicate(const Item: LongInt; const DuplicateCount: Cardinal);
  end;

  TDynIntegerArray = class(specialize TFPGList<Integer>)
  public
    function Sum: Integer;
    procedure AddList(L: TDynIntegerArray);
    procedure AddArray(const A: array of Integer);
  end;

  TDynStringArray = class(specialize TFPGList<string>)
  public
    procedure AddList(L: TDynStringArray);
    procedure AddArray(const A: array of string);
    procedure AssignArray(const A: array of string);
  end;

{$endif}

{$ifdef read_implementation}

{ TDynLongWordArray ------------------------------------------------------------ }

procedure TDynLongWordArray.AddList(L: TDynLongWordArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(LongWord) * L.Count);
end;

procedure TDynLongWordArray.AddArray(const A: array of LongWord);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(LongWord) * (High(A) + 1));
end;

{ TDynFloatArray ------------------------------------------------------------ }

procedure TDynFloatArray.AppendFloats(Floats: TDynSingleArray);
var OldCount, i: Integer;
begin
 OldCount := Count;
 Count := Count + Floats.Count;
 for i := 0 to Floats.Count - 1 do Items[OldCount+i] := Floats.Items[i];
end;

procedure TDynFloatArray.AddList(L: TDynFloatArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(Float) * L.Count);
end;

procedure TDynFloatArray.AddArray(const A: array of Float);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(Float) * (High(A) + 1));
end;

{ TDynSingleArray ------------------------------------------------------------ }

procedure TDynSingleArray.AppendFloats(Floats: TDynFloatArray);
var OldCount, i: Integer;
begin
 OldCount := Count;
 Count := Count + Floats.Count;
 for i := 0 to Floats.Count - 1 do Items[OldCount+i] := Floats.Items[i];
end;

function TDynSingleArray.ToDouble: TDynDoubleArray;
begin
  Result := TDynDoubleArray.Create;
  Result.Assign(Self);
end;

procedure TDynSingleArray.Assign(Source: TDynDoubleArray);
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

procedure TDynSingleArray.AssignLerp(const Fraction: Single;
  V1, V2: TDynSingleArray; Index1, Index2, ACount: Integer);
var
  I: Integer;
begin
  Count := ACount;
  for I := 0 to Count - 1 do
    Items[I] := Lerp(Fraction, V1.Items[Index1 + I], V2.Items[Index2 + I]);
end;

procedure TDynSingleArray.AddList(L: TDynSingleArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(Single) * L.Count);
end;

procedure TDynSingleArray.AddListRange(L: TDynSingleArray; Index, AddCount: Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + AddCount;
  if L.Count <> 0 then
    System.Move(L.List^[Index], List^[OldCount], SizeOf(Single) * AddCount);
end;

procedure TDynSingleArray.AddArray(const A: array of Single);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(Single) * (High(A) + 1));
end;

{ TDynDoubleArray ------------------------------------------------------------ }

function TDynDoubleArray.ToSingle: TDynSingleArray;
begin
  Result := TDynSingleArray.Create;
  Result.Assign(Self);
end;

procedure TDynDoubleArray.Assign(Source: TDynSingleArray);
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

procedure TDynDoubleArray.AddList(L: TDynDoubleArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(Double) * L.Count);
end;

procedure TDynDoubleArray.AddArray(const A: array of Double);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(Double) * (High(A) + 1));
end;

{ TDynCardinalArray ------------------------------------------------------------ }

function TDynCardinalArray.BigSum: Int64;
var i: integer;
begin
 result := 0;
 for i := 0 to Count-1 do result := result + Items[i];
end;

function TDynCardinalArray.Sum: Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result += Items[I];
end;

procedure TDynCardinalArray.AddList(L: TDynCardinalArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(Cardinal) * L.Count);
end;

procedure TDynCardinalArray.AddArray(const A: array of Cardinal);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(Cardinal) * (High(A) + 1));
end;

{ TDynBooleanArray ------------------------------------------------------------ }

procedure TDynBooleanArray.SetFalse(Index: Integer);
begin
 Items[Index] := false;
end;

procedure TDynBooleanArray.SetTrue(Index: Integer);
begin
 Items[Index] := true;
end;

function TDynBooleanArray.ToLongInt: TDynLongIntArray;
var
  I: Integer;
begin
  Result := TDynLongIntArray.Create;
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.Items[I] := Ord(Items[I]);
end;

procedure TDynBooleanArray.AddList(L: TDynBooleanArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(boolean) * L.Count);
end;

procedure TDynBooleanArray.AddArray(const A: array of boolean);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(boolean) * (High(A) + 1));
end;

{ TDynLongIntArray ----------------------------------------------------------- }

function TDynLongIntArray.Max: LongInt;
var
  I: Integer;
begin
  Result := Low(LongInt);
  for I := 0 to Count - 1 do
    MaxTo1st(Result, Items[I]);
end;

function TDynLongIntArray.Sum: LongInt;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result += Items[I];
end;

procedure TDynLongIntArray.DeleteRange(const Index: Integer; DelCount: Integer);
var
  I: Integer;
begin
  { Make sure Index and DelCount are sensible first }
  if Index >= Count then
    Exit;
  MinTo1st(DelCount, Count - Index);

  for I := Index to Count - 1 - DelCount do
    Items[I] := Items[I + DelCount];

  Count := Count - DelCount;
end;

procedure TDynLongIntArray.AddList(L: TDynLongIntArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(LongInt) * L.Count);
end;

procedure TDynLongIntArray.AddArray(const A: array of LongInt);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(LongInt) * (High(A) + 1));
end;

procedure TDynLongIntArray.AddDuplicate(const Item: LongInt; const DuplicateCount: Cardinal);
var
  I, C: Integer;
begin
  C := Count;
  Count := Count + DuplicateCount;
  for I := C to Count - 1 do
    Items[I] := Item;
end;

{ TDynIntegerArray ----------------------------------------------------------- }

function TDynIntegerArray.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result += Items[I];
end;

procedure TDynIntegerArray.AddList(L: TDynIntegerArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(Integer) * L.Count);
end;

procedure TDynIntegerArray.AddArray(const A: array of Integer);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(Integer) * (High(A) + 1));
end;

{ TDynStringArray ------------------------------------------------------------ }

procedure TDynStringArray.AddList(L: TDynStringArray);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + L.Count;
  if L.Count <> 0 then
    System.Move(L.List^[0], List^[OldCount], SizeOf(string) * L.Count);
end;

procedure TDynStringArray.AddArray(const A: array of string);
var
  OldCount: Integer;
begin
  OldCount := Count;
  Count := Count + High(A) + 1;
  if High(A) <> -1 then
    System.Move(A[0], List^[OldCount], SizeOf(string) * (High(A) + 1));
end;

procedure TDynStringArray.AssignArray(const A: array of string);
begin
  Clear;
  AddArray(A);
end;

{$endif}
