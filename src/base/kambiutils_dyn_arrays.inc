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

(*
  Base abstract dynamic array class (TDynArrayBase)
  and dynamic arrays for basic types (TDynIntegerArray etc.)

  Our dynarray.inc work like templates / generics, but hacked with only
  include files (so they work with compiler).
  The template file is named dynarray.inc, it is also multiplicated
  inside templates/ directory as dynarray_1.inc etc.
  (multiplication is useful to be able to use the same template
  more than once in a single unit).

  To use this template:
  @orderedList(
    @item(
      In the unit's interface, when symbol @code(read_interface) is defined, use:

@longCode(#
type
  TDynArrayItem_x = <TTT>;
  PDynArrayItem_x = ^TDynArrayItem_x;
  // optionally: {$define DYNARRAY_x_IS_STRUCT}
  // optionally: {$define DYNARRAY_x_IS_INIT_FINI_TYPE}
  {$I dynarray_x.inc}
  TDyn<TTT>Array = TDynArray_x;
#))

    @item(
      In the unit's implementation, when symbol @code(read_implementation)
      is defined, use:

@longCode(#
  {$I dynarray_x.inc}
#)))

  And don't change anywhere else the state of defines DYNARRAY_x_*,
  and don't use anywhere else the names TDynArray_x, TDynArrayItem_x etc.
*)

{$ifdef read_interface}

type
  { Forward-declare TDynLongintArray, used by AssignToInterleaved definitions. }
  { }
  TDynLongintArray = class;

  EAssignInterleavedRangeError = class(Exception);

  { Base abstract dynamic array class. }
  TDynArrayBase = class
  private
    FCount: Integer;
    FCapacity, FAllowedCapacityOverflow: Cardinal;
    ItemSize: Cardinal;

    { This is the buffer used by Exchange, allocated for ItemSize bytes
      in constructor, freed in desctructor. This way Exchange doesn't
      have to do any Get/FreeMem and can work fast, not dependent on the
      speed of memory manager. }
    ExchangeBuf: Pointer;

    procedure SetAllowedCapacityOverflow(Value: Cardinal);
  protected
    { @noAutoLinkHere }
    FList: Pointer;

    { In this class, SetCount is just equivalent to SetCountNoInitFini.

      In descendants, you can override SetCount to allow to Initialize
      and Finalize items. But this procedure, @name, is not virtual
      and is not intended to be overriden, so you can use this in cases
      when you're sure that you don't want to do Initialize / Finalize
      your items. }
    procedure SetCountNoInitFini(NewCount: Integer);
    procedure SetCount(NewCount: Integer); virtual;
    procedure IncCount;
    procedure DecCount;
    procedure ChangeCount(LenChange: Integer);
  public
    { How much the allocated @link(Capacity) may be larger than @link(Count).
      Larger values allow us to be more prepared for adding new items,
      but also waste more memory.

      Always @code((Capacity - Count) <= AllowedCapacityOverflow).

      For arrays that are going to be read-only for some time,
      it's useful to even set AllowedCapacityOverflow := 0. }
    property AllowedCapacityOverflow: Cardinal read FAllowedCapacityOverflow
      write SetAllowedCapacityOverflow default 4;

    property Capacity: Cardinal read FCapacity;

    { Count of items.
      These are actually always unsigned, but are declared as signed Integers
      to avoid range errors on constructs like @code(Count - 1). }
    property Count: Integer read FCount write SetCount;

    procedure Exchange(Index1, Index2: Cardinal);

    { Fill the array memory. }
    procedure FillChar(FillValue: byte);

    { Reverse the order of items on the array. }
    procedure Reverse;

    { Remove all items, equivalent to doing @code(Count := 0). }
    procedure Clear;

    constructor Create(ElementSize, InitialCount: Integer);

    destructor Destroy; override;
  end;

{ Declare all TDynXxxArray for standard Pascal types }

type
  TDynSingleArray = class;
  TDynDoubleArray = class;

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

{ TDynArrayBase ---------------------------------------- }

constructor TDynArrayBase.Create(ElementSize, InitialCount: Integer);
begin
 inherited Create;
 ItemSize := ElementSize;
 FAllowedCapacityOverflow := 4;
 ExchangeBuf := GetMem(ItemSize);
 SetCount(InitialCount);
end;

destructor TDynArrayBase.Destroy;
begin
 { SetCount sluzy tylko temu zeby zrobic ew. Finalize na elementach tablicy.
   Dopiero FreeMemNiling gwarantuje ze pamiec zaalokowana dla A zostanie
   rzeczywiscie zwolniona. }
 SetCount(0);
 FreeMemNiling(FList);
 FreeMemNiling(ExchangeBuf);
 inherited;
end;

procedure TDynArrayBase.SetAllowedCapacityOverflow(Value: Cardinal);
var NewCapacity: Cardinal;
begin
 if Capacity - Cardinal(Count) > Value then
 begin
  NewCapacity := Cardinal(Count) + Value;
  ReallocMem(FList, NewCapacity * ItemSize);
  FCapacity := NewCapacity;
 end;
 FAllowedCapacityOverflow := Value;
end;

{ Note that the only methods that reallocate some memory are
  SetAllowedCapacityCount and this SetCountNoInitFini.
  These are also the only methods that can change FCapacity,
  FCount (only by SetCountNoInitFini) and
  FAllowedCapacityOverflow (only by SetAllowedCapacityCount).
  In these methods we care about keeping assertions

    Count <= Capacity

  and

    (Capacity-Count) <= AllowedCapacityOverflow.

  Note that items with indexes Count .. Capacity-1 are never
  read/written and are never supposed to be initialized by Initialize.
  So only on items with indexes 0 .. Count-1 you can call
  Initialize / Finalize, other indexes are just our "safety bag
  of memory" and are not used by SetCount implementation in descendants
  that call Initialize / Finalize. }

procedure TDynArrayBase.SetCountNoInitFini(NewCount: Integer);

  procedure ReallocCapacity(NewCapacity: Cardinal);
  begin
   ReallocMem(FList, NewCapacity * ItemSize);
   FCapacity := NewCapacity;
  end;

begin
 if NewCount > Count then
 begin
  if Cardinal(NewCount) > Capacity then
   ReallocCapacity(Cardinal(NewCount) + AllowedCapacityOverflow);
   { else no need to do any memory reallocation }
 end else
 if NewCount < Count then
 begin
  if Capacity - Cardinal(NewCount) > AllowedCapacityOverflow then
   ReallocCapacity(NewCount);
 end;

 FCount := NewCount;
end;

procedure TDynArrayBase.SetCount(NewCount: Integer);
begin
 SetCountNoInitFini(NewCount);
end;

procedure TDynArrayBase.IncCount;
begin ChangeCount(1) end;

procedure TDynArrayBase.DecCount;
begin ChangeCount(-1) end;

procedure TDynArrayBase.ChangeCount(LenChange: Integer);
begin SetCount(Count+LenChange) end;

procedure TDynArrayBase.Exchange(Index1, Index2: Cardinal);
var p1, p2: PByte;
begin
  {$I norqcheckbegin.inc}
  PtrUInt(p1) := PtrUInt(FList) + Index1 * ItemSize;
  PtrUInt(p2) := PtrUInt(FList) + Index2 * ItemSize;
  {$I norqcheckend.inc}
  Move(p1^, ExchangeBuf^, ItemSize);
  Move(p2^, p1^, ItemSize);
  Move(ExchangeBuf^, p2^, ItemSize);
end;

procedure TDynArrayBase.FillChar(FillValue: byte);
begin
 System.FillChar(FList^, ItemSize * Cardinal(Count), FillValue);
end;

procedure TDynArrayBase.Reverse;
var
  I: Integer;
begin
  { Need to specially check for Count = 0 case, since (0-1) div 2 = -1 div 2 = 0
    which means that loop would try invalid Exchange(0, -1). }
  if Count = 0 then Exit;
  for I := 0 to (Count - 1) div 2 do
    Exchange(I, Count - 1 - I);
end;

procedure TDynArrayBase.Clear;
begin
  Count := 0;
end;

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
