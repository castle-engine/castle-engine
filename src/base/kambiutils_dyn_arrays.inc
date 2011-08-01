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
  and dynamic arrays for basic types (TDynIntegerArray, TDynStringArray etc.)

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
    FLength: Integer;
    FCapacity, FAllowedCapacityOverflow: Cardinal;
    ItemSize: Cardinal;

    { This is the buffer used by SwapItems, allocated for ItemSize bytes
      in constructor, freed in desctructor. This way SwapItems doesn't
      have to do any Get/FreeMem and can work fast, not dependent on the
      speed of memory manager. }
    SwapItemsBuf: Pointer;

    procedure SetAllowedCapacityOverflow(Value: Cardinal);
  protected
    { @noAutoLinkHere }
    A: pointer;

    { In this class, SetLength is just equivalent to SetLengthNoInitFini.

      In descendants, you can override SetLength to allow to Initialize
      and Finalize items. But this procedure, @name, is not virtual
      and is not intended to be overriden, so you can use this in cases
      when you're sure that you don't want to do Initialize / Finalize
      your items. }
    procedure SetLengthNoInitFini(NewLength: Integer);
  public
    { How much the allocated @link(Capacity) may be larger than @link(Length).
      Larger values allow us to be more prepared for adding new items,
      but also waste more memory.

      Always @code((Capacity - Length) <= AllowedCapacityOverflow).

      For arrays that are going to be read-only for some time,
      it's useful to even set AllowedCapacityOverflow := 0. }
    property AllowedCapacityOverflow: Cardinal read FAllowedCapacityOverflow
      write SetAllowedCapacityOverflow default 4;

    property Capacity: Cardinal read FCapacity;

    procedure SetLength(NewLength: Integer); virtual;
    procedure IncLength;
    procedure DecLength;
    procedure ChangeLength(LenChange: Integer);

    { Count of items. Count is the same thing as Length.
      These are actually always unsigned, but are declared as signed Integers
      to avoid range errors on constructs like @code(Count - 1). }
    property Length: Integer read FLength write SetLength;
    property Count: Integer read FLength write SetLength;

    { Last item index. Just @code(Length - 1), in particular when Count = 0
      then High = -1. }
    function High: Integer;

    procedure SwapItems(Index1, Index2: Cardinal);

    { Fill the array memory. }
    procedure FillChar(FillValue: byte);

    { Reverse the order of items on the array. }
    procedure Reverse;

    { Remove all items, equivalent to doing @code(Count := 0). }
    procedure Clear;

    constructor Create(ElementSize, InitialLength: Integer);

    destructor Destroy; override;

    { Copy our contents to given Target memory. Each item in Target
      is separated by the Stride bytes.

      We copy CopyCount items (you usually want to pass the count of Target
      data here). Our Count must be >= CopyCount,
      we check it and eventually raise EAssignInterleavedRangeError.

      Warning: this is safely usable only for arrays of types that don't
      require initialization / finalization. Otherwise target memory data
      will be properly referenced.

      @raises EAssignInterleavedRangeError When Count < CopyCount. }
    procedure AssignToInterleaved(Target: Pointer;
      const Stride, CopyCount: Cardinal);

    { Copy our contents to given Target memory. Each item in Target
      is separated by the Stride bytes.

      We copy CopyCount items (you usually want to pass the count of Target
      data here). Indexes.Count must be >= CopyCount,
      we check it and eventually raise EAssignInterleavedRangeError.

      Item number I is taken from Items[Indexes[I]].
      All values on Indexes list must be valid (that is >= 0 and < our Count),
      or we raise EAssignInterleavedRangeError.

      Warning: this is safely usable only for arrays of types that don't
      require initialization / finalization. Otherwise target memory data
      will be properly referenced.

      @raises(EAssignInterleavedRangeError When Indexes.Count < CopyCount,
        or some index points outside of array.) }
    procedure AssignToInterleavedIndexed(Target: Pointer;
      const Stride, CopyCount: Cardinal; Indexes: TDynLongIntArray);
  end;

{ Declare all TDynXxxArray for standard Pascal types }

type
  {$define DYNARRAY_1_USE_EQUALITY_OP}
  {$define DYNARRAY_1_USE_PLUSMINUS_OP}
  { }
  TDynArrayItem_1  = Cardinal; PDynArrayItem_1  = PCardinal; {$I dynarray_1.inc}  {TDynCardinalArray is below} { }      TArray_Cardinal = TInfiniteArray_1 ; PArray_Cardinal = PInfiniteArray_1 ;

  {$define DYNARRAY_2_USE_EQUALITY_OP}
  {$define DYNARRAY_2_USE_PLUSMINUS_OP}
  TDynArrayItem_2  = Integer ; PDynArrayItem_2  = PInteger ; {$I dynarray_2.inc}  TDynIntegerArray  = TDynArray_2 ; TArray_Integer  = TInfiniteArray_2 ; PArray_Integer  = PInfiniteArray_2 ;

  {$define DYNARRAY_3_USE_EQUALITY_OP}
  {$define DYNARRAY_3_USE_PLUSMINUS_OP}
  TDynArrayItem_3  = Float   ; PDynArrayItem_3  = PFloat   ; {$I dynarray_3.inc}  {TDynFloatArray is below} { }         TArray_Float    = TInfiniteArray_3 ; PArray_Float    = PInfiniteArray_3 ;

  {$define DYNARRAY_8_USE_EQUALITY_OP}
  {$define DYNARRAY_8_USE_PLUSMINUS_OP}
  TDynArrayItem_8  = Longint ; PDynArrayItem_8  = PLongint ; {$I dynarray_8.inc}                                    TArray_Longint  = TInfiniteArray_8 ; PArray_Longint  = PInfiniteArray_8 ;

  {$define DYNARRAY_11_USE_EQUALITY_OP}
  {$define DYNARRAY_11_USE_PLUSMINUS_OP}
  TDynArrayItem_11 = Double  ; PDynArrayItem_11 = PDouble  ; {$I dynarray_11.inc} {TDynDoubleArray is below} { }        TArray_Double   = TInfiniteArray_11; PArray_Double   = PInfiniteArray_11;

  {$define DYNARRAY_12_USE_EQUALITY_OP}
  {$define DYNARRAY_12_USE_PLUSMINUS_OP}
  TDynArrayItem_12 = Single  ; PDynArrayItem_12 = PSingle  ; {$I dynarray_12.inc} {TDynSingleArray is below} { }        TArray_Single   = TInfiniteArray_12; PArray_Single   = PInfiniteArray_12;

  {$define DYNARRAY_13_USE_EQUALITY_OP}
  TDynArrayItem_13 = Pointer ; PDynArrayItem_13 = PPointer ; {$I dynarray_13.inc} TDynPointerArray  = TDynArray_13; TArray_Pointer  = TInfiniteArray_13; PArray_Pointer  = PInfiniteArray_13;

  {$define DYNARRAY_14_USE_EQUALITY_OP}
  {$define DYNARRAY_14_USE_PLUSMINUS_OP}
  TDynArrayItem_14 = LongWord; PDynArrayItem_14 = PLongWord; {$I dynarray_14.inc} TDynLongWordArray = TDynArray_14; TArray_LongWord = TInfiniteArray_14; PArray_LongWord = PInfiniteArray_14;

  {$define DYNARRAY_15_USE_EQUALITY_OP}
  {$define DYNARRAY_15_IS_INIT_FINI_TYPE}
  TDynArrayItem_15 = String  ; PDynArrayItem_15 = PString  ; {$I dynarray_15.inc} TDynStringArray   = TDynArray_15; TArray_String   = TInfiniteArray_15; PArray_String   = PInfiniteArray_15;

  {$define DYNARRAY_16_USE_EQUALITY_OP}
  TDynArrayItem_16 = Boolean ; PDynArrayItem_16 = PBoolean ; {$I dynarray_16.inc} {TDynBooleanArray is below} { }       TArray_Boolean  = TInfiniteArray_16; PArray_Boolean  = PInfiniteArray_16;

  TDynSingleArray = class;
  TDynDoubleArray = class;

  TDynFloatArray = class(TDynArray_3)
    procedure AppendFloats(Floats: TDynSingleArray);
  end;

  TDynSingleArray = class(TDynArray_12)
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
  end;

  TDynDoubleArray = class(TDynArray_11)
    function ToSingle: TDynSingleArray;
    { Assign value from TDynSingleArray, converting to double-precision. }
    procedure Assign(Source: TDynSingleArray); overload;
  end;

  TDynCardinalArray = class(TDynArray_1)
    function BigSum: Int64;
    function Sum: Cardinal;
  end;

  TDynBooleanArray = class(TDynArray_16)
  public
    { Set appropriate item to given value.
      These are useful methods to pass as callback in some situations
      (e.g. TVRMLGLScene.RenderFrustumOctree passes
      RenderFrustumOctree_Visible.SetTrue method as callback
      that should mark visible items.) }
    procedure SetFalse(Index: Integer);
    procedure SetTrue(Index: Integer);

    function ToLongInt: TDynLongIntArray;
  end;

  TDynLongIntArray = class(TDynArray_8)
  public
    function Max: LongInt;
    function Sum: LongInt;
  end;

{$endif}

{$ifdef read_implementation}

{ TDynArrayBase ---------------------------------------- }

constructor TDynArrayBase.Create(ElementSize, InitialLength: Integer);
begin
 inherited Create;
 ItemSize := ElementSize;
 FAllowedCapacityOverflow := 4;
 SwapItemsBuf := GetMem(ItemSize);
 SetLength(InitialLength);
end;

destructor TDynArrayBase.Destroy;
begin
 { SetLength sluzy tylko temu zeby zrobic ew. Finalize na elementach tablicy.
   Dopiero FreeMemNiling gwarantuje ze pamiec zaalokowana dla A zostanie
   rzeczywiscie zwolniona. }
 SetLength(0);
 FreeMemNiling(A);
 FreeMemNiling(SwapItemsBuf);
 inherited;
end;

function TDynArrayBase.High: Integer; begin result := Length-1 end;

procedure TDynArrayBase.SetAllowedCapacityOverflow(Value: Cardinal);
var NewCapacity: Cardinal;
begin
 if Capacity - Cardinal(Length) > Value then
 begin
  NewCapacity := Cardinal(Length) + Value;
  ReallocMem(A, NewCapacity * ItemSize);
  FCapacity := NewCapacity;
 end;
 FAllowedCapacityOverflow := Value;
end;

{ Note that the only methods that reallocate some memory are
  SetAllowedCapacityCount and this SetLengthNoInitFini.
  These are also the only methods that can change FCapacity,
  FLength (only by SetLengthNoInitFini) and
  FAllowedCapacityOverflow (only by SetAllowedCapacityCount).
  In these methods we care about keeping assertions

    Length <= Capacity

  and

    (Capacity-Length) <= AllowedCapacityOverflow.

  Note that items with indexes Length .. Capacity-1 are never
  read/written and are never supposed to be initialized by Initialize.
  So only on items with indexes 0 .. Length-1 you can call
  Initialize / Finalize, other indexes are just our "safety bag
  of memory" and are not used by SetLength implementation in descendants
  that call Initialize / Finalize. }

procedure TDynArrayBase.SetLengthNoInitFini(NewLength: Integer);

  procedure ReallocCapacity(NewCapacity: Cardinal);
  begin
   ReallocMem(A, NewCapacity * ItemSize);
   FCapacity := NewCapacity;
  end;

begin
 if NewLength > Length then
 begin
  if Cardinal(NewLength) > Capacity then
   ReallocCapacity(Cardinal(NewLength) + AllowedCapacityOverflow);
   { else no need to do any memory reallocation }
 end else
 if NewLength < Length then
 begin
  if Capacity - Cardinal(NewLength) > AllowedCapacityOverflow then
   ReallocCapacity(NewLength);
 end;

 FLength := NewLength;
end;

procedure TDynArrayBase.SetLength(NewLength: Integer);
begin
 SetLengthNoInitFini(NewLength);
end;

procedure TDynArrayBase.IncLength;
begin ChangeLength(1) end;

procedure TDynArrayBase.DecLength;
begin ChangeLength(-1) end;

procedure TDynArrayBase.ChangeLength(LenChange: Integer);
begin SetLength(Length+LenChange) end;

procedure TDynArrayBase.SwapItems(Index1, Index2: Cardinal);
var p1, p2: PByte;
begin
 {$I norqcheckbegin.inc}
 PtrUInt(p1) := PtrUInt(A) + Index1 * ItemSize;
 PtrUInt(p2) := PtrUInt(A) + Index2 * ItemSize;
 {$I norqcheckend.inc}
 move(p1^, SwapItemsBuf^, ItemSize);
 move(p2^, p1^, ItemSize);
 move(SwapItemsBuf^, p2^, ItemSize);
end;

procedure TDynArrayBase.FillChar(FillValue: byte);
begin
 System.FillChar(A^, ItemSize * Cardinal(Length), FillValue);
end;

procedure TDynArrayBase.Reverse;
var i: Integer;
begin
 { musimy osobno sprawdzac przypadek Count = 0, bo (0-1) div 2 = -1 div 2 = 0
   a to spowodowaloby blad w ponzszej for, iterowalaby od 0 do 0 a nie przeciez
   elementu numer 0 gdy Count = 0. }
 if Count = 0 then Exit;
 for i := 0 to (Count-1) div 2 do SwapItems(i, Count-1-i);
end;

procedure TDynArrayBase.Clear;
begin
  Count := 0;
end;

procedure TDynArrayBase.AssignToInterleaved(Target: Pointer;
  const Stride, CopyCount: Cardinal);
var
  I: Integer;
  Source: Pointer;
begin
  if Count < CopyCount then
    raise EAssignInterleavedRangeError.CreateFmt('Not enough items: %d, but at least %d required',
      [Count, CopyCount]);

  Source := A;
  for I := 0 to CopyCount - 1 do
  begin
    Move(Source^, Target^, ItemSize);
    PtrUInt(Source) += ItemSize;
    PtrUInt(Target) += Stride;
  end;
end;

procedure TDynArrayBase.AssignToInterleavedIndexed(Target: Pointer;
  const Stride, CopyCount: Cardinal; Indexes: TDynLongIntArray);
var
  I: Integer;
  Index: LongInt;
begin
  if Indexes.Count < CopyCount then
    raise EAssignInterleavedRangeError.CreateFmt('Not enough items: %d, but at least %d required',
      [Indexes.Count, CopyCount]);

  for I := 0 to CopyCount - 1 do
  begin
    Index := Indexes.Items[I];
    if (Index < 0) or
       (Index >= Count) then
      raise EAssignInterleavedRangeError.CreateFmt('Invalid index: %d, but we have %d items',
        [Index, Count]);

    { Beware to not make multiplication below (* ItemSize) using 64-bit ints.
      This would cause noticeable slowdown when using AssignToInterleavedIndexed
      for VRMLArraysGenerator, that in turn affects dynamic scenes
      and especially dynamic shading like radiance_transfer. }
    Move(Pointer(PtrUInt(A) + PtrUInt(Index) * ItemSize)^,
      Target^, ItemSize);
    PtrUInt(Target) += Stride;
  end;
end;

{ include all implementations ---------------------------------------- }

{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I dynarray_3.inc}
{$I dynarray_8.inc}
{$I dynarray_11.inc}
{$I dynarray_12.inc}
{$I dynarray_13.inc}
{$I dynarray_14.inc}
{$I dynarray_15.inc}
{$I dynarray_16.inc}

{ TDynFloatArray ------------------------------------------------------------ }

procedure TDynFloatArray.AppendFloats(Floats: TDynSingleArray);
var OldCount, i: Integer;
begin
 OldCount := Count;
 Count := Count + Floats.Count;
 for i := 0 to Floats.High do Items[OldCount+i] := Floats.Items[i];
end;

{ TDynSingleArray ------------------------------------------------------------ }

procedure TDynSingleArray.AppendFloats(Floats: TDynFloatArray);
var OldCount, i: Integer;
begin
 OldCount := Count;
 Count := Count + Floats.Count;
 for i := 0 to Floats.High do Items[OldCount+i] := Floats.Items[i];
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
  Src := PDouble(Source.ItemsArray);
  Dest := PSingle(ItemsArray);
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
  Src := PSingle(Source.ItemsArray);
  Dest := PDouble(ItemsArray);
  for I := 0 to Count - 1 do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TDynCardinalArray ------------------------------------------------------------ }

function TDynCardinalArray.BigSum: Int64;
var i: integer;
begin
 result := 0;
 for i := 0 to Length-1 do result := result + Items[i];
end;

function TDynCardinalArray.Sum: Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High do
    Result += Items[I];
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
  Result := TDynLongIntArray.Create(Count);
  for I := 0 to High do
    Result.Items[I] := Ord(Items[I]);
end;

function TDynLongIntArray.Max: LongInt;
var
  I: Integer;
begin
  Result := Low(LongInt);
  for I := 0 to High do
    MaxTo1st(Result, Items[I]);
end;

function TDynLongIntArray.Sum: LongInt;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High do
    Result += Items[I];
end;

{$endif}
