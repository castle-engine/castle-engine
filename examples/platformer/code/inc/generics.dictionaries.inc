{%MainUnit generics.collections.pas}

{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains the generics collections library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types and major refactoring of entire library

    Thanks to mORMot (http://synopse.info) project for the best implementations
    of hashing functions like crc32c and xxHash32 :)

 **********************************************************************}

{ TPair<TKey,TValue> }

class function TPair<TKey, TValue>.Create(AKey: TKey;
  AValue: TValue): TPair<TKey, TValue>;
begin
  Result.Key := AKey;
  Result.Value := AValue;
end;

{ TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS> }

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.PairNotify(constref APair: TDictionaryPair;
  ACollectionNotification: TCollectionNotification);
begin
  KeyNotify(APair.Key, ACollectionNotification);
  ValueNotify(APair.Value, ACollectionNotification);
end;

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.KeyNotify(constref AKey: TKey;
  ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, AKey, ACollectionNotification);
end;

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.SetValue(var AValue: TValue; constref ANewValue: TValue);
var
  LOldValue: TValue;
begin
  LOldValue := AValue;
  AValue := ANewValue;

  ValueNotify(LOldValue, cnRemoved);
  ValueNotify(ANewValue, cnAdded);
end;

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.ValueNotify(constref AValue: TValue;
  ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, AValue, ACollectionNotification);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create;
begin
  Create(0);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACapacity: SizeInt); overload;
begin
  Create(ACapacity, TEqualityComparer<TKey>.Default(THashFactory));
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACapacity: SizeInt;
  const AComparer: IEqualityComparer<TKey>);
begin
  FEqualityComparer := AComparer;
  SetCapacity(ACapacity);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>);
begin
  Create(ACollection, TEqualityComparer<TKey>.Default(THashFactory));
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>);
begin
  Create(ACollection, TEqualityComparer<TKey>.Default(THashFactory));
end;
{$ENDIF}

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>;
  const AComparer: IEqualityComparer<TKey>); overload;
var
  LItem: TDictionaryPair;
begin
  Create(AComparer);
  for LItem in ACollection do
    Add(LItem);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>;
  const AComparer: IEqualityComparer<TKey>); overload;
var
  LItem: PDictionaryPair;
begin
  Create(AComparer);
  for LItem in ACollection.Ptr^ do
    Add(LItem^);
end;
{$ENDIF}

destructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Destroy;
begin
  Clear;
  FKeys.Free;
  FValues.Free;
  inherited;
end;

function TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.ToArray(ACount: SizeInt): TArray<TDictionaryPair>;
var
  i: SizeInt;
  LEnumerator: TEnumerator<TDictionaryPair>;
begin
  Result := nil;
  SetLength(Result, ACount);
  LEnumerator := DoGetEnumerator;

  i := 0;
  while LEnumerator.MoveNext do
  begin
    Result[i] := LEnumerator.Current;
    Inc(i);
  end;
  LEnumerator.Free;
end;

function TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.ToArray: TArray<TDictionaryPair>;
begin
  Result := ToArray(Count);
end;

{ TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS> }

constructor TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>.Create(
  ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

{ TDictionaryEnumerable<TDictionaryEnumerator, TDictionaryPointersEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS> }

function TDictionaryEnumerable<TDictionaryEnumerator, TDictionaryPointersEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.GetPtrEnumerator: TEnumerator<PT>;
begin
  Result := TDictionaryPointersEnumerator.Create(FDictionary);
end;

constructor TDictionaryEnumerable<TDictionaryEnumerator, TDictionaryPointersEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.Create(
  ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
begin
  FDictionary := ADictionary;
end;

function TDictionaryEnumerable<TDictionaryEnumerator, TDictionaryPointersEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.
  DoGetEnumerator: TDictionaryEnumerator;
begin
  Result := TDictionaryEnumerator(TDictionaryEnumerator.NewInstance);
  TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>(Result).Create(FDictionary);
end;

function TDictionaryEnumerable<TDictionaryEnumerator, TDictionaryPointersEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.GetCount: SizeInt;
begin
  Result := TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).Count;
end;

function TDictionaryEnumerable<TDictionaryEnumerator, TDictionaryPointersEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(FDictionary.Count);
end;

{ TOpenAddressingEnumerator<T, DICTIONARY_CONSTRAINTS> }

function TOpenAddressingEnumerator<T, OPEN_ADDRESSING_CONSTRAINTS>.DoMoveNext: Boolean;
var
  LLength: SizeInt;
begin
  Inc(FIndex);

  LLength := Length(TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>(FDictionary).FItems);

  if FIndex >= LLength then
    Exit(False);

  // maybe related to bug #24098
  // compiler error for (TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Hash and UInt32.GetSignMask) = 0
  while ((TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>(FDictionary).FItems[FIndex].Hash) and UInt32.GetSignMask) = 0 do
  begin
    Inc(FIndex);
    if FIndex = LLength then
      Exit(False);
  end;

  Result := True;
end;

{ TOpenAddressingPointersEnumerator<TItem, PDictionaryPair> }

function TOpenAddressingPointersEnumerator<TItem, PDictionaryPair>.DoMoveNext: boolean;
var
  LLength: SizeInt;
begin
  Inc(FIndex);

  LLength := Length(FItems^);

  if FIndex >= LLength then
    Exit(False);

  // maybe related to bug #24098
  // compiler error for (TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Hash and UInt32.GetSignMask) = 0
  while (FItems^[FIndex].Hash and UInt32.GetSignMask) = 0 do
  begin
    Inc(FIndex);
    if FIndex = LLength then
      Exit(False);
  end;

  Result := True;
end;

function TOpenAddressingPointersEnumerator<TItem, PDictionaryPair>.DoGetCurrent: PDictionaryPair;
begin
  Result := GetCurrent;
end;

function TOpenAddressingPointersEnumerator<TItem, PDictionaryPair>.GetCurrent: PDictionaryPair;
begin
  Result := @FItems^[FIndex].Pair;
end;

constructor TOpenAddressingPointersEnumerator<TItem, PDictionaryPair>.Create(var AItems);
begin
  FIndex := -1;
  FItems := @AItems;
end;

{ TOpenAddressingPointersCollection<TPointersEnumerator, TItem, PDictionaryPair> }

function TOpenAddressingPointersCollection<TPointersEnumerator, TItem, PDictionaryPair>.Items: PArray;
begin
  Result := PArray(@((@Self)^));
end;

function TOpenAddressingPointersCollection<TPointersEnumerator, TItem, PDictionaryPair>.GetCount: SizeInt;
begin
  Result := PSizeInt(PByte(@((@Self)^))-SizeOf(SizeInt))^;
end;

function TOpenAddressingPointersCollection<TPointersEnumerator, TItem, PDictionaryPair>.GetEnumerator: TPointersEnumerator;
begin
  Result := TPointersEnumerator(TPointersEnumerator.NewInstance);
  TPointersEnumerator(Result).Create(Items^);
end;

function TOpenAddressingPointersCollection<TPointersEnumerator, TItem, PDictionaryPair>.ToArray: TArray<PDictionaryPair>;
{begin
  Result := ToArrayImpl(FList.Count);
end;}
var
  i: SizeInt;
  LEnumerator: TPointersEnumerator;
begin
  Result := nil;
  SetLength(Result, GetCount);

  try
    LEnumerator := GetEnumerator;

    i := 0;
    while LEnumerator.MoveNext do
    begin
      Result[i] := LEnumerator.Current;
      Inc(i);
    end;
  finally
    LEnumerator.Free;
  end;
end;

{ TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS> }

constructor TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACapacity: SizeInt;
  const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);

  FMaxLoadFactor := TProbeSequence.DEFAULT_LOAD_FACTOR;
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetKeys: TKeyCollection;
begin
  if not Assigned(FKeys) then
    FKeys := TKeyCollection.Create(Self);
  Result := TKeyCollection(FKeys);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetValues: TValueCollection;
begin
  if not Assigned(FValues) then
    FValues := TValueCollection.Create(Self);
  Result := TValueCollection(FValues);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndex(constref AKey: TKey): SizeInt;
var
  LHash: UInt32;
begin
  Result := FindBucketIndex(FItems, AKey, LHash);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.PrepareAddingItem;
begin
  if RealItemsLength > FItemsThreshold then
    Rehash(Length(FItems) shl 1)
  else if FItemsThreshold = 0 then
  begin
    SetLength(FItems, 8);
    UpdateItemsThreshold(8);
  end
  else if FItemsLength = $40000001 then // High(TIndex) ... Error: Type mismatch
    OutOfMemoryError;
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.UpdateItemsThreshold(ASize: SizeInt);
begin
  if ASize = $40000000 then
    FItemsThreshold := $40000001
  else
    FItemsThreshold := Pred(Round(ASize * FMaxLoadFactor));
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.AddItem(var AItem: TItem; constref AKey: TKey;
  constref AValue: TValue; const AHash: UInt32);
begin
  AItem.Hash := AHash;
  AItem.Pair.Key := AKey;
  AItem.Pair.Value := AValue;

  // ! very important. FItemsLength must be increased after above code (because constref has meaning)
  Inc(FItemsLength);

  PairNotify(AItem.Pair, cnAdded);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetPointers: PPointersCollection;
begin
  Result := PPointersCollection(@FItems);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.Add(constref AKey: TKey; constref AValue: TValue);
begin
  DoAdd(AKey, AValue);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.Add(constref APair: TPair<TKey, TValue>);
begin
  DoAdd(APair.Key, APair.Value);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.DoAdd(constref AKey: TKey; constref AValue: TValue): SizeInt;
var
  LHash: UInt32;
begin
  PrepareAddingItem;

  Result := FindBucketIndex(FItems, AKey, LHash);
  if Result >= 0 then
    raise EListError.CreateRes(@SDuplicatesNotAllowed);

  Result := not Result;
  AddItem(FItems[Result], AKey, AValue, LHash);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.DoRemove(AIndex: SizeInt;
  ACollectionNotification: TCollectionNotification): TValue;
var
  LItem: PItem;
  LPair: TPair<TKey, TValue>;
begin
  LItem := @FItems[AIndex];
  LItem.Hash := 0;
  Result := LItem.Pair.Value;
  LPair := LItem.Pair;
  LItem.Pair := Default(TPair<TKey, TValue>);
  Dec(FItemsLength);
  PairNotify(LPair, ACollectionNotification);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.Remove(constref AKey: TKey);
var
  LIndex: SizeInt;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex < 0 then
    Exit;

  DoRemove(LIndex, cnRemoved);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.ExtractPair(constref AKey: TKey): TPair<TKey, TValue>;
var
  LIndex: SizeInt;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex < 0 then
    Exit(Default(TPair<TKey, TValue>));

  Result.Key := AKey;
  Result.Value := DoRemove(LIndex, cnExtracted);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.Clear;
var
  LItem: PItem;
  i: SizeInt;
  LOldItems: array of TItem;
begin
  FItemsLength := 0;
  FItemsThreshold := 0;
  // ClearTombstones;
  LOldItems := FItems;
  FItems := nil;

  for i := 0 to High(LOldItems) do
  begin
    LItem := @LOldItems[i];
    if (LItem.Hash and UInt32.GetSignMask = 0) then
      Continue;

    PairNotify(LItem.Pair, cnRemoved);
  end;
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.RealItemsLength: SizeInt;
begin
  Result := FItemsLength;
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.Rehash(ASizePow2: SizeInt; AForce: Boolean): Boolean;
var
  LNewItems: TArray<TItem>;
  LHash: UInt32;
  LIndex: SizeInt;
  i: SizeInt;
  LItem, LNewItem: PItem;
begin
  if (ASizePow2 = Length(FItems)) and not AForce then
    Exit(False);
  if ASizePow2 < 0 then
    OutOfMemoryError;

  LNewItems := nil;
  SetLength(LNewItems, ASizePow2);
  UpdateItemsThreshold(ASizePow2);

  for i := 0 to High(FItems) do
  begin
    LItem := @FItems[i];

    if (LItem.Hash and UInt32.GetSignMask) <> 0 then
    begin
      LIndex := FindBucketIndex(LNewItems, LItem.Pair.Key, LHash);
      LIndex := not LIndex;

      LNewItem := @LNewItems[LIndex];
      LNewItem.Hash := LHash;
      LNewItem.Pair := LItem.Pair;
    end;
  end;

  FItems := LNewItems;
  Result := True;
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.DoGetEnumerator: TEnumerator<TDictionaryPair>;
begin
  Result := GetEnumerator;
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.SetCapacity(ACapacity: SizeInt);
begin
  if ACapacity < FItemsLength then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Resize(ACapacity);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.SetMaxLoadFactor(AValue: single);
var
  LItemsLength: SizeInt;
begin
  if (AValue > TProbeSequence.MAX_LOAD_FACTOR) or (AValue <= 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  FMaxLoadFactor := AValue;

  repeat
    LItemsLength := Length(FItems);
    UpdateItemsThreshold(LItemsLength);
    if RealItemsLength > FItemsThreshold then
      Rehash(LItemsLength shl 1);
  until RealItemsLength <= FItemsThreshold;
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetLoadFactor: single;
begin
  Result := FItemsLength / Length(FItems);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetCapacity: SizeInt;
begin
  Result := Length(FItems);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.Resize(ANewSize: SizeInt);
var
  LNewSize: SizeInt;
begin
  if ANewSize < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LNewSize := 0;
  if ANewSize > 0 then
  begin
    LNewSize := 8;
    while LNewSize < ANewSize do
      LNewSize := LNewSize shl 1;
  end;

  Rehash(LNewSize);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetItem(const AKey: TKey): TValue;
var
  LIndex: SizeInt;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex < 0 then
    raise EListError.CreateRes(@SDictionaryKeyDoesNotExist);
  Result := FItems[LIndex].Pair.Value;
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TrimExcess;
begin
  SetCapacity(Succ(FItemsLength));
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.SetItem(const AKey: TKey; const AValue: TValue);
var
  LIndex: SizeInt;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex < 0 then
    raise EListError.CreateRes(@SItemNotFound);

  SetValue(FItems[LIndex].Pair.Value, AValue);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TryGetValue(constref AKey: TKey; out AValue: TValue): Boolean;
var
  LIndex: SizeInt;
begin
  LIndex := FindBucketIndex(AKey);
  Result := LIndex >= 0;

  if Result then
    AValue := FItems[LIndex].Pair.Value
  else
    AValue := Default(TValue);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TryAdd(constref AKey: TKey; constref AValue: TValue): Boolean;
var
  LHash: UInt32;
begin
  Result := FindBucketIndex(FItems, AKey, LHash) < 0;
  if Result then
    DoAdd(AKey, AValue);
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.AddOrSetValue(constref AKey: TKey; constref AValue: TValue);
var
  LIndex: SizeInt;
  LHash: UInt32;
begin
  LIndex := FindBucketIndex(FItems, AKey, LHash);

  if LIndex < 0 then
    DoAdd(AKey, AValue)
  else
    SetValue(FItems[LIndex].Pair.Value, AValue);
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.ContainsKey(constref AKey: TKey): Boolean;
var
  LIndex: SizeInt;
begin
  LIndex := FindBucketIndex(AKey);
  Result := LIndex >= 0;
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.ContainsValue(constref AValue: TValue): Boolean;
begin
  Result := ContainsValue(AValue, TEqualityComparer<TValue>.Default(THashFactory));
end;

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.ContainsValue(constref AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Boolean;
var
  i: SizeInt;
  LItem: PItem;
begin
  if Length(FItems) = 0 then
    Exit(False);

  for i := 0 to High(FItems) do
  begin
    LItem := @FItems[i];
    if (LItem.Hash and UInt32.GetSignMask) = 0 then
      Continue;

    if AEqualityComparer.Equals(AValue, LItem.Pair.Value) then
      Exit(True);
  end;
  Result := False;
end;

procedure TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.GetMemoryLayout(
  const AOnGetMemoryLayoutKeyPosition: TOnGetMemoryLayoutKeyPosition);
var
  i: SizeInt;
begin
  for i := 0 to High(FItems) do
    if (FItems[i].Hash and UInt32.GetSignMask) <> 0 then
      AOnGetMemoryLayoutKeyPosition(Self, i);
end;

{ TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TPairEnumerator }

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  Result := TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair;
end;

{ TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TValueEnumerator }

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Value;
end;

{ TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TPValueEnumerator }

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TPValueEnumerator.GetCurrent: PValue;
begin
  Result := @(TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Value);
end;

{ TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TKeyEnumerator }

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Key;
end;

{ TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TPKeyEnumerator }

function TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.TPKeyEnumerator.GetCurrent: PKey;
begin
  Result := @(TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Key);
end;

{ TOpenAddressingLP<DICTIONARY_CONSTRAINTS> }

procedure TOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.NotifyIndexChange(AFrom, ATo: SizeInt);
begin
end;

function TOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.DoRemove(AIndex: SizeInt;
  ACollectionNotification: TCollectionNotification): TValue;
var
  LItem: PItem;
  LPair: TPair<TKey, TValue>;
  LLengthMask: SizeInt;
  i, LIndex, LGapIndex: SizeInt;
  LHash, LBucket: UInt32;
begin
  LItem := @FItems[AIndex];
  LPair := LItem.Pair;

  // try fill gap
  LHash := LItem.Hash;
  LItem.Hash := 0; // prevents an infinite searching loop
  LLengthMask := Length(FItems) - 1;
  i := Succ(AIndex - (LHash and LLengthMask));
  LGapIndex := AIndex;
  repeat
    LIndex := TProbeSequence.Probe(i, LHash) and LLengthMask;
    LItem := @FItems[LIndex];

    // Empty position
    if (LItem.Hash and UInt32.GetSignMask) = 0 then
      Break; // breaking bad!

    LBucket := LItem.Hash and LLengthMask;
    if not InCircularRange(LGapIndex, LBucket, LIndex) then
    begin
      NotifyIndexChange(LIndex, LGapIndex);
      FItems[LGapIndex] := LItem^;
      LItem.Hash := 0; // new gap
      LGapIndex := LIndex;
    end;
    Inc(i);
  until false;

  LItem := @FItems[LGapIndex];
  LItem.Hash := 0;
  LItem.Pair := Default(TPair<TKey, TValue>);
  Dec(FItemsLength);

  Result := LPair.Value;
  PairNotify(LPair, ACollectionNotification);
end;

function TOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndex(constref AItems: TArray<TItem>;
  constref AKey: TKey; out AHash: UInt32): SizeInt;
var
  LItem: {TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.}_TItem; // for workaround Lazarus bug #25613
  LLengthMask: SizeInt;
  i, m: SizeInt;
  LHash: UInt32;
begin
  m := Length(AItems);
  LLengthMask := m - 1;

  LHash := FEqualityComparer.GetHashCode(AKey);

  i := 0;
  AHash := LHash or UInt32.GetSignMask;

  if m = 0 then
    Exit(-1);

  Result := AHash and LLengthMask;

  repeat
    LItem := _TItem(AItems[Result]);

    // Empty position
    if (LItem.Hash and UInt32.GetSignMask) = 0 then
      Exit(not Result); // insert!

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer.Equals(AKey, LItem.Pair.Key) then
        Exit;

    Inc(i);

    Result := TProbeSequence.Probe(i, AHash) and LLengthMask;

  until false;
end;

{ TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS> }

function TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>.Rehash(ASizePow2: SizeInt; AForce: Boolean): Boolean;
begin
  if inherited then
    FTombstonesCount := 0;
end;

function TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>.RealItemsLength: SizeInt;
begin
  Result := FItemsLength + FTombstonesCount
end;

procedure TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>.ClearTombstones;
begin
  Rehash(Length(FItems), True);
end;

procedure TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>.Clear;
begin
  FTombstonesCount := 0;
  inherited;
end;

function TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>.DoRemove(AIndex: SizeInt;
  ACollectionNotification: TCollectionNotification): TValue;
begin
  Result := inherited;

  FItems[AIndex].Hash := 1;
  Inc(FTombstonesCount);
end;

function TOpenAddressingTombstones<OPEN_ADDRESSING_CONSTRAINTS>.DoAdd(constref AKey: TKey;
  constref AValue: TValue): SizeInt;
var
  LHash: UInt32;
begin
  PrepareAddingItem;

  Result := FindBucketIndexOrTombstone(FItems, AKey, LHash);
  if Result >= 0 then
    raise EListError.CreateRes(@SDuplicatesNotAllowed);

  Result := not Result;
  // Can't ovverride because we lost info about old hash
  if FItems[Result].Hash <> 0 then
    Dec(FTombstonesCount);

  AddItem(FItems[Result], AKey, AValue, LHash);
end;

{ TOpenAddressingSH<OPEN_ADDRESSING_CONSTRAINTS> }

function TOpenAddressingSH<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndex(constref AItems: TArray<TItem>;
  constref AKey: TKey; out AHash: UInt32): SizeInt;
var
  LItem: {TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.}_TItem; // for workaround Lazarus bug #25613
  LLengthMask: SizeInt;
  i, m: SizeInt;
  LHash: UInt32;
begin
  m := Length(AItems);
  LLengthMask := m - 1;

  LHash := FEqualityComparer.GetHashCode(AKey);

  i := 0;
  AHash := LHash or UInt32.GetSignMask;

  if m = 0 then
    Exit(-1);

  Result := AHash and LLengthMask;

  repeat
    LItem := _TItem(AItems[Result]);
    // Empty position
    if LItem.Hash = 0 then
      Exit(not Result); // insert!

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer.Equals(AKey, LItem.Pair.Key) then
        Exit;

    Inc(i);

    Result := TProbeSequence.Probe(i, AHash) and LLengthMask;

  until false;
end;

function TOpenAddressingSH<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndexOrTombstone(constref AItems: TArray<TItem>;
  constref AKey: TKey; out AHash: UInt32): SizeInt;
var
  LItem: {TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.}_TItem; // for workaround Lazarus bug #25613
  LLengthMask: SizeInt;
  i, m: SizeInt;
  LHash: UInt32;
begin
  m := Length(AItems);
  LLengthMask := m - 1;

  LHash := FEqualityComparer.GetHashCode(AKey);

  i := 0;
  AHash := LHash or UInt32.GetSignMask;

  if m = 0 then
    Exit(-1);

  Result := AHash and LLengthMask;

  repeat
    LItem := _TItem(AItems[Result]);

    // Empty position or tombstone
    if LItem.Hash and UInt32.GetSignMask = 0 then
      Exit(not Result); // insert!

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer.Equals(AKey, LItem.Pair.Key) then
        Exit;

    Inc(i);

    Result := TProbeSequence.Probe(i, AHash) and LLengthMask;

  until false;
end;

{ TOpenAddressingQP<OPEN_ADDRESSING_CONSTRAINTS> }

procedure TOpenAddressingQP<OPEN_ADDRESSING_CONSTRAINTS>.UpdateItemsThreshold(ASize: SizeInt);
begin
  if ASize = $40000000 then
    FItemsThreshold := $40000001
  else
    begin
      FPrimaryNumberAsSizeApproximation := PrimaryNumbersJustLessThanPowerOfTwo[
        MultiplyDeBruijnBitPosition[UInt32(((ASize and -ASize) * $077CB531)) shr 27]];

      FItemsThreshold := Pred(Round(FPrimaryNumberAsSizeApproximation * FMaxLoadFactor));
    end;
end;

function TOpenAddressingQP<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndex(constref AItems: TArray<TItem>;
  constref AKey: TKey; out AHash: UInt32): SizeInt;
var
  LItem: {TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.}_TItem; // for workaround Lazarus bug #25613
  i: SizeInt;
  LHash: UInt32;
begin
  LHash := FEqualityComparer.GetHashCode(AKey);

  i := 0;
  AHash := LHash or UInt32.GetSignMask;

  if Length(AItems) = 0 then
    Exit(-1);

  for i := 0 to FPrimaryNumberAsSizeApproximation - 1 do
  begin
    Result := TProbeSequence.Probe(i, AHash) mod FPrimaryNumberAsSizeApproximation;
    LItem := _TItem(AItems[Result]);

    // Empty position
    if LItem.Hash = 0 then
      Exit(not Result); // insert!

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer.Equals(AKey, LItem.Pair.Key) then
        Exit;
  end;

  Result := -1;
end;


function TOpenAddressingQP<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndexOrTombstone(constref AItems: TArray<TItem>;
  constref AKey: TKey; out AHash: UInt32): SizeInt;
var
  LItem: {TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.}_TItem; // for workaround Lazarus bug #25613
  i: SizeInt;
  LHash: UInt32;
begin
  LHash := FEqualityComparer.GetHashCode(AKey);

  i := 0;
  AHash := LHash or UInt32.GetSignMask;

  if Length(AItems) = 0 then
    Exit(-1);

  for i := 0 to FPrimaryNumberAsSizeApproximation - 1 do
  begin
    Result := TProbeSequence.Probe(i, AHash) mod FPrimaryNumberAsSizeApproximation;
    LItem := _TItem(AItems[Result]);

    // Empty position or tombstone
    if LItem.Hash and UInt32.GetSignMask = 0 then
      Exit(not Result); // insert!

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer.Equals(AKey, LItem.Pair.Key) then
        Exit;
  end;

  Result := -1;
end;

{ TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS> }

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACapacity: SizeInt;
  const AComparer: IEqualityComparer<TKey>);
begin
end;

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(const AComparer: IEqualityComparer<TKey>);
begin
end;

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>;
  const AComparer: IEqualityComparer<TKey>);
begin
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>;
  const AComparer: IEqualityComparer<TKey>);
begin
end;
{$ENDIF}

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACapacity: SizeInt);
begin
  Create(ACapacity, TExtendedEqualityComparer<TKey>.Default(THashFactory));
end;

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>);
begin
  Create(ACollection, TExtendedEqualityComparer<TKey>.Default(THashFactory));
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>);
begin
  Create(ACollection, TExtendedEqualityComparer<TKey>.Default(THashFactory));
end;
{$ENDIF}

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACapacity: SizeInt;
  const AComparer: IExtendedEqualityComparer<TKey>);
begin
  FMaxLoadFactor := TProbeSequence.DEFAULT_LOAD_FACTOR;
  FEqualityComparer := AComparer;
  SetCapacity(ACapacity);
end;

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(const AComparer: IExtendedEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>;
  const AComparer: IExtendedEqualityComparer<TKey>);
var
  LItem: TDictionaryPair;
begin
  Create(AComparer);
  for LItem in ACollection do
    Add(LItem);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>;
  const AComparer: IExtendedEqualityComparer<TKey>);
var
  LItem: PDictionaryPair;
begin
  Create(AComparer);
  for LItem in ACollection.Ptr^ do
    Add(LItem^);
end;
{$ENDIF}

procedure TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.UpdateItemsThreshold(ASize: SizeInt);
begin
  inherited;
  R :=
    PrimaryNumbersJustLessThanPowerOfTwo[
      MultiplyDeBruijnBitPosition[UInt32(((ASize and -ASize) * $077CB531)) shr 27]]
end;

function TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndex(constref AItems: TArray<TItem>;
  constref AKey: TKey; out AHash: UInt32): SizeInt;
var
  LItem: {TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.}_TItem; // for workaround Lazarus bug #25613
  LLengthMask: SizeInt;
  i, m: SizeInt;
  LHash: array[-1..1] of UInt32;
  LHash1: UInt32 absolute LHash[0];
  LHash2: UInt32 absolute LHash[1];
begin
  m := Length(AItems);
  LLengthMask := m - 1;
  LHash[-1] := 2; // number of hashes

  IExtendedEqualityComparer<TKey>(FEqualityComparer).GetHashList(AKey, @LHash[-1]);

  i := 0;
  AHash := LHash1 or UInt32.GetSignMask;

  if m = 0 then
    Exit(-1);

  Result := LHash1 and LLengthMask;
  // second hash function must be special
  LHash2 := (R - (LHash2 mod R)) or 1;

  repeat
    LItem := _TItem(AItems[Result]);

    // Empty position
    if LItem.Hash = 0 then
      Exit(not Result);

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer.Equals(AKey, LItem.Pair.Key) then
        Exit;

    Inc(i);

    Result := TProbeSequence.Probe(i, AHash, LHash2) and LLengthMask;
  until false;
end;

function TOpenAddressingDH<OPEN_ADDRESSING_CONSTRAINTS>.FindBucketIndexOrTombstone(constref AItems: TArray<TItem>;
  constref AKey: TKey; out AHash: UInt32): SizeInt;
var
  LItem: {TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>.}_TItem; // for workaround Lazarus bug #25613
  LLengthMask: SizeInt;
  i, m: SizeInt;
  LHash: array[-1..1] of UInt32;
  LHash1: UInt32 absolute LHash[0];
  LHash2: UInt32 absolute LHash[1];
begin
  m := Length(AItems);
  LLengthMask := m - 1;
  LHash[-1] := 2; // number of hashes

  IExtendedEqualityComparer<TKey>(FEqualityComparer).GetHashList(AKey, @LHash[-1]);

  i := 0;
  AHash := LHash1 or UInt32.GetSignMask;

  if m = 0 then
    Exit(-1);

  Result := LHash1 and LLengthMask;
  // second hash function must be special
  LHash2 := (R - (LHash2 mod R)) or 1;

  repeat
    LItem := _TItem(AItems[Result]);

    // Empty position or tombstone
    if LItem.Hash and UInt32.GetSignMask = 0 then
      Exit(not Result);

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer.Equals(AKey, LItem.Pair.Key) then
        Exit;

    Inc(i);

    Result := TProbeSequence.Probe(i, AHash, LHash2) and LLengthMask;
  until false;
end;

{ TDeamortizedDArrayCuckooMapEnumerator<T, CUCKOO_CONSTRAINTS> }

constructor TDeamortizedDArrayCuckooMapEnumerator<T, CUCKOO_CONSTRAINTS>.Create(
  ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
begin
  inherited;
  if ADictionary.Count = 0 then
    FMainIndex := TCuckooCfg.D
  else
    FMainIndex := 0;
end;

function TDeamortizedDArrayCuckooMapEnumerator<T, CUCKOO_CONSTRAINTS>.DoMoveNext: Boolean;
var
  LLength: SizeInt;
  LArray: TItemsArray;
begin
  Inc(FIndex);

  if (FMainIndex = TCuckooCfg.D) then // queue
  begin
    LLength := Length(TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FQueue.FItems);
    if FIndex >= LLength then
      Exit(False);

    while ((TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FQueue.FItems[FIndex].Hash)
      and UInt32.GetSignMask) = 0 do
    begin
      Inc(FIndex);
      if FIndex = LLength then
        Exit(False);
    end;
  end
  else // d-array
  begin
    LArray := TItemsArray(TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FItems[FMainIndex]);
    LLength := Length(LArray);
    if FIndex >= LLength then
    begin
      Inc(FMainIndex);
      FIndex := -1;
      Exit(DoMoveNext);
    end;

    while ((LArray[FIndex].Hash) and UInt32.GetSignMask) = 0 do
    begin
      Inc(FIndex);
      if FIndex = LLength then
      begin
        Inc(FMainIndex);
        FIndex := -1;
        Exit(DoMoveNext);
      end;
    end;
  end;

  Result := True;
end;

{ TDeamortizedDArrayPointersEnumerator<TCuckooCfg, TItemsArray, TItemsDArray, TQueueDictionary, PDictionaryPair> }

function TDeamortizedDArrayPointersEnumerator<TCuckooCfg, TItemsArray, TItemsDArray, TQueueDictionary, PDictionaryPair>.DoMoveNext: boolean;
var
  LLength: SizeInt;
  LArray: TItemsArray;
begin
  Inc(FIndex);

  if (FMainIndex = TCuckooCfg.D) then // queue
  begin
    LLength := Length(FQueue.FItems);
    if FIndex >= LLength then
      Exit(False);

    while ((FQueue.FItems[FIndex].Hash)
      and UInt32.GetSignMask) = 0 do
    begin
      Inc(FIndex);
      if FIndex = LLength then
        Exit(False);
    end;
  end
  else // d-array
  begin
    LArray := FItems^[FMainIndex];
    LLength := Length(LArray);
    if FIndex >= LLength then
    begin
      Inc(FMainIndex);
      FIndex := -1;
      Exit(DoMoveNext);
    end;

    while (((LArray[FIndex]).Hash) and UInt32.GetSignMask) = 0 do
    begin
      Inc(FIndex);
      if FIndex = LLength then
      begin
        Inc(FMainIndex);
        FIndex := -1;
        Exit(DoMoveNext);
      end;
    end;
  end;

  Result := True;
end;

function TDeamortizedDArrayPointersEnumerator<TCuckooCfg, TItemsArray, TItemsDArray, TQueueDictionary, PDictionaryPair>.DoGetCurrent: PDictionaryPair;
begin
  Result := GetCurrent;
end;

function TDeamortizedDArrayPointersEnumerator<TCuckooCfg, TItemsArray, TItemsDArray, TQueueDictionary, PDictionaryPair>.GetCurrent: PDictionaryPair;
begin
  if FMainIndex = TCuckooCfg.D then
    Result := @(FQueue.FItems[FIndex].Pair.Value.Pair)
  else
    Result := @((FItems^[FMainIndex])[FIndex].Pair);
end;

constructor TDeamortizedDArrayPointersEnumerator<TCuckooCfg, TItemsArray, TItemsDArray, TQueueDictionary, PDictionaryPair>.Create(var AItems; AQueue: TQueueDictionary; ACount: SizeInt);
begin
  FIndex := -1;
  if ACount = 0 then
    FMainIndex := TCuckooCfg.D
  else
    FMainIndex := 0;
  FQueue := AQueue;
  FItems := @AItems;
end;

{ TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItem, TQueueDictionary, PDictionaryPair> }

function TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItemsDArray, TQueueDictionary, PDictionaryPair>.Items: PArray;
begin
  Result := PArray(@((@Self)^));
end;

function TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItemsDArray, TQueueDictionary, PDictionaryPair>.GetCount: SizeInt;
begin
  Result := SizeInt((@PByte(@((@Self)^))[-SizeOf(SizeInt)])^);
end;

function TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItemsDArray, TQueueDictionary, PDictionaryPair>.GetQueue: TQueueDictionary;
begin
  Result := TQueueDictionary((@PByte(@((@Self)^))[SizeOf(TItemsDArray)])^);
end;

function TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItemsDArray, TQueueDictionary, PDictionaryPair>.GetEnumerator: TPointersEnumerator;
begin
  Result := TPointersEnumerator(TPointersEnumerator.NewInstance);
  TPointersEnumerator(Result).Create(Items^, GetQueue, GetCount);
end;

function TDeamortizedDArrayPointersCollection<TPointersEnumerator, TItemsDArray, TQueueDictionary, PDictionaryPair>.ToArray: TArray<PDictionaryPair>;
{begin
  Result := ToArrayImpl(FList.Count);
end;}
var
  i: SizeInt;
  LEnumerator: TPointersEnumerator;
begin
  SetLength(Result, GetCount);

  try
    LEnumerator := GetEnumerator;

    i := 0;
    while LEnumerator.MoveNext do
    begin
      Result[i] := LEnumerator.Current;
      Inc(i);
    end;
  finally
    LEnumerator.Free;
  end;
end;

{ TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS> }

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.Rehash(ASizePow2: SizeInt;
  AForce: boolean): Boolean;
var
  FOldIdx: array of TKey;
  i: SizeInt;
begin
  SetLength(FOldIdx, FIdx.Count);
  for i := 0 to FIdx.Count - 1 do
    FOldIdx[i] := FItems[FIdx[i]].Pair.Key;

  Result := inherited Rehash(ASizePow2, AForce);

  for i := 0 to FIdx.Count - 1 do
    FIdx[i] := FindBucketIndex(FOldIdx[i]);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.NotifyIndexChange(AFrom, ATo: SizeInt);
var
  i: SizeInt;
begin
  // notify change position
  for i := 0 to FIdx.Count-1 do
    if FIdx[i] = AFrom then
    begin
      FIdx[i] := ATo;
      Exit;
    end;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.InsertIntoBack(AItem: Pointer);
//var
//  LItem: TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.PItem; absolute AItem; !!! bug #25917
var
  LItem: TQueueDictionary.PValue absolute AItem;
  LIndex: SizeInt;
begin
  LIndex := DoAdd(LItem.Pair.Key, LItem^);
  FIdx.Insert(0, LIndex);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.InsertIntoHead(AItem: Pointer);
//var
//  LItem: TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.PItem absolute AItem; !!! bug #25917
var
  LItem: TQueueDictionary.PValue absolute AItem;
  LIndex: SizeInt;
begin
  LIndex := DoAdd(LItem.Pair.Key, LItem^);
  FIdx.Add(LIndex);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.IsEmpty: Boolean;
begin
  Result := FIdx.Count = 0;
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.Pop: Pointer;
var
  AIndex: SizeInt;
  //LResult: TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TItem; !!!bug #25917
begin
  AIndex := FIdx.DoRemove(FIdx.Count - 1, cnExtracted);

  Result := New(TQueueDictionary.PValue);
  TQueueDictionary.PValue(Result)^ := DoRemove(AIndex, cnExtracted);
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.Create(ACapacity: SizeInt;
  const AComparer: IEqualityComparer<TKey>);
begin
  FIdx := TList<UInt32>.Create;
  inherited Create(ACapacity, AComparer);
end;

destructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TQueueDictionary.Destroy;
begin
  FIdx.Free;
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetQueueCount: SizeInt;
begin
  Result := FQueue.Count;
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACapacity: SizeInt;
  const AComparer: IEqualityComparer<TKey>);
begin
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(const AComparer: IEqualityComparer<TKey>);
begin
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>;
  const AComparer: IEqualityComparer<TKey>);
begin
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>;
  const AComparer: IEqualityComparer<TKey>);
begin
end;
{$ENDIF}

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create;
begin
  Create(0);
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACapacity: SizeInt);
begin
  Create(ACapacity, TExtendedEqualityComparer<TKey>.Default(THashFactory));
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>);
begin
  Create(ACollection, TExtendedEqualityComparer<TKey>.Default(THashFactory));
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>);
begin
  Create(ACollection, TExtendedEqualityComparer<TKey>.Default(THashFactory));
end;
{$ENDIF}

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACapacity: SizeInt;
  const AComparer: IExtendedEqualityComparer<TKey>);
begin
  FMaxLoadFactor := TCuckooCfg.MAX_LOAD_FACTOR;
  FQueue := TQueueDictionary.Create;
  FCDM   := TCDM.Create;

  // to do - check constraint consts

  if TCuckooCfg.D > THashFactory.MAX_HASHLIST_COUNT then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  // should be moved to class constructor, but bug #24848
  CUCKOO_SIGN := UInt32.GetSizedSignMask(THashFactory.HASH_FUNCTIONS_MASK_SIZE + 1);
  CUCKOO_INDEX_SIZE := UInt32.GetBitsLength - (THashFactory.HASH_FUNCTIONS_MASK_SIZE + 1);
  CUCKOO_HASH_SIGN := THashFactory.HASH_FUNCTIONS_MASK shl CUCKOO_INDEX_SIZE;

  FEqualityComparer := AComparer;
  SetCapacity(ACapacity);
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(const AComparer: IExtendedEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>;
  const AComparer: IExtendedEqualityComparer<TKey>);
var
  LItem: TDictionaryPair;
begin
  Create(AComparer);
  for LItem in ACollection do
    Add(LItem);
end;

{$IFDEF ENABLE_METHODS_WITH_TEnumerableWithPointers}
constructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(ACollection: TEnumerableWithPointers<TDictionaryPair>;
  const AComparer: IExtendedEqualityComparer<TKey>);
var
  LItem: PDictionaryPair;
begin
  Create(AComparer);
  for LItem in ACollection.Ptr^ do
    Add(LItem^);
end;
{$ENDIF}

destructor TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Destroy;
begin
  inherited;
  FQueue.Free;
  FCDM.Free;
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetKeys: TKeyCollection;
begin
  if not Assigned(FKeys) then
    FKeys := TKeyCollection.Create(Self);
  Result := TKeyCollection(FKeys);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetValues: TValueCollection;
begin
  if not Assigned(FValues) then
    FValues := TValueCollection.Create(Self);
  Result := TValueCollection(FValues);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetPointers: PPointersCollection;
begin
  Result := PPointersCollection(@FItems);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Lookup(constref AKey: TKey;
  var AHashListOrIndex: PUInt32): SizeInt;
begin
  Result := Lookup(FItems, AKey, AHashListOrIndex);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Lookup(constref AItems: TItemsDArray; constref AKey: TKey;
  var AHashListOrIndex: PUInt32): SizeInt;
var
  LLengthMask: SizeInt;
  i, j, k: SizeInt;
  AHashList: PUInt32 absolute AHashListOrIndex;
  AHashListParams: PUInt16 absolute AHashListOrIndex;
  AIndex: PtrInt absolute AHashListOrIndex;
  // LBloomFilter: UInt32; // to rethink. now is useless
begin
  if Length(AItems[0]) = 0 then
    Exit(LR_NIL);

  LLengthMask := Length(AItems[0]) - 1;
  AHashListParams[0] := TCuckooCfg.D; // number of hashes

  i := 1; // ineks iteracji iteracji haszy
  k := 1; // indeks iteracji haszy
  // LBloomFilter := 0;
  repeat
    AHashListParams[1] := i; // iteration
    IExtendedEqualityComparer<TKey>(FEqualityComparer).GetHashList(AKey, AHashList);
    for j := 0 to THashFactory.HASHLIST_COUNT_PER_FUNCTION[i] - 1 do
    begin
      AHashList[k] := AHashList[k] or CUCKOO_SIGN;
      // LBloomFilter := LBloomFilter or AHashList[k];

      with AItems[k-1][AHashList[k] and LLengthMask] do
        if (Hash and UInt32.GetSignMask) <> 0 then
          if (AHashList[k] = Hash or CUCKOO_SIGN) and FEqualityComparer.Equals(AKey, Pair.Key) then
            Exit(k-1);

      Inc(k);
    end;
    Inc(i);
  until k > TCuckooCfg.D;

  i := FQueue.FindBucketIndex(AKey);
  if i >= 0 then
  begin
    AIndex := i;
    Exit(LR_QUEUE);
  end;

{  LBloomFilter := not LBloomFilter;
  for i := 0 to FDicQueueList.Count - 1 do
    // with FQueue[i] do
    if LBloomFilter and FQueue[i].Hash = 0 then
      for j := 1 to TCuckooCfg.D do
        if (FQueue[i].Hash or CUCKOO_SIGN = AHashList[j]) then
          if FEqualityComparer.Equals(AKey, FQueue[i].Pair.Key) then
          begin
            AIndex := i;
            Exit(LR_QUEUE);
          end;     }

  Result := LR_NIL;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.PrepareAddingItem;
var
  i: SizeInt;
begin
  if FItemsLength > FItemsThreshold then
    Rehash(Length(FItems[0]) shl 1)
  else if FItemsThreshold = 0 then
  begin
    for i := 0 to TCuckooCfg.D - 1 do
      SetLength(FItems[i], 4);
    UpdateItemsThreshold(4);
  end
  else if FItemsLength = $40000001 then // High(TIndex) ... Error: Type mismatch
    OutOfMemoryError;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.UpdateItemsThreshold(ASize: SizeInt);
var
  LLength: SizeInt;
begin
  LLength := ASize*TCuckooCfg.D;
  if LLength = $40000000 then
    FItemsThreshold := $40000001
  else
    FItemsThreshold := Pred(Round(LLength * FMaxLoadFactor));
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.AddItem(constref AItems: TItemsDArray; constref AKey: TKey;
  constref AValue: TValue; const AHashList: PUInt32);
var
  LNewItem: TItem;
  LPNewItem: PItem;
  y: boolean = false;
  b: UInt32;
  LIndex: UInt32;
  i, LLengthMask: SizeInt;
  LTempItem: TItem;
  LHashList: array[0..1] of UInt32;
  LHashListParams: array[0..3] of UInt16 absolute LHashList;
begin
  LLengthMask := Length(AItems[0]) - 1;

  LNewItem.Pair.Key := AKey;
  LNewItem.Pair.Value := AValue;
  // by concept already sign bit is set
  LNewItem.Hash := ((not CUCKOO_HASH_SIGN) and AHashList[1]) or UInt32.GetSignMask; // start at array [0]
  FQueue.InsertIntoBack(@LNewItem);

  for i := 0 to TCuckooCfg.L - 1 do
  begin
    if not y then
      if FQueue.IsEmpty then
        Exit
      else
      begin
        LPNewItem := FQueue.Pop; // bug #25917 workaround
        LNewItem := LPNewItem^;
        Dispose(LPNewItem);
        b := (LNewItem.Hash and CUCKOO_HASH_SIGN) shr CUCKOO_INDEX_SIZE;
        y := true;
      end;
    LIndex := LNewItem.Hash and LLengthMask;
    if (AItems[b][LIndex].Hash and UInt32.GetSignMask) = 0 then // insert!
    begin
      AItems[b][LIndex] := LNewItem;
      FCDM.Clear;
      y := false;
    end
    else
    begin
      if FCDM.ContainsKey(LNewItem.Pair.Key) then // found second cycle
      begin
        FQueue.InsertIntoBack(@LNewItem);
        FCDM.Clear;
        y := false;
      end
      else
      begin
        LTempItem := AItems[b][LIndex];
        AItems[b][LIndex] := LNewItem;
        LNewItem.Hash := LNewItem.Hash or CUCKOO_SIGN;
        FCDM.AddOrSetValue(LNewItem.Pair.Key, EmptyRecord);

        LNewItem := LTempItem;
        b := b + 1;
        if b >= TCuckooCfg.D then
          b := 0;
        LHashListParams[0] := -Succ(b);
        IExtendedEqualityComparer<TKey>(FEqualityComparer).GetHashList(LNewItem.Pair.Key, @LHashList[0]);
        LNewItem.Hash := (LHashList[1] and not CUCKOO_SIGN) or (b shl CUCKOO_INDEX_SIZE) or UInt32.GetSignMask;
        //  y := True; // always true in this place
      end;
    end;
  end;
  if y then
    FQueue.InsertIntoHead(@LNewItem);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.DoAdd(const AKey: TKey; const AValue: TValue;
  const AHashList: PUInt32);
begin
  AddItem(FItems, AKey, AValue, AHashList);
  Inc(FItemsLength);
  KeyNotify(AKey, cnAdded);
  ValueNotify(AValue, cnAdded);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Add(constref AKey: TKey; constref AValue: TValue);
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
begin
  PrepareAddingItem;
  LHashListOrIndex := @LHashList[0];
  if Lookup(AKey, LHashListOrIndex) <> LR_NIL then
    raise EListError.CreateRes(@SDuplicatesNotAllowed);

  DoAdd(AKey, AValue, LHashListOrIndex);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Add(constref APair: TPair<TKey, TValue>);
begin
  Add(APair.Key, APair.Value);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.DoRemove(const AHashListOrIndex: PUInt32;
  ALookupResult: SizeInt; ACollectionNotification: TCollectionNotification): TValue;
var
  LItem: PItem;
  LIndex: UInt32;
  LQueueIndex: SizeInt absolute AHashListOrIndex;
  LPair: TPair<TKey, TValue>;
begin
  case ALookupResult of
    LR_QUEUE:
      LPair := FQueue.FItems[LQueueIndex].Pair.Value.Pair;
    LR_NIL:
      raise ERangeError.Create(SItemNotFound);
  else
    LIndex := AHashListOrIndex[ALookupResult + 1] and (Length(FItems[0]) - 1);
    LItem := @FItems[ALookupResult][LIndex];
    LItem.Hash := 0;
    LPair := LItem.Pair;
    LItem.Pair := Default(TPair<TKey, TValue>);
  end;

  Result := LPair.Value;
  Dec(FItemsLength);
  if ALookupResult = LR_QUEUE then
  begin
    FQueue.FIdx.Remove(LQueueIndex);
    FQueue.DoRemove(LQueueIndex, cnRemoved);
  end;

  FCDM.Remove(LPair.Key); // item can exist in CDM

  PairNotify(LPair, ACollectionNotification);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Remove(constref AKey: TKey);
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
  LLookupResult: SizeInt;
begin
  LHashListOrIndex := @LHashList[0];
  LLookupResult := Lookup(AKey, LHashListOrIndex);
  if LLookupResult = LR_NIL then
    Exit;

  DoRemove(LHashListOrIndex, LLookupResult, cnRemoved);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.ExtractPair(constref AKey: TKey): TPair<TKey, TValue>;
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
  LLookupResult: SizeInt;
begin
  LHashListOrIndex := @LHashList[0];
  LLookupResult := Lookup(AKey, LHashListOrIndex);
  if LLookupResult = LR_NIL then
    Exit(Default(TPair<TKey, TValue>));

  Result.Key := AKey;
  Result.Value := DoRemove(LHashListOrIndex, LLookupResult, cnExtracted);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Clear;
var
  LItem: PItem;
  i, j: SizeInt;
  LOldItems: TItemsDArray;
  LOldQueueItems: TQueueDictionary.TItemsArray;
  LQueueItem: TQueueDictionary._TItem;
begin
  FItemsLength := 0;
  FItemsThreshold := 0;
  LOldItems := FItems;
  for i := 0 to TCuckooCfg.D - 1 do
    FItems[i] := nil;

  for i := 0 to TCuckooCfg.D - 1 do
  begin
    for j := 0 to High(LOldItems[0]) do
    begin
      LItem := @LOldItems[i][j];
      if (LItem.Hash and UInt32.GetSignMask <> 0) then
        PairNotify(LItem.Pair, cnRemoved);
    end;
  end;

  FCDM.Clear;

  // queue
  FQueue.FItemsLength := 0;
  FQueue.FItemsThreshold := 0;
  LOldQueueItems := FQueue.FItems;
  FQueue.FItems := nil;

  for i := 0 to High(LOldQueueItems) do
  begin
    LQueueItem := TQueueDictionary._TItem(LOldQueueItems[i]);
    if (LQueueItem.Hash and UInt32.GetSignMask = 0) then
      Continue;

    PairNotify(LQueueItem.Pair.Value.Pair, cnRemoved);
  end;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Rehash(ASizePow2: SizeInt);
var
  LNewItems: TItemsDArray;
  i, j: SizeInt;
  LItem: PItem;
  LOldQueue: TQueueDictionary;
var
  LHashList: array[0..1] of UInt32;
  LHashListParams: array[0..3] of Int16 absolute LHashList;
begin
  if ASizePow2 = Length(FItems[0]) then
    Exit;
  if ASizePow2 < 0 then
    OutOfMemoryError;

  for i := 0 to TCuckooCfg.D - 1 do
    SetLength(LNewItems[i], ASizePow2);

  LHashListParams[0] := -1;

  // opportunity to clear the queue
  LOldQueue := FQueue;
  FCDM.Clear;
  FQueue := TQueueDictionary.Create;
  for i := 0 to LOldQueue.FIdx.Count - 1 do
  begin
   LItem := @LOldQueue.FItems[LOldQueue.FIdx[i]].Pair.Value;
   LHashList[1] := FEqualityComparer.GetHashCode(LItem.Pair.Key);
   AddItem(LNewItems, LItem.Pair.Key, LItem.Pair.Value, @LHashList[0]);
  end;
  LOldQueue.Free;

  // copy the old elements
  for i := 0 to TCuckooCfg.D - 1 do
    for j := 0 to High(FItems[0]) do
    begin
      LItem := @FItems[i][j];
      if (LItem.Hash and UInt32.GetSignMask) = 0 then
        Continue;

      // small optimization. most of items exist in table 0
      if LItem.Hash and CUCKOO_HASH_SIGN = 0 then
      begin
        LHashList[1] := LItem.Hash;
        AddItem(LNewItems, LItem.Pair.Key, LItem.Pair.Value, @LHashList[0]);
      end
      else
      begin
        LHashList[1] := FEqualityComparer.GetHashCode(LItem.Pair.Key);
        AddItem(LNewItems, LItem.Pair.Key, LItem.Pair.Value, @LHashList[0]);
      end;
    end;

  FItems := LNewItems;
  UpdateItemsThreshold(ASizePow2);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.DoGetEnumerator: TEnumerator<TDictionaryPair>;
begin
  Result := GetEnumerator;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.SetCapacity(ACapacity: SizeInt);
begin
  if ACapacity < FItemsLength then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Resize(ACapacity);
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.SetMaxLoadFactor(AValue: single);
var
  LItemsLength: SizeInt;
begin
  if (AValue > TCuckooCfg.MAX_LOAD_FACTOR) or (AValue <= 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  FMaxLoadFactor := AValue;

  repeat
    LItemsLength := Length(FItems[0]);
    UpdateItemsThreshold(LItemsLength);
    if FItemsLength > FItemsThreshold then
      Rehash(LItemsLength shl 1);
  until FItemsLength <= FItemsThreshold;
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetLoadFactor: single;
begin
  Result := FItemsLength / (Length(FItems[0]) * TCuckooCfg.D);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetCapacity: SizeInt;
begin
  Result := Length(FItems[0]) * TCuckooCfg.D;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Resize(ANewSize: SizeInt);
var
  LNewSize: SizeInt;
begin
  if ANewSize < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LNewSize := 0;
  if ANewSize > 0 then
  begin
    LNewSize := 4;
    while LNewSize * TCuckooCfg.D < ANewSize do
      LNewSize := LNewSize shl 1;
  end;

  Rehash(LNewSize);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetItem(const AKey: TKey): TValue;
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
  LLookupResult: SizeInt;
  LIndex: UInt32;
begin
  LHashListOrIndex := @LHashList[0];
  LLookupResult := Lookup(AKey, LHashListOrIndex);

  case LLookupResult of
    LR_QUEUE:
      Result := FQueue.FItems[PtrInt(LHashListOrIndex)].Pair.Value.Pair.Value;
    LR_NIL:
      raise EListError.CreateRes(@SDictionaryKeyDoesNotExist);
  else
    LIndex := LHashListOrIndex[LLookupResult + 1] and (Length(FItems[0]) - 1);
    Result := FItems[LLookupResult][LIndex].Pair.Value;
  end;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TrimExcess;
begin
  SetCapacity(Succ(FItemsLength));
  FQueue.TrimExcess;
  FQueue.FIdx.TrimExcess;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.SetItem(constref AValue: TValue;
  const AHashListOrIndex: PUInt32; ALookupResult: SizeInt);
var
  LIndex: UInt32;
begin
  case ALookupResult of
    LR_QUEUE:
      SetValue(FQueue.FItems[PtrInt(AHashListOrIndex)].Pair.Value.Pair.Value, AValue);
    LR_NIL:
      raise EListError.CreateRes(@SItemNotFound);
  else
    LIndex := AHashListOrIndex[ALookupResult + 1] and (Length(FItems[0]) - 1);
    SetValue(FItems[ALookupResult][LIndex].Pair.Value, AValue);
  end;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.SetItem(const AKey: TKey; const AValue: TValue);
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
  LLookupResult: SizeInt;
begin
  LHashListOrIndex := @LHashList[0];
  LLookupResult := Lookup(AKey, LHashListOrIndex);

  SetItem(AValue, LHashListOrIndex, LLookupResult);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TryGetValue(constref AKey: TKey; out AValue: TValue): Boolean;
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
  LLookupResult: SizeInt;
  LIndex: UInt32;
begin
  LHashListOrIndex := @LHashList[0];
  LLookupResult := Lookup(AKey, LHashListOrIndex);

  Result := LLookupResult <> LR_NIL;

  case LLookupResult of
    LR_QUEUE:
      AValue := FQueue.FItems[PtrInt(LHashListOrIndex)].Pair.Value.Pair.Value;
    LR_NIL:
      AValue := Default(TValue);
  else
    LIndex := LHashListOrIndex[LLookupResult + 1] and (Length(FItems[0]) - 1);
    AValue := FItems[LLookupResult][LIndex].Pair.Value;
  end;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.AddOrSetValue(constref AKey: TKey; constref AValue: TValue);
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
  LLookupResult: SizeInt;
begin
  LHashListOrIndex := @LHashList[0];
  LLookupResult := Lookup(AKey, LHashListOrIndex);

  if LLookupResult = LR_NIL then
    Add(AKey, AValue)
    // more optimal version for AddOrSetValue has some bug : see Test_CuckooD2_Notification
    //begin
    //  PrepareAddingItem;
    //  DoAdd(AKey, AValue, LHashListOrIndex);
    //end
  else
    SetItem(AValue, LHashListOrIndex, LLookupResult);
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.ContainsKey(constref AKey: TKey): Boolean;
var
  LHashList: array[0..TCuckooCfg.D] of UInt32;
  LHashListOrIndex: PUint32;
begin
  LHashListOrIndex := @LHashList[0];
  Result := Lookup(AKey, LHashListOrIndex) <> LR_NIL;
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.ContainsValue(constref AValue: TValue): Boolean;
begin
  Result := ContainsValue(AValue, TEqualityComparer<TValue>.Default(THashFactory));
end;

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.ContainsValue(constref AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Boolean;
var
  i, j: SizeInt;
  LItem: PItem;
begin
  if Length(FItems[0]) = 0 then
    Exit(False);

  for i := 0 to TCuckooCfg.D - 1 do
    for j := 0 to High(FItems[0]) do
    begin
      LItem := @FItems[i][j];
      if (LItem.Hash and UInt32.GetSignMask) = 0 then
        Continue;

      if AEqualityComparer.Equals(AValue, LItem.Pair.Value) then
        Exit(True);
    end;
  Result := False;
end;

procedure TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.GetMemoryLayout(
  const AOnGetMemoryLayoutKeyPosition: TOnGetMemoryLayoutKeyPosition);
var
  i, j, k: SizeInt;
begin
  k := 0;
  for i := 0 to TCuckooCfg.D - 1 do
    for j := 0 to High(FItems[0]) do
    begin
      if FItems[i][j].Hash and UInt32.GetSignMask <> 0 then
        AOnGetMemoryLayoutKeyPosition(Self, k);
      inc(k);
    end;
end;

{ TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TPairEnumerator }

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  if FMainIndex = TCuckooCfg.D then
    Result := TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FQueue.FItems[FIndex].Pair.Value.Pair
  else
    Result := TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FItems[FMainIndex][FIndex].Pair;
end;

{ TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TValueEnumerator }

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TValueEnumerator.GetCurrent: TValue;
begin
  if FMainIndex = TCuckooCfg.D then
    Result := TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FQueue.FItems[FIndex].Pair.Value.Pair.Value
  else
    Result := TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FItems[FMainIndex][FIndex].Pair.Value;
end;

{ TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TPValueEnumerator }

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TPValueEnumerator.GetCurrent: PValue;
begin
  if FMainIndex = TCuckooCfg.D then
    Result := @(TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FQueue.FItems[FIndex].Pair.Value.Pair.Value)
  else
    Result := @(TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FItems[FMainIndex][FIndex].Pair.Value);
end;

{ TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TKeyEnumerator }

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FMainIndex = TCuckooCfg.D then
    Result := TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FQueue.FItems[FIndex].Pair.Value.Pair.Key
  else
    Result := TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FItems[FMainIndex][FIndex].Pair.Key;
end;

{ TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TPKeyEnumerator }

function TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.TPKeyEnumerator.GetCurrent: PKey;
begin
  if FMainIndex = TCuckooCfg.D then
    Result := @(TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FQueue.FItems[FIndex].Pair.Value.Pair.Key)
  else
    Result := @(TDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>(FDictionary).FItems[FMainIndex][FIndex].Pair.Key);
end;

{ TObjectDictionary<DICTIONARY_CONSTRAINTS> }

procedure TObjectDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.KeyNotify(
  constref AKey: TKey; ACollectionNotification: TCollectionNotification);
begin
  inherited;

  if (doOwnsKeys in FOwnerships) and (ACollectionNotification = cnRemoved) then
    TObject((@AKey)^).Free;
end;

procedure TObjectDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.ValueNotify(constref AValue: TValue;
  ACollectionNotification: TCollectionNotification);
begin
  inherited;

  if (doOwnsValues in FOwnerships) and (ACollectionNotification = cnRemoved) then
    TObject((@AValue)^).Free;
end;

constructor TObjectDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(
  AOwnerships: TDictionaryOwnerships);
begin
  Create(AOwnerships, 0);
end;

constructor TObjectDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(
  AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt);
begin
  inherited Create(ACapacity);

  FOwnerships := AOwnerships;
end;

constructor TObjectDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(
  AOwnerships: TDictionaryOwnerships; const AComparer: IExtendedEqualityComparer<TKey>);
begin
  inherited Create(AComparer);

  FOwnerships := AOwnerships;
end;

constructor TObjectDeamortizedDArrayCuckooMap<CUCKOO_CONSTRAINTS>.Create(
  AOwnerships: TDictionaryOwnerships; ACapacity: SizeInt; const AComparer: IExtendedEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);

  FOwnerships := AOwnerships;
end;

procedure TObjectOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.KeyNotify(
  constref AKey: TKey; ACollectionNotification: TCollectionNotification);
begin
  inherited;

  if (doOwnsKeys in FOwnerships) and (ACollectionNotification = cnRemoved) then
    TObject((@AKey)^).Free;
end;

procedure TObjectOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.ValueNotify(
  constref AValue: TValue; ACollectionNotification: TCollectionNotification);
begin
  inherited;

  if (doOwnsValues in FOwnerships) and (ACollectionNotification = cnRemoved) then
    TObject((@AValue)^).Free;
end;

constructor TObjectOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.Create(AOwnerships: TDictionaryOwnerships);
begin
  Create(AOwnerships, 0);
end;

constructor TObjectOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.Create(AOwnerships: TDictionaryOwnerships;
  ACapacity: SizeInt);
begin
  inherited Create(ACapacity);

  FOwnerships := AOwnerships;
end;

constructor TObjectOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.Create(AOwnerships: TDictionaryOwnerships;
  const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AComparer);

  FOwnerships := AOwnerships;
end;

constructor TObjectOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS>.Create(AOwnerships: TDictionaryOwnerships;
  ACapacity: SizeInt; const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);

  FOwnerships := AOwnerships;
end;
