{%MainUnit castleutils.pas}
{
  Copyright 2002-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Naive TDictionary implementation to workaround CASTLE_WORKAROUND_DICTIONARY_FIND,
  TDictionary from Generics.Collections fails to find existing keys on
  FPC 3.2.3 on macOS / x86_64.

  Note: The code contains some ifdefs for FPC, just for the sake of possible
  future usage. They are pointless for now, as the entire code here
  is only used when CASTLE_WORKAROUND_DICTIONARY_FIND is defined,
  which is only when FPC is used. }

{$ifdef read_interface}

{$ifdef CASTLE_WORKAROUND_DICTIONARY_FIND}
type
  {$ifdef FPC}generic{$endif}
  TDictionary<TKey,TValue> = class
  public
    type
      TDictionaryPair = record
        Key: TKey;
        Value: TValue;
      end;

      TKeysDynamicArray = array of TKey;
      TValuesDynamicArray = array of TValue;

      TKeys = class
      strict private
        type
          TEnumerator = record
          strict private
            FList: TKeys;
            FPosition: Integer;
            function GetCurrent: TKey; inline;
          public
            constructor Create(const AList: TKeys);
            function MoveNext: Boolean; inline;
            property Current: TKey read GetCurrent;
          end;
      private
        Parent: TDictionary{$ifndef FPC}<TKey,TValue>{$endif};
      public
        function ToArray: TKeysDynamicArray;
        function GetEnumerator: TEnumerator;
      end;

      TValues = class
      strict private
        type
          TEnumerator = record
          strict private
            FList: TValues;
            FPosition: Integer;
            function GetCurrent: TValue; inline;
          public
            constructor Create(const AList: TValues);
            function MoveNext: Boolean; inline;
            property Current: TValue read GetCurrent;
          end;
      private
        Parent: TDictionary{$ifndef FPC}<TKey,TValue>{$endif};
      public
        function ToArray: TValuesDynamicArray;
        function GetEnumerator: TEnumerator;
      end;

  strict private
    type
      TEnumerator = record
      strict private
        FList: TDictionary{$ifndef FPC}<TKey,TValue>{$endif};
        FPosition: Integer;
        function GetCurrent: TDictionaryPair; inline;
      public
        constructor Create(const AList: TDictionary{$ifndef FPC}<TKey,TValue>{$endif});
        function MoveNext: Boolean; inline;
        property Current: TDictionaryPair read GetCurrent;
      end;

    var
      FItems: array of TDictionaryPair;
      FKeys: TKeys;
      FValues: TValues;

    function GetItems(const AKey: TKey): TValue;
    procedure SetItems(const AKey: TKey; const AValue: TValue);
  public
    constructor Create;
    destructor Destroy; override;

    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Add(const Key: TKey; const Value: TValue);
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function Count: Integer;
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function GetEnumerator: TEnumerator;

    property Items[const AKey: TKey]: TValue read GetItems write SetItems; default;
    property Keys: TKeys read FKeys;
    property Values: TValues read FValues;
  end;
{$endif CASTLE_WORKAROUND_DICTIONARY_FIND}

{$endif read_interface}

{$ifdef read_implementation}

{$ifdef CASTLE_WORKAROUND_DICTIONARY_FIND}

{ TDictionary<TKey,TValue>.TEnumerator ------------------------------------------------- }

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TEnumerator.GetCurrent: TDictionaryPair;
begin
  Result := FList.FItems[FPosition];
end;

constructor TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TEnumerator.Create(
  const AList: TDictionary{$ifndef FPC}<TKey,TValue>{$endif});
begin
//  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < Length(FList.FItems);
end;

{ TDictionary<TKey,TValue>.TKeys.TEnumerator ------------------------------------------------- }

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TKeys.TEnumerator.GetCurrent: TKey;
begin
  Result := FList.Parent.FItems[FPosition].Key;
end;

constructor TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TKeys.TEnumerator.Create(
  const AList: TKeys);
begin
//  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TKeys.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Parent.Count;
end;

{ TDictionary<TKey,TValue>.TValues.TEnumerator ------------------------------------------------- }

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TValues.TEnumerator.GetCurrent: TValue;
begin
  Result := FList.Parent.FItems[FPosition].Value;
end;

constructor TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TValues.TEnumerator.Create(
  const AList: TValues);
begin
//  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TValues.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Parent.Count;
end;

{ TDictionary<TKey,TValue>.TKeys ------------------------------------------------- }

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TKeys.ToArray: TKeysDynamicArray;
var
  I: Integer;
begin
  SetLength(Result, Parent.Count);
  for I := 0 to Parent.Count - 1 do
    Result[I] := Parent.FItems[I].Key;
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TKeys.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TDictionary<TKey,TValue>.TValues ------------------------------------------------- }

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TValues.ToArray: TValuesDynamicArray;
var
  I: Integer;
begin
  SetLength(Result, Parent.Count);
  for I := 0 to Parent.Count - 1 do
    Result[I] := Parent.FItems[I].Value;
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TValues.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TDictionary<TKey,TValue> --------------------------------------------------- }

constructor TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.Create;
begin
  inherited Create;
  FKeys := TKeys.Create;
  FKeys.Parent := Self;
  FValues := TValues.Create;
  FValues.Parent := Self;
end;

destructor TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.Destroy;
begin
  FreeAndNil(FKeys);
  FreeAndNil(FValues);
  inherited;
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.GetItems(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise EListError.CreateFmt('Dictionary key "%s" does not exist', [AKey]);
end;

procedure TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.SetItems(const AKey: TKey; const AValue: TValue);
begin
  // TDictionary from Generics.Collections also does here equivalent of Add, not AddOrSetValue.
  Add(AKey, AValue);
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if FItems[I].Key = Key then
    begin
      Value := FItems[I].Value;
      Exit(true);
    end;
  Result := false;
  Value := Default(TValue); // make sure to fill Value with zeroes, TTestGenericsCollections.TestMapTryGetValue
end;

procedure TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.Add(const Key: TKey; const Value: TValue);
var
  IgnoredValue: TValue;
begin
  if TryGetValue(Key, IgnoredValue) then
    raise EListError.CreateFmt('Dictionary key "%s" already exists', [Key]);

  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)].Key := Key;
  FItems[High(FItems)].Value := Value;
end;

procedure TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.AddOrSetValue(const Key: TKey; const Value: TValue);
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if FItems[I].Key = Key then
    begin
      FItems[I].Value := Value;
      Exit;
    end;
  Add(Key, Value);
end;

procedure TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.Remove(const Key: TKey);
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if FItems[I].Key = Key then
    begin
      System.Delete(FItems, I, 1);
      Exit;
    end;
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.Count: Integer;
begin
  Result := Length(FItems);
end;

procedure TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.Clear;
begin
  SetLength(FItems, 0);
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.ContainsKey(const Key: TKey): Boolean;
var
  IgnoredValue: TValue;
begin
  Result := TryGetValue(Key, IgnoredValue);
end;

function TDictionary{$ifndef FPC}<TKey,TValue>{$endif}.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$endif CASTLE_WORKAROUND_DICTIONARY_FIND}

{$endif read_implementation}
