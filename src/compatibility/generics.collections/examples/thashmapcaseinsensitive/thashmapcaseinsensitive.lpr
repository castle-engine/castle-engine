// Generic types for FreeSparta.com and FreePascal!
// by Maciej Izak (hnb), 2014

program THashMapCaseInsensitive;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  Generics.Collections, Generics.Defaults;

var
  StringMap: THashMap<String, TEmptyRecord>;
  AnsiStringMap: THashMap<AnsiString, TEmptyRecord>;
  UnicodeStringMap: THashMap<UnicodeString, TEmptyRecord>;
  AdvancedHashMapWithBigLoadFactor: TCuckooD6<RawByteString, TEmptyRecord>;
  k: String;
begin
  WriteLn('Working with case insensitive THashMap');
  WriteLn;
  // example constructors for different string types
  StringMap := THashMap<String, TEmptyRecord>.Create(TIStringComparer.Ordinal);
  StringMap.Free;
  AnsiStringMap := THashMap<AnsiString, TEmptyRecord>.Create(TIAnsiStringComparer.Ordinal);
  AnsiStringMap.Free;
  UnicodeStringMap := THashMap<UnicodeString, TEmptyRecord>.Create(TIUnicodeStringComparer.Ordinal);
  UnicodeStringMap.Free;

  // standard TI*Comparer is dedicated for MAX_HASHLIST_COUNT = 4 and lower. For example DArrayCuckoo where D = 6
  // we need to create extra specialized TGIStringComparer type
  AdvancedHashMapWithBigLoadFactor := TCuckooD6<RawByteString, TEmptyRecord>.Create(
    TGIStringComparer<RawByteString, TDelphiSixfoldHashFactory>.Ordinal);
  AdvancedHashMapWithBigLoadFactor.Free;

  // ok lets start
  // another way to create case insensitive hash map
  StringMap := THashMap<String, TEmptyRecord>.Create(TGIStringComparer<String>.Ordinal);

  WriteLn('Add Cat and Dog');
  StringMap.Add('Cat', EmptyRecord);
  StringMap.Add('Dog', EmptyRecord);

  //
  WriteLn('Contains CAT = ', StringMap.ContainsKey('CAT'));
  WriteLn('Contains dOG = ', StringMap.ContainsKey('dOG'));
  WriteLn('Contains Fox = ', StringMap.ContainsKey('Fox'));

  WriteLn('Enumerate all keys :');
  for k in StringMap.Keys do
    WriteLn(' > ', k);

  ReadLn;
  StringMap.Free;
end.

