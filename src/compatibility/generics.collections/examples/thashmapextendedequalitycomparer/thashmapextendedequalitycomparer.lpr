// Generic types for FreeSparta.com and FreePascal!
// by Maciej Izak (hnb), 2014

program THashMapExtendedEqualityComparer;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections, Generics.Defaults;

type

  { TTaxPayer }

  TTaxPayer = record
    TaxID: Integer;
    Name: string;

    constructor Create(ATaxID: Integer; const AName: string);
    function ToString: string;
  end;

constructor TTaxPayer.Create(ATaxID: Integer; const AName: string);
begin
  TaxID := ATaxID;
  Name := AName;
end;

function TTaxPayer.ToString: string;
begin
  Result := Format('TaxID = %-10d Name = %-17s', [TaxID, Name]);
end;

function EqualityComparison(constref ALeft, ARight: TTaxPayer): Boolean;
begin
  Result := ALeft.TaxID = ARight.TaxID;
end;

procedure ExtendedHasher(constref AValue: TTaxPayer; AHashList: PUInt32);
begin
  // don't work with TCuckooD6 map because default TCuckooD6 needs TDelphiSixfoldHashFactory
  // and TDefaultHashFactory = TDelphiQuadrupleHashFactory
  // (TDelphiQuadrupleHashFactory is compatible with TDelphiDoubleHashFactory and TDelphiHashFactory)
  TDefaultHashFactory.GetHashList(@AValue.TaxID, SizeOf(Integer), AHashList);
end;

var
  map: THashMap<TTaxPayer, string>; // THashMap = TCuckooD4
  LTaxPayer: TTaxPayer;
  LSansa: TTaxPayer;
  LPair: TPair<TTaxPayer, string>;
begin
  WriteLn('program of tax office - ExtendedEqualityComparer for THashMap');
  WriteLn;

  // to identify the taxpayer need only nip
  map := THashMap<TTaxPayer, string>.Create(
    TExtendedEqualityComparer<TTaxPayer>.Construct(EqualityComparison, ExtendedHasher));

  map.Add(TTaxPayer.Create(1234567890, 'Joffrey Baratheon'), 'guilty');
  map.Add(TTaxPayer.Create(90, 'Little Finger'), 'swindler');
  map.Add(TTaxPayer.Create(667, 'John Snow'), 'delinquent tax');

  // useless in this place but we can convert Keys to TArray<TKey> :)
  WriteLn(Format('All taxpayers (count = %d)', [Length(map.Keys.ToArray)]));
  for LTaxPayer in map.Keys do
    WriteLn(' > ', LTaxPayer.ToString);

  LSansa := TTaxPayer.Create(667, 'Sansa Stark');

  // exist because custom EqualityComparison and ExtendedHasher
  WriteLn;
  WriteLn(LSansa.Name, ' exist in map = ',  map.ContainsKey(LSansa));
  WriteLn;

  //
  WriteLn('All taxpayers');
  for LPair in map do
    WriteLn(' > ', LPair.Key.ToString, ' is ', LPair.Value);

  // Add or set sansa? :)
  WriteLn;
  WriteLn(Format('AddOrSet(%s, ''innocent'')', [LSansa.ToString]));
  map.AddOrSetValue(LSansa, 'innocent');
  WriteLn;

  //
  WriteLn('All taxpayers');
  for LPair in map do
    WriteLn(' > ', LPair.Key.ToString, ' is ', LPair.Value);

  // Add or set sansa? :)
  WriteLn;
  LSansa.TaxID := 668;
  WriteLn(Format('AddOrSet(%s, ''innocent'')', [LSansa.ToString]));
  map.AddOrSetValue(LSansa, 'innocent');
  WriteLn;

  //
  WriteLn('All taxpayers');
  for LPair in map do
    WriteLn(' > ', LPair.Key.ToString, ' is ', LPair.Value);

  ReadLn;
  map.Free;
end.

