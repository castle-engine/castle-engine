{ Generate table of primes for IntegerNoise.
  Output Pascal code with $1 rows of primes table.
  In each row, each prime number will be randomly chosen from appropriate
  range (hardcoded in Ranges[] here).
}

uses SysUtils, CastleUtils;

function IsPrime(const Value: Cardinal): boolean;
var
  I: Cardinal;
begin
  for I := 2 to IntSqrt(Value) do
    if Value mod I = 0 then Exit(false);
  Result := true;
end;

function RandomPrime(const Min, Max: Cardinal): Cardinal;
begin
  Result := Min + Random(Max - Min);
  while not IsPrime(Result) do
  begin
    Inc(Result);
    if Result > Max then Result := Min;
  end;
end;

const
  { For 5 columns of Primes[] table in IntegerNoise, ranges from where
    to choose random prime number.
    Sizes of these consts just chosen to match the "good" numbers from
    Blender's source code: 1301 314159, 15731, 789221, 1376312589. }
  Ranges: array [0..4] of array [0..1] of Cardinal =
  (
    (       1000,        2000),
    (     200000,      800000),
    (      10000,       20000),
    (     500000,     5000000),
    ( 1000000000,  2000000000)
  );
var
  I, J: Cardinal;
begin
  Parameters.CheckHigh(1);
  for I := 1 to StrToInt(Parameters[1]) do
  begin
    Write('(');
    for J := 0 to 4 do
    begin
      Write(RandomPrime(Ranges[J, 0], Ranges[J, 1]));
      if J <> 4 then Write(', ');
    end;
    Writeln('),');
  end;
end.
