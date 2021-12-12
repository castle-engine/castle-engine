program random_speed_test;

uses SysUtils, CastleRandom;

var
  Seed: LongInt = 233489679;

function Raw(N: LongInt): LongInt;
begin
  Seed := ((Seed xor (Seed shl 1)) xor ((Seed xor (Seed shl 1)) shr 15)) xor
         (((Seed xor (Seed shl 1)) xor ((Seed xor (Seed shl 1)) shr 15)) shl 4);
  if N>1 then
    Result := LongInt((Int64(LongWord(Seed))*N) shr 32)
  else
    Result := 0
end;

const
  NTests = 300000000;

var
  i: Integer;
  Rnd: TCastleRandom;
  Sum: Double;
  SumInt: Integer;
  BiasInt, BiasFloat: Integer;
  T: TDateTime;

begin
  Rnd := TCastleRandom.Create;
  try
    SumInt := 0;
    for i:= 1 to NTests do SumInt += 1;
    {Warm-up the CPU (if any power-saving mode is active)
     and skip program post-compillation lags, otherwise
     we might get incorrect results at first tests!}

    Sum := 0;
    T := Now;
    for i := 1 to NTests do
      Sum += 0.5;
    BiasFloat := Round((Now-T)*24*60*60*1000);
    SumInt := 0;
    T := Now;
    for i := 1 to NTests do
      SumInt += 1;
    biasInt := Round((Now-T)*24*60*60*1000);
    WriteLn('Bias ineger = ',BiasInt,' float = ',BiasFloat);

    Sum := 0;
    T := Now;
    for i := 1 to NTests do
      Sum += Random;
    WriteLn('SysUtils float random   t = ',Round((Now-T)*24*60*60*1000)-BiasFloat,' ms average = ',Sum/NTests);

    Sum := 0;
    T := Now;
    for i := 1 to NTests do
      Sum += Rnd.Random;
    WriteLn('Castle float random     t = ',Round((Now-T)*24*60*60*1000)-BiasFloat,' ms average = ',Sum/NTests);

    SumInt := 0;
    T := Now;
    for i := 1 to NTests do
      SumInt += Random(2);
    WriteLn('SysUtils integer random t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);

    SumInt := 0;
    T := Now;
    for i := 1 to NTests do
      SumInt += Rnd.Random(2);
    WriteLn('Castle integer random   t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);

    SumInt := 0;
    T := Now;
    for i := 1 to NTests do
      SumInt += Rnd.RandomInt64(2);
    WriteLn('Castle int64   random   t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);

    SumInt := 0;
    T := Now;
    for i := 1 to NTests do
      SumInt += Raw(2);
    WriteLn('Raw integer Xorshift    t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);


    SumInt := 0;
    T := Now;
    for i := 1 to NTests do if Rnd.Random(2)=0 then inc(SumInt);
    WriteLn('Integer boolean t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);
    SumInt := 0;
    T := Now;
    for i := 1 to NTests do if Rnd.RandomBoolean then inc(SumInt);
    WriteLn('Random boolean t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);

    SumInt := 0;
    T := Now;
    for i := 1 to NTests do SumInt += Rnd.Random(3)-1;
    WriteLn('Integer sign t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);
    SumInt := 0;
    T := Now;
    for i := 1 to NTests do SumInt += Rnd.RandomSign;
    WriteLn('Random sign t = ',Round((Now-T)*24*60*60*1000)-BiasInt,' ms average = ',SumInt/NTests);

  finally
    FreeAndNil(Rnd);
  end;
end.

