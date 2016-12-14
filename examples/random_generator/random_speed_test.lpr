program random_speed_test;

uses SysUtils, CastleRandom;

{and test everything}

var
  seed:longint = 233489679;

function Raw(N: LongInt): LongInt;
begin
  seed := ((seed xor (seed shl 1)) xor ((seed xor (seed shl 1)) shr 15)) xor
         (((seed xor (seed shl 1)) xor ((seed xor (seed shl 1)) shr 15)) shl 4);
  if N>1 then
    result := LongInt((int64(LongWord(seed))*N) shr 32)
  else
    result := 0
end;

const
  N_of_tests = 300000000;

var
  i: integer;
  RND: TCastleRandom;
  sum: double;
  sumint: integer;
  BiasInt, BiasFloat: integer;
  T: TDateTime;

begin
  RND := TCastleRandom.create;
  try
    sumint := 0;
    for i:= 1 to N_of_tests do sumint += 1;
    {Warm-up the CPU (if any power-saving mode is active)
     and skip program post-compillation lags, otherwise
     we might get incorrect results at first tests!}

    sum := 0;
    T := now;
    for i := 1 to N_of_tests do
      sum += 0.5;
    biasFloat:=round((now-T)*24*60*60*1000);
    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do
      sumint += 1;
    biasInt:=round((now-T)*24*60*60*1000);
    writeln('bias ineger = ',biasInt,' float = ',biasFloat);

    sum := 0;
    T := now;
    for i := 1 to N_of_tests do
      sum += random;
    writeln('SysUtils float random   t = ',round((now-T)*24*60*60*1000)-BiasFloat,' ms average = ',sum/N_of_tests);

    sum := 0;
    T := now;
    for i := 1 to N_of_tests do
      sum += RND.random;
    writeln('Castle float random     t = ',round((now-T)*24*60*60*1000)-BiasFloat,' ms average = ',sum/N_of_tests);

    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do
      sumint += random(2);
    writeln('SysUtils integer random t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);

    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do
      sumint += RND.random(2);
    writeln('Castle integer random   t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);

    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do
      sumint += RND.RandomInt64(2);
    writeln('Castle int64   random   t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);

    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do
      sumint += raw(2);
    writeln('Raw integer Xorshift    t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);


    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do if RND.random(2)=0 then inc(sumint);
    writeln('Integer boolean t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);
    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do if RND.randomBoolean then inc(sumint);
    writeln('Random boolean t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);

    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do Sumint += RND.random(3)-1;
    writeln('Integer sign t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);
    sumint := 0;
    T := now;
    for i := 1 to N_of_tests do sumint += RND.randomSign;
    writeln('Random sign t = ',round((now-T)*24*60*60*1000)-BiasInt,' ms average = ',sumint/N_of_tests);

  finally
    FreeAndNil(RND);
  end;
end.

