unit CastleRandom;

interface

{$I castleconf.inc}
{$I norqcheckbegin.inc} // the whole unit should be used without overflow checking, for speed

type
  { Implementation of XorShift algorithm for random numbers generation. It works
    2 to 3 times faster than native FPC random function and provides for multiple
    repeatable random seeds to support parallel pseudo random sequences. }
  TCastleRandom = class(TObject)
  public
    { Automatically initializes the seed with given value, value 0 corresponds
      to random seed }
    constructor Create(RandomSeed: LongWord = 0);
    { Initializes current seed. The seed must be a non-zero integer. Provide Zero
      value to initialize random seed based on current time. }
    procedure Initialize(RandomSeed: LongWord = 0);
    { Returns folat random value in 0..1 range }
    function Random: single;
    { Returns random number in 0..N-1 range }
    function Random(N: LongInt): LongInt;
    { A relatively slow procedure to get a 64 bit integer random number. }
    function RandomInt64(N: int64): int64;
    { A simple Yes/No function that with 50% chance returns true or false.
      Something like throwing a coin... }
    function RandomBoolean: boolean;
    { Randomly provides "-1", "0" or "1" with equal chances }
    function RandomSign: longint;
    { Returns a random number in 0 .. 4294967295 range }
    function Random32bit: LongWord;
  private
    Seed: LongInt;
    procedure XorShiftCycle;
  end;

implementation

uses SysUtils; // required only for randomization based on "now" function

constructor TCastleRandom.Create(RandomSeed: LongWord);
begin
  Initialize(RandomSeed);
end;

procedure TCastleRandom.Initialize(RandomSeed: LongWord);
begin
  if RandomSeed = 0 then
  begin
    { I am not exacly sure how accurate such seed is? Should be 1/1000 of ms i.e.
      we should cover 2*MaxInt with 20 times excess. So we should have no "gaps"
      in initial Seed. But much can depend on specific implementation on different
      systems. I hope nothing can go wrong here. The interval of getting a new
      seed is ~50 microseconds, so it is practically impossible to accidently
      trigger equal seeds unless the program is run strictly on schedule.}
    Seed := LongInt(round(frac(now)*MaxInt*2));

    { Such approach is simpler and more reliable, but, it re-initializes
      SysUtils.Random function seed. I try to avoid that. It could be a surprise
      for a user that uses SysUtils and XorShift random in parallel and would
      get a hidden change of RandSeed }
    {Randomize;
    seed := RandSeed;}
  end
  else seed := LongInt(RandomSeed);
end;

procedure TCastleRandom.XorShiftCycle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  { such implementation works a tiny bit faster (+4%) due to better optimization
    by compiler (uses CPU registers instead of a variable) }
  seed := ((seed xor (seed shl 1)) xor ((seed xor (seed shl 1)) shr 15)) xor
         (((seed xor (seed shl 1)) xor ((seed xor (seed shl 1)) shr 15)) shl 4);
  {seed := seed xor (seed shl 1);
  seed := seed xor (seed shr 15);
  seed := seed xor (seed shl 4); }
end;

{ This procedure is slow so it is better to use XorShiftCycle + direct access
  to seed private field instead }
function TCastleRandom.Random32bit: LongWord; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  XorShiftCycle;
  result := LongWord(seed);
end;

function TCastleRandom.Random: single;
const divisor: single = 1/maxint;
begin
  XorShiftCycle;
  result := divisor*LongInt(seed shr 1);       // note: we discard 1 bit of accuracy to gain speed
  //result := divisor*longint(XorShift shr 1);    // works slower
end;

{result := LongWord((int64(seed)*N) shr 32)// := seed mod N; works slower
//result := longint((int64(xorshift)*N) shr 32) // works slower}

// Adding  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} makes this procedure
//  +35% effective. But I don't think it's a good idea
function TCastleRandom.Random(N: LongInt): LongInt;
begin
  XorShiftCycle;
  if N>1 then
    result := LongInt((int64(LongWord(seed))*N) shr 32)
  else
    result := 0
end;

{ Works much slower comparing to 32 bit version. And even slower than float version.
  Another problem is that it cycles the seed twice which might cause
  strange results if exact reproduction of the random sequence is required }
function TCastleRandom.RandomInt64(N: int64): int64;
begin
  // this line is copied from FPC system.inc
  result := int64((qword(Random32bit) or (qword(Random32bit) shl 32)) and $7fffffffffffffff);
  if N > 0 then
    result := result mod N
  else
    result := 0;
end;

function TCastleRandom.RandomBoolean: boolean;
begin
  XorShiftCycle;
  result := seed and %1 = 0   //can be %11 to provide for 1/4, %111 - 1/8 probability ...
end;

function TCastleRandom.RandomSign: longint;
begin
  XorShiftCycle;
  result := LongInt((int64(LongWord(seed))*3) shr 32)-1
end;

{$I norqcheckend.inc}

end.
