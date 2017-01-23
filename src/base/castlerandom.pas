{
  Copyright 2016-2017 Eugene Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Custom random number generator (TCastleRandom). }
unit CastleRandom;

interface

{$I castleconf.inc}
{$I norqcheckbegin.inc} // the whole unit should be used without overflow checking, for speed

type
  { Custom, fast random number generator.
    Implementation of XorShift algorithm for random numbers generation.
    In some cases it works 2 to 3 times faster than native
    FPC random function. It also allows for multiple
    repeatable random seeds to support parallel pseudo-random sequences. }
  TCastleRandom = class(TObject)
  public
    { Create and initialize (seed) the random generator.
      Parameter RandomSeed value 0 indicates to use a random seed
      (derived from current time and some other paramteres). }
    constructor Create(RandomSeed: LongWord = 0);
    { Initializes current seed. The seed must be a non-zero integer.
      Provide Zero value to initialize random seed based on
      current time (CPU ticks) and some other paramteres.
      This procedure is thread-safe, you'll get different random seeds
      even if initialization happens absolutely simultaneously. }
    procedure Initialize(RandomSeed: LongWord = 0);
    { Returns random float value in the 0..1 range. }
    function Random: single;
    { Returns random integer number in the 0..N-1 range. }
    function Random(N: LongInt): LongInt;
    { A relatively slow procedure to get a 64 bit integer random number. }
    function RandomInt64(N: int64): int64;
    { A simple Yes/No function that with 50% chance returns true or false.
      Something like flipping a coin... }
    function RandomBoolean: boolean;
    { Randomly provides "-1", "0" or "1" with equal chances. }
    function RandomSign: longint;
    { Returns a random number in 1 .. High(LongWord) range. }
    function Random32bit: LongWord;
  private
    Seed: LongInt;
    procedure XorShiftCycle;
    function GetRandomSeed: LongInt;
  end;

implementation

uses
  { Required only for randomization based on "Now" function. }
  SysUtils,
  { Required for the GetTickCount64 function (not available in FPC 2.6.x).
    Note that the CastleTimeUtils.GetTickCount64 hides the SysUtils.GetTickCount64
    in FPC 3.x. }
  CastleTimeUtils;

constructor TCastleRandom.Create(RandomSeed: LongWord);
begin
  Initialize(RandomSeed);
end;

procedure TCastleRandom.Initialize(RandomSeed: LongWord);
begin
  if RandomSeed = 0 then
  begin
    Seed := GetRandomSeed;
  end
  else seed := LongInt(RandomSeed);
end;

//we're not using dev_urandom for now to support identical implementation for different OSes and devices
{$IFDEF UNIX}
//{$DEFINE USE_DEV_URANDOM}
{$ENDIF}

{$IFDEF USE_DEV_URANDOM}
function DEV_URANDOM: longint;
var dev_rnd: file of integer;
begin
  { algorithm according to http://wiki.freepascal.org/Dev_random
   /dev/urandom is a native *nix very high-quality random number generator.
   it's 1000 times slower than CastleRandom,
   but provides a perfect seed initialization. }
  AssignFile(dev_rnd, '/dev/urandom');
  reset(dev_rnd);
  repeat
    read(dev_rnd,result);
  until result<>0; // xorshift can't get 0 as a random seed so, we just read /dev/urandom until its not zero
  CloseFile(dev_rnd);
end;
{$ELSE}

{This procedure is relatively complex. However I was trying to solve
 a whole set of problems of random seeding. Including possible
 semi-simultaneous seeding requests by threads. On the other hand, there are
 more comments than the code itself :)
 I hope I've made everything right :) At least formal tests show it is so.}
var store_64bit_seed: QWord = 0; //this variable stores 64 bit seed for reusing
   wait_for_seed: boolean = false;
function Get_Randomseed: longint;
const date_multiplier: QWord = 30000000;  // approximate accuracy of the date
      date_order: QWord = 80000 * 30000000; // order of the "now*date_multiplier" variable
      {p.s. date_order will be true until year ~2119}
var c64: QWord; //current seed;
    b64: QWord; //additional seed for multi-threading safety
  procedure xorshift64; //{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} //we're using it too many times to inline
  begin
    c64:=c64 xor (c64 shl 12);
    c64:=c64 xor (c64 shr 25);
    c64:=c64 xor (c64 shl 27);
  end;
begin
  {We add an additional semi-random variable based on local c64 variable
   64-bit address. The only profit we have here is that this address will be
   different for different threads, therefore no 2 threads can be initialized
   with equal seed even if they are absolutely simultaneous}
  c64 := QWORD(@(c64));
  xorshift64;xorshift64;xorshift64;xorshift64;xorshift64;xorshift64;
  {I've made it 6 times, because sometimes values returned by
   xorshift algorithm are not too different,
   but we want them really independent for random seed initialization.
   So, multiple xorshift64 will take this "little" difference and eventually
   transform it into truly unpredictable number in 1..high(QWORD) range}
  while wait_for_seed do xorshift64; //do something nearly useful while randomization is buisy

  wait_for_seed := true;     //prevents another randomization to start until this one is finished

  b64 := c64;   //and use our another random seed based on current thread memory allocation
  {can this actually damage randomness in case of randomization happens only once
   as b64 will be constant then?}

  {basically we don't care if threads accidently will pass "wait_for_seed" lock
   because thanks to b64 still we shall get different random values. Just the
   algorithm would not be as optimal as it might be}

  if store_64bit_seed = 0 then begin //if this is the first randomization

    {This random seed initialization follows SysUtils random.
     Actually it is a relatively bad initialization for random numbers
     comparing to *nix /dev/urandom.
     It provides a limited amount of random numbers and it has a step of
     15 or 16 ms, so it's not continuous. Moreover it has just 5 mlns of
     possible values per 24 hours while xorshift32 supports for high(LongWord) -
     i.e. we get ~800 times less variants or 2400 times less
     as a "normal" user doesn't run computer for longer than 8 hours.
     And even less than that in case the player runs the game near the time
     the computer starts - just 200 thousands of combinations for 1 hour.

     On the other hand that's relatively enough for a computer game.

     Another, much more serious problem is that initializing 2 random generators
     semi-simultaneously will seed them with EQUAL seeds
     which we obviously don't want to.}

    {so let's start by getting tick count as SysUtils does}
    c64 := gettickcount64;
    {just to make sure it's not zero. It's not really important here.}
    if c64 = 0 then c64 := 2903758934725;

    {"Trying to solve the problem" we do the following:}

    {make a 64-bit xorshift cycle several times
     to kill any possible link to gettickcount64}
    xorshift64;xorshift64;xorshift64;xorshift64;xorshift64;xorshift64;
    {the same note on quantity of xorshift's as above}

    {now we have to make sure adding "now" won't overflow our c64 variable
     and add a few xorshift64-cycles just for fun in case it will.}
    while (c64 > high(QWord)-date_order) do xorshift64;

    {to kill a random discretness introduced by gettickcount64 we add "now".
     "now" and gettickcount64 are not independent and, in fact, change
     synchronously. But after several xorshift64-s c64 has no information
     left off gettickcount64 and therefore we introduce an additional
     semi-independent shift into the random seed}
    c64 += QWord(round(now*date_multiplier));
    {now we are sure that the player will get a different random seed even
     in case he/she launches the game exactly at the same milisecond since
     the OS startup - different date&time will shift the random seed...
     unless he/she deliberately sets the date&time&tick to some specific value}

    {and another 64-bit xorshift cycle to kill everything left off "now"}
    xorshift64;
  end else
    c64 := store_64bit_seed; //else - just grab the last seed.

  {Now we cycle xorshift64 as we have a decent random c64 variable}
  xorshift64;
  {and merge another random-variable based on current thread memory allocation}
  c64 := c64 xor b64;

  {and finally...}
  repeat
    {cycle everything one more time}
    xorshift64;
    {leave higher 32-bits of c64 as a true random seed}
    result := longint(c64 shr 32);
  until result<>0;
  {and strictly demand it's not zero!
   adding a few xorshift64-cycles in case it does.}

  {Eventually, store the final and truly random 64 bit seed for reusing}
  store_64bit_seed := c64;
  {and release the next thread to continue if any pending...}
  wait_for_seed := false;
end;
{$ENDIF}

function TCastleRandom.GetRandomSeed: longint;
begin
  {$IFDEF USE_DEV_URANDOM}
    { guarantees initialization with absolutely random number provided by
      native *nix algorithm. }
    result := DEV_URANDOM;
  {$ELSE}
    result := Get_Randomseed;
  {$ENDIF}
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

{ Works >10 times slower comparing to 32 bit version. And even slower than float version.
  Another problem is that it cycles the seed twice which might cause
  strange results if exact reproduction of the random sequence is required }
function TCastleRandom.RandomInt64(N: int64): int64;
var c64: QWord;
  procedure xorshift64; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  begin
    c64:=c64 xor (c64 shl 12);
    c64:=c64 xor (c64 shr 25);
    c64:=c64 xor (c64 shl 27);
  end;
begin
  {we need to do it even if N=0..1 to cycle 32bit random seed twice as expected}
  c64 := qword(Random32bit) or (qword(Random32bit) shl 32);
  if N > 1 then begin
    {adding a xorshift64 cycle guarantees us that c64 is truly random
     in range 1..high(QWORD)
     but slows down execution by ~10%}
    xorshift64;
    {in contrast to SysUtils we make it a true 64-bit random, not a fake 63 bit :)
     There can be no overflow here, because N is int64 and it can't be
     larger than (high(QWORD) div 2), i.e. we can never get "negative" result
     as the first bit of the result will be always zero}
    result := int64(qword(c64) mod qword(N))
  end
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
