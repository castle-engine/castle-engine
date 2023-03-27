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

 **********************************************************************}

unit Generics.MemoryExpanders;
// Memory expanders

{$mode delphi}
{$MACRO ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{.$WARN 5024 OFF}
{.$WARN 4079 OFF}

interface

uses
  Classes, SysUtils;

type
  TProbeSequence = class
  public
  end;

  { TLinearProbing }

  TLinearProbing = class(TProbeSequence)
  public
    class function Probe(I, Hash: UInt32): UInt32; static; inline;

    const MAX_LOAD_FACTOR = 1;
    const DEFAULT_LOAD_FACTOR = 0.75;
  end;

  { TQuadraticProbing }

  TQuadraticProbing = class(TProbeSequence)
  public
    class function Probe(I, Hash: UInt32): UInt32; static; inline;

    const MAX_LOAD_FACTOR = 0.5;
    const DEFAULT_LOAD_FACTOR = 0.5;
  end;

  { TDoubleHashing }

  TDoubleHashing = class(TProbeSequence)
  public
    class function Probe(I, Hash1: UInt32; Hash2: UInt32 = 1): UInt32; static; inline;

    const MAX_LOAD_FACTOR = 1;
    const DEFAULT_LOAD_FACTOR = 0.85;
  end;

const
  // http://stackoverflow.com/questions/757059/position-of-least-significant-bit-that-is-set
  // MultiplyDeBruijnBitPosition[uint32(((numberInt32 and -numberInt32) * $077CB531)) shr 27]
  MultiplyDeBruijnBitPosition: array[0..31] of Int32 =
  (
    0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
  );

  // http://primes.utm.edu/lists/2small/0bit.html
  // http://www.math.niu.edu/~rusin/known-math/98/pi_x
  // http://oeis.org/A014234/
  PrimaryNumbersJustLessThanPowerOfTwo: array[0..31] of UInt32 =
  (
    0, 1, 3, 7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521, 131071,
    262139, 524287, 1048573, 2097143, 4194301, 8388593, 16777213, 33554393, 67108859,
    134217689, 268435399, 536870909, 1073741789, 2147483647
  );

  // http://oeis.org/A014210
  // http://oeis.org/A203074
  PrimaryNumbersJustBiggerThanPowerOfTwo: array[0..31] of UInt32 = (
    2,3,5,11,17,37,67,131,257,521,1031,2053,4099,
    8209,16411,32771,65537,131101,262147,524309,
    1048583,2097169,4194319,8388617,16777259,33554467,
    67108879,134217757,268435459,536870923,1073741827,
    2147483659);

  // Fibonacci numbers
  FibonacciNumbers: array[0..44] of UInt32 = (
    {0,1,1,2,3,}0,5,8,13,21,34,55,89,144,233,377,610,987,
    1597,2584,4181,6765,10946,17711,28657,46368,75025,
    121393,196418,317811,514229,832040,1346269,
    2178309,3524578,5702887,9227465,14930352,24157817,
    39088169, 63245986, 102334155, 165580141, 267914296,
    433494437, 701408733, 1134903170, 1836311903, 2971215073,
    {! not fib number - this is memory limit} 4294967295);

  // Largest prime not exceeding Fibonacci(n)
  // http://oeis.org/A138184/list
  // http://www.numberempire.com/primenumbers.php
  PrimaryNumbersJustLessThanFibonacciNumbers: array[0..44] of UInt32 = (
    {! not correlated to fib number. For empty table} 0,
    5,7,13,19,31,53,89,139,233,373,607,983,1597,
    2579,4177,6763,10939,17707,28657,46351,75017,
    121379,196387,317797,514229,832003,1346249,
    2178283,3524569,5702867,9227443,14930341,24157811,
    39088157,63245971,102334123,165580123,267914279,
    433494437,701408717,1134903127,1836311879,2971215073,
    {! not correlated to fib number - this is prime memory limit} 4294967291);

  // Smallest prime >= n-th Fibonacci number.
  // http://oeis.org/A138185
  PrimaryNumbersJustBiggerThanFibonacciNumbers: array[0..44] of UInt32 = (
    {! not correlated to fib number. For empty table} 0,
    5,11,13,23,37,59,89,149,233,379,613,
    991,1597,2591,4201,6779,10949,17713,28657,46381,
    75029,121403,196429,317827,514229,832063,1346273,
    2178313,3524603,5702897,9227479,14930387,24157823,
    39088193,63245989,102334157,165580147,267914303,
    433494437,701408753,1134903179,1836311951,2971215073,
    {! not correlated to fib number - this is prime memory limit} 4294967291);

type

  { TCuckooHashingCfg }

  TCuckooHashingCfg = class
  public
    const D = 2;
    const MAX_LOAD_FACTOR = 0.5;

    class function LoadFactor(M: Integer): Integer; virtual;
  end;

  TStdCuckooHashingCfg = class(TCuckooHashingCfg)
  public
    const MAX_LOOP = 1000;
  end;

  TDeamortizedCuckooHashingCfg = class(TCuckooHashingCfg)
  public
    const L = 5;
  end;

  TDeamortizedCuckooHashingCfg_D2 = TDeamortizedCuckooHashingCfg;

  { TDeamortizedCuckooHashingCfg_D4 }

  TDeamortizedCuckooHashingCfg_D4 = class(TDeamortizedCuckooHashingCfg)
  public
    const D = 4;
    const L = 20;
    const MAX_LOAD_FACTOR = 0.9;

    class function LoadFactor(M: Integer): Integer; override;
  end;

  { TDeamortizedCuckooHashingCfg_D6 }

  TDeamortizedCuckooHashingCfg_D6 = class(TDeamortizedCuckooHashingCfg)
  public
    const D = 6;
    const L = 170;
    const MAX_LOAD_FACTOR = 0.99;

    class function LoadFactor(M: Integer): Integer; override;
  end;

  TL5CuckooHashingCfg = class(TCuckooHashingCfg)
  public
  end;

implementation

{ TDeamortizedCuckooHashingCfg_D6 }

class function TDeamortizedCuckooHashingCfg_D6.LoadFactor(M: Integer): Integer;
begin
  Result:=Pred(Round(MAX_LOAD_FACTOR*M));
end;

{ TDeamortizedCuckooHashingCfg_D4 }

class function TDeamortizedCuckooHashingCfg_D4.LoadFactor(M: Integer): Integer;
begin
  Result:=Pred(Round(MAX_LOAD_FACTOR*M));
end;

{ TCuckooHashingCfg }

class function TCuckooHashingCfg.LoadFactor(M: Integer): Integer;
begin
  Result := Pred(M shr 1);
end;

{ TLinearProbing }

class function TLinearProbing.Probe(I, Hash: UInt32): UInt32;
begin
  Result := (Hash + I)
end;

{ TQuadraticProbing }

class function TQuadraticProbing.Probe(I, Hash: UInt32): UInt32;
begin
  Result := (Hash + Sqr(I));
end;

{ TDoubleHashingNoMod }

class function TDoubleHashing.Probe(I, Hash1: UInt32; Hash2: UInt32): UInt32;
begin
  Result := Hash1 + I * Hash2;
end;

end.

