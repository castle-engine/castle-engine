// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleRandom" -*-
{
  Copyright 2012-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleRandom unit. }
unit TestCastleRandom;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif} ;

type
  { Test CastleRandom unit. }
  TTestCastleRandom = class(TCastleTestCase)
  published
    procedure TestHash;
    procedure TestRandom;
  end;

implementation

uses CastleRandom;

procedure TTestCastleRandom.TestHash;
begin
  // Zero string hash
  AssertEquals({$ifdef FPC}%00000000000000000000000000000000{$else}0{$endif},
    StringToHash(''));
  // Short string hash
  AssertEquals({$ifdef FPC}%01001001001101000010111110101111{$else}$49342FAF{$endif},
    StringToHash('1'));
  // Zero seed hash
  AssertEquals({$ifdef FPC}%11100000010001101011001110011100{$else}$E046B39C{$endif},
    StringToHash('String to hash test'));
  // Seeded hash
  AssertEquals({$ifdef FPC}%01111101100111100000101011011100{$else}$7D9E0ADC{$endif},
    StringToHash('String to hash test', $9747b28c));
end;

procedure TTestCastleRandom.TestRandom;
const
  NTests = 1000000;
  SumTests = 1000000;
var
  Rnd: TCastleRandom;
  I: Integer;
  Sum: Double;
begin
  Rnd := TCastleRandom.Create;

  //test that random is in 0..1 limits
  for I := 0 to NTests do
    AssertTrue('Rnd.Random <= 1', Rnd.Random <= 1);
  for I := 0 to NTests do
    AssertTrue('Rnd.Random >= 0', Rnd.Random >= 0);
  for I := 0 to NTests do
    AssertTrue('Rnd.Random32bit > 0', Rnd.Random32bit > 0);

  //test random homogeneity
  {p.s. I'm not exactly sure if this is the right way to do, because random
   is random, and therefore there's always a tiny chance that it'll fail the test}
  Sum := 0;
  for I := 0 to SumTests do
    Sum := Sum + Rnd.Random;
  //checking random against shot noise (with extended margin, just in case)
  AssertTrue('Random is on average 0.5', Abs(Sum / (SumTests) - 0.5) <= 2 / Sqrt(SumTests));

  FreeAndNil(Rnd);
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCastleRandom);
{$endif}
end.
