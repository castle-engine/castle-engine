{
  Copyright 2012-2018 Michalis Kamburelis.

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
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleTestCase;

type
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
  AssertEquals(%00000000000000000000000000000000, StringToHash(''));
  // Short string hash
  AssertEquals(%01001001001101000010111110101111, StringToHash('1'));
  // Zero seed hash
  AssertEquals(%11100000010001101011001110011100, StringToHash('String to hash test'));
  // Seeded hash
  AssertEquals(%01111101100111100000101011011100, StringToHash('String to hash test',$9747b28c));
end;

procedure TTestCastleRandom.TestRandom;
const
  NTests = 10000000;
var
  Rnd: TCastleRandom;
  i: Integer;
  Sum: Double;
begin
  Rnd := TCastleRandom.Create;

  //test that random is in 0..1 limits
  for i := 0 to NTests do
    AssertTrue('Rnd.Random <= 1', Rnd.Random <= 1);
  for i := 0 to NTests do
    AssertTrue('Rnd.Random > 0', Rnd.Random > 0);

  //test random homogeneity
  {p.s. I'm not exactly sure if this is the right way to do, because random
   is random, and therefore there's always a tiny chance that it'll fail the test}
  Sum := 0;
  for i := 0 to NTests*10 do
    Sum += Rnd.Random;
  //checking random against shot noise (with extended margin, just in case)
  AssertTrue('Random is on average 0.5', Abs(Sum/(NTests*10)-0.5) <= 2/Sqrt(NTests*10));

  FreeAndNil(Rnd);
end;


initialization
  RegisterTest(TTestCastleRandom);
end.
