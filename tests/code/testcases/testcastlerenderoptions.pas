// -*- compile-command: "./test_single_testcase.sh TTestCastleRenderOptions" -*-
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test for CastleRenderOptions unit. }
unit TestCastleRenderOptions;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleRenderOptions = class(TCastleTestCase)
  published
    procedure TestEquals;
  end;

implementation

uses CastleRenderOptions;

procedure TTestCastleRenderOptions.TestEquals;
var
  R1, R2: TCastleRenderOptions;
begin
  R1 := nil;
  R2 := nil;
  try
    R1 := TCastleRenderOptions.Create(nil);
    R2 := TCastleRenderOptions.Create(nil);

    AssertTrue(R1.Equals(R2));
    AssertTrue(R2.Equals(R2));
    AssertTrue(R1.Equals(R1)); // has to be always true, obviously
    AssertTrue(R2.Equals(R1)); // has to be always true, obviously

    R1.Blending := false;

    AssertFalse(R1.Equals(R2));
    AssertFalse(R2.Equals(R1));
    AssertTrue(R1.Equals(R1)); // has to be always true, obviously
    AssertTrue(R2.Equals(R2)); // has to be always true, obviously

    R2.Blending := false;

    AssertTrue(R1.Equals(R2));
    AssertTrue(R2.Equals(R2));
    AssertTrue(R1.Equals(R1)); // has to be always true, obviously
    AssertTrue(R2.Equals(R1)); // has to be always true, obviously

    R2.Blending := true;

    AssertFalse(R1.Equals(R2));
    AssertFalse(R2.Equals(R1));
    AssertTrue(R1.Equals(R1)); // has to be always true, obviously
    AssertTrue(R2.Equals(R2)); // has to be always true, obviously

    R1.Blending := true;

    AssertTrue(R1.Equals(R2));
    AssertTrue(R2.Equals(R2));
    AssertTrue(R1.Equals(R1)); // has to be always true, obviously
    AssertTrue(R2.Equals(R1)); // has to be always true, obviously

    R1.InternalColorChannels := R1.InternalColorChannels - [3];

    AssertFalse(R1.Equals(R2));
    AssertFalse(R2.Equals(R1));
    AssertTrue(R1.Equals(R1)); // has to be always true, obviously
    AssertTrue(R2.Equals(R2)); // has to be always true, obviously

    R1.InternalColorChannels := R1.InternalColorChannels + [3];

    AssertTrue(R1.Equals(R2));
    AssertTrue(R2.Equals(R2));
    AssertTrue(R1.Equals(R1)); // has to be always true, obviously
    AssertTrue(R2.Equals(R1)); // has to be always true, obviously
  finally
    FreeAndNil(R1);
    FreeAndNil(R2);
  end;
end;

initialization
  RegisterTest(TTestCastleRenderOptions);
end.
