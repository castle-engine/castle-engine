// -*- compile-command: "./test_single_testcase.sh TTestCastleTimeUtils" -*-
{
  Copyright 2016-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleTimeUtils unit. }
unit TestCastleTimeUtils;

interface

uses CastleTester;

type
  TTestCastleTimeUtils = class(TCastleTestCase)
  published
    procedure TestGetTickCount64;
  end;

implementation

uses SysUtils, Classes, Math, CastleTimeUtils;

{$ifdef WASI}
{ WebAssembly will crash when we call standard Sleep.
  So we implement our own sleep below in a stupid way: just "busy waiting"
  until the time passes.

  DO NOT USE THIS IN YOUR OWN APPLICATIONS.

  This is used in this test just to waste some time and test CastleGetTickCount64.
  This is the purpose of "Sleep" in this application (both on web and non-web).

  It is not good to be used in real applications, because

  - This "Sleep" implementation, with "busy waiting",
    is uselessly consuming CPU time.
    The "busy waiting" is a bad way to sleep, consuming CPU time doing nothing.

  - Even the proper "Sleep" on non-web platforms is useless in real games.
    It hangs the process, doing nothing, which is something you should never
    do. Instead always finish what you want to do as quick as possible,
    and adjust to passing time by accounting for SecondsPassed in the Update
    methods.
    See https://castle-engine.io/view_events .
}
procedure Sleep(const Milliseconds: Cardinal);
var
  TimerStart: TTimerResult;
begin
  TimerStart := Timer;
  while TimerStart.ElapsedTime < Milliseconds / 1000 do
    { nothing };
end;
{$endif}

{ TTestCastleTimeUtils ---------------------------------------------------------- }

{$warnings off} // knowingly using deprecated, to check they are working

procedure TTestCastleTimeUtils.TestGetTickCount64;
var
  G1, G2: TMilisecTime;
begin
  G1 := CastleGetTickCount64;
  Sleep(1000);
  G2 := CastleGetTickCount64;

  { It should be that G2 is larger than G1 by 1000 milisecons.
    To allow wild imprecision, we check something larger.
    Also, to work even when 32-bit GetTickCount wraps,
    use TimeTickSecondLater instead of MilisecTimesSubtract. }

  // Writeln(MilisecTimesSubtract(G2, G1));
  // Writeln(G2 - G1);

  AssertTrue(TimeTickSecondLater(G1, G2, 100));
  AssertFalse(TimeTickSecondLater(G1, G2, 10000));

  { test on prepared values }
  AssertTrue(TimeTickSecondLater(1000, 2001, 1000));
  AssertFalse(TimeTickSecondLater(1000, 1999, 1000));

  { test on prepared values simulating wrap with 32-bit GetTickCount }
  AssertTrue(TimeTickSecondLater(High(UInt32) - 500, 501, 1000));
  AssertFalse(TimeTickSecondLater(High(UInt32) - 500, 499, 1000));
end;

{$warnings on}

initialization
  RegisterTest(TTestCastleTimeUtils);
end.
