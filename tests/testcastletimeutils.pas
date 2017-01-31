{
  Copyright 2016-2017 Michalis Kamburelis.

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

uses fpcunit, testutils, testregistry, CastleBaseTestCase;

type
  TTestCastleTimeUtils = class(TCastleBaseTestCase)
  published
    procedure TestGetTickCount64;
  end;

implementation

uses SysUtils, Classes, Math, CastleTimeUtils;

{ TTestCastleTimeUtils ---------------------------------------------------------- }

{$warnings off} // knowingly using deprecated, to check they are working

procedure TTestCastleTimeUtils.TestGetTickCount64;
var
  G1, G2: TMilisecTime;
begin
  G1 := CastleTimeUtils.GetTickCount64;
  Sleep(1000);
  G2 := CastleTimeUtils.GetTickCount64;

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
  AssertTrue(TimeTickSecondLater(High(LongWord) - 500, 501, 1000));
  AssertFalse(TimeTickSecondLater(High(LongWord) - 500, 499, 1000));
end;

{$warnings on}

initialization
  RegisterTest(TTestCastleTimeUtils);
end.
