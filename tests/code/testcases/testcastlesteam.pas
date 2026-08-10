{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleSteam unit. }
unit TestCastleSteam;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleSteam = class(TCastleTestCase)
  published
    procedure TestSteam;
  end;

implementation

uses CastleSteam;

procedure TTestCastleSteam.TestSteam;
begin
  { Nothing functional to test (we don't want to connect to Steam from CI
    auto-tests now), just make sure CastleSteam compiles, to avoid bugs like
    https://github.com/castle-engine/castle-engine/issues/678
    where CI didn't catch that CastleSteam doesn't compile with latest Delphi. }
  AssertEquals('TCastleSteam', TCastleSteam.ClassName);
end;

initialization
  RegisterTest(TTestCastleSteam);
end.
