{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleInternalProcess;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleInternalProcess = class(TCastleTestCase)
    procedure TestEnvironmentStrings;
  end;

implementation

uses CastleInternalProcess, CastleUtils, CastleLog;

procedure TTestCastleInternalProcess.TestEnvironmentStrings;
var
  Env: TStrings;
  I: Integer;
begin
  Env := EnvironmentStrings;
  try
    WritelnLog('Environment strings count: %d', [Env.Count]);
    //WritelnLog('Environment strings:' + NL + Env.Text);
    for I := 0 to Env.Count - 1 do
      AssertTrue(Env.Names[I] <> '');
  finally FreeAndNil(Env) end;
end;

initialization
  RegisterTest(TTestCastleInternalProcess);
end.

