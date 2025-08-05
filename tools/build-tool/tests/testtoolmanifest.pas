{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatic tests of ToolManifest unit. }
unit TestToolManifest;

interface

uses
  Classes, SysUtils, CastleTester,
  ToolManifest;

type
  TTestToolManifest = class(TCastleTestCase)
  published
    procedure TestStandaloneSourceToProgramName;
    procedure TestCheckExecutableName;
  end;

implementation

uses CastleLog;

procedure TTestToolManifest.TestStandaloneSourceToProgramName;
begin
  AssertEquals('my_program', TCastleManifest.StandaloneSourceToProgramName('my_program.lpr'));
  AssertEquals('my_program', TCastleManifest.StandaloneSourceToProgramName('code/my_program.lpr'));
end;

procedure TTestToolManifest.TestCheckExecutableName;
begin
  TCastleManifest.CheckExecutableName('my_program');
  TCastleManifest.CheckExecutableName('model-program');
  TCastleManifest.CheckExecutableName('样例中文文本');
  TCastleManifest.CheckExecutableName('my program');

  try
    TCastleManifest.CheckExecutableName('slash/not/allowed');
    Fail('Should fail');
  except
    on E: Exception do
      WritelnLog('Valid exception: ' + E.Message);
  end;

  try
    TCastleManifest.CheckExecutableName('bashslash\not allowed');
    Fail('Should fail');
  except
    on E: Exception do
      WritelnLog('Valid exception: ' + E.Message);
  end;

  try
    TCastleManifest.CheckExecutableName('as:,');
    Fail('Should fail');
  except
    on E: Exception do
      WritelnLog('Valid exception: ' + E.Message);
  end;
end;

initialization
  RegisterTest(TTestToolManifest);
end.
