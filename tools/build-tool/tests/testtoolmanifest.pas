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
  end;

implementation

procedure TTestToolManifest.TestStandaloneSourceToProgramName;
begin
  AssertEquals('my_program', TCastleManifest.StandaloneSourceToProgramName('my_program.lpr'));
  AssertEquals('my_program', TCastleManifest.StandaloneSourceToProgramName('code/my_program.lpr'));
end;

initialization
  RegisterTest(TTestToolManifest);
end.
