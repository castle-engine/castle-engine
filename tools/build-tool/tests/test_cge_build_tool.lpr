{
  Copyright 2019-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatic test runner for CGE build tool. }

uses
  SysUtils, CastleTester, CastleConsoleTester, CastleApplicationProperties,
  CastleScript, CastleLog,
  TestToolProject;

var
  ConsoleTester: TCastleConsoleTester;
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
  ScriptVerboseMessages := true;

  { Avoid warnings that opening files too early. }
  ApplicationProperties._FileAccessSafe := true;

  InitializeLog; // CastleConsoleTester puts output in log

  ConsoleTester := TCastleConsoleTester.Create;
  try
    ConsoleTester.Run('');
  finally
    FreeAndNil(ConsoleTester);
  end;
end.
