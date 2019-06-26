{
  Copyright 2019-2019 Michalis Kamburelis.

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
  SysUtils, ConsoleTestRunner,
  CastleConsoleTestRunner, CastleApplicationProperties, CastleScript,
  TestToolProject;

var
  Application: TCastleConsoleTestRunner;
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
  ScriptVerboseMessages := true;
  Application := TCastleConsoleTestRunner.Create(nil);
  try
    Application.Title := 'CGE Build Tool - Test runner (using fpcunit)';
    DefaultFormat := fPlain;
    Application.Initialize;
    Application.Run;
  finally FreeAndNil(Application) end;
end.
