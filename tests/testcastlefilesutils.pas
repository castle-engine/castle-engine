{
  Copyright 2007-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test some OS-specific utilities in castle_game_engine,
  mostly in CastleFilesUtils unit.
  Usually the mere purpose of these utilities is to hide some OS-specific
  (UNIX-specific, Windows-specific) things from program. }

unit TestCastleFilesUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestOSSpecific = class(TTestCase)
  published
    procedure TestPathDelim;
    procedure TestExeName;
    procedure TestTimer;
    {$ifdef UNIX} procedure TestHomePath; {$endif}
    procedure TestGetTempDir;
  end;

implementation

uses CastleUtils, CastleFindFiles, CastleFilesUtils, CastleTimeUtils
  {$ifdef UNIX}, BaseUnix {$endif};

procedure TTestOSSpecific.TestPathDelim;
begin
  { slash should be added / stripped on both Windows and Unix }

  AssertEquals('/tmp', ExclPathDelim('/tmp/'));
  AssertEquals('', ExclPathDelim('/'));
  AssertEquals('/tmp', ExclPathDelim('/tmp'));
  AssertEquals('', ExclPathDelim(''));

  AssertEquals('/tmp/', InclPathDelim('/tmp/'));
  AssertEquals('/', InclPathDelim('/'));
  AssertEquals('/tmp' + PathDelim, InclPathDelim('/tmp'));
  AssertEquals(PathDelim, InclPathDelim(''));

  { backslash should be likewise added / stripped on Windows }
  {$ifdef MSWINDOWS}
  AssertEquals('\tmp', ExclPathDelim('\tmp\'));
  AssertEquals('', ExclPathDelim('\'));
  AssertEquals('\tmp', ExclPathDelim('\tmp'));
  AssertEquals('', ExclPathDelim(''));

  AssertEquals('\tmp\', InclPathDelim('\tmp\'));
  AssertEquals('\', InclPathDelim('\'));
  AssertEquals('\tmp\', InclPathDelim('\tmp'));
  AssertEquals('\', InclPathDelim(''));
  {$endif}

  { backslash should not be treated like dir separator on Unix }
  {$ifdef UNIX}
  AssertEquals('\tmp\', ExclPathDelim('\tmp\'));
  AssertEquals('\', ExclPathDelim('\'));
  AssertEquals('\tmp', ExclPathDelim('\tmp'));
  AssertEquals('', ExclPathDelim(''));

  AssertEquals('\tmp\/', InclPathDelim('\tmp\'));
  AssertEquals('\/', InclPathDelim('\'));
  AssertEquals('\tmp/', InclPathDelim('\tmp'));
  AssertEquals('/', InclPathDelim(''));
  {$endif}
end;

procedure TTestOSSpecific.TestExeName;
begin
  try
//    Writeln('ExeName: '+ ExeName);
    ExeName;
  except
    on E: EExeNameNotAvailable do
    begin
//      Writeln('ExeName not available, allowed on non-Windows:' +nl+ ExceptMessage(E));
      { This is an error under Windows, where ExeName is guaranteed to be available }
      {$ifdef MSWINDOWS} raise; {$endif}
    end;
  end;
end;

procedure TTestOSSpecific.TestTimer;
{var
  TimerStart: TTimerResult;}
begin
{  Writeln('Waiting 5 seconds...');

  ProcessTimerBegin;
  TimerStart := Timer;
  Sleep(5000);
  Writeln( Format(
    'Sleep(5 seconds) done,' +nl+
    'ProcessTimer reports that %f seconds elapsed,'+nl+
    'Timer reports that %f seconds elapsed.',
    [ ProcessTimerEnd, TimerSeconds(Timer, TimerStart) ])); }
end;

{$ifdef UNIX}
procedure TTestOSSpecific.TestHomePath;
begin
{  Writeln('HomePath is ', HomePath);
  Writeln('  ~/expanded is ', ExpandHomePath('~/expanded')); }
  AssertTrue(ExpandHomePath('/bin/') = '/bin/');
  AssertTrue(ExpandHomePath('~') = ExclPathDelim(HomePath));
  AssertTrue(ExpandHomePath('~/') = InclPathDelim(HomePath));
end;
{$endif}

procedure TTestOSSpecific.TestGetTempDir;
begin
//  Writeln('TempDir: ', GetTempDir);
  GetTempDir; // ignore result, just make sure it doesn't raise errors
end;

initialization
  RegisterTest(TTestOSSpecific);
end.
