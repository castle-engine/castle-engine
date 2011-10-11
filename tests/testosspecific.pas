{
  Copyright 2007-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test some OS-specific utilities in castle_game_engine,
  mostly in CastleUtils unit.
  Usually the mere purpose of these utilities is to hide some OS-specific
  (UNIX-specific, Windows-specific) things from program. }

unit TestOSSpecific;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestOSSpecific = class(TTestCase)
  published
    procedure TestChangeDir;
    {$ifdef UNIX} procedure TestIsSymLink; {$endif}
    procedure TestExeName;
    procedure TestTimer;
    procedure TestProgramPaths;
    {$ifdef UNIX} procedure TestHomePath; {$endif}
    procedure TestGetTempPath;
  end;

implementation

uses CastleUtils, EnumerateFiles, CastleFilesUtils, CastleTimeUtils;

procedure TTestOSSpecific.TestChangeDir;
var
  PreviousDir: string;
const
  ExistingDir = {$ifdef UNIX} '/' {$endif}
                {$ifdef MSWINDOWS} 'c:/' {$endif};
  NotExistingDir = {$ifdef UNIX} '/not_existing_directory_kambi_test' {$endif}
                   {$ifdef MSWINDOWS} 'c:/not_existing_directory_kambi_test' {$endif};
begin
  PreviousDir := GetCurrentDir;
  try
    ChangeDir(ExistingDir);
    Assert(InclPathDelim(GetCurrentDir) = ExistingDir);

    try
      ChangeDir(NotExistingDir);
      Assert(false, 'ChangeDir to ' + NotExistingDir + ' didn''t raise an exception');
    except
      on E: EInOutError do ;
    end;
  finally
    { restoring dir is necessary for other tests to work, some load
      files from data/ subdir. }
    ChangeDir(PreviousDir);
  end;
end;

{$ifdef UNIX}
procedure TestIsSymLink_Proc(const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
  Assert(ExtractFileName(FileInfo.FullFileName) = 'symlink.txt');
  Assert(IsSymLink(FileInfo.FullFileName));
  Assert(KamReadLink(FileInfo.FullFileName) = 'symlink_target.txt');
end;

{ Test IsSymLink, EnumFiles, KamReadLink }
procedure TTestOSSpecific.TestIsSymLink;
begin
  EnumFiles('data/symlink.txt', faAnyFile, @TestIsSymLink_Proc, nil, [eoSymlinks]);

  try
    KamReadLink('/non_existing_file');
    Assert(false, 'KamReadLink must raise exception on non-existing file');
  except
    on E: EOSError do
      { OK, KamReadLink for /non_existing_file should raise exception };
  end;
end;
{$endif}

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
  Delay(5000);
  Writeln( Format(
    'Delay(5 seconds) done,' +nl+
    'ProcessTimer reports that %f seconds elapsed,'+nl+
    'Timer reports that %f seconds elapsed.',
    [ ProcessTimerEnd, (Timer - TimerStart) / TimerFrequency ])); }
end;

procedure TTestOSSpecific.TestProgramPaths;
{const
  Other_WindowsExeName = 'c:\foo\bar.exe';
  Other_ProgramName = 'bar';}
begin
{  Writeln(
    '---- This program''s files/paths:', nl,
    nl,
    'UserConfigPath = ', UserConfigPath, nl,
    'UserConfigFile(''.ini'') = ', UserConfigFile('.ini'), nl,
    'UserConfigFile_FromProposed(''some_config'') = ',
      UserConfigFile_FromProposed('some_config'), nl,
    'ProgramDataPath = ', ProgramDataPath, nl,
    nl,

    '---- Files/paths of program with ProgramName = ', Other_ProgramName,
      ', ExeName (Windows only) = ', Other_WindowsExeName, nl,
    nl,
    'UserConfigPath = ',
      UserConfigPath_Other(ExtractFilePath(Other_WindowsExeName)), nl,
    'UserConfigFile(''.ini'') = ',
      UserConfigFile_Other('.ini', Other_ProgramName, Other_WindowsExeName), nl,
    'UserConfigFile_FromProposed(''some_config'') = ',
      UserConfigFile_FromProposed_Other('some_config', ExtractFilePath(Other_WindowsExeName)), nl,
    'ProgramDataPath = ',
      ProgramDataPath_Other(Other_ProgramName, ExtractFilePath(Other_WindowsExeName)), nl
    );}
end;

{$ifdef UNIX}
procedure TTestOSSpecific.TestHomePath;
begin
{  Writeln('HomePath is ', HomePath);
  Writeln('  ~/expanded is ', ExpandHomePath('~/expanded')); }
  Assert(ExpandHomePath('/bin/') = '/bin/');
  Assert(ExpandHomePath('~') = ExclPathDelim(HomePath));
  Assert(ExpandHomePath('~/') = InclPathDelim(HomePath));
end;
{$endif}

procedure TTestOSSpecific.TestGetTempPath;
begin
//  Writeln('TempPath: ', GetTempPath);
  GetTempPath; // ignore result, just make sure it doesn't raise errors
end;

initialization
  RegisterTest(TTestOSSpecific);
end.
