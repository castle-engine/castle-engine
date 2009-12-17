{ Test some very platform-specific utilities in kambi_vrml_game_engine.
  Usually the mere purpose of these utilities is to hide some OS-specific
  (UNIX-specific, Windows-specific) things from program.

  Started to test porting to Unix/BaseUnit units (from old Libc unit). }

{$apptype CONSOLE}

uses SysUtils, KambiUtils, EnumerateFiles, KambiFilesUtils,
  KambiTimeUtils;

procedure TestChangeDir;
const
  ExistingDir = {$ifdef UNIX} '/usr' {$endif}
                {$ifdef MSWINDOWS} 'd:/mojepasy/units/images/' {$endif};
  NotExistingDir = {$ifdef UNIX} '/non_existing_dir' {$endif}
                   {$ifdef MSWINDOWS} 'd:/non_existing_dir' {$endif};
begin
 ChangeDir(ExistingDir);
 Writeln('Current dir is ', GetCurrentDir, ' (should be ', ExistingDir, ')');

 try
  ChangeDir(NotExistingDir);
  Writeln('WRONG! ChangeDir to ', NotExistingDir, ' didn''t raise an exception');
 except
  on E: EInOutError do
   Writeln('OK, exception:' +nl+ ExceptMessage(E));
 end;
end;

{$ifdef UNIX}
procedure TestIsSymLink_Proc(const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
 Writeln(FileInfo.FullFileName:20, ': symlink ? ',
   IsSymLink(FileInfo.FullFileName));
 if IsSymLink(FileInfo.FullFileName) then
  Writeln(' -> ', KamReadLink(FileInfo.FullFileName));
end;

{ test IsSymLink, EnumFiles, KamReadLink }
procedure TestIsSymLink;
begin
 EnumFiles( '/*', faAnyFile, @TestIsSymLink_Proc, nil, [eoSymlinks]);
end;
{$endif}

procedure TestExeName;
begin
 try
  Writeln('ExeName available: '+ ExeName);
 except
  on E: Exception do
  begin
   Writeln('ExeName not available:' +nl+ ExceptMessage(E));
   { This is an error under Windows, where ExeName is guaranteed to be available }
   {$ifdef MSWINDOWS} raise; {$endif}
  end;
 end;
end;

{$ifdef UNIX}
procedure TestKamReadLink;
const
  ExampleSymLinkName = {$ifdef LINUX} '/boot/vmlinuz' {$endif}
                       {$ifdef FREEBSD} '/home' {$endif}
                       {$ifdef DARWIN} '/etc' {$endif};
begin
 Writeln(ExampleSymLinkName + ' -> ', KamReadLink(ExampleSymLinkName));
 try
  KamReadLink('/non_existing_file');
 except
  on E: EKambiOSError do
   Writeln('OK, KamReadLink for /non_existing_file raised exception:' +nl+
     ExceptMessage(E));
 end;
end;
{$endif}

procedure TestTimer;
var
  TimerStart: TKamTimerResult;
begin
 Writeln('Waiting 5 seconds...');

 ProcessTimerBegin;
 TimerStart := KamTimer;
 Delay(5000);
 Writeln( Format(
   'Delay(5 seconds) done,' +nl+
   'ProcessTimer reports that %f seconds elapsed,'+nl+
   'Timer reports that %f seconds elapsed.',
   [ ProcessTimerEnd, (KamTimer - TimerStart) / KamTimerFrequency ]));
end;

procedure TestProgramPaths;
const
  Other_WindowsExeName = 'c:\foo\bar.exe';
  Other_ProgramName = 'bar';
begin
 Writeln(
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
   );
end;

{$ifdef UNIX}
procedure TestHomePath;
begin
 Writeln('HomePath is ', HomePath);
 Writeln('ExpandHomePath testing:');
 Writeln('  ~/expanded is ', ExpandHomePath('~/expanded'));
 Writeln('  ~/ is ', ExpandHomePath('~/'));
 Writeln('  ~ is ', ExpandHomePath('~'));
 Writeln('  /bin/ is ', ExpandHomePath('/bin/'));
end;
{$endif}

procedure TestGetTempPath;
begin
 Writeln('TempPath: ', GetTempPath);
end;

begin
 { You should be able to switch tests on/off by simple //-style comments }
 TestChangeDir;
 {$ifdef UNIX} TestIsSymLink; {$endif}
 TestExeName;
 {$ifdef UNIX} TestKamReadLink; {$endif}
 TestTimer;
 TestProgramPaths;
 {$ifdef UNIX} TestHomePath; {$endif}
 TestGetTempPath;
end.
