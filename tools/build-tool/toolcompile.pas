{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Compiling with FPC. }
unit ToolCompile;

interface

uses Classes, ToolArchitectures;

type
  TCompilationMode = (cmRelease, cmValgrind, cmDebug);

{ Compile with FPC and proper command-line option given file.
  SearchPaths, ExtraOptions may be @nil (same as empty). }
procedure Compile(const OS: TOS; const CPU: TCPU; const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths: TStrings);

{ Output path, where temporary things like units (and iOS stuff)
  are placed. }
function CompilationOutputPath(const OS: TOS; const CPU: TCPU;
  const WorkingDirectory: string): string;

function ModeToString(const M: TCompilationMode): string;
function StringToMode(const S: string): TCompilationMode;

var
  { The -V3.0.3 parameter is necessary if you got FPC
    from the fpc-3.0.3.intel-macosx.cross.ios.dmg
    (official "FPC for iOS" installation). }
  FPCVersionForIPhoneSimulator: string = '3.0.3';

implementation

uses SysUtils, Process,
  CastleUtils, CastleStringUtils, CastleLog, CastleFilesUtils, CastleFindFiles,
  ToolUtils;

(* Not useful anymore. May become useful again some day.

type
  TFPCVersion = object
    Major, Minor, Release: Integer;
  end;

{ Get FPC version by running "fpc -iV". }
function FPCVersion: TFPCVersion;
var
  FpcOutput, FpcExe, Token: string;
  FpcExitStatus, SeekPos: Integer;
begin
  FpcExe := FindExe('fpc');
  if FpcExe = '' then
    raise Exception.Create('Cannot find "fpc" program on $PATH. Make sure it is installed, and available on $PATH');
  MyRunCommandIndir(GetCurrentDir, FpcExe, ['-iV'], FpcOutput, FpcExitStatus);
  if FpcExitStatus <> 0 then
    raise Exception.Create('Failed to query FPC version');

  { parse output into 3 numbers }
  FpcOutput := Trim(FpcOutput);
  SeekPos := 1;

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
    raise Exception.CreateFmt('Failed to query FPC version: no major version in response "%s"', [FpcOutput]);
  Result.Major := StrToInt(Token);

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
    raise Exception.CreateFmt('Failed to query FPC version: no minor version in response "%s"', [FpcOutput]);
  Result.Minor := StrToInt(Token);

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
  begin
    WritelnWarning('FPC', 'Invalid FPC version: Failed to query FPC version: no release version in response "%s", assuming 0', [FpcOutput]);
    Result.Release := 0;
  end else
    Result.Release := StrToInt(Token);

  Writeln(Format('FPC version: %d.%d.%d', [Result.Major, Result.Minor, Result.Release]));
end;
*)

type
  TCleanDirectoryHelper = class
    DeletedFiles: Cardinal; //< only for DeleteFoundFile
    procedure DeleteFoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
  end;

procedure TCleanDirectoryHelper.DeleteFoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  if Verbose then
    Writeln('Deleting ' + FileInfo.AbsoluteName);
  CheckDeleteFile(FileInfo.AbsoluteName);
  Inc(DeletedFiles);
end;

{ Clean compilation trash in Directory, recursively. }
procedure CleanDirectory(const Directory: string);
var
  Helper: TCleanDirectoryHelper;

  procedure DeleteFilesRecursive(const Mask: string);
  begin
    FindFiles(Directory, Mask, false, @Helper.DeleteFoundFile, [ffRecursive]);
  end;

begin
  Helper := TCleanDirectoryHelper.Create;
  try
    // clean FPC compilation stuff
    DeleteFilesRecursive('*.ppu');
    DeleteFilesRecursive('*.o');
    DeleteFilesRecursive('*.or');
    DeleteFilesRecursive('*.rst');
    DeleteFilesRecursive('*.rsj');
    Writeln('Deleted ', Helper.DeletedFiles, ' files');
  finally FreeAndNil(Helper) end;
end;

procedure Compile(const OS: TOS; const CPU: TCPU; const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths: TStrings);
var
  CastleEnginePath, CastleEngineSrc: string;
  FpcOptions: TCastleStringList;

  procedure AddEnginePath(Path: string);
  begin
    Path := CastleEngineSrc + Path;
    if not DirectoryExists(Path) then
      WritelnWarning('Path', 'Path "%s" does not exist. Make sure that $CASTLE_ENGINE_PATH points to the directory containing Castle Game Engine sources (the castle_game_engine/ or castle-engine/ directory)', [Path]);
    FpcOptions.Add('-Fu' + Path);
    FpcOptions.Add('-Fi' + Path);
  end;

  procedure AddSearchPaths;
  var
    I: Integer;
  begin
    if SearchPaths <> nil then
      for I := 0 to SearchPaths.Count - 1 do
      begin
        FpcOptions.Add('-Fu' + SearchPaths[I]);
        FpcOptions.Add('-Fi' + SearchPaths[I]);
      end;
  end;

  procedure AddIOSOptions;
  {$ifdef DARWIN}
  const
    SimulatorSdk = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk';
    DeviceSdk = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';
  {$endif}
  var
    IOS: boolean;
  begin
    IOS := false;

    if OS = iphonesim then
    begin
      IOS := true;
      if FPCVersionForIPhoneSimulator <> '' then
        FpcOptions.Add('-V' + FPCVersionForIPhoneSimulator);
      {$ifdef DARWIN}
      FpcOptions.Add('-XR' + SimulatorSdk);
      {$endif}
    end;

    if (OS = darwin) and (CPU = arm) then
    begin
      IOS := true;
      FpcOptions.Add('-Cparmv7');
      FpcOptions.Add('-Cfvfpv3');
      {$ifdef DARWIN}
      FpcOptions.Add('-XR' + DeviceSdk);
      {$endif}
    end;

    if (OS = darwin) and (CPU = aarch64) then
    begin
      IOS := true;
      // -dCPUARM64 is checked by castleconf.inc
      FpcOptions.Add('-dCPUARM64');
      {$ifdef DARWIN}
      FpcOptions.Add('-XR' + DeviceSdk);
      {$endif}
    end;

    // options for all iOS platforms
    if IOS then
    begin
      FpcOptions.Add('-Cn');
      { This corresponds to the iOS version used when compiling FPC 3.0.3 RTL
        from the latest official FPC release for iOS.
        With -WP5.1, I got a lot of warnings that FPC RTL was for iOS 7.0.
        Also, I got an error:
        clang: error: -fembed-bitcode is not supported on versions of iOS prior to 6.0
      }
      FpcOptions.Add('-WP7.0');
      { TODO: this option is probably useless for now, since we pass -Cn
        and later create the library manually. }
      FpcOptions.Add('-o' + CompilationOutputPath(OS, CPU, WorkingDirectory) + 'libcge_ios_project_unused.a');
    end;
  end;

var
  FpcOutput, CastleEngineSrc1, CastleEngineSrc2, CastleEngineSrc3, FpcExe: string;
  FpcExitStatus: Integer;
begin
  FpcOptions := TCastleStringList.Create;
  try
    CastleEnginePath := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
    if CastleEnginePath = '' then
    begin
      Writeln('CASTLE_ENGINE_PATH environment variable not defined, so we assume that engine unit paths are already specified within fpc.cfg file.');
    end else
    begin
      { Use $CASTLE_ENGINE_PATH environment variable as the directory
        containing castle_game_engine/ or castle-engine/ as subdirectory
        (or pointing straight to castle_game_engine/ or castle-engine/
        directory).
        Then add all -Fu and -Fi options for FPC to include Castle Game Engine
        sources.

        This way you can compile programs using Castle Game Engine
        from any directory. You just have to
        set $CASTLE_ENGINE_PATH environment variable
        (or make sure that units paths are in fpc.cfg). }

      { calculate CastleEngineSrc }
      CastleEngineSrc1 := InclPathDelim(CastleEnginePath) +
        'src' + PathDelim;
      CastleEngineSrc2 := InclPathDelim(CastleEnginePath) +
        'castle_game_engine' + PathDelim + 'src' + PathDelim;
      CastleEngineSrc3 := InclPathDelim(CastleEnginePath) +
        'castle-engine' + PathDelim + 'src' + PathDelim;
      if DirectoryExists(CastleEngineSrc1) then
        CastleEngineSrc := CastleEngineSrc1 else
      if DirectoryExists(CastleEngineSrc2) then
        CastleEngineSrc := CastleEngineSrc2 else
      if DirectoryExists(CastleEngineSrc3) then
        CastleEngineSrc := CastleEngineSrc3 else
      begin
        CastleEngineSrc := '';
        Writeln('CASTLE_ENGINE_PATH environment variable defined, but we cannot find Castle Game Engine sources inside (looking in "' + CastleEngineSrc1 + '" and "' + CastleEngineSrc2 + '" and "' + CastleEngineSrc3 + '").');
        Writeln('  We continue compilation, assuming that engine unit paths are already specified within fpc.cfg file.');
      end;

      if CastleEngineSrc <> '' then
      begin
        { Note that we add OS-specific paths (windows, android, unix)
          regardless of the target OS. There is no point in filtering them
          only for specific OS, since all file names must be different anyway,
          as Lazarus packages could not compile otherwise. }

        AddEnginePath('base');
        AddEnginePath('base/android');
        AddEnginePath('base/windows');
        AddEnginePath('base/unix');
        AddEnginePath('base/opengl');
        AddEnginePath('fonts');
        AddEnginePath('fonts/windows');
        AddEnginePath('fonts/opengl');
        AddEnginePath('window');
        AddEnginePath('window/gtk');
        AddEnginePath('window/windows');
        AddEnginePath('window/unix');
        AddEnginePath('images');
        AddEnginePath('images/opengl');
        AddEnginePath('images/opengl/glsl');
        AddEnginePath('3d');
        AddEnginePath('3d/opengl');
        AddEnginePath('x3d');
        AddEnginePath('x3d/opengl');
        AddEnginePath('x3d/opengl/glsl');
        AddEnginePath('audio');
        AddEnginePath('net');
        AddEnginePath('castlescript');
        AddEnginePath('castlescript/opengl');
        AddEnginePath('ui');
        AddEnginePath('ui/windows');
        AddEnginePath('ui/opengl');
        AddEnginePath('game');
        AddEnginePath('services');

        { Do not add castle-fpc.cfg.
          Instead, rely on code below duplicating castle-fpc.cfg logic
          (reasons: engine sources, with castle-fpc.cfg, may not be available
          where build-tool is called).

        FpcOptions.Add('-dCASTLE_ENGINE_PATHS_ALREADY_DEFINED');
        FpcOptions.Add('@' + InclPathDelim(CastleEnginePath) + 'castle_game_engine(or castle-engine)/castle-fpc.cfg');
        }
      end;
    end;

    AddSearchPaths;

    { Specify the compilation options explicitly,
      duplicating logic from ../castle-fpc.cfg .
      (Engine sources may be possibly not available,
      so we also cannot depend on castle-fpc.cfg being available.) }
    FpcOptions.Add('-l');
    FpcOptions.Add('-vwn');
    FpcOptions.Add('-Ci');
    FpcOptions.Add('-Mobjfpc');
    FpcOptions.Add('-Sm');
    FpcOptions.Add('-Sc');
    FpcOptions.Add('-Sg');
    FpcOptions.Add('-Si');
    FpcOptions.Add('-Sh');
    FpcOptions.Add('-vm2045'); // do not show Warning: (2045) APPTYPE is not supported by the target OS
    FpcOptions.Add('-vm5024'); // do not show Hint: (5024) Parameter "..." not used
    FpcOptions.Add('-T' + OSToString(OS));
    FpcOptions.Add('-P' + CPUToString(CPU));

    case Mode of
      cmRelease:
        begin
          FpcOptions.Add('-O2');
          FpcOptions.Add('-Xs');
          FpcOptions.Add('-dRELEASE');
        end;
      cmValgrind:
        begin
          { Like cmRelease, but
            - without -Xs
            - with -gv, -gl
            See ../../doc/profiling_howto.txt }
          FpcOptions.Add('-O2');
          FpcOptions.Add('-dRELEASE');
          FpcOptions.Add('-gv');
          FpcOptions.Add('-gl');
        end;
      cmDebug:
        begin
          FpcOptions.Add('-Cr');
          FpcOptions.Add('-Co');
          FpcOptions.Add('-Sa');
          FpcOptions.Add('-CR');
          FpcOptions.Add('-g');
          FpcOptions.Add('-gl');
          FpcOptions.Add('-dDEBUG');
        end;
      else raise EInternalError.Create('DoCompile: Mode?');
    end;

    case OS of
      Android:
        begin
          { Our platform is armeabi-v7a, see
            data/android/base/app/src/main/jni/Application.mk .
            Note: the option below seems not necessary when using -CfVFPV3?
            At least, nothing crashes.
            Possibly -CfVFPV3 implies this anyway. }
          FpcOptions.Add('-CpARMV7A');

          { Necessary to work fast.
            See https://github.com/castle-engine/castle-engine/wiki/Android-FAQ#notes-about-compiling-with-hard-floats--cfvfpv3 }
          FpcOptions.Add('-CfVFPV3');

          { This allows to "sacrifice precision for performance"
            according to http://wiki.freepascal.org/ARM_compiler_options .

            But it causes too much precision loss?
            escape_universe fails with
            I/escape_universe( 7761): Exception: Exception "EInvalidGameConfig" :
            I/escape_universe( 7761): Gun auto_fire_interval cannot be <= 0

            Speed gain untested.

            For now unused. }
          //FpcOptions.Add('-OoFASTMATH');

          { This should *not* be defined (when compiling our code or RTL).
            It makes our code use -CaEABIHF/armeabi-v7a-hard
            https://android.googlesource.com/platform/ndk/+/353e653824b79c43b948429870d0abeedebde386/docs/HardFloatAbi.md
            which has incompatible call mechanism.

            And indeed, doing PlaySound crashes at alSourcef call (to OpenAL)
            from TSound.SetMinGain. Reproducible with escape_universe.

            fpcupdeluxe default cross-compiler to Android also uses this. }
          //FpcOptions.Add('-CaEABIHF');
        end;
    end;

    if Plugin then
    begin
      FpcOptions.Add('-dCASTLE_ENGINE_PLUGIN');
      { We need to add -fPIC otherwise compiling a shared library fails with

        /usr/bin/ld: .../castlewindow.o: relocation R_X86_64_32S against
          `U_CASTLEWINDOW_MENUITEMS' can not be used when making
          a shared object; recompile with -fPIC

        That is because FPC RTL is compiled with -fPIC for this platform,
        it seems. See
        http://lists.freepascal.org/pipermail/fpc-pascal/2014-November/043155.html
        http://lists.freepascal.org/pipermail/fpc-pascal/2014-November/043159.html

        """
        fpcmake automatically adds -Cg/-fPIC for x86-64 platforms,
        exactly because of this issue.
        """

        And http://wiki.freepascal.org/PIC_information explains about PIC:
        """
        fpcmake automatically adds -Cg/-fPIC for x86-64 platforms:
        * for freebsd, openbsd, netbsd, linux, solaris
        * for darwin -Cg is enabled by the compiler (see below)
        """
      }
      if (CPU = X86_64) and (OS in [Linux,FreeBSD,NetBSD,OpenBSD,Solaris]) then
        FpcOptions.Add('-fPIC');
    end;

    FpcOptions.Add(CompileFile);
    FpcOptions.Add('-FU' + CompilationOutputPath(OS, CPU, WorkingDirectory));

    AddIOSOptions;

    Writeln('FPC executing...');
    FpcExe := FindExe('fpc');
    if FpcExe = '' then
      raise Exception.Create('Cannot find "fpc" program on $PATH. Make sure it is installed, and available on $PATH');

    RunCommandIndirPassthrough(WorkingDirectory, FpcExe, FpcOptions.ToArray, FpcOutput, FpcExitStatus);
    if FpcExitStatus <> 0 then
    begin
      if Pos('Fatal: Internal error', FpcOutput) <> 0 then
      begin
        Writeln('-------------------------------------------------------------');
        Writeln('It seems FPC crashed with a compiler error (Internal error). If you can reproduce this problem, please report it to http://bugs.freepascal.org/ ! We want to help FPC developers to fix this problem, and the only way to do it is to report it. If you need help creating a good bugreport, speak up on the FPC or Castle Game Engine mailing list.');
        Writeln;
        Writeln('As a workaround, right now we''ll clean your project, and (if we have permissions) the Castle Game Engine units, and try compiling again.');
        Writeln('-------------------------------------------------------------');
        { when we're called to compile a project, WorkingDirectory is the project
          path, so this is always Ok. }
        CleanDirectory(WorkingDirectory);
        if CastleEngineSrc <> '' then
          CleanDirectory(CastleEngineSrc);
        RunCommandIndirPassthrough(WorkingDirectory, FpcExe, FpcOptions.ToArray, FpcOutput, FpcExitStatus);
        if FpcExitStatus <> 0 then
          { do not retry compiling in a loop, give up }
          raise Exception.Create('Failed to compile');
      end else
        raise Exception.Create('Failed to compile');
    end;
  finally FreeAndNil(FpcOptions) end;
end;

function CompilationOutputPath(const OS: TOS; const CPU: TCPU;
  const WorkingDirectory: string): string;
begin
  Result := OutputPath(WorkingDirectory) + 'compilation' + PathDelim +
    CPUToString(CPU) + '-' + OSToString(OS) + PathDelim;
  CheckForceDirectories(Result);
end;

const
  CompilationModeNames: array [TCompilationMode] of string =
  ('release', 'valgrind', 'debug');

function ModeToString(const M: TCompilationMode): string;
begin
  Result := CompilationModeNames[M];
end;

function StringToMode(const S: string): TCompilationMode;
begin
  for Result in TCompilationMode do
    if AnsiSameText(CompilationModeNames[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid compilation mode name "%s"', [S]);
end;

end.
