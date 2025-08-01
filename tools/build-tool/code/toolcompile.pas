{
  Copyright 2014-2024 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses Classes,
  CastleStringUtils, CastleInternalArchitectures,
  ToolManifest;

type
  TCompilationMode = (cmRelease, cmValgrind, cmDebug);

  { Compiler options are passed to @link(Compile) procedure and friends,
    and are read-only there.
    Caller should set the fields of this class before calling @link(Compile).

    The objects here (like TCastleStringList) are
    - owned by this class (created in constructor and destroyed in destructor)
    - never @nil
    - initially empty. }
  TCompilerOptions = class
    OS: TOS;
    CPU: TCPU;
    Mode: TCompilationMode;
    { Compile with memory leak detection.
      For now, only supported by CompileFpc, ignored by CompileLazbuild and CompileDelphi. }
    DetectMemoryLeaks: boolean;
    { Define symbols during compilation.
      For now, only supported by CompileFpc and CompileDelphi, ignored by CompileLazbuild. }
    Defines: TCastleStringList;
    { Units / includes paths.
      For now, only supported by CompileFpc and CompileDelphi, ignored by CompileLazbuild. }
    SearchPaths: TCastleStringList;
    { Library path.
      For now, only supported by CompileFpc, ignored by CompileDelphi and CompileLazbuild. }
    LibraryPaths: TCastleStringList;
    { Extra compiler options, directly passed to fpc/dcc on command-line.
      For now, only supported by CompileFpc and CompileDelphi, ignored by CompileLazbuild. }
    ExtraOptions: TCastleStringList;
    { Allow using cache. @true by default. }
    AllowCache: Boolean;

    { When not empty, this environment variable has set
      value OverrideEnvironmentValue in the compiler process.) }
    OverrideEnvironmentName: String;
    OverrideEnvironmentValue: String;

    { This parameter is filled by Compile procedure, and should contain the binary file produced at the link stage.
      For now, only supported by CompileFpc. }
    LinkerOutputFile: String;

    constructor Create;
    destructor Destroy; override;
  end;

{ Compile with Pascal compiler.
  SearchPaths, ExtraOptions may be @nil (same as empty). }
procedure Compile(Compiler: TCompiler;
  const WorkingDirectory, CompileFile: String; const Options: TCompilerOptions);

{ Compile with FPC and proper command-line option given file.
  SearchPaths, ExtraOptions may be @nil (same as empty). }
procedure CompileFpc(
  const WorkingDirectory, CompileFile: String; const Options: TCompilerOptions);

{ Compile with Delphi and proper command-line option given file.
  SearchPaths, ExtraOptions may be @nil (same as empty). }
procedure CompileDelphi(
  const WorkingDirectory, CompileFile: String; const Options: TCompilerOptions);

{ Compile with lazbuild. }
procedure CompileLazbuild(
  const WorkingDirectory, LazarusProjectFile: String; const Options: TCompilerOptions);

{ Run lazbuild with specified command-line options.
  Warning: This @italic(may) modify LazbuildOptions contents,
  consider them undefined after this call. }
procedure RunLazbuild(const WorkingDirectory: String; const LazbuildOptions: TCastleStringList);
procedure RunLazbuild(const WorkingDirectory: String; const LazbuildOptions: array of String);

{ Output path, where temporary things like units (and iOS stuff)
  are placed. }
function CompilationOutputPath(const Compiler: TCompiler;
  const OS: TOS; const CPU: TCPU;
  const WorkingDirectory: String): String;

function ModeToString(const M: TCompilationMode): String;
function StringToMode(const S: String): TCompilationMode;

var
  { Should we use the -Vxxx parameter, that is necessary if you got FPC
    from the fpc-3.0.3.intel-macosx.cross.ios.dmg
    (official "FPC for iOS" installation). }
  FpcVersionForIPhoneSimulator: String = 'auto';

const
  CompilationModeToStr: array [TCompilationMode] of string = (
    'release',
    'valgrind',
    'debug'
  );

implementation

uses SysUtils, Process,
  CastleUtils, CastleLog, CastleFilesUtils, CastleFindFiles,
  CastleInternalTools,
  ToolCommonUtils, ToolUtils, ToolFpcVersion, ToolCompilerInfo, ToolProcessRun;

{ TCompilerOptions ----------------------------------------------------------- }

constructor TCompilerOptions.Create;
begin
  inherited;
  Defines := TCastleStringList.Create;
  SearchPaths := TCastleStringList.Create;
  LibraryPaths := TCastleStringList.Create;
  ExtraOptions := TCastleStringList.Create;
  AllowCache := true;
end;

destructor TCompilerOptions.Destroy;
begin;
  FreeAndNil(Defines);
  FreeAndNil(SearchPaths);
  FreeAndNil(LibraryPaths);
  FreeAndNil(ExtraOptions);
  inherited;
end;

{ TFpcVersionForIPhoneSimulatorChecked --------------------------------------- }

type
  TFpcVersionForIPhoneSimulatorChecked = class
  strict private
    class var
      IsCached: boolean;
      CachedValue: String;
    class function AutoDetect(const FpcVer: TFpcVersion): String; static;
  public
    { Return FpcVersionForIPhoneSimulator, but the 1st time this is run,
      we check and optionally change the returned value to something better. }
    class function Value(const FpcVer: TFpcVersion): String; static;
  end;

class function TFpcVersionForIPhoneSimulatorChecked.AutoDetect(
  const FpcVer: TFpcVersion): String; static;
begin
  if (not Odd(FpcVer.Minor)) and
     (not Odd(FpcVer.Release)) then
  begin
    { If we have a stable FPC version (like 3.0.0, 3.0.2, 3.0.4...)
      then for iPhone Simulator pass -V with release bumped +1
      (making it 3.0.1, 3.0.3, 3.0.5...). }
    Result := Format('%d.%d.%d', [
      FpcVer.Major,
      FpcVer.Minor,
      FpcVer.Release + 1]);
    Writeln('Auto-detected FPC version for iPhone Simulator as ' + Result);
  end else
    { In other cases, do not pass any -Vxxx for iPhone Simulator.
      This is OK for FPC 3.1.1 from FPC SVN. }
    Result := '';
end;

class function TFpcVersionForIPhoneSimulatorChecked.Value(
  const FpcVer: TFpcVersion): String; static;
var
  FpcOutput, FpcExe: String;
  FpcExitStatus: Integer;
begin
  if not IsCached then
  begin
    CachedValue := FpcVersionForIPhoneSimulator;
    IsCached := true;

    if CachedValue = 'auto' then
      CachedValue := AutoDetect(FpcVer);

    if CachedValue <> '' then
    begin
      FpcExe := FindExeFpcCompiler;
      MyRunCommandIndir(GetCurrentDir, FpcExe, ['-V' + CachedValue, '-iV'], FpcOutput, FpcExitStatus);
      if FpcExitStatus <> 0 then
      begin
        WritelnWarning('Failed to execute FPC with "-V' + CachedValue + '" option, indicating that using this option for iPhone Simulator is invalid.' + NL +
          '  We will continue assuming that --fpc-version-iphone-simulator is empty (using normal FPC version to compile for iPhone Simulator).' + NL +
          '  Call with the correct --fpc-version-iphone-simulator on the command-line to get rid of this warning.');
        CachedValue := '';
      end;
    end;
  end;

  Result := CachedValue;
end;

{ CleanDirectory ------------------------------------------------------------- }

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
procedure CleanDirectory(const Directory: String);
var
  Helper: TCleanDirectoryHelper;

  procedure DeleteFilesRecursive(const Mask: String);
  begin
    FindFiles(Directory, Mask, false,
      {$ifdef FPC}@{$endif} Helper.DeleteFoundFile, [ffRecursive]);
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

{ CopyCacheContents ---------------------------------------------------------- }

type
  TCopyFromCacheHelper = class
    DestPath: String;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TCopyFromCacheHelper.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
var
  DestFile: String;
begin
  DestFile := DestPath + FileInfo.Name;

  { We copy from cache *without* overwriting. This way, in case the cache contained
    outdated / invalid outputs, on first run new compilation (FPC) should overwrite it,
    and we should not overwrite it back with cache. }

  if FileExists(DestFile) then
  begin
    WritelnVerbose(Format('Not copying from cache "%s", already exists', [FileInfo.Name]));
  end else
  begin
    WritelnVerbose(Format('Copying from cache "%s"', [FileInfo.Name]));
    CheckCopyFile(FileInfo.AbsoluteName, DestFile);
  end;
end;

procedure CopyCacheContents(const SourcePath, DestPath: String);
var
  Helper: TCopyFromCacheHelper;
begin
  Helper := TCopyFromCacheHelper.Create;
  try
    Helper.DestPath := InclPathDelim(DestPath);
    FindFiles(SourcePath, '*', false, {$ifdef FPC}@{$endif}Helper.FoundFile, []);
  finally FreeAndNil(Helper) end;
end;

{ Other routines ------------------------------------------------------------- }

{ Writeln a message that FPC/Lazarus crashed and we will retry,
  and clean compilation leftover files. }
procedure FpcLazarusCrashRetry(const WorkingDirectory, ToolName, ProjectName: String);
begin
  Writeln('-------------------------------------------------------------');
  Writeln('It seems ' + ToolName + ' crashed. If you can reproduce this problem, please report it to http://bugs.freepascal.org/ ! We want to help ' + ProjectName + ' developers to fix this problem, and the only way to do it is to report it. If you need help creating a good bugreport, speak up on the ' + ProjectName + ' mailing list or Castle Game Engine forum.');
  Writeln;
  Writeln('As a workaround, right now we''ll clean your project, and (if we have permissions) the Castle Game Engine units, and try compiling again.');
  Writeln('-------------------------------------------------------------');
  { when we're called to compile a project, WorkingDirectory is the project path }
  CleanDirectory(WorkingDirectory);
  CleanDirectory(TempOutputPath(WorkingDirectory, false));
  if CastleEnginePath <> '' then
  begin
    { Compiling project using build tool (through FPC or lazbuild)
      should *not* leave any file in src/ .
      But, just to be safe, try to clear it if possible. }
    CleanDirectory(CastleEnginePath + 'src' + PathDelim);
    CleanDirectory(CastleEnginePath + 'packages' + PathDelim + 'lib' + PathDelim);
  end;
end;

function FilterFpcOutput(var Line: String; const Data: Pointer): Boolean;
var
  LineLower: String;
begin
  { Lowercase once and later use IsPrefix many times with IgnoreCase=false (faster). }
  LineLower := LowerCase(Line);
  Result := not (
    { Occur without -vb }
    IsPrefix('generics.collections.pas(', LineLower, false) or
    // generics.dictionaries.inc -> generics.dictionariesh.inc in FPC 3.3.1
    IsPrefix('generics.dictionaries.inc(', LineLower, false) or
    IsPrefix('generics.dictionariesh.inc(', LineLower, false) or
    IsPrefix('generics.defaults.pas(', LineLower, false) or
    { Occur with -vb }
    (Pos('generics.collections.ppu:generics.collections.pas(', LineLower) <> 0) or
    (Pos('generics.collections.ppu:generics.dictionaries.inc(', LineLower) <> 0) or
    (Pos('generics.collections.ppu:generics.dictionariesh.inc(', LineLower) <> 0) or
    (Pos('generics.defaults.ppu:generics.defaults.pas(', LineLower) <> 0) or
    // with -vb and FPC 3.3.1 and our own generics collections copy
    (Pos('generics.collections/generics.collections.pas(', LineLower) <> 0) or
    (Pos('generics.collections/inc/generics.dictionaries.inc(', LineLower) <> 0) or
    (Pos('generics.collections/inc/generics.dictionariesh.inc(', LineLower) <> 0) or
    (Pos('generics.defaults/generics.defaults.pas(', LineLower) <> 0) or
    { Others }
    IsSuffix('warning: section "__datacoal_nt" is deprecated', LineLower, false) or
    IsSuffix('note: change section name to "__data"', LineLower, false) or
    (Line = '.section __DATA, __datacoal_nt, coalesced') or
    (Line = '         ^      ~~~~~~~~~~~~~~')
  );
  // Uncomment this just to debug that our line splitting in TCaptureOutputFilter works
  // Line := '<begin>' + Line + '<end>';
end;

procedure CompileFpc(const WorkingDirectory, CompileFile: String; const Options: TCompilerOptions);
var
  CastleEngineSrc: String;
  FpcVer: TFpcVersion;
  FpcOptions: TCastleStringList;

  procedure AddEnginePath(Path: String);
  begin
    Path := CastleEngineSrc + Path;
    if not DirectoryExists(Path) then
      WritelnWarning('Path', 'Path "%s" does not exist. Make sure that $CASTLE_ENGINE_PATH points to the directory containing Castle Game Engine sources.', [Path]);
    FpcOptions.Add('-Fu' + Path);
    FpcOptions.Add('-Fi' + Path);
  end;

  procedure AddEngineSearchPaths;
  var
    S: String;
  begin
    if CastleEngineSrc <> '' then
    begin
      for S in EnginePaths do
        AddEnginePath(S);

      { Do not add castle-fpc.cfg.
        Instead, rely on code below duplicating castle-fpc.cfg logic
        (reasons: engine sources, with castle-fpc.cfg, may not be available
        where build-tool is called).

      FpcOptions.Add('-dCASTLE_ENGINE_PATHS_ALREADY_DEFINED');
      FpcOptions.Add('@' + InclPathDelim(CastleEnginePath) + 'castle_game_engine(or castle-engine)/castle-fpc.cfg');
      }
    end;
  end;

  procedure AddSearchPaths;
  var
    S: String;
  begin
    for S in Options.SearchPaths do
    begin
      FpcOptions.Add('-Fu' + S);
      FpcOptions.Add('-Fi' + S);
    end;
  end;

  procedure AddEngineLibraryPaths;
  var
    S: String;
  begin
    if CastleEngineSrc <> '' then
      for S in EngineLibraryPaths do
        FpcOptions.Add('-Fl' + CastleEngineSrc + S);
  end;

  procedure AddLibraryPaths;
  var
    S: String;
  begin
    for S in Options.LibraryPaths do
      FpcOptions.Add('-Fl' + S);
  end;

  function IsIOS: boolean;
  begin
    Result :=
      (Options.OS = iphonesim) or
      ((Options.OS = iOS) and (Options.CPU = arm)) or
      ((Options.OS = iOS) and (Options.CPU = aarch64));
  end;

  procedure AddIOSOptions;
  {$ifdef DARWIN}
  const
    SimulatorSdk = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk';
    DeviceSdk = '/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk';
  {$endif}
  var
    LikeIOS: boolean; // physical iOS or iPhoneSimulator
    VersionForSimulator: String;
  begin
    LikeIOS := false;

    if Options.OS = iphonesim then
    begin
      LikeIOS := true;
      VersionForSimulator := TFpcVersionForIPhoneSimulatorChecked.Value(FpcVer);
      if VersionForSimulator <> '' then
        FpcOptions.Add('-V' + VersionForSimulator);
      {$ifdef DARWIN}
      FpcOptions.Add('-XR' + SimulatorSdk);
      {$endif}
    end;

    if (Options.OS = iOS) and (Options.CPU = arm) then
    begin
      LikeIOS := true;
      FpcOptions.Add('-Cparmv7');
      FpcOptions.Add('-Cfvfpv3');
      {$ifdef DARWIN}
      FpcOptions.Add('-XR' + DeviceSdk);
      {$endif}
    end;

    if (Options.OS = iOS) and (Options.CPU = aarch64) then
    begin
      LikeIOS := true;
      {$ifdef DARWIN}
      FpcOptions.Add('-XR' + DeviceSdk);
      {$endif}
    end;

    Assert(LikeIOS = IsIOS);

    // options for all iOS-like platforms
    if LikeIOS then
    begin
      FpcOptions.Add('-Cn');
      FpcOptions.Add('-dCASTLE_IOS');

      { This corresponds to the iOS version used when compiling FPC 3.0.3 RTL
        from the latest official FPC release for iOS.
        With -WP5.1, I got a lot of warnings that FPC RTL was for iOS 7.0.
        Also, I got an error:
        clang: error: -fembed-bitcode is not supported on versions of iOS prior to 6.0
      }
      FpcOptions.Add('-WP7.0');

      { This option is actually ununsed, since we pass -Cn
        and later create the library manually.

        Add -w to ignore linker warnings

        This seems the only way to get rid of Xcode (>= 8.3) linker errors when
        compiling iOS project. The error is

          Warning:pointer not aligned at address...

        and it is caused when compiling x86 code for iPhone Simulator.
        The error is inside FPC RTL, FPC developers consider this warning
        unnecessary, aligning the relevant structured would be wasteful:
        https://bugs.freepascal.org/view.php?id=31696

        See more:
        https://forum.lazarus.freepascal.org/index.php?topic=36978.0
        https://stackoverflow.com/questions/41229076/project-settings-recommends-compiler-warning-suspicious-moves
      }
      //FpcOptions.Add('-k-w');

      { This option is actually ununsed, since we pass -Cn
        and later create the library manually. }
      FpcOptions.Add('-o' +
        CompilationOutputPath(coFpc, Options.OS, Options.CPU, WorkingDirectory) +
        'libcge_ios_project_unused.a');
    end;
  end;

  procedure AddMacOSOptions;
  begin
    if (Options.OS = darwin) and ((Options.CPU = X86_64) or (Options.CPU = Aarch64)) then
    begin
      // Lazarus passes such options to compile with Cocoa, so we do too. Do not seem necessary in practice.
      FpcOptions.Add('-k-framework');
      FpcOptions.Add('-kCocoa');
      // TODO: Lazarus proposes such debugger options; should we pass them too? Why aren't they FPC defaults?
      // FpcOptions.Add('-gw2');
      // FpcOptions.Add('-godwarfsets');
    end;
  end;

  procedure AddDefines;
  var
    S: String;
  begin
    for S in Options.Defines do
      FpcOptions.Add('-d' + S);
  end;

  procedure CopyFromCache(const CompilationOutputPathFinal: String);
  var
    CachePathFull: String;
  begin
    CachePathFull := CachePath +
      CPUToString(Options.CPU) + '-' + OSToString(Options.OS) + PathDelim +
      CompilationModeToStr[Options.Mode] + PathDelim;
    if DirectoryExists(CachePathFull) then
    begin
      WritelnVerbose(Format('Using cache "%s" to speed up compilation', [CachePathFull]));
      CopyCacheContents(CachePathFull, CompilationOutputPathFinal);
    end;
  end;

var
  FpcOutput, FpcExe, CompilationOutputPathFinal, FpcStandardUnitsPath: String;
  FpcExitStatus: Integer;
  LinkerOutputBinaryPos: Integer;
begin
  FpcVer := FpcVersion;

  CompilationOutputPathFinal := CompilationOutputPath(coFpc, Options.OS, Options.CPU, WorkingDirectory);

  if Options.AllowCache then
    CopyFromCache(CompilationOutputPathFinal);

  FpcOptions := TCastleStringList.Create;
  try
    if CastleEnginePath <> '' then
      CastleEngineSrc := CastleEnginePath + 'src' + PathDelim
    else
      CastleEngineSrc := '';

    AddEngineSearchPaths;
    AddSearchPaths;
    AddEngineLibraryPaths;
    AddLibraryPaths;

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
    FpcOptions.Add('-vb'); // show full filenames, makes it easier for software (like VS Code problem matcher and CGE editor) to figure out the relevant file

    // do not show
    // Warning: Constructing a class "TCustomDictionaryEnumerator$4$crc6100464F" with abstract method "GetCurrent"
    // Warning: Constructing a class "TCustomDictionaryEnumerator$4$crcBD4794B2" with abstract method "DoMoveNext"
    // Update: No need to hide it anymore: our FilterFpcOutput handles it, and thus we don't need to hide this useful warning for user code
    // FpcOptions.Add('-vm04046');

    if FpcVer.AtLeast(3, 1, 1) then
    begin
      // do not show Warning: Symbol "TArrayHelper$1" is experimental
      // (only for FPC 3.1.1, for 3.0.x we fix this in our custom Generics.Collections unit)
      // Update: No need to hide it anymore: our FilterFpcOutput handles it, and thus we don't need to hide this useful warning for user code
      // FpcOptions.Add('-vm05063');

      // do not show
      // Note: Private type "TCustomPointersEnumerator$2<CASTLEVECTORSINTERNALSINGLE.TGenericVector2,CASTLEVECTORS.TCustomList$1$crc1D7BB6F0.PT>.T" never used
      // Update: No need to hide it anymore: our FilterFpcOutput handles it, and thus we don't need to hide this useful warning for user code
      // FpcOptions.Add('-vm5071');
    end;

    if FpcVer.AtLeast(3, 2, 0) then
    begin
      // do not show
      // Warning: function result variable of a managed type does not seem to be initialized
      // (a lot of false warnings since FPC 3.3.1)
      FpcOptions.Add('-vm5093');

      // do not show
      // Note: Call to subroutine "$1" marked as inline is not inlined
      // (In FPC 3.3.1, not in FPC 3.1.1 rev 38027)
      // (flood of notes after using Generics.Collections, but also from other units)
      FpcOptions.Add('-vm6058');
    end;

    if FpcVer.AtLeast(3, 3, 1) then
    begin
      // do not show
      // Warning: Local variable "$1" of a managed type does not seem to be initialized
      // (a lot of false warnings since FPC 3.3.1)
      FpcOptions.Add('-vm5089');

      // do not show
      // Warning: Variable "OutputFace" of a managed type does not seem to be initialized
      // (3 false warnings since FPC 3.3.1 in Kraft)
      FpcOptions.Add('-vm5090');
    end;

    if (Options.OS = iOS) and not FpcVer.AtLeast(3, 2, 2) then
      // Before FPC 3.2.2, the OS=iOS was designated as OS=darwin for FPC
      FpcOptions.Add('-Tdarwin')
    else
      FpcOptions.Add('-T' + OSToString(Options.OS));

    FpcOptions.Add('-P' + CPUToString(Options.CPU));

    { Release build and valgrind build are quite similar, they share many options. }
    if Options.Mode in [cmRelease, cmValgrind] then
    begin
      { Aarch64 optimizations exhibit bugs, on all OSes, with older FPC:

        - Bugs reproducible with FPC = 3.2.0 and FPC 3.3.1 (rev <= 48104).
        - Fixed in FPC 3.2.0 and later FPC unstable from GitLab.

        See https://trello.com/c/5ydB4MuA/113-enable-again-aarch64-optimizations .

        Testcases:

        - iOS:

          With FPC 3.0.3 on iOS/aarch64 (physical iOS, 64-bit)
          it seems all programs compiled with -O1 or -O2 crash at start.

        - Android:

          Reading some PNG fails (testcase: Silhouette), at least with -O2, fails.

          It is unsure with which FPC version this was reproducible.
          Probably some FPC 3.0.x.
          Michalis can no longer reproduce it with FPC 3.3.1 revision 42921
          (latest revision as of 2019/09/05).

        - Android and Nintendo Switch and iOS:

          TDrawableImage.Draw3x3 calculations are wildly wrong,
          and in effect TDrawableImage.Draw3x3 usually doesn't seem to draw anything.
          It seems like DrawWidth parameter is not received correctly,
          but workarounding it only uncovers more problems, it looks like
          the values in local Single variables there randomly change.

          This is reproducible with FPC 3.3.1 revision 42921
          (latest revision as of 2019/09/05).
      }
      if (Options.CPU = Aarch64) and
         (not FpcVer.AtLeast(3, 2, 2)) then
      begin
        FpcOptions.Add('-O-');
        WritelnWarning('Disabling optimizations, because they are buggy on Aarch64 with older FPC. Upgrade to FPC >= 3.2.2.');
      end else
      if Options.CPU = Wasm32 then
      begin
        { Wasm32 optimizations are buggy with FPC 3.3.1.
          FPC crashes with
            x3dnodes_coordinate3_1.inc(69,3) Fatal: Internal error 2018042601
          I assume this is known, even wiki page about Wasm32 says to use -O-
          TODO: web: submit FPC bug }
        FpcOptions.Add('-O-');
      end else
        FpcOptions.Add('-O2');
        // Not using -O3: Fails badly on 64-bit Raspberry Pi (Linux/Aarch64),
        // at TTestCastleComponentSerialize.TestCustomSerialization
        //FpcOptions.Add('-O3');
      FpcOptions.Add('-dRELEASE');
    end;

    case Options.Mode of
      cmRelease:
        begin
          FpcOptions.Add('-Xs');
        end;
      cmValgrind:
        begin
          { See https://castle-engine.io/profiling_using_valgrind
            for reasons of Valgrind options.

            For web:
            Do not pass -gv for WebAssembly, it causes errors
            "Can't find unit cmem used by castle_cache".
            And we need Valgrind mode to compile without errors,
            to enable "castle-engine cache --target=web". }

          if Options.OS = WasiP1 then
            Writeln('Warning: Valgrind is not supported on WebAssembly')
          else
            FpcOptions.Add('-gv');
          FpcOptions.Add('-gl');
        end;
      cmDebug:
        begin
          FpcOptions.Add('-Cr'); // Range checking, see https://github.com/modern-pascal/modern-pascal-introduction/wiki/What-are-range-and-overflow-checks-(and-errors)-in-Pascal
          if Options.CPU <> Wasm32 then
            FpcOptions.Add('-Co') // Overflow checking, see https://github.com/modern-pascal/modern-pascal-introduction/wiki/What-are-range-and-overflow-checks-(and-errors)-in-Pascal
          else
            { It seems that Overflow Checking is broken with WebAssembly,
              it causes exceptions
                EIntOverflow: Arithmetic overflow
                  $EEEEEEEE
              on definitely innocent operations, like TCastleWindow.GetColorBits
              when it sums up 0 + 0 + 0 (on Cardinal; are the unsigned Cardinal
              the reason for the problem?).
              Simplifying TCastleWindow.GetColorBits only causes EIntOverflow
              further down.
              TODO: web: submit FPC bug }
            FpcOptions.Add('-Co-');
          FpcOptions.Add('-Sa'); // Assertions
          FpcOptions.Add('-CR'); // Verify method calls
          FpcOptions.Add('-g');  // Debug info (automatic), for debuggers
          if Options.CPU <> Wasm32 then
            FpcOptions.Add('-gl') // Line info (in backtraces)
          else
            { Without this, compiling
                castle-engine compile --os=wasi --cpu=wasm32 --mode=debug
              fails with
                Fatal: Can't find unit lnfodwrf used by Program
              The default fpc.cfg contains clause to do -gl when DEBUG is defined,
              so we have to explicitly disable it with -gl-.
            }
            FpcOptions.Add('-gl-');
          FpcOptions.Add('-dDEBUG');
          { Disable -Ct (Stack checking) added to fpc.cfg in default
            fpcupdeluxe installation when DEBUG is defined.
            Because it crashes when application is run on iPhone,
            at least with FPC 3.2.2. }
          if IsIOS then
            FpcOptions.Add('-Ct-');
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('CompileFpc: Mode?');
      {$endif}
    end;

    if (Options.OS = Android) and (Options.CPU = arm) then
    begin
      { Our platform is armeabi-v7a, see ToolAndroidPackage
        comments about armeabi-v7a.
        Note: the option below seems not necessary when using -CfVFPV3?
        At least, nothing crashes.
        Possibly -CfVFPV3 implies this anyway. }
      FpcOptions.Add('-CpARMV7A');

      { Necessary to work fast.
        See https://castle-engine.io/android-FAQ#notes-about-compiling-with-hard-floats--cfvfpv3 }
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
        from TInternalSoundSource.SetMinGain. Reproducible with escape_universe.

        fpcupdeluxe default cross-compiler to Android also uses this. }
      //FpcOptions.Add('-CaEABIHF');
    end;

    if Options.DetectMemoryLeaks then // see https://castle-engine.io/memory_leaks
    begin
      FpcOptions.Add('-gl'); // HeapTrc
      FpcOptions.Add('-gh'); // LineInfo
    end;

    AddIOSOptions;
    AddMacOSOptions;
    AddDefines;
    FpcOptions.AddRange(Options.ExtraOptions);

    FpcOptions.Add(CompileFile);
    FpcOptions.Add('-FU' + CompilationOutputPathFinal);

    Writeln('FPC executing...');
    FpcExe := FindExeFpcCompiler(true, FpcStandardUnitsPath);

    if FpcStandardUnitsPath <> '' then
    begin
      FpcOptions.Add('-Fu' + FpcStandardUnitsPath);

      { Do not read system-wide FPC config, to allow this bundled FPC to coexist
        with your system-wide FPC installation without any relation. }
      FpcOptions.Add('-n');

      { As the bundled FPC has no config, by default it is rather silent.
        Add options to display info (and Warnings and Notes) during compilation to see progress. }
      FpcOptions.Add('-viwn');
    end;

    RunCommandIndirPassthrough(WorkingDirectory, FpcExe, FpcOptions.ToArray, FpcOutput, FpcExitStatus,
      Options.OverrideEnvironmentName, Options.OverrideEnvironmentValue, @FilterFpcOutput);
    if FpcExitStatus <> 0 then
    begin
      if (Pos('Fatal: Internal error', FpcOutput) <> 0) or
         (Pos('Error: Compilation raised exception internally', FpcOutput) <> 0) then
      begin
        FpcLazarusCrashRetry(WorkingDirectory, 'FPC', 'FPC');
        RunCommandIndirPassthrough(WorkingDirectory, FpcExe, FpcOptions.ToArray, FpcOutput, FpcExitStatus,
          Options.OverrideEnvironmentName, Options.OverrideEnvironmentValue, @FilterFpcOutput);
        if FpcExitStatus <> 0 then
          { do not retry compiling in a loop, give up }
          raise Exception.Create('Failed to compile');
      end else
        raise Exception.Create('Failed to compile');
    end;
    { Find the linker output from the Fpc output. The line starts with 'Linking '.

      TODO: This search assumes an English output of FPC.
      If user customizes FPC to use a different language (which can be done by
      fpc.cfg, out of our control), then this message is translated (see
      "exec_i_linking" in FPC translation files), and logic below will not work.

      - See https://github.com/castle-engine/castle-engine/pull/629#issuecomment-2886020746
        for possible ideas:
        "I tried finding another way to get the Options.LinkerOutputFile,
        but failed again. There could be a chance if ppaslink.sh file
        was not automatically deleted by fpc, as there is a ld command line inside,
        and the output file is after -o."

      - Maybe we could force the FPC language on the command-line?
    }
    LinkerOutputBinaryPos := Pos('Linking ', FpcOutput);
    if LinkerOutputBinaryPos <> 0 then
    begin
      LinkerOutputBinaryPos := LinkerOutputBinaryPos + StrLen('Linking ');
      Options.LinkerOutputFile := Copy(FpcOutput, LinkerOutputBinaryPos, Pos(NL, FpcOutput, LinkerOutputBinaryPos) - LinkerOutputBinaryPos);
    end else
      Writeln('Warning: build-tool could not recognize the linker output binary name, may cause error later.');
  finally FreeAndNil(FpcOptions) end;
end;

procedure Compile(Compiler: TCompiler; const WorkingDirectory, CompileFile: String; const Options: TCompilerOptions);
begin
  { resolve Compiler to something other than coAutodetect }
  if Compiler = coAutodetect then
  begin
    if FindExeFpcCompiler(false) <> '' then
      Compiler := coFpc
    else
    if FindDelphiPath(false) <> '' then
      Compiler := coDelphi
    else
      raise Exception.Create('Neither FPC nor Delphi found, cannot autodetect compiler');
  end;
  Assert(Compiler <> coAutodetect);

  case Compiler of
    coFpc: CompileFpc(WorkingDirectory, CompileFile, Options);
    coDelphi: CompileDelphi(WorkingDirectory, CompileFile, Options);
    else raise EInternalError.Create('Compile: Compiler?');
  end;
end;

procedure CompileDelphi(const WorkingDirectory, CompileFile: String; const Options: TCompilerOptions);
var
  CastleEngineSrc: String;
  DccOptions: TCastleStringList;

  { Add search namespaces, to keep basic units like SysUtils accessible
    without namespace. This follows the DPROJ settings generated by Delphi
    for new projects. }
  procedure AddSearchNamespaces;
  const
    SearchNamespacesGeneral = 'System;Xml;Data;Datasnap;Web;Soap';
    SearchNamespacesWindows = 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde';
  var
    SearchNamespaces: String;
  begin
    { calculate SearchNamespaces }
    SearchNamespaces := SearchNamespacesGeneral;
    if Options.OS in AllWindowsOSes then
      SearchNamespaces := SAppendPart(SearchNamespaces, ';', SearchNamespacesWindows);

    DccOptions.Add('-NS' + SearchNamespaces);
  end;

  procedure AddEnginePath(Path: String);
  begin
    Path := CastleEngineSrc + Path;
    if not DirectoryExists(Path) then
      WritelnWarning('Path', 'Path "%s" does not exist. Make sure that $CASTLE_ENGINE_PATH points to the directory containing Castle Game Engine sources.', [Path]);
    DccOptions.Add('-U' + Path);
    DccOptions.Add('-I' + Path);
  end;

  procedure AddEngineSearchPaths;
  var
    S: String;
  begin
    if CastleEngineSrc <> '' then
    begin
      for S in EnginePaths do
        AddEnginePath(S);
      for S in EnginePathsDelphi do
        AddEnginePath(S);
    end;
  end;

  procedure AddSearchPaths;
  var
    S: String;
  begin
    for S in Options.SearchPaths do
    begin
      DccOptions.Add('-U' + S);
      DccOptions.Add('-I' + S);
    end;
  end;

  procedure AddOutputPaths;
  var
    OutPath: String;
  begin
    OutPath := CompilationOutputPath(coDelphi, Options.OS, Options.CPU, WorkingDirectory);
    { Looks like DCCxxx cannot handle parameters with spaces? Answers
        Fatal: F1026 File not found: 'Game.dpr'
      for
        -NUC:\Users\michalis\Documents\Castle Game Engine Projects\my-new-project-delphi3d\castle-engine-output\compilation\delphi\x86_64-win64\
      Workaround: pass relative paths. }
    OutPath := ExtractRelativePath(InclPathDelim(WorkingDirectory), OutPath);
    DccOptions.Add('-NU' + OutPath);
    DccOptions.Add('-NH' + OutPath);
    DccOptions.Add('-NO' + OutPath);
    DccOptions.Add('-NB' + OutPath);
    DccOptions.Add('-NX' + OutPath);
  end;

  procedure AddDefines;
  var
    S: String;
  begin
    for S in Options.Defines do
      DccOptions.Add('-d' + S);
  end;

var
  DelphiPath, Dcc, DccExe: String;
  DccOutput: String;
  DccExitStatus: Integer;
begin
  DelphiPath := FindDelphiPath(true);

  { calculate Dcc, which is compiler basename with OS/CPU suffix.
    The combinations confirmed to be possible in Delphi 11:

      dcc32 - Win32
      dcc64 - Win64
      dccaarm - Android/Arm
      dccaarm64 - Android/Arm64
      dcciosarm64 - iOS/Arm64
      dcclinux64 - Linux/x86_64
      dccosx64 - macos/x86_64
      dccosxarm64 - macos/Arm64
  }
  Dcc := 'dcc';
  case Options.OS of
    Win32, Win64: ;
    Android: Dcc += 'a';
    iOS    : Dcc += 'ios';
    Linux  : Dcc += 'linux';
    MacOSX : Dcc += 'osx';
    else raise Exception.CreateFmt('Operating system "%s" not supported by Delphi', [OSToString(Options.OS)]);
  end;
  case Options.CPU of
    i386   : Dcc += '32';
    x86_64 : Dcc += '64';
    Arm    : Dcc += 'arm';
    Aarch64: Dcc += 'arm64';
    else raise Exception.CreateFmt('CPU "%s" not supported by Delphi', [CPUToString(Options.CPU)]);
  end;

  DccExe := DelphiPath + 'bin' + PathDelim + Dcc + ExeExtension;
  if not RegularFileExists(DccExe) then
    raise Exception.CreateFmt('Cannot find Delphi compiler for this OS/CPU: %s', [DccExe]);

  if CastleEnginePath <> '' then
    CastleEngineSrc := CastleEnginePath + 'src' + PathDelim
  else
    CastleEngineSrc := '';

  DccOptions := TCastleStringList.Create;
  try
    Writeln('Delphi compiler executing...');

    AddSearchNamespaces;
    AddEngineSearchPaths;
    AddSearchPaths;
    AddOutputPaths;

    // TODO: Do something more useful for release optimizations or debugging
    case Options.Mode of
      cmRelease, cmValgrind: DccOptions.Add('-dRELEASE');
      cmDebug              : DccOptions.Add('-dDEBUG');
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('CompileDelphi: Mode?');
      {$endif}
    end;

    AddDefines;
    DccOptions.AddRange(Options.ExtraOptions);

    DccOptions.Add(CompileFile);

    RunCommandIndirPassthrough(WorkingDirectory, DccExe, DccOptions.ToArray, DccOutput, DccExitStatus);
    if DccExitStatus <> 0 then
      raise Exception.Create('Failed to compile');
  finally FreeAndNil(DccOptions) end;
end;

procedure RunLazbuild(const WorkingDirectory: String; const LazbuildOptions: TCastleStringList);
var
  LazbuildExe: String;
  LazbuildOutput: String;
  LazbuildExitStatus: Integer;
begin
  LazbuildExe := FindExeLazarus('lazbuild');
  if LazbuildExe = '' then
    raise EExecutableNotFound.Create('Cannot find "lazbuild" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set Lazarus location in "Preferences".');

  RunCommandIndirPassthrough(WorkingDirectory,
    LazbuildExe, LazbuildOptions.ToArray, LazbuildOutput, LazbuildExitStatus, '', '', @FilterFpcOutput);
  if LazbuildExitStatus <> 0 then
  begin
    { Old lazbuild can fail with exception like this:

        An unhandled exception occurred at $0000000000575F5F:
        EAccessViolation: Access violation
          $0000000000575F5F line 590 of exttools.pas
          $000000000057A027 line 1525 of exttools.pas
          $000000000057B231 line 1814 of exttools.pas

      Simply retrying works.
    }
    if (Pos('Fatal: Internal error', LazbuildOutput) <> 0) or
       (Pos('EAccessViolation: Access violation', LazbuildOutput) <> 0) then
    begin
      FpcLazarusCrashRetry(WorkingDirectory, 'Lazarus (lazbuild)', 'Lazarus');
      RunCommandIndirPassthrough(WorkingDirectory,
        LazbuildExe, LazbuildOptions.ToArray, LazbuildOutput, LazbuildExitStatus, '', '', @FilterFpcOutput);
      if LazbuildExitStatus <> 0 then
        { do not retry compiling in a loop, give up }
        raise Exception.Create('Failed to compile');
    end else
      raise Exception.Create('Failed to compile');
  end else

  // lazbuild from Lazarus 1.6.4 doesn't support add-package-link
  if (Pos('Invalid option at position 3: "add-package-link"', LazbuildOutput) <> 0) and
     (LazbuildOptions.IndexOf('--add-package-link') <> -1) then
  begin
    Writeln('lazbuild does not support --add-package-link, retrying without it');
    LazbuildOptions.Delete(LazbuildOptions.IndexOf('--add-package-link'));
    RunCommandIndirPassthrough(WorkingDirectory,
      LazbuildExe, LazbuildOptions.ToArray, LazbuildOutput, LazbuildExitStatus, '', '', @FilterFpcOutput);
    if LazbuildExitStatus <> 0 then
      { do not retry compiling in a loop, give up }
      raise Exception.Create('Failed to compile');
  end;
end;

procedure RunLazbuild(const WorkingDirectory: String; const LazbuildOptions: array of String);
var
  L: TCastleStringList;
begin
  L := TCastleStringList.Create;
  try
    L.Assign(LazbuildOptions);
    RunLazbuild(WorkingDirectory, L);
  finally FreeAndNil(L) end;
end;

procedure CompileLazbuild(const WorkingDirectory, LazarusProjectFile: String; const Options: TCompilerOptions);
var
  LazbuildOptions: TCastleStringList;

  procedure LazbuildAddPackage(const LpkFileName: String);
  begin
    LazbuildOptions.Clear;
    LazbuildOptions.Add('--add-package-link');
    LazbuildOptions.Add(CastleEnginePath + LpkFileName);
    RunLazbuild(WorkingDirectory, LazbuildOptions);
  end;

begin
  LazbuildOptions := TCastleStringList.Create;
  try
    // register CGE packages first
    if CastleEnginePath <> '' then
    begin
      LazbuildAddPackage('packages/lazarus/castle_engine_base.lpk');
      LazbuildAddPackage('packages/lazarus/castle_engine_window.lpk');
      LazbuildAddPackage('packages/lazarus/castle_engine_lcl.lpk');
      LazbuildAddPackage('packages/lazarus/castle_engine_editor_components.lpk');
    end;

    LazbuildOptions.Clear;
    LazbuildOptions.Add('--os=' + OSToString(Options.OS));
    LazbuildOptions.Add('--cpu=' + CPUToString(Options.CPU));
    { // Do not pass --build-mode, as project may not have it defined.
    if Options.Mode = cmDebug then
      LazbuildOptions.Add('--build-mode=Debug')
    else
      LazbuildOptions.Add('--build-mode=Release');
    }

    { For historic reasons, Lazarus < 2.2 defaults to Carbon on macOS,
      even on 64-bit macOS where you cannot link with Carbon.
      And since macOS Catalina (10.15) all applications *must* be 64-bit
      (32-bit is no longer supported) so this is important.
      So we change it to cocoa.

      TODO: This likely prevents the project from using it's own,
      custom widgetset in case of macOS.

      But there doesn't seem any better way of fixing this per-project.
      I cannot use "Custom Options",

        if (TargetOS='darwin') and (TargetCPU='x86_64') then
          LCLWidgetType := 'cocoa';

      -- it looks like LCLWidgetType is ignored in "Custom Options".

      I cannot use "Additions and Overrides", as it doesn't seem to allow
      to choose widgetset per-platform (like per-OS, or per-OS-and-CPU).
      It would only allow to set Cocoa always.
      Or in a specific build mode, but then I would need to
      - maintain this build mode in my examples,
      - 2 times (for both Debug/Release, I would need a copy DebugMacOS and ReleaseMacOS)
      - and require it in all user projects (this is not acceptable).
    }
    if (Options.OS = darwin) and ((Options.CPU = X86_64) or (Options.CPU = Aarch64)) then
      LazbuildOptions.Add('--widgetset=cocoa');
    LazbuildOptions.Add(LazarusProjectFile);

    RunLazbuild(WorkingDirectory, LazbuildOptions);
  finally FreeAndNil(LazbuildOptions) end;
end;

function CompilationOutputPath(const Compiler: TCompiler;
  const OS: TOS; const CPU: TCPU;
  const WorkingDirectory: String): String;
begin
  Result := TempOutputPath(WorkingDirectory) + 'compilation' + PathDelim;
  if Compiler = coDelphi then
    Result += 'delphi' + PathDelim;
  Result += CPUToString(CPU) + '-' + OSToString(OS) + PathDelim;
  CheckForceDirectories(Result);
end;

const
  CompilationModeNames: array [TCompilationMode] of string =
  ('release', 'valgrind', 'debug');

function ModeToString(const M: TCompilationMode): String;
begin
  Result := CompilationModeNames[M];
end;

function StringToMode(const S: String): TCompilationMode;
begin
  for Result in TCompilationMode do
    if AnsiSameText(CompilationModeNames[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid compilation mode name "%s"', [S]);
end;

end.
