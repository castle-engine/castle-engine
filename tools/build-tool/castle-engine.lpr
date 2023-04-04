{
  Copyright 2014-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Build and package tool for Castle Game Engine programs.
  Call with --help for detailed usage instructions.
}

{$I castleconf.inc}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

uses SysUtils,
  ToolDisableDynamicLibraries, //< use this unit early, before any other CGE unit
  CastleUtils, CastleParameters, CastleFindFiles, CastleLog,
  CastleFilesUtils, CastleURIUtils, CastleStringUtils,
  CastleApplicationProperties,
  ToolPackageFormat, ToolProject, ToolCompile, ToolIOS, ToolAndroid, ToolManifest,
  ToolNintendoSwitch, ToolCommonUtils, ToolArchitectures, ToolUtils, ToolProcess,
  ToolCache;

var
  Target: TTarget;
  OS: TOS;
  CPU: TCPU;
  Mode: TCompilationMode = cmRelease;
  AssumeCompiled: boolean = false;
  Fast: boolean = false;
  CompilerExtraOptions: TCastleStringList;
  PackageFormat: TPackageFormat = pfDefault;
  PackageNameIncludeVersion: Boolean = true;
  UpdateOnlyCode: Boolean = false;
  CleanAll: Boolean = false;
  WaitForProcessId: TProcessId = 0;
  GuiErrors: Boolean = false;
  OverrideCompiler: TCompiler = DefaultCompiler;
  GuidFromName: Boolean = false;

const
  Options: array [0..23] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0 ; Long: 'target'; Argument: oaRequired),
    (Short: #0 ; Long: 'os'; Argument: oaRequired),
    (Short: #0 ; Long: 'cpu'; Argument: oaRequired),
    (Short: 'V'; Long: 'verbose'; Argument: oaNone),
    (Short: #0 ; Long: 'mode'; Argument: oaRequired),
    (Short: #0 ; Long: 'assume-compiled'; Argument: oaNone),
    (Short: #0 ; Long: 'fast'; Argument: oaNone),
    (Short: #0 ; Long: 'plugin'; Argument: oaNone),
    (Short: #0 ; Long: 'fpc-version-iphone-simulator'; Argument: oaRequired),
    (Short: #0 ; Long: 'compiler-option'; Argument: oaRequired),
    (Short: #0 ; Long: 'output'; Argument: oaRequired),
    (Short: #0 ; Long: 'project'; Argument: oaRequired),
    (Short: #0 ; Long: 'package-format'; Argument: oaRequired),
    (Short: #0 ; Long: 'package-name-no-version'; Argument: oaNone),
    (Short: #0 ; Long: 'update-only-code'; Argument: oaNone),
    (Short: #0 ; Long: 'ios-simulator'; Argument: oaNone),
    (Short: #0 ; Long: 'all'; Argument: oaNone),
    (Short: #0 ; Long: 'manifest-name'; Argument: oaRequired),
    (Short: #0 ; Long: 'wait-for-process-exit'; Argument: oaRequired),
    (Short: #0 ; Long: 'gui-errors'; Argument: oaNone),
    (Short: #0 ; Long: 'compiler'; Argument: oaRequired),
    (Short: #0 ; Long: 'guid-from-name'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);

  { Call SetCurrentDir to open project within specified directory. }
  procedure ChangeProjectDir(const DirOrManifestFile: String);
  var
    Dir: String;
  begin
    if ExtractFileName(DirOrManifestFile) = ManifestName then
      Dir := ExtractFilePath(DirOrManifestFile)
    else
      Dir := DirOrManifestFile;
    if not SetCurrentDir(Dir) then
      raise EInvalidParams.CreateFmt('Cannot enter project directory "%s"', [Dir]);
  end;

begin
  case OptionNum of
    0 : begin
          Writeln(
            'castle-engine: Build and package Castle Game Engine programs.' +NL+
            NL+
            'Call with the current directory set to your project, like this:' +NL+
            '  castle-engine [OPTIONS]... COMMAND' +NL+
            NL+
            'Possible commands:' +NL+
            NL+
            'create-manifest' +NL+
            '    Creates simple CastleEngineManifest.xml with guessed values.' +NL+
            NL+
            'compile' +NL+
            '    Compile project.' +NL+
            '    By default compiles for the current OS / current CPU (' + OSToString(DefaultOS) + ' / ' + CPUToString(DefaultCPU) + ').' +NL+
            '    You can use --os / --cpu options to compile to some other OS / CPU.' +NL+
            '    You can use --target to compile for a collection of OS / CPU' +NL+
            '    combination (like "iOS" or "Android").' +NL+
            NL+
            'package' +NL+
            '    Package the application into the best archive format for given' +NL+
            '    operating system (OS) / processor (CPU) / target.' +NL+
            '    The OS, CPU and "target" can be changed just like at "compile".' +NL+
            NL+
            'install' +NL+
            '    Install the application created by previous "package" call.' +NL+
            '    Useful when OS is "android", it installs' +NL+
            '    the apk package created by previous "package" call' +NL+
            '    for Android. Useful for quick testing of your app on a device' +NL+
            '    connected through USB.' +NL+
            NL+
            'run' +NL+
            '    Run the application. ' +NL+
            '    On some platforms, it requires installing the application first' +NL+
            '    (e.g. on Android, where we install and run on a device' +NL+
            '    connected through USB). So run the "install" command before.' +NL+
            '    On other platforms (e.g. standalone Windows, Linux, macOS...),' +NL+
            '    it simply runs the last compiled application.' +NL+
            '    So just "compile" the application first.' +NL+
            NL+
            'package-source' +NL+
            '    Package the source code of the application.' +NL+
            NL +
            'clean' +NL+
            '    Clean leftover files from compilation and packaging.' +NL+
            '    Does not remove final packaging output.' +NL+
            NL+
            'simple-compile' +NL+
            '    Compile the Object Pascal file (unit/program/library) given' +NL+
            '    as a parameter. This does not handle the Castle Game Engine projects' +NL+
            '    defined by CastleEngineManifest.xml files.' +NL+
            '    It merely calls "fpc" with proper command-line options for' +NL+
            '    units/programs/libraries using our engine.' +NL+
            '    Use this instead of "compile" only if there''s some good reason' +NL+
            '    you don''t want to use CastleEngineManifest.xml to your project.' +NL+
            NL+
            'auto-generate-textures' +NL+
            '    Create GPU-compressed versions of textures,' +NL+
            '    for the textures mentioned in <auto_compressed_textures>' +NL+
            '    inside the file data/material_properties.xml.' +NL+
            NL+
            'auto-generate-clean' +NL+
            '    Clear "auto_compressed" subdirectories, that should contain only' +NL+
            '    the output created by "auto-generate-textures" target.' +NL+
            NL+
            'generate-program' +NL+
            '    Generate files to edit and run this project in Lazarus: lpr, lpi, castleautogenerated unit.' +NL+
            '    Depends on game_units being defined in the CastleEngineManifest.xml.' +NL+
            NL+
            'editor' +NL+
            '    Run Castle Game Engine Editor within this project, with possible' +NL+
            '    project-specific components.' +NL+
            NL+
            'editor-rebuild-if-needed' +NL+
            '    Internal. 1st part of "editor" command.' + NL +
            NL+
            'editor-run [--wait-for-process-exit PROCESS-ID]' +NL+
            '    Internal. 2nd part of "editor" command.' + NL +
            NL+
            'output' +NL+
            '    Output some project information (from the manifest).' + NL +
            '    Next parameter determines the information:' + NL +
            '      version' + NL +
            '      version-code' + NL +
            NL+
            'cache' +NL+
            '    Create cache to speed up future compilations.' + NL +
            NL+
            'cache-clean' +NL+
            '    Remove the cache directory.' + NL +
            NL+
            'Available options are:' +NL+
            OptionDescription('-h / --help', 'Print this help message and exit.') + NL +
            OptionDescription('-v / --version', 'Print the version number and exit.') + NL +
            OptionDescription('-V / --verbose',
              'Verbose mode, output contains e.g. list of packaged files.') +NL+
            OptionDescription('--mode=debug|release',
              'Compilation mode, used by "compile" and "package" commands. Also packaging mode on some platforms (right now, Android). By default "release".') +NL+
            OptionDescription('--assume-compiled',
              'Do not automatically do "clean" and "compile" before "package". Instead assume that compiled executable for given OS/CPU/mode is already present in the package directory.') +NL+
            OptionDescription('--fast',
              'Do not "clean" before "package". Recompile only what changed. This is faster for development, but cannot guarantee that everything is recompiled in a release mode.') +NL+
            OptionDescription('--plugin',
              'Compile/package/install a browser NPAPI plugin. DEPRECATED.') +NL+
            OptionDescription('--fpc-version-iphone-simulator VERSION',
              'When compiling for iPhone Simulator, we pass -V<VERSION> to the "fpc" command-line. This is necessary if you use the official "FPC for iOS" package (see the "Getting Started - iOS.rtf" inside the "FPC for iOS" dmg for explanation). You can set this to "auto" (this is the default) to auto-detect this based on regular FPC version. Or you can set this to a particular version, like "3.0.5". Or you can set this to empty "" to avoid passing any -V<VERSION> (suitable for FPC 3.1.1).') +NL+
            OptionDescription('--compiler-option=PARAM',
              'Extra parameter for "fpc" command line. For example --compiler-option=-dUSE_MOUSE will add -dUSE_MOUSE. You can use this parameter multiple times.') +NL+
            OptionDescription('--output=DIR',
              'Where to place the output executables, packages, and the "castle-engine-output" directory with temporary generated files.') +NL+
            OptionDescription('--project=DIR',
              'Where to search for the project (CastleEngineManifest.xml file). By default we search in the current directory. The argument can either be a directory, or a filename of CastleEngineManifest.xml file.') +NL+
            OptionDescription('--package-format=FORMAT',
              'Use with "package" command to customize the result.' + NL +
              'Available FORMAT values: ' +NL+
              NL +
              '- default (platform specific; on most platforms creates a zip/tar.gz archive; on Android creates APK; on iOS creates Xcode project)' +NL+
              '- zip (pack all files into zip)' +NL+
              '- tar.gz (pack all files into tar.gz)' +NL+
              '- directory (put all files into a new subdirectory)' +NL+
              '- android-apk (only on Android: create APK)' +NL+
              '- android-app-bundle (only on Android: create AAB)' +NL+
              '- ios-archive-ad-hoc (only on iOS: archive using "ad-hoc" method to IPA file)' +NL+
              '- ios-archive-development (only on iOS: archive using "development" method)' +NL+
              '- ios-archive-app-store (only on iOS: distributes the application to the TestFlight and the AppStore)' +NL+
              '') + NL +
            OptionDescription('--package-name-no-version',
              'Use with "package" command. The resulting file/directory name will not contain the version number.') + NL +
            OptionDescription('--update-only-code',
              'Use with "package" command. Makes the packaging faster, as you guarantee that only the Pascal code have changed since last packaging (so you did not change e.g. data/ or CastleEngineManifest.xml).') + NL +
            OptionDescription('--ios-simulator',
              'Use with "package" command when --target=iOS. Allows to run the project on iOS simulator.') + NL +
            OptionDescription('--all',
              'Use with "auto-generate-clean" command. Indicates to clean everything auto-generated. By default we only clean unused files from "auto_generated" directories.') +NL+
            OptionDescription('--manifest-name=AlternativeManifest.xml',
              'Search and use given "AlternativeManifest.xml" file instead of standard "CastleEngineManifest.xml". Useful if you need to maintain completely different project configurations for any reason.') +NL+
            OptionDescription('--wait-for-process-exit=PROCESS-ID',
              'Internal, useful with "editor-run".') +NL+
            OptionDescription('--gui-errors',
              'Show errors as GUI boxes. On Unix, requires "zenity" installed.') +NL+
            OptionDescription('--compiler=COMPILER',
              'Select compiler: "autodetect", "fpc", "delphi".') +NL+
            OptionDescription('--guid-from-name',
              'Use with "generate-program" command. Will generate stable GUID (in Delphi DPROJ) from project''s qualified name.') +NL+
            TargetOptionHelp + NL +
            OSOptionHelp + NL +
            CPUOptionHelp + NL +
            NL+
            'Full documentation on' + NL +
            'https://castle-engine.io/build_tool' + NL +
            NL+
            ApplicationProperties.Description);
          Halt;
        end;
    1 : begin
          // include ApplicationName in version, good for help2man
          Writeln(ApplicationName + ' ' + ApplicationProperties.Version);
          Halt;
        end;
    2 : Target := StringToTarget(Argument);
    3 : OS := StringToOS(Argument);
    4 : CPU := StringToCPU(Argument);
    5 : Verbose := true;
    6 : Mode := StringToMode(Argument);
    7 : AssumeCompiled := true;
    8 : Fast := true;
    9 : WritelnWarning('NPAPI plugin is no longer available, ignoring --plugin');
    10: FPCVersionForIPhoneSimulator := Argument;
    11: CompilerExtraOptions.Add(Argument);
    12: OutputPathBase := Argument;
    13: ChangeProjectDir(Argument);
    14: PackageFormat := StringToPackageFormat(Argument);
    15: PackageNameIncludeVersion := false;
    16: UpdateOnlyCode := true;
    17: IosSimulatorSupport := true;
    18: CleanAll := true;
    19: ManifestName := Argument;
    20: WaitForProcessId := StrToInt64(Argument);
    21: GuiErrors := true;
    22: OverrideCompiler := StringToCompiler(Argument);
    23: GuidFromName := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ For some operations (like creating an Android project), the tool uses
  ApplicationData files. So make sure that ApplicationData is correct.
  We can use CastleEnginePath (that used $CASTLE_ENGINE_PATH environment variable)
  for this. }
procedure AdjustApplicationData;
var
  DataPath: string;
begin
  if CastleEnginePath <> '' then
  begin
    DataPath := CastleEnginePath +
      'tools' + PathDelim + 'build-tool' + PathDelim + 'data' + PathDelim;
    if DirectoryExists(DataPath) then
      ApplicationDataOverride := FilenameToURISafe(DataPath);

    { We do not complain when CastleEnginePath is empty or doesn't contain
      valid data, because CastleEnginePath already did that. }
  end;
end;

procedure Run;
var
  Command, S, FileName: string;
  Project: TCastleProject;
  RestOfParameters: TCastleStringList;
  SimpleCompileOptions: TCompilerOptions; // used only when command is "simple-compile"
begin
  ApplicationProperties.ApplicationName := 'castle-engine';
  ApplicationProperties.Version := CastleEngineVersion;
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  OS := DefaultOS;
  CPU := DefaultCPU;
  CompilerExtraOptions := TCastleStringList.Create();

  { parse parameters }
  Parameters.Parse(Options, @OptionProc, nil);
  if Parameters.High < 1 then
    raise EInvalidParams.Create('Not enough command-line parameters, expected a COMMAND to perform. Use --help to get usage information') else
  Command := Parameters[1];

  { check OS, CPU }
  if not OSCPUSupported[OS, CPU] then
  begin
    S := Format('The combination of operating system "%s" and processor "%s" is not possible (or not supported by FPC)',
      [OSToString(OS), CPUToString(CPU)]);
    if OS in AllWindowsOSes then
      S += '. Note: in case of Windows 64-bit, remember to specify both OS and CPU like this: "--os=win64 --cpu=x86_64"';
    raise EInvalidParams.Create(S);
  end;

  AdjustApplicationData;

  if Command = 'simple-compile' then
  begin
    Parameters.CheckHigh(2);
    FileName := Parameters[2];
    { use GetCurrentDir as WorkingDir,
      so calling "castle-engine simple-compile somesubdir/myunit.pas" works.
      Working dir for FPC must be equal to our own working dir. }
    SimpleCompileOptions := TCompilerOptions.Create;
    try
      SimpleCompileOptions.OS := OS;
      SimpleCompileOptions.CPU := CPU;
      SimpleCompileOptions.DetectMemoryLeaks := false;
      SimpleCompileOptions.Mode := Mode;
      SimpleCompileOptions.ExtraOptions.AddRange(CompilerExtraOptions);
      case Target of
        targetCustom        : Compile(OverrideCompiler, GetCurrentDir, FileName, SimpleCompileOptions);
        targetAndroid       : CompileAndroid(OverrideCompiler, nil, GetCurrentDir, FileName, SimpleCompileOptions);
        targetIOS           : CompileIOS(OverrideCompiler, GetCurrentDir, FileName, SimpleCompileOptions);
        targetNintendoSwitch: CompileNintendoSwitch(GetCurrentDir, FileName, SimpleCompileOptions);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('Operation not implemented for this target');
        {$endif}
      end;
    finally FreeAndNil(SimpleCompileOptions) end;
  end else
  if Command = 'cache' then
  begin
    Parameters.CheckHigh(1);
    CacheCreate(OverrideCompiler, Target, OS, CPU);
  end else
  if Command = 'cache-clean' then
  begin
    Parameters.CheckHigh(1);
    CacheClean;
  end else
  begin
    if (Command <> 'run') and (Command <> 'output') then
      Parameters.CheckHigh(1);
    Project := TCastleProject.Create;
    try
      if Command = 'create-manifest' then
        Project.DoCreateManifest
      else
      if Command = 'compile' then
        Project.DoCompile(OverrideCompiler, Target, OS, CPU, Mode, CompilerExtraOptions)
      else
      if Command = 'package' then
      begin
        if not AssumeCompiled then
        begin
          if not Fast then
            Project.DoClean;
          Project.DoCompile(OverrideCompiler, Target, OS, CPU, Mode, CompilerExtraOptions);
        end;
        Project.DoPackage(Target, OS, CPU, Mode, PackageFormat, PackageNameIncludeVersion, UpdateOnlyCode);
      end else
      if Command = 'install' then
        Project.DoInstall(Target, OS, CPU, Mode, PackageFormat, PackageNameIncludeVersion)
      else
      if Command = 'run' then
      begin
        RestOfParameters := TCastleStringList.Create;
        try
          RestOfParameters.Text := Parameters.Text;
          RestOfParameters.Delete(0); // remove our own name
          RestOfParameters.Delete(0); // remove "run"
          Project.DoRun(Target, OS, CPU, RestOfParameters);
        finally FreeAndNil(RestOfParameters) end;
      end else
      if Command = 'package-source' then
      begin
        Project.DoClean;
        Project.DoPackageSource(PackageFormat, PackageNameIncludeVersion);
      end else
      if Command = 'clean' then
        Project.DoClean
      else
      if Command = 'auto-generate-textures' then
        Project.DoAutoGenerateTextures
      else
      if Command = 'auto-generate-clean' then
        Project.DoAutoGenerateClean(CleanAll)
      else
      if Command = 'generate-program' then
        Project.DoGenerateProgram(GuidFromName)
      else
      if Command = 'editor' then
        Project.DoEditor
      else
      if Command = 'editor-rebuild-if-needed' then
        Project.DoEditorRebuildIfNeeded
      else
      if Command = 'editor-run' then
        Project.DoEditorRun(WaitForProcessId)
      else
      if Command = 'output' then
      begin
        Parameters.CheckHigh(2);
        Project.DoOutput(Parameters[2]);
      end else
        raise EInvalidParams.CreateFmt('Invalid COMMAND to perform: "%s". Use --help to get usage information', [Command]);
    finally FreeAndNil(Project) end;
  end;

  FreeAndNil(CompilerExtraOptions);
end;

begin
  try
    Run;
  except
    on E: TObject do
    begin
      { In case of exception, write nice message and exit with non-zero status,
        without dumping any stack trace (because it's normal for build tool to
        exit with exception in case of project/environment error, not a bug). }
      if GuiErrors then
        ErrorBox(ExceptMessage(E))
      else
        Writeln(ErrOutput, ExceptMessage(E));
      Halt(1);
    end;
  end;
end.
