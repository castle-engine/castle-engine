{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Creating Android package. }
unit ToolAndroidPackage;

interface

uses CastleUtils, CastleStringUtils,
  ToolUtils, ToolArchitectures, ToolCompile, ToolProject;

procedure CreateAndroidPackage(const Project: TCastleProject;
  const OS: TOS; const CPU: TCPU; const SuggestedPackageMode: TCompilationMode;
  const Files: TCastleStringList);

procedure InstallAndroidPackage(const Name, QualifiedName: string);

procedure RunAndroidPackage(const Project: TCastleProject);

implementation

uses SysUtils, Classes,
  CastleURIUtils, CastleWarnings, CastleFilesUtils, CastleImages,
  ToolEmbeddedImages, ExtInterpolation;

const
  PackageModeToName: array [TCompilationMode] of string = (
    'release',
    'release' { no valgrind support for Android },
    'debug');

procedure CreateAndroidPackage(const Project: TCastleProject;
  const OS: TOS; const CPU: TCPU; const SuggestedPackageMode: TCompilationMode;
  const Files: TCastleStringList);
var
  AndroidProjectPath: string;

  { Some utility procedures PackageXxx work just like Xxx,
    but target filename should not contain prefix AndroidProjectPath,
    and they avoid overwriting stuff already existing (in case CastleEngineManifest.xml
    contained explicit android_project path). }

  procedure PackageCheckForceDirectories(const Dirs: string);
  begin
    CheckForceDirectories(AndroidProjectPath + Dirs);
  end;

  procedure PackageSaveImage(const Image: TCastleImage; const FileName: string);
  begin
    PackageCheckForceDirectories(ExtractFilePath(FileName));
    if not FileExists(AndroidProjectPath + FileName) then
      SaveImage(Image, FilenameToURISafe(AndroidProjectPath + FileName)) else
    if Verbose then
      Writeln('Not overwriting custom ' + FileName);
  end;

  procedure PackageSmartCopyFile(const FileFrom, FileTo: string);
  begin
    PackageCheckForceDirectories(ExtractFilePath(FileTo));
    if not FileExists(AndroidProjectPath + FileTo) then
      SmartCopyFile(FileFrom, AndroidProjectPath + FileTo) else
    if Verbose then
      Writeln('Not overwriting custom ' + FileTo);
  end;

{
  procedure PackageStringToFile(const FileTo, Contents: string);
  begin
    if not FileExists(AndroidProjectPath + FileTo) then
    begin
      PackageCheckForceDirectories(ExtractFilePath(FileTo));
      StringToFile(FileTo, Contents);
    end else
    if Verbose then
      Writeln('Not overwriting custom ' + FileTo);
  end;
}

  { Generate simple text stuff for Android project from templates. }
  procedure GenerateFromTemplates;
  var
    TemplatePath, DestinationPath: string;
    I: Integer;
  begin
    { calculate absolute DestinationPath.
      Use CombinePaths, as AndroidProjectPath may come from
      CastleEngineManifest.xml attribute android_project, in which case it *may*
      (does not have to) be relative to project dir. }
    DestinationPath := CombinePaths(Project.Path, AndroidProjectPath);

    { add Android project core directory }
    case Project.AndroidProjectType of
      apBase      : TemplatePath := 'android/base/';
      apIntegrated: TemplatePath := 'android/integrated/';
      else raise EInternalError.Create('GenerateFromTemplates:Project.AndroidProjectType unhandled');
    end;
    Project.ExtractTemplate(TemplatePath, DestinationPath);

    if Project.AndroidProjectType = apIntegrated then
    begin
      { add Android project components }
      for I := 0 to Project.AndroidComponents.Count - 1 do
      begin
        TemplatePath := 'android/integrated-components/' + Project.AndroidComponents[I].Name;
        Project.ExtractTemplate(TemplatePath, DestinationPath);
      end;

      { add sound component, if sound library is in Dependencies }
      if depSound in Project.Dependencies then
      begin
        TemplatePath := 'android/integrated-components/sound';
        Project.ExtractTemplate(TemplatePath, DestinationPath);
      end;
    end;
  end;

  function FindSubprojects: TCastleStringList;
  var
    FileRec: TSearchRec;
    SearchError, I: integer;
  begin
    Result := TCastleStringList.Create;
    try
      SearchError := FindFirst(AndroidProjectPath + '*', faDirectory, FileRec);
      try
        while SearchError = 0 do
        begin
          if ((FileRec.Attr and faDirectory) <> 0) and
             (not SpecialDirName(FileRec.Name)) and
             FileExists(InclPathDelim(AndroidProjectPath + FileRec.Name) + 'AndroidManifest.xml') then
            Result.Add(FileRec.Name);
          SearchError := FindNext(FileRec);
        end;
      finally FindClose(FileRec) end;

      if Verbose then
      begin
        Writeln('Found ', Result.Count, ' Android subprojects');
        for I := 0 to Result.Count - 1 do
          Writeln('Found Android subproject: ', Result[I]);
      end;
    except FreeAndNil(Result) end;
  end;

  { Try to find "android" tool executable, exception if not found. }
  function AndroidExe: string;
  begin
    { try to find in $ANDROID_HOME }
    Result := AddExeExtension(InclPathDelim(GetEnvironmentVariable('ANDROID_HOME')) +
      'tools' + PathDelim + 'android');
    { try to find on $PATH }
    if not FileExists(Result) then
      Result := FindExe('android');
    if Result = '' then
      raise Exception.Create('Cannot find "android" executable on $PATH, or within $ANDROID_HOME. Install Android SDK and make sure that "android" executable is on $PATH, or that $ANDROID_HOME environment variable is set correctly.');
  end;

  procedure GenerateIcons;
  var
    Icon: TCastleImage;

    procedure SaveResized(const Size: Integer; const S: string);
    var
      R: TCastleImage;
      Dir: string;
    begin
      R := Icon.MakeResized(Size, Size, rniLanczos);
      try
        Dir := 'res' + PathDelim + 'drawable-' + S + 'dpi';
        PackageSaveImage(R, Dir + PathDelim + 'ic_launcher.png');
      finally FreeAndNil(R) end;
    end;

  begin
    Icon := Project.Icons.FindReadable;
    if Icon = nil then
    begin
      OnWarning(wtMinor, 'Icon', 'No icon in a format readable by our engine (for example, png or jpg) is specified in CastleEngineManifest.xml. Using default icon.');
      Icon := DefaultIcon;
    end;
    try
      SaveResized(36, 'l');
      SaveResized(48, 'm');
      SaveResized(72, 'h');
      SaveResized(96, 'xh');
      SaveResized(144, 'xxh');
    finally
      if Icon = DefaultIcon then
        Icon := nil else
        FreeAndNil(Icon);
    end;
  end;

  procedure GenerateAssets;
  var
    I: Integer;
    FileFrom, FileTo: string;
  begin
    for I := 0 to Files.Count - 1 do
    begin
      FileFrom := Project.DataPath + Files[I];
      FileTo := 'assets' + PathDelim + Files[I];
      PackageSmartCopyFile(FileFrom, FileTo);
      if Verbose then
        Writeln('Package file: ' + Files[I]);
    end;
  end;

  procedure GenerateLibrary;
  begin
    PackageSmartCopyFile(Project.Path + Project.AndroidLibraryFile(true),
      'jni' + PathDelim + Project.AndroidLibraryFile(false));
  end;

  procedure GenerateAntProperties(var PackageMode: TCompilationMode);
  const
    SourceAntProperties = 'AndroidAntProperties.txt';
    WWW = 'https://github.com/castle-engine/castle-engine/wiki/Android';
  var
    S: TStringList;
  begin
    if FileExists(Project.Path + SourceAntProperties) then
    begin
      S := TStringList.Create;
      try
        S.LoadFromFile(Project.Path + SourceAntProperties);
        if (PackageMode <> cmDebug) and (
            (S.IndexOfName('key.store') = -1) or
            (S.IndexOfName('key.alias') = -1) or
            (S.IndexOfName('key.store.password') = -1) or
            (S.IndexOfName('key.alias.password') = -1)) then
        if PackageMode <> cmDebug then
        begin
          OnWarning(wtMajor, 'Android', 'Key information (key.store, key.alias, key.store.password, key.alias.password) to sign release Android package not found inside ' + SourceAntProperties + ' file. See ' + WWW + ' for documentation how to create and use keys to sign release Android apk. Falling back to creating debug apk.');
          PackageMode := cmDebug;
        end;
      finally FreeAndNil(S) end;

      PackageSmartCopyFile(Project.Path + SourceAntProperties, 'ant.properties');
    end else
    begin
      if PackageMode <> cmDebug then
      begin
        OnWarning(wtMajor, 'Android', 'Key to sign release Android package not found, because ' + SourceAntProperties + ' not found. See ' + WWW + ' for documentation how to create and use keys to sign release Android apk. Falling back to creating debug apk.');
        PackageMode := cmDebug;
      end;
    end;
  end;

  procedure GenerateProjectProperties(const Subprojects: TCastleStringList);
  var
    S: string;
    I: Integer;
  begin
    S := '# Automatically generated by Castle Game Engine build tool.' + NL;
    (*# To enable ProGuard to shrink and obfuscate your code, uncomment this
      # (available properties: sdk.dir, user.home):
      # (Castle Game Engine notes: we use custom-proguard-project.txt name,
      # not standard proguard-project.txt, otherwise proguard-project.txt is overwritten
      # by every "android create project.." call done when packaging.
      #proguard.config=${sdk.dir}/tools/proguard/proguard-android.txt:custom-proguard-project.txt *)
    S += 'target=' + Project.AndroidTarget + NL;
    for I := 0 to Subprojects.Count - 1 do
      S += 'android.library.reference.' + IntToStr(I + 1) + '=./' + Subprojects[I] + '/' + NL;
    { overwrite existing file, since Android "update" project always creates
      (and overwrites, if something existed earlier...) it }
    StringToFile(AndroidProjectPath + 'project.properties', S);
  end;

  { Run "android update project",
    this creates a proper Android project files (build.xml, local.properties). }
  procedure RunAndroidUpdateProject(const LibrarySubdirectory: string = '');
  const
    WWWComponents = 'https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine';
  var
    Dir, ProcessOutput: string;
    ProcessStatus: Integer;
  begin
    Dir := AndroidProjectPath + LibrarySubdirectory;
    if LibrarySubdirectory <> '' then
    begin
      if not DirectoryExists(Dir) then
        raise Exception.Create('Cannot find directory "' + Dir + '", make sure you installed the components dependencies (see "Requires" sections on ' + WWWComponents + ')');
      RunCommandIndirPassthrough(Dir, AndroidExe,
        ['update', 'lib-project',                     '--path', '.', '--target', Project.AndroidTarget],
        ProcessOutput, ProcessStatus)
    end else
      RunCommandIndirPassthrough(Dir, AndroidExe,
        ['update', 'project', '--name', Project.Name, '--path', '.', '--target', Project.AndroidTarget],
        ProcessOutput, ProcessStatus);
    if ProcessStatus <> 0 then
      raise Exception.Create('"android" call failed, cannot create Android apk. Inspect above error messages, and make sure Android SDK is installed correctly. Make sure that target "' + Project.AndroidTarget + '" is installed.');
  end;

  { Run "ndk-build", this moves our .so correctly to the final apk. }
  procedure RunNdkBuild(const PackageMode: TCompilationMode);
  var
    NdkOverrideName, NdkOverrideValue: string;
  begin
    NdkOverrideName := '';
    NdkOverrideValue := '';
    if PackageMode = cmDebug then
    begin
      NdkOverrideName := 'NDK_DEBUG';
      NdkOverrideValue := '1';
    end;
    RunCommandSimple(AndroidProjectPath, 'ndk-build', ['--silent'], NdkOverrideName, NdkOverrideValue);
  end;

  { Run "ant debug/release" to actually build the final apk. }
  procedure RunAnt(const PackageMode: TCompilationMode);
  begin
    RunCommandSimple(AndroidProjectPath, 'ant',
      [ { enable extra warnings, following http://stackoverflow.com/questions/7682150/use-xlintdeprecation-with-android }
        '-Djava.compilerargs=-Xlint:unchecked -Xlint:deprecation',
        PackageModeToName[PackageMode], '-noinput', '-quiet']);
  end;

var
  ApkName: string;
  PackageMode: TCompilationMode;
  TemporaryAndroidProjectPath: boolean;
  Subprojects: TCastleStringList;
  I: Integer;
begin
  { use the AndroidProject value (just make it safer) for AndroidProjectPath,
    if set }
  if Project.AndroidProject <> '' then
    AndroidProjectPath := InclPathDelim(
      StringReplace(Project.AndroidProject, '\', PathDelim, [rfReplaceAll])) else
    AndroidProjectPath := '';

  TemporaryAndroidProjectPath := AndroidProjectPath = '';
  if TemporaryAndroidProjectPath then
    AndroidProjectPath := InclPathDelim(CreateTemporaryDir);
  PackageMode := SuggestedPackageMode;

  GenerateFromTemplates;

  Subprojects := FindSubprojects; // subprojects are only found once we did GenerateFromTemplates
  try
    GenerateIcons;
    GenerateAssets;
    GenerateLibrary;
    GenerateAntProperties(PackageMode);

    for I := 0 to Subprojects.Count - 1 do
    begin
      RunAndroidUpdateProject(Subprojects[I]);
      { some subprojects, like Gitfiz SDK, do not redistribute "src" subdirectory,
        but it's required. }
      PackageCheckForceDirectories(Subprojects[I] + PathDelim + 'src');
    end;

    RunAndroidUpdateProject;
    RunNdkBuild(PackageMode);
    GenerateProjectProperties(Subprojects);

    RunAnt(PackageMode);
  finally FreeAndNil(Subprojects) end;

  ApkName := Project.Name + '-' + PackageModeToName[PackageMode] + '.apk';
  CheckRenameFile(AndroidProjectPath + 'bin' + PathDelim + ApkName,
    Project.Path + ApkName);

  Writeln('Build ' + ApkName);

  if TemporaryAndroidProjectPath and not LeaveTemp then
    RemoveNonEmptyDir(AndroidProjectPath);
end;

procedure InstallAndroidPackage(const Name, QualifiedName: string);
var
  ApkDebugName, ApkReleaseName, ApkName: string;
begin
  ApkReleaseName := Name + '-' + PackageModeToName[cmRelease] + '.apk';
  ApkDebugName   := Name + '-' + PackageModeToName[cmDebug  ] + '.apk';
  if FileExists(ApkDebugName) and FileExists(ApkReleaseName) then
    raise Exception.CreateFmt('Both debug and release apk files exist in this directory: "%s" and "%s". We do not know which to install --- resigning. Simply rename or delete one of the apk files.',
      [ApkDebugName, ApkReleaseName]);
  if FileExists(ApkDebugName) then
    ApkName := ApkDebugName else
  if FileExists(ApkReleaseName) then
    ApkName := ApkReleaseName else
    raise Exception.CreateFmt('No Android apk found in this directory: "%s" or "%s"',
      [ApkDebugName, ApkReleaseName]);

  { Uninstall and then install, instead of calling "install -r",
    to avoid failures because apk signed with different keys (debug vs release). }

  Writeln('Reinstalling application identified as "' + QualifiedName + '".');
  Writeln('If this fails, an often cause is that a previous development version of the application, signed with a different key, remains on the device. In this case uninstall it first (note that it will clear your UserConfig data, unless you use -k) by "adb uninstall ' + QualifiedName + '"');
  RunCommandSimple('adb', ['install', '-r', ApkName]);
  Writeln('Install successfull.');
end;

procedure RunAndroidPackage(const Project: TCastleProject);
var
  ActivityName: string;
begin
  if Project.AndroidProjectType = apBase then
    ActivityName := 'android.app.NativeActivity' else
    ActivityName := 'net.sourceforge.castleengine.MainActivity';
  RunCommandSimple('adb', ['shell', 'am', 'start',
    '-a', 'android.intent.action.MAIN',
    '-n', Project.QualifiedName + '/' + ActivityName ]);
  Writeln('Run successfull.');
  if (FindExe('bash') <> '') and
     (FindExe('grep') <> '') then
  begin
    Writeln('Running "adb logcat | grep ' + Project.Name + '" (we are assuming that your ApplicationName is ''' + Project.Name + ''') to see log output from your application. Just break this process with Ctrl+C to stop.');
    { run through ExecuteProcess, because we don't want to capture output,
      we want to immediately pass it to user }
    ExecuteProcess(FindExe('bash'), ['-c', 'adb logcat | grep --text "' + Project.Name + '"']);
  end else
    Writeln('Run "adb logcat | grep ' + Project.Name + '" (we are assuming that your ApplicationName is ''' + Project.Name + ''') to see log output from your application. Install "bash" and "grep" on $PATH (on Windows, you may want to install MinGW or Cygwin) to run it automatically here.');
end;

end.
