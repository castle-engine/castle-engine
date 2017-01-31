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
  CastleURIUtils, CastleLog, CastleFilesUtils, CastleImages,
  ToolEmbeddedImages, ExtInterpolation;

const
  PackageModeToName: array [TCompilationMode] of string = (
    'release',
    'release' { no valgrind support for Android },
    'debug');

function Capitalize(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    Result := AnsiUpperCase(Result[1]) + SEnding(Result, 2);
end;

procedure CreateAndroidPackage(const Project: TCastleProject;
  const OS: TOS; const CPU: TCPU; const SuggestedPackageMode: TCompilationMode;
  const Files: TCastleStringList);
var
  AndroidProjectPath: string;

  { Some utility procedures PackageXxx below.
    They work just like Xxx, but target filename should not contain
    prefix AndroidProjectPath. They avoid overwriting stuff
    already existing (in case multiple components
    contain the same path inside), but warn about it. }

  procedure PackageCheckForceDirectories(const Dirs: string);
  begin
    CheckForceDirectories(AndroidProjectPath + Dirs);
  end;

  procedure PackageSaveImage(const Image: TCastleImage; const FileName: string);
  begin
    PackageCheckForceDirectories(ExtractFilePath(FileName));
    if not FileExists(AndroidProjectPath + FileName) then
      SaveImage(Image, FilenameToURISafe(AndroidProjectPath + FileName))
    else
      WritelnWarning('Android', 'Android package file specified by multiple components: ' + FileName);
  end;

  procedure PackageSmartCopyFile(const FileFrom, FileTo: string);
  begin
    PackageCheckForceDirectories(ExtractFilePath(FileTo));
    if not FileExists(AndroidProjectPath + FileTo) then
      SmartCopyFile(FileFrom, AndroidProjectPath + FileTo)
    else
      WritelnWarning('Android', 'Android package file specified by multiple components: ' + FileTo);
  end;

{
  procedure PackageStringToFile(const FileTo, Contents: string);
  begin
    if not FileExists(AndroidProjectPath + FileTo) then
    begin
      PackageCheckForceDirectories(ExtractFilePath(FileTo));
      StringToFile(FileTo, Contents);
    end else
      WritelnWarning('Android package file specified by multiple components: ' + FileTo);
  end;
}

  { Generate simple text stuff for Android project from templates. }
  procedure GenerateFromTemplates;
  var
    DestinationPath: string;

    procedure ExtractComponent(const ComponentName: string);
    var
      TemplatePath: string;
    begin
      TemplatePath := 'android/integrated-components/' + ComponentName;
      Project.ExtractTemplate(TemplatePath, DestinationPath);
    end;

  var
    TemplatePath: string;
    I: Integer;
  begin
    { calculate absolute DestinationPath.
      Use CombinePaths, in case AndroidProjectPath is relative to project dir
      (although it's not possible now). }
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
      { add declared components }
      for I := 0 to Project.AndroidComponents.Count - 1 do
        ExtractComponent(Project.AndroidComponents[I].Name);

      { add automatic components }
      if (depSound in Project.Dependencies) and
         not Project.AndroidComponents.HasComponent('sound') then
        ExtractComponent('sound');
      if (depOggVorbis in Project.Dependencies) and
         not Project.AndroidComponents.HasComponent('ogg_vorbis') then
        ExtractComponent('ogg_vorbis');
    end;
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
      R := Icon.MakeResized(Size, Size, riLanczos);
      try
        Dir := 'app' + PathDelim + 'src' + PathDelim + 'main' + PathDelim +
               'res' + PathDelim + 'mipmap-' + S + 'dpi';
        PackageSaveImage(R, Dir + PathDelim + 'ic_launcher.png');
      finally FreeAndNil(R) end;
    end;

  begin
    Icon := Project.Icons.FindReadable;
    if Icon = nil then
    begin
      WritelnWarning('Icon', 'No icon in a format readable by our engine (for example, png or jpg) is specified in CastleEngineManifest.xml. Using default icon.');
      Icon := DefaultIcon;
    end;
    try
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
      FileTo := 'app' + PathDelim + 'src' + PathDelim + 'main' + PathDelim +
                'assets' + PathDelim + Files[I];
      PackageSmartCopyFile(FileFrom, FileTo);
      if Verbose then
        Writeln('Package file: ' + Files[I]);
    end;
  end;

  procedure GenerateLibrary;
  begin
    PackageSmartCopyFile(Project.Path + Project.AndroidLibraryFile(true),
      'app' + PathDelim + 'src' + PathDelim + 'main' + PathDelim +
      { Place precompiled libs in jni/ , ndk-build will find them there. }
      'jni' + PathDelim + 'armeabi-v7a' + PathDelim + Project.AndroidLibraryFile(false));
  end;

  { Run "ndk-build", this moves our .so to the final location in jniLibs,
    also setting up debug stuff for ndk-gdb to work. }
  procedure RunNdkBuild;
  begin
    { Place precompiled .so files in jniLibs/ to make them picked up by Gradle.
      See http://stackoverflow.com/questions/27532062/include-pre-compiled-static-library-using-ndk
      http://stackoverflow.com/a/28430178

      Possibly we could also let the ndk-build to place them in libs/,
      as it does by default.

      We know we should not let them be only in jni/ subdir,
      as they would not be picked by Gradle from there. But that's
      what ndk-build does: it copies them from jni/ to another directory. }

    RunCommandSimple(AndroidProjectPath + 'app' + PathDelim + 'src' + PathDelim + 'main',
      'ndk-build', ['--silent', 'NDK_LIBS_OUT=./jniLibs']);
  end;

var
  KeyStore, KeyAlias, KeyStorePassword, KeyAliasPassword: string;

  procedure CalculateSigningProperties(var PackageMode: TCompilationMode);
  const
    WWW = 'https://github.com/castle-engine/castle-engine/wiki/Android';

    procedure LoadSigningProperties(const FileName: string);
    var
      S: TStringList;
    begin
      S := TStringList.Create;
      try
        S.LoadFromFile(FileName);
        if (PackageMode <> cmDebug) and (
            (S.IndexOfName('key.store') = -1) or
            (S.IndexOfName('key.alias') = -1) or
            (S.IndexOfName('key.store.password') = -1) or
            (S.IndexOfName('key.alias.password') = -1)) then
        begin
          WritelnWarning('Android', 'Key information (key.store, key.alias, key.store.password, key.alias.password) to sign release Android package not found inside "' + FileName + '" file. See ' + WWW + ' for documentation how to create and use keys to sign release Android apk. Falling back to creating debug apk.');
          PackageMode := cmDebug;
        end;
        if PackageMode <> cmDebug then
        begin
          KeyStore := S.Values['key.store'];
          KeyAlias := S.Values['key.alias'];
          KeyStorePassword := S.Values['key.store.password'];
          KeyAliasPassword := S.Values['key.alias.password'];

          (*
          Project.AndroidSigningConfig :=
            'signingConfigs {' + NL +
            '    release {' + NL +
            '        storeFile file("' + KeyStore + '")' + NL +
            '        storePassword "' + KeyStorePassword + '"' + NL +
            '        keyAlias "' + KeyAlias + '"' + NL +
            '        keyPassword "' + KeyAliasPassword + '"' + NL +
            '    }' + NL +
            '}' + NL;
          Project.AndroidSigningConfigDeclare := 'signingConfig signingConfigs.release';

          This fails with gradle error
          (as of 'com.android.tools.build:gradle-experimental:0.7.0' ; did not try with later versions)

            * Where:
            Build file '/tmp/castle-engine157085/app/build.gradle' line: 26
            * What went wrong:
            A problem occurred configuring project ':app'.
            > Exception thrown while executing model rule: android { ... } @ app/build.gradle line 4, column 5 > named(release)
               > Attempt to read a write only view of model of type 'org.gradle.model.ModelMap<com.android.build.gradle.managed.BuildType>' given to rule 'android { ... } @ app/build.gradle line 4, column 5'

          See
          https://code.google.com/p/android/issues/detail?id=182249
          http://stackoverflow.com/questions/32109501/adding-release-keys-in-the-experimental-gradle-plugin-for-android

          We use simpler solution now, from the bottom of
          https://plus.googleapis.com/+JoakimEngstr%C3%B6mJ/posts/NY3JUkz5dPP
          See also
          http://www.tinmith.net/wayne/blog/2014/08/gradle-sign-command-line.htm
          *)
        end;
      finally FreeAndNil(S) end;
    end;

  const
    SourceAntPropertiesOld = 'AndroidAntProperties.txt';
    SourceAntProperties = 'AndroidSigningProperties.txt';
  begin
    KeyStore := '';
    KeyAlias := '';
    KeyStorePassword := '';
    KeyAliasPassword := '';
    if FileExists(Project.Path + SourceAntProperties) then
    begin
      LoadSigningProperties(Project.Path + SourceAntProperties);
    end else
    if FileExists(Project.Path + SourceAntPropertiesOld) then
    begin
      LoadSigningProperties(Project.Path + SourceAntPropertiesOld);
      WritelnWarning('Deprecated', 'Using deprecated configuration file name "' + SourceAntPropertiesOld + '". Rename it to "' + SourceAntProperties + '".');
    end else
    if PackageMode <> cmDebug then
    begin
      WritelnWarning('Android', 'Information about the keys to sign release Android package not found, because "' + SourceAntProperties + '" file does not exist. See ' + WWW + ' for documentation how to create and use keys to sign release Android apk. Falling back to creating debug apk.');
      PackageMode := cmDebug;
    end;
  end;

  { Run "gradlew" to actually build the final apk. }
  procedure RunGradle(const PackageMode: TCompilationMode);
  var
    Args: TCastleStringList;
  begin
    Args := TCastleStringList.Create;
    try
      Args.Add('assemble' + Capitalize(PackageModeToName[PackageMode]));
      if not Verbose then
        Args.Add('--quiet');
      if PackageMode <> cmDebug then
      begin
        Args.Add('-Pandroid.injected.signing.store.file=' + KeyStore);
        Args.Add('-Pandroid.injected.signing.store.password=' + KeyStorePassword);
        Args.Add('-Pandroid.injected.signing.key.alias=' + KeyAlias);
        Args.Add('-Pandroid.injected.signing.key.password=' + KeyAliasPassword);
      end;
      {$ifdef MSWINDOWS}
      RunCommandSimple(AndroidProjectPath, AndroidProjectPath + 'gradlew.bat', Args.ToArray);
      {$else}
      Args.Insert(0, './gradlew');
      RunCommandSimple(AndroidProjectPath, 'bash', Args.ToArray);
      {$endif}
    finally FreeAndNil(Args) end;
  end;

var
  ApkName: string;
  PackageMode: TCompilationMode;
begin
  AndroidProjectPath := InclPathDelim(CreateTemporaryDir);
  PackageMode := SuggestedPackageMode;

  CalculateSigningProperties(PackageMode);

  GenerateFromTemplates;
  GenerateIcons;
  GenerateAssets;
  GenerateLibrary;
  RunNdkBuild;
  RunGradle(PackageMode);

  ApkName := Project.Name + '-' + PackageModeToName[PackageMode] + '.apk';
  CheckRenameFile(AndroidProjectPath + 'app' + PathDelim + 'build' +
    PathDelim + 'outputs' + PathDelim + 'apk' + PathDelim +
    'app-' + PackageModeToName[PackageMode] + '.apk',
    Project.Path + ApkName);

  Writeln('Build ' + ApkName);

  if not LeaveTemp then
    RemoveNonEmptyDir(AndroidProjectPath, true);
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
