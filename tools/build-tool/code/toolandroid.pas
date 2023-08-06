{
  Copyright 2014-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Compiling, packaging, installing, running on Android. }
unit ToolAndroid;

{$I castleconf.inc}

interface

uses Classes,
  CastleUtils, CastleStringUtils,
  ToolArchitectures, ToolCompile, ToolPackageFormat, ToolProject,
  ToolManifest;

{ Compile (for all possible Android CPUs) Android unit or library.
  When Project <> nil, we assume we compile libraries (one or more .so files),
  and their final names must match Project.AndroidLibraryFile(CPU) for each CPU.

  CompilerOptions.OS andCompilerOptions.CPU are ignored by this routine.
  This routine may modify CompilerOptions contents. }
procedure CompileAndroid(const Compiler: TCompiler;
  const Project: TCastleProject;
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);

{ Android CPU values supported by the current compiler. }
function DetectAndroidCPUS: TCPUS;

{ Convert CPU to an architecture name understood by Android,
  see TARGET_ARCH_ABI at https://developer.android.com/ndk/guides/android_mk . }
function CPUToAndroidArchitecture(const CPU: TCPU): String;

procedure PackageAndroid(const Project: TCastleProject;
  const OS: TOS; const CPUS: TCPUS; const SuggestedPackageMode: TCompilationMode;
  const PackageFormat: TPackageFormatNoDefault; const PackageNameIncludeVersion: Boolean);

procedure InstallAndroid(const Project: TCastleProject;
  const PackageMode: TCompilationMode;
  const PackageFormat: TPackageFormatNoDefault; const PackageNameIncludeVersion: Boolean);

procedure RunAndroid(const Project: TCastleProject);

implementation

uses SysUtils, DOM, XMLWrite,
  // TODO: Should not be needed after https://github.com/castle-engine/castle-engine/pull/302/commits/888690fdac181b6f140a71fd0d5ac20a7d7b59e6
  {$IFDEF UNIX}BaseUnix, {$ENDIF}
  CastleURIUtils, CastleXMLUtils, CastleLog, CastleFilesUtils, CastleImages,
  ToolEmbeddedImages, ToolFPCVersion, ToolCommonUtils, ToolUtils,
  ToolServicesOperations;

var
  DetectAndroidCPUSCached: TCPUS;

function DetectAndroidCPUS: TCPUS;
begin
  if DetectAndroidCPUSCached = [] then
  begin
    DetectAndroidCPUSCached := [Arm];
    if FPCVersion.AtLeast(3, 2, 0) then
      Include(DetectAndroidCPUSCached, Aarch64)
    else
      WritelnWarning('FPC version ' + FPCVersion.ToString + ' does not support compiling for 64-bit Android (Aarch64). Resulting APK will only support 32-bit Android devices.');
  end;
  Result := DetectAndroidCPUSCached;
end;

procedure CompileAndroid(const Compiler: TCompiler;
  const Project: TCastleProject;
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);
var
  CPU: TCPU;
begin
  for CPU in DetectAndroidCPUS do
  begin
    CompilerOptions.OS := Android;
    CompilerOptions.CPU := CPU;
    Compile(Compiler, WorkingDirectory, CompileFile, CompilerOptions);
    if Project <> nil then
    begin
      CheckRenameFile(Project.AndroidLibraryFile(cpuNone), Project.AndroidLibraryFile(CPU));
      Writeln('Compiled library for Android in ', Project.AndroidLibraryFile(CPU, false));
    end;
  end;
end;

const
  PackageModeToName: array [TCompilationMode] of string = (
    'release',
    'release' { no valgrind support for Android },
    'debug');

function CPUToAndroidArchitecture(const CPU: TCPU): String;
begin
  case CPU of
    { The armeabi-v7a is our proper platform, with hard floats.
      See https://developer.android.com/ndk/guides/application_mk.html
      and http://stackoverflow.com/questions/24948008/linking-so-file-within-android-ndk
      and *do not* confuse this with (removed now) armeabi-v7a-hard ABI:
      https://android.googlesource.com/platform/ndk/+show/353e653824b79c43b948429870d0abeedebde386/docs/HardFloatAbi.md
    }
    arm    : Result := 'armeabi-v7a';
    aarch64: Result := 'arm64-v8a';
    i386   : Result := 'x86';
    x86_64 : Result := 'x86_64';
    else raise Exception.CreateFmt('Android does not support CPU "%s"', [CPUToString(CPU)]);
  end;
end;

function Capitalize(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    Result := AnsiUpperCase(Result[1]) + SEnding(Result, 2);
end;

{ Try to find ExeName executable.
  If not found -> exception (if Required) or return '' (if not Required). }
function FinishExeSearch(const ExeName, BundleName, EnvVarName: string;
  const Required: boolean): string;
begin
  { try to find on $PATH }
  Result := FindExe(ExeName);
  { fail if still not found }
  if Required and (Result = '') then
    raise Exception.Create('Cannot find "' + ExeName + '" executable on $PATH, or within $' + EnvVarName + '. Install Android ' + BundleName + ' and make sure that "' + ExeName + '" executable is on $PATH, or that $' + EnvVarName + ' environment variable is set correctly.');
end;

{ Try to find "adb" tool executable.
  If not found -> exception (if Required) or return '' (if not Required). }
function AdbExe(const Required: boolean = true): string;
const
  ExeName = 'adb';
  BundleName = 'SDK';
  EnvVarName1 = 'ANDROID_SDK_ROOT';
  EnvVarName2 = 'ANDROID_HOME';
var
  Env: string;
begin
  Result := '';
  { try to find in $ANDROID_SDK_ROOT or (deprecated) $ANDROID_HOME }
  Env := GetEnvironmentVariable(EnvVarName1);
  if Env = '' then
    GetEnvironmentVariable(EnvVarName2);
  if Env <> '' then
  begin
    Result := AddExeExtension(InclPathDelim(Env) + 'platform-tools' + PathDelim + ExeName);
    if not RegularFileExists(Result) then
      Result := '';
  end;
  { try to find on $PATH }
  if Result = '' then
    Result := FinishExeSearch(ExeName, BundleName, EnvVarName1, Required);
end;

{ Filename (without any leading path) of the Android output (APK, AAB) file. }
function AndroidPackageFile(const Project: TCastleProject;
  const Mode: TCompilationMode;
  const PackageFormat: TPackageFormatNoDefault;
  const PackageNameIncludeVersion: Boolean): String;
begin
  Result := Project.Name;
  if PackageNameIncludeVersion and (Project.Version.DisplayValue <> '') then
    Result += '-' + Project.Version.DisplayValue;
  Result += '-android' + '-' + PackageModeToName[Mode];
  case PackageFormat of
    pfAndroidApk:
      Result += '.apk';
    pfAndroidAppBundle:
      Result += '.aab';
    else
      raise Exception.Create('Unexpected PackageFormat in for Android: ' + PackageFormatToString(PackageFormat));
  end;
end;

procedure PackageAndroid(const Project: TCastleProject;
  const OS: TOS; const CPUS: TCPUS; const SuggestedPackageMode: TCompilationMode;
  const PackageFormat: TPackageFormatNoDefault; const PackageNameIncludeVersion: Boolean);
var
  AndroidProjectPath: string;

  { Some utility procedures PackageXxx below.
    They work just like Xxx, but target filename should not contain
    prefix AndroidProjectPath. They avoid overwriting stuff
    already existing (in case multiple services
    contain the same path inside), but warn about it. }

  procedure PackageCheckForceDirectories(const Dirs: string);
  begin
    CheckForceDirectories(AndroidProjectPath + Dirs);
  end;

  procedure PackageSaveImage(const Image: TCastleImage; const FileName: string);
  begin
    PackageCheckForceDirectories(ExtractFilePath(FileName));
    if not RegularFileExists(AndroidProjectPath + FileName) then
      SaveImage(Image, FilenameToURISafe(AndroidProjectPath + FileName))
    else
      WritelnWarning('Android', 'Android package file specified by multiple services: ' + FileName);
  end;

  procedure PackageSmartCopyFile(const FileFrom, FileTo: string);
  begin
    PackageCheckForceDirectories(ExtractFilePath(FileTo));
    if not RegularFileExists(AndroidProjectPath + FileTo) then
      SmartCopyFile(FileFrom, AndroidProjectPath + FileTo)
    else
      WritelnWarning('Android', 'Android package file specified by multiple services: ' + FileTo);
  end;

{
  procedure PackageStringToFile(const FileTo, Contents: string);
  begin
    if not RegularFileExists(AndroidProjectPath + FileTo) then
    begin
      PackageCheckForceDirectories(ExtractFilePath(FileTo));
      StringToFile(FileTo, Contents);
    end else
      WritelnWarning('Android package file specified by multiple services: ' + FileTo);
  end;
}

  { Generate files for Android project from templates. }
  procedure GenerateFromTemplates;
  var
    TemplatePath: string;
  begin
    { Add Android project core directory.
      We used to have a choice here (base or integrated),
      but now it's always integrated. }
    TemplatePath := 'android/integrated/';
    Project.ExtractTemplate(TemplatePath, AndroidProjectPath);
  end;

  { Generate files for Android project from templates, adding services. }
  procedure GenerateServicesFromTemplates;

    procedure ExtractService(const ServiceName: string);
    var
      TemplatePath: string;
    begin
      TemplatePath := 'android/integrated-services/' + ServiceName;
      Project.ExtractTemplate(TemplatePath, AndroidProjectPath);
    end;

  var
    I: Integer;
  begin
    { add declared services }
    for I := 0 to Project.AndroidServices.Count - 1 do
      ExtractService(Project.AndroidServices[I].Name);

    { add automatic services }
    if (depSound in Project.Dependencies) and
       not Project.AndroidServices.HasService('sound') then
      ExtractService('sound');
    if (depOggVorbis in Project.Dependencies) and
       not Project.AndroidServices.HasService('ogg_vorbis') then
      ExtractService('ogg_vorbis');
    if (depFreeType in Project.Dependencies) and
       not Project.AndroidServices.HasService('freetype') then
      ExtractService('freetype');
    if (depPng in Project.Dependencies) and
       not Project.AndroidServices.HasService('png') then
      ExtractService('png');
    if (depHttps in Project.Dependencies) and
       not Project.AndroidServices.HasService('download_urls') then
      ExtractService('download_urls');
  end;

  procedure GenerateIcons;
  var
    Icon: TCastleImage;

    procedure SaveResized(const Size: Integer; const S: string);
    var
      R: TCastleImage;
      Dir: string;
    begin
      R := Icon.MakeResized(Size, Size, BestInterpolation);
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
  begin
    Project.CopyData(AndroidProjectPath +
      'app' + PathDelim +
      'src' + PathDelim +
      'main' + PathDelim +
      'assets',
      cpAndroid);
  end;

  procedure GenerateLocalization;
  var
    LocalizedAppName: TLocalizedAppName;
    Doc: TXMLDocument;
    RootNode, StringNode: TDOMNode;
    I: TXMLElementIterator;
    Language, StringsPath: String;
  begin
    if not Assigned(Project.LocalizedAppNames) then Exit;

    //Change default app_name to translatable:
    StringsPath := AndroidProjectPath + 'app' + PathDelim +'src' + PathDelim + 'main' + PathDelim + 'res' + PathDelim + 'values' + PathDelim + 'strings.xml';
    URLReadXML(Doc, StringsPath);
    try
      I := Doc.DocumentElement.ChildrenIterator;
      try
        while I.GetNext do
          if (I.Current.TagName = 'string') and (I.Current.AttributeString('name') = 'app_name') then
          begin
            I.Current.AttributeSet('translatable', 'true');
            Break; //There can only be one string 'app_name', so we don't need to continue the loop.
          end;
      finally
        I.Free;
      end;

      WriteXMLFile(Doc, StringsPath);
    finally
      Doc.Free;
    end;

    //Write strings for every chosen language:
    for LocalizedAppName in Project.LocalizedAppNames do
    begin
      Doc := TXMLDocument.Create;
      try
        RootNode := Doc.CreateElement('resources');
        Doc.Appendchild(RootNode);
        RootNode:= Doc.DocumentElement;

        StringNode := Doc.CreateElement('string');
        TDOMElement(StringNode).AttributeSet('name', 'app_name');
        StringNode.AppendChild(Doc.CreateTextNode(UTF8Decode(LocalizedAppName.AppName)));
        RootNode.AppendChild(StringNode);

        if LocalizedAppName.Language = 'default' then
          Language := ''
        else
          Language := '-' + LocalizedAppName.Language;

        StringsPath := AndroidProjectPath + 'app' + PathDelim +'src' + PathDelim + 'main' + PathDelim + 'res' + PathDelim +
                                            'values' + Language + PathDelim + 'strings.xml';

        CheckForceDirectories(ExtractFilePath(StringsPath));
        WriteXMLFile(Doc, StringsPath);
      finally
        Doc.Free;
      end;
    end;
  end;

  procedure GenerateLibrary;
  var
    CPU: TCPU;
    JniPath, LibraryWithoutCPU, LibraryFileName: String;
  begin
    JniPath := 'app' + PathDelim + 'src' + PathDelim + 'main' + PathDelim +
      { Place precompiled libs in jniLibs/ .
        This is where precompiled native libs should be, according to
        https://developer.android.com/studio/projects/gradle-external-native-builds . }
      'jniLibs' + PathDelim;
    LibraryWithoutCPU := ExtractFileName(Project.AndroidLibraryFile(cpuNone));

    for CPU in CPUS do
    begin
      LibraryFileName := Project.AndroidLibraryFile(CPU);
      PackageSmartCopyFile(LibraryFileName,
        JniPath + CPUToAndroidArchitecture(CPU) + PathDelim + LibraryWithoutCPU);
    end;
  end;

var
  KeyStore, KeyAlias, KeyStorePassword, KeyAliasPassword: string;

  procedure CalculateSigningProperties(var PackageMode: TCompilationMode);
  const
    SigningPropertiesFile = 'AndroidSigningProperties.txt';
    WWW = 'https://castle-engine.io/android_faq#_signing_a_release_apk_aab';
    MissingSigningSuffix =
      '  See ' + WWW + ' for documentation how to create and use keys to sign release Android APK / AAB.' + NL +
      '  Falling back to creating debug apk.';
    MissingSigningInfo =
      'Key information (key.store, key.alias, key.store.password, key.alias.password) to sign release Android package not found inside "' + SigningPropertiesFile + '" file.' + NL +
      MissingSigningSuffix;
    MissingSigningFile =
      'Information about the keys to sign release Android package not found, because "' + SigningPropertiesFile + '" file does not exist.' + NL +
      MissingSigningSuffix;

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
          WritelnWarning('Android', MissingSigningInfo);
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

  begin
    KeyStore := '';
    KeyAlias := '';
    KeyStorePassword := '';
    KeyAliasPassword := '';
    if RegularFileExists(Project.Path + SigningPropertiesFile) then
    begin
      LoadSigningProperties(Project.Path + SigningPropertiesFile);
    end else
    if PackageMode <> cmDebug then
    begin
      WritelnWarning('Android', MissingSigningFile);
      PackageMode := cmDebug;
    end;
  end;

  { Run Gradle to actually build the final apk. }
  procedure RunGradle(const PackageMode: TCompilationMode);
  var
    Args: TCastleStringList;
  begin
    Args := TCastleStringList.Create;
    try
      case PackageFormat of
        pfAndroidApk:
          Args.Add('assemble' + Capitalize(PackageModeToName[PackageMode]));
        pfAndroidAppBundle:
          Args.Add(':app:bundle' + Capitalize(PackageModeToName[PackageMode]));
        else
          raise Exception.Create('Unexpected PackageFormat in PackageAndroid: ' + PackageFormatToString(PackageFormat));
      end;
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
      try
        RunCommandSimple(AndroidProjectPath, AndroidProjectPath + 'gradlew.bat', Args.ToArray);
      finally
        { Gradle deamon is automatically initialized since Gradle version 3.0
          (see https://docs.gradle.org/current/userguide/gradle_daemon.html)
          but it prevents removing the castle-engine-output/android/project/ .
          E.g. you cannot run "castle-engine package --os=android --cpu=arm"
          again in the same directory, because it cannot remove the
          "castle-engine-output/android/project/" at the beginning.

          It seems the current directory of Java (Gradle) process is inside
          castle-engine-output/android/project/, and Windows doesn't allow to remove such
          directory. Doing "rm -Rf castle-engine-output/android/project/" (rm.exe from Cygwin)
          also fails with

            rm: cannot remove 'castle-engine-output/android/project/': Device or resource busy

          This may be related to
          https://discuss.gradle.org/t/the-gradle-daemon-prevents-a-clean/2473/13

          The solution for now is to kill the daemon afterwards. }
        RunCommandSimple(AndroidProjectPath, AndroidProjectPath + 'gradlew.bat', ['--stop']);
      end;

      {$else}
      if RegularFileExists(AndroidProjectPath + 'gradlew') then
      begin
        Args.Insert(0, './gradlew');
        RunCommandSimple(AndroidProjectPath, 'bash', Args.ToArray);
      end else
      begin
        Writeln('Local Gradle wrapper ("gradlew") not found, so we will call the Gradle on $PATH.');
        Writeln('Make sure you have installed Gradle (e.g. from the Debian "gradle" package), in a version compatible with the Android Gradle plugin (see https://developer.android.com/studio/releases/gradle-plugin.html#updating-gradle ).');
        RunCommandSimple(AndroidProjectPath, 'gradle', Args.ToArray);
      end;
      {$endif}
    finally FreeAndNil(Args) end;
  end;

var
  PackageName: string;
  PackageMode: TCompilationMode;
begin
  { calculate clean AndroidProjectPath }
  AndroidProjectPath := TempOutputPath(Project.Path) +
    'android' + PathDelim + 'project' + PathDelim;
  if DirectoryExists(AndroidProjectPath) then
    RemoveNonEmptyDir(AndroidProjectPath);

  PackageMode := SuggestedPackageMode;

  CalculateSigningProperties(PackageMode);

  GenerateFromTemplates;
  GenerateServicesFromTemplates;
  PackageServices(Project, Project.AndroidServices,
    'castle-data:/android/integrated-services/', AndroidProjectPath);
  GenerateIcons;
  GenerateAssets;
  GenerateLocalization;
  GenerateLibrary;
  RunGradle(PackageMode);

  PackageName := AndroidPackageFile(Project, PackageMode, PackageFormat, PackageNameIncludeVersion);
  case PackageFormat of
    pfAndroidApk:
      begin
        CheckRenameFile(AndroidProjectPath + 'app' + PathDelim +
          'build' +  PathDelim +
          'outputs' + PathDelim +
          'apk' + PathDelim +
          PackageModeToName[PackageMode] + PathDelim +
          'app-' + PackageModeToName[PackageMode] + '.apk',
          Project.OutputPath + PackageName);
      end;
    pfAndroidAppBundle:
      begin
        CheckRenameFile(AndroidProjectPath + 'app' + PathDelim +
          'build' +  PathDelim +
          'outputs' + PathDelim +
          'bundle' + PathDelim +
          PackageModeToName[PackageMode] + PathDelim +
          'app-' + PackageModeToName[PackageMode] + '.aab',
          Project.OutputPath + PackageName);
        DoMakeExecutable(Project.OutputPath + PackageName);
      end;
    else
      raise Exception.Create('Unexpected PackageFormat in PackageAndroid: ' + PackageFormatToString(PackageFormat));
  end;

  Writeln('Build ' + PackageName);
end;

procedure InstallAndroid(const Project: TCastleProject;
  const PackageMode: TCompilationMode;
  const PackageFormat: TPackageFormatNoDefault; const PackageNameIncludeVersion: Boolean);
var
  PackageName: string;
begin
  PackageName := Project.OutputPath +
    AndroidPackageFile(Project, PackageMode, PackageFormat, PackageNameIncludeVersion);
  if not RegularFileExists(PackageName) then
    raise Exception.CreateFmt('No Android package found: "%s"', [PackageName]);

  { Uninstall and then install, instead of calling "install -r",
    to avoid failures because apk signed with different keys (debug vs release). }

  Writeln('Reinstalling application identified as "' + Project.QualifiedName + '".');
  Writeln('If this fails, an often cause is that a previous development version of the application, signed with a different key, remains on the device. In this case uninstall it first (note that it will clear your UserConfig data, unless you use -k) by "adb uninstall ' + Project.QualifiedName + '"');
  Flush(Output); // don't mix output with adb output
  RunCommandSimple(AdbExe, ['install', '-r', PackageName]);
  Writeln('Install successful.');
end;

procedure RunAndroid(const Project: TCastleProject);
var
  ActivityName, LogTag: string;
begin
  ActivityName := 'net.sourceforge.castleengine.MainActivity';
  RunCommandSimple(AdbExe, ['shell', 'am', 'start',
    '-a', 'android.intent.action.MAIN',
    '-n', Project.QualifiedName + '/' + ActivityName ]);
  Writeln('Run successful.');

  LogTag := Copy(Project.Name, 1, MaxAndroidTagLength);

  Writeln('Running "adb logcat -s ' + LogTag + ':V".');
  Writeln('We are assuming that your ApplicationName is "' + Project.Name + '".');
  Writeln('Break this process with Ctrl+C.');

  { run through ExecuteProcess, because we don't want to capture output,
    we want to immediately pass it to user }
  ExecuteProcess(AdbExe, ['logcat', '-s', LogTag + ':V']);
end;

end.
