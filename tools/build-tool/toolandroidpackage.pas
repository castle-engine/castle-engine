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

implementation

uses SysUtils, Classes,
  CastleURIUtils, CastleWarnings, CastleFilesUtils, CastleImages,
  ToolEmbeddedImages, ExtInterpolation;

const
  PackageModeToName: array [TCompilationMode] of string = ('release', 'debug');

procedure CreateAndroidPackage(const Project: TCastleProject;
  const OS: TOS; const CPU: TCPU; const SuggestedPackageMode: TCompilationMode;
  const Files: TCastleStringList);
var
  AndroidProjectPath: string;

  { Some utility procedures PackageXxx work just like Xxx,
    but target filename should not contain prefix AndroidProjectPath,
    and they avoid overwriting stuff already existing (in case CastleEngineManifest.xml
    contained explicit android_project path). }

  procedure PackageStringToFile(const FileName, Contents: string);
  begin
    if not FileExists(AndroidProjectPath + FileName) then
      StringToFile(AndroidProjectPath + FileName, Contents) else
    if Verbose then
      Writeln('Not overwriting custom ' + FileName);
  end;

  procedure PackageCheckForceDirectories(const Dirs: string);
  begin
    CheckForceDirectories(AndroidProjectPath + Dirs);
  end;

  procedure PackageSaveImage(const Image: TCastleImage; const FileName: string);
  begin
    if not FileExists(AndroidProjectPath + FileName) then
      SaveImage(Image, FilenameToURISafe(AndroidProjectPath + FileName)) else
    if Verbose then
      Writeln('Not overwriting custom ' + FileName);
  end;

  procedure PackageSmartCopyFile(const FileFrom, FileTo: string);
  begin
    if not FileExists(AndroidProjectPath + FileTo) then
      SmartCopyFile(FileFrom, AndroidProjectPath + FileTo) else
    if Verbose then
      Writeln('Not overwriting custom ' + FileTo);
  end;

  { Generate simple text stuff for Android project from templates. }
  procedure GenerateFromTemplates;

    function AddExternalLibraryMk(const LibraryName: string): string;
    begin
      Result := LineEnding +
        'include $(CLEAR_VARS)' + LineEnding +
        'LOCAL_MODULE := lib' + LibraryName + LineEnding +
        'LOCAL_SRC_FILES := lib' + LibraryName + '.so' + LineEnding +
        'include $(PREBUILT_SHARED_LIBRARY)' + LineEnding +
        '';
    end;

  const
    AndroidManifestTemplate = {$I templates/android/AndroidManifest.xml.inc};
    StringsTemplate = {$I templates/android/res/values/strings.xml.inc};
    AndroidMkTemplate = {$I templates/android/jni/Android.mk.inc};
  var
    AndroidMkContents: string;
  begin
    PackageStringToFile('AndroidManifest.xml',
      Project.ReplaceMacros(AndroidManifestTemplate));

    PackageCheckForceDirectories('res' + PathDelim + 'values');
    PackageStringToFile('res' + PathDelim + 'values' + PathDelim + 'strings.xml',
      Project.ReplaceMacros(StringsTemplate));

    PackageCheckForceDirectories('jni');
    AndroidMkContents := Project.ReplaceMacros(AndroidMkTemplate);
    if depSound in Project.Dependencies then
      AndroidMkContents += AddExternalLibraryMk('openal');
    PackageStringToFile('jni' + PathDelim + 'Android.mk',
      AndroidMkContents);
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
        PackageCheckForceDirectories(Dir);
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
    PackageCheckForceDirectories('assets');
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
    WWW = 'https://sourceforge.net/p/castle-engine/wiki/Android%20development/';
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

  procedure RunAndroidUpdateProject;
  const
    AndroidTarget = 'android-19';
  var
    ProcessOutput, AndroidExe: string;
    ProcessStatus: Integer;
  begin
    { try to find "android" tool in $ANDROID_HOME }
    AndroidExe := AddExeExtension(InclPathDelim(GetEnvironmentVariable('ANDROID_HOME')) +
      'tools' + PathDelim + 'android');
    { try to find "android" tool on $PATH }
    if not FileExists(AndroidExe) then
      AndroidExe := FindExe('android');
    if AndroidExe = '' then
      raise Exception.Create('Cannot find "android" executable on $PATH, or within $ANDROID_HOME. Install Android SDK and make sure that "android" executable is on $PATH, or that $ANDROID_HOME environment variable is set correctly.');
    RunCommandIndirPassthrough(AndroidProjectPath, AndroidExe,
      ['update', 'project', '--name', Project.Name, '--path', '.', '--target', AndroidTarget],
      ProcessOutput, ProcessStatus);
    if ProcessStatus <> 0 then
      raise Exception.Create('"android" call failed, cannot create Android apk. Inspect above error messages, and make sure Android SDK is installed correctly. Make sure that target "' + AndroidTarget + '" is installed.');
  end;

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

  procedure RunAnt(const PackageMode: TCompilationMode);
  begin
    RunCommandSimple(AndroidProjectPath, 'ant', [PackageModeToName[PackageMode], '-noinput', '-quiet']);
  end;

  { Add a library from external_libraries. Adds smart, without overwriting
    custom files (like PackageSmartCopyFile). }
  procedure AddExternalLibrary(const LibraryName: string);
  var
    FileFrom, FileTo: string;
  begin
    FileTo := AndroidProjectPath + 'jni' + PathDelim + LibraryName;
    if not FileExists(FileTo) then
    begin
      FileFrom := Project.ExternalLibraryPath(OS, CPU, LibraryName);
      { Note that this is not checked if FileExists(FileTo) exists.
        So if you have a custom library already present, we don't require to have
        a standard version on ExternalLibrariesPath. }
      if not FileExists(FileFrom) then
        raise Exception.Create('Dependency library not found in ' + FileFrom);
      SmartCopyFile(FileFrom, FileTo);
    end else
    if Verbose then
      Writeln('Not overwriting custom ' + FileTo);
  end;

var
  ApkName: string;
  PackageMode: TCompilationMode;
  TemporaryAndroidProjectPath: boolean;
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
  GenerateIcons;
  GenerateAssets;
  GenerateLibrary;
  GenerateAntProperties(PackageMode);

  if depSound in Project.Dependencies then
    AddExternalLibrary('libopenal.so');

  RunAndroidUpdateProject;
  RunNdkBuild(PackageMode);

  RunAnt(PackageMode);

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

  Writeln('Uninstalling, then installing again and running application identified as "' + QualifiedName + '"');

  RunCommandSimple('adb', ['uninstall', QualifiedName]);
  RunCommandSimple('adb', ['install', ApkName]);
  RunCommandSimple('adb', ['shell', 'am', 'start',
    '-a', 'android.intent.action.MAIN',
    '-n', QualifiedName + '/android.app.NativeActivity']);

  Writeln('Install and run successfull. Run "adb logcat | grep ' + Name + '" (assuming that your ApplicationName is ''' + Name + ''') to see log output from your application.');
end;

end.
