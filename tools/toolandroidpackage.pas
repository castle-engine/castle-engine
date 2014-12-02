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
  ToolUtils, ToolArchitectures, ToolCompile;

procedure CreateAndroidPackage(const SuggestedPackageMode: TCompilationMode;
  const ProjectName: string;
  const ReplaceMacros: TReplaceMacros;
  const Icons: TIconFileNames; const DataPath: string;
  const Files: TCastleStringList;
  const AndroidLibrarySubdir, AndroidLibrary, PackagePath: string);

procedure InstallAndroidPackage(const Name, QualifiedName: string);

implementation

uses SysUtils, Classes,
  CastleURIUtils, CastleWarnings, CastleFilesUtils, CastleImages,
  ToolEmbeddedImages;

const
  PackageModeToName: array [TCompilationMode] of string = ('release', 'debug');

procedure CreateAndroidPackage(const SuggestedPackageMode: TCompilationMode;
  const ProjectName: string;
  const ReplaceMacros: TReplaceMacros;
  const Icons: TIconFileNames; const DataPath: string;
  const Files: TCastleStringList;
  const AndroidLibrarySubdir, AndroidLibrary, PackagePath: string);
var
  AndroidProjectPath: string;

  { Generate simple text stuff for Android project from templates. }
  procedure GenerateFromTemplates;
  const
    AndroidManifestTemplate = {$I templates/android/AndroidManifest.xml.inc};
    StringsTemplate = {$I templates/android/res/values/strings.xml.inc};
    AndroidMkTemplate = {$I templates/android/jni/Android.mk.inc};
  begin
    StringToFile(AndroidProjectPath + 'AndroidManifest.xml',
      ReplaceMacros(AndroidManifestTemplate));

    CheckForceDirectories(AndroidProjectPath + 'res' + PathDelim + 'values');
    StringToFile(AndroidProjectPath +
      'res' + PathDelim + 'values' + PathDelim + 'strings.xml',
      ReplaceMacros(StringsTemplate));

    CheckForceDirectories(AndroidProjectPath + 'jni');
    StringToFile(AndroidProjectPath + 'jni' + PathDelim + 'Android.mk',
      ReplaceMacros(AndroidMkTemplate));
  end;

  procedure GenerateIcons;
  var
    Icon: TCastleImage;

    procedure SaveResized(const Size: Integer; const S: string);
    var
      R: TCastleImage;
      Dir: string;
    begin
      R := Icon.MakeResized(Size, Size, riBilinear);
      try
        Dir := AndroidProjectPath + 'res' + PathDelim + 'drawable-' + S + 'dpi';
        CheckForceDirectories(Dir);
        SaveImage(R, FilenameToURISafe(Dir + PathDelim + 'ic_launcher.png'));
      finally FreeAndNil(R) end;
    end;

  begin
    Icon := Icons.FindReadable;
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
    CheckForceDirectories(AndroidProjectPath + 'assets');
    for I := 0 to Files.Count - 1 do
    begin
      FileFrom := DataPath + Files[I];
      FileTo := AndroidProjectPath + 'assets' + PathDelim + Files[I];
      SmartCopyFile(FileFrom, FileTo);
      if Verbose then
        Writeln('Package file: ' + Files[I]);
    end;
  end;

  procedure GenerateLibrary;
  begin
    SmartCopyFile(AndroidLibrarySubdir,
      AndroidProjectPath + 'jni' + PathDelim + AndroidLibrary);
  end;

  procedure GenerateAntProperties(var PackageMode: TCompilationMode);
  const
    SourceAntProperties = 'AndroidAntProperties.txt';
    WWW = 'https://sourceforge.net/p/castle-engine/wiki/Android%20development/';
  var
    S: TStringList;
  begin
    if FileExists(PackagePath + SourceAntProperties) then
    begin
      S := TStringList.Create;
      try
        S.LoadFromFile(PackagePath + SourceAntProperties);
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

      SmartCopyFile(PackagePath + SourceAntProperties, AndroidProjectPath + 'ant.properties');
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
    AndroidExe := InclPathDelim(GetEnvironmentVariable('ANDROID_HOME')) +
      'tools' + PathDelim + 'android'  + ExeExtension;
    { try to find "android" tool on $PATH }
    if not FileExists(AndroidExe) then
      AndroidExe := PathFileSearch('android'  + ExeExtension, false);
    if AndroidExe = '' then
      raise Exception.Create('Cannot find "android" executable on $PATH, or within $ANDROID_HOME. Install Android SDK and make sure that "android" executable is on $PATH, or that $ANDROID_HOME environment variable is set correctly.');
    RunCommandIndirPassthrough(AndroidProjectPath, AndroidExe,
      ['update', 'project', '--name', ProjectName, '--path', '.', '--target', AndroidTarget],
      ProcessOutput, ProcessStatus);
    if ProcessStatus <> 0 then
      raise Exception.Create('"android" call failed, cannot create Android apk. Inspect above error messages, and make sure Android SDK is installed correctly. Make sure that target "' + AndroidTarget + '" is installed.');
  end;

  procedure RunNdkBuild(const PackageMode: TCompilationMode);
  var
    ProcessOutput, NdkOverrideName, NdkOverrideValue: string;
    ProcessStatus: Integer;
  begin
    NdkOverrideName := '';
    NdkOverrideValue := '';
    if PackageMode = cmDebug then
    begin
      NdkOverrideName := 'NDK_DEBUG';
      NdkOverrideValue := '1';
    end;
    RunCommandIndirPassthrough(AndroidProjectPath, 'ndk-build', [],
      ProcessOutput, ProcessStatus, NdkOverrideName, NdkOverrideValue);
    if ProcessStatus <> 0 then
      raise Exception.Create('"ndk-build" call failed, cannot create Android apk');
  end;

  procedure RunAnt(const PackageMode: TCompilationMode);
  var
    ProcessOutput: string;
    ProcessStatus: Integer;
  begin
    RunCommandIndirPassthrough(AndroidProjectPath, 'ant', [PackageModeToName[PackageMode], '-noinput'],
      ProcessOutput, ProcessStatus);
    if ProcessStatus <> 0 then
      raise Exception.Create('"ant" call failed, cannot create Android apk');
  end;

var
  ApkName: string;
  PackageMode: TCompilationMode;
begin
  AndroidProjectPath := InclPathDelim(CreateTemporaryDir);
  PackageMode := SuggestedPackageMode;

  GenerateFromTemplates;
  GenerateIcons;
  GenerateAssets;
  GenerateLibrary;
  GenerateAntProperties(PackageMode);

  RunAndroidUpdateProject;
  RunNdkBuild(PackageMode);
  RunAnt(PackageMode);

  ApkName := ProjectName + '-' + PackageModeToName[PackageMode] + '.apk';
  CheckRenameFile(AndroidProjectPath + 'bin' + PathDelim + ApkName,
    PackagePath + ApkName);

  if not LeaveTemp then
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

  RunCommandSimple('adb', ['uninstall', QualifiedName]);
  RunCommandSimple('adb', ['install', ApkName]);
  RunCommandSimple('adb', ['shell', 'am', 'start',
    '-a', 'android.intent.action.MAIN',
    '-n', QualifiedName + '/android.app.NativeActivity']);

  Writeln('Install and run successfull. Run "adb logcat | grep ' + Name + '" (assuming that your OnGetApplicationName returns ' + Name + ') to see log output from your application.');
end;

end.
