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
  ToolUtils, ToolArchitectures;

procedure CreateAndroidPackage(const ReplaceMacros: TReplaceMacros{;
  const Path: string; const Icons: TCastleStringList});

implementation

uses SysUtils,
  CastleURIUtils, CastleWarnings, CastleFilesUtils;

procedure CreateAndroidPackage(const ReplaceMacros: TReplaceMacros{;
  const Path: string; const Icons: TCastleStringList});
const
  AndroidManifestTemplate = {$I templates/android/AndroidManifest.xml.inc};
  StringsTemplate = {$I templates/android/res/values/strings.xml.inc};
var
  AndroidOutput, AndroidExe, AndroidProjectPath: string;
  AndroidStatus: Integer;
begin
  AndroidProjectPath := InclPathDelim(CreateTemporaryDir);

  StringToFile(InclPathDelim(AndroidProjectPath) + 'AndroidManifest.xml',
    ReplaceMacros(AndroidManifestTemplate));

  CheckForceDirectories(AndroidProjectPath + 'res' + PathDelim + 'values');
  StringToFile(InclPathDelim(AndroidProjectPath) +
    'res' + PathDelim + 'values' + PathDelim + 'strings.xml',
    ReplaceMacros(StringsTemplate));

  { try to find "android" tool in $ANDROID_HOME }
  AndroidExe := InclPathDelim(GetEnvironmentVariable('ANDROID_HOME')) +
    'tools' + PathDelim + 'android'  + ExeExtension;
  { try to find "android" tool on $PATH }
  if not FileExists(AndroidExe) then
    AndroidExe := PathFileSearch('android'  + ExeExtension, false);
  if AndroidExe = '' then
    raise Exception.Create('Cannot find "android" executable on $PATH, or within $ANDROID_HOME. Install Android SDK and make sure that "android" executable is on $PATH, or that $ANDROID_HOME environment variable is set correctly.');

  RunCommandIndirPassthrough(AndroidProjectPath, AndroidExe,
    ['update', 'lib-project', '--path', '.', '--target', 'android-8'],
    AndroidOutput, AndroidStatus);
  if AndroidStatus <> 0 then
    raise Exception.Create('"android" call failed, cannot create Android apk. Inspect above error messages, and make sure Android SDK is installed correctly. Make sure that target "android-8" (Android 2.2., API 8) is installed.');

  if not LeaveTemp then
    RemoveNonEmptyDir(AndroidProjectPath);
end;

end.
