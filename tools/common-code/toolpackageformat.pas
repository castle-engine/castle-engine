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

{ Packaging format. }
unit ToolPackageFormat;

{$I castleconf.inc}

interface

type
  TPackageFormat = (
    pfDefault,
    pfDirectory,
    pfZip,
    pfTarGz,
    pfDeb,
    pfAndroidApk,
    pfAndroidAppBundle,
    pfIosXcodeProject,
    pfIosArchiveDevelopment,
    pfIosArchiveAdHoc,
    pfIosArchiveAppStore,
    pfNintendoSwitchProject,
    pfMacAppBundle,
    pfMacAppBundleZip
  );
  TPackageFormatNoDefault = pfDirectory..High(TPackageFormat);

function PackageFormatToString(const O: TPackageFormat): String;
function StringToPackageFormat(const S: String): TPackageFormat;

implementation

uses SysUtils;

const
  PackageFormatNames: array [TPackageFormat] of String = (
    'default',
    'directory',
    'zip',
    'targz',
    'deb',
    'android-apk',
    'android-app-bundle',
    'ios-xcode-project',
    'ios-archive-development',
    'ios-archive-ad-hoc',
    'ios-archive-app-store',
    'nintendo-switch-project',
    'mac-app-bundle',
    'mac-app-bundle-zip'
  );

function PackageFormatToString(const O: TPackageFormat): String;
begin
  Result := PackageFormatNames[O];
end;

function StringToPackageFormat(const S: String): TPackageFormat;
begin
  for Result in TPackageFormat do
    if AnsiSameText(PackageFormatNames[Result], S) then
      Exit;
  raise Exception.CreateFmt('Invalid package-format name "%s"', [S]);
end;

end.
