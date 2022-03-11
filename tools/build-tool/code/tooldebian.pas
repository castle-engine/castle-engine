{
  Copyright 2021-2021 Yevhen Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".
  Parts of this file are based on FPC packages/fcl-process/src/process.pp ,
  which conveniently uses *exactly* the same license as Castle Game Engine.

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
unit ToolDebian;

{$I castleconf.inc}

interface

uses
  CastleFindFiles,
  ToolArchitectures, ToolManifest, ToolPackage;

type
  { Package a project to a DEB (package format of Debian and derivatives). }
  TPackageDebian = class(TPackageDirectoryAbstract)
  strict private
    { Total size of all binaries in the project, required for Debian metadata }
    BinariesSize: Int64;
    { Helper to find total size of all files inside the folder. }
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  public
    procedure Make(const OutputProjectPath: String; const TempPath: String;
      const PackageFileName: String; const Cpu: TCpu; const Manifest: TCastleManifest);
  end;

implementation

uses
  SysUtils, Process, {$ifdef UNIX} BaseUnix, {$endif}
  CastleUtils, CastleFilesUtils, CastleDownload, CastleImages, CastleLog,
  CastleStringUtils,
  ToolCommonUtils, ToolUtils;

procedure TPackageDebian.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  BinariesSize += FileInfo.Size;
end;

procedure TPackageDebian.Make(const OutputProjectPath: String; const TempPath: String;
  const PackageFileName: String; const Cpu: TCpu; const Manifest: TCastleManifest);

  function CpuToArchitectureString(const Cpu: TCpu): String;
  begin
    case Cpu of
      { Architectures supported by https://packages.debian.org/buster/bash }
      x86_64: Result := 'amd64';
      aarch64: Result := 'arm64';
      arm:
        begin
          Result := 'armel';
          WriteLnWarning('Architecture ' + CpuToString(Cpu) + ' is ambiguous between "armel" and "armhf" in Debian.');
        end;
      i386: Result := 'i386';
      mips: Result := 'mips';
      mipsel: Result := 'mipsel';
      powerpc64:
        begin
          Result := 'ppc64el';
          WriteLnWarning('Architecture ' + CpuToString(Cpu) + ' is ambiguous between "ppc64el" and "ppc64" in Debian.');
        end;
      // "mips64el"
      // "s390x"
      { Unofficial Debian ports https://packages.debian.org/sid/bash }
      m68k: Result := 'm68k';
      {$ifdef VER3_0} // FPC 3.2.2 removed riscv64, see https://github.com/castle-engine/castle-engine/commit/04fed13723a985f37f53b3a852d7d84d7e4b975b
      riscv64: Result := 'riscv64';
      {$endif}
      sparc64: Result := 'sparc64';
      // "alpha"
      // "sh4"
      // "x32" = X32 is an ABI for amd64/x86_64 CPUs using 32-bit integers, longs and pointers
      else {powerpc,sparc,avr,jvm,i8086,riscv32,armeb}
      begin
        Result := CpuToString(Cpu);
        WriteLnWarning('Architecture ' + CpuToString(Cpu) + ' is not officially supported by Debian.');
      end;
    end;
  end;

const
  DefaultIconXpmString = {$I ../embedded_images/tooldefaulticonxpm.inc};
  AllowedDebianPackageChars = ['a'..'z', '0'..'9', '-', '+', '.'];
var
  PathToExecutableLocal, PathToExecutableUnix: String;
  PathToIconFileLocal, PathToIconFileUnix: String;
  ShareDirLocal, ShareDirUrl: String;
  PackageDirLocal, PackageDirUrl: String;
  ImageMagickExe, PngIcon: String;
  DebianDescription: String;
begin
  { dpkg-deb only accepts package names with these characters. }
  if CharsPos(AllChars - AllowedDebianPackageChars, Manifest.Name) <> 0 then
    raise Exception.CreateFmt('Project name is "%s", but for Debian package it can only contain lowercase letters, digits, and -+. .' + NL +
      '  TODO: In the future, we will fix project name for Debian to work with any CGE-allowed project name.', [
      Manifest.Name
    ]);

  PackageDirLocal := TempPath + PackageFileName;
  PackageDirUrl := StringReplace(PackageDirLocal, PathDelim, '/', [rfReplaceAll]);
  if DirectoryExists(PackageDirLocal) then
    RemoveNonEmptyDir(PackageDirLocal);

  // Copy the binaries

  PathToExecutableUnix := '/usr/bin/' + Manifest.Name;
  PathToExecutableLocal := StringReplace(PathToExecutableUnix, '/', PathDelim, [rfReplaceAll]);
  CopyDirectory(Path, PackageDirLocal + PathToExecutableLocal);

  ShareDirLocal := PackageDirLocal + PathDelim + 'usr' + PathDelim + 'share';
  ShareDirUrl := StringReplace(ShareDirLocal, PathDelim, '/', [rfReplaceAll]);
  CheckForceDirectories(ShareDirLocal);

  // Calculate binaries size

  BinariesSize := 0;
  FindFiles(PackageDirLocal + PathToExecutableLocal, '*', false, {$ifdef FPC}@{$endif}FoundFile, [ffRecursive]);

  // Copy or generate XPM icon

  CheckForceDirectories(ShareDirLocal + PathDelim + 'pixmaps');
  PathToIconFileUnix := '/usr/share/pixmaps/' + Manifest.ExecutableName + '.xpm';
  PathToIconFileLocal := StringReplace(PathToIconFileUnix, '/', PathDelim, [rfReplaceAll]);
  if Manifest.Icons.FindExtension(['.xpm']) <> '' then
    CheckCopyFile(Manifest.Icons.FindExtension(['.xpm']), PackageDirLocal + PathToIconFileLocal)
  else
  begin
    PngIcon := Manifest.Icons.FindExtension(['.png']);
    // using ImageMagick - FPWriteXPM first doesn't properly write alpha channel, second uses palette char size = 2 which results in large files
    ImageMagickExe := FindExe('convert');
    if (PngIcon <> '') and (ImageMagickExe <> '') then
    begin
      WritelnVerbose('Converting PNG icon to XPM using ImageMagick.');
      // 96 colors in XPM still produces 1 symbol per color, larger values double the file size
      RunCommandSimple(ImageMagickExe, ['-colors', '96', PngIcon, PackageDirLocal + PathToIconFileLocal])
    end else
    begin
      WritelnVerbose('Using default XPM icon.' + NL +
        '  Reason:' + NL +
        '  - We did not find in CastleEngineManifest.xml XPM icon.' + NL +
        '  - We also did not find in CastleEngineManifest.xml PNG icon or we did not find ImageMagick.');
      StringToFile(PackageDirLocal + PathToIconFileLocal, DefaultIconXpmString);
    end;
  end;

  // Create menu item for the game

  CheckForceDirectories(ShareDirLocal + PathDelim + 'menu');
  StringToFile(
    ShareDirUrl + '/menu/' + Manifest.Name,
    '?package(' + Manifest.Name + '): \' + NL +
    'needs="X11" \' + NL +
    'section="' + Manifest.DebianMenuSection + '" \' + NL +
    'title="' + Manifest.Caption + '" \' + NL +
    { Note: this assumes that the application is installed under $PATH,
      and that application will find its own system-wide data using standard
      ApplicationData logic on Linux. }
    'command="' + Manifest.ExecutableName + '" \' + NL +
    'icon="' + PathToIconFileUnix +'"'
  );

  CheckForceDirectories(ShareDirLocal + PathDelim + 'applications');
  StringToFile(
    ShareDirUrl + '/applications/' + Manifest.ExecutableName + '.desktop',
    '[Desktop Entry]' + NL +
    'Version=' + Manifest.Version.DisplayValue + NL +
    'Terminal=false' + NL +
    'Exec=' + Manifest.ExecutableName + NL +
    'Icon=' + PathToIconFileUnix + NL +
    'Type=Application' + NL +
    'Categories=' + Manifest.FreeDesktopCategories + NL +
    'Name=' + Manifest.Caption + NL +
    'Comment=' + Manifest.FreeDesktopComment
  );

  if Trim(Manifest.Author) = '' then
    WriteLnWarning('No "author" was provided in CastleEngineManifest.xml. This field is recommended to build a Debian package.');
  if Trim(Manifest.FreeDesktopComment) = '' then
  begin
    WriteLnWarning('No "comment" was provided in CastleEngineManifest.xml. This field is recommended to build a Debian package.');
    DebianDescription := 'No comment provided';
  end else
    DebianDescription := Manifest.FreeDesktopComment;

  CheckForceDirectories(PackageDirLocal + PathDelim + 'DEBIAN');
  StringToFile(
    PackageDirUrl + '/DEBIAN/control',
    'Package: ' + Manifest.Name + NL +
    'Version: ' + Manifest.Version.DisplayValue + NL +
    'Section: ' + Manifest.DebianControlSection + NL +
    'Priority: optional' + NL +
    'Architecture: ' + CpuToArchitectureString(Cpu) + NL +
    'Maintainer: ' + Manifest.Author + NL +
    'Installed-Size: ' + IntToStr(BinariesSize div 1024) + NL +
    { TODO: This should reflect Project.Dependencies. }
    'Depends: libopenal1, libpng16-16, zlib1g, libvorbis0a, libvorbisfile3, libfreetype6, libgl1-mesa-dri, libgtk2.0-0' + NL +
    'Description: ' + Manifest.Caption + NL +
    ' ' + DebianDescription + NL //final new line
  );

  // Post-installation running - assign executable permissions

  StringToFile(
    PackageDirUrl + '/DEBIAN/postinst',
    '#!/bin/sh' + NL + NL +
    'chmod +x ' + PathToExecutableUnix + '/' + Manifest.ExecutableName + NL + NL +
    'exit 0'
  );
  DoMakeExecutable(PackageDirLocal + PathDelim + 'DEBIAN' + PathDelim + 'postinst');

  // Calculate MD5 checksums

  RunCommandSimple(TempPath + PackageFileName, 'bash', ['-c',
    'find -type f | egrep -v ''^\./DEBIAN'' | xargs --replace=hh -n1 md5sum "hh" | sed ''s/\ \.\///'' > DEBIAN/md5sums']);

  // Package DEB

  RunCommandSimple(TempPath, 'dpkg-deb', ['--build', PackageFileName]);
  RenameFile(PackageDirLocal + '.deb', PackageFileName + '.deb');

  // And finally clean up the temporary files
  RemoveNonEmptyDir(PackageDirLocal);
  DeleteFile(TempPath + 'package-debian.sh');
end;

end.
