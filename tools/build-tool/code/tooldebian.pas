{
  Copyright 2021-2022 Yevhen Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Package a project to a DEB (package format of Debian and derivatives). }
unit ToolDebian;

{$I castleconf.inc}

interface

uses
  CastleFindFiles,
  ToolArchitectures, ToolManifest, ToolPackage;

{ Create Debian package (DEB).

  Assumes that PackagedPath has been created by TPackageDirectory,
  and it contains the final executable and data files.

  PackageOutputPath, PackageFileName have the same meaning
  as for TPackageDirectory.Make. }
procedure PackageDebian(const PackagedPath: String;
  const PackageOutputPath, PackageFileName: String;
  const Cpu: TCpu; const Manifest: TCastleManifest);

implementation

uses
  SysUtils, Process, {$ifdef UNIX} BaseUnix, {$endif}
  CastleUtils, CastleFilesUtils, CastleDownload, CastleImages, CastleLog,
  CastleStringUtils,
  ToolCommonUtils, ToolUtils;

{ DirectorySize utility ------------------------------------------------------ }

type
  TDirectorySizeHelper = class
  public
    { Total size of all files in the directory. }
    Size: Int64;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TDirectorySizeHelper.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  Size += FileInfo.Size;
end;

function DirectorySize(const Dir: String): Int64;
var
  Helper: TDirectorySizeHelper;
begin
  Helper := TDirectorySizeHelper.Create;
  try
    FindFiles(Dir, '*', false, {$ifdef FPC}@{$endif}Helper.FoundFile, [ffRecursive]);
    Result := Helper.Size;
  finally FreeAndNil(Helper) end;
end;

{ FindFileCheck ----------------------------------------------------- }

{ Various checks on files/dirs in package main directory. }
procedure FindFileCheck(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: Boolean);
const
  ErrPackagedDescription = 'In Debian package, we only package the main executable and "data" subdirectory now. If you include any other files, they are not placed in the final Debian package.';
var
  Manifest: TCastleManifest;
begin
  Manifest := TCastleManifest(Data);

  if FileInfo.Name = Manifest.ExecutableName then
  begin
    if FileInfo.Directory then
      WritelnWarning('"%s" should not be a directory', [FileInfo.Name]);
  end else
  if FileInfo.Name = 'data' then
  begin
    if not FileInfo.Directory then
      WritelnWarning('"%s" should be a directory', [FileInfo.Name]);
  end else
  begin
    if FileInfo.Directory then
      WritelnWarning('Directory "%s" will not be packaged. ' + ErrPackagedDescription, [FileInfo.Name])
    else
      WritelnWarning('File "%s" will not be packaged. ' + ErrPackagedDescription, [FileInfo.Name]);
  end;
end;

{ PackageDebian -------------------------------------------------------------- }

procedure PackageDebian(const PackagedPath: String;
  const PackageOutputPath, PackageFileName: String;
  const Cpu: TCpu; const Manifest: TCastleManifest);

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
  DebianPackageName, TempPath: String;
  BinariesSize: Int64;
  PathToExecutableLocal, PathToExecutableUnix: String;
  PathToIconFileLocal, PathToIconFileUnix: String;
  ShareDirLocal, ShareDirUrl: String;
  PackageDirBaseName, PackageDirLocal, PackageDirUrl: String;
  ImageMagickExe, PngIcon: String;
  DebianDescription: String;
begin
  TempPath := InclPathDelim(CreateTemporaryDir);

  // Use FindFileCheck to warn about files not packaged into DEB
  FindFiles(PackagedPath, '*', false, {$ifdef FPC}@{$endif}FindFileCheck, Manifest, []);

  // Calculate DebianPackageName, avoid using _ in package name -- not allowed
  DebianPackageName := SReplaceChars(Manifest.Name, '_', '-');
  { dpkg-deb only accepts package names with these characters. }
  if CharsPos(AllChars - AllowedDebianPackageChars, DebianPackageName) <> 0 then
    raise Exception.CreateFmt('Debiian package name is "%s", but for Debian package it can only contain lowercase letters, digits, and -+.', [
      DebianPackageName
    ]);

  PackageDirBaseName := DeleteFileExt(PackageFileName);
  PackageDirLocal := TempPath + PackageDirBaseName;
  PackageDirUrl := StringReplace(PackageDirLocal, PathDelim, '/', [rfReplaceAll]);
  if DirectoryExists(PackageDirLocal) then
    RemoveNonEmptyDir(PackageDirLocal);

  // Copy the executable to /usr/bin/<project-name>
  PathToExecutableUnix := '/usr/bin/' + Manifest.ExecutableName;
  PathToExecutableLocal := StringReplace(PathToExecutableUnix, '/', PathDelim, [rfReplaceAll]);
  CheckForceDirectories(PackageDirLocal + PathDelim + 'usr' + PathDelim + 'bin');
  CheckCopyFile(InclPathDelim(PackagedPath) + Manifest.ExecutableName,
    PackageDirLocal + PathDelim + PathToExecutableLocal);
  { This should be enough for file to be packaged as executable. }
  DoMakeExecutable(PackageDirLocal + PathDelim + PathToExecutableLocal);

  BinariesSize := FileSize(PackageDirLocal + PathDelim + PathToExecutableLocal);

  // Copy the data to /usr/share/<project-data> (ApplicationData is prepared to detect it by default)
  ShareDirLocal := PackageDirLocal + PathDelim + 'usr' + PathDelim + 'share';
  ShareDirUrl := StringReplace(ShareDirLocal, PathDelim, '/', [rfReplaceAll]);
  CheckForceDirectories(ShareDirLocal);

  if DirectoryExists(InclPathDelim(PackagedPath) + 'data') then
  begin
    { ApplicationData expects to find data under /usr/share/<project-name>,
      so using here Manifest.Name. }
    CopyDirectory(InclPathDelim(PackagedPath) + 'data', InclPathDelim(ShareDirLocal) + Manifest.Name);
    BinariesSize += DirectorySize(InclPathDelim(PackagedPath) + 'data');
  end;

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
    ShareDirUrl + '/menu/' + Manifest.ExecutableName,
    '?package(' + DebianPackageName + '): \' + NL +
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
    'Package: ' + DebianPackageName + NL +
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

  // Calculate MD5 checksums

  RunCommandSimple(TempPath + PackageDirBaseName, 'bash', ['-c',
    'find -type f | egrep -v ''^\./DEBIAN'' | xargs --replace=hh -n1 md5sum "hh" | sed ''s/\ \.\///'' > DEBIAN/md5sums']);

  // Package DEB

  RunCommandSimple(TempPath, 'dpkg-deb', ['--build', PackageDirBaseName]);

  CheckRenameFile(TempPath + PackageDirBaseName + '.deb', CombinePaths(PackageOutputPath, PackageFileName));

  // clean up the temporary files
  RemoveNonEmptyDir(TempPath);
end;

end.
