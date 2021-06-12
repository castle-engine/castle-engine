unit ToolDebian;

{$I castleconf.inc}

interface

uses
  CastleFindFiles,
  ToolArchitectures, ToolManifest, ToolPackage;

type
  { Package a project to a directory. }
  TPackageDebian = class(TPackageDirectoryAbstract)
  strict private
    { Total size of all binaries in the project, required for Debian metadata }
    BinariesSize: Int64;
    { Helper to find total size of all files inside the folder,
      To satisfy FindFile syntax has to be of Object }
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  public
    procedure Make(const OutputProjectPath: String; const PackageFileName: String;
      const Cpu: TCpu; const Manifest: TCastleManifest);
  end;

implementation
uses
  SysUtils, Process, {$ifdef UNIX} BaseUnix, {$endif}
  CastleUtils, CastleFilesUtils, CastleDownload, CastleLog;

procedure TPackageDebian.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  BinariesSize += FileInfo.Size;
end;

procedure TPackageDebian.Make(const OutputProjectPath: String; const PackageFileName: String;
      const Cpu: TCpu; const Manifest: TCastleManifest);

  function CpuToArchitectureString(const Cpu: TCpu): String;
  begin
    case Cpu of
      { Architectures supported by https://packages.debian.org/buster/bash }
      x86_64: Result := 'amd64';
      aarch64: Result := 'arm64';
      arm: begin
          Result := 'armel';
          WriteLnWarning('Architecture ' + CpuToString(Cpu) + ' is ambiguous between "armel" and "armhf" in Debian.');
        end;
      i386: Result := 'i386';
      mips: Result := 'mips';
      mipsel: Result := 'mipsel';
      powerpc64: begin
          Result := 'ppc64el';
          WriteLnWarning('Architecture ' + CpuToString(Cpu) + ' is ambiguous between "ppc64el" and "ppc64" in Debian.');
        end;
      // "mips64el"
      // "s390x"
      { Unofficial Debian ports https://packages.debian.org/sid/bash }
      m68k: Result := 'm68k';
      riscv64: Result := 'riscv64';
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

var
  AppCategoryFolder: String = 'games';
  AppCategoryDebian: String = 'Games/Adventure';
  ProjectComment: String = 'Roguelite hide-and-seek game on an infinite map';
  CategoriesString: String = 'Game;RolePlaying;';
  ProjectIconFile: String = 'icon.xpm';


  PathToExecutableLocal, PathToExecutableUnix: String;
  PathToIconFileLocal, PathToIconFileUnix: String;
  ShareDir: String;
  TextWriter: TTextWriter;
  OutString: String;
begin
  if DirectoryExists(PackageFileName) then
    RemoveNonEmptyDir(PackageFileName);

  // Copy the binaries

  PathToExecutableUnix := '/usr/' + AppCategoryFolder + '/' + Manifest.Name;
  PathToExecutableLocal := StringReplace(PathToExecutableUnix, '/', PathDelim, [rfReplaceAll]);
  CopyDirectory(Path, PackageFileName + PathToExecutableLocal);
  ShareDir := PackageFileName + PathDelim + 'usr' + PathDelim + 'share';
  CreateDir(ShareDir);

  // Calculate binaries size

  BinariesSize := 0;
  FindFiles(PackageFileName + PathToExecutableLocal, '*', false, {$ifdef CASTLE_OBJFPC}@{$endif} FoundFile, [ffRecursive]);

  // Copy XPM icon

  CreateDir(ShareDir + PathDelim + 'pixmaps');
  PathToIconFileUnix := '/usr/share/pixmaps/' + Manifest.ExecutableName + '.xpm';
  PathToIconFileLocal := StringReplace(PathToIconFileUnix, '/', PathDelim, [rfReplaceAll]);
  CheckCopyFile(ProjectIconFile, PackageFileName + PathToIconFileLocal);

  // Create menu item for the game

  CreateDir(ShareDir + PathDelim + 'menu');
  TextWriter := TTextWriter.Create(ShareDir + PathDelim + 'menu' + PathDelim + Manifest.Name);
  TextWriter.Write(
    '?package(' + Manifest.Name + '): \' + NL +
    'needs="X11" \' + NL +
    'section="' + AppCategoryDebian + '" \' + NL +
    'title="' + Manifest.Caption + '" \' + NL +
    'command="bash -c ''cd ' + PathToExecutableUnix + ' && ./' + Manifest.ExecutableName + '''" \' + NL +
    'icon="' + PathToIconFileUnix +'"'
    );
  FreeAndNil(TextWriter);

  CreateDir(ShareDir + PathDelim + 'applications');
  TextWriter := TTextWriter.Create(ShareDir + PathDelim + 'applications' + PathDelim + Manifest.ExecutableName + '.desktop');
  TextWriter.Write(
    '[Desktop Entry]' + NL +
    'Version=' + Manifest.Version.DisplayValue + NL +
    'Terminal=false' + NL +
    'Exec=bash -c ''cd ' + PathToExecutableUnix + ' && ./' + Manifest.ExecutableName + '''' + NL +
    'Icon=' + PathToIconFileUnix + NL +
    'Type=Application' + NL +
    'Categories=' + CategoriesString + NL +
    'Name=' + Manifest.Caption + NL +
    'Comment=' + ProjectComment
    );
  FreeAndNil(TextWriter);

  CreateDir(PackageFileName + PathDelim + 'DEBIAN');
  TextWriter := TTextWriter.Create(PackageFileName + PathDelim + 'DEBIAN' + PathDelim + 'control');
  TextWriter.Write(
    'Package: ' + Manifest.Name + NL +
    'Version: ' + Manifest.Version.DisplayValue + NL +
    'Section: ' + AppCategoryFolder + NL +
    'Priority: optional' + NL +
    'Architecture: ' + CpuToArchitectureString(Cpu) + NL +
    'Maintainer: ' + Manifest.Author + NL +
    'Installed-Size: ' + IntToStr(BinariesSize div 1024) + NL +
    'Depends: libopenal1, libpng16-16, zlib1g, libvorbis0a, libvorbisfile3, libfreetype6, libgl1-mesa-dri, libgtk2.0-0' + NL +
    'Description: ' + Manifest.Caption + NL +
    ' ' + ProjectComment + NL //final new line
    );
  FreeAndNil(TextWriter);

  // Post-installation running - assign executable permissions

  TextWriter := TTextWriter.Create(PackageFileName + PathDelim + 'DEBIAN' + PathDelim + 'postinst');
  TextWriter.Write(
    '#!/bin/sh' + NL + NL +
    'chmod +x ' + PathToExecutableUnix + '/' + Manifest.ExecutableName + NL + NL +
    'exit 0'
    );
  FreeAndNil(TextWriter);
  fpChmod(PackageFileName + PathDelim + 'DEBIAN' + PathDelim + 'postinst', &555);

  // Workaround, we need to execute a shell script somehow

  TextWriter := TTextWriter.Create(OutputProjectPath + 'package-debian.sh');
  TextWriter.Write(
    'cd ' + PackageFileName + NL +
    'find -type f | egrep -v ''^\./DEBIAN'' | xargs --replace=hh -n1 md5sum "hh" | sed ''s/\ \.\///'' > DEBIAN/md5sums' + NL +
    'cd ..' + NL +
    'dpkg-deb --build ' + PackageFileName
    );
  FreeAndNil(TextWriter);

  //RunCommandSimple('sh ' + OutputProjectPath + 'package-debian.sh', []);
  RunCommand('/bin/bash', ['package-debian.sh'], OutString);
  WriteLn(OutString);
end;

end.

