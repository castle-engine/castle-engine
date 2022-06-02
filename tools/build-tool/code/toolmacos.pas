{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ macOS specific utilities. }
unit ToolMacOS;

interface

uses Classes,
  CastleUtils, CastleStringUtils,
  ToolProject;

{ Create AppBundle to run the project in castle-engine-output.
  This is the only reliable way to run GUI applications on macOS.

  @param(BundleParenPath Path (may, but doesn't have to, end with PathDelim)
    where the xxx.app directory (app bundle) will be placed.)

  @param(SymlinkToFiles If @true we'll only make (relative) symlinks to actual project
    executable, icon (if already in icns format) and data,
    otherwise we will copy them into the bundle.) }
procedure CreateMacAppBundle(const Project: TCastleProject; const BundleParenPath: String;
  const SymlinkToFiles: Boolean; out ExeInBundle: String);
procedure CreateMacAppBundle(const Project: TCastleProject; const BundleParenPath: String;
  const SymlinkToFiles: Boolean);

implementation

uses {$ifdef UNIX} BaseUnix, {$endif} SysUtils,
  CastleFilesUtils, CastleLog, CastleImages,
  ToolArchitectures, ToolCommonUtils, ToolUtils, ToolEmbeddedImages;

procedure SaveResized(const Image: TCastleImage; const Size: Integer; const OutputFileName: string);
var
  R: TCastleImage;
begin
  R := Image.MakeResized(Size, Size, BestInterpolation);
  try
    SaveImage(R, OutputFileName);
  finally FreeAndNil(R) end;
end;

{ Turn a TCastleImage -> ICNS.

  Based on
  https://www.codingforentrepreneurs.com/blog/create-icns-icons-for-macos-apps/
  https://stackoverflow.com/questions/12306223/how-to-manually-create-icns-files-using-iconutil/20703594#20703594
  https://gist.github.com/jamieweavis/b4c394607641e1280d447deed5fc85fc

  We don't use sips, ImageMagick etc., instead process image using our TCastleImage.
  This way we control the resize quality and the output messages.
}
function ImageToIcns(const Project: TCastleProject; const Image: TCastleImage): String;
var
  TempPath, IconsetDir: String;
begin
  { create clean TempPath }
  TempPath := TempOutputPath(Project.Path) +
    'macos' + PathDelim + 'icns_generation' + PathDelim;
  if DirectoryExists(TempPath) then
    RemoveNonEmptyDir(TempPath);

  IconsetDir := TempPath + Project.Name + '.iconset';
  ForceDirectories(IconsetDir);

  SaveResized(Image,   16, IconsetDir + PathDelim + 'icon_16x16.png');
  SaveResized(Image,   32, IconsetDir + PathDelim + 'icon_16x16@2x.png');
  SaveResized(Image,   32, IconsetDir + PathDelim + 'icon_32x32.png');
  SaveResized(Image,   64, IconsetDir + PathDelim + 'icon_32x32@2x.png');
  SaveResized(Image,  128, IconsetDir + PathDelim + 'icon_128x128.png');
  SaveResized(Image,  256, IconsetDir + PathDelim + 'icon_128x128@2x.png');
  SaveResized(Image,  256, IconsetDir + PathDelim + 'icon_256x256.png');
  SaveResized(Image,  512, IconsetDir + PathDelim + 'icon_256x256@2x.png');
  SaveResized(Image,  512, IconsetDir + PathDelim + 'icon_512x512.png');
  SaveResized(Image, 1024, IconsetDir + PathDelim + 'icon_512x512@2x.png');

  RunCommandSimple(TempPath, 'iconutil', ['-c', 'icns', Project.Name + '.iconset']);

  Result := TempPath + Project.Name + '.icns';
end;

{ Create App Bundle to run on macOS, with reliable input, menu etc.

  See
  https://wiki.freepascal.org/Application_Bundle
  https://wiki.freepascal.org/macOS_property_list_files
  https://developer.apple.com/library/archive/documentation/CoreFoundation/Conceptual/CFBundles/BundleTypes/BundleTypes.html
}
procedure CreateMacAppBundle(const Project: TCastleProject; const BundleParenPath: String;
  const SymlinkToFiles: Boolean; out ExeInBundle: String);

  procedure SymlinkCheck(const Src, Dst: String);
  begin
    {$ifdef UNIX}
    OSCheck(FpSymlink(PChar(Src), PChar(Dst)) <> -1,
      Format('Cannot create symlink "%s" -> "%s"', [
        Src,
        Dst
      ]));
    {$else}
    raise Exception.CreateFmt('Cannot create symlink on non-Unix systems. Tried to create symlink "%s" -> "%s"', [
      Src,
      Dst
    ]);
    {$endif}
  end;

  procedure CopyOrSymlinkFile(const Src, Dst: String);
  begin
    if SymlinkToFiles then
      SymlinkCheck(Src, Dst)
    else
      CheckCopyFile(Src, Dst);
  end;

  procedure CopyOrSymlinkData(const Dst: String);
  begin
    if SymlinkToFiles then
      SymlinkCheck(ExclPathDelim(Project.DataPath), ExclPathDelim(Dst))
    else
      Project.CopyData(Dst, cpDesktop);
  end;

var
  OutputBundlePath, OutputBundleExePath, OutputBundleResourcesPath, IconIcns, IconPng: String;
  LoadedIcon: TCastleImage;
begin
  { create clean OutputBundlePath }
  OutputBundlePath := InclPathDelim(BundleParenPath) + Project.Caption + '.app' + PathDelim;
  if DirectoryExists(OutputBundlePath) then
    RemoveNonEmptyDir(OutputBundlePath);

  Project.ExtractTemplate('macos/app_bundle', OutputBundlePath);

  { Check Info.plist correctness, following https://wiki.freepascal.org/macOS_property_list_files.
    See https://www.unix.com/man-page/osx/1/plutil/ for command-line options:
    -s means "Don't print anything on success." }
  RunCommandSimple('plutil', ['-s', OutputBundlePath + 'Contents' + PathDelim + 'Info.plist']);

  OutputBundleExePath := OutputBundlePath +
    'Contents' + PathDelim + 'MacOS' + PathDelim;
  OutputBundleResourcesPath := OutputBundlePath +
    'Contents' + PathDelim + 'Resources' + PathDelim;

  ForceDirectories(OutputBundleExePath);
  ForceDirectories(OutputBundleResourcesPath);

  ExeInBundle := OutputBundleExePath + Project.ExecutableName;
  CopyOrSymlinkFile(Project.Path + Project.ExecutableName, ExeInBundle);
  DoMakeExecutable(ExeInBundle);

  IconIcns := Project.Icons.FindExtension(['.icns']);
  if IconIcns <> '' then
  begin
    { Icons.FindExtension is relative to project path,
      we need absolute path -- as current dir may not be project path,
      and also for symlink we need absolute path. }
    IconIcns := CombinePaths(Project.Path, IconIcns);
  end else
  begin
    IconPng := Project.Icons.FindExtension(['.png']);
    if IconPng <> '' then
    begin
      LoadedIcon := LoadImage(IconPng);
      try
        IconIcns := ImageToIcns(Project, LoadedIcon);
      finally FreeAndNil(LoadedIcon) end;
    end else
      IconIcns := ImageToIcns(Project, DefaultIcon);
  end;
  CopyOrSymlinkFile(IconIcns, OutputBundleResourcesPath + Project.Name + '.icns');

  if Project.DataExists then
    CopyOrSymlinkData(OutputBundleResourcesPath + 'data' + PathDelim);

  Writeln(Format('Created macOS AppBundle %s"%s"', [
    Iff(SymlinkToFiles, '(using symlinks to actual data, in temp dir) ', ''),
    ExtractFileName(ExclPathDelim(OutputBundlePath))
  ]));
end;

procedure CreateMacAppBundle(const Project: TCastleProject; const BundleParenPath: String;
  const SymlinkToFiles: Boolean);
var
  IgnoreExeInBundle: String;
begin
  CreateMacAppBundle(Project, BundleParenPath, SymlinkToFiles, IgnoreExeInBundle);
end;

end.
