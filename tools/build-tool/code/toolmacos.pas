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

  @param(SymlinkToFiles If @true we'll only make (relative) symlinks to actual project
    executable, icon (if already in icns format) and data,
    otherwise we will copy them into the bundle.) }
procedure CreateAppBundle(const Project: TCastleProject; const SymlinkToFiles: Boolean;
  out ExeInBundle: String);

implementation

uses {$ifdef UNIX} BaseUnix, {$endif} SysUtils,
  CastleFilesUtils, CastleLog, CastleImages,
  ToolArchitectures, ToolCommonUtils, ToolUtils, ToolEmbeddedImages;

{ Turn a PNG -> ICNS.

  Based on
  https://www.codingforentrepreneurs.com/blog/create-icns-icons-for-macos-apps/
  https://stackoverflow.com/questions/12306223/how-to-manually-create-icns-files-using-iconutil/20703594#20703594
  https://gist.github.com/jamieweavis/b4c394607641e1280d447deed5fc85fc
}
function PngToIcns(const Project: TCastleProject; const PngFile: String): String;
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

  RunCommandSimple(TempPath, 'sips', ['-z', '16'  , '16'  , PngFile, '--out', IconsetDir + PathDelim + 'icon_16x16.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '32'  , '32'  , PngFile, '--out', IconsetDir + PathDelim + 'icon_16x16@2x.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '32'  , '32'  , PngFile, '--out', IconsetDir + PathDelim + 'icon_32x32.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '64'  , '64'  , PngFile, '--out', IconsetDir + PathDelim + 'icon_32x32@2x.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '128' , '128' , PngFile, '--out', IconsetDir + PathDelim + 'icon_128x128.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '256' , '256' , PngFile, '--out', IconsetDir + PathDelim + 'icon_128x128@2x.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '256' , '256' , PngFile, '--out', IconsetDir + PathDelim + 'icon_256x256.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '512' , '512' , PngFile, '--out', IconsetDir + PathDelim + 'icon_256x256@2x.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '512' , '512' , PngFile, '--out', IconsetDir + PathDelim + 'icon_512x512.png']);
  RunCommandSimple(TempPath, 'sips', ['-z', '1024', '1024', PngFile, '--out', IconsetDir + PathDelim + 'icon_512x512@2x.png']);

  RunCommandSimple(TempPath, 'iconutil', ['-c', 'icns', Project.Name + '.iconset']);

  Result := TempPath + Project.Name + '.icns';
end;

{ Create App Bundle to run on macOS, with reliable input, menu etc.

  See
  https://wiki.freepascal.org/Application_Bundle
  https://wiki.freepascal.org/macOS_property_list_files
  https://developer.apple.com/library/archive/documentation/CoreFoundation/Conceptual/CFBundles/BundleTypes/BundleTypes.html
}
procedure CreateAppBundle(const Project: TCastleProject; const SymlinkToFiles: Boolean;
  out ExeInBundle: String);

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
begin
  { create clean OutputBundlePath }
  OutputBundlePath := TempOutputPath(Project.Path) +
    'macos' + PathDelim + Project.Name + '.app' + PathDelim;
  if DirectoryExists(OutputBundlePath) then
    RemoveNonEmptyDir(OutputBundlePath);

  Project.ExtractTemplate('macos/app_bundle', OutputBundlePath);

  // check Info.plist correctness, following https://wiki.freepascal.org/macOS_property_list_files
  RunCommandSimple('plutil', [OutputBundlePath + 'Contents' + PathDelim + 'Info.plist']);

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
  if IconIcns = '' then
  begin
    IconPng := Project.Icons.FindExtension(['.png']);
    if IconPng = '' then
    begin
      WritelnWarning('No icon in ICNS or PNG format found, using default Castle Game Engine icon.');
      IconPng := TempOutputPath(Project.Path) + 'macos' + PathDelim + 'cge_icon.png';
      SaveImage(DefaultIcon, IconPng);
    end;
    IconIcns := PngToIcns(Project, IconPng);
  end;
  CopyOrSymlinkFile(IconIcns, OutputBundleResourcesPath + Project.Name + '.icns');

  if Project.DataExists then
    CopyOrSymlinkData(OutputBundleResourcesPath + 'data' + PathDelim);
end;

end.
