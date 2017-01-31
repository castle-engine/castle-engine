{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on FPC packages/fpmkunit/src/fpmkunit.pp unit,
  which conveniently uses *exactly* the same license as Castle Game Engine.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Windows rc and res stuff. }
unit ToolWindowsResources;

interface

uses CastleUtils, CastleStringUtils,
  ToolUtils, ToolArchitectures;

procedure GenerateWindowsResources(const ReplaceMacros: TReplaceMacros;
  const Path: string; const Icons: TIconFileNames; const CPU: TCpu;
  const Plugin: boolean);

implementation

uses SysUtils,
  CastleURIUtils, CastleLog, CastleFilesUtils;

procedure GenerateWindowsResources(const ReplaceMacros: TReplaceMacros;
  const Path: string; const Icons: TIconFileNames; const CPU: TCpu;
  const Plugin: boolean);
const
  RcTemplate: array [boolean { plugin? }] of string = (
    {$I templates/windows/automatic-windows-resources.rc.inc},
    {$I templates/windows/plugin-automatic-windows-resources.rc.inc}
  );
  RcName: array [boolean { plugin? }] of string = (
    'automatic-windows-resources.rc',
    'plugin-automatic-windows-resources.rc'
  );
  ManifestTemplate = {$I templates/windows/automatic-windows.manifest.inc};
var
  IcoPath, OutputRc, OutputManifest: string;
  WindresOutput, WindresExe, RcFilename, ManifestFilename, ResName: string;
  WindresStatus: Integer;
begin
  OutputRc := ReplaceMacros(RcTemplate[Plugin]);

  IcoPath := Icons.FindExtension(['.ico']);
  if IcoPath <> '' then
    OutputRc := 'MainIcon ICON "' + IcoPath + '"' + NL + OutputRc else
    WritelnWarning('Windows Resources', 'Icon in format suitable for Windows (.ico) not found. Exe file will not have icon.');

  RcFilename := InclPathDelim(Path) + RcName[Plugin];
  StringToFile(RcFilename, OutputRc);

  ManifestFilename := InclPathDelim(Path) + 'automatic-windows.manifest';
  OutputManifest := ReplaceMacros(ManifestTemplate);
  StringToFile(ManifestFilename, OutputManifest);

  WindresExe := FindExe('windres');
  if WindresExe = '' then
    case CPU of
      i386  : WindresExe := FindExe('i586-mingw32msvc-windres');
      x86_64: WindresExe := FindExe('amd64-mingw32msvc-windres');
    end;
  { try new names, https://packages.debian.org/search?searchon=contents&keywords=windres&mode=path&suite=stable&arch=any }
  if WindresExe = '' then
    case CPU of
      i386  : WindresExe := FindExe('i686-w64-mingw32-windres');
      x86_64: WindresExe := FindExe('x86_64-w64-mingw32-windres');
    end;
  if WindresExe = '' then
    raise Exception.Create('Cannot find "windres" executable on $PATH. On Windows, it should be installed along with FPC (Free Pascal Compiler), so just make sure FPC is installed and available on $PATH. On Linux, "windres" is usually available as part of MinGW, so install the package named like "mingw*-binutils".');

  ResName := ChangeFileExt(RcName[Plugin], '.res');
  RunCommandIndirPassthrough(Path, WindresExe,
    ['-i', RcName[Plugin], '-o', ResName],
    WindresOutput, WindresStatus);
  if WindresStatus <> 0 then
    raise Exception.Create('windres failed, cannot create Windows resource');

  Writeln('Generated ' + ResName + ', make sure you include it in your .lpr source file like this:');
  Writeln('  {$ifdef MSWINDOWS} {$R ' + ResName + '} {$endif MSWINDOWS}');

  if not LeaveTemp then
  begin
    CheckDeleteFile(RcFilename);
    CheckDeleteFile(ManifestFilename);
  end;
end;

end.
