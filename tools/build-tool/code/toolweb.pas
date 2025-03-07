{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities specific to web target.
  See https://castle-engine.io/web for an overview how this works. }
unit ToolWeb;

interface

uses Classes,
  ToolProject, ToolCompile;

procedure CompileWeb(const Project: TCastleProject;
  const CompilerOptions: TCompilerOptions);
procedure RunWeb(const Project: TCastleProject);

{ Package web dist/, made by CompileWeb.
  PackageZipFileName is only name (without directory) of the zip file,
  it will be placed in Project.OutputPath, like all "package" output. }
procedure PackageWeb(const Project: TCastleProject;
  const PackageZipFileName: String);

implementation

uses SysUtils,
  CastleUtils, CastleUriUtils, CastleFilesUtils, CastleOpenDocument,
  CastleInternalArchitectures, CastleStringUtils, CastleTimeUtils,
  ToolUtils, ToolCommonUtils, ToolManifest, ToolFonts;

procedure CompileWeb(const Project: TCastleProject;
  const CompilerOptions: TCompilerOptions);

  procedure RunWasmOpt(const WorkingDirectory, ProjectWasmExe: String);
  var
    WasmOptExe: String;
    SizeBefore, SizeAfter: Int64;
    TimeStart: TTimerResult;
  begin
    WasmOptExe := FindExe('wasm-opt');
    if WasmOptExe <> '' then
    begin
      Writeln('wasm-opt found, using it to optimize the release build.');
      SizeBefore := FileSize(ProjectWasmExe);
      TimeStart := Timer;
      RunCommandSimple(WorkingDirectory, WasmOptExe, [
        '-O3',
        '--all-features',
        ProjectWasmExe,
        '-o', ProjectWasmExe
      ]);
      SizeAfter := FileSize(ProjectWasmExe);
      Writeln(Format('Optimized .wasm file in %fsec, size decrease %d%% (from %s to %s).', [
        TimeStart.ElapsedTime,
        Round(100 * SizeAfter / SizeBefore),
        SizeToStr(SizeBefore),
        SizeToStr(SizeAfter)
      ]));
    end;
  end;

var
  OutputPath, DistPath, Pas2jsExe, SourceExe, DestExe,
    LibraryFileName: String;
begin
  OutputPath := TempOutputPath(Project.Path) + 'web' + PathDelim;
  DistPath := OutputPath + 'dist' + PathDelim;

  // always start clean
  if DirectoryExists(OutputPath) then
    RemoveNonEmptyDir(OutputPath);

  Project.ExtractTemplate('web/', OutputPath);

  // place auto-generated fonts alongside the WASM library library_template.lpr
  GenerateEmbeddedFonts(Project, OutputPath);

  Pas2jsExe := FindExe('pas2js');
  if Pas2jsExe = '' then
    raise Exception.Create('Cannot find "pas2js" executable on $PATH');

  RunCommandSimple(OutputPath, Pas2jsExe, [
    '-Jc', '-Jirtl.js',
    '-Tbrowser',
    // no hints -- just like FPC, pas2js displays excessive hints
    '-vh-',
    // Note: to easily find the error message number, pass -vq .
    // avoid warning we cannot do anything about: ".../pas2js/packages/rtl/src/js.pas(1012,15) Warning: Symbol "Int64" is not implemented"
    '-vm5078',
    // avoid warnings from standard units we cannot do anything about like ".../rtl/src/sysutils.pas(3106,24) Warning: Symbol "substr" is deprecated"
    '-vm5043',
    'program_js.lpr']);
  CheckRenameFile(OutputPath + 'program_js.js', DistPath + Project.ExecutableName + '.js');

  { Compile library. }
  LibraryFileName := OutputPath + 'library_template.lpr';
  CompilerOptions.OS := WasiP1;
  CompilerOptions.CPU := Wasm32;
  Compile(coFpc, Project.Path, LibraryFileName, CompilerOptions);

  // move and rename to castle-engine-output/web/dist/xxx.wasm
  SourceExe := CombinePaths(Project.Path,
    { We build "library", not "program" now for web
      and FPC by default doesn't add .wasm extension to the produced library file. }
    //ChangeFileExt(LibraryFileName, ExeExtensionOS(CompilerOptions.OS)));
    ChangeFileExt(LibraryFileName, ''));
  DestExe := CombinePaths(DistPath,
    ChangeFileExt(Project.ExecutableName, ExeExtensionOS(CompilerOptions.OS)));
  { move exe to dist/ and eventually rename to follow ExecutableName }
  MoveFileVerbose(SourceExe, DestExe);

  if CompilerOptions.Mode = cmRelease then
    RunWasmOpt(DistPath, DestExe);

  { Place data "zip", to be ready for "run" after "compile".
    And to enable using "compile" to populate the dist/ with everything necessary. }
  Project.ZipData(DistPath, cpWeb);
end;

procedure RunWeb(const Project: TCastleProject);
var
  OutputPath, DistPath, CompileServerExe: String;
begin
  OutputPath := TempOutputPath(Project.Path) + 'web' + PathDelim;
  DistPath := OutputPath + 'dist' + PathDelim;

  CompileServerExe := FindExe('compileserver');
  if CompileServerExe = '' then
    raise Exception.Create('Cannot find "compileserver" executable (part of Pa2js utilities) on $PATH');

  // OpenUrl('http://localhost:3000/'); // also works, but allows browsers to cache index.html
  OpenUrl('http://localhost:3000/index.html?random_suffix_to_avoid_cache=' + RandomString);

  // must be run last; Ctrl+C on our build tool should kill the compileserver too
  RunCommandSimple(DistPath, CompileServerExe, ['--port=3000']);
end;

procedure PackageWeb(const Project: TCastleProject;
  const PackageZipFileName: String);
var
  OutputPath, DistPath, FullZipFileName: String;
begin
  OutputPath := TempOutputPath(Project.Path) + 'web' + PathDelim;
  DistPath := OutputPath + 'dist' + PathDelim;

  FullZipFileName := CombinePaths(Project.OutputPath, PackageZipFileName);
  ZipDirectoryTool(FullZipFileName, DistPath, false);

  Writeln(Format('Packed web distributable files to "%s"', [PackageZipFileName]));
end;

end.
