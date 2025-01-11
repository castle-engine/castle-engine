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

implementation

uses SysUtils,
  CastleUtils, CastleUriUtils, CastleFilesUtils, CastleOpenDocument,
  CastleInternalArchitectures,
  ToolUtils, ToolCommonUtils, ToolManifest;

procedure CompileWeb(const Project: TCastleProject;
  const CompilerOptions: TCompilerOptions);
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
  CompilerOptions.OS := Wasi;
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

  OpenUrl('http://localhost:3000/');

  // must be run last; Ctrl+C on our build tool should kill the compileserver too
  RunCommandSimple(DistPath, CompileServerExe, ['--port=3000']);
end;

end.
