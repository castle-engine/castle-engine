{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ iOS specific utilities. }
unit ToolIOS;

interface

uses Classes,
  CastleUtils, CastleStringUtils,
  ToolUtils, ToolArchitectures, ToolCompile, ToolProject;

procedure CompileIOS(const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths: TStrings);

procedure LinkIOSLibrary(const CompilationWorkingDirectory, OutputFile: string);

procedure PackageIOS(const Project: TCastleProject);
procedure InstallIOS(const Project: TCastleProject);
procedure RunIOS(const Project: TCastleProject);

implementation

uses SysUtils;

const
  IOSPartialLibraryName = 'lib_cge_project.a';

procedure CompileIOS(const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths: TStrings);

  procedure CompileLibrary(const OS: TOS; const CPU: TCPU);
  var
    CompilationOutput, LinkRes, OutputLibrary: string;
    LinkResContents, ObjectFiles: TCastleStringList;
    I: Integer;
  begin
    Compile(OS, CPU, Plugin, Mode, WorkingDirectory, CompileFile, SearchPaths);

    CompilationOutput := CompilationOutputPath(OS, CPU, WorkingDirectory);
    LinkRes := CompilationOutput + 'link.res';
    if not FileExists(LinkRes) then
    begin
      if Verbose then
        Writeln('link.res not found inside "', LinkRes, '", probably what we compiled was only a unit, not a library');
    end else
    begin
      OutputLibrary := CompilationOutput + IOSPartialLibraryName;

      { grep '\.o$' link.res > lib_cge_project_object_files.txt }
      LinkResContents := TCastleStringList.Create;
      try
        LinkResContents.LoadFromFile(LinkRes);

        ObjectFiles := TCastleStringList.Create;
        try
          for I := 0 to LinkResContents.Count - 1 do
            if IsSuffix('.o', LinkResContents[I]) then
              ObjectFiles.Add(LinkResContents[I]);
          ObjectFiles.SaveToFile(CompilationOutput + 'lib_cge_project_object_files.txt');
        finally FreeAndNil(ObjectFiles) end;
      finally FreeAndNil(LinkResContents) end;

      RunCommandSimple('libtool', ['-static', '-o', OutputLibrary, '-filelist',
        CompilationOutput + 'lib_cge_project_object_files.txt']);
      if not FileExists(OutputLibrary) then
        raise Exception.CreateFmt('Creating library "%s" failed', [OutputLibrary]);
    end;
  end;

begin
  CompileLibrary(iphonesim, i386);
  CompileLibrary(iphonesim, x86_64);
  CompileLibrary(darwin, arm);
  CompileLibrary(darwin, aarch64);
end;

procedure LinkIOSLibrary(const CompilationWorkingDirectory, OutputFile: string);
begin
  RunCommandSimple('libtool', ['-static', '-o', OutputFile,
    CompilationOutputPath(iphonesim, i386   , CompilationWorkingDirectory) + IOSPartialLibraryName,
    CompilationOutputPath(iphonesim, x86_64 , CompilationWorkingDirectory) + IOSPartialLibraryName,
    CompilationOutputPath(darwin   , arm    , CompilationWorkingDirectory) + IOSPartialLibraryName,
    CompilationOutputPath(darwin   , aarch64, CompilationWorkingDirectory) + IOSPartialLibraryName
  ]);
end;

procedure PackageIOS(const Project: TCastleProject);
begin
  // TODO
  raise Exception.Create('The "run" command is not implemented for iOS right now');
end;

procedure InstallIOS(const Project: TCastleProject);
begin
  // TODO
  raise Exception.Create('The "run" command is not implemented for iOS right now');
end;

procedure RunIOS(const Project: TCastleProject);
begin
  // TODO
  raise Exception.Create('The "run" command is not implemented for iOS right now');
end;

end.
