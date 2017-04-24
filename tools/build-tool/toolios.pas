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

procedure CompileIOS(const Plugin: boolean;
  const Mode: TCompilationMode; const WorkingDirectory, CompileFile: string;
  const SearchPaths: TStrings);

  procedure CompileLibrary(const OS: TOS; const CPU: TCPU);
  begin
    Compile(OS, CPU, Plugin, Mode, WorkingDirectory, CompileFile, SearchPaths);

    (*
    local LINK_RES="${DIR}/link.res"
    local OUTPUT_LIB="${DIR}/${EXECUTABLE_NAME}"

    grep '\.o$' "${LINK_RES}" > compile_ios_filelist.tmp
    run_logging libtool -static -o "${OUTPUT_LIB}" -filelist compile_ios_filelist.tmp
    if [ ! -e "${OUTPUT_LIB}" ]; then
      echo "Error: Output ${OUTPUT_LIB} not found"
      exit 1
    fi
    rm -f compile_ios_filelist.tmp
    *)
  end;

begin
  CompileLibrary(iphonesim, i386);
  CompileLibrary(iphonesim, x86_64);
  CompileLibrary(darwin, arm);
  CompileLibrary(darwin, aarch64);
end;

procedure LinkIOSLibrary(const CompilationWorkingDirectory, OutputFile: string);
begin
  // TODO: libtool -static "${OUTPUT_LIBRARIES[@]}" -o "./$EXECUTABLE_NAME"
  // TODO
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
