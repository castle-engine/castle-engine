{
  Copyright 2019-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Support Nintendo Switch compilation / packaging. }
unit ToolNintendoSwitch;

interface

uses Classes,
  CastleUtils, CastleStringUtils,
  ToolUtils, ToolArchitectures, ToolCompile, ToolProject;

{ Raise exception that Nintendo Switch is not supported in open-source version. }
procedure NxNotSupported;

{ Compile any Pascal unit for Nintendo Switch.

  CompilerOptions.OS andCompilerOptions.CPU are ignored by this routine.
  This routine may modify CompilerOptions contents. }
procedure CompileNintendoSwitch(
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);

{ Compile a final library for Nintendo Switch.

  CompilerOptions.OS andCompilerOptions.CPU are ignored by this routine.
  This routine may modify CompilerOptions contents. }
procedure CompileNintendoSwitchLibrary(const Project: TCastleProject;
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);

procedure PackageNintendoSwitch(const Project: TCastleProject);

implementation

uses SysUtils,
  CastleFilesUtils, CastleUriUtils,
  ToolPackage;

procedure NxNotSupported;
begin
  raise Exception.Create('The open-source version of the Castle Game Engine build tool does not support Nintendo Switch.' + NL +
    'Contact us if you are a registered Nintendo Developer and you want to use CGE on Nintendo Switch:' + NL +
    'https://castle-engine.io/talk.php');
end;

procedure CompileNintendoSwitch(
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);
begin
  NxNotSupported;
end;

procedure CompileNintendoSwitchLibrary(const Project: TCastleProject;
  const WorkingDirectory, CompileFile: string;
  const CompilerOptions: TCompilerOptions);
begin
  NxNotSupported;
end;

procedure PackageNintendoSwitch(const Project: TCastleProject);
begin
  NxNotSupported;
end;

end.
