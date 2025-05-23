{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Project-related castle-editor utilities. }
unit ProjectUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Fill directory for new project with the build-tool generated stuff. }
procedure GenerateProgramWithBuildTool(const ProjectDirUrl: String);

type
  TBuildMode = (bmDebug, bmRelease);

{ For some operations (like creating a project from template
  or loading demo design castle-data:/demo_animation/view_demo_animation.castle-user-interface )
  the editor uses castle-data:/ URLs, assuming they lead to editor's data.

  To do this, make sure that castle-data:/ is correct,
  by setting ApplicationDataOverride.
  We can use CastleEnginePath (that uses $CASTLE_ENGINE_PATH environment variable)
  for this. }
procedure UseEditorApplicationData;

type
  { Make sure that castle-data:/ points to editor data on creation,
    make sure it's restored to previous state on destruction. }
  TEditorApplicationData = class
  strict private
    OldApplicationDataOverride: String;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Forms,
  CastleUriUtils, CastleStringUtils, CastleFindFiles, CastleUtils,
  CastleFilesUtils,
  ToolCommonUtils, ToolProcessRun,
  EditorUtils, FormProject;

procedure GenerateProgramWithBuildTool(const ProjectDirUrl: String);
var
  BuildToolExe, BuildToolOutput: String;
  BuildToolStatus: integer;
  NewEnvironment: TStringList;
begin
  BuildToolExe := FindExeCastleTool('castle-engine');
  if BuildToolExe = '' then
  begin
    WarningBox('Cannot find build tool (castle-engine) on $PATH environment variable. You will need to manually run "castle-engine generate-program" within project''s directory.');
    Exit;
  end;

  { Pass CASTLE_ENGINE_PATH to build tool, to use the same CGE as detected by editor.
    This means that e.g. editor that autodetects CGE (based on GetCastleEnginePathFromExeName,
    because editor exe is in <cge>/tools/castle-editor/castle-editor)
    invokes build tool in local bin (like ~/bin)
    and the build tool uses the same <cge> as detected by editor. }
  NewEnvironment := nil;
  if CastleEnginePath <> '' then
  begin
    NewEnvironment := EnvironmentStrings;
    NewEnvironment.Values['CASTLE_ENGINE_PATH'] := CastleEnginePath;
  end;

  MyRunCommandIndir(UriToFilenameSafe(ProjectDirUrl), BuildToolExe,
    ['generate-program'], BuildToolOutput, BuildToolStatus, nil, nil,
    // prevent from blinking console on Windows
    [rcNoConsole],
    NewEnvironment);
  if BuildToolStatus <> 0 then
  begin
    WarningBox(Format('Generating program with the build tool failed with status code %d and output: "%s"',
      [BuildToolStatus, BuildToolOutput]));
    Exit;
  end;
end;

procedure UseEditorApplicationData;
var
  DataPath: string;
begin
  { start by resetting ApplicationDataOverride to empty, to reset
    previous customizations of ApplicationDataOverride done by some editor code. }
  ApplicationDataOverride := '';

  if CastleEnginePath <> '' then
  begin
    DataPath := CastleEnginePath +
      'tools' + PathDelim + 'castle-editor' + PathDelim + 'data' + PathDelim;
    if DirectoryExists(DataPath) then
      ApplicationDataOverride := FilenameToUriSafe(DataPath);
  end;
end;

constructor TEditorApplicationData.Create;
begin
  inherited;
  OldApplicationDataOverride := ApplicationDataOverride;
  UseEditorApplicationData;
end;

destructor TEditorApplicationData.Destroy;
begin
  ApplicationDataOverride := OldApplicationDataOverride;
  inherited;
end;

end.
