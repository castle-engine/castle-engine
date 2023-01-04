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

{ Fill directory for new project with the template. }
procedure CopyTemplate(const ProjectDirUrl: String;
  const TemplateName, ProjectName, ProjectCaption, MainView: String);

{ Fill directory for new project with the build-tool generated stuff. }
procedure GenerateProgramWithBuildTool(const ProjectDirUrl: String);

type
  TBuildMode = (bmDebug, bmRelease);

{ For some operations (like creating a project from template), the editor uses
  ApplicationData files (castle-data:/xxx URLs).
  So make sure that ApplicationData is correct, by setting ApplicationDataOverride.
  We can use CastleEnginePath (that uses $CASTLE_ENGINE_PATH environment variable)
  for this. }
procedure UseEditorApplicationData;

implementation

uses Forms,
  CastleURIUtils, CastleStringUtils, CastleFindFiles, CastleUtils,
  CastleFilesUtils,
  EditorUtils, ToolCommonUtils, FormProject;

{ TTemplateCopyProcess ------------------------------------------------------------ }

type
  TTemplateCopyProcess = class
    TemplateUrl: String;
    ProjectDirUrl: String;
    MainView: String;
    Macros: TStringStringMap;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TTemplateCopyProcess.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
var
  Contents, RelativeUrl, TargetUrl, TargetFileName, Mime: String;
begin
  { Ignore case at IsPrefix / PrefixRemove calls,
    in case it's not case-sensitive file-system, then the case in theory
    can differ. }
  if not IsPrefix(TemplateUrl, FileInfo.URL, true) then
    raise Exception.CreateFmt('Unexpected: %s is not a prefix of %s, report a bug',
      [TemplateUrl, FileInfo.URL]);
  RelativeUrl := PrefixRemove(TemplateUrl, FileInfo.URL, true);
  TargetUrl := CombineURI(ProjectDirUrl, RelativeUrl);
  { Rename target files that depend on MainView. }
  if ExtractURIName(TargetUrl) = 'gameviewmain.pas' then
    TargetUrl := ExtractURIPath(TargetUrl) + 'gameview' + LowerCase(MainView) + '.pas';
  if ExtractURIName(TargetUrl) = 'gameviewmain.castle-user-interface' then
    TargetUrl := ExtractURIPath(TargetUrl) + 'gameview' + LowerCase(MainView) + '.castle-user-interface';
  TargetFileName := URIToFilenameSafe(TargetUrl);

  if FileInfo.Directory then
  begin
    // create directory
    if not ForceDirectories(TargetFileName) then
      raise Exception.CreateFmt('Cannot create directory "%s"', [TargetFileName]);
  end else
  begin
    Mime := URIMimeType(FileInfo.URL);
    if (Mime = 'application/xml') or
       (Mime = 'text/plain') then
    begin
      // copy text file, replacing macros
      Contents := FileToString(FileInfo.URL);
      Contents := SReplacePatterns(Contents, Macros, false);
      StringToFile(TargetFileName, Contents);
    end else
    begin
      // simply copy other file types (e.g. sample png images in project templates)
      CheckCopyFile(URIToFilenameSafe(FileInfo.URL), TargetFileName);
    end;
  end;
end;

{ global routines ------------------------------------------------------------ }

procedure AddMacroXmlQuote(const Macros: TStringStringMap; const MacroName: String);

  function XmlQuote(const S: String): String;
  begin
    Result := SReplacePatterns(S,
      ['&', '<', '>', '"'],
      ['&amp;', '&lt;', '&gt;', '&quot;'],
      false { IgnoreCase; can be false, it doesn't matter, as our patterns are not letters }
    );
  end;

begin
  Macros.Add('${XmlQuote(' + MacroName + ')}', XmlQuote(Macros['${' + MacroName + '}']));
end;

procedure CopyTemplate(const ProjectDirUrl: String;
  const TemplateName, ProjectName, ProjectCaption, MainView: String);
var
  TemplateUrl, ProjectQualifiedName, ProjectPascalName: String;
  CopyProcess: TTemplateCopyProcess;
  Macros: TStringStringMap;
begin
  Assert(ProjectName <> '');

  TemplateUrl := 'castle-data:/project_templates/' + TemplateName + '/files/';
  { Logic in TTemplateCopyProcess.FoundFile assumes that
    TemplateUrl does not any longer start with castle-data:/ }
  TemplateUrl := ResolveCastleDataURL(TemplateUrl);

  if URIExists(TemplateUrl) <> ueDirectory then
    raise Exception.CreateFmt('Cannot find template directory %s, make sure that $CASTLE_ENGINE_PATH is configured correctly',
      [TemplateUrl]);

  ProjectQualifiedName := MakeQualifiedName(ProjectName);
  ProjectPascalName := MakeProjectPascalName(ProjectName);

  Macros := TStringStringMap.Create;
  try
    Macros.Add('${PROJECT_NAME}', ProjectName);
    Macros.Add('${PROJECT_QUALIFIED_NAME}', ProjectQualifiedName);
    Macros.Add('${PROJECT_PASCAL_NAME}', ProjectPascalName);
    Macros.Add('${PROJECT_CAPTION}', ProjectCaption);
    Macros.Add('${MAIN_VIEW}', MainView);
    Macros.Add('${MAIN_VIEW_LOWERCASE}', LowerCase(MainView));

    { Generate versions of some macros with xml_quote function. }
    AddMacroXmlQuote(Macros, 'PROJECT_NAME');
    AddMacroXmlQuote(Macros, 'PROJECT_QUALIFIED_NAME');
    AddMacroXmlQuote(Macros, 'PROJECT_PASCAL_NAME');
    AddMacroXmlQuote(Macros, 'PROJECT_CAPTION');

    CopyProcess := TTemplateCopyProcess.Create;
    try
      CopyProcess.TemplateUrl := TemplateUrl;
      CopyProcess.ProjectDirUrl := ProjectDirUrl;
      CopyProcess.Macros := Macros;
      CopyProcess.MainView := MainView;
      FindFiles(TemplateUrl, '*', true, @CopyProcess.FoundFile, [ffRecursive]);
    finally FreeAndNil(CopyProcess) end;
  finally FreeAndNil(Macros) end;
end;

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

  MyRunCommandIndir(URIToFilenameSafe(ProjectDirUrl), BuildToolExe,
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
      ApplicationDataOverride := FilenameToURISafe(DataPath);
  end;
end;

end.
