{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Choose one of the existing projects to open. }
unit EditorViewChooseExistingProject;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleRecentFiles, CastleComponentSerialize, CastleFindFiles;

type
  { What project to show as a choice.
    Used by @link(TViewChooseExistingProject.ProjectSource). }
  TProjectsSource = (
    { Load searching ApplicationConfig('my-projects'). }
    psConfigMyProjects,
    { Load one of recently opened projects. }
    psRecent,
    { Load from a directory URL given to
      @link(TViewChooseExistingProject.ProjectSourceDirectoryUrl). }
    psDirectoryUrl
  );

  { Choose one of the existing projects to open.
    Before starting, set @link(ProjectsSource) and @link(ProjectsSourceDirectoryUrl). }
  TViewChooseExistingProject = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonCancel: TCastleButton;
    ButtonProjectTemplate: TCastleButton;
    FactoryButtonProject: TCastleComponentFactory;
    ListOpenExistingProject: TCastleVerticalGroup;
    ScrollListOpenExistingProject: TCastleUserInterface;
    LabelListCaption: TCastleLabel;
  private
    ProjectUrls: TStringList;
    RecentProjects: TRecentFiles;
    procedure ClickCancel(Sender: TObject);
    procedure ClickOpenProject(Sender: TObject);
    procedure FoundInMyProjects(const FileInfo: TFileInfo; var StopSearch: Boolean);
    procedure FoundInDirectory(const FileInfo: TFileInfo; var StopSearch: Boolean);
  public
    { Set before @link(Start). Determines what projects to show as a choice. }
    ProjectsSource: TProjectsSource;

    { In case @link(ProjectsSource) is @link(psDirectoryUrl),
      this is the URL of the directory to load projects from. }
    ProjectsSourceDirectoryUrl: String;

    { If you open a project, call this method to add it to the list
      of recent projects. This method is useful even when the view
      is not active (so it's stopped).

      Used in practice only by @link(TViewProject.Start),
      so it happens both when you open a projec using this view,
      and when you open a new project. }
    procedure AddRecentProject(const ManifestUrl: String);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewChooseExistingProject: TViewChooseExistingProject;

implementation

uses SysUtils,
  CastleConfig, CastleUtils, CastleLog, CastleUriUtils, CastleFilesUtils,
  CastleStringUtils,
  ToolManifest,
  EditorViewProject, EditorViewChooseProject;

const
  ManifestName = 'CastleEngineManifest.xml';

constructor TViewChooseExistingProject.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/editorviewchooseexistingproject.castle-user-interface';
  RecentProjects := TRecentFiles.Create(Self);
  RecentProjects.LoadFromConfig(UserConfig);
end;

destructor TViewChooseExistingProject.Destroy;
begin
  if RecentProjects <> nil then
  begin
    RecentProjects.SaveToConfig(UserConfig);
    FreeAndNil(RecentProjects);
  end;
  inherited;
end;

procedure TViewChooseExistingProject.AddRecentProject(const ManifestUrl: String);
begin
  RecentProjects.Add(ManifestUrl);
end;

type
  TButtonProjectDesign = class(TPersistent)
  published
    LabelProjectName, LabelProjectUrl: TCastleLabel;
  end;

procedure TViewChooseExistingProject.Start;

  procedure LoadConfigMyProjects;
  begin
    LabelListCaption.Caption := 'Open My Project:';
    FindFiles(ApplicationConfig('my-projects'), '*', true,
      {$ifdef FPC}@{$endif} FoundInMyProjects, []);
  end;

  procedure LoadDirectoryUrl;
  begin
    LabelListCaption.Caption := 'Open Project From ' +
      ExtractUriName(UriExcludeSlash(ProjectsSourceDirectoryUrl)) + ':';
    FindFiles(ProjectsSourceDirectoryUrl, ManifestName, false,
      {$ifdef FPC}@{$endif} FoundInDirectory, [ffRecursive]);
  end;

  procedure LoadRecent;
  var
    Url: String;
  begin
    LabelListCaption.Caption := 'Open Recent Project:';
    for Url in RecentProjects.Urls do
      ProjectUrls.Add(Url);
  end;

  { Fill ProjectUrls with the list of potential projects. }
  procedure FillProjectUrls;
  begin
    ProjectUrls := TStringList.Create;
    case ProjectsSource of
      psConfigMyProjects: LoadConfigMyProjects;
      psRecent: LoadRecent;
      psDirectoryUrl: LoadDirectoryUrl;
      else raise Exception.Create('Unknown ProjectsSource');
    end;
  end;

  { For each URL in ProjectUrls, validate it (make sure project exists
    and has valid CastleEngineManifest.xml) and add a button to the list. }
  procedure FilterProjectUrlsAndMakeButtons;
  var
    Url: String;
    Manifest: TCastleManifest;
    I: Integer;
    ButtonProject: TCastleButton;
    ButtonProjectDesign: TButtonProjectDesign;
  begin
    I := 0;
    while I < ProjectUrls.Count do
    begin
      Url := ProjectUrls[I];
      if not UriFileExists(Url) then
      begin
        ProjectUrls.Delete(I);
        Continue;
      end;

      try
        Manifest := TCastleManifest.CreateFromUrl(Url);
      except
        on E: Exception do
        begin
          WritelnLog('Error loading project manifest from ' + Url + ': ' + ExceptMessage(E));
          ProjectUrls.Delete(I);
          Continue;
        end;
      end;

      try
        ButtonProjectDesign := TButtonProjectDesign.Create;
        try
          ButtonProject := FactoryButtonProject.ComponentLoad(FreeAtStop, ButtonProjectDesign) as TCastleButton;
          ButtonProject.Tag := I;
          ButtonProject.OnClick := {$ifdef FPC}@{$endif} ClickOpenProject;
          ButtonProject.Width := ScrollListOpenExistingProject.EffectiveWidthForChildren;
          ListOpenExistingProject.InsertFront(ButtonProject);

          ButtonProjectDesign.LabelProjectName.Caption := Manifest.Caption;
          ButtonProjectDesign.LabelProjectUrl.Caption :=
            SuffixRemove('/CastleEngineManifest.xml', Url, true);
        finally FreeAndNil(ButtonProjectDesign) end;
      finally
        FreeAndNil(Manifest);
      end;

      Inc(I);
    end;
  end;

begin
  inherited;

  ButtonCancel.OnClick := {$ifdef FPC}@{$endif} ClickCancel;

  FactoryButtonProject.LoadFromComponent(ButtonProjectTemplate);
  // note that ButtonProjectTemplate children remain existing, doesn't matter
  FreeAndNil(ButtonProjectTemplate);

  FillProjectUrls;
  FilterProjectUrlsAndMakeButtons;
end;

procedure TViewChooseExistingProject.Stop;
begin
  FreeAndNil(ProjectUrls);
  inherited;
end;

procedure TViewChooseExistingProject.FoundInMyProjects(const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  { In this case, we receive URL of a directory containing the project. }
  WritelnLog('Found project in my projects: ' + FileInfo.Url);
  if FileInfo.Directory then
    ProjectUrls.Add(CombineUri(UriIncludeSlash(FileInfo.Url), ManifestName));
end;

procedure TViewChooseExistingProject.FoundInDirectory(const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  { In this case, we receive URL of CastleEngineManifest.xml file. }
  ProjectUrls.Add(FileInfo.Url);
end;

procedure TViewChooseExistingProject.Resize;
var
  Ui: TCastleUserInterface;
begin
  inherited;
  // fix size of all children of ListOpenExistingProject
  for Ui in ListOpenExistingProject do
    Ui.Width := ScrollListOpenExistingProject.EffectiveWidthForChildren;
end;

procedure TViewChooseExistingProject.ClickCancel(Sender: TObject);
begin
  Container.View := ViewChooseProject;
end;

procedure TViewChooseExistingProject.ClickOpenProject(Sender: TObject);
var
  ProjectUrl: String;
  SenderButton: TCastleButton;
begin
  SenderButton := Sender as TCastleButton;
  ProjectUrl := ProjectUrls[SenderButton.Tag];

  ViewProject.ProjectPathUrl := ExtractUriPath(ProjectUrl);
  ViewProject.ProjectManifestUrl := ProjectUrl;
  Container.View := ViewProject;
end;

function TViewChooseExistingProject.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Event.IsKey(keyEscape) then
  begin
    ClickCancel(nil);
    Exit(true);
  end;
end;

end.
