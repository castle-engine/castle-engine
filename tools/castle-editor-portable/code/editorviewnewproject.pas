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

{ Create new project. }
unit EditorViewNewProject;

interface

uses Classes, SysUtils,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Create new project. }
  TViewNewProject = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonTemplateEmpty: TCastleButton;
    ButtonTemplate3DModelViewer: TCastleButton;
    ButtonTemplate3DFpsGame: TCastleButton;
    ButtonTemplate2DGame: TCastleButton;
    EditLocation: TCastleEdit;
    EditProjectName: TCastleEdit;
    EditProjectCaption: TCastleEdit;
    EditViewName: TCastleEdit;
    ButtonCancel: TCastleButton;
    ButtonCreateProject: TCastleButton;
  private
    procedure ClickTemplateAny(Sender: TObject);
    procedure ClickCancel(Sender: TObject);
    procedure ClickCreateProject(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewNewProject: TViewNewProject;

implementation

uses CastleFilesUtils, CastleInternalTools, CastleUtils, CastleUriUtils,
  CastleMessages, CastleWindow,
  ToolCommonUtils,
  EditorViewProject, EditorViewChooseProject;

{ TViewNewProject ---------------------------------------------------------------- }

constructor TViewNewProject.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/editorviewnewproject.castle-user-interface';
end;

procedure TViewNewProject.Start;
begin
  inherited;
  ButtonTemplateEmpty.OnClick := {$ifdef FPC}@{$endif} ClickTemplateAny;
  ButtonTemplate3DModelViewer.OnClick := {$ifdef FPC}@{$endif} ClickTemplateAny;
  ButtonTemplate3DFpsGame.OnClick := {$ifdef FPC}@{$endif} ClickTemplateAny;
  ButtonTemplate2DGame.OnClick := {$ifdef FPC}@{$endif} ClickTemplateAny;
  ButtonCancel.OnClick := {$ifdef FPC}@{$endif} ClickCancel;
  ButtonCreateProject.OnClick := {$ifdef FPC}@{$endif} ClickCreateProject;

  EditLocation.Text := 'castle-config:/my-projects/';
end;

procedure TViewNewProject.ClickTemplateAny(Sender: TObject);
var
  SenderButton: TCastleButton;
begin
  SenderButton := Sender as TCastleButton;
  // Only 1 template button should be pressed at a time
  ButtonTemplateEmpty.Pressed := SenderButton = ButtonTemplateEmpty;
  ButtonTemplate3DModelViewer.Pressed := SenderButton = ButtonTemplate3DModelViewer;
  ButtonTemplate3DFpsGame.Pressed := SenderButton = ButtonTemplate3DFpsGame;
  ButtonTemplate2DGame.Pressed := SenderButton = ButtonTemplate2DGame;
end;

procedure TViewNewProject.ClickCancel(Sender: TObject);
begin
  Container.View := ViewChooseProject;
end;

procedure TViewNewProject.ClickCreateProject(Sender: TObject);
var
  TemplateName, ProjectDirUrl: String;
  Options: TProjectCreationOptions;
begin
  try
    // Calculate TemplateName
    if ButtonTemplateEmpty.Pressed then
      TemplateName := 'empty'
    else
    if ButtonTemplate3DModelViewer.Pressed then
      TemplateName := '3d_model_viewer'
    else
    if ButtonTemplate3DFpsGame.Pressed then
      TemplateName := '3d_fps_game'
    else
    if ButtonTemplate2DGame.Pressed then
      TemplateName := '2d_game'
    else
      raise EInternalError.Create('Unknown project template selected');

    // Fill Options
    { TODO: Options.ParentDir and ProjectCreateFromTemplate
      work only if ApplicationConfig is on filesystem, they don't operate on URLs. }
    Options.ParentDir := UriToFilenameSafe(EditLocation.Text);
    Options.TemplateName := TemplateName;
    Options.ProjectName := EditProjectName.Text;
    Options.ProjectCaption := EditProjectCaption.Text;
    Options.MainView := EditViewName.Text;
    ProjectCreateFromTemplate(CastleEnginePath, Options, ProjectDirUrl);

    // TODO:
    //GenerateProgramWithBuildTool(ProjectDirUrl);

    // Open new project using ViewProject
    ViewProject.ProjectPathUrl := UriIncludeSlash(ProjectDirUrl);
    ViewProject.ProjectManifestUrl := CombineUri(ViewProject.ProjectPathUrl,
      'CastleEngineManifest.xml');
    Container.View := ViewProject;

  except
    on E: Exception do
    begin
      { Exceptions during project creation are always possible, as it creates
        a number of files, so many things can go wrong (disk full,
        dir already exists etc.).
        Show them nicely. }
      MessageOK(Application.MainWindow,
        'Error when creating project:' + NL + ExceptMessage(E));
    end;
  end;
end;

end.
