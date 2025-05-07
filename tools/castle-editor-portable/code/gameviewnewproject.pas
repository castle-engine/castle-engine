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
unit GameViewNewProject;

interface

uses Classes,
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
    EditProjectLocation: TCastleEdit;
    EditProjectName: TCastleEdit;
    EditProjectCaption: TCastleEdit;
    EditMainView: TCastleEdit;
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

uses CastleFilesUtils,
  GameViewProject, GameViewChooseProject;

{ TViewNewProject ---------------------------------------------------------------- }

constructor TViewNewProject.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewnewproject.castle-user-interface';
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

  EditProjectLocation.Text := ApplicationConfig('my-projects/');
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
begin
  // TODO: actually create project
  Container.View := ViewProject;
end;

end.
