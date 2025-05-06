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

{ Choose new or existing project. }
unit GameViewChooseProject;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Choose new or existing project. }
  TViewChooseProject = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonNewProject: TCastleButton;
    ButtonOpenProject: TCastleButton;
    ButtonOpenRecent: TCastleButton;
    ButtonOpenExample: TCastleButton;
    ButtonPreferences: TCastleButton;
    ButtonSupport: TCastleButton;
    ButtonQuit: TCastleButton;
  private
    procedure ClickNewProject(Sender: TObject);
    procedure ClickOpenProject(Sender: TObject);
    procedure ClickOpenRecent(Sender: TObject);
    procedure ClickOpenExample(Sender: TObject);
    procedure ClickPreferences(Sender: TObject);
    procedure ClickSupport(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewChooseProject: TViewChooseProject;

implementation

uses CastleFilesUtils, CastleOpenDocument, CastleApplicationProperties,
  CastleWindow,
  GameViewNewProject, GameViewProject;

{ TViewChooseProject ----------------------------------------------------------- }

constructor TViewChooseProject.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewchooseproject.castle-user-interface';
end;

procedure TViewChooseProject.Start;
begin
  inherited;
  ButtonNewProject.OnClick := {$ifdef FPC}@{$endif} ClickNewProject;
  ButtonOpenProject.OnClick := {$ifdef FPC}@{$endif} ClickOpenProject;
  ButtonOpenExample.OnClick := {$ifdef FPC}@{$endif} ClickOpenExample;
  ButtonOpenRecent.OnClick := {$ifdef FPC}@{$endif} ClickOpenExample;
  ButtonPreferences.OnClick := {$ifdef FPC}@{$endif} ClickPreferences;
  ButtonSupport.OnClick := {$ifdef FPC}@{$endif} ClickSupport;
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif} ClickQuit;

  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
end;

procedure TViewChooseProject.ClickNewProject(Sender: TObject);
begin
  Container.View := ViewNewProject;
end;

procedure TViewChooseProject.ClickOpenProject(Sender: TObject);
begin
  // TODO
end;

procedure TViewChooseProject.ClickOpenRecent(Sender: TObject);
begin
  // TODO
end;

procedure TViewChooseProject.ClickOpenExample(Sender: TObject);
begin
  // TODO
end;

procedure TViewChooseProject.ClickPreferences(Sender: TObject);
begin
  // TODO
end;

procedure TViewChooseProject.ClickSupport(Sender: TObject);
begin
  OpenUrl('https://www.patreon.com/castleengine');
end;

procedure TViewChooseProject.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
