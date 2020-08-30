{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ New project form (@link(TNewProjectForm)). }
unit FormNewProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ButtonPanel, StdCtrls, EditBtn;

type
  { Determine new project settings. }
  TNewProjectForm = class(TForm)
    ButtonChooseLocation: TButton;
    ButtonPanel1: TButtonPanel;
    ButtonTemplate2d: TSpeedButton;
    EditLocation: TEdit;
    EditProjectName: TEdit;
    EditProjectCaption: TEdit;
    GroupProjectTemplate: TGroupBox;
    LabelProjectLocation: TLabel;
    LabelProjectCaption: TLabel;
    LabelTitle: TLabel;
    LabelProjectName: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ButtonTemplateEmpty: TSpeedButton;
    ButtonTemplate3dModelViewer: TSpeedButton;
    ButtonTemplate3dFps: TSpeedButton;
    procedure EditLocationButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure ButtonTemplateClick(Sender: TObject);
  private

  public

  end;

var
  NewProjectForm: TNewProjectForm;

implementation

{$R *.lfm}

uses {$ifdef MSWINDOWS} WinDirs, {$endif}
  LazFileUtils,
  CastleURIUtils, CastleConfig, CastleUtils, CastleStringUtils,
  EditorUtils;

procedure TNewProjectForm.FormShow(Sender: TObject);

  function DefaultProjectsParentDir: String;
  begin
    Result :=
      {$ifdef MSWINDOWS}
      // get "Documents" dir
      GetWindowsSpecialDir(CSIDL_PERSONAL);
      {$else}
      GetUserDir;
      {$endif}
  end;

var
  DefaultNewProjectDir, NewProjectDir: String;
begin
  { Initialize everything to default values }

  ButtonTemplateEmpty.Down := true;

  DefaultNewProjectDir := InclPathDelim(DefaultProjectsParentDir) +
    'Castle Game Engine Projects';
  NewProjectDir := UserConfig.GetValue('new_project/default_dir', DefaultNewProjectDir);
  // SelectDirectoryDialog1.InitialDir := NewProjectDir; // not neeeded
  SelectDirectoryDialog1.FileName := NewProjectDir;
  EditLocation.Text := NewProjectDir;

  EditProjectName.Text := 'my-new-project';
end;

procedure TNewProjectForm.ButtonTemplateClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
end;

procedure TNewProjectForm.EditLocationButtonClick(Sender: TObject);
begin
  // SelectDirectoryDialog1.InitialDir := EditLocation.Text; // not neeeded
  SelectDirectoryDialog1.FileName := EditLocation.Text;
  if SelectDirectoryDialog1.Execute then
    EditLocation.Text := SelectDirectoryDialog1.FileName;
end;

procedure TNewProjectForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
const
  AlphaNum = ['a'..'z', 'A'..'Z', '0'..'9'];
  ValidProjectNameChars = AlphaNum + ['_', '-'];
  InvalidProjectNameChars = AllChars - ValidProjectNameChars;
var
  ProjectDir: String;
  InvalidIndex: Integer;
  ProjectName, ProjectLocation: TCaption;
begin
  if ModalResult = mrOK then
  begin
    ProjectName := EditProjectName.Text;
    ProjectLocation := EditLocation.Text;

    if ProjectName = '' then
    begin
      ErrorBox('Project name cannot be empty.');
      CanClose := false;
      Exit;
    end;

    InvalidIndex := CharsPos(InvalidProjectNameChars, ProjectName);
    if InvalidIndex <> 0 then
    begin
      ErrorBox(Format('Project name contains invalid character "%s".' + NL +
        NL +
        'The internal project name is used with various tools, in various contexts, and thus it is limited to alphanumeric characters plus underscore ("_") and hyphen ("-").' + NL +
        NL +
        'Note that this is only an internal project name. The user-visible "Project Caption" has no such limitations.',
        [SReadableForm(ProjectName[InvalidIndex])]));
      CanClose := false;
      Exit;
    end;

    if ProjectLocation = '' then
    begin
      ErrorBox('No Project Location chosen.');
      CanClose := false;
      Exit;
    end;

    ProjectDir := InclPathDelim(ProjectLocation) + ProjectName;
    if DirectoryExists(ProjectDir) then
    begin
      ErrorBox(Format('Directory "%s" already exists, cannot create a project there. Please pick a project name that does not correspond to an already-existing directory.',
        [ProjectDir]));
      CanClose := false;
      Exit;
    end;

    UserConfig.SetValue('new_project/default_dir', ProjectLocation);
  end;
end;

end.
