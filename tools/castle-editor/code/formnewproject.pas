{
  Copyright 2018-2024 Michalis Kamburelis.

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
  Buttons, ButtonPanel, StdCtrls, EditBtn, LCLType;

type
  { Determine new project settings. }
  TNewProjectForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ButtonTemplate2d: TSpeedButton;
    EditLocation: TDirectoryEdit;
    EditViewName: TEdit;
    EditProjectName: TEdit;
    EditProjectCaption: TEdit;
    GroupProjectTemplate: TGroupBox;
    LabelViewName: TLabel;
    LabelProjectLocation: TLabel;
    LabelProjectCaption: TLabel;
    LabelTitle: TLabel;
    LabelProjectName: TLabel;
    ButtonTemplateEmpty: TSpeedButton;
    ButtonTemplate3dModelViewer: TSpeedButton;
    ButtonTemplate3dFps: TSpeedButton;
    procedure EditProjectNameUTF8KeyPress(Sender: TObject;
      var UTF8Key: TUTF8Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure ButtonTemplateClick(Sender: TObject);
  private
    procedure AdjustViewNameUi;
  public

  end;

var
  NewProjectForm: TNewProjectForm;

implementation

{$R *.lfm}

uses {$ifdef MSWINDOWS} WinDirs, {$endif}
  LazFileUtils,
  CastleUriUtils, CastleConfig, CastleUtils, CastleStringUtils,
  EditorUtils;

const
  AlphaNum = ['a'..'z', 'A'..'Z', '0'..'9'];
  ValidProjectNameChars = AlphaNum + ['_', '-'];
  InvalidProjectNameChars = AllChars - ValidProjectNameChars;

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

  { We used to base form height on whether EditViewName is available,
    but this
    - causes ugly jump in window on GTK2 when switching templates
    - for user it is not obvious why EditViewName appears/disappears.
    Now we keep form height constant during operation,
    and when EditViewName is not available - we explain it. }
  procedure AdjustFormSize;
  const
    ButtonsMargin = 8;
  begin
    ClientHeight := EditViewName.Top + EditViewName.Height + ButtonsMargin + ButtonPanel1.Height;
  end;

var
  DefaultNewProjectDir, NewProjectDir: String;
begin
  { Initialize everything to default values }

  ButtonTemplateEmpty.Down := true;

  DefaultNewProjectDir := InclPathDelim(DefaultProjectsParentDir) +
    'Castle Game Engine Projects';
  NewProjectDir := UserConfig.GetValue('new_project/default_dir', DefaultNewProjectDir);
  EditLocation.Directory := NewProjectDir;

  EditProjectName.Text := 'my-new-project';
  EditProjectCaption.Text := 'My New Project';
  EditViewName.Text := 'Main';

  AdjustViewNameUi;
  AdjustFormSize;
end;

procedure TNewProjectForm.AdjustViewNameUi;
var
  ViewNameConfigurable: Boolean;
begin
  ViewNameConfigurable := ButtonTemplateEmpty.Down or ButtonTemplate3dModelViewer.Down;
  LabelViewName.Enabled := ViewNameConfigurable;
  EditViewName.Enabled := ViewNameConfigurable;
  if ViewNameConfigurable then
    LabelViewName.Caption := 'Main View Name (determines name of the initial design and Pascal unit)'
  else
    LabelViewName.Caption := 'Main View Name (not customizable, as this template has multiple views)';
end;

procedure TNewProjectForm.ButtonTemplateClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := true;
  AdjustViewNameUi;
end;

procedure TNewProjectForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  ProjectDir: String;
  InvalidIndex: Integer;
  ProjectName, ProjectLocation: TCaption;
begin
  if ModalResult = mrOK then
  begin
    ProjectName := EditProjectName.Text;
    ProjectLocation := EditLocation.Directory;

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

    if EditViewName.Visible and not IsValidIdent(EditViewName.Text) then
    begin
      ErrorBox(Format('View name "%s" is not a valid Pascal identifier',
        [EditViewName.Text]));
      CanClose := false;
      Exit;
    end;

    UserConfig.SetValue('new_project/default_dir', ProjectLocation);
  end;
end;

procedure TNewProjectForm.EditProjectNameUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
const
  { Although these chars are not allowed in project name,
    but they are allowed by this routine, as they allow to edit the text
    (e.g. delete by backspace, cut/copy/paste by Ctrl+X/C/V). }
  ControlChars = [CtrlA .. CtrlZ] +
    { actually this is already in CtrlA .. CtrlZ, but seemed more obvious to list it explicitly. }
    [CharBackspace];
begin
  if (Length(UTF8Key) <> 1) or
     ( (UTF8Key[1] in InvalidProjectNameChars) and
       (not (UTF8Key[1] in ControlChars))
     ) then
    UTF8Key := '';
end;

end.
