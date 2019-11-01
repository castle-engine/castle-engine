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

{ Form to choose project (@link(TChooseProjectForm)). }
unit FormChooseProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus,
  CastleDialogs, CastleLCLRecentFiles;

type
  { Choose project (new or existing). }
  TChooseProjectForm = class(TForm)
    ButtonPreferences: TBitBtn;
    ButtonOpenRecent: TBitBtn;
    ButtonNew: TBitBtn;
    ButtonOpen: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    OpenProject: TCastleOpenDialog;
    ImageLogo: TImage;
    LabelTitle: TLabel;
    PanelWarningFpcLazarus: TPanel;
    PopupMenuRecentProjects: TPopupMenu;
    procedure ButtonPreferencesClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonOpenRecentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    RecentProjects: TCastleRecentFiles;
    CommandLineHandled: Boolean;
    procedure MenuItemRecentClick(Sender: TObject);
    procedure OpenProjectFromCommandLine;
    procedure UpdateWarningFpcLazarus;
  public

  end;

var
  ChooseProjectForm: TChooseProjectForm;

implementation

{$R *.lfm}

uses CastleConfig, CastleLCLUtils, CastleURIUtils, CastleUtils,
  CastleFilesUtils, CastleParameters, CastleLog,
  ProjectUtils, EditorUtils, FormNewProject, FormPreferences,
  ToolCompilerInfo, ToolFpcVersion;

{ TChooseProjectForm ------------------------------------------------------------- }

procedure TChooseProjectForm.ButtonOpenClick(Sender: TObject);
begin
  if OpenProject.Execute then
  begin
    RecentProjects.Add(OpenProject.URL, false);

    Hide;
    try
      ProjectOpen(OpenProject.URL);
    except
      Show;
      raise;
    end;
  end;
end;

procedure TChooseProjectForm.ButtonNewClick(Sender: TObject);
var
  ProjectDir, ProjectDirUrl, ManifestUrl, TemplateName: String;
begin
  Hide;

  if NewProjectForm.ShowModal = mrOK then
  begin
    DetectEditorApplicationData; // we use our castle-data:/xxx to copy template

    try
      // Create project dir
      ProjectDir := InclPathDelim(NewProjectForm.EditLocation.Text) +
        NewProjectForm.EditProjectName.Text;
      ProjectDirUrl := FilenameToURISafe(InclPathDelim(ProjectDir));
      if not ForceDirectories(ProjectDir) then
        raise Exception.CreateFmt('Cannot create directory "%s".', [ProjectDir]);

      // Calculate TemplateName
      if NewProjectForm.ButtonTemplateEmpty.Down then
        TemplateName := 'empty'
      else
      if NewProjectForm.ButtonTemplate3D.Down then
        TemplateName := '3d'
      else
      if NewProjectForm.ButtonTemplate2D.Down then
        TemplateName := '2d'
      else
        raise EInternalError.Create('Unknown project template selected');

      // Fill project dir
      CopyTemplate(ProjectDirUrl, TemplateName, NewProjectForm.EditProjectName.Text);
      GenerateProgramWithBuildTool(ProjectDirUrl);

      // Open new project
      ManifestUrl := CombineURI(ProjectDirUrl, 'CastleEngineManifest.xml');
      ProjectOpen(ManifestUrl);
      RecentProjects.Add(ManifestUrl, false);
    except
      on E: Exception do
      begin
        Show;
        ErrorBox(ExceptMessage(E));
      end;
    end;
  end else
    Show;
end;

procedure TChooseProjectForm.ButtonPreferencesClick(Sender: TObject);
begin
  PreferencesForm.ShowModal;
  UpdateWarningFpcLazarus;
end;

procedure TChooseProjectForm.ButtonOpenRecentClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  I: Integer;
  Url: String;
begin
  PopupMenuRecentProjects.Items.Clear;
  for I := 0 to RecentProjects.URLs.Count - 1 do
  begin
    Url := RecentProjects.URLs[I];
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := SQuoteLCLCaption(Url);
    MenuItem.Tag := I;
    MenuItem.OnClick := @MenuItemRecentClick;
    PopupMenuRecentProjects.Items.Add(MenuItem);
  end;
  PopupMenuRecentProjects.PopupComponent := ButtonOpenRecent;
  PopupMenuRecentProjects.Popup;
end;

procedure TChooseProjectForm.FormCreate(Sender: TObject);

  procedure PathsConfigLoad;
  begin
    FpcCustomPath := UserConfig.GetValue('fpc_custom_path', '');
    LazarusCustomPath := UserConfig.GetValue('lazarus_custom_path', '');
  end;

begin
  UserConfig.Load;
  RecentProjects := TCastleRecentFiles.Create(Self);
  RecentProjects.LoadFromConfig(UserConfig);
  //  RecentProjects.NextMenuItem := ; // unused for now
  PathsConfigLoad;
end;

procedure TChooseProjectForm.FormDestroy(Sender: TObject);

  procedure PathsConfigSave;
  begin
    UserConfig.SetDeleteValue('fpc_custom_path', FpcCustomPath, '');
    UserConfig.SetDeleteValue('lazarus_custom_path', LazarusCustomPath, '');
  end;

begin
  PathsConfigSave;
  RecentProjects.SaveToConfig(UserConfig);
  UserConfig.Save;
end;

procedure TChooseProjectForm.FormShow(Sender: TObject);
begin
  ButtonOpenRecent.Enabled := RecentProjects.URLs.Count <> 0;
  OpenProjectFromCommandLine;
  UpdateWarningFpcLazarus;
end;

procedure TChooseProjectForm.UpdateWarningFpcLazarus;

  function FpcOrLazarusMissing: Boolean;
  begin
    Result := true;
    try
      FindExeFpcCompiler;
      FpcVersion;
      FindExeLazarusIDE;
      Result := false;
    except
      { FindExeFpcCompiler or FindExeLazarusIDE exit with EExecutableNotFound,
        but FpcVersion may fail with any Exception unfortunately
        (it runs external process, and many things can go wrong). }
      on E: Exception do
        WritelnLog('FPC or Lazarus not detected, or cannot run FPC to get version: ' + ExceptMessage(E));
    end;
  end;

begin
  PanelWarningFpcLazarus.Visible := FpcOrLazarusMissing;
end;

procedure TChooseProjectForm.MenuItemRecentClick(Sender: TObject);
var
  Url: String;
begin
  Url := RecentProjects.URLs[(Sender as TMenuItem).Tag];

  Hide;
  try
    ProjectOpen(Url);
  except
    Show;
    raise;
  end;
end;

procedure TChooseProjectForm.OpenProjectFromCommandLine;
begin
  if CommandLineHandled then Exit;
  CommandLineHandled := true;

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
  begin
    Hide;
    try
      ProjectOpen(Parameters[1]);
      RecentProjects.Add(Parameters[1], false);
    except
      Show;
      raise;
    end;
  end;
end;

end.
