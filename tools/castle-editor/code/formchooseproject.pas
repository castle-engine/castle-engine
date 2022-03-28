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
    ButtonOpen: TBitBtn;
    ButtonOpenRecent: TBitBtn;
    ButtonOpenExample: TBitBtn;
    ButtonNew: TBitBtn;
    ButtonPreferences: TBitBtn;
    ButtonSupportUs: TBitBtn;
    GroupBoxOpen: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    OpenProject: TCastleOpenDialog;
    ImageLogo: TImage;
    LabelTitle: TLabel;
    PanelBottom: TPanel;
    PanelWarningMissingCompiler: TPanel;
    PopupMenuRecentProjects: TPopupMenu;
    procedure ButtonOpenExampleClick(Sender: TObject);
    procedure ButtonPreferencesClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonOpenRecentClick(Sender: TObject);
    procedure ButtonSupportUsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    RecentProjects: TCastleRecentFiles;
    CommandLineHandled: Boolean;
    procedure MenuItemRecentClick(Sender: TObject);
    procedure OpenProjectFromCommandLine;
    procedure UpdateWarningMissingCompiler;
    { Open ProjectForm.
      ManifestUrl may be absolute or relative here. }
    procedure ProjectOpen(ManifestUrl: string);
  public

  end;

var
  ChooseProjectForm: TChooseProjectForm;

implementation

{$R *.lfm}

uses CastleConfig, CastleLCLUtils, CastleURIUtils, CastleUtils, CastleOpenDocument,
  CastleFilesUtils, CastleParameters, CastleLog, CastleStringUtils,
  ProjectUtils, EditorUtils, FormNewProject, FormPreferences,
  ToolCompilerInfo, ToolFpcVersion, ToolManifest, ToolCommonUtils,
  FormProject, FormNewUnit;

{ TChooseProjectForm ------------------------------------------------------------- }

procedure TChooseProjectForm.ProjectOpen(ManifestUrl: string);
begin
  ManifestUrl := AbsoluteURI(ManifestUrl);

  // Validate
  if not URIFileExists(ManifestUrl) then
    raise Exception.CreateFmt('Cannot find CastleEngineManifest.xml at this location: "%s". Invalid project opened.',
      [ManifestUrl]);

  ProjectForm := TProjectForm.Create(Application);
  ProjectForm.OpenProject(ManifestUrl);
  ProjectForm.Show;

  { Do this even if you just opened this project through "recent" menu.
    This way URL is moved to the top. }
  RecentProjects.Add(ManifestUrl);
end;

procedure TChooseProjectForm.ButtonOpenClick(Sender: TObject);
begin
  { This is critical in a corner case:
    - You run CGE editor such that it detects as "data directory"
      current directory. E.g. you compiled it manually and run on Unix as
      "tools/castle-editor/castle-editor"
    - Now you open project in subdirectory. (E.g. some CGE example,
      to continue previous example.)
    - With UseCastleDataProtocol, OpenProject.URL will now be like
      'castle-data:/examples/xxx/CastleEngineManifest.xml'.
      Which means that it's absolute (AbsoluteURI in ProjectOpen will not change it),
      but it's also bad to be used (because later we will set ApplicationDataOverride
      to something derived from it, thus ResolveCastleDataURL will resolve
      castle-data:/ to another castle-data:/ , and it will make no sense
      since one castle-data:/ assumes ApplicationDataOverride = '' ...).
  }
  OpenProject.UseCastleDataProtocol := false;

  if OpenProject.Execute then
  begin
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
    UseEditorApplicationData; // we use our castle-data:/xxx to copy template

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
      if NewProjectForm.ButtonTemplate3dModelViewer.Down then
        TemplateName := '3d_model_viewer'
      else
      if NewProjectForm.ButtonTemplate3dFps.Down then
        TemplateName := '3d_fps_game'
      else
      if NewProjectForm.ButtonTemplate2d.Down then
        TemplateName := '2d_game'
      else
        raise EInternalError.Create('Unknown project template selected');

      // Fill project dir
      CopyTemplate(ProjectDirUrl, TemplateName,
        NewProjectForm.EditProjectName.Text,
        NewProjectForm.EditProjectCaption.Text,
        NewProjectForm.EditStateName.Text);
      GenerateProgramWithBuildTool(ProjectDirUrl);

      // Open new project
      ManifestUrl := CombineURI(ProjectDirUrl, 'CastleEngineManifest.xml');
      ProjectOpen(ManifestUrl);
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
  UpdateWarningMissingCompiler;
end;

procedure TChooseProjectForm.ButtonOpenExampleClick(Sender: TObject);
begin
  if CastleEnginePath <> '' then
  begin
    OpenProject.FileName := CastleEnginePath + 'examples' + PathDelim;
  end else
    WritelnWarning('Cannot find CGE directory');

  ButtonOpenClick(nil);
end;

procedure TChooseProjectForm.ButtonOpenRecentClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  I: Integer;
  Url, S, NotExistingSuffix: String;
begin
  PopupMenuRecentProjects.Items.Clear;
  for I := 0 to RecentProjects.URLs.Count - 1 do
  begin
    Url := RecentProjects.URLs[I];
    MenuItem := TMenuItem.Create(Self);

    if URIExists(Url) in [ueFile, ueUnknown] then
      NotExistingSuffix := ''
    else
      NotExistingSuffix := ' (PROJECT FILES MISSING)';

    // show file URLs simpler, esp to avoid showing space as %20
    Url := SuffixRemove('/CastleEngineManifest.xml', Url, true);
    if URIProtocol(Url) = 'file' then
      S := URIToFilenameSafeUTF8(Url)
    else
      S := URIDisplay(Url);
    MenuItem.Caption := SQuoteLCLCaption(S + NotExistingSuffix);

    MenuItem.Tag := I;
    MenuItem.OnClick := @MenuItemRecentClick;
    PopupMenuRecentProjects.Items.Add(MenuItem);
  end;
  PopupMenuRecentProjects.PopupComponent := ButtonOpenRecent;
  PopupMenuRecentProjects.Popup;
end;

procedure TChooseProjectForm.ButtonSupportUsClick(Sender: TObject);
begin
  OpenURL('https://patreon.com/castleengine/');
end;

procedure TChooseProjectForm.FormCreate(Sender: TObject);

  procedure ConfigLoad;
  begin
    FpcCustomPath := UserConfig.GetValue('fpc_custom_path', '');
    LazarusCustomPath := UserConfig.GetValue('lazarus_custom_path', '');
    CodeEditor := TCodeEditor(UserConfig.GetValue('code_editor/setting', Ord(DefaultCodeEditor)));
    CodeEditorCommand := UserConfig.GetValue('code_editor/command', '');
    CodeEditorCommandProject := UserConfig.GetValue('code_editor/command_project', '');
    MuteOnRun := UserConfig.GetValue('sound/mute_on_run', DefaultMuteOnRun);
    EditorVolume := UserConfig.GetFloat('sound/editor_volume', DefaultEditorVolume);
    Compiler := StringToCompiler(UserConfig.GetValue('compiler', CompilerToString(DefaultCompiler)));
    SoundEngineSetVolume;
  end;

begin
  UserConfig.Load;
  RecentProjects := TCastleRecentFiles.Create(Self);
  RecentProjects.LoadFromConfig(UserConfig);
  //  RecentProjects.NextMenuItem := ; // unused for now
  ConfigLoad;

  UseEditorApplicationData;
  InternalCastleDesignData := ApplicationData('');
end;

procedure TChooseProjectForm.FormDestroy(Sender: TObject);

  procedure ConfigSave;
  begin
    UserConfig.SetDeleteValue('fpc_custom_path', FpcCustomPath, '');
    UserConfig.SetDeleteValue('lazarus_custom_path', LazarusCustomPath, '');
    UserConfig.SetDeleteValue('code_editor/setting', Ord(CodeEditor), Ord(DefaultCodeEditor));
    UserConfig.SetDeleteValue('code_editor/command', CodeEditorCommand, '');
    UserConfig.SetDeleteValue('code_editor/command_project', CodeEditorCommandProject, '');
    UserConfig.SetDeleteValue('sound/mute_on_run', MuteOnRun, DefaultMuteOnRun);
    UserConfig.SetDeleteFloat('sound/editor_volume', EditorVolume, DefaultEditorVolume);
    UserConfig.SetDeleteValue('compiler', CompilerToString(Compiler), CompilerToString(DefaultCompiler));
  end;

begin
  ConfigSave;
  RecentProjects.SaveToConfig(UserConfig);
  UserConfig.Save;
end;

procedure TChooseProjectForm.FormShow(Sender: TObject);
begin
  ButtonOpenRecent.Enabled := RecentProjects.URLs.Count <> 0;
  OpenProjectFromCommandLine;
  UpdateWarningMissingCompiler;
end;

procedure TChooseProjectForm.UpdateWarningMissingCompiler;

  function CompilerFound: Boolean;
  begin
    Result := false;
    try
      FindExeFpcCompiler;
      FpcVersion;
      Result := true;
    except
      { FindExeFpcCompiler exits with EExecutableNotFound,
        but FpcVersion may fail with any Exception unfortunately
        (it runs external process, and many things can go wrong). }
      on E: Exception do
      begin
        WritelnLog('FPC not found, or cannot run FPC to get version: ' + ExceptMessage(E));

        { if FPC failed, try to find Delphi }
        if FindDelphiPath(false) <> '' then
          Result := true;
      end;
    end;
  end;

begin
  PanelWarningMissingCompiler.Visible := not CompilerFound;
end;

procedure TChooseProjectForm.MenuItemRecentClick(Sender: TObject);
var
  Url: String;
begin
  Url := RecentProjects.URLs[(Sender as TMenuItem).Tag];

  if not (URIExists(Url) in [ueFile, ueUnknown]) then
  begin
    if YesNoBox(Format('Project file "%s" does not exist. Remove the project from the recent list?', [
      URIDisplay(Url)
    ])) then
    begin
      RecentProjects.Remove(Url);
      PopupMenuRecentProjects.Items.Remove(Sender as TMenuItem);
    end;
    Exit;
  end;

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
    except
      Show;
      raise;
    end;
  end;
end;

end.
