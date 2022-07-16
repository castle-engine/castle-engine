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
  CastleDialogs, CastleLCLRecentFiles, CastleSoundEngine;

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
    LabelWarning: TLabel;
    OpenProject: TCastleOpenDialog;
    ImageLogo: TImage;
    LabelTitle: TLabel;
    PanelBottom: TPanel;
    PanelWarning: TPanel;
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
    procedure FormHide(Sender: TObject);
  private
    RecentProjects: TCastleRecentFiles;
    CommandLineHandled: Boolean;
    CodingTheme: TCastleSound;
    CodingThemePlaying: TCastlePlayingSound;
    procedure MenuItemRecentClick(Sender: TObject);
    procedure OpenProjectFromCommandLine;
    procedure UpdateWarning;
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
  UpdateWarning;
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
    CastleEngineOverridePath := UserConfig.GetValue('castle_engine_override_path', '');
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
    UserConfig.SetDeleteValue('castle_engine_override_path', CastleEngineOverridePath, '');
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
  UpdateWarning;

  if (CastleEnginePath <> '') and URIFileExists(CastleEnginePath + 'game.pas:666') then
  begin
    CodingTheme := TCastleSound.Create(Self);
    CodingTheme.Stream := true;
    CodingTheme.Url := InternalCastleDesignData + 'not_an_easter_egg/ente_evil.ogg';
    CodingThemePlaying := TCastlePlayingSound.Create(Self);
    CodingThemePlaying.Loop := true;
    CodingThemePlaying.Sound := CodingTheme;
    SoundEngine.Play(CodingThemePlaying);
  end;
end;

procedure TChooseProjectForm.FormHide(Sender: TObject);
begin
  FreeAndNil(CodingThemePlaying);
  FreeAndNil(CodingTheme);
end;

procedure TChooseProjectForm.UpdateWarning;

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
  if (CastleEnginePath = '') or not CgePathStatus(CastleEnginePath) then
  begin
    PanelWarning.Visible := true;
    LabelWarning.Caption :=
      'Warning: Engine path not found or invalid.' + NL +
      'Configure valid path in "Preferences".';
  end else
  if not CompilerFound then
  begin
    PanelWarning.Visible := true;
    LabelWarning.Caption :=
      'Warning: Compiler (FPC or Delphi) not found.' + NL +
      'Install a compiler and configure in "Preferences".';
  end else
    PanelWarning.Visible := false;
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

  { On macOS, on the first run (after accepting "open application downloaded from the Internet")
    we may get additional command-line parameter -psn_...,
    which stands for "Process Serial Number".

    See e.g.
    https://stackoverflow.com/questions/10242115/os-x-strange-psn-command-line-parameter-when-launched-from-finder
    https://forums.macrumors.com/threads/application-adding-argument.207344/
    Note: It definitely happens even for Cocoa-based editor, not only Carbon,
    despite some suggestions on the Internet that it's Carbon-specific.
    Unfortunately there no (not anymore) official docs about it from Apple.

    We have to remove it, to not confuse it with a project name. }
  procedure RemoveMacOsProcessSerialNumber;
  {$ifdef DARWIN}
  var
    I: Integer;
  begin
    for I := 1 to Parameters.Count - 1 do
      if IsPrefix('-psn_', Parameters[I], false) then
      begin
        Parameters.Delete(I);
        Exit;
      end;
  {$else}
  begin
  {$endif}
  end;

begin
  if CommandLineHandled then Exit;
  CommandLineHandled := true;

  RemoveMacOsProcessSerialNumber;
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
