{
  Copyright 2019-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Form to configure CGE editor (@link(TPreferencesForm)). }
unit FormPreferences;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ExtCtrls, ButtonPanel, ComCtrls, Spin, AnchorDockPanel;

type
  TPreferencesForm = class(TForm)
    ButtonRegisterLazarusPackages: TButton;
    ButtonTestStyle: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBoxEditorAppearance: TCheckBox;
    CheckBoxChangeMenu: TCheckBox;
    CheckBoxMuteOnRun: TCheckBox;
    DirectoryEditAndroidHome: TDirectoryEdit;
    DirectoryEditCgePath: TDirectoryEdit;
    DirectoryEditFpc: TDirectoryEdit;
    DirectoryEditJavaHome: TDirectoryEdit;
    DirectoryEditLazarus: TDirectoryEdit;
    EditCodeEditorCommand: TFileNameEdit;
    EditCodeEditorCommandLineColumn: TFileNameEdit;
    EditCodeEditorCommandProject: TFileNameEdit;
    EditSplitterSize: TSpinEdit;
    LabelSplitterTestRight: TLabel;
    LabelSplitterTestLeft: TLabel;
    Label2: TLabel;
    LabelCodeEditorCommand: TLabel;
    LabelCodeEditorCommandLineColumn: TLabel;
    LabelCodeEditorCommandProjectInstructions: TLabel;
    LabelFontSize: TLabel;
    LabelAndroidDocsWww: TLabel;
    LabelAndroidDocsWwwCaption: TLabel;
    LabelAndroidHome: TLabel;
    LabelAndroidHomeHint: TLabel;
    LabelCgePath: TLabel;
    LabelCgePathAutoDetected: TLabel;
    LabelCgePathAutoDetectedCaption: TLabel;
    LabelCodeEditorAutodetect: TLabel;
    LabelCodeEditorCommandInstructions: TLabel;
    LabelCodeEditorDelphi: TLabel;
    LabelCodeEditorHeader: TLabel;
    LabelCodeEditorLazarus: TLabel;
    LabelCodeEditorVSCode: TLabel;
    LabelCompilationHeader: TLabel;
    LabelCompilerAutodetect: TLabel;
    LabelCompilerDelphi: TLabel;
    LabelCompilerFpc: TLabel;
    LabelFpc: TLabel;
    LabelFpcAutoDetected: TLabel;
    LabelFpcAutoDetectedCaption: TLabel;
    LabelInstructions0: TLabel;
    LabelInstructions1: TLabel;
    LabelInstructions2: TLabel;
    LabelJavaHome: TLabel;
    LabelJavaHomeHint: TLabel;
    LabelLazarus: TLabel;
    LabelLazarusAutoDetected: TLabel;
    LabelLazarusAutoDetectedCaption: TLabel;
    LabelLazarusWebsite: TLabel;
    LabelSound: TLabel;
    LabelSplitterSize: TLabel;
    LabelTitle: TLabel;
    LabelVolume: TLabel;
    ListPages: TListBox;
    PanelSplitterTest: TPanel;
    PanelRegisterLazarusPackages: TPanel;
    PanelCustomCommands: TPanel;
    PanelSplitterSize: TPanel;
    PanelEditorFontSize: TPanel;
    RadioCodeEditorAutodetect: TRadioButton;
    RadioCodeEditorCustom: TRadioButton;
    RadioCodeEditorDelphi: TRadioButton;
    RadioCodeEditorLazarus: TRadioButton;
    RadioCodeEditorVSCode: TRadioButton;
    RadioCompilerAutodetect: TRadioButton;
    RadioCompilerDelphi: TRadioButton;
    RadioCompilerFpc: TRadioButton;
    ScrollBoxAndroid: TScrollBox;
    ScrollBoxCodeEditor: TScrollBox;
    ScrollBoxCompilation: TScrollBox;
    ScrollBoxGeneral: TScrollBox;
    ScrollBoxFpcLazarusConfig: TScrollBox;
    ScrollBoxSound: TScrollBox;
    EditFontSize: TSpinEdit;
    Splitter1: TSplitter;
    SplitterTest: TSplitter;
    TrackVolume: TTrackBar;
    procedure ButtonRegisterLazarusPackagesClick(Sender: TObject);
    procedure ButtonTestStyleClick(Sender: TObject);
    procedure CheckBoxEditorAppearanceChange(Sender: TObject);
    procedure DirectoryEditAndroidHomeAcceptDirectory(Sender: TObject;
      var Value: String);
    procedure DirectoryEditCgePathChange(Sender: TObject);
    procedure DirectoryEditFpcChange(Sender: TObject);
    procedure DirectoryEditLazarusChange(Sender: TObject);
    procedure EditCodeEditorCommandAcceptFileName(Sender: TObject;
      var Value: String);
    procedure EditCodeEditorCommandLineColumnAcceptFileName(Sender: TObject;
      var Value: String);
    procedure EditCodeEditorCommandProjectAcceptFileName(Sender: TObject;
      var Value: String);
    procedure EditSplitterSizeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LabelAndroidDocsWwwClick(Sender: TObject);
    procedure LabelLazarusWebsiteClick(Sender: TObject);
    procedure ListPagesClick(Sender: TObject);
    procedure RadioCodeEditorAnyChange(Sender: TObject);
    procedure TrackVolumeChange(Sender: TObject);
  private
    OriginalFpcCustomPath, OriginalLazarusCustomPath, OriginalCastleEngineOverridePath: String;
    procedure UpdateAutoDetectedLabels;
    procedure UpdatePageVisible;
    procedure UpdateStyle;
    procedure TestStyle;
    procedure SaveStyle;
    procedure LoadStyle;
  public

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses CastleOpenDocument, CastleUtils, CastleLog, CastleSoundEngine,
  CastleStringUtils, CastleFilesUtils, CastleUriUtils,
  ToolCompilerInfo, ToolFpcVersion, ToolCommonUtils, ToolManifest,
  EditorUtils, ProjectUtils, StyleUtils;

{$R *.lfm}

{ TPreferencesForm }

procedure TPreferencesForm.LabelLazarusWebsiteClick(Sender: TObject);
begin
  OpenUrl('https://www.lazarus-ide.org/');
end;

procedure TPreferencesForm.ListPagesClick(Sender: TObject);
begin
  UpdatePageVisible;
end;

procedure TPreferencesForm.RadioCodeEditorAnyChange(Sender: TObject);
begin
  EditCodeEditorCommand.Enabled := RadioCodeEditorCustom.Checked;
  EditCodeEditorCommandLineColumn.Enabled := RadioCodeEditorCustom.Checked;
  EditCodeEditorCommandProject.Enabled := RadioCodeEditorCustom.Checked;
end;

procedure TPreferencesForm.TrackVolumeChange(Sender: TObject);
begin
  { While the volume will be adjusted in TPreferencesForm.FormClose too,
    along with EditorVolume, but it is natural to also modify the audible
    volume immediately. }
  SoundEngineSetVolume(TrackVolume.Position / TrackVolume.Max);
end;

procedure TPreferencesForm.UpdateAutoDetectedLabels;

  function CgePathAutodetectedLine: String;
  begin
    if (CastleEngineOverridePath = '') and
       (CastleEnginePath <> '') then
      Result := 'Auto-detected in: ' + CastleEnginePath + NL
    else
      Result := '';
  end;

var
  FpcExe, FpcVer, LazarusExe, LazarusVer, DelphiPath, VSCodeExe,
    CgePathStatusText: String;
begin
  CgePathStatus(CastleEnginePath, CgePathStatusText);
  LabelCgePathAutoDetected.Caption := CgePathAutodetectedLine + CgePathStatusText;

  FpcExe := '';
  try
    FpcExe := FindExeFpcCompiler;
    FpcVer := FpcVersion.ToString;
    LabelFpcAutoDetected.Caption :=
      'FPC executable: ' + FpcExe + NL +
      'FPC version: ' + FpcVer;
  except
    on E: Exception do
    begin
      WritelnLog('Cannot determine FPC executable or version, error: ' + ExceptMessage(E));
      LabelFpcAutoDetected.Caption :=
        'Cannot determine FPC executable.' + NL +
        'Make sure FPC is installed, and available on $PATH or configured above.';
    end;
  end;

  LazarusExe := '';
  try
    LazarusExe := FindExeLazarusIDE;
    LazarusVer := LazarusVersion.ToString;
    LabelLazarusAutoDetected.Caption :=
      'Lazarus executable: ' + LazarusExe + NL +
      'Lazarus version: ' + LazarusVer;
  except
    on E: EExecutableNotFound do
    begin
      WritelnLog('Cannot determine Lazarus executable, error: ' + ExceptMessage(E));
      LabelLazarusAutoDetected.Caption :=
        'Cannot determine Lazarus executable.' + NL +
        'Make sure Lazarus is installed, and available on $PATH or configured above.';
    end;
  end;

  if FpcExe <> '' then
    LabelCompilerFpc.Caption := 'Detected: ' + FpcExe
  else
    LabelCompilerFpc.Caption := 'Not found.';

  if LazarusExe <> '' then
    LabelCodeEditorLazarus.Caption := 'Detected: ' + LazarusExe
  else
    LabelCodeEditorLazarus.Caption := 'Not found.';

  DelphiPath := FindDelphiPath(false);
  if DelphiPath <> '' then
  begin
    LabelCodeEditorDelphi.Caption := 'Detected path: ' + DelphiPath;
    LabelCompilerDelphi.Caption := 'Detected path: ' + DelphiPath;
  end else
  begin
    LabelCodeEditorDelphi.Caption := 'Not found.';
    LabelCompilerDelphi.Caption := 'Not found.';
  end;

  VSCodeExe := FindExeVSCode(false);
  if VSCodeExe <> '' then
    LabelCodeEditorVSCode.Caption := 'Detected: ' + VSCodeExe
  else
    LabelCodeEditorVSCode.Caption := 'Not found.';

  if LazarusExe <> '' then
    LabelCodeEditorAutodetect.Caption := 'First auto-detected IDE: Lazarus'
  else
  if DelphiPath <> '' then
    LabelCodeEditorAutodetect.Caption := 'First auto-detected IDE: Delphi'
  else
  if VSCodeExe <> '' then
    LabelCodeEditorAutodetect.Caption := 'First auto-detected IDE: Visual Studio Code'
  else
    LabelCodeEditorAutodetect.Caption := 'First auto-detected IDE: None';

  if FpcExe <> '' then
    LabelCompilerAutodetect.Caption := 'First auto-detected compiler: FPC'
  else
  if DelphiPath <> '' then
    LabelCompilerAutodetect.Caption := 'First auto-detected compiler: Delphi'
  else
    LabelCompilerAutodetect.Caption := 'First auto-detected compiler: None';
end;

procedure TPreferencesForm.FormShow(Sender: TObject);
begin
  LoadStyle;
  // ListPages.ItemIndex := ; // leave previous
  UpdatePageVisible;

  UpdateAutoDetectedLabels;

  { Update Enabled of edit fields }
  RadioCodeEditorAnyChange(nil);

  DirectoryEditFpc.Directory := FpcCustomPath;
  DirectoryEditLazarus.Directory := LazarusCustomPath;
  DirectoryEditCgePath.Directory := CastleEngineOverridePath;
  DirectoryEditAndroidHome.Directory := AndroidHome;
  DirectoryEditJavaHome.Directory := JavaHome;
  { We will change the global Fpc/LazarusCustomPath during this dialog,
    so allow to revert them on "Cancel". }
  OriginalFpcCustomPath := FpcCustomPath;
  OriginalLazarusCustomPath := LazarusCustomPath;
  OriginalCastleEngineOverridePath := CastleEngineOverridePath;

  // Note that making any RadioCodeEditorXxx checked will uncheck the others
  case CodeEditor of
    ceAutodetect: RadioCodeEditorAutodetect.Checked := true;
    ceLazarus   : RadioCodeEditorLazarus.Checked := true;
    ceDelphi    : RadioCodeEditorDelphi.Checked := true;
    ceVSCode    : RadioCodeEditorVSCode.Checked := true;
    ceCustom    : RadioCodeEditorCustom.Checked := true;
    else raise EInternalError.Create('CodeEditor?');
  end;
  EditCodeEditorCommand.Text := CodeEditorCommand;
  EditCodeEditorCommandLineColumn.Text := CodeEditorCommandLineColumn;
  EditCodeEditorCommandProject.Text := CodeEditorCommandProject;

  // compilation tab
  case Compiler of
    coAutodetect: RadioCompilerAutodetect.Checked := true;
    coFpc       : RadioCompilerFpc.Checked := true;
    coDelphi    : RadioCompilerDelphi.Checked := true;
    else raise EInternalError.Create('Compiler?');
  end;

  // sound tab
  TrackVolume.Position := Round(EditorVolume * TrackVolume.Max);
  CheckBoxMuteOnRun.Checked := MuteOnRun;
end;

procedure TPreferencesForm.LabelAndroidDocsWwwClick(Sender: TObject);
begin
  OpenUrl('https://castle-engine.io/android');
end;

procedure TPreferencesForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);

  { When CastleEngineOverridePath changed, we need to recalculate stuff based on it. }
  procedure CastleEngineOverridePathChanged;
  var
    EditorApplicationData: TEditorApplicationData;
  begin
    { Recalculate InternalCastleDesignData that depends on detected CGE path.
      This way changes to CGE path in "Preferences" update also
      InternalCastleDesignData used to read e.g. 3D models of gizmos.

      Testcase when this is needed:
      - test with castle-editor in bin/ subdirectory of CGE
        (like in binary distribution; this means that ExeName will not be enough
        to guess editor data location)
      - set CastleEngineOverridePath to something invalid but non-empty
        (i.e. to non-existing dir)
      - restart editor (TChooseProjectForm.FormCreate will set
        InternalCastleDesignData to '',
        because CastleEnginePath is invalid and
        CastleEnginePath + tools/castle-editor/data doesn't exist)
      - open some project, open some design with viewport.
        It will fail, and it's kind of OK -- engine path was invalid.
      - go to Preferences and change CastleEngineOverridePath to empty
      - reopen the project (or reopen design within it)
      - now it should open OK.
    }
    EditorApplicationData := TEditorApplicationData.Create;
    try
      InternalCastleDesignData := ResolveCastleDataUrl('castle-data:/');
    finally FreeAndNil(EditorApplicationData) end;
  end;

begin
  if ModalResult = mrOK then
  begin
    { copy UI -> global variables }

    SaveStyle;

    // code editor tab
    if RadioCodeEditorCustom.Checked then
      CodeEditor := ceCustom
    else
    if RadioCodeEditorAutodetect.Checked then
      CodeEditor := ceAutodetect
    else
    if RadioCodeEditorLazarus.Checked then
      CodeEditor := ceLazarus
    else
    if RadioCodeEditorDelphi.Checked then
      CodeEditor := ceDelphi
    else
    if RadioCodeEditorVSCode.Checked then
      CodeEditor := ceVSCode
    else
      raise EInternalError.Create('Cannot determine CodeEditor choice, no radio selected');

    CodeEditorCommand := EditCodeEditorCommand.Text;
    CodeEditorCommandLineColumn := EditCodeEditorCommandLineColumn.Text;
    CodeEditorCommandProject := EditCodeEditorCommandProject.Text;

    // compilation tab
    if RadioCompilerAutodetect.Checked then
      Compiler := coAutodetect
    else
    if RadioCompilerFpc.Checked then
      Compiler := coFpc
    else
    if RadioCompilerDelphi.Checked then
      Compiler := coDelphi
    else
      raise EInternalError.Create('Cannot determine Compiler choice, no radio selected');

    // sound tab
    EditorVolume := TrackVolume.Position / TrackVolume.Max;
    MuteOnRun := CheckBoxMuteOnRun.Checked;

    // Android tab
    AndroidHome := DirectoryEditAndroidHome.Directory;
    JavaHome := DirectoryEditJavaHome.Directory;
  end else
  begin
    { XxxCustomPath are special.
      They are updated all the time when you change them in UI,
      this makes functions like FindExeLazarusIDE work OK,
      and at the end we restore them to orignal values if user *did not*
      accept the changes by clicking "OK". }
    FpcCustomPath := OriginalFpcCustomPath;
    LazarusCustomPath := OriginalLazarusCustomPath;
    CastleEngineOverridePath := OriginalCastleEngineOverridePath;
  end;

  CastleEngineOverridePathChanged;

  { Set SoundEngine.Volume regardless if we accepted
    (so MuteOnRun, EditorVolume changed) or not (so they are unchanged)
    to revert any immediate changes to volume in TPreferencesForm.TrackVolumeChange. }
  SoundEngineSetVolume;
end;

procedure TPreferencesForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOK then
  begin
    if RadioCodeEditorCustom.Checked and
       (Trim(EditCodeEditorCommand.Text) = '') then
    begin
      ErrorBox('You must specify some custom editor command, or switch to use other (e.g. autodetected) code editor');
      CanClose := false;
      Exit;
    end;
  end;
end;

procedure TPreferencesForm.DirectoryEditFpcChange(Sender: TObject);
begin
  FpcCustomPath := DirectoryEditFpc.Directory;
  UpdateAutoDetectedLabels;
end;

procedure TPreferencesForm.ButtonRegisterLazarusPackagesClick(Sender: TObject);

  procedure RegisterPackage(const LpkFileName: String);
  var
    LazbuildExe, LazbuildOutput, PackageFileName: String;
    LazbuildExitStatus: integer;
  begin
    LazbuildExe := FindExeLazbuild;

    PackageFileName := CastleEnginePath + LpkFileName;

    WritelnLog('Executing: lazbuild --add-package-link "' + PackageFileName + '"');
    MyRunCommandIndir(
      GetCurrentDir { no better directory, but also should not matter },
      LazbuildExe, [
        '--add-package-link',
        PackageFileName
      ], LazbuildOutput, LazbuildExitStatus);

    WritelnLog('Execution finished: lazbuild status %d, output:' + NL + '%s', [LazbuildExitStatus, LazbuildOutput]);

    if (LazbuildExitStatus <> 0) or
       (Pos('Invalid option', LazbuildOutput) <> 0) { lazbuild has exit status 0 in this case } then
      raise Exception.Create('Executing lazbuild failed:' + NL + NL + LazbuildOutput);
  end;

begin
  try
    RegisterPackage('packages/castle_base.lpk');
    RegisterPackage('packages/castle_window.lpk');
    RegisterPackage('packages/castle_components.lpk');
    RegisterPackage('packages/castle_editor_components.lpk');
    RegisterPackage('packages/alternative_castle_window_based_on_lcl.lpk');
    RegisterPackage('packages/castle_indy.lpk');

    ShowMessage('Lazarus packages registered successfully.');
  except
    on E: Exception do
      ErrorBox(E.Message);
  end;
end;

procedure TPreferencesForm.ButtonTestStyleClick(Sender: TObject);
begin
  TestStyle;
end;

procedure TPreferencesForm.CheckBoxEditorAppearanceChange(Sender: TObject);
begin
  if CheckBoxEditorAppearance.Checked then
  begin
    EnableStyles;
    LoadStyle;
  end
  else
  begin
    DisableStyles;
    ShowMessage('Changes will be applied after app restart');
  end;
end;

procedure TPreferencesForm.DirectoryEditAndroidHomeAcceptDirectory(
  Sender: TObject; var Value: String);
begin
  if Value <> '' then
  begin
    if not DirectoryExists(Value) then
    begin
      WarningBox(Format('Directory "%s" does not exist', [Value]));
    end else
    if not (
        // Android SDK 30 doesn't contain 'tools', only 'platform-tools', so don't check it
        // DirectoryExists(InclPathDelim(Value) + 'tools') and
        DirectoryExists(InclPathDelim(Value) + 'platform-tools')) then
    begin
      WarningBox(Format('Directory "%s" does not contain typical Android SDK subdirectories "tools", "platform-tools". Make sure it is correct.', [Value]));
    end;
  end;
end;

procedure TPreferencesForm.DirectoryEditCgePathChange(Sender: TObject);
begin
  CastleEngineOverridePath := DirectoryEditCgePath.Directory;
  UpdateAutoDetectedLabels;
end;

procedure TPreferencesForm.DirectoryEditLazarusChange(Sender: TObject);
begin
  LazarusCustomPath := DirectoryEditLazarus.Directory;
  UpdateAutoDetectedLabels;
end;

procedure TPreferencesForm.EditCodeEditorCommandAcceptFileName(Sender: TObject;
  var Value: String);
begin
  // auto-add ${PAS} macro and propose quoting
  Value := '"' + Value + '" ${PAS}';
end;

procedure TPreferencesForm.EditCodeEditorCommandLineColumnAcceptFileName(
  Sender: TObject; var Value: String);
begin
  // auto-add macros and propose quoting
  Value := '"' + Value + '" ${PAS}:${LINE}:${COLUMN}';
end;

procedure TPreferencesForm.EditCodeEditorCommandProjectAcceptFileName(
  Sender: TObject; var Value: String);
begin
  // auto-add ${PROJECT_DIR} macro and propose quoting
  Value := '"' + Value + '" ${PROJECT_DIR}';
end;

procedure TPreferencesForm.EditSplitterSizeChange(Sender: TObject);
begin
  SplitterTest.Width := EditSplitterSize.Value;
  SplitterTest.Constraints.MinWidth := EditSplitterSize.Value;
end;

procedure TPreferencesForm.UpdatePageVisible;
var
  PageIndex: Integer;
  SelectedPage: TScrollBox;
begin
  PageIndex := ListPages.ItemIndex;
  if PageIndex = -1 then
    // on macOS (Cocoa) user can click anywhere in list to deselect it, tolerate it
    PageIndex := 0;

  case PageIndex of
    0: SelectedPage := ScrollBoxGeneral;
    1: SelectedPage := ScrollBoxCodeEditor;
    2: SelectedPage := ScrollBoxCompilation;
    3: SelectedPage := ScrollBoxFpcLazarusConfig;
    4: SelectedPage := ScrollBoxSound;
    5: SelectedPage := ScrollBoxAndroid;
    else raise Exception.CreateFmt('Unexpected ListPages.ItemIndex %d', [ListPages.ItemIndex]);
  end;
  SetEnabledVisible(ScrollBoxGeneral         , ScrollBoxGeneral          = SelectedPage);
  SetEnabledVisible(ScrollBoxCodeEditor      , ScrollBoxCodeEditor       = SelectedPage);
  SetEnabledVisible(ScrollBoxCompilation     , ScrollBoxCompilation      = SelectedPage);
  SetEnabledVisible(ScrollBoxFpcLazarusConfig, ScrollBoxFpcLazarusConfig = SelectedPage);
  SetEnabledVisible(ScrollBoxSound           , ScrollBoxSound            = SelectedPage);
  SetEnabledVisible(ScrollBoxAndroid         , ScrollBoxAndroid          = SelectedPage);
end;

procedure TPreferencesForm.UpdateStyle;
begin
  StyleUtils.UpdateControlStyle(Self, false, true);
end;

procedure TPreferencesForm.TestStyle;
var
  savedStyle: TCastleEditorStyle;
begin
  savedStyle := CurrentStyle;
  CurrentStyle.FontSize := EditFontSize.Value;
  CurrentStyle.SplitterSize := EditSplitterSize.Value;

  UpdateControlStyle(Self, false, true);

  CurrentStyle := savedStyle;
end;

procedure TPreferencesForm.SaveStyle;
begin
  CurrentStyle.FontSize := EditFontSize.Value;
  CurrentStyle.SplitterSize := EditSplitterSize.Value;
  CurrentStyle.ChangeMenu := CheckBoxChangeMenu.Checked;
  CurrentStyle.UsePlugin := CheckBoxEditorAppearance.Checked;
  SaveStyleSettings;
end;

procedure TPreferencesForm.LoadStyle;
begin
  EditFontSize.MinValue := StyleUtils.EditorFontSizeMin;
  EditFontSize.MaxValue := StyleUtils.EditorFontSizeMax;
  EditSplitterSize.MinValue := StyleUtils.SplitterSizeMin;
  EditSplitterSize.MaxValue := StyleUtils.SplitterSizeMax;

  LoadStyleSettings;

  CheckBoxEditorAppearance.Checked := CurrentStyle.UsePlugin;
  EditFontSize.Value := CurrentStyle.FontSize;
  //EditFontSize.Enabled := CurrentStyle.UsePlugin;
  EditSplitterSize.Value := CurrentStyle.SplitterSize;
  //EditSplitterSize.Enabled:= CurrentStyle.UsePlugin;
  CheckBoxChangeMenu.Checked := CurrentStyle.ChangeMenu;
  //CheckBoxChangeMenu.Enabled := CurrentStyle.UsePlugin;

  UpdateStyle;
end;

end.
