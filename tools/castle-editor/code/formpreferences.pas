{
  Copyright 2019-2022 Michalis Kamburelis.

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
  ExtCtrls, ButtonPanel, ComCtrls;

type
  TPreferencesForm = class(TForm)
    ButtonRegisterLazarusPackages: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBoxMuteOnRun: TCheckBox;
    DirectoryEditFpc: TDirectoryEdit;
    DirectoryEditLazarus: TDirectoryEdit;
    EditCodeEditorCommand: TFileNameEdit;
    EditCodeEditorCommandLineColumn: TFileNameEdit;
    EditCodeEditorCommandProject: TFileNameEdit;
    LabelCodeEditorAutodetect: TLabel;
    LabelCodeEditorCommandLineColumn: TLabel;
    LabelCompilerAutodetect: TLabel;
    LabelCompilerDelphi: TLabel;
    LabelCompilationHeader: TLabel;
    LabelCodeEditorLazarus: TLabel;
    LabelCodeEditorDelphi: TLabel;
    LabelCompilerFpc: TLabel;
    LabelCodeEditorVSCode: TLabel;
    LabelInstructions0: TLabel;
    LabelInstructions1: TLabel;
    LabelInstructions2: TLabel;
    LabelLazarusWebsite: TLabel;
    LabelVolume: TLabel;
    LabelCodeEditorCommandInstructions: TLabel;
    LabelCodeEditorCommand: TLabel;
    LabelCodeEditorCommandProjectInstructions: TLabel;
    LabelCodeEditorHeader: TLabel;
    LabelSound: TLabel;
    LabelFpc: TLabel;
    LabelFpcAutoDetectedCaption: TLabel;
    LabelLazarus: TLabel;
    LabelLazarusAutoDetectedCaption: TLabel;
    LabelTitle: TLabel;
    LabelFpcAutoDetected: TLabel;
    LabelLazarusAutoDetected: TLabel;
    ListPages: TListBox;
    PanelCompilation: TPanel;
    PanelInstructions: TPanel;
    PanelCodeEditor: TPanel;
    PanelSound: TPanel;
    PanelFpcLazarusConfig: TPanel;
    RadioCodeEditorAutodetect: TRadioButton;
    RadioCompilerAutodetect: TRadioButton;
    RadioCodeEditorCustom: TRadioButton;
    RadioCompilerDelphi: TRadioButton;
    RadioCodeEditorLazarus: TRadioButton;
    RadioCodeEditorDelphi: TRadioButton;
    RadioCompilerFpc: TRadioButton;
    RadioCodeEditorVSCode: TRadioButton;
    TrackVolume: TTrackBar;
    procedure ButtonRegisterLazarusPackagesClick(Sender: TObject);
    procedure DirectoryEditFpcChange(Sender: TObject);
    procedure DirectoryEditLazarusChange(Sender: TObject);
    procedure EditCodeEditorCommandAcceptFileName(Sender: TObject;
      var Value: String);
    procedure EditCodeEditorCommandLineColumnAcceptFileName(Sender: TObject;
      var Value: String);
    procedure EditCodeEditorCommandProjectAcceptFileName(Sender: TObject;
      var Value: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LabelLazarusWebsiteClick(Sender: TObject);
    procedure ListPagesClick(Sender: TObject);
    procedure RadioCodeEditorAnyChange(Sender: TObject);
    procedure TrackVolumeChange(Sender: TObject);
  private
    OriginalFpcCustomPath, OriginalLazarusCustomPath: String;
    procedure UpdateAutoDetectedLabels;
    procedure UpdatePageVisible;
  public

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses CastleOpenDocument, CastleUtils, CastleLog, CastleSoundEngine,
  ToolCompilerInfo, ToolFpcVersion, ToolCommonUtils, ToolManifest,
  EditorUtils;

{$R *.lfm}

{ TPreferencesForm }

procedure TPreferencesForm.LabelLazarusWebsiteClick(Sender: TObject);
begin
  OpenURL('https://www.lazarus-ide.org/');
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
var
  FpcExe, FpcVer, LazarusExe, LazarusVer, DelphiPath, VSCodeExe: String;
begin
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
  // ListPages.ItemIndex := ; // leave previous
  UpdatePageVisible;

  UpdateAutoDetectedLabels;

  { Update Enabled of edit fields }
  RadioCodeEditorAnyChange(nil);

  DirectoryEditFpc.Directory := FpcCustomPath;
  DirectoryEditLazarus.Directory := LazarusCustomPath;
  { We will change the global Fpc/LazarusCustomPath during this dialog,
    so allow to revert them on "Cancel". }
  OriginalFpcCustomPath := FpcCustomPath;
  OriginalLazarusCustomPath := LazarusCustomPath;

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

procedure TPreferencesForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    { copy UI -> global variables }

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
  end else
  begin
    { XxxCustomPath are special.
      They are updated all the time when you change them in UI,
      this makes functions like FindExeLazarusIDE work OK,
      and at the end we restore them to orignal values if user *did not*
      accept the changes by clicking "OK". }
    FpcCustomPath := OriginalFpcCustomPath;
    LazarusCustomPath := OriginalLazarusCustomPath;
  end;

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
var
  ExecutionLog: String;

  procedure RegisterPackage(const LpkFileName: String);
  var
    LazbuildExe, LazbuildOutput, PackageFileName, CommandLog: String;
    LazbuildExitStatus: integer;
  begin
    LazbuildExe := FindExeLazbuild;

    PackageFileName := CastleEnginePath + LpkFileName;

    MyRunCommandIndir(
      GetCurrentDir { no better directory, but also should not matter },
      LazbuildExe, [
        '--add-package-link',
        PackageFileName
      ], LazbuildOutput, LazbuildExitStatus);

    CommandLog := 'lazbuild --add-package-link "' + PackageFileName + '"';
    ExecutionLog := ExecutionLog + NL + NL + CommandLog;
    WritelnLog('lazbuild status %d, output:' + NL + '%s', [LazbuildExitStatus, LazbuildOutput]);

    if (LazbuildExitStatus <> 0) or
       (Pos('Invalid option', LazbuildOutput) <> 0) { lazbuild has exit status 0 in this case } then
      raise Exception.Create('Executing lazbuild failed:' + NL + NL + LazbuildOutput);
  end;

begin
  ExecutionLog := 'Lazarus packages registed successfully.' + NL + NL +
    'Executed the following commands:';
  try
    RegisterPackage('src/vampyre_imaginglib/src/Packages/VampyreImagingPackage.lpk');
    RegisterPackage('src/vampyre_imaginglib/src/Packages/VampyreImagingPackageExt.lpk');

    RegisterPackage('packages/castle_base.lpk');
    RegisterPackage('packages/castle_window.lpk');
    RegisterPackage('packages/castle_components.lpk');
    RegisterPackage('packages/alternative_castle_window_based_on_lcl.lpk');
    RegisterPackage('packages/castle_indy.lpk');

    ShowMessage(ExecutionLog);
  except
    on E: Exception do
      ErrorBox(E.Message);
  end;
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

procedure TPreferencesForm.UpdatePageVisible;
var
  SelectedPage: TPanel;
begin
  case ListPages.ItemIndex of
    0: SelectedPage := PanelCodeEditor;
    1: SelectedPage := PanelCompilation;
    2: SelectedPage := PanelFpcLazarusConfig;
    3: SelectedPage := PanelSound;
    else raise Exception.CreateFmt('Unexpected ListPages.ItemIndex %d', [ListPages.ItemIndex]);
  end;
  SetEnabledVisible(PanelCodeEditor      , PanelCodeEditor       = SelectedPage);
  SetEnabledVisible(PanelCompilation     , PanelCompilation       = SelectedPage);
  SetEnabledVisible(PanelFpcLazarusConfig, PanelFpcLazarusConfig = SelectedPage);
  SetEnabledVisible(PanelSound           , PanelSound            = SelectedPage);
end;

end.
