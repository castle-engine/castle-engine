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

{ Project form (@link(TProjectForm)). }
unit FormProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, ShellCtrls, StdCtrls, ValEdit, ProjectUtils,
  Types, Contnrs,
  CastleControl, CastleUIControls, CastlePropEdits, CastleDialogs,
  EditorUtils, FrameDesign;

type
  { Main project management. }
  TProjectForm = class(TForm)
    DesignFrame1: TDesignFrame;
    MenuItemDesign: TMenuItem;
    OpenDesignDialog: TCastleOpenDialog;
    MenuItemOpenDesign: TMenuItem;
    MenuItemSeparator201: TMenuItem;
    MenuItemNewDesignTransform: TMenuItem;
    MenuItemNewDesignUserInterface: TMenuItem;
    SaveDesignDialog: TCastleSaveDialog;
    MenuItemSaveAsDesign: TMenuItem;
    MenuItemSaveDesign: TMenuItem;
    ListOutput: TListBox;
    MainMenu1: TMainMenu;
    MenuItemSeparator101: TMenuItem;
    MenuItemBreakProcess: TMenuItem;
    MenuItemSeprator100: TMenuItem;
    MenuItemAutoGenerateClean: TMenuItem;
    MenuItemAutoGenerateTextures: TMenuItem;
    MenuItemPackageSource: TMenuItem;
    MenuItemModeRelease: TMenuItem;
    MenuItemPackage: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemModeDebug: TMenuItem;
    MenuItemSeparator3: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MenuItemReference: TMenuItem;
    MenuItemManual: TMenuItem;
    MenuItemCgeWww: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemSeparator: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemClean: TMenuItem;
    MenuItemOnlyRun: TMenuItem;
    MenuItemCompileRun: TMenuItem;
    MenuItemCompile: TMenuItem;
    MenuItemSwitchProject: TMenuItem;
    MenuItemRun: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemQuit: TMenuItem;
    PageControlBottom: TPageControl;
    PanelAboveTabs: TPanel;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;
    SplitterBetweenFiles: TSplitter;
    Splitter2: TSplitter;
    TabFiles: TTabSheet;
    TabOutput: TTabSheet;
    ProcessUpdateTimer: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListOutputClick(Sender: TObject);
    procedure MenuItemAutoGenerateCleanClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemAutoGenerateTexturesClick(Sender: TObject);
    procedure MenuItemBreakProcessClick(Sender: TObject);
    procedure MenuItemCgeWwwClick(Sender: TObject);
    procedure MenuItemCleanClick(Sender: TObject);
    procedure MenuItemCompileClick(Sender: TObject);
    procedure MenuItemCompileRunClick(Sender: TObject);
    procedure MenuItemManualClick(Sender: TObject);
    procedure MenuItemModeDebugClick(Sender: TObject);
    procedure MenuItemNewDesignUserInterfaceClick(Sender: TObject);
    procedure MenuItemNewDesignTransformClick(Sender: TObject);
    procedure MenuItemOnlyRunClick(Sender: TObject);
    procedure MenuItemOpenDesignClick(Sender: TObject);
    procedure MenuItemPackageClick(Sender: TObject);
    procedure MenuItemPackageSourceClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemReferenceClick(Sender: TObject);
    procedure MenuItemModeReleaseClick(Sender: TObject);
    procedure MenuItemSaveAsDesignClick(Sender: TObject);
    procedure MenuItemSaveDesignClick(Sender: TObject);
    procedure MenuItemSwitchProjectClick(Sender: TObject);
    procedure ProcessUpdateTimerTimer(Sender: TObject);
  private
    ProjectName: String;
    ProjectPath, ProjectPathUrl: String;
    BuildMode: TBuildMode;
    OutputList: TOutputList;
    RunningProcess: TAsynchronousProcessQueue;
    procedure BuildToolCall(const Commands: array of String);
    procedure SetEnabledCommandRun(const AEnabled: Boolean);
    procedure FreeProcess;
    procedure UpdateFormCaption(Sender: TObject);
    { Propose saving the hierarchy.
      Returns should we continue (user did not cancel). }
    function ProposeSaveDesign: Boolean;
  public
    procedure OpenProject(const ManifestUrl: String);
  end;

var
  ProjectForm: TProjectForm;

implementation

{$R *.lfm}

uses TypInfo,
  CastleXMLUtils, CastleLCLUtils, CastleOpenDocument, CastleURIUtils,
  CastleFilesUtils, CastleUtils, X3DNodes, CastleVectors, CastleColors,
  CastleScene, CastleSceneManager, Castle2DSceneManager,
  CastleTransform, CastleControls, CastleDownload, CastleApplicationProperties,
  CastleLog, CastleComponentSerialize,
  FormChooseProject, ToolUtils;

procedure TProjectForm.MenuItemQuitClick(Sender: TObject);
begin
  // TODO ask only if unsaved things
//  if YesNoBox('Quit the editor?') then
    Application.Terminate;
end;

procedure TProjectForm.MenuItemReferenceClick(Sender: TObject);
begin
  OpenURL('https://castle-engine.io/apidoc/html/index.html');
end;

procedure TProjectForm.MenuItemModeReleaseClick(Sender: TObject);
begin
  BuildMode := bmRelease;
  MenuItemModeRelease.Checked := true;
end;

procedure TProjectForm.MenuItemSaveAsDesignClick(Sender: TObject);
begin
  // TODO -- disable when DesignRoot = nil

  if DesignFrame1.DesignRoot is TCastleUserInterface then
    SaveDesignDialog.DefaultExt := 'castle-user-interface'
  else
  if DesignFrame1.DesignRoot is TCastleTransform then
    SaveDesignDialog.DefaultExt := 'castle-transform'
  else
    raise EInternalError.Create('DesignRoot does not descend from TCastleUserInterface or TCastleTransform');

  SaveDesignDialog.Url := DesignFrame1.DesignUrl;
  if SaveDesignDialog.Execute then
    DesignFrame1.SaveDesign(SaveDesignDialog.Url);
    // TODO: save DesignUrl somewhere? CastleEditorSettings.xml?
end;

procedure TProjectForm.MenuItemSaveDesignClick(Sender: TObject);
begin
  if DesignFrame1.DesignUrl = '' then
    MenuItemSaveAsDesignClick(Sender)
  else
    DesignFrame1.SaveDesign(DesignFrame1.DesignUrl);
end;

procedure TProjectForm.MenuItemCgeWwwClick(Sender: TObject);
begin
  OpenURL('https://castle-engine.io/');
end;

procedure TProjectForm.MenuItemAboutClick(Sender: TObject);
begin
  // TODO
  // Show logo, website link, Patreon link,
  // Copyright Michalis Kamburelis and many contributors (thank you!)
  // Show current (runtime) CGE, FPC version
  // Show CGE, FPC version when compiling editor
end;

procedure TProjectForm.MenuItemAutoGenerateTexturesClick(Sender: TObject);
begin
  BuildToolCall(['auto-generate-textures']);
end;

procedure TProjectForm.MenuItemBreakProcessClick(Sender: TObject);
begin
  if RunningProcess = nil then
    raise EInternalError.Create('It should not be possible to call this when RunningProcess = nil');

  OutputList.AddSeparator;
  OutputList.AddLine('Forcefully killing the process.', okError);
  FreeProcess;
end;

procedure TProjectForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // TODO ask only if unsaved things
  //if YesNoBox('Quit the editor?') then
    Application.Terminate;
end;

procedure TProjectForm.FormCreate(Sender: TObject);
begin
  OutputList := TOutputList.Create(ListOutput);
  DesignFrame1.OnUpdateFormCaption := @UpdateFormCaption;
end;

procedure TProjectForm.FormDestroy(Sender: TObject);
begin
  ApplicationDataOverride := '';
  FreeProcess;
  FreeAndNil(OutputList);
end;

procedure TProjectForm.ListOutputClick(Sender: TObject);
begin
  // TODO: just to source code line in case of error message here
end;

procedure TProjectForm.MenuItemAutoGenerateCleanClick(Sender: TObject);
begin
  BuildToolCall(['auto-generate-clean']);
end;

procedure TProjectForm.MenuItemCleanClick(Sender: TObject);
begin
  BuildToolCall(['clean']);
end;

procedure TProjectForm.MenuItemCompileClick(Sender: TObject);
begin
  BuildToolCall(['compile']);
end;

procedure TProjectForm.MenuItemCompileRunClick(Sender: TObject);
begin
  if ProposeSaveDesign then
    BuildToolCall(['compile', 'run']);
end;

procedure TProjectForm.MenuItemManualClick(Sender: TObject);
begin
  OpenURL('https://castle-engine.io/manual_intro.php');
end;

procedure TProjectForm.MenuItemModeDebugClick(Sender: TObject);
begin
  BuildMode := bmDebug;
  MenuItemModeDebug.Checked := true;
end;

procedure TProjectForm.MenuItemNewDesignUserInterfaceClick(Sender: TObject);
var
  NewRoot: TCastleUserInterfaceRect;
  NewDesignOwner: TComponent;
begin
  NewDesignOwner := TComponent.Create(Self);

  // TODO: Allow choosing starting class?
  NewRoot := TCastleUserInterfaceRect.Create(NewDesignOwner);
  NewRoot.Name := 'Group1';
  NewRoot.FullSize := true;
  DesignFrame1.OpenDesign(NewRoot, NewDesignOwner, '');
end;

procedure TProjectForm.MenuItemNewDesignTransformClick(Sender: TObject);
var
  NewRoot: TCastleTransform;
  NewDesignOwner: TComponent;
begin
  NewDesignOwner := TComponent.Create(Self);

  // TODO: Allow choosing starting class?
  // TODO: after adding new scenes, trasforms, adjust camera?
  NewRoot := TCastleTransform.Create(NewDesignOwner);
  NewRoot.Name := 'Transform1';
  DesignFrame1.OpenDesign(NewRoot, NewDesignOwner, '');
end;

procedure TProjectForm.MenuItemOnlyRunClick(Sender: TObject);
begin
  if ProposeSaveDesign then
    BuildToolCall(['run']);
end;

procedure TProjectForm.MenuItemOpenDesignClick(Sender: TObject);
begin
  OpenDesignDialog.Url := DesignFrame1.DesignUrl;
  if OpenDesignDialog.Execute then
    DesignFrame1.OpenDesign(OpenDesignDialog.Url);
end;

procedure TProjectForm.MenuItemPackageClick(Sender: TObject);
begin
  BuildToolCall(['package']);
end;

procedure TProjectForm.MenuItemPackageSourceClick(Sender: TObject);
begin
  BuildToolCall(['package-source']);
end;

procedure TProjectForm.MenuItemSwitchProjectClick(Sender: TObject);
begin
  // TODO ask only if unsaved things
  //if YesNoBox('Close this editor project?') then

  Free; // do not call Close, to avoid OnCloseQuery
  ChooseProjectForm.Show;
end;

procedure TProjectForm.ProcessUpdateTimerTimer(Sender: TObject);
begin
  if RunningProcess <> nil then
  begin
    RunningProcess.Update;
    if not RunningProcess.Running then
      FreeProcess;
  end;
end;

procedure TProjectForm.FreeProcess;
begin
  FreeAndNil(RunningProcess);
  SetEnabledCommandRun(true);
  ProcessUpdateTimer.Enabled := false;
end;

procedure TProjectForm.BuildToolCall(const Commands: array of String);
var
  BuildToolExe, ModeString, Command: String;
  QueueItem: TAsynchronousProcessQueue.TQueueItem;
begin
  if RunningProcess <> nil then
    raise EInternalError.Create('It should not be possible to call this when RunningProcess <> nil');

  BuildToolExe := FindExe('castle-engine');
  if BuildToolExe = '' then
  begin
    EditorUtils.ErrorBox('Cannot find build tool (castle-engine) on $PATH environment variable.');
    Exit;
  end;

  case BuildMode of
    bmDebug  : ModeString := '--mode=debug';
    bmRelease: ModeString := '--mode=release';
    else raise EInternalError.Create('BuildMode?');
  end;

  SetEnabledCommandRun(false);
  OutputList.Clear;
  PageControlBottom.ActivePage := TabOutput;
  ProcessUpdateTimer.Enabled := true;

  RunningProcess := TAsynchronousProcessQueue.Create;
  RunningProcess.OutputList := OutputList;

  for Command in Commands do
  begin
    QueueItem := TAsynchronousProcessQueue.TQueueItem.Create;
    QueueItem.ExeName := BuildToolExe;
    QueueItem.CurrentDirectory := ProjectPath;
    QueueItem.Parameters.Add(ModeString);
    QueueItem.Parameters.Add(Command);
    RunningProcess.Queue.Add(QueueItem);
  end;

  RunningProcess.Start;
end;

procedure TProjectForm.SetEnabledCommandRun(const AEnabled: Boolean);
begin
  MenuItemCompile.Enabled := AEnabled;
  MenuItemCompileRun.Enabled := AEnabled;
  MenuItemOnlyRun.Enabled := AEnabled;
  MenuItemClean.Enabled := AEnabled;
  MenuItemPackage.Enabled := AEnabled;
  MenuItemPackageSource.Enabled := AEnabled;
  MenuItemAutoGenerateTextures.Enabled := AEnabled;
  MenuItemAutoGenerateClean.Enabled := AEnabled;
  MenuItemBreakProcess.Enabled := not AEnabled;
end;

procedure TProjectForm.UpdateFormCaption(Sender: TObject);
begin
  Caption := DesignFrame1.FormCaption +
    SQuoteLCLCaption(ProjectName) + ' | Castle Game Engine';
end;

function TProjectForm.ProposeSaveDesign: Boolean;
var
  Mr: TModalResult;
begin
  Result := true;

  DesignFrame1.BeforeProposeSaveDesign;

  if DesignFrame1.DesignModified then
  begin
    Mr := MessageDlg('Save Design',
      'Design "' + DesignFrame1.DesignUrl + '" was modified but not saved yet. Save it before running the application?',
      mtConfirmation, mbYesNoCancel, 0);
    case Mr of
      mrYes: MenuItemSaveDesign.Click;
      mrCancel: Result := false;
    end;
  end;
end;

procedure TProjectForm.OpenProject(const ManifestUrl: String);
var
  ManifestDoc: TXMLDocument;
begin
  ManifestDoc := URLReadXML(ManifestUrl);
  try
    ProjectName := ManifestDoc.DocumentElement.AttributeString('name');
  finally FreeAndNil(ManifestDoc) end;

  ProjectPathUrl := ExtractURIPath(ManifestUrl);
  ProjectPath := URIToFilenameSafe(ProjectPathUrl);

  { override ApplicationData interpretation, and castle-data:/xxx URL,
    while this project is open. }
  ApplicationDataOverride := CombineURI(ProjectPathUrl, 'data/');
  OpenDesignDialog.InitialDir := URIToFilenameSafe(ApplicationDataOverride);
  SaveDesignDialog.InitialDir := URIToFilenameSafe(ApplicationDataOverride);

  ShellTreeView1.Root := ProjectPath;

  // start
  MenuItemNewDesignUserInterfaceClick(nil);

  // It's too easy to change it visually and forget, so we set it from code
  PageControlBottom.ActivePage := TabFiles;
  SetEnabledCommandRun(true);

  BuildMode := bmDebug;
  MenuItemModeDebug.Checked := true;
end;

initialization
  // initialize CGE log
  ApplicationProperties.ApplicationName := 'castle-editor';
  InitializeLog;
end.
