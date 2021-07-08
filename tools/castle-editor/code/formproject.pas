{
  Copyright 2018-2021 Michalis Kamburelis.

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
  ExtCtrls, ComCtrls, CastleShellCtrls, StdCtrls, ValEdit, ActnList, Buttons,
  ProjectUtils, Types, Contnrs, CastleControl, CastleUIControls,
  CastlePropEdits, CastleDialogs, X3DNodes, CastleFindFiles,
  EditorUtils, FrameDesign, FrameViewFile, FormNewUnit, ToolManifest;

type
  { Main project management. }
  TProjectForm = class(TForm)
    ActionNewSpriteSheet: TAction;
    ActionList: TActionList;
    MenuItemDesignNewNonVisualCustomRoot: TMenuItem;
    MenuItemDesignNewNonVisual: TMenuItem;
    MenuItemDesignAddNonVisual: TMenuItem;
    MenuItemDesignAddBehavior: TMenuItem;
    MenuItemNewCastleSpriteSheet: TMenuItem;
    MenuItemData: TMenuItem;
    MenuItemNewSpriteSheet: TMenuItem;
    MenuItemSepraratorSLP002: TMenuItem;
    ActionRegenerateProject: TAction;
    ActionEditAssociatedUnit: TAction;
    ActionNewUnitHereClass: TAction;
    ActionNewUnitHereState: TAction;
    ActionNewUnitHereEmpty: TAction;
    ActionNewUnitClass: TAction;
    ActionNewUnitState: TAction;
    ActionNewUnitEmpty: TAction;
    ActionEditUnit: TAction;
    ActionOpenProjectCode: TAction;
    ApplicationProperties1: TApplicationProperties;
    ButtonClearWarnings: TBitBtn;
    MenuItem1: TMenuItem;
    MenuItemRegenerateProject: TMenuItem;
    MenuItemSeparator123123345: TMenuItem;
    OpenPascalUnitDialog: TCastleOpenPascalUnitDialog;
    MenuItemPopupNewUnitEmpty: TMenuItem;
    MenuItemPopupNewUnitClass: TMenuItem;
    MenuItemPopupNewUnitState: TMenuItem;
    MenuItemPopupNewUnit: TMenuItem;
    N3: TMenuItem;
    MenuItemNewUnitState: TMenuItem;
    MenuItemNewUnitClass: TMenuItem;
    MenuItemNewUnitEmpty: TMenuItem;
    MenuItemNewUnit: TMenuItem;
    N2: TMenuItem;
    MenuItemEditUnitCode: TMenuItem;
    MenuItemOpenProjectCode: TMenuItem;
    MenuItemCode: TMenuItem;
    MenuItemNewDirectory: TMenuItem;
    MenuItemShellTreeSeparator13123: TMenuItem;
    MenuItemShellTreeRefresh: TMenuItem;
    PanelWarnings: TPanel;
    ShellIcons: TImageList;
    LabelNoDesign: TLabel;
    ListWarnings: TListBox;
    MenuItemRename: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemSeparator78: TMenuItem;
    MenuItemReferenceOfCurrent: TMenuItem;
    MenuItemSeparator2303403o: TMenuItem;
    MenuItemRefreshDir: TMenuItem;
    MenuItemSeparator123123213: TMenuItem;
    MenuItemOpenDirFromFile: TMenuItem;
    MenuItemDeleteFile: TMenuItem;
    MenuItemOpenDefault: TMenuItem;
    MenuItemShellTreeOpenDir: TMenuItem;
    MenuItemPreferences: TMenuItem;
    N1: TMenuItem;
    MenuItemDuplicateComponent: TMenuItem;
    MenuItemPasteComponent: TMenuItem;
    MenuItemCopyComponent: TMenuItem;
    MenuItemSupport: TMenuItem;
    MenuItemSeparator788: TMenuItem;
    MenuItemRestartRebuildEditor: TMenuItem;
    MenuItemSeparator1300: TMenuItem;
    MenuItemSeparator170: TMenuItem;
    MenuItemDesignNewUserInterfaceCustomRoot: TMenuItem;
    MenuItemDesignNewTransformCustomRoot: TMenuItem;
    MenuItemDesignDeleteComponent: TMenuItem;
    MenuItemDesignAddTransform: TMenuItem;
    MenuItemDesignAddUserInterface: TMenuItem;
    MenuItemSeparator150: TMenuItem;
    MenuItemDesignClose: TMenuItem;
    MenuItemDesign: TMenuItem;
    OpenDesignDialog: TCastleOpenDialog;
    MenuItemOpenDesign: TMenuItem;
    MenuItemSeparator201: TMenuItem;
    MenuItemDesignNewTransform: TMenuItem;
    MenuItemDesignNewUserInterfaceRect: TMenuItem;
    ShellListPopupMenu: TPopupMenu;
    ShellTreePopupMenu: TPopupMenu;
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
    SplitterBetweenFiles: TSplitter;
    Splitter2: TSplitter;
    TabFiles: TTabSheet;
    TabOutput: TTabSheet;
    ProcessUpdateTimer: TTimer;
    TabWarnings: TTabSheet;
    procedure ActionNewSpriteSheetExecute(Sender: TObject);
    procedure ActionEditAssociatedUnitExecute(Sender: TObject);
    procedure ActionEditUnitExecute(Sender: TObject);
    procedure ActionNewUnitClassExecute(Sender: TObject);
    procedure ActionNewUnitEmptyExecute(Sender: TObject);
    procedure ActionNewUnitHereClassExecute(Sender: TObject);
    procedure ActionNewUnitHereEmptyExecute(Sender: TObject);
    procedure ActionNewUnitHereStateExecute(Sender: TObject);
    procedure ActionNewUnitStateExecute(Sender: TObject);
    procedure ActionOpenProjectCodeExecute(Sender: TObject);
    procedure ActionRegenerateProjectExecute(Sender: TObject);
    procedure ApplicationProperties1Activate(Sender: TObject);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure ButtonClearWarningsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListOutputClick(Sender: TObject);
    procedure MenuItemDesignNewNonVisualClick(Sender: TObject);
    procedure MenuItemNewDirectoryClick(Sender: TObject);
    procedure MenuItemRenameClick(Sender: TObject);
    procedure MenuItemShellTreeRefreshClick(Sender: TObject);
    procedure UpdateUndo(Sender: TObject);
    procedure UpdateRenameItem(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure MenuItemDeleteFileClick(Sender: TObject);
    procedure MenuItemOpenDefaultClick(Sender: TObject);
    procedure MenuItemOpenDirFromFileClick(Sender: TObject);
    procedure MenuItemPreferencesClick(Sender: TObject);
    procedure MenuItemAutoGenerateCleanClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemAutoGenerateTexturesClick(Sender: TObject);
    procedure MenuItemBreakProcessClick(Sender: TObject);
    procedure MenuItemCgeWwwClick(Sender: TObject);
    procedure MenuItemCleanClick(Sender: TObject);
    procedure MenuItemCompileClick(Sender: TObject);
    procedure MenuItemCompileRunClick(Sender: TObject);
    procedure MenuItemCopyComponentClick(Sender: TObject);
    procedure MenuItemDesignCloseClick(Sender: TObject);
    procedure MenuItemDesignDeleteComponentClick(Sender: TObject);
    procedure MenuItemDuplicateComponentClick(Sender: TObject);
    procedure MenuItemManualClick(Sender: TObject);
    procedure MenuItemModeDebugClick(Sender: TObject);
    procedure MenuItemDesignNewUserInterfaceRectClick(Sender: TObject);
    procedure MenuItemDesignNewTransformClick(Sender: TObject);
    procedure MenuItemOnlyRunClick(Sender: TObject);
    procedure MenuItemOpenDesignClick(Sender: TObject);
    procedure MenuItemPackageClick(Sender: TObject);
    procedure MenuItemPackageSourceClick(Sender: TObject);
    procedure MenuItemPasteComponentClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemReferenceClick(Sender: TObject);
    procedure MenuItemModeReleaseClick(Sender: TObject);
    procedure MenuItemReferenceOfCurrentClick(Sender: TObject);
    procedure MenuItemRefreshDirClick(Sender: TObject);
    procedure MenuItemRestartRebuildEditorClick(Sender: TObject);
    procedure MenuItemSaveAsDesignClick(Sender: TObject);
    procedure MenuItemSaveDesignClick(Sender: TObject);
    procedure MenuItemShellTreeOpenDirClick(Sender: TObject);
    procedure MenuItemSupportClick(Sender: TObject);
    procedure MenuItemSwitchProjectClick(Sender: TObject);
    procedure ProcessUpdateTimerTimer(Sender: TObject);
    procedure ShellListPopupMenuPopup(Sender: TObject);
  private
    type
      // Argument for RefreshFiles
      TRefreshFiles = (
        // only files (not directories) within current directory changed
        rfFilesInCurrentDir,
        // files or directories, but only within current directory (not outside) changed
        rfEverythingInCurrentDir,
        // everything potentially changed
        rfEverything
      );
    var
      Manifest: TCastleManifest;
      ProjectName: String;
      ProjectPath, ProjectPathUrl, ProjectStandaloneSource, ProjectLazarus: String;
      BuildMode: TBuildMode;
      OutputList: TOutputList;
      RunningProcess: TAsynchronousProcessQueue;
      Design: TDesignFrame;
      ShellListView1: TCastleShellListView;
      ShellTreeView1: TCastleShellTreeView;
      ViewFileFrame: TViewFileFrame;
      SplitterBetweenViewFile: TSplitter;
    procedure BuildToolCall(const Commands: array of String;
      const ExitOnSuccess: Boolean = false);
    procedure BuildToolCallFinished(Sender: TObject);
    procedure MenuItemAddComponentClick(Sender: TObject);
    procedure MenuItemDesignNewCustomRootClick(Sender: TObject);
    procedure OpenPascal(const FileName: String);
    procedure RefreshFiles(const RefreshNecessary: TRefreshFiles);
    (*Runs custom code editor.
      Use this only when CodeEditor = ceCustom.
      CustomCodeEditorCommand is the command to use (like CodeEditorCommand
      or CodeEditorCommandProject).
      PascalFileName will be used as ${PAS} macro value. *)
    procedure RunCustomCodeEditor(const CustomCodeEditorCommand: String;
      const PascalFileName: String);
    procedure SetEnabledCommandRun(const AEnabled: Boolean);
    procedure FreeProcess;
    procedure ShellListViewDoubleClick(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShowNewUnitForm(const AUnitType: TNewUnitType;
      const UnitOutputDirFromFileBrowser: Boolean);
    procedure UpdateFormCaption(Sender: TObject);
    { Propose saving the hierarchy.
      Returns should we continue (user did not cancel). }
    function ProposeSaveDesign: Boolean;
    { Call always when Design<>nil value changed. }
    procedure DesignExistenceChanged;
    { Create Design, if nil. }
    procedure NeedsDesignFrame;
    { Separated procedure to not duplicate code in various ways to create new
      Design }
    procedure NewDesign(const ComponentClass: TComponentClass;
      const ComponentOnCreate: TNotifyEvent);
    { Separated procedure to not duplicate code in various ways to open Design
      (files view, menu) }
    procedure OpenDesign(const DesignUrl: String);
    procedure WarningNotification(const Category, Message: string);
    { Clears all warnings and hides warnings tab }
    procedure ClearAllWarnings;
  public
    { Open a project, given an absolute path to CastleEngineManifest.xml }
    procedure OpenProject(const ManifestUrl: String);
  end;

var
  ProjectForm: TProjectForm;

implementation

{$R *.lfm}

uses TypInfo, LCLType,
  CastleXMLUtils, CastleLCLUtils, CastleOpenDocument, CastleURIUtils,
  CastleFilesUtils, CastleUtils, CastleVectors, CastleColors, CastleConfig,
  CastleScene, CastleViewport, Castle2DSceneManager, CastleCameras,
  CastleTransform, CastleControls, CastleDownload, CastleApplicationProperties,
  CastleLog, CastleComponentSerialize, CastleSceneCore, CastleStringUtils,
  CastleFonts, X3DLoad, CastleFileFilters, CastleImages, CastleSoundEngine,
  CastleClassUtils,
  FormAbout, FormChooseProject, FormPreferences, FormSpriteSheetEditor,
  ToolCompilerInfo, ToolCommonUtils;

procedure TProjectForm.MenuItemQuitClick(Sender: TObject);
begin
  if ProposeSaveDesign then
    Application.Terminate;
end;

procedure TProjectForm.MenuItemReferenceClick(Sender: TObject);
begin
  OpenURL(ApiReferenceUrl + 'index.html');
end;

procedure TProjectForm.MenuItemModeReleaseClick(Sender: TObject);
begin
  BuildMode := bmRelease;
  MenuItemModeRelease.Checked := true;
end;

procedure TProjectForm.MenuItemReferenceOfCurrentClick(Sender: TObject);
var
  Url: String;
begin
  Url := ApiReferenceUrl + 'index.html';
  if Design <> nil then
    Design.CurrentComponentApiUrl(Url);
  OpenURL(Url);
end;

procedure TProjectForm.MenuItemRefreshDirClick(Sender: TObject);
begin
  RefreshFiles(rfEverythingInCurrentDir);
end;

procedure TProjectForm.MenuItemRestartRebuildEditorClick(Sender: TObject);
begin
  BuildToolCall(['editor'], true);
end;

procedure TProjectForm.MenuItemSaveAsDesignClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise

  if Design.DesignRoot is TCastleUserInterface then
  begin
    SaveDesignDialog.DefaultExt := 'castle-user-interface';
    SaveDesignDialog.Filter := 'CGE User Interface Design (*.castle-user-interface)|*.castle-user-interface|All Files|*';
  end else
  if Design.DesignRoot is TCastleTransform then
  begin
    { We modify both Filter and DefaultExt, otherwise (at least on GTK2)
      the default extension (for filter like '*.castle-user-interface;*.castle-transform')
      would still be castle-user-interface. I.e. DefaultExt seems to be ignored,
      and instead GTK applies first filter. }
    SaveDesignDialog.DefaultExt := 'castle-transform';
    SaveDesignDialog.Filter := 'CGE Transform Design (*.castle-transform)|*.castle-transform|All Files|*';
  end else
  begin
    SaveDesignDialog.DefaultExt := 'castle-component';
    SaveDesignDialog.Filter := 'CGE Component Design (*.castle-component)|*.castle-component|All Files|*';
  end;

  SaveDesignDialog.Url := Design.DesignUrl;
  if SaveDesignDialog.Execute then
    Design.SaveDesign(SaveDesignDialog.Url);
    // TODO: save DesignUrl somewhere? CastleEditorSettings.xml?
end;

procedure TProjectForm.MenuItemSaveDesignClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise

  if Design.DesignUrl = '' then
    MenuItemSaveAsDesignClick(Sender)
  else
    Design.SaveDesign(Design.DesignUrl);
end;

procedure TProjectForm.MenuItemShellTreeOpenDirClick(Sender: TObject);
var
  Dir: String;
begin
  if ShellTreeView1.Selected <> nil then
  begin
    Dir := ShellTreeView1.GetPathFromNode(ShellTreeView1.Selected);
    OpenDocument(Dir);
  end;
end;

procedure TProjectForm.MenuItemSupportClick(Sender: TObject);
begin
  OpenURL('https://patreon.com/castleengine/');
end;

procedure TProjectForm.MenuItemCgeWwwClick(Sender: TObject);
begin
  OpenURL('https://castle-engine.io/');
end;

procedure TProjectForm.MenuItemAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
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
  if ProposeSaveDesign then
  begin
    { Close sprite sheet editor window if visible }
    if (SpriteSheetEditorForm <> nil) and (SpriteSheetEditorForm.Visible) then
    begin
      if not SpriteSheetEditorForm.CloseQuery then
      begin
        CanClose := false;
        Exit;
      end;
    end;

    Application.Terminate
  end else
    CanClose := false;
end;

procedure TProjectForm.ButtonClearWarningsClick(Sender: TObject);
begin
  ClearAllWarnings;
end;

procedure TProjectForm.ActionNewSpriteSheetExecute(Sender: TObject);
begin
  if SpriteSheetEditorForm = nil then
    SpriteSheetEditorForm := TSpriteSheetEditorForm.Create(Application);

  SpriteSheetEditorForm.Show;
  SpriteSheetEditorForm.NewSpriteSheet;
end;

procedure TProjectForm.ApplicationProperties1Activate(Sender: TObject);
begin
  { Refresh contents of selected dir, and tree of subdirectories,
    in case user created some files/directories in other applications. }
  RefreshFiles(rfEverything);
end;

procedure TProjectForm.ActionOpenProjectCodeExecute(Sender: TObject);
var
  Exe: String;
begin
  case CodeEditor of
    ceCustom:
      begin
        if CodeEditorCommandProject <> '' then
          RunCustomCodeEditor(CodeEditorCommandProject, '')
        else
        if ProjectStandaloneSource <> '' then
          RunCustomCodeEditor(CodeEditorCommand, ProjectStandaloneSource)
        else
          { We should not really pass directory name
            as PascalFileName to RunCustomCodeEditor,
            but in this case we kind of have no choice: we want to run custom editor
            with something. }
          RunCustomCodeEditor(CodeEditorCommand, ProjectPath);
      end;
    ceLazarus:
      begin
        Exe := FindExeLazarusIDE;

        { Open through LPI to change the project. }
        if ProjectLazarus <> '' then
          RunCommandNoWait(CreateTemporaryDir, Exe, [ProjectLazarus])
        else
          ErrorBox('Lazarus project not defined (neither "standalone_source" nor "lazarus_project" were specified in CastleEngineManifest.xml).' + NL +
            NL +
            'Create Lazarus project (e.g. by "castle-engine generate-program") and update CastleEngineManifest.xml.');
      end;
    else raise EInternalError.Create('CodeEditor?');
  end;
end;

procedure TProjectForm.ActionRegenerateProjectExecute(Sender: TObject);
begin
  BuildToolCall(['generate-program']);
end;

procedure TProjectForm.ActionEditUnitExecute(Sender: TObject);
begin
  if OpenPascalUnitDialog.Execute then
    OpenPascal(OpenPascalUnitDialog.FileName);
end;

procedure TProjectForm.ActionEditAssociatedUnitExecute(Sender: TObject);
var
  AUnitName, UnitFileNameAbsolute: String;
begin
  AUnitName := DeleteURIExt(ExtractURIName(Design.DesignUrl));
  UnitFileNameAbsolute := Manifest.SearchPascalUnit(AUnitName);
  if UnitFileNameAbsolute <> '' then
    OpenPascal(UnitFileNameAbsolute)
  else
    ErrorBox('Cannot find Pascal unit ' + AUnitName + ' (filename like "' + AUnitName + '.pas") among the search paths listed in CastleEngineManifest.xml');
end;

procedure TProjectForm.ActionNewUnitClassExecute(Sender: TObject);
begin
  ShowNewUnitForm(utClass, false);
end;

procedure TProjectForm.ActionNewUnitEmptyExecute(Sender: TObject);
begin
  ShowNewUnitForm(utEmpty, false);
end;

procedure TProjectForm.ActionNewUnitHereClassExecute(Sender: TObject);
begin
  ShowNewUnitForm(utClass, true);
end;

procedure TProjectForm.ActionNewUnitHereEmptyExecute(Sender: TObject);
begin
  ShowNewUnitForm(utEmpty, true);
end;

procedure TProjectForm.ActionNewUnitHereStateExecute(Sender: TObject);
begin
  ShowNewUnitForm(utState, true);
end;

procedure TProjectForm.ActionNewUnitStateExecute(Sender: TObject);
begin
  ShowNewUnitForm(utState, false);
end;

procedure TProjectForm.ShowNewUnitForm(const AUnitType: TNewUnitType;
  const UnitOutputDirFromFileBrowser: Boolean);

  { Check if unit is on search path, as we don't edit CastleEngineManifest.xml
    search paths automatically now. }
  procedure CheckNewUnitOnSearchPath;
  var
    UnitBaseName: String;
  begin
    if NewUnitForm.CreatedUnitRelative <> '' then
    begin
      UnitBaseName := DeleteFileExt(ExtractFileName(NewUnitForm.CreatedUnitRelative));
      if Manifest.SearchPascalUnit(UnitBaseName) = '' then
      begin
        WarningBox(Format('Created unit "%s" not found on the search path. Make sure that subdirectory "%s" is listed among search paths inside CastleEngineManifest.xml.', [
          NewUnitForm.CreatedUnitRelative,
          ExtractFilePath(NewUnitForm.CreatedUnitRelative)
        ]));
      end;
    end;
  end;

  { Propose to open design or unit code immediately. }
  procedure ProposeToOpenNewFile;
  begin
    if NewUnitForm.CreatedDesignRelative <> '' then
    begin
      Assert(NewUnitForm.CreatedUnitRelative <> '');
      if YesNoBox('Open design', Format(
        'Created unit: %s' + NL +
        NL +
        'Created user interface design: %s' + NL +
        NL +
        'Open the user interface design now?', [
        NewUnitForm.CreatedUnitRelative,
        NewUnitForm.CreatedDesignRelative
      ])) then
        OpenDesign(NewUnitForm.CreatedDesignAbsolute);
    end else
    if NewUnitForm.CreatedUnitRelative <> '' then
    begin
      if YesNoBox('Edit unit', Format(
        'Created unit: %s' + NL +
        NL +
        'Edit the unit code now?', [
        NewUnitForm.CreatedUnitRelative
      ])) then
        OpenPascal(NewUnitForm.CreatedUnitAbsolute);
    end;
  end;

var
  UnitOutputPath: String;
begin
  { calculate UnitOutputPath }
  if UnitOutputDirFromFileBrowser then
  begin
    UnitOutputPath := ShellListView1.Root;
  end else
  begin
    { place code in code/ subdirectory, if possible }
    UnitOutputPath := CombinePaths(Manifest.Path, 'code');
    if not DirectoryExists(UnitOutputPath) then
      UnitOutputPath := Manifest.Path;
  end;
  UnitOutputPath := InclPathDelim(UnitOutputPath);

  NewUnitForm.InitializeUi(AUnitType, UnitOutputPath, Manifest);

  if NewUnitForm.ShowModal = mrOK then
  begin
    CheckNewUnitOnSearchPath;
    ProposeToOpenNewFile;
    RefreshFiles(rfFilesInCurrentDir);
  end;
end;

procedure TProjectForm.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  { In case of CGE editor, exceptions are nothing alarming.

    We may raise exception during normal execution e.g.
    - when you try to do invalid file operation (creating dir with invalid name,
      deleting something to which you have no permission),
    - renaming component to invalid value,
    - in general doing anything that causes exception in FPC or CGE code.

    So display a normal dialog box about them, user-friendly
    (don't even show exception class for now),
    instead of the default LCL dialog that proposes to kill the application. }
  ErrorBox(E.Message);
end;

procedure TProjectForm.FormCreate(Sender: TObject);

  { We create some components by code, this way we don't have to put
    in package TCastleShellTreeView and TCastleShellListView,
    making compiling CGE editor a bit easier. }
  procedure CreateShellViews;
  const
    { Similar to paths removed by build-tool "clean", or excluded by default by
      build-tool "package". This should be configurable some day. }
    ExcludeMask = 'castle-engine-output;*~;*.bak;*.exe;*.dll';
  begin
    ShellTreeView1 := TCastleShellTreeView.Create(Self);
    ShellTreeView1.Parent := TabFiles;
    ShellTreeView1.Width := MulDiv(250, PixelsPerInch, 96);
    ShellTreeView1.Align := alLeft;
    ShellTreeView1.FileSortType := fstAlphabet;
    ShellTreeView1.HotTrack := True;
    ShellTreeView1.ReadOnly := True;
    ShellTreeView1.ShowRoot := False;
    ShellTreeView1.TabOrder := 0;
    ShellTreeView1.Options := [tvoAutoItemHeight, tvoHideSelection, tvoHotTrack, tvoKeepCollapsedNodes, tvoReadOnly, tvoShowButtons, tvoShowLines, tvoToolTips, tvoThemedDraw];
    ShellTreeView1.ObjectTypes := [otFolders];
    ShellTreeView1.ExcludeMask := ExcludeMask;
    ShellTreeView1.PopupMenu := ShellTreePopupMenu;

    ShellListView1 := TCastleShellListView.Create(Self);
    ShellListView1.Parent := TabFiles;
    ShellListView1.Align := alClient;
    ShellListView1.ReadOnly := True;
    ShellListView1.SortColumn := 0;
    ShellListView1.TabOrder := 1;
    ShellListView1.ObjectTypes := [otNonFolders, otFolders];
    { Without this, files are in undefined order
      (it seems SortColumn=0 above doesn't work). }
    ShellListView1.FileSortType := fstFoldersFirst;
    ShellListView1.ExcludeMask := ExcludeMask;
    ShellListView1.OnDblClick := @ShellListViewDoubleClick;
    ShellListView1.ShowHint := true;
    ShellListView1.RowSelect := true;
    ShellListView1.OnSelectItem := @ShellListViewSelectItem;
    ShellListView1.Hint := 'Double-click to open.' + NL +
      NL +
      '- Scenes open in engine viewer (view3dscene).' + NL +
      '- Images open in engine viewer (castle-view-image).' + NL +
      '- Design opens in this editor window.' + NL +
      '- Pascal files open in Lazarus.' + NL +
      '- Other files open in external applications.';
    ShellListView1.PopupMenu := ShellListPopupMenu;
    ShellListView1.SmallImages := ShellIcons;
    ShellListView1.DragMode := dmAutomatic;

    ShellTreeView1.ShellListView := ShellListView1;
    ShellListView1.ShellTreeView := ShellTreeView1;
  end;

begin
  OutputList := TOutputList.Create(ListOutput);
  BuildComponentsMenu(
    MenuItemDesignNewUserInterfaceCustomRoot,
    MenuItemDesignNewTransformCustomRoot,
    nil,
    MenuItemDesignNewNonVisualCustomRoot,
    @MenuItemDesignNewCustomRootClick);
  BuildComponentsMenu(
    MenuItemDesignAddUserInterface,
    MenuItemDesignAddTransform,
    MenuItemDesignAddBehavior,
    MenuItemDesignAddNonVisual,
    @MenuItemAddComponentClick);
  CreateShellViews;
  ApplicationProperties.OnWarning.Add(@WarningNotification);
end;

procedure TProjectForm.FormDestroy(Sender: TObject);
begin
  FormHide(Self); //to save config properly
  ApplicationProperties.OnWarning.Remove(@WarningNotification);
  ApplicationDataOverride := '';
  FreeProcess;
  FreeAndNil(OutputList);
  FreeAndNil(Manifest);
end;

procedure TProjectForm.FormHide(Sender: TObject);

  function WindowStateToStr(const AWindowState: TWindowState): String;
  begin
    case AWindowState of
      wsNormal: Result := 'wsNormal';
      wsMinimized: Result := 'wsMinimized';
      wsMaximized: Result := 'wsMaximized';
      wsFullScreen: Result := 'wsFullScreen';
    end;
  end;

begin
  UserConfig.SetValue('ProjectForm_Saved', true);
  UserConfig.SetValue('ProjectForm_Width', Width);
  UserConfig.SetValue('ProjectForm_Height', Height);
  UserConfig.SetValue('ProjectForm_Left', Left);
  UserConfig.SetValue('ProjectForm_Top', Top);
  UserConfig.SetValue('ProjectForm_WindowState', WindowStateToStr(WindowState));
  UserConfig.SetValue('ProjectForm_PageControlBottom.Height', PageControlBottom.Height);
  if Design <> nil then
  begin
    UserConfig.SetValue('ProjectForm_DesignSaved', true);
    UserConfig.SetValue('ProjectForm_Design.PanelRight.Width', Design.PanelRight.Width);
    UserConfig.SetValue('ProjectForm_Design.PanelLeft.Width', Design.PanelLeft.Width);
  end;
  UserConfig.Save;
end;

procedure TProjectForm.FormShow(Sender: TObject);

  function StrToWindowState(const AWindowStateStr: String): TWindowState;
  begin
    case AWindowStateStr of
      'wsNormal': Result := wsNormal;
      'wsMaximized': Result := wsMaximized;
      else
        Result := wsNormal; //treat wsMinimized and any other value as wsNormal
    end;
  end;

var
  NewWidth, NewHeight, NewLeft, NewTop, NewControlHeight: Integer;
begin
  if UserConfig.GetValue('ProjectForm_Saved', false) then
  begin
    NewWidth := UserConfig.GetValue('ProjectForm_Width', -MaxInt);
    NewHeight := UserConfig.GetValue('ProjectForm_Height', -MaxInt);
    NewLeft := UserConfig.GetValue('ProjectForm_Left', -MaxInt);
    NewTop := UserConfig.GetValue('ProjectForm_Top', -MaxInt);
    NewControlHeight := UserConfig.GetValue('ProjectForm_PageControlBottom.Height', -MaxInt);
    WindowState := StrToWindowState(UserConfig.GetValue('ProjectForm_WindowState', 'wsNormal'));
    if (NewWidth + NewLeft <= Screen.Width + 32) and (NewWidth > 128) and
       (NewHeight + NewTop <= Screen.Height + 32) and (NewHeight > 128) and
       (NewControlHeight < NewHeight) and (NewControlHeight >= 0) and
       (NewLeft > -32) and (NewTop > -32) then
    begin
      Width := NewWidth;
      Height := NewHeight;
      Left := NewLeft;
      Top := NewTop;
      PageControlBottom.Height := NewControlHeight;
    end;
  end;
end;

procedure TProjectForm.ListOutputClick(Sender: TObject);
begin
  // TODO: just to source code line in case of error message here
end;

procedure TProjectForm.MenuItemDesignNewNonVisualClick(Sender: TObject);
begin
  if ProposeSaveDesign then
    NewDesign(TCastleComponent, nil);
end;

procedure TProjectForm.MenuItemNewDirectoryClick(Sender: TObject);
var
  NewDir, FullNewDir: String;
begin
  if InputQuery('New Directory', 'Create a new directory:', NewDir) then
  begin
    FullNewDir := InclPathDelim(ShellListView1.Root) + NewDir;
    if not CreateDir(FullNewDir) then
      raise Exception.CreateFmt('Creating new directory "%s" failed', [FullNewDir]);
    RefreshFiles(rfEverythingInCurrentDir);
    // TODO: Select newly added dir, not so easy in ShellListView1
  end;
end;

procedure TProjectForm.MenuItemRenameClick(Sender: TObject);
begin
  Design.RenameSelectedItem;
end;

procedure TProjectForm.MenuItemShellTreeRefreshClick(Sender: TObject);
begin
  RefreshFiles(rfEverything);
end;

procedure TProjectForm.UpdateRenameItem(Sender: TObject);
begin
  if (Design <> nil) and Design.RenamePossible then
    MenuItemRename.Enabled := true
  else
    MenuItemRename.Enabled := false;
end;

procedure TProjectForm.UpdateUndo(Sender: TObject);
begin
  if Design <> nil then
  begin
    MenuItemUndo.Enabled := Design.UndoSystem.IsUndoPossible;
    MenuItemUndo.Caption := Design.UndoSystem.UndoComment;
    MenuItemRedo.Enabled := Design.UndoSystem.IsRedoPossible;
    MenuItemRedo.Caption := Design.UndoSystem.RedoComment;
  end else
  begin
    MenuItemUndo.Enabled := false;
    MenuItemRedo.Enabled := false;
  end;
end;

procedure TProjectForm.MenuItemRedoClick(Sender: TObject);
begin
  Design.PerformRedo;
end;

procedure TProjectForm.MenuItemUndoClick(Sender: TObject);
begin
  Design.PerformUndo;
end;

procedure TProjectForm.MenuItemDeleteFileClick(Sender: TObject);
var
  SelectedFileName: String;
begin
  if ShellListView1.Selected <> nil then
  begin
    SelectedFileName := ShellListView1.GetPathFromItem(ShellListView1.Selected);
    if DirectoryExists(SelectedFileName) then
    begin
      if MessageDlg('Delete Directory', 'Delete directory "' + SelectedFileName + '"?',
        mtConfirmation, mbYesNo, 0) = mrYes then
      begin
        RemoveNonEmptyDir(SelectedFileName);
        RefreshFiles(rfEverythingInCurrentDir);
      end;
    end else
    begin
      if MessageDlg('Delete File', 'Delete file "' + SelectedFileName + '"?',
        mtConfirmation, mbYesNo, 0) = mrYes then
      begin
        CheckDeleteFile(SelectedFileName);
        RefreshFiles(rfFilesInCurrentDir);
      end;
    end;
  end;
end;

procedure TProjectForm.MenuItemOpenDefaultClick(Sender: TObject);
begin
  ShellListViewDoubleClick(ShellListView1);
end;

procedure TProjectForm.MenuItemOpenDirFromFileClick(Sender: TObject);
begin
  OpenDocument(ShellListView1.Root);
end;

procedure TProjectForm.MenuItemPreferencesClick(Sender: TObject);
begin
  PreferencesForm.ShowModal;
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
  begin
    RunningApplication := true;
    SoundEngineSetVolume;
    BuildToolCall(['compile', 'run']);
  end;
end;

procedure TProjectForm.MenuItemCopyComponentClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.CopyComponent;
end;

procedure TProjectForm.MenuItemDesignCloseClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise

  if ProposeSaveDesign then
  begin
    UserConfig.SetValue('ProjectForm_Design.PanelRight.Width', Design.PanelRight.Width);
    UserConfig.SetValue('ProjectForm_Design.PanelLeft.Width', Design.PanelLeft.Width);
    UserConfig.SetValue('ProjectForm_DesignSaved', true);
    UserConfig.Save;
    FreeAndNil(Design);
    DesignExistenceChanged;
  end;
end;

procedure TProjectForm.MenuItemDesignDeleteComponentClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.DeleteComponent;
end;

procedure TProjectForm.MenuItemDuplicateComponentClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.DuplicateComponent;
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

procedure TProjectForm.DesignExistenceChanged;
var
  NewPanelRightWidth, NewPanelLeftWidth: Integer;
begin
  MenuItemSaveAsDesign.Enabled := Design <> nil;
  MenuItemSaveDesign.Enabled := Design <> nil;
  MenuItemDesignClose.Enabled := Design <> nil;
  MenuItemDesignAddTransform.Enabled := Design <> nil;
  MenuItemDesignAddUserInterface.Enabled := Design <> nil;
  MenuItemDesignDeleteComponent.Enabled := Design <> nil;
  MenuItemCopyComponent.Enabled := Design <> nil;
  MenuItemPasteComponent.Enabled := Design <> nil;
  MenuItemDuplicateComponent.Enabled := Design <> nil;
  ActionEditAssociatedUnit.Enabled := Design <> nil;

  UpdateUndo(nil);
  UpdateRenameItem(nil);
  UpdateFormCaption(nil);

  if (Design <> nil) and UserConfig.GetValue('ProjectForm_DesignSaved', false) then
  begin
    NewPanelRightWidth := UserConfig.GetValue('ProjectForm_Design.PanelRight.Width', -MaxInt);
    NewPanelLeftWidth := UserConfig.GetValue('ProjectForm_Design.PanelLeft.Width', -MaxInt);
    if (NewPanelRightWidth >= 0) and (NewPanelLeftWidth >= 0) and
      (NewPanelRightWidth + NewPanelLeftWidth <= Width) then
    begin
      Design.PanelRight.Width := NewPanelRightWidth;
      Design.PanelLeft.Width := NewPanelLeftWidth;
    end;
  end;

  LabelNoDesign.Visible := Design = nil;
end;

procedure TProjectForm.NeedsDesignFrame;
begin
  if Design = nil then
  begin
    Design := TDesignFrame.Create(Self);
    Design.Parent := PanelAboveTabs;
    Design.Align := alClient;
    Design.OnUpdateFormCaption := @UpdateFormCaption;
    Design.UndoSystem.OnUpdateUndo := @UpdateUndo;
    Design.OnSelectionChanged := @UpdateRenameItem;
    DesignExistenceChanged;
  end;
end;

procedure TProjectForm.NewDesign(const ComponentClass: TComponentClass;
  const ComponentOnCreate: TNotifyEvent);
begin
  NeedsDesignFrame;
  ClearAllWarnings;
  Design.NewDesign(ComponentClass, ComponentOnCreate);
end;

procedure TProjectForm.OpenDesign(const DesignUrl: String);
begin
  NeedsDesignFrame;
  ClearAllWarnings;
  try
    Design.OpenDesign(DesignUrl);
  except
    { In case Design.OpenDesign raised exception (e.g. file unreadable or invalid),
      do not leave Design in half-uninitialized state (e.g. treeview will not
      show anything valid). }
    FreeAndNil(Design);
    DesignExistenceChanged;
    raise;
  end;
end;

procedure TProjectForm.WarningNotification(const Category,
  Message: string);
begin
  if Category <> '' then
    ListWarnings.Items.Add(Category + ': ' + Message)
  else
    ListWarnings.Items.Add(Message);
  TabWarnings.Caption := 'Warnings (' + IntToStr(ListWarnings.Count) + ')';
  TabWarnings.TabVisible := true;
end;

procedure TProjectForm.ClearAllWarnings;
begin
  ListWarnings.Clear;
  TabWarnings.TabVisible := false;
end;

procedure TProjectForm.MenuItemDesignNewUserInterfaceRectClick(Sender: TObject);
begin
  if ProposeSaveDesign then
    NewDesign(TCastleUserInterface, nil);
end;

procedure TProjectForm.MenuItemDesignNewTransformClick(Sender: TObject);
begin
  if ProposeSaveDesign then
    NewDesign(TCastleTransform, nil);
end;

procedure TProjectForm.MenuItemOnlyRunClick(Sender: TObject);
begin
  if ProposeSaveDesign then
  begin
    RunningApplication := true;
    SoundEngineSetVolume;
    BuildToolCall(['run']);
  end;
end;

procedure TProjectForm.MenuItemOpenDesignClick(Sender: TObject);
begin
  if ProposeSaveDesign then
  begin
    if Design <> nil then
      OpenDesignDialog.Url := Design.DesignUrl;
    if OpenDesignDialog.Execute then
    begin
      OpenDesign(OpenDesignDialog.Url);
    end;
  end;
end;

procedure TProjectForm.MenuItemPackageClick(Sender: TObject);
begin
  BuildToolCall(['package']);
end;

procedure TProjectForm.MenuItemPackageSourceClick(Sender: TObject);
begin
  BuildToolCall(['package-source']);
end;

procedure TProjectForm.MenuItemPasteComponentClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.PasteComponent;
end;

procedure TProjectForm.MenuItemSwitchProjectClick(Sender: TObject);
begin
  if ProposeSaveDesign then
  begin
    { Close sprite sheet editor window if visible }
    if (SpriteSheetEditorForm <> nil) and (SpriteSheetEditorForm.Visible) then
    begin
      if not SpriteSheetEditorForm.CloseQuery then
        Exit;
      SpriteSheetEditorForm.Close; // not needed on GTK2, maybe add ifdef?
    end;

    Free; // do not call MenuItemDesignClose, to avoid OnCloseQuery
    ChooseProjectForm.Show;
  end;
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

procedure TProjectForm.ShellListPopupMenuPopup(Sender: TObject);
begin
  MenuItemOpenDefault.Enabled := ShellListView1.Selected <> nil;
  MenuItemDeleteFile.Enabled := ShellListView1.Selected <> nil;
end;

procedure TProjectForm.FreeProcess;
begin
  FreeAndNil(RunningProcess);
  SetEnabledCommandRun(true);
  ProcessUpdateTimer.Enabled := false;
end;

procedure TProjectForm.ShellListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

  { Make sure ViewFileFrame is created and visible.
    For now we create ViewFileFrame on-demand, just in case there's a problem
    with initializing 2nd OpenGL context on some computer. }
  procedure NeedsViewFile;
  begin
    if ViewFileFrame = nil then
    begin
      ViewFileFrame := TViewFileFrame.Create(Self);
      ViewFileFrame.Parent := TabFiles;
      ViewFileFrame.Align := alRight;

      SplitterBetweenViewFile := TSplitter.Create(Self);
      SplitterBetweenViewFile.Parent := TabFiles;
      SplitterBetweenViewFile.Align := alRight;
    end;
    ViewFileFrame.Enabled := true;
    ViewFileFrame.Visible := true;
    SplitterBetweenViewFile.Enabled := true;
    SplitterBetweenViewFile.Visible := true;
  end;

var
  SelectedFileName, SelectedURL: String;
begin
  if ShellListView1.Selected <> nil then
  begin
    SelectedFileName := ShellListView1.GetPathFromItem(ShellListView1.Selected);
    SelectedURL := FilenameToURISafe(SelectedFileName);

    { Check for images first because TCastleScene can now load images. }
    if LoadImage_FileFilters.Matches(SelectedURL) then
    begin
      NeedsViewFile;
      ViewFileFrame.LoadImage(SelectedURL);
      Exit;
    end;

    if TFileFilterList.Matches(LoadScene_FileFilters, SelectedURL) then
    begin
      NeedsViewFile;
      ViewFileFrame.LoadScene(SelectedURL);
      Exit;
    end;

    if TFileFilterList.Matches(LoadSound_FileFilters, SelectedURL) then
    begin
      NeedsViewFile;
      ViewFileFrame.LoadSound(SelectedURL);
      Exit;
    end;
  end;

  { if control reached here, hide ViewFileFrame if needed }
  if ViewFileFrame <> nil then
  begin
    ViewFileFrame.ClearLoaded; // stops playing preview sound
    ViewFileFrame.Enabled := false;
    ViewFileFrame.Visible := false;
    SplitterBetweenViewFile.Enabled := false;
    SplitterBetweenViewFile.Visible := false;
  end;
end;

procedure TProjectForm.RunCustomCodeEditor(
  const CustomCodeEditorCommand: String;
  const PascalFileName: String);

  { Copied from FPC packages/fcl-process/src/processbody.inc
    (licence "LGPL with static linking exception", so compatible with us). }
  procedure CommandToList(S : String; List : TStringList);

    Function GetNextWord : String;

    Const
      WhiteSpace = [' ',#9,#10,#13];
      Literals = ['"',''''];

    Var
      Wstart,wend : Integer;
      InLiteral : Boolean;
      LastLiteral : Char;

    begin
      WStart:=1;
      While (WStart<=Length(S)) and charinset(S[WStart],WhiteSpace) do
        Inc(WStart);
      WEnd:=WStart;
      InLiteral:=False;
      LastLiteral:=#0;
      While (Wend<=Length(S)) and (Not charinset(S[Wend],WhiteSpace) or InLiteral) do
        begin
        if charinset(S[Wend],Literals) then
          If InLiteral then
            InLiteral:=Not (S[Wend]=LastLiteral)
          else
            begin
            InLiteral:=True;
            LastLiteral:=S[Wend];
            end;
         inc(wend);
         end;

       Result:=Copy(S,WStart,WEnd-WStart);

       if  (Length(Result) > 0)
       and (Result[1] = Result[Length(Result)]) // if 1st char = last char and..
       and (Result[1] in Literals) then // it's one of the literals, then
         Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)

       While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
         inc(Wend);
       Delete(S,1,WEnd-1);

    end;

  Var
    W : String;

  begin
    While Length(S)>0 do
      begin
      W:=GetNextWord;
      If (W<>'') then
        List.Add(W);
      end;
  end;
  { End of copy from FPC. }

var
  Exe: String;
  Parameters: TCastleStringList;
  I: Integer;
begin
  Parameters := TCastleStringList.Create;
  try
    CommandToList(CustomCodeEditorCommand, Parameters);
    if Parameters.Count = 0 then
      raise Exception.CreateFmt('Code editor command was split into zero items: "%s"', [CustomCodeEditorCommand]);
    Exe := Parameters[0];
    Parameters.Delete(0);
    for I := 0 to Parameters.Count - 1 do
      Parameters[I] := SReplacePatterns(Parameters[I],
        ['${PAS}', '${STANDALONE_SOURCE}', '${PROJECT_DIR}'],
        [PascalFileName, ProjectStandaloneSource, ProjectPath],
        true);
    RunCommandNoWait(CreateTemporaryDir, Exe, Parameters.ToArray);
  finally FreeAndNil(Parameters) end;
end;

procedure TProjectForm.OpenPascal(const FileName: String);
var
  Exe: String;
begin
  case CodeEditor of
    ceCustom:
      begin
        RunCustomCodeEditor(CodeEditorCommand, FileName);
      end;
    ceLazarus:
      begin
        Exe := FindExeLazarusIDE;

        { It would be cleaner to use LPI file, like this:

            if ProjectLazarus <> '' then
              // pass both project name, and particular filename, to open file within this project.
              RunCommandNoWait(CreateTemporaryDir, Exe, [ProjectLazarus, FileName])
            else
            begin
              WritelnWarning('Lazarus project not defined (neither "standalone_source" nor "lazarus_project" were specified in CastleEngineManifest.xml), the file will be opened without changing Lazarus project.');
              RunCommandNoWait(CreateTemporaryDir, Exe, [FileName]);
            end;

          But it doesn't work: Lazarus opens LPI as a regular XML file then,
          without changing the project.
          There seems to be no solution:
          - using LPR doesn't change the project either
          - using *only* LPI asks to change the project, even if it's already the current project
            (so we cannot fix the problem by executing it twice in a row, once with LPI once with PAS
            -- it would show dialog box every time)
        }

        // if ProjectStandaloneSource = '' then // see comments below, we use ProjectStandaloneSource
        //   WritelnWarning('Lazarus project not defined ("standalone_source" was not specified in CastleEngineManifest.xml), the file will be opened without changing Lazarus project.');

        RunCommandNoWait(CreateTemporaryDir, Exe, [FileName]);
      end;
    else raise EInternalError.Create('CodeEditor?');
  end;
end;

procedure TProjectForm.ShellListViewDoubleClick(Sender: TObject);

  procedure OpenWithCastleTool(const ToolName: String;
    const SelectedURL: String;
    const Arguments: array of String);
  var
    Exe: String;
  begin
    Exe := FindExeCastleTool(ToolName);
    if Exe = '' then
    begin
      ErrorBox(Format('Cannot find Castle Game Engine tool "%s", opening "%s" failed. Make sure CGE is installed correctly, the tool should be distributed along with engine binary.',
        [ToolName, SelectedURL]));
      Exit;
    end;

    RunCommandNoWait(CreateTemporaryDir, Exe, Arguments);
  end;

  procedure OpenLazarusProject(const FileName: String);
  var
    Exe: String;
  begin
    Exe := FindExeLazarusIDE;
    RunCommandNoWait(CreateTemporaryDir, Exe, [FileName]);
  end;

var
  SelectedFileName, Ext, SelectedURL: String;
begin
  if ShellListView1.Selected <> nil then
  begin
    SelectedFileName := ShellListView1.GetPathFromItem(ShellListView1.Selected);
    if DirectoryExists(SelectedFileName) then
    begin
      ShellTreeView1.Path := SelectedFileName;
      Exit;
    end;

    SelectedURL := FilenameToURISafe(SelectedFileName);
    Ext := ExtractFileExt(SelectedFileName);

    { Check for images first because TCastleScene can now load images. }
    if LoadImage_FileFilters.Matches(SelectedURL) then
    begin
      OpenWithCastleTool('castle-view-image', SelectedURL, [SelectedURL]);
      Exit;
    end;

    { Check for Castle Sprite Sheets, to open them in sprite editor }
    if (URIMimeType(SelectedURL) = 'application/x-castle-sprite-sheet') then
    begin
      if SpriteSheetEditorForm = nil then
        SpriteSheetEditorForm := TSpriteSheetEditorForm.Create(Application);
      SpriteSheetEditorForm.Show;
      SpriteSheetEditorForm.OpenSpriteSheet(SelectedURL, true);
      Exit;
    end;

    if TFileFilterList.Matches(LoadScene_FileFilters, SelectedURL) then
    begin
      OpenWithCastleTool('view3dscene', SelectedURL,
        ['--project', ProjectPathUrl, SelectedURL]);
      Exit;
    end;

    if AnsiSameText(Ext, '.castle-user-interface') or
       AnsiSameText(Ext, '.castle-transform') or
       AnsiSameText(Ext, '.castle-component') then
    begin
      if ProposeSaveDesign then
        OpenDesign(SelectedURL);
      Exit;
    end;

    if AnsiSameText(Ext, '.pas') or
       AnsiSameText(Ext, '.inc') or
       AnsiSameText(Ext, '.pp') or
       AnsiSameText(Ext, '.lpr') or
       AnsiSameText(Ext, '.dpr') then
    begin
      OpenPascal(SelectedFileName);
      Exit;
    end;

    if AnsiSameText(Ext, '.lpi') then
    begin
      OpenLazarusProject(SelectedFileName);
      Exit;
    end;

    if not OpenDocument(SelectedFileName) then
      ErrorBox(Format('Opening "%s" failed.', [SelectedFileName]));
  end;
end;

procedure TProjectForm.BuildToolCall(const Commands: array of String;
  const ExitOnSuccess: Boolean);
var
  BuildToolExe, ModeString, Command: String;
  QueueItem: TAsynchronousProcessQueue.TQueueItem;
begin
  if RunningProcess <> nil then
    raise EInternalError.Create('It should not be possible to call this when RunningProcess <> nil');

  BuildToolExe := FindExeCastleTool('castle-engine');
  if BuildToolExe = '' then
  begin
    ErrorBox('Cannot find build tool (castle-engine) on $PATH environment variable.');
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

  if ExitOnSuccess then
    RunningProcess.OnSuccessfullyFinishedAll := @MenuItemQuitClick
  else
    RunningProcess.OnFinished := @BuildToolCallFinished;

  RunningProcess.Start;
end;

procedure TProjectForm.BuildToolCallFinished(Sender: TObject);
begin
  // bring back volume, in case MuteOnRun
  RunningApplication := false;
  SoundEngineSetVolume;
end;

procedure TProjectForm.MenuItemAddComponentClick(Sender: TObject);
var
  R: TRegisteredComponent;
begin
  R := TRegisteredComponent(Pointer((Sender as TComponent).Tag));
  Design.AddComponent(R.ComponentClass, R.OnCreate);
end;

procedure TProjectForm.MenuItemDesignNewCustomRootClick(Sender: TObject);
var
  R: TRegisteredComponent;
begin
  if ProposeSaveDesign then
  begin
    R := TRegisteredComponent(Pointer((Sender as TComponent).Tag));
    NewDesign(R.ComponentClass, R.OnCreate);
  end;
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
  MenuItemRestartRebuildEditor.Enabled := AEnabled;
  MenuItemBreakProcess.Enabled := not AEnabled;
end;

procedure TProjectForm.UpdateFormCaption(Sender: TObject);
var
  S: String;
begin
  if Design <> nil then
    S := Design.FormCaption
  else
    S := '';
  S := S + SQuoteLCLCaption(ProjectName);
  if InternalHasCustomComponents then
    S := S + ' (With Custom Components)';
  Caption := S + ' | Castle Game Engine';
end;

function TProjectForm.ProposeSaveDesign: Boolean;
var
  Mr: TModalResult;
  DesignName: String;
begin
  Result := true;

  if Design <> nil then
  begin
    Design.BeforeProposeSaveDesign;
    if Design.DesignModified then
    begin
      if Design.DesignUrl <> '' then
        DesignName := '"' + Design.DesignUrl + '"'
      else
        DesignName := '<unnnamed>';
      Mr := MessageDlg('Save Design',
        'Design ' + DesignName + ' was modified but not saved yet. Save it now?',
        mtConfirmation, mbYesNoCancel, 0);
      case Mr of
        mrYes: MenuItemSaveDesign.Click;
        mrCancel: Result := false;
      end;
    end;
  end;
end;

procedure TProjectForm.OpenProject(const ManifestUrl: String);
begin
  { Below we assume ManifestUrl contains an absolute path,
    otherwise ProjectPathUrl could be '',
    and OpenDesignDialog.InitialDir would be left '' and so on. }

  FreeAndNil(Manifest); // free previous manifest, if any
  Manifest := TCastleManifest.CreateFromUrl(ManifestUrl);

  ProjectName := Manifest.Name;
  ProjectPath := Manifest.Path;
  ProjectPathUrl := Manifest.PathUrl;
  ProjectStandaloneSource := Manifest.StandaloneSource;
  ProjectLazarus := Manifest.LazarusProject;
  if (Manifest.EditorUnits <> '') and
     (not InternalHasCustomComponents) then
    WritelnWarning('Project uses custom components (declares editor_units in CastleEngineManifest.xml), but this is not a custom editor build.' + NL + 'Use the menu item "Project -> Restart Editor (With Custom Components)" to build and run correct editor.');

  { Make some fields absolute paths, or empty }
  if ProjectStandaloneSource <> '' then
    ProjectStandaloneSource := CombinePaths(ProjectPath, ProjectStandaloneSource);
  if ProjectLazarus <> '' then
    ProjectLazarus := CombinePaths(ProjectPath, ProjectLazarus);

  { override ApplicationData interpretation, and castle-data:/xxx URL,
    while this project is open. }
  ApplicationDataOverride := CombineURI(ProjectPathUrl, 'data/');

  { adjust some InitialDir values to make open dialogs natural }
  OpenDesignDialog.InitialDir := URIToFilenameSafe(ApplicationDataOverride);
  SaveDesignDialog.InitialDir := URIToFilenameSafe(ApplicationDataOverride);
  OpenPascalUnitDialog.InitialDir := ProjectPath;

  ShellTreeView1.Root := ProjectPath;

  // It's too easy to change it visually and forget, so we set it from code
  PageControlBottom.ActivePage := TabFiles;
  SetEnabledCommandRun(true);

  BuildMode := bmDebug;
  MenuItemModeDebug.Checked := true;

  DesignExistenceChanged;
  UpdateFormCaption(nil); // make form Caption reflect project name (although this is now done also by DesignExistenceChanged)
end;

procedure TProjectForm.RefreshFiles(const RefreshNecessary: TRefreshFiles);
var
  TreeViewPath: String;
begin
  ShellListView1.RefreshContents;

  { It is important to refresh ShellTreeView1 e.g. when new directory
    was added, otherwise user could not navigate
    into newly created directories, since they must exist in ShellTreeView1. }

  case RefreshNecessary of
    { Unfortunately this special implementation of "refresh current dir"
      doesn't work for non-root directories. }
    //rfEverythingInCurrentDir:
    //  ShellTreeView1.Refresh(ShellTreeView1.Selected);
    rfEverythingInCurrentDir, rfEverything:
      begin
        TreeViewPath := ShellTreeView1.Path;
        ShellTreeView1.Refresh(nil);
        ShellTreeView1.Path := TreeViewPath;
      end;
  end;
end;

initialization
  // initialize CGE log
  ApplicationProperties.ApplicationName := 'castle-editor';
  // Useful for testing of custom editor run by "Restart Editor", to see the log easily on Unix
  // LogFileName := FileNameAutoInc('/tmp/castle-editor-%d.log');
  InitializeLog;
end.
