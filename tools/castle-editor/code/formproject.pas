{
  Copyright 2018-2023 Michalis Kamburelis.

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

{ Hack to use OpenDocument instead of RunCommandNoWait to execute Delphi.
  This assumes that Delphi is associated on your system with Pascal files.
  OTOH it will work a bit nicer, not opening new Delphi instance each time,
  as Windows underneath will use DDE to communicate with Delphi BDSLauncher. }
{$define DELPHI_OPEN_SHELL}

interface

uses
  Classes, SysUtils, DOM, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, CastleShellCtrls, StdCtrls, ValEdit, ActnList, Buttons,
  AnchorDocking, XMLPropStorage, ImgList,
  ProjectUtils, Types, Contnrs, CastleControl, CastleUIControls,
  CastlePropEdits, CastleDialogs, X3DNodes, CastleFindFiles,
  DataModuleIcons,
  EditorUtils, FrameDesign, FrameViewFile, FormNewUnit, ToolManifest,
  ToolPackageFormat;

const
  DockLayoutFileName = 'layout.dock-layout';
  DockLayoutFileNameDefault = 'default.dock-layout';

type
  { Main project management. }
  TProjectForm = class(TForm)
    ActionShowStatistics: TAction;
    ActionRunParameterCapabilitiesForceFixedFunction: TAction;
    ActionRunParameterCapabilitiesForceModern: TAction;
    ActionRunParameterCapabilitiesDefault: TAction;
    ActionRunParameterDefaultWindowOrFullscreen: TAction;
    ActionRunParameterRequestWindow: TAction;
    ActionRunParameterRequestFullScreen: TAction;
    ActionRunParameterDisableFpsLimit: TAction;
    ActionRunParameterDisableSound: TAction;
    ActionPlayStop: TAction;
    ActionShowColliders: TAction;
    ActionSimulationPlayStop: TAction;
    ActionSimulationPauseUnpause: TAction;
    ActionViewportRenderNext: TAction;
    ActionViewportRenderSolidWireframe: TAction;
    ActionPhysicsHideAllJointsTools: TAction;
    ActionPhysicsShowAllJointsTools: TAction;
    ActionModeSelect: TAction;
    ActionModeTranslate: TAction;
    ActionModeRotate: TAction;
    ActionModeScale: TAction;
    ActionModeInteract: TAction;
    ActionFocusDesign: TAction;
    ActionWarningsCopyAll: TAction;
    ActionWarningsCopySelected: TAction;
    ActionWarningsClean: TAction;
    ActionViewportGridAxis: TAction;
    ActionViewportRenderWireframeOnly: TAction;
    ActionViewportRenderNormal: TAction;
    ActionComponentDuplicate: TAction;
    ActionComponentSaveSelected: TAction;
    ActionComponentDelete: TAction;
    ActionComponentCut: TAction;
    ActionComponentPaste: TAction;
    ActionComponentCopy: TAction;
    ActionViewportToggleProjection: TAction;
    ActionViewportSetup2D: TAction;
    ActionViewportSort2D: TAction;
    ActionViewportAlignCameraToView: TAction;
    ActionViewportTop: TAction;
    ActionNavigationToggle2D: TAction;
    ActionViewportAlignViewToCamera: TAction;
    ActionViewportBottom: TAction;
    ActionViewportFront: TAction;
    ActionViewportBack: TAction;
    ActionViewportRight: TAction;
    ActionViewportLeft: TAction;
    ActionNavigationFly: TAction;
    ActionNavigationExamine: TAction;
    ActionNavigation2D: TAction;
    ActionViewportViewSelected: TAction;
    ActionViewportViewAll: TAction;
    ActionSystemInformation: TAction;
    ActionOutputCopyAll: TAction;
    ActionOutputCopySelected: TAction;
    ActionOutputClean: TAction;
    ActionNewSpriteSheet: TAction;
    ActionList: TActionList;
    BitBtnPlayStop: TBitBtn;
    BitBtnNewView: TBitBtn;
    LabelOpenExistingView: TLabel;
    ListOpenExistingView: TListView;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem2888888: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    Separator12: TMenuItem;
    MenuItemRunParameterDefaultWindowOrFullscreen: TMenuItem;
    Separator11: TMenuItem;
    MenuItemRunParameterRequestWindow: TMenuItem;
    MenuItemRunParameterRequestFullScreen: TMenuItem;
    MenuItemRunParameterDisableFpsLimit: TMenuItem;
    MenuItemRunParameterDisableSound: TMenuItem;
    MenuItemRunParameters: TMenuItem;
    PanelOpenExistingView: TPanel;
    PanelNoDesign: TPanel;
    PanelNoDesignTop: TPanel;
    Separator9: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItemSimulationPauseUnpause: TMenuItem;
    MenuItemSimulationPlayStop: TMenuItem;
    SeparatorBeforeShowColliders: TMenuItem;
    MenuItemShowColliders: TMenuItem;
    MenuItemPhysics: TMenuItem;
    MenuItemCacheClean: TMenuItem;
    MenuItemCache: TMenuItem;
    SeparatorBeforeCache: TMenuItem;
    MenuItemWireframe: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    Separator10: TMenuItem;
    MenuShowJointTools28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    Separator8: TMenuItem;
    Separator7: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    Separator888888: TMenuItem;
    WarningsPopup: TPopupMenu;
    Separator6: TMenuItem;
    MenuItem23: TMenuItem;
    MenuSeparator6123: TMenuItem;
    MenuSeparator6: TMenuItem;
    Separator5: TMenuItem;
    MenuItem20: TMenuItem;
    Separator4: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    Separator2: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    Separator3: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    Separator1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItemViewportViewAll: TMenuItem;
    MenuItemViewportViewSelected: TMenuItem;
    MenuItemViewport: TMenuItem;
    MenuItemSystemInformation: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemOutputCopyAll: TMenuItem;
    MenuItemOutputCopySelected: TMenuItem;
    MenuItemSeparator12312123123: TMenuItem;
    MenuItemOutputClean: TMenuItem;
    MenuItemUIOutput: TMenuItem;
    MenuItemUIWarnings: TMenuItem;
    MenuItemUIFiles: TMenuItem;
    MenuItemUIRestoreDefaultDockSettings: TMenuItem;
    MenuItemEnableDisableDocking: TMenuItem;
    MenuItemUIProperties: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemUIHierarchy: TMenuItem;
    MenuItemUIExplorer: TMenuItem;
    MenuItemUIDesign: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemWindow: TMenuItem;
    MenuItemPackageFormat: TMenuItem;
    MenuItemSeparator12312332424: TMenuItem;
    MenuItemInstall: TMenuItem;
    MenuItemSeparator12312131: TMenuItem;
    MenuItemPlatform: TMenuItem;
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
    ActionNewUnitHereView: TAction;
    ActionNewUnitHereEmpty: TAction;
    ActionNewUnitHereBehavior: TAction;
    ActionNewUnitClass: TAction;
    ActionNewUnitView: TAction;
    ActionNewUnitEmpty: TAction;
    ActionNewUnitBehavior: TAction;
    ActionEditUnit: TAction;
    ActionOpenProjectCode: TAction;
    ApplicationProperties1: TApplicationProperties;
    MenuItem1: TMenuItem;
    MenuItemRegenerateProject: TMenuItem;
    MenuItemSeparator123123345: TMenuItem;
    OpenPascalUnitDialog: TCastleOpenPascalUnitDialog;
    MenuItemPopupNewUnitEmpty: TMenuItem;
    MenuItemPopupNewUnitClass: TMenuItem;
    MenuItemPopupNewUnitView: TMenuItem;
    MenuItemPopupNewUnitBehavior: TMenuItem;
    MenuItemPopupNewUnit: TMenuItem;
    N3: TMenuItem;
    MenuItemNewUnitView: TMenuItem;
    MenuItemNewUnitClass: TMenuItem;
    MenuItemNewUnitEmpty: TMenuItem;
    MenuItemNewUnitBehavior: TMenuItem;
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
    OutputPopup: TPopupMenu;
    ShellTreePopupMenu: TPopupMenu;
    SaveDesignDialog: TCastleSaveDialog;
    MenuItemSaveAsDesign: TMenuItem;
    MenuItemSaveDesign: TMenuItem;
    ListOutput: TListBox;
    MainMenu1: TMainMenu;
    MenuItemSeparator101: TMenuItem;
    MenuItemStopProcess: TMenuItem;
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
    procedure ActionPhysicsShowAllJointsToolsExecute(Sender: TObject);
    procedure ActionPhysicsHideAllJointsToolsExecute(Sender: TObject);
    procedure ActionFocusDesignExecute(Sender: TObject);
    procedure ActionModeInteractExecute(Sender: TObject);
    procedure ActionModeRotateExecute(Sender: TObject);
    procedure ActionModeScaleExecute(Sender: TObject);
    procedure ActionModeSelectExecute(Sender: TObject);
    procedure ActionModeTranslateExecute(Sender: TObject);
    procedure ActionPlayStopExecute(Sender: TObject);
    procedure ActionPlayStopUpdate(Sender: TObject);
    procedure ActionRunParameterCapabilitiesDefaultExecute(Sender: TObject);
    procedure ActionRunParameterCapabilitiesForceFixedFunctionExecute(
      Sender: TObject);
    procedure ActionRunParameterCapabilitiesForceModernExecute(Sender: TObject);
    procedure ActionRunParameterDefaultWindowOrFullscreenExecute(Sender: TObject
      );
    procedure ActionRunParameterDisableFpsLimitExecute(Sender: TObject);
    procedure ActionRunParameterDisableSoundExecute(Sender: TObject);
    procedure ActionRunParameterRequestFullScreenExecute(Sender: TObject);
    procedure ActionRunParameterRequestWindowExecute(Sender: TObject);
    procedure ActionShowCollidersExecute(Sender: TObject);
    procedure ActionSimulationPauseUnpauseExecute(Sender: TObject);
    procedure ActionSimulationPauseUnpauseUpdate(Sender: TObject);
    procedure ActionSimulationPlayStopExecute(Sender: TObject);
    procedure ActionSimulationPlayStopUpdate(Sender: TObject);
    procedure ActionViewportGridAxisExecute(Sender: TObject);
    procedure ActionComponentCutExecute(Sender: TObject);
    procedure ActionComponentSaveSelectedExecute(Sender: TObject);
    procedure ActionViewportAlignCameraToViewExecute(Sender: TObject);
    procedure ActionViewportAlignViewToCameraExecute(Sender: TObject);
    procedure ActionViewportGridAxisUpdate(Sender: TObject);
    procedure ActionViewportRenderNextExecute(Sender: TObject);
    procedure ActionViewportRenderNormalExecute(Sender: TObject);
    procedure ActionViewportRenderSolidWireframeExecute(Sender: TObject);
    procedure ActionViewportRenderWireframeOnlyExecute(Sender: TObject);
    procedure ActionViewportToggleProjectionExecute(Sender: TObject);
    procedure ActionNavigation2DExecute(Sender: TObject);
    procedure ActionNavigationExamineExecute(Sender: TObject);
    procedure ActionNavigationFlyExecute(Sender: TObject);
    procedure ActionNavigationToggle2DExecute(Sender: TObject);
    procedure ActionSystemInformationExecute(Sender: TObject);
    procedure ActionOutputCleanExecute(Sender: TObject);
    procedure ActionNewSpriteSheetExecute(Sender: TObject);
    procedure ActionEditAssociatedUnitExecute(Sender: TObject);
    procedure ActionEditUnitExecute(Sender: TObject);
    procedure ActionNewUnitClassExecute(Sender: TObject);
    procedure ActionNewUnitEmptyExecute(Sender: TObject);
    procedure ActionNewUnitViewExecute(Sender: TObject);
    procedure ActionNewUnitBehaviorExecute(Sender: TObject);
    procedure ActionNewUnitHereClassExecute(Sender: TObject);
    procedure ActionNewUnitHereEmptyExecute(Sender: TObject);
    procedure ActionNewUnitHereViewExecute(Sender: TObject);
    procedure ActionNewUnitHereBehaviorExecute(Sender: TObject);
    procedure ActionOpenProjectCodeExecute(Sender: TObject);
    procedure ActionOutputCopyAllExecute(Sender: TObject);
    procedure ActionOutputCopySelectedExecute(Sender: TObject);
    procedure ActionOutputCopySelectedUpdate(Sender: TObject);
    procedure ActionRegenerateProjectExecute(Sender: TObject);
    procedure ActionViewportBackExecute(Sender: TObject);
    procedure ActionViewportBottomExecute(Sender: TObject);
    procedure ActionViewportFrontExecute(Sender: TObject);
    procedure ActionViewportLeftExecute(Sender: TObject);
    procedure ActionViewportRightExecute(Sender: TObject);
    procedure ActionViewportSetup2DExecute(Sender: TObject);
    procedure ActionViewportSort2DExecute(Sender: TObject);
    procedure ActionViewportTopExecute(Sender: TObject);
    procedure ActionViewportViewAllExecute(Sender: TObject);
    procedure ActionViewportViewSelectedExecute(Sender: TObject);
    procedure ActionViewportUpdate(Sender: TObject);
    procedure ActionWarningsCleanExecute(Sender: TObject);
    procedure ActionWarningsCopyAllExecute(Sender: TObject);
    procedure ActionWarningsCopySelectedExecute(Sender: TObject);
    procedure ApplicationProperties1Activate(Sender: TObject);
    procedure ApplicationProperties1Deactivate(Sender: TObject);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ListOpenExistingViewDblClick(Sender: TObject);
    procedure ListOutputDblClick(Sender: TObject);
    procedure MenuItemCacheCleanClick(Sender: TObject);
    procedure MenuItemCacheClick(Sender: TObject);
    procedure MenuItemDesignNewNonVisualClick(Sender: TObject);
    procedure MenuItemEnableDisableDockingClick(Sender: TObject);
    procedure MenuItemInstallClick(Sender: TObject);
    procedure MenuItemNewDirectoryClick(Sender: TObject);
    procedure MenuItemRenameClick(Sender: TObject);
    procedure MenuItemShellTreeRefreshClick(Sender: TObject);
    procedure MenuItemUIDesignClick(Sender: TObject);
    procedure MenuItemUIExplorerClick(Sender: TObject);
    procedure MenuItemUIFilesClick(Sender: TObject);
    procedure MenuItemUIHierarchyClick(Sender: TObject);
    procedure MenuItemUIOutputClick(Sender: TObject);
    procedure MenuItemUIPropertiesClick(Sender: TObject);
    procedure MenuItemUIRestoreDefaultDockSettingsClick(Sender: TObject);
    procedure MenuItemUIWarningsClick(Sender: TObject);
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
    procedure MenuItemStopProcessClick(Sender: TObject);
    procedure MenuItemCgeWwwClick(Sender: TObject);
    procedure MenuItemCleanClick(Sender: TObject);
    procedure MenuItemCompileClick(Sender: TObject);
    procedure MenuItemCompileRunClick(Sender: TObject);
    procedure ActionCopyComponentExecute(Sender: TObject);
    procedure MenuItemDesignCloseClick(Sender: TObject);
    procedure ActionDeleteComponentExecute(Sender: TObject);
    procedure ActionDuplicateComponentExecute(Sender: TObject);
    procedure MenuItemManualClick(Sender: TObject);
    procedure MenuItemModeDebugClick(Sender: TObject);
    procedure MenuItemDesignNewUserInterfaceRectClick(Sender: TObject);
    procedure MenuItemDesignNewTransformClick(Sender: TObject);
    procedure MenuItemOnlyRunClick(Sender: TObject);
    procedure MenuItemOpenDesignClick(Sender: TObject);
    procedure MenuItemPackageClick(Sender: TObject);
    procedure MenuItemPackageSourceClick(Sender: TObject);
    procedure ActionPasteComponentExecute(Sender: TObject);
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
      ProjectPath, ProjectPathUrl, ProjectStandaloneSource,
        ProjectLazarus, ProjectDelphi: String;
      BuildMode: TBuildMode;
      OutputList: TOutputList;
      RunningProcess: TAsynchronousProcessQueue;
      Design: TDesignFrame;
      ShellListView1: TCastleShellListView;
      ShellTreeView1: TCastleShellTreeView;
      ViewFileFrame: TViewFileFrame;
      SplitterBetweenViewFile: TSplitter;
      Docking: Boolean;
      WantedDocking: Boolean;
      ErrorShownRefreshFilesMissingDirectory: Boolean;
      { Non-zero prevents the ShellListViewSelectItem from updating
        preview window (ViewFileFrame). }
      ShellListViewUpdating: Cardinal;
      PlatformsInfo: TPlatformInfoList;
      CurrentPlatformInfo: Integer; //< Index to PlatformsInfo
      CurrentPackageFormat: TPackageFormat;
      ListOpenExistingViewStr: TStringList;
      { Anchor docking forms }
      DesignForm: TForm;
      DesignHierarchyForm: TForm;
      DesignPropertiesForm: TForm;
      DesignExplorerForm: TForm;
      DesignFilesForm: TForm;
      DesignOutputForm: TForm;
      DesignWarningsForm: TForm;
    procedure BuildToolCall(const Commands: array of String;
      const RestartOnSuccess: Boolean = false);
    procedure BuildToolCallFinished(Sender: TObject);
    procedure ListOpenExistingViewAddFile(const FileInfo: TFileInfo;
      var StopSearch: boolean);
    procedure ListOpenExistingViewRefresh;
    procedure MenuItemAddComponentClick(Sender: TObject);
    procedure MenuItemDesignNewCustomRootClick(Sender: TObject);
    procedure MenuItemPackageFormatChangeClick(Sender: TObject);
    { Open Pascal file in preferred code editor.
      Line = -1 means to not use any particular line.
      Column = -1 means to not use any particular column. }
    procedure OpenPascal(const FileName: String;
      Line: Integer = -1;
      Column: Integer = -1);
    procedure RefreshFiles(const RefreshNecessary: TRefreshFiles);
    (*Runs custom code editor.
      Use this only when CodeEditor = ceCustom.
      CustomCodeEditorCommand is the command to use (like CodeEditorCommand
      or CodeEditorCommandProject).
      PascalFileName will be used as ${PAS} macro value.
      Line and Column, if not -1, will be passed as ${LINE} ${COLUMN macros. *)
    procedure RunCustomCodeEditor(const CustomCodeEditorCommand: String;
      const PascalFileName: String;
      const Line: Integer = -1;
      const Column: Integer = -1);
    procedure IsRunningChanged;
    procedure FreeProcess;
    procedure RunningToggle(Sender: TObject);
    function IsRunning: Boolean;
    procedure ShellListViewDoubleClick(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShowNewUnitForm(const AUnitType: TNewUnitType;
      const UnitOutputDirFromFileBrowser: Boolean);
    function ShowStatistics: Boolean;
    procedure UpdateFormCaption(Sender: TObject);
    { Propose saving the hierarchy.
      Returns should we continue (user did not cancel). }
    function ProposeSaveDesign: Boolean;
    { Propose saving the current design (if any) and then (unless user said "cancel")
      open the given design URL. }
    procedure ProposeOpenDesign(const DesignUrl: String);
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
    { Update ViewFileFrame existence and visibility to show currently
      selected item in ShellListView1. }
    procedure ViewFileFrameUpdate;
    procedure LoadDockLayout;
    procedure SaveDockLayout;
    procedure MenuItemPlatformChangeClick(Sender: TObject);
    procedure RestartEditor(Sender: TObject);
    procedure CurrentViewportChanged(Sender: TObject);
    { Question about saving during physics simulation. }
    function SaveDuringPhysicsSimulation: Boolean;
    function IsCreatingNewDesignAvailable: Boolean;
  public
    { Open a project, given an absolute path to CastleEngineManifest.xml }
    procedure OpenProject(const ManifestUrl: String);
  end;

var
  ProjectForm: TProjectForm;

implementation

{$R *.lfm}

uses TypInfo, LCLType, RegExpr, StrUtils, LCLVersion,
  CastleXMLUtils, CastleLCLUtils, CastleOpenDocument, CastleURIUtils,
  CastleFilesUtils, CastleUtils, CastleVectors, CastleColors, CastleConfig,
  CastleScene, CastleViewport, Castle2DSceneManager, CastleCameras,
  CastleTransform, CastleControls, CastleDownload, CastleApplicationProperties,
  CastleLog, CastleComponentSerialize, CastleSceneCore, CastleStringUtils,
  CastleFonts, X3DLoad, CastleFileFilters, CastleImages, CastleSoundEngine,
  CastleClassUtils, CastleLclEditHack, CastleRenderOptions, CastleTimeUtils,
  FormAbout, FormChooseProject, FormPreferences, FormSpriteSheetEditor,
  FormSystemInformation, FormRestartCustomEditor,
  ToolCompilerInfo, ToolCommonUtils, ToolArchitectures, ToolProcess,
  ToolFpcVersion;

procedure TProjectForm.MenuItemQuitClick(Sender: TObject);
begin
  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    InfoBox('Stop the physics simulation to be able to close editor.');
    Exit;
  end;

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
  BuildToolCall(['editor-rebuild-if-needed'], true);
end;

procedure TProjectForm.MenuItemSaveAsDesignClick(Sender: TObject);
begin
  if not SaveDuringPhysicsSimulation then
    Exit;

  Assert(Design <> nil); // menu item is disabled otherwise
  PrepareSaveDesignDialog(SaveDesignDialog, Design.DesignRoot);
  SaveDesignDialog.Url := Design.DesignUrl;
  if SaveDesignDialog.Execute then
  begin
    Design.SaveDesign(SaveDesignDialog.Url);
    // TODO: save DesignUrl somewhere? CastleEditorSettings.xml?

    // make sure to show new file in "Files" in editor
    RefreshFiles(rfFilesInCurrentDir);
  end;

  { On GTK, this happens when we open a dialog box, like open/save.
    It's important to stop treating keys/mouse as pressed then.

    Testcase:
    - Make new design,
    - add 3D viewport,
    - press right mouse button and S (to move back),
    - press Ctrl (invokes Save dialog),
    - release all keys, press "Cancel",
    -> without this line, TCastleControl would think "S" key is still down.

    Note:
    - TCastleControl.DoExit is not called in this case.
    - Form OnDeactive is also not called (matches docs on
      https://wiki.lazarus.freepascal.org/Event_order#Form.OnDeactivate ).
    - ApplicationProperties1Deactivate is not effective workaround for this
      (workarounds "open" but not "save" testcase for some reason,
      maybe because of "S" and Ctrl+S interaction).
  }
  Design.ReleaseAllKeysAndMouse;
end;

procedure TProjectForm.MenuItemSaveDesignClick(Sender: TObject);
begin
  if not SaveDuringPhysicsSimulation then
    Exit;

  Assert(Design <> nil); // menu item is disabled otherwise

  if Design.DesignUrl = '' then
    MenuItemSaveAsDesignClick(Sender)
  else
  begin
    Design.SaveDesign(Design.DesignUrl);

    // make sure to show new file in "Files" in editor
    RefreshFiles(rfFilesInCurrentDir);
  end;
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

procedure TProjectForm.MenuItemStopProcessClick(Sender: TObject);
begin
  if RunningProcess = nil then
    raise EInternalError.Create('No process is running now');

  OutputList.AddSeparator;
  OutputList.AddLine('Stopping the process.', okInfo);
  RunningProcess.TerminateChildrenHarder;
  FreeProcess;
end;

procedure TProjectForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    InfoBox('Stop the physics simulation to be able to turn off the editor.');
    CanClose := false;
    Exit;
  end;

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

procedure TProjectForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveDockLayout;
end;

procedure TProjectForm.ActionNewSpriteSheetExecute(Sender: TObject);
begin
  if SpriteSheetEditorForm = nil then
    SpriteSheetEditorForm := TSpriteSheetEditorForm.Create(Application);

  SpriteSheetEditorForm.Show;
  SpriteSheetEditorForm.NewSpriteSheet;
end;

procedure TProjectForm.ActionOutputCleanExecute(Sender: TObject);
begin
  ListOutput.Clear;
end;

procedure TProjectForm.ActionSystemInformationExecute(Sender: TObject);
begin
  if SystemInformationForm = nil then
    SystemInformationForm := TSystemInformationForm.Create(Application);
  SystemInformationForm.Show;
end;

procedure TProjectForm.ActionNavigationToggle2DExecute(Sender: TObject);
begin
  if (Design <> nil) and (Design.CurrentViewport <> nil) then
  begin
    { This comparison also determines what happens if current navigation
      is neither 2D, nor Fly.
      In this case we want to switch to 2D, because all other navigations
      are more 3D. }
    if Design.CurrentViewport.InternalDesignNavigationType = dn2D then
    begin
      Design.CurrentViewport.InternalDesignNavigationType := dnFly;
      ActionNavigationFly.Checked := true;
    end else
    begin
      Design.CurrentViewport.InternalDesignNavigationType := dn2D;
      ActionNavigation2D.Checked := true;
    end;
  end;
end;

procedure TProjectForm.ActionNavigationFlyExecute(Sender: TObject);
begin
  if (Design <> nil) and (Design.CurrentViewport <> nil) then
  begin
    Design.CurrentViewport.InternalDesignNavigationType := dnFly;
    ActionNavigationFly.Checked := true;
  end;
end;

procedure TProjectForm.ActionNavigation2DExecute(Sender: TObject);
begin
  if (Design <> nil) and (Design.CurrentViewport <> nil) then
  begin
    Design.CurrentViewport.InternalDesignNavigationType := dn2D;
    ActionNavigation2D.Checked := true;
  end;
end;

procedure TProjectForm.ActionNavigationExamineExecute(Sender: TObject);
begin
  if (Design <> nil) and (Design.CurrentViewport <> nil) then
  begin
    Design.CurrentViewport.InternalDesignNavigationType := dnExamine;
    ActionNavigationExamine.Checked := true;
  end;
end;

procedure TProjectForm.ActionViewportToggleProjectionExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportToggleProjection;
end;

procedure TProjectForm.ActionViewportAlignViewToCameraExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportAlignViewToCamera;
end;

procedure TProjectForm.ActionViewportGridAxisUpdate(Sender: TObject);
begin
  ActionViewportGridAxis.Checked :=
    (Design <> nil) and
    (Design.CurrentViewport <> nil) and
    Design.CurrentViewport.InternalGridAxis;
  ActionViewportUpdate(Sender);
end;

procedure TProjectForm.ActionViewportRenderNextExecute(Sender: TObject);
begin
  case InternalForceWireframe of
    weNormal        : ActionViewportRenderWireframeOnlyExecute(nil);
    weWireframeOnly : ActionViewportRenderSolidWireframeExecute(nil);
    weSolidWireframe: ActionViewportRenderNormalExecute(nil);
    else
      begin
        WritelnWarning('Unexpected InternalForceWireframe value');
        ActionViewportRenderWireframeOnlyExecute(nil);
      end;
  end;
end;

procedure TProjectForm.ActionViewportRenderNormalExecute(Sender: TObject);
begin
  InternalForceWireframe := weNormal;
  ActionViewportRenderNormal.Checked := true;
end;

procedure TProjectForm.ActionViewportRenderSolidWireframeExecute(Sender: TObject);
begin
  InternalForceWireframe := weSolidWireframe;
  ActionViewportRenderSolidWireframe.Checked := true;
end;

procedure TProjectForm.ActionViewportRenderWireframeOnlyExecute(Sender: TObject);
begin
  InternalForceWireframe := weWireframeOnly;
  ActionViewportRenderWireframeOnly.Checked := true;
end;

procedure TProjectForm.ActionViewportAlignCameraToViewExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportAlignCameraToView;
end;

procedure TProjectForm.ActionComponentCutExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.CutComponent;
end;

procedure TProjectForm.ActionViewportGridAxisExecute(Sender: TObject);
begin
  if (Design <> nil) and (Design.CurrentViewport <> nil) then
    Design.CurrentViewport.InternalGridAxis := not Design.CurrentViewport.InternalGridAxis;
end;

procedure TProjectForm.ActionPhysicsShowAllJointsToolsExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.ShowAllJointsTools;
end;

procedure TProjectForm.ActionPhysicsHideAllJointsToolsExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.HideAllJointsTools;
end;

procedure TProjectForm.ActionFocusDesignExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.FocusDesign;
end;

procedure TProjectForm.ActionModeInteractExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.ChangeMode(moInteract);
end;

procedure TProjectForm.ActionModeRotateExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.ChangeMode(moRotate);
end;

procedure TProjectForm.ActionModeScaleExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.ChangeMode(moScale);
end;

procedure TProjectForm.ActionModeSelectExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.ChangeMode(moSelect);
end;

procedure TProjectForm.ActionModeTranslateExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.ChangeMode(moTranslate);
end;

procedure TProjectForm.ActionPlayStopExecute(Sender: TObject);
begin
  RunningToggle(Sender);
end;

procedure TProjectForm.ActionPlayStopUpdate(Sender: TObject);
var
  NowIsRunning: Boolean;
begin
  NowIsRunning := IsRunning;
  if NowIsRunning then
    ActionPlayStop.ImageIndex := TImageIndex(iiStop)
  else
    ActionPlayStop.ImageIndex := TImageIndex(iiPlay);
  ActionPlayStop.Checked := NowIsRunning;

  BitBtnPlayStop.ImageIndex := ActionPlayStop.ImageIndex;
  if NowIsRunning then
  begin
    BitBtnPlayStop.Caption := 'Stop';
    BitBtnPlayStop.Hint := 'Break Compilation or Run (Ctrl + F2)';
  end else
  begin
    BitBtnPlayStop.Caption := 'Compile and Run';
    BitBtnPlayStop.Hint := 'Compile and Run (F9)';
  end;
  //BitBtnPlayStop.Checked := NowIsRunning;
end;

procedure TProjectForm.ActionRunParameterCapabilitiesDefaultExecute(
  Sender: TObject);
begin
  (Sender as TAction).Checked := true; // GroupIndex will make others unselected
end;

procedure TProjectForm.ActionRunParameterCapabilitiesForceFixedFunctionExecute(
  Sender: TObject);
begin
  (Sender as TAction).Checked := true; // GroupIndex will make others unselected
end;

procedure TProjectForm.ActionRunParameterCapabilitiesForceModernExecute(
  Sender: TObject);
begin
  (Sender as TAction).Checked := true; // GroupIndex will make others unselected
end;

procedure TProjectForm.ActionRunParameterDefaultWindowOrFullscreenExecute(
  Sender: TObject);
begin
  (Sender as TAction).Checked := true; // GroupIndex will make others unselected
end;

procedure TProjectForm.ActionRunParameterDisableFpsLimitExecute(Sender: TObject
  );
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
end;

procedure TProjectForm.ActionRunParameterDisableSoundExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
end;

procedure TProjectForm.ActionRunParameterRequestFullScreenExecute(
  Sender: TObject);
begin
  (Sender as TAction).Checked := true; // GroupIndex will make others unselected
end;

procedure TProjectForm.ActionRunParameterRequestWindowExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := true; // GroupIndex will make others unselected
end;

procedure TProjectForm.ActionShowCollidersExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.ShowColliders := not Design.ShowColliders;
  ActionShowColliders.Checked := Design.ShowColliders;
end;

procedure TProjectForm.ActionSimulationPauseUnpauseExecute(Sender: TObject);
begin
  Assert(Design <> nil);
  Design.SimulationPauseUnpause;
end;

procedure TProjectForm.ActionSimulationPauseUnpauseUpdate(Sender: TObject);
begin
  ActionSimulationPauseUnpause.Enabled := (Design <> nil) and
    (CastleApplicationMode in [appSimulation, appSimulationPaused]);
  ActionSimulationPauseUnpause.Checked := (Design <> nil) and
    (CastleApplicationMode = appSimulationPaused);
end;

procedure TProjectForm.ActionSimulationPlayStopExecute(Sender: TObject);
begin
  Assert(Design <> nil);
  Design.SimulationPlayStop;
end;

procedure TProjectForm.ActionSimulationPlayStopUpdate(Sender: TObject);
begin
  ActionSimulationPlayStop.Enabled := Design <> nil;
  ActionSimulationPlayStop.Checked := (Design <> nil) and
    (CastleApplicationMode in [appSimulation, appSimulationPaused]);
end;

procedure TProjectForm.ActionComponentSaveSelectedExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.SaveSelected;
end;

procedure TProjectForm.ApplicationProperties1Activate(Sender: TObject);
begin
  { Refresh contents of selected dir, and tree of subdirectories,
    in case user created some files/directories in other applications. }
  RefreshFiles(rfEverything);
end;

procedure TProjectForm.ApplicationProperties1Deactivate(Sender: TObject);
begin
  { On GTK, this happens when we open a dialog box, like open/save.
    It's important to stop treating keys/mouse as pressed then.

    Testcase:
    - Make new design,
    - add 3D viewport,
    - press right mouse button and W (to move forward),
    - release mouse,
    - press Ctrl+O (invokes Open dialog),
    - release all keys, press "Cancel",
    -> without this line, TCastleControl would think "W" key is still down.

    Note: TCastleControl.DoExit is not called in this case.
    Form OnDeactive is also not called (matches docs on
    https://wiki.lazarus.freepascal.org/Event_order#Form.OnDeactivate ).
  }

  if Design <> nil then
    Design.ReleaseAllKeysAndMouse;
end;

procedure TProjectForm.ActionOpenProjectCodeExecute(Sender: TObject);
var
  Exe, DelphiExe: String;
  Ce: TCodeEditor;
begin
  if CodeEditor = ceAutodetect then
    Ce := AutodetectCodeEditor
  else
    Ce := CodeEditor;

  case Ce of
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
    ceDelphi:
      begin
        FindDelphiPath(true, DelphiExe);

        { Open through DPROJ, this seems to be the only thing that works reliably. }
        if ProjectDelphi = '' then
        begin
          ErrorBox('Delphi project not defined (neither "standalone_source" nor "delphi_project" were specified in CastleEngineManifest.xml).' + NL +
            NL +
            'Create Delphi project (e.g. by "castle-engine generate-program") and update CastleEngineManifest.xml.');
          Exit;
        end;
        if not RegularFileExists(ProjectDelphi) then
        begin
          ErrorBox(Format('Delphi project file does not exist: %s.' + NL +
            NL +
            'Create Delphi project (by "castle-engine generate-program" or using Delphi).', [
            ProjectDelphi
          ]));
          Exit;
        end;

        {$ifdef DELPHI_OPEN_SHELL}
        OpenDocument(ProjectDelphi); // hack to open Pascal names in existing Delphi window, using DDE
        {$else}
        RunCommandNoWait(ProjectPath, DelphiExe, [
          ProjectDelphi
          //ProjectStandaloneSource
        ]);
        {$endif}
      end;
    ceVSCode:
      begin
        Exe := FindExeVSCode(true);
        RunCommandNoWait(ProjectPath, Exe, [
          { --add would add project to workspace in current window.
            See OpenPascal for comments. }
          //'--add',

          { We pass relative filenames, not absolute, to avoid
            VS Code on Windows inability to deal with spaces in filenames.
            See OpenPascal for comments. }
          '.'
        ], [rcNoConsole]);
      end;
    else raise EInternalError.Create('CodeEditor?');
  end;
end;

procedure TProjectForm.ActionOutputCopyAllExecute(Sender: TObject);
begin
  Clipboard.AsText := ListOutput.Items.Text;
end;

procedure TProjectForm.ActionOutputCopySelectedExecute(Sender: TObject);
begin
  Clipboard.AsText := ListOutput.GetSelectedText;
end;

procedure TProjectForm.ActionOutputCopySelectedUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListOutput.SelCount <> 0;
end;

procedure TProjectForm.ActionRegenerateProjectExecute(Sender: TObject);
begin
  BuildToolCall(['generate-program']);
end;

procedure TProjectForm.ActionViewportBackExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportViewAxis(Vector3(0, 0, 1), Vector3(0, 1, 0));
end;

procedure TProjectForm.ActionViewportBottomExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportViewAxis(Vector3(0, 1, 0), Vector3(0, 0, -1));
end;

procedure TProjectForm.ActionViewportFrontExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportViewAxis(Vector3(0, 0, -1), Vector3(0, 1, 0));
end;

procedure TProjectForm.ActionViewportLeftExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportViewAxis(Vector3(1, 0, 0), Vector3(0, 1, 0));
end;

procedure TProjectForm.ActionViewportRightExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportViewAxis(Vector3(-1, 0, 0), Vector3(0, 1, 0));
end;

procedure TProjectForm.ActionViewportSetup2DExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportSetup2D;
end;

procedure TProjectForm.ActionViewportSort2DExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportSort(bs2D);
end;

procedure TProjectForm.ActionViewportTopExecute(Sender: TObject);
begin
  if Design <> nil then
    { up -Z better than up +Z: makes more natural rotation when using 1/3/7 }
    Design.ViewportViewAxis(Vector3(0, -1, 0), Vector3(0, 0, -1));
end;

procedure TProjectForm.ActionViewportViewAllExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportViewAll;
end;

procedure TProjectForm.ActionViewportViewSelectedExecute(Sender: TObject);
begin
  if Design <> nil then
    Design.ViewportViewSelected;
end;

procedure TProjectForm.ActionViewportUpdate(Sender: TObject);
var
  ViewportActionsAllowed: Boolean;
begin
  ViewportActionsAllowed := (Design <> nil) and (Design.CurrentViewport <> nil);
  (Sender as TAction).Enabled := ViewportActionsAllowed;
  // MenuItemViewport.Enabled := ViewportActionsAllowed; // TODO would disable everything without ability to restore
end;

procedure TProjectForm.ActionWarningsCleanExecute(Sender: TObject);
begin
  ClearAllWarnings;
end;

procedure TProjectForm.ActionWarningsCopyAllExecute(Sender: TObject);
begin
  Clipboard.AsText := ListWarnings.Items.Text;
end;

procedure TProjectForm.ActionWarningsCopySelectedExecute(Sender: TObject);
//var
//  S: String;
begin
  Clipboard.AsText := ListWarnings.GetSelectedText;
  //S := '';
  //for I := 0 to ListWarnings.SelCount - 1 do
  //  S := SAppendPart(S, NL, ListWarnings.Selected[I]);
  //Clipboard.AsText := S;
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

procedure TProjectForm.ActionNewUnitViewExecute(Sender: TObject);
begin
  ShowNewUnitForm(utView, false);
end;

procedure TProjectForm.ActionNewUnitBehaviorExecute(Sender: TObject);
begin
  ShowNewUnitForm(utBehavior, false);
end;

procedure TProjectForm.ActionNewUnitHereClassExecute(Sender: TObject);
begin
  ShowNewUnitForm(utClass, true);
end;

procedure TProjectForm.ActionNewUnitHereEmptyExecute(Sender: TObject);
begin
  ShowNewUnitForm(utEmpty, true);
end;

procedure TProjectForm.ActionNewUnitHereViewExecute(Sender: TObject);
begin
  ShowNewUnitForm(utView, true);
end;

procedure TProjectForm.ActionNewUnitHereBehaviorExecute(Sender: TObject);
begin
  ShowNewUnitForm(utBehavior, true);
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
    ListOpenExistingViewRefresh;
    CheckNewUnitOnSearchPath;
    ProposeToOpenNewFile;
    RefreshFiles(rfFilesInCurrentDir);
  end;
end;

function TProjectForm.ShowStatistics: Boolean;
begin
  Result := ActionShowStatistics.Checked;
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

procedure TProjectForm.LoadDockLayout;
var
  XMLConfig: TXMLConfigStorage;
  Site: TAnchorDockHostSite;
  URLFileName: String;
begin
  if not Docking then Exit;
  URLFileName := ApplicationConfig(DockLayoutFileName);
  { Try to load default layout if user layout is not exist }
  if not URIFileExists(URLFileName) then
    URLFileName := InternalCastleDesignData + 'layouts/' + DockLayoutFileNameDefault;
  try
    XMLConfig := TXMLConfigStorage.Create(URIToFilenameSafe(URLFileName), True);
    try
      DockMaster.LoadLayoutFromConfig(XMLConfig, True);
    finally
      FreeAndNil(XMLConfig);
    end;
  except
    on E: Exception do
    begin
      { If no default layout setting is found, we manually dock design form to
        main form, and let other forms scatter around. }
      ErrorBox('Error while loading layout:' + NL + E.Message + NL + NL +
        'The editor will try to use default layout instead.');
      Site := DockMaster.GetAnchorSite(DesignForm);
      DockMaster.ManualDock(Site, Self, alClient);
    end;
  end;
end;

procedure TProjectForm.SaveDockLayout;
var
  XMLConfig: TXMLConfigStorage;
begin
  if not Docking then Exit;
  try
    XMLConfig := TXMLConfigStorage.Create(URIToFilenameSafe(ApplicationConfig(DockLayoutFileName)), false);
    try
      DockMaster.SaveLayoutToConfig(XMLConfig);
      XMLConfig.WriteToDisk;
    finally
      FreeAndNil(XMLConfig);
    end;
  except
    on E: Exception do
      ErrorBox('Error saving layout:' + NL + E.Message);
  end;
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
      '- Scenes and images open in engine viewers (view3dscene, castle-view-image).' + NL +
      '- Designs open in this editor.' + NL +
      '- Pascal files open in the code editor.' + NL +
      '- Other files open in OS default applications.';
    ShellListView1.PopupMenu := ShellListPopupMenu;
    ShellListView1.SmallImages := ShellIcons;
    ShellListView1.DragMode := dmAutomatic;

    ShellTreeView1.ShellListView := ShellListView1;
    ShellListView1.ShellTreeView := ShellTreeView1;
  end;

  procedure BuildPlatformsMenu;

    procedure AddPlatform(const Name: String; const Target: TTarget; const OS: TOS; const CPU: TCPU);
    var
      Mi: TMenuItem;
      MiCaption: String;
      P: TPlatformInfo;
    begin
      Mi := TMenuItem.Create(MenuItemPlatform);
      MiCaption := Name;
      if Target = targetCustom then
        MiCaption += ' (' + OSToString(OS) + ' / ' + CPUToString(CPU) + ')';
      Mi.Caption := MiCaption;
      Mi.Tag := PlatformsInfo.Count;
      Mi.OnClick := @MenuItemPlatformChangeClick;
      Mi.GroupIndex := 200;
      Mi.RadioItem := true;
      Mi.ShowAlwaysCheckable := true;
      Mi.Checked := Mi.Tag = CurrentPlatformInfo;
      MenuItemPlatform.Add(Mi);

      P := TPlatformInfo.Create;
      P.Target := Target;
      P.OS := OS;
      P.CPU := CPU;
      PlatformsInfo.Add(P);
    end;

    procedure AddPlatformSeparator;
    var
      Mi: TMenuItem;
    begin
      Mi := TMenuItem.Create(MenuItemPlatform);
      Mi.Caption := '-';
      MenuItemPlatform.Add(Mi);
    end;

  begin
    PlatformsInfo := TPlatformInfoList.Create(true);
    AddPlatform('Default', targetCustom, DefaultOS, DefaultCPU);
    AddPlatformSeparator;
    AddPlatform('Android (Arm 32-bit and 64-bit)', targetAndroid, { OS and CPU ignored } DefaultOS, DefaultCPU);
    AddPlatformSeparator;
    AddPlatform('iOS (Arm 32-bit and 64-bit)', targetIOS, { OS and CPU ignored } DefaultOS, DefaultCPU);
    AddPlatformSeparator;
    AddPlatform('Linux 32-bit', targetCustom, Linux, i386);
    AddPlatform('Linux 64-bit', targetCustom, Linux, x86_64);
    AddPlatform('Linux Arm 32-bit', targetCustom, Linux, Arm);
    AddPlatform('Linux Arm 64-bit', targetCustom, Linux, Aarch64);
    AddPlatformSeparator;
    AddPlatform('Windows 32-bit', targetCustom, Win32, i386);
    AddPlatform('Windows 64-bit', targetCustom, Win64, x86_64);
    AddPlatformSeparator;
    AddPlatform('macOS 64-bit', targetCustom, Darwin, x86_64);
    AddPlatform('macOS Arm 64-bit', targetCustom, Darwin, Aarch64);
    AddPlatformSeparator;
    AddPlatform('FreeBSD 32-bit', targetCustom, FreeBSD, i386);
    AddPlatform('FreeBSD 64-bit', targetCustom, FreeBSD, x86_64);
  end;

  procedure BuildPackageFormatsMenu;
  const
    PackageFormatCaptions: array [TPackageFormat] of string = (
      'Default',
      'Directory',
      'Compressed zip',
      'Compressed tar.gz',
      'Debian Package (DEB)',
      'Android APK',
      'Android App Bundle (AAB)',
      'iOS Xcode Project',
      'iOS Archive -> Development',
      'iOS Archive -> ad-hoc',
      'iOS Archive -> AppStore',
      'Nintendo Switch Project',
      'macOS App Bundle (APP)',
      'macOS App Bundle (APP) zip'
    );
  var
    Mi: TMenuItem;
    P: TPackageFormat;
  begin
    for P in TPackageFormat do
    begin
      Mi := TMenuItem.Create(MenuItemPackageFormat);
      Mi.Caption := PackageFormatCaptions[P];
      Mi.Tag := Ord(P);
      Mi.OnClick := @MenuItemPackageFormatChangeClick;
      Mi.GroupIndex := 210;
      Mi.RadioItem := true;
      Mi.ShowAlwaysCheckable := true;
      Mi.Checked := P = CurrentPackageFormat;
      MenuItemPackageFormat.Add(Mi);
    end;
  end;

var
  EnableDocking: Boolean;
begin
  EnableDocking := URIFileExists(ApplicationConfig('enable-docking.txt'));
  MenuItemWindow.SetEnabledVisible(EnableDocking);
  Docking := EnableDocking and UserConfig.GetValue('ProjectForm_Docking', false);
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
  BuildPlatformsMenu;
  BuildPackageFormatsMenu;
  ApplicationProperties.OnWarning.Add(@WarningNotification);
  ListOpenExistingViewStr := TStringList.Create;
  if Docking then
  begin
    // Create dockable forms
    //  DockMaster.DockSitesCanBeMinimized := True;
    DockMaster.MakeDockSite(Self, [akBottom], admrpNone);
    DesignForm := TForm.CreateNew(nil);
    DesignForm.Name := 'DesignForm';
    DesignForm.Caption := 'Design';
    DesignHierarchyForm := TForm.CreateNew(nil);
    DesignHierarchyForm.Name := 'DesignHierarchyForm';
    DesignHierarchyForm.Caption := 'Hierarchy';
    { We need to set the DesignTimePPI on new forms,
      otherwise when user has system-wide font scaling 125% then the layout
      from the initial LoadDockLayout call is not correct (and only calling
      later "Restore Default Docking Layout" helps). }
    DesignHierarchyForm.DesignTimePPI := DesignTimePPI;
    DesignPropertiesForm := TForm.CreateNew(nil);
    DesignPropertiesForm.Name := 'DesignPropertiesForm';
    DesignPropertiesForm.Caption := 'Properties';
    DesignPropertiesForm.DesignTimePPI := DesignTimePPI;
    DesignExplorerForm := TForm.CreateNew(nil);
    DesignExplorerForm.Name := 'DesignExplorerForm';
    DesignExplorerForm.Caption := 'Explorer';
    DesignExplorerForm.DesignTimePPI := DesignTimePPI;
    DesignFilesForm := TForm.CreateNew(nil);
    DesignFilesForm.Name := 'DesignFilesForm';
    DesignFilesForm.Caption := 'Files';
    DesignFilesForm.DesignTimePPI := DesignTimePPI;
    DesignOutputForm := TForm.CreateNew(nil);
    DesignOutputForm.Name := 'DesignOutputForm';
    DesignOutputForm.Caption := 'Output';
    DesignOutputForm.DesignTimePPI := DesignTimePPI;
    DesignWarningsForm := TForm.CreateNew(nil);
    DesignWarningsForm.Name := 'DesignWarningsForm';
    DesignWarningsForm.Caption := 'Warnings';
    DesignWarningsForm.DesignTimePPI := DesignTimePPI;
    DockMaster.MakeDockable(DesignForm, true, true);
    DockMaster.MakeDockable(DesignHierarchyForm, true, true);
    DockMaster.MakeDockable(DesignPropertiesForm, true, true);
    DockMaster.MakeDockable(DesignExplorerForm, true, true);
    DockMaster.MakeDockable(DesignFilesForm, true, true);
    DockMaster.MakeDockable(DesignOutputForm, true, true);
    DockMaster.MakeDockable(DesignWarningsForm, true, true);

    PageControlBottom.Parent := DesignExplorerForm;
    PageControlBottom.Align := alClient;
    ShellListView1.Parent := DesignFilesForm;
    ShellTreeView1.Align := alClient;
    ShellListView1.Align := alClient;
    ListOutput.Parent := DesignOutputForm;
    ListWarnings.Parent := DesignWarningsForm;
    PanelWarnings.Parent := DesignWarningsForm;
    // Hide splitters, as they aren't needed anymore since we use docked forms
    Splitter2.Visible := false;
    SplitterBetweenFiles.Visible := false;
    // Hide bottom tab's header
    PageControlBottom.ShowTabs := false;

    LoadDockLayout;
  end;
  WantedDocking := Docking;
  MenuItemEnableDisableDocking.Checked := Docking;
  MenuItemUIRestoreDefaultDockSettings.Enabled := Docking;
  MenuItemUIDesign.Enabled := Docking;
  MenuItemUIExplorer.Enabled := Docking;
  MenuItemUIHierarchy.Enabled := Docking;
  MenuItemUIProperties.Enabled := Docking;
  MenuItemUIFiles.Enabled := Docking;
  MenuItemUIOutput.Enabled := Docking;
  MenuItemUIWarnings.Enabled := Docking;
end;

procedure TProjectForm.FormDestroy(Sender: TObject);
begin
  UserConfig.SetValue('ProjectForm_Docking', WantedDocking);
  FormHide(Self); //to save config properly
  ApplicationProperties.OnWarning.Remove(@WarningNotification);
  ApplicationDataOverride := '';
  FreeProcess;
  FreeAndNil(OutputList);
  FreeAndNil(Manifest);
  FreeAndNil(DesignForm);
  FreeAndNil(DesignHierarchyForm);
  FreeAndNil(DesignPropertiesForm);
  FreeAndNil(DesignExplorerForm);
  FreeAndNil(DesignFilesForm);
  FreeAndNil(DesignOutputForm);
  FreeAndNil(DesignWarningsForm);
  FreeAndNil(PlatformsInfo);
  FreeAndNil(ListOpenExistingViewStr);
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
  if not Docking then
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
end;

procedure TProjectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { See CastleLclEditHack for an expanation of this hack. }
  ProcessKeyToPerformEdit(ActiveControl, Key, Shift);
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
  NewWindowState: TWindowState;
begin
  if (not Docking) and UserConfig.GetValue('ProjectForm_Saved', false) then
  begin
    NewWidth := UserConfig.GetValue('ProjectForm_Width', -MaxInt);
    NewHeight := UserConfig.GetValue('ProjectForm_Height', -MaxInt);
    NewLeft := UserConfig.GetValue('ProjectForm_Left', -MaxInt);
    NewTop := UserConfig.GetValue('ProjectForm_Top', -MaxInt);
    NewControlHeight := UserConfig.GetValue('ProjectForm_PageControlBottom.Height', -MaxInt);
    NewWindowState := StrToWindowState(UserConfig.GetValue('ProjectForm_WindowState', 'wsNormal'));

    { Apply new values.
      Note that we don't apply position/size values when NewWindowState <> wsMaximized,
      as on Windows this causes form to flicker when it appears (initially with invalid sizes
      inside a maximized form). }
    case NewWindowState of
      wsNormal:
        begin
          WindowState := NewWindowState;
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
      wsMaximized:
        begin
          WindowState := NewWindowState;
          { We use NewControlHeight, just like with wsNormal, but with a different condition. }
          if (NewControlHeight <= Screen.Height - 32) and (NewControlHeight >= 0) then
          begin
            PageControlBottom.Height := NewControlHeight;
          end;
        end;
    end;
  end;
end;

procedure TProjectForm.ListOpenExistingViewDblClick(Sender: TObject);
var
  DesignFileName, DesignUrl: String;
begin
  if ListOpenExistingView.ItemIndex <> -1 then
  begin
    DesignFileName := ListOpenExistingViewStr[ListOpenExistingView.ItemIndex];
    DesignUrl := FilenameToURISafe(DesignFileName);
    ProposeOpenDesign(DesignUrl);
  end;
end;

procedure TProjectForm.ListOutputDblClick(Sender: TObject);

  { For a filename in output, return absolute filename or raise exception. }
  function FilenameFromOutput(const S: String): String;
  var
    Test: String;
    CgeFileInfo: TFileInfo;
  begin
    // absolute filename
    if IsPathAbsolute(S) then
      Exit(S);

    // filename relative to project root
    Test := CombinePaths(ProjectPath, S);
    if FileExists(Test) then
      Exit(Test);

    // filename that has to be found on search paths
    if CharsPos(AllowDirectorySeparators, S) = 0 then
    begin
      Test := Manifest.SearchPascalFile(S);
      if Test <> '' then
         Exit(Test);
    end;

    // filename in CGE sources
    if (CastleEnginePath <> '') and
       FindFirstFile(CastleEnginePath, S, false, [ffRecursive], CgeFileInfo) then
      Exit(CgeFileInfo.AbsoluteName);

    raise Exception.CreateFmt('Cannot find Pascal filename "%s"', [S]);
  end;

var
  R: TRegExpr;
  Line: String;
begin
  // jump to source code line in case of error message here

  if ListOutput.ItemIndex = -1 then
    Exit;
  Line := ListOutput.Items[ListOutput.ItemIndex];

  R := TRegExpr.Create;
  try
    R.Expression := '^([^() ]+)\(([\d]+),([\d]+)\) (Error|Fatal|Warning|Note):';
    if R.Exec(Line) then
    begin
      OpenPascal(FilenameFromOutput(R.Match[1]), StrToInt(R.Match[2]), StrToInt(R.Match[3]));
      Exit;
    end;

    R.Expression := '^([^() ]+)\(([\d]+)\) (Error|Fatal|Warning|Note):';
    if R.Exec(Line) then
    begin
      OpenPascal(FilenameFromOutput(R.Match[1]), StrToInt(R.Match[2]));
      Exit;
    end;

    R.Expression := '^Compiling ([^() ]+)';
    if R.Exec(Line) then
    begin
      OpenPascal(FilenameFromOutput(R.Match[1]));
      Exit;
    end;
  finally
    FreeAndNil(R);
  end;
end;

procedure TProjectForm.MenuItemCacheCleanClick(Sender: TObject);
begin
  BuildToolCall(['cache-clean']);
end;

procedure TProjectForm.MenuItemCacheClick(Sender: TObject);
begin
  BuildToolCall(['cache']);
end;

procedure TProjectForm.MenuItemDesignNewNonVisualClick(Sender: TObject);
begin
  if not IsCreatingNewDesignAvailable then
    Exit;

  if ProposeSaveDesign then
    NewDesign(TCastleComponent, nil);
end;

procedure TProjectForm.MenuItemEnableDisableDockingClick(Sender: TObject);
begin
  InfoBox('Please close the project window and open it again to activate / deactivate the Docking Layout.');
  WantedDocking := not WantedDocking;
  MenuItemEnableDisableDocking.Checked := WantedDocking;
end;

procedure TProjectForm.MenuItemInstallClick(Sender: TObject);
begin
  BuildToolCall(['install']);
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
    // Select newly added dir
    ShellListView1.SelectedFileNameInRoot := NewDir;
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

procedure TProjectForm.MenuItemUIDesignClick(Sender: TObject);
begin
  DockMaster.MakeDockable(DesignForm, True, True);
end;

procedure TProjectForm.MenuItemUIExplorerClick(Sender: TObject);
begin
  DockMaster.MakeDockable(DesignExplorerForm, True, True);
end;

procedure TProjectForm.MenuItemUIFilesClick(Sender: TObject);
begin
  DockMaster.MakeDockable(DesignFilesForm, True, True);
end;

procedure TProjectForm.MenuItemUIHierarchyClick(Sender: TObject);
begin
  DockMaster.MakeDockable(DesignHierarchyForm, True, True);
end;

procedure TProjectForm.MenuItemUIOutputClick(Sender: TObject);
begin
  DockMaster.MakeDockable(DesignOutputForm, True, True);
end;

procedure TProjectForm.MenuItemUIPropertiesClick(Sender: TObject);
begin
  DockMaster.MakeDockable(DesignPropertiesForm, True, True);
end;

procedure TProjectForm.MenuItemUIRestoreDefaultDockSettingsClick(Sender: TObject
  );
var
  DockLayoutUrl: String;
begin
  { Simply remove the dock ui config file in order to restore default settings }
  DockLayoutUrl := ApplicationConfig(DockLayoutFileName);
  if URIFileExists(DockLayoutUrl) then
    CheckDeleteFile(URIToFilenameSafe(DockLayoutUrl));
  LoadDockLayout;
end;

procedure TProjectForm.MenuItemUIWarningsClick(Sender: TObject);
begin
  DockMaster.MakeDockable(DesignWarningsForm, True, True);
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
  { It is important to disable these actions, as e.g. calling
    TUndoSystem.Undo when IsUndoPossible=false will result in an exception.
    Testcase: right after loading the design, undo should not be possible,
    Ctrl+Z should do nothing. }
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
    if PlatformsInfo[CurrentPlatformInfo].Target = targetAndroid then
      BuildToolCall(['package', 'install', 'run'])
    else
      BuildToolCall(['compile', 'run']);
  end;
end;

procedure TProjectForm.ActionCopyComponentExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.CopyComponent;
end;

procedure TProjectForm.MenuItemDesignCloseClick(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise

  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    InfoBox('Stop the physics simulation to be able to close design.');
    Exit;
  end;

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

procedure TProjectForm.ActionDeleteComponentExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.DeleteComponent;
end;

procedure TProjectForm.ActionDuplicateComponentExecute(Sender: TObject);
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

procedure TProjectForm.ListOpenExistingViewAddFile(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  ListOpenExistingViewStr.Append(FileInfo.AbsoluteName);
end;

procedure TProjectForm.ListOpenExistingViewRefresh;

  function ShortDesignName(const S: String): String;
  begin
    Result := DeleteFileExt(ExtractFileName(S));
    Result := PrefixRemove('gameview', Result, true);
    Result := PrefixRemove('gamestate', Result, true);
    Result := SuffixRemove('.castle-user-interface', Result, true);
  end;

var
  ListItem: TListItem;
  DesignFileName, ProjectDataUrl: String;
begin
  { calculate ListOpenExistingViewStr contents }
  ListOpenExistingViewStr.Clear;
  { Search in ProjectDataUrl, not ProjectPathUrl, as all designs should be part of data
    to be possible to open them at runtime.
    This also avoids finding stuff in castle-engine-output, which is possible,
    e.g. after "castle-engine package --target=android" the castle-engine-output contains
    some temporary data with copies of design files -- and we *do not* want to show them here. }
  ProjectDataUrl := CombineURI(ProjectPathUrl, 'data/');
  if URIExists(ProjectDataUrl) <> ueNotExists then
  begin
    FindFiles(ProjectDataUrl, 'gameview*.castle-user-interface', false, @ListOpenExistingViewAddFile, [ffRecursive]);
    // support deprecated names
    FindFiles(ProjectDataUrl, 'gamestate*.castle-user-interface', false, @ListOpenExistingViewAddFile, [ffRecursive]);
  end;
  { without sorting, the order would be ~random (as FindFiles enumarates).
    Note that we sort including the subdirectory names, which is good,
    we want files in the same subdirectory to be together. }
  ListOpenExistingViewStr.Sort;

  { TODO: It seems LCL UI always shows as if the "Last Modified" (column 2)
    was sorted, and setting ListOpenExistingView.SortColumn from code
    or LFM doesn't change it. }

  { copy ListOpenExistingViewStr contents -> ListOpenExistingView GUI contents }
  ListOpenExistingView.Items.Clear;
  for DesignFileName in ListOpenExistingViewStr do
  begin
    ListItem := ListOpenExistingView.Items.Add;
    ListItem.Caption := ShortDesignName(DesignFileName);
    ListItem.SubItems.Append(ExtractRelativePath(ProjectPath, DesignFileName));
    ListItem.SubItems.Append(FileDateTimeStr(DesignFileName));
  end;
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
  MenuItemDesignAddBehavior.Enabled := Design <> nil;
  MenuItemDesignAddNonVisual.Enabled := Design <> nil;
  ActionComponentDelete.Enabled := Design <> nil;
  ActionComponentCopy.Enabled := Design <> nil;
  ActionComponentPaste.Enabled := Design <> nil;
  ActionComponentCut.Enabled := Design <> nil;
  ActionComponentDuplicate.Enabled := Design <> nil;
  ActionComponentSaveSelected.Enabled := Design <> nil;
  ActionEditAssociatedUnit.Enabled := Design <> nil;
  ActionFocusDesign.Enabled := Design <> nil;
  ActionModeInteract.Enabled := Design <> nil;
  ActionModeSelect.Enabled := Design <> nil;
  ActionModeTranslate.Enabled := Design <> nil;
  ActionModeRotate.Enabled := Design <> nil;
  ActionModeScale.Enabled := Design <> nil;
  ActionShowStatistics.Enabled := Design <> nil;

  { Options that toggle InternalForceWireframe could actually work with Design=nil,
    with current implementation.
    But their effect would be invisible, so better disable. }
  ActionViewportRenderNormal.Enabled := Design <> nil;
  ActionViewportRenderWireframeOnly.Enabled := Design <> nil;
  ActionViewportRenderSolidWireframe.Enabled := Design <> nil;
  ActionViewportRenderNext.Enabled := Design <> nil;
  MenuItemWireframe.Enabled := Design <> nil;

  UpdateUndo(nil);
  UpdateRenameItem(nil);
  UpdateFormCaption(nil);

  if (Design <> nil) and
     UserConfig.GetValue('ProjectForm_DesignSaved', false) and
     (not Docking) then
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

  SetEnabledVisible(PanelNoDesign, Design = nil);

  if Design = nil then
    ListOpenExistingViewRefresh;
end;

procedure TProjectForm.ProposeOpenDesign(const DesignUrl: String);
begin
  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    InfoBox('Stop the physics simulation to be able to open design.');
    Exit;
  end;

  if ProposeSaveDesign then
    OpenDesign(DesignUrl);
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
    Design.OnCurrentViewportChanged := @CurrentViewportChanged;
    Design.OnProposeOpenDesign := @ProposeOpenDesign;
    Design.OnIsRunning  := @IsRunning;
    Design.OnShowStatistics  := @ShowStatistics;
    Design.OnRunningToggle  := @RunningToggle;
    Design.OnApiReferenceOfCurrent := @MenuItemReferenceOfCurrentClick;

    // Update Design.ActionPlayStop, after OnIsRunning and OnRunningToggle are set
    Design.ActionPlayStopUpdate(Design.ActionPlayStop);

    DesignExistenceChanged;
    if Docking then
    begin
      // Transfer controls to dock forms, and modify it's align rule
      Design.Parent := DesignForm;
      Design.PanelLeft.Parent := DesignHierarchyForm;
      Design.PanelRight.Parent := DesignPropertiesForm;
      Design.Align := alClient;
      Design.PanelLeft.Align := alClient;
      Design.PanelRight.Align := alClient;
      // Hide splitters, as they dont need anymore since we use docked forms
      Design.SplitterLeft.Visible := False;
      Design.SplitterRight.Visible := False;
    end;
  end;
end;

procedure TProjectForm.CurrentViewportChanged(Sender: TObject);

  procedure UnselectAll;
  begin
    ActionNavigation2D.Checked := false;
    ActionNavigationFly.Checked := false;
    ActionNavigationExamine.Checked := false;
  end;

begin
  if (Design <> nil) and (Design.CurrentViewport <> nil) then
  begin
    { update menu state from Design.CurrentViewport.InternalDesignNavigationType }
    case Design.CurrentViewport.InternalDesignNavigationType of
      dn2D     : ActionNavigation2D.Checked := true;
      dnFly    : ActionNavigationFly.Checked := true;
      dnExamine: ActionNavigationExamine.Checked := true;
      else UnselectAll;
    end;
  end else
    UnselectAll;
end;

function TProjectForm.SaveDuringPhysicsSimulation: Boolean;
begin
  Result := true;
  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    Result := YesNoBox('The editor is during of physics simulation.'+ NL +
      'Saving the design will save the current state, not the state before the start of the simulation. Do you want to continue?');
  end;
end;

function TProjectForm.IsCreatingNewDesignAvailable: Boolean;
begin
  Result := true;
  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    InfoBox('Stop the physics simulation to be able to create new design.');
    Result := false;
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
  if not IsCreatingNewDesignAvailable then
    Exit;

  if ProposeSaveDesign then
    NewDesign(TCastleUserInterface, nil);
end;

procedure TProjectForm.MenuItemDesignNewTransformClick(Sender: TObject);
begin
  if not IsCreatingNewDesignAvailable then
    Exit;

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
  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    InfoBox('Stop the physics simulation to be able to open design.');
    Exit;
  end;

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

procedure TProjectForm.ActionPasteComponentExecute(Sender: TObject);
begin
  Assert(Design <> nil); // menu item is disabled otherwise
  Design.PasteComponent;
end;

procedure TProjectForm.MenuItemSwitchProjectClick(Sender: TObject);
begin
  if CastleApplicationMode in [appSimulation, appSimulationPaused] then
  begin
    InfoBox('Stop the physics simulation to be able to switch project.');
    Exit;
  end;

  if ProposeSaveDesign then
  begin
    { Close sprite sheet editor window if visible }
    if (SpriteSheetEditorForm <> nil) and (SpriteSheetEditorForm.Visible) then
    begin
      if not SpriteSheetEditorForm.CloseQuery then
        Exit;
      SpriteSheetEditorForm.Close; // not needed on GTK2, maybe add ifdef?
    end;

    Release; // do not call MenuItemDesignClose, to avoid OnCloseQuery
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
  ProcessUpdateTimer.Enabled := false;
  IsRunningChanged;
end;

procedure TProjectForm.ShellListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if ShellListViewUpdating = 0 then
    ViewFileFrameUpdate;
end;

procedure TProjectForm.ViewFileFrameUpdate;

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
  const CustomCodeEditorCommand: String; const PascalFileName: String;
  const Line: Integer; const Column: Integer);

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
  Macros: TStringStringMap;
begin
  Parameters := TCastleStringList.Create;
  try
    CommandToList(CustomCodeEditorCommand, Parameters);
    if Parameters.Count = 0 then
      raise Exception.CreateFmt('Code editor command was split into zero items: "%s"', [CustomCodeEditorCommand]);
    Exe := Parameters[0];
    Parameters.Delete(0);
    Macros := TStringStringMap.Create;
    try
      Macros.Add('${PAS}', PascalFileName);
      Macros.Add('${STANDALONE_SOURCE}', ProjectStandaloneSource);
      Macros.Add('${PROJECT_DIR}', ProjectPath);
      if Line <> -1 then
        Macros.Add('${LINE}', IntToStr(Line));
      if Column <> -1 then
        Macros.Add('${COLUMN}', IntToStr(Column));
      for I := 0 to Parameters.Count - 1 do
        Parameters[I] := SReplacePatterns(Parameters[I], Macros, true);
    finally FreeAndNil(Macros) end;
    RunCommandNoWait(CreateTemporaryDir, Exe, Parameters.ToArray);
  finally FreeAndNil(Parameters) end;
end;

function TProjectForm.IsRunning: Boolean;
begin
  Result := RunningProcess <> nil;
end;

procedure TProjectForm.RunningToggle(Sender: TObject);
begin
  if RunningProcess = nil then
    MenuItemCompileRunClick(MenuItemCompileRun)
  else
    MenuItemStopProcessClick(MenuItemStopProcess);
end;

procedure TProjectForm.OpenPascal(const FileName: String; Line: Integer;
  Column: Integer);
var
  Exe, DelphiExe, VsCodeFileArgument: String;
  Ce: TCodeEditor;
begin
  if CodeEditor = ceAutodetect then
    Ce := AutodetectCodeEditor
  else
    Ce := CodeEditor;

  case Ce of
    ceCustom:
      begin
        if (Line <> -1) and
           (CodeEditorCommandLineColumn <> '') then
        begin
          if Column = -1 then
            Column := 1; // we don't have a command to open only at line, so use column = 1
          RunCustomCodeEditor(CodeEditorCommandLineColumn, FileName, Line, Column);
        end else
          RunCustomCodeEditor(CodeEditorCommand, FileName);
      end;
    ceLazarus:
      begin
        Exe := FindExeLazarusIDE;

        if ProjectLazarus = '' then
        begin
          WritelnWarning('Lazarus project not defined (neither "standalone_source" nor "lazarus_project" were specified in CastleEngineManifest.xml), the file will be opened without changing Lazarus project.');
          RunCommandNoWait(CreateTemporaryDir, Exe, [FileName]);
        end else
        if not LazarusVersion.AtLeast(2, 2, 0) then
        begin
          { Before Lazarus 2.2, that brought fix to https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39338 ,
            using LPI on the command-line didn't work OK.

            Lazarus < 2.2 opens LPI as a regular XML file then, without changing the project.
            Moreover:
            - using LPR doesn't change the project either
            - using *only* LPI asks to change the project, even if it's already the current project
              (so we cannot fix the problem by executing it twice in a row, once with LPI once with PAS
              -- it would show dialog box every time)
          }

          WritelnWarning('Lazarus is older than 2.2, file will be opened without changing Lazarus project.');
          RunCommandNoWait(CreateTemporaryDir, Exe, [FileName]);
        end else
        begin
          // pass both project name, and particular filename, to open file within this project.
          RunCommandNoWait(CreateTemporaryDir, Exe, [ProjectLazarus, FileName])
        end;
      end;
    ceDelphi:
      begin
        FindDelphiPath(true, DelphiExe);

        { Open through DPROJ }
        (*
        if ProjectDelphi = '' then
        begin
          ErrorBox('Delphi project not defined (neither "standalone_source" nor "delphi_project" were specified in CastleEngineManifest.xml).' + NL +
            NL +
            'Create Delphi project (e.g. by "castle-engine generate-program") and update CastleEngineManifest.xml.');
          Exit;
        end;
        if not RegularFileExists(ProjectDelphi) then
        begin
          ErrorBox(Format('Delphi project file does not exist: %s.' + NL +
            NL +
            'Create Delphi project (by "castle-engine generate-program" or using Delphi).', [
            ProjectDelphi
          ]));
          Exit;
        end;
        *)

        { We use DelphiExe, which is BDS.exe.

          Notes:

          - Do not use BDSLauncher.exe. BDSLauncher is defined in registry as the application
            doing "shell open", but using BDSLauncher is not so easy: we would need
            then to pass filename using DDE (Windows inter-process communication API).
            Using just BDS is simpler.

          - Multiple filenames are not supported.
            We cannot point to the project *and* filename within, it seems.
            TODO: Need to use Delphi DDE for this?

          - We cannot open in existing Delphi instance?
            TODO: Need to use Delphi DDE for this?

          - Using /np doesn't work, in fact it makes the following filename ignored.
            Maybe BDS doesn't understand /np at all, and treats it as another filename?
            And only 1 filename is handler, as stated above.
        }

        {$ifdef DELPHI_OPEN_SHELL}
        OpenDocument(FileName); // hack to open Pascal names in existing Delphi window, using DDE
        {$else}
        RunCommandNoWait(ProjectPath, DelphiExe, [
          FileName
        ]);
        {$endif}

      end;
    ceVSCode:
      begin
        Exe := FindExeVSCode(true);

        { Explanation of ExtractRelativePath:

          We pass relative filenames, not absolute, to avoid
          VS Code on Windows inability to deal with spaces in filenames.
          Other solutions tried:

          - calling code.exe without intermediate code.cmd
          - using vscode:// URL with spaces encoded using %20.

          See EditorUtils -- nothing helped.

          Using relative paths is a workaround, as long as you don't
          place Pascal code in subdirectory with spaces. }
        VsCodeFileArgument := ExtractRelativePath(ProjectPath, FileName);

        { Explanation of '.':

          How to open a project?
          -add would add project to workspace in current window.
          It avoids opening new window ever,
          but it seems more confusing than helpful in our case
          -- it creates multi-root workspace which may be surprising to users.

          Instead we just pass project dir, as ".", to make sure this is
          opened as a workspace.
          See https://code.visualstudio.com/docs/editor/command-line ,
          https://stackoverflow.com/questions/29955785/opening-microsoft-visual-studio-code-from-command-prompt-windows
          //'--add'
        }

        if Line <> -1 then
        begin
          VsCodeFileArgument += ':' + IntToStr(Line);
          if Column <> -1 then
            VsCodeFileArgument += ':' + IntToStr(Column);
          RunCommandNoWait(ProjectPath, Exe, ['.', '--goto', VsCodeFileArgument],
            [rcNoConsole]);
        end else
          RunCommandNoWait(ProjectPath, Exe, ['.', VsCodeFileArgument],
            [rcNoConsole]);
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
  { Forcefully stop dragging.

    This workarounds LCL error with GTK 2 backend:
    1. if you open another design (xxx.castle-transform
       or xxx.castle-user-interface) by double-clicking (*not* by menu item "open...")
    2. and it causes a dialog box "save this design" (you can answer yes or no,
       doesn't matter)
    3. .. then the dragging remains "true" (even though you're no longer pressing
       down the mouse button).

    This causes weird behavior if you then do some mouse-down + move + mouse-up
    in newly opened design.
    - e.g. mouse look by right-click on any viewport in newly opened design.
    - or left click (mouse down and up) anywhere on UI.
    ... They will all try to drag-and-drop the design you have just opened onto
    itself.

    TODO: There remains a problem in the above case, even after this fix:
    First mouse down after such "forceful break of dragging" is not passed to TCastleControl.
    So you need to click again to actually start e.g. mouse look on a viewport.
  }
  if DragManager.IsDragging then
  begin
    DragManager.DragStop(false);
    WritelnLog('Forcefully breaking drag-and-drop on double-click to workaround LCL bug, afterwards IsDragging: %s', [
      BoolToStr(DragManager.IsDragging, true)
    ]);
  end;

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
      ProposeOpenDesign(SelectedURL);
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
  const RestartOnSuccess: Boolean);

  procedure AddPlatformParameters(const Params: TStrings; const PlatformInfo: TPlatformInfo);
  begin
    if (PlatformInfo.Target = targetCustom) and
       (PlatformInfo.OS = DefaultOS) and
       (PlatformInfo.CPU = DefaultCPU) then
      // keep command-line simple, to be simpler for user; no point is adding extra parameters
      Exit;

    if PlatformInfo.Target <> targetCustom then
    begin
      Params.Add('--target=' + TargetToString(PlatformInfo.Target));
    end else
    begin
      Params.Add('--os=' + OSToString(PlatformInfo.OS));
      Params.Add('--cpu=' + CPUToString(PlatformInfo.CPU));
    end;
  end;

  procedure AddModeParameters(const Params: TStrings);
  var
    ModeString: String;
  begin
    case BuildMode of
      bmDebug  : ModeString := '--mode=debug';
      bmRelease: ModeString := '--mode=release';
      else raise EInternalError.Create('BuildMode?');
    end;
    Params.Add(ModeString);
  end;

  procedure AddCompilerParameters(const Params: TStrings);
  begin
    if Compiler <> DefaultCompiler then
      Params.Add('--compiler=' + CompilerToString(Compiler));
  end;

  procedure AddPackageFormatParameters(const Params: TStrings; const Format: TPackageFormat);
  begin
    if Format <> pfDefault then
      Params.Add('--package-format=' + PackageFormatToString(Format));
  end;

  { Add parameters for "castle-engine run".
    Call it last, because it has to add also "--" that delimits build tool params
    from application params. }
  procedure AddRunParameters(const Params: TStrings);
  begin
    Params.Add('--');
    if ActionRunParameterDisableSound.Checked then
      Params.Add('--no-sound');
    if ActionRunParameterDisableFpsLimit.Checked then
      Params.Add('--no-limit-fps');
    if ActionRunParameterRequestFullScreen.Checked then
      Params.Add('--fullscreen');
    if ActionRunParameterRequestWindow.Checked then
      Params.Add('--window');
    if ActionRunParameterCapabilitiesForceFixedFunction.Checked then
      Params.Add('--capabilities=force-fixed-function');
    if ActionRunParameterCapabilitiesForceModern.Checked then
      Params.Add('--capabilities=force-modern');
  end;

var
  BuildToolExe, Command: String;
  QueueItem: TAsynchronousProcessQueue.TQueueItem;
begin
  if RunningProcess <> nil then
    raise EInternalError.Create('It should not be possible to call this when RunningProcess <> nil');

  BuildToolExe := FindExeCastleTool('castle-engine');
  if BuildToolExe = '' then
  begin
    ErrorBox('Cannot find build tool (castle-engine). Set the $CASTLE_ENGINE_PATH or $PATH environment variables or place all tools in CGE bin/ subdirectory.');
    Exit;
  end;

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
    QueueItem.Parameters.Add(Command);
    // add --mode=xxx parameter
    if not (
        (Command = 'package-source') or
        (Command = 'clean') or
        (Command = 'auto-generate-textures') or
        (Command = 'auto-generate-clean') or
        (Command = 'generate-program') or
        (Command = 'editor') or
        (Command = 'editor-rebuild-if-needed') or
        (Command = 'editor-run') or
        (Command = 'cache')
      ) then
      AddModeParameters(QueueItem.Parameters);
    // add --compiler parameter
    if (Command = 'compile') or
       (Command = 'package') then
      AddCompilerParameters(QueueItem.Parameters);
    // add --target, --os, --cpu parameters
    if (Command = 'compile') or
       (Command = 'run') or
       (Command = 'package') or
       (Command = 'install') or
       (Command = 'cache') then
      AddPlatformParameters(QueueItem.Parameters, PlatformsInfo[CurrentPlatformInfo]);
    // add --package-format
    if (Command = 'package') or
       (Command = 'install') then
      AddPackageFormatParameters(QueueItem.Parameters, CurrentPackageFormat);
    // editor always add --fast to package, as its more comfortable for normal development
    if (Command = 'package') then
      QueueItem.Parameters.Add('--fast');
    // add --target, --os, --cpu parameters
    if (Command = 'run') then
      AddRunParameters(QueueItem.Parameters);
    RunningProcess.Queue.Add(QueueItem);
  end;

  if RestartOnSuccess then
    RunningProcess.OnSuccessfullyFinishedAll := @RestartEditor;
  RunningProcess.OnFinished := @BuildToolCallFinished;

  RunningProcess.Start;

  IsRunningChanged;
end;

procedure TProjectForm.RestartEditor(Sender: TObject);
var
  BuildToolExe: String;
begin
  if ProposeSaveDesign then
  begin
    { Run custom editor, and finish current process.
      We use --wait-for-process-exit primarily because on Windows,
      we need to "unlock" the exe, to be able to copy new exe over the old exe
      (otherwise recompiling the custom editor could not just place
      new editor at the same exe).
      It is also sensible on all platforms, to make sure previous editor closes
      stuf (e.g. config files) reliably. }
    BuildToolExe := FindExeCastleTool('castle-engine');
    if BuildToolExe = '' then
    begin
      ErrorBox('Cannot find build tool (castle-engine). Set the $CASTLE_ENGINE_PATH or $PATH environment variables or place all tools in CGE bin/ subdirectory.');
      Exit;
    end;

    RunCommandNoWait(ProjectPath, BuildToolExe,
      ['editor-run', '--gui-errors', '--wait-for-process-exit', IntToStr(CurrentProcessId)],
      [rcNoConsole]);

    { Once ProposeSaveDesign and RunCommandNoWait are both successful,
      we want to terminate ASAP as user is waiting for new editor to run. }
    Application.Terminate;
  end;
end;

procedure TProjectForm.BuildToolCallFinished(Sender: TObject);
begin
  // bring back volume, in case MuteOnRun
  RunningApplication := false;
  SoundEngineSetVolume;
  RefreshFiles(rfFilesInCurrentDir); // show new files created by "package"
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
  if not IsCreatingNewDesignAvailable then
    Exit;

  if ProposeSaveDesign then
  begin
    R := TRegisteredComponent(Pointer((Sender as TComponent).Tag));
    NewDesign(R.ComponentClass, R.OnCreate);
  end;
end;

procedure TProjectForm.IsRunningChanged;
var
  EnableRun: Boolean;
begin
  EnableRun := not IsRunning;

  MenuItemCompile.Enabled := EnableRun;
  MenuItemCompileRun.Enabled := EnableRun;
  MenuItemOnlyRun.Enabled := EnableRun;
  MenuItemClean.Enabled := EnableRun;
  MenuItemPackage.Enabled := EnableRun;
  MenuItemPackageSource.Enabled := EnableRun;
  MenuItemInstall.Enabled := EnableRun;
  MenuItemAutoGenerateTextures.Enabled := EnableRun;
  MenuItemAutoGenerateClean.Enabled := EnableRun;
  MenuItemRestartRebuildEditor.Enabled := EnableRun;
  MenuItemCache.Enabled := EnableRun;
  MenuItemCacheClean.Enabled := EnableRun;
  ActionRegenerateProject.Enabled := EnableRun;

  MenuItemStopProcess.Enabled := not EnableRun;

  // Looks like we need to call this manually
  // (to update because of ActionPlayStopExecute or when process starts/stops independently)
  if Design <> nil then
    Design.ActionPlayStopUpdate(Design.ActionPlayStop);
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
  if InternalCustomComponentsForProject <> '' then
  begin
    if InternalCustomComponentsForProject = ProjectName then
      S := S + ' (With Custom Components)'
    else
      S := S + ' (With Custom Components from ' + InternalCustomComponentsForProject + ')';
  end;
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
  ProjectDelphi := Manifest.DelphiProject;

  { Make some fields absolute paths, or empty }
  if ProjectStandaloneSource <> '' then
    ProjectStandaloneSource := CombinePaths(ProjectPath, ProjectStandaloneSource);
  if ProjectLazarus <> '' then
    ProjectLazarus := CombinePaths(ProjectPath, ProjectLazarus);
  if ProjectDelphi <> '' then
    ProjectDelphi := CombinePaths(ProjectPath, ProjectDelphi);

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
  IsRunningChanged;

  BuildMode := bmDebug;
  MenuItemModeDebug.Checked := true;

  DesignExistenceChanged;
  UpdateFormCaption(nil); // make form Caption reflect project name (although this is now done also by DesignExistenceChanged)

  if (Manifest.EditorUnits <> '') and
     (ProjectName <> InternalCustomComponentsForProject) then
  begin
    RestartCustomEditorForm.Initialize(ProjectName, ProjectPath);
    case RestartCustomEditorForm.ShowModal of
      mrOK: MenuItemRestartRebuildEditorClick(nil);
      mrYesToAll: RestartEditor(nil);
    end;
    //WritelnWarning('Project uses custom components (declares editor_units in CastleEngineManifest.xml), but this is not a custom editor build.' + NL + 'Use the menu item "Project -> Restart Editor (With Custom Components)" to build and run correct editor.');
  end;
end;

procedure TProjectForm.RefreshFiles(const RefreshNecessary: TRefreshFiles);
var
  DirToRefresh, ErrorStr, TreeViewPath, SavedSelectedFileNameInRoot: String;
  SavedListViewOrigin: TPoint;
begin
  DirToRefresh := CombinePaths(ShellTreeView1.Root, ShellTreeView1.Path);
  if not DirectoryExists(DirToRefresh) then
  begin
    ErrorStr := Format('Directory "%s" no longer exists.', [DirToRefresh]);
    if not ErrorShownRefreshFilesMissingDirectory then
    begin
      { Set it before showing ErrorBox,
        as going back from ErrorBox can again cause this method to be called,
        so it may be in recursive call. }
      ErrorShownRefreshFilesMissingDirectory := true;
      ErrorBox(ErrorStr + NL +
        NL +
        'Further errors about this will be only shown as warnings.');
    end else
      WritelnWarning(ErrorStr);
    Exit;
  end;

  { save and restore selected file when refreshing.
    This way Alt+Tab doesn't deselect file. }
  SavedSelectedFileNameInRoot := ShellListView1.SelectedFileNameInRoot;
  SavedListViewOrigin := ShellListView1.ViewOrigin;
  Inc(ShellListViewUpdating);

  try
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

    ShellListView1.SelectedFileNameInRoot := SavedSelectedFileNameInRoot;
    ShellListView1.ViewOrigin := SavedListViewOrigin;

  finally
    { we update ViewFileFrame after everything is done,
      this way it doesn't blink off/on when doing Alt+Tab }
    Dec(ShellListViewUpdating);
    if ShellListViewUpdating = 0 then
      ViewFileFrameUpdate;
  end;
end;

procedure TProjectForm.MenuItemPlatformChangeClick(Sender: TObject);
var
  Mi: TMenuItem;
begin
  Mi := Sender as TMenuItem;
  CurrentPlatformInfo := Mi.Tag;
  Mi.Checked := true;
end;

procedure TProjectForm.MenuItemPackageFormatChangeClick(Sender: TObject);
var
  Mi: TMenuItem;
begin
  Mi := Sender as TMenuItem;
  CurrentPackageFormat := TPackageFormat(Mi.Tag);
  Mi.Checked := true;
end;

initialization
  // initialize CGE log
  ApplicationProperties.ApplicationName := 'castle-editor';
  // Useful for testing of custom editor run by "Restart Editor", to see the log easily on Unix
  // LogFileName := FileNameAutoInc('/tmp/castle-editor-%d.log');
  InitializeLog;
end.
