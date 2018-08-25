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
  // for TOIPropertyGrid usage
  ObjectInspector, PropEdits, PropEditUtils, GraphPropEdits,
  // CGE units
  CastleControl, CastleUIControls, CastlePropEdits, CastleDialogs,
  // castle-editor units
  EditorUtils;

type
  { Main project management. }
  TProjectForm = class(TForm)
    OpenHierarchyDialog: TCastleOpenDialog;
    MenuItemOpen: TMenuItem;
    MenuItemSeparator201: TMenuItem;
    MenuItemNewHierarchySceneTransform: TMenuItem;
    MenuItemNewHierarchyUserInterface: TMenuItem;
    SaveHierarchyDialog: TCastleSaveDialog;
    ControlsTree: TTreeView;
    LabelHierarchy: TLabel;
    MenuItemSeparator200: TMenuItem;
    MenuItemSaveAsHierarchy: TMenuItem;
    MenuItemSaveHierarchy: TMenuItem;
    PanelLeft: TPanel;
    LabelControlSelected: TLabel;
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
    ControlProperties: TPageControl;
    PanelRight: TPanel;
    PanelAboveTabs: TPanel;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;
    SplitterBetweenFiles: TSplitter;
    Splitter2: TSplitter;
    SplitterLeft: TSplitter;
    SplitterRight: TSplitter;
    TabFiles: TTabSheet;
    TabOutput: TTabSheet;
    ProcessUpdateTimer: TTimer;
    TabSimple: TTabSheet;
    TabAdvanced: TTabSheet;
    procedure ControlsTreeSelectionChanged(Sender: TObject);
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
    procedure MenuItemNewHierarchyUserInterfaceClick(Sender: TObject);
    procedure MenuItemNewHierarchySceneTransformClick(Sender: TObject);
    procedure MenuItemOnlyRunClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemPackageClick(Sender: TObject);
    procedure MenuItemPackageSourceClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemReferenceClick(Sender: TObject);
    procedure MenuItemModeReleaseClick(Sender: TObject);
    procedure MenuItemSaveAsHierarchyClick(Sender: TObject);
    procedure MenuItemSaveHierarchyClick(Sender: TObject);
    procedure MenuItemSwitchProjectClick(Sender: TObject);
    procedure ProcessUpdateTimerTimer(Sender: TObject);
  private
    ProjectName: String;
    ProjectPath, ProjectPathUrl: String;
    BuildMode: TBuildMode;
    OutputList: TOutputList;
    RunningProcess: TAsynchronousProcessQueue;
    InspectorSimple, InspectorAdvanced: TOIPropertyGrid;
    PropertyEditorHook: TPropertyEditorHook;
    HierarchyUrl: String;
    { Root saved/loaded to component file }
    HierarchyRoot: TComponent;
    { Owner of all components saved/loaded to component file,
      also temporary scene manager for .cge-scene-transform.
      Everything specific to this hierarchy in CastleControl. }
    HierarchyOwner: TComponent;
    CastleControl: TCastleControlCustom;
    procedure BuildToolCall(const Commands: array of String);
    function ComponentCaption(const C: TComponent): String;
    { calculate Selected list, non-nil <=> non-empty }
    procedure GetSelected(out Selected: TComponentList;
      out SelectedCount: Integer);
    procedure InspectorSimpleFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure PropertyGridModified(Sender: TObject);
    procedure SaveHierarchy(const Url: string);
    { Changes HierarchyRoot, HierarchyUrl and all the associated user-interface. }
    procedure OpenHierarchy(const NewHierarchyRoot, NewHierarchyOwner: TComponent;
      const NewHierarchyUrl: String);
    procedure OpenHierarchy(const NewHierarchyUrl: String);
    procedure SetEnabledCommandRun(const AEnabled: Boolean);
    procedure FreeProcess;
    procedure UpdateHierarchy(const Root: TComponent);
    procedure UpdateSelectedControl;
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

procedure TProjectForm.SaveHierarchy(const Url: string);
begin
  if HierarchyRoot is TUIControl then
    UserInterfaceSave(TUIControl(HierarchyRoot), Url)
  else
  if HierarchyRoot is TCastleTransform then
    TransformSave(TCastleTransform(HierarchyRoot), Url)
  else
    raise EInternalError.Create('We can only save HierarchyRoot that descends from TUIControl or TCastleTransform');
end;

procedure TProjectForm.OpenHierarchy(const NewHierarchyRoot, NewHierarchyOwner: TComponent;
  const NewHierarchyUrl: String);

  procedure ClearHierarchy;
  begin
    ControlsTree.Items.Clear;
    UpdateSelectedControl;
    CastleControl.Controls.Clear;
    HierarchyRoot := nil;

    // this actually frees everything inside HierarchyRoot
    FreeAndNil(HierarchyOwner);
  end;

var
  Background: TCastleSimpleBackground;
  TempSceneManager: TCastleSceneManager;
begin
  ClearHierarchy;

  if NewHierarchyRoot is TUIControl then
  begin
    CastleControl.Controls.InsertFront(NewHierarchyRoot as TUIControl)
  end else
  if NewHierarchyRoot is TCastleTransform then
  begin
    TempSceneManager := TCastleSceneManager.Create(NewHierarchyOwner);
    TempSceneManager.Transparent := true;
    TempSceneManager.Items.Add(NewHierarchyRoot as TCastleTransform);
    CastleControl.Controls.InsertFront(TempSceneManager);
  end else
    raise EInternalError.Create('HierarchyRoot from file does not descend from TUIControl or TCastleTransform');

  // make background defined
  Background := TCastleSimpleBackground.Create(NewHierarchyOwner);
  Background.Color := Vector4(0.5, 0.5, 0.5, 1);
  CastleControl.Controls.InsertBack(Background);

  // replace HierarchyXxx variables, once loading successfull
  HierarchyRoot := NewHierarchyRoot;
  HierarchyUrl := NewHierarchyUrl;
  HierarchyOwner := NewHierarchyOwner;
  // TODO: is this correct? what should be set here?
  PropertyEditorHook.LookupRoot := HierarchyOwner;

  UpdateHierarchy(HierarchyRoot);
end;

procedure TProjectForm.OpenHierarchy(const NewHierarchyUrl: string);
var
  NewHierarchyRoot, NewHierarchyOwner: TComponent;
  Mime: String;
begin
  NewHierarchyOwner := TComponent.Create(Self);

  Mime := URIMimeType(NewHierarchyUrl);
  if Mime = 'text/x-cge-user-interface' then
    NewHierarchyRoot := UserInterfaceLoad(NewHierarchyUrl, NewHierarchyOwner)
  else
  if Mime = 'text/x-cge-scene-transform' then
    NewHierarchyRoot := TransformLoad(NewHierarchyUrl, NewHierarchyOwner)
  else
    raise Exception.CreateFmt('Unrecgnized file extension (MIME type %s)',
      [Mime]);

  OpenHierarchy(NewHierarchyRoot, NewHierarchyOwner, NewHierarchyUrl);
end;

procedure TProjectForm.MenuItemSaveAsHierarchyClick(Sender: TObject);
begin
  // TODO -- disable when HierarchyRoot = nil

  if HierarchyRoot is TUIControl then
    SaveHierarchyDialog.DefaultExt := 'cge-user-interface'
  else
  if HierarchyRoot is TCastleTransform then
    SaveHierarchyDialog.DefaultExt := 'cge-scene-transform'
  else
    raise EInternalError.Create('HierarchyRoot does not descend from TUIControl or TCastleTransform');

  SaveHierarchyDialog.Url := HierarchyUrl;
  if SaveHierarchyDialog.Execute then
  begin
    SaveHierarchy(SaveHierarchyDialog.Url);
    HierarchyUrl := SaveHierarchyDialog.Url; // after successfull save
    // TODO: save HierarchyUrl somewhere? CastleEditorSettings.xml?
  end;
end;

procedure TProjectForm.MenuItemSaveHierarchyClick(Sender: TObject);
begin
  if HierarchyUrl = '' then
    MenuItemSaveAsHierarchyClick(Sender)
  else
    SaveHierarchy(HierarchyUrl);
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

procedure TProjectForm.ControlsTreeSelectionChanged(Sender: TObject);
begin
  UpdateSelectedControl;
end;

procedure TProjectForm.FormCreate(Sender: TObject);

  function CommonInspectorCreate: TOIPropertyGrid;
  begin
    Result := TOIPropertyGrid.Create(Self);
    // This code is inspired by Kraft sandbox demo
    Result.Filter := [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
      , tkSString, tkLString, tkAString, tkWString, tkVariant
      , tkArray, tkRecord, tkInterface, tkClass, tkObject, tkWChar, tkBool
      , tkInt64, tkQWord];
    Result.PropertyEditorHook := PropertyEditorHook;
    Result.Align := alClient;
    Result.OnModified := @PropertyGridModified;
    Result.CheckboxForBoolean := true;
    Result.PreferredSplitterX := 150;
    Result.ValueFont.Bold := true;
    Result.ShowGutter := false;
  end;

begin
  OutputList := TOutputList.Create(ListOutput);

  PropertyEditorHook := TPropertyEditorHook.Create(Self);

  InspectorSimple := CommonInspectorCreate;
  InspectorSimple.Parent := TabSimple;
  InspectorSimple.OnEditorFilter := @InspectorSimpleFilter;

  InspectorAdvanced := CommonInspectorCreate;
  InspectorAdvanced.Parent := TabAdvanced;

  CastleControl := TCastleControlCustom.Create(Self);
  CastleControl.Parent := PanelAboveTabs;
  CastleControl.Align := alClient;
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

procedure TProjectForm.MenuItemNewHierarchyUserInterfaceClick(Sender: TObject);
var
  NewRoot: TUIControlSizeable;
  NewHierarchyOwner: TComponent;
begin
  NewHierarchyOwner := TComponent.Create(Self);

  // TODO: Allow choosing starting class?
  NewRoot := TUIControlSizeable.Create(NewHierarchyOwner);
  NewRoot.Name := 'Group1';
  NewRoot.FullSize := true;
  OpenHierarchy(NewRoot, NewHierarchyOwner, '');
end;

procedure TProjectForm.MenuItemNewHierarchySceneTransformClick(Sender: TObject);
var
  NewRoot: TCastleTransform;
  NewHierarchyOwner: TComponent;
begin
  NewHierarchyOwner := TComponent.Create(Self);

  // TODO: Allow choosing starting class?
  // TODO: after adding new scenes, trasforms, adjust camera?
  NewRoot := TCastleTransform.Create(NewHierarchyOwner);
  NewRoot.Name := 'Transform1';
  OpenHierarchy(NewRoot, NewHierarchyOwner, '');
end;

procedure TProjectForm.MenuItemOnlyRunClick(Sender: TObject);
begin
  BuildToolCall(['run']);
end;

procedure TProjectForm.MenuItemOpenClick(Sender: TObject);
begin
  OpenHierarchyDialog.Url := HierarchyUrl;
  if OpenHierarchyDialog.Execute then
    OpenHierarchy(OpenHierarchyDialog.Url);
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

  Caption := SQuoteLCLCaption(ProjectName) + ' | Castle Game Engine';

  ShellTreeView1.Root := ProjectPath;

  // initialize CastleControl
  // TODO: This should follow the auto-scale settings of loaded file
  CastleControl.Container.UIReferenceWidth := 1600;
  CastleControl.Container.UIReferenceHeight := 900;
  CastleControl.Container.UIScaling := usEncloseReferenceSize;

  // start
  MenuItemNewHierarchyUserInterfaceClick(nil);

  // It's too easy to change it visually and forget, so we set it from code
  PageControlBottom.ActivePage := TabFiles;
  SetEnabledCommandRun(true);
  ControlProperties.ActivePage := TabSimple;

  BuildMode := bmDebug;
  MenuItemModeDebug.Checked := true;
end;

function TProjectForm.ComponentCaption(const C: TComponent): String;

  function ClassCaption(const C: TClass): String;
  begin
    Result := C.ClassName;

    // hide some internal classes by instead displaying ancestor name
    if (C = TControlGameSceneManager) or
       (C = TSceneManagerWorld) or
       (Result = 'TSceneManagerWorldConcrete') then
      Result := ClassCaption(C.ClassParent);
  end;

begin
  Result := C.Name + ' (' + ClassCaption(C.ClassType) + ')';
end;

procedure TProjectForm.InspectorSimpleFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  AShow := (aEditor.GetPropInfo <> nil) and
    (
      (aEditor.GetPropInfo^.Name = 'URL') or
      (aEditor.GetPropInfo^.Name = 'Name') or
      (aEditor.GetPropInfo^.Name = 'Caption')
    );
end;

procedure TProjectForm.PropertyGridModified(Sender: TObject);
var
  SelectedComponent: TComponent;
  Selected: TComponentList;
  SelectedCount: Integer;
begin
  // when you modify component Name in PropertyGrid, update it in the ControlsTree
  Assert(ControlsTree.Selected <> nil);
  Assert(ControlsTree.Selected.Data <> nil);
  Assert(TObject(ControlsTree.Selected.Data) is TComponent);
  SelectedComponent := TComponent(ControlsTree.Selected.Data);

  ControlsTree.Selected.Text := ComponentCaption(SelectedComponent);

  { update also LabelControlSelected }
  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount = 1 then
      LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Selected[0]);
  finally FreeAndNil(Selected) end;
end;

procedure TProjectForm.UpdateHierarchy(const Root: TComponent);

  function AddTransform(const Parent: TTreeNode; const T: TCastleTransform): TTreeNode;
  var
    S: String;
    I: Integer;
  begin
    S := ComponentCaption(T);
    Result := ControlsTree.Items.AddChildObject(Parent, S, T);
    for I := 0 to T.Count - 1 do
      AddTransform(Result, T[I]);
  end;

  function AddControl(const Parent: TTreeNode; const C: TUIControl): TTreeNode;
  var
    S: String;
    I: Integer;
    SceneManager: TCastleSceneManager;
  begin
    S := ComponentCaption(C);
    Result := ControlsTree.Items.AddChildObject(Parent, S, C);
    for I := 0 to C.ControlsCount - 1 do
      AddControl(Result, C.Controls[I]);

    if C is TCastleSceneManager then
    begin
      SceneManager := TCastleSceneManager(C);
      AddTransform(Result, SceneManager.Items);
    end;
  end;

var
  Node: TTreeNode;
begin
  ControlsTree.Items.Clear;

  if Root is TUIControl then
    Node := AddControl(nil, Root as TUIControl)
  else
  if Root is TCastleTransform then
    Node := AddTransform(nil, Root as TCastleTransform)
  else
    raise EInternalError.Create('Cannot UpdateHierarchy with other classes than TUIControl or TCastleTransform');

  // show expanded by default
  Node.Expand(true);

  UpdateSelectedControl;
end;

procedure TProjectForm.GetSelected(out Selected: TComponentList;
  out SelectedCount: Integer);

  function SelectedFromNode(const Node: TTreeNode): TComponent;
  var
    SelectedObject: TObject;
    //SelectedControl: TUIControl;
    //SelectedTransform: TCastleTransform;
  begin
    SelectedObject := nil;
    Result := nil;
    //SelectedControl := nil;
    //SelectedTransform := nil;

    if Node <> nil then
    begin
      SelectedObject := TObject(Node.Data);
      if SelectedObject is TComponent then
      begin
        Result := TComponent(SelectedObject);
        //if SelectedComponent is TUIControl then
        //  SelectedControl := TUIControl(SelectedComponent)
        //else
        //if SelectedComponent is TCastleTransform then
        //  SelectedTransform := TCastleTransform(SelectedComponent);
      end;
    end;
  end;

var
  I: Integer;
  C: TComponent;
begin
  Selected := nil;

  for I := 0 to ControlsTree.SelectionCount - 1 do
  begin
    C := SelectedFromNode(ControlsTree.Selections[I]);
    if C <> nil then
    begin
      if Selected = nil then
        Selected := TComponentList.Create(false);
      Selected.Add(C);
    end;
  end;

  if Selected <> nil then
    SelectedCount := Selected.Count
  else
    SelectedCount := 0;
end;

procedure TProjectForm.UpdateSelectedControl;
var
  Selected: TComponentList;
  SelectionForOI: TPersistentSelectionList;
  I, SelectedCount: Integer;
begin
  GetSelected(Selected, SelectedCount);
  try
    case SelectedCount of
      0: LabelControlSelected.Caption := 'Nothing Selected';
      1: LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Selected[0]);
      else LabelControlSelected.Caption := 'Selected:' + NL + IntToStr(SelectedCount) + ' components';
    end;

    ControlProperties.Visible := SelectedCount <> 0;
    ControlProperties.Enabled := SelectedCount <> 0;

    SelectionForOI := TPersistentSelectionList.Create;
    try
      for I := 0 to SelectedCount - 1 do
        SelectionForOI.Add(Selected[I]);
      InspectorSimple.Selection := SelectionForOI;
      InspectorAdvanced.Selection := SelectionForOI;
    finally FreeAndNil(SelectionForOI) end;
  finally FreeAndNil(Selected) end;
end;

initialization
  // initialize CGE log
  ApplicationProperties.ApplicationName := 'castle-editor';
  InitializeLog;

  { Enable using our property edits e.g. for TCastleScene.URL }
  CastlePropEdits.Register;
end.
