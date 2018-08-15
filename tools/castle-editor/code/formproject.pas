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
  Types,
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
    HierarchyRoot: TComponent;
    CastleControl: TCastleControlCustom;
    procedure BuildToolCall(const Commands: array of String);
    procedure ClearHierarchy;
    function ComponentCaption(const C: TComponent): String;
    procedure FindComponentClass(Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
    procedure InspectorSimpleFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure PropertyGridModified(Sender: TObject);
    procedure SaveHierarchy(const Url: string);
    procedure OpenHierarchy(const Url: string);
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

uses TypInfo, Contnrs, LResources,
  CastleXMLUtils, CastleLCLUtils, CastleOpenDocument, CastleURIUtils,
  CastleFilesUtils, CastleUtils, X3DNodes, CastleVectors, CastleColors,
  CastleScene, CastleSceneManager, Castle2DSceneManager,
  CastleTransform, CastleControls, CastleDownload,
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
var
  Stream: TStream;
begin
  Stream := URLSaveStream(Url);
  try
    WriteComponentAsTextToStream(Stream, HierarchyRoot);
  finally FreeAndNil(Stream) end;
end;

procedure TProjectForm.ClearHierarchy;
begin
  ControlsTree.Items.Clear;
  UpdateSelectedControl;
  CastleControl.Controls.Clear;
  FreeAndNil(HierarchyRoot);
end;

procedure TProjectForm.OpenHierarchy(const Url: string);
var
  NewHierarchyRoot: TComponent;
  TempSceneManager: TCastleSceneManager;
  Stream: TStream;
begin
  Stream := Download(Url);
  try
    NewHierarchyRoot := nil;
    ReadComponentFromTextStream(Stream, NewHierarchyRoot, @FindComponentClass,
      CastleControl);

    ClearHierarchy;

    if NewHierarchyRoot is TUIControl then
      CastleControl.Controls.InsertFront(NewHierarchyRoot as TUIControl)
    else
    if NewHierarchyRoot is TCastleTransform then
    begin
      TempSceneManager := TCastleSceneManager.Create(CastleControl);
      TempSceneManager.Items.Add(NewHierarchyRoot as TCastleTransform);
      CastleControl.Controls.InsertFront(TempSceneManager);
    end else
      raise EInternalError.Create('HierarchyRoot from file does not descend from TUIControl or TCastleTransform');

    // replace HierarchyRoot variable, once loading successfull
    HierarchyRoot := NewHierarchyRoot;
    UpdateHierarchy(HierarchyRoot);
  finally FreeAndNil(Stream) end;
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
  // TODO: is this correct? what should be set here?
  PropertyEditorHook.LookupRoot := CastleControl;

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
  Root: TUIControlSizeable;
begin
  ClearHierarchy;
  HierarchyUrl := '';

  // TODO: Allow choosing starting class?
  Root := TUIControlSizeable.Create(CastleControl);
  Root.Name := 'Group1';
  Root.FullSize := true;
  HierarchyRoot := Root;
  CastleControl.Controls.InsertFront(Root);

  UpdateHierarchy(HierarchyRoot);

  // TODO: should be automatic, by both Clear and InsertFront above
  CastleControl.Invalidate;
end;

procedure TProjectForm.MenuItemNewHierarchySceneTransformClick(Sender: TObject);
var
  TempSceneManager: TCastleSceneManager;
  Root: TCastleTransform;
begin
  ClearHierarchy;
  HierarchyUrl := '';

  // TODO: Allow choosing starting class?
  // TODO: after adding new scenes, trasforms, adjust camera?
  Root := TCastleTransform.Create(CastleControl);
  Root.Name := 'Transform1';

  TempSceneManager := TCastleSceneManager.Create(CastleControl);
  TempSceneManager.Items.Add(Root);
  CastleControl.Controls.InsertFront(TempSceneManager);

  HierarchyRoot := Root;

  UpdateHierarchy(HierarchyRoot);
end;

procedure TProjectForm.MenuItemOnlyRunClick(Sender: TObject);
begin
  BuildToolCall(['run']);
end;

procedure TProjectForm.MenuItemOpenClick(Sender: TObject);
begin
  OpenHierarchyDialog.Url := HierarchyUrl;
  if OpenHierarchyDialog.Execute then
  begin
    OpenHierarchy(OpenHierarchyDialog.Url);
    HierarchyUrl := OpenHierarchyDialog.Url; // once successfully loaded
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

  function CreateSceneRoot: TX3DRootNode;
  var
    //Sphere: TSphereNode;
    //Box: TBoxNode;
    SphereShape, BoxShape: TShapeNode;
    SphereTransform, BoxTransform: TTransformNode;
  begin
    Result := TX3DRootNode.Create;

    {Sphere := }TSphereNode.CreateWithTransform(SphereShape, SphereTransform);
    Result.AddChildren(SphereTransform);

    SphereShape.Material := TMaterialNode.Create;
    SphereShape.Material.DiffuseColor := YellowRGB;

    {Box := }TBoxNode.CreateWithTransform(BoxShape, BoxTransform);
    BoxTransform.Translation := Vector3(3, 0, 0);
    Result.AddChildren(BoxTransform);

    BoxShape.Material := TMaterialNode.Create;
    BoxShape.Material.DiffuseColor := BlueRGB;
  end;

  { Add some sample stuff to CastleControl, just for test.
    TODO: just temporary stuff. }
  procedure SampleControls;
  var
    SceneManager: TCastleSceneManager;
    Scene: TCastleScene;
    Button: TCastleButton;
    RectangleGroup: TCastleRectangleControl;
    Lab: TCastleLabel;
    Root: TUIControlSizeable;
  begin
    CastleControl.Controls.Clear;

    // TODO: This should follow the auto-scale settings of loaded file
    CastleControl.Container.UIReferenceWidth := 1600;
    CastleControl.Container.UIReferenceHeight := 900;
    CastleControl.Container.UIScaling := usEncloseReferenceSize;

    // Note that the owner of all components below is CastleControl1,
    // not Self, to avoid Name conflicts with our form.

    Root := TUIControlSizeable.Create(CastleControl);
    Root.Name := 'Group1';
    Root.FullSize := true;
    HierarchyRoot := Root;
    CastleControl.Controls.InsertFront(Root);

    SceneManager := TCastleSceneManager.Create(CastleControl);
    SceneManager.Name := 'SceneManager1';
    Root.InsertFront(SceneManager);

    Scene := TCastleScene.Create(CastleControl);
    Scene.Name := 'Scene1';
    Scene.Load(CreateSceneRoot, true);
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    RectangleGroup := TCastleRectangleControl.Create(CastleControl);
    RectangleGroup.Name := 'Rectangle1';
    RectangleGroup.Anchor(hpRight, -10);
    RectangleGroup.Anchor(vpTop, -10);
    RectangleGroup.Width := 500;
    RectangleGroup.Height := 500;
    RectangleGroup.Color := Vector4(0.5, 0.5, 1, 0.2); // transparent light-blue
    Root.InsertFront(RectangleGroup);

    { As you see, you need to explicitly set size of RectangleGroup,
      and position of children within it.
      If you would prefer an automatic layout (that auto-sizes following
      children, and automatically sets up children positions),
      use TCastleVerticalGroup or TCastleHorizontalGroup. }

    Button := TCastleButton.Create(CastleControl);
    Button.Name := 'Button1';
    Button.Caption := 'I am a button';
    Button.FontSize := 50;
    Button.Anchor(vpTop, -10);
    Button.Anchor(hpMiddle);
    RectangleGroup.InsertFront(Button);

    Lab := TCastleLabel.Create(CastleControl);
    Lab.Name := 'Label1';
    Lab.Caption := 'I am a label';
    Lab.FontSize := 50;
    Lab.Anchor(vpTop, -100);
    Lab.Anchor(hpMiddle);
    RectangleGroup.InsertFront(Lab);
  end;

var
  ManifestDoc: TXMLDocument;
begin
  ManifestDoc := URLReadXML(ManifestUrl);
  try
    ProjectName := ManifestDoc.DocumentElement.AttributeString('name');
  finally FreeAndNil(ManifestDoc) end;

  ProjectPathUrl := ExtractURIPath(ManifestUrl);
  ProjectPath := URIToFilenameSafe(ProjectPathUrl);

  Caption := SQuoteLCLCaption(ProjectName) + ' | Castle Game Engine';

  ShellTreeView1.Root := ProjectPath;

  // TODO CastleControl1 should be TCastleControlCustom ?

  SampleControls;

  UpdateHierarchy(HierarchyRoot);

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

procedure TProjectForm.FindComponentClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
const
  // TODO: should we just register this all, and it should work then automatically?
  Classes: array [0..8] of TComponentClass = (
    TUIControlSizeable,
    TCastleButton,
    TCastleLabel,
    TCastleRectangleControl,
    TCastleSceneManager,
    TCastle2DSceneManager,
    TCastleTransform,
    TCastleScene,
    TCastle2DScene
  );
var
  C: TComponentClass;
begin
  for C in Classes do
    if C.ClassName = AClassName then
      ComponentClass := C;
end;

procedure TProjectForm.InspectorSimpleFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  AShow := (aEditor.GetPropInfo <> nil) and
    (
      (aEditor.GetPropInfo^.Name = 'URL') or
      (aEditor.GetPropInfo^.Name = 'Name')
    );
end;

procedure TProjectForm.PropertyGridModified(Sender: TObject);
var
  SelectedComponent: TComponent;
begin
  // when you modify component Name in PropertyGrid, update it in the ControlsTree

  Assert(ControlsTree.Selected <> nil);
  Assert(ControlsTree.Selected.Data <> nil);
  Assert(TObject(ControlsTree.Selected.Data) is TComponent);
  SelectedComponent := TComponent(ControlsTree.Selected.Data);

  ControlsTree.Selected.Text := ComponentCaption(SelectedComponent);
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

procedure TProjectForm.UpdateSelectedControl;

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
  Selected: TComponentList;
  SelectionForOI: TPersistentSelectionList;
  I, SelectedCount: Integer;
  C: TComponent;
begin
  { calculate Selected list, non-nil <=> non-empty }
  Selected := nil;
  try
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
  { Enable using our property edits e.g. for TCastleScene.URL }
  CastlePropEdits.Register;
end.

