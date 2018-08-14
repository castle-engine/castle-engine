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
  CastleControl, CastleUIControls,
  // castle-editor units
  EditorUtils;

type
  { Main project management. }
  TProjectForm = class(TForm)
    CastleControl1: TCastleControl;
    PropertiesSimple: TValueListEditor;
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
    PageControl1: TPageControl;
    ControlProperties: TPageControl;
    PanelRight: TPanel;
    PanelAboveTabs: TPanel;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TabFiles: TTabSheet;
    TabOutput: TTabSheet;
    ProcessUpdateTimer: TTimer;
    ControlsTree: TTreeView;
    TabSheetSimple: TTabSheet;
    TabSheetAdvanced: TTabSheet;
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
    procedure MenuItemOnlyRunClick(Sender: TObject);
    procedure MenuItemPackageClick(Sender: TObject);
    procedure MenuItemPackageSourceClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemReferenceClick(Sender: TObject);
    procedure MenuItemModeReleaseClick(Sender: TObject);
    procedure MenuItemSwitchProjectClick(Sender: TObject);
    procedure ProcessUpdateTimerTimer(Sender: TObject);
  private
    ProjectName: String;
    ProjectPath, ProjectPathUrl: String;
    BuildMode: TBuildMode;
    OutputList: TOutputList;
    RunningProcess: TAsynchronousProcessQueue;
    PropertyGrid: TOIPropertyGrid;
    PropertyEditorHook: TPropertyEditorHook;
    procedure BuildToolCall(const Commands: array of String);
    function ComponentCaption(const C: TComponent): String;
    procedure PropertyGridModified(Sender: TObject);
    procedure SetEnabledCommandRun(const AEnabled: Boolean);
    procedure FreeProcess;
    procedure UpdateControlsTree(const List: TChildrenControls);
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
  CastleScene, CastleSceneManager, CastleTransform, CastleControls,
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
begin
  OutputList := TOutputList.Create(ListOutput);

  // This code is like in Kraft
  PropertyEditorHook := TPropertyEditorHook.Create(Self);
  PropertyGrid := TOIPropertyGrid.CreateWithParams(Self, PropertyEditorHook
     ,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
     , tkSString, tkLString, tkAString, tkWString, tkVariant
     , tkArray, tkRecord, tkInterface, tkClass, tkObject, tkWChar, tkBool
     , tkInt64, tkQWord],
     0 { auto size });
  PropertyGrid.Parent := TabSheetAdvanced;
  PropertyGrid.Align := alClient;
  PropertyGrid.OnModified := @PropertyGridModified;
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

procedure TProjectForm.MenuItemOnlyRunClick(Sender: TObject);
begin
  BuildToolCall(['run']);
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
  PageControl1.ActivePage := TabOutput;
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

  { Add some sample stuff to CastleControl1, just for test.
    TODO: just temporary stuff. }
  procedure SampleControls;
  var
    Scene: TCastleScene;
    Button: TCastleButton;
    RectangleGroup: TCastleRectangleControl;
    Lab: TCastleLabel;
  begin
    // TODO: This should follow the auto-scale settings of loaded file
    CastleControl1.Container.UIReferenceWidth := 1024;
    CastleControl1.Container.UIReferenceHeight := 768;
    CastleControl1.Container.UIScaling := usEncloseReferenceSize;

    Scene := TCastleScene.Create(Self);
    Scene.Name := 'ExampleScene';
    Scene.Load(CreateSceneRoot, true);
    CastleControl1.SceneManager.Items.Add(Scene);
    CastleControl1.SceneManager.MainScene := Scene;

    RectangleGroup := TCastleRectangleControl.Create(Self);
    RectangleGroup.Name := 'Rectangle1';
    RectangleGroup.Anchor(hpRight, -10);
    RectangleGroup.Anchor(vpTop, -10);
    RectangleGroup.Width := 500;
    RectangleGroup.Height := 500;
    RectangleGroup.Color := Vector4(0.5, 0.5, 1, 0.2); // transparent light-blue
    CastleControl1.Controls.InsertFront(RectangleGroup);

    { As you see, you need to explicitly set size of RectangleGroup,
      and position of children within it.
      If you would prefer an automatic layout (that auto-sizes following
      children, and automatically sets up children positions),
      use TCastleVerticalGroup or TCastleHorizontalGroup. }

    Button := TCastleButton.Create(Self);
    Button.Name := 'Button1';
    Button.Caption := 'I am a button';
    Button.FontSize := 50;
    Button.Anchor(vpTop, -10);
    Button.Anchor(hpMiddle);
    RectangleGroup.InsertFront(Button);

    Lab := TCastleLabel.Create(Self);
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

  UpdateControlsTree(CastleControl1.Controls);
  UpdateSelectedControl;

  // It's too easy to change it visually and forget, so we set it from code
  PageControl1.ActivePage := TabFiles;
  SetEnabledCommandRun(true);

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

procedure TProjectForm.UpdateControlsTree(const List: TChildrenControls);

  procedure AddTransform(const Parent: TTreeNode; const T: TCastleTransform);
  var
    S: String;
    Node: TTreeNode;
    I: Integer;
  begin
    S := ComponentCaption(T);
    Node := ControlsTree.Items.AddChildObject(Parent, S, T);
    for I := 0 to T.Count - 1 do
      AddTransform(Node, T[I]);
  end;

  procedure AddControl(const Parent: TTreeNode; const C: TUIControl);
  var
    S: String;
    Node: TTreeNode;
    I: Integer;
    SceneManager: TCastleSceneManager;
  begin
    S := ComponentCaption(C);
    Node := ControlsTree.Items.AddChildObject(Parent, S, C);
    for I := 0 to C.ControlsCount - 1 do
      AddControl(Node, C.Controls[I]);

    if C is TCastleSceneManager then
    begin
      SceneManager := TCastleSceneManager(C);
      AddTransform(Node, SceneManager.Items);
    end;
  end;

var
  Node: TTreeNode;
  C: TUIControl;
begin
  ControlsTree.Items.Clear;

  Node := ControlsTree.Items.AddChildObject(nil, 'Controls (list of TUIControl)', List);
  for C in List do
    AddControl(Node, C);

  // show expanded by default
  Node.Expand(true);
end;

procedure TProjectForm.UpdateSelectedControl;
var
  SelectedNode: TTreeNode;
  SelectedObject: TObject;
  SelectedComponent: TComponent;
  SelectedControl: TUIControl;
  SelectedTransform: TCastleTransform;
  SelectionForOI: TPersistentSelectionList;
begin
  SelectedControl := nil;
  SelectedTransform := nil;
  SelectedComponent := nil;
  SelectedObject := nil;
  SelectedNode := ControlsTree.Selected;
  if SelectedNode <> nil then
  begin
    SelectedObject := TObject(SelectedNode.Data);
    if SelectedObject is TComponent then
    begin
      SelectedComponent := TComponent(SelectedObject);
      if SelectedComponent is TUIControl then
        SelectedControl := TUIControl(SelectedComponent)
      else
      if SelectedComponent is TCastleTransform then
        SelectedTransform := TCastleTransform(SelectedComponent);
    end;
  end;

  if SelectedComponent <> nil then
    LabelControlSelected.Caption := ComponentCaption(SelectedComponent)
  else
    LabelControlSelected.Caption := 'Nothing Selected';

  ControlProperties.Visible := SelectedComponent <> nil;
  ControlProperties.Enabled := SelectedComponent <> nil;

  PropertyEditorHook.LookupRoot := SelectedComponent;
  SelectionForOI := TPersistentSelectionList.Create;
  try
    if SelectedComponent <> nil then
      SelectionForOI.Add(SelectedComponent);
    PropertyGrid.Selection := SelectionForOI;
  finally FreeAndNil(SelectionForOI) end;
end;

end.

