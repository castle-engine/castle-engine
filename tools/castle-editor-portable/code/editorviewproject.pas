{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Edit a chosen project. }
unit EditorViewProject;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleInternalInspector,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleFindFiles;

type
  { Edit a chosen project. }
  TViewProject = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonCloseProject: TCastleButton;
    ButtonCloseDesign: TCastleButton;
    ButtonSaveDesign: TCastleButton;
    ButtonViewTemplate: TCastleButton;
    ContainerDesignView, ContainerOpenView, ContainerLoadedDesign: TCastleUserInterface;
    FactoryButtonView: TCastleComponentFactory;
    ListOpenExistingView: TCastleVerticalGroup;
    ScrollListOpenExistingView: TCastleUserInterface;
    CheckboxShowHierarchy: TCastleCheckbox;
    CheckboxShowProperties: TCastleCheckbox;
  private
    { Root of the design, saved/loaded to component file.
      @nil if no design is loaded. }
    DesignRoot: TCastleUserInterface;
    { URL of the currently open design.
      Empty if no design is loaded. }
    CurrentDesignUrl: String;
    { Owner of all components saved/loaded to the design file.
      Also owner of a temporary viewport for .castle-transform,
      in general this owns everything specific to display currrent design. }
    DesignOwner: TComponent;
    Properties: TCastleComponentProperties;
    Hierarchy: TCastleComponentsHierarchy;
    ListOpenExistingViewStr: TStringList;
    procedure ClickCloseProject(Sender: TObject);
    procedure ClickCloseDesign(Sender: TObject);
    procedure ClickSaveDesign(Sender: TObject);
    procedure ClickOpenView(Sender: TObject);
    procedure ChangeShowHierarchy(Sender: TObject);
    procedure ChangeShowProperties(Sender: TObject);
    procedure HierarchySelect(const Selected: TComponent);
    procedure ListOpenExistingViewAddFile(const FileInfo: TFileInfo;
      var StopSearch: boolean);
    procedure ProposeOpenDesign(const OpenDesignUrl: String);
    procedure ListViewsRefresh;
    { Is Child selectable and visible in hierarchy. }
    class function Selectable(const Child: TComponent): Boolean; static;
    { Is Child deletable by user (this implies it is also selectable). }
    function Deletable(const Child: TComponent): Boolean;
    { Free component C (which should be part of this designed, owned by DesignOwner)
      and all children.

      We have to delete things recursively, otherwise they would keep existing,
      taking resources and reserving names in DesignOwner,
      even though they would not be visible when disconnected from parent
      hierarchy.

      This does nothing if you try to free some internal component
      (like csTransient) or the design root (which can never be freed). }
    procedure FreeComponentRecursively(const C: TComponent);
    { Free and clear DesignRoot, DesignOwner. }
    procedure ClearDesign;
    { Update ContainerLoadedDesignSize size and anchor,
      depending on visibility of Hierarchy / Properties. }
    procedure UpdateContainerLoadedDesignSize;
    { Update UI based on whether a design is loaded now (Design <> nil). }
    procedure DesignExistenceChanged;
  public
    // set before starting the project
    // Absolute project path, as URL, ending with /.
    ProjectPathUrl: String;
    // Full URL to the project manifest, including final filename.
    ProjectManifestUrl: String;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewProject: TViewProject;

implementation

uses SysUtils,
  CastleStringUtils, CastleUriUtils, CastleUtils, CastleFilesUtils,
  CastleInternalPhysicsVisualization, CastleClassUtils, CastleViewport,
  CastleTransform,
  ToolEditorUtils,
  EditorViewChooseProject, EditorViewChooseExistingProject;

{ TViewProject ----------------------------------------------------------------- }

constructor TViewProject.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/editorviewproject.castle-user-interface';
end;

procedure TViewProject.Start;
begin
  inherited;
  ButtonCloseProject.OnClick := {$ifdef FPC}@{$endif} ClickCloseProject;
  ButtonCloseDesign.OnClick := {$ifdef FPC}@{$endif} ClickCloseDesign;
  ButtonSaveDesign.OnClick := {$ifdef FPC}@{$endif} ClickSaveDesign;
  CheckboxShowHierarchy.OnChange := {$ifdef FPC}@{$endif} ChangeShowHierarchy;
  CheckboxShowProperties.OnChange := {$ifdef FPC}@{$endif} ChangeShowProperties;

  Hierarchy := TCastleComponentsHierarchy.Create(FreeAtStop);
  Hierarchy.ButtonHierarchyHide.Exists := false; // hide, not handled
  Hierarchy.WidthFraction := 0.25;
  Hierarchy.HeightFraction := 1;
  Hierarchy.OnSelect := {$ifdef FPC}@{$endif} HierarchySelect;
  ContainerDesignView.InsertFront(Hierarchy);

  Properties := TCastleComponentProperties.Create(FreeAtStop);
  Properties.ButtonPropertiesHide.Exists := false; // hide, not handled
  Properties.WidthFraction := 0.25;
  Properties.HeightFraction := 1;
  Properties.Anchor(hpRight);
  ContainerDesignView.InsertFront(Properties);

  FactoryButtonView.LoadFromComponent(ButtonViewTemplate);
  // note that ButtonViewTemplate children remain existing, doesn't matter
  FreeAndNil(ButtonViewTemplate);

  ListOpenExistingViewStr := TStringList.Create;
  ListViewsRefresh;

  DesignExistenceChanged;
  UpdateContainerLoadedDesignSize;

  ViewChooseExistingProject.AddRecentProject(ProjectManifestUrl);

  { override ApplicationData interpretation, and castle-data:/xxx URL,
    while this project is open. }
  ApplicationDataOverride := CombineUri(ProjectPathUrl, 'data/');
end;

procedure TViewProject.Stop;
begin
  FreeAndNil(ListOpenExistingViewStr);

  { Has to be done before ApplicationDataOverride:='', before we want
    to free things with the same ApplicationDataOverride,
    to make URL notification mechanism happy. }
  ClearDesign;

  ApplicationDataOverride := '';
  inherited;
end;

procedure TViewProject.ListOpenExistingViewAddFile(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  ListOpenExistingViewStr.Append(FileInfo.Url);
end;

procedure TViewProject.Resize;
var
  Ui: TCastleUserInterface;
begin
  inherited;
  // fix size of all children of ListOpenExistingView
  for Ui in ListOpenExistingView do
    Ui.Width := ScrollListOpenExistingView.EffectiveWidthForChildren;
end;

type
  TButtonViewDesign = class(TPersistent)
  published
    LabelViewName, LabelViewFile, LabelLastModified: TCastleLabel;
  end;

procedure TViewProject.ListViewsRefresh;

  function ShortDesignName(const S: String): String;
  begin
    Result := DeleteUriExt(ExtractUriName(S));
    Result := PrefixRemove('gameview', Result, true);
    Result := PrefixRemove('gamestate', Result, true);
    Result := SuffixRemove('.castle-user-interface', Result, true);
  end;

  function ExtractRelativeUrl(const BaseUrl, Url: String): String;
  var
    BaseFileName, FileName: String;
  begin
    { If both BaseUrl and Url are file URLs, then we can use ExtractRelativePath
      to get the relative path. }
    BaseFileName := UriToFilenameSafe(BaseUrl);
    FileName := UriToFilenameSafe(Url);
    if (BaseFileName <> '') and (FileName <> '') then
      Exit(ExtractRelativePath(BaseFileName, FileName));

    { Otherwise, simply use PrefixRemove.
      Don't ignore case, in case it mattered for these URLs. }
    if IsPrefix(BaseUrl, Url, false) then
      Result := PrefixRemove(BaseUrl, Url, false)
    else
      Result := Url;
  end;

var
  OpenDesignUrl, ProjectDataUrl: String;
  ButtonViewDesign: TButtonViewDesign;
  I: Integer;
  ButtonView: TCastleButton;
begin
  { calculate ListOpenExistingViewStr contents }
  ListOpenExistingViewStr.Clear;
  { Search in ProjectDataUrl, not ProjectPathUrl, as all designs should be part of data
    to be possible to open them at runtime.
    This also avoids finding stuff in castle-engine-output, which is possible,
    e.g. after "castle-engine package --target=android" the castle-engine-output contains
    some temporary data with copies of design files -- and we *do not* want to show them here. }
  ProjectDataUrl := CombineUri(ProjectPathUrl, 'data/');
  if UriExists(ProjectDataUrl) <> ueNotExists then
  begin
    FindFiles(ProjectDataUrl, 'gameview*.castle-user-interface', false, @ListOpenExistingViewAddFile, [ffRecursive]);
    // support deprecated names
    FindFiles(ProjectDataUrl, 'gamestate*.castle-user-interface', false, @ListOpenExistingViewAddFile, [ffRecursive]);
  end;
  { without sorting, the order would be ~random (as FindFiles enumarates).
    Note that we sort including the subdirectory names, which is good,
    we want files in the same subdirectory to be together. }
  ListOpenExistingViewStr.Sort;

  { copy ListOpenExistingViewStr contents -> ListOpenExistingView GUI contents }
  ListOpenExistingView.ClearControls; // TODO: also free items, use TCastleListBox
  for I := 0 to ListOpenExistingViewStr.Count -1 do
  begin
    OpenDesignUrl := ListOpenExistingViewStr[I];
    ButtonViewDesign := TButtonViewDesign.Create;
    try
      ButtonView := FactoryButtonView.ComponentLoad(FreeAtStop, ButtonViewDesign) as TCastleButton;
      ButtonView.Tag := I;
      ButtonView.OnClick := {$ifdef FPC}@{$endif} ClickOpenView;
      ButtonView.Width := ScrollListOpenExistingView.EffectiveWidthForChildren;
      ListOpenExistingView.InsertFront(ButtonView);

      ButtonViewDesign.LabelViewName.Caption := ShortDesignName(OpenDesignUrl);
      ButtonViewDesign.LabelViewFile.Caption := ExtractRelativeUrl(ProjectPathUrl, OpenDesignUrl);
      ButtonViewDesign.LabelLastModified.Caption := UrlDateTimeStr(OpenDesignUrl);
    finally FreeAndNil(ButtonViewDesign) end;
  end;
end;

procedure TViewProject.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewProject.ClickCloseProject(Sender: TObject);
begin
  Container.View := ViewChooseProject;
end;

procedure TViewProject.ClickCloseDesign(Sender: TObject);
begin
  ClearDesign;
  ListViewsRefresh;
  DesignExistenceChanged;
end;

procedure TViewProject.DesignExistenceChanged;
begin
  // ButtonCloseDesign.Exists := Design <> nil;
  // ButtonSaveDesign.Exists := Design <> nil;
  // ContainerDesignView.Exists := Design <> nil;
  // ContainerOpenView.Exists := Design = nil;
  ButtonCloseDesign.Exists := CurrentDesignUrl <> '';
  ButtonSaveDesign.Exists := CurrentDesignUrl <> '';
  ContainerDesignView.Exists := CurrentDesignUrl <> '';
  ContainerOpenView.Exists := CurrentDesignUrl = '';
end;

procedure TViewProject.ClickOpenView(Sender: TObject);
var
  OpenDesignUrl: String;
  ViewIndex: Integer;
begin
  ViewIndex := (Sender as TComponent).Tag;
  OpenDesignUrl := ListOpenExistingViewStr[ViewIndex];
  ProposeOpenDesign(OpenDesignUrl);
end;

procedure TViewProject.ClickSaveDesign(Sender: TObject);
begin
  UserInterfaceSave(DesignRoot, CurrentDesignUrl);
end;

procedure TViewProject.ClearDesign;
begin
  DesignRoot := nil;
  // this actually frees everything inside DesignRoot
  FreeAndNil(DesignOwner);
  CurrentDesignUrl := '';
end;

procedure TViewProject.ProposeOpenDesign(const OpenDesignUrl: String);
var
  NewDesignOwner: TComponent;
  NewDesignRoot: TCastleUserInterface;
begin
  { First load new design to local variables.
    Only if it succeeds, we will do ClearDesign and assign it to fields. }
  NewDesignOwner := TComponent.Create(Self);
  // TODO: allow opening other design types
  NewDesignRoot := UserInterfaceLoad(OpenDesignUrl, NewDesignOwner);

  ClearDesign;

  DesignOwner := NewDesignOwner;
  DesignRoot := NewDesignRoot;
  CurrentDesignUrl := OpenDesignUrl;
  Hierarchy.Root := DesignRoot;
  ContainerLoadedDesign.InsertFront(DesignRoot);

  DesignExistenceChanged;
end;

class function TViewProject.Selectable(const Child: TComponent): Boolean;
begin
  { Note: When changing conditions here, consider also updating ReasonWhyNotDeletable,
    that explains to user *why* something is not deletable (not being selectable
    also makes it not deletable). }

  { csTransient reason:

    Do not show in hierarchy the TCastleDesign loaded hierarchy,
    as it will not be saved.
    Same for TCastleCheckbox children.
    Consequently, do not allow to select stuff inside.

    However, show TCastleToolTransform, even though it is csTransient.
    We want to allow selecting joint tools.
  }
  // Define this to inspect all transformations, including internal (gizmos)
  {.$define EDITOR_DEBUG_TRANSFORMS}
  {$ifdef EDITOR_DEBUG_TRANSFORMS}
  Result := true;
  {$else}
  Result := (not (csTransient in Child.ComponentStyle)) or (Child is TCastleToolTransform);
  {$endif}
end;

function TViewProject.Deletable(const Child: TComponent): Boolean;
begin
  { Note: When changing conditions here, consider also updating ReasonWhyNotDeletable,
    that explains to user *why* something is not deletable. }

  Result := Selectable(Child) and
    (not (csSubComponent in Child.ComponentStyle)) and
    (Child <> DesignRoot) and (not (Child is TCastleToolTransform));
end;

procedure TViewProject.FreeComponentRecursively(const C: TComponent);

  procedure FreeNonVisualChildren(const C: TCastleComponent);
  var
    I: Integer;
  begin
    for I := C.NonVisualComponentsCount - 1 downto 0 do
      if Deletable(C.NonVisualComponents[I]) then
        FreeComponentRecursively(C.NonVisualComponents[I]);
  end;

  procedure FreeTransformChildren(const T: TCastleTransform);
  var
    I: Integer;
  begin
    for I := T.Count - 1 downto 0 do
      if Deletable(T[I]) then
        FreeComponentRecursively(T[I]);
  end;

  procedure FreeBehaviorChildren(const T: TCastleTransform);
  var
    I: Integer;
  begin
    for I := T.BehaviorsCount - 1 downto 0 do
      if Deletable(T.Behaviors[I]) then
        FreeComponentRecursively(T.Behaviors[I]);
  end;

  procedure FreeUiChildren(const C: TCastleUserInterface);
  var
    I: Integer;
  begin
    for I := C.ControlsCount - 1 downto 0 do
      if Deletable(C.Controls[I]) then
        FreeComponentRecursively(C.Controls[I]);
  end;

begin
  if not Deletable(C) then
    Exit;

  { Check this assertion after Deletable check, as it may be invalid
    e.g. for gizmos that are csTransient. }
  Assert(C.Owner = DesignOwner); // for now, castle-editor-portable doesn't have any DesignOwner

  if C is TCastleComponent then
  begin
    FreeNonVisualChildren(TCastleComponent(C));
    if C is TCastleTransform then
    begin
      FreeBehaviorChildren(TCastleTransform(C));
      FreeTransformChildren(TCastleTransform(C));
    end else
    if C is TCastleUserInterface then
    begin
      FreeUiChildren(TCastleUserInterface(C));
      if C is TCastleViewport then
      begin
        FreeBehaviorChildren(TCastleViewport(C).Items);
        FreeTransformChildren(TCastleViewport(C).Items);
      end;
    end;
  end;
  { Remove designing objects before delete behavior }
//  if C is TCastleBehavior then
//    TCastleBehavior(C).DesigningEnd;
  C.Free;

  //UpdateDesign; // for now, our hierarchy doesn't need it in castle-editor-portable
end;

procedure TViewProject.ChangeShowHierarchy(Sender: TObject);
begin
  Hierarchy.Exists := CheckboxShowHierarchy.Checked;
  UpdateContainerLoadedDesignSize;
end;

procedure TViewProject.ChangeShowProperties(Sender: TObject);
begin
  Properties.Exists := CheckboxShowProperties.Checked;
  UpdateContainerLoadedDesignSize;
end;

procedure TViewProject.UpdateContainerLoadedDesignSize;
var
  L, R: Boolean;
  WidthFractionFree: Single;
begin
  L := Hierarchy.Exists;
  R := Properties.Exists;

  WidthFractionFree := 1.0;
  if L then
    WidthFractionFree := WidthFractionFree - 0.25;
  if R then
    WidthFractionFree := WidthFractionFree - 0.25;
  ContainerLoadedDesign.WidthFraction := WidthFractionFree;

  if L and R then
    ContainerLoadedDesign.Anchor(hpMiddle)
  else
  if L then
    ContainerLoadedDesign.Anchor(hpRight)
  else
  if R then
    ContainerLoadedDesign.Anchor(hpLeft)
  else
    // otherwise it doesn't really matter
    ContainerLoadedDesign.Anchor(hpMiddle);
end;

function TViewProject.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;

  if Event.IsKey(keyLeftBracket) then
  begin
    CheckboxShowHierarchy.Checked := not CheckboxShowHierarchy.Checked;
    { Call the OnChange explicitly, because it is not automatically
      called when changing  Checked programmatically. }
    ChangeShowHierarchy(nil);
    Exit(true);
  end;

  if Event.IsKey(keyRightBracket) then
  begin
    CheckboxShowProperties.Checked := not CheckboxShowProperties.Checked;
    { Call the OnChange explicitly, because it is not automatically
      called when changing  Checked programmatically. }
    ChangeShowProperties(nil);
    Exit(true);
  end;
end;

procedure TViewProject.HierarchySelect(const Selected: TComponent);
begin
  Properties.SelectedComponent := Selected;
  Hierarchy.SelectComponent(Selected);
end;

end.
