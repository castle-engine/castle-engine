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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleFindFiles,
  EditorDesign;

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
    ContainerDesignView, ContainerOpenView: TCastleUserInterface;
    FactoryButtonView: TCastleComponentFactory;
    ListOpenExistingView: TCastleVerticalGroup;
    ScrollListOpenExistingView: TCastleUserInterface;
    CheckboxShowHierarchy: TCastleCheckbox;
    CheckboxShowProperties: TCastleCheckbox;
  private
    { Loaded design or @nil. }
    Design: TDesign;
    ListOpenExistingViewStr: TStringList;
    procedure ClickCloseProject(Sender: TObject);
    procedure ClickCloseDesign(Sender: TObject);
    procedure ClickSaveDesign(Sender: TObject);
    procedure ClickOpenView(Sender: TObject);
    procedure ChangeShowHierarchy(Sender: TObject);
    procedure ChangeShowProperties(Sender: TObject);
    procedure ListOpenExistingViewAddFile(const FileInfo: TFileInfo;
      var StopSearch: boolean);
    procedure ProposeOpenDesign(const OpenDesignUrl: String);
    procedure ListViewsRefresh;
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

  FactoryButtonView.LoadFromComponent(ButtonViewTemplate);
  // note that ButtonViewTemplate children remain existing, doesn't matter
  FreeAndNil(ButtonViewTemplate);

  ListOpenExistingViewStr := TStringList.Create;
  ListViewsRefresh;

  DesignExistenceChanged;

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
  FreeAndNil(Design);

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
  FreeAndNil(Design);
  ListViewsRefresh;
  DesignExistenceChanged;
end;

procedure TViewProject.DesignExistenceChanged;
begin
  ButtonCloseDesign.Exists := Design <> nil;
  ButtonSaveDesign.Exists := Design <> nil;
  ContainerDesignView.Exists := Design <> nil;
  ContainerOpenView.Exists := Design = nil;
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
  Design.SaveDesign;
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

  FreeAndNil(Design);
  Design := TDesign.Create(Self, NewDesignOwner, NewDesignRoot, OpenDesignUrl);
  Design.FullSize := true;
  ContainerDesignView.InsertFront(Design);

  DesignExistenceChanged;
end;

procedure TViewProject.ChangeShowHierarchy(Sender: TObject);
begin
  Design.HierarchyExists := CheckboxShowHierarchy.Checked;
end;

procedure TViewProject.ChangeShowProperties(Sender: TObject);
begin
  Design.PropertiesExists := CheckboxShowProperties.Checked;
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

end.
