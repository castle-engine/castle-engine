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
  ToolEditorUtils, ToolManifest,
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
    ContainerDesignView, ContainerOpenView, ToolbarWhenDesignExists: TCastleUserInterface;
    FactoryButtonView: TCastleComponentFactory;
    ListOpenExistingView: TCastleVerticalGroup;
    ScrollListOpenExistingView: TCastleUserInterface;
    CheckboxShowHierarchy: TCastleCheckbox;
    CheckboxShowProperties: TCastleCheckbox;
    ToolbarTransformManipulate: TCastleUserInterface;
    ButtonSelect, ButtonTranslate, ButtonRotate, ButtonScale: TCastleButton;
    LabelOpenExistingView: TCastleLabel;
  private
    { Loaded design or @nil. }
    Design: TDesign;
    DesignToolbar: TDesignToolbar;
    ProjectViews: TProjectViewList;
    ProjectManifest: TCastleManifest;
    procedure ClickCloseProject(Sender: TObject);
    procedure ClickCloseDesign(Sender: TObject);
    procedure ClickOpenView(Sender: TObject);
    procedure ProposeOpenDesign(const OpenDesignUrl: String);
    procedure ProjectViewsRefresh;
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
  CastleTransform, CastleLog,
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

  DesignToolbar := TDesignToolbar.Create;
  DesignToolbar.ButtonSaveDesign := ButtonSaveDesign;
  DesignToolbar.CheckboxShowHierarchy := CheckboxShowHierarchy;
  DesignToolbar.CheckboxShowProperties := CheckboxShowProperties;
  DesignToolbar.ToolbarTransformManipulate := ToolbarTransformManipulate;
  DesignToolbar.ButtonSelect := ButtonSelect;
  DesignToolbar.ButtonTranslate := ButtonTranslate;
  DesignToolbar.ButtonRotate := ButtonRotate;
  DesignToolbar.ButtonScale := ButtonScale;

  FactoryButtonView.LoadFromComponent(ButtonViewTemplate);
  // note that ButtonViewTemplate children remain existing, doesn't matter
  FreeAndNil(ButtonViewTemplate);

  ProjectManifest := TCastleManifest.CreateFromUrl(ProjectManifestUrl);

  ProjectViews := TProjectViewList.Create;
  ProjectViewsRefresh;

  DesignExistenceChanged;

  ViewChooseExistingProject.AddRecentProject(ProjectManifestUrl);

  { override ApplicationData interpretation, and castle-data:/xxx URL,
    while this project is open. }
  ApplicationDataOverride := CombineUri(ProjectPathUrl, 'data/');
end;

procedure TViewProject.Stop;
begin
  FreeAndNil(ProjectViews);

  { Has to be done before ApplicationDataOverride:='', before we want
    to free things with the same ApplicationDataOverride,
    to make URL notification mechanism happy. }
  FreeAndNil(Design);

  FreeAndNil(DesignToolbar);
  FreeAndNil(ProjectManifest);

  ApplicationDataOverride := '';
  inherited;
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

procedure TViewProject.ProjectViewsRefresh;
var
  View: TProjectView;
  ButtonViewDesign: TButtonViewDesign;
  I: Integer;
  ButtonView: TCastleButton;
  MaskSearched: String;
begin
  ProjectViews.ScanProject(ProjectPathUrl, ProjectManifest.ProposedUnitPrefix,
    MaskSearched);

  // update LabelOpenExistingView to show mask used for views
  LabelOpenExistingView.Caption := 'Open Existing View (' + MaskSearched + ') :';

  { copy ProjectViews contents -> ListOpenExistingView GUI contents }
  ListOpenExistingView.ClearControls; // TODO: also free items, use TCastleListBox
  for I := 0 to ProjectViews.Count -1 do
  begin
    View := ProjectViews[I];
    ButtonViewDesign := TButtonViewDesign.Create;
    try
      ButtonView := FactoryButtonView.ComponentLoad(FreeAtStop, ButtonViewDesign) as TCastleButton;
      ButtonView.Tag := I;
      ButtonView.OnClick := {$ifdef FPC}@{$endif} ClickOpenView;
      ButtonView.Width := ScrollListOpenExistingView.EffectiveWidthForChildren;
      ListOpenExistingView.InsertFront(ButtonView);

      ButtonViewDesign.LabelViewName.Caption := View.Name;
      ButtonViewDesign.LabelViewFile.Caption := View.Path;
      ButtonViewDesign.LabelLastModified.Caption := View.LastModified;
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
  ProjectViewsRefresh;
  DesignExistenceChanged;
end;

procedure TViewProject.DesignExistenceChanged;
begin
  ToolbarWhenDesignExists.Exists := Design <> nil;
  ContainerDesignView.Exists := Design <> nil;
  ContainerOpenView.Exists := Design = nil;
end;

procedure TViewProject.ClickOpenView(Sender: TObject);
var
  OpenDesignUrl: String;
  ViewIndex: Integer;
begin
  ViewIndex := (Sender as TComponent).Tag;
  OpenDesignUrl := ProjectViews[ViewIndex].Url;
  ProposeOpenDesign(OpenDesignUrl);
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
  Design := TDesign.Create(Self, DesignToolbar,
    NewDesignOwner, NewDesignRoot, OpenDesignUrl);
  Design.FullSize := true;
  ContainerDesignView.InsertFront(Design);

  DesignExistenceChanged;
end;

function TViewProject.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;

  if Event.IsKey(CtrlW) then
  begin
    ClickCloseDesign(nil);
    Exit(true);
  end;

  if (Design <> nil) and
     (not Container.Focus.Contains(Design)) then
  begin
    WritelnLog('Passing input to design even though it is not focused: %s', [
      Event.ToString
    ]);
    if Design.Press(Event) then
      Exit(true);
  end;
end;

end.
