{
  Copyright 2015-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Inspect Castle Game Engine state at runtime (TCastleInspector).
  Invoke this automatically in debug builds by F12 (see @link(TCastleContainer.InspectorKey)). }
unit CastleInternalInspector;

{$I castleconf.inc}

interface

uses Classes,
  CastleControls, CastleUIControls, CastleColors, CastleRectangles,
  CastleVectors, CastleKeysMouse, CastleComponentSerialize;

type
  { Inspect Castle Game Engine state.
    Show log, current UI and viewports state.
    Invoke this automatically in debug builds by F12 (see @link(TCastleContainer.InspectorKey)). }
  TCastleInspector = class(TCastleUserInterfaceFont)
  strict private
    const
      ProfilerDataCount = 100;
    type
      TProfilerMetric = (pmUpdate, pmPhysics, pmRender, pmSwap);

      TProfilerDataItem = record
        { Frame time, in 0..1 range (how much of frame do they occupy. }
        Time: array [TProfilerMetric] of Single;
        Fps: Single;
      end;
      PProfilerDataItem = ^TProfilerDataItem;

      TProfilerData = array [0..ProfilerDataCount - 1] of TProfilerDataItem;

      { This type is local for ProfilerGraphRender, but it has to be declared here
        to compile, otherwise FPC 3.2.0 doesn't handle TProfilerMetric reference. }

    var
      { Controls loaded from inspector_ui.castle-user-interface.inc }
      CheckboxShowEvenInternal: TCastleCheckbox;
      RectOptions, RectProperties, RectLog, RectHierarchy, RectProfiler: TCastleRectangleControl;
      ButtonHierarchyShow, ButtonHierarchyHide,
        ButtonLogShow, ButtonLogHide,
        ButtonPropertiesShow, ButtonPropertiesHide,
        ButtonProfilerShow, ButtonProfilerHide: TCastleButton;
      HorizontalGroupShow: TCastleUserInterface;
      HierarchyRowParent: TCastleUserInterface;
      PropertyRowParent: TCastleUserInterface;
      ScrollLogs: TCastleScrollView;
      LogsVerticalGroup: TCastleVerticalGroup;
      LabelEarlierLogsRemoved: TCastleLabel;
      LabelLogHeader: TCastleLabel;
      LabelPropertiesHeader: TCastleLabel;
      LabelInspectorHelp: TCastleLabel;
      SliderOpacity: TCastleFloatSlider;
      ButtonLogClear: TCastleButton;
      CheckboxLogAutoScroll: TCastleCheckbox;
      LabelProfilerHeader: TCastleLabel;
      CheckboxProfilerDetailsInLog: TCastleCheckbox;
      ProfilerGraph: TCastleUserInterface;

      FOpacity: Single;
      FSelectedComponent: TComponent;
      InsideLogCallback: Boolean;
      SerializedHierarchyRowTemplate: TSerializedComponent;
      SerializedPropertyRowTemplate: TSerializedComponent;
      ProfilerData: TProfilerData;
      ProfilerDataNonEmpty: Boolean;
      { We have useful data at ProfilerDataFirst...ProfilerDataLast-1 indexes,
        if ProfilerDataNonEmpty.
        So ProfilerDataNonEmpty and (ProfilerDataLast = ProfilerDataFirst) means that queue is full.

        When ProfilerDataLast < ProfilerDataFirst, then it wraps modulo ProfilerDataCount.
        Both ProfilerDataFirst and ProfilerDataLast are always between 0 and ProfilerDataCount-1. }
      ProfilerDataFirst, ProfilerDataLast: Integer;
      LogCount: Cardinal;

    procedure ChangeOpacity(Sender: TObject);
    procedure SetOpacity(const Value: Single);
    { Show component in hierarchy. }
    function ComponentShow(const C: TComponent): Boolean;
    procedure ClickHierarchyShow(Sender: TObject);
    procedure ClickHierarchyHide(Sender: TObject);
    procedure ClickLogShow(Sender: TObject);
    procedure ClickLogHide(Sender: TObject);
    procedure ClickPropertiesShow(Sender: TObject);
    procedure ClickPropertiesHide(Sender: TObject);
    procedure ClickProfilerShow(Sender: TObject);
    procedure ClickProfilerHide(Sender: TObject);
    procedure ClickLogClear(Sender: TObject);
    procedure ChangeProfilerDetailsInLog(Sender: TObject);
    { Synchronize state of HorizontalGroupShow and its children with the existence of rectangles
      like RectHierarchy. So you only need to change RectHierarchy.Exists and call this method
      to have UI consistent. }
    procedure SynchronizeButtonsToShow;
    procedure LogCallback(const Message: String);
    procedure SetSelectedComponent(const Value: TComponent);
    { Used to display C as String throughout the UI. }
    function ComponentCaption(const C: TComponent; const Level: Integer): String;
    procedure UpdateHierarchy(Sender: TObject);
    procedure ClickHierarchyRow(Sender: TObject);
    { Update properties to reflect current FSelectedComponent.
      Only SetSelectedComponent needs to call it. }
    procedure UpdateProperties;
    procedure ProfilerSummaryAvailable(Sender: TObject);
    procedure ProfilerGraphRender(const Sender: TCastleUserInterface);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    const
      DefaultOpacity = 0.9;
    type
      TPersistentState = record
        Opacity: Single;
        RectPropertiesExists: Boolean;
        RectLogExists: Boolean;
        RectProfilerExists: Boolean;
        RectHierarchyExists: Boolean;
      end;
    class var
      { Class variables, to save across all inspector instances. }
      PersistentState: TPersistentState;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;  var HandleInput: boolean); override;
    procedure Resize; override;

    { Opacity of the whole UI.
      Can be changed by user while operating this UI. }
    property Opacity: Single read FOpacity write SetOpacity;

    { Selected object in hierarchy, for which we display the properties.
      Can be changed by user while operating this UI. }
    property SelectedComponent: TComponent read FSelectedComponent write SetSelectedComponent;
  published
    property KeepInFront stored false;
    property FullSize stored false;
  end;

implementation

uses SysUtils, StrUtils,
  CastleStringUtils, CastleGLUtils, CastleApplicationProperties, CastleClassUtils,
  CastleTransform, CastleViewport, CastleScene, CastleURIUtils, CastleTimeUtils,
  CastleUtils;

{ TODO:

  Allow selecting components in game view, following focused

  Show size as extra property - (C.EffectiveRect - like in CGE editor)

  Show C.Focused marked by some border of button on hierarchy

  Show historic logs immediately, instead of starting LabelLog always empty.
  Just keep last X logs in DEBUG build always?

  Animate RectXxx existence changes.

  untangle dependencies: it is a bit dirty that CastleUIControls, basic unit,
  uses this unit -- which includes everything, like TCastleViewport.
  CastleUIControls should instead expose InternalInspectorClass that this unit would set.

    Hm, but then how to make sure this is always used?
    This unit will have to be used by CastleWindow, CastleControl...
    again creating a bigger dependency than needed.
    Would be best if this unit didn't need some, at least no CastleViewport, CastleScene.

  checkbox to also show X3D nodes
}

constructor TCastleInspector.Create(AOwner: TComponent);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
  HierarchyRowTemplate: TCastleButton;
  PropertyRowTemplate: TCastleUserInterface;
begin
  inherited;

  // adjust inherited published properties
  FullSize := true;
  KeepInFront := true;

  UiOwner := TComponent.Create(Self);
  Ui := StringToComponent({$I ui/inspector_ui.castle-user-interface.inc}, UiOwner)
    as TCastleUserInterface;
  InsertFront(Ui);

  CheckboxShowEvenInternal := UiOwner.FindRequiredComponent('CheckboxShowEvenInternal') as TCastleCheckbox;
  RectOptions := UiOwner.FindRequiredComponent('RectOptions') as TCastleRectangleControl;
  RectProperties := UiOwner.FindRequiredComponent('RectProperties') as TCastleRectangleControl;
  RectLog := UiOwner.FindRequiredComponent('RectLog') as TCastleRectangleControl;
  RectHierarchy := UiOwner.FindRequiredComponent('RectHierarchy') as TCastleRectangleControl;
  RectProfiler := UiOwner.FindRequiredComponent('RectProfiler') as TCastleRectangleControl;
  ButtonHierarchyShow := UiOwner.FindRequiredComponent('ButtonHierarchyShow') as TCastleButton;
  ButtonHierarchyHide := UiOwner.FindRequiredComponent('ButtonHierarchyHide') as TCastleButton;
  ButtonLogShow := UiOwner.FindRequiredComponent('ButtonLogShow') as TCastleButton;
  ButtonLogHide := UiOwner.FindRequiredComponent('ButtonLogHide') as TCastleButton;
  ButtonPropertiesShow := UiOwner.FindRequiredComponent('ButtonPropertiesShow') as TCastleButton;
  ButtonPropertiesHide := UiOwner.FindRequiredComponent('ButtonPropertiesHide') as TCastleButton;
  ButtonProfilerShow := UiOwner.FindRequiredComponent('ButtonProfilerShow') as TCastleButton;
  ButtonProfilerHide := UiOwner.FindRequiredComponent('ButtonProfilerHide') as TCastleButton;
  HorizontalGroupShow := UiOwner.FindRequiredComponent('HorizontalGroupShow') as TCastleUserInterface;
  HierarchyRowTemplate := UiOwner.FindRequiredComponent('HierarchyRowTemplate') as TCastleButton;
  PropertyRowTemplate := UiOwner.FindRequiredComponent('PropertyRowTemplate') as TCastleUserInterface;
  HierarchyRowParent := UiOwner.FindRequiredComponent('HierarchyRowParent') as TCastleUserInterface;
  PropertyRowParent := UiOwner.FindRequiredComponent('PropertyRowParent') as TCastleUserInterface;
  ScrollLogs := UiOwner.FindRequiredComponent('ScrollLogs') as TCastleScrollView;
  LogsVerticalGroup := UiOwner.FindRequiredComponent('LogsVerticalGroup') as TCastleVerticalGroup;
  LabelEarlierLogsRemoved := UiOwner.FindRequiredComponent('LabelEarlierLogsRemoved') as TCastleLabel;
  LabelLogHeader := UiOwner.FindRequiredComponent('LabelLogHeader') as TCastleLabel;
  LabelPropertiesHeader := UiOwner.FindRequiredComponent('LabelPropertiesHeader') as TCastleLabel;
  LabelInspectorHelp := UiOwner.FindRequiredComponent('LabelInspectorHelp') as TCastleLabel;
  SliderOpacity := UiOwner.FindRequiredComponent('SliderOpacity') as TCastleFloatSlider;
  ButtonLogClear := UiOwner.FindRequiredComponent('ButtonLogClear') as TCastleButton;
  CheckboxLogAutoScroll := UiOwner.FindRequiredComponent('CheckboxLogAutoScroll') as TCastleCheckbox;
  LabelProfilerHeader := UiOwner.FindRequiredComponent('LabelProfilerHeader') as TCastleLabel;
  CheckboxProfilerDetailsInLog := UiOwner.FindRequiredComponent('CheckboxProfilerDetailsInLog') as TCastleCheckbox;
  ProfilerGraph := UiOwner.FindRequiredComponent('ProfilerGraph') as TCastleUserInterface;

  ForceFallbackLook(Ui);

  CheckboxShowEvenInternal.OnChange := {$ifdef FPC}@{$endif} UpdateHierarchy;
  ButtonHierarchyShow.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyShow;
  ButtonHierarchyHide.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyHide;
  ButtonLogShow.OnClick := {$ifdef FPC}@{$endif} ClickLogShow;
  ButtonLogHide.OnClick := {$ifdef FPC}@{$endif} ClickLogHide;
  ButtonPropertiesShow.OnClick := {$ifdef FPC}@{$endif} ClickPropertiesShow;
  ButtonPropertiesHide.OnClick := {$ifdef FPC}@{$endif} ClickPropertiesHide;
  ButtonProfilerShow.OnClick := {$ifdef FPC}@{$endif} ClickProfilerShow;
  ButtonProfilerHide.OnClick := {$ifdef FPC}@{$endif} ClickProfilerHide;
  ButtonLogClear.OnClick := {$ifdef FPC}@{$endif} ClickLogClear;
  CheckboxProfilerDetailsInLog.OnChange := {$ifdef FPC}@{$endif} ChangeProfilerDetailsInLog;
  ProfilerGraph.OnRender := {$ifdef FPC}@{$endif} ProfilerGraphRender;

  { initial state of UI to show/hide }
  RectProperties.Exists := PersistentState.RectPropertiesExists;
  RectLog.Exists := PersistentState.RectLogExists;
  RectHierarchy.Exists := PersistentState.RectHierarchyExists;
  RectProfiler.Exists := PersistentState.RectProfilerExists;
  SynchronizeButtonsToShow;

  FOpacity := -1; // to force setting below call SetOpacity
  Opacity := PersistentState.Opacity;
  SliderOpacity.Value := Opacity;
  SliderOpacity.OnChange := {$ifdef FPC}@{$endif} ChangeOpacity;

  { initialize templates }
  SerializedHierarchyRowTemplate := TSerializedComponent.CreateFromString(ComponentToString(HierarchyRowTemplate));
  FreeAndNil(HierarchyRowTemplate);
  SerializedPropertyRowTemplate := TSerializedComponent.CreateFromString(ComponentToString(PropertyRowTemplate));
  FreeAndNil(PropertyRowTemplate);

  { initialize log }
  ApplicationProperties.OnLog.Add({$ifdef FPC}@{$endif} LogCallback);
  LogCount := 0;
  LabelLogHeader.Caption := Format('Log (%d)', [LogCount]);
  CheckboxLogAutoScroll.Checked := true;

  { initialize profiler }
  FrameProfiler.Enabled := RectProfiler.Exists;
  FrameProfiler.OnSummaryAvailable := {$ifdef FPC}@{$endif} ProfilerSummaryAvailable;
  // FrameProfiler.FramesForSummary := 2; // useful to quickly test
  CheckboxProfilerDetailsInLog.Checked := FrameProfiler.LogSummary;
end;

destructor TCastleInspector.Destroy;
begin
  ApplicationProperties.OnLog.Remove({$ifdef FPC}@{$endif} LogCallback);

  { set to nil by SetSelectedComponent, to detach free notification }
  SelectedComponent := nil;

  { update PersistentState }
  PersistentState.Opacity := Opacity;
  PersistentState.RectPropertiesExists := RectProperties.Exists;
  PersistentState.RectLogExists := RectLog.Exists;
  PersistentState.RectHierarchyExists := RectHierarchy.Exists;
  PersistentState.RectProfilerExists := RectProfiler.Exists;

  FrameProfiler.OnSummaryAvailable := nil;
  FrameProfiler.Enabled := false;

  inherited;
end;

const
  SLevelPrefix = '- ';

function TCastleInspector.ComponentCaption(const C: TComponent; const Level: Integer): String;
begin
  Result := DupeString(SLevelPrefix, Level) + C.Name + ' (' + C.ClassName + ')';
end;

function TCastleInspector.ComponentShow(const C: TComponent): Boolean;
begin
  { Never show Self,
    this would cause problems as we'll create HierarchyRow to show HierarchyRow instances... }
  if C = Self then Exit(false);

  Result := (not (csTransient in C.ComponentStyle)) or CheckboxShowEvenInternal.Checked;
end;

procedure TCastleInspector.UpdateHierarchy(Sender: TObject);

{ Parts of this are deliberately consistent with TDesignFrame.UpdateDesign. }

var
  RowIndex: Integer;

  procedure AddHierarchyEntry(const Caption: String; const C: TComponent);
  var
    HierarchyButton: TCastleButton;
  begin
    if RowIndex < HierarchyRowParent.ControlsCount then
    begin
      HierarchyButton := HierarchyRowParent.Controls[RowIndex] as TCastleButton;
    end else
    begin
      HierarchyButton := SerializedHierarchyRowTemplate.ComponentLoad(Self) as TCastleButton;
      ForceFallbackLook(HierarchyButton);
      HierarchyButton.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyRow;
      HierarchyButton.Culling := true; // many such buttons are often not visible, in scroll view
      HierarchyButton.Width := RectHierarchy.EffectiveWidthForChildren;
      HierarchyRowParent.InsertFront(HierarchyButton);
    end;

    HierarchyButton.Caption := Caption;
    HierarchyButton.Tag := PtrUInt(C);

    Inc(RowIndex);
  end;

  { Add given component, and its children in C.NonVisualComponents }
  procedure AddNonVisualComponent(const C: TComponent; const Level: Integer);
  var
    Child: TComponent;
  begin
    AddHierarchyEntry(ComponentCaption(C, Level), C);

    if C is TCastleComponent then
    begin
      for Child in TCastleComponent(C).NonVisualComponentsEnumerate do
        if ComponentShow(Child) then
          AddNonVisualComponent(Child, Level + 1);
    end;
  end;

  { If C has some NonVisualComponents, then create a tree item
    'Non-Visual Components' and add them to it. }
  procedure AddNonVisualComponentsSection(const C: TCastleComponent; const Level: Integer);
  var
    Child: TComponent;
  begin
    if C.NonVisualComponentsCount <> 0 then
    begin
      AddHierarchyEntry(DupeString(SLevelPrefix, Level) + 'Non-Visual Components', C);
      for Child in C.NonVisualComponentsEnumerate do
        if ComponentShow(Child) then
          AddNonVisualComponent(Child, Level + 1);
    end;
  end;

  { If T has some Behaviors, then create a tree item
    'Behaviors' and add them to it. }
  procedure AddBehaviorsSection(const T: TCastleTransform; const Level: Integer);
  var
    Child: TCastleBehavior;
  begin
    if T.BehaviorsCount <> 0 then
    begin
      AddHierarchyEntry(DupeString(SLevelPrefix, Level) + 'Behaviors', T);
      for Child in T.BehaviorsEnumerate do
        if ComponentShow(Child) then
          AddNonVisualComponent(Child, Level + 1);
    end;
  end;

  { Add given transform, and its children
    (transform children, T.NonVisualComponents, T.Behaviors). }
  procedure AddTransform(const T: TCastleTransform; const Level: Integer);
  var
    I: Integer;
  begin
    AddHierarchyEntry(ComponentCaption(T, Level), T);

    AddNonVisualComponentsSection(T, Level + 1);
    AddBehaviorsSection(T, Level + 1);

    for I := 0 to T.Count - 1 do
      if ComponentShow(T[I]) then
        AddTransform(T[I], Level + 1);
  end;

  { Add given UI control, and its children. }
  procedure AddControl(const C: TCastleUserInterface; const Level: Integer);
  var
    I: Integer;
    Viewport: TCastleViewport;
  begin
    AddHierarchyEntry(ComponentCaption(C, Level), C);

    AddNonVisualComponentsSection(C, Level + 1);

    for I := 0 to C.ControlsCount - 1 do
    begin
      if ComponentShow(C.Controls[I]) then
        AddControl(C.Controls[I], Level + 1);
    end;

    if C is TCastleViewport then
    begin
      Viewport := TCastleViewport(C);
      if ComponentShow(Viewport.Items) then
        AddTransform(Viewport.Items, Level + 1);
    end;
  end;

var
  C: TCastleUserInterface;
  I: Integer;
begin
  { To avoid recreating lots of UI controls each frame,
    we use RowIndex and AddHierarchyEntry to preserve previous instances of HierarchyRowTemplate
    across update calls, if possible.
    In effect, usual call to UpdateHierarchy should be mostly an analysis that says "nothing has
    changed, so not updating any UI". }
  RowIndex := 0;

  for I := 0 to Container.Controls.Count - 1 do
  begin
    C := Container.Controls[I];
    if ComponentShow(C) then
      AddControl(C, 0);
  end;

  while HierarchyRowParent.ControlsCount > RowIndex do
    HierarchyRowParent.Controls[RowIndex].Free;
end;

procedure TCastleInspector.Update(const SecondsPassed: Single;  var HandleInput: boolean);
begin
  inherited;
  UpdateHierarchy(nil);

  LabelProfilerHeader.Caption := 'Profiler | Current FPS: ' + Container.Fps.ToString;

  LabelInspectorHelp.Exists := Container.InspectorKey <> keyNone;
  if LabelInspectorHelp.Exists then
    LabelInspectorHelp.Caption := 'Press ' + KeyToStr(Container.InspectorKey) + ' to hide inspector';
end;

procedure TCastleInspector.ChangeOpacity(Sender: TObject);
begin
  Opacity := SliderOpacity.Value;
end;

procedure TCastleInspector.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    RectOptions.ColorPersistent.Alpha := Value;
    RectProperties.ColorPersistent.Alpha := Value;
    RectLog.ColorPersistent.Alpha := Value;
    RectProfiler.ColorPersistent.Alpha := Value;
    RectHierarchy.ColorPersistent.Alpha := Value;
  end;
end;

procedure TCastleInspector.SynchronizeButtonsToShow;
begin
  ButtonLogShow.Exists := not RectLog.Exists;
  ButtonPropertiesShow.Exists := not RectProperties.Exists;
  ButtonHierarchyShow.Exists := not RectHierarchy.Exists;
  ButtonProfilerShow.Exists := not RectProfiler.Exists;
  HorizontalGroupShow.Exists :=
    ButtonHierarchyShow.Exists or
    ButtonLogShow.Exists or
    ButtonPropertiesShow.Exists or
    ButtonProfilerShow.Exists;
end;

procedure TCastleInspector.ClickHierarchyShow(Sender: TObject);
begin
  RectHierarchy.Exists := true;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickHierarchyHide(Sender: TObject);
begin
  RectHierarchy.Exists := false;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickLogShow(Sender: TObject);
begin
  RectLog.Exists := true;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickLogHide(Sender: TObject);
begin
  RectLog.Exists := false;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickPropertiesShow(Sender: TObject);
begin
  RectProperties.Exists := true;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickPropertiesHide(Sender: TObject);
begin
  RectProperties.Exists := false;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickProfilerShow(Sender: TObject);
begin
  RectProfiler.Exists := true;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickProfilerHide(Sender: TObject);
begin
  RectProfiler.Exists := false;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.LogCallback(const Message: String);
const
  { Do not keep too many logs, to not slow down rendering. }
  MaxLogs = 100;
var
  NewLabelLog: TCastleLabel;
begin
  { Use InsideLogCallback to prevent from infinite recursion,
    in case anything inside would also cause WritelnLog. }
  if InsideLogCallback then Exit;

  InsideLogCallback := true;
  try
    if LogsVerticalGroup.ControlsCount = MaxLogs then
    begin
      LogsVerticalGroup.Controls[1].Free; // free 1st, leave LabelEarlierLogsRemoved
      LabelEarlierLogsRemoved.Exists := true;
    end;

    { Creating new TCastleLabel for each log is more efficient than stuffing all logs
      into one big TCastleLabel, tested (with "Log Details" of profiler). }
    NewLabelLog := TCastleLabel.Create(Self);
    NewLabelLog.Text.AddMultiLine(TrimEndingNewline(Message));
    NewLabelLog.Culling := true; // makes rendering much faster in case of multiple logs
    LogsVerticalGroup.InsertFront(NewLabelLog);

    Inc(LogCount);
    LabelLogHeader.Caption := Format('Log (%d)', [LogCount]);
    if CheckboxLogAutoScroll.Checked then
      ScrollLogs.Scroll := ScrollLogs.ScrollMax;
  finally InsideLogCallback := false end;
end;

procedure TCastleInspector.ClickLogClear(Sender: TObject);
begin
  LogCount := 0;
  LabelLogHeader.Caption := Format('Log (%d)', [LogCount]);
  LogsVerticalGroup.ClearControls;
  LabelEarlierLogsRemoved.Exists := false;
end;

procedure TCastleInspector.ClickHierarchyRow(Sender: TObject);
begin
  SelectedComponent := TComponent(Pointer((Sender as TCastleButton).Tag));
end;

procedure TCastleInspector.SetSelectedComponent(const Value: TComponent);
var
  HierarchyRow: TCastleUserInterface;
  HierachyRowButton: TCastleButton;
begin
  if FSelectedComponent <> Value then
  begin
    if FSelectedComponent <> nil then
      FSelectedComponent.RemoveFreeNotification(Self);
    FSelectedComponent := Value;
    if FSelectedComponent <> nil then
      FSelectedComponent.FreeNotification(Self);

    for HierarchyRow in HierarchyRowParent do
    begin
      HierachyRowButton := HierarchyRow as TCastleButton;
      HierachyRowButton.Pressed := TComponent(Pointer(HierachyRowButton.Tag)) = FSelectedComponent;
    end;

    if FSelectedComponent <> nil then
      LabelPropertiesHeader.Caption := 'Properties - ' + ComponentCaption(FSelectedComponent, 0)
    else
      LabelPropertiesHeader.Caption := 'Properties';

    UpdateProperties;
  end;
end;

procedure TCastleInspector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSelectedComponent) then
    { set to nil by SetSelectedComponent to clean nicely }
    SelectedComponent := nil;
end;

procedure TCastleInspector.UpdateProperties;

  procedure AddPropertyRow(const PropNameStr, PropValueStr: String);
  var
    PropertyOwner: TComponent;
    Ui: TCastleUserInterface;
    PropName: TCastleLabel;
    PropValue: TCastleEdit;
  begin
    // TODO: We will create lots of PropertyOwner and never free them, until you close the TCastleInspector

    PropertyOwner := TComponent.Create(Self);
    Ui := SerializedPropertyRowTemplate.ComponentLoad(PropertyOwner) as TCastleUserInterface;
    ForceFallbackLook(Ui);
    Ui.Culling := true; // many such rows are often not visible, in scroll view
    Ui.Width := RectProperties.EffectiveWidthForChildren;
    PropertyRowParent.InsertFront(Ui);

    PropName := PropertyOwner.FindRequiredComponent('PropName') as TCastleLabel;
    PropValue := PropertyOwner.FindRequiredComponent('PropValue') as TCastleEdit;

    PropName.Caption := PropNameStr;
    // TODO: editing PropValue has no effect now
    PropValue.Text := PropValueStr;
  end;

begin
  while PropertyRowParent.ControlsCount > 0 do
    PropertyRowParent.Controls[0].Free;

  // TODO: hardcoded properties for now, use RTTI instead to get all
  if FSelectedComponent <> nil then
  begin
    AddPropertyRow('Name', FSelectedComponent.Name);
    if FSelectedComponent is TCastleUserInterface then
    begin
      AddPropertyRow('Exists', BoolToStr(TCastleUserInterface(FSelectedComponent).Exists, true));
    end;
    if FSelectedComponent is TCastleTransform then
    begin
      AddPropertyRow('Translation', TCastleTransform(FSelectedComponent).Translation.ToString);
      AddPropertyRow('Rotation', TCastleTransform(FSelectedComponent).Rotation.ToString);
      AddPropertyRow('Scale', TCastleTransform(FSelectedComponent).Scale.ToString);
      if FSelectedComponent is TCastleScene then
      begin
        AddPropertyRow('URL', URIDisplay(TCastleScene(FSelectedComponent).URL));
      end;
    end;
  end;
end;

procedure TCastleInspector.Resize;
var
  C: TCastleUserInterface;
begin
  inherited;

  { We need to manually adjust rows widths to follow available space,
    righ now this is not doable by simple properties,
    because ScrollArea looks at children for size (and we cannot look
    at children for height, but at the same time determine children width). }
  for C in HierarchyRowParent do
    C.Width := RectHierarchy.EffectiveWidthForChildren;
  for C in PropertyRowParent do
    C.Width := RectProperties.EffectiveWidthForChildren;
end;

procedure TCastleInspector.ChangeProfilerDetailsInLog(Sender: TObject);
begin
  FrameProfiler.LogSummary := CheckboxProfilerDetailsInLog.Checked;
end;

procedure TCastleInspector.ProfilerSummaryAvailable(Sender: TObject);
var
  Data: PProfilerDataItem;
  TotalTime: TFloatTime;
begin
  { Container = nil may happen if TCastleInspector exists,
    but is no longer on the Window.Controls, e.g. if you invoke Window.FileDialog. }
  if Container = nil then
    Exit;

  Data := @ProfilerData[ProfilerDataLast];

  TotalTime := FrameProfiler.SummaryTotalFrameTime;

  // fmRender includes fmRenderSwapFlush, we make them separate for display
  Data^.Time[pmRender] := (FrameProfiler.SummaryTime(fmRender) - FrameProfiler.SummaryTime(fmRenderSwapFlush)) / TotalTime;
  Data^.Time[pmSwap] := FrameProfiler.SummaryTime(fmRenderSwapFlush) / TotalTime;
  // fmUpdate includes fmUpdatePhysics, we make them separate for display
  Data^.Time[pmUpdate] := (FrameProfiler.SummaryTime(fmUpdate) - FrameProfiler.SummaryTime(fmUpdatePhysics)) / TotalTime;
  Data^.Time[pmPhysics] := FrameProfiler.SummaryTime(fmUpdatePhysics) / TotalTime;

  Data^.Fps := Container.Fps.RealFps;

  if ProfilerDataNonEmpty and (ProfilerDataLast = ProfilerDataFirst) then
    ProfilerDataFirst := (ProfilerDataFirst + 1) mod ProfilerDataCount;
  ProfilerDataLast := (ProfilerDataLast + 1) mod ProfilerDataCount;
  ProfilerDataNonEmpty := true;
end;

procedure TCastleInspector.ProfilerGraphRender(const Sender: TCastleUserInterface);
const
  Colors: array [TProfilerMetric] of String = (
    'FFFF00',
    '7A7A00',
    '6767FF',
    'B1B1FF'
  );
  ColorFpsHex = '00B300';
type
  TTimeSum = array [TProfilerMetric] of Single;
  TProfilerItemDraw = record
    { For each metric: sum of time of this metric + lower metrics, in 0..1. }
    TimeSum: TTimeSum;
    { From 0 upward. }
    Index: Integer;
    { From ProfilerDataFirst to ProfilerDataLast-1. }
    DataIndex: Integer;
  end;
  PProfilerItemDraw = ^TProfilerItemDraw;

  { Calcualate ItemDraw.XxxY based on ProfilerData[ItemDraw.DataIndex]. }
  procedure CalculateItemDraw(var ItemDraw: TProfilerItemDraw);
  var
    Data: PProfilerDataItem;
    TimeSum: Single;
    Metric: TProfilerMetric;
  begin
    Data := @ProfilerData[ItemDraw.DataIndex];
    TimeSum := 0;
    for Metric := High(TProfilerMetric) downto Low(TProfilerMetric) do
    begin
      TimeSum := TimeSum + Data^.Time[Metric];
      ItemDraw.TimeSum[Metric] := TimeSum;
    end;
  end;

var
  RR: TFloatRectangle;
  Triangles: array [TProfilerMetric] of array of TVector2;
  PointsFps: array of TVector2;
  CurrentDataCount: Integer;
  Previous, Next: TProfilerItemDraw;
  Metric: TProfilerMetric;
  {ItemWidth, }X1, X2, Y1, Y2: Single;
begin
  if not ProfilerDataNonEmpty then Exit; // empty data

  if ProfilerDataLast <= ProfilerDataFirst then
    CurrentDataCount := ProfilerDataLast + ProfilerDataCount - ProfilerDataFirst
  else
    CurrentDataCount := ProfilerDataLast - ProfilerDataFirst;
  Assert(Between(CurrentDataCount, 1, ProfilerDataCount));

  { Allocate all necessary memory for TVector2 arrays }
  for Metric in TProfilerMetric do
    SetLength(Triangles[Metric], 6 * (CurrentDataCount - 1));

  RR := ProfilerGraph.RenderRect;
  { (ProfilerDataCount - 1) is maximum number of rectangles we want to squeeze
    within ProfilerGraph area. }
  //ItemWidth := RR.Width / (ProfilerDataCount - 1);

  Next.Index := 0;
  Next.DataIndex := ProfilerDataFirst;
  CalculateItemDraw(Next);

  { Calculate contents of Triangles by iteration over available data items.
    This has to be fast, and generate efficient lists to render graphs using
    only a few draw calls. }
  while (Next.DataIndex + 1) mod ProfilerDataCount <> ProfilerDataLast do
  begin
    Previous := Next;
    Inc(Next.Index);
    Next.DataIndex := (Next.DataIndex + 1) mod ProfilerDataCount;
    CalculateItemDraw(Next);

    X1 := Lerp(Previous.Index / (ProfilerDataCount - 1), RR.Left, RR.Right);
    X2 := Lerp(Next.Index / (ProfilerDataCount - 1), RR.Left, RR.Right);

    for Metric in TProfilerMetric do
    begin
      Y1 := Lerp(Previous.TimeSum[Metric], RR.Bottom, RR.Top);
      Y2 := Lerp(Next.TimeSum[Metric], RR.Bottom, RR.Top);
      { use 2 triangles to express a quad, from old to new data point }
      Triangles[Metric][6 * Previous.Index    ] := Vector2(X1, RR.Bottom);
      Triangles[Metric][6 * Previous.Index + 1] := Vector2(X2, RR.Bottom);
      Triangles[Metric][6 * Previous.Index + 2] := Vector2(X2, Y2);
      Triangles[Metric][6 * Previous.Index + 3] := Vector2(X1, RR.Bottom);
      Triangles[Metric][6 * Previous.Index + 4] := Vector2(X2, Y2);
      Triangles[Metric][6 * Previous.Index + 5] := Vector2(X1, Y1);
    end;
  end;

  Assert(Next.Index + 1 = CurrentDataCount);

  for Metric in TProfilerMetric do
    DrawPrimitive2D(pmTriangles, Triangles[Metric], HexToColor(Colors[Metric]));

  // TODO: FPS calculate and display
  //SetLength(PointsFps, CurrentDataCount);

end;

initialization
  TCastleInspector.PersistentState.Opacity := TCastleInspector.DefaultOpacity;
  TCastleInspector.PersistentState.RectPropertiesExists := true;
  TCastleInspector.PersistentState.RectLogExists := true;
  TCastleInspector.PersistentState.RectHierarchyExists := true;
  TCastleInspector.PersistentState.RectProfilerExists := true;
end.
