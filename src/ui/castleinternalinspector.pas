{
  Copyright 2015-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Inspect Castle Game Engine state at runtime (TCastleInspector).
  Invoke this automatically in debug builds by F8 (see @link(TCastleContainer.InputInspector)). }
unit CastleInternalInspector;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections, TypInfo,
  CastleControls, CastleUIControls, CastleColors, CastleRectangles,
  CastleVectors, CastleKeysMouse, CastleComponentSerialize, CastleTimeUtils;

type
  { Inspect Castle Game Engine state.
    Show log, current UI and viewports state.
    Invoke this automatically in debug builds by F8 (see @link(TCastleContainer.InputInspector)). }
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

      TAutoSelect = (asNothing, asUi, asTransform);

      TPropertyOwner = class(TComponent)
      public
        Ui: TCastleUserInterface;
        LabelName: TCastleLabel;
        EditValue: TCastleEdit;
        PropObject: TObject;
        PropInfo: PPropInfo;
      end;
      TPropertyOwnerList = {$ifdef FPC}specialize{$endif} TObjectList<TPropertyOwner>;

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
      ScrollProperties: TCastleScrollView;
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
      ButtonAutoSelectNothing: TCastleButton;
      ButtonAutoSelectUi: TCastleButton;
      ButtonAutoSelectTransform: TCastleButton;

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
      AutoSelect: TAutoSelect;
      { Properties of the SelectedComponent. }
      Properties: TPropertyOwnerList;
      TimeToUpdatePropertiesValues: TFloatTime;

    procedure ChangeOpacity(Sender: TObject);
    procedure SetOpacity(const Value: Single);
    { Show component in hierarchy, allow to select it. }
    function Selectable(const C: TComponent): Boolean;
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
    procedure UpdateHierarchy(Sender: TObject);
    procedure ClickHierarchyRow(Sender: TObject);
    { Update properties to reflect current FSelectedComponent.
      Only SetSelectedComponent needs to call it. }
    procedure UpdateProperties;
    { Like UpdateProperties, but only updates property Values, assuming SelectedComponent
      didn't change. }
    procedure UpdatePropertiesValues;
    procedure ProfilerSummaryAvailable(Sender: TObject);
    procedure ProfilerGraphRender(const Sender: TCastleUserInterface);
    { Synchronize state of ButtonAutoSelectXxx based on current AutoSelect value. }
    procedure SynchronizeButtonsAutoSelect;
    procedure ClickAutoSelectNothing(Sender: TObject);
    procedure ClickAutoSelectUi(Sender: TObject);
    procedure ClickAutoSelectTransform(Sender: TObject);
    class procedure AdjustColorsBasedOnPropertyDefault(
      const Edit: TCastleEdit; const IsDefault: Boolean);
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
    function Press(const Event: TInputPressRelease): Boolean; override;

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

uses SysUtils, StrUtils, RttiUtils,
  CastleStringUtils, CastleGLUtils, CastleApplicationProperties, CastleClassUtils,
  CastleUtils, CastleLog, CastleInternalRttiUtils,
  CastleTransform, CastleViewport, CastleScene, CastleURIUtils;

{ ---------------------------------------------------------------------------- }

type
  // Just for test, this will also work.
  //  TSimpleLabel = TCastleLabel;

  { Display string. A much simpler alternative to TCastleLabel,
    missing a lot of its features.

    Compared to TCastleLabel,

    - it only supports displaying a single line
      (newline characters in Caption will be visible as weird chars).

    - does not support Html.

    - does not support wrapping (MaxWidth), text alignment in any way.

    - does not calculate own width properly (so e.g. anchors to right will not work).

    - doesn't manage visual updates (it assumes screeb will always be redrawn after changing
      Caption; in practice, it is easiest to only change Caption at creation).

    - doesn't allow translation.

    - doesn't serialize to/from JSON.
  }
  TSimpleLabel = class(TCastleUserInterfaceFont)
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    Caption: String;
    Color: TCastleColor;
    procedure Render; override;
  end;

procedure TSimpleLabel.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  // inherited; // no need
  PreferredWidth := 10; // hardcoded arbitrary non-zero value, to return fast
  { Add Font.DescenderHeight, although it is already contained in Font.Height.
    See TCastleLabel.PreferredSize for reason. }
  PreferredHeight := Font.Height + Font.DescenderHeight;
end;

procedure TSimpleLabel.Render;
var
  R: TFloatRectangle;
begin
  // inherited; // no need
  R := RenderRect;
  { TODO: Font.DescenderHeight * 1.5 to match button caption alignment,
    but we should be rather "Font.DescenderHeight * 1" to be consistent with TCastleLabel. }
  Font.Print(R.Left, R.Bottom + Font.DescenderHeight * 1.5, Color, Caption);
end;

{ TCastleInspector ----------------------------------------------------------- }

{ TODO:

  Show size as extra property - (C.EffectiveRect - like in CGE editor)

  Show C.Focused marked by some border of button on hierarchy

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

  procedure AddLastLogs;
  var
    I: Integer;
  begin
    for I := LastLogCount - 1 downto 0 do
      LogCallback(LastLog(I));
    // TODO: for some reason, this is necessary to correctly layout the newly added logs
    LogsVerticalGroup.VisibleChange([chChildren]);
  end;

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
  Ui := StringToComponent({$I designs/inspector_ui.castle-user-interface.inc}, UiOwner)
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
  ScrollProperties := UiOwner.FindRequiredComponent('ScrollProperties') as TCastleScrollView;
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
  ButtonAutoSelectNothing := UiOwner.FindRequiredComponent('ButtonAutoSelectNothing') as TCastleButton;
  ButtonAutoSelectUi := UiOwner.FindRequiredComponent('ButtonAutoSelectUi') as TCastleButton;
  ButtonAutoSelectTransform := UiOwner.FindRequiredComponent('ButtonAutoSelectTransform') as TCastleButton;

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
  ButtonAutoSelectNothing.OnClick := {$ifdef FPC}@{$endif} ClickAutoSelectNothing;
  ButtonAutoSelectUi.OnClick := {$ifdef FPC}@{$endif} ClickAutoSelectUi;
  ButtonAutoSelectTransform.OnClick := {$ifdef FPC}@{$endif} ClickAutoSelectTransform;

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
  AddLastLogs;

  { initialize profiler }
  FrameProfiler.Enabled := RectProfiler.Exists;
  FrameProfiler.OnSummaryAvailable := {$ifdef FPC}@{$endif} ProfilerSummaryAvailable;
  // FrameProfiler.FramesForSummary := 2; // useful to quickly test
  CheckboxProfilerDetailsInLog.Checked := FrameProfiler.LogSummary;

  { initialize AutoSelect }
  AutoSelect := asNothing;
  SynchronizeButtonsAutoSelect;

  Properties := TPropertyOwnerList.Create(true);
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

  FreeAndNil(Properties);

  FreeAndNil(SerializedHierarchyRowTemplate);
  FreeAndNil(SerializedPropertyRowTemplate);

  inherited;
end;

const
  SLevelPrefix = '- ';

function TCastleInspector.Selectable(const C: TComponent): Boolean;
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

  { Add hierarchy entry.
    MainCaption is displayed using prominent (black) font color,
    DimCaption is displayed using dim font color -- it can be used for class name. }
  procedure AddHierarchyEntryCore(const MainCaption, DimCaption: String; const RowTag: Pointer);
  const
    // CGE editor uses lighter gray, but inspector is often on partially-transparent bg, darker gray looks better
    //DimColor: TCastleColor = (X: 0.75; Y: 0.75; Z: 0.75; W: 1.0);
    DimColor: TCastleColor = (X: 0.6; Y: 0.6; Z: 0.6; W: 1.0);
  var
    HierarchyButton: TCastleButton;
    DimLabel: TSimpleLabel;
  begin
    if RowIndex < HierarchyRowParent.ControlsCount then
    begin
      HierarchyButton := HierarchyRowParent.Controls[RowIndex] as TCastleButton;
      DimLabel := HierarchyButton.Controls[0] as TSimpleLabel;
    end else
    begin
      HierarchyButton := SerializedHierarchyRowTemplate.ComponentLoad(Self) as TCastleButton;
      HierarchyButton.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyRow;
      HierarchyButton.Culling := true; // many such buttons are often not visible, in scroll view
      HierarchyButton.Width := RectHierarchy.EffectiveWidthForChildren;
      HierarchyRowParent.InsertFront(HierarchyButton);

      DimLabel := TSimpleLabel.Create(HierarchyButton);
      DimLabel.Color := DimColor;
      DimLabel.Anchor(vpMiddle);
      HierarchyButton.InsertFront(DimLabel);

      ForceFallbackLook(HierarchyButton);
    end;

    HierarchyButton.Caption := MainCaption;

    DimLabel.Caption := DimCaption;
    DimLabel.Anchor(hpLeft, FallbackFont.TextWidth(MainCaption));

    { TComponent.Tag is a (signed) PtrInt in FPC, (signed) NativeInt in Delphi,
      so typecast to PtrInt to avoid range check errors. }
    HierarchyButton.Tag := PtrInt(RowTag);

    Inc(RowIndex);
  end;

  procedure AddHierarchyEntry(const C: TComponent; const Level: Integer);
  begin
    AddHierarchyEntryCore(
      DupeString(SLevelPrefix, Level) + C.Name,
      ' (' + C.ClassName + ')',
      C
    );
  end;

  { Add given component, and its children in C.NonVisualComponents }
  procedure AddNonVisualComponent(const C: TComponent; const Level: Integer);
  var
    Child: TComponent;
  begin
    AddHierarchyEntry(C, Level);

    if C is TCastleComponent then
    begin
      for Child in TCastleComponent(C).NonVisualComponentsEnumerate do
        if Selectable(Child) then
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
      AddHierarchyEntryCore(DupeString(SLevelPrefix, Level) + 'Non-Visual Components', '', C);
      for Child in C.NonVisualComponentsEnumerate do
        if Selectable(Child) then
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
      AddHierarchyEntryCore(DupeString(SLevelPrefix, Level) + 'Behaviors', '', T);
      for Child in T.BehaviorsEnumerate do
        if Selectable(Child) then
          AddNonVisualComponent(Child, Level + 1);
    end;
  end;

  { Add given transform, and its children
    (transform children, T.NonVisualComponents, T.Behaviors). }
  procedure AddTransform(const T: TCastleTransform; const Level: Integer);
  var
    I: Integer;
  begin
    AddHierarchyEntry(T, Level);

    AddNonVisualComponentsSection(T, Level + 1);
    AddBehaviorsSection(T, Level + 1);

    for I := 0 to T.Count - 1 do
      if Selectable(T[I]) then
        AddTransform(T[I], Level + 1);
  end;

  { Add given UI control, and its children. }
  procedure AddControl(const C: TCastleUserInterface; const Level: Integer);
  var
    I: Integer;
    Viewport: TCastleViewport;
  begin
    AddHierarchyEntry(C, Level);

    AddNonVisualComponentsSection(C, Level + 1);

    for I := 0 to C.ControlsCount - 1 do
    begin
      if Selectable(C.Controls[I]) then
        AddControl(C.Controls[I], Level + 1);
    end;

    if C is TCastleViewport then
    begin
      Viewport := TCastleViewport(C);
      if Selectable(Viewport.Items) then
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
    if Selectable(C) then
      AddControl(C, 0);
  end;

  while HierarchyRowParent.ControlsCount > RowIndex do
    HierarchyRowParent.Controls[RowIndex].Free;
end;

procedure TCastleInspector.Update(const SecondsPassed: Single;  var HandleInput: boolean);

  { Detect UI control that mouse hovers over.
    Adjusted from CGE editor TDesignFrame.TDesignerLayer.HoverUserInterface }
  function HoverUserInterface(const AMousePosition: TVector2): TCastleUserInterface;

    { Like TCastleUserInterface.CapturesEventsAtPosition, but
      - ignores CapturesEvents
      - uses RenderRectWithBorder (to be able to drag complete control)
      - doesn't need "if the control covers the whole Container" hack. }
    function SimpleCapturesEventsAtPosition(const UI: TCastleUserInterface;
      const Position: TVector2; const TestWithBorder: Boolean): Boolean;
    begin
      if TestWithBorder then
        Result := UI.RenderRectWithBorder.Contains(Position)
      else
        Result := UI.RenderRect.Contains(Position);
    end;

    function ControlUnder(const C: TCastleUserInterface;
      const MousePos: TVector2; const TestWithBorder: Boolean): TCastleUserInterface;
    var
      I: Integer;
    begin
      Result := nil;

      { To allow selecting even controls that have bad rectangle (outside
        of parent, which can happen, e.g. if you enlarge caption of label
        with AutoSize), do not check C.CapturesEventsAtPosition(MousePos)
        too early here. So the condition

          and C.CapturesEventsAtPosition(MousePos)

        is not present in "if" below. }

      if C.Exists then
      begin
        { First try to find children, with TestWithBorder=false (so it doesn't detect
          control if we merely point at its border). This allows to find controls
          places on another control's border. }
        for I := C.ControlsCount - 1 downto 0 do
          if Selectable(C.Controls[I]) then
          begin
            Result := ControlUnder(C.Controls[I], MousePos, false);
            if Result <> nil then Exit;
          end;

        { Next try to find children, with TestWithBorder=true, so it tries harder
          to find something. }
        for I := C.ControlsCount - 1 downto 0 do
          if Selectable(C.Controls[I]) then
          begin
            Result := ControlUnder(C.Controls[I], MousePos, true);
            if Result <> nil then Exit;
          end;

        { Eventually return yourself, C. }
        //if C.CapturesEventsAtPosition(MousePos) then
        if SimpleCapturesEventsAtPosition(C, MousePos, TestWithBorder) and
           C.EditorSelectOnHover then
          Result := C;
      end;
    end;

  var
    I: Integer;
  begin
    Result := nil;
    for I := Container.Controls.Count - 1 downto 0 do
      if Container.Controls[I] <> Self then
      begin
        Result := ControlUnder(Container.Controls[I], AMousePosition, true);
        if Result <> nil then Exit;
      end;
  end;

  { Detect TCastleTransform that mouse hovers over.
    Adjusted from CGE editor TDesignFrame.TDesignerLayer.HoverTransform }
  function HoverTransform(const AMousePosition: TVector2): TCastleTransform;
  var
    UI: TCastleUserInterface;
    Viewport: TCastleViewport;
    RayOrigin, RayDirection: TVector3;
    RayHit: TRayCollision;
    I: Integer;
  begin
    UI := HoverUserInterface(AMousePosition);
    if UI is TCastleViewport then // also checks UI <> nil
      Viewport := TCastleViewport(UI)
    else
      Viewport := nil;

    if Viewport = nil then
      Exit(nil);

    Viewport.PositionToRay(AMousePosition, true, RayOrigin, RayDirection);
    RayHit := Viewport.Items.WorldRay(RayOrigin, RayDirection);
    try
      Result := nil;
      // set the inner-most TCastleTransform hit, but not anything transient (to avoid hitting gizmo)
      if RayHit <> nil then
        for I := 0 to RayHit.Count - 1 do
          if Selectable(RayHit[I].Item) then
          begin
            Result := RayHit[I].Item;
            Break;
          end;
    finally FreeAndNil(RayHit) end;
  end;

  procedure UpdateAutoSelect;
  var
    C: TComponent;
  begin
    case AutoSelect of
     asUi: C := HoverUserInterface(Container.MousePosition);
     asTransform: C := HoverTransform(Container.MousePosition);
     else Exit; // on asNothing, just Exit
   end;
   { do not change SelectedComponent to nil, this avoids e.g. unselecting UI when going into
     asTransform mode for a short time. }
   if C <> nil then
     SelectedComponent := C;
  end;

const
  { Delay between updating properties. }
  UpdatePropertiesValuesInterval = 0.5;
var
  InspectorInputStr: String;
begin
  inherited;
  UpdateHierarchy(nil);

  LabelProfilerHeader.Caption := 'Profiler | Current FPS: ' + Container.Fps.ToString;

  InspectorInputStr := Container.InputInspector.ToString;
  LabelInspectorHelp.Exists := InspectorInputStr <> '';
  if LabelInspectorHelp.Exists then
    LabelInspectorHelp.Caption := 'Hide inspector by ' + NL +
      '  ' + InspectorInputStr + '.';

  UpdateAutoSelect;

  if RectProperties.Exists then // do not do UpdatePropertiesValues when not visible, to save speed
  begin
    TimeToUpdatePropertiesValues := TimeToUpdatePropertiesValues - SecondsPassed;
    if TimeToUpdatePropertiesValues < 0 then
    begin
      UpdatePropertiesValues;
      TimeToUpdatePropertiesValues := UpdatePropertiesValuesInterval;
    end;
  end;
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
  { We didn't update properties values when RectProperties.Exists was false,
    so update them now. }
  UpdatePropertiesValues;
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
    ForceFallbackLook(NewLabelLog);
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

  { Used to display C as String. }
  function ComponentCaption(const C: TComponent): String;
  begin
    Result := C.Name + ' (' + C.ClassName + ')';
  end;

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
      LabelPropertiesHeader.Caption := 'Properties - ' + ComponentCaption(FSelectedComponent)
    else
      LabelPropertiesHeader.Caption := 'Properties';

    ScrollProperties.Scroll := ScrollProperties.ScrollMin;

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

  procedure AddPropertyRow(const PropObject: TObject; const PropInfo: PPropInfo;
    const PropName, PropValue: String; const IsDefault: Boolean);
  var
    PropertyOwner: TPropertyOwner;
  begin
    PropertyOwner := TPropertyOwner.Create(Self);
    PropertyOwner.PropObject := PropObject;
    PropertyOwner.PropInfo := PropInfo;
    Properties.Add(PropertyOwner);

    PropertyOwner.Ui := SerializedPropertyRowTemplate.ComponentLoad(PropertyOwner) as TCastleUserInterface;
    PropertyOwner.Ui.Culling := true; // many such rows are often not visible, in scroll view
    PropertyOwner.Ui.Width := RectProperties.EffectiveWidthForChildren;
    ForceFallbackLook(PropertyOwner.Ui);
    PropertyRowParent.InsertFront(PropertyOwner.Ui);

    PropertyOwner.LabelName := PropertyOwner.FindRequiredComponent('PropName') as TCastleLabel;
    PropertyOwner.EditValue := PropertyOwner.FindRequiredComponent('PropValue') as TCastleEdit;

    PropertyOwner.LabelName.Caption := PropName;
    // TODO: editing PropValue has no effect now
    PropertyOwner.EditValue.Text := PropValue;
    AdjustColorsBasedOnPropertyDefault(PropertyOwner.EditValue, IsDefault);
  end;

  function PropertyShow(const PropObject: TComponent; const PropInfo: PPropInfo): Boolean;
  var
    PropName: String;
  begin
    PropName := PropInfo^.Name;
    if (PropName = 'Name') and
       (csSubComponent in PropObject.ComponentStyle) then
    begin
      { Do not show names of subcomponents, they are not useful (to view or edit).
        CastleComponentSerialize also doesn't save them (see TCastleJsonWriter.StreamProperty),
        CGE editor also doesn't show them (see TDesignFrame.InspectorFilter). }
      Exit(false);
    end;

    Result := true;
  end;

var
  PropInfos: TPropInfoList;
  I: Integer;
  PropName, PropValue: String;
begin
  // frees TPropertyOwner instances along with their UIs
  Properties.Clear;

  if FSelectedComponent <> nil then
  begin
    PropInfos := TPropInfoList.Create(FSelectedComponent, tkProperties);
    try
      for I := 0 to PropInfos.Count - 1 do
        if PropertyShow(FSelectedComponent, PropInfos.Items[I]) and
           PropertyGet(FSelectedComponent, PropInfos.Items[I], PropName, PropValue) then
          AddPropertyRow(FSelectedComponent, PropInfos.Items[I], PropName, PropValue,
            PropertyHasDefaultValue(FSelectedComponent, PropInfos.Items[I], true));
    finally FreeAndNil(PropInfos) end;
  end;
end;

procedure TCastleInspector.UpdatePropertiesValues;
var
  Po: TPropertyOwner;
  PropName, PropValue: String;
begin
  for Po in Properties do
  begin
    if PropertyGet(Po.PropObject, Po.PropInfo, PropName, PropValue) then
    begin
      Po.EditValue.Text := PropValue;
      AdjustColorsBasedOnPropertyDefault(Po.EditValue,
        PropertyHasDefaultValue(Po.PropObject, Po.PropInfo, true));
    end else
      WritelnWarning('Cannot read property name/value, but it was possible to read it earlier');
  end;
end;

procedure TCastleInspector.Resize;
var
  C: TCastleUserInterface;
begin
  inherited;

  // TODO: doing this in constructor makes a wild scrollbar move
  if CheckboxLogAutoScroll.Checked then
    ScrollLogs.Scroll := ScrollLogs.ScrollMax;

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
  { Use instant_fpc_color_hex_to_pascal to easily convert hex colors to TCastleColor
    Pascal constants. }
  Colors: array [TProfilerMetric] of TCastleColor = (
    (X:1.00; Y: 1.00; Z: 0.00; W: 1.00), // hex: FFFF00
    (X:0.48; Y: 0.48; Z: 0.00; W: 1.00), // hex: 7A7A00
    (X:0.40; Y: 0.40; Z: 1.00; W: 1.00), // hex: 6767FF
    (X:0.69; Y: 0.69; Z: 1.00; W: 1.00) // hex: B1B1FF
  );
  ColorFpsHex: TCastleColor = (X:0.00; Y: 0.70; Z: 0.00; W: 1.00); // hex: 00B300
type
  TTimeSum = array [TProfilerMetric] of Single;
  TProfilerItemDraw = record
    { For each metric: sum of time of this metric + lower metrics, in 0..1. }
    TimeSum: TTimeSum;
    { From 0 upward. }
    Index: Integer;
    { From ProfilerDataFirst to ProfilerDataLast-1. }
    DataIndex: Integer;
    { FPS in 0..1 range. }
    Fps: Single;
  end;
  PProfilerItemDraw = ^TProfilerItemDraw;

  { Calcualate ItemDraw.TimeSum, Fps based on ProfilerData[ItemDraw.DataIndex]. }
  procedure CalculateItemDraw(var ItemDraw: TProfilerItemDraw);
  const
    FpsMiddle = 60.0;
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
    ItemDraw.Fps := Clamped(Data^.Fps * 0.5 / FpsMiddle, 0, 1);
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
  for Metric := Low(TProfilerMetric) to High(TProfilerMetric) do
    SetLength(Triangles[Metric], 6 * (CurrentDataCount - 1));
  SetLength(PointsFps, CurrentDataCount);

  RR := ProfilerGraph.RenderRect;
  { (ProfilerDataCount - 1) is maximum number of rectangles we want to squeeze
    within ProfilerGraph area. }
  //ItemWidth := RR.Width / (ProfilerDataCount - 1);

  Next.Index := 0;
  Next.DataIndex := ProfilerDataFirst;
  CalculateItemDraw(Next);
  PointsFps[0] := Vector2(RR.Left, Lerp(Next.Fps, RR.Bottom, RR.Top));

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

    PointsFps[Next.Index] := Vector2(X2, Lerp(Next.Fps, RR.Bottom, RR.Top));

    for Metric := Low(TProfilerMetric) to High(TProfilerMetric) do
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

  for Metric := Low(TProfilerMetric) to High(TProfilerMetric) do
    DrawPrimitive2D(pmTriangles, Triangles[Metric], Colors[Metric]);

  DrawPrimitive2D(pmLineStrip, PointsFps, ColorFpsHex);
end;

function TCastleInspector.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyF9) then
  begin
    if AutoSelect = High(AutoSelect) then
      AutoSelect := Low(AutoSelect)
    else
      AutoSelect := Succ(AutoSelect);
    SynchronizeButtonsAutoSelect;
    Exit(true);
  end;
end;

procedure TCastleInspector.SynchronizeButtonsAutoSelect;
begin
  ButtonAutoSelectNothing.Pressed := AutoSelect = asNothing;
  ButtonAutoSelectUi.Pressed := AutoSelect = asUi;
  ButtonAutoSelectTransform.Pressed := AutoSelect = asTransform;
end;

procedure TCastleInspector.ClickAutoSelectNothing(Sender: TObject);
begin
  AutoSelect := asNothing;
  SynchronizeButtonsAutoSelect;
end;

procedure TCastleInspector.ClickAutoSelectUi(Sender: TObject);
begin
  AutoSelect := asUi;
  SynchronizeButtonsAutoSelect;
end;

procedure TCastleInspector.ClickAutoSelectTransform(Sender: TObject);
begin
  AutoSelect := asTransform;
  SynchronizeButtonsAutoSelect;
end;

class procedure TCastleInspector.AdjustColorsBasedOnPropertyDefault(
  const Edit: TCastleEdit; const IsDefault: Boolean);
begin
  if IsDefault then
  begin
    // TCastleEdit defaults
    Edit.FocusedColor := Black;
    Edit.UnfocusedColor := Vector4(0.25, 0.25, 0.25, 1);
  end else
  begin
    Edit.FocusedColor := Blue;
    Edit.UnfocusedColor := Vector4(0.25, 0.25, 1, 1);
  end;
end;

initialization
  TCastleInspector.PersistentState.Opacity := TCastleInspector.DefaultOpacity;
  TCastleInspector.PersistentState.RectPropertiesExists := true;
  TCastleInspector.PersistentState.RectLogExists := true;
  TCastleInspector.PersistentState.RectHierarchyExists := true;
  TCastleInspector.PersistentState.RectProfilerExists := true;
end.
