{
  Copyright 2015-2025 Michalis Kamburelis.

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
  CastleVectors, CastleKeysMouse, CastleComponentSerialize, CastleTimeUtils,
  CastleTransform, CastleInternalRttiUtils, CastleClassUtils;

{$define read_interface}
{$I castleinternalinspector_properties.inc}
{$I castleinternalinspector_hierarchy.inc}
{$undef read_interface}

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

    var
      { Controls loaded from inspector_ui.castle-user-interface.inc }
      CheckboxUiBatching: TCastleCheckbox;
      RectOptions, RectLog, RectProfiler: TCastleRectangleControl;
      ButtonHierarchyShow,
        ButtonLogShow, ButtonLogHide,
        ButtonPropertiesShow,
        ButtonProfilerShow, ButtonProfilerHide: TCastleButton;
      HorizontalGroupShow: TCastleUserInterface;
      ScrollLogs: TCastleScrollView;
      LogsVerticalGroup: TCastleVerticalGroup;
      LabelEarlierLogsRemoved: TCastleLabel;
      LabelLogHeader: TCastleLabel;
      LabelInspectorHelp: TCastleLabel;
      SliderOpacity: TCastleFloatSlider;
      ButtonLogClear: TCastleButton;
      CheckboxLogAutoScroll: TCastleCheckbox;
      LabelProfilerHeader: TCastleLabel;
      CheckboxProfilerDetailsInLog: TCastleCheckbox;
      CheckboxProfilerMore: TCastleCheckbox;
      CheckboxFileMonitorEnabled: TCastleCheckbox;
      ProfilerGraph: TCastleUserInterface;
      RectStatsMore: TCastleUserInterface;
      LabelStatsMore: TCastleLabel;
      ButtonAutoSelectNothing: TCastleButton;
      ButtonAutoSelectUi: TCastleButton;
      ButtonAutoSelectTransform: TCastleButton;
      SafeBorderContainer: TCastleUserInterface;
      HeaderProfiler, HeaderProfiler2ndRow: TCastleUserInterface;

      RectProperties: TCastleComponentProperties;
      RectHierarchy: TCastleComponentsHierarchy;
      FSelectedComponent: TComponent;
      FOpacity: Single;
      InsideLogCallback: Boolean;
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
    procedure ChangeProfilerMore(Sender: TObject);
    procedure ChangeFileMonitorEnabled(Sender: TObject);
    { Synchronize state of HorizontalGroupShow and its children with the existence of rectangles
      like RectHierarchy. So you only need to change RectHierarchy.Exists and call this method
      to have UI consistent. }
    procedure SynchronizeButtonsToShow;
    procedure LogCallback(const Message: String);
    procedure ProfilerSummaryAvailable(Sender: TObject);
    procedure ProfilerGraphRender(const Sender: TCastleUserInterface);
    { Synchronize state of ButtonAutoSelectXxx based on current AutoSelect value. }
    procedure SynchronizeButtonsAutoSelect;
    procedure ClickAutoSelectNothing(Sender: TObject);
    procedure ClickAutoSelectUi(Sender: TObject);
    procedure ClickAutoSelectTransform(Sender: TObject);
    { Detect TCastleTransform that mouse hovers over.
      Adjusted from CGE editor TDesignFrame.TDesignerLayer.HoverTransform }
    function HoverTransform(const AMousePosition: TVector2): TCastleTransform;
    { Detect UI control that mouse hovers over.
      Adjusted from CGE editor TDesignFrame.TDesignerLayer.HoverUserInterface }
    function HoverUserInterface(const AMousePosition: TVector2): TCastleUserInterface;
    procedure UpdateLabelProfilerHeader;
    procedure UpdateLabelStatsMore;
    procedure ChangeUiBatching(Sender: TObject);
    procedure SetSelectedComponent(const Value: TComponent);
    procedure HierarchySelect(Sender: TObject);
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
    procedure InternalSetContainer(const Value: TCastleContainer); override;

    { Opacity of the whole UI.
      Can be changed by user while operating this UI. }
    property Opacity: Single read FOpacity write SetOpacity;

    { Selected object in hierarchy, for which we display the properties.
      Can be changed by user while operating this UI.
      Synchronized with RectProperties.SelectedComponent
      and RectHierarchy.SelectedComponent. }
    property SelectedComponent: TComponent read FSelectedComponent write SetSelectedComponent;
  published
    property KeepInFront stored false;
    property FullSize stored false;
  end;

implementation

uses SysUtils, StrUtils,
  CastleStringUtils, CastleGLUtils, CastleApplicationProperties,
  CastleUtils, CastleLog, CastleGLImages,
  CastleViewport, CastleScene, CastleUriUtils, CastleInternalFileMonitor;

{$define read_implementation}
{$I castleinternalinspector_properties.inc}
{$I castleinternalinspector_hierarchy.inc}
{$undef read_implementation}

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
begin
  inherited;

  // adjust inherited published properties
  FullSize := true;
  KeepInFront := true;

  UiOwner := TComponent.Create(Self);
  Ui := StringToComponent({$I designs/inspector_ui.castle-user-interface.inc}, UiOwner)
    as TCastleUserInterface;
  InsertFront(Ui);

  CheckboxUiBatching := UiOwner.FindRequiredComponent('CheckboxUiBatching') as TCastleCheckbox;
  RectOptions := UiOwner.FindRequiredComponent('RectOptions') as TCastleRectangleControl;
  RectLog := UiOwner.FindRequiredComponent('RectLog') as TCastleRectangleControl;
  RectProfiler := UiOwner.FindRequiredComponent('RectProfiler') as TCastleRectangleControl;
  ButtonHierarchyShow := UiOwner.FindRequiredComponent('ButtonHierarchyShow') as TCastleButton;
  ButtonLogShow := UiOwner.FindRequiredComponent('ButtonLogShow') as TCastleButton;
  ButtonLogHide := UiOwner.FindRequiredComponent('ButtonLogHide') as TCastleButton;
  ButtonPropertiesShow := UiOwner.FindRequiredComponent('ButtonPropertiesShow') as TCastleButton;
  ButtonProfilerShow := UiOwner.FindRequiredComponent('ButtonProfilerShow') as TCastleButton;
  ButtonProfilerHide := UiOwner.FindRequiredComponent('ButtonProfilerHide') as TCastleButton;
  HorizontalGroupShow := UiOwner.FindRequiredComponent('HorizontalGroupShow') as TCastleUserInterface;
  ScrollLogs := UiOwner.FindRequiredComponent('ScrollLogs') as TCastleScrollView;
  LogsVerticalGroup := UiOwner.FindRequiredComponent('LogsVerticalGroup') as TCastleVerticalGroup;
  LabelEarlierLogsRemoved := UiOwner.FindRequiredComponent('LabelEarlierLogsRemoved') as TCastleLabel;
  LabelLogHeader := UiOwner.FindRequiredComponent('LabelLogHeader') as TCastleLabel;
  LabelInspectorHelp := UiOwner.FindRequiredComponent('LabelInspectorHelp') as TCastleLabel;
  SliderOpacity := UiOwner.FindRequiredComponent('SliderOpacity') as TCastleFloatSlider;
  ButtonLogClear := UiOwner.FindRequiredComponent('ButtonLogClear') as TCastleButton;
  CheckboxLogAutoScroll := UiOwner.FindRequiredComponent('CheckboxLogAutoScroll') as TCastleCheckbox;
  LabelProfilerHeader := UiOwner.FindRequiredComponent('LabelProfilerHeader') as TCastleLabel;
  CheckboxProfilerDetailsInLog := UiOwner.FindRequiredComponent('CheckboxProfilerDetailsInLog') as TCastleCheckbox;
  CheckboxProfilerMore := UiOwner.FindRequiredComponent('CheckboxProfilerMore') as TCastleCheckbox;
  CheckboxFileMonitorEnabled := UiOwner.FindRequiredComponent('CheckboxFileMonitorEnabled') as TCastleCheckbox;
  ProfilerGraph := UiOwner.FindRequiredComponent('ProfilerGraph') as TCastleUserInterface;
  RectStatsMore := UiOwner.FindRequiredComponent('RectStatsMore') as TCastleUserInterface;
  LabelStatsMore := UiOwner.FindRequiredComponent('LabelStatsMore') as TCastleLabel;
  ButtonAutoSelectNothing := UiOwner.FindRequiredComponent('ButtonAutoSelectNothing') as TCastleButton;
  ButtonAutoSelectUi := UiOwner.FindRequiredComponent('ButtonAutoSelectUi') as TCastleButton;
  ButtonAutoSelectTransform := UiOwner.FindRequiredComponent('ButtonAutoSelectTransform') as TCastleButton;
  SafeBorderContainer := UiOwner.FindRequiredComponent('SafeBorderContainer') as TCastleUserInterface;
  HeaderProfiler := UiOwner.FindRequiredComponent('HeaderProfiler') as TCastleUserInterface;
  HeaderProfiler2ndRow := UiOwner.FindRequiredComponent('HeaderProfiler2ndRow') as TCastleUserInterface;

  RectHierarchy := TCastleComponentsHierarchy.Create(Self);
  RectHierarchy.FullSize := false;
  RectHierarchy.WidthFraction := 0.25;
  RectHierarchy.HeightFraction := 0.75;
  RectHierarchy.Anchor(vpTop);
  RectHierarchy.OnSelect := {$ifdef FPC}@{$endif} HierarchySelect;
  RectHierarchy.Border.AllSides := 10;
  RectHierarchy.ShowEverythingIfRootUnset := true;
  Ui.InsertFront(RectHierarchy);

  RectProperties := TCastleComponentProperties.Create(Self);
  RectProperties.FullSize := false;
  RectProperties.WidthFraction := 0.25;
  RectProperties.HeightFraction := 0.75;
  RectProperties.Anchor(hpRight);
  RectProperties.Anchor(vpTop);
  RectProperties.Border.AllSides := 10;
  Ui.InsertFront(RectProperties);

  ForceFallbackLook(Ui);

  // TODO: TCastleCheckbox.Enabled would be useful here
  CheckboxFileMonitorEnabled.{Enabled}Exists := FileMonitor.PossiblyEnabled;
  CheckboxFileMonitorEnabled.Checked := FileMonitor.Enabled;

  CheckboxUiBatching.OnChange := {$ifdef FPC}@{$endif} ChangeUiBatching;
  ButtonHierarchyShow.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyShow;
  RectHierarchy.ButtonHierarchyHide.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyHide;
  ButtonLogShow.OnClick := {$ifdef FPC}@{$endif} ClickLogShow;
  ButtonLogHide.OnClick := {$ifdef FPC}@{$endif} ClickLogHide;
  ButtonPropertiesShow.OnClick := {$ifdef FPC}@{$endif} ClickPropertiesShow;
  RectProperties.ButtonPropertiesHide.OnClick := {$ifdef FPC}@{$endif} ClickPropertiesHide;
  ButtonProfilerShow.OnClick := {$ifdef FPC}@{$endif} ClickProfilerShow;
  ButtonProfilerHide.OnClick := {$ifdef FPC}@{$endif} ClickProfilerHide;
  ButtonLogClear.OnClick := {$ifdef FPC}@{$endif} ClickLogClear;
  CheckboxProfilerDetailsInLog.OnChange := {$ifdef FPC}@{$endif} ChangeProfilerDetailsInLog;
  CheckboxProfilerMore.OnChange := {$ifdef FPC}@{$endif} ChangeProfilerMore;
  CheckboxFileMonitorEnabled.OnChange := {$ifdef FPC}@{$endif} ChangeFileMonitorEnabled;
  ProfilerGraph.OnRender := {$ifdef FPC}@{$endif} ProfilerGraphRender;
  ButtonAutoSelectNothing.OnClick := {$ifdef FPC}@{$endif} ClickAutoSelectNothing;
  ButtonAutoSelectUi.OnClick := {$ifdef FPC}@{$endif} ClickAutoSelectUi;
  ButtonAutoSelectTransform.OnClick := {$ifdef FPC}@{$endif} ClickAutoSelectTransform;

  { initial state of UI to show/hide }
  RectProperties.Exists := PersistentState.RectPropertiesExists;
  RectLog.Exists := PersistentState.RectLogExists;
  RectHierarchy.Exists := PersistentState.RectHierarchyExists;
  RectProfiler.Exists := PersistentState.RectProfilerExists;
  RectStatsMore.Exists := RectProfiler.Exists and CheckboxProfilerMore.Checked;
  SynchronizeButtonsToShow;

  FOpacity := -1; // to force setting below call SetOpacity
  Opacity := PersistentState.Opacity;
  SliderOpacity.Value := Opacity;
  SliderOpacity.OnChange := {$ifdef FPC}@{$endif} ChangeOpacity;

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
end;

destructor TCastleInspector.Destroy;
begin
  ApplicationProperties.OnLog.Remove({$ifdef FPC}@{$endif} LogCallback);

  { set to nil by SetSelectedComponent, to detach free notification }
  SelectedComponent := nil;

  { update PersistentState }
  PersistentState.Opacity := Opacity;
  PersistentState.RectPropertiesExists := (RectProperties = nil) or RectProperties.Exists;
  PersistentState.RectLogExists := (RectLog = nil) or RectLog.Exists;
  PersistentState.RectHierarchyExists := (RectHierarchy = nil) or RectHierarchy.Exists;
  PersistentState.RectProfilerExists := (RectProfiler = nil) or RectProfiler.Exists;

  FrameProfiler.OnSummaryAvailable := nil;
  FrameProfiler.Enabled := false;

  FreeAndNil(RectProperties);
  FreeAndNil(RectHierarchy);
  inherited;
end;

procedure TCastleInspector.InternalSetContainer(const Value: TCastleContainer);
begin
  inherited;
  if Container <> nil then
    CheckboxUiBatching.Checked := Container.UserInterfaceBatching;
end;

function TCastleInspector.Selectable(const C: TComponent): Boolean;
begin
  { Never show Self,
    this would cause problems as we'll create HierarchyRow to show HierarchyRow instances... }
  if C = Self then Exit(false);

  Result := (not (csTransient in C.ComponentStyle))
    { TODO: restore:
    or CheckboxShowEvenInternal.Checked };
end;

procedure TCastleInspector.UpdateLabelProfilerHeader;
begin
  LabelProfilerHeader.Caption := 'Profiler | Current FPS: ' + Container.Fps.ToString;
end;

procedure TCastleInspector.UpdateLabelStatsMore;

  { Viewport implied by current SelectedComponent.
    Similar to CGE editor TDesignFrame.SelectedViewport. }
  function SelectedViewport: TCastleViewport;

    { Return viewport indicated by T, or @nil if none. }
    function ViewportOfTransform(const T: TCastleTransform): TCastleViewport;
    begin
      if T.World <> nil then
        Result := T.World.Owner as TCastleViewport
      else
        Result := nil;
    end;

    { Return viewport indicated by B, or @nil if none. }
    function ViewportOfBehavior(const B: TCastleBehavior): TCastleViewport;
    begin
      if B.Parent <> nil then
        Result := ViewportOfTransform(B.Parent)
      else
        Result := nil;
    end;

    { Return viewport indicated by C, or @nil if none. }
    function ViewportOfComponent(const C: TComponent): TCastleViewport;
    var
      ViewportChild: TCastleUserInterface;
    begin
      Result := nil;

      if C is TCastleViewport then
      begin
        Result := C as TCastleViewport;
      end else
      if C is TCastleUserInterface then
      begin
        { When hovering over TCastleNavigation, or TCastleTouchNavigation,
          or really any UI as viewport child -> select viewport. }
        ViewportChild := C as TCastleUserInterface;
        if {ViewportChild.FullSize and} (ViewportChild.Parent is TCastleViewport) then
        begin
          Result := ViewportChild.Parent as TCastleViewport;
        end;
      end else
      if C is TCastleTransform then
        Result := ViewportOfTransform(C as TCastleTransform)
      else
      if C is TCastleBehavior then
        Result := ViewportOfBehavior(C as TCastleBehavior);
    end;

  begin
    Result := ViewportOfComponent(SelectedComponent);
  end;

  { Viewport to act on.
    Similar to CGE editor TDesignFrame.CurrentViewport, that has similar goal. }
  function CurrentViewport: TCastleViewport;
  var
    HoverUi: TCastleUserInterface;
  begin
    Result := SelectedViewport;
    if Result = nil then
    begin
      { try HoverUserInterface as TCastleViewport }
      HoverUi := HoverUserInterface(Container.MousePosition);
      if HoverUi is TCastleViewport then // also checks HoverUi <> nil
        Result := TCastleViewport(HoverUi)
      else
      { try HoverUserInterface as TCastleViewport child, like TCastleNavigation, TCastleTouchNavigation }
      if (HoverUi <> nil) and (HoverUi.Parent is TCastleViewport) then // also checks HoverUi.Parent <> nil
        Result := TCastleViewport(HoverUi.Parent);
    end;
  end;

var
  S: String;
  CurrentV: TCastleViewport;
begin
  if CheckboxProfilerMore.Checked then
  begin
    S := 'TDrawableImage.Statistics: ' + TDrawableImage.Statistics.ToString + NL +
      '  (TODO: Rendering inspector may easily cause ~100 draw calls now. Better UI batching is coming.)';
    CurrentV := CurrentViewport;
    if CurrentV <> nil then
    begin
      S := S + NL +
        'Viewport "' + CurrentV.Name + '":' + NL +
        CurrentV.Statistics.ToString;
    end;
    LabelStatsMore.Caption := S;
  end;
end;

procedure TCastleInspector.ChangeUiBatching(Sender: TObject);
begin
  Container.UserInterfaceBatching := CheckboxUiBatching.Checked;
end;

function TCastleInspector.HoverUserInterface(const AMousePosition: TVector2): TCastleUserInterface;

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

function TCastleInspector.HoverTransform(const AMousePosition: TVector2): TCastleTransform;
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

procedure TCastleInspector.Update(const SecondsPassed: Single;  var HandleInput: boolean);

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

  procedure UpdateSafeBorder;
  var
    B: TBorder;
  begin
    { Honor Container.SafeBorder, to
      - avoid showing inspector where it would be obscured
      - also to show to user that it is correct. }
    B := Container.SafeBorder;
    SafeBorderContainer.Border.Top := B.Top;
    SafeBorderContainer.Border.Right := B.Right;
    SafeBorderContainer.Border.Bottom := B.Bottom;
    SafeBorderContainer.Border.Left := B.Left;
  end;

  { When width is small (e.g. on potrait mobile resolutions)
    we need 2nd row to display full "Profiler" header. }
  procedure UpdateHeaderProfilerNeeds2ndRow;

    { Change Control's parent. NewParent may be @nil. }
    procedure ChangeParent(const Control, NewParent: TCastleUserInterface);
    begin
      if Control.Parent <> nil then
        Control.Parent.RemoveControl(Control);
      if NewParent <> nil then
        NewParent.InsertFront(Control);
    end;

  const
    RectProfilerHeightFractionInitial = 0.15;
  var
    Needs2ndRow: Boolean;
  begin
    Needs2ndRow := LabelProfilerHeader.EffectiveWidth >
      HeaderProfiler.EffectiveWidth
      - CheckboxProfilerMore.EffectiveWidth
      - CheckboxProfilerDetailsInLog.EffectiveWidth
      - ButtonProfilerHide.EffectiveWidth;
    if HeaderProfiler2ndRow.Exists <> Needs2ndRow then
    begin
      HeaderProfiler2ndRow.Exists := Needs2ndRow;
      if Needs2ndRow then
      begin
        ProfilerGraph.Border.Top := HeaderProfiler.EffectiveHeight + HeaderProfiler2ndRow.EffectiveHeight;
        RectProfiler.HeightFraction := 0;
        RectProfiler.Height :=
          RectProfilerHeightFractionInitial * RectProfiler.Parent.EffectiveHeight +
          HeaderProfiler2ndRow.EffectiveHeight;
        ChangeParent(CheckboxProfilerMore, HeaderProfiler2ndRow);
        ChangeParent(CheckboxProfilerDetailsInLog, HeaderProfiler2ndRow);
      end else
      begin
        ProfilerGraph.Border.Top := HeaderProfiler.EffectiveHeight;
        RectProfiler.HeightFraction := RectProfilerHeightFractionInitial;
        ChangeParent(CheckboxProfilerMore, HeaderProfiler);
        ChangeParent(CheckboxProfilerDetailsInLog, HeaderProfiler);
      end;
    end;
  end;

  { Update RectStatsMore.Translation,
    necessary when RectProfiler.EffectiveRect changes.
    Easiest to just do it from Update. }
  procedure UpdateRectStatsMoreTranslation;
  begin
    RectStatsMore.Translation := Vector2(
      RectProfiler.EffectiveRect.Left,
      RectProfiler.Translation.Y - RectProfiler.EffectiveHeight);
  end;

var
  InspectorInputStr: String;
begin
  inherited;

  UpdateLabelProfilerHeader;
  UpdateLabelStatsMore;

  InspectorInputStr := Container.InputInspector.ToString;
  LabelInspectorHelp.Exists := InspectorInputStr <> '';
  if LabelInspectorHelp.Exists then
    LabelInspectorHelp.Caption :=
      { Two lines are safer to fit longer InspectorInputStr, but we need
        vertical space to show "Monitor and Auto-Reload" checkbox ... }
      //'Hide inspector by ' + NL + '  ' + InspectorInputStr + '.';
      'Hide inspector: ' + InspectorInputStr;

  UpdateAutoSelect;
  UpdateSafeBorder;
  UpdateHeaderProfilerNeeds2ndRow;
  UpdateRectStatsMoreTranslation;
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
    // TODO: restore: RectProperties.ColorPersistent.Alpha := Value;
    RectLog.ColorPersistent.Alpha := Value;
    RectProfiler.ColorPersistent.Alpha := Value;
    // TODO: restore: RectHierarchy.ColorPersistent.Alpha := Value;
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
    so update them now.
    TODO: restore:
    TCastleComponentProperties can override SetExists?
  UpdatePropertiesValues;
  }
end;

procedure TCastleInspector.ClickPropertiesHide(Sender: TObject);
begin
  RectProperties.Exists := false;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickProfilerShow(Sender: TObject);
begin
  RectProfiler.Exists := true;
  RectStatsMore.Exists := RectProfiler.Exists and CheckboxProfilerMore.Checked;
  SynchronizeButtonsToShow;
end;

procedure TCastleInspector.ClickProfilerHide(Sender: TObject);
begin
  RectProfiler.Exists := false;
  RectStatsMore.Exists := RectProfiler.Exists and CheckboxProfilerMore.Checked;
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

procedure TCastleInspector.HierarchySelect(Sender: TObject);
begin
  SelectedComponent := RectHierarchy.SelectedComponent;
end;

procedure TCastleInspector.Resize;
begin
  inherited;
  // TODO: doing this in constructor makes a wild scrollbar move
  if CheckboxLogAutoScroll.Checked then
    ScrollLogs.Scroll := ScrollLogs.ScrollMax;
end;

procedure TCastleInspector.ChangeProfilerDetailsInLog(Sender: TObject);
begin
  FrameProfiler.LogSummary := CheckboxProfilerDetailsInLog.Checked;
end;

procedure TCastleInspector.ChangeProfilerMore(Sender: TObject);
begin
  RectStatsMore.Exists := RectProfiler.Exists and CheckboxProfilerMore.Checked;
  UpdateLabelProfilerHeader;
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

procedure TCastleInspector.ChangeFileMonitorEnabled(Sender: TObject);
begin
  FileMonitor.Enabled := CheckboxFileMonitorEnabled.Checked;
end;

procedure TCastleInspector.SetSelectedComponent(const Value: TComponent);
begin
  if FSelectedComponent <> Value then
  begin
    RectProperties.SelectedComponent := Value;
    RectHierarchy.SelectedComponent := Value;
  end;
end;

initialization
  TCastleInspector.PersistentState.Opacity := TCastleInspector.DefaultOpacity;
  TCastleInspector.PersistentState.RectPropertiesExists := true;
  TCastleInspector.PersistentState.RectLogExists := true;
  TCastleInspector.PersistentState.RectHierarchyExists := true;
  TCastleInspector.PersistentState.RectProfilerExists := true;
end.
