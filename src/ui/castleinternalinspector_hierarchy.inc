{%MainUnit castleinternalinspector.pas}
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

{$ifdef read_interface}

type
  { Display components' hierarchy. }
  TCastleComponentsHierarchy = class(TCastleUserInterface)
  strict private
    // designed components
    CheckboxShowEvenInternal: TCastleCheckbox;
    HierarchyRowParent: TCastleUserInterface;
    HierarchyBottomControls: TCastleUserInterface;

    // other fields
    UiOwner: TComponent;
    RowFactory: TCastleComponentFactory;
    Ui: TCastleUserInterface;
    FRoot: TComponent;
    FShowEverythingIfRootUnset: Boolean;
    FRootObserver: TFreeNotificationObserver;
    FSelectedComponent: TComponent;
    FSelectedComponentObserver: TFreeNotificationObserver;
    procedure UpdateHierarchy(Sender: TObject);
    procedure BeforeRenderHierarchyRows(const Sender: TCastleUserInterface);
    procedure ClickRow(Sender: TObject);
    procedure SetRoot(const Value: TComponent);
    procedure SetShowEverythingIfRootUnset(const Value: Boolean);
    procedure RootFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SelectedComponentFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetSelectedComponent(const Value: TComponent);
  public
    // designed components that are just public
    ButtonHierarchyHide: TCastleButton;

    OnSelect: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    { Show components starting from this root.
      If @nil (default), then (depending on ShowEverythingIfRootUnset)
      we either show everything from the container, or nothing. }
    property Root: TComponent read FRoot write SetRoot;

    { What to show when @link(Root) is @nil. }
    property ShowEverythingIfRootUnset: Boolean
      read FShowEverythingIfRootUnset write SetShowEverythingIfRootUnset default false;

    { Currently selected component in the hierarchy.
      Selected component has highlighted row in the hierarchy.
      OnSelect is called when this changes. }
    property SelectedComponent: TComponent read FSelectedComponent write SetSelectedComponent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  // In hierarchy row, we shift main button caption by Level * LevelWidthInSpaces * space width.
  LevelWidthInSpaces = 4;

{ TSimpleLabel --------------------------------------------------------------- }

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

    - doesn't manage visual updates (it assumes screen will always be redrawn
      after changing Caption; in practice, it is easiest to only
      change Caption at creation).

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

{ THierarchyRowLevelDisplay --------------------------------------------------------- }

type
  { Display the level (indentation) of hierarchy row. }
  THierarchyRowLevelDisplay = class(TCastleUserInterface)
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    { Both sizes in final pixels.
      Updated every frame before drawing.
      This way we properly adjust to current UI scaling. }
    class var
      OneLevelWidth, HierarchyRowHeight: Single;
    var
      PreviousLevel, Level: Cardinal;
    procedure Render; override;
  end;

procedure THierarchyRowLevelDisplay.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  // inherited; // no need
  PreferredWidth := OneLevelWidth * Level;
  PreferredHeight := HierarchyRowHeight;
end;

procedure THierarchyRowLevelDisplay.Render;
const
  ColorBars: TCastleColor = (X: 0.5; Y: 0.5; Z: 0.5; W: 1.0);

  procedure DrawBar(const R: TFloatRectangle; const TopMostY: Single);
  begin
    DrawPrimitive2D(pmLineStrip, [
      Vector2(R.Center.X, TopMostY),
      Vector2(R.Center.X, R.Bottom)
    ],
    ColorBars);
  end;

  procedure DrawBarToRight(const R: TFloatRectangle; const TopMostY: Single);
  begin
    DrawPrimitive2D(pmLineStrip, [
      Vector2(R.Center.X, TopMostY),
      Vector2(R.Center.X, R.Center.Y),
      Vector2(R.Right, R.Center.Y)
    ],
    ColorBars);
  end;

var
  I: Integer;
  RR, R: TFloatRectangle;
  TopMostY: Single;
begin
  // inherited; // no need
  if Level = 0 then Exit; // early exit when nothing to do
  RR := RenderRect;
  for I := 0 to Integer(Level) - 1 do
  begin
    R.Left := RR.Left + I * OneLevelWidth;
    R.Bottom := RR.Bottom;
    R.Width := OneLevelWidth;
    R.Height := RR.Height;
    // Make TopMostY go to the middle of *previous* hierarchy row, to connect to it
    if I < PreviousLevel then
      TopMostY := R.Top + HierarchyRowHeight / 2
    else
      TopMostY := R.Top;
    if I <> Integer(Level) - 1 then
      DrawBar(R, TopMostY)
    else
      DrawBarToRight(R, TopMostY);
  end;
end;

{ TCastleComponentsHierarchy ------------------------------------------------- }

constructor TCastleComponentsHierarchy.Create(AOwner: TComponent);
var
  HierarchyRowTemplate: TCastleUserInterface;
begin
  inherited;
  UiOwner := TComponent.Create(nil);
  Ui := StringToComponent({$I designs/components_hierarchy.castle-user-interface.inc}, UiOwner)
    as TCastleUserInterface;
  InsertFront(Ui);

  HierarchyRowTemplate := UiOwner.FindRequiredComponent('HierarchyRowTemplate') as TCastleButton;
  HierarchyRowParent := UiOwner.FindRequiredComponent('HierarchyRowParent') as TCastleUserInterface;
  CheckboxShowEvenInternal := UiOwner.FindRequiredComponent('CheckboxShowEvenInternal') as TCastleCheckbox;
  ButtonHierarchyHide := UiOwner.FindRequiredComponent('ButtonHierarchyHide') as TCastleButton;
  HierarchyBottomControls := UiOwner.FindRequiredComponent('HierarchyBottomControls') as TCastleUserInterface;

  { For now, this functionality is hidden, HierarchyBottomControls has
    Exsist=false in UI (and border of list adjusted to make it look good).

    Reason: this is not really useful for normal
    engine users, and may have unintended consequences if you modify an
    internal component.

    Developers: you can uncomment the line to see HierarchyBottomControls
    and thus CheckboxShowEvenInternal and be able to click on it,
    to see internal info about your own components. }
  //HierarchyBottomControls.Exists := true;
  CheckboxShowEvenInternal.OnChange := {$ifdef FPC}@{$endif} UpdateHierarchy;

  HierarchyRowParent.OnRender := {$ifdef FPC}@{$endif} BeforeRenderHierarchyRows;

  // create factory, remove template that is no longer useful
  RowFactory := TCastleComponentFactory.Create(nil);
  RowFactory.LoadFromComponent(HierarchyRowTemplate);
  FreeAndNil(HierarchyRowTemplate);

  FRootObserver := TFreeNotificationObserver.Create(Self);
  FRootObserver.OnFreeNotification := {$ifdef FPC}@{$endif} RootFreeNotification;

  FSelectedComponentObserver := TFreeNotificationObserver.Create(Self);
  FSelectedComponentObserver.OnFreeNotification :=
    {$ifdef FPC}@{$endif} SelectedComponentFreeNotification;
end;

destructor TCastleComponentsHierarchy.Destroy;
begin
  FreeAndNil(UiOwner);
  FreeAndNil(RowFactory);
  inherited;
end;

procedure TCastleComponentsHierarchy.Resize;
var
  C: TCastleUserInterface;
begin
  inherited;
  { We need to manually adjust rows widths to follow available space,
    righ now this is not doable by simple properties,
    because ScrollArea looks at children for size (and we cannot look
    at children for height, but at the same time determine children width). }
  for C in HierarchyRowParent do
    C.Width := EffectiveWidthForChildren;
end;

procedure TCastleComponentsHierarchy.SetSelectedComponent(const Value: TComponent);
var
  HierarchyRow: TCastleUserInterface;
  HierachyRowButton: TCastleButton;
begin
  if FSelectedComponent <> Value then
  begin
    FSelectedComponent := Value;
    FSelectedComponentObserver.Observed := Value;

    for HierarchyRow in HierarchyRowParent do
    begin
      HierachyRowButton := HierarchyRow as TCastleButton;
      HierachyRowButton.Pressed := TComponent(Pointer(HierachyRowButton.Tag)) = Value;
    end;

    if Assigned(OnSelect) then
      OnSelect(Self);
  end;
end;

procedure TCastleComponentsHierarchy.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  // TODO: inefficient: just update it every frame
  UpdateHierarchy(nil);
end;

procedure TCastleComponentsHierarchy.ClickRow(Sender: TObject);
begin
  SelectedComponent := TComponent(Pointer((Sender as TCastleButton).Tag));
end;

procedure TCastleComponentsHierarchy.UpdateHierarchy(Sender: TObject);

{ Parts of this are deliberately consistent with TDesignFrame.UpdateDesign. }

var
  RowIndex: Integer;
  PreviousLevel: Cardinal;

  { TODO: restore: avoid duplicating this in TCastleInspector,
    and in castle-editor and castle-editor-portable... }
  function Selectable(const C: TComponent): Boolean;
  begin
    { Never show TCastleInspector or hierarchy/properties,
      this would cause problems as we'll create HierarchyRow to show HierarchyRow
      instances... }
    if (C is TCastleInspector) or
       (C is TCastleComponentProperties) or
       (C is TCastleComponentsHierarchy) then
      Exit(false);

    Result := (not (csTransient in C.ComponentStyle))
      { TODO: restore:
      or CheckboxShowEvenInternal.Checked };
  end;

  { Add hierarchy entry.
    @param Level Determines the initial | (and similar) symbols count.
    @param MainCaption Displayed using prominent (black) font color.
    @param DimCaption Displayed using dim font color -- it can be used for class name. }
  procedure AddHierarchyEntryCore(
    const Level: Cardinal;
    const MainCaption, DimCaption: String; const RowTag: Pointer);
  const
    // CGE editor uses lighter gray, but inspector is often on partially-transparent bg, darker gray looks better
    //DimColor: TCastleColor = (X: 0.75; Y: 0.75; Z: 0.75; W: 1.0);
    DimColor: TCastleColor = (X: 0.6; Y: 0.6; Z: 0.6; W: 1.0);
  var
    HierarchyButton: TCastleButton;
    CaptionWithLevel: String;
    DimLabel: TSimpleLabel;
    DimLabelX: Single;
    LevelDisplay: THierarchyRowLevelDisplay;
  begin
    if RowIndex < HierarchyRowParent.ControlsCount then
    begin
      HierarchyButton := HierarchyRowParent.Controls[RowIndex] as TCastleButton;
      DimLabel := HierarchyButton.Controls[0] as TSimpleLabel;
      LevelDisplay := HierarchyButton.Controls[1] as THierarchyRowLevelDisplay;
    end else
    begin
      HierarchyButton := RowFactory.ComponentLoad(Self) as TCastleButton;
      HierarchyButton.OnClick := {$ifdef FPC}@{$endif} ClickRow;
      HierarchyButton.Culling := true; // many such buttons are often not visible, in scroll view
      HierarchyButton.Width := EffectiveWidthForChildren;
      HierarchyRowParent.InsertFront(HierarchyButton);

      DimLabel := TSimpleLabel.Create(HierarchyButton);
      DimLabel.Color := DimColor;
      DimLabel.Anchor(vpMiddle);
      HierarchyButton.InsertFront(DimLabel);

      LevelDisplay := THierarchyRowLevelDisplay.Create(HierarchyButton);
      HierarchyButton.InsertFront(LevelDisplay);

      ForceFallbackLook(HierarchyButton);
    end;

    CaptionWithLevel := DupeString(' ', Level * LevelWidthInSpaces) + MainCaption;

    HierarchyButton.Caption := CaptionWithLevel;

    DimLabel.Caption := DimCaption;
    if MainCaption <> '' then
      DimLabelX := FallbackFont.TextWidth(CaptionWithLevel + ' ')
    else
      DimLabelX := FallbackFont.TextWidth(CaptionWithLevel);
    DimLabel.Anchor(hpLeft, DimLabelX);

    { TComponent.Tag is a (signed) PtrInt in FPC, (signed) NativeInt in Delphi,
      so typecast to PtrInt to avoid range check errors. }
    HierarchyButton.Tag := PtrInt(RowTag);

    LevelDisplay.PreviousLevel := PreviousLevel;
    LevelDisplay.Level := Level;

    Inc(RowIndex);
    PreviousLevel := Level;
  end;

  procedure AddHierarchyEntry(const C: TComponent; const Level: Integer);
  begin
    AddHierarchyEntryCore(Level, C.Name,' (' + C.ClassName + ')', C);
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
      AddHierarchyEntryCore(Level, 'Non-Visual Components', '', C);
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
      AddHierarchyEntryCore(Level, 'Behaviors', '', T);
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

  { Add given component, and its children.
    This is a helper function that calls
    - AddControl
    - or AddTransform
    - or AddNonVisualComponent
    depending on the type of component. }
  procedure AddAutoDetect(const C: TComponent; const Level: Integer);
  begin
    if C is TCastleUserInterface then
      AddControl(TCastleUserInterface(C), Level)
    else
    if C is TCastleTransform then
      AddTransform(TCastleTransform(C), Level)
    else
      AddNonVisualComponent(C, Level)
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
  PreviousLevel := 0;

  if FRoot = nil then
  begin
    if FShowEverythingIfRootUnset and (Container <> nil) then
    begin
      // show everything from the container
      for I := 0 to Container.Controls.Count - 1 do
      begin
        C := Container.Controls[I];
        if Selectable(C) then
          AddControl(C, 0);
      end;
    end;
  end else
  begin
    // show everything from the root
    if Selectable(FRoot) then
      AddAutoDetect(FRoot, 0);
  end;

  while HierarchyRowParent.ControlsCount > RowIndex do
    HierarchyRowParent.Controls[RowIndex].Free;
end;

procedure TCastleComponentsHierarchy.BeforeRenderHierarchyRows(const Sender: TCastleUserInterface);
var
  AnyRowUi: TCastleUserInterfaceFont;
begin
  { Set THierarchyRowLevelDisplay.Xxx class variables
    used when rendering rows. }
  if HierarchyRowParent.ControlsCount > 0 then
  begin
    AnyRowUi := HierarchyRowParent.Controls[0] as TCastleUserInterfaceFont;
    THierarchyRowLevelDisplay.OneLevelWidth :=
      { Measure using AnyRowUi, to use current font size adjusted to UI scale. }
      AnyRowUi.Font.TextWidth(DupeString(' ', LevelWidthInSpaces));
    THierarchyRowLevelDisplay.HierarchyRowHeight :=
      AnyRowUi.EffectiveHeight * AnyRowUi.UIScale;
  end;
end;

procedure TCastleComponentsHierarchy.SetRoot(const Value: TComponent);
begin
  if FRoot <> Value then
  begin
    FRoot := Value;
    FRootObserver.Observed := Value;
    UpdateHierarchy(nil);
  end;
end;

procedure TCastleComponentsHierarchy.RootFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FRoot := nil;
end;

procedure TCastleComponentsHierarchy.SelectedComponentFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FSelectedComponent := nil;
end;

procedure TCastleComponentsHierarchy.SetShowEverythingIfRootUnset(const Value: Boolean);
begin
  if FShowEverythingIfRootUnset <> Value then
  begin
    FShowEverythingIfRootUnset := Value;
    // When FRoot <> nil, then changing this property has no immediate effect.
    if FRoot = nil then
      UpdateHierarchy(nil);
  end;
end;

{$endif read_implementation}
