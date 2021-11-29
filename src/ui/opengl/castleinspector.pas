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
unit CastleInspector;

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
    { Controls loaded from inspector_ui.castle-user-interface.inc }
    CheckboxShowEvenInternal: TCastleCheckbox;
    RectOptions, RectProperties, RectLog, RectHierarchy: TCastleRectangleControl;
    ButtonHierarchyShow, ButtonHierarchyHide,
      ButtonLogShow, ButtonLogHide,
      ButtonPropertiesShow, ButtonPropertiesHide: TCastleButton;
    HorizontalGroupShow: TCastleUserInterface;
    HierarchyRowParent: TCastleUserInterface;
    PropertyRowParent: TCastleUserInterface;
    ScrollLogs: TCastleScrollView;
    LabelLog: TCastleLabel;
    LabelLogHeader: TCastleLabel;
    LabelPropertiesHeader: TCastleLabel;
    LabelInspectorFps: TCastleLabel;
    LabelInspectorHelp: TCastleLabel;
    SliderOpacity: TCastleFloatSlider;

    FOpacity: Single;
    FSelectedComponent: TComponent;
    InsideLogCallback: Boolean;
    SerializedHierarchyRowTemplate: TSerializedComponent;
    SerializedPropertyRowTemplate: TSerializedComponent;
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
    { Force using FallbackFont (not Container.DefaultFont) in this UI.
      This makes proper look, ignoring user's UI font specified in CastleSettings.xml,
      e.g. test on platformer. }
    procedure ForceUsingFallbackFont(const Ui: TCastleUserInterface);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    const
      DefaultOpacity = 0.9;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;  var HandleInput: boolean); override;
  published
    property KeepInFront stored false;
    property FullSize stored false;

    { Opacity of the whole UI.
      Can be changed by user while operating this UI. }
    property Opacity: Single read FOpacity write SetOpacity default DefaultOpacity;

    { Selected object in hierarchy, for which we display the properties.
      Can be changed by user while operating this UI. }
    property SelectedComponent: TComponent read FSelectedComponent write SetSelectedComponent;
  end;

implementation

uses SysUtils, StrUtils,
  CastleStringUtils, CastleGLUtils, CastleApplicationProperties, CastleClassUtils,
  CastleTransform, CastleViewport, CastleScene, CastleURIUtils;

{ TODO:

  Allow selecting components in game view, following focused

  Show size as extra property - (C.EffectiveRect - like in CGE editor)

  Show C.Focused marked by some border of button on hierarchy

  Show historic logs immediately, instead of starting LabelLog always empty.
  Just keep last X logs in DEBUG build always?

  Are multiline logs displayed and counted OK?

  Animate RectXxx existence changes.

  untangle dependencies: it is a bit dirty that CastleUIControls, basic unit,
  uses this unit -- which includes everything, like TCastleViewport.
  CastleUIControls should instead expose InternalInspectorClass that this unit would set.

    Hm, but then how to make sure this is always used?
    This unit will have to be used by CastleWindow, CastleControl...
    again creating a bigger dependency than needed.
    Would be best if this unit didn't need some, at least no CastleViewport, CastleScene.

  checkbox to also show X3D nodes

  better name for FallbackFont?
  DefaultFont would be confusing.

  better way to reset everything to FallbackFont - ParentFont would be more comfortable, I'd set it in one place then

  we still use global Theme, for our buttons, and we don't want to.

  properties, hierarchy rows don't adjust to possible size

  hierarchy button needs to be aligned left
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
  ButtonHierarchyShow := UiOwner.FindRequiredComponent('ButtonHierarchyShow') as TCastleButton;
  ButtonHierarchyHide := UiOwner.FindRequiredComponent('ButtonHierarchyHide') as TCastleButton;
  ButtonLogShow := UiOwner.FindRequiredComponent('ButtonLogShow') as TCastleButton;
  ButtonLogHide := UiOwner.FindRequiredComponent('ButtonLogHide') as TCastleButton;
  ButtonPropertiesShow := UiOwner.FindRequiredComponent('ButtonPropertiesShow') as TCastleButton;
  ButtonPropertiesHide := UiOwner.FindRequiredComponent('ButtonPropertiesHide') as TCastleButton;
  HorizontalGroupShow := UiOwner.FindRequiredComponent('HorizontalGroupShow') as TCastleUserInterface;
  HierarchyRowTemplate := UiOwner.FindRequiredComponent('HierarchyRowTemplate') as TCastleButton;
  PropertyRowTemplate := UiOwner.FindRequiredComponent('PropertyRowTemplate') as TCastleUserInterface;
  HierarchyRowParent := UiOwner.FindRequiredComponent('HierarchyRowParent') as TCastleUserInterface;
  PropertyRowParent := UiOwner.FindRequiredComponent('PropertyRowParent') as TCastleUserInterface;
  ScrollLogs := UiOwner.FindRequiredComponent('ScrollLogs') as TCastleScrollView;
  LabelLog := UiOwner.FindRequiredComponent('LabelLog') as TCastleLabel;
  LabelLogHeader := UiOwner.FindRequiredComponent('LabelLogHeader') as TCastleLabel;
  LabelPropertiesHeader := UiOwner.FindRequiredComponent('LabelPropertiesHeader') as TCastleLabel;
  LabelInspectorFps := UiOwner.FindRequiredComponent('LabelInspectorFps') as TCastleLabel;
  LabelInspectorHelp := UiOwner.FindRequiredComponent('LabelInspectorHelp') as TCastleLabel;
  SliderOpacity := UiOwner.FindRequiredComponent('SliderOpacity') as TCastleFloatSlider;

  ForceUsingFallbackFont(Ui);

  CheckboxShowEvenInternal.OnChange := {$ifdef FPC}@{$endif} UpdateHierarchy;
  ButtonHierarchyShow.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyShow;
  ButtonHierarchyHide.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyHide;
  ButtonLogShow.OnClick := {$ifdef FPC}@{$endif} ClickLogShow;
  ButtonLogHide.OnClick := {$ifdef FPC}@{$endif} ClickLogHide;
  ButtonPropertiesShow.OnClick := {$ifdef FPC}@{$endif} ClickPropertiesShow;
  ButtonPropertiesHide.OnClick := {$ifdef FPC}@{$endif} ClickPropertiesHide;

  { initial state of UI to show/hide }
  HorizontalGroupShow.Exists := false;
  ButtonPropertiesShow.Exists := false;
  ButtonHierarchyShow.Exists := false;
  ButtonLogShow.Exists := false;

  FOpacity := 0.0; // to force setting below call SetOpacity
  Opacity := DefaultOpacity;
  SliderOpacity.Value := Opacity;
  SliderOpacity.OnChange := {$ifdef FPC}@{$endif} ChangeOpacity;

  { initialize templates }
  SerializedHierarchyRowTemplate := TSerializedComponent.CreateFromString(ComponentToString(HierarchyRowTemplate));
  FreeAndNil(HierarchyRowTemplate);
  SerializedPropertyRowTemplate := TSerializedComponent.CreateFromString(ComponentToString(PropertyRowTemplate));
  FreeAndNil(PropertyRowTemplate);

  { initialize log }
  ApplicationProperties.OnLog.Add({$ifdef FPC}@{$endif} LogCallback);
  LabelLogHeader.Caption := 'Log (0)';
  LabelLog.Caption := '';
end;

destructor TCastleInspector.Destroy;
begin
  ApplicationProperties.OnLog.Remove({$ifdef FPC}@{$endif} LogCallback);

  { set to nil by SetSelectedComponent, to detach free notification }
  SelectedComponent := nil;

  inherited;
end;

procedure TCastleInspector.ForceUsingFallbackFont(const Ui: TCastleUserInterface);
var
  Child: TCastleUserInterface;
begin
  if Ui is TCastleUserInterfaceFont then
    TCastleUserInterfaceFont(Ui).CustomFont := FallbackFont;
  for Child in Ui do
    ForceUsingFallbackFont(Child);
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
      ForceUsingFallbackFont(HierarchyButton);
      HierarchyButton.OnClick := {$ifdef FPC}@{$endif} ClickHierarchyRow;
      HierarchyButton.Culling := true; // many such buttons are often not visible, in scroll view
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

  LabelInspectorFps.Caption := 'FPS: ' + Container.Fps.ToString;

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
    RectHierarchy.ColorPersistent.Alpha := Value;
  end;
end;

procedure TCastleInspector.SynchronizeButtonsToShow;
begin
  ButtonLogShow.Exists := not RectLog.Exists;
  ButtonPropertiesShow.Exists := not RectProperties.Exists;
  ButtonHierarchyShow.Exists := not RectHierarchy.Exists;
  HorizontalGroupShow.Exists := ButtonHierarchyShow.Exists or ButtonLogShow.Exists or ButtonPropertiesShow.Exists;
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

procedure TCastleInspector.LogCallback(const Message: String);
begin
  { Use InsideLogCallback to prevent from infinite recursion,
    in case anything inside would also cause WritelnLog. }
  if InsideLogCallback then Exit;

  InsideLogCallback := true;
  try
    // We use TrimRight to strip traling newline
    LabelLog.Text.Add(TrimRight(Message));
    LabelLogHeader.Caption := 'Log (' + IntToStr(LabelLog.Text.Count) + ')';
    ScrollLogs.Scroll := ScrollLogs.ScrollMax;
  finally InsideLogCallback := false end;
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
    ForceUsingFallbackFont(Ui);
    PropertyRowParent.InsertFront(Ui);
    PropertyRowParent.Culling := true; // many such rows are often not visible, in scroll view

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

end.
