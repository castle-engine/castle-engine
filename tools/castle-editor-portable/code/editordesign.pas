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

{ Edit the "design file" (.castle-user-interface, .castle-transform,
  .castle-component) in the editor.
  Used by EditorViewProject. }
unit EditorDesign;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleInternalInspector,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleFindFiles,
  CastleTransformManipulate, CastleClassUtils;

type
  { Toolbar subset that is handled by TDesign, because it is only
    useful when the design is loaded.
    Components of this toolbar are visually created inside
    editorviewproject.castle-user-interface, but TViewProject
    doesn't do anything with them, instead it is handled by TDesign. }
  TDesignToolbar = class
    ButtonSaveDesign: TCastleButton;
    CheckboxShowHierarchy: TCastleCheckbox;
    CheckboxShowProperties: TCastleCheckbox;
    ToolbarTransformManipulate: TCastleUserInterface;
    ButtonSelect, ButtonTranslate, ButtonRotate, ButtonScale: TCastleButton;
  end;

  { Edit the "design file" (.castle-user-interface, .castle-transform,
    .castle-component) in the editor.
    This exists only when the design is loaded. }
  TDesign = class(TCastleUserInterface)
  private
    { Root of the design, saved/loaded to component file. Never @nil. }
    FDesignRoot: TCastleUserInterface;

    { URL of the currently open design. Never empty. }
    FCurrentDesignUrl: String;

    { Owner of all components saved/loaded to the design file. Never @nil.
      Also owner of a temporary viewport for .castle-transform,
      in general this owns everything specific to display currrent design. }
    FDesignOwner: TComponent;

    FToolbar: TDesignToolbar;

    // UI inside
    ContainerLoadedDesign: TCastleUserInterface;
    Properties: TCastleComponentProperties;
    Hierarchy: TCastleComponentsHierarchy;
    TransformHover: TCastleTransformHover;
    TransformManipulate: TCastleTransformManipulate;

    // UI callbacks
    procedure ClickSaveDesign(Sender: TObject);
    procedure HierarchySelect(Sender: TObject);
    procedure ChangeShowHierarchy(Sender: TObject);
    procedure ChangeShowProperties(Sender: TObject);
    procedure ClickSelect(Sender: TObject);
    procedure ClickTranslate(Sender: TObject);
    procedure ClickRotate(Sender: TObject);
    procedure ClickScale(Sender: TObject);

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

    { Update ContainerLoadedDesignSize size and anchor,
      depending on visibility of Hierarchy / Properties. }
    procedure UpdateContainerLoadedDesignSize;

    { Set pressed and visibility of buttons based on
      - TransformManipulate.Mode
      - and whether a TCastleTransform is selected. }
    procedure UpdateTransformButtons;

    { UI under given mouse position.

      Note: In most cases, prefer to call HoverComponent.
      HoverUserInterface *does not* consider transforms within TCastleViewport,
      it will just return TCastleViewport if mouse is over it.

      @param(EnterNestedDesigns If @true, we enter children loaded by TCastleDesign,
        in TCastleDesign.Design. This should almost never be used: such children
        are not selectable, their names may collide with names in main design etc.) }
    function HoverUserInterface(const AMousePosition: TVector2;
      const EnterNestedDesigns: Boolean = false): TCastleUserInterface;

    { UI or transform under given mouse position.
      AMousePosition is like for HoverUserInterface. }
    function HoverComponent(const AMousePosition: TVector2): TCastleComponent;

    property DesignRoot: TCastleUserInterface read FDesignRoot;
    property CurrentDesignUrl: String read FCurrentDesignUrl;
    property DesignOwner: TComponent read FDesignOwner;
  public
    constructor Create(
      const AOwner: TComponent;
      const AToolbar: TDesignToolbar;
      const NewDesignOwner: TComponent;
      const NewDesignRoot: TCastleUserInterface;
      const NewCurrentDesignUrl: String); reintroduce;
    destructor Destroy; override;

    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

implementation

uses SysUtils,
  CastleStringUtils, CastleUriUtils, CastleUtils, CastleFilesUtils,
  CastleInternalPhysicsVisualization, CastleViewport,
  CastleTransform, CastleLog;

constructor TDesign.Create(
  const AOwner: TComponent;
  const AToolbar: TDesignToolbar;
  const NewDesignOwner: TComponent;
  const NewDesignRoot: TCastleUserInterface;
  const NewCurrentDesignUrl: String);
begin
  inherited Create(AOwner);

  // create UI inside TDesign

  ContainerLoadedDesign := TCastleUserInterface.Create(Self);
  ContainerLoadedDesign.WidthFraction := 0.5;
  ContainerLoadedDesign.HeightFraction := 1;
  ContainerLoadedDesign.Anchor(hpMiddle);
  ContainerLoadedDesign.Anchor(vpMiddle);
  InsertFront(ContainerLoadedDesign);

  Hierarchy := TCastleComponentsHierarchy.Create(Self);
  Hierarchy.ButtonHierarchyHide.Exists := false; // hide, not handled
  Hierarchy.WidthFraction := 0.25;
  Hierarchy.HeightFraction := 1;
  Hierarchy.OnSelect := {$ifdef FPC}@{$endif} HierarchySelect;
  InsertFront(Hierarchy);

  Properties := TCastleComponentProperties.Create(Self);
  Properties.ButtonPropertiesHide.Exists := false; // hide, not handled
  Properties.WidthFraction := 0.25;
  Properties.HeightFraction := 1;
  Properties.Anchor(hpRight);
  InsertFront(Properties);

  // handle toolbar
  FToolbar := AToolbar;
  AToolbar.ButtonSaveDesign.OnClick := {$ifdef FPC}@{$endif} ClickSaveDesign;
  AToolbar.CheckboxShowHierarchy.OnChange := {$ifdef FPC}@{$endif} ChangeShowHierarchy;
  AToolbar.CheckboxShowProperties.OnChange := {$ifdef FPC}@{$endif} ChangeShowProperties;
  AToolbar.ButtonSelect.OnClick := {$ifdef FPC}@{$endif} ClickSelect;
  AToolbar.ButtonTranslate.OnClick := {$ifdef FPC}@{$endif} ClickTranslate;
  AToolbar.ButtonRotate.OnClick := {$ifdef FPC}@{$endif} ClickRotate;
  AToolbar.ButtonScale.OnClick := {$ifdef FPC}@{$endif} ClickScale;

  // load the design
  FDesignOwner := NewDesignOwner;
  FDesignRoot := NewDesignRoot;
  FCurrentDesignUrl := NewCurrentDesignUrl;
  ContainerLoadedDesign.InsertFront(DesignRoot);
  Hierarchy.Root := DesignRoot;

  UpdateContainerLoadedDesignSize;

  { Tracking hover and manipulated objects }
  TransformHover := TCastleTransformHover.Create(Self);
  TransformManipulate := TCastleTransformManipulate.Create(Self);
  TransformManipulate.Mode := mmTranslate;

  UpdateTransformButtons;
end;

destructor TDesign.Destroy;
begin
  FDesignRoot := nil;
  // this actually frees everything inside FDesignRoot
  FreeAndNil(FDesignOwner);
  inherited;
end;

class function TDesign.Selectable(const Child: TComponent): Boolean;
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

function TDesign.Deletable(const Child: TComponent): Boolean;
begin
  { Note: When changing conditions here, consider also updating ReasonWhyNotDeletable,
    that explains to user *why* something is not deletable. }

  Result := Selectable(Child) and
    (not (csSubComponent in Child.ComponentStyle)) and
    (Child <> DesignRoot) and (not (Child is TCastleToolTransform));
end;

procedure TDesign.FreeComponentRecursively(const C: TComponent);

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

procedure TDesign.UpdateContainerLoadedDesignSize;
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

procedure TDesign.ChangeShowHierarchy(Sender: TObject);
begin
  Hierarchy.Exists := FToolbar.CheckboxShowHierarchy.Checked;
  UpdateContainerLoadedDesignSize;
end;

procedure TDesign.ChangeShowProperties(Sender: TObject);
begin
  Properties.Exists := FToolbar.CheckboxShowProperties.Checked;
  UpdateContainerLoadedDesignSize;
end;

procedure TDesign.ClickSaveDesign(Sender: TObject);
begin
  UserInterfaceSave(DesignRoot, CurrentDesignUrl);
  WritelnLog('Saved design to %s', [CurrentDesignUrl]);
end;

function TDesign.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyLeftBracket) then
  begin
    FToolbar.CheckboxShowHierarchy.Checked := not FToolbar.CheckboxShowHierarchy.Checked;
    { Call the OnChange explicitly, because it is not automatically
      called when changing Checked programmatically. }
    ChangeShowHierarchy(nil);
    Exit(true);
  end;

  if Event.IsKey(keyRightBracket) then
  begin
    FToolbar.CheckboxShowProperties.Checked := not FToolbar.CheckboxShowProperties.Checked;
    { Call the OnChange explicitly, because it is not automatically
      called when changing Checked programmatically. }
    ChangeShowProperties(nil);
    Exit(true);
  end;

  if Event.IsKey(CtrlS) then
  begin
    ClickSaveDesign(nil);
    Exit(true);
  end;

  if Event.IsMouseButton(buttonLeft) and (TransformHover.Current <> nil) then
  begin
    Properties.SelectedComponent := TransformHover.Current;
    Hierarchy.SelectedComponent := TransformHover.Current;
    TransformManipulate.SetSelected([TransformHover.Current]);
    UpdateTransformButtons;
    Exit(true);
  end;
end;

procedure TDesign.HierarchySelect(Sender: TObject);
begin
  Properties.SelectedComponent := Hierarchy.SelectedComponent;
  if Hierarchy.SelectedComponent is TCastleTransform then // also checks its <> nil
    TransformManipulate.SetSelected([Hierarchy.SelectedComponent])
  else
    TransformManipulate.SetSelected([]);
  UpdateTransformButtons;
end;

procedure TDesign.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  // Update TransformHover.Current
  procedure UpdateTransformHover;
  var
    HoverC: TComponent;
  begin
    HoverC := HoverComponent(Container.MousePosition);
    if HoverC is TCastleTransform then
      TransformHover.Current := TCastleTransform(HoverC)
    else
      TransformHover.Current := nil;
  end;

begin
  inherited;
  UpdateTransformHover;
end;

procedure TDesign.UpdateTransformButtons;
begin
  FToolbar.ToolbarTransformManipulate.Exists := TransformManipulate.SelectedCount <> 0;
  FToolbar.ButtonSelect.Pressed := TransformManipulate.Mode = mmSelect;
  FToolbar.ButtonTranslate.Pressed := TransformManipulate.Mode = mmTranslate;
  FToolbar.ButtonRotate.Pressed := TransformManipulate.Mode = mmRotate;
  FToolbar.ButtonScale.Pressed := TransformManipulate.Mode = mmScale;
end;

procedure TDesign.ClickSelect(Sender: TObject);
begin
  TransformManipulate.Mode := mmSelect;
  UpdateTransformButtons;
end;

procedure TDesign.ClickTranslate(Sender: TObject);
begin
  TransformManipulate.Mode := mmTranslate;
  UpdateTransformButtons;
end;

procedure TDesign.ClickRotate(Sender: TObject);
begin
  TransformManipulate.Mode := mmRotate;
  UpdateTransformButtons;
end;

procedure TDesign.ClickScale(Sender: TObject);
begin
  TransformManipulate.Mode := mmScale;
  UpdateTransformButtons;
end;

function TDesign.HoverUserInterface(
  const AMousePosition: TVector2;
  const EnterNestedDesigns: Boolean): TCastleUserInterface;

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

  { Is C selectable, or we should enter it anyway because of EnterNestedDesigns. }
  function Enter(const Parent, C: TCastleUserInterface): Boolean;
  begin
    Result := Selectable(C) or
      ( EnterNestedDesigns and
        (Parent is TCastleDesign) and
        (TCastleDesign(Parent).InternalDesign = C) );
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
        if Enter(C, C.Controls[I]) then
        begin
          Result := ControlUnder(C.Controls[I], MousePos, false);
          if Result <> nil then Exit;
        end;

      { Next try to find children, with TestWithBorder=true, so it tries harder
        to find something. }
      for I := C.ControlsCount - 1 downto 0 do
        if Enter(C, C.Controls[I]) then
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

begin
  Result := nil;
  if DesignRoot is TCastleUserInterface then
    Result := ControlUnder(DesignRoot as TCastleUserInterface, AMousePosition, true)
  { TODO: prepare for DesignRoot being TCastleTransform.
  else
  if DesignRoot is TCastleTransform then
    Result := FDesignViewportForTransforms};
end;

function TDesign.HoverComponent(const AMousePosition: TVector2): TCastleComponent;

  function SelectionFromRayHit(const RayHit: TRayCollision): TCastleTransform;
  var
    I: Integer;
  begin
    // set outer-most TCastleTransformReference, if any, to show selection at TCastleTransformReference
    for I := RayHit.Count - 1 downto 0 do
      if (RayHit[I].Item is TCastleTransformReference) and Selectable(RayHit[I].Item) then
        Exit(RayHit[I].Item);

    { We want the inner-most TCastleTransform hit, but not anything non-selectable
      (which means csTransient) (to avoid hitting gizmo).
      Moreover, things that are children of csTransient should always be treated like csTransient
      too, e.g. GizmoSelect in TInternalCastleEditorGizmo should never be returned by this.

      So if there's anything non-selectable (which just means csTransient),
      searching from outer (world),
      then pick the selected transform right above it. }

    for I := RayHit.Count - 1 downto 0 do
      if not Selectable(RayHit[I].Item) then
      begin
        if I + 1 < RayHit.Count then
          Exit(RayHit[I + 1].Item)
        else
          Exit(nil);
      end;

    { Nothing non-selectable (csTransient) on the list, pick the inner-most transform }
    if RayHit.Count <> 0 then
      Result := RayHit[0].Item
    else
      Result := nil;
  end;

var
  Viewport: TCastleViewport;
  RayOrigin, RayDirection: TVector3;
  RayHit: TRayCollision;
begin
  { Note: We don't call here CurrentViewport.

    Unlike with CurrentViewport, here we don't try to forcefully select viewport
    (from hover or selection). If the mouse is over non-viewport, then this
    should return non-viewport.

    Also, unlike CurrentViewport,
    we don't want to remember last hovered/selected viewport here,
    it would be weird for user here.

    Also, unlike CurrentViewport, we don't pass EnterNestedDesigns as
    UpdateCurrentViewport does. We don't need to look for viewport that aggressive. }

  Result := HoverUserInterface(AMousePosition);
  if Result is TCastleViewport then // also checks Result <> nil
  begin
    Viewport := TCastleViewport(Result);
    Viewport.PositionToRay(AMousePosition, true, RayOrigin, RayDirection);
    RayHit := Viewport.Items.WorldRay(RayOrigin, RayDirection);
    try
      if RayHit <> nil then
        Result := SelectionFromRayHit(RayHit);
    finally FreeAndNil(RayHit) end;
  end;
end;

end.
