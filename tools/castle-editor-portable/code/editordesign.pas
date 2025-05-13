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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleFindFiles;

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
    ButtonTranslate, ButtonRotate, ButtonScale: TCastleButton;
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

    // UI callbacks
    procedure ClickSaveDesign(Sender: TObject);
    procedure HierarchySelect(const Selected: TComponent);
    procedure ChangeShowHierarchy(Sender: TObject);
    procedure ChangeShowProperties(Sender: TObject);

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

    procedure PressToggleHierarchy;
    procedure PressToggleProperties;
  end;

implementation

uses SysUtils,
  CastleStringUtils, CastleUriUtils, CastleUtils, CastleFilesUtils,
  CastleInternalPhysicsVisualization, CastleClassUtils, CastleViewport,
  CastleTransform;

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

  // load the design
  FDesignOwner := NewDesignOwner;
  FDesignRoot := NewDesignRoot;
  FCurrentDesignUrl := NewCurrentDesignUrl;
  ContainerLoadedDesign.InsertFront(DesignRoot);
  Hierarchy.Root := DesignRoot;

  UpdateContainerLoadedDesignSize;
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

procedure TDesign.HierarchySelect(const Selected: TComponent);
begin
  Properties.SelectedComponent := Selected;
  Hierarchy.SelectComponent(Selected);
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

procedure TDesign.PressToggleHierarchy;
begin
  FToolbar.CheckboxShowHierarchy.Checked := not FToolbar.CheckboxShowHierarchy.Checked;
  { Call the OnChange explicitly, because it is not automatically
    called when changing  Checked programmatically. }
  ChangeShowHierarchy(nil);
end;

procedure TDesign.PressToggleProperties;
begin
  FToolbar.CheckboxShowProperties.Checked := not FToolbar.CheckboxShowProperties.Checked;
  { Call the OnChange explicitly, because it is not automatically
    called when changing  Checked programmatically. }
  ChangeShowProperties(nil);
end;

procedure TDesign.ClickSaveDesign(Sender: TObject);
begin
  UserInterfaceSave(DesignRoot, CurrentDesignUrl);
end;

end.
