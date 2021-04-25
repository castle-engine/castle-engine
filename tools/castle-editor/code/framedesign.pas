{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Frame where you can design
  a xxx.castle-user-interface or xxx.castle-transform file. }
unit FrameDesign;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  Spin, Buttons, Menus, Contnrs, Generics.Collections,
  // for TOIPropertyGrid usage
  ObjectInspector, PropEdits, PropEditUtils, GraphPropEdits,
  // CGE units
  CastleControl, CastleUIControls, CastlePropEdits, CastleDialogs,
  CastleSceneCore, CastleKeysMouse, CastleVectors, CastleRectangles,
  CastleViewport, CastleClassUtils, CastleControls, CastleTiledMap,
  CastleCameras, CastleBoxes, CastleTransform, CastleDebugTransform,
  CastleColors,
  // editor units
  FrameAnchors, VisualizeTransform,
  CastleUndoSystem;

type
  { Frame to visually design component hierarchy. }
  TDesignFrame = class(TFrame)
    ButtonResetTransformation: TButton;
    ButtonClearAnchorDeltas: TButton;
    ButtonViewportMenu: TSpeedButton;
    LabelHeaderTransform: TLabel;
    LabelHeaderUi: TLabel;
    LabelEventsInfo: TLabel;
    LabelSizeInfo: TLabel;
    LabelSelectedViewport: TLabel;
    MenuViewportNavigation2D: TMenuItem;
    MenuTreeViewItemRename: TMenuItem;
    MenuTreeViewItemAddTransform: TMenuItem;
    MenuTreeViewItemAddUserInterface: TMenuItem;
    MenuTreeViewItemDelete: TMenuItem;
    MenuTreeViewItemPaste: TMenuItem;
    MenuTreeViewItemCopy: TMenuItem;
    MenuTreeViewItemDuplicate: TMenuItem;
    MenuViewportNavigationFly: TMenuItem;
    MenuItemViewportCameraCurrentFromInitial: TMenuItem;
    MenuItemSeparator123: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MenuItemViewportCamera2DViewInitial: TMenuItem;
    MenuItemViewportCameraSetInitial: TMenuItem;
    MenuItemViewportCameraViewAll: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemViewportSort2D: TMenuItem;
    MenuViewportNavigationWalk: TMenuItem;
    MenuViewportNavigationThirdPerson: TMenuItem;
    MenuViewportNavigationExamine: TMenuItem;
    MenuViewportNavigationNone: TMenuItem;
    PanelLayoutTop: TPanel;
    PanelLayoutTransform: TPanel;
    PanelEventsInfo: TPanel;
    PanelAnchors: TPanel;
    MenuViewport: TPopupMenu;
    MenuTreeView: TPopupMenu;
    SelfAnchorsFrame: TAnchorsFrame;
    ParentAnchorsFrame: TAnchorsFrame;
    CheckParentSelfAnchorsEqual: TCheckBox;
    ControlProperties: TPageControl;
    ControlsTree: TTreeView;
    LabelUIScaling: TLabel;
    LabelControlSelected: TLabel;
    LabelHierarchy: TLabel;
    PanelMiddleTop: TPanel;
    PanelMiddle: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    ButtonInteractMode: TSpeedButton;
    ButtonModifyUiMode: TSpeedButton;
    ButtonTransformSelectMode: TSpeedButton;
    ButtonTransformTranslateMode: TSpeedButton;
    ButtonTransformRotateMode: TSpeedButton;
    ButtonTransformScaleMode: TSpeedButton;
    Splitter1: TSplitter;
    TabLayoutScrollBox: TScrollBox;
    SpinEditSnap: TSpinEdit;
    SplitterLeft: TSplitter;
    SplitterRight: TSplitter;
    TabAll: TTabSheet;
    TabEvents: TTabSheet;
    TabLayout: TTabSheet;
    TabBasic: TTabSheet;
    UpdateObjectInspector: TTimer;
    procedure ButtonClearAnchorDeltasClick(Sender: TObject);
    procedure ButtonResetTransformationClick(Sender: TObject);
    procedure ButtonTransformRotateModeClick(Sender: TObject);
    procedure ButtonTransformScaleModeClick(Sender: TObject);
    procedure ButtonTransformSelectModeClick(Sender: TObject);
    procedure ButtonTransformTranslateModeClick(Sender: TObject);
    procedure ButtonViewportMenuClick(Sender: TObject);
    procedure CheckParentSelfAnchorsEqualChange(Sender: TObject);
    procedure ControlsTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ControlsTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ControlsTreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ControlsTreeEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ControlsTreeEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure ControlsTreeEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ControlsTreeSelectionChanged(Sender: TObject);
    procedure ButtonInteractModeClick(Sender: TObject);
    procedure ButtonModifyUiModeClick(Sender: TObject);
    procedure MenuItemAddComponentClick(Sender: TObject);
    procedure MenuTreeViewItemRenameClick(Sender: TObject);
    procedure MenuTreeViewItemDeleteClick(Sender: TObject);
    procedure MenuTreeViewItemCopyClick(Sender: TObject);
    procedure MenuTreeViewItemDuplicateClick(Sender: TObject);
    procedure MenuItemViewportCamera2DViewInitialClick(Sender: TObject);
    procedure MenuItemViewportCameraCurrentFromInitialClick(Sender: TObject);
    procedure MenuItemViewportCameraViewAllClick(Sender: TObject);
    procedure MenuItemViewportCameraSetInitialClick(Sender: TObject);
    procedure MenuItemViewportSort2DClick(Sender: TObject);
    procedure MenuTreeViewItemPasteClick(Sender: TObject);
    procedure MenuTreeViewPopup(Sender: TObject);
    procedure MenuViewportNavigation2DClick(Sender: TObject);
    procedure MenuViewportNavigationExamineClick(Sender: TObject);
    procedure MenuViewportNavigationFlyClick(Sender: TObject);
    procedure MenuViewportNavigationNoneClick(Sender: TObject);
    procedure MenuViewportNavigationWalkClick(Sender: TObject);
    procedure MenuViewportNavigationThirdPersonClick(Sender: TObject);
    procedure ClearDesign;
    procedure RenameSelectedItem;
    procedure PerformUndoRedo(const UHE: TUndoHistoryElement);
    procedure PerformRedo;
    procedure PerformUndo;
    procedure UpdateObjectInspectorTimer(Sender: TObject);
  protected
    procedure SetParent(AParent: TWinControl); override;
  private
    type
      { UI layer that can intercept mouse clicks and drags. }
      TDesignerLayer = class(TCastleUserInterface)
      strict private
        type
          TDraggingMode = (dmNone, dmTranslate, dmResize);
        var
          PendingMove: TVector2;
          DraggingMode: TDraggingMode;
          ResizingHorizontal: THorizontalPosition; //< Defined only when DraggingMode=dmResize
          ResizingVertical: TVerticalPosition; //< Defined only when DraggingMode=dmResize
          LabelHover, LabelSelected: TCastleLabel;
          RectHover, RectSelected: TCastleRectangleControl;
        { Should clicking inside UI rectangle start resizing (not only moving?). }
        function IsResizing(const UI: TCastleUserInterface; const Position: TVector2;
          out Horizontal: THorizontalPosition;
          out Vertical: TVerticalPosition): Boolean;
      public
        Frame: TDesignFrame;
        constructor Create(AOwner: TComponent); override;
        function Press(const Event: TInputPressRelease): Boolean; override;
        function Release(const Event: TInputPressRelease): Boolean; override;
        function Motion(const Event: TInputMotion): Boolean; override;
        procedure Render; override;
        function HoverUserInterface(const AMousePosition: TVector2): TCastleUserInterface;
        function HoverTransform(const AMousePosition: TVector2): TCastleTransform;
      end;

      TTreeNodeMap = class(specialize TDictionary<TComponent, TTreeNode>)
      end;

      TMode = (
        moInteract,
        moModifyUi,
        moTransformSelect,
        moTransformTranslate,
        moTransformRotate,
        moTransformScale
      );

      TTreeNodeSide = (tnsRight, tnsBottom, tnsTop);

      TInspectorType = (itBasic, itLayout, itEvents, itAll);

    const
      TransformModes = [
        moTransformSelect,
        moTransformTranslate,
        moTransformRotate,
        moTransformScale
      ];

    var
      Inspector: array [TInspectorType] of TOIPropertyGrid;
      FUndoSystem: TUndoSystem;
      PropertyEditorHook: TPropertyEditorHook;
      FDesignUrl: String;
      FDesignRoot: TComponent;
      { Owner of all components saved/loaded to the design file.
        Also owner of a temporary viewport for .castle-transform,
        in general this owns everything specific to display currrent design. }
      DesignOwner: TComponent;
      FDesignerLayer: TDesignerLayer;
      FDesignModified: Boolean;
      CastleControl: TCastleControlBase;
      TreeNodeMap: TTreeNodeMap;
      Mode: TMode;
      InsideToggleModeClick: Boolean;
      ControlsTreeNodeUnderMouse: TTreeNode;
      ControlsTreeNodeUnderMouseSide: TTreeNodeSide;
      PendingErrorBox: String;
      VisualizeTransformHover, VisualizeTransformSelected: TVisualizeTransform;

    procedure CastleControlOpen(Sender: TObject);
    procedure CastleControlResize(Sender: TObject);
    procedure CastleControlUpdate(Sender: TObject);
    procedure CastleControlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CastleControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ChangeViewportNavigation(
      const NewNavigation: TCastleNavigation);
    function ComponentCaption(const C: TComponent): String;
    function ControlsTreeAllowDrag(const Src, Dst: TTreeNode): Boolean;
    procedure FrameAnchorsChange(Sender: TObject);
    procedure AdjustUserInterfaceAnchorsToKeepRect(const UI: TCastleUserInterface;
      const RenderRectBeforeChange: TFloatRectangle);
    // Save and restore selection.
    // Careful: you can use it only if the operation between will *never* free any of them.
    //procedure SelectionRestoreAndFree(var Selection: Classes.TList);
    //function SelectionSave: Classes.TList;

    { Calculate Selected list, non-nil <=> non-empty }
    procedure GetSelected(out Selected: TComponentList;
      out SelectedCount: Integer);

    { If there is exactly one item selected, and it is TCastleUserInterface,
      return it. Otherwise return nil. }
    function GetSelectedUserInterface: TCastleUserInterface;
    procedure SetSelectedUserInterface(const Value: TCastleUserInterface);
    property SelectedUserInterface: TCastleUserInterface
      read GetSelectedUserInterface write SetSelectedUserInterface;

    { If there is exactly one item selected, return it. Otherwise return nil. }
    function GetSelectedComponent: TComponent;
    procedure SetSelectedComponent(const Value: TComponent);
    property SelectedComponent: TComponent
      read GetSelectedComponent write SetSelectedComponent;

    { If the selected items all have the same TCastleViewport parent,
      return it. Otherwise return nil. }
    function SelectedViewport: TCastleViewport;

    { If there is exactly one item selected, and it is TCastleTransform,
      return it. Otherwise return nil. }
    function GetSelectedTransform: TCastleTransform;
    procedure SetSelectedTransform(const Value: TCastleTransform);
    property SelectedTransform: TCastleTransform
      read GetSelectedTransform write SetSelectedTransform;

    procedure InspectorBasicFilter(Sender: TObject; AEditor: TPropertyEditor;
      var aShow: Boolean);
    procedure InspectorLayoutFilter(Sender: TObject; AEditor: TPropertyEditor;
      var aShow: Boolean);
    procedure MarkModified;
    function UndoMessageModified(const Sel: TPersistent;
      const ModifiedProperty, ModifiedValue: String; const SelectedCount: Integer): String;
    { PropertyGridModified and PropertyEditorModified are called when
      something changes in the design.
      PropertyGridModified and PropertyEditorModified are both called when
      something changes within Object Inspector basic features
      (such as editing string, boolean, enum or numeric values)
      In this case PropertyEditorModified usually comes first.
      In case something changed outside of Object inspector (e.g. drag-and-drops,
      rename components in treeview, add components, etc.)
      only PropertyGridModified is called
      In case a custom dialog is used to change a value
      (e.g. a Color picker, TStrings editor, Open File dialogue, etc.)
      then only PropertyEditorModified is called. }
    procedure PropertyGridModified(Sender: TObject);
    procedure PropertyEditorModified(Sender: TObject);
    { Is Child selectable and visible in hierarchy. }
    class function Selectable(const Child: TComponent): Boolean; static;
    { Is Child deletable by user (this implies it is also selectable). }
    function Deletable(const Child: TComponent): Boolean;
    procedure UpdateDesign;
    procedure UpdateSelectedControl;
    function ProposeName(const ComponentClass: TComponentClass;
      const ComponentsOwner: TComponent): String;
    procedure UpdateLabelSizeInfo(const UI: TCastleUserInterface);
    { Update anchors shown, based on UI state.
      Updates which buttons are pressed inside 2 TAnchorFrame instances.
      If AllowToHideParentAnchorsFrame, updates also checkbox
      "parent and self equal" and may hide the parent anchor frame if yes. }
    procedure UpdateAnchors(const UI: TCastleUserInterface;
      const AllowToHideParentAnchorsFrame: Boolean);
    procedure ChangeMode(const NewMode: TMode);
    procedure ModifiedOutsideObjectInspector(const UndoComment: String;
      const UndoCommentPriority: TUndoCommentPriority; const UndoOnRelease: Boolean = false);
    procedure InspectorFilter(Sender: TObject;
      AEditor: TPropertyEditor; var AShow: Boolean; const Section: TPropertySection);
    procedure GizmoHasModifiedParent(Sender: TObject);
    procedure GizmoStopDrag(Sender: TObject);
  public
    OnUpdateFormCaption: TNotifyEvent;
    OnSelectionChanged: TNotifyEvent;
    function RenamePossible: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveDesign(const Url: String);
    { Changes DesignRoot, DesignUrl and all the associated user-interface. }
    procedure OpenDesign(const NewDesignRoot, NewDesignOwner: TComponent;
      const NewDesignUrl: String);
    procedure OpenDesign(const NewDesignUrl: String);
    procedure NewDesign(const ComponentClass: TComponentClass;
      const ComponentOnCreate: TNotifyEvent);

    function FormCaption: String;
    procedure BeforeProposeSaveDesign;
    procedure AddComponent(const ComponentClass: TComponentClass;
      const ComponentOnCreate: TNotifyEvent);
    function AddComponent(const ParentComponent: TComponent; const ComponentClass: TComponentClass;
      const ComponentOnCreate: TNotifyEvent): TComponent;
    procedure DeleteComponent;
    procedure CopyComponent;
    procedure PasteComponent;
    procedure DuplicateComponent;
    { set UIScaling values. }
    procedure UIScaling(const UIScaling: TUIScaling;
      const UIReferenceWidth, UIReferenceHeight: Single);

    property UndoSystem: TUndoSystem read FUndoSystem;
    property DesignUrl: String read FDesignUrl;
    { Root saved/loaded to component file }
    property DesignRoot: TComponent read FDesignRoot;
    property DesignModified: Boolean read FDesignModified;
    procedure RecordUndo(const UndoComment: String;
      const UndoCommentPriority: TUndoCommentPriority; const ItemIndex: Integer = -1);

    procedure CurrentComponentApiUrl(var Url: String);
  end;

implementation

uses // use Windows unit with FPC 3.0.x, to get TSplitRectType enums
  {$ifdef VER3_0} {$ifdef MSWINDOWS} Windows, {$endif} {$endif}
  TypInfo, StrUtils, Math, Graphics, Types, Dialogs, LCLType, ObjInspStrConsts,
  Castle2DSceneManager, CastleComponentSerialize, CastleFileFilters,
  CastleGLUtils, CastleImages, CastleLog,  CastleProjection, CastleScene,
  CastleShellCtrls, CastleStringUtils, CastleThirdPersonNavigation,
  CastleTimeUtils, CastleURIUtils, CastleUtils,
  X3DLoad,
  EditorUtils, FormProject;

{$R *.lfm}

{$ifdef VER3_0}
{$ifdef MSWINDOWS}
type
  TSplitRectType = Windows.TSplitRectType;
const
  srLeft = TSplitRectType.srLeft;
  srRight = TSplitRectType.srRight;
  srTop = TSplitRectType.srTop;
  srBottom = TSplitRectType.srBottom;
{$endif}
{$endif}

function ParentRenderRect(const UI: TCastleUserInterface): TFloatRectangle;
begin
  if UI.Parent = nil then
    Result := FloatRectangle(UI.Container.Rect)
  else
    Result := UI.Parent.RenderRect;
end;

{ TDesignFrame.TDesignerLayer ------------------------------------------------ }

constructor TDesignFrame.TDesignerLayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FullSize := true;

  RectHover := TCastleRectangleControl.Create(Self);
  RectHover.Color := Vector4(0, 0, 0, 0.25);
  RectHover.Exists := false;
  { LabelHover with scaling sometimes looks too tiny.
    Also, positioning RectHover is easier without UI scaling. }
  RectHover.EnableUIScaling := false;
  InsertFront(RectHover);

  LabelHover := TCastleLabel.Create(Self);
  LabelHover.Anchor(hpMiddle);
  LabelHover.Anchor(vpMiddle);
  LabelHover.FontSize := 15;
  LabelHover.EnableUIScaling := false;
  RectHover.InsertFront(LabelHover);

  RectSelected := TCastleRectangleControl.Create(Self);
  RectSelected.Color := Vector4(0, 0, 0, 0.25);
  RectSelected.Exists := false;
  RectSelected.EnableUIScaling := false;
  InsertFront(RectSelected);

  LabelSelected := TCastleLabel.Create(Self);
  LabelSelected.Anchor(hpMiddle);
  LabelSelected.Anchor(vpMiddle);
  LabelSelected.FontSize := 15;
  LabelSelected.EnableUIScaling := false;
  RectSelected.InsertFront(LabelSelected);
end;

function TDesignFrame.TDesignerLayer.HoverUserInterface(
  const AMousePosition: TVector2): TCastleUserInterface;

  { Like TCastleUserInterface.CapturesEventsAtPosition, but
    - ignores CapturesEvents
    - uses RenderRectWithBorder (to be able to drag complete control)
    - doesn't need "if the control covers the whole Container" hack. }
  function SimpleCapturesEventsAtPosition(const UI: TCastleUserInterface;
    const Position: TVector2): Boolean;
  begin
    Result := UI.RenderRectWithBorder.Contains(Position);
  end;

  function ControlUnder(const C: TCastleUserInterface;
    const MousePos: TVector2): TCastleUserInterface;
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

    if C.GetExists then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if TDesignFrame.Selectable(C.Controls[I]) then
        begin
          Result := ControlUnder(C.Controls[I], MousePos);
          if Result <> nil then Exit;
        end;
      //if C.CapturesEventsAtPosition(MousePos) then
      if SimpleCapturesEventsAtPosition(C, MousePos) and
         { Do not select TCastleNavigation, they would always obscure TCastleViewport. }
         (not (C is TCastleNavigation)) then
        Result := C;
    end;
  end;

  function MouseOverControl(const Control: TCastleControlBase): Boolean;
  var
    PosInClient: TPoint;
  begin
    PosInClient := Control.ScreenToClient(Mouse.CursorPos);
    Result := Control.ClientRect.Contains(PosInClient);
  end;

begin
  if MouseOverControl(Frame.CastleControl) and
     (Frame.DesignRoot is TCastleUserInterface) then
    Result := ControlUnder(Frame.DesignRoot as TCastleUserInterface, AMousePosition)
  else
    Result := nil;
end;

function TDesignFrame.TDesignerLayer.HoverTransform(
  const AMousePosition: TVector2): TCastleTransform;
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

  { If HoverUserInterface didn't have a useful viewport, try SelectedViewport.
    This way you can select stuff in viewport, even when it's obscured
    e.g. by a TCastleButton. }
  if (Viewport = nil) and
     (Frame.SelectedViewport <> nil) and
     Frame.SelectedViewport.RenderRectWithBorder.Contains(AMousePosition) then
    Viewport := Frame.SelectedViewport;

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

function TDesignFrame.TDesignerLayer.IsResizing(const UI: TCastleUserInterface;
  const Position: TVector2; out Horizontal: THorizontalPosition;
  out Vertical: TVerticalPosition): Boolean;
const
  BorderDragMargin = 10;
var
  R: TFloatRectangle;
  ResizeWidth, ResizeHeight: Boolean;
  ResizeDisabledReason: String;
begin
  R := UI.RenderRectWithBorder;

  UI.EditorAllowResize(ResizeWidth, ResizeHeight, ResizeDisabledReason);

  { the order of checking (top or bottom) matters in case of very
    small heights. }
  if ResizeHeight and R.TopPart(BorderDragMargin).Contains(Position) then
    Vertical := vpTop
  else
  if ResizeHeight and R.BottomPart(BorderDragMargin).Contains(Position) then
    Vertical := vpBottom
  else
    Vertical := vpMiddle;

  if ResizeWidth and R.RightPart(BorderDragMargin).Contains(Position) then
    Horizontal := hpRight
  else
  if ResizeWidth and R.LeftPart(BorderDragMargin).Contains(Position) then
    Horizontal := hpLeft
  else
    Horizontal := hpMiddle;

  Result := (Vertical <> vpMiddle) or (Horizontal <> hpMiddle);
end;

function TDesignFrame.TDesignerLayer.Press(
  const Event: TInputPressRelease): Boolean;
var
  UI: TCastleUserInterface;
  T: TCastleTransform;
begin
  Result := inherited Press(Event);
  if Result then Exit;

  if (Frame.Mode = moModifyUi) and
     (Event.IsMouseButton(buttonLeft) or Event.IsMouseButton(buttonRight)) then
  begin
    { Left mouse button selects before moving/resizing.
      Right mouse button doesn't. This allows to change the size of the control
      without changing the selected control, e.g. when you want to change
      the size of TCastleScrollView without
      selecting TCastleScrollView.ScrollArea inside. }
    if Event.IsMouseButton(buttonLeft) then
      Frame.SelectedUserInterface := HoverUserInterface(Event.Position);

    UI := Frame.SelectedUserInterface;
    if UI <> nil then
    begin
      if IsResizing(UI, Event.Position, ResizingHorizontal, ResizingVertical) then
        DraggingMode := dmResize
      else
        DraggingMode := dmTranslate;
      Exit(ExclusiveEvents);
    end;

    PendingMove := TVector2.Zero;
  end;

  if (Frame.Mode in TransformModes) and
      Event.IsMouseButton(buttonLeft) then
  begin
    T := HoverTransform(Event.Position);
    { Do not change Frame.SelectedTransform in case T is nil,
      as then clicking in moTransformXxx modes at some place where no scene
      exists would deselect UI item, also deselecting current viewport.
      So it's not useful, and not expected. }
    if T <> nil then
    begin
      Frame.SelectedTransform := T;
      { No need for this Exit(true).
        In practice, it is acceptable and even comfortable that a single click
        both selects a transform, and allows to navigate (e.g. TCastleExamineNavigation
        will handle this click too, and allow to rotate).
        Even when Exit(true) was done only when "Frame.SelectedTransform <> T",
        it seemed unnecessary. }
      //Exit(ExclusiveEvents);
    end;
  end;
end;

function TDesignFrame.TDesignerLayer.Release(const Event: TInputPressRelease): Boolean;
var
  Sel: TComponent;
begin
  Result := inherited Press(Event);
  if Result then Exit;

  if (Event.IsMouseButton(buttonLeft) or Event.IsMouseButton(buttonRight)) then
  begin
    DraggingMode := dmNone;

    { Note, that we may want to have better comment message here }
    if Frame.UndoSystem.ScheduleRecordUndoOnRelease then
    begin
      Sel := Frame.GetSelectedComponent;
      if Sel <> nil then
        Frame.RecordUndo('Drag''n''drop ' + Sel.Name, ucHigh)
      else
        Frame.RecordUndo('Drag''n''drop', ucHigh);
    end;
  end;
end;

function TDesignFrame.TDesignerLayer.Motion(const Event: TInputMotion): Boolean;

  { Grow the rectangle from the appropriate sides, following
    ResizingHorizontal/Vertical field values. }
  function ResizeRect(const R: TFloatRectangle; const Move: TVector2): TFloatRectangle;
  begin
    Result := R;
    case ResizingHorizontal of
      hpLeft : Result := Result.GrowLeft(-Move.X);
      hpRight: Result := Result.GrowRight(Move.X);
    end;
    case ResizingVertical of
      vpBottom: Result := Result.GrowBottom(-Move.Y);
      vpTop   : Result := Result.GrowTop(Move.Y);
    end;
  end;

  function DragAllowed(const UI: TCastleUserInterface; const Move: TVector2): Boolean;
  var
    CurrentRect, ResultingRect, ParentR: TFloatRectangle;
  begin
    CurrentRect := UI.RenderRectWithBorder;
    case DraggingMode of
      dmTranslate: ResultingRect := CurrentRect.Translate(Move);
      dmResize   : ResultingRect := ResizeRect(CurrentRect, Move);
    end;
    ParentR := ParentRenderRect(UI);
    { Only allow movement/resize if control will not go outside of parent,
      unless it was already outside.

      Note that we limit to the parent rect *without border*,
      that is: we don't allow to move control over a border of the parent.
      This makes sense, since FullSize and WidthFraction = 1 keep the control
      within the parent rect *without border* too.
    }
    Result := (not ParentR.Contains(CurrentRect)) or
      ParentR.Contains(ResultingRect);
  end;

  procedure ApplyDrag(const UI: TCastleUserInterface; X, Y: Single);
  const
    MinWidth  = 10;
    MinHeight = 10;
  begin
    if not DragAllowed(UI, Vector2(X, Y)) then Exit;
    case DraggingMode of
      dmTranslate:
        begin
          UI.HorizontalAnchorDelta := UI.HorizontalAnchorDelta + X;
          UI.VerticalAnchorDelta   := UI.VerticalAnchorDelta   + Y;
        end;
      dmResize:
        begin
          case ResizingHorizontal of
            hpLeft:
              begin
                // do not allow to set UI.Width < MinWidth, by limiting X
                if UI.Width - X < MinWidth then
                  X := UI.Width - MinWidth;
                UI.Width := UI.Width - X;

                case UI.HorizontalAnchorSelf of
                  hpLeft  : UI.HorizontalAnchorDelta := UI.HorizontalAnchorDelta + X;
                  hpMiddle: UI.HorizontalAnchorDelta := UI.HorizontalAnchorDelta + X / 2;
                  //hpRight : UI.HorizontalAnchorDelta := no need to change
                end;
              end;
            hpRight:
              begin
                // do not allow to set UI.Width < MinWidth, by limiting X
                if UI.Width + X < MinWidth then
                  X := MinWidth - UI.Width;
                UI.Width := UI.Width + X;

                case UI.HorizontalAnchorSelf of
                  // hpLeft  : UI.HorizontalAnchorDelta := no need to change
                  hpMiddle: UI.HorizontalAnchorDelta := UI.HorizontalAnchorDelta + X / 2;
                  hpRight : UI.HorizontalAnchorDelta := UI.HorizontalAnchorDelta + X;
                end;
              end;
          end;
          case ResizingVertical of
            vpBottom:
              begin
                // do not allow to set UI.Height < MinHeight, by limiting Y
                if UI.Height - Y < MinHeight then
                  Y := UI.Height - MinHeight;
                UI.Height := UI.Height - Y;

                case UI.VerticalAnchorSelf of
                  vpBottom: UI.VerticalAnchorDelta := UI.VerticalAnchorDelta + Y;
                  vpMiddle: UI.VerticalAnchorDelta := UI.VerticalAnchorDelta + Y / 2;
                  //vpTop : UI.VerticalAnchorDelta := no need to change
                end;
              end;
            vpTop:
              begin
                // do not allow to set UI.Height < MinHeight, by limiting Y
                if UI.Height + Y < MinHeight then
                  Y := MinHeight - UI.Height;
                UI.Height := UI.Height + Y;

                case UI.VerticalAnchorSelf of
                  //vpBottom: UI.VerticalAnchorDelta := no need to change
                  vpMiddle: UI.VerticalAnchorDelta := UI.VerticalAnchorDelta + Y / 2;
                  vpTop   : UI.VerticalAnchorDelta := UI.VerticalAnchorDelta + Y;
                end;
              end;
          end;
        end;
    end;

    { We pass UndoOnRelease = true to defer recording Undo until Release is called
      to avoid recording Undo on every user Motion event.
      In this case UndoComment and UndoPriority do not matter,
      they will be replaced by appropriate values on Release. }
    Frame.ModifiedOutsideObjectInspector('', ucLow, true); // UndoComment doesn't matter here
  end;

  function ResizingCursor(const H: THorizontalPosition;
    const V: TVerticalPosition): TMouseCursor;
  begin
    case H of
      hpLeft:
        case V of
          vpBottom: Result := mcResizeBottomLeft;
          vpMiddle: Result := mcResizeLeft;
          vpTop   : Result := mcResizeTopLeft;
        end;
      hpMiddle:
        case V of
          vpBottom: Result := mcResizeBottom;
          vpMiddle: raise EInternalError.Create('No resizing, cannot determine cursor');
          vpTop   : Result := mcResizeTop;
        end;
      hpRight:
        case V of
          vpBottom: Result := mcResizeBottomRight;
          vpMiddle: Result := mcResizeRight;
          vpTop   : Result := mcResizeTopRight;
        end;
    end;
  end;

  procedure UpdateCursor;
  var
    UI: TCastleUserInterface;
    WouldResizeHorizontal: THorizontalPosition;
    WouldResizeVertical: TVerticalPosition;
    NewCursor: TMouseCursor;
  begin
    if Frame.Mode <> moModifyUi then
      NewCursor := mcDefault
    else
    case DraggingMode of
      dmNone:
        begin
          // calculate cursor based on what would happen if you Press
          UI := HoverUserInterface(Event.Position);
          if UI <> nil then
          begin
            if IsResizing(UI, Event.Position,
              WouldResizeHorizontal, WouldResizeVertical) then
            begin
              NewCursor := ResizingCursor(
                WouldResizeHorizontal, WouldResizeVertical);
            end else
              NewCursor := mcHand;
          end else
            NewCursor := mcDefault;
        end;
      dmTranslate:
        NewCursor := mcHand;
      dmResize:
        NewCursor := ResizingCursor(ResizingHorizontal, ResizingVertical);
    end;
    Frame.CastleControl.Container.OverrideCursor := NewCursor;
  end;

  procedure UpdateHoverTransform;
  begin
    if Frame.Mode in TransformModes then
      Frame.VisualizeTransformHover.Parent := HoverTransform(Event.Position) // works also in case HoverTransform is nil
    else
      Frame.VisualizeTransformHover.Parent := nil;
  end;

var
  UI: TCastleUserInterface;
  Move: TVector2;
  Snap: Single;
begin
  Result := inherited Motion(Event);
  if Result then Exit;

  { in case user left mouse button, but the event didn't reach us for some reason
    (maybe can happen e.g. if you Alt+Tab during dragging?),
    reset DraggingMode. }
  if (DraggingMode <> dmNone) and
     // neither buttonLeft nor buttonRight
     ([buttonLeft, buttonRight] * Event.Pressed = []) then
    DraggingMode := dmNone;

  if (Frame.Mode = moModifyUi) and (DraggingMode <> dmNone) then
  begin
    UI := Frame.SelectedUserInterface;
    if UI <> nil then
    begin
      Move := (Event.Position - Event.OldPosition) / UI.UIScale;

      Snap := Frame.SpinEditSnap.Value;
      if Snap <> 0 then
      begin
        PendingMove += Move;
        while Abs(PendingMove.X) >= Snap do
        begin
          ApplyDrag(UI, Sign(PendingMove.X) * Snap, 0);
          PendingMove.X := PendingMove.X - Sign(PendingMove.X) * Snap;
        end;
        while Abs(PendingMove.Y) >= Snap do
        begin
          ApplyDrag(UI, 0, Sign(PendingMove.Y) * Snap);
          PendingMove.Y := PendingMove.Y - Sign(PendingMove.Y) * Snap;
        end;
      end else
      begin
        ApplyDrag(UI, Move.X, Move.Y);
      end;

      Exit(ExclusiveEvents);
    end;
  end;

  UpdateCursor;
  UpdateHoverTransform;
end;

procedure TDesignFrame.TDesignerLayer.Render;

  procedure UpdateAttachedLabel(const UI: TCastleUserInterface;
    const UIRect: TFloatRectangle;
    const Lab: TCastleLabel; const Rect: TCastleRectangleControl;
    const LabelColor: TCastleColor);
  begin
    if UI <> nil then
    begin
      Lab.Caption := Frame.ComponentCaption(UI);
      Lab.Color := LabelColor;

      Rect.Exists := true;
      Rect.Width := Lab.EffectiveWidth + 6;
      Rect.Height := Lab.EffectiveHeight + 6;
      { Place in left-top corner. Because:
        - We want left-xxx corner, this way if the label is cut off,
          at least the beginning looks OK.
        - We don't want left-bottom corner, as that's where child controls
          are placed by default, so the text would be over them too often. }
      Rect.Anchor(hpLeft, UIRect.Left);
      Rect.Anchor(vpBottom, UIRect.Top);

      if Rect.RenderRect.Top > Rect.Container.Height then
        // put Rect inside UI, otherwise it would be offscreen
        Rect.Anchor(vpTop, vpBottom, UIRect.Top);
    end else
      Rect.Exists := false;
  end;

var
  SelectedUI, HoverUI: TCastleUserInterface;
  SelectedUIRect, HoverUIRect: TFloatRectangle;
begin
  inherited;

  SelectedUI := Frame.SelectedUserInterface;
  if SelectedUI <> nil then
  begin
    SelectedUIRect := SelectedUI.RenderRectWithBorder;
    DrawRectangleOutline(SelectedUIRect, White);
    DrawRectangleOutline(SelectedUIRect.Grow(-1), Black);
  end;

  HoverUI := HoverUserInterface(Container.MousePosition);
  if HoverUI <> nil then
  begin
    HoverUIRect := HoverUI.RenderRectWithBorder;
    DrawRectangleOutline(HoverUIRect, Vector4(1, 1, 0, 0.75));

    // TODO: for now hide, too confusing in case of auto-sized label/button
    {
    if not UI.FullSize then
    begin
      // show desired Width / Height, useful e.g. for TCastleImageControl
      R.Width  := UI.Width  * UI.UIScale;
      R.Height := UI.Height * UI.UIScale;
      DrawRectangleOutline(R, Vector4(1, 1, 0, 0.25));
    end;
    }
  end;

  if (HoverUI <> nil) and (HoverUI = SelectedUI) then
  begin
    UpdateAttachedLabel(SelectedUI, SelectedUIRect, LabelSelected, RectSelected,
      ColorHoverAndSelected);
    // make sure to hide RectHover in this case
    UpdateAttachedLabel(nil, TFloatRectangle.Empty, LabelHover, RectHover, Black);
  end else
  begin
    UpdateAttachedLabel(SelectedUI, SelectedUIRect, LabelSelected, RectSelected,
      ColorSelected);
    UpdateAttachedLabel(HoverUI, HoverUIRect, LabelHover, RectHover,
      ColorHover);

    { Improve special case, when both RectSelected and RectHover would
      be displayed on top of each other. In this case,
      shift the RectHover (as the more often changing one). }
    if RectSelected.Exists and
       RectHover.Exists and
       (RectSelected.RenderRect.Bottom = RectHover.RenderRect.Bottom) and
       (RectSelected.RenderRect.Left   = RectHover.RenderRect.Left  ) then
    begin
      RectHover.VerticalAnchorDelta := RectHover.VerticalAnchorDelta -
        RectSelected.EffectiveHeight;
    end;
  end;
end;

{ TDesignFrame --------------------------------------------------------------- }

constructor TDesignFrame.Create(TheOwner: TComponent);

  function CommonInspectorCreate: TOIPropertyGrid;
  begin
    Result := TOIPropertyGrid.Create(Self);
    Result.PropertyEditorHook := PropertyEditorHook;
    Result.Align := alClient;
    Result.OnModified := @PropertyGridModified;
    Result.CheckboxForBoolean := true;
    Result.PreferredSplitterX := 150;
    Result.ValueFont.Bold := true;
    Result.ShowGutter := false;
    Result.ReadOnlyColor := clWindowText;
  end;

begin
  inherited;

  PropertyEditorHook := TPropertyEditorHook.Create(Self);

  FUndoSystem := TUndoSystem.Create(Self);

  Inspector[itBasic] := CommonInspectorCreate;
  Inspector[itBasic].Parent := TabBasic;
  Inspector[itBasic].OnEditorFilter := @InspectorBasicFilter;
  Inspector[itBasic].Filter := tkProperties;

  Inspector[itLayout] := CommonInspectorCreate;
  Inspector[itLayout].Parent := TabLayout;
  Inspector[itLayout].OnEditorFilter := @InspectorLayoutFilter;
  Inspector[itLayout].Filter := tkProperties;
  Inspector[itLayout].Align := alBottom;
  Inspector[itLayout].AnchorToNeighbour(akTop, 0, PanelLayoutTop);

  Inspector[itAll] := CommonInspectorCreate;
  Inspector[itAll].Parent := TabAll;
  Inspector[itAll].Filter := tkProperties;

  Inspector[itEvents] := CommonInspectorCreate;
  Inspector[itEvents].Parent := TabEvents;
  Inspector[itEvents].Filter := tkMethods;
  Inspector[itEvents].AnchorToNeighbour(akTop, 0, PanelEventsInfo);

  CastleControl := TCastleControlBase.Create(Self);
  CastleControl.Parent := PanelMiddle;
  CastleControl.Align := alClient;
  CastleControl.OnResize := @CastleControlResize;
  CastleControl.OnOpen := @CastleControlOpen;
  CastleControl.OnUpdate := @CastleControlUpdate;
  CastleControl.StencilBits := 8; // enable shadow volumes
  CastleControl.OnDragOver := @CastleControlDragOver;
  CastleControl.OnDragDrop := @CastleControlDragDrop;

  {$ifdef DEBUG_GIZMO_PICK}
  TCastleControl.MainControl := CastleControl;
  {$endif DEBUG_GIZMO_PICK}

  FDesignerLayer := TDesignerLayer.Create(Self);
  FDesignerLayer.Frame := Self;
  CastleControl.Controls.InsertFront(FDesignerLayer);

  // It's too easy to change it visually and forget, so we set it from code
  ControlProperties.ActivePage := TabBasic;

  TreeNodeMap := TTreeNodeMap.Create;

  SelfAnchorsFrame.OnAnchorChange := @FrameAnchorsChange;
  ParentAnchorsFrame.OnAnchorChange := @FrameAnchorsChange;

  VisualizeTransformHover := TVisualizeTransform.Create(Self, true);
  VisualizeTransformSelected := TVisualizeTransform.Create(Self, false);
  VisualizeTransformSelected.OnParentModified := @GizmoHasModifiedParent;
  VisualizeTransformSelected.OnGizmoStopDrag := @GizmoStopDrag;

  //ChangeMode(moInteract);
  ChangeMode(moModifyUi); // most expected default, it seems

  BuildComponentsMenu(MenuTreeViewItemAddUserInterface, MenuTreeViewItemAddTransform, @MenuItemAddComponentClick);
  // Input_Interact (for gizmos) reacts to both left and right
  Input_Interact.MouseButton2Use := true;
  Input_Interact.MouseButton2 := buttonRight;
end;

destructor TDesignFrame.Destroy;
begin
  FreeAndNil(TreeNodeMap);
  inherited Destroy;
end;

procedure TDesignFrame.SaveDesign(const Url: String);
begin
  if DesignRoot is TCastleUserInterface then
    UserInterfaceSave(TCastleUserInterface(DesignRoot), Url)
  else
  if DesignRoot is TCastleTransform then
    TransformSave(TCastleTransform(DesignRoot), Url)
  else
    raise EInternalError.Create('We can only save DesignRoot that descends from TCastleUserInterface or TCastleTransform');
  FDesignModified := false;
  FDesignUrl := Url; // after successfull save
  OnUpdateFormCaption(Self);
end;

procedure TDesignFrame.ClearDesign;
begin
  ControlsTree.Items.Clear;
  UpdateSelectedControl;
  //CastleControl.Controls.Clear; // don't clear it, leave DesignerLayer
  FDesignRoot := nil;

  // this actually frees everything inside DesignRoot
  FreeAndNil(DesignOwner);
end;

procedure TDesignFrame.PerformUndoRedo(const UHE: TUndoHistoryElement);

  function GetInspectorForActiveTab: TOIPropertyGrid;
  var
    InspectorType: TInspectorType;
  begin
    for InspectorType in TInspectorType do
    begin
      if Inspector[InspectorType].Parent = ControlProperties.ActivePage then
        Exit(Inspector[InspectorType]);
    end;
    Result := nil;
  end;

var
  NewDesignOwner: TComponent;
  InspectorType: TInspectorType;
begin
  for InspectorType in TInspectorType do
    Inspector[InspectorType].SaveChanges;

  NewDesignOwner := TComponent.Create(Self);
  OpenDesign(StringToComponent(UHE.Data, NewDesignOwner), NewDesignOwner, FDesignUrl);

  if UHE.Selected <> '' then
    SetSelectedComponent(NewDesignOwner.FindRequiredComponent(UHE.Selected));
  if UHE.ItemIndex >= 0 then
  begin
    ControlProperties.TabIndex := UHE.TabIndex;
    GetInspectorForActiveTab.SetItemIndexAndFocus(UHE.ItemIndex);
  end;
end;

procedure TDesignFrame.PerformRedo;
begin
  PerformUndoRedo(UndoSystem.Redo);
end;

procedure TDesignFrame.PerformUndo;
begin
  {//save current edited value and
  if not UndoSystem.IsRedoPossible then
  begin
    RecordUndo;
    UndoSystem.Undo;
  end;}
  PerformUndoRedo(UndoSystem.Undo);
end;

procedure TDesignFrame.UpdateObjectInspectorTimer(Sender: TObject);
//var
//  InspectorType: TInspectorType;
begin
  { In many cases, properties may change but property editor doesn't reflect it.
    E.g.
    - TCastleTransform changes by ExposeTransforms mechanism
    - Caption changes because you modified Name, and they used to match.
    The only universal solution to make OI up-to-date seems to be to just
    occasionally refresh it. }

  // TODO: This is not good, it breaks editing within object inspector, resets cursor
  //for InspectorType in TInspectorType do
  //  Inspector[InspectorType].RefreshPropertyValues;
end;

procedure TDesignFrame.OpenDesign(const NewDesignRoot, NewDesignOwner: TComponent;
  const NewDesignUrl: String);
var
  Background: TCastleRectangleControl;
  TempViewport: TCastleViewport;
begin
  ClearDesign;

  { We use CastleControl.Controls.InsertBack here, to keep DesignerLayer
    in the front. }

  if NewDesignRoot is TCastleUserInterface then
  begin
    CastleControl.Controls.InsertBack(NewDesignRoot as TCastleUserInterface)
  end else
  if NewDesignRoot is TCastleTransform then
  begin
    TempViewport := TCastleViewport.Create(NewDesignOwner);
    TempViewport.Transparent := true;
    TempViewport.Items.UseHeadlight := hlOn;
    TempViewport.Items.Add(NewDesignRoot as TCastleTransform);
    TempViewport.FullSize := true;
    TempViewport.AutoCamera := true;
    TempViewport.AutoNavigation := true;
    CastleControl.Controls.InsertBack(TempViewport);
  end else
    raise EInternalError.Create('DesignRoot from file does not descend from TCastleUserInterface or TCastleTransform');

  // make background defined
  Background := TCastleRectangleControl.Create(NewDesignOwner);
  Background.Color := Vector4(0.5, 0.5, 0.5, 1);
  Background.FullSize := true;
  CastleControl.Controls.InsertBack(Background);

  // replace DesignXxx variables, once loading successfull
  FDesignRoot := NewDesignRoot;
  FDesignUrl := NewDesignUrl;
  DesignOwner := NewDesignOwner;
  FDesignModified := DesignUrl = ''; // when opening '', mark new hierarchy modified

  // Allows object inspectors to find matching components, e.g. when editing Viewport.Items.MainScene
  PropertyEditorHook.LookupRoot := DesignOwner;
  PropertyEditorHook.AddHandlerModified(@PropertyEditorModified);

  UpdateDesign;
  OnUpdateFormCaption(Self);
end;

procedure TDesignFrame.OpenDesign(const NewDesignUrl: String);
var
  NewDesignRoot, NewDesignOwner: TComponent;
  Mime: String;
begin
  NewDesignOwner := TComponent.Create(Self);

  try
    Mime := URIMimeType(NewDesignUrl);
    if Mime = 'text/x-castle-user-interface' then
      NewDesignRoot := UserInterfaceLoad(NewDesignUrl, NewDesignOwner)
    else
    if Mime = 'text/x-castle-transform' then
      NewDesignRoot := TransformLoad(NewDesignUrl, NewDesignOwner)
    else
      raise Exception.CreateFmt('Unrecognized file extension %s (MIME type %s)',
        [ExtractFileExt(NewDesignUrl), Mime]);
  except
    { Testcase: try to load using UserInterfaceLoad a file
      that has TCastleTransform inside. UserInterfaceLoad makes EInvalidCast. }
    on E: Exception do
    begin
      E.Message := 'Error when loading ' + URIDisplay(NewDesignUrl) + ': ' + E.Message;
      raise;
    end;
  end;

  UndoSystem.ClearUndoHistory;

  OpenDesign(NewDesignRoot, NewDesignOwner, NewDesignUrl);

  RecordUndo('Open design', High(TUndoCommentPriority)); // Technically this is impossible to see this Undo comment
end;

function TDesignFrame.FormCaption: String;
var
  DesignName: String;
begin
  // calculate DesignName
  if DesignUrl <> '' then
    DesignName := ExtractURIName(DesignUrl)
  else
  if DesignRoot is TCastleTransform then
    DesignName := 'New Transform'
  else
  if DesignRoot is TCastleUserInterface then
    DesignName := 'New User Interface'
  else
    // generic, should not happen now
    DesignName := 'New Component';
  Result := '[' + Iff(DesignModified, '*', '') + DesignName + '] ';
end;

procedure TDesignFrame.BeforeProposeSaveDesign;
var
  InspectorType: TInspectorType;
begin
  { call SaveChanges to be sure to have good DesignModified value.
    Otherwise when editing e.g. TCastleButton.Caption,
    you can press F9 and have DesignModified = false,
    because PropertyGridModified doesn't occur because we actually
    press "tab" to focus another control. }
  for InspectorType in TInspectorType do
    Inspector[InspectorType].SaveChanges;
end;

procedure TDesignFrame.AddComponent(const ComponentClass: TComponentClass;
  const ComponentOnCreate: TNotifyEvent);
var
  Selected: TComponentList;
  SelectedCount: Integer;
  ParentComponent:TComponent;
begin
  // calculate ParentComponent
  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount = 1 then
      ParentComponent := Selected.First
    else
      ParentComponent := DesignRoot;
  finally FreeAndNil(Selected) end;

  AddComponent(ParentComponent, ComponentClass, ComponentOnCreate);
end;

function TDesignFrame.AddComponent(const ParentComponent: TComponent; const ComponentClass: TComponentClass;
  const ComponentOnCreate: TNotifyEvent): TComponent;

  procedure FinishAddingComponent(const NewComponent: TComponent);
  begin
    UpdateDesign;
    SelectedComponent := NewComponent; // select after adding, makes it natural to edit
    ModifiedOutsideObjectInspector('Add ' + NewComponent.Name + ' to ' + ParentComponent.Name, ucHigh);
  end;

  function AddTransform(const ParentComponent: TCastleTransform): TCastleTransform;
  begin
    if ComponentClass.InheritsFrom(TCastleTransform) then
    begin
      Result := ComponentClass.Create(DesignOwner) as TCastleTransform;
      if Assigned(ComponentOnCreate) then // call ComponentOnCreate ASAP after constructor
        ComponentOnCreate(Result);
      Result.Name := ProposeName(ComponentClass, DesignOwner);
      ParentComponent.Add(Result);
      FinishAddingComponent(Result);
    end else
      raise Exception.Create(Format('Cannot add component class %s when the parent is a TCastleTransform scendant (%s). Select a parent that descends from TCastleUserInterface.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
  end;

  function AddUserInterface(const ParentComponent: TCastleUserInterface): TCastleUserInterface;
  begin
    if ComponentClass.InheritsFrom(TCastleUserInterface) then
    begin
      Result := ComponentClass.Create(DesignOwner) as TCastleUserInterface;
      if Assigned(ComponentOnCreate) then // call ComponentOnCreate ASAP after constructor
        ComponentOnCreate(Result);
      Result.Name := ProposeName(ComponentClass, DesignOwner);
      ParentComponent.InsertFront(Result);
      FinishAddingComponent(Result);
    end else
      raise Exception.Create(Format('Cannot add component class %s when the parent is a TCastleUserInterface descendant (%s). Select a parent that descends from TCastleTransform, for example select Viewport.Items.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
  end;

begin
  { Cancel editing the component name, when adding a component.
    See https://trello.com/c/IC6NQx0X/59-bug-adding-a-component-to-a-component-that-is-being-currently-renamed-triggers-and-exception . }
  if ControlsTree.Selected <> nil then
    ControlsTree.Selected.EndEdit(true);

  if ParentComponent is TCastleUserInterface then
  begin
    Exit(AddUserInterface(ParentComponent as TCastleUserInterface));
  end else
  if ParentComponent is TCastleTransform then
  begin
    Exit(AddTransform(ParentComponent as TCastleTransform));
  end else
    raise Exception.Create(Format('Cannot add to the parent of class %s, select other parent before adding.',
      [ParentComponent.ClassName]))
end;

procedure TDesignFrame.DeleteComponent;

  function FirstDeletableComponent(const List: TComponentList): TComponent;
  var
    I: Integer;
  begin
    for I := 0 to List.Count - 1 do
      if Deletable(List[I]) then
        Exit(List[I]);
    Result := nil;
  end;

  procedure FreeTransformChildren(const T: TCastleTransform); forward;
  procedure FreeUiChildren(const C: TCastleUserInterface); forward;

  { Delete C and all children.
    We have to delete things recursively, otherwise they would keep existing,
    taking resources and reserving names in DesignRoot,
    even though they would not be visible when disconnected from parent
    hierarchy. }
  procedure FreeRecursively(const C: TComponent);
  begin
    if not Deletable(C) then
      Exit;
    if C is TCastleTransform then
    begin
      FreeTransformChildren(TCastleTransform(C));
    end else
    if C is TCastleUserInterface then
    begin
      FreeUiChildren(TCastleUserInterface(C));
      if C is TCastleViewport then
        FreeTransformChildren(TCastleViewport(C).Items);
    end;
    C.Free;
  end;

  procedure FreeTransformChildren(const T: TCastleTransform);
  var
    I: Integer;
  begin
    for I := T.Count - 1 downto 0 do
      if Deletable(T[I]) then
        FreeRecursively(T[I]);
  end;

  procedure FreeUiChildren(const C: TCastleUserInterface);
  var
    I: Integer;
  begin
    for I := C.ControlsCount - 1 downto 0 do
      if Deletable(C.Controls[I]) then
        FreeRecursively(C.Controls[I]);
  end;

var
  Selected: TComponentList;
  SelectedCount: Integer;
  C: TComponent;
  UndoSummary: String;
begin
  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount <> 0 then // check this, otherwise Selected may be nil
    begin
      if SelectedCount = 1 then
        UndoSummary := Selected[0].Name
      else
        UndoSummary := SelectedCount.ToString + ' components';

      { We depend on the fact TComponentList observes freed items,
        and removes them automatically.
        This way also freeing something that frees something else
        should work (although we don't really need it now,
        DesignOwner owns everything). }

      repeat
        C := FirstDeletableComponent(Selected);
        if C <> nil then
          FreeRecursively(C)
        else
          Break;
      until false;

      // temporarily disable this event, as some pointers are invalid now
      ControlsTree.OnSelectionChanged := nil;
      ControlsTree.Items.Clear;
      TreeNodeMap.Clear;
      ControlsTree.OnSelectionChanged := @ControlsTreeSelectionChanged;

      UpdateDesign;

      { call this after UpdateDesign, otherwise tree is not ready,
        and events caused by ModifiedOutsideObjectInspector may expect it is. }
      ModifiedOutsideObjectInspector('Delete ' + UndoSummary, ucHigh);
    end;
  finally FreeAndNil(Selected) end;
end;

procedure TDesignFrame.CopyComponent;
var
  Sel: TComponent;
begin
  Sel := SelectedComponent;
  if (Sel <> nil) and
     (not (csSubComponent in Sel.ComponentStyle)) then
  begin
    Clipboard.AsText := ComponentToString(Sel)
  end else
  begin
    ErrorBox('Select exactly one component, that is not a subcomponent, to copy');
  end;
end;

procedure TDesignFrame.PasteComponent;

  procedure FinishAddingComponent(const NewComponent: TComponent);
  begin
    UpdateDesign;
    SelectedComponent := NewComponent; // select after adding, makes it natural to edit
    ModifiedOutsideObjectInspector('Paste ' + NewComponent.Name, ucHigh);
  end;

var
  Selected: TComponentList;
  SelectedCount: Integer;
  ParentComponent, NewComponent: TComponent;
begin
  try
    NewComponent := StringToComponent(Clipboard.AsText, DesignOwner);
  except
    on E: Exception do
    begin
      ErrorBox('Cliboard doesn''t seem to contain a copied component.' + NL + NL +
        'Trying to deserialize it failed with the error:' + NL + NL +
        ExceptMessage(E));
      Exit;
    end;
  end;

  // calculate ParentComponent
  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount = 1 then
      ParentComponent := Selected.First
    else
      ParentComponent := DesignRoot;
  finally FreeAndNil(Selected) end;

  if NewComponent is TCastleUserInterface then
  begin
    if not (ParentComponent is TCastleUserInterface) then
    begin
      ErrorBox('Clipboard contains a TCastleUserInterface instance, you need to select a TCastleUserInterface as a parent before doing "Paste Component"');
      FreeAndNil(NewComponent);
      Exit;
    end;
    (ParentComponent as TCastleUserInterface).InsertFront(NewComponent as TCastleUserInterface);
    FinishAddingComponent(NewComponent);
  end else
  if NewComponent is TCastleTransform then
  begin
    if not (ParentComponent is TCastleTransform) then
    begin
      ErrorBox('Clipboard contains a TCastleTransform instance, you need to select a TCastleTransform as a parent before doing "Paste Component"');
      FreeAndNil(NewComponent);
      Exit;
    end;
    (ParentComponent as TCastleTransform).Add(NewComponent as TCastleTransform);
    FinishAddingComponent(NewComponent);
  end else
  begin
    ErrorBox(Format('Clipboard contains an instance of %s class, cannot insert it into the design',
      [NewComponent.ClassName]));
    FreeAndNil(NewComponent);
  end;
end;

procedure TDesignFrame.DuplicateComponent;

  procedure FinishAddingComponent(const NewComponent: TComponent);
  var
    OldComponentName: String;
  begin
    OldComponentName := SelectedComponent.Name;
    UpdateDesign;
    SelectedComponent := NewComponent; // select after adding, makes it natural to edit
    ModifiedOutsideObjectInspector('Duplicate ' + OldComponentName + '->' + NewComponent.Name, ucHigh);
  end;

  procedure DuplicateUserInterface(const Selected: TCastleUserInterface);
  var
    ParentComp, NewComp: TCastleUserInterface;
    ComponentString: String;
    InsertIndex: Integer;
  begin
    ParentComp := Selected.Parent;
    if ParentComp = nil then
    begin
      ErrorBox('To duplicate, select component with exactly one parent');
      Exit;
    end;
    ComponentString := ComponentToString(Selected);
    NewComp := StringToComponent(ComponentString, DesignOwner) as TCastleUserInterface;
    InsertIndex := ParentComp.IndexOfControl(Selected);
    ParentComp.InsertControl(InsertIndex + 1, NewComp);
    FinishAddingComponent(NewComp);
  end;

  procedure DuplicateTransform(const Selected: TCastleTransform);
  var
    ParentComp, NewComp: TCastleTransform;
    ComponentString: String;
    InsertIndex: Integer;
  begin
    ParentComp := Selected.UniqueParent;
    if ParentComp = nil then
    begin
      ErrorBox('To duplicate, select component with exactly one parent');
      Exit;
    end;
    ComponentString := ComponentToString(Selected);
    NewComp := StringToComponent(ComponentString, DesignOwner) as TCastleTransform;
    InsertIndex := ParentComp.List.IndexOf(Selected);
    ParentComp.Insert(InsertIndex + 1, NewComp);
    FinishAddingComponent(NewComp);
  end;

var
  Sel: TComponent;
begin
  Sel := SelectedComponent;

  if (Sel <> nil) and
     (not (csSubComponent in Sel.ComponentStyle)) then
  begin
    if Sel is TCastleUserInterface then
      DuplicateUserInterface(Sel as TCastleUserInterface)
    else
    if Sel is TCastleTransform then
      DuplicateTransform(Sel as TCastleTransform)
    else
      ErrorBox('To duplicate, select TCastleUserInterface or TCastleTransform component');
  end else
    ErrorBox('To duplicate, select exactly one component that is not a subcomponent');
end;

function TDesignFrame.SelectedViewport: TCastleViewport;
var
  Selected: TComponentList;
  SelectedCount, I: Integer;
  World: TCastleAbstractRootTransform;
  Sel: TComponent;
  NewResult: TCastleViewport;
  Nav: TCastleNavigation;
begin
  Result := nil;

  GetSelected(Selected, SelectedCount);
  try
    for I := 0 to SelectedCount - 1 do
    begin
      Sel := Selected[I];
      if Sel is TCastleViewport then
      begin
        NewResult := Sel as TCastleViewport;
        if (Result <> nil) and (Result <> NewResult) then
          Exit(nil); // multiple viewports selected
        Result := NewResult;
      end else
      if Sel is TCastleNavigation then
      begin
        Nav := Sel as TCastleNavigation;
        if Nav.InternalViewport is TCastleViewport then
        begin
          NewResult := Nav.InternalViewport as TCastleViewport;
          if (Result <> nil) and (Result <> NewResult) then
            Exit(nil); // multiple viewports selected
          Result := NewResult;
        end;
      end else
      if Sel is TCastleTransform then
      begin
        World := (Sel as TCastleTransform).World;
        if World <> nil then
        begin
          NewResult := World.Owner as TCastleViewport;
          if (Result <> nil) and (Result <> NewResult) then
            Exit(nil); // multiple viewports selected
          Result := NewResult;
        end;
      end else
      begin
        // UI that is not viewport selected
        Exit(nil);
      end;
    end;
  finally FreeAndNil(Selected) end;
end;

procedure TDesignFrame.UIScaling(const UIScaling: TUIScaling;
  const UIReferenceWidth, UIReferenceHeight: Single);
begin
  CastleControl.Container.UIScaling := UIScaling;
  CastleControl.Container.UIReferenceWidth := UIReferenceWidth;
  CastleControl.Container.UIReferenceHeight := UIReferenceHeight;
end;

procedure TDesignFrame.CurrentComponentApiUrl(var Url: String);

  function InspectorTypeFromActiveControl(const C: TWinControl;
    out InspectorType: TInspectorType): Boolean;
  begin
    for InspectorType in TInspectorType do
      if Inspector[InspectorType] = C then
        Exit(true);

    if C.Parent <> nil then
      Exit(InspectorTypeFromActiveControl(C.Parent, InspectorType));

    Result := false;
  end;

  { If a property of the SelectedComponent is now focused
    in one of our object inspectors, return property name. }
  function SelectedProperty(out PropertyInstance: TObject;
    out PropertyName, PropertyNameForLink: String): Boolean;
  var
    ParentForm: TCustomForm;
    InspectorType: TInspectorType;
    ActiveRow: TOIPropertyGridRow;
  begin
    ParentForm := GetParentForm(Self);
    if (ParentForm.ActiveControl <> nil) and
       InspectorTypeFromActiveControl(ParentForm.ActiveControl, InspectorType) and
       (Inspector[InspectorType].GetActiveRow <> nil) then
    begin
      ActiveRow := Inspector[InspectorType].GetActiveRow;
      PropertyInstance := ActiveRow.Editor.GetComponent(0);

      { Note that "GetActiveRow.Name" may not be the actual property name.
        The actual property name is in "GetActiveRow.Editor.GetPropInfo^.Name",
        and "GetActiveRow.Name" may be overrided by the property editor for presentation.
        E.g. our TVector3Persistent, TCastleColorPersistent modify the name
        to remove "Persistent" suffix.
        That said, we actually want to link to the version without "Persistent"
        suffix (but we need to use the version with "Persistent" for GetPropInfo
        as only "XxxPersistent" is published).

        So we need to pass on this complication to ApiReference.
      }
      PropertyName := ActiveRow.Editor.GetPropInfo^.Name;
      PropertyNameForLink := ActiveRow.Name;
      Result := true;
    end else
      Result := false;
  end;

var
  C: TComponent;
  PropertyInstance: TObject;
  PropertyName, PropertyNameForLink: String;
begin
  C := SelectedComponent;
  if C <> nil then
  begin
    { We do not use C for PropertyInstance, because in case of property
      in SubComponent, PropertyInstance needs to be <> nil.
      For example if you click F1 when being over "Blending" inside
      TCastleScene.RenderOptions. }
    if SelectedProperty(PropertyInstance, PropertyName, PropertyNameForLink) then
      Url := ApiReference(PropertyInstance, PropertyName, PropertyNameForLink)
    else
      Url := ApiReference(C, '', '');
  end;
end;

function TDesignFrame.ComponentCaption(const C: TComponent): String;

  function ClassCaption(const C: TClass): String;
  begin
    Result := C.ClassName;

    // hide some internal classes by instead displaying ancestor name
    // No point in doing this now
    // if C = Txxx then
    //   Result := ClassCaption(C.ClassParent);
  end;

begin
  Result := C.Name + ' (' + ClassCaption(C.ClassType) + ')';
end;

procedure TDesignFrame.CastleControlResize(Sender: TObject);
var
  CalculatedUIScale: Single;
  H, CalculatedUIScaleStr: String;
begin
  // trick to get private TUIContainer.FCalculatedUIScale
  CalculatedUIScale :=  (1 / CastleControl.Container.UnscaledWidth) *
    CastleControl.Container.Width;
  CalculatedUIScaleStr := IntToStr(Round(CalculatedUIScale * 100)) + '%';

  LabelUIScaling.Caption := CalculatedUIScaleStr;
  case CastleControl.Container.UIScaling of
    usNone                : H := 'No user interface scaling';
    usEncloseReferenceSize: H := Format('User interface scaling in effect: window must enclose a reference size of %f x %f.' + NL,
      [CastleControl.Container.UIReferenceWidth,
       CastleControl.Container.UIReferenceHeight]);
    usFitReferenceSize    : H := Format('User interface scaling in effect: window must fit inside a reference size of %f x %f.' + NL,
      [CastleControl.Container.UIReferenceWidth,
       CastleControl.Container.UIReferenceHeight]);
    usExplicitScale       : H := Format('User interface scaling in effect: explicit scale %f.' + NL,
      [CastleControl.Container.UIExplicitScale]);
    usDpiScale            : H := Format('User interface scaling in effect: scale to follow DPI (pixels per inch). DPI detected now is %f.' + NL,
      [CastleControl.Container.Dpi]);
    else raise EInternalError.Create('CastleControl.Container.UIScaling?');
  end;
  if CastleControl.Container.UIScaling <> usNone then
  begin
    H := Format(H +
      'Actual window size is %d x %d.' + NL +
      'Calculated scale is %s, which simulates surface of size %f x %f.',
      [CastleControl.Container.Width,
       CastleControl.Container.Height,
       CalculatedUIScaleStr,
       CastleControl.Container.UnscaledWidth,
       CastleControl.Container.UnscaledHeight]);
  end;
  LabelUIScaling.Hint := H;
end;

procedure TDesignFrame.CastleControlOpen(Sender: TObject);

  procedure ReadSettings;
  var
    SettingsUrl: String;
  begin
    SettingsUrl := 'castle-data:/CastleSettings.xml';
    if URIFileExists(SettingsUrl) then
    try
      CastleControl.Container.LoadSettings(SettingsUrl);
    except
      on E: Exception do
      begin
        { Showing a message box (using ErrorBox, which calls LCL MessageDlg)
          from CastleControl.OnOpen (after OpenGL context is initialized)
          is not reliable.

          - WinAPI widgetset: works OK.
          - GTK widgetset: shows an empty message box (seems like the rendered text
            is invisible), and shows GTK error in the console
            (castle-editor:6999): Gtk-CRITICAL **: 21:23:50.518: IA__gtk_widget_realize: assertion 'GTK_WIDGET_ANCHORED (widget) || GTK_IS_INVISIBLE (widget)' failed

          To workaround this, we set PendingErrorBox field here,
          instead of showing ErrorBox immediately.
        }
        PendingErrorBox := SAppendPart(PendingErrorBox, NL,
          'An error occurred when reading the CastleSettings.xml file in your project:' +
          NL + NL + ExceptMessage(E));
        { and continue, this way you can still open a project with broken
          CastleSettings.xml }
      end;
    end;
  end;

begin
  ReadSettings;
end;

procedure TDesignFrame.CastleControlUpdate(Sender: TObject);
begin
  { process PendingErrorBox }
  if PendingErrorBox <> '' then
  begin
    ErrorBox(PendingErrorBox);
    PendingErrorBox := '';
  end;

  if InternalCastleDesignInvalidate then
  begin
    UpdateDesign;
    //WritelnWarning('CGE needed to explicitly tell editor to refresh hierarchy');
    ModifiedOutsideObjectInspector('', ucLow);
  end;
end;

procedure TDesignFrame.CastleControlDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ShellList: TCastleShellListView;
  SelectedFileName: String;
  SelectedURL: String;
begin
  Accept := false;
  if Source is TCastleShellListView then
  begin
    ShellList := TCastleShellListView(Source);

    { ShellList.Selected may be nil, testcase:
      - open any project (empty from template is OK)
      - create new design using menu item
        (looks like this step is necessary into tricking LCL that we're
        in the middle of drag-and-drop on GTK?)
      - double-click on some design file in data/ by double-clicking
      - mouse over the design -> without this check, would have access violation
        due to TDesignFrame.CastleControlDragOver being called with
        ShellList.Selected = nil. }

    if ShellList.Selected <> nil then
    begin
      SelectedFileName := ShellList.GetPathFromItem(ShellList.Selected);
      SelectedURL := FilenameToURISafe(SelectedFileName);

      Accept := TFileFilterList.Matches(LoadScene_FileFilters, SelectedURL);
    end;
  end;
end;

procedure TDesignFrame.CastleControlDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  ShellList: TCastleShellListView;
  SelectedFileName: String;
  SelectedURL: String;
  Scene: TCastleScene;
  UI: TCastleUserInterface;
  Viewport: TCastleViewport;
  ScenePos: TVector3;
  RayOrigin, RayDirection: TVector3;
  RayHit: TRayCollision;
  Distance: Single;
  OldPickable: Boolean;
  PlaneZ: Single;
begin
  if Source is TCastleShellListView then
  begin
    ShellList := TCastleShellListView(Source);
    if ShellList.Selected <> nil then
    begin
      SelectedFileName := ShellList.GetPathFromItem(ShellList.Selected);
      SelectedURL := MaybeUseDataProtocol(FilenameToURISafe(SelectedFileName));

      if not TFileFilterList.Matches(LoadScene_FileFilters, SelectedURL) then
        Exit;

      UI := FDesignerLayer.HoverUserInterface(Vector2(X, Y));
      if not (UI is TCastleViewport) then
        Exit;

      Viewport := TCastleViewport(UI);

      Scene := AddComponent(Viewport.Items, TCastleScene, nil) as TCastleScene;
      Scene.URL := SelectedURL;

      { Make gizmos not pickable when looking for new scene position,
        because ray can hit on gizmo. }
      OldPickable := VisualizeTransformSelected.Pickable;
      try
        VisualizeTransformSelected.Pickable := false;
        Viewport.PositionToRay(Vector2(X, CastleControl.Height - Y), true, RayOrigin, RayDirection);
        RayHit := Viewport.Items.WorldRay(RayOrigin, RayDirection);
      finally
        VisualizeTransformSelected.Pickable := OldPickable;
      end;
      if (RayHit = nil) and (Viewport.Camera.ProjectionType = ptOrthographic) then
      begin
        PlaneZ := (Viewport.Camera.EffectiveProjectionNear + Viewport.Camera.EffectiveProjectionFar) / 2;
        if not TrySimplePlaneRayIntersection(ScenePos, 2, PlaneZ, RayOrigin, RayDirection) then
          Exit; // camera direction parallel to 3D plane with Z = constant
      end else
      begin
        if RayHit <> nil then
        begin
          Distance := RayHit.Distance;
          FreeAndNil(RayHit);
        end else
        begin
          { If we don't hit any other scene set Distance to default value. }
          Distance := 10;
        end;
        ScenePos := RayOrigin + (RayDirection * Distance);

        { In case of 2D game move scene a little closser to camera }
        if Viewport.Camera.ProjectionType = ptOrthographic then
          ScenePos := ScenePos - Viewport.Camera.Direction;
      end;

      Scene.Translation := ScenePos;
    end;
  end;
end;

procedure TDesignFrame.InspectorFilter(Sender: TObject;
  AEditor: TPropertyEditor; var AShow: Boolean; const Section: TPropertySection);
var
  PropertyName: String;
  Instance: TPersistent;
begin
  AShow := false;

  if AEditor.GetPropInfo = nil then
    Exit;

  PropertyName := AEditor.GetPropInfo^.Name;

  if (AEditor.GetInstProp <> nil) and
     (AEditor.GetInstProp^.Instance <> nil) then
  begin
    Instance := AEditor.GetInstProp^.Instance;
    { Show=true when Instance is some class used for subcomponents,
      like TCastleVector3Persistent, TBorder, TCastleImagePersistent... }
    if (not (Instance is TCastleComponent)) or
       (Section in TCastleComponent(Instance).PropertySections(PropertyName)) then
    begin
      AShow := true;
      Exit;
    end;
  end;
end;

procedure TDesignFrame.GizmoHasModifiedParent(Sender: TObject);
begin
  { Same comment as in Apply Drag:
    UndoOnRelease = true here means that we don't record the actual undo
    but defer it to GizmoStopDrag event }
  ModifiedOutsideObjectInspector('', ucLow, true); // UndoComment doesn't matter here
end;

procedure TDesignFrame.GizmoStopDrag(Sender: TObject);
begin
  if UndoSystem.ScheduleRecordUndoOnRelease then
    RecordUndo('Transform ' + (Sender as TVisualizeTransform).Parent.Name +
      ' with Gizmo', ucHigh);
end;

procedure TDesignFrame.InspectorBasicFilter(Sender: TObject;
  AEditor: TPropertyEditor; var aShow: Boolean);
begin
  InspectorFilter(Sender, AEditor, AShow, psBasic);
end;

procedure TDesignFrame.InspectorLayoutFilter(Sender: TObject;
  AEditor: TPropertyEditor; var aShow: Boolean);
begin
  InspectorFilter(Sender, AEditor, AShow, psLayout);
end;

function TDesignFrame.UndoMessageModified(const Sel: TPersistent;
  const ModifiedProperty, ModifiedValue: String; const SelectedCount: Integer): String;
const
  { Unreadable chars are defined like in SReplaceChars.
    Note they include newlines, we don't want to include newlines in undo description,
    as it would make menu item look weird (actually multiline on GTK2). }
  UnreadableChars = [Low(AnsiChar) .. Pred(' '), #128 .. High(AnsiChar)];
var
  ToValue: String;
begin
  if (Length(ModifiedValue) < 24) and (CharsPos(UnreadableChars, ModifiedValue) = 0) then
    ToValue := ' to ' + ModifiedValue
  else
    ToValue := '';

  { Right now, when SelectedCount = 1 then we know that Sel <> nil
    (but it is better to not depend on it).
    But it may not be TComponent, in case when changing property like X
    of TCastleVector3Persistent. }
  if (SelectedCount = 1) and
     (Sel is TComponent) then
    Result := 'Change ' + TComponent(Sel).Name + '.' + ModifiedProperty + ToValue
  else
  if SelectedCount > 1 then
    Result := 'Change ' + ModifiedProperty + ToValue + ' in multiple components'
  else
    Result := 'Change ' + ModifiedProperty + ToValue;
end;

procedure TDesignFrame.PropertyGridModified(Sender: TObject);
var
  Sel: TComponent;
  UI: TCastleUserInterface;
begin
  { Workaround possible ControlsTree.Selected = nil when the user deselects
    the currently edited component by clicking somewhere else.
    See https://trello.com/c/V6v2rBwv/75-bug-access-violation-in-castle-editor . }
  if ControlsTree.Selected = nil then
  begin
    UpdateDesign; // Something has changed, but we don't know what exactly. Maybe it's some component's name? Let's rebuild everything to be safe
    //if not UndoSystem.ScheduleRecordUndoOnRelease then // We don't care here, this situation is an error, so we're saving what we can
    RecordUndo('', ucLow); // We're recording a generic Undo message
    Exit;
  end;
  // This knows we have selected *at least one* component.
  // When you modify component Name in PropertyGrid, update it in the ControlsTree.
  Assert(ControlsTree.Selected <> nil);
  Assert(ControlsTree.Selected.Data <> nil);
  Assert(TObject(ControlsTree.Selected.Data) is TComponent);
  Sel := TComponent(ControlsTree.Selected.Data);
  ControlsTree.Selected.Text := ComponentCaption(Sel);

  // This checks we have selected *exactly one* component.
  Sel := SelectedComponent;
  if Sel <> nil then
  begin
    // update also LabelControlSelected
    LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Sel);

    // update also LabelSizeInfo
    if Sel is TCastleUserInterface then
    begin
      UI := Sel as TCastleUserInterface;
      UpdateLabelSizeInfo(UI);
      UpdateAnchors(UI, true);
    end;
  end;

  { When UndoSystem.ScheduleRecordUndoOnRelease we ignore changes,
    otherwise we would record an undo for every OnMotion of dragging. }
  if not UndoSystem.ScheduleRecordUndoOnRelease then
  begin
    if Sender is TOICustomPropertyGrid then
    begin
      RecordUndo(
        UndoMessageModified(Sel, TOICustomPropertyGrid(Sender).GetActiveRow.Name,
        TOICustomPropertyGrid(Sender).CurrentEditValue, ControlsTree.SelectionCount),
        ucHigh, TOICustomPropertyGrid(Sender).ItemIndex);
    end else
      { Sender is nil when PropertyGridModified is called
        by ModifiedOutsideObjectInspector. }
      if Sel <> nil then
        RecordUndo('Change ' + Sel.Name, ucLow)
      else
      if ControlsTree.SelectionCount > 1 then
        RecordUndo('Change multiple components', ucLow)
      else
        RecordUndo('', ucLow)
  end;

  MarkModified;
end;

procedure TDesignFrame.PropertyEditorModified(Sender: TObject);
var
  Sel: TPersistent;
begin
  if Sender is TPropertyEditor then
  begin
    if TPropertyEditor(Sender).PropCount = 1 then
      Sel := TPropertyEditor(Sender).GetComponent(0)
    else
      Sel := nil;
    RecordUndo(
      UndoMessageModified(Sel, TPropertyEditor(Sender).GetName,
        TPropertyEditor(Sender).GetValue, TPropertyEditor(Sender).PropCount),
      ucHigh);
  end else
    raise EInternalError.Create('PropertyEditorModified can only be called with TPropertyEditor as a Sender.');
end;

procedure TDesignFrame.RecordUndo(const UndoComment: String;
  const UndoCommentPriority: TUndoCommentPriority; const ItemIndex: Integer = -1);
var
  StartTimer: TTimerResult;
  SelectedName: String;
  SelectedC: TComponent;
begin
  StartTimer := Timer;

  SelectedC := GetSelectedComponent;
  { In case of modifying subcomponent, like TCastleViewport.Items,
    the currently selected component is not owned by whole design owner,
    so it could not be later found by name in PerformUndoRedo. }
  if (SelectedC <> nil) and (SelectedC.Owner = DesignOwner) then
    SelectedName := SelectedC.Name
  else
    SelectedName := '';

  UndoSystem.RecordUndo(ComponentToString(FDesignRoot), SelectedName, ItemIndex, ControlProperties.TabIndex, UndoComment, UndoCommentPriority);

  WriteLnLog('Undo "%s" recorded in %fs for "%s".', [UndoComment, StartTimer.ElapsedTime, SelectedName]);
end;

procedure TDesignFrame.MarkModified;
begin
  // mark modified
  FDesignModified := true;
  OnUpdateFormCaption(Self);
end;

class function TDesignFrame.Selectable(const Child: TComponent): Boolean;
begin
  { Do not show in hierarchy the TCastleDesign loaded hierarchy,
    as it will not be saved.
    Same for TCastleCheckbox children.
    Consequently, do not allow to select stuff inside. }
  Result := not (csTransient in Child.ComponentStyle);
end;

function TDesignFrame.Deletable(const Child: TComponent): Boolean;
begin
  Result := Selectable(Child) and
    (not (csSubComponent in Child.ComponentStyle)) and
    (Child <> DesignRoot);
end;

procedure TDesignFrame.UpdateDesign;

  function AddTransform(const Parent: TTreeNode; const T: TCastleTransform): TTreeNode;
  var
    S: String;
    I: Integer;
  begin
    S := ComponentCaption(T);
    Result := ControlsTree.Items.AddChildObject(Parent, S, T);
    TreeNodeMap.AddOrSetValue(T, Result);
    for I := 0 to T.Count - 1 do
      if Selectable(T[I]) then
        AddTransform(Result, T[I]);
  end;

  function AddControl(const Parent: TTreeNode; const C: TCastleUserInterface): TTreeNode;
  var
    S: String;
    I: Integer;
    Viewport: TCastleViewport;
  begin
    S := ComponentCaption(C);
    Result := ControlsTree.Items.AddChildObject(Parent, S, C);
    TreeNodeMap.AddOrSetValue(C, Result);
    for I := 0 to C.ControlsCount - 1 do
    begin
      if Selectable(C.Controls[I]) then
        AddControl(Result, C.Controls[I]);
    end;

    if C is TCastleViewport then
    begin
      Viewport := TCastleViewport(C);
      if Selectable(Viewport.Items) then
        AddTransform(Result, Viewport.Items);
    end;
  end;

var
  Node: TTreeNode;
begin
  ControlsTree.Items.Clear;
  TreeNodeMap.Clear;

  if DesignRoot is TCastleUserInterface then
    Node := AddControl(nil, DesignRoot as TCastleUserInterface)
  else
  if DesignRoot is TCastleTransform then
    Node := AddTransform(nil, DesignRoot as TCastleTransform)
  else
    raise EInternalError.Create('Cannot UpdateDesign with other classes than TCastleUserInterface or TCastleTransform');

  // show expanded by default
  Node.Expand(true);

  UpdateSelectedControl;

  InternalCastleDesignInvalidate := false;
end;

procedure TDesignFrame.GetSelected(out Selected: TComponentList;
  out SelectedCount: Integer);

  function SelectedFromNode(const Node: TTreeNode): TComponent;
  var
    SelectedObject: TObject;
  begin
    SelectedObject := nil;
    Result := nil;

    if Node <> nil then
    begin
      SelectedObject := TObject(Node.Data);
      if SelectedObject is TComponent then
        Result := TComponent(SelectedObject);
    end;
  end;

var
  I: Integer;
  C: TComponent;
begin
  Selected := nil;

  for I := 0 to ControlsTree.SelectionCount - 1 do
  begin
    C := SelectedFromNode(ControlsTree.Selections[I]);
    if C <> nil then
    begin
      if Selected = nil then
        Selected := TComponentList.Create(false);
      Selected.Add(C);
    end;
  end;

  if Selected <> nil then
    SelectedCount := Selected.Count
  else
    SelectedCount := 0;
end;

function TDesignFrame.GetSelectedUserInterface: TCastleUserInterface;
var
  C: TComponent;
begin
  C := GetSelectedComponent;
  if C is TCastleUserInterface then
    Result := TCastleUserInterface(C)
  else
    Result := nil;
end;

procedure TDesignFrame.SetSelectedUserInterface(const Value: TCastleUserInterface);
begin
  SelectedComponent := Value;
end;

function TDesignFrame.GetSelectedTransform: TCastleTransform;
var
  C: TComponent;
begin
  C := GetSelectedComponent;
  if C is TCastleTransform then
    Result := TCastleTransform(C)
  else
    Result := nil;
end;

procedure TDesignFrame.SetSelectedTransform(const Value: TCastleTransform);
begin
  SelectedComponent := Value;
end;

function TDesignFrame.GetSelectedComponent: TComponent;
begin
  if ControlsTree.SelectionCount = 1 then
    Result := TComponent(ControlsTree.Selections[0].Data)
  else
    Result := nil;
end;

procedure TDesignFrame.SetSelectedComponent(const Value: TComponent);
var
  Node: TTreeNode;
begin
  if Value = nil then
    ControlsTree.Select([])
  else
  if TreeNodeMap.TryGetValue(Value, Node) then
    ControlsTree.Select([Node]);
end;

procedure TDesignFrame.UpdateSelectedControl;
var
  Selected: TComponentList;
  SelectionForOI: TPersistentSelectionList;
  I, SelectedCount: Integer;
  UI: TCastleUserInterface;
  InspectorType: TInspectorType;
  V: TCastleViewport;
  T: TCastleTransform;
begin
  OnSelectionChanged(Self); // Calling it in ControlsTreeSelectionChanged doesn't seem to be enough as RenamePossible is true there even in case SelectedCount = 0 (does it use some obsolete value?)

  GetSelected(Selected, SelectedCount);
  try
    case SelectedCount of
      0: LabelControlSelected.Caption := 'Nothing Selected';
      1: LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Selected[0]);
      else LabelControlSelected.Caption := 'Selected:' + NL + IntToStr(SelectedCount) + ' components';
    end;

    SetEnabledVisible(ControlProperties, SelectedCount <> 0);

    SelectionForOI := TPersistentSelectionList.Create;
    try
      for I := 0 to SelectedCount - 1 do
        SelectionForOI.Add(Selected[I]);
      for InspectorType in TInspectorType do
        Inspector[InspectorType].Selection := SelectionForOI;
    finally FreeAndNil(SelectionForOI) end;
  finally FreeAndNil(Selected) end;

  UI := SelectedUserInterface;
  SetEnabledVisible(PanelAnchors, UI <> nil);
  if UI <> nil then
  begin
    UpdateLabelSizeInfo(UI);
    UpdateAnchors(UI, true);
  end;

  V := SelectedViewport;
  SetEnabledVisible(LabelSelectedViewport, V <> nil);
  SetEnabledVisible(ButtonViewportMenu, V <> nil);
  if V <> nil then
    LabelSelectedViewport.Caption := V.Name + ':';

  T := SelectedTransform;
  SetEnabledVisible(PanelLayoutTransform, T <> nil);
  VisualizeTransformSelected.Parent := T; // works also in case SelectedTransform is nil
end;

procedure TDesignFrame.ControlsTreeSelectionChanged(Sender: TObject);
begin
  UpdateSelectedControl;
end;

procedure TDesignFrame.ControlsTreeDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);

  function NodeSide(const Node: TTreeNode; const X, Y: Integer): TTreeNodeSide;
  var
    R: TRect;
  begin
    R := Node.DisplayRect(false);
    if X > R.SplitRect(srRight, 0.33).Left then
      Result := tnsRight
    else
    if Y > R.CenterPoint.Y then
      Result := tnsBottom
    else
      Result := tnsTop;
  end;

var
  Src, Dst: TTreeNode;
begin
  // Thanks to answer on https://stackoverflow.com/questions/18856374/delphi-treeview-drag-and-drop-between-nodes
  Src := ControlsTree.Selected;
  Dst := ControlsTree.GetNodeAt(X, Y);
  ControlsTreeNodeUnderMouse := Dst;

  Accept := ControlsTreeAllowDrag(Src, Dst);
  if not Accept then
    ControlsTreeNodeUnderMouse := nil;

  { We don't use TCustomTreeView.GetHitTestInfoAt,
    it never contains flags htAbove, htBelow, htOnRight that interest us.
    (Simply not implemented, marked by TODO in Lazarus sources.) }
  //ControlsTreeNodeUnderMouseHit := ControlsTree.GetHitTestInfoAt(X, Y);
  if ControlsTreeNodeUnderMouse <> nil then
    ControlsTreeNodeUnderMouseSide := NodeSide(ControlsTreeNodeUnderMouse, X, Y);
  ControlsTree.Invalidate; // force custom-drawn look redraw
end;

procedure TDesignFrame.ControlsTreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  { This event is fired when calling TCustomListView.CanEdit
    which itself is called in TCustomListView.ShowEditor
    therefore this event preceeds initializing and showing of the editor.

    Here we have to "restore" the pure name of the component (without class name)
    before starting edit. }
  Node.Text := TComponent(Node.Data).Name;
end;

procedure TDesignFrame.ControlsTreeEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
var
  UndoComment: String;
  Sel: TComponent;
begin
  try
    Sel := TComponent(Node.Data);
    UndoComment := 'Rename ' + Sel.Name + ' into ' + Node.Text;
    { Without this check, one could change Sel.Name to empty ('').
      Although TComponent.SetName checks that it's a valid Pascal identifier already,
      but it also explicitly allows to set Name = ''.
      Object inspector has special code to secure from empty Name
      (in TComponentNamePropertyEditor.SetValue), so we need a similar check here. }
    if not IsValidIdent(Node.Text) then
      raise Exception.Create(Format(oisComponentNameIsNotAValidIdentifier, [Node.Text]));
    Sel.Name := Node.Text;
    ModifiedOutsideObjectInspector(UndoComment, ucHigh); // It'd be good if we set "ItemIndex" to index of "name" field, but there doesn't seem to be an easy way to
  finally
    { This method must set Node.Text, to cleanup after ControlsTreeEditing + user editing.
      - If the name was correct, then "Sel.Name := " goes without exception,
        and we want to show new name + class name.
      - If the name was not correct, then "Sel.Name := " raises exception,
        and we want to show old name + class name. }
    Node.Text := ComponentCaption(Sel);
  end;
end;

function TDesignFrame.ControlsTreeAllowDrag(const Src, Dst: TTreeNode): Boolean;
var
  SrcComponent: TComponent;
begin
  Result := (Src <> nil) and (Dst <> nil) and (Src <> Dst);
  if Result then
  begin
    { Do not allow to drag subcomponents (like TCastleScrollView.ScrollArea)
      or root component. }
    SrcComponent := TObject(Src.Data) as TComponent;
    if (SrcComponent = DesignRoot) or
       (csSubComponent in SrcComponent.ComponentStyle) then
      Result := false;
  end;
end;

procedure TDesignFrame.FrameAnchorsChange(Sender: TObject);
var
  UI: TCastleUserInterface;
  OldRect: TFloatRectangle;
begin
  UI := SelectedUserInterface;
  if UI <> nil then
  begin
    OldRect := UI.RenderRectWithBorder;

    UI.HorizontalAnchorSelf := SelfAnchorsFrame.HorizontalAnchor;
    UI.VerticalAnchorSelf := SelfAnchorsFrame.VerticalAnchor;
    if CheckParentSelfAnchorsEqual.Checked then
    begin
      UI.HorizontalAnchorParent := UI.HorizontalAnchorSelf;
      UI.VerticalAnchorParent := UI.VerticalAnchorSelf;
      // keep invisible ParentAnchorsFrame synchronized
      ParentAnchorsFrame.HorizontalAnchor := UI.HorizontalAnchorParent;
      ParentAnchorsFrame.VerticalAnchor := UI.VerticalAnchorParent;
    end else
    begin
      UI.HorizontalAnchorParent := ParentAnchorsFrame.HorizontalAnchor;
      UI.VerticalAnchorParent := ParentAnchorsFrame.VerticalAnchor;
    end;

    AdjustUserInterfaceAnchorsToKeepRect(UI, OldRect);
  end;
end;

procedure TDesignFrame.AdjustUserInterfaceAnchorsToKeepRect(
  const UI: TCastleUserInterface; const RenderRectBeforeChange: TFloatRectangle);
var
  NewRect: TFloatRectangle;
begin
  // adjust anchors, to preserve previous position
  NewRect := UI.RenderRectWithBorder;
  if NewRect.IsEmpty or RenderRectBeforeChange.IsEmpty then
  begin
    // don't know what to do, adjust delta to 0, to avoid leaving some crazy value
    UI.HorizontalAnchorDelta := 0;
    UI.VerticalAnchorDelta := 0;
  end else
  begin
    UI.HorizontalAnchorDelta := UI.HorizontalAnchorDelta +
      (RenderRectBeforeChange.Left - NewRect.Left) / UI.UIScale;
    UI.VerticalAnchorDelta := UI.VerticalAnchorDelta +
      (RenderRectBeforeChange.Bottom - NewRect.Bottom) / UI.UIScale;
  end;

  ModifiedOutsideObjectInspector('', ucLow);
end;

procedure TDesignFrame.ControlsTreeEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  ControlsTreeNodeUnderMouse := nil;
  ControlsTree.Invalidate; // force custom-drawn look redraw
end;

function TDesignFrame.RenamePossible: Boolean;
begin
  Result := ControlsTree.SelectionCount = 1;
end;

procedure TDesignFrame.RenameSelectedItem;
begin
  if RenamePossible then
    ControlsTree.Selected.EditText;
end;

procedure TDesignFrame.ControlsTreeDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Src, Dst: TTreeNode;
  SrcComponent, DstComponent: TComponent;

  procedure Refresh;
  var
    DestinationName: String;
  begin
    { TODO: In theory we could replicate the movement in UI controls tree
      by doing a movement in nodes tree using:

        tnsRight: Src.MoveTo(Dst, naAddChild);
        tnsBottom: Src.MoveTo(Dst, naInsertBehind);
        tnsTop: Src.MoveTo(Dst, naInsert);

      However:

      - It is error prone. It's safer to do the operation in UI controls tree,
        and then show the resulting tree using this method.
        This way, in case we do something unexpected,
        at least the "shown tree" will reflect the "actual tree".

      - Also, this way we workaround a problem (at least with GTK2 backend):
        Dragging the same item right after dropping it doesn't work.
        So it is easiest to deselect it.
        Which already happens when we do UpdateDesign.
    }

    UpdateDesign;
    case ControlsTreeNodeUnderMouseSide of
      tnsRight: DestinationName := DstComponent.Name;
      tnsBottom, tnsTop: DestinationName := TComponent(Dst.Parent.Data).Name;
    end;
    ModifiedOutsideObjectInspector('Drag''n''drop ' + SrcComponent.Name + ' into ' +
      DestinationName, ucHigh);
  end;

  { Does Parent contains PotentialChild, searching recursively.
    It checks is Parent equal PotentialChild,
    and searches Parent's children,
    and Parent's children's children etc. }
  function ContainsRecursive(const Parent, PotentialChild: TCastleUserInterface): Boolean;
  var
    I: Integer;
  begin
    if Parent = PotentialChild then
      Exit(true);
    for I := 0 to Parent.ControlsCount - 1 do
      if ContainsRecursive(Parent.Controls[I], PotentialChild) then
        Exit(true);
    Result := false;
  end;

  { As above, but overloaded for TCastleTransform. }
  function ContainsRecursive(const Parent, PotentialChild: TCastleTransform): Boolean;
  var
    I: Integer;
  begin
    if Parent = PotentialChild then
      Exit(true);
    for I := 0 to Parent.Count - 1 do
      if ContainsRecursive(Parent.Items[I], PotentialChild) then
        Exit(true);
    Result := false;
  end;

  procedure MoveUserInterface(const Src, Dst: TCastleUserInterface);
  var
    Index: Integer;
    OldRect: TFloatRectangle;
  begin
    OldRect := Src.RenderRectWithBorder;

    case ControlsTreeNodeUnderMouseSide of
      tnsRight:
        begin
          if not ContainsRecursive(Src, Dst) then
          begin
            if Src.Parent <> nil then
              Src.Parent.RemoveControl(Src);
            Dst.InsertFront(Src);
            AdjustUserInterfaceAnchorsToKeepRect(Src, OldRect);
            Refresh;
          end;
        end;
      tnsBottom, tnsTop:
        begin
          if (Dst.Parent <> nil) and
             not ContainsRecursive(Src, Dst.Parent) then
          begin
            if Src.Parent <> nil then
              Src.Parent.RemoveControl(Src);
            Index := Dst.Parent.IndexOfControl(Dst);
            Assert(Index <> -1);
            if ControlsTreeNodeUnderMouseSide = tnsBottom then
              Inc(Index);
            Dst.Parent.InsertControl(Index, Src);
            AdjustUserInterfaceAnchorsToKeepRect(Src, OldRect);
            Refresh;
          end;
        end;
      else raise EInternalError.Create('ControlsTreeDragDrop:ControlsTreeNodeUnderMouseSide?');
    end;
  end;

  procedure MoveTransform(const Src, Dst: TCastleTransform);
  var
    Index: Integer;
  begin
    case ControlsTreeNodeUnderMouseSide of
      tnsRight:
        begin
          if not ContainsRecursive(Src, Dst) then
          begin
            if Src.UniqueParent <> nil then
              Src.UniqueParent.Remove(Src);
            Dst.Add(Src);
            Refresh;
          end;
        end;
      tnsBottom, tnsTop:
        begin
          if (Dst.UniqueParent <> nil) and
             not ContainsRecursive(Src, Dst.UniqueParent) then
          begin
            if Src.UniqueParent <> nil then
              Src.UniqueParent.Remove(Src);
            Index := Dst.UniqueParent.List.IndexOf(Dst);
            Assert(Index <> -1);
            if ControlsTreeNodeUnderMouseSide = tnsBottom then
              Inc(Index);
            Dst.UniqueParent.Insert(Index, Src);
            Refresh;
          end;
        end;
      else raise EInternalError.Create('ControlsTreeDragDrop:ControlsTreeNodeUnderMouseSide?');
    end;
  end;

begin
  Src := ControlsTree.Selected;
  //Dst := ControlsTree.GetNodeAt(X,Y);
  Dst := ControlsTreeNodeUnderMouse;
  { Paranoidally check ControlsTreeAllowDrag again.
    It happens that Src is nil, in my tests. }
  if ControlsTreeAllowDrag(Src, Dst) then
  begin
    SrcComponent := TComponent(Src.Data);
    DstComponent := TComponent(Dst.Data);
    if (SrcComponent is TCastleUserInterface) and
       (DstComponent is TCastleUserInterface) then
    begin
      MoveUserInterface(
        TCastleUserInterface(SrcComponent),
        TCastleUserInterface(DstComponent));
    end else
    if (SrcComponent is TCastleTransform) and
       (DstComponent is TCastleTransform) then
    begin
      MoveTransform(
        TCastleTransform(SrcComponent),
        TCastleTransform(DstComponent));
    end;
  end;
end;

procedure TDesignFrame.ControlsTreeAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
const
  ColorOutline = clBlack;
  ColorDecoration = clGray;
var
  NodeRect, R: TRect;
begin
  DefaultDraw := true;

  if Node = ControlsTreeNodeUnderMouse then
  begin
    case Stage of
      cdPostPaint:
        begin
          NodeRect := Node.DisplayRect(false);

          { We can't draw it in cdPrePaint, as after csPrePaint
            the node rectangle is cleared anyway.
            And we can't draw it in any cdXxxErase, which are not implemented
            in LCL (2.1.0). }
          ControlsTree.Canvas.Pen.Color := ColorOutline;
          ControlsTree.Canvas.Pen.Style := psDot;
          ControlsTree.Canvas.Brush.Style := bsClear;
          ControlsTree.Canvas.Rectangle(NodeRect);

          if ControlsTreeNodeUnderMouseSide = tnsRight then
          begin
            R := NodeRect.SplitRect(srRight, NodeRect.Height);
            R.Inflate(-5, -5);
            ControlsTree.Canvas.Brush.Color := ColorDecoration;
            ControlsTree.Canvas.Brush.Style := bsSolid;
            ControlsTree.Canvas.Pen.Color := ColorOutline;
            ControlsTree.Canvas.Pen.Style := psSolid;
            //ControlsTree.Canvas.FillRect(R);
            ControlsTree.Canvas.Polygon([
              Point(R.Left , R.Top),
              Point(R.Right, R.CenterPoint.Y),
              Point(R.Left , R.Bottom)
            ]);
          end else
          begin
            if ControlsTreeNodeUnderMouseSide = tnsTop then
              R := NodeRect.SplitRect(srTop, 0.1)
            else
              R := NodeRect.SplitRect(srBottom, 0.1);
            R.Left := R.Left + ((Node.Level + 1)* ControlsTree.Indent);

            ControlsTree.Canvas.Brush.Color := ColorDecoration;
            ControlsTree.Canvas.Brush.Style := bsSolid;
            ControlsTree.Canvas.FillRect(R);
          end;
        end;
    end;
  end;
end;

procedure TDesignFrame.CheckParentSelfAnchorsEqualChange(Sender: TObject);
var
  UI: TCastleUserInterface;
  OldRect: TFloatRectangle;
begin
  ParentAnchorsFrame.Visible := not CheckParentSelfAnchorsEqual.Checked;
  ParentAnchorsFrame.Enabled := not CheckParentSelfAnchorsEqual.Checked;
  if CheckParentSelfAnchorsEqual.Checked then
  begin
    UI := SelectedUserInterface;
    if UI <> nil then
    begin
      OldRect := UI.RenderRectWithBorder;

      UI.HorizontalAnchorParent := UI.HorizontalAnchorSelf;
      UI.VerticalAnchorParent := UI.VerticalAnchorSelf;

      AdjustUserInterfaceAnchorsToKeepRect(UI, OldRect);

      // update also (invisible now) ParentAnchorsFrame UI state
      UpdateAnchors(UI, false);
    end;
  end;
end;

procedure TDesignFrame.ButtonClearAnchorDeltasClick(Sender: TObject);
var
  UI: TCastleUserInterface;
begin
  UI := SelectedUserInterface;
  if UI <> nil then
  begin
    UI.HorizontalAnchorDelta := 0;
    UI.VerticalAnchorDelta := 0;
    ModifiedOutsideObjectInspector('Clear anchor deltas for ' + UI.Name, ucHigh);
  end;
end;

procedure TDesignFrame.ButtonResetTransformationClick(Sender: TObject);
var
  T: TCastleTransform;
begin
  T := SelectedTransform;
  if T <> nil then
  begin
    T.Identity;
    ModifiedOutsideObjectInspector('Reset transformations for ' + T.Name, ucHigh);
  end;
end;

procedure TDesignFrame.ButtonTransformRotateModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moTransformRotate);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonTransformScaleModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moTransformScale);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonTransformSelectModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moTransformSelect);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonTransformTranslateModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moTransformTranslate);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonViewportMenuClick(Sender: TObject);
begin
  MenuViewport.PopupComponent := ButtonViewportMenu;
  MenuViewport.PopUp;
end;

procedure TDesignFrame.ButtonInteractModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moInteract);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonModifyUiModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moModifyUi);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.MenuItemAddComponentClick(Sender: TObject);
var
  R: TRegisteredComponent;
begin
  R := TRegisteredComponent(Pointer((Sender as TComponent).Tag));
  AddComponent(R.ComponentClass, R.OnCreate);
end;

procedure TDesignFrame.MenuTreeViewItemRenameClick(Sender: TObject);
begin
  RenameSelectedItem;
end;

procedure TDesignFrame.MenuTreeViewItemDeleteClick(Sender: TObject);
begin
  DeleteComponent;
end;

procedure TDesignFrame.MenuTreeViewItemCopyClick(Sender: TObject);
begin
  CopyComponent;
end;

procedure TDesignFrame.MenuTreeViewItemPasteClick(Sender: TObject);
begin
  PasteComponent;
end;

procedure TDesignFrame.MenuTreeViewPopup(Sender: TObject);
var
  Sel: TComponent;
begin
  Sel := SelectedComponent;
  MenuTreeViewItemRename.Enabled := RenamePossible;
  MenuTreeViewItemDuplicate.Enabled := Sel <> nil;
  MenuTreeViewItemCopy.Enabled := Sel <> nil;
  MenuTreeViewItemDelete.Enabled := ControlsTree.SelectionCount > 0; // delete can handle multiple objects
  if (Sel is TCastleUserInterface) or ((Sel = nil) and (DesignRoot is TCastleUserInterface)) then
  begin
    MenuTreeViewItemAddUserInterface.SetEnabledVisible(true);
    MenuTreeViewItemAddTransform.SetEnabledVisible(false);
  end else
  if (Sel is TCastleTransform) or ((Sel = nil) and (DesignRoot is TCastleTransform)) then
  begin
    MenuTreeViewItemAddUserInterface.SetEnabledVisible(false);
    MenuTreeViewItemAddTransform.SetEnabledVisible(true);
  end else
  begin
    WritelnWarning('Unexpected situation, selected / design root have unexpected classes');
    MenuTreeViewItemAddUserInterface.SetEnabledVisible(false);
    MenuTreeViewItemAddTransform.SetEnabledVisible(false);
  end;
  MenuTreeView.PopupComponent := ControlsTree; // I'm not sure what it means, something like menu owner?
end;

procedure TDesignFrame.MenuViewportNavigation2DClick(Sender: TObject);
begin
  ChangeViewportNavigation(TCastle2DNavigation.Create(DesignOwner));
end;

procedure TDesignFrame.MenuTreeViewItemDuplicateClick(Sender: TObject);
begin
  DuplicateComponent;
end;

procedure TDesignFrame.MenuItemViewportCamera2DViewInitialClick(
  Sender: TObject);
var
  V: TCastleViewport;
begin
  V := SelectedViewport;
  V.Setup2D;
  ModifiedOutsideObjectInspector('Camera Setup for 2D View and Projection for ' + V.Name, ucHigh);
end;

procedure TDesignFrame.MenuItemViewportCameraCurrentFromInitialClick(
  Sender: TObject);
var
  V: TCastleViewport;
begin
  V := SelectedViewport;
  V.Camera.SetView(
    V.Camera.InitialPosition,
    V.Camera.InitialDirection,
    V.Camera.InitialUp);
  ModifiedOutsideObjectInspector('Camera Current := Initial for ' + V.Name, ucHigh);
end;

procedure TDesignFrame.MenuItemViewportCameraViewAllClick(Sender: TObject);
var
  V: TCastleViewport;
  Position, Direction, Up, GravityUp: TVector3;
  ProjectionWidth, ProjectionHeight, ProjectionFar: Single;
  Box: TBox3D;
begin
  V := SelectedViewport;
  Box := V.Items.BoundingBox;

  if V.Camera.ProjectionType = ptOrthographic then
  begin
    CameraOrthoViewpointForWholeScene(Box,
      V.EffectiveWidth,
      V.EffectiveHeight,
      V.Camera.Orthographic.Origin,
      Position, ProjectionWidth, ProjectionHeight, ProjectionFar);
    V.Camera.Orthographic.Width := ProjectionWidth;
    V.Camera.Orthographic.Height := ProjectionHeight;
    V.Camera.ProjectionFar := ProjectionFar;
    // set the rest of variables to constant values, matching 2D game view
    Direction := Vector3(0, 0, -1);
    Up := Vector3(0, 1, 0);
    GravityUp := Up;
  end else
  begin
    CameraViewpointForWholeScene(Box,
      2, 1, false, true, // dir = -Z, up = +Y
      Position, Direction, Up, GravityUp);
  end;

  V.Camera.AnimateTo(Position, Direction, Up, 0.5);
  V.Camera.GravityUp := GravityUp;
  if V.Navigation <> nil then
    // Makes Examine camera pivot, and scroll speed, adjust to sizes
    V.Navigation.ModelBox := Box;

  ModifiedOutsideObjectInspector('Camera Current := View All for ' + V.Name, ucHigh);
end;

procedure TDesignFrame.MenuItemViewportCameraSetInitialClick(Sender: TObject);
var
  V: TCastleViewport;
  APos, ADir, AUp: TVector3;
begin
  V := SelectedViewport;

  V.Camera.GetView(APos, ADir, AUp);
  V.Camera.SetInitialView(APos, ADir, AUp, false);
  V.AutoCamera := false;

  ModifiedOutsideObjectInspector('Camera Initial := Current for ' + V.Name, ucHigh);
end;

procedure TDesignFrame.MenuItemViewportSort2DClick(Sender: TObject);
var
  V: TCastleViewport;
begin
  V := SelectedViewport;

  V.Items.SortBackToFront2D;

  UpdateDesign; // make the tree reflect new order
  ModifiedOutsideObjectInspector('Sort Items for Correct 2D Blending for ' + V.Name, ucHigh);
end;

{
function TDesignFrame.SelectionSave: Classes.TList;
var
  I: Integer;
begin
  Result := Classes.TList.Create;
  Result.Count := ControlsTree.SelectionCount;
  for I := 0 to Result.Count - 1 do
    Result[I] := ControlsTree.Selections[I];
end;

procedure TDesignFrame.SelectionRestoreAndFree(var Selection: Classes.TList);
begin
  ControlsTree.Select(Selection);
  FreeAndNil(Selection);
end;
}

procedure TDesignFrame.ChangeViewportNavigation(const NewNavigation: TCastleNavigation);
var
  V: TCastleViewport;
begin
  V := SelectedViewport;

  // fixes crash, in case current selection was equal to old V.Navigation that will be freed
  SelectedUserInterface := nil;

  // free previous V.Navigation
  if (V.Navigation <> nil) and
     (V.Navigation.Owner = DesignOwner) then
    V.Navigation.Free
  else
    // using internal navigation instance, through SetNavigationType
    V.Navigation := nil;
  Assert(V.Navigation = nil);

  // set new V.Navigation
  V.Navigation := NewNavigation;
  if NewNavigation <> nil then
    NewNavigation.Name := ProposeName(TComponentClass(NewNavigation.ClassType), DesignOwner);

  // otherwise, setting Navigation to nil would not work, as it would be replaced by internal navigation
  V.AutoNavigation := false;

  UpdateDesign;

  if NewNavigation <> nil then
    SelectedUserInterface := NewNavigation
  else
    SelectedUserInterface := V;
  ModifiedOutsideObjectInspector('Change Viewport Navigation for ' + V.Name, ucHigh);
end;

procedure TDesignFrame.MenuViewportNavigationNoneClick(Sender: TObject);
begin
  ChangeViewportNavigation(nil);
end;

procedure TDesignFrame.MenuViewportNavigationExamineClick(Sender: TObject);
begin
  ChangeViewportNavigation(TCastleExamineNavigation.Create(DesignOwner));
end;

procedure TDesignFrame.MenuViewportNavigationFlyClick(Sender: TObject);
var
  W: TCastleWalkNavigation;
begin
  W := TCastleWalkNavigation.Create(DesignOwner);
  W.Gravity := false;
  ChangeViewportNavigation(W);
end;

procedure TDesignFrame.MenuViewportNavigationWalkClick(Sender: TObject);
var
  W: TCastleWalkNavigation;
begin
  W := TCastleWalkNavigation.Create(DesignOwner);
  W.Gravity := true;
  ChangeViewportNavigation(W);
end;

procedure TDesignFrame.MenuViewportNavigationThirdPersonClick(Sender: TObject);
var
  N: TCastleThirdPersonNavigation;
begin
  N := TCastleThirdPersonNavigation.Create(DesignOwner);
  ChangeViewportNavigation(N);
end;

procedure TDesignFrame.SetParent(AParent: TWinControl);
{$ifdef LCLwin32}
var
  H: Integer;
  InspectorType: TInspectorType;
{$endif}
begin
  inherited SetParent(AParent);
  {$ifdef LCLwin32}
  // fix height of rows of object inspector
  if AParent <> nil then
  begin
    H := Canvas.TextHeight('Wg') + 4;
    for InspectorType in TInspectorType do
      Inspector[InspectorType].DefaultItemHeight := H;
  end;
  {$endif}
end;

function TDesignFrame.ProposeName(const ComponentClass: TComponentClass;
  const ComponentsOwner: TComponent): String;

  { Cleanup S (right now, always taken from some ClassName)
    to be a nice component name, which also must make it a valid Pascal identifier. }
  function CleanComponentName(const S: String): String;
  begin
    Result := S;

    // remove common prefixes
    if IsPrefix('TCastleUserInterface', Result, true) then
      Result := PrefixRemove('TCastleUserInterface', Result, true)
    else
    if IsPrefix('TCastle', Result, true) then
      Result := PrefixRemove('TCastle', Result, true)
    else
    if IsPrefix('T', Result, true) then
      Result := PrefixRemove('T', Result, true);

    // move 2D and 3D to the back, as component name cannot start with a number
    if IsPrefix('2D', Result, true) then
      Result := PrefixRemove('2D', Result, true) + '2D';
    if IsPrefix('3D', Result, true) then
      Result := PrefixRemove('3D', Result, true) + '3D';

    // in case the replacements above made '', fix it (can happen in case of TCastleUserInterface)
    if Result = '' then
      Result := 'Group';

    if SCharIs(Result, 1, ['0'..'9']) then
      Result := 'Component' + Result;
  end;

var
  ResultBase: String;
  I: Integer;
begin
  ResultBase := CleanComponentName(ComponentClass.ClassName);

  { A simple test of the CleanComponentName routine.
    This is *not* a good place for such automated test, but for now it was simplest to put it here. }
  {
  Assert(CleanComponentName('TSomething') = 'Something');
  Assert(CleanComponentName('TCastleUserInterface') = 'Group');
  Assert(CleanComponentName('TCastleUserInterfaceButton') = 'Button');
  Assert(CleanComponentName('TCastleSomething') = 'Something');
  Assert(CleanComponentName('TCastle2DStuff') = 'Stuff2D');
  Assert(CleanComponentName('TCastle3DStuff') = 'Stuff3D');
  Assert(CleanComponentName('TCastle4DProcessing') = 'Component4DProcessing');
  }

  // make unique
  I := 1;
  Result := ResultBase + IntToStr(I);
  while ComponentsOwner.FindComponent(Result) <> nil do
  begin
    Inc(I);
    Result := ResultBase + IntToStr(I);
  end;
end;

procedure TDesignFrame.UpdateLabelSizeInfo(const UI: TCastleUserInterface);
var
  RR: TFloatRectangle;
  S: String;
begin
  RR := UI.RenderRectWithBorder;
  if RR.IsEmpty then
    S := 'Size: Empty'
  else
  begin
    S := Format(
      'Effective size: %f x %f' + NL +
      NL +
      'Render rectangle (scaled and with anchors):' + NL +
      '  Left x Bottom: %f x %f' + NL +
      '  Size: %f x %f',
      [ UI.EffectiveWidth,
        UI.EffectiveHeight,
        RR.Left,
        RR.Bottom,
        RR.Width,
        RR.Height
      ]);
  end;

  if (UI.Parent <> nil) and
     (not UI.Parent.RenderRect.Contains(UI.RenderRectWithBorder)) then
    S := S + NL + NL + 'WARNING: The rectangle occupied by this control is outside of the parent rectangle. The events (like mouse clicks) may not reach this control. You must always fit child control inside the parent.';

  LabelSizeInfo.Hint := S;
end;

procedure TDesignFrame.UpdateAnchors(const UI: TCastleUserInterface;
  const AllowToHideParentAnchorsFrame: Boolean);
begin
  SelfAnchorsFrame.HorizontalAnchor := UI.HorizontalAnchorSelf;
  SelfAnchorsFrame.VerticalAnchor := UI.VerticalAnchorSelf;
  ParentAnchorsFrame.HorizontalAnchor := UI.HorizontalAnchorParent;
  ParentAnchorsFrame.VerticalAnchor := UI.VerticalAnchorParent;

  if AllowToHideParentAnchorsFrame then
  begin
    CheckParentSelfAnchorsEqual.Checked :=
      (UI.HorizontalAnchorSelf = UI.HorizontalAnchorParent) and
      (UI.VerticalAnchorSelf = UI.VerticalAnchorParent);
    { The above will automatically call CheckParentSelfAnchorsEqualChanged,
      if Checked just changed. }
  end;
end;

procedure TDesignFrame.ChangeMode(const NewMode: TMode);
begin
  Mode := NewMode;

  ButtonInteractMode.Down := Mode = moInteract;
  ButtonModifyUiMode.Down := Mode = moModifyUi;

  { Hiding this is not nice for user, as then clicking on ButtonTransformSelectMode
    when current mode is moModifyUi will shift the position of the
    ButtonTransformSelectMode under your mouse. }
  //SetEnabledVisible(SpinEditSnap, Mode = moModifyUi);

  ButtonTransformSelectMode.Down := Mode = moTransformSelect;
  ButtonTransformTranslateMode.Down := Mode = moTransformTranslate;
  ButtonTransformRotateMode.Down := Mode = moTransformRotate;
  ButtonTransformScaleMode.Down := Mode = moTransformScale;

  case Mode of
    moTransformTranslate: VisualizeTransformSelected.Operation := voTranslate;
    moTransformRotate: VisualizeTransformSelected.Operation := voRotate;
    moTransformScale: VisualizeTransformSelected.Operation := voScale;
    else VisualizeTransformSelected.Operation := voSelect;
  end;
end;

procedure TDesignFrame.ModifiedOutsideObjectInspector(const UndoComment: String;
  const UndoCommentPriority: TUndoCommentPriority; const UndoOnRelease: Boolean = false);
var
  InspectorType: TInspectorType;
begin
  // TODO: this moves UI scrollbar up,
  // TODO: this is not optimized
  // (PropertyGridModified does some unnecessary things if we only changed size)

  for InspectorType in TInspectorType do
    Inspector[InspectorType].RefreshPropertyValues;
  // do not call PropertyGridModified if nothing selected, e.g. after delete operation
  if ControlsTree.Selected <> nil then
    PropertyGridModified(nil);

  MarkModified;

  if UndoOnRelease then
    UndoSystem.ScheduleRecordUndoOnRelease := true
  else
    RecordUndo(UndoComment, UndoCommentPriority); //it will overwrite Undo recorded in PropertyGridModified with a better comment
end;

procedure TDesignFrame.NewDesign(const ComponentClass: TComponentClass;
  const ComponentOnCreate: TNotifyEvent);
var
  NewRoot: TComponent;
  NewDesignOwner: TComponent;
begin
  NewDesignOwner := TComponent.Create(Self);

  NewRoot := ComponentClass.Create(NewDesignOwner);
  if Assigned(ComponentOnCreate) then
    ComponentOnCreate(NewRoot);
  NewRoot.Name := ProposeName(ComponentClass, NewDesignOwner);

  { In these special cases, set FullSize to true,
    since this is almost certainly what user wants when creating a new UI
    that has this component class as root. }
  if (ComponentClass = TCastleUserInterface) or
     (ComponentClass = TCastleRectangleControl) then
    (NewRoot as TCastleUserInterface).FullSize := true;

  UndoSystem.ClearUndoHistory;

  OpenDesign(NewRoot, NewDesignOwner, '');

  RecordUndo('Start new design', High(TUndoCommentPriority)); // This Undo comment is never seen
end;

initialization
  { Enable using our property edits e.g. for TCastleScene.URL }
  CastlePropEdits.Register;
  PropertyEditorsAdviceDataDirectory := true;
  CastleDesignMode := true;
end.
