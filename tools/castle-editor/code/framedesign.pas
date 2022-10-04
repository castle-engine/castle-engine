{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Frame where you can design
  a xxx.castle-user-interface, xxx.castle-transform, xxx.castle-component file. }
unit FrameDesign;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  Spin, Buttons, Menus, ActnList, Contnrs, Generics.Collections,
  // for icons
  DataModuleIcons, ImgList,
  // for TOIPropertyGrid usage
  ObjectInspector, PropEdits, PropEditUtils, GraphPropEdits,
  CollectionPropEditForm, ComponentEditors,
  // CGE units
  CastleControl, CastleUIControls, CastlePropEdits, CastleDialogs,
  CastleSceneCore, CastleKeysMouse, CastleVectors, CastleRectangles,
  CastleViewport, CastleClassUtils, CastleControls, CastleTiledMap,
  CastleCameras, CastleBoxes, CastleTransform, CastleDebugTransform,
  CastleColors, CastleScene,
  // editor units
  FrameAnchors, CastleShellCtrls,
  DesignVisualizeTransform, DesignUndoSystem, DesignCameraPreview;

type
  TProposeOpenDesignEvent = procedure (const DesignUrl: String) of object;

  { Frame to visually design component hierarchy. }
  TDesignFrame = class(TFrame)
    ActionPhysicsPauseSimulation: TAction;
    ActionPhysicsPlayStopSimulation: TAction;
    ActionListDesign: TActionList;
    ButtonResetTransformation: TButton;
    ButtonClearAnchorDeltas: TButton;
    LabelPhysics: TLabel;
    LabelViewport: TLabel;
    LabelHeaderUi: TLabel;
    LabelEventsInfo: TLabel;
    LabelPhysicsSimulation: TLabel;
    LabelSizeInfo: TLabel;
    MenuTreeViewItemSaveSelected: TMenuItem;
    MenuSeparator1: TMenuItem;
    MenuTreeViewItemCut: TMenuItem;
    MenuItemSeparator898989: TMenuItem;
    MenuTreeViewItemSeparator127u30130120983: TMenuItem;
    MenuTreeViewItemAddNonVisual: TMenuItem;
    MenuTreeViewItemAddBehavior: TMenuItem;
    MenuTreeViewItemRename: TMenuItem;
    MenuTreeViewItemAddTransform: TMenuItem;
    MenuTreeViewItemAddUserInterface: TMenuItem;
    MenuTreeViewItemDelete: TMenuItem;
    MenuTreeViewItemPaste: TMenuItem;
    MenuTreeViewItemCopy: TMenuItem;
    MenuTreeViewItemDuplicate: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    PanelLayoutTop: TPanel;
    PanelLayoutTransform: TPanel;
    PanelEventsInfo: TPanel;
    PanelAnchors: TPanel;
    MenuTreeView: TPopupMenu;
    SaveDesignDialog: TCastleSaveDialog;
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
    ButtonPhysicsPlayStop: TSpeedButton;
    ButtonPhysicsPause: TSpeedButton;
    ButtonSelectMode: TSpeedButton;
    ButtonTranslateMode: TSpeedButton;
    ButtonRotateMode: TSpeedButton;
    ButtonScaleMode: TSpeedButton;
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
    procedure ActionPhysicsPauseSimulationExecute(Sender: TObject);
    procedure ActionPhysicsPauseSimulationUpdate(Sender: TObject);
    procedure ActionPhysicsPlayStopSimulationExecute(Sender: TObject);
    procedure ButtonClearAnchorDeltasClick(Sender: TObject);
    procedure ButtonResetTransformationClick(Sender: TObject);
    procedure ButtonRotateModeClick(Sender: TObject);
    procedure ButtonScaleModeClick(Sender: TObject);
    procedure ButtonSelectModeClick(Sender: TObject);
    procedure ButtonTranslateModeClick(Sender: TObject);
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
    { Procedure called before selecton change unfortunately it does not return
      previously selected nodes. }
    procedure ControlsTreeBeforeSelectionChange(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ButtonInteractModeClick(Sender: TObject);
    procedure MenuItemAddComponentClick(Sender: TObject);
    procedure MenuTreeViewItemCutClick(Sender: TObject);
    procedure MenuTreeViewItemRenameClick(Sender: TObject);
    procedure MenuTreeViewItemDeleteClick(Sender: TObject);
    procedure MenuTreeViewItemCopyClick(Sender: TObject);
    procedure MenuTreeViewItemDuplicateClick(Sender: TObject);
    procedure MenuTreeViewItemPasteClick(Sender: TObject);
    procedure MenuTreeViewItemSaveSelectedClick(Sender: TObject);
    procedure MenuTreeViewPopup(Sender: TObject);
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
          TUiDraggingMode = (dmNone, dmTranslate, dmResize);
        var
          UiPendingMove: TVector2;
          UiDraggingMode: TUiDraggingMode;
          ResizingHorizontal: THorizontalPosition; //< Defined only when UiDraggingMode=dmResize
          ResizingVertical: TVerticalPosition; //< Defined only when UiDraggingMode=dmResize
          LabelHover, LabelSelected: TCastleLabel;
          RectHover, RectSelected: TCastleRectangleControl;
        { Should clicking inside UI rectangle start resizing (not only moving?). }
        function IsResizing(const UI: TCastleUserInterface; const Position: TVector2;
          out Horizontal: THorizontalPosition;
          out Vertical: TVerticalPosition; out Hint: String): Boolean;
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

        { UI under given mouse position.
          AMousePosition is in coordinates local to TCastleControl and follows
          CGE conventions that Y goes from bottom to top.

          Note: In most cases, prefer to call HoverComponent.
          HoverUserInterface *does not* consider transforms within TCastleViewport,
          it will just return TCastleViewport if mouse is over it. }
        function HoverUserInterface(const AMousePosition: TVector2): TCastleUserInterface;

        { UI or transform under given mouse position.
          AMousePosition is like for HoverUserInterface. }
        function HoverComponent(const AMousePosition: TVector2): TCastleComponent;
      end;

      TTreeNodeMap = class(specialize TDictionary<TComponent, TTreeNode>)
      end;

      TMode = (
        moInteract,
        moSelect,
        moTranslate,
        moRotate,
        moScale
      );

      TTreeNodeSide = (tnsRight, tnsBottom, tnsTop);

      TInspectorType = (itBasic, itLayout, itEvents, itAll);

    var
      Inspector: array [TInspectorType] of TOIPropertyGrid;
      FUndoSystem: TUndoSystem;
      PropertyEditorHook: TPropertyEditorHook;
      FDesignUrl: String;
      FDesignRoot: TComponent;
      { Viewport created for editing design with FDesignRoot being TCastleTransform. }
      FDesignViewportForTransforms: TCastleViewport;
      { Owner of all components saved/loaded to the design file.
        Also owner of a temporary viewport for .castle-transform,
        in general this owns everything specific to display currrent design. }
      DesignOwner: TComponent;
      { If design is visual (DesignRoot is TCastleUserInterface or
        TCastleTransform) then this is non-nil and allows to visualize
        alternative selected camera view. }
      CameraPreview: TCameraPreview;
      FDesignerLayer: TDesignerLayer;
      FDesignModified: Boolean;
      CastleControl: TCastleControl;
      TreeNodeMap: TTreeNodeMap;
      Mode: TMode;
      InsideToggleModeClick: Boolean;
      ControlsTreeNodeUnderMouse: TTreeNode;
      ControlsTreeNodeUnderMouseSide: TTreeNodeSide;
      PendingErrorBox: String;
      VisualizeTransformHover, VisualizeTransformSelected: TVisualizeTransform;
      CollectionPropertyEditorForm: TCollectionPropertyEditorForm;
      DesignStateBeforePhysicsRun: String;
      DesignModifiedBeforePhysicsRun: Boolean;
      FCurrentViewport: TCastleViewport;
      FCurrentViewportObserver: TFreeNotificationObserver;
      FComponentEditorDesigner: TComponentEditorDesigner;

      { Needed to check what was deselected after InternalSelectionStart.
        The only classes allowed here are TCastleBehavior.

        Note: This is TComponentList and as such it will automatically
        remove from itself the freed instances.
        Testcase that this is needed, when this is just TList:

        To any TCastleTransform, add 2 behaviors (like TCastleRigidBody
        and TCastleXxxCollider), select both with Shift, Delete,
        press "End" key (this sends an event that assumes that Behaviors
        list doesn't contain any dangling pointers).
      }
      FSelectionStartBehaviorList: TComponentList;

    { Create and add to the designed parent a new component,
      whose type best matches currently selected file in SourceShellList.
      May return @nil (and do nothing) if the SourceShellList does not
      have a suitable file selected for the given parent. }
    function ShellListAddComponent(
      const SourceShellList: TCastleShellListView;
      const ParentComponent: TComponent): TComponent;
    { Returns exactly the class that will be returned by
      ShellListAddComponent for the same arguments.
      May return @nil exactly if ShellListAddComponent also returns @nil. }
    function ShellListComponentClass(
      const SourceShellList: TCastleShellListView;
      const ParentComponent: TComponent): TComponentClass;

    function CameraToSynchronize(const V: TCastleViewport): TCastleCamera;
    procedure CameraSynchronize(const Source, Target: TCastleCamera; const MakeUndo: Boolean);
    procedure CastleControlOpen(Sender: TObject);
    procedure CastleControlResize(Sender: TObject);
    procedure CastleControlUpdate(Sender: TObject);
    procedure CastleControlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CastleControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CollectionPropertyEditorFormUnassign;
    function ComponentCaption(const C: TComponent): String;
    function TreeNodeCaption(const C: TComponent): String;
    function ControlsTreeAllowDrag(const Src, Dst: TTreeNode): Boolean;
    procedure FrameAnchorsChange(Sender: TObject);
    procedure AdjustUserInterfaceAnchorsToKeepRect(const UI: TCastleUserInterface;
      const RenderRectBeforeChange: TFloatRectangle);
    // Save and restore selection.
    // Careful: you can use it only if the operation between will *never* free any of them.
    //procedure SelectionRestoreAndFree(var Selection: Classes.TList);
    //function SelectionSave: Classes.TList;

    { Get selected component from tree node.
      Should be used directly only by GetSelected and GetSelectedComponent,
      and everything else should look at GetSelected and GetSelectedComponent
      to know selection. }
    function SelectedFromNode(Node: TTreeNode): TComponent;

    { Calculate all selected components as a list, non-nil <=> non-empty. }
    procedure GetSelected(out Selected: TComponentList;
      out SelectedCount: Integer);

    function GetSelectedUserInterface: TCastleUserInterface;
    procedure SetSelectedUserInterface(const Value: TCastleUserInterface);
    { If there is exactly one item selected, and it is TCastleUserInterface,
      return it. Otherwise return nil. }
    property SelectedUserInterface: TCastleUserInterface
      read GetSelectedUserInterface write SetSelectedUserInterface;

    function GetSelectedComponent: TComponent;
    procedure SetSelectedComponent(const Value: TComponent);
    { If there is exactly one item selected, return it. Otherwise return nil. }
    property SelectedComponent: TComponent
      read GetSelectedComponent write SetSelectedComponent;

    { After calling InternalSelectionStart we need check current selection to
      call InternalSelectionEnd }
    procedure CheckBehaviorsStillSelected;

    procedure DoInternalSelectionStart(const Behavior: TCastleBehavior;
      const TransformsToSynchronize: TCastleTransformList);
    procedure DoInternalSelectionEnd(const Behavior: TCastleBehavior);
    procedure DoAllInternalSelectionEnd;

    { When Joint is selected we need remove other joint gui. }
    procedure RemoveJointsAnchors;
    { Removes/adds transforms in this parent node, currently synchronizes only
      CastleTransforms }
    procedure SynchronizeTreeNodeChildTransforms(ParentNode: TTreeNode);
    { Synchronizes list of parent transforms. }
    procedure SynchronizeListOfTransforms(const TranformsList: TCastleTransformList);


    { If the selected items all have the same TCastleViewport parent,
      return it. Otherwise return nil. }
    function SelectedViewport: TCastleViewport;

    { Look at current selection and hover and possibly change CurrentViewport,
      calling also OnCurrentViewportChanged. }
    procedure UpdateCurrentViewport;

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
    procedure InspectorAllFilter(Sender: TObject; AEditor: TPropertyEditor;
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
    procedure PropertyGridCollectionItemClick(Sender: TObject);
    procedure PropertyGridCollectionItemClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PropertyGridCollectionItemAdd(Sender: TObject);
    procedure PropertyGridCollectionItemDelete(Sender: TObject);
    procedure PropertyGridCollectionItemMoveUp(Sender: TObject);
    procedure PropertyGridCollectionItemMoveDown(Sender: TObject);
    { Is Child selectable and visible in hierarchy. }
    class function Selectable(const Child: TComponent): Boolean; static;
    { Is Child deletable by user (this implies it is also selectable). }
    function Deletable(const Child: TComponent): Boolean;

    { Internal helper for UpdateDesign or ValidateHierarchy,
      should only be used by these 2 methods. }
    procedure ValidateOrUpdateHierarchy(const Validate: Boolean);

    { Make sure the hierarchy tree (ControlsTree) reflects current DesignRoot state.
      Add/remove tree nodes from ControlsTree as necessary.

      Also set TreeNodeMap (unconditionally, i.e. it is always cleared and filled
      from scratch by this method).

      This guarantees that editor UI shows the correct state of everything. }
    procedure UpdateDesign;

    { Returns a warning and false value when hierarchy tree (ControlsTree) differs from
      castle design hierarchy (DesignRoot). }
    function ValidateHierarchy: Boolean;

    procedure UpdateSelectedControl;
    procedure UpdateLabelSizeInfo(const UI: TCastleUserInterface);
    { Update anchors shown, based on UI state.
      Updates which buttons are pressed inside 2 TAnchorFrame instances.
      If AllowToHideParentAnchorsFrame, updates also checkbox
      "parent and self equal" and may hide the parent anchor frame if yes. }
    procedure UpdateAnchors(const UI: TCastleUserInterface;
      const AllowToHideParentAnchorsFrame: Boolean);
    procedure ChangeMode(const NewMode: TMode);
    { Filter property in object inspector.
      When FilterBySection = true, then Section matters and only properties in this section
      are displayed. }
    procedure InspectorFilter(const Sender: TObject; const AEditor: TPropertyEditor;
      var AShow: Boolean;
      const FilterBySection: Boolean; const Section: TPropertySection);
    procedure GizmoHasModifiedParent(Sender: TObject);
    procedure GizmoStopDrag(Sender: TObject);
    { Fix camera position, to look at Pos.XY in case of 2D games.
      Use this before doing V.InternalCamera.AnimateTo/SetWorldView with given Pos,Dir,Up.

      In case of 2D this means we should:

      - Fix Pos.Z, to keep camera Z matching current run-time camera.
        (this seems most natural and simplest camera behavior at design-time,
        so that design-time camera uses the same Z).

      - Fix Pos.XY, to account that Camera.Orthographic.Origin may <> (0.5,0.5).

      Done when operating in 2D (*not* detected by navigation type, but by projection type and axis,
      to keep this also working in Fly mode for 2D games).
    }
    procedure FixCamera2D(const V: TCastleViewport; var Pos: TVector3; const Dir, Up: TVector3);
    procedure ViewportViewBox(const V: TCastleViewport; Box: TBox3D);
    procedure CurrentViewportFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    OnUpdateFormCaption: TNotifyEvent;
    OnSelectionChanged: TNotifyEvent;
    { Called always when CurrentViewport value changed. }
    OnCurrentViewportChanged: TNotifyEvent;
    OnProposeOpenDesign: TProposeOpenDesignEvent;
    function RenamePossible: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveDesign(const Url: String);
    { Changes DesignRoot, DesignUrl and all the associated user-interface. }
    procedure OpenDesign(const NewDesignRoot, NewDesignOwner: TComponent;
      NewDesignUrl: String);
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
    { Free component C (which should be part of this designed, owned by DesignOwner)
      and all children.

      We have to delete things recursively, otherwise they would keep existing,
      taking resources and reserving names in DesignOwner,
      even though they would not be visible when disconnected from parent
      hierarchy.

      This does nothing if you try to free some internal component
      (like csTransient) or the design root (which can never be freed). }
    procedure FreeComponentRecursively(const C: TComponent);
    procedure CopyComponent;
    procedure PasteComponent;
    procedure CutComponent;
    procedure DuplicateComponent;
    { Set UIScaling values. }
    procedure UIScaling(const UIScaling: TUIScaling;
      const UIReferenceWidth, UIReferenceHeight: Single);

    property UndoSystem: TUndoSystem read FUndoSystem;
    property DesignUrl: String read FDesignUrl;
    { Root saved/loaded to component file }
    property DesignRoot: TComponent read FDesignRoot;
    property DesignModified: Boolean read FDesignModified;
    procedure RecordUndo(const UndoComment: String;
      const UndoCommentPriority: TUndoCommentPriority; const ItemIndex: Integer = -1);

    procedure ModifiedOutsideObjectInspector(const UndoComment: String;
      const UndoCommentPriority: TUndoCommentPriority; const UndoOnRelease: Boolean = false);

    procedure SaveSelected;

    procedure CurrentComponentApiUrl(var Url: String);

    { Viewport to act on using various commands.

      Chosen "aggressively" by looking at selected viewport
      (based on selected ui or transform within viewport)
      or viewport over which mouse hovers. We want to enable user to set current viewport
      as easily as possible, to avoid the "selection of viewport" being an extra step
      user needs to remember to do.

      @nil if none. }
    property CurrentViewport: TCastleViewport read FCurrentViewport;

    procedure ViewportViewAxis(const Dir, Up: TVector3);
    procedure ViewportViewAll;
    procedure ViewportViewSelected;
    procedure ViewportSetup2D;
    procedure ViewportSort2D;
    procedure ViewportToggleProjection;
    procedure ViewportAlignViewToCamera;
    procedure ViewportAlignCameraToView;
  end;

implementation

uses
  { Standard FPC/Lazarus units }
  // use Windows unit with FPC 3.0.x, to get TSplitRectType enums
  {$ifdef VER3_0} {$ifdef MSWINDOWS} Windows, {$endif} {$endif}
  TypInfo, StrUtils, Math, Graphics, Types, Dialogs, LCLType, ObjInspStrConsts,
  { CGE units }
  CastleUtils, CastleComponentSerialize, CastleFileFilters, CastleGLUtils, CastleImages,
  CastleLog, CastleProjection, CastleStringUtils, CastleTimeUtils,
  CastleURIUtils, X3DLoad, CastleFilesUtils, CastleInternalPhysicsVisualization,
  { CGE unit to keep in uses clause even if they are not explicitly used by FrameDesign,
    to register the core CGE components for (de)serialization. }
  Castle2DSceneManager, CastleNotifications, CastleThirdPersonNavigation, CastleSoundEngine,
  CastleBehaviors,
  { Editor units }
  EditorUtils, FormProject, CastleComponentEditorDesigner;

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
        if TDesignFrame.Selectable(C.Controls[I]) then
        begin
          Result := ControlUnder(C.Controls[I], MousePos, false);
          if Result <> nil then Exit;
        end;

      { Next try to find children, with TestWithBorder=true, so it tries harder
        to find something. }
      for I := C.ControlsCount - 1 downto 0 do
        if TDesignFrame.Selectable(C.Controls[I]) then
        begin
          Result := ControlUnder(C.Controls[I], MousePos, true);
          if Result <> nil then Exit;
        end;

      { Eventually return yourself, C. }
      //if C.CapturesEventsAtPosition(MousePos) then
      if SimpleCapturesEventsAtPosition(C, MousePos, TestWithBorder) and
         { Do not select TCastleNavigation, they would always obscure TCastleViewport. }
         (not (C is TCastleNavigation)) then
        Result := C;
    end;
  end;

  function MouseOverControl(const Control: TCastleControl): Boolean;
  var
    PosInClient: TPoint;
  begin
    PosInClient := Control.ScreenToClient(Mouse.CursorPos);
    Result := Control.ClientRect.Contains(PosInClient);
  end;

begin
  Result := nil;
  if MouseOverControl(Frame.CastleControl) then
  begin
    if Frame.DesignRoot is TCastleUserInterface then
      Result := ControlUnder(Frame.DesignRoot as TCastleUserInterface, AMousePosition, true)
    else
    if Frame.DesignRoot is TCastleTransform then
      Result := Frame.FDesignViewportForTransforms;
  end;
end;

function TDesignFrame.TDesignerLayer.HoverComponent(const AMousePosition: TVector2): TCastleComponent;

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
    it would be weird for user here. }

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

function TDesignFrame.TDesignerLayer.IsResizing(const UI: TCastleUserInterface;
  const Position: TVector2; out Horizontal: THorizontalPosition;
  out Vertical: TVerticalPosition; out Hint: String): Boolean;
const
  BorderDragMargin = 10;
var
  R: TFloatRectangle;
  ResizeWidth, ResizeHeight, IsTop, IsBottom, IsRight, IsLeft: Boolean;
  ResizeDisabledReason: String;
begin
  R := UI.RenderRectWithBorder;

  UI.EditorAllowResize(ResizeWidth, ResizeHeight, ResizeDisabledReason);

  Hint := '';

  IsTop := R.TopPart(BorderDragMargin).Contains(Position);
  IsBottom := R.BottomPart(BorderDragMargin).Contains(Position);
  IsRight := R.RightPart(BorderDragMargin).Contains(Position);
  IsLeft := R.LeftPart(BorderDragMargin).Contains(Position);

  { the order of checking (top or bottom) matters in case of very
    small heights. }
  if ResizeHeight and IsTop then
    Vertical := vpTop
  else
  if ResizeHeight and IsBottom then
    Vertical := vpBottom
  else
  begin
    Vertical := vpMiddle;
    if IsTop or IsBottom then
      Hint := ResizeDisabledReason;
  end;

  if ResizeWidth and IsRight then
    Horizontal := hpRight
  else
  if ResizeWidth and IsLeft then
    Horizontal := hpLeft
  else
  begin
    Horizontal := hpMiddle;
    if IsLeft or IsRight then
      Hint := ResizeDisabledReason;
  end;

  if Hint <> '' then
    Hint := UI.Name + ':' + NL + Hint;

  Result := (Vertical <> vpMiddle) or (Horizontal <> hpMiddle);
end;

function TDesignFrame.TDesignerLayer.IsResizing(const UI: TCastleUserInterface;
  const Position: TVector2; out Horizontal: THorizontalPosition;
  out Vertical: TVerticalPosition): Boolean;
var
  Hint: String;
begin
  Result := IsResizing(UI, Position, Horizontal, Vertical, Hint);
end;

function TDesignFrame.TDesignerLayer.Press(
  const Event: TInputPressRelease): Boolean;
var
  UI: TCastleUserInterface;
begin
  Result := inherited Press(Event);
  if Result then Exit;

  { We have a few shortcuts that should only work when the focus is over viewport
    (that is, if they reach TDesignerLayer.Press),
    because otherwise they would conflict with editing text in OI or names in hierarchy. }
  if Event.IsKey(CtrlZ) and (not (mkShift in Event.ModifiersDown)) then
  begin
    Frame.PerformUndo;
    Exit(ExclusiveEvents);
  end;
  if Event.IsKey(CtrlZ) and (mkShift in Event.ModifiersDown) then
  begin
    Frame.PerformRedo;
    Exit(ExclusiveEvents);
  end;
  if Event.IsKey(keyF) then
  begin
    Frame.ViewportViewSelected;
    Exit(ExclusiveEvents);
  end;
  if Event.IsKey(keyHome) then
  begin
    Frame.ViewportViewAll;
    Exit(ExclusiveEvents);
  end;

  { Avoid handling mouse events over CameraPreview, to avoid messing with clicking
    on CameraPreview buttons, and using gizmos in CameraPreview viewport. }
  if Frame.CameraPreview.UiRoot.Exists and
     Frame.CameraPreview.UiRoot.RenderRectWithBorder.Contains(Event.Position) then
    Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    { Below we will always calculate correct UiDraggingMode }
    UiDraggingMode := dmNone;

    if Frame.Mode <> moInteract then
    begin
      { Calculate Frame.SelectedComponent.
        Without shift pressed, we select before moving/resizing.
        With shift pressed we don't change selection.
        This allows to change the component without changing the selected component. }
      if (not (mkShift in Event.ModifiersDown)) then
      begin
        Frame.SelectedComponent := HoverComponent(Event.Position);
      end;

      if Frame.Mode <> moSelect then
      begin
        UI := Frame.SelectedUserInterface;
        if UI <> nil then
        begin
          if IsResizing(UI, Event.Position, ResizingHorizontal, ResizingVertical) then
            UiDraggingMode := dmResize
          else
            UiDraggingMode := dmTranslate;

          { No need for this Exit(true) otherwise.
            In particular if we selected transform, we should not do Exit(ExclusiveEvents),
            to allow gizmos to start dragging. }
          Exit(ExclusiveEvents);
        end;
      end;

      UiPendingMove := TVector2.Zero;
    end;
  end;
end;

function TDesignFrame.TDesignerLayer.Release(const Event: TInputPressRelease): Boolean;
var
  Sel: TComponent;
begin
  Result := inherited Press(Event);
  if Result then Exit;

  { Avoid handling mouse events over CameraPreview, to avoid messing with clicking
    on CameraPreview buttons, and using gizmos in CameraPreview viewport. }
  if Frame.CameraPreview.UiRoot.Exists and
     Frame.CameraPreview.UiRoot.RenderRectWithBorder.Contains(Event.Position) then
    Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    UiDraggingMode := dmNone;

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
    case UiDraggingMode of
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
    case UiDraggingMode of
      dmTranslate:
        begin
          UI.Translation := UI.Translation + Vector2(X, Y);
        end;
      dmResize:
        begin
          {$warnings off} // TODO: using deprecated Horizontal/VerticalAnchorDelta
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
          {$warnings on}
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
    WouldResizeHorizontal: THorizontalPosition;
    WouldResizeVertical: TVerticalPosition;
    NewCursor: TMouseCursor;
    Hint: String;
    HoverC: TCastleComponent;
  begin
    Hint := '';
    if Frame.Mode = moInteract then
      NewCursor := mcDefault
    else
    case UiDraggingMode of
      dmNone:
        begin
          // calculate cursor based on what would happen if you Press
          HoverC := HoverComponent(Event.Position);
          if HoverC is TCastleUserInterface then
          begin
            if IsResizing(HoverC as TCastleUserInterface, Event.Position,
              WouldResizeHorizontal, WouldResizeVertical, Hint) then
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
    Frame.CastleControl.Hint := Hint;
    Frame.CastleControl.ShowHint := Hint <> '';
  end;

  procedure UpdateHoverComponent;
  var
    HoverC: TCastleComponent;
  begin
    if Frame.Mode <> moInteract then
    begin
      HoverC := HoverComponent(Event.Position); // may also return nil
      if HoverC is TCastleTransform then
        Frame.VisualizeTransformHover.Parent := TCastleTransform(HoverC)
      else
        Frame.VisualizeTransformHover.Parent := nil;
    end else
      Frame.VisualizeTransformHover.Parent := nil;
  end;

  { Sometimes, trying to drag (translate/resize) UI,
    should actually drag the UI's parent (or parent's parent...).
    This happens when UI has FullSize,
    or it is within a group that auto-aligns children.

    The DragAllowed is such case would block some actions on UI,
    and it is intuitive to perform them on parent.

    TODO: This has hardcoded logic for FullSize and group cases.
    There should be a virtual method instead like
      TCastleUserInterface.EditorDragParent(UiDraggingMode)
      TCastleUserInterface.EditorDragMeInsteadOfChild(UiDraggingMode)
  }
  procedure ChangeDraggedUI(var UI: TCastleUserInterface);
  begin
    if (UiDraggingMode in [dmResize, dmTranslate]) and
       UI.FullSize and
       (UI.Parent <> nil) then
    begin
      UI := UI.Parent;
      ChangeDraggedUI(UI); // act recursively if necessary, to choose parent's parent...
    end;

    if (UiDraggingMode in [dmResize, dmTranslate]) and
       (UI.Parent <> nil) and
       { When the parent uses AutoSizeToChildren on a single child, then the effect is similar
         as it child had FullSize. }
       UI.Parent.AutoSizeToChildren and
       (UI.Parent.ControlsCount = 1) and
       {$warnings off} // looking at deprecated Left, Bottom to keep them working
       (UI.Left = 0) and
       (UI.Bottom = 0) and
       {$warnings on}
       TVector2.PerfectlyEquals(UI.Translation, TVector2.Zero) then
    begin
      UI := UI.Parent;
      ChangeDraggedUI(UI); // act recursively if necessary, to choose parent's parent...
    end;

    if (UiDraggingMode in [dmTranslate]) and
       (UI.Parent <> nil) and
       ( (UI.Parent is TCastleHorizontalGroup) or
         (UI.Parent is TCastleVerticalGroup) ) then
    begin
      UI := UI.Parent;
      ChangeDraggedUI(UI); // act recursively if necessary, to choose parent's parent...
    end;
  end;

var
  UI: TCastleUserInterface;
  Move: TVector2;
  Snap: Single;
begin
  Result := inherited Motion(Event);
  if Result then Exit;

  { Avoid handling mouse events over CameraPreview, to avoid messing with clicking
    on CameraPreview buttons, and using gizmos in CameraPreview viewport. }
  if Frame.CameraPreview.UiRoot.Exists and
     Frame.CameraPreview.UiRoot.RenderRectWithBorder.Contains(Event.Position) then
    Exit;

  { in case user released mouse button, but the event didn't reach us for some reason
    (maybe can happen e.g. if you Alt+Tab during dragging?),
    reset UiDraggingMode. }
  if (UiDraggingMode <> dmNone) and
     (not (buttonLeft in Event.Pressed)) then
    UiDraggingMode := dmNone;

  if (Frame.Mode <> moInteract) and
     (UiDraggingMode <> dmNone) then
  begin
    UI := Frame.SelectedUserInterface;
    if UI <> nil then
    begin
      ChangeDraggedUI(UI);

      Move := (Event.Position - Event.OldPosition) / UI.UIScale;

      Snap := Frame.SpinEditSnap.Value;
      if Snap <> 0 then
      begin
        UiPendingMove += Move;
        while Abs(UiPendingMove.X) >= Snap do
        begin
          ApplyDrag(UI, Sign(UiPendingMove.X) * Snap, 0);
          UiPendingMove.X := UiPendingMove.X - Sign(UiPendingMove.X) * Snap;
        end;
        while Abs(UiPendingMove.Y) >= Snap do
        begin
          ApplyDrag(UI, 0, Sign(UiPendingMove.Y) * Snap);
          UiPendingMove.Y := UiPendingMove.Y - Sign(UiPendingMove.Y) * Snap;
        end;
      end else
      begin
        ApplyDrag(UI, Move.X, Move.Y);
      end;

      Exit(ExclusiveEvents);
    end;
  end;

  if not InternalDesignMouseLook then
    { do not override cursor when InternalDesignMouseLook,
      to allow mouse look to hide cursor. }
    UpdateCursor;

  UpdateHoverComponent;
end;

procedure TDesignFrame.TDesignerLayer.Render;

  procedure UpdateAttachedLabel(const UI: TCastleUserInterface;
    const UIRect: TFloatRectangle;
    const Lab: TCastleLabel; const Rect: TCastleRectangleControl;
    const LabelColor: TCastleColor);
  begin
    { Show additional UI to show selected UI.
      While these labels can be helpful, they often obscure important UI
      (even the one you're trying to edit) so it is also causing trouble.

      After some experimenting, we only show them on components with zero size.
      This allows to e.g. know where is new TCastleVertical/HorizontalGroup
      (as it is selected right after addition, and has zero size since has
      no children, and AutoSize=true).
    }
    if (UI <> nil) and
       (
         (UI.EffectiveWidth = 0) or
         (UI.EffectiveHeight = 0)
       ) then
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
      Rect.Anchor(hpLeft, Max(0, UIRect.Left));
      Rect.Anchor(vpBottom, UIRect.Top);

      if Rect.RenderRect.Top > Rect.Container.Height then
        // put Rect inside UI, otherwise it would be offscreen
        Rect.Anchor(vpTop, vpBottom, Min(Rect.Container.Height, UIRect.Top));
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

  { Note: HoverUI is in practice never visible now,
    as UpdateAttachedLabel makes it visible only for components with size zero.
    This code can be simplified, to remove HoverUI, LabelHover, RectHover
    -- once we're sure that showing "hover" is a useless idea at some point. }
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
      RectHover.Translation := RectHover.Translation - Vector2(0, RectSelected.EffectiveHeight);
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

    if UseIconsAndColorsForDarkTheme then
    begin
      Result.GutterColor := $9EFFFF;
      Result.ValueFont.Color := $9EFFFF;
      Result.SubPropertiesColor := clWindowText;
      Result.ReferencesColor := $9EFFFF;
    end;
  end;

begin
  inherited;

  PropertyEditorHook := TPropertyEditorHook.Create(Self);

  FComponentEditorDesigner := TConcreteEditorDesigner.Create(Self, PropertyEditorHook);

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
  Inspector[itAll].OnEditorFilter := @InspectorAllFilter;
  Inspector[itAll].Parent := TabAll;
  Inspector[itAll].Filter := tkProperties;

  Inspector[itEvents] := CommonInspectorCreate;
  Inspector[itEvents].Parent := TabEvents;
  Inspector[itEvents].Filter := tkMethods;
  Inspector[itEvents].AnchorToNeighbour(akTop, 0, PanelEventsInfo);

  CastleControl := TCastleControl.Create(Self);
  CastleControl.AutoFocus := true; // needed on Windows to receive AWSD, Ctrl+Z...
  CastleControl.Align := alClient;
  CastleControl.OnResize := @CastleControlResize;
  CastleControl.OnOpen := @CastleControlOpen;
  CastleControl.OnUpdate := @CastleControlUpdate;
  CastleControl.OnDragOver := @CastleControlDragOver;
  CastleControl.OnDragDrop := @CastleControlDragDrop;
  CastleControl.Parent := PanelMiddle; // set Parent last, following https://wiki.freepascal.org/LCL_Tips#Set_the_Parent_as_last

  {$ifdef DEBUG_GIZMO_PICK}
  TCastleControl.MainControl := CastleControl;
  {$endif DEBUG_GIZMO_PICK}

  FDesignerLayer := TDesignerLayer.Create(Self);
  FDesignerLayer.Frame := Self;
  CastleControl.Controls.InsertFront(FDesignerLayer);

  // It's too easy to change it visually and forget, so we set it from code
  ControlProperties.ActivePage := TabBasic;

  TreeNodeMap := TTreeNodeMap.Create;
  FSelectionStartBehaviorList := TComponentList.Create(false);

  SelfAnchorsFrame.OnAnchorChange := @FrameAnchorsChange;
  ParentAnchorsFrame.OnAnchorChange := @FrameAnchorsChange;

  VisualizeTransformHover := TVisualizeTransform.Create(Self, true);
  VisualizeTransformSelected := TVisualizeTransform.Create(Self, false);
  VisualizeTransformSelected.OnParentModified := @GizmoHasModifiedParent;
  VisualizeTransformSelected.OnGizmoStopDrag := @GizmoStopDrag;

  SaveDesignDialog.InitialDir := URIToFilenameSafe(ApplicationDataOverride);

  ChangeMode(moTranslate); // most expected default

  BuildComponentsMenu(
    MenuTreeViewItemAddUserInterface,
    MenuTreeViewItemAddTransform,
    MenuTreeViewItemAddBehavior,
    MenuTreeViewItemAddNonVisual,
    @MenuItemAddComponentClick);

  FCurrentViewportObserver := TFreeNotificationObserver.Create(Self);
  FCurrentViewportObserver.OnFreeNotification := {$ifdef FPC}@{$endif} CurrentViewportFreeNotification;

  // needed to set right action state maybe lazarus bug?
  ActionPhysicsPauseSimulation.Update;
end;

destructor TDesignFrame.Destroy;
var
  F: TCollectionPropertyEditorForm;
begin
  FreeAndNil(FSelectionStartBehaviorList);
  FreeAndNil(TreeNodeMap);
  FreeAndNil(CameraPreview);
  FreeAndNil(FComponentEditorDesigner);

  if CollectionPropertyEditorForm <> nil then
  begin
    F := CollectionPropertyEditorForm;
    CollectionPropertyEditorFormUnassign;
    F.Close;
  end;

  inherited Destroy;
end;

procedure TDesignFrame.CollectionPropertyEditorFormUnassign;
begin
  if CollectionPropertyEditorForm <> nil then
  begin
    // unassign our callbacks from the form, as this TDesignFrame instance will no longer be valid
    CollectionPropertyEditorForm.OnClose := nil;
    CollectionPropertyEditorForm.CollectionListBox.OnClick := nil;
    CollectionPropertyEditorForm.AddButton.OnClick := nil;
    CollectionPropertyEditorForm.DeleteButton.OnClick := nil;
    CollectionPropertyEditorForm.MoveUpButton.OnClick := nil;
    CollectionPropertyEditorForm.MoveDownButton.OnClick := nil;
    CollectionPropertyEditorForm.RemoveFreeNotification(Self);
    CollectionPropertyEditorForm := nil;
  end;
end;

procedure TDesignFrame.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = CollectionPropertyEditorForm) then
    CollectionPropertyEditorFormUnassign;
end;

procedure TDesignFrame.SaveDesign(const Url: String);
begin
  if DesignRoot is TCastleUserInterface then
    UserInterfaceSave(TCastleUserInterface(DesignRoot), Url)
  else
  if DesignRoot is TCastleTransform then
    TransformSave(TCastleTransform(DesignRoot), Url)
  else
    ComponentSave(DesignRoot, Url);
  FDesignModified := false;
  FDesignUrl := Url; // after successfull save
  OnUpdateFormCaption(Self);
end;

procedure TDesignFrame.ClearDesign;
begin
  RemoveJointsAnchors;
  FSelectionStartBehaviorList.Clear;

  // ControlsTree.Items.Clear; // do not clear, we will always rebuild ControlsTree to just apply differences
  ControlsTree.Selected := nil; // TODO: for now we reset selection, though maybe we could preserve it in some cases

  UpdateSelectedControl;
  //CastleControl.Controls.Clear; // don't clear it, leave DesignerLayer
  FDesignRoot := nil;
  FreeAndNil(CameraPreview);

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
  NewDesignOwner, NewDesignRoot: TComponent;
  InspectorType: TInspectorType;
begin
  for InspectorType in TInspectorType do
    Inspector[InspectorType].SaveChanges;

  NewDesignOwner := TComponent.Create(Self);
  NewDesignRoot := InternalStringToComponent(UHE.Data, NewDesignOwner, DesignOwner);
  OpenDesign(NewDesignRoot, NewDesignOwner, FDesignUrl);

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
  NewDesignUrl: String);

  { Note: NewDesignUrl parameter is *not* const.

    Because OpenDesign uses ClearDesign which destroys some components,
    and one of these components may be the owner of AnsiString with NewDesignUrl.
    So we have to keep refcount of NewDesignUrl,
    otherwise ClearDesign could free NewDesignUrl and then all its
    refences are invalid pointers, and OnUpdateFormCaption will crash.

    Testcase: Open any UI state design that uses TCastleDesign, click
    "Open Referenced Design".
    In this case, TCastleDesignComponentEditor.ExecuteVerb passes
    TCastleDesign.URL and NewDesignUrl, and ClearDesign removes this
    TCastleDesign instance.
  }

  { Initialize TCastleViewport that is internal, and used to edit
    .castle-transform. It requires some special considerations,
    as it needs a good camera/navigation, but also user cannot modify them,
    and they are not saved to .castle-transform. }
  procedure InitializeDesignViewportForTransforms(const V: TCastleViewport);
  var
    APos, ADir, AUp, AGravityUp: TVector3;
  begin
    { This Name is user-visible: if user selects anything in viewport,
      we show CurrentViewport.Name in header. }
    V.Name := 'InternalViewport';
    V.Transparent := true;
    V.FullSize := true;

    { Add headlight to design-time camera this way.
      Note that doing "V.Items.UseHeadLight := hlOn" would not have a desired
      effect, as this headlight would be done only from runtime camera,
      which we don't care about in this case. }
    V.InternalCamera.Add(TCastleDirectionalLight.Create(NewDesignOwner));

    CameraViewpointForWholeScene(V.Items.BoundingBox, 2, 1, false, true,
      APos, ADir, AUp, AGravityUp);
    V.InternalCamera.SetWorldView(APos, ADir, AUp);
    V.InternalCamera.GravityUp := AGravityUp;
  end;

var
  //LabelNonVisualHint: TCastleLabel;
  DesignRootVisual: Boolean;
begin
  ClearDesign;

  { We use CastleControl.Controls.InsertBack here, to keep DesignerLayer
    in the front. }

  DesignRootVisual :=
    (NewDesignRoot is TCastleUserInterface) or
    (NewDesignRoot is TCastleTransform);

  if DesignRootVisual then
  begin
    CameraPreview := TCameraPreview.Create(NewDesignOwner);
    CastleControl.Controls.InsertBack(CameraPreview.UiRoot);
  end;

  if NewDesignRoot is TCastleUserInterface then
  begin
    CastleControl.Controls.InsertBack(NewDesignRoot as TCastleUserInterface);
    Assert(DesignRootVisual);
  end else
  if NewDesignRoot is TCastleTransform then
  begin
    FDesignViewportForTransforms := TCastleViewport.Create(NewDesignOwner);
    FDesignViewportForTransforms.Items.Add(NewDesignRoot as TCastleTransform);
    { Do this after adding NewDesignRoot, as it wants good Items.BoundingBox }
    InitializeDesignViewportForTransforms(FDesignViewportForTransforms);
    CastleControl.Controls.InsertBack(FDesignViewportForTransforms);
    Assert(DesignRootVisual);
  end else
  begin
    { This is normal situation for non-visual components. }
    (*
    LabelNonVisualHint := TCastleLabel.Create(NewDesignOwner);
    LabelNonVisualHint.Caption := 'This design does not contain any visual components.' + NL +
      'The root of this design has class:' + NL +
      NL +
      NewDesignRoot.ClassName;
    LabelNonVisualHint.Anchor(hpMiddle);
    LabelNonVisualHint.Anchor(vpMiddle);
    LabelNonVisualHint.FontSize := 40;
    CastleControl.Controls.InsertBack(LabelNonVisualHint);
    *)
    Assert(not DesignRootVisual);
  end;

  SetEnabledVisible(CastleControl, DesignRootVisual);
  SetEnabledVisible(PanelMiddleTop, DesignRootVisual);

  // set background to gray
  CastleControl.Container.BackgroundColor := Vector4(0.5, 0.5, 0.5, 1);

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
    if Mime = 'text/x-castle-component' then
      NewDesignRoot := ComponentLoad(NewDesignUrl, NewDesignOwner)
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

function TDesignFrame.AddComponent(const ParentComponent: TComponent;
  const ComponentClass: TComponentClass;
  const ComponentOnCreate: TNotifyEvent): TComponent;

  function CreateComponent: TComponent;
  begin
    Result := ComponentClass.Create(DesignOwner) as TComponent;
    if Assigned(ComponentOnCreate) then // call ComponentOnCreate ASAP after constructor
      ComponentOnCreate(Result);
    Result.Name := InternalProposeName(ComponentClass, DesignOwner);
  end;

  procedure FinishAddingComponent(const NewComponent: TComponent);
  begin
    UpdateDesign;
    SelectedComponent := NewComponent; // select after adding, makes it natural to edit
    ModifiedOutsideObjectInspector('Add ' + NewComponent.Name + ' to ' + ParentComponent.Name, ucHigh);
  end;

  function AddToTransform(const ParentComponent: TCastleTransform): TComponent;
  begin
    if ComponentClass.InheritsFrom(TCastleTransform) then
    begin
      Result := CreateComponent;
      ParentComponent.Add(Result as TCastleTransform);
      FinishAddingComponent(Result);
    end else
    if ComponentClass.InheritsFrom(TCastleUserInterface) then
    begin
      raise Exception.Create(Format('Cannot add TCastleUserInterface descendant (%s) when the parent is a TCastleTransform descendant (%s). Select a parent that descends from TCastleUserInterface.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
    end else
    if ComponentClass.InheritsFrom(TCastleBehavior) then
    begin
      Result := CreateComponent;
      ParentComponent.AddBehavior(Result as TCastleBehavior);
      try
        { If component is TCastleMeshCollider try to set Scene property to parent }
        if (Result is TCastleMeshCollider) and ParentComponent.HasColliderMesh then
          (Result as TCastleMeshCollider).Scene := ParentComponent;
      finally
        FinishAddingComponent(Result);
      end;
    end else
    begin
      Result := CreateComponent;
      ParentComponent.AddNonVisualComponent(Result);
      FinishAddingComponent(Result);
    end;
  end;

  function AddToUserInterface(const ParentComponent: TCastleUserInterface): TComponent;
  begin
    if ComponentClass.InheritsFrom(TCastleUserInterface) then
    begin
      Result := CreateComponent;
      ParentComponent.InsertFront(Result as TCastleUserInterface);
      FinishAddingComponent(Result);
    end else
    if ComponentClass.InheritsFrom(TCastleTransform) then
    begin
      raise Exception.Create(Format('Cannot add TCastleTransform descendant (%s) when the parent is a TCastleUserInterface descendant (%s). Select a parent that descends from TCastleTransform, for example select Viewport.Items.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
    end else
    if ComponentClass.InheritsFrom(TCastleBehavior) then
    begin
      raise Exception.Create(Format('Cannot add TCastleBehavior descendant (%s) when the parent is a TCastleUserInterface descendant (%s). Select a parent that descends from TCastleTransform, like TCastleTransform itself or TCastleScene.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
    end else
    begin
      Result := CreateComponent;
      ParentComponent.AddNonVisualComponent(Result);
      FinishAddingComponent(Result);
    end;
  end;

  function AddToComponent(const ParentComponent: TCastleComponent): TComponent;
  begin
    { Note that, technically,
      we could add TCastleUserInterface/TCastleTransform/TCastleBehavior
      to non-visual components list.
      But this would be confusing, so we disallow it in editor. }
    if ComponentClass.InheritsFrom(TCastleUserInterface) then
    begin
      raise Exception.Create(Format('To add TCastleUserInterface descendant (%s), select a parent that descends from TCastleUserInterface.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
    end else
    if ComponentClass.InheritsFrom(TCastleTransform) then
    begin
      raise Exception.Create(Format('To add TCastleTransform descendant (%s), select a parent that descends from TCastleTransform, for example select Viewport.Items.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
    end else
    if ComponentClass.InheritsFrom(TCastleBehavior) then
    begin
      raise Exception.Create(Format('To add TCastleBehavior descendant (%s), select a parent that descends from TCastleTransform, like TCastleTransform itself or TCastleScene.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
    end else
    begin
      Result := CreateComponent;
      ParentComponent.AddNonVisualComponent(Result);
      FinishAddingComponent(Result);
    end;
  end;

begin
  { Cancel editing the component name, when adding a component.
    See https://trello.com/c/IC6NQx0X/59-bug-adding-a-component-to-a-component-that-is-being-currently-renamed-triggers-and-exception . }
  if ControlsTree.Selected <> nil then
    ControlsTree.Selected.EndEdit(true);

  if ParentComponent is TCastleUserInterface then
  begin
    Exit(AddToUserInterface(TCastleUserInterface(ParentComponent)));
  end else
  if ParentComponent is TCastleTransform then
  begin
    Exit(AddToTransform(TCastleTransform(ParentComponent)));
  end else
  if ParentComponent is TCastleComponent then
  begin
    Exit(AddToComponent(ParentComponent as TCastleComponent));
  end else
    raise Exception.Create(Format('Cannot add to the parent of class %s, select other parent before adding.',
      [ParentComponent.ClassName]))
end;

procedure TDesignFrame.FreeComponentRecursively(const C: TComponent);

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
  Assert(C.Owner = DesignOwner);

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
  C.Free;

  UpdateDesign;
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

      { Run InternalSelectionEnd on all Behaviors before freeing it and
        ControlsTree.Items.Clear }
      DoAllInternalSelectionEnd;

      { We depend on the fact TComponentList observes freed items,
        and removes them automatically.
        This way also freeing something that frees something else
        should work (although we don't really need it now,
        DesignOwner owns everything). }

      repeat
        C := FirstDeletableComponent(Selected);
        if C <> nil then
          FreeComponentRecursively(C)
        else
          Break;
      until false;

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
  if NewComponent is TCastleBehavior then
  begin
    if not (ParentComponent is TCastleTransform) then
    begin
      ErrorBox('Clipboard contains a TCastleTransform instance, you need to select a TCastleTransform as a parent before doing "Paste Component"');
      FreeAndNil(NewComponent);
      Exit;
    end;
    (ParentComponent as TCastleTransform).AddBehavior(NewComponent as TCastleBehavior);
    FinishAddingComponent(NewComponent);
  end else
  begin
    ErrorBox(Format('Clipboard contains an instance of %s class, cannot insert it into the design',
      [NewComponent.ClassName]));
    FreeAndNil(NewComponent);
  end;
end;

procedure TDesignFrame.CutComponent;
var
  Sel: TComponent;
begin
  Sel := SelectedComponent;
  if (Sel <> nil) and
     (not (csSubComponent in Sel.ComponentStyle)) then
  begin
    Clipboard.AsText := ComponentToString(Sel);
    DeleteComponent;
  end else
  begin
    ErrorBox('Select exactly one component, that is not a subcomponent, to copy');
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
    ParentComp := Selected.Parent;
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
  ViewportChild: TCastleUserInterface;
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
      if Sel is TCastleUserInterface then
      begin
        { When hovering over TCastleNavigation, or TCastleTouchNavigation,
          or really any UI over viewport -> select viewport. }
        ViewportChild := Sel as TCastleUserInterface;
        if {ViewportChild.FullSize and} (ViewportChild.Parent is TCastleViewport) then
        begin
          NewResult := ViewportChild.Parent as TCastleViewport;
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

procedure TDesignFrame.UpdateCurrentViewport;
var
  NewCurrentViewport: TCastleViewport;
  HoverUi: TCastleUserInterface;
begin
  { try SelectedViewport }
  NewCurrentViewport := SelectedViewport;

  if NewCurrentViewport = nil then
  begin
    { try HoverUserInterface as TCastleViewport }
    HoverUi := FDesignerLayer.HoverUserInterface(CastleControl.MousePosition);
    if HoverUi is TCastleViewport then // also checks HoverUi <> nil
      NewCurrentViewport := TCastleViewport(HoverUi)
    else
    { try HoverUserInterface as TCastleViewport child, like TCastleNavigation, TCastleTouchNavigation }
    if (HoverUi <> nil) and (HoverUi.Parent is TCastleViewport) then // also checks HoverUi.Parent <> nil
      NewCurrentViewport := TCastleViewport(HoverUi.Parent);
  end;

  if (NewCurrentViewport <> nil) and
     (FCurrentViewport <> NewCurrentViewport) then
  begin
    FCurrentViewport := NewCurrentViewport;
    FCurrentViewportObserver.Observed := NewCurrentViewport;
    if Assigned(OnCurrentViewportChanged) then
      OnCurrentViewportChanged(Self);
  end;
  { otherwise keep using FCurrentViewport we had so far }
end;

procedure TDesignFrame.CurrentViewportFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  FCurrentViewport := nil;
  if Assigned(OnCurrentViewportChanged) then
    OnCurrentViewportChanged(Self);
end;

const
  CameraTransitionTime = 0.25;

procedure TDesignFrame.ViewportViewAxis(const Dir, Up: TVector3);
var
  V: TCastleViewport;
  Box: TBox3D;
  Distance: Single;
  NewPos: TVector3;
begin
  V := CurrentViewport;
  if V = nil then Exit;
  if V.Items = nil then Exit;

  Box := V.Items.BoundingBox;
  if Box.IsEmpty then Exit;

  Distance := PointsDistance(V.InternalCamera.WorldTranslation, Box.Center);
  NewPos := Box.Center - Dir * Distance;
  FixCamera2D(V, NewPos, Dir, Up);
  V.InternalCamera.AnimateTo(NewPos, Dir, Up, CameraTransitionTime);
end;

procedure TDesignFrame.FixCamera2D(const V: TCastleViewport; var Pos: TVector3; const Dir, Up: TVector3);
begin
  if (V.InternalCamera.ProjectionType = ptOrthographic) and
     TVector3.Equals(Dir, Vector3(0, 0, -1)) and
     TVector3.Equals(Up, Vector3(0, 1, 0)) then
  begin
    if V.Camera <> nil then
      Pos.Z := V.Camera.Translation.Z
    else
      Pos.Z := Default2DCameraZ;

    Pos.X := Pos.X - (0.5 - V.InternalCamera.Orthographic.Origin.X) * V.InternalCamera.Orthographic.EffectiveRect.Width;
    Pos.Y := Pos.Y - (0.5 - V.InternalCamera.Orthographic.Origin.Y) * V.InternalCamera.Orthographic.EffectiveRect.Height;
  end;
end;

procedure TDesignFrame.ViewportViewBox(const V: TCastleViewport; Box: TBox3D);
var
  APos, ADir, AUp: TVector3;
  IntersectionDistance: Single;
begin
  // in particular, condition below means we don't do anything if no TCastleTransform selected
  if not Box.IsEmpty then
  begin
    V.InternalCamera.GetWorldView(APos, ADir, AUp);

    { Convert Box to use maximum size in all 3 dimensions.
      This results in better camera view for boxes mostly flat in 1 dimension. }
    Box := Box3DAroundPoint(Box.Center, Box.Size.Max);
    if not Box.TryRayClosestIntersection(IntersectionDistance, Box.Center, -ADir) then
    begin
      { TryRayClosestIntersection may return false for box with size zero
        (though not observed in practice),
        only then ray from Box.Center may not hit one of box walls. }
      IntersectionDistance := 1;
      WritelnWarning('Ray from box center didn''t hit any of box walls');
    end;
    APos := Box.Center - ADir * IntersectionDistance * 2;

    { Older version of this routine was doing:

    CameraViewpointForWholeScene(Box, 2, 1, false, true,
      APos, ADir, AUp, AGravityUp);

    New version only modifies the camera position, preserving existing
    dir, up. This is more flexible for movement by key shortcuts,
    and consistent with
    - Blender home
    - Unity F
    - Godot O
    }

    FixCamera2D(V, APos, ADir, AUp);
    V.InternalCamera.AnimateTo(APos, ADir, AUp, CameraTransitionTime);
  end;
end;

procedure TDesignFrame.ViewportViewAll;
var
  V: TCastleViewport;
begin
  V := CurrentViewport;
  if V = nil then Exit;

  if V.Items <> nil then
    ViewportViewBox(V, V.Items.BoundingBox);
end;

procedure TDesignFrame.ViewportViewSelected;

  { Determine selected TCastleTransform based on selected component, or @nil.
    This routine treats selecting a behavior just like selecting a transform,
    so that pressing F on behavior works. }
  function TransformFromSelected(const C: TComponent): TCastleTransform;
  begin
    if C is TCastleTransform then
      Result := TCastleTransform(C)
    else
    if C is TCastleBehavior then
      Result := TCastleBehavior(C).Parent
    else
      Result := nil;
  end;

var
  Selected: TComponentList;
  SelectedCount, I: Integer;
  SelectedBox: TBox3D;
  V: TCastleViewport;
  T: TCastleTransform;
begin
  V := CurrentViewport;
  if V = nil then Exit;

  SelectedBox := TBox3D.Empty;

  GetSelected(Selected, SelectedCount);
  try
    for I := 0 to SelectedCount - 1 do
    begin
      T := TransformFromSelected(Selected[I]);
      if (T <> nil) and
         (T.World = V.Items) then
        SelectedBox := SelectedBox + T.WorldBoundingBox;
    end;
  finally FreeAndNil(Selected) end;

  ViewportViewBox(V, SelectedBox);
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

function TDesignFrame.TreeNodeCaption(const C: TComponent): String;
begin
  Result := C.Name;
end;

procedure TDesignFrame.CastleControlResize(Sender: TObject);
var
  CalculatedUIScale: Single;
  H, CalculatedUIScaleStr: String;
begin
  // trick to get private TCastleContainer.FCalculatedUIScale
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

  function ViewportDebugInfo(const V: TCastleViewport): String;

    function CameraDirectionStr: String;
    begin
      { This detection matches TProjectForm.ActionViewport*Execute methods in CGE editor. }
      if TVector3.Equals(V.InternalCamera.Direction, Vector3(0, -1, 0)) then
        Result := 'Top '
      else
      if TVector3.Equals(V.InternalCamera.Direction, Vector3(0, 1, 0)) then
        Result := 'Bottom '
      else
      if TVector3.Equals(V.InternalCamera.Direction, Vector3(0, 0, -1)) then
        Result := 'Front '
      else
      if TVector3.Equals(V.InternalCamera.Direction, Vector3(0, 0, 1)) then
        Result := 'Back '
      else
      if TVector3.Equals(V.InternalCamera.Direction, Vector3(1, 0, 0)) then
        Result := 'Left '
      else
      if TVector3.Equals(V.InternalCamera.Direction, Vector3(-1, 0, 0)) then
        Result := 'Right '
      else
        Result := '';
    end;

    function ProjectionStr: String;
    begin
      Result := ProjectionTypeToStr(V.InternalCamera.ProjectionType);
    end;

    function DesignNavigationStr: String;
    const
      Names: array [TInternalDesignNavigationType] of String = ('Fly', 'Examine', '2D');
    begin
      if not V.InternalDesignManipulation then
      begin
        WritelnWarning('Viewport not in design mode, but selected (submit a bug): %s', [V.Name]);
        Exit;
      end;
      Result := Names[V.InternalDesignNavigationType];
      if V.InternalDesignNavigationType = dnFly then
        Result := Result + Format(' (speed %f)', [
          (V.InternalDesignNavigation as TCastleWalkNavigation).MoveSpeed
        ]);
    end;

  begin
    Result := V.Name + ': ' + CameraDirectionStr + ProjectionStr + ' ' + NL +
      DesignNavigationStr;
  end;

var
  SavedErrorBox: String;
begin
  { process PendingErrorBox }
  if PendingErrorBox <> '' then
  begin
    SavedErrorBox := PendingErrorBox;
    { Clear PendingErrorBoxthis *before* doing ErrorBox, as on WinAPI,
      the CastleControlUpdate will keep occurring underneath the box,
      and we would spawn ~infinite number of ErrorBox.
      This can happen e.g. in case of invalid CastleSettings.xml file,
      that sets PendingErrorBox. }
    PendingErrorBox := '';
    ErrorBox(SavedErrorBox);
  end;

  if InternalCastleDesignInvalidate then
  begin
    UpdateDesign;
    //WritelnWarning('CGE needed to explicitly tell editor to refresh hierarchy');
    ModifiedOutsideObjectInspector('', ucLow);
  end;

  { Hide some tabs if they don't contain anything. }
  TabLayout.TabVisible :=
    (Inspector[itLayout].RowCount <> 0) or
    PanelLayoutTransform.Visible or
    PanelAnchors.Visible;
  TabEvents.TabVisible :=
    (Inspector[itEvents].RowCount <> 0);

  { If necessary, reset OverrideCursor modified by UpdateCursor, to allow mouse look to hide cursor }
  if InternalDesignMouseLook then
    CastleControl.Container.OverrideCursor := mcDefault;

  UpdateCurrentViewport;

  LabelViewport.Visible := FCurrentViewport <> nil;
  if FCurrentViewport <> nil then
    LabelViewport.Caption := ViewportDebugInfo(FCurrentViewport);
end;

procedure TDesignFrame.CastleControlDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  SourceShellList: TCastleShellListView;
  UI: TCastleUserInterface;
  ParentComponent: TComponent;
begin
  Accept := false;
  if Source is TCastleShellListView then
  begin
    SourceShellList := TCastleShellListView(Source);
    UI := FDesignerLayer.HoverUserInterface(Vector2(X, CastleControl.Height - Y));
    if (UI is TCastleViewport) and not (ssShift in GetKeyShiftState) then
      ParentComponent := TCastleViewport(UI).Items
    else
      ParentComponent := UI;
    if ParentComponent = nil then // may happen because UI was nil
      Exit;

    Accept := ShellListComponentClass(SourceShellList, ParentComponent) <> nil;
  end;
end;

procedure TDesignFrame.CastleControlDragDrop(Sender, Source: TObject; X, Y: Integer);

  { Calculate 3D position of a TCastleTransform created by drag-and-drop on a vieport. }
  function DropPosition(const Viewport: TCastleViewport; out DropPos: TVector3): Boolean;
  var
    RayOrigin, RayDirection: TVector3;
    RayHit: TRayCollision;
    Distance: Single;
    OldPickable: Boolean;
    PlaneZ: Single;
    Cam: TCastleCamera;
  begin
    Result := true;

    Cam := Viewport.InternalCamera;
    if Cam = nil then
    begin
      WritelnWarning('Cannot drop on the viewport %s, as it has no camera (not even a design-time camera)', [
        Viewport.Name
      ]);
      Exit(false);
    end;

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

    if (RayHit = nil) and (Cam.ProjectionType = ptOrthographic) then
    begin
      { In the past, we used to calculate this as
        "(Cam.EffectiveProjectionNear + Cam.EffectiveProjectionFar) / 2".

        But now that we auto-calculate projection, the above "smart" calculation is
        - unnecessary (projection will adjust to new item)
        - often results in weird values (as projection near/far are often "around zero"
          but usually their average is not exactly zero, as users expect zero).
      }
      PlaneZ := 0;
      if not TrySimplePlaneRayIntersection(DropPos, 2, PlaneZ, RayOrigin, RayDirection) then
        Exit(false); // camera direction parallel to 3D plane with Z = constant
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
      DropPos := RayOrigin + (RayDirection * Distance);

      { In case of 2D game move scene a little closser to camera }
      if Cam.ProjectionType = ptOrthographic then
        DropPos := DropPos - Cam.Direction;
    end;
  end;

var
  SourceShellList: TCastleShellListView;
  ParentComponent, NewComponent: TComponent;
  NewComponentClass: TComponentClass;
  UI: TCastleUserInterface;
  Viewport: TCastleViewport;
  DropPosition2D: TVector2;
  DropPos: TVector3;
  Transform: TCastleTransform;
begin
  if Source is TCastleShellListView then
  begin
    SourceShellList := TCastleShellListView(Source);
    DropPosition2D := Vector2(X, CastleControl.Height - Y);

    { calculate ParentComponent and UI }
    if DesignRoot is TCastleTransform then
    begin
      { When we edit .castle-transform design, then the parent to drop must be DesignRoot.
        Not the Viewport, not even Viewport.Items -- only things inside DesignRoot
        are the hierarchy edited by user. }
      ParentComponent := DesignRoot;
      Assert(FDesignViewportForTransforms <> nil);
      UI := FDesignViewportForTransforms;
    end else
    begin
      UI := FDesignerLayer.HoverUserInterface(DropPosition2D);
      if (UI is TCastleViewport) and not (ssShift in GetKeyShiftState) then
        ParentComponent := TCastleViewport(UI).Items
      else
        ParentComponent := UI;
    end;

    if ParentComponent = nil then // may happen because UI was nil
      Exit;

    NewComponentClass := ShellListComponentClass(SourceShellList, ParentComponent);
    if NewComponentClass = nil then
      Exit;

    if NewComponentClass.InheritsFrom(TCastleTransform) then
    begin
      if not (UI is TCastleViewport) then
      begin
        WritelnWarning('Cannot drag-and-drop %s on UI %s', [
          NewComponentClass.ClassName,
          UI.ClassName
        ]);
        Exit;
      end;
      Viewport := TCastleViewport(UI);

      if not DropPosition(Viewport, DropPos) then
      begin
        WritelnWarning('Cannot drag-and-drop %s on viewport, cannot determine drop position', [
          NewComponentClass.ClassName
        ]);
        Exit;
      end;

      { We can assume that ShellListAddComponent creates non-nil,
        and TCastleTransform, because ShellListComponentClass
        returned non-nil TCastleTransform descendant. }
      Transform := ShellListAddComponent(SourceShellList, ParentComponent) as TCastleTransform;
      Transform.Translation := DropPos;
      WritelnLog('Dropped transform %s:%s into viewport %s at translation %s', [
        Transform.Name,
        Transform.ClassName,
        Viewport.Name,
        DropPos.ToString
      ]);
    end else
    if NewComponentClass.InheritsFrom(TCastleUserInterface) then
    begin
      NewComponent := ShellListAddComponent(SourceShellList, ParentComponent);
      if (NewComponent is TCastleUserInterface) and
         (ParentComponent is TCastleUserInterface) then
      begin
        TCastleUserInterface(NewComponent).Translation :=
          TCastleUserInterface(ParentComponent).ContainerToLocalPosition(DropPosition2D, true);
      end;
    end else
    begin
      WritelnWarning('Cannot drag-and-drop %s on UI %s', [
        NewComponentClass.ClassName,
        UI.ClassName
      ]);
    end;
  end;
end;

const
  LoadUiDesign_FileFilters = 'CGE User Interace Design (*.castle-user-interface)|*.castle-user-interface';
  LoadTransformDesign_FileFilters = 'CGE Transform Design (*.castle-transform)|*.castle-transform';

function TDesignFrame.ShellListComponentClass(const SourceShellList: TCastleShellListView;
  const ParentComponent: TComponent): TComponentClass;
var
  SelectedFileName: String;
  SelectedUrl: String;
  PreferTransform: Boolean;
begin
  Result := nil;
  PreferTransform := ParentComponent is TCastleTransform;

  { SourceShellList.Selected may be nil, testcase:
    - open any project (empty from template is OK)
    - create new design using menu item
      (looks like this step is necessary into tricking LCL that we're
      in the middle of drag-and-drop on GTK?)
    - double-click on some design file in data/ by double-clicking
    - mouse over the design -> without this check, would have access violation
      due to TDesignFrame.CastleControlDragOver being called with
      ShellList.Selected = nil. }

  if SourceShellList.Selected <> nil then
  begin
    SelectedFileName := SourceShellList.GetPathFromItem(SourceShellList.Selected);
    SelectedUrl := MaybeUseDataProtocol(FilenameToURISafe(SelectedFileName));

    if LoadImage_FileFilters.Matches(SelectedUrl) then
    begin
      if PreferTransform then
        Result := TCastleImageTransform
      else
        Result := TCastleImageControl;
    end else
    if TFileFilterList.Matches(LoadScene_FileFilters, SelectedUrl) then
      Result := TCastleScene
    else
    if TFileFilterList.Matches(LoadSound_FileFilters, SelectedUrl) then
      Result := TCastleTransform // ShellListAddComponent creates TCastleTransform with TCastleSoundSource behavior
    else
    if TFileFilterList.Matches(LoadUiDesign_FileFilters, SelectedUrl) then
      Result := TCastleDesign
    else
    if TFileFilterList.Matches(LoadTransformDesign_FileFilters, SelectedUrl) then
      Result := TCastleTransformDesign;
  end;
end;

function TDesignFrame.ShellListAddComponent(const SourceShellList: TCastleShellListView;
  const ParentComponent: TComponent): TComponent;

  function AddImageTransform(const Url: String): TCastleImageTransform;
  begin
    Result := AddComponent(ParentComponent, TCastleImageTransform, nil) as TCastleImageTransform;
    Result.Url := Url;
  end;

  function AddImageControl(const Url: String): TCastleImageControl;
  begin
    Result := AddComponent(ParentComponent, TCastleImageControl, nil) as TCastleImageControl;
    Result.Url := Url;
  end;

  function AddScene(const Url: String): TCastleScene;
  begin
    Result := AddComponent(ParentComponent, TCastleScene, nil) as TCastleScene;
    Result.Url := Url;
  end;

  function AddSound(const Url: String): TCastleTransform;
  var
    SoundSource: TCastleSoundSource;
    Sound: TCastleSound;
  begin
    Result := AddComponent(ParentComponent, TCastleTransform, nil) as TCastleTransform;
    SoundSource := AddComponent(Result, TCastleSoundSource, nil) as TCastleSoundSource;
    Sound := AddComponent(SoundSource, TCastleSound, nil) as TCastleSound;
    Sound.Url := Url;
    SoundSource.Sound := Sound;
  end;

  function AddUiDesign(const Url: String): TCastleDesign;
  begin
    Result := AddComponent(ParentComponent, TCastleDesign, nil) as TCastleDesign;
    Result.Url := Url;
  end;

  function AddTransformDesign(const Url: String): TCastleTransformDesign;
  begin
    Result := AddComponent(ParentComponent, TCastleTransformDesign, nil) as TCastleTransformDesign;
    Result.Url := Url;
  end;

var
  SelectedFileName: String;
  SelectedUrl: String;
  PreferTransform: Boolean;
begin
  Result := nil;
  PreferTransform := ParentComponent is TCastleTransform;
  if SourceShellList.Selected <> nil then
  begin
    SelectedFileName := SourceShellList.GetPathFromItem(SourceShellList.Selected);
    SelectedUrl := MaybeUseDataProtocol(FilenameToURISafe(SelectedFileName));

    if LoadImage_FileFilters.Matches(SelectedUrl) then
    begin
      if PreferTransform then
        Result := AddImageTransform(SelectedUrl)
      else
        Result := AddImageControl(SelectedUrl);
    end else
    if TFileFilterList.Matches(LoadScene_FileFilters, SelectedUrl) then
      Result := AddScene(SelectedUrl)
    else
    if TFileFilterList.Matches(LoadSound_FileFilters, SelectedUrl) then
      Result := AddSound(SelectedUrl)
    else
    if TFileFilterList.Matches(LoadUiDesign_FileFilters, SelectedUrl) then
      Result := AddUiDesign(SelectedUrl)
    else
    if TFileFilterList.Matches(LoadTransformDesign_FileFilters, SelectedUrl) then
      Result := AddTransformDesign(SelectedUrl);
  end;
end;

procedure TDesignFrame.InspectorFilter(const Sender: TObject;
  const AEditor: TPropertyEditor; var AShow: Boolean;
  const FilterBySection: Boolean; const Section: TPropertySection);
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
    if Instance is TComponent then
    begin
      { Early exit: never show Name on components not owned by DesignOwner.
        Such components are subcomponents of something,
        like Camera.Perspective, and their Name is
        - shown weird (with dot, like 'Camera1.Perspective')
        - not useful to edit (we don't really want to allow editing it,
          you cannot search for such component later with DesignedComponent
          anyway, and the name must be unique within the owner -- better
          to leave it unedited) }
      if (PropertyName = 'Name') and
         (TComponent(Instance).Owner <> DesignOwner) then
        Exit;

      if FilterBySection and (Instance is TCastleComponent) then
      begin
        AShow := Section in TCastleComponent(Instance).PropertySections(PropertyName);
      end else
      begin
        AShow := true;
      end;
    end else
    begin
      { Show=true when Instance is some non-TComponent class used for subcomponents,
        like TBorder, TCastleImagePersistent... }
      AShow := true;
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
  InspectorFilter(Sender, AEditor, AShow, true, psBasic);
end;

procedure TDesignFrame.InspectorLayoutFilter(Sender: TObject;
  AEditor: TPropertyEditor; var aShow: Boolean);
begin
  InspectorFilter(Sender, AEditor, AShow, true, psLayout);
end;

procedure TDesignFrame.InspectorAllFilter(Sender: TObject;
  AEditor: TPropertyEditor; var aShow: Boolean);
begin
  InspectorFilter(Sender, AEditor, AShow, false, {Section doesn't matter here}psBasic);
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

  { Do a subset of work that UpdateSelectedControl also does:
    update UI to reflect the current state of selected object,
    when the properties of the selected object possibly changed
    (but the selected object stays the same -- otherwise
    UpdateSelectedControl would do the full job.)

    It also updates ControlsTree,
    in case you changed the TComponent.Name. }
  procedure UpdateSelectedControlModified;
  var
    Sel: TComponent;
    SelUI: TCastleUserInterface;
    SelNode: TTreeNode;
  begin
    // This checks we have selected *exactly one* component.
    Sel := SelectedComponent;

    if Sel <> nil then
    begin
      // update also LabelControlSelected
      LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Sel);

      // update also LabelSizeInfo
      if Sel is TCastleUserInterface then
      begin
        SelUI := Sel as TCastleUserInterface;
        UpdateLabelSizeInfo(SelUI);
        UpdateAnchors(SelUI, true);
      end;

      { Note that we use TreeNodeMap to find SelNode,
        not just assume that SelNode := ControlsTree.Selected.
        That is because in case of special tree items "Behaviors" or "Non-Visual Components",
        the ControlsTree.Selected could be different. }
      if TreeNodeMap.TryGetValue(Sel, SelNode) then
        SelNode.Text := TreeNodeCaption(Sel);
    end;
  end;

  procedure DoRecordUndo;
  var
    Selected: TComponentList;
    Sel: TComponent;
    SelectedCount: Integer;
  begin
    GetSelected(Selected, SelectedCount);
    try
      if SelectedCount = 0 then
      begin
        // Something has changed, but we don't know what exactly. Let's rebuild everything to be safe.
        UpdateDesign;
        RecordUndo('', ucLow); // We're recording a generic Undo message
      end else
      begin
        if SelectedCount = 1 then
          Sel := Selected[0]
        else
          Sel := nil;

        if Sender is TOICustomPropertyGrid then
        begin
          RecordUndo(
            UndoMessageModified(Sel, TOICustomPropertyGrid(Sender).GetActiveRow.Name,
            TOICustomPropertyGrid(Sender).CurrentEditValue, SelectedCount),
            ucHigh, TOICustomPropertyGrid(Sender).ItemIndex);
        end else
        { Sender is nil when PropertyGridModified is called
          by ModifiedOutsideObjectInspector. }
        if Sel <> nil then // so SelectedCount = 1
          RecordUndo('Change ' + Sel.Name, ucLow)
        else
        begin
          // we handled SelectedCount = 0 or 1 above
          Assert(SelectedCount > 1);
          RecordUndo('Change multiple components', ucLow)
        end;
      end;
    finally FreeAndNil(Selected) end;
  end;

begin
  UpdateSelectedControlModified;

  { When UndoSystem.ScheduleRecordUndoOnRelease we ignore changes,
    otherwise we would record an undo for every OnMotion of dragging. }
  if not UndoSystem.ScheduleRecordUndoOnRelease then
    DoRecordUndo;

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

    { Need to set modified flag.
      PropertyGridModified also does this, but not everything causes PropertyGridModified,
      e.g. setting URL property (done by TPropertyEditor.SetStrValue that calls
      TPropertyEditor.Modified) only results in PropertyEditorModified call. }
    MarkModified;
  end else
    raise EInternalError.Create('PropertyEditorModified can only be called with TPropertyEditor as a Sender.');
end;

procedure TDesignFrame.PropertyGridCollectionItemClick(Sender: TObject);
var
  SelectionForOI: TPersistentSelectionList;
  InspectorType: TInspectorType;
  Ed: TCollectionPropertyEditorForm;
  ListBox: TListBox;
begin
  SelectionForOI := TPersistentSelectionList.Create;
  try
    ListBox := Sender as TListBox;
    if ListBox.ItemIndex >= 0 then
    begin
      Ed := ListBox.Parent as TCollectionPropertyEditorForm;
      SelectionForOI.Add(Ed.Collection.Items[ListBox.ItemIndex]);
      for InspectorType in TInspectorType do
        Inspector[InspectorType].Selection := SelectionForOI;
    end;
  finally FreeAndNil(SelectionForOI) end;
end;

procedure TDesignFrame.PropertyGridCollectionItemClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UpdateSelectedControl;
end;

procedure TDesignFrame.PropertyGridCollectionItemAdd(Sender: TObject);
begin
  ((Sender as TToolButton).Parent.Parent as TCollectionPropertyEditorForm).actAddExecute(Sender);
  RecordUndo('Add item', ucLow);
end;

procedure TDesignFrame.PropertyGridCollectionItemDelete(Sender: TObject);
begin
  ((Sender as TToolButton).Parent.Parent as TCollectionPropertyEditorForm).actDelExecute(Sender);
  RecordUndo('Delete item', ucLow);
end;

procedure TDesignFrame.PropertyGridCollectionItemMoveUp(Sender: TObject);
var
  FakeSender: TComponent;
begin
  FakeSender := TComponent.Create(nil);
  try
    { This is a weird decision. It depends on sender's name to determine if
      it should move item up or move item down }
    FakeSender.Name := 'actMoveUp';
    ((Sender as TToolButton).Parent.Parent as TCollectionPropertyEditorForm).actMoveUpDownExecute(FakeSender);
    RecordUndo('Move item up', ucLow);
  finally
    FreeAndNil(FakeSender);
  end;
end;

procedure TDesignFrame.PropertyGridCollectionItemMoveDown(Sender: TObject);
var
  FakeSender: TComponent;
begin
  FakeSender := TComponent.Create(nil);
  try
    { This is a weird decision. It depends on sender's name to determine if
      it should move item up or move item down }
    FakeSender.Name := 'actMoveDown';
    ((Sender as TToolButton).Parent.Parent as TCollectionPropertyEditorForm).actMoveUpDownExecute(FakeSender);
    RecordUndo('Move item down', ucLow);
  finally
    FreeAndNil(FakeSender);
  end;
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
  UndoSystem.DoLog('Undo "%s" recorded in %fs for "%s".', [UndoComment, StartTimer.ElapsedTime, SelectedName]);
end;

procedure TDesignFrame.SaveSelected;
var
  ComponentToSave: TComponent;
begin
  ComponentToSave := SelectedComponent;
  if ComponentToSave = nil then
  begin
    ErrorBox('Select exactly one component to save');
    Exit;
  end;

  PrepareSaveDesignDialog(SaveDesignDialog, ComponentToSave);
  SaveDesignDialog.Url := '';
  if SaveDesignDialog.Execute then
  begin
    if ComponentToSave is TCastleUserInterface then
      UserInterfaceSave(TCastleUserInterface(ComponentToSave), SaveDesignDialog.Url)
    else
    if ComponentToSave is TCastleTransform then
      TransformSave(TCastleTransform(ComponentToSave), SaveDesignDialog.Url)
    else
      ComponentSave(ComponentToSave, SaveDesignDialog.Url);
  end;
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
  Result := (not (csTransient in Child.ComponentStyle)) or (Child is TTemporaryJointTransform);
end;

function TDesignFrame.Deletable(const Child: TComponent): Boolean;
begin
  Result := Selectable(Child) and
    (not (csSubComponent in Child.ComponentStyle)) and
    (Child <> DesignRoot) and (not (Child is TTemporaryJointTransform));
end;

type
  EHierarchyValidationFailed = class(Exception);
const
  ValidationError = 'Hierarchy view desynchronized with CGE internal hierarchy. 1. Please submit a bug, this should never happen. 2. To workaround it for now, save and reopen the design file.';

procedure TDesignFrame.ValidateOrUpdateHierarchy(const Validate: Boolean);
var
  NodesToExpand: TObjectList;

  { Make sure that given parent, at given index, has given child.
    Return this child (TTreeNode),
    and increase IndexInParent by 1.

    Parent may be nil, indicating the root of ControlsTree.
    (It's a bit uncomfortable, but there's no single "root" node for
    ControlsTree. At the top level there is TTreeNodes, which is already
    a list.)

    In case of Validate = false: if there is a mismatch,
    it fixes the mismatch by adding a new child.
    But it assumes that Parent has at least IndexInParent-1 children,
    i.e. all previous children (with smaller IndexInParent) are matching.

    In case of Validate = true: if there is a mismatch,
    raises EHierarchyValidationFailed. }
  function EnsureChildNodeIs(const Parent: TTreeNode; var IndexInParent: Integer;
    const ChildNodeCaption: String; const ChildNodeData: Pointer): TTreeNode;
  var
    Valid, NextIsValid: Boolean;
  begin
    if Parent = nil then
    begin
      Assert(ControlsTree.Items.TopLvlCount >= IndexInParent);
      Valid := (ControlsTree.Items.TopLvlCount > IndexInParent) and
        (ControlsTree.Items.TopLvlItems[IndexInParent].Text = ChildNodeCaption) and
        (ControlsTree.Items.TopLvlItems[IndexInParent].Data = ChildNodeData);
      NextIsValid := (ControlsTree.Items.TopLvlCount > IndexInParent + 1) and
        (ControlsTree.Items.TopLvlItems[IndexInParent + 1].Text = ChildNodeCaption) and
        (ControlsTree.Items.TopLvlItems[IndexInParent + 1].Data = ChildNodeData);
    end else
    begin
      Assert(Parent.Count >= IndexInParent);
      Valid := (Parent.Count > IndexInParent) and
        (Parent.Items[IndexInParent].Text = ChildNodeCaption) and
        (Parent.Items[IndexInParent].Data = ChildNodeData);
      NextIsValid := (Parent.Count > IndexInParent + 1) and
        (Parent.Items[IndexInParent + 1].Text = ChildNodeCaption) and
        (Parent.Items[IndexInParent + 1].Data = ChildNodeData);
    end;

    // observe this log after delete/add operation
    {
    WritelnLog('Child node %s valid? %s', [
      ChildNodeCaption,
      BoolToStr(Valid, true)
    ]);
    }

    if Valid then
    begin
      if Parent = nil then
        Result := ControlsTree.Items.TopLvlItems[IndexInParent]
      else
        Result := Parent.Items[IndexInParent];
    end else
    begin
      if Validate then
      begin
        raise EHierarchyValidationFailed.Create(ValidationError);
      end;

      // now we know Validate = false

      if NextIsValid then
      begin
        // delete the IndexInParent child of Parent and we will be OK
        if Parent = nil then
        begin
          ControlsTree.Items.Delete(ControlsTree.Items.TopLvlItems[IndexInParent]);
          Result := ControlsTree.Items.TopLvlItems[IndexInParent];
        end else
        begin
          ControlsTree.Items.Delete(Parent.Items[IndexInParent]);
          Result := Parent.Items[IndexInParent];
        end;
      end else
      begin
        { insert new child at position IndexInParent to Parent.

          Looks like TTreeNode doesn't give nice API to Insert at specified
          position into an indicated parent. But we can just remove
          all excessive nodes -- they are all likely invalid at this point. }

        if Parent = nil then
        begin
          while ControlsTree.Items.TopLvlCount > IndexInParent do
            ControlsTree.Items.Delete(ControlsTree.Items.TopLvlItems[IndexInParent]);
          Assert(IndexInParent = ControlsTree.Items.TopLvlCount); // this should be true if previous children were valid
        end else
        begin
          while Parent.Count > IndexInParent do
            ControlsTree.Items.Delete(Parent.Items[IndexInParent]);
          Assert(IndexInParent = Parent.Count); // this should be true if previous children were valid
        end;

        // this works for Parent = nil or non-nil
        Result := ControlsTree.Items.AddChildObject(Parent, ChildNodeCaption, ChildNodeData);

        { Expand newly added nodes.
          Note that we cannot call now Result.Expand(true),
          because Result has no children and calling Expand() on it does nothing. }
        if not Validate then
          NodesToExpand.Add(Result);
      end;
    end;

    Assert(Result.Text = ChildNodeCaption);
    Assert(Result.Data = ChildNodeData);
    Inc(IndexInParent);
  end;

  { Make sure given Parent node has *at most* ParentCount children.

    In case of Validate = false: if there is a mismatch,
    it removes excessive children.

    In case of Validate = true: if there is a mismatch,
    raises EHierarchyValidationFailed. }
  procedure EnsureChildrenNodesCount(const Parent: TTreeNode; const ParentCount: Integer);
  var
    Valid: Boolean;
  begin
    if Parent = nil then
    begin
      Valid := ControlsTree.Items.TopLvlCount <= ParentCount;
    end else
    begin
      Valid := Parent.Count <= ParentCount;
    end;

    if not Valid then
    begin
      if Validate then
      begin
        raise EHierarchyValidationFailed.Create(ValidationError);
      end;

      // now we know Validate = false

      if Parent = nil then
      begin
        while ControlsTree.Items.TopLvlCount > ParentCount do
          ControlsTree.Items.Delete(ControlsTree.Items.TopLvlItems[ParentCount]);
      end else
      begin
        while Parent.Count > ParentCount do
          ControlsTree.Items.Delete(Parent.Items[ParentCount]);
      end;
    end;
  end;

  { Update TreeNodeMap, mapping components -> to their corresponding tree node.
    Ignored when Validate = false. }
  procedure UpdateTreeNodeMap(const C: TComponent; const Node: TTreeNode);
  begin
    if not Validate then
      TreeNodeMap.AddOrSetValue(C, Node);
  end;

  { Add given component, and its children in C.NonVisualComponents }
  function AddNonVisualComponent(const Parent: TTreeNode; var IndexInParent: Integer; const C: TComponent): TTreeNode;
  var
    S: String;
    Child: TComponent;
    ChildrenNodesCount: Integer;
  begin
    S := TreeNodeCaption(C);
    Result := EnsureChildNodeIs(Parent, IndexInParent, S, C);
    UpdateTreeNodeMap(C, Result);

    if C is TCastleComponent then
    begin
      ChildrenNodesCount := 0;
      for Child in TCastleComponent(C).NonVisualComponentsEnumerate do
        if Selectable(Child) then
          AddNonVisualComponent(Result, ChildrenNodesCount, Child);
      EnsureChildrenNodesCount(Result, ChildrenNodesCount);
    end;
  end;

  { If C has some NonVisualComponents, then create a tree item
    'Non-Visual Components' and add them to it. }
  function AddNonVisualComponentsSection(const Parent: TTreeNode; var IndexInParent: Integer; const C: TCastleComponent): TTreeNode;
  var
    Child: TComponent;
    ChildrenNodesCount: Integer;
  begin
    if C.NonVisualComponentsCount <> 0 then
    begin
      Result := EnsureChildNodeIs(Parent, IndexInParent, 'Non-Visual Components', nil);

      ChildrenNodesCount := 0;
      for Child in C.NonVisualComponentsEnumerate do
        if Selectable(Child) then
          AddNonVisualComponent(Result, ChildrenNodesCount, Child);
      EnsureChildrenNodesCount(Result, ChildrenNodesCount);
    end;
  end;

  { If T has some Behaviors, then create a tree item
    'Behaviors' and add them to it. }
  function AddBehaviorsSection(const Parent: TTreeNode; var IndexInParent: Integer; const T: TCastleTransform): TTreeNode;
  var
    Child: TCastleBehavior;
    ChildrenNodesCount: Integer;
  begin
    if T.BehaviorsCount <> 0 then
    begin
      Result := EnsureChildNodeIs(Parent, IndexInParent, 'Behaviors', nil);

      ChildrenNodesCount := 0;
      for Child in T.BehaviorsEnumerate do
        if Selectable(Child) then
          AddNonVisualComponent(Result, ChildrenNodesCount, Child);
      EnsureChildrenNodesCount(Result, ChildrenNodesCount);
    end;
  end;

  { Add given transform, and its children
    (transform children, T.NonVisualComponents, T.Behaviors). }
  function AddTransform(const Parent: TTreeNode; var IndexInParent: Integer; const T: TCastleTransform): TTreeNode;
  var
    S: String;
    I, ChildrenNodesCount: Integer;
  begin
    S := TreeNodeCaption(T);
    Result := EnsureChildNodeIs(Parent, IndexInParent, S, T);
    UpdateTreeNodeMap(T, Result);

    ChildrenNodesCount := 0;
    AddNonVisualComponentsSection(Result, ChildrenNodesCount, T);
    AddBehaviorsSection(Result, ChildrenNodesCount, T);

    for I := 0 to T.Count - 1 do
      if Selectable(T[I]) then
        AddTransform(Result, ChildrenNodesCount, T[I]);

    EnsureChildrenNodesCount(Result, ChildrenNodesCount);
  end;

  { Add given UI control, and its children. }
  function AddControl(const Parent: TTreeNode; var IndexInParent: Integer; const C: TCastleUserInterface): TTreeNode;
  var
    S: String;
    I, ChildrenNodesCount: Integer;
    Viewport: TCastleViewport;
  begin
    S := TreeNodeCaption(C);
    Result := EnsureChildNodeIs(Parent, IndexInParent, S, C);
    UpdateTreeNodeMap(C, Result);

    ChildrenNodesCount := 0;
    AddNonVisualComponentsSection(Result, ChildrenNodesCount, C);

    for I := 0 to C.ControlsCount - 1 do
    begin
      if Selectable(C.Controls[I]) then
        AddControl(Result, ChildrenNodesCount, C.Controls[I]);
    end;

    if C is TCastleViewport then
    begin
      Viewport := TCastleViewport(C);
      if Selectable(Viewport.Items) then
        AddTransform(Result, ChildrenNodesCount, Viewport.Items);
    end;
    EnsureChildrenNodesCount(Result, ChildrenNodesCount);
  end;

var
  ChildrenNodesCount: Integer;
  O: Pointer;
begin
  if not Validate then
    NodesToExpand := TObjectList.Create(false)
  else
    NodesToExpand := nil;
  try
    ChildrenNodesCount := 0;
    if DesignRoot is TCastleUserInterface then
      AddControl(nil, ChildrenNodesCount, DesignRoot as TCastleUserInterface)
    else
    if DesignRoot is TCastleTransform then
      AddTransform(nil, ChildrenNodesCount, DesignRoot as TCastleTransform)
    else
      AddNonVisualComponent(nil, ChildrenNodesCount, DesignRoot);
    EnsureChildrenNodesCount(nil, ChildrenNodesCount);

    if not Validate then
    begin
      for O in NodesToExpand do
        TTreeNode(O).Expand(false); // no need for recurse=true, because we will call this on children too
    end;
  finally
    FreeAndNil(NodesToExpand);
  end;
end;

procedure TDesignFrame.UpdateDesign;
begin
  // temporarily disable events, as some pointers in ControlsTree data are invalid now
  ControlsTree.OnSelectionChanged := nil;
  ControlsTree.OnChanging := nil;

  ControlsTree.Selected := nil; // TODO: for now we reset selection, though maybe we could preserve it

  TreeNodeMap.Clear; // ValidateOrUpdateHierarchy(false) will fill TreeNodeMap

  ValidateOrUpdateHierarchy(false);

  // restore events
  ControlsTree.OnChanging := @ControlsTreeBeforeSelectionChange;
  ControlsTree.OnSelectionChanged := @ControlsTreeSelectionChanged;

  UpdateSelectedControl;

  InternalCastleDesignInvalidate := false;
end;

function TDesignFrame.ValidateHierarchy: Boolean;
begin
  try
    ValidateOrUpdateHierarchy(true);
    Result := true;
  except
    on E: EHierarchyValidationFailed do
    begin
      Result := false;
      WritelnWarning(E.Message);
    end;
  end;
end;

function TDesignFrame.SelectedFromNode(Node: TTreeNode): TComponent;
begin
  // This should never be called with Node = nil
  Assert(Node <> nil);

  { In case of special tree items "Behaviors" or "Non-Visual Components",
    treat parent as selected. }
  if Node.Data = nil then
  begin
    Node := Node.Parent;
    if Node = nil then
    begin
      WritelnWarning('No parent of special node (without the associated TComponent), this should not happen');
      Exit(nil);
    end;
    if Node.Data = nil then
    begin
      WritelnWarning('Parent of special node (without the associated TComponent) also has no associated TComponent, this should not happen');
      Exit(nil);
    end;
  end;

  Assert(Node.Data <> nil);
  Assert(TObject(Node.Data) is TComponent); // we only store nil or TComponent instance in Node.Data
  Result := TComponent(Node.Data);
end;

procedure TDesignFrame.GetSelected(out Selected: TComponentList;
  out SelectedCount: Integer);

{ This implementation is synchronized with GetSelectedComponent closely,
  as GetSelectedComponent needs to do something similar. }

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
      { Avoid duplicates that could occur if you select a component
        and special tree item "Behaviors" or "Non-Visual Components" within. }
      if Selected.IndexOf(C) = -1 then
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

{ This implementation is synchronized with GetSelected closely.

  Naive version (using GetSelected to create TComponentList,
  that also registers notifications -- so it has some cost) is defined below,
  and you can use Assert to make sure it is equal to the optimized one.
  But it is commented out now, as the editor just uses debug mode now always,
  for simplicity -- as there are no practical points when it needs optimization.
  *This* optimization is also not practical, i.e. not proven by any test. }

{
  function GetSelectedComponentNaive: TComponent;
  var
    Selected: TComponentList;
    SelectedCount: Integer;
  begin
    GetSelected(Selected, SelectedCount);
    try
      if SelectedCount = 1 then
        Result := Selected[0]
      else
        Result := nil;
    finally FreeAndNil(Selected) end;
  end;
}

var
  I: Integer;
  C: TComponent;
begin
  Result := nil;

  for I := 0 to ControlsTree.SelectionCount - 1 do
  begin
    C := SelectedFromNode(ControlsTree.Selections[I]);
    if C <> nil then
    begin
      if Result <> nil then
      begin
        if Result <> C then
        begin
          { more than one component selected -> exit nil }
          // Assert(nil = GetSelectedComponentNaive);
          Exit(nil);
        end;
        { else C is the same thing as already selected
          (possible when you select both the component and its special child like "Behaviors"),
          so do nothing }
      end else
        { at least one component is selected }
        Result := C;
    end;
  end;

  // Assert(Result = GetSelectedComponentNaive);
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

procedure TDesignFrame.CheckBehaviorsStillSelected;
var
  SelectedComponents: TComponentList;
  SelectedCount: Integer;
  I: Integer;
  B: TCastleBehavior;
  Index: Integer;
  BehaviorCount: Integer;
begin
  BehaviorCount := FSelectionStartBehaviorList.Count;
  if BehaviorCount = 0 then
    Exit;

  GetSelected(SelectedComponents, SelectedCount);

  { If behavior is not on selected list call InternalSelectionEnd and
    remove it from FSelectionStartBehaviorList }
  for I := BehaviorCount - 1 downto 0 do
  begin
    B := FSelectionStartBehaviorList[I] as TCastleBehavior;
    if SelectedComponents <> nil then
    begin
      Index := SelectedComponents.IndexOf(B);
      if Index < 0 then
        DoInternalSelectionEnd(B);
    end
    else
      DoInternalSelectionEnd(B);
  end;
end;

procedure TDesignFrame.DoInternalSelectionStart(const Behavior: TCastleBehavior;
  const TransformsToSynchronize: TCastleTransformList);
begin
  Behavior.InternalSelectionStart(TransformsToSynchronize);

  { Do not allow duplicates.

    Reason: TODO: not understood to the end, but testcase:

    To any TCastleTransform, add 2 behaviors (like TCastleRigidBody
    and TCastleXxxCollider), select 2nd one and Delete, select the 1st
    one, reopen design by double-clicking (do not save it), press "End" key. }

  if FSelectionStartBehaviorList.IndexOf(Behavior) = -1 then
    FSelectionStartBehaviorList.Add(Behavior);
end;

procedure TDesignFrame.DoInternalSelectionEnd(const Behavior: TCastleBehavior);
begin
  Behavior.InternalSelectionEnd;
  FSelectionStartBehaviorList.Remove(Behavior);
end;

procedure TDesignFrame.DoAllInternalSelectionEnd;
var
  I: Integer;
begin
  for I := FSelectionStartBehaviorList.Count - 1 downto 0 do
    DoInternalSelectionEnd(FSelectionStartBehaviorList[I] as TCastleBehavior);
end;

procedure TDesignFrame.RemoveJointsAnchors;
var
  Item: {$ifdef FPC} TTreeNodeMap.TDictionaryPair {$else} TPair<TComponent, TTreeNode> {$endif};
  TransformsToSynchronize: TCastleTransformList;
begin
  TransformsToSynchronize := TCastleTransformList.Create(false);
  try
    for Item in TreeNodeMap do
    begin
      if Item.Key is TAbstractJoint then
      begin
        (Item.Key as TAbstractJoint).RemoveAuxiliaryEditorUi(TransformsToSynchronize);

        if TransformsToSynchronize.Count  > 0 then
        SynchronizeListOfTransforms(TransformsToSynchronize);
        TransformsToSynchronize.Clear;
      end;
    end;
  finally
    FreeAndNil(TransformsToSynchronize);
  end;
end;

procedure TDesignFrame.SynchronizeTreeNodeChildTransforms(ParentNode: TTreeNode);
var
  C, ChildComponent: TComponent;
  ParentTransform, ChildTransform: TCastleTransform;
  ChildNode: TTreeNode;
  I, J: Integer;
  Title: String;
  Found: Boolean;
begin
  C := TComponent(ParentNode.Data);
  if C = nil then
    Exit;

  if C is TCastleTransform then
  begin
    ParentTransform := TCastleTransform(C);

    // remove elements
    for I := ParentNode.Count - 1 downto 0 do
    begin
      ChildNode := ParentNode.Items[I];


      WritelnLog('ChildNode ' + ChildNode.Text);

      if ChildNode.Data = nil then
        continue;

      ChildComponent := TComponent(ChildNode.Data);


      { if ChildComponent is TCastleTransform then -  weacnt do that because
        some pointers can be dangling pointers - we know here that with
        Data are only transforms }
      Found := false;

      for J := ParentTransform.Count - 1 downto 0 do
      begin
        if ParentTransform.Items[J] = ChildComponent then
        begin
          Found := true;
          break;
        end;
      end;

      if not Found then
      begin
        { remove tree node if not found }
        TreeNodeMap.Remove(ChildComponent);

        ChildNode.Data := nil; { Needed because selection is changed when we
                                 remove ChildNode and removed component with
                                 dangling pointer is here. So trying access it
                                 ends with crash }
        ChildNode.Delete;
      end;
    end;

    { add elements }
    for I := 0 to ParentTransform.Count -1 do
    begin
      ChildTransform := ParentTransform.Items[I];
      if not Selectable(ChildTransform) then
        continue;

      if not TreeNodeMap.TryGetValue(ChildTransform, ChildNode) then
      begin
        // add TreeNode
        Title := TreeNodeCaption(ChildTransform);
        ChildNode := ControlsTree.Items.AddChildObject(ParentNode, Title, ChildTransform);
        TreeNodeMap.AddOrSetValue(ChildTransform, ChildNode);

        // add sub items
        SynchronizeTreeNodeChildTransforms(ChildNode);
      end;
    end;
  end;
end;

procedure TDesignFrame.SynchronizeListOfTransforms(
  const TranformsList: TCastleTransformList);
var
  Transform: TCastleTransform;
  ParentNode: TTreeNode;
  I: Integer;
begin
  for I := 0 to TranformsList.Count - 1 do
  begin
    Transform := TranformsList[I];
    if TreeNodeMap.TryGetValue(Transform, ParentNode) then
      SynchronizeTreeNodeChildTransforms(ParentNode);
  end;
end;

procedure TDesignFrame.UpdateSelectedControl;

  procedure InitializeCollectionFormEvents(InspectorType: TInspectorType);
  var
    I: Integer;
    Row: TOIPropertyGridRow;
    Ed: TCollectionPropertyEditor = nil;
  begin
    if CollectionPropertyEditorForm = nil then
    begin
      { If there is any property that can have a TCollectionPropertyEditor,
        then (once for the whole lifetime of this TDesignFrame) we need to assign
        our callbacks to the associated TCollectionPropertyEditorForm .
        *Before* the form can be actually invoked by user pressing "..." button
        near the respective field.

        The TCollectionPropertyEditorForm form instance is internal in LCL,
        it is reused by all property editors and it stays constant
        for the rest of the application's lifetime.
        The code below detects if there's *any* field with TCollectionPropertyEditor,
        and if yes -- creates (and immediately closes) the associated
        TCollectionPropertyEditorForm, just to initialize our callbacks.

        Example field: TDbf.FieldDefs, test with
        https://github.com/castle-engine/castle-db-aware-controls }

      for I := 0 to Inspector[InspectorType].RowCount - 1 do
      begin
        Row := Inspector[InspectorType].Rows[I];
        if Row.Editor is TCollectionPropertyEditor then
        begin
          Ed := TCollectionPropertyEditor(Row.Editor);
          Break;
        end;
      end;
      if Ed <> nil then
      begin
        CollectionPropertyEditorForm := Ed.ShowCollectionEditor(nil, nil, '') as TCollectionPropertyEditorForm;
        CollectionPropertyEditorForm.FreeNotification(Self);
        CollectionPropertyEditorForm.Close; // Hide the form
        CollectionPropertyEditorForm.OnClose := @PropertyGridCollectionItemClose;
        CollectionPropertyEditorForm.FormStyle := fsStayOnTop;
        CollectionPropertyEditorForm.CollectionListBox.OnClick := @PropertyGridCollectionItemClick;
        { We remove TToolButton's actions and use our own's OnClick events
          instead so that we can hook our undo/redo system in }
        CollectionPropertyEditorForm.AddButton.Action := nil;
        CollectionPropertyEditorForm.AddButton.Enabled := True;
        CollectionPropertyEditorForm.DeleteButton.Action := nil;
        CollectionPropertyEditorForm.DeleteButton.Enabled := True;
        CollectionPropertyEditorForm.MoveUpButton.Action := nil;
        CollectionPropertyEditorForm.MoveUpButton.Enabled := True;
        CollectionPropertyEditorForm.MoveDownButton.Action := nil;
        CollectionPropertyEditorForm.MoveDownButton.Enabled := True;
        CollectionPropertyEditorForm.AddButton.OnClick := @PropertyGridCollectionItemAdd;
        CollectionPropertyEditorForm.DeleteButton.OnClick := @PropertyGridCollectionItemDelete;
        CollectionPropertyEditorForm.MoveUpButton.OnClick := @PropertyGridCollectionItemMoveUp;
        CollectionPropertyEditorForm.MoveDownButton.OnClick := @PropertyGridCollectionItemMoveDown;
      end;
    end;
  end;

var
  Selected: TComponentList;
  I, SelectedCount: Integer;
  SelectionForOI: TPersistentSelectionList;
  UI: TCastleUserInterface;
  InspectorType: TInspectorType;
  V: TCastleViewport;
  T: TCastleTransform;
  ParentNode: TTreeNode;
  TransformList: TCastleTransformList;
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

      { Inspector itAll includes all fields from all inspectors, always.
        So there's no reason to run InitializeCollectionFormEvents on other itXxx inspectors. }
      InitializeCollectionFormEvents(itAll);
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
  if SelectedComponent is TCastleBehavior then
    { Highlight using VisualizeTransformSelected also transformation of selected behavior }
    T := TCastleBehavior(SelectedComponent).Parent
  else
    T := SelectedTransform;
  SetEnabledVisible(PanelLayoutTransform, T <> nil);
  VisualizeTransformSelected.Parent := T; // works also in case SelectedTransform is nil

  if CameraPreview <> nil then
    CameraPreview.SelectedChanged(T, V);

  { if selection determines CurrentViewport, update CurrentViewport immediately
    (without waiting for OnUpdate) -- maybe this will be relevant at some point }
  UpdateCurrentViewport;

  { Call InternalSelectionStart when SelectedComponent is a behavior. I think
    this should be done on end of this function }
  if SelectedComponent is TCastleBehavior then
  begin
    TransformList := TCastleTransformList.Create(false);
    try
      if TCastleBehavior(SelectedComponent) is TAbstractJoint then
        RemoveJointsAnchors;

      DoInternalSelectionStart(TCastleBehavior(SelectedComponent), TransformList);

      for I := 0 to TransformList.Count - 1 do
      begin
        if TreeNodeMap.TryGetValue(TransformList[I], ParentNode) then
          SynchronizeTreeNodeChildTransforms(ParentNode);
      end;
    finally FreeAndNil(TransformList) end;
  end;
end;

procedure TDesignFrame.ControlsTreeSelectionChanged(Sender: TObject);
begin
  UpdateSelectedControl;
end;

procedure TDesignFrame.ControlsTreeBeforeSelectionChange(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  CheckBehaviorsStillSelected;
  AllowChange := true;
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
  Accept := false;
  if Source = ControlsTree then
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
  end else
  if Source is TCastleShellListView then
  begin
    if SelectedComponent <> nil then
      Accept := ShellListComponentClass(TCastleShellListView(Source),
        SelectedComponent) <> nil;
  end;
end;

procedure TDesignFrame.ControlsTreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
  C: TComponent;
begin
  { This event is fired when calling TCustomListView.CanEdit
    which itself is called in TCustomListView.ShowEditor
    therefore this event preceeds initializing and showing of the editor.

    Here we have to "restore" the pure name of the component (without class name)
    before starting edit. }
  C := TComponent(Node.Data);
  AllowEdit := C <> nil; // may be nil on special tree items "Behaviors" or "Non-Visual Components"
  if AllowEdit then
    Node.Text := C.Name;
end;

procedure TDesignFrame.ControlsTreeEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
var
  UndoComment: String;
  Sel: TComponent;
begin
  try
    Sel := TComponent(Node.Data);
    if Sel = nil then
    begin
      // may be nil on special tree items "Behaviors" or "Non-Visual Components"
      WritelnWarning('Node data is nil at ControlsTreeEditingEnd, this should not happen');
      Exit;
    end;

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
    Node.Text := TreeNodeCaption(Sel);
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
    // SrcComponent is nil if you try to drag special tree items "Behaviors" or "Non-Visual Components"
    if (SrcComponent <> nil) and
       ( (SrcComponent = DesignRoot) or
         (csSubComponent in SrcComponent.ComponentStyle) ) then
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
    UI.Translation := TVector2.Zero;
  end else
  begin
    UI.Translation := UI.Translation + Vector2(
      (RenderRectBeforeChange.Left   - NewRect.Left) / UI.UIScale,
      (RenderRectBeforeChange.Bottom - NewRect.Bottom) / UI.UIScale);
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
  { Note: do not check GetSelectedComponent <> nil,
    as a component may also be selected indirectly by selecting
    special tree items "Behaviors" or "Non-Visual Components" underneath.
    For rename, it has to be selected directly. }
  Result :=
    (ControlsTree.SelectionCount = 1) and
    (ControlsTree.Selections[0].Data <> nil);
end;

procedure TDesignFrame.RenameSelectedItem;
begin
  if RenamePossible then
    ControlsTree.Selected.EditText;
end;

procedure TDesignFrame.ControlsTreeDragDrop(Sender, Source: TObject; X,
  Y: Integer);

  procedure MoveOnlyTreeNodes; forward;

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
            MoveOnlyTreeNodes;
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
            MoveOnlyTreeNodes;
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
            if Src.Parent <> nil then
              Src.Parent.Remove(Src);
            Dst.Add(Src);
            MoveOnlyTreeNodes;
          end;
        end;
      tnsBottom, tnsTop:
        begin
          if (Dst.Parent <> nil) and
             not ContainsRecursive(Src, Dst.Parent) then
          begin
            if Src.Parent <> nil then
              Src.Parent.Remove(Src);
            Index := Dst.Parent.List.IndexOf(Dst);
            Assert(Index <> -1);
            if ControlsTreeNodeUnderMouseSide = tnsBottom then
              Inc(Index);
            Dst.Parent.Insert(Index, Src);
            MoveOnlyTreeNodes;
          end;
        end;
      else raise EInternalError.Create('ControlsTreeDragDrop:ControlsTreeNodeUnderMouseSide?');
    end;
  end;

  procedure MoveBehavior(const Src: TCastleBehavior; const Dst: TCastleTransform);
  begin
    case ControlsTreeNodeUnderMouseSide of
      tnsRight:
        begin
          Src.Parent.RemoveBehavior(Src);
          Dst.AddBehavior(Src);
          // TODO: update tree in a simple way for now
          UpdateDesign;
          ModifiedOutsideObjectInspector('Drag''n''drop ' + Src.Name + ' into ' +
            Dst.Name, ucHigh);
        end;
    end;
  end;

  procedure MoveNonVisual(const SrcParentComponent: TCastleComponent;
    const Src: TComponent;
    const Dst: TCastleComponent);
  begin
    case ControlsTreeNodeUnderMouseSide of
      tnsRight:
        begin
          SrcParentComponent.RemoveNonVisualComponent(Src);
          Dst.AddNonVisualComponent(Src);
          // TODO: update tree in a simple way for now
          UpdateDesign;
          ModifiedOutsideObjectInspector('Drag''n''drop ' + Src.Name + ' into ' +
            Dst.Name, ucHigh);
        end;
    end;
  end;

  function ViewportItemsNode(const Viewport: TCastleViewport;
    const ViewportTreeNode: TTreeNode): TTreeNode;
  var
    I: Integer;
  begin
    Assert(TObject(ViewportTreeNode.Data) = Viewport);
    Assert(ViewportTreeNode.Data <> nil);
    Assert(TObject(ViewportTreeNode.Data) is TCastleViewport);

    for I := 0 to ViewportTreeNode.Count - 1 do
      if TObject(ViewportTreeNode[I].Data) = Viewport.Items then
        Exit(ViewportTreeNode[I]);
    raise EInternalError.CreateFmt('No tree node for viewport Items (TCastleTransform) found: %s %s', [
      Viewport.Name,
      Viewport.ClassName
    ]);
  end;

var
  Src, Dst: TTreeNode;
  SrcComponent, DstComponent: TComponent;

  { Move only the nodes in TTreeView, and update their captions.
    Assumes the move is possible.
    Also calls ModifiedOutsideObjectInspector to make Undo work. }
  procedure MoveOnlyTreeNodes;
  var
    DestinationName: String;
  begin
    case ControlsTreeNodeUnderMouseSide of
      tnsRight:
        begin
          { Special treatment when inserting UI into TCastleViewport,
            testcase: try to drag some UI into TCastleViewport (with tnsRight),
            it should be added right before "Items" node. }
          if (DstComponent is TCastleViewport) and
             (SrcComponent is TCastleUserInterface) then
            Src.MoveTo(ViewportItemsNode(TCastleViewport(DstComponent), Dst), naInsert)
          else
            Src.MoveTo(Dst, naAddChild);
          DestinationName := DstComponent.Name;
        end;
      tnsBottom:
        begin
          Src.MoveTo(Dst, naInsertBehind);
          DestinationName := TComponent(Dst.Parent.Data).Name;
        end;
      tnsTop:
        begin
          Src.MoveTo(Dst, naInsert);
          DestinationName := TComponent(Dst.Parent.Data).Name;
        end;
    end;
    ModifiedOutsideObjectInspector('Drag''n''drop ' + SrcComponent.Name + ' into ' +
      DestinationName, ucHigh);
  end;

begin
  if Source = ControlsTree then
  begin
    Src := ControlsTree.Selected;
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
        { Fixes selection after drag'n'drop.
          I think when we use TTreeNode.MoveTo(), TTreeView.Selected property value
          is changed to nil but in TTreeNode.Selected stays true. That's why we see
          selection but TTreeView.Selected state is incorect }
        ControlsTree.Selected := Src;
      end else
      if (SrcComponent is TCastleTransform) and
         (DstComponent is TCastleTransform) then
      begin
        MoveTransform(
          TCastleTransform(SrcComponent),
          TCastleTransform(DstComponent));
        { Fixes selection after drag'n'drop.
          I think when we use TTreeNode.MoveTo(), TTreeView.Selected property value
          is changed to nil but in TTreeNode.Selected stays true. That's why we see
          selection but TTreeView.Selected state is incorect }
        ControlsTree.Selected := Src;
      end else
      if (SrcComponent is TCastleBehavior) and
         (DstComponent is TCastleTransform) then
      begin
        MoveBehavior(
          TCastleBehavior(SrcComponent),
          TCastleTransform(DstComponent));
        // as for now we just refresh tree view, so set SelectedComponent and don't do ValidateHierarchy
        SelectedComponent := SrcComponent;
        Exit;
      end else
      if (not ( (SrcComponent is TCastleBehavior) or
                (SrcComponent is TCastleTransform) or
                (SrcComponent is TCastleUserInterface) ) ) and
         (DstComponent is TCastleComponent) and
         (Src.Parent <> nil) and
         (SelectedFromNode(Src.Parent) is TCastleComponent) then
      begin
        MoveNonVisual(
          TCastleComponent(SelectedFromNode(Src.Parent)),
          SrcComponent,
          TCastleComponent(DstComponent));
        // as for now we just refresh tree view, so set SelectedComponent and don't do ValidateHierarchy
        SelectedComponent := SrcComponent;
        Exit;
      end;
      ValidateHierarchy;
    end;
  end else
  if Source is TCastleShellListView then
  begin
    if SelectedComponent <> nil then
      ShellListAddComponent(TCastleShellListView(Source), SelectedComponent)
    else
      { TODO: This is never displayed, since Accept=false in this case.
        But it's a pity -- we should communicate to user better why drag-and-drop
        is not allowed now. }
      WritelnWarning('Select a component in hierarchy, to allow to drag-and-drop children into it');
  end;
end;

procedure TDesignFrame.ControlsTreeAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  C: TCanvas;

  procedure DrawTreeNodeUnderMouse;
  const
    ColorOutline = clBlack;
    ColorDecoration = clGray;
  var
    NodeRect, R: TRect;
  begin
    NodeRect := Node.DisplayRect(false);

    { We can't draw it in cdPrePaint, as after csPrePaint
      the node rectangle is cleared anyway.
      And we can't draw it in any cdXxxErase, which are not implemented
      in LCL (2.1.0). }
    C.Pen.Color := ColorOutline;
    C.Pen.Style := psDot;
    C.Brush.Style := bsClear;
    C.Rectangle(NodeRect);

    if ControlsTreeNodeUnderMouseSide = tnsRight then
    begin
      R := NodeRect.SplitRect(srRight, NodeRect.Height);
      R.Inflate(-5, -5);
      C.Brush.Color := ColorDecoration;
      C.Brush.Style := bsSolid;
      C.Pen.Color := ColorOutline;
      C.Pen.Style := psSolid;
      //C.FillRect(R);
      C.Polygon([
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

      C.Brush.Color := ColorDecoration;
      C.Brush.Style := bsSolid;
      C.FillRect(R);
    end;
  end;

  procedure DrawTreeNodeClassName(const NodeClassName: String);
  var
    TextRect: TRect;
  begin
    TextRect := Node.DisplayRect(true);
    C.Brush.Style := bsClear;
    C.Font.Color := clLtGray;
    C.TextOut(TextRect.Right, TextRect.Top, ' (' + NodeClassName + ')');
  end;

begin
  C := ControlsTree.Canvas;
  DefaultDraw := true;

  case Stage of
    cdPostPaint:
      begin
        if Node = ControlsTreeNodeUnderMouse then
          DrawTreeNodeUnderMouse;

        if Node.Data <> nil then
          DrawTreeNodeClassName(TObject(Node.Data).ClassName);
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
    UI.Translation := TVector2.Zero;
    ModifiedOutsideObjectInspector('Clear anchor deltas for ' + UI.Name, ucHigh);
  end;
end;

procedure TDesignFrame.ActionPhysicsPlayStopSimulationExecute(Sender: TObject);
var
  NewDesignOwner: TComponent;
begin
  if CastleDesignPhysicsMode in [pmPlaying, pmPaused] then
    CastleDesignPhysicsMode := pmStopped
  else
    CastleDesignPhysicsMode := pmPlaying;

  if CastleDesignPhysicsMode = pmPlaying then
  begin
    ActionPhysicsPlayStopSimulation.ImageIndex := TImageIndex(iiPhysicsStop);
    ActionPhysicsPauseSimulation.Visible := true;
    DesignStateBeforePhysicsRun := ComponentToString(DesignRoot);
    DesignModifiedBeforePhysicsRun := FDesignModified;
  end
  else
    begin
      ActionPhysicsPlayStopSimulation.ImageIndex := TImageIndex(iiPhysicsPlay);
      ActionPhysicsPauseSimulation.Visible := false;
      ActionPhysicsPauseSimulation.Checked := false;
      NewDesignOwner := TComponent.Create(Self);
      OpenDesign(InternalStringToComponent(DesignStateBeforePhysicsRun, NewDesignOwner, DesignOwner),
        NewDesignOwner, FDesignUrl);
      FDesignModified := DesignModifiedBeforePhysicsRun;
      OnUpdateFormCaption(Self);
    end;
end;

procedure TDesignFrame.ActionPhysicsPauseSimulationUpdate(Sender: TObject);
begin
  ActionPhysicsPauseSimulation.Checked := CastleDesignPhysicsMode = pmPaused;
  ActionPhysicsPauseSimulation.Visible := CastleDesignPhysicsMode in [pmPlaying, pmPaused];
end;

procedure TDesignFrame.ActionPhysicsPauseSimulationExecute(Sender: TObject);
begin
  if CastleDesignPhysicsMode = pmPaused then
    CastleDesignPhysicsMode := pmPlaying
  else if CastleDesignPhysicsMode = pmPlaying then
    CastleDesignPhysicsMode := pmPaused;

  ActionPhysicsPauseSimulation.Checked := CastleDesignPhysicsMode = pmPaused;
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

procedure TDesignFrame.ButtonRotateModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moRotate);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonScaleModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moScale);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonSelectModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moSelect);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonTranslateModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moTranslate);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ButtonInteractModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moInteract);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.MenuItemAddComponentClick(Sender: TObject);
var
  R: TRegisteredComponent;
begin
  R := TRegisteredComponent(Pointer((Sender as TComponent).Tag));
  AddComponent(R.ComponentClass, R.OnCreate);
end;

procedure TDesignFrame.MenuTreeViewItemCutClick(Sender: TObject);
begin
  CutComponent;
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

procedure TDesignFrame.MenuTreeViewItemSaveSelectedClick(Sender: TObject);
begin
  SaveSelected;
end;

procedure TDesignFrame.MenuTreeViewPopup(Sender: TObject);

  procedure ClearComponentEditorVerbs;
  begin
    { remove all TMenuItemToExecuteVerb }
    while (MenuTreeView.Items.Count > 0) and
          (MenuTreeView.Items[0] is TMenuItemToExecuteVerb) do
      MenuTreeView.Items[0].Free;

    { remove separator }
    if (MenuTreeView.Items.Count > 0) and
       (MenuTreeView.Items[0].Caption = '-') then
      MenuTreeView.Items[0].Free;
  end;

  procedure AddComponentEditorVerbs(const C: TComponent);
  var
    E: TBaseComponentEditor;
    I: Integer;
    MenuItem: TMenuItemToExecuteVerb;
    MenuItemSeparator: TMenuItem;
    InsertedVerbs: Cardinal;
  begin
    E := GetComponentEditor(C, FComponentEditorDesigner);
    if E <> nil then
    begin
      InsertedVerbs := 0;
      for I := 0 to E.GetVerbCount - 1 do
      begin
        { Ignore "Create default event" that we couldn't handle now,
          as our designer cannot initialize Code Tools to insert the event in code. }
        if E.GetVerb(I) = oisCreateDefaultEvent then
          Continue;
        MenuItem := TMenuItemToExecuteVerb.Create(Self);
        MenuItem.Caption := E.GetVerb(I);
        MenuItem.VerbIndex := I;
        MenuItem.ComponentEditor := E;
        MenuItem.ComponentToExecute := C;
        MenuTreeView.Items.Insert(InsertedVerbs, MenuItem);
        Inc(InsertedVerbs);
      end;

      { add separator if needed }
      if InsertedVerbs <> 0 then
      begin
        MenuItemSeparator := TMenuItem.Create(Self);
        MenuItemSeparator.Caption := '-';
        MenuTreeView.Items.Insert(InsertedVerbs, MenuItemSeparator);
      end;
    end;
  end;

var
  Sel: TComponent;
begin
  Sel := SelectedComponent;

  ClearComponentEditorVerbs;
  if Sel <> nil then
    AddComponentEditorVerbs(Sel);

  MenuTreeViewItemRename.Enabled := RenamePossible;
  MenuTreeViewItemDuplicate.Enabled := Sel <> nil;
  MenuTreeViewItemCut.Enabled := Sel <> nil;
  MenuTreeViewItemCopy.Enabled := Sel <> nil;
  MenuTreeViewItemSaveSelected.Enabled := Sel <> nil;
  MenuTreeViewItemDelete.Enabled := ControlsTree.SelectionCount > 0; // delete can handle multiple objects
  if (Sel is TCastleUserInterface) or ((Sel = nil) and (DesignRoot is TCastleUserInterface)) then
  begin
    MenuTreeViewItemAddUserInterface.SetEnabledVisible(true);
    MenuTreeViewItemAddTransform.SetEnabledVisible(false);
    MenuTreeViewItemAddBehavior.SetEnabledVisible(false);
  end else
  if (Sel is TCastleTransform) or ((Sel = nil) and (DesignRoot is TCastleTransform)) then
  begin
    MenuTreeViewItemAddUserInterface.SetEnabledVisible(false);
    MenuTreeViewItemAddTransform.SetEnabledVisible(true);
    MenuTreeViewItemAddBehavior.SetEnabledVisible(true);
  end else
  begin
    // on other components, you can add NonVisualComponent
    MenuTreeViewItemAddUserInterface.SetEnabledVisible(false);
    MenuTreeViewItemAddTransform.SetEnabledVisible(false);
    MenuTreeViewItemAddBehavior.SetEnabledVisible(false);
  end;
  MenuTreeView.PopupComponent := ControlsTree; // I'm not sure what it means, something like menu owner?
end;

procedure TDesignFrame.MenuTreeViewItemDuplicateClick(Sender: TObject);
begin
  DuplicateComponent;
end;

procedure TDesignFrame.ViewportSetup2D;
var
  V: TCastleViewport;
begin
  V := CurrentViewport;
  if V = nil then Exit;

  V.Setup2D;
  ModifiedOutsideObjectInspector('2D Camera And Projection At Runtime: ' + V.Name, ucHigh);
end;

procedure TDesignFrame.ViewportSort2D;
var
  V: TCastleViewport;
begin
  V := CurrentViewport;
  if V = nil then Exit;

  V.Items.SortBackToFront2D;
  UpdateDesign; // make the tree reflect new order
  ModifiedOutsideObjectInspector('Sort Items for Correct 2D Blending: ' + V.Name, ucHigh);
end;

procedure TDesignFrame.ViewportToggleProjection;
var
  V: TCastleViewport;
begin
  V := CurrentViewport;
  if V = nil then Exit;

  if V.InternalCamera.ProjectionType = ptPerspective then
    V.InternalCamera.ProjectionType := ptOrthographic
  else
    V.InternalCamera.ProjectionType := ptPerspective;
end;

function TDesignFrame.CameraToSynchronize(const V: TCastleViewport): TCastleCamera;
begin
  { Prefer to use camera from CameraPreview, as this is most natural in UI,
    because user sees CameraPreview.
    Do this even when SelectedTransform is also (maybe different) camera. }
  if (CameraPreview <> nil) and
     (CameraPreview.SelectedCamera <> nil) and
     (CameraPreview.SelectedCamera.World = V.Items) then
    Result := CameraPreview.SelectedCamera
  else
  if (SelectedTransform is TCastleCamera) and
     (TCastleCamera(SelectedTransform).World = V.Items) then
    Result := TCastleCamera(SelectedTransform)
  else
    Result := V.Camera;
end;

procedure TDesignFrame.CameraSynchronize(const Source, Target: TCastleCamera; const MakeUndo: Boolean);
var
  BeginPos, BeginDir, BeginUp, EndPos, EndDir, EndUp: TVector3;
begin
  Target.ProjectionType := Source.ProjectionType;
  Target.Perspective.FieldOfView     := Source.Perspective.FieldOfView;
  Target.Perspective.FieldOfViewAxis := Source.Perspective.FieldOfViewAxis;
  Target.Orthographic.Origin  := Source.Orthographic.Origin;
  Target.Orthographic.Width   := Source.Orthographic.Width;
  Target.Orthographic.Height  := Source.Orthographic.Height;

  Target.GetWorldView(BeginPos, BeginDir, BeginUp);
  Source.GetWorldView(EndPos, EndDir, EndUp);

  { Instead of using FixCamera2D, just explicitly force Z values to be unchanged. }
  if (Source.ProjectionType = ptOrthographic) and
     TVector3.Equals(EndDir, Vector3(0, 0, -1)) and
     TVector3.Equals(EndUp, Vector3(0, 1, 0)) then
  begin
    EndPos.Z := BeginPos.Z;
  end;

  if MakeUndo then
  begin
    { To record undo for Target camera pos/dir/up, we do a little trick.
      As we want to animate using AnimateTo, but we want to record undo state
      with already final pos/dir/up, so we *temporarily* adjust Target instantly
      to Source pos/dir/up. }
    Target.SetWorldView(EndPos, EndDir, EndUp);
    ModifiedOutsideObjectInspector('Align Camera To View: ' + Target.Name, ucHigh);
    // restore Target to begin positions, to animate to it
    Target.SetWorldView(BeginPos, BeginDir, BeginUp);
  end;

  Target.AnimateTo(EndPos, EndDir, EndUp, CameraTransitionTime);
end;

procedure TDesignFrame.ViewportAlignViewToCamera;
var
  V: TCastleViewport;
  C: TCastleCamera;
begin
  V := CurrentViewport;
  if V = nil then Exit;

  C := CameraToSynchronize(V);
  if C <> nil then
    CameraSynchronize(C, V.InternalCamera, false);
end;

procedure TDesignFrame.ViewportAlignCameraToView;
var
  V: TCastleViewport;
  C: TCastleCamera;
begin
  V := CurrentViewport;
  if V = nil then Exit;

  C := CameraToSynchronize(V);
  if C <> nil then
    CameraSynchronize(V.InternalCamera, C, true);
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
  ButtonSelectMode.Down := Mode = moSelect;
  ButtonTranslateMode.Down := Mode = moTranslate;
  ButtonRotateMode.Down := Mode = moRotate;
  ButtonScaleMode.Down := Mode = moScale;

  case Mode of
    moTranslate: VisualizeTransformSelected.Operation := voTranslate;
    moRotate: VisualizeTransformSelected.Operation := voRotate;
    moScale: VisualizeTransformSelected.Operation := voScale;
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
  NewRoot.Name := InternalProposeName(ComponentClass, NewDesignOwner);

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
  CastleDesignMode := true;
  CastleDesignPhysicsMode := pmStopped;
end.
