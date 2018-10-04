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
  Spin, Contnrs, Generics.Collections,
  // for TOIPropertyGrid usage
  ObjectInspector, PropEdits, PropEditUtils, GraphPropEdits,
  // CGE units
  CastleControl, CastleUIControls, CastlePropEdits, CastleDialogs,
  CastleSceneCore, CastleKeysMouse, CastleVectors, CastleRectangles,
  CastleSceneManager;

type
  { Frame to visually design component hierarchy. }
  TDesignFrame = class(TFrame)
    CheckParentSelfAnchorsEqual: TCheckBox;
    ControlProperties: TPageControl;
    ControlsTree: TTreeView;
    LabelSnap: TLabel;
    LabelSizeInfo: TLabel;
    LabelUIScaling: TLabel;
    LabelControlSelected: TLabel;
    LabelHierarchy: TLabel;
    PanelMiddleTop: TPanel;
    PanelMiddle: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    ScrollBox1: TScrollBox;
    SpinEditSnap: TSpinEdit;
    SplitterLeft: TSplitter;
    SplitterRight: TSplitter;
    TabAdvanced: TTabSheet;
    TabEvents: TTabSheet;
    TabAnchors: TTabSheet;
    TabSimple: TTabSheet;
    ToggleInteractMode: TToggleBox;
    ToggleSelectTranslateResizeMode: TToggleBox;
    procedure ControlsTreeSelectionChanged(Sender: TObject);
    procedure ToggleInteractModeClick(Sender: TObject);
    procedure ToggleSelectTranslateResizeModeClick(Sender: TObject);
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
        function GetSelectedUserInterface: TCastleUserInterface;
        procedure SetSelectedUserInterface(const Value: TCastleUserInterface);
        function HoverUserInterface(const AMousePosition: TVector2): TCastleUserInterface;
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
        property SelectedUserInterface: TCastleUserInterface
          read GetSelectedUserInterface write SetSelectedUserInterface;
      end;

      TTreeNodeMap = class(specialize TDictionary<TComponent, TTreeNode>)
      end;

      TMode = (moInteract, moSelectTranslateResize);

      TCastleSceneManagerCallback = procedure (const ASceneManager: TCastleSceneManager) is nested;

    var
      InspectorSimple, InspectorAdvanced, InspectorEvents: TOIPropertyGrid;
      PropertyEditorHook: TPropertyEditorHook;
      FDesignUrl: String;
      FDesignRoot: TComponent;
      { Owner of all components saved/loaded to component file,
        also temporary scene manager for .castle-transform.
        Everything specific to this hierarchy in CastleControl. }
      DesignOwner: TComponent;
      FDesignModified: Boolean;
      CastleControl: TCastleControlCustom;
      TreeNodeMap: TTreeNodeMap;
      Mode: TMode;
      InsideToggleModeClick: Boolean;
    procedure CastleControlResize(Sender: TObject);
    function ComponentCaption(const C: TComponent): String;
    { calculate Selected list, non-nil <=> non-empty }
    procedure GetSelected(out Selected: TComponentList;
      out SelectedCount: Integer);
    procedure InspectorSimpleFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure PropertyGridModified(Sender: TObject);
    procedure UpdateDesign(const Root: TComponent);
    procedure UpdateSelectedControl;
    function ProposeName(const ComponentClass: TComponentClass;
      const ComponentsOwner: TComponent): String;
    procedure UpdateComponentCaptionFromName(const C: TComponent);
    procedure UpdateLabelSizeInfo(const UI: TCastleUserInterface);
    procedure ChangeMode(const NewMode: TMode);
    procedure ModifiedOutsideObjectInspector;
    function ForEachSelectedSceneManager(const Callback: TCastleSceneManagerCallback): Cardinal;
  public
    OnUpdateFormCaption: TNotifyEvent;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveDesign(const Url: string);
    { Changes DesignRoot, DesignUrl and all the associated user-interface. }
    procedure OpenDesign(const NewDesignRoot, NewDesignOwner: TComponent;
      const NewDesignUrl: String);
    procedure OpenDesign(const NewDesignUrl: String);
    procedure NewDesign(const ComponentClass: TComponentClass);

    function FormCaption: String;
    procedure BeforeProposeSaveDesign;
    procedure AddComponent(const ComponentClass: TComponentClass;
      const PrimitiveGeometry: TPrimitiveGeometry = pgNone);
    procedure DeleteComponent;
    procedure CameraViewAll;
    procedure SortBackToFront2D;
    { set UIScaling values. }
    procedure UIScaling(const UIScaling: TUIScaling;
      const UIReferenceWidth, UIReferenceHeight: Single);

    property DesignUrl: String read FDesignUrl;
    { Root saved/loaded to component file }
    property DesignRoot: TComponent read FDesignRoot;
    property DesignModified: Boolean read FDesignModified;
  end;

implementation

uses TypInfo, StrUtils, Math,
  CastleComponentSerialize, CastleTransform, CastleUtils,
  CastleControls, CastleURIUtils, CastleStringUtils,
  CastleGLUtils, CastleColors, CastleCameras,
  EditorUtils;

{$R *.lfm}

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
end;

function TDesignFrame.TDesignerLayer.GetSelectedUserInterface: TCastleUserInterface;
var
  Selected: TComponentList;
  SelectedCount: Integer;
begin
  Result := nil;
  Frame.GetSelected(Selected, SelectedCount);
  try
    if (SelectedCount = 1) and (Selected[0] is TCastleUserInterface) then
      Result := TCastleUserInterface(Selected[0]);
  finally FreeAndNil(Selected) end;
end;

procedure TDesignFrame.TDesignerLayer.SetSelectedUserInterface(
  const Value: TCastleUserInterface);
var
  Node: TTreeNode;
begin
  if Frame.TreeNodeMap.TryGetValue(Value, Node) then
    Frame.ControlsTree.Select([Node]);
end;

function TDesignFrame.TDesignerLayer.HoverUserInterface(
  const AMousePosition: TVector2): TCastleUserInterface;

  function ControlUnder(const C: TCastleUserInterface;
    const MousePos: TVector2): TCastleUserInterface;
  var
    I: Integer;
  begin
    Result := nil;
    { To allow selecting even controls that have bad rectangle (outside
      of parent, which can happen, e.g. if you enlarge caption of label
      with AutoSize), do not check C.CapturesEventsAtPosition(MousePos)
      too early here. }
    if C.GetExists {and
       C.CapturesEventsAtPosition(MousePos)} then
    begin
      for I := C.ControlsCount - 1 downto 0 do
      begin
        Result := ControlUnder(C.Controls[I], MousePos);
        if Result <> nil then Exit;
      end;
      if C.CapturesEventsAtPosition(MousePos) then
        Result := C;
    end;
  end;

begin
  if Frame.DesignRoot is TCastleUserInterface then
    Result := ControlUnder(Frame.DesignRoot as TCastleUserInterface, AMousePosition)
  else
    Result := nil;
end;

function TDesignFrame.TDesignerLayer.IsResizing(const UI: TCastleUserInterface;
  const Position: TVector2; out Horizontal: THorizontalPosition;
  out Vertical: TVerticalPosition): Boolean;
const
  BorderDragMargin = 10;
var
  R: TFloatRectangle;
begin
  R := UI.RenderRect;

  { the order of checking (top or bottom) matters in case of very
    small heights. }
  if R.TopPart(BorderDragMargin).Contains(Position) then
    Vertical := vpTop
  else
  if R.BottomPart(BorderDragMargin).Contains(Position) then
    Vertical := vpBottom
  else
    Vertical := vpMiddle;

  if R.RightPart(BorderDragMargin).Contains(Position) then
    Horizontal := hpRight
  else
  if R.LeftPart(BorderDragMargin).Contains(Position) then
    Horizontal := hpLeft
  else
    Horizontal := hpMiddle;

  Result := (Vertical <> vpMiddle) or (Horizontal <> hpMiddle);
end;

function TDesignFrame.TDesignerLayer.Press(
  const Event: TInputPressRelease): Boolean;
var
  UI: TCastleUserInterface;
begin
  Result := inherited Press(Event);
  if Result then Exit;

  if (Frame.Mode = moSelectTranslateResize) and Event.IsMouseButton(mbLeft) then
  begin
    // TODO: for now selects only UI control, not TCastleTransform
    UI := HoverUserInterface(Event.Position);
    if UI <> nil then
    begin
      if IsResizing(UI, Event.Position, ResizingHorizontal, ResizingVertical) then
        DraggingMode := dmResize
      else
        DraggingMode := dmTranslate;
      SelectedUserInterface := UI;
      Exit(ExclusiveEvents);
    end;

    PendingMove := TVector2.Zero;
  end;
end;

function TDesignFrame.TDesignerLayer.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited Press(Event);
  if Result then Exit;

  if Event.IsMouseButton(mbLeft) then
    DraggingMode := dmNone;
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
    CurrentRect := UI.RenderRect;
    case DraggingMode of
      dmTranslate: ResultingRect := CurrentRect.Translate(Move);
      dmResize   : ResultingRect := ResizeRect(CurrentRect, Move);
    end;
    ParentR := ParentRenderRect(UI);
    { only allow movement/resize if control will not go outside of parent,
      unless it was already outside }
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
    // everything done here relies on anchors being active
    UI.HasHorizontalAnchor := true;
    UI.HasVerticalAnchor := true;

    Frame.ModifiedOutsideObjectInspector;
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
    if Frame.Mode <> moSelectTranslateResize then
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
  if (DraggingMode <> dmNone) and (not (mbLeft in Event.Pressed)) then
    DraggingMode := dmNone;

  if (Frame.Mode = moSelectTranslateResize) and (DraggingMode <> dmNone) then
  begin
    UI := SelectedUserInterface;
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
end;

procedure TDesignFrame.TDesignerLayer.Render;
var
  UI: TCastleUserInterface;
  R: TFloatRectangle;
begin
  inherited;

  UI := SelectedUserInterface;
  if UI <> nil then
  begin
    R := UI.RenderRect;
    DrawRectangleOutline(R, White);
    DrawRectangleOutline(R.Grow(-1), Black);
  end;

  UI := HoverUserInterface(Container.MousePosition);
  if UI <> nil then
  begin
    R := UI.RenderRect;
    DrawRectangleOutline(R, Vector4(1, 1, 0, 0.75));

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
  end;

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
        ErrorBox('An error occurred when reading the CastleSettings.xml file in your project:' +
          NL + NL + ExceptMessage(E));
        { and continue, this way you can still open a project with broken
          CastleSettings.xml }
      end;
    end;
  end;

var
  DesignerLayer: TDesignerLayer;
begin
  inherited;

  PropertyEditorHook := TPropertyEditorHook.Create(Self);

  InspectorSimple := CommonInspectorCreate;
  InspectorSimple.Parent := TabSimple;
  InspectorSimple.OnEditorFilter := @InspectorSimpleFilter;
  InspectorSimple.Filter := tkProperties;

  InspectorAdvanced := CommonInspectorCreate;
  InspectorAdvanced.Parent := TabAdvanced;
  InspectorAdvanced.Filter := tkProperties;

  InspectorEvents := CommonInspectorCreate;
  InspectorEvents.Parent := TabEvents;
  InspectorEvents.Filter := tkMethods;

  CastleControl := TCastleControlCustom.Create(Self);
  CastleControl.Parent := PanelMiddle;
  CastleControl.Align := alClient;
  CastleControl.OnResize := @CastleControlResize;

  ReadSettings;

  DesignerLayer := TDesignerLayer.Create(Self);
  DesignerLayer.Frame := Self;
  CastleControl.Controls.InsertFront(DesignerLayer);

  // It's too easy to change it visually and forget, so we set it from code
  ControlProperties.ActivePage := TabSimple;

  TreeNodeMap := TTreeNodeMap.Create;

  ChangeMode(moInteract);
end;

destructor TDesignFrame.Destroy;
begin
  FreeAndNil(TreeNodeMap);
  inherited Destroy;
end;

procedure TDesignFrame.SaveDesign(const Url: string);
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

procedure TDesignFrame.OpenDesign(const NewDesignRoot, NewDesignOwner: TComponent;
  const NewDesignUrl: String);

  procedure ClearDesign;
  begin
    ControlsTree.Items.Clear;
    UpdateSelectedControl;
    //CastleControl.Controls.Clear; // don't clear it, leave DesignerLayer
    FDesignRoot := nil;

    // this actually frees everything inside DesignRoot
    FreeAndNil(DesignOwner);
  end;

var
  Background: TCastleSimpleBackground;
  TempSceneManager: TCastleSceneManager;
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
    TempSceneManager := TCastleSceneManager.Create(NewDesignOwner);
    TempSceneManager.Transparent := true;
    TempSceneManager.UseHeadlight := hlOn;
    TempSceneManager.Items.Add(NewDesignRoot as TCastleTransform);
    CastleControl.Controls.InsertBack(TempSceneManager);
  end else
    raise EInternalError.Create('DesignRoot from file does not descend from TCastleUserInterface or TCastleTransform');

  // make background defined
  Background := TCastleSimpleBackground.Create(NewDesignOwner);
  Background.Color := Vector4(0.5, 0.5, 0.5, 1);
  CastleControl.Controls.InsertBack(Background);

  // replace DesignXxx variables, once loading successfull
  FDesignRoot := NewDesignRoot;
  FDesignUrl := NewDesignUrl;
  DesignOwner := NewDesignOwner;
  FDesignModified := DesignUrl = ''; // when opening '', mark new hierarchy modified
  // TODO: is this correct? what should be set here?
  PropertyEditorHook.LookupRoot := DesignOwner;

  UpdateDesign(DesignRoot);
  OnUpdateFormCaption(Self);
end;

procedure TDesignFrame.OpenDesign(const NewDesignUrl: String);
var
  NewDesignRoot, NewDesignOwner: TComponent;
  Mime: String;
begin
  NewDesignOwner := TComponent.Create(Self);

  Mime := URIMimeType(NewDesignUrl);
  if Mime = 'text/x-castle-user-interface' then
    NewDesignRoot := UserInterfaceLoad(NewDesignUrl, NewDesignOwner)
  else
  if Mime = 'text/x-castle-transform' then
    NewDesignRoot := TransformLoad(NewDesignUrl, NewDesignOwner)
  else
    raise Exception.CreateFmt('Unrecognized file extension %s (MIME type %s)',
      [ExtractFileExt(NewDesignUrl), Mime]);

  OpenDesign(NewDesignRoot, NewDesignOwner, NewDesignUrl);
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
begin
  { call SaveChanges to be sure to have good DesignModified value.
    Otherwise when editing e.g. TCastleButton.Caption,
    you can press F9 and have DesignModified = false,
    because PropertyGridModified doesn't occur because we actually
    press "tab" to focus another control. }
  InspectorSimple.SaveChanges;
  InspectorAdvanced.SaveChanges;
  InspectorEvents.SaveChanges;
end;

procedure TDesignFrame.AddComponent(const ComponentClass: TComponentClass;
  const PrimitiveGeometry: TPrimitiveGeometry = pgNone);

  procedure AddTransform(const ParentComponent: TCastleTransform);
  var
    NewTransform: TCastleTransform;
  begin
    if ComponentClass.InheritsFrom(TCastleTransform) then
    begin
      NewTransform := ComponentClass.Create(DesignOwner) as TCastleTransform;
      NewTransform.Name := ProposeName(ComponentClass, DesignOwner);
      UpdateComponentCaptionFromName(NewTransform);
      if PrimitiveGeometry <> pgNone then
      begin
        Assert(NewTransform is TCastleSceneCore);
        (NewTransform as TCastleSceneCore).PrimitiveGeometry := PrimitiveGeometry;
      end;
      ParentComponent.Add(NewTransform);
      UpdateDesign(DesignRoot);
    end else
      ErrorBox(Format('Cannot add component class %s when the parent is a TCastleTransform scendant (%s). Select a parent that descends from TCastleUserInterface.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
  end;

  procedure AddUserInterface(const ParentComponent: TCastleUserInterface);
  var
    NewUserInterface: TCastleUserInterface;
  begin
    if ComponentClass.InheritsFrom(TCastleUserInterface) then
    begin
      NewUserInterface := ComponentClass.Create(DesignOwner) as TCastleUserInterface;
      NewUserInterface.Name := ProposeName(ComponentClass, DesignOwner);
      UpdateComponentCaptionFromName(NewUserInterface);
      ParentComponent.InsertFront(NewUserInterface);
      UpdateDesign(DesignRoot);
    end else
      ErrorBox(Format('Cannot add component class %s when the parent is a TCastleUserInterface descendant (%s). Select a parent that descends from TCastleTransform, for example select SceneManager.Items.',
        [ComponentClass.ClassName, ParentComponent.ClassName]))
  end;

var
  Selected: TComponentList;
  SelectedCount: Integer;
  ParentComponent: TComponent;
begin
  // calculate ParentComponent
  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount = 1 then
      ParentComponent := Selected.First
    else
      ParentComponent := DesignRoot;
  finally FreeAndNil(Selected) end;

  if ParentComponent is TCastleUserInterface then
  begin
    AddUserInterface(ParentComponent as TCastleUserInterface);
  end else
  if ParentComponent is TCastleTransform then
  begin
    AddTransform(ParentComponent as TCastleTransform);
  end else
    ErrorBox(Format('Cannot add to the parent of class %s, select other parent before adding.',
      [ParentComponent.ClassName]))
end;

procedure TDesignFrame.DeleteComponent;

  function FirstDeletableComponent(const List: TComponentList): TComponent;
  var
    I: Integer;
  begin
    for I := 0 to List.Count - 1 do
      if (not (csSubComponent in List[I].ComponentStyle)) and
         (List[I] <> DesignRoot) then
        Exit(List[I]);
    Result := nil;
  end;

var
  Selected: TComponentList;
  SelectedCount: Integer;
  C: TComponent;
begin
  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount <> 0 then // check this, otherwise Selected may be nil
    begin
      { We depend on the fact TComponentList observes freed items,
        and removes them automatically.
        This way also freeing something that frees something else
        should work (although we don't really need it now,
        DesignOwner owns everything). }

      repeat
        C := FirstDeletableComponent(Selected);
        if C <> nil then
          FreeAndNil(C)
        else
          Break;
      until false;

      // temporarily disable this event, as some pointers are invalid now
      ControlsTree.OnSelectionChanged := nil;
      ControlsTree.Items.Clear;
      ControlsTree.OnSelectionChanged := @ControlsTreeSelectionChanged;

      UpdateDesign(DesignRoot);
    end;
  finally FreeAndNil(Selected) end;
end;

function TDesignFrame.ForEachSelectedSceneManager(
  const Callback: TCastleSceneManagerCallback): Cardinal;
var
  Selected: TComponentList;
  SelectedCount, I: Integer;
  World: TSceneManagerWorld;
  Sel: TComponent;
begin
  Result := 0;

  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount = 0 then
    begin
      ErrorBox('Select some SceneManager, or any transformation that is part of scene manager, first');
      Exit;
    end;

    for I := 0 to SelectedCount - 1 do
    begin
      Sel := Selected[I];
      if Sel is TCastleSceneManager then
      begin
        Callback(Sel as TCastleSceneManager);
        Inc(Result);
      end else
      if Sel is TCastleTransform then
      begin
        World := (Sel as TCastleTransform).World;
        if World <> nil then
        begin
          Callback(World.Owner as TCastleSceneManager);
          Inc(Result);
        end;
      end;
    end;
  finally FreeAndNil(Selected) end;
end;

procedure TDesignFrame.CameraViewAll;

  procedure AdjustCamera(const SceneManager: TCastleSceneManager);
  var
    Position, Direction, Up, GravityUp: TVector3;
  begin
    CameraViewpointForWholeScene(SceneManager.Items.BoundingBox,
      2, 1, false, true, // dir = -Z, up = +Y
      Position, Direction, Up, GravityUp);
    SceneManager.RequiredCamera.AnimateTo(Position, Direction, Up, 0.5);
    SceneManager.RequiredCamera.GravityUp := GravityUp;
    // Makes Examine camera pivot, and scroll speed, adjust to sizes
    SceneManager.RequiredCamera.ModelBox := SceneManager.Items.BoundingBox;
  end;

begin
  ForEachSelectedSceneManager(@AdjustCamera);
end;

procedure TDesignFrame.SortBackToFront2D;

  procedure CallSortBackToFront2D(const SceneManager: TCastleSceneManager);
  begin
    SceneManager.Items.SortBackToFront2D;
  end;

begin
  if ForEachSelectedSceneManager(@CallSortBackToFront2D) <> 0 then
  begin
    ModifiedOutsideObjectInspector;
    UpdateDesign(DesignRoot); // make the tree reflect new order
  end;
end;

procedure TDesignFrame.UIScaling(const UIScaling: TUIScaling;
  const UIReferenceWidth, UIReferenceHeight: Single);
begin
  CastleControl.Container.UIScaling := UIScaling;
  CastleControl.Container.UIReferenceWidth := UIReferenceWidth;
  CastleControl.Container.UIReferenceHeight := UIReferenceHeight;
end;

function TDesignFrame.ComponentCaption(const C: TComponent): String;

  function ClassCaption(const C: TClass): String;
  begin
    Result := C.ClassName;

    // hide some internal classes by instead displaying ancestor name
    if (C = TControlGameSceneManager) or
       (C = TSceneManagerWorld) or
       (Result = 'TSceneManagerWorldConcrete') then
      Result := ClassCaption(C.ClassParent);
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
    usExplicitScale       : H := 'User interface scaling in effect: explicit scale.' + NL;
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

procedure TDesignFrame.InspectorSimpleFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  AShow := (aEditor.GetPropInfo <> nil) and
    (
      (aEditor.GetPropInfo^.Name = 'URL') or
      (aEditor.GetPropInfo^.Name = 'Name') or
      (aEditor.GetPropInfo^.Name = 'Caption') or
      (aEditor.GetPropInfo^.Name = 'Exists') or
      (aEditor.GetPropInfo^.Name = 'ProcessEvents')
    );
end;

procedure TDesignFrame.PropertyGridModified(Sender: TObject);
var
  SelectedComponent: TComponent;
  Selected: TComponentList;
  SelectedCount: Integer;
begin
  // when you modify component Name in PropertyGrid, update it in the ControlsTree
  Assert(ControlsTree.Selected <> nil);
  Assert(ControlsTree.Selected.Data <> nil);
  Assert(TObject(ControlsTree.Selected.Data) is TComponent);
  SelectedComponent := TComponent(ControlsTree.Selected.Data);

  ControlsTree.Selected.Text := ComponentCaption(SelectedComponent);

  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount = 1 then
    begin
      // update also LabelControlSelected
      LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Selected[0]);

      // update also LabelSizeInfo
      if Selected[0] is TCastleUserInterface then
        UpdateLabelSizeInfo(Selected[0] as TCastleUserInterface);
    end;
  finally FreeAndNil(Selected) end;

  // mark modified
  FDesignModified := true;
  OnUpdateFormCaption(Self);
end;

procedure TDesignFrame.UpdateDesign(const Root: TComponent);

  function AddTransform(const Parent: TTreeNode; const T: TCastleTransform): TTreeNode;
  var
    S: String;
    I: Integer;
  begin
    S := ComponentCaption(T);
    Result := ControlsTree.Items.AddChildObject(Parent, S, T);
    TreeNodeMap.AddOrSetValue(T, Result);
    for I := 0 to T.Count - 1 do
      AddTransform(Result, T[I]);
  end;

  function AddControl(const Parent: TTreeNode; const C: TCastleUserInterface): TTreeNode;
  var
    S: String;
    I: Integer;
    SceneManager: TCastleSceneManager;
  begin
    S := ComponentCaption(C);
    Result := ControlsTree.Items.AddChildObject(Parent, S, C);
    TreeNodeMap.AddOrSetValue(C, Result);
    for I := 0 to C.ControlsCount - 1 do
      AddControl(Result, C.Controls[I]);

    if C is TCastleSceneManager then
    begin
      SceneManager := TCastleSceneManager(C);
      AddTransform(Result, SceneManager.Items);
    end;
  end;

var
  Node: TTreeNode;
begin
  ControlsTree.Items.Clear;
  TreeNodeMap.Clear;

  if Root is TCastleUserInterface then
    Node := AddControl(nil, Root as TCastleUserInterface)
  else
  if Root is TCastleTransform then
    Node := AddTransform(nil, Root as TCastleTransform)
  else
    raise EInternalError.Create('Cannot UpdateDesign with other classes than TCastleUserInterface or TCastleTransform');

  // show expanded by default
  Node.Expand(true);

  UpdateSelectedControl;
end;

procedure TDesignFrame.GetSelected(out Selected: TComponentList;
  out SelectedCount: Integer);

  function SelectedFromNode(const Node: TTreeNode): TComponent;
  var
    SelectedObject: TObject;
    //SelectedControl: TCastleUserInterface;
    //SelectedTransform: TCastleTransform;
  begin
    SelectedObject := nil;
    Result := nil;
    //SelectedControl := nil;
    //SelectedTransform := nil;

    if Node <> nil then
    begin
      SelectedObject := TObject(Node.Data);
      if SelectedObject is TComponent then
      begin
        Result := TComponent(SelectedObject);
        //if SelectedComponent is TCastleUserInterface then
        //  SelectedControl := TCastleUserInterface(SelectedComponent)
        //else
        //if SelectedComponent is TCastleTransform then
        //  SelectedTransform := TCastleTransform(SelectedComponent);
      end;
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

procedure TDesignFrame.UpdateSelectedControl;
var
  Selected: TComponentList;
  SelectionForOI: TPersistentSelectionList;
  I, SelectedCount: Integer;
  UI: Boolean;
begin
  GetSelected(Selected, SelectedCount);
  try
    case SelectedCount of
      0: LabelControlSelected.Caption := 'Nothing Selected';
      1: LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Selected[0]);
      else LabelControlSelected.Caption := 'Selected:' + NL + IntToStr(SelectedCount) + ' components';
    end;

    ControlProperties.Visible := SelectedCount <> 0;
    ControlProperties.Enabled := SelectedCount <> 0;

    SelectionForOI := TPersistentSelectionList.Create;
    try
      for I := 0 to SelectedCount - 1 do
        SelectionForOI.Add(Selected[I]);
      InspectorSimple.Selection := SelectionForOI;
      InspectorAdvanced.Selection := SelectionForOI;
      InspectorEvents.Selection := SelectionForOI;
    finally FreeAndNil(SelectionForOI) end;

    UI := (SelectedCount = 1) and (Selected[0] is TCastleUserInterface);
    TabAnchors.TabVisible := UI;
    if UI then
      UpdateLabelSizeInfo(Selected[0] as TCastleUserInterface);
  finally FreeAndNil(Selected) end;
end;

procedure TDesignFrame.ControlsTreeSelectionChanged(Sender: TObject);
begin
  UpdateSelectedControl;
end;

procedure TDesignFrame.ToggleInteractModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moInteract);
  InsideToggleModeClick := false;
end;

procedure TDesignFrame.ToggleSelectTranslateResizeModeClick(Sender: TObject);
begin
  if InsideToggleModeClick then Exit;
  InsideToggleModeClick := true;
  ChangeMode(moSelectTranslateResize);
  InsideToggleModeClick := false;
end;

function TDesignFrame.ProposeName(const ComponentClass: TComponentClass;
  const ComponentsOwner: TComponent): String;
var
  ResultBase: String;
  I: Integer;
begin
  ResultBase := ComponentClass.ClassName;

  // remove common prefixes
  if IsPrefix('TCastleUserInterface', ResultBase, true) then
    ResultBase := PrefixRemove('TCastleUserInterface', ResultBase, true)
  else
  if IsPrefix('TCastle', ResultBase, true) then
    ResultBase := PrefixRemove('TCastle', ResultBase, true)
  else
  if IsPrefix('T', ResultBase, true) then
    ResultBase := PrefixRemove('T', ResultBase, true);

  // remove 2D, as component name cannot start with that
  if IsPrefix('2D', ResultBase, true) then
    ResultBase := PrefixRemove('2D', ResultBase, true);

  // in case the replacements above made '', fix it (can happen in case of TCastleUserInterface)
  if ResultBase = '' then
    ResultBase := 'Group';

  // make unique
  I := 1;
  Result := ResultBase + IntToStr(I);
  while ComponentsOwner.FindComponent(Result) <> nil do
  begin
    Inc(I);
    Result := ResultBase + IntToStr(I);
  end;
end;

procedure TDesignFrame.UpdateComponentCaptionFromName(const C: TComponent);
begin
  if C is TCastleLabel then
    TCastleLabel(C).Caption := C.Name
  else
  if C is TCastleButton then
    TCastleButton(C).Caption := C.Name
  else
  if C is TCastleEdit then
    TCastleEdit(C).Text := C.Name;
end;

procedure TDesignFrame.UpdateLabelSizeInfo(const UI: TCastleUserInterface);
var
  RR: TFloatRectangle;
  S: String;
begin
  RR := UI.RenderRect;
  if RR.IsEmpty then
    S := 'Size: Empty'
  else
  begin
    S := Format(
      'Calculated size: %f x %f' + NL +
      NL +
      'Screen rectangle (scaled and with anchors):' + NL +
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
     (not UI.Parent.RenderRect.Contains(UI.RenderRect)) then
    S := S + NL + NL + 'WARNING: The rectangle occupied by this control is outside of the parent rectangle. The events (like mouse clicks) may not reach this control. You must always fit child control inside the parent.';

  LabelSizeInfo.Caption := S;
end;

procedure TDesignFrame.ChangeMode(const NewMode: TMode);
begin
  Mode := NewMode;
  ToggleInteractMode.Checked := Mode = moInteract;

  ToggleSelectTranslateResizeMode.Checked := Mode = moSelectTranslateResize;
  LabelSnap.Visible := Mode = moSelectTranslateResize;
  LabelSnap.Enabled := Mode = moSelectTranslateResize;
  SpinEditSnap.Visible := Mode = moSelectTranslateResize;
  SpinEditSnap.Enabled := Mode = moSelectTranslateResize;
end;

procedure TDesignFrame.ModifiedOutsideObjectInspector;
begin
  // TODO: this moves UI scrollbar up,
  // TODO: this is not optimized
  // (PropertyGridModified does some unnecessary things if we only changed size)
  InspectorSimple.RefreshPropertyValues;
  InspectorAdvanced.RefreshPropertyValues;
  InspectorEvents.RefreshPropertyValues;
  PropertyGridModified(nil);
end;

procedure TDesignFrame.NewDesign(const ComponentClass: TComponentClass);
var
  NewRoot: TComponent;
  NewDesignOwner: TComponent;
begin
  NewDesignOwner := TComponent.Create(Self);

  NewRoot := ComponentClass.Create(NewDesignOwner);
  NewRoot.Name := ProposeName(ComponentClass, NewDesignOwner);
  UpdateComponentCaptionFromName(NewRoot);

  //if NewRoot is TCastleUserInterface then
  //  (NewRoot as TCastleUserInterface).FullSize := true;

  OpenDesign(NewRoot, NewDesignOwner, '');
end;

initialization
  { Enable using our property edits e.g. for TCastleScene.URL }
  CastlePropEdits.Register;
  PropertyEditorsAdviceDataDirectory := true;
end.
