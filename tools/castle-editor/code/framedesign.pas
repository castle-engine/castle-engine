unit FrameDesign;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  Contnrs,
  // for TOIPropertyGrid usage
  ObjectInspector, PropEdits, PropEditUtils, GraphPropEdits,
  // CGE units
  CastleControl, CastleUIControls, CastlePropEdits, CastleDialogs,
  CastleSceneCore;

type
  TDesignFrame = class(TFrame)
    ControlProperties: TPageControl;
    ControlsTree: TTreeView;
    LabelUIScaling: TLabel;
    LabelControlSelected: TLabel;
    LabelHierarchy: TLabel;
    PanelMiddle: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    SplitterLeft: TSplitter;
    SplitterRight: TSplitter;
    TabAdvanced: TTabSheet;
    TabEvents: TTabSheet;
    TabSimple: TTabSheet;
    procedure ControlsTreeSelectionChanged(Sender: TObject);
  private
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
    procedure CastleControlResize(Sender: TObject);
    function ComponentCaption(const C: TComponent): String;
    { calculate Selected list, non-nil <=> non-empty }
    procedure GetSelected(out Selected: TComponentList;
      out SelectedCount: Integer);
    procedure DesignModifiedNotification(Sender: TObject);
    procedure InspectorSimpleFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure PropertyGridModified(Sender: TObject);
    procedure UpdateDesign(const Root: TComponent);
    procedure UpdateSelectedControl;
    function ProposeName(const ComponentClass: TComponentClass;
      const ComponentsOwner: TComponent): String;
    procedure UpdateComponentCaptionFromName(const C: TComponent);
  public
    OnUpdateFormCaption: TNotifyEvent;
    constructor Create(TheOwner: TComponent); override;

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

    property DesignUrl: String read FDesignUrl;
    { Root saved/loaded to component file }
    property DesignRoot: TComponent read FDesignRoot;
    property DesignModified: Boolean read FDesignModified;
  end;

implementation

uses TypInfo, StrUtils,
  CastleComponentSerialize, CastleTransform, CastleSceneManager, CastleUtils,
  CastleControls, CastleURIUtils, CastleVectors, CastleStringUtils,
  EditorUtils;

{$R *.lfm}

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
    Result.OnModified := @DesignModifiedNotification;
  end;

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

  // initialize CastleControl
  // TODO: This should follow the auto-scale settings of loaded file
  CastleControl.Container.UIReferenceWidth := 1600;
  CastleControl.Container.UIReferenceHeight := 900;
  CastleControl.Container.UIScaling := usEncloseReferenceSize;

  // It's too easy to change it visually and forget, so we set it from code
  ControlProperties.ActivePage := TabSimple;
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
    CastleControl.Controls.Clear;
    FDesignRoot := nil;

    // this actually frees everything inside DesignRoot
    FreeAndNil(DesignOwner);
  end;

var
  Background: TCastleSimpleBackground;
  TempSceneManager: TCastleSceneManager;
begin
  ClearDesign;

  if NewDesignRoot is TCastleUserInterface then
  begin
    CastleControl.Controls.InsertFront(NewDesignRoot as TCastleUserInterface)
  end else
  if NewDesignRoot is TCastleTransform then
  begin
    TempSceneManager := TCastleSceneManager.Create(NewDesignOwner);
    TempSceneManager.Transparent := true;
    TempSceneManager.UseHeadlight := hlOn;
    TempSceneManager.Items.Add(NewDesignRoot as TCastleTransform);
    CastleControl.Controls.InsertFront(TempSceneManager);
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
    because DesignModifiedNotification doesn't occur because we actually
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
    if SelectedCount <> 1 then
    begin
      ErrorBox('Select exactly one component as a parent for the added component.');
      Exit;
    end;
    ParentComponent := Selected.First;
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
  CalculatedUIScale :=  (1 / CastleControl.Container.UnscaledFloatWidth) *
    CastleControl.Container.Width;
  CalculatedUIScaleStr := IntToStr(Round(CalculatedUIScale * 100)) + '%';

  LabelUIScaling.Caption := CalculatedUIScaleStr;
  case CastleControl.Container.UIScaling of
    usNone                : H := 'No user interface scaling';
    usEncloseReferenceSize: H := 'User interface scaling in effect: window must fit inside a simulated size of %dx%d.' + NL;
    usFitReferenceSize    : H := 'User interface scaling in effect: window must enclose a simulated size of %dx%d.' + NL;
    usExplicitScale       : H := 'User interface scaling in effect: explicit scale.' + NL;
    else raise EInternalError.Create('CastleControl.Container.UIScaling?');
  end;
  if CastleControl.Container.UIScaling <> usNone then
  begin
    H := Format(H + 'Actual window size is %dx%d.' + NL + 'Calculated scale is %s, which simulates surface of size %dx%d.',
      [CastleControl.Container.UIReferenceWidth,
       CastleControl.Container.UIReferenceHeight,
       CastleControl.Container.Width,
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

  { update also LabelControlSelected }
  GetSelected(Selected, SelectedCount);
  try
    if SelectedCount = 1 then
      LabelControlSelected.Caption := 'Selected:' + NL + ComponentCaption(Selected[0]);
  finally FreeAndNil(Selected) end;
end;

procedure TDesignFrame.UpdateDesign(const Root: TComponent);

  function AddTransform(const Parent: TTreeNode; const T: TCastleTransform): TTreeNode;
  var
    S: String;
    I: Integer;
  begin
    S := ComponentCaption(T);
    Result := ControlsTree.Items.AddChildObject(Parent, S, T);
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

procedure TDesignFrame.DesignModifiedNotification(Sender: TObject);
begin
  FDesignModified := true;
  OnUpdateFormCaption(Self);
end;

procedure TDesignFrame.UpdateSelectedControl;
var
  Selected: TComponentList;
  SelectionForOI: TPersistentSelectionList;
  I, SelectedCount: Integer;
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
  finally FreeAndNil(Selected) end;
end;

procedure TDesignFrame.ControlsTreeSelectionChanged(Sender: TObject);
begin
  UpdateSelectedControl;
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

procedure TDesignFrame.NewDesign(const ComponentClass: TComponentClass);
var
  NewRoot: TComponent;
  NewDesignOwner: TComponent;
begin
  NewDesignOwner := TComponent.Create(Self);

  NewRoot := ComponentClass.Create(NewDesignOwner);
  NewRoot.Name := ProposeName(ComponentClass, NewDesignOwner);
  UpdateComponentCaptionFromName(NewRoot);

  if NewRoot is TCastleUserInterfaceRect then
    (NewRoot as TCastleUserInterfaceRect).FullSize := true;

  OpenDesign(NewRoot, NewDesignOwner, '');
end;

initialization
  { Enable using our property edits e.g. for TCastleScene.URL }
  CastlePropEdits.Register;
  PropertyEditorsAdviceDataDirectory := true;
end.
