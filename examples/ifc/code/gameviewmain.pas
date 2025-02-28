{
  Copyright 2024-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleScene, CastleViewport,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleIfc,
  CastleCameras, CastleTransform, CastleTransformManipulate, X3DNodes,
  CastleShapes, CastleBoxes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps, LabelWireframeEffect, LabelHierarchy: TCastleLabel;
    ButtonNew, ButtonLoad, ButtonSaveIfc, ButtonSaveNode,
      ButtonAddWall, ButtonAddWallAndWindow,
      ButtonModifyRandomElement, ButtonChangeWireframeEffect,
      ButtonViewEntireModel: TCastleButton;
    ButtonMouseLeftSelects, ButtonMouseLeftNavigates: TCastleButton;
    IfcScene: TCastleScene;
    MainViewport: TCastleViewport;
    ExamineNavigation: TCastleExamineNavigation;
    TransformSelectedProduct: TCastleTransform;
    ButtonHierarchyTemplate: TCastleButton;
    GroupHierarchy: TCastleVerticalGroup;
    CheckboxIfcDebugDisplay: TCastleCheckbox;
  private
    IfcFile: TIfcFile;
    { New elements (TIfcElement instances) have to be added to this container.
      You cannot just add them to IfcFile.Project,
      IFC specification constaints what can be the top-level spatial element. }
    IfcContainer: TIfcSpatialElement;
    IfcMapping: TCastleIfcMapping;

    { Selected IFC product.
      Change by ChangeSelectedProduct. }
    IfcSelectedProduct: TIfcProduct;
    { Value of TransformSelectedProduct.Translation
      that corresponds to the IfcSelectedProduct current position.
      Only meaningful when IfcSelectedProduct <> nil, undefined and unused otherwise. }
    IfcSelectedProductShapeTranslation: TVector3;

    { Used to assing unique names to products. }
    NextProductNumber: Cardinal;

    //TransformHover: TCastleTransformHover; //< TODO, show hover
    TransformManipulate: TCastleTransformManipulate;

    ButtonHierarchyFactory: TCastleComponentFactory;

    MouseButtonToSelect: TCastleMouseButton;

    { Create new IfcMapping instance and update what IfcScene shows,
      based on IfcFile contents.
      Use this after completely changing the IfcFile contents
      (like loading new file, or creating new file). }
    procedure NewIfcMapping(const NewIfcFile: TIfcFile);

    { Update hierarchy sidebar to show state of IfcFile. }
    procedure UpdateHierarchy;

    { Change IfcSelectedProduct and update hierarchy sidebar. }
    procedure ChangeSelectedProduct(const NewSelectedProduct: TIfcProduct);

    { Bounding box, in the scene space, of all representations of the product.
      The box is empty (TBox3D.IsEmpty) if no representations. }
    function ProductBoundingBox(const Product: TIfcProduct): TBox3D;

    { Update using mouse left or right buttons. }
    procedure SetMouseMode(const LeftSelectsRightNavigates: Boolean);

    { Callback handlers. }

    procedure ClickNew(Sender: TObject);
    procedure ClickLoad(Sender: TObject);
    procedure ClickSaveIfc(Sender: TObject);
    procedure ClickSaveNode(Sender: TObject);
    procedure ClickAddWall(Sender: TObject);
    procedure ClickAddWallAndWindow(Sender: TObject);
    procedure ClickModifyRandomElement(Sender: TObject);
    procedure ClickChangeWireframeEffect(Sender: TObject);
    procedure ClickViewEntireModel(Sender: TObject);
    procedure ClickMouseLeftSelects(Sender: TObject);
    procedure ClickMouseLeftNavigates(Sender: TObject);
    procedure MainViewportPress(const Sender: TCastleUserInterface;
      const Event: TInputPressRelease; var Handled: Boolean);
    procedure TransformManipulateTransformModified(Sender: TObject);
    procedure ButtonHierarchyClick(Sender: TObject);
    procedure IfcDebugDisplayChange(Sender: TObject);
    procedure IfcDebugDisplayToggleShape(Node: TX3DNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, TypInfo,
  CastleUtils, CastleUriUtils, CastleWindow, X3DLoad, CastleLog,
  CastleRenderOptions, CastleApplicationProperties;

function WireframeEffectToStr(const WireframeEffect: TWireframeEffect): String;
begin
  Result := GetEnumName(TypeInfo(TWireframeEffect), Ord(WireframeEffect));
end;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  TransformManipulate := TCastleTransformManipulate.Create(FreeAtStop);
  TransformManipulate.Mode := mmTranslate;
  TransformManipulate.OnTransformModified := {$ifdef FPC}@{$endif} TransformManipulateTransformModified;

  ButtonHierarchyFactory := TCastleComponentFactory.Create(FreeAtStop);
  ButtonHierarchyFactory.LoadFromComponent(ButtonHierarchyTemplate);
  FreeAndNil(ButtonHierarchyTemplate);

  { Initialize empty model.
    TransformManipulate, ButtonHierarchyFactory must be set earlier. }
  ClickNew(nil);

  ButtonNew.OnClick := {$ifdef FPC}@{$endif} ClickNew;
  ButtonLoad.OnClick := {$ifdef FPC}@{$endif} ClickLoad;
  ButtonSaveIfc.OnClick := {$ifdef FPC}@{$endif} ClickSaveIfc;
  ButtonSaveNode.OnClick := {$ifdef FPC}@{$endif} ClickSaveNode;
  ButtonAddWall.OnClick := {$ifdef FPC}@{$endif} ClickAddWall;
  ButtonAddWallAndWindow.OnClick := {$ifdef FPC}@{$endif} ClickAddWallAndWindow;
  ButtonModifyRandomElement.OnClick := {$ifdef FPC}@{$endif} ClickModifyRandomElement;
  ButtonChangeWireframeEffect.OnClick := {$ifdef FPC}@{$endif} ClickChangeWireframeEffect;
  ButtonViewEntireModel.OnClick := {$ifdef FPC}@{$endif} ClickViewEntireModel;
  CheckboxIfcDebugDisplay.OnChange := {$ifdef FPC}@{$endif} IfcDebugDisplayChange;
  ButtonMouseLeftNavigates.OnClick := {$ifdef FPC}@{$endif} ClickMouseLeftNavigates;
  ButtonMouseLeftSelects.OnClick := {$ifdef FPC}@{$endif} ClickMouseLeftSelects;

  MainViewport.OnPress := {$ifdef FPC}@{$endif} MainViewportPress;

  LabelWireframeEffect.Caption := WireframeEffectToStr(IfcScene.RenderOptions.WireframeEffect);

  { On desktops, by default "left selects, right navigates".

    But right mouse button is unavailable on touch devices (mobile)
    and uncomfortable (conflicts with browser context menu) on web.
    Assign by default left to navigation there. }
  SetMouseMode(
    {$ifdef WASI} false {$else} not ApplicationProperties.TouchDevice {$endif});

  // TODO: show on hover
  // TransformHover := TCastleTransformHover.Create(FreeAtStop);
  // TransformHover.Current := TransformSelectedProduct;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(IfcFile);
  FreeAndNil(IfcMapping);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.NewIfcMapping(const NewIfcFile: TIfcFile);
begin
  FreeAndNil(IfcMapping);

  IfcMapping := TCastleIfcMapping.Create;
  { The 'castle-data:/' below will be used as base URL to resolve any texture
    URLs inside IFC. As they are not possible in this demo for now,
    this value doesn't really matter. }
  IfcMapping.Load(IfcFile, 'castle-data:/');

  IfcScene.Load(IfcMapping.RootNode, true);

  // update debug boxes display
  if not CheckboxIfcDebugDisplay.Checked then
    IfcDebugDisplayChange(nil);

  UpdateHierarchy;

  IfcSelectedProduct := nil;
  TransformManipulate.SetSelected([]);
end;

procedure TViewMain.ClickNew(Sender: TObject);

  procedure AddFloor;
  const
    FloorSize = 10;
  var
    Slab: TIfcSlab;
  begin
    Slab := TIfcSlab.Create(IfcFile);
    Slab.Name := 'Floor';
    Slab.PredefinedType := TIfcSlabTypeEnum.Floor;
    Slab.AddMeshRepresentation(IfcFile.Project.ModelContext, [
      Vector3(-FloorSize / 2, -FloorSize / 2, 0),
      Vector3( FloorSize / 2, -FloorSize / 2, 0),
      Vector3( FloorSize / 2,  FloorSize / 2, 0),
      Vector3(-FloorSize / 2,  FloorSize / 2, 0)
    ], [0, 1, 2, 3]);

    IfcContainer.AddContainedElement(Slab);

    { Note: one needs to call IfcMapping.Update(IfcFile) after the changes.
      But in this case, we call NewIfcMapping after AddFloor,
      so no need for extra update. }
  end;

var
  IfcSite: TIfcSite;
  IfcBuilding: TIfcBuilding;
  IfcBuildingStorey: TIfcBuildingStorey;
begin
  FreeAndNil(IfcFile); // owns all other IFC classes, so this frees everything

  IfcFile := TIfcFile.Create(nil);

  { Obligatory initialization of new IFC files structure.
    We must set IfcFile.Project,
    each project must have units,
    and each project must have a 3D model context. }
  IfcFile.Project := TIfcProject.Create(IfcFile);
  IfcFile.Project.SetupUnits;
  IfcFile.Project.SetupModelContext;

  { We need IfcContainer inside the project.
    Reason: We cannot add elements directly to IfcProject, we need
    to add them to a spatial root element: (IfcSite || IfcBuilding || IfcSpatialZone)
    (see https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProject.htm ).

    The IfcSpatialStructureElement
    (see https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialStructureElement.htm )
    defines the hierarchy of root classes, and implies that root hierarchy
    can be (one case, seems most common -- BlenderBIM also follows it):

    - IfcSite inside IfcProject,
    - with IfcBuilding inside,
    - with IfcBuildingStorey inside,
    inserted into each other using "is composed by" relationship. }

  IfcSite := TIfcSite.Create(IfcFile);
  IfcSite.Name := 'My Site';
  IfcFile.Project.AddIsDecomposedBy(IfcSite);

  IfcBuilding := TIfcBuilding.Create(IfcFile);
  IfcBuilding.Name := 'My Building';
  IfcSite.AddIsDecomposedBy(IfcBuilding);

  IfcBuildingStorey := TIfcBuildingStorey.Create(IfcFile);
  IfcBuildingStorey.Name := 'My Building Storey';
  IfcBuilding.AddIsDecomposedBy(IfcBuildingStorey);

  IfcContainer := IfcBuildingStorey;

  AddFloor;

  NewIfcMapping(IfcFile);
end;

const
  IfcFileFilter = 'IFC JSON (*.ifcjson)|*.ifcjson|All Files|*';

procedure TViewMain.ClickLoad(Sender: TObject);
var
  Url: string;
  IfcSite: TIfcSite;
  IfcBuilding: TIfcBuilding;
  IfcBuildingStorey: TIfcBuildingStorey;
begin
  Url := 'castle-data:/';
  if Application.MainWindow.FileDialog('Load IFC file', Url, true, IfcFileFilter) then
  begin
    FreeAndNil(IfcFile);
    IfcFile := IfcJsonLoad(Url);

    // initialize IfcContainer, needed as parent for new walls
    IfcContainer := IfcFile.Project.BestContainer;
    if IfcContainer = nil then
    begin
      WritelnWarning('IFC model "%s" is missing a spatial root element (IfcSite, IfcBuilding, IfcBuildingStorey), adding a dummy one', [
        Url
      ]);

      IfcSite := TIfcSite.Create(IfcFile);
      IfcSite.Name := 'My Site';
      IfcFile.Project.AddIsDecomposedBy(IfcSite);

      IfcBuilding := TIfcBuilding.Create(IfcFile);
      IfcBuilding.Name := 'My Building';
      IfcSite.AddIsDecomposedBy(IfcBuilding);

      IfcBuildingStorey := TIfcBuildingStorey.Create(IfcFile);
      IfcBuildingStorey.Name := 'My Building Storey';
      IfcBuilding.AddIsDecomposedBy(IfcBuildingStorey);

      IfcContainer := IfcBuildingStorey;
    end;

    WritelnLog('IFC best container in "%s" guessed as "%s"', [
      Url,
      IfcContainer.ClassName
    ]);

    // make sure we have ModelContext to add new walls
    if IfcFile.Project.ModelContext = nil then
    begin
      WritelnWarning('IFC model "%s" is missing a "Model" context in project.representationContexts, this can happen for IFC exported from BonsaiBIM', [
        Url
      ]);
      IfcFile.Project.SetupModelContext;
    end;
    NewIfcMapping(IfcFile);
  end;
end;

procedure TViewMain.ClickSaveIfc(Sender: TObject);
var
  Url: string;
begin
  Url := 'castle-data:/out.ifcjson';
  if Application.MainWindow.FileDialog('Save IFC file',
      Url, false, IfcFileFilter) then
  begin
    IfcJsonSave(IfcFile, Url);
  end;
end;

procedure TViewMain.ClickSaveNode(Sender: TObject);
var
  Url: string;
begin
  Url := 'castle-data:/out.x3d';
  if Application.MainWindow.FileDialog('Save Node To X3D or STL',
      Url, false, SaveNode_FileFilters) then
  begin
    SaveNode(IfcMapping.RootNode, Url);
  end;
end;

procedure TViewMain.ClickAddWall(Sender: TObject);
var
  Wall: TIfcWall;
  SizeX, SizeY, WallHeight: Single;
begin
  Wall := TIfcWall.Create(IfcFile);
  Wall.Name := 'Wall' + IntToStr(NextProductNumber);
  Inc(NextProductNumber);

  SizeX := RandomFloatRange(1, 4);
  SizeY := RandomFloatRange(1, 4);
  WallHeight := RandomFloatRange(1.5, 2.5); // Z is "up", by convention, in IFC
  Wall.AddBoxRepresentation(IfcFile.Project.ModelContext,
    Box3D(
      Vector3(-SizeX / 2, -SizeY / 2, 0),
      Vector3( SizeX / 2,  SizeY / 2, WallHeight)
    ));

  Wall.Translation := Vector3(
    RandomFloatRange(-5, 5),
    RandomFloatRange(-5, 5),
    0
  );

  IfcContainer.AddContainedElement(Wall);
  IfcMapping.Update(IfcFile);
  UpdateHierarchy;
end;

procedure TViewMain.ClickAddWallAndWindow(Sender: TObject);
var
  Wall: TIfcWall;
  Window: TIfcWindow;
  WallHeight: Single;
  WallSize, WindowSize, WindowPosition: TVector2;
  Opening: TIfcOpeningElement;
begin
  // randomize sizes / positions of wall and window
  WallSize := Vector2(
    RandomFloatRange(4, 6), // large X, this is wall's width
    RandomFloatRange(0.5, 1) // small Y, this is wall's thickness in this case
  );
  WallHeight := RandomFloatRange(1.5, 2.5); // Z is "up", by convention, in IFC
  { Window sizes randomized, always much smaller than wall
    width (WallSize.X) and WallHeight.
    Note that WindowSize.Y and WindowPosition.Y are going to affect the
    Z axis (height) along the wall, because Z is "up" in IFC. }
  WindowSize := Vector2(
    RandomFloatRange(0.25, 1),
    RandomFloatRange(0.25, 1)
  );
  WindowPosition := Vector2(0, 0.66 * WallHeight);

  // add wall
  Wall := TIfcWall.Create(IfcFile);
  Wall.Name := 'WallWithWindow' + IntToStr(NextProductNumber);
  Inc(NextProductNumber);
  Wall.AddBoxRepresentation(IfcFile.Project.ModelContext,
    Box3D(
      Vector3(-WallSize.X / 2, -WallSize.Y / 2, 0),
      Vector3( WallSize.X / 2,  WallSize.Y / 2, WallHeight)
    ));
  Wall.Translation := Vector3(
    RandomFloatRange(-5, 5),
    RandomFloatRange(-5, 5),
    0
  );
  IfcContainer.AddContainedElement(Wall);

  // add opening to the wall
  Opening := TIfcOpeningElement.Create(IfcFile);
  Opening.PredefinedType := TIfcOpeningElementTypeEnum.Opening;
  Opening.AddBoxRepresentation(IfcFile.Project.ModelContext,
    Box3D(
      Vector3(- WindowSize.X / 2, -WallSize.Y / 2, - WindowSize.Y / 2),
      Vector3(  WindowSize.X / 2,  WallSize.Y / 2,   WindowSize.Y / 2)
    ));
  Opening.Translation := Vector3(
    WindowPosition.X,
    0,
    WindowPosition.Y
  );
  Wall.AddOpening(Opening);

  // Add window
  Window := TIfcWindow.Create(IfcFile);
  Window.Name := 'Window' + IntToStr(NextProductNumber);
  Inc(NextProductNumber);
  Window.AddBoxRepresentation(IfcFile.Project.ModelContext,
    Box3D(
      Vector3(- WindowSize.X / 2, -WallSize.Y / 2, - WindowSize.Y / 2),
      Vector3(  WindowSize.X / 2,  WallSize.Y / 2,   WindowSize.Y / 2)
    ));

  { How to position window?

    1. Completely manually:
        Window.Translation := Wall.Translation + Opening.Translation;
        // no Window.SetTransformRelativeTo(...)

      Valid, but then changing the wall position leaves the window behind
      "hanging in the air".

    2. Relative to wall:
        Window.Translation := Opening.Translation;
        Window.SetTransformRelativeTo(Wall);

      Valid and seems most functional.

    3. Relative to the opening:
        // no need to set this: Window.Translation := ...
        Window.SetTransformRelativeTo(Opening);

      Seems not valid.
      Crashes BonsaiBIM.
      In FreeCAD, no crash, but window is not visible.
  }
  Window.Translation := Opening.Translation;
  Window.SetTransformRelativeTo(Wall);

  IfcContainer.AddContainedElement(Window);

  { Connect wall and window, as spec suggests
    https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWall.htm :
    "Walls with openings that have already been modeled within
    the enclosing geometry may use the relationship IfcRelConnectsElements
    to associate the wall with embedded elements such as doors and windows." }
  Wall.AddConnected(Window);

  IfcMapping.Update(IfcFile);
  UpdateHierarchy;
end;

procedure TViewMain.ClickModifyRandomElement(Sender: TObject);
var
  ElementList: TIfcElementList;
  RandomElement: TIfcElement;
begin
  ElementList := TIfcElementList.Create(false);
  try
    IfcContainer.GetContainedElements(ElementList);
    if ElementList.Count = 0 then
      Exit;
    RandomElement := ElementList[Random(ElementList.Count)];
    if RandomElement.TransformSupported then
    begin
      RandomElement.Translation := Vector3(
        RandomFloatRange(-5, 5),
        RandomFloatRange(-5, 5),
        0
      );
      WritelnLog('Modified element "%s" placement', [RandomElement.Name]);
    end else
      WritelnWarning('Element "%s" is not positioned relative to parent, cannot modify', [
        RandomElement.Name
      ]);
  finally FreeAndNil(ElementList) end;

  IfcMapping.Update(IfcFile);

  { IfcSelectedProductShapeTranslation is no longer valid because
    IfcSelectedProduct moved, so cancel selection. }
  if RandomElement = IfcSelectedProduct then
  begin
    IfcSelectedProduct := nil;
    TransformManipulate.SetSelected([]);
  end;
end;

procedure TViewMain.ClickChangeWireframeEffect(Sender: TObject);
begin
  if IfcScene.RenderOptions.WireframeEffect = High(TWireframeEffect) then
    IfcScene.RenderOptions.WireframeEffect := Low(TWireframeEffect)
  else
    IfcScene.RenderOptions.WireframeEffect := Succ(IfcScene.RenderOptions.WireframeEffect);
  LabelWireframeEffect.Caption := WireframeEffectToStr(IfcScene.RenderOptions.WireframeEffect);
end;

procedure TViewMain.ButtonHierarchyClick(Sender: TObject);
var
  NewSelectedObject: TIfcObjectDefinition;
  NewSelectedProduct: TIfcProduct;
begin
  NewSelectedObject := TIfcObjectDefinition(TCastleButton(Sender).Tag);
  if NewSelectedObject is TIfcProduct then
  begin
    NewSelectedProduct := TIfcProduct(NewSelectedObject);
    ChangeSelectedProduct(NewSelectedProduct);
  end;
end;

type
  TButtonHierarchyDesign = class(TPersistent)
  published
    LabelHierarchyItemName: TCastleLabel;
  end;

procedure TViewMain.UpdateHierarchy;
const
  Indent = '  ';

  procedure ShowHierarchy(const RelationName: String;
    const Parent: TIfcObjectDefinition; const NowIndent: String);
  var
    ParentSpatial: TIfcSpatialElement;
    RelAggregates: TIfcRelAggregates;
    RelatedObject: TIfcObjectDefinition;
    RelContained: TIfcRelContainedInSpatialStructure;
    RelatedProduct: TIfcProduct;
    ParentElement: TIfcElement;
    RelVoidsElement: TIfcRelVoidsElement;
    S: String;
    Button: TCastleButton;
    ButtonHierarchybDesign: TButtonHierarchyDesign;
  begin
    S := Parent.ClassName + ' "' + Parent.Name + '"';
    if RelationName <> '' then
      S := RelationName + ': ' + S;
    S := NowIndent + S;
    if Parent = IfcFile.Project.BestContainer then
      S := S + NL + NowIndent + Indent + '<font color="#0000aa">(^detected best container)</font>';
    if (Parent is TIfcProduct) and
       (not TIfcProduct(Parent).TransformSupported) then
      S := S + NL + NowIndent + Indent + '<font color="#aa0000">(^dragging may be not intuitive)</font>';

    ButtonHierarchybDesign := TButtonHierarchyDesign.Create;
    try
      Button := ButtonHierarchyFactory.ComponentLoad(FreeAtStop, ButtonHierarchybDesign) as TCastleButton;
      ButtonHierarchybDesign.LabelHierarchyItemName.Caption := S;
    finally FreeAndNil(ButtonHierarchybDesign) end;
    Button.Pressed := Parent = IfcSelectedProduct;
    Button.OnClick := {$ifdef FPC}@{$endif} ButtonHierarchyClick;
    Button.Tag := PtrInt(Parent);
    GroupHierarchy.InsertFront(Button);

    for RelAggregates in Parent.IsDecomposedBy do
      for RelatedObject in RelAggregates.RelatedObjects do
        ShowHierarchy('IsDecomposedBy', RelatedObject, NowIndent + Indent);

    if Parent is TIfcSpatialElement then
    begin
      ParentSpatial := TIfcSpatialElement(Parent);
      for RelContained in ParentSpatial.ContainsElements do
        for RelatedProduct in RelContained.RelatedElements do
          ShowHierarchy('ContainsElements', RelatedProduct, NowIndent + Indent);
    end;

    if Parent is TIfcElement then
    begin
      ParentElement := TIfcElement(Parent);
      for RelVoidsElement in ParentElement.HasOpenings do
        ShowHierarchy('HasOpenings', RelVoidsElement.RelatedOpeningElement, NowIndent + Indent);
    end;
  end;

var
  RepresentationContext: TIfcRepresentationContext;
  SList: TStringList;
begin
  { Showing the IFC hierarchy, as seen by Castle Game Engine,
    is a useful debug tool (both for CGE and for your IFC models).
    We show various relations (like IsDecomposedBy, ContainsElements),
    we show the representation contexts, we mark what was detected
    as Project.ModelContext, Project.PlanContext, Project.BestContainer. }

  SList := TStringList.Create;
  try
    SList.Add('<font color="#008800">Representation contexts:</font>');

    for RepresentationContext in IfcFile.Project.RepresentationContexts do
    begin
      SList.Add(Indent + RepresentationContext.ClassName);
      if RepresentationContext = IfcFile.Project.ModelContext then
        SList.Add(Indent + Indent + '<font color="#0000aa">(^detected 3D ModelContext)</font>');
      if RepresentationContext = IfcFile.Project.PlanContext then
        SList.Add(Indent + Indent + '<font color="#0000aa">(^detected 2D PlanContext)</font>');
    end;

    SList.Add('');
    SList.Add('<font color="#008800">Project Hierarchy:</font>');
    LabelHierarchy.Text.Assign(SList);
  finally FreeAndNil(SList) end;

  GroupHierarchy.ClearControls;
  ShowHierarchy('', IfcFile.Project, Indent);
end;

function TViewMain.ProductBoundingBox(const Product: TIfcProduct): TBox3D;
var
  Shapes: TX3DNodeList;
  Shape: TX3DNode;
begin
  Result := TBox3D.Empty;
  Shapes := IfcMapping.ProductToNodes(Product);
  try
    for Shape in Shapes do
      Result.Include(IfcScene.ShapeBoundingBox(Shape as TAbstractShapeNode));
  finally FreeAndNil(Shapes) end;
end;

procedure TViewMain.ChangeSelectedProduct(const NewSelectedProduct: TIfcProduct);
var
  ProductBox: TBox3D;
begin
  if NewSelectedProduct <> IfcSelectedProduct then
  begin
    IfcSelectedProduct := NewSelectedProduct;
    UpdateHierarchy;

    // Update TransformManipulate, to allow dragging selected product, if any

    if IfcSelectedProduct <> nil then
    begin
      ProductBox := ProductBoundingBox(IfcSelectedProduct);
      if not ProductBox.IsEmpty then
      begin
        { Note that we ignore IfcSelectedProduct.TransformSupported,
          allow dragging always.
          User can see TransformSupported=false in sidebar, if we have weird
          transformation situation. }
        TransformSelectedProduct.Translation := ProductBox.Center;
        IfcSelectedProductShapeTranslation := TransformSelectedProduct.Translation;
        TransformManipulate.SetSelected([TransformSelectedProduct]);
      end else
        TransformManipulate.SetSelected([]);
    end else
      TransformManipulate.SetSelected([]);
  end;
end;

procedure TViewMain.SetMouseMode(const LeftSelectsRightNavigates: Boolean);
begin
  ButtonMouseLeftNavigates.Pressed := not LeftSelectsRightNavigates;
  ButtonMouseLeftSelects.Pressed := LeftSelectsRightNavigates;

  if LeftSelectsRightNavigates then
    { Rotate by dragging with right mouse button,
      because we use left mouse button for selection. }
    ExamineNavigation.Input_Rotate.MouseButton := buttonRight
  else
    ExamineNavigation.Input_Rotate.MouseButton := buttonLeft;
  ExamineNavigation.Input_Zoom.MouseButtonUse := false;

  if LeftSelectsRightNavigates then
    MouseButtonToSelect := buttonLeft
  else
    MouseButtonToSelect := buttonRight;
end;

procedure TViewMain.ClickMouseLeftNavigates(Sender: TObject);
begin
  SetMouseMode(false);
end;

procedure TViewMain.ClickMouseLeftSelects(Sender: TObject);
begin
  SetMouseMode(true);
end;

procedure TViewMain.MainViewportPress(const Sender: TCastleUserInterface;
  const Event: TInputPressRelease; var Handled: Boolean);
var
  HitInfo: TRayCollisionNode;
  HitShape: TAbstractShapeNode;
  NewSelectedProduct: TIfcProduct;
begin
  if Event.IsMouseButton(MouseButtonToSelect) then
  begin
    { Select new IFC product by extracting from MainViewport.MouseRayHit
      information about the selected X3D shape (HitShape) and then converting it
      to the IFC product (using IfcMapping.NodeToProduct). }
    if (MainViewport.MouseRayHit <> nil) and
       MainViewport.MouseRayHit.Info(HitInfo) then
    begin
      if (HitInfo.Item = IfcScene) and
         (HitInfo.Triangle <> nil) and
         (HitInfo.Triangle^.ShapeNode <> nil) then
      begin
        HitShape := HitInfo.Triangle^.ShapeNode;
        NewSelectedProduct := IfcMapping.NodeToProduct(HitShape);
      end else
        { If we hit something, but it's not IFC scene, abort handling
          this click. This may be a start of dragging on TransformManipulate,
          we don't want to interfere with it. }
        Exit;
    end else
      NewSelectedProduct := nil;

    ChangeSelectedProduct(NewSelectedProduct);

    Handled := true;
  end;
end;

procedure TViewMain.TransformManipulateTransformModified(Sender: TObject);
begin
  Assert(IfcSelectedProduct <> nil, 'TransformManipulateTransformModified called without IfcSelectedProduct, should not happen');

  { Update IfcSelectedProduct.Translation based on
    difference in shift of TransformSelectedProduct. Note that this simple way
    to update translation assumes we don't have rotations or scale in IFC. }
  IfcSelectedProduct.Translation :=
    IfcSelectedProduct.Translation +
    TransformSelectedProduct.Translation -
    IfcSelectedProductShapeTranslation;
  IfcMapping.Update(IfcFile);
  IfcSelectedProductShapeTranslation := TransformSelectedProduct.Translation;
end;

procedure TViewMain.IfcDebugDisplayChange(Sender: TObject);
begin
  IfcScene.RootNode.EnumerateNodes(TShapeNode,
    {$ifdef FPC}@{$endif} IfcDebugDisplayToggleShape, false);
end;

procedure TViewMain.IfcDebugDisplayToggleShape(Node: TX3DNode);
var
  ShapeNode: TShapeNode;
begin
  { Change the visibility of shapes representing IfcBoundingBox
    to match current state of CheckboxIfcDebugDisplay. }
  ShapeNode := Node as TShapeNode; // our EnumerateNodes call finds only TShapeNode
  if ShapeNode.MetadataString['IFC_ClassName'] = 'TIfcBoundingBox' then
    ShapeNode.Visible := CheckboxIfcDebugDisplay.Checked;
end;

procedure TViewMain.ClickViewEntireModel(Sender: TObject);
var
  Box: TBox3D;
  CameraDir, CameraPos: TVector3;
begin
  Box := IfcScene.WorldBoundingBox;
  if Box.IsEmpty then
    Exit;

  { Add some margin around the model, making all sizes non-zero.
    Makes good camera view even when the model is a flat plane. }
  Box := Box.Grow(1);

  CameraDir := Box.Center - Box.Max;
  CameraPos := Box.Max - CameraDir * 2;
  MainViewport.Camera.AnimateTo(CameraPos, CameraDir, TVector3.One[1], 0.25);
end;

end.
