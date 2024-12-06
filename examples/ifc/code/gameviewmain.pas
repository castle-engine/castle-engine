{
  Copyright 2024-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleScene,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleIfc;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonNew, ButtonLoad, ButtonSaveIfc, ButtonSaveNode,
      ButtonAddWall, ButtonModifyWall: TCastleButton;
    IfcScene: TCastleScene;
  private
    IfcFile: TIfcFile;
    { New products have to be added to this container.
      You cannot just add them to IfcFile.Project,
      IFC specification constaints what can be the top-level spatial element.
      TODO: initilalize this for new files. }
    IfcContainer: TIfcSpatialElement;
    IfcMapping: TCastleIfcMapping;

    { Used to assing unique names to walls. }
    NextWallNumber: Cardinal;

    { Create new IfcMapping instance and update what IfcScene shows,
      based on IfcFile contents.
      Use this after completely changing the IfcFile contents
      (like loading new file, or creating new file). }
    procedure NewIfcMapping(const NewIfcFile: TIfcFile);

    procedure ClickNew(Sender: TObject);
    procedure ClickLoad(Sender: TObject);
    procedure ClickSaveIfc(Sender: TObject);
    procedure ClickSaveNode(Sender: TObject);
    procedure ClickAddWall(Sender: TObject);
    procedure ClickModifyWall(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleUriUtils, CastleWindow, CastleBoxes, X3DLoad, CastleLog;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ClickNew(nil);
  ButtonNew.OnClick := {$ifdef FPC}@{$endif} ClickNew;
  ButtonLoad.OnClick := {$ifdef FPC}@{$endif} ClickLoad;
  ButtonSaveIfc.OnClick := {$ifdef FPC}@{$endif} ClickSaveIfc;
  ButtonSaveNode.OnClick := {$ifdef FPC}@{$endif} ClickSaveNode;
  ButtonAddWall.OnClick := {$ifdef FPC}@{$endif} ClickAddWall;
  ButtonModifyWall.OnClick := {$ifdef FPC}@{$endif} ClickModifyWall;
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
    Reason: We cannot add products directly to IfcProject, we need
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
  Url := 'castle-data:/';
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
  Wall.Name := 'Wall' + IntToStr(NextWallNumber);
  Inc(NextWallNumber);

  SizeX := RandomFloatRange(1, 4);
  SizeY := RandomFloatRange(1, 4);
  WallHeight := RandomFloatRange(1.5, 2.5); // Z is "up", by convention, in IFC
  Wall.AddBoxRepresentation(IfcFile.Project.ModelContext,
    Box3D(
      Vector3(-SizeX / 2, -SizeY / 2, 0),
      Vector3( SizeX / 2,  SizeY / 2, WallHeight)
    ));

  Wall.SetRelativePlacement(Vector3(
    RandomFloatRange(-5, 5),
    RandomFloatRange(-5, 5),
    0
  ));

  IfcContainer.AddContainedElement(Wall);
  IfcMapping.Update(IfcFile);
end;

procedure TViewMain.ClickModifyWall(Sender: TObject);
begin
  // IfcFile.Project.... // TODO
  IfcMapping.Update(IfcFile);
end;

end.
