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
  CastleVectors, CastleComponentSerialize, CastleTimeUtils,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  X3DNodes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps, LabelMeshInfo: TCastleLabel;
    MainScene: TCastleScene;
    CheckboxWireframe: TCastleCheckbox;
  private
    const
      GridWidth = 100;
      GridHeight = 100;
    var
      CoordinateNode: TCoordinateNode;
      Time: TFloatTime;
    { Update CoordinateNode vertexes by CoordinateNode.SetPoint,
      which can be called every frame and the new coordinates are efficiently
      updated on the mesh. }
    procedure UpdateCoordinateNode;
    procedure WireframeChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.UpdateCoordinateNode;
var
  Vertexes: TVector3List;
  X, Z: Integer;
  H: Single;
begin
  Vertexes := TVector3List.Create;
  try
    Vertexes.Count := (GridWidth + 1) * (GridHeight + 1);
    for X := 0 to GridWidth do
      for Z := 0 to GridHeight do
      begin
        // the numbers below are nothing special, just chosen experimentally
        H := ( Sin((X + Time * 20.0) * 0.5) +
               Sin((Z + Time * 15.0) * 0.3) ) * 0.5;
        Vertexes[(GridWidth + 1) * Z + X] := Vector3(X, H, Z);
      end;
    CoordinateNode.SetPoint(Vertexes);
  finally FreeAndNil(Vertexes) end;
end;

procedure TViewMain.Start;

  { Create X3D nodes that define a grid.

    In particular initialize CoordinateNode that can be later updated
    as often as we want by "CoordinateNode.SetPoint"
    (done from our method "UpdateCoordinateNode"). }
  procedure BuildMainScene;
  var
    RootNode: TX3DRootNode;
    Triangles: TIndexedTriangleSetNode;
    Indexes: TInt32List;
    Material: TPhysicalMaterialNode;
    Appearance: TAppearanceNode;
    Shape: TShapeNode;
    X, Z: Integer;
  begin
    CoordinateNode := TCoordinateNode.Create;
    UpdateCoordinateNode;

    { Define triangles to form a grid.
      We have GridWidth * GridHeight quads, each split into 2 triangles. }
    Indexes := TInt32List.Create;
    for X := 1 to GridWidth do
      for Z := 1 to GridHeight do
      begin
        Indexes.Add((GridWidth + 1) * Z + X);
        Indexes.Add((GridWidth + 1) * (Z - 1) + X);
        Indexes.Add((GridWidth + 1) * (Z - 1) + X - 1);

        Indexes.Add((GridWidth + 1) * Z + X);
        Indexes.Add((GridWidth + 1) * (Z - 1) + X - 1);
        Indexes.Add((GridWidth + 1) * Z + X - 1);
      end;

    Triangles := TIndexedTriangleSetNode.Create;
    Triangles.Coord := CoordinateNode;
    Triangles.SetIndex(Indexes);

    Material := TPhysicalMaterialNode.Create;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    Shape := TShapeNode.Create;
    Shape.Geometry := Triangles;
    Shape.Appearance := Appearance;

    RootNode := TX3DRootNode.Create;
    RootNode.AddChildren(Shape);

    MainScene.Load(RootNode, true);
    MainScene.Translation := Vector3(-GridWidth / 2, 0, -GridHeight / 2);

    Assert(MainScene.TrianglesCount = Indexes.Count div 3);
    Assert(MainScene.TrianglesCount = GridWidth * GridHeight * 2);
  end;

begin
  inherited;

  CheckboxWireframe.OnChange := {$ifdef FPC}@{$endif} WireframeChange;

  BuildMainScene;

  // show triangles count
  LabelMeshInfo.Caption := Format('Grid size %d x %d, triangles: %d', [
    GridWidth,
    GridHeight,
    MainScene.TrianglesCount
  ]);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  Time := Time + SecondsPassed;
  UpdateCoordinateNode;
end;

procedure TViewMain.WireframeChange(Sender: TObject);
begin
  if CheckboxWireframe.Checked then
    MainScene.RenderOptions.WireframeEffect := weSolidWireframe
  else
    MainScene.RenderOptions.WireframeEffect := weNormal;
end;

end.
