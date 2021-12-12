{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTransform,
  CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    RoadScene: TCastleScene;
    MainViewport: TCastleViewport;

    CarScene: TCastleScene;
    CarTransforms: array [1..20] of TCastleTransform;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleBoxes, X3DNodes, CastleUtils;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  function CreateBoxesScene: TCastleScene;
  const
    WallHeight = 5;
  var
    RoadBox: TBox3D;
    RootNode: TX3DRootNode;
    Appearance: TAppearanceNode;
    Material: TMaterialNode;
    Shape1, Shape2: TShapeNode;
    Box1, Box2: TBoxNode;
    Transform1, Transform2: TTransformNode;
  begin
    { The created geometry will automatically adjust to the bounding box
      of the road 3D model. }
    RoadBox := RoadScene.BoundingBox;
    if RoadBox.IsEmpty then
      raise Exception.Create('Invalid road 3D model: empty bounding box');

    Material := TMaterialNode.Create;
    { Yellow (we could have also used YellowRGB constant from CastleColors unit) }
    Material.DiffuseColor := Vector3(1, 1, 0);
    Material.Transparency := 0.75;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    { Note: you could use TBoxNode.CreateWithTransform shortcut method
      to create Box1, Shape1, Transform1 in one instruction.
      But we show the longer version below, as it's easier to understand. }

    Box1 := TBoxNode.Create('box_1_geometry');
    Box1.Size := Vector3(0.5, WallHeight, RoadBox.Size.Z);

    Shape1 := TShapeNode.Create('box_1_shape');
    Shape1.Appearance := Appearance;
    Shape1.Geometry := Box1;

    Transform1 := TTransformNode.Create('box_1_transform');
    Transform1.Translation := Vector3(RoadBox.Min.X, WallHeight / 2, RoadBox.Center.Z);
    Transform1.AddChildren(Shape1);

    Box2 := TBoxNode.Create('box_2_geometry');
    Box2.Size := Vector3(0.5, WallHeight, RoadBox.Size.Z);

    Shape2 := TShapeNode.Create('box_2_shape');
    { Reuse the same Appearance node for another shape.
      This is perfectly allowed (the X3D is actually a graph, not a tree). }
    Shape2.Appearance := Appearance;
    Shape2.Geometry := Box2;

    Transform2 := TTransformNode.Create('box_2_transform');
    Transform2.Translation := Vector3(RoadBox.Max.X, WallHeight / 2, RoadBox.Center.Z);
    Transform2.AddChildren(Shape2);

    RootNode := TX3DRootNode.Create;
    RootNode.AddChildren(Transform1);
    RootNode.AddChildren(Transform2);

    Result := TCastleScene.Create(FreeAtStop);
    Result.Load(RootNode, true);
  end;

var
  I: Integer;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  RoadScene := DesignedComponent('RoadScene') as TCastleScene;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;

  MainViewport.Items.Add(CreateBoxesScene);

  CarScene := TCastleScene.Create(FreeAtStop);
  CarScene.Load('castle-data:/car.gltf');
  CarScene.Spatial := [ssRendering, ssDynamicCollisions];
  CarScene.PlayAnimation('wheels_turning', true);

  for I := Low(CarTransforms) to High(CarTransforms) do
  begin
    CarTransforms[I] := TCastleTransform.Create(FreeAtStop);
    CarTransforms[I].Translation := Vector3(
       (Random(4) - 2) * 6, 0, RandomFloatRange(-70, 50));
    CarTransforms[I].Add(CarScene);
    MainViewport.Items.Add(CarTransforms[I]);
  end;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure UpdateCarTransform(const CarTransform: TCastleTransform);
  var
    T: TVector3;
  begin
    T := CarTransform.Translation;
    { Thanks to multiplying by SecondsPassed, it is a time-based operation,
      and will always move 40 units / per second along the +Z axis. }
    T := T + Vector3(0, 0, 40) * Container.Fps.SecondsPassed;
    { Wrap the Z position, to move in a loop }
    if T.Z > 70 then
      T.Z := -50;
    CarTransform.Translation := T;
  end;

var
  I: Integer;
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  for I := Low(CarTransforms) to High(CarTransforms) do
    UpdateCarTransform(CarTransforms[I]);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey('c') then
  begin
    CarTransforms[1].Exists := not CarTransforms[1].Exists;
    Exit(true);
  end;

  { capture a screenshot }
  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;
end;

end.
