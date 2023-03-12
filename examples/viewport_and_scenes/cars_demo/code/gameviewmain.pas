{
  Copyright 2010-2023 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTransform,
  CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    RoadScene: TCastleScene;
    MainViewport: TCastleViewport;
  private
    CarScene: TCastleScene;
    CarTransforms: array [1..20] of TCastleTransform;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleBoxes, X3DNodes, CastleUtils;

{ TCarBehavior --------------------------------------------------------------- }

type
  TCarBehavior = class(TCastleBehavior)
  public
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

procedure TCarBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  T: TVector3;
begin
  T := Parent.Translation;
  { Thanks to multiplying by SecondsPassed, it is a time-based operation,
    and will always move 40 units / per second along the +Z axis. }
  T := T + Vector3(0, 0, 40) * SecondsPassed;
  { Wrap the Z position, to move in a loop }
  if T.Z > 70 then
    T.Z := -50;
  Parent.Translation := T;
end;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  function CreateAdditionalMesh: TCastleScene;
  var
    Coord: TCoordinateNode;
    TexCoord: TTextureCoordinateNode;
    IndexedFaceSet: TIndexedFaceSetNode;
    BaseTexture: TImageTextureNode;
    Material: TPhysicalMaterialNode;
    Appearance: TAppearanceNode;
    Shape: TShapeNode;
    Transform: TTransformNode;
    RootNode: TX3DRootNode;
  begin
    Coord := TCoordinateNode.Create;
    Coord.SetPoint([
      Vector3(-15.205387, -66.775894, -0.092525),
      Vector3(9.317978, -66.775894, -0.092525),
      Vector3(-15.205387, -68.674622, -0.092525),
      Vector3(9.317978, -68.674622, -0.092525),
      Vector3(9.317978, -78.330063, 3.456294),
      Vector3(-15.205387, -78.330063, 3.456294),
      Vector3(9.317978, -80.814240, 7.241702),
      Vector3(-15.205387, -80.814240, 7.241702)
    ]);

    TexCoord := TTextureCoordinateNode.Create;
    TexCoord.SetPoint([
      Vector2(0.0001, 0.9964),
      Vector2(1.0000, 0.9964),
      Vector2(1.0000, 0.8541),
      Vector2(0.0001, 0.8541),
      Vector2(0.0001, 0.7118),
      Vector2(1.0000, 0.7118),
      Vector2(1.0000, 0.5695),
      Vector2(0.0001, 0.5695),
      Vector2(0.0001, 0.5695),
      Vector2(1.0000, 0.5695),
      Vector2(1.0000, 0.4272),
      Vector2(0.0001, 0.4272)
    ]);

    IndexedFaceSet := TIndexedFaceSetNode.Create;
    IndexedFaceSet.Coord := Coord;
    IndexedFaceSet.TexCoord := TexCoord;
    IndexedFaceSet.SetTexCoordIndex([0, 1, 2, 3, -1, 4, 5, 6, 7, -1, 8, 9, 10, 11, -1]);
    IndexedFaceSet.SetCoordIndex([0, 1, 3, 2, -1, 2, 3, 4, 5, -1, 5, 4, 6, 7, -1]);
    IndexedFaceSet.Solid := false; // make it visible from both sides

    BaseTexture := TImageTextureNode.Create;
    BaseTexture.SetUrl(['castle-data:/textures/tunnel_road.jpg']);

    Material := TPhysicalMaterialNode.Create;
    Material.BaseTexture := BaseTexture;
    Material.BaseColor := Vector3(1, 1, 0); // yellow

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    Shape := TShapeNode.Create;
    Shape.Geometry := IndexedFaceSet;
    Shape.Appearance := Appearance;

    Transform := TTransformNode.Create;
    Transform.Translation := Vector3(0, 0, 0);
    Transform.Rotation := Vector4(1, 0, 0, -Pi / 2);
    Transform.AddChildren(Shape);

    RootNode := TX3DRootNode.Create;
    RootNode.AddChildren(Transform);

    Result := TCastleScene.Create(FreeAtStop);
    Result.Load(RootNode, true);
  end;

var
  I: Integer;
begin
  inherited;

  MainViewport.Items.Add(CreateAdditionalMesh);

  CarScene := TCastleScene.Create(FreeAtStop);
  CarScene.Load('castle-data:/car.gltf');
  CarScene.PreciseCollisions := true;
  CarScene.PlayAnimation('wheels_turning', true);

  for I := Low(CarTransforms) to High(CarTransforms) do
  begin
    CarTransforms[I] := TCastleTransform.Create(FreeAtStop);
    CarTransforms[I].Translation := Vector3(
       (Random(4) - 2) * 6, 0, RandomFloatRange(-70, 50));
    CarTransforms[I].Add(CarScene);
    CarTransforms[I].AddBehavior(TCarBehavior.Create(FreeAtStop));
    MainViewport.Items.Add(CarTransforms[I]);
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyR) then
  begin
    RoadScene.Exists := not RoadScene.Exists;
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
