{
  Copyright 2016-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game view where you actually play a game. }
unit GameViewPlay;

interface

uses Classes,
  CastleControls, CastleUIControls, CastleViewport, CastleSceneCore, CastleScene,
  CastleCameras, CastleKeysMouse, X3DNodes, CastleVectors;

type
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    MainViewport, MapViewport: TCastleViewport;
    ButtonBack: TCastleButton;
  strict private
    { TMaterialInfo of the triangle under mouse. }
    HighlightedMaterial: TMaterialInfo;
    HighlightedMaterialInitialColor: TVector3;
    { Do we current point to some enemy (looks at MainViewport.TriangleHit). }
    function HitEnemy(out MaterialInfo: TMaterialInfo; out Male: Boolean): Boolean;
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

implementation

uses CastleColors, CastleFilesUtils, CastleUtils, CastleTriangles, CastleShapes,
  CastleComponentSerialize, CastleStringUtils,
  GameViewMainMenu, GameViewAskDialog;

{ TViewPlay ------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;

  { turn off head bobbing, it makes a feeling that sprites sometimes "tremble" }
//  WalkNavigation.HeadBobbing := 0;

  // see https://castle-engine.io/multiple_viewports_to_display_one_world
  MapViewport.Items.Remove(MapViewport.Camera);
  MapViewport.Items := MainViewport.Items;
  MapViewport.Items.Add(MapViewport.Camera);

  MapViewport.Camera.SetView(
    Vector3(5, 92.00, 0.99),
    Vector3(0, -1, 0),
    Vector3(0, 0, 1));

  ButtonBack.OnClick := {$ifdef FPC}@{$endif}ClickBack;
end;

procedure TViewPlay.ClickBack(Sender: TObject);
begin
  Container.View := TViewMainMenu.CreateUntilStopped;
end;

function TViewPlay.HitEnemy(out MaterialInfo: TMaterialInfo; out Male: Boolean): Boolean;
const
  { Names below correspond to how materials have been named in Blender.
    Blender puts them in glTF material names,
    and our importer puts them in TAppearanceNode.X3DName. }
  MaterialPrefix: array[Boolean] of String = (
    'female_zombie_material',
    'male_zombie_material'
  );
var
  Triangle: PTriangle;
  Appearance: TAppearanceNode;
begin
  // default out values
  MaterialInfo := nil;
  Male := false;

  Triangle := MainViewport.TriangleHit;
  // abort if triangle has no detailed information (e.g. PreciseCollisions=false)
  if Triangle = nil then Exit(false);

  // abort if triangle has no shape node (this can happen only if model is VRML 1.0 now)
  if Triangle^.ShapeNode = nil then Exit(false);

  Appearance := Triangle^.ShapeNode.Appearance;
  if Appearance = nil then Exit(false);

  MaterialInfo := Triangle^.MaterialInfo;
  if MaterialInfo = nil then Exit(false);

  Result :=
    IsPrefix(MaterialPrefix[false], Appearance.X3DName, false) or
    IsPrefix(MaterialPrefix[true] , Appearance.X3DName, false);
  if Result then
  begin
    Male := IsPrefix(MaterialPrefix[true], Appearance.X3DName, false);
  end;
end;

function TViewPlay.Press(const Event: TInputPressRelease): boolean;
var
  EnemyMale: Boolean;
  IgnoreMaterial: TMaterialInfo;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    if HitEnemy(IgnoreMaterial, EnemyMale) then
    begin
      ViewAskDialog.Male := EnemyMale;
      Container.PushView(ViewAskDialog);
      Exit(true);
    end;
  end;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  NewMaterial: TMaterialInfo;
  IgnoreMale: Boolean;
begin
  inherited;

  { update HighlightedMaterial, changing colors of previously and newly
    selected enemy. }
  if not HitEnemy(NewMaterial, IgnoreMale) then
    NewMaterial := nil;
  if HighlightedMaterial <> NewMaterial then
  begin
    if HighlightedMaterial <> nil then
      HighlightedMaterial.MainColor := HighlightedMaterialInitialColor;
    HighlightedMaterial := NewMaterial;
    if HighlightedMaterial <> nil then
    begin
      HighlightedMaterialInitialColor := HighlightedMaterial.MainColor;
      HighlightedMaterial.MainColor := Vector3(1.5, 1.5, 1); // bump yellowish
    end;
  end;
end;

end.
