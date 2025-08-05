{
  Copyright 2025-2025 Michalis Kamburelis.

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

// Define COMPILER_CASE_ANALYSIS to be able to avoid some FPC warnings.
// See https://castle-engine.io/coding_conventions#case_analysis .
{$ifdef FPC}
  {$ifndef VER3_0}
    {$ifndef VER3_1}
      {$define COMPILER_CASE_ANALYSIS}
    {$endif}
  {$endif}
{$endif}

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTerrain,
  CastleViewport, CastleTransform;

type
  TTerrainOperation = (
    toRaise,
    toLower,
    toFlatten
  );

  TTerrainOperations = set of TTerrainOperation;

  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;

    Terrain: TCastleTerrain;
    TerrainImage: TCastleTerrainImage;
    Viewport: TCastleViewport;

    StrengthSlider: TCastleIntegerSlider;
    BrushMaxHeightSlider: TCastleIntegerSlider;
    BrushSizeSlider: TCastleIntegerSlider;
    RingThicknessSlider: TCastleFloatSlider;
    BrushRotationSlider: TCastleFloatSlider;

    RaiseTerrainButton: TCastleButton;
    LowerTerrainButton: TCastleButton;
    FlattenTerrainButton: TCastleButton;

    FixedSquareBrushButton: TCastleButton;
    SquareBrushButton: TCastleButton;
    PyramidBrushButton: TCastleButton;
    CircleBrushButton: TCastleButton;
    ConeBrushButton: TCastleButton;
    RingBrushButton: TCastleButton;
    LyingCylinderBrushButton: TCastleButton;

    RotationPlus15DegButton: TCastleButton;

    SetHeightMapSizeButton: TCastleButton;
    HeightMapWidthIntegerEdit: TCastleIntegerEdit;
    HeightMapHeightIntegerEdit: TCastleIntegerEdit;

    HorizontalGroupMaxHeight: TCastleHorizontalGroup;

    HorizontalGroupFlattenHeight: TCastleHorizontalGroup;
    BrushFlattenHeightSlider: TCastleIntegerSlider;

    LabelOperation: TCastleLabel;
    LabelTerrainMode: TCastleLabel;

    SaveHeightMapUrlEdit: TCastleEdit;
    SaveHeightMapButton: TCastleButton;

    ChangeModeButton: TCastleButton;
  private
    Operation: TTerrainOperation;
    FBrush: TCastleTerrainBrush;
    IsFirstFramePressed: Boolean;
    procedure OperationClick(Sender: TObject);
    procedure BrushTypeClick(Sender: TObject);
    procedure RotationPlus15DegButtonClick(Sender: TObject);
    procedure SaveHeightMapClick(Sender: TObject);
    procedure ChangeModeClick(Sender: TObject);
    procedure UpdateOperationAndBrushLabel;
    function BrushToString(const ABrush: TCastleTerrainBrush): String;
    function OperationToString(const AOperation: TTerrainOperation): String;
    procedure SetHeightMapSizeButtonClick(Sender: TObject);
    procedure UpdateTerrainMode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleLog, CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
  Operation := toRaise;
  FBrush := ctbCone;
  IsFirstFramePressed := true;
end;

procedure TViewMain.Start;
begin
  inherited;
  RaiseTerrainButton.OnClick := {$ifdef FPC}@{$endif} OperationClick;
  LowerTerrainButton.OnClick := {$ifdef FPC}@{$endif} OperationClick;
  FlattenTerrainButton.OnClick := {$ifdef FPC}@{$endif} OperationClick;

  FixedSquareBrushButton.OnClick := {$ifdef FPC}@{$endif} BrushTypeClick;
  SquareBrushButton.OnClick := {$ifdef FPC}@{$endif} BrushTypeClick;
  PyramidBrushButton.OnClick := {$ifdef FPC}@{$endif} BrushTypeClick;
  CircleBrushButton.OnClick := {$ifdef FPC}@{$endif} BrushTypeClick;
  ConeBrushButton.OnClick := {$ifdef FPC}@{$endif} BrushTypeClick;
  RingBrushButton.OnClick := {$ifdef FPC}@{$endif} BrushTypeClick;
  LyingCylinderBrushButton.OnClick := {$ifdef FPC}@{$endif} BrushTypeClick;
  RotationPlus15DegButton.OnClick := {$ifdef FPC}@{$endif} RotationPlus15DegButtonClick;

  SetHeightMapSizeButton.OnClick := {$ifdef FPC}@{$endif} SetHeightMapSizeButtonClick;

  SaveHeightMapButton.OnClick := {$ifdef FPC}@{$endif} SaveHeightMapClick;

  ChangeModeButton.OnClick := {$ifdef FPC}@{$endif} ChangeModeClick;

  UpdateOperationAndBrushLabel;
  UpdateTerrainMode;
end;

procedure TViewMain.OperationClick(Sender: TObject);
begin
  if Sender = RaiseTerrainButton then
  begin
    Operation := toRaise;
    HorizontalGroupMaxHeight.Exists := true;
    HorizontalGroupFlattenHeight.Exists := false;
  end
  else if Sender = LowerTerrainButton then
  begin
    Operation := toLower;
    HorizontalGroupMaxHeight.Exists := false;
    HorizontalGroupFlattenHeight.Exists := false;
  end
  else if Sender = FlattenTerrainButton then
  begin
    Operation := toFlatten;
    HorizontalGroupMaxHeight.Exists := false;
    HorizontalGroupFlattenHeight.Exists := true;
  end;

  UpdateOperationAndBrushLabel;
end;

procedure TViewMain.BrushTypeClick(Sender: TObject);
begin
  if Sender = FixedSquareBrushButton then
    FBrush := ctbFixedSquare
  else if Sender = SquareBrushButton then
    FBrush := ctbSquare
  else if Sender = PyramidBrushButton then
    FBrush := ctbPyramid
  else if Sender = CircleBrushButton then
    FBrush := ctbCircle
  else if Sender = ConeBrushButton then
    FBrush := ctbCone
  else if Sender = RingBrushButton then
    FBrush := ctbRing
  else if Sender = LyingCylinderBrushButton then
    FBrush := ctbLyingCylinder;

  UpdateOperationAndBrushLabel;
end;

procedure TViewMain.RotationPlus15DegButtonClick(Sender: TObject);
begin
  if BrushRotationSlider.Value + 15.0 > 360.0 then
    BrushRotationSlider.Value := 360.0
  else
    BrushRotationSlider.Value := BrushRotationSlider.Value + 15.0;
end;

procedure TViewMain.SaveHeightMapClick(Sender: TObject);
begin
  if Terrain.Mode = ctmMesh then
    ChangeModeClick(nil);
  Assert(Terrain.Mode = ctmShader);

  Terrain.EditMode.SaveEditModeHeightMap(SaveHeightMapUrlEdit.Text);
end;

procedure TViewMain.UpdateTerrainMode;
const
  TerainModeNames: array [TCastleTerrainMode] of String = (
    'Heights In Vertexes (Not Editable)',
    'Heights In Texture (Editable)'
  );
begin
  LabelTerrainMode.Caption := 'Terrain mode: ' + TerainModeNames[Terrain.Mode];
end;

procedure TViewMain.ChangeModeClick(Sender: TObject);
begin
  if Terrain.Mode = ctmMesh then
    Terrain.Mode := ctmShader
  else
    Terrain.Mode := ctmMesh;
  UpdateTerrainMode;
end;

procedure TViewMain.UpdateOperationAndBrushLabel;
begin
  LabelOperation.Caption := OperationToString(Operation) + ': ' + BrushToString(FBrush);
end;

function TViewMain.BrushToString(const ABrush: TCastleTerrainBrush): String;
const
  Names: array [TCastleTerrainBrush] of String = (
    'Fixed Square',
    'Square',
    'Pyramid',
    'Circle',
    'Cone',
    'Ring',
    'Lying Cylinder'
  );
begin
  Result := Names[ABrush];
end;

function TViewMain.OperationToString(const AOperation: TTerrainOperation): String;
const
  Names: array [TTerrainOperation] of String = (
    'Raise',
    'Lower',
    'Flatten'
  );
begin
  Result := Names[AOperation];
end;

procedure TViewMain.SetHeightMapSizeButtonClick(Sender: TObject);
begin
  if Terrain.Mode = ctmMesh then
    ChangeModeClick(nil);
  Assert(Terrain.Mode = ctmShader);

  Terrain.EditMode.SetEditModeHeightMapSize(Vector2Integer(
    HeightMapWidthIntegerEdit.Value,
    HeightMapHeightIntegerEdit.Value)
  );
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  RayCollision: TRayCollision;
  HitInfo: TRayCollisionNode;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if Container.MousePressed = [buttonLeft] then
  begin
    RayCollision := Viewport.MouseRayHit;
    if (RayCollision <> nil) and RayCollision.Info(HitInfo) then
    begin
      if Terrain.Mode = ctmMesh then
        ChangeModeClick(nil);
      Assert(Terrain.Mode = ctmShader);

      case Operation of
        toRaise:
          Terrain.EditMode.AlterTerrain(Container, HitInfo.Point, FBrush, BrushSizeSlider.Value,
            StrengthSlider.Value, DegToRad(BrushRotationSlider.Value),
            BrushMaxHeightSlider.Value, RingThicknessSlider.Value);
        toLower:
          Terrain.EditMode.AlterTerrain(Container, HitInfo.Point, FBrush, BrushSizeSlider.Value,
            StrengthSlider.Value, DegToRad(BrushRotationSlider.Value),
            0, RingThicknessSlider.Value);
        toFlatten:
          begin
            if IsFirstFramePressed then
              BrushFlattenHeightSlider.Value := Terrain.EditMode.TerrainHeight(HitInfo.Point);
            Terrain.EditMode.AlterTerrain(Container, HitInfo.Point, FBrush, BrushSizeSlider.Value,
              StrengthSlider.Value, DegToRad(BrushRotationSlider.Value),
              BrushFlattenHeightSlider.Value, RingThicknessSlider.Value);
          end;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('Operation not implemented');
        {$endif}
      end;

      IsFirstFramePressed := false;
      // Terrain.RaiseTerrain(HitInfo.Point, StrengthSlider.Value);
      //TerrainImage.SetHeight(Vector2(HitInfo.Point.X, -HitInfo.Point.Z), Vector2(HitInfo.Point.X, -HitInfo.Point.Z), 255);
    end;
  end else
    IsFirstFramePressed := true;

  if Container.MousePressed = [] then
  begin
    if Operation = toFlatten then
    begin
      RayCollision := Viewport.MouseRayHit;
      if (RayCollision <> nil) and RayCollision.Info(HitInfo) then
      begin
        BrushFlattenHeightSlider.Value := Terrain.EditMode.TerrainHeight(HitInfo.Point);
      end;
    end;
  end;

end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
// var
//   RayOrigin, RayDirection: TVector3;
//   RayCollision: TRayCollision;
//   HitInfo: TRayCollisionNode;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  {if Event.IsMouseButton(buttonLeft) then
  begin
    RayCollision := Viewport.MouseRayHit;
    WritelnLog('Distance :' + FloatToStr(RayCollision.Distance));
    WritelnLog('Transform :' + RayCollision.Transform.Name);
    if RayCollision.Info(HitInfo) then
    begin
      WritelnLog('Point: ' + HitInfo.Point.ToString);
      WritelnLog('Terrain.Size: ' + Terrain.Size.ToString);
      WritelnLog('Terrain.Translation ' + Terrain.Translation.ToString);

      TerrainImage.SetHeight(Vector2(HitInfo.Point.X, -HitInfo.Point.Z), Vector2(HitInfo.Point.X, -HitInfo.Point.Z), 255);
      //Terrain.Data := nil;
      //Terrain.Data := TerrainImage;
    end;

  end;}
end;

end.
