{
  Copyright 2023-2023 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleTransform, CastleTiledMap,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    TiledMap: TCastleTiledMap;
    ButtonOpen, ButtonAnimations: TCastleButton;
    CheckboxSmoothScaling, CheckboxSmoothScalingSafeBorder: TCastleCheckbox;
    MapCamera: TCastleCamera;
    TestMapObjects: TCastleTransform;
  private
    TiledAnimations: Boolean;
    procedure ClickOpen(Sender: TObject);
    procedure ClickAnimations(Sender: TObject);
    procedure CheckboxSmoothScalingChange(Sender: TObject);
    procedure CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
    procedure OpenMap(const MapUrl: String);
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
  CastleParameters, CastleRenderOptions, CastleWindow, CastleUriUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Assign events }
  ButtonOpen.OnClick := {$ifdef FPC}@{$endif} ClickOpen;
  ButtonAnimations.OnClick := {$ifdef FPC}@{$endif} ClickAnimations;
  CheckboxSmoothScaling.OnChange := {$ifdef FPC}@{$endif} CheckboxSmoothScalingChange;
  CheckboxSmoothScalingSafeBorder.OnChange := {$ifdef FPC}@{$endif} CheckboxSmoothScalingSafeBorderChange;

  { Synchronize initial checkbox state with map properties }
  CheckboxSmoothScaling.Checked := TiledMap.SmoothScaling ;
  CheckboxSmoothScalingSafeBorder.Checked := TiledMap.SmoothScalingSafeBorder;

  { Load the map from parameter or default. }
  if Parameters.High = 1 then
    OpenMap(Parameters[1])
  else
    OpenMap('castle-data:/maps/desert.tmx');
end;

procedure TViewMain.OpenMap(const MapUrl: String);
begin
  { Empty TestMapObjects, freeing all children.
    This removes the effect of pressing "T" on previously loaded map. }
  while TestMapObjects.Count > 0 do
    TestMapObjects[0].Free;

  TiledMap.Url := MapUrl;
  MapCamera.Translation := TVector3.Zero;
  MapCamera.Orthographic.Height := 1000; // resets zoom in/out

  TiledAnimations := true;
  ButtonAnimations.Caption := 'Stop Animations';
  ButtonAnimations.Enabled := TiledMap.HasAnimations;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickOpen(Sender: TObject);
var
  Url: String;
begin
  Url := TiledMap.Url;
  if Application.MainWindow.FileDialog('Open Map', Url, true, 'Tiled Map (*.tmx)|*.tmx|All Files|*') then
    OpenMap(Url);
end;

procedure TViewMain.ClickAnimations(Sender: TObject);
begin
  TiledAnimations := not TiledAnimations;
  if TiledAnimations then
  begin
    TiledMap.PlayAnimations;
    ButtonAnimations.Caption := 'Stop Animations';
  end else
  begin
    TiledMap.StopAnimations(false);
    ButtonAnimations.Caption := 'Play Animations';
  end;
end;

procedure TViewMain.CheckboxSmoothScalingChange(Sender: TObject);
begin
  TiledMap.SmoothScaling := CheckboxSmoothScaling.Checked;
end;

procedure TViewMain.CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
begin
  TiledMap.SmoothScalingSafeBorder := CheckboxSmoothScalingSafeBorder.Checked;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

  { Add a few rectangles on positions matching tiles on the map.
    You press T multiple times to add more rectangles. }
  procedure TestAddingComponents;

    { Create new component (simple randomly colored rectangle) to put on a map,
      placing it at (MapX, MapY).
      Given MapX, MapY are "tile coordinates", so
      - 0,0 is bottom-left tile of the map,
      - Map.Data.Width-1, Map.Data.Height-1 is top-right tile of the map. }
    function AddTestRectangle(const MapX, MapY: Integer;
      const ZInFrontMap: Single): TCastleTransform;
    var
      NewPlane: TCastlePlane;
      T: TVector2;
    begin
      NewPlane := TCastlePlane.Create(FreeAtStop);
      NewPlane.Axis := 2;
      NewPlane.Material := pmUnlit;
      NewPlane.Color := Vector4(Random, Random, Random, 1);
      NewPlane.Size := Vector2(TiledMap.Data.TileWidth, TiledMap.Data.TileHeight);

      // Using TileRenderPosition for this also works.
      // T := TiledMap.Data.TileRenderPosition(Vector2Integer(MapX, MapY));
      // T := T + Vector2(TiledMap.Data.TileWidth / 2, TiledMap.Data.TileHeight / 2);

      { Note: Instead of using ZInFrontMap here, we could also set
        TestMapObjects.Translation to Vector3(0, 0, ZInFrontMap).
        Then NewPlane.Translation would need simple Z = 0. }

      T := TiledMap.TileRectangle(Vector2Integer(MapX, MapY)).Center;
      NewPlane.Translation := Vector3(T, ZInFrontMap);

      { We add to TestMapObjects, so we can easily remove all test objects later.
        TestMapObjects is a trivial child of TiledMap with no extra transformation. }
      TestMapObjects.Add(NewPlane);

      // This also works, we would just have more work to cleanup the test rectangles later.
      //TiledMap.Add(NewPlane);

      Result := NewPlane;
    end;

  var
    ZInFrontMap: Single;
    I: Integer;
  begin
    { Camera looks down in -Z direction, so place at positive Z to be above
      the map.
      Note: This needs to be in front of the map, so Z must account for
      the Z of the front-most layer of the map. }
    ZInFrontMap := TiledMap.Data.Layers.Count  * TiledMap.LayersZDistance;

    // sample object at left-bottom tile of the map
    AddTestRectangle(0, 0, ZInFrontMap);
    // sample objects at right-bottom tile of the map
    AddTestRectangle(TiledMap.Data.Width - 1, TiledMap.Data.Height - 1, ZInFrontMap);
    // a few random objects at random tiles of the map
    for I := 1 to 100 do
    begin
      AddTestRectangle(
        Random(TiledMap.Data.Width),
        Random(TiledMap.Data.Height), ZInFrontMap);
    end;
  end;

begin
  Result := inherited;
  if Result then Exit;

  { TODO: Simple test of TiledMap.Data.Layers[xxx].Exists.
    See https://castle-engine.io/roadmap#tiled_layers for plans. }
  if Event.IsKey(key0) then
    TiledMap.Data.Layers[0].Exists := not TiledMap.Data.Layers[0].Exists;
  if Event.IsKey(key1) then
    if TiledMap.Data.Layers.Count > 1 then
      TiledMap.Data.Layers[1].Exists := not TiledMap.Data.Layers[1].Exists;
  if Event.IsKey(key2) then
    if TiledMap.Data.Layers.Count > 2 then
      TiledMap.Data.Layers[2].Exists := not TiledMap.Data.Layers[2].Exists;

  if Event.IsKey(keyT) then
    TestAddingComponents;
end;

end.
