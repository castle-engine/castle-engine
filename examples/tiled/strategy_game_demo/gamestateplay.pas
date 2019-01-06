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

{ Display the game map, play the game. }
unit GameStatePlay;

interface

uses CastleUIState, CastleControls, CastleTiledMap, CastleUIControls,
  CastleVectors, CastleKeysMouse,
  GameUnit;

type
  TStatePlay = class(TUIState)
  strict private
    MapControl: TCastleTiledMapControl;
    ButtonQuit: TCastleButton;
    ButtonEndTurn: TCastleButton;
    LabelStatus, LabelTurnStatus: TCastleLabel;
    TileUnderMouseImage: TCastleImageControl;
    TileUnderMouseExists: Boolean;
    TileUnderMouse: TVector2Integer;
    UnitsOnMap: TUnitsOnMap;
    HumanTurn: Boolean;
    SelectedUnit: TUnit;
    procedure ClickQuit(Sender: TObject);
    procedure ClickEndTurn(Sender: TObject);
    procedure MapPress(const Sender: TInputListener;
      const Event: TInputPressRelease; var Handled: Boolean);
    procedure UpdateTurnStatus;
  public
    { Set this before starting this state. }
    MapName: String;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils, Classes,
  CastleComponentSerialize, CastleUtils, CastleRectangles,
  GameStateMainMenu;

procedure TStatePlay.Start;
var
  Ui: TCastleUserInterface;
  UiOwner: TComponent;
  RandomUnit: TUnit;
  I: Integer;
begin
  inherited;

  { UiOwner allows to search for components using FindRequiredComponent,
    and makes sure the entire UI will be freed when state stops
    (because UiOwner is owned by FreeAtStop). }
  UiOwner := TComponent.Create(FreeAtStop);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/state_play.castle-user-interface', UiOwner);
  InsertFront(Ui);

  // find components in designed user interface
  MapControl := UiOwner.FindRequiredComponent('MapControl') as TCastleTiledMapControl;
  ButtonQuit := UiOwner.FindRequiredComponent('ButtonQuit') as TCastleButton;
  ButtonEndTurn := UiOwner.FindRequiredComponent('ButtonEndTurn') as TCastleButton;
  LabelStatus := UiOwner.FindRequiredComponent('LabelStatus') as TCastleLabel;
  LabelTurnStatus := UiOwner.FindRequiredComponent('LabelTurnStatus') as TCastleLabel;

  MapControl.URL := 'castle-data:/maps/' + MapName + '.tmx';
  MapControl.OnPress := @MapPress;
  ButtonQuit.OnClick := @ClickQuit;
  ButtonEndTurn.OnClick := @ClickEndTurn;

  UnitsOnMap := TUnitsOnMap.Create(FreeAtStop, MapControl);

  for I := 1 to 10 do
  begin
    RandomUnit := TUnit.Create(FreeAtStop);
    RandomUnit.TilePosition := Vector2Integer(
      Random(MapControl.Map.Width),
      Random(MapControl.Map.Height)
    );
    RandomUnit.Initialize(UnitsOnMap,
      TUnitKind(Random(Ord(High(TUnitKind)) + 1)),
      RandomIntRange(3, 10),
      RandomIntRange(3, 10),
      RandomIntRange(3, 10));
    MapControl.InsertFront(RandomUnit.Ui);
  end;

  TileUnderMouseImage := TCastleImageControl.Create(FreeAtStop);
  TileUnderMouseImage.Stretch := true;
  case MapControl.Map.Orientation of
    moOrthogonal: TileUnderMouseImage.URL := 'castle-data:/tile_hover/orthogonal.png';
    moHexagonal : TileUnderMouseImage.URL := 'castle-data:/tile_hover/hexagonal.png';
    else          TileUnderMouseImage.URL := 'castle-data:/tile_hover/isometric.png';
  end;
  TileUnderMouseImage.Exists := false;
  MapControl.InsertFront(TileUnderMouseImage);

  HumanTurn := true;
  UpdateTurnStatus;
end;

procedure TStatePlay.ClickQuit(Sender: TObject);
begin
  TUIState.Current := StateMainMenu;
end;

procedure TStatePlay.ClickEndTurn(Sender: TObject);

  procedure ResetMovement;
  var
    I: Integer;
  begin
    for I := 0 to UnitsOnMap.UnitsCount - 1 do
      UnitsOnMap.Units[I].Movement := UnitsOnMap.Units[I].InitialMovement;
  end;

begin
  HumanTurn := not HumanTurn;
  SelectedUnit := nil;
  ResetMovement;
  UpdateTurnStatus;
end;

procedure TStatePlay.MapPress(const Sender: TInputListener;
  const Event: TInputPressRelease; var Handled: Boolean);
var
  UnitUnderMouse: TUnit;
begin
  if Event.IsMouseButton(mbLeft) and TileUnderMouseExists then
  begin
    Handled := true;
    UnitUnderMouse := UnitsOnMap[TileUnderMouse];
    if (UnitUnderMouse <> nil) and (UnitUnderMouse.Human = HumanTurn) then
    begin
      // select new unit
      SelectedUnit := UnitUnderMouse;
      UpdateTurnStatus;
    end else
    if (SelectedUnit <> nil) and
       (SelectedUnit.CanMove(TileUnderMouse)) then
    begin
      if UnitUnderMouse <> nil then
      begin
        // hurt enemy UnitUnderMouse.
        UnitUnderMouse.Life := UnitUnderMouse.Life - SelectedUnit.Attack;
        // Above operation *maybe* freed and removed enemy from the map,
        // so UnitUnderMouse pointer afterwards is no longer valid.
        UnitUnderMouse := nil;
      end else
      begin
        // move
        SelectedUnit.TilePosition := TileUnderMouse;
      end;
      SelectedUnit.Movement := SelectedUnit.Movement - 1;
    end;
  end;
end;

procedure TStatePlay.UpdateTurnStatus;
var
  SideName: String;
begin
  if HumanTurn then
    SideName := 'Human'
  else
    SideName := 'Alien';
  LabelTurnStatus.Caption := SideName + ' Turn';
  LabelTurnStatus.Text.Add(''); // newline

  if SelectedUnit <> nil then
  begin
    LabelTurnStatus.Text.Add('Selected: ' + SelectedUnit.ToString);
    LabelTurnStatus.Text.Add('Move or attack');
    LabelTurnStatus.Text.Add('  or select another ' + SideName + ' unit');
    LabelTurnStatus.Text.Add('  or press "End Turn".');
  end else
  begin
    LabelTurnStatus.Text.Add('No unit selected.');
    LabelTurnStatus.Text.Add('Select ' + SideName + ' unit');
    LabelTurnStatus.Text.Add('  or press "End Turn".');
  end;
end;

procedure TStatePlay.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  TileStr: String;
  TileRect: TFloatRectangle;
  UnitUnderMouse: TUnit;
begin
  { update TileUnderMouseExists, TileUnderMouse }
  TileUnderMouseExists := MapControl.PositionToTile(
    Container.MousePosition, true, TileUnderMouse);

  { update TileUnderMouseImage, UnitUnderMouse }
  TileUnderMouseImage.Exists := TileUnderMouseExists;
  if TileUnderMouseExists then
  begin
    TileRect := MapControl.TileRectangle(TileUnderMouse, false);
    TileUnderMouseImage.Left := TileRect.Left;
    TileUnderMouseImage.Bottom := TileRect.Bottom;
    TileUnderMouseImage.Width := TileRect.Width;
    TileUnderMouseImage.Height := TileRect.Height;
    UnitUnderMouse := UnitsOnMap[TileUnderMouse];
  end else
    UnitUnderMouse := nil;

  { update TileUnderMouseImage.Color }
  if SelectedUnit = nil then
    TileUnderMouseImage.Color := Vector4(1, 1, 1, 0.75)
  else
  if SelectedUnit.CanMove(TileUnderMouse) then
    // can move by clicking here
    TileUnderMouseImage.Color := Vector4(0, 1, 0, 0.75)
  else
  if (UnitUnderMouse <> nil) and
     (UnitUnderMouse.Human = HumanTurn) then
    // can select by clicking here
    TileUnderMouseImage.Color := Vector4(1, 1, 1, 0.75)
  else
    // cannot do anything by clicking here
    TileUnderMouseImage.Color := Vector4(1, 0, 0, 0.75);

  { update LabelStatus }
  if TileUnderMouseExists then
  begin
    TileStr := TileUnderMouse.ToString;
    if UnitsOnMap.IsWater(TileUnderMouse) then
      TileStr += NL + ' Water';
    if UnitUnderMouse <> nil then
      TileStr += NL + ' Unit: ' + UnitUnderMouse.ToString;
  end else
    TileStr := 'none';
  LabelStatus.Caption := Format('FPS: %s' + NL + 'Tile: %s', [
    Container.Fps.ToString,
    TileStr
  ]);
end;

end.
