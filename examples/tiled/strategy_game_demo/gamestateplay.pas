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
  CastleVectors,
  GameUnit;

type
  TStatePlay = class(TUIState)
  strict private
    MapControl: TCastleTiledMapControl;
    ButtonQuit: TCastleButton;
    LabelStatus: TCastleLabel;
    TileUnderMouseImage: TCastleImageControl;
    TileUnderMouseExists: Boolean;
    TileUnderMouse: TVector2Integer;
    UnitsOnMap: TUnitsOnMap;
    procedure ClickQuit(Sender: TObject);
    function IsWater(const TilePosition: TVector2Integer): Boolean;
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

  MapControl := UiOwner.FindRequiredComponent('MapControl') as TCastleTiledMapControl;
  MapControl.URL := 'castle-data:/maps/' + MapName + '.tmx';

  ButtonQuit := UiOwner.FindRequiredComponent('ButtonQuit') as TCastleButton;
  ButtonQuit.OnClick := @ClickQuit;

  LabelStatus := UiOwner.FindRequiredComponent('LabelStatus') as TCastleLabel;

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
  TileUnderMouseImage.Color := Vector4(0, 1, 0, 0.75);
  case MapControl.Map.Orientation of
    moOrthogonal: TileUnderMouseImage.URL := 'castle-data:/tile_hover/orthogonal.png';
    moHexagonal : TileUnderMouseImage.URL := 'castle-data:/tile_hover/hexagonal.png';
    else          TileUnderMouseImage.URL := 'castle-data:/tile_hover/isometric.png';
  end;
  TileUnderMouseImage.Exists := false;
  MapControl.InsertFront(TileUnderMouseImage);
end;

procedure TStatePlay.ClickQuit(Sender: TObject);
begin
  TUIState.Current := StateMainMenu;
end;

function TStatePlay.IsWater(const TilePosition: TVector2Integer): Boolean;
var
  Tileset: TTiledMap.TTileset;
  Frame: Integer;
  HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
begin
  Result := MapControl.Map.TileRenderData(TilePosition,
    MapControl.Map.Layers[0],
    Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) and
    { Water is on 1, 5, 9 tiles (counting from 0) in data/maps/tileset-terrain.png . }
    ((Frame mod 4) = 1);
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

  { update TileUnderMouseImage }
  TileUnderMouseImage.Exists := TileUnderMouseExists;
  if TileUnderMouseExists then
  begin
    TileRect := MapControl.TileRectangle(TileUnderMouse, false);
    TileUnderMouseImage.Left := TileRect.Left;
    TileUnderMouseImage.Bottom := TileRect.Bottom;
    TileUnderMouseImage.Width := TileRect.Width;
    TileUnderMouseImage.Height := TileRect.Height;
  end;

  { update LabelStatus }
  if TileUnderMouseExists then
  begin
    TileStr := TileUnderMouse.ToString;
    if IsWater(TileUnderMouse) then
      TileStr += NL + ' Water';
    UnitUnderMouse := UnitsOnMap[TileUnderMouse];
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
