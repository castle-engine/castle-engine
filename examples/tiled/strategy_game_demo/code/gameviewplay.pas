{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display the game map, play the game. }
unit GameViewPlay;

interface

uses Classes,
  CastleControls, CastleTiledMap, CastleUIControls,
  CastleVectors, CastleKeysMouse, CastleScene, CastleViewport,
  GameUnit;

type
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    Map: TCastleTiledMap;
    ButtonQuit: TCastleButton;
    ButtonInstructions: TCastleButton;
    ButtonEndTurn: TCastleButton;
    LabelStatus, LabelTurnStatus: TCastleLabel;
    ViewportMap: TCastleViewport;
  strict private
    TileUnderMouseImage: TCastleImageTransform;
    TileUnderMouseExists: Boolean;
    TileUnderMouse: TVector2Integer;
    UnitsOnMap: TUnitsOnMap;
    HumanTurn: Boolean;
    SelectedUnit: TUnit;
    procedure ClickQuit(Sender: TObject);
    procedure ClickInstructions(Sender: TObject);
    procedure ClickEndTurn(Sender: TObject);
    procedure UpdateTurnStatus;
  public
    { Set this before starting this view. }
    MapName: String;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils,
  CastleComponentSerialize, CastleUtils, CastleRectangles,
  GameViewMainMenu, GameViewInstructions, GameViewWin;

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;

  procedure PlaceInitialUnits;

    procedure AddUnit(const Kind: TUnitKind; const XBegin, XEnd, YBegin, YEnd: Integer);
    const
      MaxTries = 50;
    var
      I: Integer;
      Pos: TVector2Integer;
      Un: TUnit;
    begin
      { Choose a random position, within given X and Y ranges.
        Since we may play on various map sizes, with various rivers
        --- prepare that the units may not fit, and resign after MaxTries
        from adding a new unit. }
      for I := 1 to MaxTries do
      begin
        Pos.X := RandomIntRangeInclusive(XBegin, XEnd);
        Pos.Y := RandomIntRangeInclusive(YBegin, YEnd);
        if (UnitsOnMap[Pos] = nil) and
           (not UnitsOnMap.IsWater(Pos)) then
        begin
          Un := TUnit.Create(FreeAtStop);
          Un.TilePosition := Pos;
          Un.Initialize(UnitsOnMap, Kind);
          Map.Add(Un.Transform);
          Exit;
        end;
      end;
    end;

  var
    I, W, H, YBegin, YEnd: Integer;
  begin
    W := Map.Data.Width;
    H := Map.Data.Height;
    YBegin := H div 3;
    YEnd := H - 1 - H div 3;

    for I := 0 to 2 do
      AddUnit(ukAlienHeavy, W div 8 + 0, W div 8 + 0, YBegin, YEnd);
    for I := 0 to 3 do
      AddUnit(ukAlienLight, W div 8 + 2, W div 8 + 2, YBegin, YEnd);
    for I := 0 to 2 do
      AddUnit(ukHumanHeavy, W - 1 - W div 8 - 0, W - 1 - W div 8 - 0, YBegin, YEnd);
    for I := 0 to 3 do
      AddUnit(ukHumanLight, W - 1 - W div 8 - 2, W - 1 - W div 8 - 2, YBegin, YEnd);
  end;

begin
  inherited;

  Map.URL := 'castle-data:/maps/' + MapName + '.tmx';
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif}ClickQuit;
  ButtonInstructions.OnClick := {$ifdef FPC}@{$endif}ClickInstructions;
  ButtonEndTurn.OnClick := {$ifdef FPC}@{$endif}ClickEndTurn;

  UnitsOnMap := TUnitsOnMap.Create(FreeAtStop, Map);

  PlaceInitialUnits;

  TileUnderMouseImage := TCastleImageTransform.Create(FreeAtStop);
  case Map.Data.Orientation of
    moOrthogonal: TileUnderMouseImage.URL := 'castle-data:/tile_hover/orthogonal.png';
    moHexagonal : TileUnderMouseImage.URL := 'castle-data:/tile_hover/hexagonal.png';
    else          TileUnderMouseImage.URL := 'castle-data:/tile_hover/isometric.png';
  end;
  TileUnderMouseImage.Exists := false;
  Map.Add(TileUnderMouseImage);

  HumanTurn := true;
  UpdateTurnStatus;
end;

procedure TViewPlay.Stop;
begin
  { Make sure to clear fields, otherwise stopping + starting this view again
    (when you exit the game and start a new game) could have non-nil but
    invalid SelectedUnit reference. }
  TileUnderMouseExists := false;
  SelectedUnit := nil;
  inherited;
end;

procedure TViewPlay.ClickQuit(Sender: TObject);
begin
  Container.View := ViewMainMenu;
end;

procedure TViewPlay.ClickInstructions(Sender: TObject);
begin
  Container.PushView(ViewInstructions);
end;

procedure TViewPlay.ClickEndTurn(Sender: TObject);

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

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;

  procedure CheckWin;
  var
    FoundEnemy: Boolean;
    I: Integer;
  begin
    FoundEnemy := false;
    for I := 0 to UnitsOnMap.UnitsCount - 1 do
      if UnitsOnMap.Units[I].Human <> HumanTurn then
      begin
        FoundEnemy := true;
        Break;
      end;

    if not FoundEnemy then
    begin
      ViewWin.HumansWin := HumanTurn;
      Container.PushView(ViewWin);
    end;
  end;

var
  UnitUnderMouse: TUnit;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) and TileUnderMouseExists then
  begin
    UnitUnderMouse := UnitsOnMap[TileUnderMouse];
    if (UnitUnderMouse <> nil) and (UnitUnderMouse.Human = HumanTurn) then
    begin
      // select new unit
      SelectedUnit := UnitUnderMouse;
      UpdateTurnStatus;
      Exit(true); // event handled
    end else
    if (SelectedUnit <> nil) and
       // CanMove also checks that SelectedUnit.Movement > 0
       (SelectedUnit.CanMove(TileUnderMouse)) then
    begin
      if UnitUnderMouse <> nil then
      begin
        // hurt enemy UnitUnderMouse.
        UnitUnderMouse.Life := UnitUnderMouse.Life - SelectedUnit.Attack;
        // Above operation *maybe* freed and removed enemy from the map,
        // so UnitUnderMouse pointer afterwards is no longer valid.
        UnitUnderMouse := nil;
        SelectedUnit.Movement := 0;
        CheckWin;
      end else
      begin
        // move
        SelectedUnit.TilePosition := TileUnderMouse;
        SelectedUnit.Movement := SelectedUnit.Movement - 1;
      end;
      Exit(true); // event handled
    end;

    { When clicking on other map tile, do not mark event as handled
      by Exit(true) here.
      This allows TCastle2DNavigation to handle clicks to pan the map. }
  end;
end;

procedure TViewPlay.UpdateTurnStatus;
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

procedure TViewPlay.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  TileStr: String;
  TileRect: TFloatRectangle;
  UnitUnderMouse: TUnit;
  RayOrigin, RayDirection: TVector3;
  //MapIndex: Integer;
begin
  ViewportMap.PositionToRay(Container.MousePosition, true, RayOrigin, RayDirection);

  { Update TileUnderMouseExists, TileUnderMouse.
    See https://castle-engine.io/tiled_maps#tile_under_mouse for an explanation
    how to query the map tile under mouse. }
  TileUnderMouseExists := Map.Data.PositionToTile(RayOrigin.XY, TileUnderMouse);

  { This is alternative way to query the map tile under mouse,
    that will work even if map is possibly transformed,
    directly or by parent TCastleTransform,
    or if you may have different camera direction, maybe even 3D with perspective.

  TileUnderMouseExists := false;
  if ViewportMap.MouseRayHit <> nil then
  begin
    MapIndex := ViewportMap.MouseRayHit.IndexOfItem(Map);
    if MapIndex <> -1 then
    begin
      TileUnderMouseExists := Map.Data.PositionToTile(
        ViewportMap.MouseRayHit[MapIndex].Point.XY, TileUnderMouse);
    end;
  end;
  }

  { update TileUnderMouseImage, UnitUnderMouse }
  TileUnderMouseImage.Exists := TileUnderMouseExists;
  if TileUnderMouseExists then
  begin
    TileRect := Map.TileRectangle(TileUnderMouse);
    TileUnderMouseImage.Translation := Vector3(TileRect.Center, ZHover);
    TileUnderMouseImage.Size := Vector2(TileRect.Width, TileRect.Height);
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
      TileStr := TileStr + NL + ' Water';
    if UnitUnderMouse <> nil then
      TileStr := TileStr + NL + ' Unit: ' + UnitUnderMouse.ToString;
  end else
    TileStr := 'none';
  LabelStatus.Caption := Format('FPS: %s' + NL + 'Tile: %s', [
    Container.Fps.ToString,
    TileStr
  ]);
end;

end.
