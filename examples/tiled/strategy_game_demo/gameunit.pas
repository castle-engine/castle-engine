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

{ Unit (soldier) on a map. }
unit GameUnit;

interface

uses Classes,
  CastleUIControls, CastleControls, CastleComponentSerialize, CastleVectors,
  CastleTiledMap;

type
  TUnitKind = (utAlien1, utAlien2, utHuman1, utHuman2);

  TUnit = class;

  TUnitsOnMap = class(TComponent)
  private
    FItems: array of array of TUnit;
  strict private
    FMapControl: TCastleTiledMapControl;
    function GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
  public
    constructor Create(const AOwner: TComponent;
      const AMapControl: TCastleTiledMapControl); reintroduce;

    { What unit is present at each map tile (@nil if none).

      You cannot change it directly, instead
      - add new TUnit,
      - destroy a TUnit,
      - change TUnit.TilePosition.

      In other words, changes to TUnit are automatically reflected here. }
    property Items[const TilePosition: TVector2Integer]: TUnit read GetUnitOnMap; default;

    property MapControl: TCastleTiledMapControl read FMapControl;
  end;

  TUnit = class(TComponent)
  private
    class var
      UiTemplate: TSerializedComponent;
  strict private
    var
      FKind: TUnitKind;
      FAttack, FDefense, FMovement: Cardinal;
      ImageIcon: TCastleImageControl;
      LabelAttack: TCastleLabel;
      LabelDefense: TCastleLabel;
      LabelMovement: TCastleLabel;
      FTilePosition: TVector2Integer;
      FUnitsOnMap: TUnitsOnMap;
    procedure SetTilePosition(const Value: TVector2Integer);
    procedure SetUnitsOnMap(const Value: TUnitsOnMap);
    procedure PlaceOnMap;
    procedure RemoveFromMap;
    property UnitsOnMap: TUnitsOnMap read FUnitsOnMap write SetUnitsOnMap;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Ui: TCastleUserInterface;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const AUnitsOnMap: TUnitsOnMap;
      const AKind: TUnitKind; const AnAttack, ADefense, AMovement: Cardinal);
    function ToString: String; override;

    property Kind: TUnitKind read FKind;
    property Attack: Cardinal read FAttack;
    property Defense: Cardinal read FDefense;
    property Movement: Cardinal read FMovement;
    property TilePosition: TVector2Integer read FTilePosition write SetTilePosition;
  end;

implementation

uses SysUtils, TypInfo,
  CastleRectangles, CastleStringUtils;

{ TUnitsOnMap ---------------------------------------------------------------- }

constructor TUnitsOnMap.Create(const AOwner: TComponent;
  const AMapControl: TCastleTiledMapControl);
begin
  inherited Create(AOwner);
  FMapControl := AMapControl;
  SetLength(FItems, MapControl.Map.Width, MapControl.Map.Height);
end;

function TUnitsOnMap.GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
begin
  Result := FItems[TilePosition.X, TilePosition.Y];
end;

{ TUnit ----------------------------------------------------------------------- }

constructor TUnit.Create(AOwner: TComponent);
begin
  inherited;
  if UiTemplate = nil then
    UiTemplate := TSerializedComponent.Create('castle-data:/unit.castle-user-interface');
  Ui := UiTemplate.UserInterfaceLoad(Self);

  ImageIcon := FindRequiredComponent('ImageIcon') as TCastleImageControl;
  LabelAttack := FindRequiredComponent('LabelAttack') as TCastleLabel;
  LabelDefense := FindRequiredComponent('LabelDefense') as TCastleLabel;
  LabelMovement := FindRequiredComponent('LabelMovement') as TCastleLabel;
end;

procedure TUnit.Initialize(const AUnitsOnMap: TUnitsOnMap;
  const AKind: TUnitKind;
  const AnAttack, ADefense, AMovement: Cardinal);
const
  UnitIconUrls: array [TUnitKind] of string  =
  ( 'castle-data:/units/alien1.png',
    'castle-data:/units/alien2.png',
    'castle-data:/units/human1.png',
    'castle-data:/units/human2.png'
  );
begin
  FKind := AKind;
  FAttack := AnAttack;
  FDefense := ADefense;
  FMovement := AMovement;

  ImageIcon.URL := UnitIconUrls[AKind];
  LabelAttack.Caption := IntToStr(Attack);
  LabelDefense.Caption := IntToStr(Defense);
  LabelMovement.Caption := IntToStr(Movement);

  UnitsOnMap := AUnitsOnMap;

  { In case of isometric map, the displayed units should be smaller,
    otherwise it's too crowdy on the map. }
  if UnitsOnMap.MapControl.Map.Orientation in [moIsometric, moIsometricStaggered] then
  begin
    Ui.Width := 75;
    Ui.Height := 75;
  end;

  PlaceOnMap;
end;

destructor TUnit.Destroy;
begin
  RemoveFromMap;
  UnitsOnMap := nil;
  inherited;
end;

procedure TUnit.PlaceOnMap;
var
  R: TFloatRectangle;
begin
  if UnitsOnMap <> nil then
  begin
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y] := Self;

    R := UnitsOnMap.MapControl.TileRectangle(TilePosition, false);
    R := R.CenterInside(Ui.EffectiveWidth, Ui.EffectiveHeight);
    Ui.Left := R.Left;
    Ui.Bottom := R.Bottom;
    Ui.Exists := true;
  end;
end;

procedure TUnit.RemoveFromMap;
begin
  if (UnitsOnMap <> nil) and
     (UnitsOnMap.FItems[TilePosition.X, TilePosition.Y] = Self) then
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y] := nil;

  Ui.Exists := false;
end;

procedure TUnit.SetTilePosition(const Value: TVector2Integer);
begin
  RemoveFromMap;
  FTilePosition := Value;
  PlaceOnMap;
end;

function TUnit.ToString: String;
begin
  Result := Format('%s, %d / %d / %d', [
    SEnding(GetEnumName(TypeInfo(TUnitKind), Ord(Kind)), 3),
    Attack,
    Defense,
    Movement
  ]);
end;

procedure TUnit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FUnitsOnMap) then
    { set to nil by SetUnitsOnMap to clean nicely }
    UnitsOnMap := nil;
end;

procedure TUnit.SetUnitsOnMap(const Value: TUnitsOnMap);
begin
  if FUnitsOnMap <> Value then
  begin
    if FUnitsOnMap <> nil then
      FUnitsOnMap.RemoveFreeNotification(Self);
    FUnitsOnMap := Value;
    if FUnitsOnMap <> nil then
      FUnitsOnMap.FreeNotification(Self);
  end;
end;

finalization
  FreeAndNil(TUnit.UiTemplate);
end.
