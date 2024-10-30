{
  Copyright 2018-2023 Michalis Kamburelis.

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

uses Classes, Contnrs,
  CastleTransform, CastleComponentSerialize, CastleVectors, CastleScene,
  CastleTiledMap;

type
  TUnitKind = (ukAlienLight, ukAlienHeavy, ukHumanLight, ukHumanHeavy);

  TUnit = class;

  TUnitsOnMap = class(TComponent)
  private
    FItems: array of array of TUnit;
    FUnits: TComponentList;
  strict private
    FMap: TCastleTiledMap;
    FUnitsCount: Integer;
    function GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
    function GetUnits(const Index: Integer): TUnit;
    procedure SetUnitsCount(const AValue: Integer);
  public
    constructor Create(const AOwner: TComponent;
      const AMap: TCastleTiledMap); reintroduce;
    destructor Destroy; override;

    { What unit is present at each map tile (@nil if none).

      You cannot change it directly, instead
      - add new TUnit,
      - destroy a TUnit,
      - change TUnit.TilePosition.

      In other words, changes to TUnit are automatically reflected here. }
    property Items[const TilePosition: TVector2Integer]: TUnit read GetUnitOnMap; default;

    function UnitsCount: Integer;
    property Units[const Index: Integer]: TUnit read GetUnits;

    property Map: TCastleTiledMap read FMap;

    function IsWater(const TilePosition: TVector2Integer): Boolean;
  end;

  TUnit = class(TComponent)
  private
    class var
      TransformTemplate: TSerializedComponent;
  strict private
    var
      FKind: TUnitKind;
      FAttack, FLife, FMovement: Integer;
      FInitialMovement: Integer;
      ImageIcon: TCastleImageTransform;
      TextAttack: TCastleText;
      TextLife: TCastleText;
      TextMovement: TCastleText;
      Background: TCastleImageTransform;
      FTilePosition: TVector2Integer;
      FUnitsOnMap: TUnitsOnMap;
    procedure SetTilePosition(const Value: TVector2Integer);
    procedure SetUnitsOnMap(const Value: TUnitsOnMap);
    procedure PlaceOnMap;
    procedure RemoveFromMap;
    property UnitsOnMap: TUnitsOnMap read FUnitsOnMap write SetUnitsOnMap;
    procedure SetLife(const Value: Integer);
    procedure SetMovement(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Transform: TCastleTransform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const AUnitsOnMap: TUnitsOnMap;
      const AKind: TUnitKind; const AnAttack, ALife, AMovement: Integer); overload;
    { Initialize, choosing default statistics for this TUnitKind. }
    procedure Initialize(const AUnitsOnMap: TUnitsOnMap;
      const AKind: TUnitKind); overload;
    function ToString: String; override;
    function Human: Boolean;

    { Can this unit move to given new position.
      Returns @true if the tile is empty and possible to walk into (not water),
      or occupied by the enemy (in which case we do attack).}
    function CanMove(const NewTilePosition: TVector2Integer): Boolean;

    property Kind: TUnitKind read FKind;
    property Attack: Integer read FAttack;
    property InitialMovement: Integer read FInitialMovement;
    { You can change unit life. Setting life to <= 0 frees the unit instance,
      removing it also from the map. }
    property Life: Integer read FLife write SetLife;
    property Movement: Integer read FMovement write SetMovement;
    property TilePosition: TVector2Integer read FTilePosition write SetTilePosition;
  end;

const
  ZUnit = 100.0;
  ZHover = 200.0;

implementation

uses SysUtils, TypInfo, Math,
  CastleRectangles, CastleStringUtils, CastleColors, CastleViewport;

{ TUnitsOnMap ---------------------------------------------------------------- }

constructor TUnitsOnMap.Create(const AOwner: TComponent;
  const AMap: TCastleTiledMap);
begin
  inherited Create(AOwner);
  FMap := AMap;
  SetLength(FItems, Map.Data.Width, Map.Data.Height);
  FUnits := TComponentList.Create(false);
end;

destructor TUnitsOnMap.Destroy;
begin
  FreeAndNil(FUnits);
  inherited;
end;

function TUnitsOnMap.GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
begin
  Result := FItems[TilePosition.X, TilePosition.Y];
end;

function TUnitsOnMap.UnitsCount: Integer;
begin
  Result := FUnits.Count;
end;

function TUnitsOnMap.GetUnits(const Index: Integer): TUnit;
begin
  Result := FUnits[Index] as TUnit;
end;

procedure TUnitsOnMap.SetUnitsCount(const AValue: Integer);
begin
  if FUnitsCount = AValue then Exit;
  FUnitsCount := AValue;
end;

function TUnitsOnMap.IsWater(const TilePosition: TVector2Integer): Boolean;
var
  Tileset: TCastleTiledMapData.TTileset;
  Frame: Integer;
  HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
begin
  Result := Map.Data.TileRenderData(TilePosition,
    Map.Data.Layers[0],
    Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) and
    { Water is on 1, 5, 9 tiles (counting from 0) in data/maps/tileset-terrain.png . }
    ((Frame mod 4) = 1);
end;

{ TUnit ----------------------------------------------------------------------- }

constructor TUnit.Create(AOwner: TComponent);
begin
  inherited;
  if TransformTemplate = nil then
    TransformTemplate := TSerializedComponent.Create('castle-data:/unit.castle-transform');
  Transform := TransformTemplate.TransformLoad(Self);

  Background := FindRequiredComponent('Background') as TCastleImageTransform;
  ImageIcon := FindRequiredComponent('ImageIcon') as TCastleImageTransform;
  TextAttack := FindRequiredComponent('TextAttack') as TCastleText;
  TextLife := FindRequiredComponent('TextLife') as TCastleText;
  TextMovement := FindRequiredComponent('TextMovement') as TCastleText;
end;

procedure TUnit.Initialize(const AUnitsOnMap: TUnitsOnMap;
  const AKind: TUnitKind;
  const AnAttack, ALife, AMovement: Integer);
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
  FLife := ALife;
  FInitialMovement := AMovement;
  FMovement := AMovement;

  // adjust components in Transform
  ImageIcon.URL := UnitIconUrls[AKind];
  // change Background.Color (RGB, leave alpha as it was)
  if Human then
    Background.Color := ColorOpacity(HexToColor('4CFF6B'), Background.Color.W)
  else
    Background.Color := ColorOpacity(HexToColor('FF664C'), Background.Color.W);
  TextAttack.Caption := IntToStr(Attack);
  TextLife.Caption := IntToStr(Life);
  TextMovement.Caption := IntToStr(Movement);

  UnitsOnMap := AUnitsOnMap;
  UnitsOnMap.FUnits.Add(Self);

  PlaceOnMap;
end;

procedure TUnit.Initialize(const AUnitsOnMap: TUnitsOnMap; const AKind: TUnitKind);
var
  Heavy: Boolean;
  AnAttack, ALife, AMovement: Integer;
begin
  Heavy := AKind in [ukAlienHeavy, ukHumanHeavy];
  AnAttack := IfThen(Heavy, 7, 3);
  ALife := IfThen(Heavy, 20, 10);
  AMovement := IfThen(Heavy, 2, 5);
  Initialize(AUnitsOnMap, AKind, AnAttack, ALife, AMovement);
end;

destructor TUnit.Destroy;
begin
  if UnitsOnMap <> nil then
  begin
    UnitsOnMap.FUnits.Remove(Self);
    RemoveFromMap;
    UnitsOnMap := nil;
  end;
  inherited;
end;

procedure TUnit.PlaceOnMap;
var
  R: TFloatRectangle;
  Scale: Single;
const
  { Must correspond to the unit.castle-transform original size (see in editor at design-time). }
  UnitDesignedSize = 256.0;
  { Additional unit scale to look better. }
  UnitScale = 0.8;
  { Additional unit scale to look better on Isometric maps. }
  UnitScaleIsometric = 0.75;
begin
  if UnitsOnMap <> nil then
  begin
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y] := Self;

    R := UnitsOnMap.Map.TileRectangle(TilePosition);
    Transform.Translation := Vector3(R.Center, ZUnit);

    { The unit.castle-transform is designed for tile size = 256.
      Adjust it to match the actual tile size. }
    Scale := UnitScale * R.Height / UnitDesignedSize;
    { In case of isometric map, the displayed units should be smaller,
      otherwise it's too crowdy on the map. }
    if UnitsOnMap.Map.Data.Orientation in [moIsometric, moIsometricStaggered] then
      Scale := Scale * UnitScaleIsometric;
    Transform.Scale := Vector3(Scale, Scale, 1.0);

    Transform.Exists := true;
  end;
end;

procedure TUnit.RemoveFromMap;
begin
  if (UnitsOnMap <> nil) and
     (UnitsOnMap.FItems[TilePosition.X, TilePosition.Y] = Self) then
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y] := nil;

  Transform.Exists := false;
end;

procedure TUnit.SetTilePosition(const Value: TVector2Integer);
begin
  RemoveFromMap;
  FTilePosition := Value;
  PlaceOnMap;
end;

function TUnit.ToString: String;
begin
  Result := Format('%s (Attack:%d, Life:%d, Movement:%d)', [
    SEnding(GetEnumName(TypeInfo(TUnitKind), Ord(Kind)), 3),
    Attack,
    Life,
    Movement
  ]);
end;

function TUnit.Human: Boolean;
begin
  Result := FKind in [ukHumanLight, ukHumanHeavy];
end;

function TUnit.CanMove(const NewTilePosition: TVector2Integer): Boolean;
const
  { Both true and false work OK, change to determine
    whether you can move/attack along diagonals. }
  CornersAreNeighbors = true;
var
  UnitOnNewPosition: TUnit;
begin
  Result :=
    (UnitsOnMap <> nil) and
    (Movement > 0) and
    // can only move over neighbor tile, that is not water
    (UnitsOnMap.Map.Data.TileNeighbor(
      TilePosition, NewTilePosition, CornersAreNeighbors)) and
    not UnitsOnMap.IsWater(NewTilePosition);

  // cannot move over a unit of the same side
  if Result then
  begin
    UnitOnNewPosition := UnitsOnMap[NewTilePosition];
    if (UnitOnNewPosition <> nil) and (UnitOnNewPosition.Human = Human) then
      Exit(false);
  end;
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

procedure TUnit.SetLife(const Value: Integer);
begin
  if FLife <> Value then
  begin
    FLife := Value;
    if Value > 0 then
      TextLife.Caption := IntToStr(Life)
    else
      Self.Destroy;
  end;
end;

procedure TUnit.SetMovement(const Value: Integer);
begin
  if FMovement <> Value then
  begin
    FMovement := Value;
    TextMovement.Caption := IntToStr(Movement);
  end;
end;

initialization

finalization
  FreeAndNil(TUnit.TransformTemplate);
end.
