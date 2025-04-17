{%MainUnit castlescene.pas}
{
  Copyright 2021-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Cone with configurable size, position and material. }
  TCastleCone = class(TCastleAbstractPrimitive)
  strict private
    ConeNode: TConeNode;
    FBottomRadius, FHeight: Single;
    FDoubleSided: Boolean;
    FBottom, FSide: Boolean;
    FSlices, FStacks: Cardinal;
    procedure SetBottomRadius(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetDoubleSided(const Value: Boolean);
    procedure SetBottom(const Value: Boolean);
    procedure SetSide(const Value: Boolean);
    procedure SetSlices(const Value: Cardinal);
    procedure SetStacks(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Cone bottom radius. }
    property BottomRadius: Single read FBottomRadius write SetBottomRadius {$ifdef FPC}default 1{$endif};

    { Cone height. }
    property Height: Single read FHeight write SetHeight {$ifdef FPC}default 2{$endif};

    { When this is @true, the object is visible (and lit) from both sides. }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default false;

    { Display bottom of the cone.
      When @false, you can see the cone interior if looking from the outside
      (note: turn on DoubleSided to make interior visible). }
    property Bottom: Boolean read FBottom write SetBottom default true;

    { Display sides of the cone.
      When this is @false, and @link(Bottom) is @true,
      then cone bottom is effectively displayed as just a disk. }
    property Side: Boolean read FSide write SetSide default true;

    { How many "slices" to use to approximate the cone.
      The cone is rendered using polygons (triangles and quads)
      that approximate the cone shape.
      The more slices, the more the cone will look like a cone.

      Slices divide the objects like slices of a pizza.

      The default value of 0 means to follow global variable
      @link(DefaultTriangulationSlices).}
    property Slices: Cardinal read FSlices write SetSlices default 0;

    { How many "stacks" to use to approximate the cone.
      The cone is rendered using polygons (triangles and quads)
      that approximate the cone shape.
      The more stacks, the more the cone will look like a cone.

      Stacks divide the objects like stacks of a tower.

      The default value of 0 means to follow global variable
      @link(DefaultTriangulationStacks).}
    property Stacks: Cardinal read FStacks write SetStacks default 0;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleCone ---------------------------------------------------------------- }

constructor TCastleCone.Create(AOwner: TComponent);
begin
  inherited;

  { default properties values }
  FBottomRadius := 1;
  FHeight := 2;
  FBottom := true;
  FSide := true;

  ConeNode := TConeNode.Create;
  ConeNode.BottomRadius := FBottomRadius;
  ConeNode.Height := FHeight;
  ConeNode.Solid := not FDoubleSided;
  ConeNode.Slices := FSlices;
  ConeNode.Stacks := FStacks;
  ShapeNode.Geometry := ConeNode;
end;

function TCastleCone.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['BottomRadius', 'Height', 'DoubleSided',
       'Bottom', 'Side', 'Slices', 'Stacks']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleCone.SetBottomRadius(const Value: Single);
begin
  if FBottomRadius <> Value then
  begin
    FBottomRadius := Value;
    ConeNode.BottomRadius := FBottomRadius;
    UpdateCollider;
  end;
end;

procedure TCastleCone.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    ConeNode.Height := FHeight;
    UpdateCollider;
  end;
end;

procedure TCastleCone.SetDoubleSided(const Value: Boolean);
begin
  if FDoubleSided <> Value then
  begin
    FDoubleSided := Value;
    ConeNode.Solid := not Value;
  end;
end;

procedure TCastleCone.SetBottom(const Value: Boolean);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    ConeNode.Bottom := Value;
  end;
end;

procedure TCastleCone.SetSide(const Value: Boolean);
begin
  if FSide <> Value then
  begin
    FSide := Value;
    ConeNode.Side := Value;
  end;
end;

procedure TCastleCone.SetSlices(const Value: Cardinal);
begin
  if FSlices <> Value then
  begin
    FSlices := Value;
    ConeNode.Slices := FSlices;
  end;
end;

procedure TCastleCone.SetStacks(const Value: Cardinal);
begin
  if FStacks <> Value then
  begin
    FStacks := Value;
    ConeNode.Stacks := FStacks;
  end;
end;

{$endif read_implementation}
