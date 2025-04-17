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
  { Cylinder with configurable size, position and material. }
  TCastleCylinder = class(TCastleAbstractPrimitive)
  strict private
    CylinderNode: TCylinderNode;
    FRadius, FHeight: Single;
    FDoubleSided: Boolean;
    FBottom, FSide, FTop: Boolean;
    FSlices: Cardinal;
    procedure SetRadius(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetDoubleSided(const Value: Boolean);
    procedure SetBottom(const Value: Boolean);
    procedure SetSide(const Value: Boolean);
    procedure SetTop(const Value: Boolean);
    procedure SetSlices(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Cylinder radius. }
    property Radius: Single read FRadius write SetRadius {$ifdef FPC}default 1{$endif};

    { Cylinder height. }
    property Height: Single read FHeight write SetHeight {$ifdef FPC}default 2{$endif};

    { When this is @true, the object is visible (and lit) from both sides. }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default false;

    { Display bottom of the cylinder.
      When @false, you can see the cylinder interior if looking from the outside
      (note: turn on DoubleSided to make interior visible). }
    property Bottom: Boolean read FBottom write SetBottom default true;

    { Display side of the cylinder.
      When @false, you can see the cylinder interior if looking from the outside
      (note: turn on DoubleSided to make interior visible). }
    property Side: Boolean read FSide write SetSide default true;

    { Display top of the cylinder.
      When @false, you can see the cylinder interior if looking from the outside
      (note: turn on DoubleSided to make interior visible). }
    property Top: Boolean read FTop write SetTop default true;

    { How many "slices" to use to approximate the cylinder.
      The cylinder is rendered using polygons (triangles and quads)
      that approximate the cylinder shape.
      The more slices, the more the cylinder will look like a cylinder.

      Slices divide the objects like slices of a pizza.

      The default value of 0 means to follow global variable
      @link(DefaultTriangulationSlices).}
    property Slices: Cardinal read FSlices write SetSlices default 0;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleCylinder ---------------------------------------------------------------- }

constructor TCastleCylinder.Create(AOwner: TComponent);
begin
  inherited;

  { default properties values }
  FRadius := 1;
  FHeight := 2;
  FBottom := true;
  FSide := true;
  FTop := true;

  CylinderNode := TCylinderNode.Create;
  CylinderNode.Radius := FRadius;
  CylinderNode.Height := FHeight;
  CylinderNode.Solid := not FDoubleSided;
  CylinderNode.Slices := FSlices;
  ShapeNode.Geometry := CylinderNode;
end;

function TCastleCylinder.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['Radius', 'Height', 'DoubleSided',
       'Bottom', 'Side', 'Top', 'Slices']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleCylinder.SetRadius(const Value: Single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    CylinderNode.Radius := FRadius;
    UpdateCollider;
  end;
end;

procedure TCastleCylinder.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    CylinderNode.Height := FHeight;
    UpdateCollider;
  end;
end;

procedure TCastleCylinder.SetDoubleSided(const Value: Boolean);
begin
  if FDoubleSided <> Value then
  begin
    FDoubleSided := Value;
    CylinderNode.Solid := not Value;
  end;
end;

procedure TCastleCylinder.SetBottom(const Value: Boolean);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    CylinderNode.Bottom := Value;
  end;
end;

procedure TCastleCylinder.SetSide(const Value: Boolean);
begin
  if FSide <> Value then
  begin
    FSide := Value;
    CylinderNode.Side := Value;
  end;
end;

procedure TCastleCylinder.SetTop(const Value: Boolean);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    CylinderNode.Top := Value;
  end;
end;

procedure TCastleCylinder.SetSlices(const Value: Cardinal);
begin
  if FSlices <> Value then
  begin
    FSlices := Value;
    CylinderNode.Slices := FSlices;
  end;
end;

{$endif read_implementation}
