{
  Copyright 2021-2021 Michalis Kamburelis.

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
    procedure SetRadius(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetDoubleSided(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Cylinder radius. }
    property Radius: Single read FRadius write SetRadius default 1;

    { Cylinder height. }
    property Height: Single read FHeight write SetHeight default 2;

    { When this is @true, the object is visible (and lit) from both sides. }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default false;
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

  CylinderNode := TCylinderNode.Create;
  CylinderNode.Radius := FRadius;
  CylinderNode.Height := FHeight;
  ShapeNode.Geometry := CylinderNode;
end;

function TCastleCylinder.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'Radius', 'Height', 'DoubleSided':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

procedure TCastleCylinder.SetRadius(const Value: Single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    CylinderNode.Radius := FRadius;
  end;
end;

procedure TCastleCylinder.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    CylinderNode.Height := FHeight;
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

{$endif read_implementation}
