{%MainUnit castlescene.pas}
{
  Copyright 2021-2022 Michalis Kamburelis.

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
  { Sphere with configurable size, position and material. }
  TCastleSphere = class(TCastleAbstractPrimitive)
  strict private
    SphereNode: TSphereNode;
    FRadius: Single;
    FDoubleSided: Boolean;
    procedure SetRadius(const Value: Single);
    procedure SetDoubleSided(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Sphere radius. }
    property Radius: Single read FRadius write SetRadius {$ifdef FPC}default 1{$endif};

    { When this is @true, the object is visible (and lit) from both sides. }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default false;

    { CollisionSphereRadius is always derived from Radius for this component. }
    property CollisionSphereRadius stored false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleSphere ---------------------------------------------------------------- }

constructor TCastleSphere.Create(AOwner: TComponent);
begin
  inherited;

  { default properties values }
  FRadius := 1;

  SphereNode := TSphereNode.Create;
  SphereNode.Radius := FRadius;
  ShapeNode.Geometry := SphereNode;

  { set CollisionSphereRadius to make collisions work better for sphere }
  CollisionSphereRadius := FRadius;
end;

destructor TCastleSphere.Destroy;
begin
  inherited;
end;

function TCastleSphere.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Radius') or
     (PropertyName = 'DoubleSided') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleSphere.SetRadius(const Value: Single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    SphereNode.Radius := FRadius;
    CollisionSphereRadius := FRadius;
    UpdateCollider;
  end;
end;

procedure TCastleSphere.SetDoubleSided(const Value: Boolean);
begin
  if FDoubleSided <> Value then
  begin
    FDoubleSided := Value;
    SphereNode.Solid := not Value;
  end;
end;

{$endif read_implementation}
