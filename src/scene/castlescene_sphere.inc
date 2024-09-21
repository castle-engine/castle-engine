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
  { Sphere with configurable size, position and material. }
  TCastleSphere = class(TCastleAbstractPrimitive)
  strict private
    SphereNode: TSphereNode;
    FRadius: Single;
    FDoubleSided: Boolean;
    FSlices, FStacks: Cardinal;
    procedure SetRadius(const Value: Single);
    procedure SetDoubleSided(const Value: Boolean);
    procedure SetSlices(const Value: Cardinal);
    procedure SetStacks(const Value: Cardinal);
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

    { How many "slices" to use to approximate the sphere.
      The sphere is rendered using polygons (triangles and quads)
      that approximate the sphere shape.
      The more slices, the more the sphere will look like a sphere.

      Slices divide the objects like slices of a pizza.

      The default value of 0 means to follow global variable
      @link(DefaultTriangulationSlices).}
    property Slices: Cardinal read FSlices write SetSlices default 0;

    { How many "stacks" to use to approximate the sphere.
      The sphere is rendered using polygons (triangles and quads)
      that approximate the sphere shape.
      The more stacks, the more the sphere will look like a sphere.

      Stacks divide the objects like stacks of a tower.

      The default value of 0 means to follow global variable
      @link(DefaultTriangulationStacks).}
    property Stacks: Cardinal read FStacks write SetStacks default 0;
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
  SphereNode.Solid := not FDoubleSided;
  SphereNode.Slices := FSlices;
  SphereNode.Stacks := FStacks;
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
  if ArrayContainsString(PropertyName, ['Radius', 'DoubleSided',
       'Slices', 'Stacks']) then
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

procedure TCastleSphere.SetSlices(const Value: Cardinal);
begin
  if FSlices <> Value then
  begin
    FSlices := Value;
    SphereNode.Slices := FSlices;
  end;
end;

procedure TCastleSphere.SetStacks(const Value: Cardinal);
begin
  if FStacks <> Value then
  begin
    FStacks := Value;
    SphereNode.Stacks := FStacks;
  end;
end;

{$endif read_implementation}
