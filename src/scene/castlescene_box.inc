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
  { Box with configurable size, position and material. }
  TCastleBox = class(TCastleAbstractPrimitive)
  strict private
    BoxNode: TBoxNode;
    FSize: TVector3;
    FDoubleSided: Boolean;
    procedure SetSize(const Value: TVector3);
    procedure SetDoubleSided(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Size of the box in 3D. By default (2, 2, 2). }
    property Size: TVector3 read FSize write SetSize;

    { Get or set the box @link(Size) and @link(Translation) using the TBox3D type. }
    // TODO: property Box: TBox3D read GetBox write SetBox;
  published
    { When this is @true, the object is visible (and lit) from both sides. }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default false;
  
  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlebox_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleBox ---------------------------------------------------------------- }

constructor TCastleBox.Create(AOwner: TComponent);
begin
  inherited;

  { default properties values }
  FSize := Vector3(2, 2, 2);

  BoxNode := TBoxNode.Create;
  BoxNode.Size := FSize;
  ShapeNode.Geometry := BoxNode;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlebox_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleBox.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlebox_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  inherited;
end;

function TCastleBox.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'SizePersistent') or
     (PropertyName = 'DoubleSided') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleBox.SetSize(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FSize, Value) then
  begin
    FSize := Value;
    BoxNode.Size := FSize;
  end;
end;

procedure TCastleBox.SetDoubleSided(const Value: Boolean);
begin
  if FDoubleSided <> Value then
  begin
    FDoubleSided := Value;
    BoxNode.Solid := not Value;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlebox_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
