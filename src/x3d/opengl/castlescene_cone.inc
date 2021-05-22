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
  { Cone with configurable size, position and material. }
  TCastleCone = class(TCastleAbstractPrimitive)
  strict private
    ConeNode: TConeNode;
    FBottomRadius, FHeight: Single;
    FDoubleSided: Boolean;
    procedure SetBottomRadius(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetDoubleSided(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Cone bottom radius. }
    property BottomRadius: Single read FBottomRadius write SetBottomRadius default 1;

    { Cone height. }
    property Height: Single read FHeight write SetHeight default 2;

    { When this is @true, the object is visible (and lit) from both sides. }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default false;
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

  ConeNode := TConeNode.Create;
  ConeNode.BottomRadius := FBottomRadius;
  ConeNode.Height := FHeight;
  ShapeNode.Geometry := ConeNode;
end;

function TCastleCone.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'BottomRadius', 'Height', 'DoubleSided':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

procedure TCastleCone.SetBottomRadius(const Value: Single);
begin
  if FBottomRadius <> Value then
  begin
    FBottomRadius := Value;
    ConeNode.BottomRadius := FBottomRadius;
  end;
end;

procedure TCastleCone.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    ConeNode.Height := FHeight;
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

{$endif read_implementation}
