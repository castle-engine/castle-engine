{
  Copyright 2022-2022 Michalis Kamburelis.

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
  { Control fog in 3D world. }
  TCastleFog = class(TCastleComponent)
  strict private
    FFogNode: TFogNode;
    FColor: TCastleColorRGB;
    FFogType: TFogType;
    FVisibilityRange: Single;
    procedure SetColor(const Value: TCastleColorRGB);
    procedure SetFogType(const Value: TFogType);
    procedure SetVisibilityRange(const Value: Single);
    procedure UpdateFogNode;
  public
    const
      DefaultColor: TCastleColorRGB = (X: 0.5; Y: 0.5; Z: 0.5);
      DefaultFogType = ftExponential;
      DefaultVisibilityRange = 100;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { In the distance, the rendered color changes into this fog color.
      By default this is gray (RGB 0.5,0.5,0.5). }
    property Color: TCastleColorRGB read FColor write SetColor;

    { @exclude }
    property InternalFogNode: TFogNode read FFogNode;
  published
    { Type of fog determines how quickly is the @link(Color) applied.
      The default is a more realistic ftExponential. }
    property FogType: TFogType read FFogType write SetFogType default DefaultFogType;

    { The fog reaches maximum intensity, that is all colors are equal to @link(Color),
      at this distance from the observer. }
    property VisibilityRange: Single read FVisibilityRange write SetVisibilityRange
      {$ifdef FPC}default DefaultVisibilityRange{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlefog_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleFog ----------------------------------------------------------------- }

constructor TCastleFog.Create(AOwner: TComponent);
begin
  inherited;

  { set default values }
  FColor := DefaultColor;
  FFogType := DefaultFogType;
  FVisibilityRange := DefaultVisibilityRange;

  FFogNode := TFogNode.Create;
  UpdateFogNode;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlefog_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleFog.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlefog_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  FreeAndNil(FFogNode);
  inherited;
end;

procedure TCastleFog.UpdateFogNode;
begin
  FFogNode.Color := Color;
  FFogNode.FogType := FogType;
  FFogNode.VisibilityRange := VisibilityRange;
end;

procedure TCastleFog.SetColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    UpdateFogNode;
  end;
end;

procedure TCastleFog.SetFogType(const Value: TFogType);
begin
  if FFogType <> Value then
  begin
    FFogType := Value;
    UpdateFogNode;
  end;
end;

procedure TCastleFog.SetVisibilityRange(const Value: Single);
begin
  if FVisibilityRange <> Value then
  begin
    FVisibilityRange := Value;
    UpdateFogNode;
  end;
end;

function TCastleFog.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ColorPersistent') or
     (PropertyName = 'FogType') or
     (PropertyName = 'VisibilityRange') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlefog_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
