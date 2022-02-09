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

{ Spot light. }

{$ifdef read_interface}

type
  { Spot light shines a cone of light, from a given location, along a direction
    (-Z in local coordinates).

    See @url(https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/lighting.html#SpotLight
    X3D SpotLight specification) for various details. }
  TCastleSpotLight = class(TCastleAbstractLight)
  strict private
    { Same thing as FLightNode, but with proper type. }
    FSpotLight: TSpotLightNode;

    FColor: TCastleColorRGB;
    FAttenuation: TVector3;
    FIntensity: Single;
    FRadius: Single;
    FBeamWidth: Single;
    FCutOffAngle: Single;

    procedure SetColor(const Value: TCastleColorRGB);
    procedure SetAttenuation(const Value: TVector3);
    procedure SetIntensity(const Value: Single);
    procedure SetRadius(const Value: Single);
    procedure SetBeamWidth(const Value: Single);
    procedure SetCutOffAngle(const Value: Single);
  public
    const
      DefaultBeamWidth = Pi / 4;
      DefaultCutOffAngle = Pi / 2;
      DefaultRadius = 100.0;

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Light color shining over surface. By default white. }
    property Color: TCastleColorRGB read FColor write SetColor;

    { Attenuation determines how quickly light intensity falls off with distance.
      By default Vector3(0, 0, 1), which means light falls off according to inverse square law. }
    property Attenuation: TVector3 read FAttenuation write SetAttenuation;
  published
    { Intensity, multiplied by color. Can be anything >= 0. }
    property Intensity: Single read FIntensity write SetIntensity {$ifdef FPC}default 1.0{$endif};

    { Light doesn't shine at all outside of a sphere with this radius.
      For performance reasons, set this as small as possible, to avoid even considering
      this light at further shapes.
      Set to -1 to have no limit. }
    property Radius: Single read FRadius write SetRadius {$ifdef FPC}default DefaultRadius{$endif};

    { Angle within which the spot light has maximum intensity. }
    property BeamWidth: Single read FBeamWidth write SetBeamWidth {$ifdef FPC}default DefaultBeamWidth{$endif};

    { Angle within which the spot light has any intensity.
      The light intensity falls off between BeamWidth and CutOffAngle, and is zero outside of CutOffAngle. }
    property CutOffAngle: Single read FCutOffAngle write SetCutOffAngle {$ifdef FPC}default DefaultCutOffAngle{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlespotlight_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleSpotLight.Create(AOwner: TComponent);
begin
  inherited;

  FIntensity := 1;
  FRadius := DefaultRadius;
  FAttenuation := Vector3(0, 0, 1);
  FColor := WhiteRGB;
  FBeamWidth := DefaultBeamWidth;
  FCutOffAngle := DefaultCutOffAngle;

  { internal stuff create }
  FSpotLight := TSpotLightNode.Create;
  LightNode := FSpotLight;
  GizmoTextureUrl := InternalCastleDesignData + 'gizmos/light/light_spot.png';

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlespotlight_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

function TCastleSpotLight.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ColorPersistent') or
     (PropertyName = 'AttenuationPersistent') or
     (PropertyName = 'Intensity') or
     (PropertyName = 'Radius') or
     (PropertyName = 'BeamWidth') or
     (PropertyName = 'CutOffAngle') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleSpotLight.SetColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    FSpotLight.Color := Value;
  end;
end;

procedure TCastleSpotLight.SetAttenuation(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FAttenuation, Value) then
  begin
    FAttenuation := Value;
    FSpotLight.Attenuation := Value;
  end;
end;

procedure TCastleSpotLight.SetIntensity(const Value: Single);
begin
  if FIntensity <> Value then
  begin
    FIntensity := Value;
    FSpotLight.Intensity := Value;
  end;
end;

procedure TCastleSpotLight.SetRadius(const Value: Single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    FSpotLight.Radius := Value;
  end;
end;

procedure TCastleSpotLight.SetBeamWidth(const Value: Single);
begin
  if FBeamWidth <> Value then
  begin
    FBeamWidth := Value;
    FSpotLight.BeamWidth := Value;
  end;
end;

procedure TCastleSpotLight.SetCutOffAngle(const Value: Single);
begin
  if FCutOffAngle <> Value then
  begin
    FCutOffAngle := Value;
    FSpotLight.CutOffAngle := Value;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlespotlight_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
