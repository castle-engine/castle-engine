{%MainUnit castlescene.pas}
{
  Copyright 2022-2024 Michalis Kamburelis.

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
  TCastleSpotLight = class(TCastlePunctualLight)
  strict private
    { Same thing as FLightNode, but with proper type. }
    FSpotLight: TSpotLightNode;

    FGizmoCoord: TCoordinateNode;

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
    procedure UpdateGizmoCoord;
  public
    const
      DefaultBeamWidth = Pi * 3 / 16;
      DefaultCutOffAngle = Pi / 4;
      DefaultRadius = 100.0;
      DefaultIntensity = 100.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Light color shining over surface. By default white. }
    property Color: TCastleColorRGB read FColor write SetColor;

    { Attenuation determines how quickly light intensity falls off with distance.
      By default Vector3(0, 0, 1), which means light falls off according to inverse square law. }
    property Attenuation: TVector3 read FAttenuation write SetAttenuation;
  published
    { Intensity, multiplied by color. Can be anything >= 0.
      Intuitively, how bright is the light.

      Following @url(https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_lights_punctual/README.md glTF lights specification),
      the spot light's intensity physical unit
      is luminous intensity in candela (lm/sr).
      This is exactly like TCastlePointLight, but different than TCastleDirectionalLight.

      The default intensity is 100.0, which is a reasonable value for a point/spot light,
      it remains bright for a few meters around the light source. }
    property Intensity: Single read FIntensity write SetIntensity {$ifdef FPC}default DefaultIntensity{$endif};

    { Light doesn't shine at all outside of a sphere with this radius.
      For performance reasons, set this as small as possible, to avoid even considering
      this light at further shapes.
      Set to -1 to have no limit. }
    property Radius: Single read FRadius write SetRadius {$ifdef FPC}default DefaultRadius{$endif};

    { Angle within which the spot light has maximum intensity.

      If this is >= CutOffAngle (which is the default case), then the light
      has maximum intensity  within CutOffAngle, and zero intensity outside CutOffAngle,
      and there it no smooth falloff.

      You need to make BeamWidth < CutOffAngle to have a smooth falloff at the egdes
      of the spot light. }
    property BeamWidth: Single read FBeamWidth write SetBeamWidth {$ifdef FPC}default DefaultBeamWidth{$endif};

    { Angle within which the spot light has any intensity.

      If BeamWidth < CutOffAngle then the light intensity falls off smoothly
      between BeamWidth and CutOffAngle.

      The light intensity is always zero outside of CutOffAngle,
      regardless of the BeamWidth value. }
    property CutOffAngle: Single read FCutOffAngle write SetCutOffAngle {$ifdef FPC}default DefaultCutOffAngle{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlespotlight_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleSpotLight.Create(AOwner: TComponent);

  function BuildGizmo: TX3DRootNode;
  begin
    Result := TX3DRootNode.Create;
    Result.AddChildren(Gizmo.LinesShape(FGizmoCoord));
    UpdateGizmoCoord;
  end;

begin
  inherited;

  // defaults
  FIntensity := DefaultIntensity;
  FRadius := DefaultRadius;
  FAttenuation := Vector3(0, 0, 1);
  FColor := WhiteRGB;
  FBeamWidth := DefaultBeamWidth;
  FCutOffAngle := DefaultCutOffAngle;

  // initialize light node
  FSpotLight := TSpotLightNode.Create;
  LightNode := FSpotLight;
  FPunctualLight := FSpotLight;

  // apply TCastleSpotLight defaults to FSpotLight
  FSpotLight.Color := FColor;
  FSpotLight.Attenuation := FAttenuation;
  FSpotLight.Intensity := FIntensity;
  FSpotLight.Radius := FRadius;
  FSpotLight.BeamWidth := FBeamWidth;
  FSpotLight.CutOffAngle := FCutOffAngle;

  // initialize Gizmo
  if Gizmo <> nil then
  begin
    if InternalCastleDesignData <> '' then
      Gizmo.SetIconUrl(InternalCastleDesignData + 'gizmos/light/light_spot.png');
    Gizmo.LoadVisualization(BuildGizmo);
  end;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlespotlight_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleSpotLight.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlespotlight_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleSpotLight.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ColorPersistent', 'AttenuationPersistent', 'Intensity', 'Radius',
       'BeamWidth', 'CutOffAngle'
     ]) then
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
    if Gizmo <> nil then
      UpdateGizmoCoord;
  end;
end;

procedure TCastleSpotLight.UpdateGizmoCoord;
const
  { Length of gizmo line.
    We need to do AdjustToLength later, otherwise the lines could be ~infinite
    of case of CutOffAngle = Pi / 2. }
  D = 10.0;
var
  Shift: Single;
begin
  Shift := Tan(CutOffAngle) * D;
  FGizmoCoord.SetPoint([
    Vector3(0, 0, 0), Vector3(0,  Shift, -D).AdjustToLength(D),
    Vector3(0, 0, 0), Vector3(0, -Shift, -D).AdjustToLength(D),
    Vector3(0, 0, 0), Vector3( Shift, 0, -D).AdjustToLength(D),
    Vector3(0, 0, 0), Vector3(-Shift, 0, -D).AdjustToLength(D)
  ]);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlespotlight_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
