{%MainUnit castlescene.pas}
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

{ Punctual light: point, spot, directional. }

{$ifdef read_interface}

type
  { Ancestor class for all punctual lights: point, spot, directional.

    The name "punctual" follows the glTF terminology,
    https://github.com/KhronosGroup/glTF/tree/main/extensions/2.0/Khronos/KHR_lights_punctual .
    Punctual lights are infinitely small points that emit light from simple position
    or direction. This makes them different e.g. from area lights, of environment lights. }
  TCastlePunctualLight = class(TCastleAbstractLight)
  strict private
    FShadows: Boolean;
    procedure SetShadows(const Value: Boolean);
  protected
    FPunctualLight: TAbstractPunctualLightNode; //< Descendants should set it in constructor
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Cast shadows from this light.

      Right now this property activates
      @url(https://castle-engine.io/shadow_volumes shadow volumes).
      Please consult the @url(https://castle-engine.io/shadow_volumes shadow volumes documentation)
      about important requirements of this technique,
      namely: models that cast shadows must be 2-manifold.
      Moreover, right now, only one light in the world can cast shadows using
      shadow volumes.

      In the future we want to offer a choice between using
      @url(https://castle-engine.io/shadow_volumes shadow volumes)
      and
      @url(https://castle-engine.io/x3d_extensions_shadow_maps.php shadow maps). }
    property Shadows: Boolean read FShadows write SetShadows default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastlePunctualLight.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TCastlePunctualLight.SetShadows(const Value: Boolean);
begin
  if FShadows <> Value then
  begin
    FShadows := Value;
    FPunctualLight.ShadowVolumesMain := Value;
    FPunctualLight.ShadowVolumes := Value;
  end;
end;

function TCastlePunctualLight.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['Shadows']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
