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

      This simple property is @bold(experimental). Right now:

      @unorderedList(
        @item(It only makes shadow volumes.
          In the future we want to make shadow maps available through this property,
          and actually default.

          Note that shadow volumes only cast shadows from shapes that are 2-manifold.
          See https://castle-engine.io/x3d_extensions_shadow_volumes.php .

          TODO: Shadow maps do not yet work cross-scene.
          See https://github.com/castle-engine/castle-engine/issues/284 .
        )

        @item(It only takes into account 1 light. Do not set this on more than 1 light,
          it is undefined now which light we will choose (and in the future,
          we may honor it on all lights -- with both shadow maps and shadow volumes;
          shadow maps already honor multiple lights).
        )

        @item(It does not work on OpenGLES (mobile).

          See PR https://github.com/castle-engine/castle-engine/pull/312 for (in progress) fix.
        )
      )
    }
    property Shadows: Boolean read FShadows write SetShadows default false; experimental;
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
