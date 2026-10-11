# Shadow Volumes From Several Lights

Shadow volumes cast by more than one light at the same time, using `TCastleViewport.ShadowVolumesMaxLights`.

Three colored point lights circle above a box, a sphere and a cylinder. Every light has `Shadows` = `true`. With `ShadowVolumesMaxLights` = 3 every object casts three shadows, and each shadow is lit (tinted) by the two other lights. Press _M_ to compare with `ShadowVolumesMaxLights` = 1 (the default, and the only behavior before this property existed): only one light casts shadows, and the other shadow-casting lights are turned off inside its shadow, so the shadow is black.

![Screenshot](screenshot.png)

Keys:

- _M_: switch `ShadowVolumesMaxLights` between 3 and 1.
- _1_, _2_, _3_: turn a light on / off.
- _S_: shadow volumes on / off (`TCastleViewport.ShadowVolumes`).
- _V_: show the shadow volumes (`TCastleViewport.ShadowVolumesRender`).
- _Space_: stop / resume moving the lights.
- Drag with the mouse to rotate the view.

## How to use it

1. Set `Shadows` = `true` on the lights that should cast shadows (`TCastlePointLight`, `TCastleSpotLight`, `TCastleDirectionalLight`). This sets the X3D light fields `shadowVolumes` and `shadowVolumesMain` to `TRUE`. When you use X3D files, set these fields on the light nodes.

2. Set `TCastleViewport.ShadowVolumesMaxLights` to the maximum number of lights that may cast shadows at the same time:

    ```delphi
    Viewport.ShadowVolumesMaxLights := 3;
    ```

That's it. The first `ShadowVolumesMaxLights` lights that are on, with `shadowVolumes` = `shadowVolumesMain` = `TRUE`, cast shadows. The shadow casters have the same requirements as with one light: they must be [2-manifold](https://castle-engine.io/shadow_volumes) (closed).

If you have more shadow-casting lights than the limit (for example lights all over a large level), choose the ones that matter each frame, for example the lights closest to the camera. Set `TCastlePunctualLight.Shadows` (or the X3D `shadowVolumes` and `shadowVolumesMain` fields) to `true` on them, and to `false` on the others.

## How it works

With `ShadowVolumesMaxLights` = 1, CGE renders shadow volumes like this (unchanged):

1. Render the shapes that don't receive shadows, with all lights.
2. Render the shapes that receive shadows with the shadow-casting lights turned off (`shadowVolumes` = `TRUE`). This fills the depth buffer.
3. Render the shadow volumes of the main light into the stencil buffer.
4. Render the shapes that receive shadows again, with all lights, only where the stencil buffer is 0 (not in shadow). The depth test "less or equal" draws over the same surfaces.
5. Render the transparent shapes, with all lights.

With `ShadowVolumesMaxLights` > 1 and at least 2 lights found, steps 3 and 4 are done for every light, adding (blending) the light's contribution, like in Doom 3's renderer:

1. Render the shapes that don't receive shadows, with all lights.
2. Render the shapes that receive shadows with the found lights turned off. This fills the depth buffer, and draws everything that doesn't depend on these lights: emissive color, ambient color, other lights.
3. For each light:
    1. Clear the stencil buffer.
    2. Render the shadow volumes of this light into the stencil buffer.
    3. Render the shapes that receive shadows, with only this light, only where the stencil buffer is 0, adding the result to the color buffer (blending "one, one", no depth buffer writes). The shaders of this pass leave out everything that doesn't depend on the light (`CASTLE_LIGHT_CONTRIBUTION_ONLY` in the GLSL code): emissive color, ambient color, the color of unlit materials. Fog only attenuates the light (black fog color), as the base pass already added the fog color.
4. Render the transparent shapes, with all lights.

When only one light is found in a frame, rendering is the same as with `ShadowVolumesMaxLights` = 1.

### Cost

Every light costs one more rendering of its shadow volumes and one more rendering of the shapes that receive shadows. With _N_ lights, the shapes receiving shadows are rendered _N + 1_ times (instead of 2). The extra shader programs (one per shape and lighting setup) are compiled the first time each one is needed, and then cached.

Other approaches considered:

- One stencil bit (or counter) per light and one pass per _combination_ of lights in shadow: exact also with gamma correction, but _2^N_ passes.
- Shadow maps: CGE supports them for spot and directional lights. Point lights would need cube shadow maps.

## Limitations

- **Gamma correction and tone mapping.** The light contributions are added in the color buffer, after the shaders apply gamma correction or tone mapping. Adding colors after gamma correction gives a brighter result than adding the lights before it, because gamma(a) + gamma(b) > gamma(a + b). So when the receiving shapes use gamma correction (`ColorSpace` = `csLinear`, or the default `csLinearWhenPhysicalMaterial` with PBR materials) or tone mapping, the result is too bright where several lights overlap. It is exact for Phong and unlit materials with the default `ColorSpace`, or for all materials with `ColorSpace` = `csSRGB`. This demo uses Phong materials (`TCastleAbstractPrimitive.Material` = `pmPhong`). Fixing this needs rendering to a linear (sRGB or floating-point) framebuffer.
- Colors are clamped to 1 in each pass, not after adding all passes.
- Only lights in scenes that cast global lights are considered when `ShadowVolumesMaxLights` > 1: `TCastle*Light` components, and scenes with `TCastleScene.CastGlobalLights` (or the deprecated `MainScene`).
- When 2 or more lights cast shadows, the other lights with `shadowVolumes` = `TRUE` (because `shadowVolumesMain` = `FALSE`, or over the limit) light everything, also inside the shadows. With one casting light they are turned off inside its shadow, as before.
- The "fake light" trick (a main light that is off, only to position the shadows) works only when it is the only casting light: when `ShadowVolumesMaxLights` > 1, lights that are off don't count.
- Transparent shapes don't receive shadows, as with one light.

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `shadow_volumes_several_lights_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `shadow_volumes_several_lights_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
