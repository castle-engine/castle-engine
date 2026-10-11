# Shader Effects On Several Lights

Shader effects on light sources: the light of each lamp shines in rings around it, like light through a patterned lampshade.

Three lamps (`TCastlePointLight`) light three floor tiles. Each lamp gets an `Effect` using `TCastleAbstractLight.SetEffects`. All effects use the same GLSL code, [data/shaders/rings_light.fs](data/shaders/rings_light.fs), with a different value of the `ring_count` uniform, so every tile shows a different number of rings.

![Screenshot](screenshot.png)

Keys:

- _Space_: animate the ring counts.
- Drag with the mouse to rotate the view.

## How it works

The lamps, tiles and camera are set up in the [CGE editor](https://castle-engine.io/editor), in [data/gameviewmain.castle-user-interface](data/gameviewmain.castle-user-interface). Each lamp has a `Radius` that reaches only its own tile.

The code in [code/gameviewmain.pas](code/gameviewmain.pas) only adds the effects:

```delphi
Effect := TEffectNode.Create;
Effect.Language := slGLSL;
Effect.SetShaderLibraries(['castle-shader:/EyeWorldSpace.glsl']);
RingCount := TSFFloat.Create(Effect, true, 'ring_count', 2);
Effect.AddCustomField(RingCount);
Part := TEffectPartNode.Create;
Part.ShaderType := stFragment;
Part.SetUrl(['castle-data:/shaders/rings_light.fs']);
Effect.SetParts([Part]);

Lamp2.SetEffects([Effect]);
```

An effect on a light can define `PLUG_light_scale`. It is called for every point lit by this light, and can scale the light contribution there:

```glsl
void PLUG_light_scale(inout float scale, const in vec3 normal_eye, const in vec3 light_dir)
```

`light_dir` is the direction from the lit point to the light, in eye space. The shader converts it to world space with `direction_eye_to_world_space`, a function of the `EyeWorldSpace` shader library (enabled by `Effect.SetShaderLibraries`). From the world direction it calculates how far (horizontally) the point is from the lamp, relative to the lamp height, and makes rings from it.

The fields of the `Effect` node are uniforms in the shader. To change a uniform later, `Send` a new value to the field: `RingCount.Send(...)`.

Each lamp has its own `Effect` node, so each lamp can have different uniform values. Shapes lit by lights with different `Effect` nodes use different shader programs. If many lights should use the same effect with the same uniform values, give them the same `Effect` node.

See [shaders documentation](https://castle-engine.io/shaders) for more information about shader effects.

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `shader_effects_on_several_lights_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `shader_effects_on_several_lights_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
