# Sound reverb

Reverb (reverberation): the echoes of a room, a hall or a cave added to the spatial sounds, using `SoundEngine.Reverb` (`TCastleSoundReverb`).

- A green ball with a `TCastleSoundSource` plays a short sound every second (a spatial sound), so the reverb after it is easy to hear.
- The buttons set `SoundEngine.Reverb` to a few environments: small room, bathroom, concert hall, cave, hangar, or no reverb.
- _+_ / _-_ change the decay time (`TCastleSoundReverb.DecayTime`): how long the reverb lasts.
- _"Play non-spatial"_ plays the same sound as non-spatial (like user interface sounds and music): the reverb doesn't affect it.

Using the reverb in your own application:

```pascal
SoundEngine.Reverb.DecayTime := 3.0;
SoundEngine.Reverb.ReflectionsGain := 0.25;
SoundEngine.Reverb.LateReverbGain := 1.2;
SoundEngine.Reverb.Enabled := true;
```

`TCastleSoundReverb` has the parameters of the OpenAL EFX _EAX reverb_ effect, with the same defaults and ranges. `TCastleSoundReverb.SetDefaults` returns to the defaults (a generic medium room). The environments in this example are rough approximations, made by ear.

The reverb needs a sound backend that supports it, `SoundEngine.ReverbSupported` tells. Right now this is the OpenAL backend (the default on desktop), when the OpenAL implementation has the EFX extension. [OpenAL Soft](https://openal-soft.org/) has it, and so does Creative's OpenAL for Windows. Without it, the reverb is silently ignored.

Sound `data/blip.wav` by Lokif, CC0, see `data/AUTHORS.txt`.

![Screenshot](screenshot.png)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `sound_reverb_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `sound_reverb_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
