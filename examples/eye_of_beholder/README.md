# Eye of the Beholder

Demo of a game like "Eye of the Beholder":

- first-person view
- movement on a grid (AWSD keys)
    - walking in a labirynth designed in Blender
    - we prevent walking on a tile if we detect collision
- rotations by 90 degrees (QE keys)
- UI with stats for a band of 6 heroes (not functional, just a demo)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `eye_of_beholder_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
