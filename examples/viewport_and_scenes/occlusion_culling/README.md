# Occlusion Culling Demo

Demo of occlusion culling: https://castle-engine.io/occlusion_culling .

Just move around and notice in status that when occlusion culling is _on_, we can eliminate most shapes from being displayed, instead we draw a trivial _"occlusion query box"_ just to detect their visibility next frame.

Note that controls of this demo are deliberately useful also on mobile (drag on screen to rotate, use the walk control in bottom-right corner to move).

![Screenshot](screenshot.png)

![Screenshot from editor](screenshot_editor.png)

![Screenshot running on Android](screenshot_android.png)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `occlusion_culling_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `occlusion_culling_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
