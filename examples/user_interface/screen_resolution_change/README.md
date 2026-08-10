# Test Changing Screen Resolutions

Test changing physical screen resolution using `Application.TryVideoChange`.

This changes actual monitor resolution, and makes sense for full-screen games.

![Screenshot](screenshot.png)

## Warning

Changing the physical screen resolution is **not recommended** and it's **not implemented** on most platforms except Windows.

See https://castle-engine.io/window_size#video_change for an explanation.

## Building

Using [Castle Game Engine](https://castle-engine.io/).

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `screen_resolution_change_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `screen_resolution_change_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.