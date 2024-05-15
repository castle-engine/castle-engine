# Progress Bar UI Example

Shows how to make a progress bar where the image inside ("filler" of the progress bar) is cropped inside.

Uses `TCastleImageControl.Clip` and `TCastleImageControl.ClipLine` (equation of a line in `Ax + By + z = 0` form).

Note: If you don't need cropping, there are other ways to do this. E.g. one can resize any control using `WidthFraction` to adjust to the parent's width.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `progress_bar_ui_example_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `progress_bar_ui_example_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
