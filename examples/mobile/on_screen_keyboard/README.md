# On-screen keyboard test

This is an example of using the on-screen keyboard:

- tap edit to show the keyboard
- tap another edit to switch focus to it
- third edit shows password mode

To make the `TCastleEdit` receive focus and open on-screen keyboard correctly in your applications:

- With FPC, use `<service name="keyboard" />` in the `CastleEngineManifest.xml`. See [keyboard service documentation](https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/services/keyboard/README.adoc). TODO: It is only available for Android for now.
- With Delphi, adding the service doesn't matter. We rely on FireMonkey for this and it works with both Android and iOS.
- Set [AutoOnScreenKeyboard](https://castle-engine.io/api?id=TCastleEdit.AutoOnScreenKeyboard) to `true` on the `TCastleEdit` instance (you can set it from the editor, or from code).
- If you want to focus the edit before user touches it, do it by `Edit1.Focused := true`

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `on_screen_keyboard_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `on_screen_keyboard_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
