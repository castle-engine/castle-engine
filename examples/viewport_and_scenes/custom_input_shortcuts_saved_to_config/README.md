# Using TInputShortcut for configurable input shortcuts

This example shows how to create user-configurable input shortcuts (own `TInputShortcut` instances, saved and loaded to UserConfig file) and use them for a `TCastleWalkNavigation` or for custom actions.

Use AWSD to move camera (up/down or left/right strife).

Use C to change material to a random color.

Note how all key/mouse shortcuts are expressed as `TInputShortcut` instance, and thus can be easily changed.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `multiple_viewports_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
