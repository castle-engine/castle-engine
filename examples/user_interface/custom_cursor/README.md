# Custom Cursor

Example how to define a custom cursor in _Castle Game Engine_.

- Define a class `TCustomCursor`, descending from `TCastleUserInterface`, that contains your custom cursor. It can be any CGE UI component, like

    - TCastleImageControl, to express a cursor as a simple image.

    - TCastleViewport with TCastleScene, to express a cursor as a full-featured animated scene.

    You can create the contents of `TCustomCursor` in various ways. We present an approach that loads a designed `xxx_cursor.castle-user-interface` file, so custom cursors can be designed in the CGE editor.

- `TCustomCursor` class also implements:

    - Synchronizing the displayed cursor UI with `Container.MousePosition`.

    - It must be always displayed on top, as cursor is on top of everything, using `KeepInFront`.

- To use it:

    - Set `Cursor` to `mcForceNone` to hide the system cursor.

    - Create `TCustomCursor` instance and add it to `Window.Controls` or inside some state. This example adds the cursor inside the `ApplicationInitialize`, so it is available in all game states.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `custom_cursor_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).`

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `custom_cursor_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
