# Dragging Test

Test two approaches to implement dragging in CGE:

1. Just watch `Motion` events (e.g. by overriding `TCastleView.Motion`), and subtract `new mouse position - old mouse position`.

    This is simple, and makes sense if

    - uses can see the mouse cursor position
    - and the dragging area is naturally limited to the window.

    E.g. this makes sense when you implement drag-and-drop.

2. Use the same logic as _mouse look_. In this case the mouse cursor should be hidden, and you use `TCastleContainer.MouseLookDelta` and friends to

    - read mouse motion movement,
    - and keep the mouse position around the middle of the window (to pretend that the dragging area is infinite).

    This makes sense if you want the movement of something to *not* be limited by window borders.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `dragging_test_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `dragging_test_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.