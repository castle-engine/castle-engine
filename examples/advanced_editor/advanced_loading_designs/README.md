# Advanced Loading Of Designs

This example some advanced features of how you can use _"designs"_.

"Designs" are files `xxx.castle-user-interface`, `xxx.castle-transform`, `xxx.castle-component` that you can visually create in the CGE editor.

The default CGE templates start with just one design for one `TCastleView` (like `gameviewmain.castle-user-interface` for `TViewMain`) and the design is set using `DesignUrl` and automatically shown/hidden when the view start/stops.

But the designs are much more flexible. You can create any number of design files, load them (once or many times), and show/hide whenever you need. This demo shows how:

1. You can design reusable user interface (hierarchy of objects, with root descending from `TCastleUserInterface`) by `xxx.castle-user-interface` files.

    Such reusable components may be instantiated using any of these methods:

    - instantiate by code using global `UserInterfaceLoad` function, to easily instantiate them once

    - instantiate by code using `TCastleComponentFactory.UserInterfaceLoad`, to efficiently instantiate them multiple times

    - instantiate using `TCastleDesign` (you can do this visually, in the editor, you can also create `TCastleDesign` using Pascal code).

    This is like using Unity prefabs to design UI.

2. You can design reusable transformation ((hierarchy of objects, with root descending from `TCastleTransform`) by `xxx.castle-transform` files.

    Such reusable components may be instantiated using any of these methods:

    - instantiate by code using global `TransformLoad` function, to easily instantiate them once

    - instantiate by code using `TCastleComponentFactory.TransformLoad`, to efficiently instantiate them multiple times

    - instantiate using `TCastleTransformDesign` (you can do this visually, in the editor, you can also create `TCastleTransformDesign` using Pascal code).

    This is like using Unity prefabs to design game stuff (3D or 2D).

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `advanced_loading_designs_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `advanced_loading_designs_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
