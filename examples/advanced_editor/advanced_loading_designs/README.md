# Advanced Loading Of Designs

This example some advanced features of how you can use _"designs"_.

"Designs" are files `xxx.castle-user-interface`, `xxx.castle-transform`, `xxx.castle-component` that you can visually create in the CGE editor.

The default CGE templates start with just one design for one TUIState (like `gamestatemain.castle-user-interface` for `TStateMain`) and the design is set using `DesignUrl` and automatically shown/hidden when the state start/stops.

But the designs are much more flexible. You can create any number of design files, load them (once or many times), and show/hide whenever you need. This demo shows how:

1. You can design reusable user interface (hierarchy of objects, with root descending from `TCastleUserInterface`) by `xxx.castle-user-interface` files.

    Such reusable components may be instantiated using any of these methods:

    - instantiate by code using global `UserInterfaceLoad` function, to easily instantiate them once

    - instantiate by code using `TSerializedComponent.UserInterfaceLoad`, to efficiently instantiate them multiple times

    - instantiate using `TCastleDesign` (you can do this visually, in the editor, you can also create `TCastleDesign` using Pascal code).

    This is like using Unity prefabs to design UI.

2. You can design reusable transformation ((hierarchy of objects, with root descending from `TCastleTransform`) by `xxx.castle-transform` files.

    Such reusable components may be instantiated using any of these methods:

    - instantiate by code using global `TransformLoad` function, to easily instantiate them once

    - instantiate by code using `TSerializedComponent.TransformLoad`, to efficiently instantiate them multiple times

    - instantiate using `TCastleTransformDesign` (you can do this visually, in the editor, you can also create `TCastleTransformDesign` using Pascal code).

    This is like using Unity prefabs to design game stuff (3D or 2D).
