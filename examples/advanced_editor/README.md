Show some advanced features of our https://castle-engine.io/manual_editor.php :

1. You can design reusable components that descend from `TCastleUserInterface` by `xxx.castle-user-interface` files. Such reusable components may be:

    - instantiated by code using global `UserInterfaceLoad` function, to easily instantiate them once

    - instantiated by code using `TSerializedComponent.UserInterfaceLoad` to efficiently instantiate them multiple times

    - instantiated in editor using `TCastleDesign`.

    This is like using Unity prefabs to design UI.

2. You can design reusable components that descend from `TCastleTransform` by `xxx.castle-transform` files. Such reusable components may be:

    - instantiated by code using global `TransformLoad` function, to easily instantiate them once

    - instantiated by code using `TSerializedComponent.TransformLoad` to efficiently instantiate them multiple times

    - instantiated in editor using `TCastleTransformDesign`.

    This is like using Unity prefabs to design game stuff (3D or 2D).

3. You can define custom components in your project.

     - To do this, you need to registed your component using `RegisterSerializableComponent`.

     - Moreover, you have to set `editor_units` in `CastleEngineManifest.xml` to include the unit with your components.

     - Run the custom version of the editor, with your custom components, using _"Project -> Restart Editor (With Custom Components)"_ from the normal editor. Or use `castle-engine editor` from command-line, when inside the project. The custom editor will be automatically build.

     - See https://castle-engine.io/manual_editor.php#section_custom_components .
