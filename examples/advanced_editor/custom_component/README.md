# Custom Component in CGE editor

You can define custom components, specific to your project, that are available inside [CGE editor](https://castle-engine.io/manual_editor.php):

- Define a new component by creating a class descending from `TComponent`. You will usually want to descend from CGE class like `TCastleUserInterface` (to make new UI components) or `TCastleTransform` (to make new things that can be put inside a viewport).

    The `code/gamecontrols.pas` unit here demonstrates this by defininig `TImageGrid`.

- Register your component using `RegisterSerializableComponent`.

- Set `editor_units` in `CastleEngineManifest.xml` to include the unit with your components.

- Run the custom version of the editor, with your custom components, using _"Project -> Restart Editor (With Custom Components)"_ from the normal editor. Or use `castle-engine editor` from command-line, when inside the project. The custom editor will be automatically build.

- Done. Use the new component as a regular component.

    In this example, we placed `TImageGrid` in our main design, `data/gamestatemain.castle-user-interface`.

See https://castle-engine.io/manual_editor.php#section_custom_components for more information.
