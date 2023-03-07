# Demos that use Castle Game Engine in an outdated (not advised) way

This directory contains demos that are dear to my heart,
and I want to have them as _Castle Game Engine_ demos,
but the way they are done *right now* is not something I want to advise to new developers.
So they are not good as demos "how to make this or that in CGE".

TODO: All these demos are "TODO, to remake them using new CGE API".
Because the demos present something useful / pretty,
we want to show how to do it using CGE.

In particular:

- `isometric_game/` reads a map in custom (text file) format and renders it using TDrawableImage.

    While this is allowed, it not advised. I would rather recommend to

    - Use Tiled ( https://www.mapeditor.org/ ) to design such map, and render it using our `TCastleTiledMapControl`.

    - Or use TCastleImageTransform inside a TCastleViewport to represent all images. Once we add "snapping" for TCasleTransform in editor, you will even be able to design such map in CGE editor.

    See https://castle-engine.io/using_images , https://castle-engine.io/how_to_render_2d .

    The effort to show a "new way" is already started -- see `examples/isometric_game` for a new version. We only keep this deprecated version to show alternative rendering method.

- `fixed_camera_game/` sets up a specialized rendering using code. This can be now largely designed in CGE editor and be much more flexible.

- `joystick/` uses UI not designed in editor. Also it shows a complicated joystick API, that we hope to improve in near future. Reading joystick axis and button should be easier, and account for various joystick types better.
