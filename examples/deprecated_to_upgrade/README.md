# Demos that use Castle Game Engine in an outdated (not advised) way

This directory contains demos that are dear to my heart,
and I want to have them as _Castle Game Engine_ demos,
but the way they are done *right now* is not something I want to advise to new developers.
So they are not good as demos "how to make this or that in CGE".

TODO: All these demos are "TODO, to remake them using new CGE API".
Because the demos present something useful / pretty,
we want to show how to do it using CGE.

In particular:

- `fixed_camera_game/` sets up a specialized rendering using code. This can be now largely designed in CGE editor and be much more flexible.

- `joystick/` uses UI not designed in editor. Also it shows a complicated joystick API, that we hope to improve in near future. Reading joystick axis and button should be easier, and account for various joystick types better.
