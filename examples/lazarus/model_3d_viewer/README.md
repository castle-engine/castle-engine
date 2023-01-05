# 3D model viewer on Lazarus (LCL) form using TCastleControl

This is a simple model viewer (glTF, X3D, etc.) in Lazarus, based on `TCastleControl` component. Something like "mini-view3dscene" (see https://castle-engine.io/view3dscene.php ) but on a Lazarus (LCL) form.

## Building

1. Install Lazarus packages following https://castle-engine.io/control_on_form .

2. Open this project in Lazarus and compile + run it from Lazarus, as usual Lazarus application.

    - Or use [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_ or _"Compile And Run"_.

    - Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

## Usage:

- Open any model supported by CGE (see [supported model format](https://castle-engine.io/creating_data_model_formats.php)), like glTF, X3D and more. You can open using _"File -> Open"_ menu item or just pass the filename on command-line.

- Switch navigation mode using the buttons.

    See view3dscene docs for keys to control Walk/Examine navigation [https://castle-engine.io/view3dscene.php], or just try moving with arrows, mouse etc. Remember that you must have focus on 3D area to use them --- press Escape (menu item _"View -> Switch focus to 3D area"_) when needed.

- You can also change camera vectors by hand if you like. Just modify values in edit boxes at the bottom and press "Change".
