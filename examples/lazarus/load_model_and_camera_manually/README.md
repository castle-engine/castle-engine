# Manually creating CGE Pascal components from code, and using them in TCastleControl

Demo how to use Pascal code to

- load 3D scene
- set camera
- setup navigation

in an LCL application, and load it to `TCastleControl`. See https://castle-engine.io/control_on_form for more information about using `TCastleControl`.

Note: we do this by code here, just as a demo. The setup shown here could be also just designed in CGE editor, and loaded easily using `TCastleControl.DesignUrl` -- this is generally better than doing it by code, as it means you can visually see what you're doing at development.

This example is for Lazarus (LCL) TCastleControl. Though the principle (API how to change views) works everywhere -- with any container, including Delphi VCL and FMX TCastleControl, and including TCastleWindow.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

1. Install Lazarus packages following https://castle-engine.io/control_on_form .

2. Open this project in Lazarus and compile + run it from Lazarus, as usual Lazarus application.

    - Or use [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_ or _"Compile And Run"_.

    - Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.
