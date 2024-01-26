# From X3D Call Pascal Code

You can enable the scripts inside X3D 3D model to call a particular ObjectPascal function.

This is one way to enable your 3D models to do something that is usually outside the scope (and security) of a normal X3D browser. This way you can enable X3D models to interact with some database, or desktop task, or... well, do anything that native ObjectPascal code can do. This approach gives the whole control of when the given operation is executed to the author of X3D 3D model. The ObjectPascal code only implements and registers methods, that may be then called at any time while the X3D is loaded.

Inside the X3D model, this is declared by "compiled:" protocol inside a Script node. See data/compiled_script_tests.x3dv. See https://castle-engine.io/x3d_extensions.php#section_ext_script_compiled for more information.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `x3d_call_pascal_code_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
