# Animate Transform By Code

Example how to animate (change, modify) the model using ObjectPascal code.
Makes a trivial animation by changing TTransformNode.Translation values
in each Update.
You can rotate/move the scene by dragging with mouse.

Generally, you can just change the nodes graph (in Scene.RootNode)
however you like, whenever you like. TX3DNode class has a lot of methods
to find and change nodes within the graph, you can insert/delete/change
any of their children nodes, fields, and generally do everything.

Of course, in case of such trivial animation as in this program, we could
also express it directly in the model (X3D or glTF, designed in Blender)
and just load the scene, and call MainScene.PlayAnimation.
This would make the scene animate, without the need for any ObjectPascal code to do this.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `animate_transform_by_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `animate_transform_by_code_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
