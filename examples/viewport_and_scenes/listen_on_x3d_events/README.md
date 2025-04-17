# Listen on X3D events from Pascal

Example how to build a scene with a X3D node TouchSensor (TTouchSensorNode in Pascal), and listen on TouchSensor "click" events.

This builds a scene with a simple Cone and a TouchSensor, using X3D nodes. TouchSensor is a node defined by X3D that can "sense" when user clicks on a shape. See http://www.web3d.org/documents/specifications/19775-1/V3.2/Part01/components/pointingsensor.html#TouchSensor and https://castle-engine.io/x3d_implementation_pointingdevicesensor.php for the details about the TouchSensor.

Then it registers simple callbacks on the TouchSensor touchTime and isActive events. You register callbacks simply by event's AddNotification.

Note: Often a simpler approach to capture clicks is to use `TCastleViewport.TransformUnderMouse` (see `../detect_scene_hit` example) or override `TCastleTransform.PointingDevicePress`.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `multiple_viewports_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
