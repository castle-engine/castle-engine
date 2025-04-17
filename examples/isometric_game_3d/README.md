# Isometric View in 3D

Demo how to arrange camera such that your 3D world is seen as isometric.

The important parts are:

- Set `MyViewport.Camera.ProjectionType` to `ptOrthographic`

- Set `MyViewport.Camera.Orthographic.Origin` to `(0.5,0.5)`

- Set `MyViewport.Camera.Orthographic.Width` to the size you want to see (like `10` -- just an example),

- Rotate camera `45` degrees around +Y (or `90 + 45` degrees, or `180 + 45` degrees...).

    You can do this by just rotating `MyViewport.Camera` using the editor gizmo, but it is hard to make it precisely `45` then. Better to input the necessary values directly.

    Moreover, it's even better to add a `TCastleTransform` that is camera parent (we call it `CameraHorizontalRotation` in this example) to perform this rotation. This allows to later independently move and rotate horizontally the camera (by `CameraHorizontalRotation`) without affecting camera vertical rotation (which you can do at child `Camera`).

    To make `CameraHorizontalRotation` rotate `45` degrees around +Y:

    - Option A: Set `MyViewport.Camera.Rotation` to rotate around _Axis (XYZ)_ = (0,1,0) by _Angle (W)_ = `pi/4`. This means that you can edit the `Rotation` vector, place 0 as `X`, place 1 as `Y`, place 0 as `Z`, place `pi/4` as `W` (editor will calculate what `pi/4` means).

    - Option B: Set `MyViewport.Camera.Direction` to `-1 0 1`. Do this by typing `-1 0 1` as whole vector value in `MyViewport.Camera.Direction` (`Direction` is part of _All_ tab) and accept by Enter -- this will actually normalize this vector making it like `-0.71 0.00 0.71`.

- Rotate camera to look down.

    Use gizmo to rotate camera along X axis (red) to look down as much as you want.

Note: You *can* have also isometric camera at design-time (in the editor). The easiest way to have it is to set up camera as above, and then use _"Viewport -> Align View To Camera"_ (numpad 0) menu item. After this, switch between your isometric and regular perspective as often as you need using _"Viewport -> Toggle Perspective / Orthogonal"_ (numpad 5).

TODO: The example design shows a warning, as rendering using _shadow volumes_ in orthographic view isn't guaranteed to be correct from all camera views (as we cannot have projection far in infinity). But as it happens, it is correct for views interesting in this demo, so we use here shadows -- they look cool :)

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `isometric_game_3d_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `isometric_game_3d_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
